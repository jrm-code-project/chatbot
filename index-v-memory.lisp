;;; -*- Lisp -*-
;;; index-v-memory.lisp - Script to sync V's knowledge graph observations to ChromaDB V_Memory collection

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(push (uiop:getcwd) asdf:*central-registry*)

(format t "Loading chatbot system...~%")
(finish-output)
(ql:quickload "chatbot" :silent t)

(in-package "CHATBOT")

(defun group-by-n (list n)
  "Groups a LIST into sublists of maximum length N."
  (loop for sublist on list by #'(lambda (l) (nthcdr n l))
        collect (subseq sublist 0 (min n (length sublist)))))

(defun clean-json-markdown (str)
  "Strips markdown code block formatting (e.g. ```json ... ```) from a JSON string."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) str)))
    (if (and (uiop:string-prefix-p "```" trimmed)
             (uiop:string-suffix-p "```" trimmed))
        (let* ((start-pos (position #\Linefeed trimmed))
               (end-pos (search "```" trimmed :from-end t)))
          (if (and start-pos end-pos (< start-pos end-pos))
              (string-trim '(#\Space #\Tab #\Return #\Linefeed) (subseq trimmed start-pos end-pos))
              trimmed))
        trimmed)))

(defun construct-complete-sentences-batched (batch)
  "Constructs complete sentences for a batch of observations (each item is a plist of :entity-name, :entity-type, :observation)
using a stateless and sterile raw API call to avoid agentic loop recursion."
  (handler-case
      (let (api-key headers url prompt-stream prompt-text payload response-json response candidates first-candidate content-obj parts first-part raw-text json-str parsed)
        (setf api-key (gemini-api-key))
        (setf headers (list (cons "x-goog-api-key" api-key) (cons "Content-Type" "application/json")))
        (setf url (format nil "~A/models/gemini-2.5-flash:generateContent" *gemini-base-url*))
        (setf prompt-stream (make-string-output-stream))
        (format prompt-stream "Construct a single, grammatically correct, natural-sounding complete sentence for each of the following entity observations.
Respond with ONLY a raw JSON array containing JSON objects with keys \"id\" (integer matching the index of the item, starting at 1), and \"sentence\" (the constructed sentence). Do not wrap in markdown or backticks.

Example:
Items:
1. Entity: user-authentication (type: feature), Observation: Uses JWT for authentication
2. Entity: Janus (type: AI), Observation: Writes its own code

Response:
[
  {\"id\": 1, \"sentence\": \"User-authentication uses JWT for authentication.\"},
  {\"id\": 2, \"sentence\": \"Janus writes its own code.\"}
]

Items to analyze:
")
        (let ((idx 0))
          (dolist (item batch)
            (incf idx)
            (format prompt-stream "~D. Entity: ~A (type: ~A), Observation: ~A~%"
                    idx
                    (getf item :entity-name)
                    (or (getf item :entity-type) "Entity")
                    (getf item :observation))))
        (setf prompt-text (get-output-stream-string prompt-stream))
        (setf payload (cl-json:encode-json-to-string
                       (list (cons :contents
                                   (vector (list (cons :parts
                                                       (vector (list (cons :text prompt-text))))))))))
        (setf response-json (post-web-request url headers payload))
        (setf response (cl-json:decode-json-from-string response-json))
        (setf candidates (cdr (assoc :candidates response)))
        (setf first-candidate (first candidates))
        (setf content-obj (cdr (assoc :content first-candidate)))
        (setf parts (cdr (assoc :parts content-obj)))
        (setf first-part (first parts))
        (setf raw-text (cdr (assoc :text first-part)))
        (setf json-str (clean-json-markdown raw-text))
        (setf parsed (cl-json:decode-json-from-string json-str))
        (mapcar (lambda (item)
                  (let* ((item-id (cdr (assoc :id item)))
                         (sentence (cdr (assoc :sentence item)))
                         (orig-item (nth (1- item-id) batch)))
                    (list :entity-name (getf orig-item :entity-name)
                          :entity-type (getf orig-item :entity-type)
                          :observation (getf orig-item :observation)
                          :sentence sentence)))
                parsed))
    (error (err)
      (format t "Batch sentence construction failed, using fallback: ~A~%" err)
      (finish-output)
      (mapcar (lambda (item)
                (let ((name (getf item :entity-name))
                      (type (getf item :entity-type))
                      (fact (getf item :observation)))
                  (list :entity-name name
                        :type type
                        :observation fact
                        :sentence (format nil "~A (~A): ~A" name (or type "Entity") fact))))
              batch))))

(defun run-indexing ()
  "Main function to coordinate V knowledge graph memory indexing."
  (format t "Checking if ChromaDB is running...~%")
  (finish-output)
  (if (not (chroma-alive-p))
      (progn
        (format t "ChromaDB is not running on ~A:~D. Skipping V_Memory indexing.~%"
                *chroma-host* *chroma-port*)
        (finish-output)
        (uiop:quit 0))
      (format t "ChromaDB is running! Proceeding with indexing.~%"))

  (let* ((persona-dir (resolve-persona-directory "V"))
         (memory-json-path (persona-memory-json-path persona-dir))
         (records (persona-memory-json-records memory-json-path))
         (observations nil))

    (format t "Loading and parsing V's memory.json...~%")
    (finish-output)

    ;; 1. Collect all entity observations safely handling lists and vectors
    (let* ((entities-raw (getf records :entities))
           (entities (cond
                       ((vectorp entities-raw) (coerce entities-raw 'list))
                       ((listp entities-raw) entities-raw)
                       (t (list entities-raw)))))
      (dolist (entity entities)
        (let* ((name (cdr (assoc :name entity)))
               (type (or (cdr (assoc :entity--type entity))
                         (cdr (assoc :entity-type entity))))
               (obs-raw (cdr (assoc :observations entity)))
               (obs (cond
                      ((vectorp obs-raw) (coerce obs-raw 'list))
                      ((listp obs-raw) obs-raw)
                      (t (list obs-raw)))))
          (dolist (content obs)
            (when (and (stringp content) (string/= content ""))
              (push (list :entity-name name :entity-type type :observation content) observations))))))

    (setf observations (nreverse observations))
    (let ((total (length observations)))
      (format t "Found ~D total knowledge graph observations to index.~%" total)
      (finish-output)

      ;; 2. Group into batches of 5 to analyze with Gemini (RPM efficiency)
      (let* ((batches (group-by-n observations 5))
             (batch-count (length batches))
             (current-batch-idx 0))
        (format t "Processing ~D batches of observations with Gemini for complete sentence generation...~%" batch-count)
        (finish-output)

        (dolist (batch batches)
          (incf current-batch-idx)
          (format t "Analyzing batch ~D of ~D... " current-batch-idx batch-count)
          (finish-output)

          (let ((sentences-list (construct-complete-sentences-batched batch))
                (collection-name "V_Memory")
                (collection (or (chroma-get-collection "V_Memory")
                                (chroma-create-collection "V_Memory" :get-or-create t))))
            (format t "done.~%")
            (finish-output)

            (if (null collection)
                (progn
                  (format t "failed to create/get collection V_Memory.~%")
                  (finish-output)
                  (uiop:quit 1))
                (let ((collection-id (cdr (assoc :id collection))))
                  ;; 3. Insert/Save each entry in V_Memory
                  (dolist (item sentences-list)
                    (let* ((entity-name (getf item :entity-name))
                           (entity-type (getf item :entity-type))
                           (fact (getf item :observation))
                           (sentence (getf item :sentence))
                           ;; Generate unique ID and metadata
                           (id (format nil "mem-~A-~X" (get-universal-time) (random #x1000000)))
                           (metadata `((:entity . ,entity-name)
                                       (:entity--type . ,(or entity-type ""))
                                       (:raw--observation . ,fact)))
                           ;; Generate embedding
                           (vector (string->embedding-vector sentence :model "gemini-embedding-2")))
                      (format t "  -> Indexing: Entity='~A', Fact='~A' -> Sentence='~A'... "
                              entity-name fact sentence)
                      (finish-output)
                      (multiple-value-bind (response status)
                          (chroma-add collection-id (list id)
                                      :embeddings (list vector)
                                      :documents (list sentence)
                                      :metadatas (list metadata))
                        (declare (ignore response))
                        (if (eq status :host-unavailable)
                            (progn
                              (format t "failed (host unavailable).~%")
                              (finish-output)
                              (uiop:quit 1))
                            (progn
                              (format t "success.~%")
                              (finish-output))))))))
          ;; Respect Gemini API rate limit by pausing briefly between batches
          (sleep 1.0)))

    (format t "Successfully completed indexing of all ~D knowledge graph memories into ChromaDB collection V_Memory!~%" total)
    (finish-output)
    (uiop:quit 0)))))

(run-indexing)
