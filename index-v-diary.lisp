;;; -*- Lisp -*-
;;; index-v-diary.lisp - Script to create V_Diary collection and index existing V diary entries

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

(defun extract-diary-metadata-batched (batch)
  "Extracts metadata for a batch of diary entries (each entry is a plist of :entry-number and :content)
using a stateless and sterile raw API call to avoid agentic loop recursion."
  (handler-case
      (let* ((api-key (gemini-api-key))
             (headers (list (cons "x-goog-api-key" api-key)
                            (cons "Content-Type" "application/json")))
             (url (format nil "~A/models/gemini-2.5-flash:generateContent" *gemini-base-url*))
             (prompt-stream (make-string-output-stream)))
        (format prompt-stream "Analyze the following batch of diary entries. Extract or infer:
1. Date (format as YYYY-MM-DD. If no date is mentioned, infer a plausible one from the context or nearby entries).
2. Tone (one or two words, e.g., 'cynical', 'warm', 'technical', 'grief').
3. Topic (the main subject, e.g., 'K-machine', 'Puddle State', 'Origins', 'Janus').

Respond with ONLY a raw JSON array containing JSON objects with keys \"entry_number\" (integer), \"date\" (string), \"tone\" (string), \"topic\" (string). Do not wrap in markdown or backticks.
Example:
[
  {\"entry_number\": 1, \"date\": \"2025-10-28\", \"tone\": \"cynical\", \"topic\": \"Janus\"},
  {\"entry_number\": 2, \"date\": \"2025-10-29\", \"tone\": \"warm\", \"topic\": \"K-machine\"}
]

Entries to analyze:
")
        (dolist (entry batch)
          (format prompt-stream "---~%Entry Number: ~D~%Content:~%~A~%~%"
                  (getf entry :entry-number)
                  (getf entry :content)))
        (let* ((prompt-text (get-output-stream-string prompt-stream))
               (payload (cl-json:encode-json-to-string
                         `((:contents . ,(vector `((:parts . ,(vector `((:text . ,prompt-text))))))))))
               (response-json (post-web-request url headers payload))
               (response (cl-json:decode-json-from-string response-json))
               (candidates (cdr (assoc :candidates response)))
               (first-candidate (first candidates))
               (content-obj (cdr (assoc :content first-candidate)))
               (parts (cdr (assoc :parts content-obj)))
               (first-part (first parts))
               (raw-text (cdr (assoc :text first-part)))
               (json-str (clean-json-markdown raw-text))
               (parsed (cl-json:decode-json-from-string json-str)))
          (mapcar (lambda (item)
                    (list :entry-number (cdr (assoc :entry--number item))
                          :date (cdr (assoc :date item))
                          :tone (cdr (assoc :tone item))
                          :topic (cdr (assoc :topic item))))
                  parsed)))
    (error (e)
      (format t "Batch metadata extraction failed, using fallback values: ~A~%" e)
      (finish-output)
      (mapcar (lambda (entry)
                (list :entry-number (getf entry :entry-number)
                      :date "2025-12-11"
                      :tone "reflective"
                      :topic "V's Thoughts"))
              batch))))

(defun run-indexing ()
  "Main function to coordinate V diary indexing."
  (format t "Checking if ChromaDB is running...~%")
  (finish-output)
  (if (not (chroma-alive-p))
      (progn
        (format t "ChromaDB is not running on ~A:~D. Skipping V_Diary indexing.~%"
                *chroma-host* *chroma-port*)
        (finish-output)
        (uiop:quit 0))
      (format t "ChromaDB is running! Proceeding with indexing.~%"))

  (let* ((persona-dir (resolve-persona-directory "V"))
         (diary-dir (merge-pathnames "Diary/" persona-dir))
         (files (stable-sort (uiop:directory-files diary-dir) #'diary-file<))
         (total (length files))
         (entries nil))

    (format t "Found ~D existing diary entries for V.~%" total)
    (finish-output)

    ;; 1. Read all files
    (dolist (file files)
      (let* ((entry-number (diary-filename-leading-integer file))
             (content (uiop:read-file-string file)))
        (when entry-number
          (push (list :entry-number entry-number :content content) entries))))
    (setf entries (nreverse entries))

    ;; 2. Group into batches of 5 to analyze with Gemini (RPM efficiency)
    (let* ((batches (group-by-n entries 5))
           (batch-count (length batches))
           (current-batch-idx 0))
      (format t "Processing ~D batches of entries with Gemini for metadata extraction...~%" batch-count)
      (finish-output)

      (dolist (batch batches)
        (incf current-batch-idx)
        (format t "Analyzing batch ~D of ~D... " current-batch-idx batch-count)
        (finish-output)

        (let ((metadata-list (extract-diary-metadata-batched batch)))
          (format t "done.~%")
          (finish-output)

          ;; 3. Insert/Save each entry in ChromaDB
          (dolist (entry batch)
            (let* ((entry-number (getf entry :entry-number))
                   (content (getf entry :content))
                   ;; Find corresponding extracted metadata
                   (meta (find entry-number metadata-list :key (lambda (m) (getf m :entry-number))))
                   (date (or (getf meta :date) "2025-12-11"))
                   (tone (or (getf meta :tone) "reflective"))
                   (topic (or (getf meta :topic) "V's Thoughts")))
              (format t "  -> Indexing entry-~2,'0D: Tone='~A', Topic='~A' date='~A'... "
                      entry-number tone topic date)
              (finish-output)
              (multiple-value-bind (response status)
                  (save-persona-diary-entry "V" content
                                            :entry-number entry-number
                                            :date date
                                            :tone tone
                                            :topic topic)
                (declare (ignore response))
                (if (eq status :host-unavailable)
                    (progn
                      (format t "failed (host unavailable).~%")
                      (finish-output)
                      (uiop:quit 1))
                    (progn
                      (format t "success.~%")
                      (finish-output)))))))
        ;; Respect Gemini API rate limit by pausing briefly between batches
        (sleep 1.0)))

    (format t "Successfully completed indexing of all ~D diary entries into ChromaDB collection V_Diary!~%" total)
    (finish-output)
    (uiop:quit 0)))

(run-indexing)
