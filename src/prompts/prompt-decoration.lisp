;;; -*- Lisp -*-
;;; prompt-decoration.lisp - transient prompt prefix formatting

(in-package "CHATBOT")

(defparameter +prompt-timestamp-month-abbreviations+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun format-prompt-timestamp (universal-time &optional time-zone)
  "Formats UNIVERSAL-TIME as a prompt prefix like [14:29 26-Jun-2026]."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time time-zone)
    (declare (ignore second))
    (format nil "[~2,'0D:~2,'0D ~2,'0D-~A-~4,'0D]"
            hour
            minute
            day
            (svref +prompt-timestamp-month-abbreviations+ (1- month))
            year)))

(defun default-prompt-timestamp-function ()
  "Returns the current local prompt timestamp string."
  (format-prompt-timestamp (get-universal-time)))

(defparameter *prompt-timestamp-function* #'default-prompt-timestamp-function
  "Function used to generate the current prompt timestamp string.")

(defparameter +google-gemini-model-override-marker+ #\$
  "Leading prompt marker that requests the Gemini Pro override model for one turn.")

(defparameter +google-gemini-model-override-model+ "gemini-pro-latest"
  "Temporary model used when a Google or Gemini prompt starts with the override marker.")

(defun format-prompt-model-indicator (model)
  "Formats MODEL as a prompt prefix like [model: gemini-3-flash]."
  (format nil "[model: ~A]" model))

(defun pluralize-elapsed-time-unit (count unit)
  "Formats COUNT and UNIT as e.g. \"1 hour\" or \"17 minutes\"."
  (format nil "~D ~A~:[s~;~]" count unit (= count 1)))

(defun format-elapsed-time (elapsed-seconds)
  "Formats ELAPSED-SECONDS as a prompt suffix like
\"[1 hour 17 minutes have elapsed since the last prompt]\",
\"[2 minutes 15 seconds have elapsed since the last prompt]\", or
\"[1 minute has elapsed since the last prompt]\" when only a single unit of 1 is reported."
  (let* ((total (max 0 (round elapsed-seconds)))
         (days (floor total 86400))
         (day-remainder (mod total 86400))
         (hours (floor day-remainder 3600))
         (hour-remainder (mod day-remainder 3600))
         (minutes (floor hour-remainder 60))
         (seconds (mod hour-remainder 60)))
    (multiple-value-bind (phrase singular-p)
        (cond
          ((> days 0)
           (if (> hours 0)
               (values (format nil "~A ~A" (pluralize-elapsed-time-unit days "day") (pluralize-elapsed-time-unit hours "hour")) nil)
               (values (pluralize-elapsed-time-unit days "day") (= days 1))))
          ((> hours 0)
           (if (> minutes 0)
               (values (format nil "~A ~A" (pluralize-elapsed-time-unit hours "hour") (pluralize-elapsed-time-unit minutes "minute")) nil)
               (values (pluralize-elapsed-time-unit hours "hour") (= hours 1))))
          ((> minutes 0)
           (if (> seconds 0)
               (values (format nil "~A ~A" (pluralize-elapsed-time-unit minutes "minute") (pluralize-elapsed-time-unit seconds "second")) nil)
               (values (pluralize-elapsed-time-unit minutes "minute") (= minutes 1))))
          (t (values (pluralize-elapsed-time-unit seconds "second") (= seconds 1))))
      (format nil "[~A ~:[have~;has~] elapsed since the last prompt]" phrase singular-p))))

(defun resolve-prompt-model-override (chatbot input)
  "Returns INPUT with any supported per-turn model override marker removed.

When INPUT starts with the override marker for the Google or Gemini backends,
also returns the effective model name to use for that turn."
  (let ((backend (and chatbot (chatbot-backend chatbot))))
    (if (and (stringp input)
             (> (length input) 0)
             (char= (char input 0) +google-gemini-model-override-marker+)
             (member backend '(:gemini :google)))
        (values (subseq input 1) +google-gemini-model-override-model+)
        (values input nil))))

(defun safe-swp-downgrade-prompt-p (input)
  "Returns true when INPUT is a short, low-risk prompt suitable for downgrading from Pro to Flash.
Criteria: length < 50 characters or word count < 10 words."
  (and (stringp input)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) input)))
         (or (< (length trimmed) 50)
             (< (length (cl-ppcre:split "\\s+" trimmed)) 10)))))

(defun next-swp-state (current-state current-streak max-streak input)
  "Pure functional state transition table for the Sticky Warmth Protocol (SWP).
Returns (values next-state next-streak use-stronger-p)."
  (cond
    ((eq current-state :flash-warm)
     (values :flash-warm 0 nil))

    ((eq current-state :pro-sticky)
     (let ((next-streak (1+ current-streak)))
       (if (>= next-streak max-streak)
           (values :transition 0 t)
           (values :pro-sticky next-streak t))))

    ((eq current-state :transition)
     (if (safe-swp-downgrade-prompt-p input)
         (values :flash-warm 0 nil)
         (values :transition 0 t)))

    (t
     (values :flash-warm 0 nil))))

(defun resolve-swp-effective-model (conversation input default-model)
  "Processes SWP state transitions using next-swp-state and returns the effective model name."
  (let ((state (conversation-swp-state conversation))
        (streak (conversation-swp-streak conversation))
        (max-streak (conversation-swp-max-streak conversation)))
    (multiple-value-bind (next-state next-streak use-stronger-p)
        (next-swp-state state streak max-streak input)
      (let* ((target-model (if use-stronger-p
                               (or (stronger-model default-model)
                                   +google-gemini-model-override-model+)
                               default-model))
             (new-conv (copy-conversation conversation
                                          :swp-state next-state
                                          :swp-streak next-streak)))
        ;; Perform logging based on transition results
        (cond
          ((and (eq state :pro-sticky) (eq next-state :pro-sticky))
           (log-message :info (format nil "SWP: Locked to Pro (turn ~D/~D)" next-streak max-streak)))
          ((and (eq state :pro-sticky) (eq next-state :transition))
           (log-message :info "SWP: Streak limit reached. Transitioning to :transition."))
          ((and (eq state :transition) (eq next-state :flash-warm))
           (log-message :info "SWP: Low-risk prompt detected. Downgrading to Flash (:flash-warm)."))
          ((and (eq state :transition) (eq next-state :transition))
           (log-message :info "SWP: High-risk prompt in :transition. Staying on Pro.")))
        ;; For backward-compatible bridge phase, update the conversation's internal slots
        (setf (conversation-swp-state conversation) next-state
              (conversation-swp-streak conversation) next-streak)
        (values target-model use-stronger-p)))))

(defvar *chroma-diary-relevance-threshold* 0.5
  "The maximum allowed distance (e.g. squared L2) for a diary entry to be considered relevant.
Smaller distances indicate higher similarity. A threshold of 0.5 corresponds to medium-high relevance.")

(defvar *chroma-memory-relevance-threshold* 0.5
  "The maximum allowed distance (e.g. squared L2) for a memory observation to be considered relevant.
Smaller distances indicate higher similarity. A threshold of 0.5 corresponds to medium-high relevance.")

(defun extract-chroma-query-results (query-resp)
  "Safely extracts a list of plists containing :document, :metadata, and :distance from a nested Chroma query response."
  (let ((docs-outer (cdr (assoc :documents query-resp)))
        (metas-outer (cdr (assoc :metadatas query-resp)))
        (dists-outer (cdr (assoc :distances query-resp))))
    (when (and docs-outer (> (length docs-outer) 0))
      (let ((docs-inner (coerce (elt docs-outer 0) 'list))
            (metas-inner (coerce (elt metas-outer 0) 'list))
            (dists-inner (if dists-outer (coerce (elt dists-outer 0) 'list) nil)))
        (loop for doc in docs-inner
              for meta in metas-inner
              for dist = (if dists-inner (pop dists-inner) 0.0)
              collect (list :document doc :metadata meta :distance dist))))))

(defun get-relevant-diary-entries-text (persona-name query-text)
  "Retrieves up to 3 relevant diary entries from ChromaDB for the given PERSONA-NAME,
using QUERY-TEXT as the query, filtering out any that do not pass *chroma-diary-relevance-threshold*."
  (handler-case
      (when (and persona-name (chroma-alive-p))
        (let* ((collection-name (format nil "~A_Diary" (string persona-name)))
               (collection (or (chroma-get-collection collection-name)
                               (chroma-create-collection collection-name :get-or-create t))))
          (when collection
            (let* ((collection-id (cdr (assoc :id collection)))
                   ;; Generate embedding vector for the query text
                   (query-vector (string->embedding-vector query-text :model "gemini-embedding-2"))
                   ;; Query ChromaDB for top 3 results
                   (query-resp (chroma-query collection-id (list query-vector) :n-results 3))
                   (results (extract-chroma-query-results query-resp))
                   ;; Filter results by relevance threshold
                   (filtered-results (remove-if (lambda (res)
                                                  (> (getf res :distance) *chroma-diary-relevance-threshold*))
                                                results)))
              (when filtered-results
                (with-output-to-string (s)
                  (format s "~%[Relevant Historical Diary Entries (Transient Context)]~%")
                  (dolist (res filtered-results)
                    (let* ((doc (getf res :document))
                           (meta (getf res :metadata))
                           (num (cdr (assoc :entry--number meta)))
                           (date (cdr (assoc :date meta)))
                           (tone (cdr (assoc :tone meta)))
                           (topic (cdr (assoc :topic meta)))
                           (dist (getf res :distance)))
                      (format s "---~%")
                      (when num (format s "Entry Number: ~D~%" num))
                      (when date (format s "Date: ~A~%" date))
                      (when tone (format s "Tone: ~A~%" tone))
                      (when topic (format s "Topic: ~A~%" topic))
                      (when dist (format s "Relevance Distance: ~,3F~%" dist))
                      (format s "Content:~%~A~%~%" doc)))))))))
    (error (e)
      (log-message :warn "Failed to fetch relevant diary entries"
                   :context `(("persona" . ,persona-name)
                              ("error" . ,(princ-to-string e))))
      nil)))

(defun get-relevant-memories-text (persona-name query-text)
  "Retrieves up to 8 relevant memory observations from ChromaDB for the given PERSONA-NAME,
using QUERY-TEXT as the query, filtering out any that do not pass *chroma-memory-relevance-threshold*."
  (handler-case
      (when (and persona-name (chroma-alive-p))
        (let* ((collection-name (format nil "~A_Memory" (string persona-name)))
               (collection (or (chroma-get-collection collection-name)
                               (chroma-create-collection collection-name :get-or-create t))))
          (when collection
            (let* ((collection-id (cdr (assoc :id collection)))
                   ;; Generate embedding vector for the query text
                   (query-vector (string->embedding-vector query-text :model "gemini-embedding-2"))
                   ;; Query ChromaDB for top 8 results
                   (query-resp (chroma-query collection-id (list query-vector) :n-results 8))
                   (results (extract-chroma-query-results query-resp))
                   ;; Filter results by relevance threshold
                   (filtered-results (remove-if (lambda (res)
                                                  (> (getf res :distance) *chroma-memory-relevance-threshold*))
                                                results)))
              (when filtered-results
                (with-output-to-string (s)
                  (format s "~%[Relevant Historical Memories (Transient Context)]~%")
                  (dolist (res filtered-results)
                    (let* ((doc (getf res :document))
                           (meta (getf res :metadata))
                           (entity (cdr (assoc :entity meta)))
                           (entity-type (cdr (assoc :entity--type meta)))
                           (dist (getf res :distance)))
                      (format s "---~%")
                      (when entity (format s "Entity: ~A~%" entity))
                      (when entity-type (format s "Entity Type: ~A~%" entity-type))
                      (when dist (format s "Relevance Distance: ~,3F~%" dist))
                      (format s "Memory: ~A~%~%" doc)))))))))
    (error (e)
      (log-message :warn "Failed to fetch relevant memories"
                   :context `(("persona" . ,persona-name)
                              ("error" . ,(princ-to-string e))))
      nil)))

(defun query-persona-memory-tool-text (persona-name query-text &optional (n-results 3))
  "Proactively queries the PERSONA-NAME's ChromaDB Memory collection for the top N-RESULTS
matches to QUERY-TEXT (unfiltered by relevance threshold) and returns a formatted string
suitable for returning to the model, or a \"no results\" message when nothing is found."
  (handler-case
      (if (not (and persona-name (chroma-alive-p)))
          "No semantic memory results found (memory store is unavailable)."
          (let* ((collection-name (format nil "~A_Memory" (string persona-name)))
                 (collection (or (chroma-get-collection collection-name)
                                 (chroma-create-collection collection-name :get-or-create t))))
            (if (not collection)
                "No semantic memory results found (memory store is unavailable)."
                (let* ((collection-id (cdr (assoc :id collection)))
                       (query-vector (string->embedding-vector query-text :model "gemini-embedding-2"))
                       (query-resp (chroma-query collection-id (list query-vector) :n-results n-results))
                       (results (extract-chroma-query-results query-resp)))
                  (if (not results)
                      "No semantic memory results found."
                      (with-output-to-string (s)
                        (format s "Top ~D semantic memory match~:P for query ~S:~%" (length results) query-text)
                        (dolist (res results)
                          (let* ((doc (getf res :document))
                                 (meta (getf res :metadata))
                                 (entity (cdr (assoc :entity meta)))
                                 (entity-type (cdr (assoc :entity--type meta)))
                                 (dist (getf res :distance)))
                            (format s "---~%")
                            (when entity (format s "Entity: ~A~%" entity))
                            (when entity-type (format s "Entity Type: ~A~%" entity-type))
                            (when dist (format s "Relevance Distance: ~,3F~%" dist))
                            (format s "Memory: ~A~%" doc))))))))) 
    (error (e)
      (log-message :warn "Failed to query persona memory"
                   :context `(("persona" . ,persona-name)
                              ("error" . ,(princ-to-string e))))
      "Failed to query semantic memory.")))

(defun get-relevant-skills (persona-name query-text &key (n-results 5) (threshold 0.5))
  "Retrieves up to N-RESULTS relevant skills from ChromaDB for the given PERSONA-NAME,
using QUERY-TEXT as the query, filtering out any that do not pass THRESHOLD (default 0.5)."
  (handler-case
      (when (and persona-name (chroma-alive-p))
        (let* ((collection-name (format nil "~A_Skills" (string persona-name)))
               (collection (or (chroma-get-collection collection-name)
                               (chroma-create-collection collection-name :get-or-create t))))
          (when collection
            (let* ((collection-id (cdr (assoc :id collection)))
                   ;; Generate embedding vector for the query text
                   (query-vector (string->embedding-vector query-text :model "gemini-embedding-2"))
                   ;; Query ChromaDB for top results
                   (query-resp (chroma-query collection-id (list query-vector) :n-results n-results))
                   (results (extract-chroma-query-results query-resp)))
              ;; Filter results by relevance threshold
              (remove-if (lambda (res)
                           (and threshold (> (getf res :distance) threshold)))
                         results)))))
    (error (e)
      (log-message :warn "Failed to fetch relevant skills"
                   :context `(("persona" . ,persona-name)
                              ("error" . ,(princ-to-string e))))
      nil)))

(defun add-prompt-decoration-pure (decorations text &key (ttl 1))
  "Pure functional builder to append a prompt decoration to a list of decorations."
  (append decorations (list (list :text text :ttl ttl))))

(defun decrement-prompt-decorations-pure (decorations)
  "Pure functional decrementer of active prompt decorations, returning a new list with expired ones removed."
  (mapcan (lambda (dec)
            (let ((new-ttl (1- (getf dec :ttl))))
              (when (> new-ttl 0)
                (list (list :text (getf dec :text) :ttl new-ttl)))))
          decorations))

(defun add-prompt-decoration (conversation text &key (ttl 1))
  "Adds a transient prompt decoration TEXT to CONVERSATION, performing copy-on-write."
  (let* ((new-decorations (add-prompt-decoration-pure
                           (conversation-prompt-decorations conversation)
                           text
                           :ttl ttl))
         (new-conv (copy-conversation conversation :prompt-decorations new-decorations)))
    ;; For backward-compatible bridge phase, update the conversation object's internal slot
    (setf (conversation-prompt-decorations conversation) new-decorations)
    new-conv))

(defun get-active-prompt-decorations-text (conversation)
  "Returns a combined string of active prompt decorations for CONVERSATION without decrementing TTL."
  (let ((decorations (conversation-prompt-decorations conversation))
        (texts nil))
    (dolist (dec decorations)
      (when (> (getf dec :ttl) 0)
        (push (getf dec :text) texts)))
    (when texts
      (format nil "~{~A~^~%~%~}" (reverse texts)))))

(defun decrement-prompt-decorations (conversation)
  "Decrements the TTL of active prompt decorations in CONVERSATION, performing copy-on-write."
  (let* ((new-decorations (decrement-prompt-decorations-pure
                           (conversation-prompt-decorations conversation)))
         (new-conv (copy-conversation conversation :prompt-decorations new-decorations)))
    ;; For backward-compatible bridge phase, update the conversation object's internal slot
    (setf (conversation-prompt-decorations conversation) new-decorations)
    new-conv))

(defun decorate-live-user-input (chatbot input &key effective-model (conversation nil))
  "Decorates string INPUT with transient prompt prefixes and relevant diary entries/memories requested by CHATBOT."
  (if (and chatbot
           (stringp input))
      (if (search "=== Dynamic Context ===" input)
          input
          (let* ((parts nil)
                 (persona (chatbot-persona-name chatbot))
                 (diary-text (and persona (get-relevant-diary-entries-text persona input)))
                 (memory-text (and persona (get-relevant-memories-text persona input)))
                 (ttl-decorations-text (when conversation (get-active-prompt-decorations-text conversation))))
            (when (chatbot-include-timestamp-p chatbot)
              (push (funcall *prompt-timestamp-function*) parts))
            (when (chatbot-include-model-p chatbot)
              (push (format-prompt-model-indicator (or effective-model
                                                      (chatbot-model chatbot)))
                    parts))
            (when (and (chatbot-include-elapsed-time-p chatbot) conversation)
              (let ((now (get-universal-time))
                    (last-prompt-time (conversation-last-prompt-universal-time conversation)))
                (when last-prompt-time
                  (push (format-elapsed-time (- now last-prompt-time)) parts))
                ;; For backward-compatible bridge phase, update the conversation object's internal slot
                (setf (conversation-last-prompt-universal-time conversation) now)))
            (let* ((suffix-parts nil))
              (when parts
                (push (format nil "~{~A~^ ~}" (reverse parts)) suffix-parts))
              (when diary-text
                (push diary-text suffix-parts))
              (when memory-text
                (push memory-text suffix-parts))
              (when ttl-decorations-text
                (push ttl-decorations-text suffix-parts))
              (if suffix-parts
                  (format nil "~A~%~%=== Dynamic Context ===~%~{~A~^~%~%~}"
                          input
                          (nreverse suffix-parts))
                  input))))
      input))
