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

(defun resolve-swp-effective-model (conversation input default-model)
  "Processes SWP state transitions and returns the effective model name."
  (let ((state (conversation-swp-state conversation))
        (streak (conversation-swp-streak conversation))
        (max-streak (conversation-swp-max-streak conversation)))
    (cond
      ((eq state :flash-warm)
       ;; Baseline: use the default model
       (values default-model nil))
      
      ((eq state :pro-sticky)
       ;; We are locked to Pro. Increment streak.
       (incf (conversation-swp-streak conversation))
       (let ((current-streak (conversation-swp-streak conversation))
             (target-model (or (stronger-model default-model)
                               +google-gemini-model-override-model+)))
         (log-message :info (format nil "SWP: Locked to Pro (turn ~D/~D)" current-streak max-streak))
         ;; If we've reached the max streak, transition to :transition
         (when (>= current-streak max-streak)
           (setf (conversation-swp-state conversation) :transition
                 (conversation-swp-streak conversation) 0)
           (log-message :info "SWP: Streak limit reached. Transitioning to :transition."))
         (values target-model t)))
      
      ((eq state :transition)
       ;; Cooldown / Return phase: look for low-risk prompt
       (if (safe-swp-downgrade-prompt-p input)
           (progn
             (setf (conversation-swp-state conversation) :flash-warm
                   (conversation-swp-streak conversation) 0)
             (log-message :info "SWP: Low-risk prompt detected. Downgrading to Flash (:flash-warm).")
             (values default-model nil))
           (progn
             ;; Remain in :transition and continue on Pro
             (let ((target-model (or (stronger-model default-model)
                                     +google-gemini-model-override-model+)))
               (log-message :info "SWP: High-risk prompt in :transition. Staying on Pro.")
               (values target-model t)))))
      
      (t (values default-model nil)))))

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
               (collection (chroma-get-collection collection-name)))
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
               (collection (chroma-get-collection collection-name)))
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

(defun decorate-live-user-input (chatbot input &key effective-model)
  "Decorates string INPUT with transient prompt prefixes and relevant diary entries/memories requested by CHATBOT."
  (if (and chatbot
           (stringp input))
      (if (search "=== Dynamic Context ===" input)
          input
          (let* ((parts nil)
                 (persona (chatbot-persona-name chatbot))
                 (diary-text (and persona (get-relevant-diary-entries-text persona input)))
                 (memory-text (and persona (get-relevant-memories-text persona input))))
            (when (chatbot-include-timestamp-p chatbot)
              (push (funcall *prompt-timestamp-function*) parts))
            (when (chatbot-include-model-p chatbot)
              (push (format-prompt-model-indicator (or effective-model
                                                      (chatbot-model chatbot)))
                    parts))
            (let* ((suffix-parts nil))
              (when parts
                (push (format nil "~{~A~^ ~}" (reverse parts)) suffix-parts))
              (when diary-text
                (push diary-text suffix-parts))
              (when memory-text
                (push memory-text suffix-parts))
              (if suffix-parts
                  (format nil "~A~%~%=== Dynamic Context ===~%~{~A~^~%~%~}"
                          input
                          (nreverse suffix-parts))
                  input))))
      input))
