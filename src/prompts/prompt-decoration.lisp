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

(defun extract-chroma-query-results (query-resp)
  "Safely extracts a list of plists containing :document and :metadata from a nested Chroma query response."
  (let ((docs-outer (cdr (assoc :documents query-resp)))
        (metas-outer (cdr (assoc :metadatas query-resp))))
    (when (and docs-outer (> (length docs-outer) 0))
      (let ((docs-inner (coerce (elt docs-outer 0) 'list))
            (metas-inner (coerce (elt metas-outer 0) 'list)))
        (loop for doc in docs-inner
              for meta in metas-inner
              collect (list :document doc :metadata meta))))))

(defun get-relevant-diary-entries-text (persona-name query-text)
  "Retrieves up to 3 relevant diary entries from ChromaDB for the given PERSONA-NAME,
using QUERY-TEXT as the query, and formats them as a clean text block."
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
                   (results (extract-chroma-query-results query-resp)))
              (when results
                (with-output-to-string (s)
                  (format s "~%[Relevant Historical Diary Entries (Transient Context)]~%")
                  (dolist (res results)
                    (let* ((doc (getf res :document))
                           (meta (getf res :metadata))
                           (num (cdr (assoc :entry--number meta)))
                           (date (cdr (assoc :date meta)))
                           (tone (cdr (assoc :tone meta)))
                           (topic (cdr (assoc :topic meta))))
                      (format s "---~%")
                      (when num (format s "Entry Number: ~D~%" num))
                      (when date (format s "Date: ~A~%" date))
                      (when tone (format s "Tone: ~A~%" tone))
                      (when topic (format s "Topic: ~A~%" topic))
                      (format s "Content:~%~A~%~%" doc)))))))))
    (error (e)
      (log-message :warn "Failed to fetch relevant diary entries"
                   :context `(("persona" . ,persona-name)
                              ("error" . ,(princ-to-string e))))
      nil)))

(defun decorate-live-user-input (chatbot input &key effective-model)
  "Decorates string INPUT with transient prompt prefixes and relevant diary entries requested by CHATBOT."
  (if (and chatbot
           (stringp input))
      (let* ((parts nil)
             (persona (chatbot-persona-name chatbot))
             (diary-text (and persona (get-relevant-diary-entries-text persona input))))
        (when (chatbot-include-timestamp-p chatbot)
          (push (funcall *prompt-timestamp-function*) parts))
        (when (chatbot-include-model-p chatbot)
          (push (format-prompt-model-indicator (or effective-model
                                                  (chatbot-model chatbot)))
                parts))
        (let* ((prefix (if parts (format nil "~{~A~^ ~} " (reverse parts)) ""))
               (decorated (format nil "~A~A" prefix input)))
          (if diary-text
              (format nil "~A~%~A" decorated diary-text)
              decorated)))
      input))
