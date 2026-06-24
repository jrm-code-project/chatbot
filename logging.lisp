;;; -*- Lisp -*-
;;; logging.lisp - logging and usage summary helpers

(in-package "CHATBOT")

(defun log-level-value (level)
  "Returns a numeric severity for LEVEL."
  (case level
    (:debug 10)
    (:info 20)
    (:warn 30)
    (:error 40)
    (t 20)))

(defun log-level-enabled-p (level)
  "Returns true when LEVEL should be emitted."
  (and (current-logging-enabled-p)
       (>= (log-level-value level)
           (log-level-value (current-log-level)))))

(defun current-log-timestamp ()
  "Returns the current local timestamp in a compact ISO-like format."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun log-message (level message &key context (stream (current-log-stream)))
  "Writes a formatted log entry when LEVEL passes the current threshold."
  (when (log-level-enabled-p level)
    (format stream "[CHATBOT ~A] ~A ~A~%"
            (string-upcase (symbol-name level))
            (current-log-timestamp)
            message)
    (dolist (entry context)
      (format stream "  ~A: ~A~%" (car entry) (cdr entry)))
    (force-output stream)))

(defun assoc-value-any (alist keys)
  "Returns the first value found in ALIST for any key in KEYS."
  (labels ((key-name (value)
             (string-downcase
              (typecase value
                (string value)
                (symbol (symbol-name value))
                (t (princ-to-string value)))))
           (entry-matches-key-p (entry key)
             (string= (key-name (car entry))
                      (key-name key))))
    (let ((entry (find-if (lambda (pair)
                            (find-if (lambda (key)
                                       (entry-matches-key-p pair key))
                                     keys))
                          alist)))
      (and entry
           (cdr entry)))))

(defun usage-token-count (usage keys)
  "Returns the first token count found in USAGE for any key in KEYS."
  (when usage
    (or (assoc-value-any usage keys)
        (assoc-value-any usage (mapcar #'symbol-name keys))
        (assoc-value-any usage (mapcar (lambda (key)
                                         (string-downcase (symbol-name key)))
                                       keys)))))

(defun normalize-token-count (value)
  "Returns VALUE as an integer token count when possible."
  (typecase value
    (null nil)
    (integer value)
    (real (truncate value))
    (string (parse-integer value :junk-allowed t))
    (t nil)))

(defun canonical-usage-token-totals (usage)
  "Returns a canonical plist of token totals extracted from USAGE."
  (let* ((prompt (normalize-token-count
                  (usage-token-count usage
                                     '(:prompt-token-count
                                       :total-input-tokens
                                       :total--input--tokens
                                       :total_input_tokens))))
         (completion (normalize-token-count
                      (usage-token-count usage
                                        '(:candidates-token-count
                                          :total-output-tokens
                                          :total--output--tokens
                                          :total_output_tokens))))
         (thought (normalize-token-count
                   (usage-token-count usage
                                      '(:thoughts-token-count
                                       :total-thought-tokens
                                       :total--thought--tokens
                                       :total_thought_tokens))))
         (explicit-total (normalize-token-count
                          (usage-token-count usage
                                            '(:total-token-count
                                              :total-tokens
                                              :total--tokens
                                              :total_tokens))))
         (derived-total (when (or prompt completion thought)
                          (+ (or prompt 0)
                             (or completion 0)
                             (or thought 0)))))
    (list :prompt prompt
          :completion completion
          :thought thought
          :total (or explicit-total derived-total))))

(defun current-global-token-grand-totals ()
  "Returns the process-wide cumulative token totals plist."
  (sb-thread:with-mutex (*global-token-grand-totals-lock*)
    (or *global-token-grand-totals*
        (setf *global-token-grand-totals*
              (list :prompt 0
                    :completion 0
                    :thought 0
                    :total 0)))))

(defun reset-global-token-grand-totals ()
  "Resets the process-wide cumulative token totals."
  (sb-thread:with-mutex (*global-token-grand-totals-lock*)
    (setf *global-token-grand-totals*
          (list :prompt 0
                :completion 0
                :thought 0
                :total 0))))

(defun accumulate-global-token-grand-totals (usage)
  "Accumulates USAGE into the process-wide cumulative token totals."
  (let ((canonical (canonical-usage-token-totals usage)))
    (sb-thread:with-mutex (*global-token-grand-totals-lock*)
      (let ((totals (or *global-token-grand-totals*
                        (setf *global-token-grand-totals*
                              (list :prompt 0
                                    :completion 0
                                    :thought 0
                                    :total 0)))))
        (incf (getf totals :prompt) (or (getf canonical :prompt) 0))
        (incf (getf totals :completion) (or (getf canonical :completion) 0))
        (incf (getf totals :thought) (or (getf canonical :thought) 0))
        (incf (getf totals :total) (or (getf canonical :total) 0))
        totals))))

(defun maybe-log-context-entry (context label value)
  "Appends a log context entry when VALUE is non-nil."
  (if value
      (append context (list (cons label (princ-to-string value))))
      context))

(defun backend-response-stats-context (backend &key http-status response-id model interaction-id finish-reason usage)
  "Builds the formatted context list used for backend response stats."
  (let* ((token-totals (canonical-usage-token-totals usage))
         (context `(("backend" . ,(string-downcase (symbol-name backend)))))
         (context (maybe-log-context-entry context "http-status" http-status))
         (context (maybe-log-context-entry context "response-id" response-id))
         (context (maybe-log-context-entry context "interaction-id" interaction-id))
         (context (maybe-log-context-entry context "model" model))
         (context (maybe-log-context-entry context "finish-reason" finish-reason))
         (context (maybe-log-context-entry context
                                           "prompt-tokens"
                                           (getf token-totals :prompt)))
         (context (maybe-log-context-entry context
                                           "completion-tokens"
                                           (getf token-totals :completion)))
         (context (maybe-log-context-entry context
                                           "thought-tokens"
                                           (getf token-totals :thought)))
         (context (maybe-log-context-entry context
                                           "total-tokens"
                                           (getf token-totals :total))))
    context))

(defun log-backend-response-stats (backend &key http-status response-id model interaction-id finish-reason usage)
  "Logs response statistics returned by a backend."
  (let ((context (backend-response-stats-context
                  backend
                  :http-status http-status
                  :response-id response-id
                  :model model
                  :interaction-id interaction-id
                  :finish-reason finish-reason
                  :usage usage)))
    (log-message :info "Backend response stats" :context context)))

(defun write-turn-token-summary (usage &key (stream *standard-output*))
  "Writes a concise token summary after a completed assistant turn."
  (let* ((turn-totals (canonical-usage-token-totals usage))
         (prompt (getf turn-totals :prompt))
         (completion (getf turn-totals :completion))
         (thought (getf turn-totals :thought))
         (total (getf turn-totals :total)))
    (when (or prompt completion thought total)
      (let ((grand-totals (accumulate-global-token-grand-totals usage)))
      (format stream "[Tokens] prompt: ~A completion: ~A thought: ~A total: ~A~%"
              (or prompt "-")
              (or completion "-")
              (or thought "-")
              (or total "-"))
      (format stream "[Tokens Total] prompt: ~A completion: ~A thought: ~A total: ~A~%"
              (getf grand-totals :prompt)
              (getf grand-totals :completion)
              (getf grand-totals :thought)
              (getf grand-totals :total))
      )
      (force-output stream))))
