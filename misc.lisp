;;;

(in-package "CHATBOT")

;;; Custom JSON encoders for booleans to work nicely with cl-json
(defmethod cl-json:encode-json ((object (eql t)) &optional (stream *standard-output*))
  (write-string "true" stream)
  nil)

(defmethod cl-json:encode-json ((object (eql :false)) &optional (stream *standard-output*))
  (write-string "false" stream)
  nil)

;;; Miscellaneous utility functions for the Chatbot framework

(defun make-interaction-payload (chatbot input &key previous-interaction-id (stream t))
  "Creates a JSON-serializable alist payload for the Gemini Interactions API."
  (let ((payload (list (cons "model" (chatbot-model chatbot))
                       (cons "input" input)
                       (cons "stream" (if stream t :false))
                       (cons "store" t))))
    (when previous-interaction-id
      (push (cons "previous_interaction_id" previous-interaction-id) payload))
    (when (chatbot-system-instruction chatbot)
      (push (cons "system_instruction" (chatbot-system-instruction chatbot)) payload))
    ;; Handle tools
    (let ((tools nil))
      (when (chatbot-google-search-p chatbot)
        (push '(("type" . "google_search")) tools))
      (when (chatbot-code-execution-p chatbot)
        (push '(("type" . "code_execution")) tools))
      (when tools
        (push (cons "tools" (nreverse tools)) payload)))
    (nreverse payload)))

(defun read-sse-line (stream)
  "Reads a single line from the stream, stripping any trailing carriage returns."
  (handler-case
      (let ((line (read-line stream nil :eof)))
        (if (eq line :eof)
            :eof
            (string-right-trim '(#\Return) line)))
    (end-of-file () :eof)))

(defun parse-sse-event (line)
  "Parses a single SSE line starting with 'data: ' and returns decoded JSON as an alist."
  (when (and (stringp line)
             (alexandria:starts-with-subseq "data: " line))
    (let ((json-str (subseq line 6)))
      (handler-case
          (cl-json:decode-json-from-string json-str)
        (error () nil)))))

(defun wrap-text (text &key (width 80))
  "Wraps a single paragraph string to the specified width."
  (let ((words (cl-ppcre:split "\\s+" text))
        (lines nil)
        (current-line nil)
        (current-length 0))
    (dolist (word words)
      (let ((word-len (length word)))
        (cond
          ((null current-line)
           (push word current-line)
           (setf current-length word-len))
          ((<= (+ current-length 1 word-len) width)
           (push word current-line)
           (incf current-length (1+ word-len)))
          (t
           (push (format nil "~{~A~^ ~}" (nreverse current-line)) lines)
           (setf current-line (list word))
           (setf current-length word-len)))))
    (when current-line
      (push (format nil "~{~A~^ ~}" (nreverse current-line)) lines))
    (nreverse lines)))

(defun format-paragraphs (text &key (width 80) (stream *standard-output*))
  "Formats text to stream, split into paragraphs wrapped at width."
  (let ((paragraphs (cl-ppcre:split "\\n{2,}" text)))
    (loop for (para . rest) on paragraphs
          do (let ((lines (wrap-text para :width width)))
               (dolist (line lines)
                 (write-line line stream))
               (when rest
                 (terpri stream))))))

(defun safe-getf (plist indicator &optional default)
  "Safely retrieves the value associated with indicator in plist, 
even if the plist is malformed (e.g. has an odd number of elements or is not a list)."
  (if (not (listp plist))
      default
      (let ((tail plist))
        (loop
          (cond
            ((null tail) (return default))
            ((not (consp tail)) (return default))
            ((eq (car tail) indicator)
             (return (if (consp (cdr tail))
                         (cadr tail)
                         default)))
            (t
             (setf tail (cdr tail))
             (if (consp tail)
                 (setf tail (cdr tail))
                 (return default))))))))
