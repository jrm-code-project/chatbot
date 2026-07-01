;;; -*- Lisp -*-
;;; text-utils.lisp - SSE parsing and text formatting helpers

(in-package "CHATBOT")

(defun call-with-stream-read-timeout (thunk &key timeout-seconds (timeout-context "streamed response"))
  "Calls THUNK, applying a hard timeout when TIMEOUT-SECONDS is a positive number."
  (handler-case
      (if (and timeout-seconds
               (> timeout-seconds 0))
          (trivial-timeout:with-timeout (timeout-seconds)
            (funcall thunk))
          (funcall thunk))
    (trivial-timeout:timeout-error ()
      (error "Timed out waiting ~A seconds for ~A."
             timeout-seconds
             timeout-context))))

(defun read-sse-line (stream &key timeout-seconds (timeout-context "streamed response"))
  "Reads a single line from the stream, stripping any trailing carriage returns."
  (handler-case
      (let ((line (call-with-stream-read-timeout
                   (lambda ()
                     (read-line stream nil :eof))
                   :timeout-seconds timeout-seconds
                   :timeout-context timeout-context)))
        (if (eq line :eof)
            :eof
            (string-right-trim '(#\Return) line)))
    (end-of-file () :eof)))

(defun parse-sse-event (line)
  "Parses a single SSE line starting with 'data: ' and returns decoded JSON as an alist."
  (when (and (stringp line)
             (alexandria:starts-with-subseq "data: " line))
    (let ((payload (subseq line 6)))
      (unless (string= payload "[DONE]")
        (parse-json-or-error payload :context "SSE event")))))

(defun wrap-text (text &key (width 80) (initial-prefix ""))
  "Wraps a single paragraph string to the specified width."
  (let ((words (cl-ppcre:split "\\s+" text))
        (initial-prefix-length (length initial-prefix)))
    (labels ((line-string (line first-line-p)
               (format nil "~A~{~A~^ ~}"
                       (if first-line-p initial-prefix "")
                       line))
             (consume-words (remaining first-line-p current-line current-length lines)
               (if (endp remaining)
                   (if current-line
                       (append lines (list (line-string current-line first-line-p)))
                       lines)
                   (let* ((word (first remaining))
                          (word-len (length word))
                          (line-prefix-length (if first-line-p initial-prefix-length 0))
                          (available-width (max 1 (- width line-prefix-length))))
                     (cond
                       ((null current-line)
                        (consume-words (rest remaining)
                                       first-line-p
                                       (list word)
                                       word-len
                                       lines))
                       ((<= (+ current-length 1 word-len) available-width)
                        (consume-words (rest remaining)
                                       first-line-p
                                       (append current-line (list word))
                                       (+ current-length 1 word-len)
                                       lines))
                       (t
                        (consume-words remaining
                                       nil
                                       nil
                                       0
                                       (append lines
                                               (list (line-string current-line first-line-p))))))))))
      (consume-words words t nil 0 nil))))

(defun blank-line-p (line)
  "Returns true when LINE contains only whitespace."
  (string= "" (string-trim '(#\Space #\Tab #\Return) line)))

(defun fenced-code-line-p (line)
  "Returns true when LINE begins a Markdown fenced code line."
  (alexandria:starts-with-subseq
   "```"
   (string-left-trim '(#\Space #\Tab) line)))

(defun ordered-bullet-line-p (line)
  "Returns true when LINE begins with an ordered Markdown list marker."
  (let ((dot-position (position #\. line)))
    (and dot-position
         (> dot-position 0)
         (every #'digit-char-p (subseq line 0 dot-position))
         (< (1+ dot-position) (length line))
         (member (char line (1+ dot-position)) '(#\Space #\Tab)))))

(defun bullet-line-p (line)
  "Returns true when LINE begins with a Markdown bullet or numbered list marker."
  (let ((trimmed-line (string-left-trim '(#\Space #\Tab) line)))
    (or (alexandria:starts-with-subseq "- " trimmed-line)
        (alexandria:starts-with-subseq "* " trimmed-line)
        (alexandria:starts-with-subseq "+ " trimmed-line)
        (ordered-bullet-line-p trimmed-line))))

(defun paragraph-preserve-verbatim-p (lines)
  "Returns true when LINES should be emitted without reflow."
  (some #'bullet-line-p lines))

(defun format-paragraphs (text &key (width 80) (stream *standard-output*))
  "Formats text to STREAM, wrapping prose paragraphs while preserving fenced code blocks verbatim."
  (let ((blocks nil)
        (paragraph-lines nil)
        (fence-lines nil)
        (in-fence-p nil))
    (labels ((flush-paragraph ()
              (when paragraph-lines
                (let ((lines (nreverse paragraph-lines)))
                  (push (if (paragraph-preserve-verbatim-p lines)
                            (cons :verbatim-lines lines)
                            (cons :prose (format nil "~{~A~^ ~}" lines)))
                        blocks))
                (setf paragraph-lines nil)))
            (flush-fence ()
              (when fence-lines
                (push (cons :fence (nreverse fence-lines)) blocks)
                (setf fence-lines nil))))
      (dolist (line (cl-ppcre:split "\\r?\\n" text))
        (cond
          (in-fence-p
           (push line fence-lines)
           (when (fenced-code-line-p line)
            (flush-fence)
            (setf in-fence-p nil)))
          ((fenced-code-line-p line)
           (flush-paragraph)
           (setf in-fence-p t
                fence-lines (list line)))
          ((blank-line-p line)
           (flush-paragraph))
          (t
           (push line paragraph-lines))))
      (flush-paragraph)
      (flush-fence))
    (loop for (block-type . content) in (nreverse blocks)
          for first-p = t then nil
          do (unless first-p
              (terpri stream))
            (ecase block-type
              (:prose
               (dolist (line (wrap-text content :width width :initial-prefix "  "))
                 (write-line line stream)))
              (:verbatim-lines
               (dolist (line content)
                 (write-line line stream)))
              (:fence
               (dolist (line content)
                 (write-line line stream)))))))

(defun print-chat-speaker-header (speaker &key (stream *standard-output*))
  "Prints a bracketed SPEAKER heading to STREAM."
  (format stream "[~A]~%" speaker))

(defun print-chat-speaker-block (speaker text &key (width 80) (stream *standard-output*))
  "Prints SPEAKER and TEXT using the standard wrapped transcript style."
  (print-chat-speaker-header speaker :stream stream)
  (format-paragraphs text :width width :stream stream)
  (terpri stream)
  (terpri stream))
