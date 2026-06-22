;;; -*- Lisp -*-
;;; text-utils.lisp - SSE parsing and text formatting helpers

(in-package "CHATBOT")

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
    (parse-json-or-error (subseq line 6) :context "SSE event")))

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
