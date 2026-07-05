;;; -*- Lisp -*-
;;; source-utils.lisp - helpers for reading Lisp source text by form boundaries

(in-package "CHATBOT")

(defun read-file-form-ranges (pathname)
  "Returns a list of character ranges for every readable top-level form in PATHNAME."
  (with-open-file (stream pathname :direction :input)
    (let ((eof-marker (gensym "EOF")))
      (loop for start = (file-position stream)
            for form = (read stream nil eof-marker)
            until (eq form eof-marker)
            collect (cons start (file-position stream))))))

(defun trim-leading-newlines (text)
  "Returns TEXT with leading newline characters removed."
  (string-left-trim '(#\Newline #\Return) text))

(defun normalize-form-source-text (text)
  "Returns TEXT with surrounding newline delimiters removed."
  (string-right-trim '(#\Newline #\Return)
                     (trim-leading-newlines text)))

(defun empty-form-source-text-p (text)
  "Returns true when TEXT contains no non-whitespace source characters."
  (string= "" (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun read-file-form-text-for-range (pathname range)
  "Returns the line-bounded source text in PATHNAME covering RANGE."
  (with-open-file (stream pathname :direction :input)
    (let ((start-position (car range))
          (end-position (cdr range))
          (buffer nil)
          (collecting-p nil))
      (loop
        for line-start = (file-position stream)
        do (multiple-value-bind (line eofp)
               (read-line stream nil nil)
             (when (null line)
               (return (normalize-form-source-text
                        (with-output-to-string (out)
                          (dolist (piece (nreverse buffer))
                            (write-string piece out))))))
             (let ((line-end (file-position stream)))
               (when (and (not collecting-p)
                          (<= line-start end-position)
                          (> line-end start-position))
                 (setf collecting-p t))
               (when collecting-p
                 (push (string-right-trim '(#\Return) line) buffer)
                 (unless eofp
                   (push (string #\Newline) buffer))
                 (when (>= line-end end-position)
                   (return (normalize-form-source-text
                            (with-output-to-string (out)
                              (dolist (piece (nreverse buffer))
                                (write-string piece out)))))))))))))

(defun read-file-forms-as-text (pathname)
  "Returns every readable top-level form in PATHNAME as line-bounded source text.

The file is processed in two passes: the first pass uses READ to locate each form,
and the second pass uses READ-LINE to recover the enclosing line text."
  (remove-if #'empty-form-source-text-p
             (mapcar (lambda (range)
                       (read-file-form-text-for-range pathname range))
                     (read-file-form-ranges pathname))))
