;;; -*- Lisp -*-
;;; tool-arguments.lisp - built-in chatbot tool argument normalization

(in-package "CHATBOT")

(defun normalize-builtin-tool-integer-argument (value argument-name tool-name)
  "Normalizes VALUE to an integer argument or signals an execution error."
  (let ((normalized
          (typecase value
            (integer value)
            (real (truncate value))
            (string (parse-integer value :junk-allowed t))
            (t nil))))
    (unless normalized
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be an integer." argument-name)))
    normalized))

(defun normalize-builtin-tool-string-argument (value argument-name tool-name)
  "Normalizes VALUE to a non-empty string argument or signals an execution error."
  (unless (and (stringp value) (string/= value ""))
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A must be a non-empty string." argument-name)))
  value)

(defun normalize-builtin-tool-real-argument (value argument-name tool-name &key allow-nil-p)
  "Normalizes VALUE to a real argument or signals an execution error."
  (when (null value)
    (if allow-nil-p
        (return-from normalize-builtin-tool-real-argument nil)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "~A is required." argument-name))))
  (let ((normalized
          (typecase value
            (real (float value 1.0d0))
            (string
             (let* ((*read-eval* nil)
                    (trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) value)))
               (handler-case
                   (multiple-value-bind (parsed position)
                       (read-from-string trimmed nil nil)
                     (if (and parsed
                              (realp parsed)
                              (= position (length trimmed)))
                         (float parsed 1.0d0)
                         nil))
                 (error () nil))))
            (t nil))))
    (unless normalized
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be a real number." argument-name)))
    normalized))

(defun builtin-tool-argument (arguments &rest keys)
  "Looks up the first present argument among KEYS in ARGUMENTS."
  (dolist (key keys (values nil nil))
    (let ((entry (mcp-assoc key arguments)))
      (when entry
        (return (values t (cdr entry)))))))

(defun normalize-builtin-tool-boolean-argument (foundp value argument-name tool-name)
  "Normalizes VALUE to a boolean argument or signals an execution error."
  (unless foundp
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A is required." argument-name)))
  (let ((printed (string-downcase (princ-to-string value))))
    (cond
      ((or (eq value t)
           (eq value :true)
           (string= printed "t")
           (string= printed "true")
           (search "json-literal true" printed))
       t)
      ((or (null value)
           (eq value :false)
           (string= printed "nil")
           (string= printed "false")
           (search "json-literal false" printed))
       nil)
      ((symbolp value)
       (cond
         ((string-equal (symbol-name value) "true") t)
         ((string-equal (symbol-name value) "false") nil)
         (t
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "~A must be a boolean." argument-name)))))
      ((stringp value)
       (cond
         ((string-equal value "true") t)
         ((string-equal value "false") nil)
         (t
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "~A must be a boolean." argument-name)))))
      (t
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason (format nil "~A must be a boolean." argument-name))))))

(defun normalize-builtin-tool-string-sequence-argument (foundp value argument-name tool-name)
  "Normalizes VALUE to a list of strings or signals an execution error."
  (unless foundp
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A is required." argument-name)))
  (let ((elements (cond
                    ((vectorp value) (coerce value 'list))
                    ((listp value) value)
                    (t nil))))
    (unless elements
      (when (or (vectorp value) (listp value))
        (return-from normalize-builtin-tool-string-sequence-argument nil))
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be an array of strings." argument-name)))
    (unless (every #'stringp elements)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must contain only strings." argument-name)))
    elements))
