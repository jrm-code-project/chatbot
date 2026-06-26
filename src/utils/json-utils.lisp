;;; -*- Lisp -*-
;;; json-utils.lisp - JSON encoding, schema, and plist helpers

(in-package "CHATBOT")

(defmethod cl-json:encode-json ((object (eql t)) &optional (stream *standard-output*))
  (write-string "true" stream)
  nil)

(defmethod cl-json:encode-json ((object (eql :false)) &optional (stream *standard-output*))
  (write-string "false" stream)
  nil)

(define-condition malformed-json-error (error)
  ((context :initarg :context :reader malformed-json-error-context)
   (payload :initarg :payload :reader malformed-json-error-payload)
   (reason :initarg :reason :reader malformed-json-error-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A JSON payload: ~A~%Payload: ~A"
                     (malformed-json-error-context condition)
                     (malformed-json-error-reason condition)
                     (malformed-json-error-payload condition)))))

(defun empty-json-object ()
  "Returns an empty JSON object value compatible with cl-json."
  (make-hash-table :test 'equal))

(defun json-key-name (key)
  "Normalizes a JSON object key to a lowercase string for comparisons."
  (string-downcase
   (typecase key
     (string key)
     (symbol (symbol-name key))
     (t (princ-to-string key)))))

(defun json-key-string (key)
  "Converts a Lisp key to the JSON field name to emit."
  (typecase key
    (string key)
    (symbol
     (let* ((groups (cl-ppcre:split "--" (string-downcase (symbol-name key))))
            (normalized-groups
              (mapcar (lambda (group)
                        (let ((parts (cl-ppcre:split "-" group)))
                          (if (null parts)
                              ""
                              (with-output-to-string (s)
                                (write-string (or (car parts) "") s)
                                (dolist (part (cdr parts))
                                  (when (> (length part) 0)
                                    (write-string (string-capitalize part) s)))))))
                      groups)))
       (format nil "~{~A~^_~}" normalized-groups)))
    (t (princ-to-string key))))

(defun json-object-alist-p (value)
  "Returns true when VALUE is an alist-style JSON object."
  (and (listp value)
       (every (lambda (entry)
                (and (consp entry)
                     (not (consp (car entry)))
                     (or (symbolp (car entry))
                         (stringp (car entry)))))
              value)))

(defun json-encodable-value (value)
  "Converts VALUE into a shape that cl-json will emit with JSON object semantics."
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((vectorp value) (map 'vector #'json-encodable-value value))
    ((hash-table-p value)
     (let ((result (make-hash-table :test 'equal)))
       (maphash (lambda (key nested-value)
                  (setf (gethash (json-key-string key) result)
                        (json-encodable-value nested-value)))
                value)
       result))
    ((json-object-alist-p value)
     (let ((result (make-hash-table :test 'equal)))
       (dolist (entry value result)
         (setf (gethash (json-key-string (car entry)) result)
               (json-encodable-value (cdr entry))))))
    ((listp value)
     (mapcar #'json-encodable-value value))
    (t value)))

(defun sanitize-gemini-schema (schema)
  "Converts tool schemas into Gemini-compatible JSON objects."
  (labels ((object-value (key-name value)
             (if (and (string= key-name "properties")
                      (null value))
                 (empty-json-object)
                 (sanitize-gemini-schema value)))
           (copy-object-entries (result entries)
             (dolist (entry entries result)
               (let* ((key (car entry))
                      (value (cdr entry))
                      (key-name (json-key-name key)))
                 (unless (string= key-name "$schema")
                   (when (or (not (null value))
                             (string= key-name "properties"))
                     (setf (gethash (json-key-string key) result)
                           (object-value key-name value))))))))
    (cond
      ((stringp schema)
       schema)
      ((vectorp schema)
       (map 'vector #'sanitize-gemini-schema schema))
      ((hash-table-p schema)
       (let ((result (make-hash-table :test 'equal))
             (entries nil))
         (maphash (lambda (key value)
                    (push (cons key value) entries))
                  schema)
         (copy-object-entries result (nreverse entries))))
      ((json-object-alist-p schema)
       (copy-object-entries (make-hash-table :test 'equal) schema))
      ((listp schema)
       (remove nil (mapcar #'sanitize-gemini-schema schema)))
      (t schema))))

(defun gemini-tool-parameters (input-schema)
  "Returns a Gemini-compatible function parameter schema."
  (let ((schema (if input-schema
                    (sanitize-gemini-schema input-schema)
                    '((:type . "object")))))
    (cond
      ((hash-table-p schema)
       (let ((type-name (gethash "type" schema))
             (properties (gethash "properties" schema)))
         (when (and (stringp type-name)
                    (string= (string-downcase type-name) "object")
                    (null properties))
           (setf (gethash "properties" schema) (empty-json-object)))
         schema))
      ((json-object-alist-p schema)
       (sanitize-gemini-schema schema))
      (t
       (let ((fallback (make-hash-table :test 'equal)))
         (setf (gethash "type" fallback) "object")
         (setf (gethash "properties" fallback) (empty-json-object))
         fallback)))))

(defun safe-getf (plist indicator &optional default)
  "Safely retrieves the value associated with indicator in plist,
even if the plist is malformed."
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

(defun safe-parse-json (line)
  "Parses LINE as JSON, returning NIL on error."
  (handler-case
      (cl-json:decode-json-from-string line)
    (error () nil)))

(defun parse-json-or-error (line &key (context "JSON"))
  "Parses LINE as JSON or signals MALFORMED-JSON-ERROR with CONTEXT."
  (handler-case
      (cl-json:decode-json-from-string line)
    (error (e)
      (error 'malformed-json-error
             :context context
             :payload line
             :reason (princ-to-string e)))))
