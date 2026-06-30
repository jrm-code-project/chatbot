;;; -*- Lisp -*-
;;; mock-mcp-server.lisp - A native Common Lisp Mock MCP Server for integration tests

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-json :silent t)

(defparameter *max-sequential-errors* 5)
(defparameter *sequential-errors-count* 0)

(defparameter *method-handlers*
  `(("initialize" . ,(lambda (id json)
                       (declare (ignore json))
                       `((:jsonrpc . "2.0")
                         (:id . ,id)
                         (:result . ((:protocol-version . "2024-11-05")
                                     (:capabilities . nil)
                                     (:server-info . ((:name . "mock-server")
                                                      (:version . "1.0.0"))))))))
    ("tools/list" . ,(lambda (id json)
                       (declare (ignore json))
                       `((:jsonrpc . "2.0")
                         (:id . ,id)
                         (:result . ((:tools . (((:name . "echo_tool")
                                                 (:description . "Echoes input")
                                                 (:input-schema . ((:type . "object")
                                                                   (:properties . ((:input . ((:type . "string")))))))))))))))
    ("tools/call" . ,(lambda (id json)
                       (let* ((params (cdr (assoc :params json)))
                              (arguments (cdr (assoc :arguments params)))
                              (input-val (cdr (assoc :input arguments))))
                         `((:jsonrpc . "2.0")
                           (:id . ,id)
                           (:result . ((:content . (((:type . "text")
                                                     (:text . ,(format nil "Echo: ~A" input-val)))))))))))))

(defun sane-json-frame-p (line)
  "Structurally validates that the input line is non-empty and wrapped in curly braces."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
    (and (plusp (length trimmed))
         (char= (char trimmed 0) #\{)
         (char= (char trimmed (1- (length trimmed))) #\}))))

(handler-case
    (loop for line = (read-line *standard-input* nil :eof)
          until (eq line :eof)
          do (if (not (sane-json-frame-p line))
                 (progn
                   (incf *sequential-errors-count*)
                   (format *error-output* "Malformed JSON frame received. Error count: ~A/~A~%"
                           *sequential-errors-count* *max-sequential-errors*)
                   (force-output *error-output*)
                   (when (>= *sequential-errors-count* *max-sequential-errors*)
                     (format *error-output* "Circuit breaker tripped on frame structure. Exiting.~%")
                     (force-output *error-output*)
                     (uiop:quit 1)))
                 (handler-case
                     (let* ((json (cl-json:decode-json-from-string line))
                            (id (cdr (assoc :id json)))
                            (method (cdr (assoc :method json)))
                            (handler (cdr (assoc method *method-handlers* :test #'string=))))
                       (if handler
                           (let ((response-payload (funcall handler id json)))
                             (format *standard-output* "~A~%"
                                     (cl-json:encode-json-to-string response-payload))
                             (force-output *standard-output*)
                             (setf *sequential-errors-count* 0))
                           (progn
                             (format *error-output* "Unknown method: ~A~%" method)
                             (force-output *error-output*)
                             (setf *sequential-errors-count* 0))))
                   (error (e)
                     (incf *sequential-errors-count*)
                     (format *error-output* "Error processing JSON payload (~A): ~A~%" *sequential-errors-count* e)
                     (force-output *error-output*)
                     (when (>= *sequential-errors-count* *max-sequential-errors*)
                       (format *error-output* "Circuit breaker tripped under exception. Exiting.~%")
                       (force-output *error-output*)
                       (uiop:quit 1))))))
  (error (e)
    (format *error-output* "Fatal mock server error: ~A~%" e)
    (force-output *error-output*)))
