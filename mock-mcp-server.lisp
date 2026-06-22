;;; -*- Lisp -*-
;;; mock-mcp-server.lisp - A native Common Lisp Mock MCP Server for integration tests

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-json :silent t)

(handler-case
    (loop for line = (read-line *standard-input* nil :eof)
          until (eq line :eof)
          do (handler-case
                 (let* ((json (cl-json:decode-json-from-string line))
                        (id (cdr (assoc :id json)))
                        (method (cdr (assoc :method json))))
                   (cond
                     ((string= method "initialize")
                      (format *standard-output* "~A~%"
                              (cl-json:encode-json-to-string
                               `((:jsonrpc . "2.0")
                                 (:id . ,id)
                                 (:result . ((:protocol-version . "2024-11-05")
                                             (:capabilities . nil)
                                             (:server-info . ((:name . "mock-server")
                                                              (:version . "1.0.0"))))))))
                      (force-output *standard-output*))
                     ((string= method "tools/list")
                      (format *standard-output* "~A~%"
                              (cl-json:encode-json-to-string
                               `((:jsonrpc . "2.0")
                                 (:id . ,id)
                                 (:result . ((:tools . (((:name . "echo_tool")
                                                         (:description . "Echoes input")
                                                         (:input-schema . ((:type . "object")
                                                                           (:properties . ((:input . ((:type . "string")))))))))))))))
                      (force-output *standard-output*))
                     ((string= method "tools/call")
                      (let* ((params (cdr (assoc :params json)))
                             (arguments (cdr (assoc :arguments params)))
                             (input-val (cdr (assoc :input arguments))))
                        (format *standard-output* "~A~%"
                                (cl-json:encode-json-to-string
                                 `((:jsonrpc . "2.0")
                                   (:id . ,id)
                                   (:result . ((:content . (((:type . "text")
                                                             (:text . ,(format nil "Echo: ~A" input-val)))))))))))
                        (force-output *standard-output*))))
               (error (e)
                 (format *error-output* "Error processing line: ~A~%" e)
                 (force-output *error-output*))))
  (error (e)
    (format *error-output* "Fatal mock server error: ~A~%" e)
    (force-output *error-output*)))
