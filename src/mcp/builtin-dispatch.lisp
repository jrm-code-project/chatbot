;;; -*- Lisp -*-
;;; builtin-dispatch.lisp - built-in chatbot tool registry and dispatch

(in-package "CHATBOT")

(defvar *builtin-tools* (make-hash-table :test 'equal)
  "Registry of built-in chatbot tools, mapping tool-name strings to handler functions.")

(defmacro define-builtin-tool (tool-name (bot-var arguments-var) &body body)
  "Defines a handler for a built-in tool and registers it in *builtin-tools*.
The handler takes BOT and ARGUMENTS. TOOL-NAME is implicitly bound lexically for use in errors."
  `(setf (gethash ,tool-name *builtin-tools*)
         (lambda (,bot-var tool-name ,arguments-var)
           (declare (ignorable ,bot-var tool-name ,arguments-var))
           ,@body)))

(defun default-execute-builtin-chatbot-tool (bot tool-name arguments)
  "Executes a built-in tool for BOT using the *builtin-tools* registry."
  (let ((handler (gethash tool-name *builtin-tools*)))
    (if handler
        (funcall handler bot tool-name arguments)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason "Unknown built-in tool."))))
