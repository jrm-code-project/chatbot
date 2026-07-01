;;; -*- Lisp -*-
;;; git-tools.lisp - built-in chatbot git tool helpers

(in-package "CHATBOT")

(defun git-call-arguments (arguments)
  "Returns the git CLI arguments extracted from ARGUMENTS as strings."
  (let ((args-list (or (mcp-val "args" arguments)
                       (mcp-val :args arguments))))
    (loop for arg in args-list
          collect (typecase arg
                    (string arg)
                    (t (format nil "~A" arg))))))

(defun execute-git-call-tool (bot arguments tool-name)
  "Runs the built-in gitCall tool."
  (unless (chatbot-enable-git-tools-p bot)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Git tool is not enabled."))
  (let* ((args (git-call-arguments arguments))
         (dir (or (chatbot-scoped-directory bot)
                  (namestring (uiop:getcwd)))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (cons "git" args)
                          :directory dir
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (format nil (concatenate 'string
                               "~&[Git Executed]~%"
                               "Command: git ~{~A ~}~%"
                               "Directory: ~A~%"
                               "Exit Code: ~D~@[~%"
                               "STDOUT:~%"
                               "~A~]~@[~%"
                               "STDERR:~%"
                               "~A~]")
              args dir exit-code
              (and (string/= stdout "") stdout)
              (and (string/= stderr "") stderr)))))
