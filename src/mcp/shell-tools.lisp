;;; -*- Lisp -*-
;;; shell-tools.lisp - built-in chatbot shell tool helpers

(in-package "CHATBOT")

(defun execute-shell-tool (bot arguments tool-name)
  "Runs the built-in shell tool."
  (unless (chatbot-enable-shell-p bot)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Shell tool is not enabled."))
  (let* ((command (normalize-builtin-tool-string-argument
                   (or (mcp-val "command" arguments)
                       (mcp-val :command arguments))
                   "command"
                   tool-name))
         (dir (or (chatbot-scoped-directory bot)
                  (namestring (uiop:getcwd)))))
    (unless (funcall *shell-approval-function* bot command tool-name)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "Shell command execution denied by user."))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :directory dir
                          :force-shell t
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (format nil (concatenate 'string
                               "~&[Shell Executed]~%"
                               "Command: ~A~%"
                               "Directory: ~A~%"
                               "Exit Code: ~D~@[~%"
                               "STDOUT:~%"
                               "~A~]~@[~%"
                               "STDERR:~%"
                               "~A~]")
              command (namestring dir) exit-code
              (and (string/= stdout "") stdout)
              (and (string/= stderr "") stderr)))))
