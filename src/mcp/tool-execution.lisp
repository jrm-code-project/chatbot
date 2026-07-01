;;; -*- Lisp -*-
;;; tool-execution.lisp - MCP and built-in chatbot tool execution

(in-package "CHATBOT")

(defun execute-chatbot-tool-by-name (bot tool-name arguments)
  "Finds TOOL-NAME for BOT and executes it with ARGUMENTS."
  (multiple-value-bind (source tool) (find-chatbot-tool bot tool-name)
    (unless source
      (error "Tool not found: ~A" tool-name))
    (execute-chatbot-tool bot
                          source
                          tool-name
                          (normalize-chatbot-tool-arguments source tool arguments))))

(defun execute-chatbot-tool-by-name-json-arguments (bot tool-name arguments-json context)
  "Parses ARGUMENTS-JSON for TOOL-NAME in CONTEXT and executes the tool for BOT."
  (execute-chatbot-tool-by-name
   bot
   tool-name
   (if (or (null arguments-json)
           (string= (string-trim '(#\Space #\Tab #\Return #\Linefeed) arguments-json) ""))
       (empty-json-object)
       (parse-json-or-error arguments-json :context context))))

(defun chatbot-tool-error-message (condition)
  "Returns the most useful human-readable message for CONDITION."
  (if (typep condition 'mcp-tool-execution-error)
      (mcp-tool-execution-error-reason condition)
      (princ-to-string condition)))

(defun chatbot-tool-error-payload (tool-name condition)
  "Returns a JSON-serializable payload describing a tool execution failure."
  `(("type" . "tool_error")
    ("toolName" . ,tool-name)
    ("message" . ,(chatbot-tool-error-message condition))))

(defun chatbot-tool-error-text (tool-name condition)
  "Returns a JSON string describing a tool execution failure for LLM-visible text fields."
  (cl-json:encode-json-to-string (chatbot-tool-error-payload tool-name condition)))

(defun map-chatbot-json-tool-call-results (bot tool-calls context-builder result-builder
                                               &key error-builder)
  "Executes JSON-argument TOOL-CALLS for BOT and returns builder outputs in order.

When ERROR-BUILDER is provided, tool execution errors are converted into result
entries instead of aborting the full turn. If ERROR-BUILDER is NIL, errors are sandboxed."
  (mapcar (lambda (tool-call)
            (let* ((id (cdr (assoc :id tool-call)))
                   (name (cdr (assoc :name tool-call)))
                   (arguments-json (coerce (cdr (assoc :arguments tool-call)) 'string)))
              (handler-case
                  (let ((res-text (execute-chatbot-tool-by-name-json-arguments
                                   bot
                                   name
                                   arguments-json
                                   (funcall context-builder name tool-call))))
                    (funcall result-builder id name arguments-json res-text tool-call))
                (error (condition)
                  (if (or (typep condition 'agentic-loop-approval-required)
                          (typep condition 'agentic-loop-interrupted))
                      (error condition)
                      (if error-builder
                          (funcall error-builder id name arguments-json condition tool-call)
                          (funcall result-builder id name arguments-json
                                   (chatbot-tool-error-text name condition)
                                   tool-call)))))))
          tool-calls))

(define-builtin-tool "promptSubordinate" (bot arguments)
  (execute-prompt-subordinate-tool bot arguments tool-name))

(define-builtin-tool "spawnMinion" (bot arguments)
  (execute-spawn-minion-tool bot arguments tool-name))

(define-builtin-tool "listMinions" (bot arguments)
  (declare (ignore arguments))
  (execute-list-minions-tool bot))

(define-builtin-tool "dismissMinion" (bot arguments)
  (execute-dismiss-minion-tool bot arguments tool-name))

(define-builtin-tool "webSearch" (bot arguments)
     (execute-web-search-tool bot arguments tool-name))

(define-builtin-tool "hyperspecSearch" (bot arguments)
     (execute-hyperspec-search-tool bot arguments tool-name))

(define-builtin-tool "gitCall" (bot arguments)
  (execute-git-call-tool bot arguments tool-name))

(define-builtin-tool "eval" (bot arguments)
     (execute-eval-tool bot arguments tool-name))

(define-builtin-tool "readSamplingParameters" (bot arguments)
  (declare (ignore arguments))
  (execute-read-sampling-parameters-tool bot))

(define-builtin-tool "startAgenticLoop" (bot arguments)
  (execute-start-agentic-loop-tool bot arguments tool-name))

(define-builtin-tool "listAgenticLoops" (bot arguments)
  (declare (ignore bot arguments))
  (execute-list-agentic-loops-tool))

(define-builtin-tool "readAgenticLoop" (bot arguments)
  (declare (ignore bot))
  (execute-read-agentic-loop-tool arguments tool-name))

(define-builtin-tool "abortAgenticLoop" (bot arguments)
  (declare (ignore bot))
  (execute-abort-agentic-loop-tool arguments tool-name))

(define-builtin-tool "resumeAgenticLoop" (bot arguments)
  (declare (ignore bot))
  (execute-resume-agentic-loop-tool arguments tool-name))

(define-builtin-tool "setSamplingParameters" (bot arguments)
     (execute-set-sampling-parameters-tool bot arguments tool-name))

(define-builtin-tool "resetSamplingParameters" (bot arguments)
     (declare (ignore arguments))
     (execute-reset-sampling-parameters-tool bot))

(define-builtin-tool "readFileLines" (bot arguments)
  (execute-read-file-lines-tool bot arguments tool-name))

(define-builtin-tool "readSystemInstructions" (bot arguments)
  (declare (ignore arguments))
  (execute-read-system-instructions-tool bot))

(define-builtin-tool "insertSystemInstructionParagraph" (bot arguments)
  (execute-insert-system-instruction-paragraph-tool bot arguments tool-name))

(define-builtin-tool "updateSystemInstructionParagraph" (bot arguments)
  (execute-update-system-instruction-paragraph-tool bot arguments tool-name))

(define-builtin-tool "deleteSystemInstructionParagraph" (bot arguments)
  (execute-delete-system-instruction-paragraph-tool bot arguments tool-name))

(define-builtin-tool "replaceSystemInstructions" (bot arguments)
  (execute-replace-system-instructions-tool bot arguments tool-name))

(define-builtin-tool "directory" (bot arguments)
  (execute-directory-tool bot arguments tool-name))

(define-builtin-tool "writeFile" (bot arguments)
  (execute-write-file-tool bot arguments tool-name))

(define-builtin-tool "deleteFile" (bot arguments)
  (execute-delete-file-tool bot arguments tool-name))

(define-builtin-tool "submitPlan" (bot arguments)
  (execute-submit-plan-tool bot arguments tool-name))

(define-builtin-tool "abortPlan" (bot arguments)
  (execute-abort-plan-tool bot arguments))

(define-builtin-tool "invokePlanner" (bot arguments)
  (execute-invoke-planner-tool bot arguments tool-name))

(defun execute-chatbot-tool (bot source tool-name arguments)
  "Executes SOURCE as either a built-in or MCP tool for BOT."
  (if (eq source :built-in)
      (default-execute-builtin-chatbot-tool bot tool-name arguments)
      (execute-mcp-tool source tool-name arguments)))
