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

(defvar *builtin-tools* (make-hash-table :test 'equal)
  "Registry of built-in chatbot tools, mapping tool-name strings to handler functions.")

(defmacro define-builtin-tool (tool-name (bot-var arguments-var) &body body)
  "Defines a handler for a built-in tool and registers it in *builtin-tools*.
The handler takes BOT and ARGUMENTS. TOOL-NAME is implicitly bound lexically for use in errors."
  `(setf (gethash ,tool-name *builtin-tools*)
         (lambda (,bot-var tool-name ,arguments-var)
           (declare (ignorable ,bot-var tool-name ,arguments-var))
           ,@body)))

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
  (let* ((filename (mcp-val :filename arguments))
            (beginning-line (normalize-builtin-tool-integer-argument
                             (or (mcp-val "beginningLine" arguments)
                                 (mcp-val :beginning-line arguments))
                             "beginningLine"
                             tool-name))
            (ending-line (normalize-builtin-tool-integer-argument
                          (or (mcp-val "endingLine" arguments)
                              (mcp-val :ending-line arguments))
                          "endingLine"
                          tool-name))
            (path (resolve-filesystem-tool-path bot filename tool-name)))
       (read-file-lines-subset path beginning-line ending-line tool-name)))

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
  (multiple-value-bind (directory-path root)
         (resolve-filesystem-tool-directory bot
                                            (or (mcp-val "pathname" arguments)
                                                (mcp-val :pathname arguments))
                                            tool-name)
       (directory-tool-result directory-path
                              root
                              (or (mcp-val "pattern" arguments)
                                  (mcp-val :pattern arguments))
                              tool-name)))

(define-builtin-tool "writeFile" (bot arguments)
  (multiple-value-bind (pathname-foundp pathname)
         (builtin-tool-argument arguments "pathname" :pathname)
       (declare (ignore pathname-foundp))
       (multiple-value-bind (use-lf-only-foundp use-lf-only-value)
           (builtin-tool-argument arguments "useLfOnly" :use-lf-only)
         (multiple-value-bind (end-with-eol-foundp end-with-eol-value)
             (builtin-tool-argument arguments "endWithEol" :end-with-eol)
           (multiple-value-bind (lines-foundp lines-value)
               (builtin-tool-argument arguments "lines" :lines)
             (multiple-value-bind (target-path root)
                 (resolve-filesystem-tool-target-path bot
                                                      (normalize-builtin-tool-string-argument pathname "pathname" tool-name)
                                                      tool-name)
               (write-file-tool-result target-path
                                       root
                                       (normalize-builtin-tool-string-sequence-argument lines-foundp
                                                                                        lines-value
                                                                                        "lines"
                                                                                        tool-name)
                                       (normalize-builtin-tool-boolean-argument use-lf-only-foundp
                                                                                use-lf-only-value
                                                                                "useLfOnly"
                                                                                tool-name)
                                       (normalize-builtin-tool-boolean-argument end-with-eol-foundp
                                                                                end-with-eol-value
                                                                                "endWithEol"
                                                                                tool-name))))))))

(define-builtin-tool "deleteFile" (bot arguments)
  (multiple-value-bind (pathname-foundp pathname)
         (builtin-tool-argument arguments "pathname" :pathname)
       (unless pathname-foundp
         (error 'mcp-tool-execution-error
                :tool-name tool-name
                :reason "pathname is required."))
       (let* ((path (resolve-filesystem-tool-path bot pathname tool-name))
              (root (chatbot-filesystem-root-truename bot tool-name)))
         (delete-file-tool-result path root))))

(define-builtin-tool "submitPlan" (bot arguments)
  (execute-submit-plan-tool bot arguments tool-name))

(define-builtin-tool "abortPlan" (bot arguments)
  (execute-abort-plan-tool bot arguments))

(define-builtin-tool "invokePlanner" (bot arguments)
  (execute-invoke-planner-tool bot arguments tool-name))

(defun default-execute-builtin-chatbot-tool (bot tool-name arguments)
  "Executes a built-in tool for BOT using the *builtin-tools* registry."
  (let ((handler (gethash tool-name *builtin-tools*)))
    (if handler
        (funcall handler bot tool-name arguments)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason "Unknown built-in tool."))))

(defun execute-chatbot-tool (bot source tool-name arguments)
  "Executes SOURCE as either a built-in or MCP tool for BOT."
  (if (eq source :built-in)
      (default-execute-builtin-chatbot-tool bot tool-name arguments)
      (execute-mcp-tool source tool-name arguments)))

(defun default-find-mcp-server-and-tool (bot tool-name)
  "Find the connected MCP server and tool definition that matches tool-name."
  (dolist (server (chatbot-mcp-servers bot))
    (handler-case
        (let* ((response (mcp-list-tools server))
               (tools (mcp-val :tools response)))
          (dolist (tool tools)
            (let ((name (mcp-val :name tool)))
              (when (string= name tool-name)
                (return-from default-find-mcp-server-and-tool (values server tool))))))
      (error (e)
        (error 'mcp-tool-lookup-error
               :tool-name tool-name
               :server-name (mcp-server-name server)
               :reason (princ-to-string e)))))
  (values nil nil))

(defun find-mcp-server-and-tool (bot tool-name)
  "Finds an MCP tool by name, honoring the configured test seam when present."
  (if *find-mcp-server-and-tool-function*
      (funcall *find-mcp-server-and-tool-function* bot tool-name)
      (default-find-mcp-server-and-tool bot tool-name)))

(defun mcp-result-items (value)
  "Returns VALUE as a proper list when it represents a JSON array."
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun mcp-structured-content (response)
  "Returns structured tool result content when present on RESPONSE."
  (or (mcp-val "structuredContent" response)
      (mcp-val :structured-content response)
      (mcp-val :structured_content response)))

(defun mcp-jsonish-value->string (value)
  "Renders VALUE as a textual tool result."
  (typecase value
    (null "null")
    (string value)
    (t (cl-json:encode-json-to-string value))))

(defun mcp-tool-result-error-p (response)
  "Returns true when RESPONSE reports a tool-level error."
  (let ((flag (or (mcp-val "isError" response)
                  (mcp-val :is-error response)
                  (mcp-val :is_error response))))
    (not (null flag))))

(defun mcp-tool-result-fallback-payload (response)
  "Returns the most useful non-text payload available on RESPONSE, or NIL."
  (let ((structured-content (mcp-structured-content response))
        (content (mcp-val :content response)))
    (cond
      (structured-content structured-content)
      (content content)
      ((or (mcp-val "result" response)
           (mcp-val :result response))
       (or (mcp-val "result" response)
           (mcp-val :result response)))
      (response response)
      (t nil))))

(defun default-execute-mcp-tool (server tool-name arguments)
  "Calls the tool on the given MCP server and returns the result content string."
  (handler-case
      (let* ((response (mcp-call-tool server tool-name arguments))
             (content (mcp-result-items (mcp-val :content response)))
             (result-texts nil))
        (when (mcp-tool-result-error-p response)
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (mcp-jsonish-value->string response)))
        (dolist (item content)
          (let ((type (mcp-val :type item))
                (text (mcp-val :text item)))
            (when (and (string= type "text") text)
              (push text result-texts))))
        (if result-texts
            (format nil "~{~A~^~%~}" (nreverse result-texts))
            (let ((fallback (mcp-tool-result-fallback-payload response)))
              (if fallback
                  (mcp-jsonish-value->string fallback)
                  "Tool completed successfully."))))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (princ-to-string e)))))

(defun execute-mcp-tool (server tool-name arguments)
  "Executes an MCP tool, honoring the configured test seam when present."
  (if *execute-mcp-tool-function*
      (funcall *execute-mcp-tool-function* server tool-name arguments)
      (default-execute-mcp-tool server tool-name arguments)))
