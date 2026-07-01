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

(defun execute-chatbot-tool (bot source tool-name arguments)
  "Executes SOURCE as either a built-in or MCP tool for BOT."
  (if (eq source :built-in)
      (default-execute-builtin-chatbot-tool bot tool-name arguments)
      (execute-mcp-tool source tool-name arguments)))
