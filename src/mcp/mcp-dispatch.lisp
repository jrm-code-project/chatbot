;;; -*- Lisp -*-
;;; mcp-dispatch.lisp - MCP server lookup and tool execution helpers

(in-package "CHATBOT")

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
