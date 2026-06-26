;;; -*- Lisp -*-
;;; tool-registry.lisp - MCP tool discovery, translation, and argument normalization

(in-package "CHATBOT")

(defun translate-mcp-tool-to-openai (mcp-tool)
  "Translates MCP tool definition to OpenAI function tool format."
  (let ((name (mcp-val :name mcp-tool))
        (description (mcp-val :description mcp-tool))
        (input-schema (mcp-val :input-schema mcp-tool)))
    `(("type" . "function")
      ("function" . (("name" . ,name)
                     ("description" . ,(or description ""))
                     ("parameters" . ,(gemini-tool-parameters input-schema)))))))

(defun translate-mcp-tool-to-gemini-fn (mcp-tool)
  "Translates MCP tool definition to Gemini function format."
  (let ((name (mcp-val :name mcp-tool))
        (description (mcp-val :description mcp-tool))
        (input-schema (mcp-val :input-schema mcp-tool)))
    `(("name" . ,name)
      ("description" . ,(or description ""))
      ("parameters" . ,(gemini-tool-parameters input-schema)))))

(defun default-get-all-mcp-tools (bot)
  "Retrieves all tools from all connected MCP servers as a list of (server . tool-plist)."
  (let ((all-tools nil))
    (dolist (server (chatbot-mcp-servers bot))
      (handler-case
          (let* ((response (mcp-list-tools server))
                 (tools (mcp-val :tools response)))
            (dolist (tool tools)
              (push (cons server tool) all-tools)))
        (error (e)
          (format *error-output* "Error listing tools from MCP server ~A: ~A~%" (mcp-server-name server) e))))
    (nreverse all-tools)))

(defun get-all-mcp-tools (bot)
  "Retrieves all MCP tools, honoring the configured test seam when present."
  (if *get-all-mcp-tools-function*
      (funcall *get-all-mcp-tools-function* bot)
      (default-get-all-mcp-tools bot)))

(defun get-all-chatbot-tools (bot)
  "Returns all built-in and MCP tools available to BOT."
  (append (default-get-all-builtin-tools bot)
          (get-all-mcp-tools bot)))

(defun find-chatbot-tool (bot tool-name)
  "Finds a built-in or MCP tool by TOOL-NAME."
  (multiple-value-bind (source tool) (default-find-builtin-tool bot tool-name)
    (if source
        (values source tool)
        (find-mcp-server-and-tool bot tool-name))))

(defun canonical-json-key-id (key)
  "Returns a comparison identifier for a JSON object KEY."
  (remove-if (lambda (char)
               (or (char= char #\-)
                   (char= char #\_)))
             (json-key-name key)))

(defun schema-field-value (schema key)
  "Returns KEY from SCHEMA, supporting alists and hash tables."
  (cond
    ((hash-table-p schema)
     (or (gethash (json-key-string key) schema)
         (gethash (json-key-name key) schema)))
    ((listp schema)
     (mcp-val key schema))
    (t nil)))

(defun schema-property-entry (properties key)
  "Returns the matching (key . schema) property entry for KEY from PROPERTIES."
  (let ((target-id (canonical-json-key-id key)))
    (cond
      ((hash-table-p properties)
       (let ((found nil))
         (maphash (lambda (property-key property-schema)
                    (when (and (null found)
                               (string= target-id (canonical-json-key-id property-key)))
                      (setf found (cons property-key property-schema))))
                  properties)
         found))
      ((listp properties)
       (find-if (lambda (entry)
                  (string= target-id (canonical-json-key-id (car entry))))
                properties))
      (t nil))))

(defun schema-object-entries (value)
  "Returns VALUE as an object entry list when it represents a JSON object."
  (cond
    ((hash-table-p value)
     (let ((entries nil))
       (maphash (lambda (key nested-value)
                  (push (cons key nested-value) entries))
                value)
       (nreverse entries)))
    ((json-object-alist-p value) value)
    (t nil)))

(defun normalize-arguments-to-schema (value schema)
  "Normalizes VALUE to use the property spelling and nested shape declared by SCHEMA."
  (let* ((type (schema-field-value schema :type))
         (type-name (and type (string-downcase (princ-to-string type)))))
    (cond
      ((and type-name (string= type-name "object"))
       (let ((entries (schema-object-entries value))
             (properties (schema-field-value schema :properties)))
         (if entries
             (mapcar (lambda (entry)
                       (let* ((property-entry (schema-property-entry properties (car entry)))
                              (normalized-key (if property-entry (car property-entry) (car entry)))
                              (property-schema (and property-entry (cdr property-entry))))
                         (cons normalized-key
                               (if property-schema
                                   (normalize-arguments-to-schema (cdr entry) property-schema)
                                   (cdr entry)))))
                     entries)
             value)))
      ((and type-name (string= type-name "array"))
       (let ((item-schema (schema-field-value schema :items)))
         (cond
           ((vectorp value)
            (map 'vector (lambda (item)
                           (if item-schema
                               (normalize-arguments-to-schema item item-schema)
                               item))
                 value))
           ((listp value)
            (mapcar (lambda (item)
                      (if item-schema
                          (normalize-arguments-to-schema item item-schema)
                          item))
                    value))
           (t value))))
      (t value))))

(defun normalize-chatbot-tool-arguments (source tool arguments)
  "Normalizes ARGUMENTS for TOOL before execution when needed."
  (if (eq source :built-in)
      arguments
      (let ((input-schema (mcp-val :input-schema tool)))
        (if input-schema
            (normalize-arguments-to-schema arguments input-schema)
            arguments))))
