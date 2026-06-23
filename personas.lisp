;;; -*- Lisp -*-
;;; personas.lisp - persona file loading and preload helpers

(in-package "CHATBOT")

(defun get-user-homedir-pathname ()
  "Wrapper around user-homedir-pathname to allow package-lock-safe testing/mocking."
  (funcall *user-homedir-pathname-function*))

(defun persona-preload-memory-text (persona-dir)
  "Returns persona memory text from compressed-memory.txt or memory.json."
  (let ((compressed-path (probe-file (merge-pathnames "compressed-memory.txt" persona-dir)))
        (json-path (probe-file (merge-pathnames "memory.json" persona-dir))))
    (cond
      (compressed-path
       (log-message :info "Loading persona memory preload"
                    :context `(("source" . "compressed-memory.txt")
                               ("path" . ,(namestring compressed-path))))
       (string-right-trim '(#\Space #\Tab #\Return #\Linefeed)
                          (uiop:read-file-string compressed-path)))
      (json-path
       (log-message :info "Loading persona memory preload"
                    :context `(("source" . "memory.json")
                               ("path" . ,(namestring json-path))))
       (uiop:read-file-string json-path))
      (t nil))))

(defun persona-memory-json-path (persona-dir)
  "Returns the persona memory.json pathname when present."
  (probe-file (merge-pathnames "memory.json" persona-dir)))

(defun json-array-elements (value)
  "Returns VALUE as a proper list when it represents a JSON array."
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun persona-memory-graph-json-p (json-data)
  "Returns true when JSON-DATA looks like a knowledge-graph JSON object."
  (and (listp json-data)
       (or (mcp-assoc :entities json-data)
           (mcp-assoc :relations json-data))))

(defun persona-memory-graph-json->jsonl (graph)
  "Converts a knowledge-graph GRAPH object to server-memory JSONL storage."
  (let ((lines nil))
    (dolist (entity (json-array-elements (mcp-val :entities graph)))
      (push (cl-json:encode-json-to-string
             `(("type" . "entity")
               ("name" . ,(mcp-val :name entity))
               ("entityType" . ,(or (mcp-val :entity-type entity)
                                    (mcp-val "entityType" entity)))
               ("observations" . ,(json-array-elements (mcp-val :observations entity)))))
            lines))
    (dolist (relation (json-array-elements (mcp-val :relations graph)))
      (push (cl-json:encode-json-to-string
             `(("type" . "relation")
               ("from" . ,(mcp-val :from relation))
               ("to" . ,(mcp-val :to relation))
               ("relationType" . ,(or (mcp-val :relation-type relation)
                                      (mcp-val "relationType" relation)))))
            lines))
    (format nil "~{~A~^~%~}" (nreverse lines))))

(defun ensure-persona-memory-server-storage-format (memory-json-path)
  "Migrates MEMORY-JSON-PATH in place when it is still in graph-object JSON format."
  (let* ((raw-text (uiop:read-file-string memory-json-path))
         (json-data (safe-parse-json raw-text)))
    (when (persona-memory-graph-json-p json-data)
      (log-message :info "Migrating persona memory for memory MCP server"
                   :context `(("path" . ,(namestring memory-json-path))))
      (with-open-file (stream memory-json-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string (persona-memory-graph-json->jsonl json-data) stream)))))

(defun attach-persona-memory-mcp-server (conversation persona-dir)
  "Attaches a persona-scoped memory MCP server when PERSONA-DIR contains memory.json."
  (let ((memory-json-path (persona-memory-json-path persona-dir)))
    (when memory-json-path
      (let* ((bot (conversation-chatbot conversation))
             (runtime-context (chatbot-runtime-context bot)))
        (call-with-runtime-context
         runtime-context
         (lambda ()
           (ensure-persona-memory-server-storage-format memory-json-path)
           (log-message :info "Attaching persona memory MCP server"
                        :context `(("path" . ,(namestring memory-json-path))))
           (let* ((persona-memory-server
                    (initialize-configured-mcp-server
                     "memory"
                     :environment `(("MEMORY_FILE_PATH" . ,(namestring memory-json-path)))))
                  (shared-servers
                    (remove-if (lambda (server)
                                 (and (typep server 'mcp-server)
                                      (string= "memory" (mcp-server-name server))))
                               (chatbot-mcp-servers bot))))
             (setf (chatbot-mcp-servers bot)
                   (append shared-servers (list persona-memory-server))))))))
    conversation))

(defun preload-persona-conversation-memory (conversation persona-dir)
  "Stores persona preload memory separately from ordinary conversation turns."
  (let ((memory-text (persona-preload-memory-text persona-dir)))
    (when memory-text
      (setf (conversation-persona-memory conversation) memory-text))
    conversation))
