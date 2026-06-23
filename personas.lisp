;;; -*- Lisp -*-
;;; personas.lisp - persona file loading and preload helpers

(in-package "CHATBOT")

(defun get-user-homedir-pathname ()
  "Wrapper around user-homedir-pathname to allow package-lock-safe testing/mocking."
  (funcall *user-homedir-pathname-function*))

(defparameter *persona-filesystem-allowlist-filename* "filesystem-allowlist.lisp"
  "Filename used to persist persona-approved filesystem directories.")

(defun persona-filesystem-allowlist-path (persona-dir)
  "Returns the persona filesystem allowlist pathname."
  (merge-pathnames *persona-filesystem-allowlist-filename* persona-dir))

(defun persona-filesystem-allowlist-directories (persona-dir)
  "Returns normalized approved filesystem directories loaded for PERSONA-DIR."
  (let ((allowlist-file (probe-file (persona-filesystem-allowlist-path persona-dir))))
    (when allowlist-file
      (with-open-file (stream allowlist-file :direction :input)
        (let* ((eof-marker (gensym "EOF"))
               (forms (loop for form = (read stream nil eof-marker)
                            until (eq form eof-marker)
                            collect form))
               (directories (cond
                              ((null forms) nil)
                              ((and (= 1 (length forms))
                                    (listp (car forms)))
                               (car forms))
                              (t forms))))
          (unless (every #'stringp directories)
            (error "Invalid filesystem allowlist in ~A: expected only directory strings." allowlist-file))
          (remove nil
                  (mapcar (lambda (entry)
                            (let ((resolved (probe-file (pathname entry))))
                              (when (and resolved
                                         (uiop:directory-exists-p resolved))
                                (uiop:ensure-directory-pathname resolved))))
                          directories)))))))

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

(defun persona-diary-directory (persona-dir)
  "Returns the preferred persona diary directory pathname when present."
  (or (uiop:directory-exists-p (merge-pathnames "CompressedDiary/" persona-dir))
      (uiop:directory-exists-p (merge-pathnames "Diary/" persona-dir))))

(defun diary-filename-leading-integer (pathname)
  "Returns the leading integer from PATHNAME's stem, or NIL when absent."
  (let* ((name (or (pathname-name pathname) ""))
         (digits (loop for char across name
                       while (digit-char-p char)
                       collect char)))
    (when digits
      (parse-integer (coerce digits 'string)))))

(defun diary-file-sort-name (pathname)
  "Returns a case-insensitive fallback sort name for PATHNAME."
  (string-downcase (or (pathname-name pathname)
                       (file-namestring pathname)
                       "")))

(defun diary-file< (left right)
  "Returns true when LEFT should sort before RIGHT for persona diary preload."
  (let ((left-number (diary-filename-leading-integer left))
        (right-number (diary-filename-leading-integer right)))
    (cond
      ((and left-number right-number)
       (if (/= left-number right-number)
           (< left-number right-number)
           (string-lessp (file-namestring left)
                         (file-namestring right))))
      (left-number t)
      (right-number nil)
      (t
       (let ((left-name (diary-file-sort-name left))
             (right-name (diary-file-sort-name right)))
         (if (string/= left-name right-name)
             (string-lessp left-name right-name)
             (string-lessp (file-namestring left)
                           (file-namestring right))))))))

(defun persona-diary-entries (persona-dir)
  "Returns ordered diary preload entries for PERSONA-DIR."
  (let ((diary-dir (persona-diary-directory persona-dir)))
    (when diary-dir
      (let ((files (stable-sort (copy-list (uiop:directory-files diary-dir))
                                #'diary-file<)))
        (when files
          (log-message :info "Loading persona diary preload"
                       :context `(("path" . ,(namestring diary-dir))
                                  ("count" . ,(princ-to-string (length files))))))
        (mapcar (lambda (path)
                  `((:filename . ,(file-namestring path))
                    (:content . ,(string-right-trim '(#\Space #\Tab #\Return #\Linefeed)
                                                    (uiop:read-file-string path)))))
                files)))))

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

(defun preload-persona-conversation-diary (conversation persona-dir)
  "Stores ordered persona diary preload entries separately from ordinary conversation turns."
  (let ((entries (persona-diary-entries persona-dir)))
    (when entries
      (setf (conversation-persona-diary-entries conversation) entries))
    conversation))
