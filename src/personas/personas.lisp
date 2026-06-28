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

(defun resolve-persona-directory (persona-name-or-directory)
  "Returns the existing persona directory for PERSONA-NAME-OR-DIRECTORY."
  (or (and (pathnamep persona-name-or-directory)
           (uiop:directory-exists-p persona-name-or-directory))
      (and (stringp persona-name-or-directory)
           (uiop:directory-exists-p (pathname persona-name-or-directory)))
      (let* ((homedir (get-user-homedir-pathname))
             (name-str (string persona-name-or-directory)))
        (or (uiop:directory-exists-p
             (merge-pathnames (make-pathname :directory (list :relative ".Personas" name-str))
                              homedir))
            (uiop:directory-exists-p
             (merge-pathnames (make-pathname :directory (list :relative ".Personas"
                                                              (string-downcase name-str)))
                              homedir))
            (error "Persona directory not found: ~A"
                   (if (pathnamep persona-name-or-directory)
                       persona-name-or-directory
                       (format nil "~~/.Personas/~A" name-str)))))))

(defun persona-compressed-memory-path (persona-dir)
  "Returns the compressed-memory.txt pathname for PERSONA-DIR."
  (merge-pathnames "compressed-memory.txt" persona-dir))

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

(defun persona-memory-json-records (memory-json-path)
  "Returns MEMORY-JSON-PATH as normalized entity/relation records."
  (let* ((raw-text (uiop:read-file-string memory-json-path))
         (json-data (safe-parse-json raw-text)))
    (if (persona-memory-graph-json-p json-data)
        (list :entities (json-array-elements (mcp-val :entities json-data))
              :relations (json-array-elements (mcp-val :relations json-data)))
        (let ((entities nil)
              (relations nil))
          (dolist (line (cl-ppcre:split "\\r?\\n" raw-text))
            (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
              (unless (string= trimmed "")
                (let* ((record (parse-json-or-error trimmed :context "persona memory JSONL"))
                       (type (string-downcase
                              (or (mcp-val :type record)
                                  (mcp-val "type" record)
                                  ""))))
                  (cond
                    ((string= type "entity")
                     (push `((:name . ,(mcp-val :name record))
                             (:entity-type . ,(or (mcp-val :entity-type record)
                                                  (mcp-val "entityType" record)))
                             (:observations . ,(json-array-elements (mcp-val :observations record))))
                           entities))
                    ((string= type "relation")
                     (push `((:from . ,(mcp-val :from record))
                             (:to . ,(mcp-val :to record))
                             (:relation-type . ,(or (mcp-val :relation-type record)
                                                    (mcp-val "relationType" record))))
                           relations))
                    (t
                     (error "Unsupported persona memory record type ~S in ~A."
                            (or (mcp-val :type record)
                                (mcp-val "type" record))
                            memory-json-path)))))))
          (list :entities (nreverse entities)
                :relations (nreverse relations))))))

(defun persona-memory-entity-summary-line (entity)
  "Returns a concise one-line summary for ENTITY."
  (let* ((name (or (mcp-val :name entity) "Unnamed entity"))
         (entity-type (or (mcp-val :entity-type entity)
                          (mcp-val "entityType" entity)))
         (observations (remove ""
                               (mapcar #'princ-to-string
                                       (json-array-elements (mcp-val :observations entity)))
                               :test #'string=)))
    (format nil "- ~A~@[ (~A)~]~@[: ~{~A~^; ~}~]"
            name
            entity-type
            observations)))

(defun persona-memory-relation-summary-line (relation)
  "Returns a concise one-line summary for RELATION."
  (format nil "- ~A -~A-> ~A"
          (or (mcp-val :from relation) "Unknown")
          (or (mcp-val :relation-type relation)
              (mcp-val "relationType" relation)
              "related-to")
          (or (mcp-val :to relation) "Unknown")))

(defun persona-memory-records->compressed-text (records)
  "Returns a concise text summary for persona memory RECORDS."
  (let ((entities (safe-getf records :entities))
        (relations (safe-getf records :relations))
        (sections nil))
    (when entities
      (push (format nil "Entities:~%~{~A~^~%~}"
                    (mapcar #'persona-memory-entity-summary-line entities))
            sections))
    (when relations
      (push (format nil "Relations:~%~{~A~^~%~}"
                    (mapcar #'persona-memory-relation-summary-line relations))
            sections))
    (if sections
        (format nil "~{~A~^~%~%~}" (nreverse sections))
        "Knowledge graph is empty.")))

(defun save-compressed-persona-memory (persona-name-or-directory)
  "Writes compressed-memory.txt for PERSONA-NAME-OR-DIRECTORY from its memory.json graph."
  (let* ((persona-dir (resolve-persona-directory persona-name-or-directory))
         (memory-json-path (or (persona-memory-json-path persona-dir)
                               (error "Persona ~A does not contain memory.json."
                                      persona-name-or-directory)))
         (compressed-path (persona-compressed-memory-path persona-dir))
         (compressed-text (persona-memory-records->compressed-text
                           (persona-memory-json-records memory-json-path))))
    (with-open-file (stream compressed-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string compressed-text stream))
    (log-message :info "Saved compressed persona memory"
                 :context `(("memory.json" . ,(namestring memory-json-path))
                            ("compressed-memory.txt" . ,(namestring compressed-path))))
    compressed-path))

(defun persona-memory-compression-thread-name (persona-dir)
  "Returns the background thread name for PERSONA-DIR compression."
  (let ((directory-components (pathname-directory persona-dir)))
    (format nil "Persona-Memory-Compression-~A"
            (or (car (last directory-components))
                (namestring persona-dir)))))

(defun compress-pending-diary-files (persona-dir runtime-context)
  "Finds any files in Diary/ that do not have a matching file in CompressedDiary/, and compresses them using the LLM."
  (let* ((diary-dir (uiop:directory-exists-p (merge-pathnames "Diary/" persona-dir)))
         (comp-dir (merge-pathnames "CompressedDiary/" persona-dir)))
    (when diary-dir
      (ensure-directories-exist comp-dir)
      (dolist (f (uiop:directory-files diary-dir))
        (let* ((filename (file-namestring f))
               (dest-path (merge-pathnames filename comp-dir)))
          (unless (probe-file dest-path)
            (handler-case
                (let* ((content (uiop:read-file-string f))
                       (prompt (format nil "Please compress and summarize the following diary entry to make it extremely concise and dense while retaining all key factual information, thoughts, and memories: ~%~%~A" content))
                       (conv (new-chat :backend :gemini :runtime-context runtime-context))
                       (response (chat prompt :conversation conv)))
                  (with-open-file (stream dest-path :direction :output :if-exists :supersede :if-does-not-exist :create)
                    (write-string response stream))
                  (log-message :info "Successfully compressed diary file"
                               :context `(("input" . ,(namestring f))
                                          ("output" . ,(namestring dest-path)))))
              (error (e)
                (log-message :error "Failed to compress diary file"
                             :context `(("input" . ,(namestring f))
                                        ("error" . ,(princ-to-string e))))))))))))

(defun start-persona-memory-compression-thread (conversation persona-dir)
  "Starts background compressed-memory generation and diary compression for PERSONA-DIR."
  (let ((memory-json-path (persona-memory-json-path persona-dir)))
    (when memory-json-path
      (let ((runtime-context (chatbot-runtime-context (conversation-chatbot conversation))))
        (funcall *persona-memory-compression-thread-function*
                 (lambda ()
                   (call-with-runtime-context
                    runtime-context
                    (lambda ()
                      ;; 1. Compress memory.json
                      (handler-case
                          (save-compressed-persona-memory persona-dir)
                        (error (condition)
                          (log-message :error "Failed to save compressed persona memory"
                                       :context `(("path" . ,(namestring memory-json-path))
                                                  ("error" . ,(princ-to-string condition))))))
                      ;; 2. Compress pending diary files
                      (handler-case
                          (compress-pending-diary-files persona-dir runtime-context)
                        (error (condition)
                          (log-message :error "Failed to compress diary files"
                                       :context `(("path" . ,(namestring persona-dir))
                                                  ("error" . ,(princ-to-string condition)))))))))
                 (persona-memory-compression-thread-name persona-dir))))))

(defun persona-diary-files (persona-dir)
  "Returns a list of preferred diary file pathnames, preferring CompressedDiary/ over Diary/."
  (let* ((diary-dir (uiop:directory-exists-p (merge-pathnames "Diary/" persona-dir)))
         (comp-dir (uiop:directory-exists-p (merge-pathnames "CompressedDiary/" persona-dir)))
         (files (make-hash-table :test #'equal)))
    (when diary-dir
      (dolist (f (uiop:directory-files diary-dir))
        (setf (gethash (file-namestring f) files) f)))
    (when comp-dir
      (dolist (f (uiop:directory-files comp-dir))
        (setf (gethash (file-namestring f) files) f)))
    (let ((result nil))
      (maphash (lambda (k v) (declare (ignore k)) (push v result)) files)
      result)))

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
  (let ((files (stable-sort (persona-diary-files persona-dir) #'diary-file<)))
    (when files
      (log-message :info "Loading persona diary preload"
                   :context `(("persona" . ,(namestring persona-dir))
                              ("count" . ,(princ-to-string (length files)))))
      (mapcar (lambda (path)
                `((:filename . ,(file-namestring path))
                  (:content . ,(string-right-trim '(#\Space #\Tab #\Return #\Linefeed)
                                                  (uiop:read-file-string path)))))
              files))))

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
