;;; -*- Lisp -*-
;;; mcp.lisp - Model Context Protocol (MCP) client implementation

(in-package "CHATBOT")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defmethod cl-json:encode-json ((object (eql :null)) &optional (stream *standard-output*))
  (write-string "null" stream)
  nil)

(defmethod cl-json:encode-json ((object (eql :empty-object)) &optional (stream *standard-output*))
  (write-string "{}" stream)
  nil)

(defclass mcp-server ()
  ((name
    :initarg :name
    :accessor mcp-server-name
    :initform nil
    :documentation "The name of the MCP server.")
   (process
    :initarg :process
    :accessor mcp-server-process
    :initform nil
    :documentation "The UIOP process object for the subprocess.")
   (input-stream
    :initarg :input-stream
    :accessor mcp-server-input-stream
    :initform nil
    :documentation "Stream to write to the subprocess.")
   (output-stream
    :initarg :output-stream
    :accessor mcp-server-output-stream
    :initform nil
    :documentation "Stream to read from the subprocess.")
   (reader-thread
    :initarg :reader-thread
    :accessor mcp-server-reader-thread
    :initform nil
    :documentation "Thread running the JSON-RPC response parser.")
   (pending-requests
    :initarg :pending-requests
    :accessor mcp-server-pending-requests
    :initform (make-hash-table :test 'equal)
    :documentation "Hash table mapping request ID to sb-concurrency mailboxes.")
   (request-id-counter
    :initarg :request-id-counter
    :accessor mcp-server-request-id-counter
    :initform 0
    :documentation "Counter for unique request IDs.")
   (request-id-lock
    :initarg :request-id-lock
    :accessor mcp-server-request-id-lock
    :initform (sb-thread:make-mutex :name "mcp-request-id-lock")
    :documentation "Lock for request counter and pending requests table.")
   (tool-list-cache
    :initarg :tool-list-cache
    :accessor mcp-server-tool-list-cache
    :initform nil
    :documentation "Cached response from tools/list.")
   (tool-list-cache-valid-p
    :initarg :tool-list-cache-valid-p
    :accessor mcp-server-tool-list-cache-valid-p
    :initform nil
    :documentation "Whether the cached tools/list response is valid.")))

(define-condition mcp-tool-lookup-error (error)
  ((tool-name :initarg :tool-name :reader mcp-tool-lookup-error-tool-name)
   (server-name :initarg :server-name :reader mcp-tool-lookup-error-server-name)
   (reason :initarg :reason :reader mcp-tool-lookup-error-reason))
  (:report (lambda (condition stream)
            (format stream "Failed to resolve MCP tool ~A from server ~A: ~A"
                    (mcp-tool-lookup-error-tool-name condition)
                    (mcp-tool-lookup-error-server-name condition)
                    (mcp-tool-lookup-error-reason condition)))))

(define-condition mcp-tool-execution-error (error)
  ((tool-name :initarg :tool-name :reader mcp-tool-execution-error-tool-name)
   (reason :initarg :reason :reader mcp-tool-execution-error-reason))
  (:report (lambda (condition stream)
            (format stream "Failed to execute MCP tool ~A: ~A"
                    (mcp-tool-execution-error-tool-name condition)
                    (mcp-tool-execution-error-reason condition)))))

(defclass mcp-startup-entry ()
  ((name
    :initarg :name
    :accessor mcp-startup-entry-name
    :documentation "Configured MCP server name.")
   (command
    :initarg :command
    :accessor mcp-startup-entry-command
    :initform nil
    :documentation "Configured executable command for the server.")
   (args
    :initarg :args
    :accessor mcp-startup-entry-args
    :initform nil
    :documentation "Configured command-line arguments for the server.")
   (required-p
    :initarg :required-p
    :accessor mcp-startup-entry-required-p
    :initform nil
    :documentation "Whether this server is required in strict startup mode.")
   (success-p
    :initarg :success-p
    :accessor mcp-startup-entry-success-p
    :initform nil
    :documentation "Whether startup completed successfully.")
   (server
    :initarg :server
    :accessor mcp-startup-entry-server
    :initform nil
    :documentation "The initialized server instance when startup succeeded.")
   (error-message
    :initarg :error-message
    :accessor mcp-startup-entry-error-message
    :initform nil
    :documentation "Failure reason when startup did not succeed.")))

(defclass mcp-startup-status ()
  ((entries
    :initarg :entries
    :accessor mcp-startup-status-entries
    :initform nil
    :documentation "Per-server startup outcomes.")
   (strict-required-p
    :initarg :strict-required-p
    :accessor mcp-startup-status-strict-required-p
    :initform nil
    :documentation "Whether strict required-server failure handling was enabled.")
   (configured-count
    :initarg :configured-count
    :accessor mcp-startup-status-configured-count
    :initform 0
    :documentation "Number of configured server definitions considered.")
   (successful-count
    :initarg :successful-count
    :accessor mcp-startup-status-successful-count
    :initform 0
    :documentation "Number of successfully initialized servers.")
   (failed-count
    :initarg :failed-count
    :accessor mcp-startup-status-failed-count
    :initform 0
    :documentation "Number of failed server startups.")
   (required-failed-count
    :initarg :required-failed-count
    :accessor mcp-startup-status-required-failed-count
    :initform 0
    :documentation "Number of failed required server startups.")))

(define-condition mcp-startup-error (error)
  ((status :initarg :status :reader mcp-startup-error-status))
  (:report (lambda (condition stream)
             (let* ((status (mcp-startup-error-status condition))
                    (required-failures
                      (remove-if-not #'mcp-startup-entry-required-p
                                     (remove-if #'mcp-startup-entry-success-p
                                                (mcp-startup-status-entries status)))))
               (format stream
                       "Required MCP server startup failed (~D of ~D configured): ~{~A~^, ~}"
                       (mcp-startup-status-failed-count status)
                       (mcp-startup-status-configured-count status)
                       (mapcar #'mcp-startup-entry-name required-failures))))))

(defun get-mcp-config-paths ()
  "Returns a list of candidate paths to the MCP configuration file."
  (let ((configured-path (current-mcp-config-path)))
    (if configured-path
       (list configured-path)
      (let* ((config-home (uiop:xdg-config-home))
             (paths (list (merge-pathnames "mcp/mcp.lisp" config-home))))
        (when (uiop:os-windows-p)
          (push (merge-pathnames "mcp/mcp.lisp" (uiop:pathname-parent-directory-pathname config-home))
                paths))
        (nreverse paths)))))

(defun get-mcp-config-path ()
  "Returns the path to the MCP configuration file that exists, or the primary candidate."
  (let ((paths (get-mcp-config-paths)))
    (or (find-if #'probe-file paths)
        (car paths))))

(defun default-read-mcp-config ()
  "Reads and parses the MCP s-expression configuration file from candidate paths."
  (let ((paths (get-mcp-config-paths)))
    (dolist (path paths nil)
      (when (probe-file path)
        (handler-case
            (return-from default-read-mcp-config
              (with-open-file (stream path :direction :input)
                (read stream nil nil)))
          (error (e)
            (format *error-output* "Error reading MCP configuration from ~A: ~A~%" path e)))))))

(defun read-mcp-config ()
  "Reads MCP configuration, honoring the configured test seam when present."
  (if *read-mcp-config-function*
      (funcall *read-mcp-config-function*)
      (default-read-mcp-config)))

(defun mcp-next-id (server)
  "Thread-safe request ID generator."
  (sb-thread:with-mutex ((mcp-server-request-id-lock server))
    (incf (mcp-server-request-id-counter server))))

(defun mcp-register-request (server id mailbox)
  "Registers a pending request mailbox."
  (sb-thread:with-mutex ((mcp-server-request-id-lock server))
    (setf (gethash id (mcp-server-pending-requests server)) mailbox)))

(defun mcp-lookup-and-remove-request (server id)
  "Retrieves and removes a registered pending request mailbox."
  (sb-thread:with-mutex ((mcp-server-request-id-lock server))
    (let ((mailbox (gethash id (mcp-server-pending-requests server))))
      (remhash id (mcp-server-pending-requests server))
      mailbox)))

(defun mcp-assoc (key alist)
  "Look up key in alist, matching stringified key name (case-insensitive)."
  (let ((key-str (string-downcase (string key))))
    (assoc key-str alist
           :test (lambda (a b)
                   (string= (string-downcase (string a))
                            (string-downcase (string b)))))))

(defun mcp-val (key alist)
  "Retrieve value for key in alist using case-insensitive lookup."
  (cdr (mcp-assoc key alist)))

(defun invalidate-mcp-tool-list-cache (server)
  "Clears the cached tools/list response for SERVER."
  (setf (mcp-server-tool-list-cache server) nil)
  (setf (mcp-server-tool-list-cache-valid-p server) nil))

(defun mcp-tool-list-change-notification-p (method)
  "Returns true when METHOD indicates the server tool list changed."
  (and method
       (member (string-downcase method)
               '("notifications/tools/list_changed"
                 "tools/list_changed"
                 "notifications/tools/update-list"
                 "tools/update-list")
               :test #'string=)))

(defun mcp-handle-incoming (server json-data)
  "Handles an incoming JSON-RPC response from the server."
  (let* ((id (mcp-val :id json-data))
         (method (mcp-val :method json-data))
         (result (mcp-val :result json-data))
         (error-data (mcp-val :error json-data)))
    (when (mcp-tool-list-change-notification-p method)
      (format t "[MCP INFO] (~A) Tool list changed; invalidating cache.~%" (mcp-server-name server))
      (invalidate-mcp-tool-list-cache server))
    (when id
      (let ((mailbox (mcp-lookup-and-remove-request server id)))
        (if mailbox
            (sb-concurrency:send-message mailbox (or error-data result json-data))
            (format *error-output* "Warning: Received response for unregistered ID ~A on server ~A~%" id (mcp-server-name server)))))))

(defun mcp-reader-loop (server)
  "Asynchronous reader thread loop that parses server's standard output."
  (let ((stream (mcp-server-output-stream server)))
    (handler-case
        (loop for line = (read-line stream nil :eof)
              until (eq line :eof)
              do (let ((json-data (safe-parse-json line)))
                   (when json-data
                     (mcp-handle-incoming server json-data))))
      (error (e)
        (format *error-output* "MCP Reader thread error for server ~A: ~A~%" (mcp-server-name server) e)))))

(defun mcp-environment-entry->cons (entry)
  "Normalizes an environment ENTRY to a (KEY . VALUE) cons."
  (cond
    ((stringp entry)
     (let ((separator (position #\= entry)))
       (if separator
          (cons (subseq entry 0 separator)
                (subseq entry (1+ separator)))
          (cons entry ""))))
    ((consp entry)
     (cons (string (car entry))
          (princ-to-string (cdr entry))))
    (t
     (error "Invalid MCP environment entry: ~S" entry))))

(defun normalize-mcp-server-environment (environment)
  "Normalizes ENVIRONMENT entries to UIOP-compatible KEY=VALUE strings."
  (when environment
    (mapcar (lambda (entry)
             (let ((normalized-entry (mcp-environment-entry->cons entry)))
               (format nil "~A=~A"
                       (car normalized-entry)
                       (cdr normalized-entry))))
           environment)))

(defun merge-mcp-server-environments (&rest environments)
  "Merges ENVIRONMENTS, letting later entries override earlier ones by key."
  (let ((merged nil))
    (dolist (environment environments)
      (dolist (entry environment)
        (let* ((normalized-entry (mcp-environment-entry->cons entry))
              (key (car normalized-entry))
              (existing (assoc key merged
                               :test (lambda (left right)
                                       (string-equal (string left)
                                                     (string right))))))
         (if existing
             (setf (cdr existing) (cdr normalized-entry))
             (push normalized-entry merged)))))
    (nreverse merged)))

(defun current-process-environment ()
  "Returns the current process environment in a UIOP-compatible shape."
  (sb-ext:posix-environ))

(defun default-start-mcp-server (name command args &optional environment)
  "Launches an MCP server subprocess and starts its reader thread."
  (format t "[MCP INFO] Launching server ~A~%" name)
  (format t "[MCP DEBUG] Command: ~A ~A~%" command args)
  (let* ((launch-options (list :input :stream
                              :output :stream
                              :error-output :stream))
         (normalized-environment
           (normalize-mcp-server-environment
            (if environment
                (merge-mcp-server-environments (current-process-environment)
                                               environment)
                nil)))
         (process-info (apply #'uiop:launch-program
                             (cons (cons command args)
                                   (if normalized-environment
                                       (append launch-options
                                               (list :environment normalized-environment))
                                       launch-options))))
         (input (uiop:process-info-input process-info))
         (output (uiop:process-info-output process-info))
         (err-output (uiop:process-info-error-output process-info))
         (server (make-instance 'mcp-server
                                :name name
                                :process process-info
                                :input-stream input
                                :output-stream output)))
    ;; Spawn an error monitoring thread
    (sb-thread:make-thread
     (lambda ()
       (handler-case
           (loop for line = (read-line err-output nil :eof)
                 until (eq line :eof)
                 do (format t "[MCP ~A STDERR] ~A~%" name line))
         (error (e)
           (format t "[MCP DEBUG] ~A STDERR thread terminated: ~A~%" name e))))
     :name (concatenate 'string "mcp-err-" name))
    
    (setf (mcp-server-reader-thread server)
          (sb-thread:make-thread (lambda () (mcp-reader-loop server))
                                 :name (concatenate 'string "mcp-reader-" name)))
    (format t "[MCP INFO] Server ~A process and threads started successfully.~%" name)
    server))

(defun start-mcp-server (name command args &optional environment)
  "Launches an MCP server, honoring the configured test seam when present."
  (if *start-mcp-server-function*
      (if environment
          (funcall *start-mcp-server-function* name command args environment)
          (funcall *start-mcp-server-function* name command args))
      (default-start-mcp-server name command args environment)))

(defun default-stop-mcp-server (server)
  "Stops the MCP server process and reader thread cleanly."
  (format t "[MCP INFO] Stopping server ~A~%" (mcp-server-name server))
  (let ((thread (mcp-server-reader-thread server))
        (proc (mcp-server-process server)))
    (when (and thread (sb-thread:thread-alive-p thread))
      (sb-thread:terminate-thread thread))
    (when proc
      (uiop:terminate-process proc :urgent t))))

(defun stop-mcp-server (server)
  "Stops an MCP server, honoring the configured test seam when present."
  (if *stop-mcp-server-function*
      (funcall *stop-mcp-server-function* server)
      (default-stop-mcp-server server)))

(defun default-mcp-send-request (server method params &key (timeout 10))
  "Sends a JSON-RPC request and blocks until a response is received or timeout occurs."
  (let* ((id (mcp-next-id server))
         (mailbox (sb-concurrency:make-mailbox))
         (payload `((:jsonrpc . "2.0")
                    (:id . ,id)
                    (:method . ,method)))
         (payload-json (cl-json:encode-json-to-string
                        (if params
                            (append payload `((:params . ,params)))
                            payload))))
    (mcp-register-request server id mailbox)
    (format t "[MCP DEBUG] (~A) -> Request ID ~A: ~A~%" (mcp-server-name server) id method)
    (handler-case
        (progn
          (write-line payload-json (mcp-server-input-stream server))
          (force-output (mcp-server-input-stream server))
          (let ((response (trivial-timeout:with-timeout (timeout)
                            (sb-concurrency:receive-message mailbox))))
             (format t "[MCP DEBUG] (~A) <- Response ID ~A received~%" (mcp-server-name server) id)
             response))
      (trivial-timeout:timeout-error ()
        (mcp-lookup-and-remove-request server id)
        (error "MCP request ~A timeout (~A seconds)" method timeout))
      (error (e)
        (mcp-lookup-and-remove-request server id)
        (error "MCP request error: ~A" e)))))

(defun mcp-send-request (server method params &key (timeout 10))
  "Sends an MCP request, honoring the configured test seam when present."
  (if *mcp-send-request-function*
      (funcall *mcp-send-request-function* server method params :timeout timeout)
      (default-mcp-send-request server method params :timeout timeout)))

(defun mcp-send-notification (server method params)
  "Sends a JSON-RPC notification (no response expected)."
  (let* ((payload `((:jsonrpc . "2.0")
                    (:method . ,method)))
         (payload-json (cl-json:encode-json-to-string
                        (if params
                            (append payload `((:params . ,params)))
                            payload))))
    (format t "[MCP DEBUG] (~A) -> Notification: ~A~%" (mcp-server-name server) method)
    (handler-case
        (progn
          (write-line payload-json (mcp-server-input-stream server))
          (force-output (mcp-server-input-stream server)))
      (error (e)
        (format *error-output* "MCP notification error: ~A~%" e)))))

(defun default-mcp-initialize (server)
  "Performs the MCP initialize handshake."
  (format t "[MCP INFO] Starting initialize handshake with ~A~%" (mcp-server-name server))
  (let ((response (mcp-send-request server "initialize"
                                    `((:protocol-version . "2024-11-05")
                                      (:capabilities . :empty-object)
                                      (:client-info . ((:name . "chatbot")
                                                       (:version . "1.0.0"))))
                                    :timeout 60)))
    (mcp-send-notification server "notifications/initialized" nil)
    (format t "[MCP INFO] Handshake with ~A complete.~%" (mcp-server-name server))
    response))

(defun mcp-initialize (server)
  "Performs the MCP initialize handshake, honoring the configured test seam when present."
  (if *mcp-initialize-function*
     (funcall *mcp-initialize-function* server)
     (default-mcp-initialize server)))

(defun mcp-list-tools (server &key refresh)
  "Lists the tools supported by the server."
  (if (and (not refresh)
           (mcp-server-tool-list-cache-valid-p server))
      (mcp-server-tool-list-cache server)
      (let ((response (mcp-send-request server "tools/list" nil)))
        (setf (mcp-server-tool-list-cache server) response)
        (setf (mcp-server-tool-list-cache-valid-p server) t)
        response)))

(defun default-mcp-call-tool (server name arguments)
  "Invokes a tool on the server with arguments."
  (mcp-send-request server "tools/call"
                    `((:name . ,name)
                      (:arguments . ,arguments))))

(defun mcp-call-tool (server name arguments)
  "Invokes a tool on the server, honoring the configured test seam when present."
  (if *mcp-call-tool-function*
      (funcall *mcp-call-tool-function* server name arguments)
      (default-mcp-call-tool server name arguments)))

(defun translate-mcp-tool-to-openai (mcp-tool)
  "Translates MCP tool definition to OpenAI function tool format."
  (let ((name (mcp-val :name mcp-tool))
        (description (mcp-val :description mcp-tool))
        (input-schema (mcp-val :input-schema mcp-tool)))
    `(("type" . "function")
      ("function" . (("name" . ,name)
                     ("description" . ,(or description ""))
                     ("parameters" . ,(or input-schema '((:type . "object") (:properties . nil)))))))))

(defun translate-mcp-tool-to-gemini-fn (mcp-tool)
  "Translates MCP tool definition to Gemini function format."
  (let ((name (mcp-val :name mcp-tool))
        (description (mcp-val :description mcp-tool))
        (input-schema (mcp-val :input-schema mcp-tool)))
    `(("name" . ,name)
      ("description" . ,(or description ""))
      ("parameters" . ,(gemini-tool-parameters input-schema)))))

(defun parse-mcp-server-def (srv-raw)
  "Parses a raw server definition from the configuration file.
Supports two formats:
1. Standard plist format: (:name \"name\" :command \"command\" :args (\"args\"))
2. Custom nested list format: (\"name\" (:command \"command\") (:args \"args\"...))"
  (cond
    ((and (listp srv-raw) (keywordp (car srv-raw)))
     (let ((name (safe-getf srv-raw :name))
           (command (safe-getf srv-raw :command))
           (args (safe-getf srv-raw :args))
           (required-p (safe-getf srv-raw :required))
           (environment (safe-getf srv-raw :env))
           (system-instruction (safe-getf srv-raw :system-instruction)))
       (values name
               command
               (cond
                 ((null args) nil)
                 ((listp args) args)
                 (t (list args)))
               required-p
               environment
               system-instruction)))
    ((and (listp srv-raw) (stringp (car srv-raw)))
     (let* ((name (car srv-raw))
            (body (cdr srv-raw))
            (cmd-entry (assoc :command body))
            (args-entry (assoc :args body))
            (required-entry (assoc :required body))
            (env-entry (assoc :env body))
            (system-instruction-entry (assoc :system-instruction body))
            (command (and cmd-entry (cadr cmd-entry)))
            (args (and args-entry (cdr args-entry)))
            (required-p (and required-entry (cadr required-entry)))
            (environment (and env-entry (cdr env-entry)))
            (system-instruction (and system-instruction-entry (cadr system-instruction-entry))))
       (values name command args required-p environment system-instruction)))
    (t (values nil nil nil nil nil nil))))

(defun mcp-config-server-definitions (config)
  "Returns the raw MCP server definitions from CONFIG."
  (cond
    ((null config) nil)
    ((and (consp config)
         (consp (car config))
         (eq (car (car config)) :mcp-servers))
     (cdr (car config)))
    (t config)))

(defun find-configured-mcp-server-definition (server-name &optional (config (read-mcp-config)))
  "Returns the raw configured MCP server definition matching SERVER-NAME."
  (find server-name
       (mcp-config-server-definitions config)
       :test #'string=
       :key (lambda (srv-raw)
              (nth-value 0 (parse-mcp-server-def srv-raw)))))

(defun initialize-configured-mcp-server (server-name &key environment)
  "Starts and initializes the configured MCP server named SERVER-NAME."
  (let ((server-def (find-configured-mcp-server-definition server-name)))
    (unless server-def
     (error "Configured MCP server not found: ~A" server-name))
    (multiple-value-bind (name command args required-p configured-environment system-instruction)
       (parse-mcp-server-def server-def)
     (declare (ignore required-p system-instruction))
     (unless (and name command)
       (error "Invalid MCP server definition for ~A." server-name))
     (let ((server nil))
       (handler-case
           (progn
             (setf server
                   (let ((merged-environment (merge-mcp-server-environments configured-environment
                                                                           environment)))
                     (if merged-environment
                         (start-mcp-server name command args merged-environment)
                         (start-mcp-server name command args))))
             (mcp-initialize server)
             server)
         (error (e)
           (when server
             (stop-mcp-server server))
           (error "Failed to start/initialize MCP server ~A: ~A" server-name e)))))))

(defun mcp-startup-status-partial-failure-p (status)
  "Returns true when STATUS represents a mix of successful and failed server startups."
  (and (> (mcp-startup-status-failed-count status) 0)
      (> (mcp-startup-status-successful-count status) 0)))

(defun mcp-startup-status-required-failures (status)
  "Returns the failed required startup entries from STATUS."
  (remove-if #'mcp-startup-entry-success-p
            (remove-if-not #'mcp-startup-entry-required-p
                           (mcp-startup-status-entries status))))

(defun startup-chatbot-mcp-status (&optional context)
  "Returns the structured MCP startup status for the shared startup chatbot."
  (let ((startup-bot (ensure-startup-chatbot context)))
    (and startup-bot
        (chatbot-mcp-startup-status startup-bot))))

(defun parse-startup-chatbot-options (args)
  "Parses startup chatbot invocation ARGS into context and keyword options."
  (let ((context nil)
        (strict-required-p nil))
    (when (and args
              (not (keywordp (car args))))
      (setf context (car args))
      (setf args (cdr args)))
    (loop while args
         for key = (pop args)
         for value = (pop args)
         do (case key
              (:strict-required-p
               (setf strict-required-p value))
              (t
               (error "Unknown startup chatbot option: ~S" key))))
    (values context strict-required-p)))

(defun default-initialize-mcp-servers-for-chatbot (bot &key strict-required-p)
  "Discovers and initializes MCP servers for the chatbot."
  (format t "[MCP INFO] Scanning for MCP configurations...~%")
  (let ((config (read-mcp-config)))
    (if config
       (let* ((servers nil)
              (entries nil)
             (raw-list (mcp-config-server-definitions config)))
         (format t "[MCP INFO] Found ~A server definitions to initialize.~%" (length raw-list))
         (dolist (cfg raw-list)
           (multiple-value-bind (name command args required-p environment system-instruction) (parse-mcp-server-def cfg)
             (declare (ignore system-instruction))
             (let ((entry (make-instance 'mcp-startup-entry
                                         :name (or name "<invalid-server>")
                                         :command command
                                         :args args
                                         :required-p required-p)))
               (cond
                 ((not (and name command))
                  (setf (mcp-startup-entry-error-message entry)
                        "Invalid MCP server definition: missing required name or command.")
                  (format *error-output* "Failed to start/initialize MCP server ~A: ~A~%"
                          (mcp-startup-entry-name entry)
                          (mcp-startup-entry-error-message entry)))
                 (t
                  (let ((server nil))
                    (handler-case
                        (progn
                          (setf server (if environment
                                           (start-mcp-server name command args environment)
                                           (start-mcp-server name command args)))
                          (mcp-initialize server)
                          (setf (mcp-startup-entry-success-p entry) t)
                          (setf (mcp-startup-entry-server entry) server)
                          (push server servers))
                      (error (e)
                        (when server
                          (stop-mcp-server server))
                        (setf (mcp-startup-entry-error-message entry) (princ-to-string e))
                        (format *error-output* "Failed to start/initialize MCP server ~A: ~A~%"
                                name
                                (mcp-startup-entry-error-message entry)))))))
               (push entry entries))))
         (let* ((entries (nreverse entries))
                (required-failed-count (count-if (lambda (entry)
                                                  (and (mcp-startup-entry-required-p entry)
                                                       (not (mcp-startup-entry-success-p entry))))
                                                entries))
                (status (make-instance 'mcp-startup-status
                                       :entries entries
                                       :strict-required-p strict-required-p
                                       :configured-count (length entries)
                                       :successful-count (count-if #'mcp-startup-entry-success-p entries)
                                       :failed-count (count-if-not #'mcp-startup-entry-success-p entries)
                                       :required-failed-count required-failed-count)))
           (setf (chatbot-mcp-servers bot) (nreverse servers))
           (setf (chatbot-mcp-startup-status bot) status)
           (format t "[MCP INFO] Successfully fully initialized ~A out of ~A configured servers.~%"
                   (mcp-startup-status-successful-count status)
                   (mcp-startup-status-configured-count status))
           (when (> (mcp-startup-status-failed-count status) 0)
             (dolist (entry (remove-if #'mcp-startup-entry-success-p entries))
               (format *error-output* "[MCP WARN] Startup failure for ~A~:[~; (required)~]: ~A~%"
                       (mcp-startup-entry-name entry)
                       (mcp-startup-entry-required-p entry)
                       (mcp-startup-entry-error-message entry))))
           (when (and strict-required-p
                      (> (mcp-startup-status-required-failed-count status) 0))
             (error 'mcp-startup-error :status status))
           status))
       (progn
         (setf (chatbot-mcp-servers bot) nil)
         (setf (chatbot-mcp-startup-status bot)
               (make-instance 'mcp-startup-status
                              :entries nil
                              :strict-required-p strict-required-p
                              :configured-count 0
                              :successful-count 0
                              :failed-count 0
                              :required-failed-count 0))
         (format t "[MCP INFO] No MCP configuration found. Falling back to zero tools.~%")
         (chatbot-mcp-startup-status bot)))))

(defun initialize-mcp-servers-for-chatbot (bot &key strict-required-p)
  "Discovers and initializes MCP servers, honoring the configured test seam when present."
  (if *initialize-mcp-servers-for-chatbot-function*
      (funcall *initialize-mcp-servers-for-chatbot-function* bot :strict-required-p strict-required-p)
      (default-initialize-mcp-servers-for-chatbot bot :strict-required-p strict-required-p)))

(defun startup-chatbot-initialized-p (&optional context)
  "Returns true when the shared startup chatbot has been created."
  (not (null (current-startup-chatbot context))))

(defun initialize-startup-chatbot (&rest args)
  "Creates the shared startup chatbot and initializes MCP servers if needed."
  (multiple-value-bind (context strict-required-p)
      (parse-startup-chatbot-options args)
    (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (call-with-runtime-context
     resolved-context
     (lambda ()
       (unless (current-startup-chatbot resolved-context)
         (let ((bot (make-instance 'chatbot :runtime-context resolved-context)))
           (initialize-mcp-servers-for-chatbot bot :strict-required-p strict-required-p)
           (setf (current-startup-chatbot resolved-context) bot)))
       (current-startup-chatbot resolved-context))))))

(defun maybe-auto-initialize-startup-chatbot (&rest args)
  "Initializes shared MCP servers only when eager startup compatibility is enabled."
  (multiple-value-bind (context strict-required-p)
      (parse-startup-chatbot-options args)
    (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
      (when (current-auto-initialize-startup-mcp-servers-p resolved-context)
        (initialize-startup-chatbot resolved-context :strict-required-p strict-required-p)))))

(defun startup-chatbot-mcp-servers (&optional context)
  "Returns the shared MCP server list when startup initialization has occurred."
  (let ((startup-bot (current-startup-chatbot context)))
    (and startup-bot
         (chatbot-mcp-servers startup-bot))))

(defun ensure-startup-chatbot (&optional context)
  "Returns the shared startup chatbot without implicitly initializing MCP servers."
  (current-startup-chatbot context))

(defun startup-chatbot-shared-servers-p (bot &optional context)
  "Returns true when BOT is using the shared startup MCP server set."
  (let* ((context (or context (chatbot-runtime-context bot)))
         (startup-bot (ensure-startup-chatbot context)))
    (and startup-bot
         (not (eq bot startup-bot))
         (eq (chatbot-mcp-servers bot)
             (chatbot-mcp-servers startup-bot)))))

(defun shutdown-chatbot (bot &optional context)
  "Closes and stops all MCP servers connected to the chatbot."
  (let* ((context (or context (chatbot-runtime-context bot)))
         (startup-bot (ensure-startup-chatbot context)))
    (dolist (server (chatbot-mcp-servers bot))
      (unless (and startup-bot
                  (not (eq bot startup-bot))
                  (member server (chatbot-mcp-servers startup-bot) :test #'eq))
        (when (typep server 'mcp-server)
         (invalidate-mcp-tool-list-cache server))
        (stop-mcp-server server)))
    (when (eq bot startup-bot)
      (if context
         (setf (current-startup-chatbot context) nil)
          (setf (current-startup-chatbot) nil)))
    (setf (chatbot-mcp-servers bot) nil)))

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

(eval-when (:load-toplevel :execute)
  (maybe-auto-initialize-startup-chatbot))

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

(defun default-execute-mcp-tool (server tool-name arguments)
  "Calls the tool on the given MCP server and returns the result content string."
  (handler-case
      (let* ((response (mcp-call-tool server tool-name arguments))
             (content (mcp-val :content response))
             (result-texts nil))
        (dolist (item content)
          (let ((type (mcp-val :type item))
                (text (mcp-val :text item)))
            (when (and (string= type "text") text)
              (push text result-texts))))
        (if result-texts
            (format nil "~{~A~^~%~}" (nreverse result-texts))
           (error 'mcp-tool-execution-error
                  :tool-name tool-name
                  :reason "Tool returned no text content.")))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (princ-to-string e)))))

(defun execute-mcp-tool (server tool-name arguments)
  "Executes an MCP tool, honoring the configured test seam when present."
  (if *execute-mcp-tool-function*
      (funcall *execute-mcp-tool-function* server tool-name arguments)
      (default-execute-mcp-tool server tool-name arguments)))
