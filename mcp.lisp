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

(defun close-mcp-server-stream (stream)
  "Closes STREAM when it is a live stream."
  (when (and stream (open-stream-p stream))
    (close stream)))

(defun default-stop-mcp-server (server)
  "Stops the MCP server process and reader thread cleanly."
  (format t "[MCP INFO] Stopping server ~A~%" (mcp-server-name server))
  (let ((thread (mcp-server-reader-thread server))
        (proc (mcp-server-process server))
        (input-stream (mcp-server-input-stream server))
        (output-stream (mcp-server-output-stream server)))
    (when (and thread (sb-thread:thread-alive-p thread))
      (sb-thread:terminate-thread thread))
    (close-mcp-server-stream input-stream)
    (close-mcp-server-stream output-stream)
    (when proc
      (uiop:terminate-process proc :urgent t))
    (setf (mcp-server-reader-thread server) nil
          (mcp-server-process server) nil
          (mcp-server-input-stream server) nil
          (mcp-server-output-stream server) nil)))

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
                        (json-encodable-value
                         (if params
                             (append payload `((:params . ,params)))
                             payload)))))
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
                        (json-encodable-value
                         (if params
                             (append payload `((:params . ,params)))
                             payload)))))
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

(defun builtin-read-file-lines-tool ()
  "Returns the built-in readFileLines tool definition."
  '((:name . "readFileLines")
    (:description . "Reads an inclusive line range from a file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("filename" . ((:type . "string")
                                                     (:description . "Path to the file, relative to the persona directory or absolute within it.")))
                                      ("beginningLine" . ((:type . "integer")
                                                          (:description . "Inclusive starting line number (1-based).")))
                                      ("endingLine" . ((:type . "integer")
                                                       (:description . "Inclusive ending line number (1-based).")))))
                      (:required . ("filename" "beginningLine" "endingLine"))))))

(defun builtin-directory-tool ()
  "Returns the built-in directory tool definition."
  '((:name . "directory")
    (:description . "Lists files in a directory that match a filename pattern.")
    (:input-schema . ((:type . "object")
                      (:properties . (("pathname" . ((:type . "string")
                                                     (:description . "Path to the directory, relative to the persona directory or absolute within it.")))
                                      ("pattern" . ((:type . "string")
                                                    (:description . "Filename pattern to match within the directory, for example *.txt.")))))
                      (:required . ("pathname" "pattern"))))))

(defun builtin-write-file-tool ()
  "Returns the built-in writeFile tool definition."
  '((:name . "writeFile")
    (:description . "Creates or overwrites a file from provided lines and newline settings.")
    (:input-schema . ((:type . "object")
                      (:properties . (("pathname" . ((:type . "string")
                                                     (:description . "Path to the file, relative to the persona directory or absolute within it.")))
                                      ("useLfOnly" . ((:type . "boolean")
                                                      (:description . "When true, use LF line endings; otherwise use CRLF line endings.")))
                                      ("endWithEol" . ((:type . "boolean")
                                                       (:description . "When true, end the file with a trailing line ending when lines are present.")))
                                      ("lines" . ((:type . "array")
                                                  (:items . ((:type . "string")))
                                                  (:description . "Array of file lines to write in order.")))))
                      (:required . ("pathname" "useLfOnly" "endWithEol" "lines"))))))

(defun builtin-delete-file-tool ()
  "Returns the built-in deleteFile tool definition."
  '((:name . "deleteFile")
    (:description . "Deletes a file.")
    (:input-schema . ((:type . "object")
                      (:properties . (("pathname" . ((:type . "string")
                                                     (:description . "Path to the file, relative to the persona directory or absolute within it.")))))
                      (:required . ("pathname"))))))

(defun builtin-eval-tool ()
  "Returns the built-in eval tool definition."
  '((:name . "eval")
    (:description . "Reads and evaluates one Lisp s-expression after explicit user approval.")
    (:input-schema . ((:type . "object")
                      (:properties . (("expression" . ((:type . "string")
                                                       (:description . "A single Lisp s-expression to read and evaluate.")))))
                      (:required . ("expression"))))))

(defun builtin-web-search-tool ()
  "Returns the built-in webSearch tool definition."
  '((:name . "webSearch")
    (:description . "Searches the web for grounding information.")
    (:input-schema . ((:type . "object")
                      (:properties . (("query" . ((:type . "string")
                                                  (:description . "The general web search query.")))))
                      (:required . ("query"))))))

(defun builtin-hyperspec-search-tool ()
  "Returns the built-in hyperspecSearch tool definition."
  '((:name . "hyperspecSearch")
    (:description . "Searches the Common Lisp HyperSpec for grounding information.")
    (:input-schema . ((:type . "object")
                      (:properties . (("query" . ((:type . "string")
                                                  (:description . "The Common Lisp / HyperSpec search query.")))))
                      (:required . ("query"))))))

(defun default-get-all-builtin-tools (bot)
  "Returns all built-in tools enabled for BOT as (source . tool) pairs."
  (let ((tools nil))
    (when (chatbot-web-tools-p bot)
      (push (cons :built-in (builtin-hyperspec-search-tool)) tools)
      (push (cons :built-in (builtin-web-search-tool)) tools))
    (when (chatbot-enable-eval-p bot)
      (push (cons :built-in (builtin-eval-tool)) tools))
    (when (chatbot-filesystem-tools-p bot)
      (push (cons :built-in (builtin-delete-file-tool)) tools)
      (push (cons :built-in (builtin-write-file-tool)) tools)
      (push (cons :built-in (builtin-directory-tool)) tools)
      (push (cons :built-in (builtin-read-file-lines-tool)) tools))
    (nreverse tools)))

(defun get-all-chatbot-tools (bot)
  "Returns all built-in and MCP tools available to BOT."
  (append (default-get-all-builtin-tools bot)
          (get-all-mcp-tools bot)))

(defun default-find-builtin-tool (bot tool-name)
  "Finds a built-in tool enabled for BOT by TOOL-NAME."
  (dolist (entry (default-get-all-builtin-tools bot))
    (let ((tool (cdr entry)))
      (when (string= (mcp-val :name tool) tool-name)
        (return-from default-find-builtin-tool (values (car entry) tool)))))
  (values nil nil))

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
   (parse-json-or-error arguments-json :context context)))

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
entries instead of aborting the full turn."
  (let ((results nil))
    (dolist (tool-call tool-calls (nreverse results))
      (let* ((id (cdr (assoc :id tool-call)))
             (name (cdr (assoc :name tool-call)))
             (arguments-json (coerce (cdr (assoc :arguments tool-call)) 'string)))
        (handler-case
            (let ((res-text (execute-chatbot-tool-by-name-json-arguments
                             bot
                             name
                             arguments-json
                             (funcall context-builder name tool-call))))
              (push (funcall result-builder id name arguments-json res-text tool-call) results))
          (error (condition)
            (if error-builder
                (push (funcall error-builder id name arguments-json condition tool-call) results)
                (error condition))))))))

(defun normalize-builtin-tool-integer-argument (value argument-name tool-name)
  "Normalizes VALUE to an integer argument or signals an execution error."
  (let ((normalized
          (typecase value
            (integer value)
            (real (truncate value))
            (string (parse-integer value :junk-allowed t))
            (t nil))))
    (unless normalized
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be an integer." argument-name)))
    normalized))

(defun normalize-builtin-tool-string-argument (value argument-name tool-name)
  "Normalizes VALUE to a non-empty string argument or signals an execution error."
  (unless (and (stringp value) (string/= value ""))
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A must be a non-empty string." argument-name)))
  value)

(defun builtin-tool-argument (arguments &rest keys)
  "Looks up the first present argument among KEYS in ARGUMENTS."
  (dolist (key keys (values nil nil))
    (let ((entry (mcp-assoc key arguments)))
      (when entry
        (return (values t (cdr entry)))))))

(defun normalize-builtin-tool-boolean-argument (foundp value argument-name tool-name)
  "Normalizes VALUE to a boolean argument or signals an execution error."
  (unless foundp
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A is required." argument-name)))
  (let ((printed (string-downcase (princ-to-string value))))
    (cond
      ((or (eq value t)
           (eq value :true)
           (string= printed "t")
           (string= printed "true")
           (search "json-literal true" printed))
       t)
      ((or (null value)
           (eq value :false)
           (string= printed "nil")
           (string= printed "false")
           (search "json-literal false" printed))
       nil)
      ((symbolp value)
       (cond
         ((string-equal (symbol-name value) "true") t)
         ((string-equal (symbol-name value) "false") nil)
         (t
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "~A must be a boolean." argument-name)))))
      ((stringp value)
       (cond
         ((string-equal value "true") t)
         ((string-equal value "false") nil)
         (t
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "~A must be a boolean." argument-name)))))
      (t
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason (format nil "~A must be a boolean." argument-name))))))

(defun normalize-builtin-tool-string-sequence-argument (foundp value argument-name tool-name)
  "Normalizes VALUE to a list of strings or signals an execution error."
  (unless foundp
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A is required." argument-name)))
  (let ((elements (cond
                    ((vectorp value) (coerce value 'list))
                    ((listp value) value)
                    (t nil))))
    (unless elements
      (when (or (vectorp value) (listp value))
        (return-from normalize-builtin-tool-string-sequence-argument nil))
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be an array of strings." argument-name)))
    (unless (every #'stringp elements)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must contain only strings." argument-name)))
    elements))

(defun chatbot-filesystem-root-truename (bot tool-name)
  "Returns the validated filesystem root directory for BOT."
  (let ((root (chatbot-filesystem-root-directory bot)))
    (unless root
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "Filesystem tools require a configured root directory."))
    (uiop:ensure-directory-pathname (truename root))))

(defun filesystem-path-within-directory-p (path directory)
  "Returns true when PATH is equal to or nested under DIRECTORY."
  (let ((path-name (string-downcase (namestring (truename path))))
        (directory-name (string-downcase (namestring (uiop:ensure-directory-pathname (truename directory))))))
    (alexandria:starts-with-subseq directory-name path-name)))

(defun canonicalize-allowed-filesystem-directories (directories)
  "Returns DIRECTORIES deduplicated and collapsed by ancestor coverage."
  (let ((sorted (sort (remove-duplicates
                      (mapcar #'uiop:ensure-directory-pathname directories)
                      :test (lambda (left right)
                              (string-equal (namestring left) (namestring right))))
                     #'<
                     :key (lambda (directory)
                            (length (namestring directory))))))
    (let ((result nil))
      (dolist (directory sorted (nreverse result))
        (unless (some (lambda (approved)
                       (filesystem-path-within-directory-p directory approved))
                     result)
         (push directory result))))))

(defun chatbot-effective-filesystem-allowed-directories (bot tool-name)
  "Returns all effective allowed directories for BOT, including the persona root."
  (canonicalize-allowed-filesystem-directories
   (cons (chatbot-filesystem-root-truename bot tool-name)
        (or (chatbot-filesystem-allowed-directories bot) nil))))

(defun persist-chatbot-filesystem-allowlist (bot)
  "Persists BOT's explicit filesystem allowlist when it has a configured allowlist file."
  (let ((allowlist-path (chatbot-filesystem-allowlist-path bot)))
    (when allowlist-path
      (with-open-file (stream allowlist-path
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
        (prin1 (mapcar #'namestring
                      (canonicalize-allowed-filesystem-directories
                       (or (chatbot-filesystem-allowed-directories bot) nil)))
              stream)))))

(defun approve-chatbot-filesystem-directory (bot directory tool-name)
  "Requests approval for DIRECTORY and persists it for BOT when granted."
  (let ((approval-function (current-filesystem-access-approval-function
                            (chatbot-runtime-context bot))))
    (unless approval-function
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason "No filesystem access approval function is configured."))
    (unless (funcall approval-function bot directory tool-name)
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason (format nil "Access to directory denied: ~A" directory)))
    (setf (chatbot-filesystem-allowed-directories bot)
         (canonicalize-allowed-filesystem-directories
          (cons directory (or (chatbot-filesystem-allowed-directories bot) nil))))
    (persist-chatbot-filesystem-allowlist bot)
    directory))

(defun ensure-filesystem-tool-directory-authorized (bot directory tool-name)
  "Ensures DIRECTORY is covered by BOT's allowlist, prompting for approval when needed."
  (let ((normalized-directory (uiop:ensure-directory-pathname (truename directory))))
    (if (some (lambda (allowed-directory)
               (filesystem-path-within-directory-p normalized-directory allowed-directory))
             (chatbot-effective-filesystem-allowed-directories bot tool-name))
        normalized-directory
        (approve-chatbot-filesystem-directory bot normalized-directory tool-name))))

(defun filesystem-containing-directory (path)
  "Returns the containing directory pathname for PATH."
  (uiop:ensure-directory-pathname
   (make-pathname :name nil :type nil :defaults path)))

(defun resolve-filesystem-tool-existing-path (bot raw-path tool-name argument-name missing-reason &key directory-target-p)
  "Resolves RAW-PATH and authorizes it for BOT."
  (let ((root (chatbot-filesystem-root-truename bot tool-name)))
  (let* ((validated-path (normalize-builtin-tool-string-argument raw-path argument-name tool-name))
        (requested (pathname validated-path))
        (candidate (if (uiop:absolute-pathname-p requested)
                       requested
                       (merge-pathnames requested root)))
        (resolved (probe-file candidate)))
    (unless resolved
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason (format nil "~A: ~A" missing-reason validated-path)))
      (ensure-filesystem-tool-directory-authorized
       bot
       (if directory-target-p
          (uiop:ensure-directory-pathname resolved)
          (filesystem-containing-directory resolved))
       tool-name)
      resolved)))

(defun resolve-filesystem-tool-path (bot filename tool-name)
  "Resolves FILENAME for BOT inside the allowed filesystem root."
  (resolve-filesystem-tool-existing-path bot
                                        filename
                                        tool-name
                                        "filename"
                                        "File not found"))

(defun resolve-filesystem-tool-target-path (bot pathname tool-name)
  "Resolves PATHNAME for BOT as a write target inside the allowed filesystem root."
  (let* ((root (chatbot-filesystem-root-truename bot tool-name))
         (validated-path (normalize-builtin-tool-string-argument pathname "pathname" tool-name))
         (requested (pathname validated-path))
         (candidate (if (uiop:absolute-pathname-p requested)
                        requested
                        (merge-pathnames requested root)))
         (parent-candidate (uiop:ensure-directory-pathname
                            (make-pathname :name nil :type nil :defaults candidate)))
         (parent-resolved (probe-file parent-candidate)))
    (when (and (null (pathname-name candidate))
               (null (pathname-type candidate)))
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Pathname must name a file: ~A" pathname)))
    (unless parent-resolved
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Parent directory not found: ~A" parent-candidate)))
    (let* ((parent-truename (uiop:ensure-directory-pathname parent-resolved))
           (safe-parent (ensure-filesystem-tool-directory-authorized bot parent-truename tool-name))
           (resolved-target (merge-pathnames (make-pathname :name (pathname-name candidate)
                                                           :type (pathname-type candidate)
                                                           :version (pathname-version candidate))
                                            safe-parent)))
      (values resolved-target root))))

(defun resolve-filesystem-tool-directory (bot pathname tool-name)
  "Resolves PATHNAME for BOT as an existing directory inside the allowed filesystem root."
  (let* ((root (chatbot-filesystem-root-truename bot tool-name))
         (resolved (resolve-filesystem-tool-existing-path bot
                                                         pathname
                                                         tool-name
                                                         "pathname"
                                                         "Directory not found"
                                                         :directory-target-p t)))
    (unless (uiop:directory-exists-p resolved)
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason (format nil "Path is not a directory: ~A" pathname)))
    (values (uiop:ensure-directory-pathname resolved) root)))

(defun read-file-lines-subset (path beginning-line ending-line tool-name)
  "Returns the inclusive line subset from PATH."
  (when (< beginning-line 1)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "beginningLine must be >= 1."))
  (when (< ending-line beginning-line)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "endingLine must be >= beginningLine."))
  (with-open-file (stream path :direction :input)
    (let ((lines nil)
          (line-count 0)
          (eof-marker (gensym "EOF")))
      (loop for line = (read-line stream nil eof-marker)
            until (eq line eof-marker)
            do (incf line-count)
               (when (<= beginning-line line-count ending-line)
                 (push line lines)))
      (when (< line-count beginning-line)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "beginningLine ~D is past end of file (~D lines)."
                               beginning-line
                               line-count)))
      (format nil "~{~A~^~%~}" (nreverse lines)))))

(defun validate-directory-tool-pattern (pattern tool-name)
  "Validates PATTERN for the built-in directory tool."
  (let ((validated-pattern (normalize-builtin-tool-string-argument pattern "pattern" tool-name))
       (pattern-path (pathname pattern)))
    (when (uiop:absolute-pathname-p pattern-path)
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason "pattern must be relative to pathname."))
    (when (pathname-directory pattern-path)
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason "pattern must not contain directory components."))
    validated-pattern))

(defun directory-tool-result (directory-path root pattern tool-name)
  "Returns a stable JSON array string of matching files for DIRECTORY-PATH and PATTERN."
  (declare (ignore tool-name))
  (let* ((validated-pattern (validate-directory-tool-pattern pattern "directory"))
        (matches (remove-if #'uiop:directory-exists-p
                            (cl:directory (merge-pathnames (pathname validated-pattern)
                                                           directory-path))))
        (relative-paths (sort (mapcar (lambda (path)
                                        (enough-namestring path root))
                                      matches)
                              #'string-lessp
                              :key #'string-downcase)))
    (cl-json:encode-json-to-string (coerce relative-paths 'vector))))

(defun write-file-content-from-lines (lines use-lf-only end-with-eol)
  "Serializes LINES using the requested line-ending controls."
  (if (null lines)
      ""
      (let ((separator (if use-lf-only
                           (string #\Linefeed)
                           (format nil "~C~C" #\Return #\Linefeed))))
        (with-output-to-string (stream)
          (loop for line in lines
                for firstp = t then nil
                do (unless firstp
                     (write-string separator stream))
                   (write-string line stream))
          (when end-with-eol
            (write-string separator stream))))))

(defun write-file-tool-result (target-path root lines use-lf-only end-with-eol)
  "Writes LINES to TARGET-PATH and returns a stable success string."
  (with-open-file (stream target-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (write-file-content-from-lines lines use-lf-only end-with-eol)
                  stream))
  (format nil "Wrote file: ~A" (enough-namestring target-path root)))

(defun delete-file-tool-result (path root)
  "Deletes PATH and returns a stable success string."
  (when (uiop:directory-exists-p path)
    (error 'mcp-tool-execution-error
           :tool-name "deleteFile"
           :reason (format nil "Path is not a file: ~A" path)))
  (delete-file path)
  (format nil "Deleted file: ~A" (enough-namestring path root)))

(defun whitespace-only-stream-remaining-p (stream)
  "Returns true when the remainder of STREAM contains only whitespace."
  (let ((eof-marker (gensym "EOF")))
    (loop for char = (read-char stream nil eof-marker)
          until (eq char eof-marker)
          always (find char '(#\Space #\Tab #\Newline #\Return #\Page)))))

(defun read-eval-tool-form (expression tool-name)
  "Reads exactly one Lisp form from EXPRESSION with reader eval disabled."
  (handler-case
      (let ((*read-eval* nil)
            (*package* (find-package "CHATBOT")))
        (with-input-from-string (stream expression)
          (let ((eof-marker (gensym "EOF")))
            (let ((form (read stream nil eof-marker)))
              (when (eq form eof-marker)
                (error 'mcp-tool-execution-error
                       :tool-name tool-name
                       :reason "expression must contain one s-expression."))
              (unless (whitespace-only-stream-remaining-p stream)
                (error 'mcp-tool-execution-error
                       :tool-name tool-name
                       :reason "expression must contain exactly one s-expression."))
              form))))
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Failed to parse expression: ~A" e)))))

(defun approve-chatbot-eval-expression (bot expression tool-name)
  "Requests approval to evaluate EXPRESSION for BOT."
  (let ((approval-function (current-eval-approval-function
                            (chatbot-runtime-context bot))))
    (unless approval-function
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "No eval approval function is configured."))
    (unless (funcall approval-function bot expression tool-name)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "Evaluation denied by user."))
    t))

(defun eval-tool-result-json (values stdout stderr)
  "Returns a stable JSON result string for VALUES, STDOUT, and STDERR."
  (cl-json:encode-json-to-string
   `(("values" . ,(coerce (mapcar #'prin1-to-string values) 'vector))
     ("stdout" . ,stdout)
     ("stderr" . ,stderr))))

(defun hash-table-value-any (table keys)
  "Returns the first value present in TABLE for any of KEYS."
  (when (hash-table-p table)
    (dolist (key keys nil)
      (multiple-value-bind (value foundp) (gethash key table)
        (when foundp
          (return value))))))

(defun normalize-grounding-search-response (response tool-name)
  "Returns RESPONSE as a hash table or signals an execution error."
  (unless (hash-table-p response)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Search returned an unexpected response shape."))
  response)

(defun grounding-search-items (response)
  "Returns the result items vector/list from RESPONSE."
  (let ((items (hash-table-value-any response '(:items "items"))))
    (cond
      ((vectorp items) (coerce items 'list))
      ((listp items) items)
      (t nil))))

(defun format-grounding-search-results (label query response tool-name)
  "Formats grounding RESPONSE into stable text for LABEL and QUERY."
  (let* ((normalized (normalize-grounding-search-response response tool-name))
         (items (grounding-search-items normalized))
         (search-info (hash-table-value-any normalized '(:search-information "searchInformation" :search--information)))
         (total-results (or (hash-table-value-any search-info '(:total-results "totalResults" :total--results))
                            (and items (princ-to-string (length items)))
                            "0")))
    (with-output-to-string (stream)
      (format stream "~A query: ~A~%Total results: ~A" label query total-results)
      (if items
          (loop for item in items
                for index from 1
                for title = (or (hash-table-value-any item '(:title "title"))
                                "(untitled)")
                for link = (or (hash-table-value-any item '(:link "link"))
                               "(no link)")
                for snippet = (or (hash-table-value-any item '(:snippet "snippet"))
                                  "")
                do (format stream "~%~%~D. ~A~%URL: ~A" index title link)
                   (when (string/= snippet "")
                     (format stream "~%Snippet: ~A" snippet)))
          (format stream "~%~%No results found.")))))

(defun run-grounding-search (tool-name label function query)
  "Runs grounding search FUNCTION for QUERY and formats a stable tool result."
  (handler-case
      (format-grounding-search-results label
                                       query
                                       (funcall function query)
                                       tool-name)
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Search failed for query ~S: ~A" query e)))))

(defvar *eval-tool-timeout-seconds* 60
  "Maximum number of seconds an approved eval tool expression may run.")

(defun execute-approved-eval-expression (expression form tool-name)
  "Evaluates FORM and returns a structured JSON string with values and captured output."
  (let ((stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream)))
    (let ((*standard-output* stdout-stream)
          (*error-output* stderr-stream)
          (*package* (find-package "CHATBOT")))
      (handler-case
          (let ((values (trivial-timeout:with-timeout (*eval-tool-timeout-seconds*)
                          (multiple-value-list (eval form)))))
            (eval-tool-result-json values
                                   (get-output-stream-string stdout-stream)
                                   (get-output-stream-string stderr-stream)))
        (trivial-timeout:timeout-error ()
          (let ((stdout (get-output-stream-string stdout-stream))
                (stderr (get-output-stream-string stderr-stream)))
            (error 'mcp-tool-execution-error
                   :tool-name tool-name
                   :reason (format nil "Evaluation timed out after ~D seconds.~@[~%stdout:~%~A~]~@[~%stderr:~%~A~]"
                                   *eval-tool-timeout-seconds*
                                   (and (string/= stdout "") stdout)
                                   (and (string/= stderr "") stderr)))))
        (error (e)
          (let ((stdout (get-output-stream-string stdout-stream))
                (stderr (get-output-stream-string stderr-stream)))
            (error 'mcp-tool-execution-error
                   :tool-name tool-name
                   :reason (format nil "Evaluation failed for expression ~S: ~A~@[~%stdout:~%~A~]~@[~%stderr:~%~A~]"
                                   expression
                                   e
                                   (and (string/= stdout "") stdout)
                                   (and (string/= stderr "") stderr)))))))))

(defun default-execute-builtin-chatbot-tool (bot tool-name arguments)
  "Executes a built-in tool for BOT."
  (cond
    ((string= tool-name "webSearch")
     (unless (chatbot-web-tools-p bot)
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "Web grounding tools are not enabled."))
     (run-grounding-search tool-name
                           "Web search"
                           *web-search-function*
                           (normalize-builtin-tool-string-argument
                            (or (mcp-val "query" arguments)
                                (mcp-val :query arguments))
                            "query"
                            tool-name)))
    ((string= tool-name "hyperspecSearch")
     (unless (chatbot-web-tools-p bot)
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "Web grounding tools are not enabled."))
     (run-grounding-search tool-name
                           "HyperSpec search"
                           *hyperspec-search-function*
                           (normalize-builtin-tool-string-argument
                            (or (mcp-val "query" arguments)
                                (mcp-val :query arguments))
                            "query"
                            tool-name)))
    ((string= tool-name "eval")
     (unless (chatbot-enable-eval-p bot)
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "Eval tool is not enabled."))
     (let* ((expression (normalize-builtin-tool-string-argument
                         (or (mcp-val "expression" arguments)
                             (mcp-val :expression arguments))
                         "expression"
                         tool-name))
            (form (read-eval-tool-form expression tool-name)))
       (approve-chatbot-eval-expression bot expression tool-name)
       (execute-approved-eval-expression expression form tool-name)))
    ((string= tool-name "readFileLines")
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
    ((string= tool-name "directory")
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
    ((string= tool-name "writeFile")
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
    ((string= tool-name "deleteFile")
     (multiple-value-bind (pathname-foundp pathname)
         (builtin-tool-argument arguments "pathname" :pathname)
       (unless pathname-foundp
         (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason "pathname is required."))
       (let* ((path (resolve-filesystem-tool-path bot pathname tool-name))
             (root (chatbot-filesystem-root-truename bot tool-name)))
         (delete-file-tool-result path root))))
    (t
     (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Unknown built-in tool."))))

(defun execute-chatbot-tool (bot source tool-name arguments)
  "Executes SOURCE as either a built-in or MCP tool for BOT."
  (if (eq source :built-in)
      (default-execute-builtin-chatbot-tool bot tool-name arguments)
      (execute-mcp-tool source tool-name arguments)))

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
