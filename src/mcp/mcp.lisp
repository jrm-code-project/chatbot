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

(defun mcp-debug-request-target (method params)
  "Returns a short debug suffix describing the request target when useful."
  (let ((tool-name (and (string= method "tools/call")
                        (listp params)
                        (mcp-val :name params))))
    (if tool-name
        (format nil " (~A)" tool-name)
        "")))

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
      (log-prefixed-message "MCP INFO"
                            (format nil "(~A) Tool list changed; invalidating cache."
                                    (mcp-server-name server)))
      (invalidate-mcp-tool-list-cache server))
    (when id
      (let ((mailbox (mcp-lookup-and-remove-request server id)))
        (if mailbox
            (sb-concurrency:send-message mailbox (or error-data result json-data))
            (log-prefixed-message "MCP WARN"
                                  (format nil "Received response for unregistered ID ~A on server ~A"
                                          id
                                          (mcp-server-name server))))))))

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
        (log-prefixed-message "MCP ERROR"
                              (format nil "Reader thread error for server ~A: ~A"
                                      (mcp-server-name server)
                                      e))))))

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
  (log-prefixed-message "MCP INFO" (format nil "Launching server ~A" name))
  (log-prefixed-message "MCP DEBUG" (format nil "Command: ~A ~A" command args))
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
                 do (log-prefixed-message (format nil "MCP ~A STDERR" name) line))
         (error (e)
           (log-prefixed-message "MCP DEBUG"
                                 (format nil "~A STDERR thread terminated: ~A" name e)))))
     :name (concatenate 'string "mcp-err-" name))
    
    (setf (mcp-server-reader-thread server)
          (sb-thread:make-thread (lambda () (mcp-reader-loop server))
                                 :name (concatenate 'string "mcp-reader-" name)))
    (log-prefixed-message "MCP INFO"
                          (format nil "Server ~A process and threads started successfully."
                                  name))
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
  (log-prefixed-message "MCP INFO"
                        (format nil "Stopping server ~A" (mcp-server-name server)))
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
         (debug-target (mcp-debug-request-target method params))
         (payload-json (cl-json:encode-json-to-string
                        (json-encodable-value
                         (if params
                             (append payload `((:params . ,params)))
                             payload)))))
    (mcp-register-request server id mailbox)
    (log-prefixed-message "MCP DEBUG"
                          (format nil "(~A) -> Request ID ~A: ~A~A"
                                 (mcp-server-name server) id method debug-target))
    (handler-case
        (progn
          (write-line payload-json (mcp-server-input-stream server))
          (force-output (mcp-server-input-stream server))
          (let ((response (trivial-timeout:with-timeout (timeout)
                            (sb-concurrency:receive-message mailbox))))
             (log-prefixed-message "MCP DEBUG"
                                  (format nil "(~A) <- Response ID ~A received~A"
                                          (mcp-server-name server) id debug-target))
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
    (log-prefixed-message "MCP DEBUG"
                          (format nil "(~A) -> Notification: ~A"
                                  (mcp-server-name server) method))
    (handler-case
        (progn
          (write-line payload-json (mcp-server-input-stream server))
          (force-output (mcp-server-input-stream server)))
      (error (e)
        (log-prefixed-message "MCP ERROR"
                              (format nil "Notification error: ~A" e))))))

(defun default-mcp-initialize (server)
  "Performs the MCP initialize handshake."
  (log-prefixed-message "MCP INFO"
                        (format nil "Starting initialize handshake with ~A"
                                (mcp-server-name server)))
  (let ((response (mcp-send-request server "initialize"
                                    `((:protocol-version . "2024-11-05")
                                      (:capabilities . :empty-object)
                                      (:client-info . ((:name . "chatbot")
                                                       (:version . "1.0.0"))))
                                    :timeout 60)))
    (mcp-send-notification server "notifications/initialized" nil)
    (log-prefixed-message "MCP INFO"
                          (format nil "Handshake with ~A complete."
                                  (mcp-server-name server)))
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
