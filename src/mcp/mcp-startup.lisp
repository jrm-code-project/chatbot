;;; -*- Lisp -*-
;;; mcp-startup.lisp - MCP startup status and shared startup orchestration

(in-package "CHATBOT")

(defclass mcp-startup-entry ()
  ((name
    :initarg :name
    :reader mcp-startup-entry-name
    :documentation "Configured MCP server name.")
   (command
    :initarg :command
    :reader mcp-startup-entry-command
    :initform nil
    :documentation "Configured executable command for the server.")
   (args
    :initarg :args
    :reader mcp-startup-entry-args
    :initform nil
    :documentation "Configured command-line arguments for the server.")
   (required-p
    :initarg :required-p
    :reader mcp-startup-entry-required-p
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
    :reader mcp-startup-status-entries
    :initform nil
    :documentation "Per-server startup outcomes.")
   (strict-required-p
    :initarg :strict-required-p
    :reader mcp-startup-status-strict-required-p
    :initform nil
    :documentation "Whether strict required-server failure handling was enabled.")
   (configured-count
    :initarg :configured-count
    :reader mcp-startup-status-configured-count
    :initform 0
    :documentation "Number of configured server definitions considered.")
   (successful-count
    :initarg :successful-count
    :reader mcp-startup-status-successful-count
    :initform 0
    :documentation "Number of successfully initialized servers.")
   (failed-count
    :initarg :failed-count
    :reader mcp-startup-status-failed-count
    :initform 0
    :documentation "Number of failed server startups.")
   (required-failed-count
    :initarg :required-failed-count
    :reader mcp-startup-status-required-failed-count
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

(defun make-mcp-startup-entry (name command args required-p)
  "Returns a fresh startup entry for one MCP server definition."
  (make-instance 'mcp-startup-entry
                :name (or name "<invalid-server>")
                :command command
                :args args
                :required-p required-p))

(defun mark-mcp-startup-entry-failed (entry message)
  "Marks ENTRY as failed with MESSAGE and logs the failure."
  (setf (mcp-startup-entry-error-message entry) message)
  (log-prefixed-message "MCP ERROR"
                       (format nil "Failed to start/initialize MCP server ~A: ~A"
                               (mcp-startup-entry-name entry)
                               message))
  entry)

(defun initialize-mcp-startup-entry (entry environment)
  "Starts and initializes ENTRY, mutating it with the resulting status."
  (let ((server nil))
    (handler-case
       (progn
         (setf server (if environment
                          (start-mcp-server (mcp-startup-entry-name entry)
                                            (mcp-startup-entry-command entry)
                                            (mcp-startup-entry-args entry)
                                            environment)
                          (start-mcp-server (mcp-startup-entry-name entry)
                                            (mcp-startup-entry-command entry)
                                            (mcp-startup-entry-args entry))))
         (mcp-initialize server)
         (setf (mcp-startup-entry-success-p entry) t)
         (setf (mcp-startup-entry-server entry) server)
         entry)
     (error (e)
       (when server
         (stop-mcp-server server))
       (mark-mcp-startup-entry-failed entry (princ-to-string e))))))

(defun startup-entry-from-server-definition (cfg)
  "Returns one startup entry initialized from raw server definition CFG."
  (multiple-value-bind (name command args required-p environment system-instruction)
     (parse-mcp-server-def cfg)
    (declare (ignore system-instruction))
    (let ((entry (make-mcp-startup-entry name command args required-p)))
     (if (and name command)
         (initialize-mcp-startup-entry entry environment)
         (mark-mcp-startup-entry-failed
          entry
          "Invalid MCP server definition: missing required name or command.")))))

(defun initialize-mcp-startup-entries (raw-list)
  "Initializes startup entries for RAW-LIST server definitions."
  (mapcar #'startup-entry-from-server-definition raw-list))

(defun mcp-startup-servers-from-entries (entries)
  "Returns the successfully initialized server list from ENTRIES."
  (remove nil (mapcar #'mcp-startup-entry-server entries)))

(defun mcp-startup-status-from-entries (entries strict-required-p)
  "Builds structured startup status for ENTRIES."
  (let ((required-failed-count
         (count-if (lambda (entry)
                     (and (mcp-startup-entry-required-p entry)
                          (not (mcp-startup-entry-success-p entry))))
                   entries)))
    (make-instance 'mcp-startup-status
                  :entries entries
                  :strict-required-p strict-required-p
                  :configured-count (length entries)
                  :successful-count (count-if #'mcp-startup-entry-success-p entries)
                  :failed-count (count-if-not #'mcp-startup-entry-success-p entries)
                  :required-failed-count required-failed-count)))

(defun apply-mcp-startup-status (bot servers status)
  "Stores startup SERVERS and STATUS on BOT."
  (setf (chatbot-mcp-servers bot) servers)
  (setf (chatbot-mcp-startup-status bot) status)
  status)

(defun log-mcp-startup-summary (status)
  "Logs the final structured startup STATUS."
  (log-prefixed-message "MCP INFO"
                       (format nil "Successfully fully initialized ~A out of ~A configured servers."
                               (mcp-startup-status-successful-count status)
                               (mcp-startup-status-configured-count status)))
  (when (> (mcp-startup-status-failed-count status) 0)
    (dolist (entry (remove-if #'mcp-startup-entry-success-p
                             (mcp-startup-status-entries status)))
     (log-prefixed-message "MCP WARN"
                           (format nil "Startup failure for ~A~:[~; (required)~]: ~A"
                                   (mcp-startup-entry-name entry)
                                   (mcp-startup-entry-required-p entry)
                                   (mcp-startup-entry-error-message entry))))))

(defun empty-mcp-startup-status (strict-required-p)
  "Returns the startup status used when no MCP configuration exists."
  (make-instance 'mcp-startup-status
                :entries nil
                :strict-required-p strict-required-p
                :configured-count 0
                :successful-count 0
                :failed-count 0
                :required-failed-count 0))

(defun apply-empty-mcp-startup-status (bot strict-required-p)
  "Stores and returns the empty startup status for BOT."
  (apply-mcp-startup-status bot nil (empty-mcp-startup-status strict-required-p)))

(defun signal-mcp-startup-required-failures (status strict-required-p)
  "Signals when STATUS contains failed required entries in strict mode."
  (when (and strict-required-p
            (> (mcp-startup-status-required-failed-count status) 0))
    (error 'mcp-startup-error :status status)))

(defun startup-status-from-config (config strict-required-p)
  "Returns initialized startup entry state from CONFIG in strict mode STRICT-REQUIRED-P."
  (let* ((raw-list (mcp-config-server-definitions config))
        (entries (initialize-mcp-startup-entries raw-list))
        (servers (mcp-startup-servers-from-entries entries))
        (status (mcp-startup-status-from-entries entries strict-required-p)))
    (log-prefixed-message "MCP INFO"
                         (format nil "Found ~A server definitions to initialize."
                                 (length raw-list)))
    (values servers status)))

(defun default-initialize-mcp-servers-for-chatbot (bot &key strict-required-p)
  "Discovers and initializes MCP servers for the chatbot."
  (log-prefixed-message "MCP INFO" "Scanning for MCP configurations...")
  (let ((config (read-mcp-config)))
    (if config
       (multiple-value-bind (servers status)
           (startup-status-from-config config strict-required-p)
         (apply-mcp-startup-status bot servers status)
         (log-mcp-startup-summary status)
         (signal-mcp-startup-required-failures status strict-required-p)
         status)
       (progn
         (apply-empty-mcp-startup-status bot strict-required-p)
         (log-prefixed-message "MCP INFO"
                               "No MCP configuration found. Falling back to zero tools.")
         (chatbot-mcp-startup-status bot)))))

(defun initialize-mcp-servers-for-chatbot (bot &key strict-required-p)
  "Discovers and initializes MCP servers, honoring the configured test seam when present."
  (if *initialize-mcp-servers-for-chatbot-function*
      (funcall *initialize-mcp-servers-for-chatbot-function* bot :strict-required-p strict-required-p)
      (default-initialize-mcp-servers-for-chatbot bot :strict-required-p strict-required-p)))

(defun startup-chatbot-initialized-p (&optional context)
  "Returns true when the shared startup chatbot has been created."
  (not (null (current-startup-chatbot context))))

(defun make-startup-chatbot (context strict-required-p)
  "Returns a newly initialized shared startup chatbot for CONTEXT."
  (let ((bot (make-instance 'chatbot :runtime-context context)))
    (initialize-mcp-servers-for-chatbot bot :strict-required-p strict-required-p)
    bot))

(defun ensure-startup-chatbot-initialized (context strict-required-p)
  "Returns the existing shared startup chatbot for CONTEXT, or creates it."
  (or (current-startup-chatbot context)
      (let ((bot (make-startup-chatbot context strict-required-p)))
        (setf (current-startup-chatbot context) bot)
        bot)))

(defun initialize-startup-chatbot (&rest args)
  "Creates the shared startup chatbot and initializes MCP servers if needed."
  (multiple-value-bind (context strict-required-p)
      (parse-startup-chatbot-options args)
    (let ((resolved-context (resolve-runtime-context context)))
      (call-with-runtime-context
       resolved-context
       (lambda ()
         (ensure-startup-chatbot-initialized resolved-context strict-required-p))
       :default-conversation-compatibility-p nil
       :legacy-function-seam-compatibility-p nil))))

(defun maybe-auto-initialize-startup-chatbot (&rest args)
  "Initializes shared MCP servers only when eager startup compatibility is enabled."
  (multiple-value-bind (context strict-required-p)
      (parse-startup-chatbot-options args)
    (let ((resolved-context (resolve-runtime-context context)))
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

(defun chatbot-owned-mcp-servers (bot startup-bot)
  "Returns the MCP servers BOT should stop itself.
Servers shared from STARTUP-BOT remain owned by the startup chatbot."
  (remove-if (lambda (server)
              (and startup-bot
                   (not (eq bot startup-bot))
                   (member server (chatbot-mcp-servers startup-bot) :test #'eq)))
            (chatbot-mcp-servers bot)))

(defun clear-startup-chatbot-reference (bot startup-bot context)
  "Clears the shared startup chatbot reference when BOT owns STARTUP-BOT."
  (when (eq bot startup-bot)
    (if context
        (setf (current-startup-chatbot context) nil)
        (setf (current-startup-chatbot) nil))))

(defun shutdown-chatbot (bot &optional context)
  "Closes and stops all MCP servers connected to the chatbot."
  (let* ((context (or context (chatbot-runtime-context bot)))
         (startup-bot (ensure-startup-chatbot context)))
    (dolist (server (chatbot-owned-mcp-servers bot startup-bot))
      (when (typep server 'mcp-server)
        (invalidate-mcp-tool-list-cache server))
      (stop-mcp-server server))
    (clear-startup-chatbot-reference bot startup-bot context)
    (setf (chatbot-mcp-servers bot) nil)))

(eval-when (:load-toplevel :execute)
  (maybe-auto-initialize-startup-chatbot))
