;;; -*- Lisp -*-
;;; mcp-startup.lisp - Model Context Protocol (MCP) server lifecycle & config

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

(defun built-in-mcp-server-definition (server-name)
  "Returns a built-in raw MCP server definition for SERVER-NAME, when one exists."
  (cond
    ((string= server-name "memory")
    '(:name "memory"
      :command "npx"
      :args ("-y" "@modelcontextprotocol/server-memory")))
    (t nil)))

(defun find-configured-mcp-server-definition (server-name &optional (config (read-mcp-config)))
  "Returns the raw configured MCP server definition matching SERVER-NAME."
  (find server-name
       (mcp-config-server-definitions config)
       :test #'string=
       :key (lambda (srv-raw)
              (nth-value 0 (parse-mcp-server-def srv-raw)))))

(defun initialize-configured-mcp-server (server-name &key environment)
  "Starts and initializes the configured MCP server named SERVER-NAME."
  (let* ((configured-server-def (find-configured-mcp-server-definition server-name))
        (server-def (or configured-server-def
                        (built-in-mcp-server-definition server-name))))
    (unless server-def
     (error "Configured MCP server not found: ~A" server-name))
    (unless configured-server-def
     (log-prefixed-message "MCP INFO"
                           (format nil "Using built-in MCP server definition for ~A."
                                   server-name)))
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
  (log-prefixed-message "MCP INFO" "Scanning for MCP configurations...")
  (let ((config (read-mcp-config)))
    (if config
       (let* ((servers nil)
              (entries nil)
             (raw-list (mcp-config-server-definitions config)))
         (log-prefixed-message "MCP INFO"
                               (format nil "Found ~A server definitions to initialize."
                                       (length raw-list)))
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
                  (log-prefixed-message "MCP ERROR"
                                        (format nil "Failed to start/initialize MCP server ~A: ~A"
                                                (mcp-startup-entry-name entry)
                                                (mcp-startup-entry-error-message entry))))
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
                        (log-prefixed-message "MCP ERROR"
                                              (format nil "Failed to start/initialize MCP server ~A: ~A"
                                                      name
                                                      (mcp-startup-entry-error-message entry))))))))
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
           (log-prefixed-message "MCP INFO"
                                 (format nil "Successfully fully initialized ~A out of ~A configured servers."
                                         (mcp-startup-status-successful-count status)
                                         (mcp-startup-status-configured-count status)))
           (when (> (mcp-startup-status-failed-count status) 0)
             (dolist (entry (remove-if #'mcp-startup-entry-success-p entries))
               (log-prefixed-message "MCP WARN"
                                     (format nil "Startup failure for ~A~:[~; (required)~]: ~A"
                                             (mcp-startup-entry-name entry)
                                             (mcp-startup-entry-required-p entry)
                                             (mcp-startup-entry-error-message entry)))))
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

(eval-when (:load-toplevel :execute)
  (maybe-auto-initialize-startup-chatbot))
