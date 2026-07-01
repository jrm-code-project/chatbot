;;; -*- Lisp -*-
;;; mcp-config.lisp - MCP configuration discovery and definition loading

(in-package "CHATBOT")

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
