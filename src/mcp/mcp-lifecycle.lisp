;;; -*- Lisp -*-
;;; mcp-lifecycle.lisp - Model Context Protocol (MCP) server process and lifecycle management

(in-package "CHATBOT")

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

(defun explicit-command-path-p (command)
  "Returns true when COMMAND already names an explicit path."
  (or (find #\/ command)
      (find #\\ command)
      (and (> (length command) 1)
           (char= #\: (char command 1)))))

(defun split-windows-path-variable (value)
  "Splits a Windows PATH-like VALUE into non-empty components."
  (remove ""
          (uiop:split-string (or value "") :separator '(#\;))
          :test #'string=))

(defun windows-command-extension-candidates (command &optional pathext)
  "Returns executable-name candidates for COMMAND using PATHEXT."
  (let ((extensions (split-windows-path-variable
                    (or pathext
                        (uiop:getenv "PATHEXT")
                        ".COM;.EXE;.BAT;.CMD"))))
    (if (pathname-type (pathname command))
        (list command)
        (cons command
             (mapcar (lambda (extension)
                       (concatenate 'string command (string-downcase extension)))
                     extensions)))))

(defun resolve-command-from-search-path (command &key path pathext)
  "Returns an executable pathname for COMMAND when it can be found on PATH."
  (let ((directories (split-windows-path-variable (or path (uiop:getenv "PATH")))))
    (some (lambda (directory)
           (let ((base-directory (uiop:ensure-directory-pathname directory)))
             (some (lambda (candidate)
                     (probe-file (merge-pathnames candidate base-directory)))
                   (windows-command-extension-candidates command pathext))))
          directories)))

(defun resolve-mcp-launch-command (command &key path pathext)
  "Returns the best launchable command path for COMMAND in the current environment."
  (if (or (not (uiop:os-windows-p))
          (explicit-command-path-p command))
      command
      (let ((resolved (resolve-command-from-search-path command :path path :pathext pathext)))
        (if resolved
           (namestring resolved)
           command))))

(defun default-start-mcp-server (name command args &optional environment)
  "Launches an MCP server subprocess and starts its reader thread."
  (let* ((resolved-command (resolve-mcp-launch-command command))
         (launch-options (list :input :stream
                              :output :stream
                              :error-output :stream))
         (normalized-environment
           (normalize-mcp-server-environment
            (if environment
                (merge-mcp-server-environments (current-process-environment)
                                               environment)
                nil)))
         (log-command (if (string= resolved-command command)
                         command
                         (format nil "~A (resolved from ~A)" resolved-command command)))
         (process-info (apply #'uiop:launch-program
                             (cons (cons resolved-command args)
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
    (log-prefixed-message "MCP INFO" (format nil "Launching server ~A" name))
    (log-prefixed-message "MCP DEBUG" (format nil "Command: ~A ~A" log-command args))
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
