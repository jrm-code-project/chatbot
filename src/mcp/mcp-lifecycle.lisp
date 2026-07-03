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

(defun mcp-environment-key= (left right)
  "Returns true when LEFT and RIGHT name the same environment key."
  (string-equal (string left)
               (string right)))

(defun merge-mcp-server-environments (&rest environments)
  "Merges ENVIRONMENTS, letting later entries override earlier ones by key."
  (flet ((merge-entry (merged entry)
           (let* ((normalized-entry (mcp-environment-entry->cons entry))
                 (key (car normalized-entry)))
            (if (assoc key merged :test #'mcp-environment-key=)
                (mapcar (lambda (existing)
                          (if (mcp-environment-key= (car existing) key)
                              normalized-entry
                              existing))
                        merged)
                (append merged (list normalized-entry))))))
    (reduce (lambda (merged environment)
              (reduce #'merge-entry
                      environment
                      :initial-value merged))
            environments
            :initial-value nil)))

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
        (append (mapcar (lambda (extension)
                         (concatenate 'string command (string-downcase extension)))
                       extensions)
               (list command)))))

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

(defun resolve-mcp-launch-args (args)
  "Returns launch ARGS with repository-local test fixtures normalized."
  (mapcar (lambda (arg)
           (if (and (stringp arg) (search "mock-mcp-server.lisp" arg))
               (namestring (merge-pathnames "mock-mcp-server.lisp"
                                            (asdf:system-source-directory :chatbot)))
               arg))
         args))

(defun mcp-launch-environment (environment)
  "Returns the normalized subprocess environment for ENVIRONMENT."
  (normalize-mcp-server-environment
   (if environment
      (merge-mcp-server-environments (current-process-environment)
                                     environment)
      nil)))

(defun mcp-launch-options (environment)
  "Returns UIOP launch options for one MCP subprocess."
  (let ((base-options (list :input :stream
                           :output :stream
                           :error-output :stream)))
    (if environment
       (append base-options
               (list :environment environment))
       base-options)))

(defun launch-mcp-server-process (command args environment)
  "Launches one MCP subprocess with COMMAND, ARGS, and ENVIRONMENT."
  (apply #'uiop:launch-program
        (cons (cons command args)
              (mcp-launch-options environment))))

(defun make-mcp-server-from-process (name process-info)
  "Returns an MCP server wrapper around PROCESS-INFO for NAME."
  (make-instance 'mcp-server
                :name name
                :process process-info
                :input-stream (uiop:process-info-input process-info)
                :output-stream (uiop:process-info-output process-info)
                :error-stream (uiop:process-info-error-output process-info)))

(defun spawn-mcp-server-thread (thunk name)
  "Starts one MCP supervision thread named NAME running THUNK."
  (sb-thread:make-thread thunk :name name))

(defun start-mcp-server-supervision (server)
  "Starts stderr and reader supervision threads for SERVER."
  (let ((name (mcp-server-name server))
       (err-output (mcp-server-error-stream server)))
    (setf (mcp-server-stderr-thread server)
         (spawn-mcp-server-thread
          (lambda ()
            (handler-case
                (loop for line = (read-line err-output nil :eof)
                      until (eq line :eof)
                      do (log-prefixed-message (format nil "MCP ~A STDERR" name) line))
              (error (e)
                (log-prefixed-message "MCP DEBUG"
                                      (format nil "~A STDERR thread terminated: ~A" name e)))))
          (concatenate 'string "mcp-err-" name)))
    (setf (mcp-server-reader-thread server)
         (spawn-mcp-server-thread
          (lambda () (mcp-reader-loop server))
          (concatenate 'string "mcp-reader-" name)))
    server))

(defun default-start-mcp-server (name command args &optional environment)
  "Launches an MCP server subprocess and starts its reader thread."
  (let* ((resolved-args (resolve-mcp-launch-args args))
        (resolved-command (resolve-mcp-launch-command command))
        (normalized-environment (mcp-launch-environment environment))
        (log-command (if (string= resolved-command command)
                         command
                         (format nil "~A (resolved from ~A)" resolved-command command))))
    (log-prefixed-message "MCP INFO" (format nil "Launching server ~A" name))
    (log-prefixed-message "MCP DEBUG" (format nil "Command: ~A ~A" log-command args))
    (let ((server nil))
      (handler-case
         (progn
           (setf server
                 (make-mcp-server-from-process
                  name
                  (launch-mcp-server-process resolved-command
                                             resolved-args
                                             normalized-environment)))
           (start-mcp-server-supervision server)
           (log-prefixed-message "MCP INFO"
                                 (format nil "Server ~A process and threads started successfully."
                                         name))
           server)
       (error (e)
         (when server
           (ignore-errors (default-stop-mcp-server server)))
         (error e))))))

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

(defun wait-for-mcp-server-thread-shutdown (thread)
  "Waits briefly for THREAD to exit after stream/process shutdown."
  (when (and thread (sb-thread:thread-alive-p thread))
    (handler-case
        (sb-thread:join-thread thread :timeout 1.0)
      (sb-thread:join-thread-error ()
        nil)))
  (and thread
       (sb-thread:thread-alive-p thread)))

(defun stop-mcp-server-thread (thread name role)
  "Stops one MCP THREAD for server NAME with ROLE metadata."
  (when thread
    (when (wait-for-mcp-server-thread-shutdown thread)
      (log-prefixed-message "MCP WARN"
                           (format nil "Force-terminating ~A thread for server ~A."
                                   role
                                   name))
      (sb-thread:terminate-thread thread))
    nil))

(defun stop-mcp-server-process (process name)
  "Stops PROCESS for server NAME, attempting graceful termination before escalation."
  (when process
    (handler-case
        (progn
          (when (uiop:process-alive-p process)
           (uiop:terminate-process process :urgent nil)
           (sleep 0.1)
           (when (uiop:process-alive-p process)
             (log-prefixed-message "MCP WARN"
                                   (format nil "Force-terminating MCP server process for ~A."
                                           name))
             (uiop:terminate-process process :urgent t)))
          (uiop:wait-process process))
      (error (e)
        (log-prefixed-message "MCP WARN"
                             (format nil "Failed to stop MCP server process for ~A cleanly: ~A"
                                     name
                                     e))))
    nil))

(defun default-stop-mcp-server (server)
  "Stops the MCP server process and reader thread cleanly."
  (log-prefixed-message "MCP INFO"
                       (format nil "Stopping server ~A" (mcp-server-name server)))
  (let ((name (mcp-server-name server))
        (reader-thread (mcp-server-reader-thread server))
        (stderr-thread (mcp-server-stderr-thread server))
        (proc (mcp-server-process server))
        (input-stream (mcp-server-input-stream server))
        (output-stream (mcp-server-output-stream server))
        (error-stream (mcp-server-error-stream server)))
    (close-mcp-server-stream input-stream)
    (close-mcp-server-stream output-stream)
    (close-mcp-server-stream error-stream)
    (stop-mcp-server-thread reader-thread name "reader")
    (stop-mcp-server-thread stderr-thread name "stderr")
    (stop-mcp-server-process proc name)
    (setf (mcp-server-reader-thread server) nil
          (mcp-server-stderr-thread server) nil
          (mcp-server-process server) nil
          (mcp-server-input-stream server) nil
          (mcp-server-output-stream server) nil
          (mcp-server-error-stream server) nil)))

(defun stop-mcp-server (server)
  "Stops an MCP server, honoring the configured test seam when present."
  (if *stop-mcp-server-function*
      (funcall *stop-mcp-server-function* server)
      (default-stop-mcp-server server)))
