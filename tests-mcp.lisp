;;; tests-mcp.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-mcp-config-resolution
  (let ((*mcp-config-path* "test-mcp-config.lisp"))
    (fiveam:is (string= "test-mcp-config.lisp" (get-mcp-config-path)))))

(fiveam:test test-mcp-tool-translation
  (let ((mcp-tool '((:name . "calculate_sum")
                    (:description . "Adds two numbers")
                    (:input-schema . ((:type . "object")
                                      (:properties . ((:a . ((:type . "number")))
                                                      (:b . ((:type . "number"))))))))))
    (let ((openai-tool (translate-mcp-tool-to-openai mcp-tool)))
      (fiveam:is (string= "function" (cdr (assoc "type" openai-tool :test #'string=))))
      (let ((fn (cdr (assoc "function" openai-tool :test #'string=))))
        (fiveam:is (string= "calculate_sum" (cdr (assoc "name" fn :test #'string=))))
        (fiveam:is (string= "Adds two numbers" (cdr (assoc "description" fn :test #'string=))))))
    (let ((gemini-fn (translate-mcp-tool-to-gemini-fn mcp-tool)))
      (fiveam:is (string= "calculate_sum" (cdr (assoc "name" gemini-fn :test #'string=))))
      (fiveam:is (string= "Adds two numbers" (cdr (assoc "description" gemini-fn :test #'string=)))))))

(fiveam:test test-parse-mcp-server-def-supports-required-flag
  (multiple-value-bind (name command args required-p)
      (parse-mcp-server-def '(:name "required-server"
                             :command "sbcl"
                             :args ("--script" "server.lisp")
                             :required t))
    (fiveam:is (string= "required-server" name))
    (fiveam:is (string= "sbcl" command))
    (fiveam:is (equal '("--script" "server.lisp") args))
    (fiveam:is-true required-p))
  (multiple-value-bind (name command args required-p)
      (parse-mcp-server-def '("optional-server"
                              (:command "node")
                              (:args "server.js")
                              (:required nil)))
    (fiveam:is (string= "optional-server" name))
    (fiveam:is (string= "node" command))
    (fiveam:is (equal '("server.js") args))
    (fiveam:is-false required-p)))

(fiveam:test test-parse-mcp-server-def-supports-environment
  (multiple-value-bind (name command args required-p environment system-instruction)
     (parse-mcp-server-def '(:name "memory"
                            :command "npx"
                            :args ("-y" "@modelcontextprotocol/server-memory")
                            :env (("MEMORY_FILE_PATH" . "persona-memory.json"))
                            :system-instruction "memory tools"))
    (fiveam:is (string= "memory" name))
    (fiveam:is (string= "npx" command))
    (fiveam:is (equal '("-y" "@modelcontextprotocol/server-memory") args))
    (fiveam:is-false required-p)
    (fiveam:is (equal '(("MEMORY_FILE_PATH" . "persona-memory.json")) environment))
    (fiveam:is (string= "memory tools" system-instruction))))

(fiveam:test test-merge-mcp-server-environments-inherits-and-overrides
  (let ((merged (merge-mcp-server-environments
                 '("PATH=C:\\Windows\\System32" "MEMORY_FILE_PATH=default.json")
                 '(("MEMORY_FILE_PATH" . "persona.json")
                   ("HOME" . "C:\\Users\\bitdi")))))
    (fiveam:is (equal '(("PATH" . "C:\\Windows\\System32")
                        ("MEMORY_FILE_PATH" . "persona.json")
                        ("HOME" . "C:\\Users\\bitdi"))
                      merged))))

(fiveam:test test-mcp-tool-list-is-cached
  (let* ((server (make-instance 'mcp-server :name "cached-server"))
         (call-count 0))
    (let ((*mcp-send-request-function*
            (lambda (srv method params &key timeout)
              (declare (ignore srv params timeout))
              (when (string= method "tools/list")
                (incf call-count))
              '((:tools . (((:name . "cached_tool")
                            (:description . "Cached tool"))))))))
      (mcp-list-tools server)
      (mcp-list-tools server)
      (fiveam:is (= 1 call-count))
      (fiveam:is (mcp-server-tool-list-cache-valid-p server)))))

(fiveam:test test-mcp-tool-list-cache-invalidates-on-notification
  (let ((server (make-instance 'mcp-server :name "cached-server")))
    (setf (mcp-server-tool-list-cache server)
          '((:tools . (((:name . "cached_tool"))))))
    (setf (mcp-server-tool-list-cache-valid-p server) t)
    (mcp-handle-incoming server '((:jsonrpc . "2.0")
                                  (:method . "notifications/tools/list_changed")
                                  (:params . :null)))
    (fiveam:is-false (mcp-server-tool-list-cache-valid-p server))
    (fiveam:is-false (mcp-server-tool-list-cache server))))

(fiveam:test test-find-mcp-server-and-tool-uses-cached-tool-list
  (let* ((server (make-instance 'mcp-server :name "cached-server"))
         (bot (make-instance 'chatbot :mcp-servers (list server)))
         (call-count 0))
    (let ((*mcp-send-request-function*
            (lambda (srv method params &key timeout)
              (declare (ignore srv params timeout))
              (when (string= method "tools/list")
                (incf call-count))
              '((:tools . (((:name . "cached_tool")
                            (:description . "Cached tool"))))))))
      (get-all-mcp-tools bot)
      (find-mcp-server-and-tool bot "cached_tool")
      (fiveam:is (= 1 call-count)))))

(fiveam:test test-find-mcp-server-and-tool-signals-list-failure
  (let* ((server (make-instance 'mcp-server :name "cached-server"))
         (bot (make-instance 'chatbot :mcp-servers (list server))))
    (let ((*mcp-send-request-function*
            (lambda (srv method params &key timeout)
              (declare (ignore srv method params timeout))
              (error "tools/list failed"))))
      (fiveam:signals mcp-tool-lookup-error
        (find-mcp-server-and-tool bot "cached_tool")))))

(fiveam:test test-execute-mcp-tool-signals-execution-failure
  (let* ((server (make-instance 'mcp-server :name "cached-server"))
         )
    (let ((*mcp-call-tool-function*
            (lambda (srv name arguments)
              (declare (ignore srv name arguments))
              (error "tool call failed"))))
      (fiveam:signals mcp-tool-execution-error
        (execute-mcp-tool server "cached_tool" nil)))))

(fiveam:test test-execute-mcp-tool-signals-missing-text-content
  (let* ((server (make-instance 'mcp-server :name "cached-server"))
         )
    (let ((*mcp-call-tool-function*
            (lambda (srv name arguments)
              (declare (ignore srv name arguments))
              '((:content . (((:type . "image"))))))))
      (fiveam:signals mcp-tool-execution-error
        (execute-mcp-tool server "cached_tool" nil)))))

(fiveam:test test-initialize-mcp-servers-records-partial-failure-status
  (let* ((bot (make-instance 'chatbot))
         )
    (let ((*read-mcp-config-function*
            (lambda ()
              '((:name "healthy-server" :command "sbcl" :args ("--script" "healthy.lisp"))
                (:name "optional-failure" :command "sbcl" :args ("--script" "broken.lisp")))))
          (*start-mcp-server-function*
            (lambda (name command args)
              (declare (ignore command args))
              (if (string= name "optional-failure")
                  (error "launch failed")
                  (make-instance 'mcp-server :name name))))
          (*mcp-initialize-function* (lambda (server) server)))
      (let ((status (initialize-mcp-servers-for-chatbot bot)))
        (fiveam:is (= 2 (mcp-startup-status-configured-count status)))
        (fiveam:is (= 1 (mcp-startup-status-successful-count status)))
        (fiveam:is (= 1 (mcp-startup-status-failed-count status)))
        (fiveam:is (= 0 (mcp-startup-status-required-failed-count status)))
        (fiveam:is-true (mcp-startup-status-partial-failure-p status))
        (fiveam:is (= 1 (length (chatbot-mcp-servers bot))))
        (fiveam:is (string= "healthy-server"
                            (mcp-server-name (car (chatbot-mcp-servers bot)))))
        (fiveam:is (eq status (chatbot-mcp-startup-status bot)))))))

(fiveam:test test-initialize-mcp-servers-signals-required-failure-in-strict-mode
  (let* ((bot (make-instance 'chatbot))
         (captured-status nil))
    (let ((*read-mcp-config-function*
            (lambda ()
              '((:name "required-failure"
                 :command "sbcl"
                 :args ("--script" "broken.lisp")
                 :required t))))
          (*start-mcp-server-function*
            (lambda (name command args)
              (declare (ignore name command args))
              (error "launch failed")))
          (*mcp-initialize-function* (lambda (server) server)))
      (handler-case
          (progn
            (initialize-mcp-servers-for-chatbot bot :strict-required-p t)
            (fiveam:fail "Expected required MCP startup failure."))
        (mcp-startup-error (condition)
          (setf captured-status (mcp-startup-error-status condition)))))
    (fiveam:is (typep captured-status 'mcp-startup-status))
    (fiveam:is (= 1 (mcp-startup-status-failed-count captured-status)))
    (fiveam:is (= 1 (mcp-startup-status-required-failed-count captured-status)))
    (fiveam:is (eq captured-status (chatbot-mcp-startup-status bot)))))

(fiveam:test test-mcp-end-to-end
  (let* ((mock-server-path (merge-pathnames "mock-mcp-server.lisp" (uiop:getcwd)))
        (server (start-mcp-server "test-server" "sbcl" (list "--script" (namestring mock-server-path)))))
    (unwind-protect
         (progn
           (let ((init-resp (mcp-initialize server)))
             (fiveam:is (not (null init-resp)))
             (fiveam:is (string= "2024-11-05" (mcp-val :protocol-version init-resp)))
             (fiveam:is (string= "mock-server" (mcp-val :name (mcp-val :server-info init-resp)))))
           (let* ((list-resp (mcp-list-tools server))
                  (tools (mcp-val :tools list-resp)))
             (fiveam:is (= 1 (length tools)))
             (let ((tool (car tools)))
               (fiveam:is (string= "echo_tool" (mcp-val :name tool)))
               (fiveam:is (string= "Echoes input" (mcp-val :description tool)))))
           (let* ((call-resp (mcp-call-tool server "echo_tool" '((:input . "Hello MCP world"))))
                  (content (mcp-val :content call-resp))
                  (first-item (car content)))
             (fiveam:is (string= "text" (mcp-val :type first-item)))
             (fiveam:is (string= "Echo: Hello MCP world" (mcp-val :text first-item)))))
      (stop-mcp-server server))))
