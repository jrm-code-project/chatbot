;;; tests-mcp.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(defun read-test-file-octets-as-string (path)
  "Reads PATH as octets and returns an ASCII string for newline-sensitive assertions."
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets nil)
          (eof-marker (gensym "EOF")))
      (loop for octet = (read-byte stream nil eof-marker)
            until (eq octet eof-marker)
            do (push octet octets))
      (coerce (mapcar #'code-char (nreverse octets)) 'string))))

(defun read-test-lisp-form (path)
  "Reads the first Lisp form stored at PATH."
  (with-open-file (stream path :direction :input)
    (read stream nil nil)))

(defun test-subordinate-control-response (&key reply spawn)
  "Returns one strict subordinate control JSON string for tests."
  (format nil
          "{\"reply\":~A,\"spawn\":~A}"
          (cl-json:encode-json-to-string reply)
          (if spawn
              (cl-json:encode-json-to-string spawn)
              "null")))

(fiveam:test test-parse-subordinate-control-response-accepts-provider-wrapper
  (let* ((payload "<antml:thinking>draft</antml:thinking>{\"reply\":\"done\",\"spawn\":null}")
         (control (parse-subordinate-control-response payload)))
    (fiveam:is (string= "done" (getf control :reply)))
    (fiveam:is (null (getf control :spawn)))))

(defun test-openai-subordinate-stream (reply &key spawn)
  "Returns one OpenAI-compatible SSE stream whose content is a structured subordinate reply."
  (format nil
          "data: ~A~%data: [DONE]"
          (cl-json:encode-json-to-string
           `(("choices" . ,(vector `(("delta" . (("content" . ,(test-subordinate-control-response
                                                                 :reply reply
                                                                 :spawn spawn)))))))))))

(defun test-gemini-subordinate-stream (reply &key spawn (interaction-id "session-1") (model "gemini-3.5-flash"))
  "Returns one Gemini-compatible SSE stream whose text is a structured subordinate reply."
  (format nil
          "data: ~A~%data: ~A~%data: ~A"
          (cl-json:encode-json-to-string
           `(("event_type" . "interaction.created")
             ("interaction" . (("id" . ,interaction-id)))))
          (cl-json:encode-json-to-string
           `(("event_type" . "step.delta")
             ("delta" . (("type" . "text")
                         ("text" . ,(test-subordinate-control-response
                                     :reply reply
                                     :spawn spawn))))))
          (cl-json:encode-json-to-string
           `(("event_type" . "interaction.completed")
             ("interaction" . (("id" . ,interaction-id)
                               ("model" . ,model)
                               ("usage" . (("total_input_tokens" . 1)
                                           ("total_output_tokens" . 1)
                                           ("total_tokens" . 2)))))))))

(defun make-grounding-search-response (&key total-results items)
  "Builds a mock GOOGLE search response hash table."
  (let ((response (make-hash-table :test #'eql))
        (search-info (make-hash-table :test #'eql)))
    (setf (gethash :total-results search-info) (or total-results "0"))
    (setf (gethash :search-information response) search-info)
    (setf (gethash :items response)
          (coerce
           (mapcar (lambda (item)
                     (let ((entry (make-hash-table :test #'eql)))
                       (setf (gethash :title entry) (getf item :title))
                       (setf (gethash :link entry) (getf item :link))
                       (setf (gethash :snippet entry) (getf item :snippet))
                       entry))
                   items)
           'vector))
    response))

(fiveam:test test-mcp-config-resolution
  (let ((context (make-runtime-context :mcp-config-path "test-mcp-config.lisp")))
    (call-with-runtime-context
     context
     (lambda ()
       (fiveam:is (string= "test-mcp-config.lisp" (get-mcp-config-path)))))))

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

(fiveam:test test-openai-tool-translation-normalizes-parameter-properties-to-objects
  (let* ((bot (make-instance 'chatbot
                             :model "gpt-4o"
                             :filesystem-tools-p t
                             :enable-eval-p t
                             :system-instruction-path #p"persona/system-instruction.md"))
         (tools (openai-request-tools bot)))
    (dolist (tool tools)
      (let* ((function (cdr (assoc "function" tool :test #'string=)))
             (parameters (cdr (assoc "parameters" function :test #'string=)))
             (properties (and parameters
                              (typecase parameters
                                (hash-table (gethash "properties" parameters))
                                (list (cdr (assoc "properties" parameters :test #'string=)))
                                (t nil)))))
        (fiveam:is (or (null properties)
                       (hash-table-p properties)
                       (json-object-alist-p properties)))))))

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

(fiveam:test test-initialize-configured-mcp-server-falls-back-to-built-in-memory-definition
  (let ((captured-name nil)
        (captured-command nil)
        (captured-args nil)
        (captured-environment nil))
    (let ((*read-mcp-config-function* (lambda () nil))
          (*start-mcp-server-function*
            (lambda (name command args &optional environment)
              (setf captured-name name)
              (setf captured-command command)
              (setf captured-args args)
              (setf captured-environment environment)
              (make-instance 'mcp-server :name name)))
          (*mcp-initialize-function* (lambda (server) server)))
      (let ((server (initialize-configured-mcp-server
                     "memory"
                     :environment '(("MEMORY_FILE_PATH" . "persona-memory.json")))))
        (fiveam:is (string= "memory" (mcp-server-name server)))
        (fiveam:is (string= "memory" captured-name))
        (fiveam:is (search "npx" (string-downcase captured-command)))
        (fiveam:is (equal '("-y" "@modelcontextprotocol/server-memory") captured-args))
        (fiveam:is (equal '(("MEMORY_FILE_PATH" . "persona-memory.json"))
                          captured-environment))))))

(fiveam:test test-merge-mcp-server-environments-inherits-and-overrides
  (let ((merged (merge-mcp-server-environments
                 '("PATH=C:\\Windows\\System32" "MEMORY_FILE_PATH=default.json")
                 '(("MEMORY_FILE_PATH" . "persona.json")
                   ("HOME" . "C:\\Users\\bitdi")))))
    (fiveam:is (equal '(("PATH" . "C:\\Windows\\System32")
                        ("MEMORY_FILE_PATH" . "persona.json")
                        ("HOME" . "C:\\Users\\bitdi"))
                      merged))))

(fiveam:test test-resolve-mcp-launch-command-finds-windows-pathext-script
  (let* ((temp-dir (uiop:default-temporary-directory))
         (bin-dir (merge-pathnames "mock-mcp-bin/" temp-dir))
         (npx-path (merge-pathnames "npx.cmd" bin-dir)))
    (ensure-directories-exist npx-path)
    (with-open-file (stream npx-path :direction :output :if-exists :supersede)
      (write-line "@echo off" stream))
    (unwind-protect
         (fiveam:is (equal (namestring npx-path)
                          (resolve-mcp-launch-command "npx"
                                                      :path (namestring bin-dir)
                                                      :pathext ".CMD;.EXE")))
      (uiop:delete-directory-tree bin-dir :validate t))))

(fiveam:test test-resolve-mcp-launch-command-prefers-pathext-over-extensionless-shim
  (let* ((temp-dir (uiop:default-temporary-directory))
         (bin-dir (merge-pathnames "mock-mcp-bin-prefers-ext/" temp-dir))
         (bare-path (merge-pathnames "npx" bin-dir))
         (cmd-path (merge-pathnames "npx.cmd" bin-dir)))
    (ensure-directories-exist cmd-path)
    (with-open-file (stream bare-path :direction :output :if-exists :supersede)
      (write-line "shell shim" stream))
    (with-open-file (stream cmd-path :direction :output :if-exists :supersede)
      (write-line "@echo off" stream))
    (unwind-protect
         (fiveam:is (equal (namestring cmd-path)
                          (resolve-mcp-launch-command "npx"
                                                      :path (namestring bin-dir)
                                                      :pathext ".CMD;.EXE")))
      (uiop:delete-directory-tree bin-dir :validate t))))

(fiveam:test test-resolve-mcp-launch-command-preserves-explicit-paths
  (fiveam:is (equal "C:\\tools\\npx.cmd"
                   (resolve-mcp-launch-command "C:\\tools\\npx.cmd"
                                               :path "C:\\ignored"
                                               :pathext ".CMD"))))

(fiveam:test test-mcp-request-json-encodes-tool-arguments-as-records
  (let* ((payload `((:jsonrpc . "2.0")
                   (:id . 1)
                    (:method . "tools/call")
                    (:params . ((:name . "add_observations")
                                (:arguments . ((:observations ((:entityName . "Boss")
                                                               (:contents "watching")))))))))
         (json (decode-test-json
                (cl-json:encode-json-to-string (json-encodable-value payload))))
         (params (test-json-value-any json '("params" :params)))
         (arguments (test-json-value-any params '("arguments" :arguments)))
         (observations (test-json-elements
                        (test-json-value-any arguments '("observations" :observations))))
         (first-observation (first observations))
         (contents (test-json-elements
                    (test-json-value-any first-observation '("contents" :contents)))))
    (assert-json-field= params "name" "add_observations")
    (fiveam:is (= 1 (length observations)))
    (assert-json-field= first-observation "entityName" "Boss")
    (fiveam:is (equal '("watching") contents))))

(fiveam:test test-mcp-debug-logging-includes-tool-call-name
  (let* ((server (make-instance 'mcp-server
                                :name "mock-server"
                                :input-stream (make-string-output-stream)))
         (worker (sb-thread:make-thread
                  (lambda ()
                    (loop for mailbox = (gethash 1 (mcp-server-pending-requests server))
                          until mailbox
                          do (sleep 0.01)
                          finally (sb-concurrency:send-message
                                   mailbox
                                   '((:result . "ok"))))))))
    (unwind-protect
         (let ((output (with-output-to-string (s)
                         (let ((context (make-runtime-context :logging-enabled-p t
                                                              :log-level :info
                                                              :log-stream s))
                               (*mcp-debug-p* t))
                           (call-with-runtime-context
                            context
                            (lambda ()
                              (fiveam:is (equal '((:result . "ok"))
                                                (default-mcp-send-request
                                                 server
                                                 "tools/call"
                                                 '((:name . "echo_tool")
                                                   (:arguments . ((:value . "payload")))))))))))))
           (fiveam:is (search "Request ID 1: tools/call (echo_tool)" output))
           (fiveam:is (search "Response ID 1 received (echo_tool)" output)))
      (sb-thread:join-thread worker))))

(fiveam:test test-execute-chatbot-tool-read-file-lines
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-root/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Line one" s)
      (write-line "Line two" s)
      (write-line "Line three" s))
    (unwind-protect
         (fiveam:is (string= (format nil "Line two~%Line three")
                             (execute-chatbot-tool bot
                                                   :built-in
                                                   "readFileLines"
                                                   '(("filename" . "notes.txt")
                                                     ("beginningLine" . 2)
                                                     ("endingLine" . 3)))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-by-name-dispatches-mcp-tools
  (let ((*find-mcp-server-and-tool-function*
         (lambda (bot tool-name)
           (declare (ignore bot))
           (values :mock-server `((:name . ,tool-name)))))
       (*execute-mcp-tool-function*
         (lambda (server tool-name arguments)
           (declare (ignore server))
           (fiveam:is (string= "echo_tool" tool-name))
           (fiveam:is (string= "payload" (cdr (assoc :value arguments))))
           "tool result")))
    (let ((bot (conversation-chatbot (new-chat :backend :openai))))
      (fiveam:is (string= "tool result"
                         (execute-chatbot-tool-by-name bot
                                                       "echo_tool"
                                                       '((:value . "payload"))))))))

(fiveam:test test-execute-chatbot-tool-by-name-errors-when-tool-is-missing
  (let ((*find-mcp-server-and-tool-function*
         (lambda (bot tool-name)
           (declare (ignore bot tool-name))
           (values nil nil))))
    (let ((bot (conversation-chatbot (new-chat :backend :openai))))
      (fiveam:signals error
       (execute-chatbot-tool-by-name bot
                                     "missing_tool"
                                     '())))))

(fiveam:test test-map-chatbot-json-tool-call-results-error-builder-captures-tool-errors
  (let ((*find-mcp-server-and-tool-function*
         (lambda (bot tool-name)
           (declare (ignore bot tool-name))
           (values nil nil))))
    (let* ((bot (conversation-chatbot (new-chat :backend :openai)))
           (tool-calls (list '((:id . "call-1")
                               (:name . "missing_tool")
                               (:arguments . "{}"))))
           (results
             (map-chatbot-json-tool-call-results
              bot
              tool-calls
              (lambda (name tool-call)
                (declare (ignore name tool-call))
                "MCP tool execution test")
              (lambda (id name arguments-json res-text tool-call)
                (declare (ignore id name arguments-json res-text tool-call))
                (error "Result builder should not run for failing tools."))
              :error-builder
              (lambda (id name arguments-json condition tool-call)
                (declare (ignore tool-call))
                (list id name arguments-json (chatbot-tool-error-message condition))))))
      (fiveam:is (equal '(("call-1" "missing_tool" "{}" "Tool not found: missing_tool"))
                        results)))))

(fiveam:test test-execute-chatbot-tool-by-name-json-arguments-parses-before-execution
  (let ((*find-mcp-server-and-tool-function*
         (lambda (bot tool-name)
           (declare (ignore bot))
           (values :mock-server `((:name . ,tool-name)))))
       (*execute-mcp-tool-function*
         (lambda (server tool-name arguments)
           (declare (ignore server))
           (fiveam:is (string= "echo_tool" tool-name))
           (fiveam:is (string= "payload" (cdr (assoc :value arguments))))
           "json tool result")))
    (let ((bot (conversation-chatbot (new-chat :backend :openai))))
      (fiveam:is (string= "json tool result"
                         (execute-chatbot-tool-by-name-json-arguments
                          bot
                          "echo_tool"
                          "{\"value\":\"payload\"}"
                          "MCP helper test"))))))

(fiveam:test test-execute-chatbot-tool-by-name-json-arguments-allows-empty-object-for-no-arg-builtins
  (let* ((bot (make-instance 'chatbot
                           :backend :gemini
                           :system-instruction #("Paragraph one." "Paragraph two.")
                           :system-instruction-path #p"C:/Users/bitdi/.Personas/Test/system-instructions.md"
                           :system-instruction-storage-kind :markdown-file))
        (payload (cl-json:decode-json-from-string
                  (execute-chatbot-tool-by-name-json-arguments
                   bot
                   "readSystemInstructions"
                   ""
                   "Empty built-in tool arguments"))))
    (fiveam:is (= 2 (cdr (assoc :count payload))))
    (fiveam:is (equal '("Paragraph one." "Paragraph two.")
                     (coerce (cdr (assoc :paragraphs payload)) 'list)))))

(fiveam:test test-built-in-sampling-parameter-tools-update-runtime-state
  (let* ((bot (make-instance 'chatbot
                          :backend :gemini
                          :temperature 0.2d0
                          :top-p 0.3d0))
        (initial (execute-chatbot-tool-by-name bot "readSamplingParameters" '()))
        (updated (execute-chatbot-tool-by-name bot
                                               "setSamplingParameters"
                                               '(("temperature" . 0.8d0)
                                                 ("topP" . 0.9d0))))
        (reset (execute-chatbot-tool-by-name bot "resetSamplingParameters" '())))
    (assert-sampling-parameters initial :temperature 0.2d0 :top-p 0.3d0)
    (assert-sampling-parameters updated :temperature 0.8d0 :top-p 0.9d0 :saved t)
    (assert-sampling-parameters reset :temperature nil :top-p nil :saved t)))

(fiveam:test test-execute-chatbot-tool-by-name-normalizes-mcp-argument-keys-from-schema
  (let ((*find-mcp-server-and-tool-function*
        (lambda (bot tool-name)
          (declare (ignore bot))
          (values :mock-server
                  `((:name . ,tool-name)
                    (:input-schema . ((:type . "object")
                                      (:properties . (("observations" . ((:type . "array")
                                                                         (:items . ((:type . "object")
                                                                                    (:properties . (("entityName" . ((:type . "string")))
                                                                                                    ("contents" . ((:type . "array")
                                                                                                                   (:items . ((:type . "string")))))))))))))))))))
       (*execute-mcp-tool-function*
        (lambda (server tool-name arguments)
          (declare (ignore server))
          (fiveam:is (string= "add_observations" tool-name))
          (let* ((observations (mcp-val "observations" arguments))
                 (first-observation (first observations)))
            (fiveam:is (string= "Boss" (mcp-val "entityName" first-observation)))
            (fiveam:is (equal '("watching") (mcp-val "contents" first-observation))))
          "normalized")))
    (let ((bot (conversation-chatbot (new-chat :backend :openai))))
      (fiveam:is (string= "normalized"
                        (execute-chatbot-tool-by-name
                         bot
                         "add_observations"
                         '((:observations . (((:entityname . "Boss")
                                              (:contents . ("watching"))))))))))))

(fiveam:test test-map-chatbot-json-tool-call-results-preserves-order
  (let* ((executions nil)
        (*find-mcp-server-and-tool-function*
         (lambda (bot tool-name)
           (declare (ignore bot))
           (values :mock-server `((:name . ,tool-name)))))
        (*execute-mcp-tool-function*
         (lambda (server tool-name arguments)
           (declare (ignore server))
           (push (list tool-name (cdr (assoc :value arguments))) executions)
           (format nil "~A=>~A" tool-name (cdr (assoc :value arguments))))))
    (let* ((bot (conversation-chatbot (new-chat :backend :openai)))
          (tool-calls
            (list (list (cons :id "call-1")
                        (cons :name "first_tool")
                        (cons :arguments "{\"value\":\"alpha\"}"))
                  (list (cons :id "call-2")
                        (cons :name "second_tool")
                        (cons :arguments "{\"value\":\"beta\"}"))))
          (results
            (map-chatbot-json-tool-call-results
             bot
             tool-calls
             (lambda (name tool-call)
               (declare (ignore tool-call))
               (format nil "Tool arguments for ~A" name))
             (lambda (id name arguments-json res-text tool-call)
               (declare (ignore tool-call))
               (list id name arguments-json res-text)))))
      (fiveam:is (equal '(("call-1" "first_tool" "{\"value\":\"alpha\"}" "first_tool=>alpha")
                         ("call-2" "second_tool" "{\"value\":\"beta\"}" "second_tool=>beta"))
                       results))
      (fiveam:is (equal '(("first_tool" "alpha")
                         ("second_tool" "beta"))
                       (nreverse executions))))))

(fiveam:test test-map-chatbot-json-tool-call-results-can-report-errors
  (let* ((executions nil)
        (*find-mcp-server-and-tool-function*
         (lambda (bot tool-name)
           (declare (ignore bot))
           (values :mock-server `((:name . ,tool-name)))))
        (*execute-mcp-tool-function*
         (lambda (server tool-name arguments)
           (declare (ignore server))
           (push (list tool-name (cdr (assoc :value arguments))) executions)
           (if (string= tool-name "first_tool")
               (error 'mcp-tool-execution-error
                      :tool-name tool-name
                      :reason "mock failure")
               (format nil "~A=>~A" tool-name (cdr (assoc :value arguments)))))))
    (let* ((bot (conversation-chatbot (new-chat :backend :openai)))
           (tool-calls
             (list (list (cons :id "call-1")
                         (cons :name "first_tool")
                         (cons :arguments "{\"value\":\"alpha\"}"))
                   (list (cons :id "call-2")
                         (cons :name "second_tool")
                         (cons :arguments "{\"value\":\"beta\"}"))))
           (results
             (map-chatbot-json-tool-call-results
              bot
              tool-calls
              (lambda (name tool-call)
                (declare (ignore tool-call))
                (format nil "Tool arguments for ~A" name))
              (lambda (id name arguments-json res-text tool-call)
                (declare (ignore tool-call))
                (list id name arguments-json res-text))
              :error-builder
              (lambda (id name arguments-json condition tool-call)
                (declare (ignore tool-call))
                (list id name arguments-json (chatbot-tool-error-payload name condition))))))
      (fiveam:is (equal '(("call-1" "first_tool" "{\"value\":\"alpha\"}"
                          (("type" . "tool_error")
                           ("toolName" . "first_tool")
                           ("message" . "mock failure")))
                         ("call-2" "second_tool" "{\"value\":\"beta\"}" "second_tool=>beta"))
                       results))
      (fiveam:is (equal '(("first_tool" "alpha")
                         ("second_tool" "beta"))
                       (nreverse executions))))))

(fiveam:test test-execute-chatbot-tool-read-file-lines-rejects-invalid-range
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-root-invalid/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Line one" s))
    (unwind-protect
         (fiveam:signals mcp-tool-execution-error
           (execute-chatbot-tool bot
                                 :built-in
                                 "readFileLines"
                                 '(("filename" . "notes.txt")
                                   ("beginningLine" . 2)
                                   ("endingLine" . 1))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-read-file-lines-truncates-ending-line-past-eof
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-root-truncated/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root))
         (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Line one" s)
      (write-line "Line two" s)
      (write-line "Line three" s))
    (unwind-protect
         (fiveam:is (string= (format nil "Line two~%Line three")
                            (execute-chatbot-tool bot
                                                  :built-in
                                                  "readFileLines"
                                                  '(("filename" . "notes.txt")
                                                    ("beginningLine" . 2)
                                                    ("endingLine" . 100)))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-read-file-lines-rejects-out-of-scope-path
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-root-scope/" temp-dir))
         (outside-file (merge-pathnames "outside.txt" temp-dir))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s outside-file :direction :output :if-exists :supersede)
      (write-line "Outside" s))
    (unwind-protect
         (let ((*filesystem-access-approval-function* (lambda (&rest ignored)
                                                       (declare (ignore ignored))
                                                       nil)))
           (fiveam:signals mcp-tool-execution-error
             (execute-chatbot-tool bot
                                  :built-in
                                  "readFileLines"
                                  `(("filename" . ,(namestring outside-file))
                                    ("beginningLine" . 1)
                                    ("endingLine" . 1)))))
      (delete-file outside-file)
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-directory
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-directory-root/" temp-dir))
        (nested (merge-pathnames "docs/" root))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist nested)
    (with-open-file (s (merge-pathnames "alpha.txt" nested) :direction :output :if-exists :supersede)
      (write-line "Alpha" s))
    (with-open-file (s (merge-pathnames "beta.txt" nested) :direction :output :if-exists :supersede)
      (write-line "Beta" s))
    (with-open-file (s (merge-pathnames "notes.md" nested) :direction :output :if-exists :supersede)
      (write-line "Notes" s))
    (ensure-directories-exist (merge-pathnames "subdir/" nested))
    (unwind-protect
        (fiveam:is (equal '("docs/alpha.txt" "docs/beta.txt")
                          (coerce (cl-json:decode-json-from-string
                                   (execute-chatbot-tool bot
                                                         :built-in
                                                         "directory"
                                                         '(("pathname" . "docs")
                                                           ("pattern" . "*.txt"))))
                                  'list)))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-directory-rejects-missing-directory
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-directory-missing/" temp-dir))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
        (fiveam:signals mcp-tool-execution-error
          (execute-chatbot-tool bot
                                :built-in
                                "directory"
                                '(("pathname" . "docs")
                                  ("pattern" . "*.txt"))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-directory-rejects-non-directory-target
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-directory-file-target/" temp-dir))
        (file-path (merge-pathnames "notes.txt" root))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Not a directory" s))
    (unwind-protect
        (fiveam:signals mcp-tool-execution-error
          (execute-chatbot-tool bot
                                :built-in
                                "directory"
                                '(("pathname" . "notes.txt")
                                  ("pattern" . "*.txt"))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-directory-rejects-out-of-scope-path
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-directory-scope/" temp-dir))
        (outside-dir (merge-pathnames "outside-dir/" temp-dir))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (ensure-directories-exist outside-dir)
    (unwind-protect
        (let ((*filesystem-access-approval-function* (lambda (&rest ignored)
                                                       (declare (ignore ignored))
                                                       nil)))
          (fiveam:signals mcp-tool-execution-error
            (execute-chatbot-tool bot
                                  :built-in
                                  "directory"
                                  `(("pathname" . ,(namestring outside-dir))
                                    ("pattern" . "*.txt")))))
      (uiop:delete-directory-tree outside-dir :validate t)
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-web-search
  (let ((bot (make-instance 'chatbot
                           :web-tools-p t)))
    (let ((*web-search-function* (lambda (query)
                                  (fiveam:is (string= "common lisp" query))
                                  (make-grounding-search-response
                                   :total-results "2"
                                   :items '((:title "Common Lisp"
                                             :link "https://lisp-lang.org/"
                                             :snippet "A programmable programming language.")
                                            (:title "CL Cookbook"
                                             :link "https://lispcookbook.github.io/cl-cookbook/"
                                             :snippet "Practical Common Lisp examples."))))))
      (let ((result (execute-chatbot-tool bot
                                         :built-in
                                         "webSearch"
                                         '(("query" . "common lisp")))))
       (fiveam:is (search "Web search query: common lisp" result))
       (fiveam:is (search "1. Common Lisp" result))
       (fiveam:is (search "URL: https://lisp-lang.org/" result))
       (fiveam:is (search "Snippet: A programmable programming language." result))))))

(fiveam:test test-execute-chatbot-tool-hyperspec-search
  (let ((bot (make-instance 'chatbot
                           :web-tools-p t)))
    (let ((*hyperspec-search-function* (lambda (query)
                                        (fiveam:is (string= "format" query))
                                        (make-grounding-search-response
                                         :total-results "1"
                                         :items '((:title "CLHS: Section 22.3"
                                                   :link "https://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm"
                                                   :snippet "FORMAT Basic Output."))))))
      (let ((result (execute-chatbot-tool bot
                                         :built-in
                                         "hyperspecSearch"
                                         '(("query" . "format")))))
       (fiveam:is (search "HyperSpec search query: format" result))
       (fiveam:is (search "1. CLHS: Section 22.3" result))
       (fiveam:is (search "URL: https://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm" result))))))

(fiveam:test test-execute-chatbot-tool-eval
  (let ((bot (make-instance 'chatbot
                           :enable-eval-p t))
       (captured-expression nil))
    (let ((*eval-approval-function* (lambda (approval-bot source tool-name)
                                     (declare (ignore approval-bot tool-name))
                                     (setf captured-expression source)
                                     t)))
      (let* ((result-json (execute-chatbot-tool bot
                                               :built-in
                                               "eval"
                                               '(("expression" . "(progn (format t \"hello\") (format *error-output* \"oops\") (values 42 :done))"))))
            (result (cl-json:decode-json-from-string result-json)))
       (fiveam:is (string= "(progn (format t \"hello\") (format *error-output* \"oops\") (values 42 :done))"
                           captured-expression))
       (fiveam:is (equal '("42" ":DONE")
                         (coerce (cdr (assoc :values result)) 'list)))
       (fiveam:is (string= "hello" (cdr (assoc :stdout result))))
       (fiveam:is (string= "oops" (cdr (assoc :stderr result))))))))

(fiveam:test test-execute-chatbot-tool-eval-rejects-parse-failure
  (let ((bot (make-instance 'chatbot
                           :enable-eval-p t))
       (approval-called-p nil))
    (let ((*eval-approval-function* (lambda (&rest ignored)
                                     (declare (ignore ignored))
                                     (setf approval-called-p t)
                                     t)))
      (fiveam:signals mcp-tool-execution-error
       (execute-chatbot-tool bot
                             :built-in
                             "eval"
                             '(("expression" . "("))))
      (fiveam:is-false approval-called-p))))

(fiveam:test test-execute-chatbot-tool-eval-denies-without-evaluating
  (let ((bot (make-instance 'chatbot
                           :enable-eval-p t)))
    (setf (get 'eval-tool-denied-sentinel :hit) nil)
    (unwind-protect
        (let ((*eval-approval-function* (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          nil)))
          (fiveam:signals mcp-tool-execution-error
            (execute-chatbot-tool bot
                                  :built-in
                                  "eval"
                                  '(("expression" . "(setf (get 'eval-tool-denied-sentinel :hit) t)"))))
          (fiveam:is-false (get 'eval-tool-denied-sentinel :hit)))
      (remprop 'eval-tool-denied-sentinel :hit))))

(fiveam:test test-execute-chatbot-tool-eval-times-out
  (let ((bot (make-instance 'chatbot
                           :enable-eval-p t)))
    (let ((*eval-approval-function* (lambda (&rest ignored)
                                     (declare (ignore ignored))
                                     t))
          (*eval-tool-timeout-seconds* 1))
      (fiveam:signals mcp-tool-execution-error
        (execute-chatbot-tool bot
                             :built-in
                             "eval"
                             '(("expression" . "(sleep 2)")))))))

(fiveam:test test-execute-chatbot-tool-write-file-lf-with-trailing-eol
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-write-file-lf/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
         (progn
           (fiveam:is (string= "Wrote file: notes.txt"
                               (execute-chatbot-tool bot
                                                     :built-in
                                                     "writeFile"
                                                     `(("pathname" . "notes.txt")
                                                       ("useLfOnly" . t)
                                                       ("endWithEol" . t)
                                                       ("lines" . ,#("Alpha" "Beta"))))))
           (fiveam:is (string= (format nil "Alpha~%Beta~%")
                               (read-test-file-octets-as-string file-path))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-write-file-crlf-without-trailing-eol
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-write-file-crlf/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
         (progn
           (execute-chatbot-tool bot
                                 :built-in
                                 "writeFile"
                                 `(("pathname" . "notes.txt")
                                   ("useLfOnly" . :false)
                                   ("endWithEol" . :false)
                                   ("lines" . ,#("Alpha" "Beta"))))
           (fiveam:is (string= (format nil "Alpha~C~CBeta" #\Return #\Linefeed)
                               (read-test-file-octets-as-string file-path))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-write-file-empty-lines-produces-empty-file
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-write-file-empty/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
         (progn
           (execute-chatbot-tool bot
                                 :built-in
                                 "writeFile"
                                 `(("pathname" . "notes.txt")
                                   ("useLfOnly" . t)
                                   ("endWithEol" . t)
                                   ("lines" . ,#())))
           (fiveam:is (string= "" (read-test-file-octets-as-string file-path))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-write-file-rejects-invalid-lines
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-write-file-invalid/" temp-dir))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
         (fiveam:signals mcp-tool-execution-error
           (execute-chatbot-tool bot
                                 :built-in
                                 "writeFile"
                                 `(("pathname" . "notes.txt")
                                   ("useLfOnly" . t)
                                   ("endWithEol" . t)
                                   ("lines" . ,#("Alpha" 3)))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-write-file-rejects-out-of-scope-path
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "filesystem-tool-write-file-scope/" temp-dir))
         (outside-file (merge-pathnames "outside.txt" temp-dir))
         (bot (make-instance 'chatbot
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
         (let ((*filesystem-access-approval-function* (lambda (&rest ignored)
                                                        (declare (ignore ignored))
                                                        nil)))
           (fiveam:signals mcp-tool-execution-error
             (execute-chatbot-tool bot
                                   :built-in
                                   "writeFile"
                                   `(("pathname" . ,(namestring outside-file))
                                     ("useLfOnly" . t)
                                     ("endWithEol" . t)
                                     ("lines" . ,#("Alpha"))))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-delete-file
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-delete-file/" temp-dir))
        (file-path (merge-pathnames "notes.txt" root))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Delete me" s))
    (unwind-protect
        (progn
          (fiveam:is (string= "Deleted file: notes.txt"
                              (execute-chatbot-tool bot
                                                    :built-in
                                                    "deleteFile"
                                                    '(("pathname" . "notes.txt")))))
          (fiveam:is-false (probe-file file-path)))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-delete-file-rejects-missing-file
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-delete-file-missing/" temp-dir))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
        (fiveam:signals mcp-tool-execution-error
          (execute-chatbot-tool bot
                                :built-in
                                "deleteFile"
                                '(("pathname" . "notes.txt"))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-delete-file-rejects-directory-target
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-delete-file-directory/" temp-dir))
        (dir-path (merge-pathnames "docs/" root))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist dir-path)
    (unwind-protect
        (fiveam:signals mcp-tool-execution-error
          (execute-chatbot-tool bot
                                :built-in
                                "deleteFile"
                                '(("pathname" . "docs"))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-delete-file-rejects-out-of-scope-path
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "filesystem-tool-delete-file-scope/" temp-dir))
        (outside-file (merge-pathnames "outside.txt" temp-dir))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s outside-file :direction :output :if-exists :supersede)
      (write-line "Outside" s))
    (unwind-protect
        (let ((*filesystem-access-approval-function* (lambda (&rest ignored)
                                                       (declare (ignore ignored))
                                                       nil)))
          (fiveam:signals mcp-tool-execution-error
            (execute-chatbot-tool bot
                                  :built-in
                                  "deleteFile"
                                  `(("pathname" . ,(namestring outside-file))))))
      (delete-file outside-file)
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-read-file-lines-prompts-and-persists-approved-directory
  (let* ((temp-dir (uiop:default-temporary-directory))
        (persona-root (merge-pathnames "filesystem-tool-allowlist-root/" temp-dir))
        (outside-dir (merge-pathnames "approved-dir/" temp-dir))
        (file-path (merge-pathnames "notes.txt" outside-dir))
        (allowlist-path (merge-pathnames "filesystem-allowlist.lisp" persona-root))
        (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory persona-root
                            :filesystem-allowlist-path allowlist-path))
        (prompted-directory nil))
    (ensure-directories-exist persona-root)
    (ensure-directories-exist outside-dir)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Alpha" s)
      (write-line "Beta" s))
    (unwind-protect
        (let ((*filesystem-access-approval-function*
                (lambda (ignored-bot directory tool-name)
                  (declare (ignore ignored-bot))
                  (setf prompted-directory (list (namestring directory) tool-name))
                  t)))
          (fiveam:is (string= (format nil "Alpha~%Beta")
                              (execute-chatbot-tool bot
                                                    :built-in
                                                    "readFileLines"
                                                    `(("filename" . ,(namestring file-path))
                                                      ("beginningLine" . 1)
                                                      ("endingLine" . 10)))))
          (fiveam:is (equal (list (namestring (uiop:ensure-directory-pathname (truename outside-dir)))
                                  "readFileLines")
                            prompted-directory))
          (fiveam:is (equal (list (namestring (uiop:ensure-directory-pathname (truename outside-dir))))
                            (read-test-lisp-form allowlist-path)))
          (fiveam:is (equal (list (uiop:ensure-directory-pathname (truename outside-dir)))
                            (chatbot-filesystem-allowed-directories bot))))
      (uiop:delete-directory-tree outside-dir :validate t)
      (uiop:delete-directory-tree persona-root :validate t))))

(fiveam:test test-execute-chatbot-tool-directory-allows-nested-approved-directory
  (let* ((temp-dir (uiop:default-temporary-directory))
         (persona-root (merge-pathnames "filesystem-tool-nested-root/" temp-dir))
         (approved-dir (merge-pathnames "approved-parent/" temp-dir))
         (nested-dir (merge-pathnames "child/" approved-dir))
         (bot (make-instance 'chatbot
                            :filesystem-tools-p t
                            :filesystem-root-directory persona-root
                            :filesystem-allowed-directories (list approved-dir))))
    (ensure-directories-exist persona-root)
    (ensure-directories-exist nested-dir)
    (with-open-file (s (merge-pathnames "alpha.txt" nested-dir) :direction :output :if-exists :supersede)
      (write-line "Alpha" s))
    (unwind-protect
         (fiveam:is (equal (list (enough-namestring (merge-pathnames "alpha.txt" nested-dir)
                                                   (truename persona-root)))
                          (coerce (cl-json:decode-json-from-string
                                   (execute-chatbot-tool bot
                                                         :built-in
                                                         "directory"
                                                         `(("pathname" . ,(namestring nested-dir))
                                                           ("pattern" . "*.txt"))))
                                  'list)))
      (uiop:delete-directory-tree approved-dir :validate t)
      (uiop:delete-directory-tree persona-root :validate t))))

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

(fiveam:test test-execute-mcp-tool-falls-back-to-nontext-content
  (let* ((server (make-instance 'mcp-server :name "cached-server"))
         )
    (let ((*mcp-call-tool-function*
            (lambda (srv name arguments)
              (declare (ignore srv name arguments))
              '((:content . (((:type . "image"))))))))
      (fiveam:is (string= "[{\"type\":\"image\"}]"
                          (execute-mcp-tool server "cached_tool" nil))))))

(fiveam:test test-execute-mcp-tool-falls-back-to-structured-content
  (let* ((server (make-instance 'mcp-server :name "cached-server")))
    (let ((*mcp-call-tool-function*
            (lambda (srv name arguments)
              (declare (ignore srv name arguments))
              '(("structuredContent" . ((:entities . #(((:name . "alpha"))))
                                        (:relations . #())))))))
      (fiveam:is (string= "{\"entities\":[{\"name\":\"alpha\"}],\"relations\":[]}"
                          (execute-mcp-tool server "read_graph" nil))))))

(fiveam:test test-execute-mcp-tool-falls-back-to-success-message-when-payload-is-empty
  (let* ((server (make-instance 'mcp-server :name "cached-server")))
    (let ((*mcp-call-tool-function*
            (lambda (srv name arguments)
              (declare (ignore srv name arguments))
              '())))
      (fiveam:is (string= "Tool completed successfully."
                          (execute-mcp-tool server "add_observations" nil))))))

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
  (let* ((mock-server-path (merge-pathnames "mock-mcp-server.lisp" (asdf:system-source-directory :chatbot)))
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

(fiveam:test test-default-stop-mcp-server-closes-streams
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "mcp-stop-streams/" temp-dir))
         (input-path (merge-pathnames "input.txt" root))
        (output-path (merge-pathnames "output.txt" root))
        (error-path (merge-pathnames "error.txt" root)))
    (when (probe-file root)
      (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist root)
    (with-open-file (stream input-path :direction :output :if-exists :supersede)
      (write-string "" stream))
    (with-open-file (stream output-path :direction :output :if-exists :supersede)
      (write-string "" stream))
    (with-open-file (stream error-path :direction :output :if-exists :supersede)
      (write-string "" stream))
    (let ((input-stream (open input-path :direction :io :if-exists :overwrite))
         (output-stream (open output-path :direction :input))
         (error-stream (open error-path :direction :input)))
      (unwind-protect
          (let ((server (make-instance 'mcp-server
                                       :name "test-server"
                                       :input-stream input-stream
                                       :output-stream output-stream
                                       :error-stream error-stream)))
             (default-stop-mcp-server server)
             (fiveam:is-false (open-stream-p input-stream))
             (fiveam:is-false (open-stream-p output-stream))
             (fiveam:is-false (open-stream-p error-stream))
             (fiveam:is-false (mcp-server-input-stream server))
             (fiveam:is-false (mcp-server-output-stream server))
             (fiveam:is-false (mcp-server-error-stream server)))
        (when (open-stream-p input-stream)
          (close input-stream))
        (when (open-stream-p output-stream)
          (close output-stream))
        (when (open-stream-p error-stream)
          (close error-stream))
        (uiop:delete-directory-tree root :validate t)))))

(fiveam:test test-default-stop-mcp-server-clears-and-stops-tracked-threads
  (let* ((reader-thread (sb-thread:make-thread (lambda () (sleep 60))
                                              :name "test-mcp-reader"))
        (stderr-thread (sb-thread:make-thread (lambda () (sleep 60))
                                              :name "test-mcp-stderr"))
        (server (make-instance 'mcp-server
                               :name "test-server"
                               :reader-thread reader-thread
                               :stderr-thread stderr-thread)))
    (unwind-protect
        (progn
          (default-stop-mcp-server server)
          (sleep 0.1)
          (fiveam:is-false (sb-thread:thread-alive-p reader-thread))
          (fiveam:is-false (sb-thread:thread-alive-p stderr-thread))
          (fiveam:is-false (mcp-server-reader-thread server))
          (fiveam:is-false (mcp-server-stderr-thread server)))
      (when (sb-thread:thread-alive-p reader-thread)
        (sb-thread:terminate-thread reader-thread))
      (when (sb-thread:thread-alive-p stderr-thread)
        (sb-thread:terminate-thread stderr-thread)))))

(fiveam:test test-execute-chatbot-tool-dynamic-minions
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-dynamic-minions/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "test-minion-persona/" personas-dir))
         (bot (conversation-chatbot (new-chat :backend :gemini)))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore args))
             (cond
              ((search "api.openai.com" url)
               (values
                (make-string-input-stream
                 (test-openai-subordinate-stream "Minion Bello here."))
                200))
              (t
               (values
                (make-string-input-stream
                 (test-gemini-subordinate-stream "Minion Bello here." :model "gpt-4o"))
                200))))))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"persona-model\" :backend :openai)" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home))
               (*gemini-api-key-function* (lambda () "mocked-api-key"))
               (*openai-api-key* "mocked-openai-key"))
           ;; 1. List minions initially (expect empty list / empty JSON array)
           (let ((list-res (execute-chatbot-tool-by-name bot "listMinions" '())))
             (fiveam:is (string= "[]" list-res)))
           ;; 2. Spawn a custom minion named Bello
           (let ((spawn-res (execute-chatbot-tool-by-name bot "spawnMinion"
                                                          '(("name" . "Bello")
                                                            ("backend" . "openai")
                                                            ("model" . "gpt-4o")
                                                            ("systemInstruction" . "You are Bello")))))
             (fiveam:is (string= "Minion 'Bello' spawned successfully." spawn-res)))
           ;; 3. List minions again (expect 1)
           (let* ((list-res (execute-chatbot-tool-by-name bot "listMinions" '()))
                  (parsed (cl-json:decode-json-from-string list-res)))
             (fiveam:is (= 1 (length parsed)))
             (fiveam:is (string-equal "Bello" (cdr (assoc :name (first parsed)))))
             (fiveam:is (string-equal "openai" (cdr (assoc :backend (first parsed)))))
             (fiveam:is (string-equal "gpt-4o" (cdr (assoc :model (first parsed))))))
           ;; 4. Prompt the minion Bello
           (let ((prompt-res (execute-chatbot-tool-by-name bot "promptSubordinate"
                                                           '(("name" . "Bello")
                                                             ("prompt" . "Hi Bello")))))
             (fiveam:is (string= "Minion Bello here." prompt-res)))
           ;; 5. Spawn a persona-based minion named Jerry
           (let ((spawn-res (execute-chatbot-tool-by-name bot "spawnMinion"
                                                          '(("name" . "Jerry")
                                                            ("personaName" . "test-minion-persona")))))
             (fiveam:is (string= "Minion 'Jerry' spawned successfully." spawn-res)))
           ;; 6. List minions again (expect Jerry and Bello, total 2)
           (let* ((list-res (execute-chatbot-tool-by-name bot "listMinions" '()))
                  (parsed (cl-json:decode-json-from-string list-res)))
             (fiveam:is (= 2 (length parsed)))
             ;; jerry info
             (let ((jerry-info (find "Jerry" parsed :key (lambda (x) (cdr (assoc :name x))) :test #'string-equal)))
               (fiveam:is (not (null jerry-info)))
               (fiveam:is (string-equal "openai" (cdr (assoc :backend jerry-info))))
               (fiveam:is (string-equal "persona-model" (cdr (assoc :model jerry-info))))))
           ;; 7. Dismiss Jerry and Bello
           (let ((dismiss-res (execute-chatbot-tool-by-name bot "dismissMinion" '(("name" . "Bello")))))
             (fiveam:is (string= "Minion 'Bello' and all of its subordinates dismissed successfully." dismiss-res)))
           (let ((dismiss-res (execute-chatbot-tool-by-name bot "dismissMinion" '(("name" . "Jerry")))))
             (fiveam:is (string= "Minion 'Jerry' and all of its subordinates dismissed successfully." dismiss-res)))
           ;; 8. Verify list is empty again
           (let ((list-res (execute-chatbot-tool-by-name bot "listMinions" '())))
             (fiveam:is (string= "[]" list-res))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-recursive-minions-lifecycle
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-recursive-minions/" temp-dir))
         (bot (conversation-chatbot (new-chat :backend :gemini :token-budget 2000 :depth 1)))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url args))
             (values
              (make-string-input-stream
               (test-gemini-subordinate-stream
                "I need to delegate."
                :spawn '(("name" . "Minion-L3")
                         ("budget" . 400))))
              200))))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home))
               (*gemini-api-key-function* (lambda () "mocked-api-key")))
           
           ;; 1. Spawn parent minion (Minion-L2) at depth 2 with budget 1000
           (let ((spawn-res (execute-chatbot-tool-by-name bot "spawnMinion"
                                                          '(("name" . "Minion-L2")
                                                            ("budget" . 1000)))))
             (fiveam:is (string= "Minion 'Minion-L2' spawned successfully." spawn-res)))
           
           (let* ((subs (chatbot-subordinates bot))
                  (minion-l2-conv (first subs))
                  (minion-l2-bot (conversation-chatbot minion-l2-conv)))
             (fiveam:is (= 1 (length subs)))
             (fiveam:is (string= "Minion-L2" (chatbot-persona-name minion-l2-bot)))
             (fiveam:is (= 2 (chatbot-depth minion-l2-bot)))
             (fiveam:is (= 1000 (chatbot-token-budget minion-l2-bot)))
             (fiveam:is (= 1000 (chatbot-spent-tokens bot))) ; deducted from parent
             
             ;; 2. Prompt minion-l2-bot, which responds with a spawn trigger for Minion-L3
             ;; This trigger should auto-spawn Minion-L3 at depth 3 with budget 400
             (let ((prompt-res (execute-chatbot-tool-by-name bot "promptSubordinate"
                                                             '(("name" . "Minion-L2")
                                                               ("prompt" . "Delegate task")))))
               (fiveam:is (not (null (search "Successfully spawned subordinate minion 'Minion-L3'" prompt-res))))
               (fiveam:is (not (null (search "budget 400 at depth 3" prompt-res)))))
             
             ;; Verify Minion-L3 was created under Minion-L2
             (let* ((l2-subs (chatbot-subordinates minion-l2-bot))
                    (minion-l3-conv (first l2-subs))
                    (minion-l3-bot (conversation-chatbot minion-l3-conv)))
               (fiveam:is (= 1 (length l2-subs)))
               (fiveam:is (string= "Minion-L3" (chatbot-persona-name minion-l3-bot)))
               (fiveam:is (= 3 (chatbot-depth minion-l3-bot)))
               (fiveam:is (= 400 (chatbot-token-budget minion-l3-bot)))
               (fiveam:is (= 400 (chatbot-spent-tokens minion-l2-bot))) ; deducted from Minion-L2
               
               ;; 3. Depth Guard validation
               ;; If we try to spawn under Minion-L3, depth would be 4, which exceeds *max-minion-depth* of 3.
               ;; Let's try to prompt Minion-L2 again to trigger a spawn from Minion-L3 (which will attempt depth 4)
               ;; Change mock response to trigger spawn from Minion-L3
               (let ((*http-post-function*
                       (lambda (url &rest args)
                         (declare (ignore url args))
                         (values
                          (make-string-input-stream
                           (test-gemini-subordinate-stream
                            "Attempting depth 4."
                            :spawn '(("name" . "Minion-L4")
                                     ("budget" . 100))
                            :interaction-id "session-depth"
                            :model "gemini-3.5-flash"))
                          200))))
                 (let ((prompt-res (execute-chatbot-tool-by-name minion-l2-bot "promptSubordinate"
                                                                 '(("name" . "Minion-L3")
                                                                   ("prompt" . "Delegate further")))))
                   ;; Verify spawn failed with Maximum nesting depth exceeded message
                   (fiveam:is (not (null (search "Spawn failed: Maximum nesting depth (3) exceeded" prompt-res))))))
               
               ;; 4. Budget Guard validation
               ;; Minion-L2 has budget 1000, already spent 400, so remaining is 600.
               ;; If it tries to spawn something with budget 700, it should fail.
               (let ((*http-post-function*
                       (lambda (url &rest args)
                         (declare (ignore url args))
                         (values
                          (make-string-input-stream
                           (test-gemini-subordinate-stream
                            "Excessive budget."
                            :spawn '(("name" . "Minion-Rich")
                                     ("budget" . 700))
                            :interaction-id "session-budget"
                            :model "gemini-3.5-flash"))
                          200))))
                 (let ((prompt-res (execute-chatbot-tool-by-name bot "promptSubordinate"
                                                                 '(("name" . "Minion-L2")
                                                                   ("prompt" . "Delegate expensive")))))
                   (fiveam:is (not (null (search "Spawn failed: Requested budget (700) exceeds remaining budget (600)" prompt-res))))))
               
               ;; 5. Sandbox Inheritance validation
               ;; Validate that the scoped directory of Minion-L3 is a subdirectory of Minion-L2's scoped directory
               (fiveam:is (not (null (chatbot-scoped-directory minion-l2-bot))))
               (fiveam:is (not (null (chatbot-scoped-directory minion-l3-bot))))
               (fiveam:is (filesystem-path-within-directory-p (chatbot-scoped-directory minion-l3-bot)
                                                              (chatbot-scoped-directory minion-l2-bot))))
             
             ;; 6. Recursive Dismissal (Recursive Reaper) validation
             ;; Dismissing Minion-L2 should recursively clean up Minion-L3 and remove Minion-L2 from the parent list
             (let ((dismiss-res (execute-chatbot-tool-by-name bot "dismissMinion" '(("name" . "Minion-L2")))))
               (fiveam:is (string= "Minion 'Minion-L2' and all of its subordinates dismissed successfully." dismiss-res)))
             (fiveam:is (null (chatbot-subordinates bot)))))
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-spawn-minion-tool-is-idempotent-for-identical-input
  (let* ((context (make-runtime-context))
         (bot (conversation-chatbot (new-chat :backend :google
                                              :runtime-context context
                                              :token-budget 1000))))
    (let ((first-result (execute-chatbot-tool-by-name
                         bot
                         "spawnMinion"
                         '(("name" . "Bello")
                           ("backend" . "google")
                           ("model" . "gemini-3.5-flash")
                           ("budget" . 250))))
          (second-result (execute-chatbot-tool-by-name
                          bot
                          "spawnMinion"
                          '(("name" . "Bello")
                            ("backend" . "google")
                            ("model" . "gemini-3.5-flash")
                            ("budget" . 250)))))
      (fiveam:is (string= "Minion 'Bello' spawned successfully." first-result))
      (fiveam:is (string= first-result second-result))
      (fiveam:is (= 1 (length (chatbot-subordinates bot))))
      (fiveam:is (= 250 (chatbot-spent-tokens bot)))))

(fiveam:test test-prompt-subordinate-tool-is-idempotent-for-identical-input
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-idempotent-minions/" temp-dir))
         (http-calls 0)
         (bot (conversation-chatbot (new-chat :backend :gemini :token-budget 2000 :depth 1))))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home))
               (*gemini-api-key-function* (lambda () "mocked-api-key"))
               (*http-post-function*
                 (lambda (url &rest args)
                   (declare (ignore url args))
                   (incf http-calls)
                   (values
                    (make-string-input-stream
                     (test-gemini-subordinate-stream
                      "I need to delegate."
                      :spawn '(("name" . "Minion-L3")
                               ("budget" . 400))))
                    200)))))
           (let ((spawn-res (execute-chatbot-tool-by-name bot "spawnMinion"
                                                          '(("name" . "Minion-L2")
                                                            ("budget" . 1000)))))
             (fiveam:is (string= "Minion 'Minion-L2' spawned successfully." spawn-res)))
           (let* ((minion-l2-conv (first (chatbot-subordinates bot)))
                  (minion-l2-bot (conversation-chatbot minion-l2-conv))
                  (first-result (execute-chatbot-tool-by-name
                                 bot
                                 "promptSubordinate"
                                 '(("name" . "Minion-L2")
                                   ("prompt" . "Delegate task"))))
                  (second-result (execute-chatbot-tool-by-name
                                  bot
                                  "promptSubordinate"
                                  '(("name" . "Minion-L2")
                                    ("prompt" . "Delegate task")))))
             (fiveam:is (string= first-result second-result))
             (fiveam:is (= 1 http-calls))
             (fiveam:is (= 1 (length (chatbot-subordinates minion-l2-bot))))
             (fiveam:is (= 400 (chatbot-spent-tokens minion-l2-bot)))
             (fiveam:is (not (null (search "Successfully spawned subordinate minion 'Minion-L3'"
                                           first-result))))))
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-minion-checkpoint-and-recovery
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-mcrs/" temp-dir))
         (mock-minions-dir (merge-pathnames "data/minions/" mock-home))
         (bot (conversation-chatbot (new-chat :backend :gemini :persona-name "Top-Boss")))
         (*minions-data-directory* mock-minions-dir)
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url args))
             (values
              (make-string-input-stream
               (format nil
                       "data: ~A~%data: ~A"
                       (cl-json:encode-json-to-string
                        '(("event_type" . "interaction.created")
                          ("interaction" . (("id" . "session-1")))))
                       (cl-json:encode-json-to-string
                        `(("event_type" . "step.delta")
                          ("delta" . (("type" . "text")
                                      ("text" . ,(test-subordinate-control-response
                                                  :reply "Reporting"
                                                  :spawn '(("name" . "Sub-Minion")
                                                           ("budget" . 500))))))))))
              200))))
    (ensure-directories-exist mock-minions-dir)
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home))
               (*gemini-api-key-function* (lambda () "mocked-api-key")))
           
           ;; 1. Spawn a minion named Gopher
           (execute-chatbot-tool-by-name bot "spawnMinion"
                                         '(("name" . "Gopher")
                                           ("budget" . 1000)))
           
           ;; Verify Gopher state file is created
           (let ((gopher-file (merge-pathnames "Gopher.json" mock-minions-dir)))
             (fiveam:is (not (null (probe-file gopher-file))))
             
             ;; 2. Prompt Gopher, which will trigger spawning Sub-Minion
             (execute-chatbot-tool-by-name bot "promptSubordinate"
                                           '(("name" . "Gopher")
                                             ("prompt" . "Report state")))
             
             ;; Verify Sub-Minion state file is also created
             (let ((sub-file (merge-pathnames "Sub-Minion.json" mock-minions-dir)))
               (fiveam:is (not (null (probe-file sub-file))))
               
               ;; 3. Now simulate reboot and recover. We instantiate a fresh new bot.
               (let* ((fresh-bot (conversation-chatbot (new-chat :backend :gemini :persona-name "Top-Boss")))
                      (restored (restore-minions fresh-bot)))
                 (declare (ignore restored))
                 
                 ;; Verify the parent-child structure and telemetry is perfectly restored!
                 (let* ((subs (chatbot-subordinates fresh-bot))
                        (restored-gopher-conv (first subs))
                        (restored-gopher-bot (conversation-chatbot restored-gopher-conv)))
                   (fiveam:is (= 1 (length subs)))
                   (fiveam:is (string= "Gopher" (chatbot-persona-name restored-gopher-bot)))
                   (fiveam:is (= 2 (chatbot-depth restored-gopher-bot)))
                   (fiveam:is (= 1000 (chatbot-token-budget restored-gopher-bot)))
                   (fiveam:is (= 500 (chatbot-spent-tokens restored-gopher-bot)))
                   
                   ;; Verify Sub-Minion is nested under Gopher
                   (let* ((gopher-subs (chatbot-subordinates restored-gopher-bot))
                          (restored-sub-conv (first gopher-subs))
                          (restored-sub-bot (conversation-chatbot restored-sub-conv)))
                     (fiveam:is (= 1 (length gopher-subs)))
                     (fiveam:is (string= "Sub-Minion" (chatbot-persona-name restored-sub-bot)))
                     (fiveam:is (= 3 (chatbot-depth restored-sub-bot)))
                     (fiveam:is (= 500 (chatbot-token-budget restored-sub-bot)))
                     
                     ;; Verify recovery handshake message was appended
                     (let* ((history (conversation-messages restored-sub-conv))
                            (last-msg (car (last history))))
                       (fiveam:is (string= "user" (cdr (assoc "role" last-msg :test #'string=))))
                       (fiveam:is (search "Recovered from unexpected shutdown" (cdr (assoc "content" last-msg :test #'string=))))))))))))
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-minion-web-tools-inheritance
  (let* ((bot-with (conversation-chatbot (new-chat :backend :gemini :web-tools-p t)))
         (bot-without (conversation-chatbot (new-chat :backend :gemini :web-tools-p nil))))
    ;; 1. Spawn from parent with web-tools-p = t
    ;; child should inherit t by default
    (execute-chatbot-tool-by-name bot-with "spawnMinion" '(("name" . "Inherited-True")))
    (let* ((child-conv (first (chatbot-subordinates bot-with)))
           (child-bot (conversation-chatbot child-conv)))
      (fiveam:is-true (chatbot-web-tools-p child-bot)))
    
    ;; 2. Spawn from parent with web-tools-p = t, but explicitly override to nil
    (execute-chatbot-tool-by-name bot-with "spawnMinion" '(("name" . "Overridden-False") ("webTools" . nil)))
    (let* ((child-conv (second (chatbot-subordinates bot-with)))
           (child-bot (conversation-chatbot child-conv)))
      (fiveam:is-false (chatbot-web-tools-p child-bot)))
    
    ;; 3. Spawn from parent with web-tools-p = nil
    ;; child should inherit nil by default
    (execute-chatbot-tool-by-name bot-without "spawnMinion" '(("name" . "Inherited-False")))
    (let* ((child-conv (first (chatbot-subordinates bot-without)))
           (child-bot (conversation-chatbot child-conv)))
      (fiveam:is-false (chatbot-web-tools-p child-bot)))
    
    ;; 4. Spawn from parent with web-tools-p = nil, but explicitly override to t
    (execute-chatbot-tool-by-name bot-without "spawnMinion" '(("name" . "Overridden-True") ("webTools" . t)))
    (let* ((child-conv (second (chatbot-subordinates bot-without)))
           (child-bot (conversation-chatbot child-conv)))
      (fiveam:is-true (chatbot-web-tools-p child-bot)))))

(fiveam:test test-minion-filesystem-sandbox-read-only
  (let* ((parent-bot (conversation-chatbot (new-chat :backend :gemini)))
         (temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-fs-sandbox/" temp-dir)))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home))
               (*gemini-api-key-function* (lambda () "mocked-api-key")))
           ;; 1. Spawn minion Gopher
           (execute-chatbot-tool-by-name parent-bot "spawnMinion" '(("name" . "Gopher")))
           (let* ((gopher-conv (first (chatbot-subordinates parent-bot)))
                  (gopher-bot (conversation-chatbot gopher-conv)))
             ;; Verify read-only and filesystem flag are set to T
             (fiveam:is-true (chatbot-filesystem-tools-p gopher-bot))
             (fiveam:is-true (chatbot-filesystem-read-only-p gopher-bot))
             
             ;; Verify read-only tools directory and readFileLines are registered
             (multiple-value-bind (source tool) (find-chatbot-tool gopher-bot "directory")
               (fiveam:is (eq :built-in source))
               (fiveam:is (not (null tool))))
             (multiple-value-bind (source tool) (find-chatbot-tool gopher-bot "readFileLines")
               (fiveam:is (eq :built-in source))
               (fiveam:is (not (null tool))))
             
             ;; Verify write/delete tools are NOT registered
             (multiple-value-bind (source tool) (find-chatbot-tool gopher-bot "writeFile")
               (fiveam:is (null source))
               (fiveam:is (null tool)))
             (multiple-value-bind (source tool) (find-chatbot-tool gopher-bot "deleteFile")
               (fiveam:is (null source))
               (fiveam:is (null tool)))))
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-tool-execution-sandboxing-prevents-bubble-crash
  (let* ((bot (conversation-chatbot (new-chat :enable-eval-p t)))
         (tool-calls (list '((:id . "call-1")
                             (:name . "eval")
                             (:arguments . "{\"expression\":\"(/ 1 0)\"}"))))
         (results (map-chatbot-json-tool-call-results
                   bot
                   tool-calls
                   (lambda (name tool-call)
                     (declare (ignore name tool-call))
                     "eval sandbox")
                   (lambda (id name arguments-json res-text tool-call)
                     (declare (ignore id name arguments-json tool-call))
                     res-text)
                   :error-builder nil)))
    ;; Verify that instead of bubble-crashing, the error is caught and returned as serialized JSON
    (fiveam:is (= 1 (length results)))
    (let* ((res-text (first results))
           (parsed (cl-json:decode-json-from-string res-text)))
      (fiveam:is (string= "tool_error" (cdr (assoc :type parsed))))
      (fiveam:is (string= "eval" (cdr (assoc :tool-name parsed))))
      (fiveam:is (search "division-by-zero" (string-downcase (cdr (assoc :message parsed))))))))

(fiveam:test test-submit-plan-tool
  (let* ((parent-conv (new-chat :backend :google))
         (bot (conversation-chatbot parent-conv))
         (*active-planner* parent-conv)
         (*active-planner-parent-conversation* parent-conv)
         (plan-content "## My Strategic Plan~%1. Learn Lisp~%2. Rule the world.~%")
         (res-text (execute-chatbot-tool-by-name bot "submitPlan" `(("planContent" . ,plan-content)))))
    (fiveam:is (stringp res-text))
    (fiveam:is (search "Plan saved successfully to" res-text))
    ;; Verify state was toggled
    (fiveam:is-false *active-planner*)
    ;; Verify parent conversation messages received the transient system notification
    (let ((history (conversation-messages parent-conv)))
      (fiveam:is (= 1 (length history)))
      (let ((msg (first history)))
        (fiveam:is (string= "user" (cdr (assoc "role" msg :test #'string=))))
        (fiveam:is (search "[System: Plan saved to plans/plan-" (cdr (assoc "content" msg :test #'string=))))))
    ;; Clean up generated plans files if any are generated
    (let* ((files (and (uiop:directory-exists-p "plans/")
                       (uiop:directory-files "plans/" "*.md"))))
      (dolist (f files)
        (delete-file f))
      (when (uiop:directory-exists-p "plans/")
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname "plans/") :validate t)))))

(fiveam:test test-abort-plan-tool
  (let* ((parent-conv (new-chat :backend :google))
         (bot (conversation-chatbot parent-conv))
         (*active-planner* parent-conv)
         (*active-planner-parent-conversation* parent-conv)
         (res-text (execute-chatbot-tool-by-name bot "abortPlan" '(("reason" . "User decided to cancel.")))))
    (fiveam:is (stringp res-text))
    (fiveam:is (search "Planner mode aborted" res-text))
    ;; Verify state was toggled
    (fiveam:is-false *active-planner*)
    ;; Verify parent conversation messages received the transient system notification
    (let ((history (conversation-messages parent-conv)))
      (fiveam:is (= 1 (length history)))
      (let ((msg (first history)))
        (fiveam:is (string= "user" (cdr (assoc "role" msg :test #'string=))))
        (fiveam:is (string= "[System: Planner mode aborted.]" (cdr (assoc "content" msg :test #'string=))))))))

(fiveam:test test-invoke-planner-tool
  (let* ((parent-conv (new-chat :backend :google))
         (bot (conversation-chatbot parent-conv))
         (*active-planner* nil)
         (*active-planner-parent-conversation* nil)
         (*active-conversation* parent-conv)
         (res-text (execute-chatbot-tool-by-name bot "invokePlanner" '(("contextSummary" . "Develop schema for leaders.")))))
    (fiveam:is (stringp res-text))
    (fiveam:is (search "Planner minion successfully spawned" res-text))
    ;; Verify state variables were set
    (fiveam:is-true (typep *active-planner* 'conversation))
    (fiveam:is (eq parent-conv *active-planner-parent-conversation*))
    
    (let* ((planner-bot (conversation-chatbot *active-planner*))
           (history (conversation-messages *active-planner*))
           (tools (default-get-all-builtin-tools planner-bot)))
      ;; Verify planner-p flag and system instructions
      (fiveam:is-true (chatbot-planner-p planner-bot))
      (fiveam:is (string= "Planner" (chatbot-persona-name planner-bot)))
      (fiveam:is (string= +planner-system-instruction+ (chatbot-system-instruction planner-bot)))
      
      ;; Verify injected initial prompt
      (fiveam:is (= 1 (length history)))
      (let ((msg (first history)))
        (fiveam:is (string= "user" (cdr (assoc "role" msg :test #'string=))))
        (fiveam:is (search "Planning Session Initiated." (cdr (assoc "content" msg :test #'string=))))
        (fiveam:is (search "Develop schema for leaders." (cdr (assoc "content" msg :test #'string=)))))
      
      ;; Verify restricted toolset: readFileLines, directory, webSearch, hyperspecSearch, submitPlan, abortPlan
      (let ((tool-names (mapcar (lambda (entry)
                                  (mcp-val :name (cdr entry)))
                                tools)))
        (fiveam:is (= 6 (length tool-names)))
        (fiveam:is (member "readFileLines" tool-names :test #'string=))
        (fiveam:is (member "directory" tool-names :test #'string=))
        (fiveam:is (member "webSearch" tool-names :test #'string=))
        (fiveam:is (member "hyperspecSearch" tool-names :test #'string=))
        (fiveam:is (member "submitPlan" tool-names :test #'string=))
        (fiveam:is (member "abortPlan" tool-names :test #'string=))
        ;; Verify execution tools (eval, writeFile, deleteFile, spawnMinion) are NOT present
        (fiveam:is-false (member "eval" tool-names :test #'string=))
        (fiveam:is-false (member "writeFile" tool-names :test #'string=))
        (fiveam:is-false (member "deleteFile" tool-names :test #'string=))
        (fiveam:is-false (member "spawnMinion" tool-names :test #'string=))))))

(fiveam:test test-load-plan-to-system-instructions
  (let* ((bot (conversation-chatbot (new-chat :backend :google :system-instruction "Base instruction.")))
         (filename "test-plan-file-123.md")
         (plan-content "## Step 1: Code.~%## Step 2: Test."))
    (with-open-file (s filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string plan-content s))
    (unwind-protect
         (let ((res (load-plan-to-system-instructions bot filename)))
           (fiveam:is (stringp res))
           (fiveam:is (search "test-plan-file-123.md" res))
           ;; Verify it was appended to system-instruction
           (let ((inst (chatbot-system-instruction bot)))
             (fiveam:is (search "Base instruction." inst))
             (fiveam:is (search "[EXECUTING PLAN FROM test-plan-file-123.md]" inst))
             (fiveam:is (search "## Step 1: Code." inst))))
      (when (probe-file filename)
        (delete-file filename)))))

(fiveam:test test-chat-routing-to-active-planner
  (let* ((custom-context (make-runtime-context))
         (parent-conv (new-chat :backend :google :runtime-context custom-context))
         (planner-conv (new-chat :backend :google :runtime-context custom-context))
         (*active-planner* planner-conv)
         (*active-planner-parent-conversation* parent-conv)
         (parent-called-p nil)
         (planner-called-p nil))
    (let ((mock-post-fn
            (lambda (url &rest args)
              (declare (ignore args))
              (cond
                ((search "gemini-3.5-flash" url)
                 (setf planner-called-p t)
                 (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Planner Response.\"}], \"role\": \"model\"}}]}" 200))
                (t
                 (setf parent-called-p t)
                 (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Parent Response.\"}], \"role\": \"model\"}}]}" 200))))))
      (setf (runtime-context-http-post-function custom-context) mock-post-fn)
      (let ((*http-post-function* mock-post-fn))
        (let ((res (chat "User message for Parent." :conversation parent-conv)))
          (fiveam:is (string= "Planner Response." res))
          (fiveam:is-true planner-called-p)
          (fiveam:is-false parent-called-p))))))

(fiveam:test test-write-file-auto-creates-directories
  (let* ((root (uiop:default-temporary-directory))
         (bot (conversation-chatbot (new-chat :backend :google :filesystem-tools-p t)))
         (sub-dir (merge-pathnames "test-nested-write-dir/" root))
         (nested-file-path (merge-pathnames "a/b/c/nested-test-file.txt" sub-dir)))
    ;; Override default-temporary-directory or use sub-dir as chatbot-filesystem-root-directory
    (setf (chatbot-filesystem-root-directory bot) (namestring sub-dir))
    ;; Ensure parent directory does not exist initially, but root sub-dir itself does!
    (uiop:delete-directory-tree sub-dir :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist sub-dir)
    (fiveam:is-false (probe-file (merge-pathnames "a/b/c/" sub-dir)))
    (unwind-protect
         (let ((args (list (cons "pathname" "a/b/c/nested-test-file.txt")
                           (cons "lines" (vector "Hello Nested World" "Line Two"))
                           (cons "useLfOnly" t)
                           (cons "endWithEol" t))))
           (let ((res (execute-chatbot-tool-by-name bot "writeFile" args)))
             (fiveam:is (stringp res))
             (fiveam:is (search "Wrote file:" res))
             ;; Verify it actually exists and has correct content
             (fiveam:is-true (probe-file nested-file-path))
             (fiveam:is (string= (format nil "Hello Nested World~%Line Two~%")
                                 (uiop:read-file-string nested-file-path)))))
      (uiop:delete-directory-tree sub-dir :validate t :if-does-not-exist :ignore))))

(fiveam:test test-chat-checkpoint-for-default-conversation
  (let* ((custom-context (make-runtime-context))
         (*default-conversation* (new-chat :backend :google :runtime-context custom-context))
         (checkpoint-file (merge-pathnames "DefaultConversation.json" (minions-data-directory))))
    ;; Delete any pre-existing checkpoint
    (when (probe-file checkpoint-file)
      (delete-file checkpoint-file))
    (unwind-protect
         (let ((mock-post-fn
                 (lambda (url &rest args)
                   (declare (ignore args))
                   (if (search "gemini-3.5-flash" url)
                       (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Response.\"}], \"role\": \"model\"}}]}" 200)
                       (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Response.\"}], \"role\": \"model\"}}]}" 200)))))
           (setf (runtime-context-http-post-function custom-context) mock-post-fn)
           (let ((*http-post-function* mock-post-fn))
             (let ((res (chat "Hello ambient" :conversation *default-conversation*)))
               (fiveam:is (string= "Response." res))
               (fiveam:is-false (chatbot-persona-name
                                 (conversation-chatbot *default-conversation*)))
               ;; Assert that the checkpoint file was successfully written!
               (fiveam:is-true (probe-file checkpoint-file))
               ;; Read checkpoint to verify payload
               (let ((content (uiop:read-file-string checkpoint-file)))
                 (fiveam:is (search "DefaultConversation" content))
                 (fiveam:is (search "Hello ambient" content))))))
      (when (probe-file checkpoint-file)
        (delete-file checkpoint-file)))))

(fiveam:test test-restore-conversation-from-checkpoint
  (let* ((filename "TestRestoreConv.json")
         (checkpoint-file (merge-pathnames filename (minions-data-directory)))
         (bot (conversation-chatbot (new-chat :backend :google :model "gemini-3.5-flash" :system-instruction "Sterile context.")))
         (conv (make-instance 'conversation :chatbot bot)))
    (setf (chatbot-persona-name bot) "TestRestoreConv")
    (setf (conversation-messages conv) (list (list (cons "role" "user") (cons "content" "Original Msg"))))
    (save-minion-state conv)
    (fiveam:is-true (probe-file checkpoint-file))
    (unwind-protect
         (let ((restored (restore-conversation-from-checkpoint filename)))
           (fiveam:is (typep restored 'conversation))
           (let ((r-bot (conversation-chatbot restored)))
             (fiveam:is (eq :google (chatbot-backend r-bot)))
             (fiveam:is (string= "gemini-3.5-flash" (chatbot-model r-bot)))
             (fiveam:is (string= "Sterile context." (chatbot-system-instruction r-bot)))
             (fiveam:is (equal (list (list (cons "role" "user") (cons "content" "Original Msg")))
                               (conversation-messages restored)))))
      (when (probe-file checkpoint-file)
        (delete-file checkpoint-file)))))
