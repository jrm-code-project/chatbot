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

(fiveam:test test-parse-subordinate-control-response-accepts-llambda-raw-text
  (let ((control (parse-subordinate-control-response
                  "  Local inference result.  "
                  :backend :llambda)))
    (fiveam:is (string= "Local inference result." (getf control :reply)))
    (fiveam:is (null (getf control :spawn)))))

(fiveam:test test-parse-subordinate-control-response-rejects-other-raw-text
  (fiveam:signals malformed-json-error
    (parse-subordinate-control-response
     "Local inference result."
     :backend :gemini)))

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

(fiveam:test test-startup-entry-from-server-definition-marks-invalid-definitions-failed
  (let ((entry (startup-entry-from-server-definition '(:name "broken-server"))))
    (fiveam:is (typep entry 'mcp-startup-entry))
    (fiveam:is-false (mcp-startup-entry-success-p entry))
    (fiveam:is (string= "broken-server" (mcp-startup-entry-name entry)))
    (fiveam:is (string= "Invalid MCP server definition: missing required name or command."
                       (mcp-startup-entry-error-message entry)))))

(fiveam:test test-initialize-mcp-startup-entry-stops-server-after-initialize-failure
  (let* ((entry (make-mcp-startup-entry "test-server" "sbcl" '("--script" "broken.lisp") nil))
        (server (make-instance 'mcp-server :name "test-server"))
        (stopped nil))
    (let ((*start-mcp-server-function*
           (lambda (&rest ignored)
             (declare (ignore ignored))
             server))
         (*mcp-initialize-function*
           (lambda (srv)
             (declare (ignore srv))
             (error "initialize failed")))
         (*stop-mcp-server-function*
           (lambda (srv)
             (setf stopped srv))))
     (let ((result (initialize-mcp-startup-entry entry nil)))
       (fiveam:is (eq entry result))
       (fiveam:is-false (mcp-startup-entry-success-p entry))
       (fiveam:is (eq server stopped))
       (fiveam:is (string= "initialize failed"
                           (mcp-startup-entry-error-message entry)))))))

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
         (context (make-runtime-context
                   :filesystem-access-approval-function
                   (lambda (&rest ignored)
                     (declare (ignore ignored))
                     nil)))
         (bot (make-instance 'chatbot
                             :runtime-context context
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s outside-file :direction :output :if-exists :supersede)
      (write-line "Outside" s))
    (unwind-protect
         (fiveam:signals mcp-tool-execution-error
           (execute-chatbot-tool bot
                                 :built-in
                                 "readFileLines"
                                 `(("filename" . ,(namestring outside-file))
                                  ("beginningLine" . 1)
                                  ("endingLine" . 1))))
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
        (context (make-runtime-context
                  :filesystem-access-approval-function
                  (lambda (&rest ignored)
                    (declare (ignore ignored))
                    nil)))
        (bot (make-instance 'chatbot
                            :runtime-context context
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (ensure-directories-exist outside-dir)
    (unwind-protect
        (fiveam:signals mcp-tool-execution-error
          (execute-chatbot-tool bot
                                :built-in
                                "directory"
                                `(("pathname" . ,(namestring outside-dir))
                                  ("pattern" . "*.txt"))))
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
  (let* ((captured-expression nil)
         (context
           (make-runtime-context
            :eval-approval-function
            (lambda (approval-bot source tool-name)
              (declare (ignore approval-bot tool-name))
              (setf captured-expression source)
              t)))
         (bot (make-instance 'chatbot
                             :runtime-context context
                             :enable-eval-p t))
         (result-json (execute-chatbot-tool bot
                                            :built-in
                                            "eval"
                                            '(("expression" . "(progn (format t \"hello\") (format *error-output* \"oops\") (values 42 :done))"))))
         (result (cl-json:decode-json-from-string result-json)))
    (fiveam:is (string= "(progn (format t \"hello\") (format *error-output* \"oops\") (values 42 :done))"
                        captured-expression))
    (fiveam:is (equal '("42" ":DONE")
                      (coerce (cdr (assoc :values result)) 'list)))
    (fiveam:is (string= "hello" (cdr (assoc :stdout result))))
    (fiveam:is (string= "oops" (cdr (assoc :stderr result))))))

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
  (let* ((context (make-runtime-context
                   :eval-approval-function
                   (lambda (&rest ignored)
                     (declare (ignore ignored))
                     nil)))
         (bot (make-instance 'chatbot
                             :runtime-context context
                             :enable-eval-p t)))
    (setf (get 'eval-tool-denied-sentinel :hit) nil)
    (unwind-protect
        (progn
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
         (context (make-runtime-context
                   :filesystem-access-approval-function
                   (lambda (&rest ignored)
                     (declare (ignore ignored))
                     nil)))
         (bot (make-instance 'chatbot
                             :runtime-context context
                             :filesystem-tools-p t
                             :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (unwind-protect
         (fiveam:signals mcp-tool-execution-error
           (execute-chatbot-tool bot
                                 :built-in
                                 "writeFile"
                                 `(("pathname" . ,(namestring outside-file))
                                   ("useLfOnly" . t)
                                   ("endWithEol" . t)
                                   ("lines" . ,#("Alpha")))))
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
        (context (make-runtime-context
                  :filesystem-access-approval-function
                  (lambda (&rest ignored)
                    (declare (ignore ignored))
                    nil)))
        (bot (make-instance 'chatbot
                            :runtime-context context
                            :filesystem-tools-p t
                            :filesystem-root-directory root)))
    (ensure-directories-exist root)
    (with-open-file (s outside-file :direction :output :if-exists :supersede)
      (write-line "Outside" s))
    (unwind-protect
        (fiveam:signals mcp-tool-execution-error
          (execute-chatbot-tool bot
                                :built-in
                                "deleteFile"
                                `(("pathname" . ,(namestring outside-file)))))
      (delete-file outside-file)
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-execute-chatbot-tool-read-file-lines-prompts-and-persists-approved-directory
  (let* ((temp-dir (uiop:default-temporary-directory))
        (persona-root (merge-pathnames "filesystem-tool-allowlist-root/" temp-dir))
        (outside-dir (merge-pathnames "approved-dir/" temp-dir))
        (file-path (merge-pathnames "notes.txt" outside-dir))
        (allowlist-path (merge-pathnames "filesystem-allowlist.lisp" persona-root))
        (prompted-directory nil)
        (context
          (make-runtime-context
           :filesystem-access-approval-function
           (lambda (ignored-bot directory tool-name)
             (declare (ignore ignored-bot))
             (setf prompted-directory (list (namestring directory) tool-name))
             t)))
        (bot (make-instance 'chatbot
                            :runtime-context context
                            :filesystem-tools-p t
                            :filesystem-root-directory persona-root
                            :filesystem-allowlist-path allowlist-path)))
    (ensure-directories-exist persona-root)
    (ensure-directories-exist outside-dir)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Alpha" s)
      (write-line "Beta" s))
    (unwind-protect
        (progn
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

(fiveam:test test-initialize-mcp-servers-without-config-records-empty-status
  (let* ((bot (make-instance 'chatbot)))
    (let ((*read-mcp-config-function* (lambda () nil)))
      (let ((status (initialize-mcp-servers-for-chatbot bot :strict-required-p t)))
        (fiveam:is (typep status 'mcp-startup-status))
        (fiveam:is (= 0 (mcp-startup-status-configured-count status)))
        (fiveam:is (= 0 (mcp-startup-status-successful-count status)))
        (fiveam:is (= 0 (mcp-startup-status-failed-count status)))
        (fiveam:is (null (chatbot-mcp-servers bot)))
        (fiveam:is (eq status (chatbot-mcp-startup-status bot)))
        (fiveam:is-true (mcp-startup-status-strict-required-p status))))))

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

(fiveam:test test-default-stop-mcp-server-aborts-pending-requests
  (let* ((server (make-instance 'mcp-server
                                :name "test-server"
                                :input-stream (make-string-output-stream)))
         (result nil)
         (worker (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (default-mcp-send-request server "tools/list" nil :timeout 10)
                      (error (e)
                        (setf result (princ-to-string e))))))))
    (unwind-protect
        (progn
          (loop repeat 100
                until (> (hash-table-count (mcp-server-pending-requests server)) 0)
                do (sleep 0.01))
          (fiveam:is (> (hash-table-count (mcp-server-pending-requests server)) 0))
          (default-stop-mcp-server server)
          (sleep 0.1)
          (fiveam:is-false (sb-thread:thread-alive-p worker))
          (fiveam:is (stringp result))
          (fiveam:is (search "aborted" result))
          (fiveam:is (= 0 (hash-table-count (mcp-server-pending-requests server)))))
      (when (sb-thread:thread-alive-p worker)
        (sb-thread:terminate-thread worker)))))

(fiveam:test test-stop-mcp-server-process-skips-force-when-graceful-stop-succeeds
  (let ((original-alive-function (symbol-function 'mcp-server-process-alive-p))
        (original-request-stop-function (symbol-function 'request-mcp-server-process-stop))
        (original-wait-before-force-function
          (symbol-function 'wait-before-force-stopping-mcp-server-process))
        (original-wait-exit-function (symbol-function 'wait-for-mcp-server-process-exit))
        (alive-p t)
        (calls nil))
    (unwind-protect
         (progn
          (setf (symbol-function 'mcp-server-process-alive-p)
                (lambda (process)
                  (declare (ignore process))
                  alive-p))
          (setf (symbol-function 'request-mcp-server-process-stop)
                (lambda (process &key urgent-p)
                  (declare (ignore process))
                  (push (if urgent-p :force :graceful) calls)
                  (unless urgent-p
                    (setf alive-p nil))))
          (setf (symbol-function 'wait-before-force-stopping-mcp-server-process)
                (lambda ()
                  (push :wait calls)))
          (setf (symbol-function 'wait-for-mcp-server-process-exit)
                (lambda (process)
                  (declare (ignore process))
                  (push :join calls)))
          (stop-mcp-server-process :fake-process "test-server")
          (fiveam:is (equal '(:graceful :wait :join)
                            (reverse calls))))
      (setf (symbol-function 'mcp-server-process-alive-p) original-alive-function)
      (setf (symbol-function 'request-mcp-server-process-stop) original-request-stop-function)
      (setf (symbol-function 'wait-before-force-stopping-mcp-server-process)
           original-wait-before-force-function)
      (setf (symbol-function 'wait-for-mcp-server-process-exit) original-wait-exit-function))))

(fiveam:test test-stop-mcp-server-process-escalates-to-force-when-still-alive
  (let ((original-alive-function (symbol-function 'mcp-server-process-alive-p))
        (original-request-stop-function (symbol-function 'request-mcp-server-process-stop))
        (original-wait-before-force-function
          (symbol-function 'wait-before-force-stopping-mcp-server-process))
        (original-force-stop-function (symbol-function 'force-stop-mcp-server-process))
        (original-wait-exit-function (symbol-function 'wait-for-mcp-server-process-exit))
        (alive-p t)
        (calls nil))
    (unwind-protect
         (progn
          (setf (symbol-function 'mcp-server-process-alive-p)
                (lambda (process)
                  (declare (ignore process))
                  alive-p))
          (setf (symbol-function 'request-mcp-server-process-stop)
                (lambda (process &key urgent-p)
                  (declare (ignore process urgent-p))
                  (push :graceful calls)))
          (setf (symbol-function 'wait-before-force-stopping-mcp-server-process)
                (lambda ()
                  (push :wait calls)))
          (setf (symbol-function 'force-stop-mcp-server-process)
                (lambda (process name)
                  (declare (ignore process name))
                  (push :force calls)
                  (setf alive-p nil)))
          (setf (symbol-function 'wait-for-mcp-server-process-exit)
                (lambda (process)
                  (declare (ignore process))
                  (push :join calls)))
          (stop-mcp-server-process :fake-process "test-server")
          (fiveam:is (equal '(:graceful :wait :force :join)
                            (reverse calls))))
      (setf (symbol-function 'mcp-server-process-alive-p) original-alive-function)
      (setf (symbol-function 'request-mcp-server-process-stop) original-request-stop-function)
      (setf (symbol-function 'wait-before-force-stopping-mcp-server-process)
           original-wait-before-force-function)
      (setf (symbol-function 'force-stop-mcp-server-process) original-force-stop-function)
      (setf (symbol-function 'wait-for-mcp-server-process-exit) original-wait-exit-function))))

(fiveam:test test-mcp-reader-loop-aborts-pending-requests-on-eof
  (let* ((server (make-instance 'mcp-server
                               :name "test-server"
                                :output-stream (make-string-input-stream "")))
         (mailbox (sb-concurrency:make-mailbox)))
    (mcp-register-request server 7 mailbox)
    (mcp-reader-loop server)
    (let ((message (sb-concurrency:receive-message mailbox)))
      (fiveam:is (mcp-request-aborted-message-p message))
      (fiveam:is (search "reader stopped" (mcp-request-aborted-reason message)))
      (fiveam:is (= 0 (hash-table-count (mcp-server-pending-requests server)))))))

(fiveam:test test-default-start-mcp-server-cleans-up-after-supervision-failure
  (let* ((mock-server-path (merge-pathnames "mock-mcp-server.lisp"
                                            (asdf:system-source-directory :chatbot)))
         (original-supervision-function (symbol-function 'start-mcp-server-supervision))
         (captured-server nil)
         (captured-process nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'start-mcp-server-supervision)
                (lambda (server)
                  (setf captured-server server)
                  (setf captured-process (mcp-server-process server))
                  (error "supervision setup failed")))
          (fiveam:signals error
            (default-start-mcp-server "test-server"
                                      "sbcl"
                                      (list "--script" (namestring mock-server-path))))
          (fiveam:is (typep captured-server 'mcp-server))
          (fiveam:is-true captured-process)
          (fiveam:is-false (uiop:process-alive-p captured-process))
          (fiveam:is-false (mcp-server-process captured-server))
          (fiveam:is-false (mcp-server-input-stream captured-server))
          (fiveam:is-false (mcp-server-output-stream captured-server))
          (fiveam:is-false (mcp-server-error-stream captured-server))
          (fiveam:is-false (mcp-server-reader-thread captured-server))
          (fiveam:is-false (mcp-server-stderr-thread captured-server)))
      (setf (symbol-function 'start-mcp-server-supervision) original-supervision-function)
      (when (and captured-process
                 (uiop:process-alive-p captured-process))
        (uiop:terminate-process captured-process :urgent t)
        (uiop:wait-process captured-process)))))

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
           (let* ((bello-conv (first (chatbot-subordinates bot)))
                  (bello-inst (system-instruction-text
                               (chatbot-system-instruction
                                (conversation-chatbot bello-conv)))))
             (fiveam:is (search "You are Bello" bello-inst))
             (fiveam:is (search +agentic-operational-directive+ bello-inst)))
           ;; 3. List minions again (expect 1)
           (let* ((list-res (execute-chatbot-tool-by-name bot "listMinions" '()))
                  (parsed (cl-json:decode-json-from-string list-res)))
             (fiveam:is (= 1 (length parsed)))
             (fiveam:is (string-equal "Bello" (cdr (assoc :name (first parsed)))))
             (fiveam:is (string-equal "delegated:Bello" (cdr (assoc :worker-id (first parsed)))))
             (fiveam:is (string-equal "delegated" (cdr (assoc :kind (first parsed)))))
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
           (let* ((jerry-conv (find "Jerry"
                                    (chatbot-subordinates bot)
                                    :key (lambda (conversation)
                                           (chatbot-persona-name
                                            (conversation-chatbot conversation)))
                                    :test #'string-equal))
                  (jerry-inst (system-instruction-text
                               (chatbot-system-instruction
                                (conversation-chatbot jerry-conv)))))
             (fiveam:is (search +agentic-operational-directive+ jerry-inst)))
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

(fiveam:test test-qwen-minion-instructions-include-operational-directive
  (let* ((bot (conversation-chatbot (new-chat :backend :openai
                                             :model "qwen2.5-coder"))))
    (append-delegation-instructions bot "QwenWorker" 2 256)
    (let ((instruction (system-instruction-text (chatbot-system-instruction bot))))
      (fiveam:is (search +agentic-operational-directive+ instruction))
      (fiveam:is (search "Do NOT request delegation or child spawns." instruction))
      (fiveam:is (search "{\"reply\":\"plain text for the parent shell\",\"spawn\":null}" instruction)))))

(fiveam:test test-worker-tools-wrap-minion-lifecycle
  (let* ((bot (conversation-chatbot (new-chat :backend :gemini)))
         (*openai-api-key* "mocked-openai-key")
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore args))
             (if (search "api.openai.com" url)
                 (values
                  (make-string-input-stream
                   (test-openai-subordinate-stream "Worker Bello here."))
                  200)
                 (values
                  (make-string-input-stream
                   (test-gemini-subordinate-stream "Worker Bello here." :model "gpt-4o"))
                  200)))))
    (let* ((spawn-payload (parse-json-or-error
                           (execute-chatbot-tool-by-name bot "spawnWorker"
                                                         '(("mode" . "delegated")
                                                           ("name" . "Bello")
                                                           ("backend" . "openai")
                                                           ("model" . "gpt-4o")
                                                           ("systemInstruction" . "You are Bello")))
                           :context "spawnWorker delegated result"))
           (worker-id (json-object-field spawn-payload "workerId"))
           (read-payload (parse-json-or-error
                          (execute-chatbot-tool-by-name bot "readWorker"
                                                        `(("workerId" . ,worker-id)))
                          :context "readWorker delegated result"))
           (list-payload (parse-json-or-error
                          (execute-chatbot-tool-by-name bot "listWorkers" '())
                          :context "listWorkers delegated result"))
           (workers (json-object-field list-payload "workers")))
      (assert-json-field= spawn-payload :workerId "delegated:Bello")
      (assert-json-field= spawn-payload :kind "delegated")
      (assert-json-field= read-payload :name "Bello")
      (fiveam:is (find worker-id workers
                       :key (lambda (worker) (json-object-field worker "workerId"))
                       :test #'string=))
      (let ((entry (find-runtime-worker-entry worker-id (chatbot-runtime-context bot))))
        (fiveam:is (not (null entry)))
        (fiveam:is (eq :delegated (runtime-worker-entry-kind entry)))
        (fiveam:is (eq bot (runtime-worker-entry-owner-bot entry))))
      (fiveam:is (string= "Worker Bello here."
                          (execute-chatbot-tool-by-name bot "promptWorker"
                                                        `(("workerId" . ,worker-id)
                                                          ("prompt" . "Hi Bello")))))
      (let ((abort-payload (parse-json-or-error
                            (execute-chatbot-tool-by-name bot "abortWorker"
                                                          `(("workerId" . ,worker-id)))
                            :context "abortWorker delegated result")))
        (assert-json-field= abort-payload :workerId worker-id)
        (assert-json-field= abort-payload :status "dismissed")))))

(fiveam:test test-runtime-worker-kind-normalization
  (let ((delegated-entry (make-runtime-worker-entry :worker-id "delegated:Alias"
                                                    :kind :subordinate))
        (loop-entry (make-runtime-worker-entry :worker-id "loop:42"
                                               :kind "autonomous")))
    (fiveam:is (eq :delegated (runtime-worker-entry-kind delegated-entry)))
    (fiveam:is (eq :loop (runtime-worker-entry-kind loop-entry)))
    (fiveam:is (string= "delegated"
                        (runtime-worker-kind-public-name
                         (runtime-worker-entry-kind delegated-entry))))
    (fiveam:is (string= "autonomous"
                        (runtime-worker-kind-public-name
                         (runtime-worker-entry-kind loop-entry))))))

(fiveam:test test-spawn-worker-tool-planner-mode
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-worker-planner-mode/" temp-dir))
         (parent-conv (new-chat :backend :google))
         (bot (conversation-chatbot parent-conv))
         (context (chatbot-runtime-context bot))
         (original-active-conversation (current-active-conversation context))
         (original-active-planner (current-active-planner context))
         (original-parent-conversation (current-active-planner-parent-conversation context)))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (setf (current-active-planner context) nil)
          (setf (current-active-planner-parent-conversation context) nil)
          (setf (current-active-conversation context) parent-conv)
          (let* ((payload (parse-json-or-error
                           (execute-chatbot-tool-by-name bot "spawnWorker"
                                                         '(("mode" . "planner")
                                                           ("goal" . "Develop schema for leaders.")))
                           :context "spawnWorker planner result"))
                 (planner-conv (current-active-planner context)))
            (fiveam:is-true (typep planner-conv 'conversation))
            (assert-json-field= payload :kind "planner")
            (assert-json-field= payload :name "Planner")
            (assert-json-field= payload :workerId "planner:Planner")
            (fiveam:is (eq parent-conv (current-active-planner-parent-conversation context)))))
      (setf (current-active-conversation context) original-active-conversation)
      (setf (current-active-planner context) original-active-planner)
      (setf (current-active-planner-parent-conversation context) original-parent-conversation)
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-abort-worker-tool-dismisses-planner-and-clears-active-state
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-worker-planner-abort/" temp-dir))
         (parent-conv (new-chat :backend :google))
         (bot (conversation-chatbot parent-conv))
         (context (chatbot-runtime-context bot))
         (original-active-conversation (current-active-conversation context))
         (original-active-planner (current-active-planner context))
         (original-parent-conversation (current-active-planner-parent-conversation context)))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (setf (current-active-planner context) nil)
          (setf (current-active-planner-parent-conversation context) nil)
          (setf (current-active-conversation context) parent-conv)
          (let* ((spawn-payload (parse-json-or-error
                                (execute-chatbot-tool-by-name bot "spawnWorker"
                                                              '(("mode" . "planner")
                                                                ("goal" . "Prepare a plan.")))
                                :context "spawnWorker planner result"))
                (worker-id (json-object-field spawn-payload "workerId"))
                (abort-payload (parse-json-or-error
                                (execute-chatbot-tool-by-name bot "abortWorker"
                                                              `(("workerId" . ,worker-id)))
                                :context "abortWorker planner result")))
            (assert-json-field= abort-payload :workerId worker-id)
            (assert-json-field= abort-payload :status "dismissed")
            (fiveam:is-false (current-active-planner context))
            (fiveam:is-false (current-active-planner-parent-conversation context))
            (fiveam:is-false (find-runtime-worker-entry worker-id context))
            (fiveam:is (= 0 (length (chatbot-subordinates bot))))))
      (setf (current-active-conversation context) original-active-conversation)
      (setf (current-active-planner context) original-active-planner)
      (setf (current-active-planner-parent-conversation context) original-parent-conversation)
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

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
      (fiveam:is (= 250 (chatbot-spent-tokens bot))))))

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
                    200))))
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

(fiveam:test test-planner-checkpoint-and-recovery
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-planner-recovery/" temp-dir))
         (mock-minions-dir (merge-pathnames "data/minions/" mock-home))
         (parent-conv (new-chat :backend :gemini :persona-name "Top-Boss"))
         (bot (conversation-chatbot parent-conv))
         (*minions-data-directory* mock-minions-dir))
    (ensure-directories-exist mock-minions-dir)
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((planner-payload (parse-json-or-error
                                    (execute-chatbot-tool-by-name bot
                                                                  "spawnWorker"
                                                                  '(("mode" . "planner")
                                                                    ("goal" . "Recover planner mode")))
                                    :context "spawnWorker planner result"))
                  (planner-file (merge-pathnames "Planner.json" mock-minions-dir)))
             (assert-json-field= planner-payload :workerId "planner:Planner")
             (fiveam:is (not (null (probe-file planner-file))))
             (let* ((fresh-parent-conv (new-chat :backend :gemini :persona-name "Top-Boss"))
                    (fresh-bot (conversation-chatbot fresh-parent-conv))
                    (fresh-context (chatbot-runtime-context fresh-bot)))
               (setf (current-active-conversation fresh-context) fresh-parent-conv)
               (restore-minions fresh-bot)
               (let* ((subs (chatbot-subordinates fresh-bot))
                      (restored-planner (first subs))
                      (restored-planner-bot (and restored-planner
                                                 (conversation-chatbot restored-planner)))
                      (entry (find-runtime-worker-entry "planner:Planner" fresh-context)))
                 (fiveam:is (= 1 (length subs)))
                 (fiveam:is-true (chatbot-planner-p restored-planner-bot))
                 (fiveam:is (string= "Planner" (chatbot-persona-name restored-planner-bot)))
                 (fiveam:is (eq restored-planner (current-active-planner fresh-context)))
                 (fiveam:is (eq fresh-parent-conv
                                (current-active-planner-parent-conversation fresh-context)))
                 (fiveam:is (not (null entry)))
                 (fiveam:is (eq :planner (runtime-worker-entry-kind entry)))))))
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

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
         (context (chatbot-runtime-context bot))
         (original-active-planner (current-active-planner context))
         (original-parent-conversation (current-active-planner-parent-conversation context))
         (plan-content "## My Strategic Plan~%1. Learn Lisp~%2. Rule the world.~%")
         (res-text nil))
    (unwind-protect
         (progn
           (setf (current-active-planner context) parent-conv)
           (setf (current-active-planner-parent-conversation context) parent-conv)
           (setf res-text
                 (execute-chatbot-tool-by-name bot "submitPlan" `(("planContent" . ,plan-content))))
           (fiveam:is (stringp res-text))
           (fiveam:is (search "Plan saved successfully to" res-text))
           (fiveam:is-false (current-active-planner context))
           (let ((history (conversation-messages parent-conv)))
             (fiveam:is (= 1 (length history)))
             (let ((msg (first history)))
               (fiveam:is (string= "user" (cdr (assoc "role" msg :test #'string=))))
               (fiveam:is (search "[System: Plan saved to plans/plan-"
                                  (cdr (assoc "content" msg :test #'string=)))))))
      (setf (current-active-planner context) original-active-planner)
      (setf (current-active-planner-parent-conversation context) original-parent-conversation))
    (let* ((files (and (uiop:directory-exists-p "plans/")
                       (uiop:directory-files "plans/" "*.md"))))
      (dolist (f files)
        (delete-file f))
      (when (uiop:directory-exists-p "plans/")
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname "plans/") :validate t)))))

(fiveam:test test-abort-plan-tool
  (let* ((parent-conv (new-chat :backend :google))
         (bot (conversation-chatbot parent-conv))
         (context (chatbot-runtime-context bot))
         (original-active-planner (current-active-planner context))
         (original-parent-conversation (current-active-planner-parent-conversation context))
         (res-text nil))
    (unwind-protect
         (progn
           (setf (current-active-planner context) parent-conv)
           (setf (current-active-planner-parent-conversation context) parent-conv)
           (setf res-text
                 (execute-chatbot-tool-by-name bot "abortPlan" '(("reason" . "User decided to cancel."))))
           (fiveam:is (stringp res-text))
           (fiveam:is (search "Planner mode aborted" res-text))
           (fiveam:is-false (current-active-planner context))
           (let ((history (conversation-messages parent-conv)))
             (fiveam:is (= 1 (length history)))
             (let ((msg (first history)))
               (fiveam:is (string= "user" (cdr (assoc "role" msg :test #'string=))))
               (fiveam:is (string= "[System: Planner mode aborted.]"
                                   (cdr (assoc "content" msg :test #'string=)))))))
      (setf (current-active-planner context) original-active-planner)
      (setf (current-active-planner-parent-conversation context) original-parent-conversation))))

(fiveam:test test-invoke-planner-tool
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-invoke-planner/" temp-dir))
        (parent-conv (new-chat :backend :google))
        (bot (conversation-chatbot parent-conv))
        (context (chatbot-runtime-context bot))
        (original-active-conversation (current-active-conversation context))
        (original-active-planner (current-active-planner context))
        (original-parent-conversation (current-active-planner-parent-conversation context))
        (res-text nil))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (setf (current-active-planner context) nil)
          (setf (current-active-planner-parent-conversation context) nil)
          (setf (current-active-conversation context) parent-conv)
          (setf res-text
                (execute-chatbot-tool-by-name bot "invokePlanner"
                                              '(("contextSummary" . "Develop schema for leaders."))))
          (fiveam:is (stringp res-text))
          (fiveam:is (search "Planner minion successfully spawned" res-text))
          (fiveam:is-true (typep (current-active-planner context) 'conversation))
          (fiveam:is (eq parent-conv (current-active-planner-parent-conversation context)))
          (let* ((planner-conv (current-active-planner context))
                 (planner-bot (conversation-chatbot planner-conv))
                 (history (conversation-messages planner-conv))
                 (tools (default-get-all-builtin-tools planner-bot))
                 (tool-names (mapcar (lambda (entry)
                                       (mcp-val :name (cdr entry)))
                                     tools)))
            (fiveam:is-true (chatbot-planner-p planner-bot))
            (fiveam:is (string= "Planner" (chatbot-persona-name planner-bot)))
            (fiveam:is (string= +planner-system-instruction+ (chatbot-system-instruction planner-bot)))
            (fiveam:is (search +agentic-operational-directive+
                               (chatbot-system-instruction planner-bot)))
            (fiveam:is (search "TOOL EXECUTION MANDATE (NO HALLUCINATIONS)"
                               (chatbot-system-instruction planner-bot)))
            (fiveam:is (= 1 (length history)))
            (let ((msg (first history)))
              (fiveam:is (string= "user" (cdr (assoc "role" msg :test #'string=))))
              (fiveam:is (search "Planning Session Initiated."
                                 (cdr (assoc "content" msg :test #'string=))))
              (fiveam:is (search "Develop schema for leaders."
                                 (cdr (assoc "content" msg :test #'string=)))))
            (fiveam:is (= 6 (length tool-names)))
            (fiveam:is (member "readFileLines" tool-names :test #'string=))
            (fiveam:is (member "directory" tool-names :test #'string=))
            (fiveam:is (member "webSearch" tool-names :test #'string=))
            (fiveam:is (member "hyperspecSearch" tool-names :test #'string=))
            (fiveam:is (member "submitPlan" tool-names :test #'string=))
            (fiveam:is (member "abortPlan" tool-names :test #'string=))
            (fiveam:is-false (member "eval" tool-names :test #'string=))
            (fiveam:is-false (member "writeFile" tool-names :test #'string=))
            (fiveam:is-false (member "deleteFile" tool-names :test #'string=))
            (fiveam:is-false (member "spawnMinion" tool-names :test #'string=))))
      (setf (current-active-conversation context) original-active-conversation)
      (setf (current-active-planner context) original-active-planner)
      (setf (current-active-planner-parent-conversation context) original-parent-conversation)
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

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
           (let ((inst (system-instruction-text (chatbot-system-instruction bot))))
             (fiveam:is (search "Base instruction." inst))
             (fiveam:is (search "[EXECUTING PLAN FROM test-plan-file-123.md]" inst))
             (fiveam:is (search "## Step 1: Code." inst))))
      (when (probe-file filename)
        (delete-file filename)))))

(fiveam:test test-load-plan-to-system-instructions-replaces-older-plan
  (let* ((bot (conversation-chatbot (new-chat :backend :google :system-instruction "Base instruction.")))
         (first-file "test-plan-file-older.md")
         (second-file "test-plan-file-newer.md"))
    (with-open-file (s first-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string "Old plan body." s))
    (with-open-file (s second-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string "New plan body." s))
    (unwind-protect
         (progn
           (load-plan-to-system-instructions bot first-file)
           (load-plan-to-system-instructions bot second-file)
           (let ((inst (system-instruction-text (chatbot-system-instruction bot))))
            (fiveam:is (search "Base instruction." inst))
            (fiveam:is-false (search "Old plan body." inst))
            (fiveam:is-false (search "[EXECUTING PLAN FROM test-plan-file-older.md]" inst))
            (fiveam:is (search "[EXECUTING PLAN FROM test-plan-file-newer.md]" inst))
            (fiveam:is (search "New plan body." inst))))
      (when (probe-file first-file)
        (delete-file first-file))
      (when (probe-file second-file)
        (delete-file second-file)))))

(fiveam:test test-chat-routing-to-active-planner
  (let* ((custom-context (make-runtime-context))
         (parent-conv (new-chat :backend :google :runtime-context custom-context))
         (planner-conv (new-chat :backend :google :runtime-context custom-context))
         (original-active-planner (current-active-planner custom-context))
         (original-parent-conversation (current-active-planner-parent-conversation custom-context))
         (parent-called-p nil)
         (planner-called-p nil))
    (unwind-protect
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
           (setf (current-active-planner custom-context) planner-conv)
           (setf (current-active-planner-parent-conversation custom-context) parent-conv)
           (setf (runtime-context-http-post-function custom-context) mock-post-fn)
           (let ((*http-post-function* mock-post-fn))
             (let ((res (chat "User message for Parent." :conversation parent-conv)))
               (fiveam:is (string= "Planner Response." res))
               (fiveam:is-true planner-called-p)
               (fiveam:is-false parent-called-p))))
      (setf (current-active-planner custom-context) original-active-planner)
      (setf (current-active-planner-parent-conversation custom-context) original-parent-conversation))))

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

(fiveam:test test-checkpoint-name-validation-and-defaults
  (let* ((conv1 (new-chat))
         (bot1 (conversation-chatbot conv1))
         (conv2 (new-chat :persona-name "Bob"))
         (bot2 (conversation-chatbot conv2))
         (conv3 (new-chat :checkpoint-name "TaskX"))
         (bot3 (conversation-chatbot conv3)))
    (fiveam:is (string= "DefaultConversation" (chatbot-checkpoint-name bot1)))
    (fiveam:is (string= "DefaultConversation" (conversation-checkpoint-name conv1)))
    (fiveam:is (string= "Bob" (chatbot-checkpoint-name bot2)))
    (fiveam:is (string= "Bob" (conversation-checkpoint-name conv2)))
    (fiveam:is (string= "TaskX" (chatbot-checkpoint-name bot3)))
    (fiveam:is (string= "TaskX" (conversation-checkpoint-name conv3)))
    ;; Verify error is thrown if checkpoint name is empty/nil
    (setf (chatbot-checkpoint-name bot1) nil)
    (fiveam:signals error (conversation-checkpoint-name conv1))
    (fiveam:signals error (save-minion-state conv1))))

(fiveam:test test-chat-checkpoint-for-default-conversation
  (let* ((custom-context (make-runtime-context))
         (*default-conversation* (new-chat :backend :google :runtime-context custom-context))
         (*minions-data-directory* (merge-pathnames "default-conversation-checkpoints/"
                                                    (uiop:default-temporary-directory)))
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
         (*minions-data-directory* (merge-pathnames "restore-conversation-checkpoints/"
                                                    (uiop:default-temporary-directory)))
         (checkpoint-file (merge-pathnames filename (minions-data-directory)))
          (system-instruction-path (merge-pathnames "restore-system-instruction.md"
                                                  (uiop:temporary-directory)))
         (scoped-directory (uiop:ensure-directory-pathname
                            (merge-pathnames "restore-scope/" (uiop:temporary-directory))))
          (filesystem-root-directory (uiop:ensure-directory-pathname
                                    (merge-pathnames "restore-filesystem-root/"
                                                     (uiop:temporary-directory))))
          (filesystem-allowed-directory (uiop:ensure-directory-pathname
                                       (merge-pathnames "restore-allowed/"
                                                        (uiop:temporary-directory))))
          (filesystem-allowlist-path (merge-pathnames "restore-filesystem-allowlist.lisp"
                                                    (uiop:temporary-directory)))
         (bot (conversation-chatbot (new-chat :backend :google
                                              :model "gemini-3.5-flash"
                                              :system-instruction "Sterile context."
                                              :system-instruction-path system-instruction-path
                                              :system-instruction-storage-kind :markdown-file
                                              :temperature 0.75d0
                                              :top-p 0.9d0
                                              :content-cache-policy :auto
                                              :content-cache-ttl-seconds 1800
                                              :content-cache-min-tokens 256
                                              :google-search-p t
                                              :gemini-fallback-to-google-p t
                                              :web-tools-p t
                                              :code-execution-p t
                                              :include-timestamp-p t
                                              :include-model-p t
                                              :enable-eval-p t
                                              :enable-git-tools-p t
                                              :filesystem-tools-p t
                                              :filesystem-root-directory filesystem-root-directory
                                              :filesystem-allowed-directories
                                              (list filesystem-allowed-directory)
                                              :filesystem-allowlist-path filesystem-allowlist-path
                                              :parent-name "Supervisor"
                                              :depth 4
                                              :token-budget 900
                                              :spent-tokens 321
                                              :scoped-directory scoped-directory
                                              :filesystem-read-only-p t)))
         (conv (make-instance 'conversation :chatbot bot)))
    (setf (chatbot-checkpoint-name bot) "TestRestoreConv")
    (setf (conversation-adaptive-context-pruning-max-tokens conv) 84)
    (setf (conversation-interaction-id conv) "interaction-42")
    (setf (conversation-cached-content-name conv) "cachedContents/abc123")
    (setf (conversation-cached-content-key conv) "fingerprint-1")
    (setf (conversation-cached-content-metadata conv)
          '((:ttl . "1800s") (:display-name . "Test cache")))
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
             (fiveam:is (equal system-instruction-path
                               (chatbot-system-instruction-path r-bot)))
             (fiveam:is (eq :markdown-file
                            (chatbot-system-instruction-storage-kind r-bot)))
             (fiveam:is (= 0.75d0 (chatbot-temperature r-bot)))
             (fiveam:is (= 0.9d0 (chatbot-top-p r-bot)))
             (fiveam:is (eq :auto (chatbot-content-cache-policy r-bot)))
             (fiveam:is (= 1800 (chatbot-content-cache-ttl-seconds r-bot)))
             (fiveam:is (= 256 (chatbot-content-cache-min-tokens r-bot)))
             (fiveam:is-true (chatbot-google-search-p r-bot))
             (fiveam:is-true (chatbot-gemini-fallback-to-google-p r-bot))
             (fiveam:is-true (chatbot-web-tools-p r-bot))
             (fiveam:is-true (chatbot-code-execution-p r-bot))
             (fiveam:is-true (chatbot-include-timestamp-p r-bot))
             (fiveam:is-true (chatbot-include-model-p r-bot))
             (fiveam:is-true (chatbot-enable-eval-p r-bot))
             (fiveam:is-true (chatbot-enable-git-tools-p r-bot))
             (fiveam:is-true (chatbot-filesystem-tools-p r-bot))
             (fiveam:is (equal (chatbot-filesystem-root-directory bot)
                               (chatbot-filesystem-root-directory r-bot)))
             (fiveam:is (equal (list filesystem-allowed-directory)
                               (chatbot-filesystem-allowed-directories r-bot)))
             (fiveam:is (equal filesystem-allowlist-path
                               (chatbot-filesystem-allowlist-path r-bot)))
             (fiveam:is-true (chatbot-filesystem-read-only-p r-bot))
             (fiveam:is (string= "Supervisor" (chatbot-parent-name r-bot)))
             (fiveam:is (= 4 (chatbot-depth r-bot)))
             (fiveam:is (= 900 (chatbot-token-budget r-bot)))
             (fiveam:is (= 321 (chatbot-spent-tokens r-bot)))
             (fiveam:is (string= "TestRestoreConv" (chatbot-checkpoint-name r-bot)))
             (fiveam:is-false (chatbot-persona-name r-bot))
             (fiveam:is-false (chatbot-persona-source-name r-bot))
             (fiveam:is (equal scoped-directory (chatbot-scoped-directory r-bot)))
             (fiveam:is (= 84 (conversation-adaptive-context-pruning-max-tokens restored)))
             (fiveam:is (string= "interaction-42" (conversation-interaction-id restored)))
             (fiveam:is (string= "cachedContents/abc123" (conversation-cached-content-name restored)))
             (fiveam:is (string= "fingerprint-1" (conversation-cached-content-key restored)))
             (fiveam:is (equal '((:ttl . "1800s") (:display-name . "Test cache"))
                               (conversation-cached-content-metadata restored)))
             (fiveam:is (equal (list (list (cons "role" "user") (cons "content" "Original Msg")))
                               (conversation-messages restored)))))
      (when (probe-file checkpoint-file)
        (delete-file checkpoint-file)))))

(fiveam:test test-restore-persona-backed-minion-preserves-source-persona-metadata
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-restore-persona-minion/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (worker-persona-dir (merge-pathnames "restore-worker-persona/" personas-dir))
         (helper-persona-dir (merge-pathnames "restore-helper-persona/" personas-dir))
         (mock-minions-dir (merge-pathnames "data/minions/" mock-home))
         (outside-dir (merge-pathnames "restore-allowlisted/" temp-dir))
         (allowlist-path (merge-pathnames "filesystem-allowlist.lisp" worker-persona-dir))
         (worker-system-instruction-path (merge-pathnames "system-instruction.md" worker-persona-dir))
         (*minions-data-directory* mock-minions-dir))
    (ensure-directories-exist worker-persona-dir)
    (ensure-directories-exist helper-persona-dir)
    (ensure-directories-exist outside-dir)
    (with-open-file (s (merge-pathnames "config.lisp" worker-persona-dir)
                     :direction :output
                     :if-exists :supersede)
      (write-line "(:model \"persona-model\" :enable-filesystem-tools t :subordinates (\"restore-helper-persona\"))" s))
    (with-open-file (s worker-system-instruction-path
                     :direction :output
                     :if-exists :supersede)
      (write-line "Worker persona instructions." s))
    (with-open-file (s allowlist-path
                     :direction :output
                     :if-exists :supersede)
      (prin1 (list (namestring outside-dir)) s))
    (with-open-file (s (merge-pathnames "config.lisp" helper-persona-dir)
                     :direction :output
                     :if-exists :supersede)
      (write-line "(:model \"helper-model\")" s))
    (ensure-directories-exist mock-minions-dir)
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((parent-conv (new-chat :backend :gemini :persona-name "Top-Boss"))
                  (bot (conversation-chatbot parent-conv)))
             (execute-chatbot-tool-by-name bot
                                           "spawnMinion"
                                           '(("name" . "Jerry")
                                             ("personaName" . "restore-worker-persona")))
             (let* ((fresh-parent-conv (new-chat :backend :gemini :persona-name "Top-Boss"))
                    (fresh-bot (conversation-chatbot fresh-parent-conv)))
               (restore-minions fresh-bot)
               (let* ((restored-jerry (first (chatbot-subordinates fresh-bot)))
                      (restored-jerry-bot (and restored-jerry
                                               (conversation-chatbot restored-jerry))))
                 (fiveam:is (not (null restored-jerry-bot)))
                 (fiveam:is (string= "Jerry" (chatbot-persona-name restored-jerry-bot)))
                 (fiveam:is (string= "Jerry" (chatbot-checkpoint-name restored-jerry-bot)))
                 (fiveam:is (string= "restore-worker-persona"
                                     (chatbot-persona-source-name restored-jerry-bot)))
                 (fiveam:is (equal (persona-filesystem-allowlist-path worker-persona-dir)
                                   (chatbot-filesystem-allowlist-path restored-jerry-bot)))
                 (fiveam:is (equal (list (uiop:ensure-directory-pathname (truename outside-dir)))
                                   (chatbot-filesystem-allowed-directories restored-jerry-bot)))
                 (fiveam:is (equal worker-system-instruction-path
                                   (chatbot-system-instruction-path restored-jerry-bot)))
                 (fiveam:is (null (chatbot-subordinates restored-jerry-bot)))))))
      (when (uiop:directory-exists-p outside-dir)
        (uiop:delete-directory-tree outside-dir :validate t))
      (when (uiop:directory-exists-p mock-home)
        (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-restore-persona-backed-checkpoint-preserves-persisted-option-overrides
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-restore-persona-overrides/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (worker-persona-dir (merge-pathnames "restore-override-persona/" personas-dir))
        (mock-minions-dir (merge-pathnames "data/minions/" mock-home))
        (filename "PersonaRestoreOptions.json")
        (checkpoint-file (merge-pathnames filename mock-minions-dir))
        (filesystem-root-directory (uiop:ensure-directory-pathname
                                    (merge-pathnames "persona-restore-root/" temp-dir)))
        (filesystem-allowed-directory (uiop:ensure-directory-pathname
                                       (merge-pathnames "persona-restore-allowed/" temp-dir)))
        (*minions-data-directory* mock-minions-dir))
    (ensure-directories-exist worker-persona-dir)
    (ensure-directories-exist mock-minions-dir)
    (ensure-directories-exist filesystem-root-directory)
    (ensure-directories-exist filesystem-allowed-directory)
    (with-open-file (s (merge-pathnames "config.lisp" worker-persona-dir)
                    :direction :output
                    :if-exists :supersede)
      (write-line "(:model \"persona-model\" :enable-filesystem-tools nil :enable-web-tools nil :code-execution-p nil :include-timestamp nil :include-model nil :enable-eval nil)" s))
    (with-open-file (s (merge-pathnames "system-instruction.md" worker-persona-dir)
                    :direction :output
                    :if-exists :supersede)
      (write-line "Original persona instructions." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "restore-override-persona"))
                 (bot (conversation-chatbot conv)))
            (setf (chatbot-checkpoint-name bot) "PersonaRestoreOptions")
            (setf (chatbot-system-instruction bot) "Persisted override instructions.")
            (setf (chatbot-temperature bot) 0.55d0)
            (setf (chatbot-top-p bot) 0.8d0)
            (setf (chatbot-google-search-p bot) t)
            (setf (chatbot-gemini-fallback-to-google-p bot) t)
            (setf (chatbot-web-tools-p bot) t)
            (setf (chatbot-code-execution-p bot) t)
            (setf (chatbot-include-timestamp-p bot) t)
            (setf (chatbot-include-model-p bot) t)
            (setf (chatbot-enable-eval-p bot) t)
            (setf (chatbot-enable-git-tools-p bot) t)
            (setf (chatbot-filesystem-tools-p bot) t)
            (setf (chatbot-filesystem-root-directory bot) filesystem-root-directory)
            (setf (chatbot-filesystem-allowed-directories bot)
                  (list filesystem-allowed-directory))
            (setf (chatbot-filesystem-read-only-p bot) t)
            (save-minion-state conv)
            (fiveam:is-true (probe-file checkpoint-file))
            (let* ((restored (restore-conversation-from-checkpoint filename))
                   (restored-bot (conversation-chatbot restored)))
              (fiveam:is (string= "restore-override-persona"
                                  (chatbot-persona-source-name restored-bot)))
              (fiveam:is (string= "Persisted override instructions."
                                  (chatbot-system-instruction restored-bot)))
              (fiveam:is (= 0.55d0 (chatbot-temperature restored-bot)))
              (fiveam:is (= 0.8d0 (chatbot-top-p restored-bot)))
              (fiveam:is-true (chatbot-google-search-p restored-bot))
              (fiveam:is-true (chatbot-gemini-fallback-to-google-p restored-bot))
              (fiveam:is-true (chatbot-web-tools-p restored-bot))
              (fiveam:is-true (chatbot-code-execution-p restored-bot))
              (fiveam:is-true (chatbot-include-timestamp-p restored-bot))
              (fiveam:is-true (chatbot-include-model-p restored-bot))
              (fiveam:is-true (chatbot-enable-eval-p restored-bot))
              (fiveam:is-true (chatbot-enable-git-tools-p restored-bot))
              (fiveam:is-true (chatbot-filesystem-tools-p restored-bot))
              (fiveam:is (equal filesystem-root-directory
                                (chatbot-filesystem-root-directory restored-bot)))
              (fiveam:is (equal (list filesystem-allowed-directory)
                                (chatbot-filesystem-allowed-directories restored-bot)))
              (fiveam:is-true (chatbot-filesystem-read-only-p restored-bot)))))
      (when (probe-file checkpoint-file)
       (delete-file checkpoint-file))
      (when (uiop:directory-exists-p mock-home)
       (uiop:delete-directory-tree mock-home :validate t))
      (when (uiop:directory-exists-p filesystem-root-directory)
       (uiop:delete-directory-tree filesystem-root-directory :validate t))
      (when (uiop:directory-exists-p filesystem-allowed-directory)
       (uiop:delete-directory-tree filesystem-allowed-directory :validate t)))))
