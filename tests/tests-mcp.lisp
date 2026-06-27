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

(fiveam:test test-merge-mcp-server-environments-inherits-and-overrides
  (let ((merged (merge-mcp-server-environments
                 '("PATH=C:\\Windows\\System32" "MEMORY_FILE_PATH=default.json")
                 '(("MEMORY_FILE_PATH" . "persona.json")
                   ("HOME" . "C:\\Users\\bitdi")))))
    (fiveam:is (equal '(("PATH" . "C:\\Windows\\System32")
                        ("MEMORY_FILE_PATH" . "persona.json")
                        ("HOME" . "C:\\Users\\bitdi"))
                      merged))))

(fiveam:test test-mcp-request-json-encodes-tool-arguments-as-records
  (let* ((payload `((:jsonrpc . "2.0")
                    (:id . 1)
                    (:method . "tools/call")
                    (:params . ((:name . "add_observations")
                                (:arguments . ((:observations ((:entityName . "Boss")
                                                               (:contents "watching")))))))))
         (json (cl-json:encode-json-to-string (json-encodable-value payload))))
    (fiveam:is (search "\"arguments\":{\"observations\":[{"
                       json))
    (fiveam:is (search "\"contents\":[\"watching\"]"
                       json))
    (fiveam:is-false (search "\"arguments\":["
                             json))))

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
                         (let ((*standard-output* s))
                           (fiveam:is (equal '((:result . "ok"))
                                             (default-mcp-send-request
                                              server
                                              "tools/call"
                                              '((:name . "echo_tool")
                                                (:arguments . ((:value . "payload")))))))))))
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
    (fiveam:is (search "\"temperature\":0.2" initial))
    (fiveam:is (search "\"topP\":0.3" initial))
    (fiveam:is (search "\"temperature\":0.8" updated))
    (fiveam:is (search "\"topP\":0.9" updated))
    (fiveam:is (search "\"temperature\":null" reset))
    (fiveam:is (search "\"topP\":null" reset))))

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

(fiveam:test test-default-stop-mcp-server-closes-streams
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "mcp-stop-streams/" temp-dir))
         (input-path (merge-pathnames "input.txt" root))
         (output-path (merge-pathnames "output.txt" root)))
    (when (probe-file root)
      (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist root)
    (with-open-file (stream input-path :direction :output :if-exists :supersede)
      (write-string "" stream))
    (with-open-file (stream output-path :direction :output :if-exists :supersede)
      (write-string "" stream))
    (let ((input-stream (open input-path :direction :io :if-exists :overwrite))
          (output-stream (open output-path :direction :input)))
      (unwind-protect
           (let ((server (make-instance 'mcp-server
                                       :name "test-server"
                                       :input-stream input-stream
                                       :output-stream output-stream)))
             (default-stop-mcp-server server)
             (fiveam:is-false (open-stream-p input-stream))
             (fiveam:is-false (open-stream-p output-stream))
             (fiveam:is-false (mcp-server-input-stream server))
             (fiveam:is-false (mcp-server-output-stream server)))
        (when (open-stream-p input-stream)
          (close input-stream))
        (when (open-stream-p output-stream)
          (close output-stream))
        (uiop:delete-directory-tree root :validate t)))))
