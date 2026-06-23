;;; tests-openai.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-openai-api-key-resolution
  (let ((*openai-api-key* "my-explicit-key"))
    (fiveam:is (string= "my-explicit-key" (openai-api-key))))
  (let ((*openai-api-key* nil)
        (*getenv-function* (lambda (name)
                             (if (string= name "OPENAI_API_KEY")
                                 "my-env-key"
                                 nil))))
    (fiveam:is (string= "my-env-key" (openai-api-key)))))

(fiveam:test test-openai-chat-flow
  (let ((captured-payloads nil))
    (let* ((*openai-api-key* "test-key")
           (context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (let ((content (getf args :content)))
                        (push content captured-payloads))
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Hello \"}}]}
data: {\"choices\": [{\"delta\": {\"content\": \"OpenAI\"}}]}
data: [DONE]")
                              200))))
           (conv (new-chat :backend :openai :system-instruction "Be helpful" :runtime-context context)))
      (let ((res1 (chat "Hi there" :conversation conv)))
        (fiveam:is (string= "Hello OpenAI" res1))
        (fiveam:is (= 1 (length captured-payloads)))
        (let* ((payload (cl-json:decode-json-from-string (first captured-payloads)))
              (messages (cdr (assoc :messages payload))))
          (fiveam:is (= 2 (length messages)))
          (fiveam:is (string= "system" (cdr (assoc :role (first messages)))))
          (fiveam:is (string= "Be helpful" (cdr (assoc :content (first messages)))))
          (fiveam:is (string= "user" (cdr (assoc :role (second messages)))))
          (fiveam:is (string= "Hi there" (cdr (assoc :content (second messages)))))))
      (let ((res2 (chat "How are you?" :conversation conv)))
        (fiveam:is (string= "Hello OpenAI" res2))
        (fiveam:is (= 2 (length captured-payloads)))
        (let* ((payload (cl-json:decode-json-from-string (first captured-payloads)))
              (messages (cdr (assoc :messages payload))))
          (fiveam:is (= 4 (length messages)))
          (fiveam:is (string= "system" (cdr (assoc :role (first messages)))))
          (fiveam:is (string= "user" (cdr (assoc :role (second messages)))))
          (fiveam:is (string= "Hi there" (cdr (assoc :content (second messages)))))
          (fiveam:is (string= "assistant" (cdr (assoc :role (third messages)))))
          (fiveam:is (string= "Hello OpenAI" (cdr (assoc :content (third messages)))))
          (fiveam:is (string= "user" (cdr (assoc :role (fourth messages)))))
          (fiveam:is (string= "How are you?" (cdr (assoc :content (fourth messages)))))))
      (let ((stored-history (conversation-messages conv)))
        (fiveam:is (= 4 (length stored-history)))
        (fiveam:is (string= "user" (cdr (assoc "role" (first stored-history) :test #'string=))))
        (fiveam:is (string= "Hi there" (cdr (assoc "content" (first stored-history) :test #'string=))))
        (fiveam:is (string= "assistant" (cdr (assoc "role" (second stored-history) :test #'string=))))
        (fiveam:is (string= "Hello OpenAI" (cdr (assoc "content" (second stored-history) :test #'string=))))
        (fiveam:is (string= "user" (cdr (assoc "role" (third stored-history) :test #'string=))))
        (fiveam:is (string= "How are you?" (cdr (assoc "content" (third stored-history) :test #'string=))))
        (fiveam:is (string= "assistant" (cdr (assoc "role" (fourth stored-history) :test #'string=))))
        (fiveam:is (string= "Hello OpenAI" (cdr (assoc "content" (fourth stored-history) :test #'string=))))))))

(fiveam:test test-openai-chat-preserves-preloaded-history-every-turn
  (let ((captured-payloads nil))
    (let* ((*openai-api-key* "test-key")
           (context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-payloads
                            (append captured-payloads (list (getf args :content))))
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                              200))))
           (conv (new-chat :backend :openai :system-instruction "Be helpful" :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (chat "First live turn" :conversation conv)
      (chat "Second live turn" :conversation conv)
      (fiveam:is (= 2 (length captured-payloads)))
      (let* ((first-payload (cl-json:decode-json-from-string (first captured-payloads)))
            (first-messages (cdr (assoc :messages first-payload)))
            (second-payload (cl-json:decode-json-from-string (second captured-payloads)))
            (second-messages (cdr (assoc :messages second-payload))))
        (fiveam:is (= 4 (length first-messages)))
        (fiveam:is (string= "system" (cdr (assoc :role (first first-messages)))))
        (fiveam:is (string= "Please concisely summarize your knowledge graph."
                           (cdr (assoc :content (second first-messages)))))
        (fiveam:is (string= "Stored persona memory."
                           (cdr (assoc :content (third first-messages)))))
        (fiveam:is (string= "First live turn"
                           (cdr (assoc :content (fourth first-messages)))))
        (fiveam:is (= 6 (length second-messages)))
        (fiveam:is (string= "system" (cdr (assoc :role (first second-messages)))))
        (fiveam:is (string= "Please concisely summarize your knowledge graph."
                           (cdr (assoc :content (second second-messages)))))
        (fiveam:is (string= "Stored persona memory."
                           (cdr (assoc :content (third second-messages)))))
        (fiveam:is (string= "First live turn"
                           (cdr (assoc :content (fourth second-messages)))))
        (fiveam:is (string= "Hello OpenAI"
                           (cdr (assoc :content (fifth second-messages)))))
        (fiveam:is (string= "Second live turn"
                           (cdr (assoc :content (sixth second-messages)))))
        (fiveam:is (= 4 (length (conversation-messages conv))))
        (fiveam:is (string= "Stored persona memory."
                           (conversation-persona-memory conv)))))))

(fiveam:test test-lm-studio-api-key-resolution
  (let ((*lm-studio-api-key* "explicit-lm-key"))
    (fiveam:is (string= "explicit-lm-key" (lm-studio-api-key))))
  (let ((*lm-studio-api-key* nil)
        (*getenv-function* (lambda (name)
                             (if (string= name "LM_API_TOKEN")
                                 "env-lm-key"
                                 nil))))
    (fiveam:is (string= "env-lm-key" (lm-studio-api-key)))))

(fiveam:test test-lm-studio-default-api-key-is-configurable
  (let ((*lm-studio-api-key* nil)
        (*lm-studio-default-api-key* "custom-lm-default")
        (*getenv-function* (lambda (name)
                             (declare (ignore name))
                             nil)))
    (fiveam:is (string= "custom-lm-default" (lm-studio-api-key)))))

(fiveam:test test-lm-studio-chat-flow
  (let ((captured-url nil)
        (captured-headers nil))
    (let* ((*lm-studio-api-key* "lm_studio")
           (*lm-studio-base-url* "http://127.0.0.1:8088/v1")
           (context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (setf captured-url url)
                      (setf captured-headers (getf args :headers))
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Hello LM Studio\"}}]}
data: [DONE]")
                              200))))
           (conv (new-chat :backend :lm-studio :runtime-context context)))
      (let ((res (chat "Hello local model" :conversation conv)))
        (fiveam:is (string= "Hello LM Studio" res))
        (fiveam:is (string= "http://127.0.0.1:8088/v1/chat/completions" captured-url))
        (fiveam:is (string= "Bearer lm_studio" (cdr (assoc "Authorization" captured-headers :test #'string=))))))))

(fiveam:test test-openai-tool-call-recursion-preserves-history
  (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
        (captured-payloads nil)
        (call-count 0))
    (let ((*get-all-mcp-tools-function*
            (lambda (bot)
              (declare (ignore bot))
              (list (cons :mock-server
                          '((:name . "echo_tool")
                            (:description . "Echo tool")
                            (:input-schema . ((:type . "object"))))))))
          (*find-mcp-server-and-tool-function*
            (lambda (bot tool-name)
              (declare (ignore bot))
              (values :mock-server `((:name . ,tool-name)))))
          (*execute-mcp-tool-function*
            (lambda (server tool-name arguments)
              (declare (ignore server tool-name))
              (fiveam:is (string= "payload" (cdr (assoc :value arguments))))
              "tool result"))
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore url))
              (incf call-count)
              (setf captured-payloads
                    (append captured-payloads (list (getf args :content))))
              (if (= call-count 1)
                  (values (make-string-input-stream
                           "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"echo_tool\", \"arguments\": \"{\\\"value\\\":\\\"payload\\\"}\"}}]}}]}
data: [DONE]")
                          200)
                  (values (make-string-input-stream
                           "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                          200)))))
      (let ((res (let ((*openai-api-key* "test-key"))
                   (chat "Run tool" :conversation conv))))
        (fiveam:is (string= "Done" res))
        (fiveam:is (= 2 (length captured-payloads)))
        (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
               (second-messages (cdr (assoc :messages second-payload)))
               (stored-history (conversation-messages conv))
               (assistant-tool-msg (third second-messages))
               (tool-msg (fourth second-messages)))
          (fiveam:is (= 4 (length second-messages)))
          (fiveam:is (string= "system" (cdr (assoc :role (first second-messages)))))
          (fiveam:is (string= "user" (cdr (assoc :role (second second-messages)))))
          (fiveam:is (string= "assistant" (cdr (assoc :role assistant-tool-msg))))
          (fiveam:is (not (null (cdr (assoc :tool--calls assistant-tool-msg)))))
          (fiveam:is (string= "tool" (cdr (assoc :role tool-msg))))
          (fiveam:is (string= "tool result" (cdr (assoc :content tool-msg))))
          (fiveam:is (= 4 (length stored-history)))
          (fiveam:is (string= "user" (cdr (assoc "role" (first stored-history) :test #'string=))))
          (fiveam:is (string= "assistant" (cdr (assoc "role" (second stored-history) :test #'string=))))
          (fiveam:is (string= "tool" (cdr (assoc "role" (third stored-history) :test #'string=))))
          (fiveam:is (string= "assistant" (cdr (assoc "role" (fourth stored-history) :test #'string=)))))))))

(fiveam:test test-openai-chat-includes-mcp-tools-in-request
  (let ((captured-payload nil))
    (let* ((*openai-api-key* "test-key")
          (*get-all-mcp-tools-function*
            (lambda (bot)
              (declare (ignore bot))
              (list (cons :mock-server
                          '((:name . "echo_tool")
                            (:description . "Echo tool")
                            (:input-schema . ((:type . "object")
                                              (:properties . nil))))))))
          (context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-payload (getf args :content))
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                              200))))
          (conv (new-chat :backend :openai :runtime-context context)))
      (fiveam:is (string= "Hello OpenAI" (chat "Hi there" :conversation conv)))
      (let* ((payload (cl-json:decode-json-from-string captured-payload))
            (tools (cdr (assoc :tools payload)))
            (first-tool (car tools))
            (function (cdr (assoc :function first-tool))))
        (fiveam:is (= 1 (length tools)))
        (fiveam:is (string= "function" (cdr (assoc :type first-tool))))
        (fiveam:is (string= "echo_tool" (cdr (assoc :name function))))
        (fiveam:is (string= "Echo tool" (cdr (assoc :description function))))))))
