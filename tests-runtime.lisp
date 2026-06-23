;;; tests-runtime.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-default-conversation
  (let ((conv (new-chat))
        (called nil)
        (original-default-conversation *default-conversation*))
    (let ((original-post-function *http-post-function*)
         (original-gemini-api-key-function *gemini-api-key-function*)
          (original-context-default (runtime-context-default-conversation *default-runtime-context*)))
      (setf *gemini-api-key-function* (lambda () "mocked-google-api-key"))
      (setf *http-post-function*
            (lambda (url &rest args)
              (declare (ignore url args))
              (setf called t)
              (values (make-string-input-stream "") 200)))
      (unwind-protect
           (progn
             (setf *default-conversation* conv)
             (chat "Hello")
             (fiveam:is-true called)
             (fiveam:is (eq conv (runtime-context-default-conversation *default-runtime-context*))))
        (setf *gemini-api-key-function* original-gemini-api-key-function)
        (setf *http-post-function* original-post-function)
        (setf *default-conversation* original-default-conversation)
        (setf (runtime-context-default-conversation *default-runtime-context*) original-context-default)))))

(fiveam:test test-explicit-runtime-context-controls-http-timeouts
  (let* ((context (make-runtime-context :http-connect-timeout 7
                                        :http-read-timeout 33))
         (conv (new-chat :backend :google :runtime-context context))
         (captured-connect-timeout nil)
         (captured-read-timeout nil)
         )
    (setf (runtime-context-gemini-api-key-function context) (lambda () "mocked-google-api-key"))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (declare (ignore url))
            (setf captured-connect-timeout (getf args :connect-timeout))
            (setf captured-read-timeout (getf args :read-timeout))
            (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from context\"}], \"role\": \"model\"}}]}" 200)))
    (fiveam:is (string= "Hello from context" (chat "Hi" :conversation conv)))
    (fiveam:is (= 7 captured-connect-timeout))
    (fiveam:is (= 33 captured-read-timeout))))

(fiveam:test test-legacy-timeout-globals-sync-through-default-runtime-context
  (let* ((conv (new-chat :backend :google))
         (captured-connect-timeout nil)
         (captured-read-timeout nil)
         (original-connect-timeout *http-connect-timeout*)
         (original-read-timeout *http-read-timeout*)
         (original-context-connect (runtime-context-http-connect-timeout *default-runtime-context*))
         (original-context-read (runtime-context-http-read-timeout *default-runtime-context*))
         (original-post-function *http-post-function*)
         (original-gemini-api-key-function *gemini-api-key-function*))
    (setf *gemini-api-key-function* (lambda () "mocked-google-api-key"))
    (setf *http-post-function*
          (lambda (url &rest args)
            (declare (ignore url))
            (setf captured-connect-timeout (getf args :connect-timeout))
            (setf captured-read-timeout (getf args :read-timeout))
            (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from globals\"}], \"role\": \"model\"}}]}" 200)))
    (unwind-protect
         (let ((*http-connect-timeout* 21)
               (*http-read-timeout* 84))
           (fiveam:is (string= "Hello from globals" (chat "Hi" :conversation conv)))
           (fiveam:is (= 21 captured-connect-timeout))
           (fiveam:is (= 84 captured-read-timeout))
           (fiveam:is (= 21 (runtime-context-http-connect-timeout *default-runtime-context*)))
           (fiveam:is (= 84 (runtime-context-http-read-timeout *default-runtime-context*))))
      (setf *gemini-api-key-function* original-gemini-api-key-function)
      (setf *http-post-function* original-post-function)
      (setf *http-connect-timeout* original-connect-timeout)
      (setf *http-read-timeout* original-read-timeout)
      (setf (runtime-context-http-connect-timeout *default-runtime-context*) original-context-connect)
      (setf (runtime-context-http-read-timeout *default-runtime-context*) original-context-read))))

(fiveam:test test-default-runtime-context-helpers-sync-legacy-globals
  (let ((original-mcp-config-path *mcp-config-path*)
       (original-auto-init *auto-initialize-startup-mcp-servers-p*)
       (original-logging-enabled *logging-enabled-p*)
       (original-log-level *log-level*)
       (original-log-stream *log-stream*)
       (original-connect-timeout *http-connect-timeout*)
       (original-read-timeout *http-read-timeout*)
       (context *default-runtime-context*)
       (test-stream (make-string-output-stream)))
    (unwind-protect
        (progn
          (setf (current-mcp-config-path) "compat-path.lisp")
          (setf (current-auto-initialize-startup-mcp-servers-p) t)
          (setf (current-logging-enabled-p) nil)
          (setf (current-log-level) :error)
          (setf (current-log-stream) test-stream)
          (setf (current-http-connect-timeout) 41)
          (setf (current-http-read-timeout) 142)
          (fiveam:is (string= "compat-path.lisp" *mcp-config-path*))
          (fiveam:is-true *auto-initialize-startup-mcp-servers-p*)
          (fiveam:is-false *logging-enabled-p*)
          (fiveam:is (eq :error *log-level*))
          (fiveam:is (eq test-stream *log-stream*))
          (fiveam:is (= 41 *http-connect-timeout*))
          (fiveam:is (= 142 *http-read-timeout*))
          (fiveam:is (string= "compat-path.lisp" (runtime-context-mcp-config-path context)))
          (fiveam:is-true (runtime-context-auto-initialize-startup-mcp-servers-p context))
          (fiveam:is-false (runtime-context-logging-enabled-p context))
          (fiveam:is (eq :error (runtime-context-log-level context)))
          (fiveam:is (eq test-stream (runtime-context-log-stream context)))
          (fiveam:is (= 41 (runtime-context-http-connect-timeout context)))
          (fiveam:is (= 142 (runtime-context-http-read-timeout context))))
      (setf *mcp-config-path* original-mcp-config-path)
      (setf *auto-initialize-startup-mcp-servers-p* original-auto-init)
      (setf *logging-enabled-p* original-logging-enabled)
      (setf *log-level* original-log-level)
      (setf *log-stream* original-log-stream)
      (setf *http-connect-timeout* original-connect-timeout)
      (setf *http-read-timeout* original-read-timeout)
      (sync-runtime-context-from-legacy-globals context))))

(fiveam:test test-explicit-runtime-context-helpers-do-not-mutate-default-globals
  (let* ((context (make-runtime-context :mcp-config-path "explicit-start.lisp"
                                       :auto-initialize-startup-mcp-servers-p nil
                                       :logging-enabled-p t
                                       :log-level :info
                                       :log-stream *error-output*
                                       :http-connect-timeout 7
                                       :http-read-timeout 33))
        (original-mcp-config-path *mcp-config-path*)
        (original-auto-init *auto-initialize-startup-mcp-servers-p*)
        (original-logging-enabled *logging-enabled-p*)
        (original-log-level *log-level*)
        (original-log-stream *log-stream*)
        (original-connect-timeout *http-connect-timeout*)
        (original-read-timeout *http-read-timeout*)
        (test-stream (make-string-output-stream)))
    (unwind-protect
        (call-with-runtime-context
         context
         (lambda ()
           (setf (current-mcp-config-path context) "explicit-next.lisp")
           (setf (current-auto-initialize-startup-mcp-servers-p context) t)
           (setf (current-logging-enabled-p context) nil)
           (setf (current-log-level context) :warn)
           (setf (current-log-stream context) test-stream)
           (setf (current-http-connect-timeout context) 19)
           (setf (current-http-read-timeout context) 88)
           (fiveam:is (string= "explicit-next.lisp" *mcp-config-path*))
           (fiveam:is-true *auto-initialize-startup-mcp-servers-p*)
           (fiveam:is-false *logging-enabled-p*)
           (fiveam:is (eq :warn *log-level*))
           (fiveam:is (eq test-stream *log-stream*))
           (fiveam:is (= 19 *http-connect-timeout*))
           (fiveam:is (= 88 *http-read-timeout*))))
      (fiveam:is (equal original-mcp-config-path *mcp-config-path*))
      (fiveam:is (eql original-auto-init *auto-initialize-startup-mcp-servers-p*))
      (fiveam:is (eql original-logging-enabled *logging-enabled-p*))
      (fiveam:is (eq original-log-level *log-level*))
      (fiveam:is (eq original-log-stream *log-stream*))
      (fiveam:is (= original-connect-timeout *http-connect-timeout*))
      (fiveam:is (= original-read-timeout *http-read-timeout*))
      (fiveam:is (string= "explicit-next.lisp" (runtime-context-mcp-config-path context)))
      (fiveam:is-true (runtime-context-auto-initialize-startup-mcp-servers-p context))
      (fiveam:is-false (runtime-context-logging-enabled-p context))
      (fiveam:is (eq :warn (runtime-context-log-level context)))
      (fiveam:is (eq test-stream (runtime-context-log-stream context)))
      (fiveam:is (= 19 (runtime-context-http-connect-timeout context)))
      (fiveam:is (= 88 (runtime-context-http-read-timeout context))))))

(fiveam:test test-legacy-runtime-global-warning-is-opt-in-and-once
  (let ((*warn-on-legacy-runtime-globals-p* t)
        (*legacy-runtime-global-warnings-issued* nil)
        (original-connect-timeout *http-connect-timeout*)
        (original-context-connect (runtime-context-http-connect-timeout *default-runtime-context*)))
    (unwind-protect
         (let ((*error-output* (make-string-output-stream)))
           (setf *http-connect-timeout* 41)
           (current-http-connect-timeout)
           (let ((first-output (get-output-stream-string *error-output*)))
             (fiveam:is (search "*HTTP-CONNECT-TIMEOUT*" first-output))
             (fiveam:is (search "MAKE-RUNTIME-CONTEXT with :HTTP-CONNECT-TIMEOUT" first-output)))
           (current-http-connect-timeout)
           (fiveam:is (string= "" (get-output-stream-string *error-output*))))
      (setf *http-connect-timeout* original-connect-timeout)
      (setf (runtime-context-http-connect-timeout *default-runtime-context*) original-context-connect)
      (setf *legacy-runtime-global-warnings-issued* nil))))

(fiveam:test test-explicit-runtime-context-does-not-warn-about-legacy-globals
  (let* ((context (make-runtime-context :http-connect-timeout 7))
         (*warn-on-legacy-runtime-globals-p* t)
         (*legacy-runtime-global-warnings-issued* nil))
    (let ((*error-output* (make-string-output-stream)))
      (call-with-runtime-context
       context
       (lambda ()
         (setf (current-http-connect-timeout context) 12)
         (fiveam:is (= 12 (current-http-connect-timeout context)))))
      (fiveam:is (string= "" (get-output-stream-string *error-output*))))))

(fiveam:test test-new-chat-without-persona-starts-empty
  (let* ((conv (new-chat))
        (bot (conversation-chatbot conv)))
    (fiveam:is (typep conv 'conversation))
    (fiveam:is (typep bot 'chatbot))
    (fiveam:is (null (conversation-messages conv)))
    (fiveam:is (null (conversation-interaction-id conv)))))

(fiveam:test test-gemini-chat-falls-back-on-interactions-404
  (let ((conv (new-chat :backend :gemini))
        (calls '()))
    (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore args))
              (push url calls)
              (if (search "/interactions?alt=sse" url)
                  (error "An HTTP request to \"~A\" returned 404 not found.~%~%{\"error\":{\"message\":\"Requested entity was not found.\",\"code\":\"not_found\"}}" url)
                  (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from fallback\"}], \"role\": \"model\"}}]}" 200)))))
      (let ((res (chat "Hi fallback" :conversation conv)))
        (fiveam:is (string= "Hello from fallback" res))
        (fiveam:is (= 2 (length calls)))
        (fiveam:is (search "/interactions?alt=sse" (second calls)))
        (fiveam:is (search ":generateContent?key=mocked-google-api-key" (first calls)))))))

(fiveam:test test-gemini-chat-continues-without-persona
  (let ((conv (new-chat :backend :gemini))
        (captured-payloads nil)
        (call-count 0))
    (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                          (declare (ignore bot))
                                          nil))
           (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
           (*http-post-function*
             (lambda (url &rest args)
               (declare (ignore url))
               (incf call-count)
               (push (getf args :content) captured-payloads)
               (values
                (make-string-input-stream
                 (if (= call-count 1)
                    "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Hello one\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
                    "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Hello two\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":2,\"total_output_tokens\":1,\"total_tokens\":3}}}"))
                200))))
      (fiveam:is (string= "Hello one" (chat "First turn" :conversation conv)))
      (fiveam:is (string= "session-1" (conversation-interaction-id conv)))
      (fiveam:is (string= "Hello two" (chat "Second turn" :conversation conv)))
      (fiveam:is (= 2 (length captured-payloads)))
      (let ((second-payload (first captured-payloads))
            (first-payload (second captured-payloads)))
        (fiveam:is (search "\"input\":\"First turn\"" first-payload))
        (fiveam:is (null (search "\"previous_interaction_id\"" first-payload)))
        (fiveam:is (search "\"input\":\"Second turn\"" second-payload))
        (fiveam:is (search "\"previous_interaction_id\":\"session-1\"" second-payload))))))

(fiveam:test test-text-formatting
  (let ((wrapped (wrap-text "This is a test of the line wrapping utility." :width 15)))
    (fiveam:is (every (lambda (line) (<= (length line) 15)) wrapped))
    (fiveam:is (string= "This is a test" (car wrapped))))
  (let ((output (with-output-to-string (s)
                  (format-paragraphs "Para one.

Para two." :width 40 :stream s))))
    (fiveam:is (search "Para one." output))
    (fiveam:is (search "Para two." output))))

(fiveam:test test-log-message-level-filtering
  (let ((*logging-enabled-p* t)
        (*log-level* :warn))
    (let ((output (with-output-to-string (s)
                    (let ((*log-stream* s))
                      (log-message :info "skip me")
                      (log-message :error "keep me")))))
      (fiveam:is (null (search "skip me" output)))
      (fiveam:is (search "keep me" output)))))

(fiveam:test test-log-backend-response-stats
  (let ((*logging-enabled-p* t)
        (*log-level* :info))
    (let ((output (with-output-to-string (s)
                    (let ((*log-stream* s))
                      (log-backend-response-stats
                       :google
                       :http-status 200
                       :response-id "resp-123"
                       :model "gemini-3.5-flash"
                       :finish-reason "STOP"
                       :usage '((:prompt-token-count . 12)
                                (:candidates-token-count . 7)
                                (:thoughts-token-count . 3)
                                (:total-token-count . 22)))))))
      (fiveam:is (search "Backend response stats" output))
      (fiveam:is (search "backend: google" output))
      (fiveam:is (search "http-status: 200" output))
      (fiveam:is (search "response-id: resp-123" output))
      (fiveam:is (search "prompt-tokens: 12" output))
      (fiveam:is (search "completion-tokens: 7" output))
      (fiveam:is (search "thought-tokens: 3" output))
      (fiveam:is (search "total-tokens: 22" output)))))

(fiveam:test test-write-turn-token-summary-supports-interactions-usage-keys
  (let ((output (with-output-to-string (s)
                  (write-turn-token-summary
                   '(("total_input_tokens" . 12)
                     ("total_output_tokens" . 7)
                     ("total_thought_tokens" . 3)
                     ("total_tokens" . 22))
                   :stream s))))
    (fiveam:is (search "[Tokens] prompt: 12" output))
    (fiveam:is (search "completion: 7" output))
    (fiveam:is (search "thought: 3" output))
    (fiveam:is (search "total: 22" output))))

(fiveam:test test-post-web-request-logging-redacts-secrets
  (let ((*logging-enabled-p* t)
        (*log-level* :info)
        (*http-connect-timeout* 15)
        (*http-read-timeout* 120)
        (captured-url nil)
        (captured-headers nil)
        (captured-content nil)
        (captured-connect-timeout nil)
        (captured-read-timeout nil)
        (original-post-function *http-post-function*))
    (setf *http-post-function*
          (lambda (url &rest args)
            (setf captured-url url)
            (setf captured-headers (getf args :headers))
            (setf captured-content (getf args :content))
            (setf captured-connect-timeout (getf args :connect-timeout))
            (setf captured-read-timeout (getf args :read-timeout))
            (values "{\"ok\":true}" 200)))
    (unwind-protect
         (let ((log-output (with-output-to-string (s)
                             (let ((*log-stream* s))
                               (post-web-request "https://example.com/test?key=secret-value"
                                                 '(("Authorization" . "Bearer secret-token")
                                                   ("Content-Type" . "application/json"))
                                                 "{\"hello\":\"world\"}")))))
           (fiveam:is (string= "https://example.com/test?key=secret-value" captured-url))
           (fiveam:is (equal '(("Authorization" . "Bearer secret-token")
                               ("Content-Type" . "application/json"))
                             captured-headers))
           (fiveam:is (string= "{\"hello\":\"world\"}" captured-content))
           (fiveam:is (= 15 captured-connect-timeout))
           (fiveam:is (= 120 captured-read-timeout))
           (fiveam:is (search "HTTP POST request" log-output))
           (fiveam:is (search "https://example.com/test?key=[REDACTED]" log-output))
           (fiveam:is (search "\"Authorization\":\"[REDACTED]\"" log-output))
           (fiveam:is (search "connect-timeout: 15" log-output))
           (fiveam:is (search "read-timeout: 120" log-output))
           (fiveam:is-false (search "{\"hello\":\"world\"}" log-output)))
      (setf *http-post-function* original-post-function))))

(fiveam:test test-backend-selection-and-defaults
  (let ((conv-gemini (new-chat :backend :gemini))
        (conv-openai (new-chat :backend :openai))
        (conv-lm (new-chat :backend :lm-studio))
        (conv-google (new-chat :backend :google))
        (conv-custom (new-chat :backend :lm-studio :model "my-model")))
    (fiveam:is (eq :gemini (chatbot-backend (conversation-chatbot conv-gemini))))
    (fiveam:is (string= "gemini-3.5-flash" (chatbot-model (conversation-chatbot conv-gemini))))
    (fiveam:is (eq :openai (chatbot-backend (conversation-chatbot conv-openai))))
    (fiveam:is (string= "gpt-4o" (chatbot-model (conversation-chatbot conv-openai))))
    (fiveam:is (eq :lm-studio (chatbot-backend (conversation-chatbot conv-lm))))
    (fiveam:is (string= "gemma-4-e4b-uncensored-hauhaucs-aggressive" (chatbot-model (conversation-chatbot conv-lm))))
    (fiveam:is (eq :google (chatbot-backend (conversation-chatbot conv-google))))
    (fiveam:is (string= "gemini-3.5-flash" (chatbot-model (conversation-chatbot conv-google))))
    (fiveam:is (eq :lm-studio (chatbot-backend (conversation-chatbot conv-custom))))
    (fiveam:is (string= "my-model" (chatbot-model (conversation-chatbot conv-custom))))))

(fiveam:test test-backend-default-models-are-configurable
  (let ((*backend-default-models*
          '((:gemini . "gemini-custom")
            (:google . "google-custom")
            (:openai . "openai-custom")
            (:lm-studio . "lm-custom"))))
    (fiveam:is (string= "gemini-custom"
                        (chatbot-model (conversation-chatbot (new-chat :backend :gemini)))))
    (fiveam:is (string= "google-custom"
                        (chatbot-model (conversation-chatbot (new-chat :backend :google)))))
    (fiveam:is (string= "openai-custom"
                        (chatbot-model (conversation-chatbot (new-chat :backend :openai)))))
    (fiveam:is (string= "lm-custom"
                        (chatbot-model (conversation-chatbot (new-chat :backend :lm-studio)))))
    (fiveam:is (string= "gemini-custom" (backend-default-model :unknown-backend)))))

(fiveam:test test-chatbot-direct-instance-uses-configured-default-model
  (let ((*backend-default-models*
          '((:gemini . "gemini-direct")
            (:google . "google-direct")
            (:openai . "openai-direct")
            (:lm-studio . "lm-direct"))))
    (fiveam:is (string= "google-direct"
                        (chatbot-model (make-instance 'chatbot :backend :google))))
    (fiveam:is (string= "gemini-direct"
                        (chatbot-model (make-instance 'chatbot))))))

(fiveam:test test-gemini-api-revision-is-configurable
  (let ((conv (new-chat :backend :gemini))
        (captured-headers nil))
    (let ((*get-all-mcp-tools-function* (lambda (bot)
                                          (declare (ignore bot))
                                          nil))
          (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*gemini-api-revision* "2099-01-01")
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore url))
              (setf captured-headers (getf args :headers))
              (values
               (make-string-input-stream
                "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Hello revision\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")
               200))))
      (fiveam:is (string= "Hello revision" (chat "Hi revision" :conversation conv)))
      (fiveam:is (string= "2099-01-01"
                          (cdr (assoc "Api-Revision" captured-headers :test #'string=)))))))

(fiveam:test test-new-chat-reuses-startup-mcp-servers
  (let ((*startup-chatbot* nil))
    (let ((*initialize-mcp-servers-for-chatbot-function*
            (lambda (bot &key strict-required-p)
              (declare (ignore strict-required-p))
              (setf (chatbot-mcp-servers bot) '(:shared-server))
              bot)))
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (initialize-startup-chatbot)
      (let* ((conv (new-chat))
             (bot (conversation-chatbot conv)))
        (fiveam:is-true (startup-chatbot-initialized-p))
        (fiveam:is (eq *startup-chatbot* (ensure-startup-chatbot)))
        (fiveam:is (eq *startup-chatbot*
                       (runtime-context-startup-chatbot *default-runtime-context*)))
        (fiveam:is (eq (chatbot-mcp-servers bot)
                       (chatbot-mcp-servers *startup-chatbot*)))))))

(fiveam:test test-explicit-runtime-context-isolates-startup-mcp-servers
  (let* ((context-a (make-runtime-context))
         (context-b (make-runtime-context)))
    (let ((*initialize-mcp-servers-for-chatbot-function*
            (lambda (bot &key strict-required-p)
              (declare (ignore strict-required-p))
              (setf (chatbot-mcp-servers bot)
                    (list (if (eq (chatbot-runtime-context bot) context-a)
                              :context-a-server
                              :context-b-server)))
              bot)))
      (initialize-startup-chatbot context-a)
      (initialize-startup-chatbot context-b)
      (let* ((conv-a (new-chat :runtime-context context-a))
             (conv-b (new-chat :runtime-context context-b))
             (conv-default (new-chat)))
        (fiveam:is (equal '(:context-a-server)
                          (chatbot-mcp-servers (conversation-chatbot conv-a))))
        (fiveam:is (equal '(:context-b-server)
                          (chatbot-mcp-servers (conversation-chatbot conv-b))))
        (fiveam:is (null (chatbot-mcp-servers (conversation-chatbot conv-default))))))))

(fiveam:test test-new-chat-does-not-start-mcp-servers
  (let ((*startup-chatbot* nil)
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore bot strict-required-p))
             (incf init-calls))))
      (let* ((conv (new-chat))
            (bot (conversation-chatbot conv)))
        (fiveam:is (= 0 init-calls))
        (fiveam:is (null (chatbot-mcp-servers bot)))))))

(fiveam:test test-new-chat-auto-initializes-startup-mcp-servers-when-enabled
  (let* ((context (make-runtime-context :auto-initialize-startup-mcp-servers-p t))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
          (lambda (bot &key strict-required-p)
            (declare (ignore strict-required-p))
            (incf init-calls)
            (setf (chatbot-mcp-servers bot) '(:shared-server))
            bot)))
      (let* ((conv (new-chat :runtime-context context))
            (bot (conversation-chatbot conv)))
       (fiveam:is (= 1 init-calls))
       (fiveam:is-true (startup-chatbot-initialized-p context))
       (fiveam:is (eq (current-startup-chatbot context)
                      (runtime-context-startup-chatbot context)))
       (fiveam:is (eq (chatbot-mcp-servers bot)
                      (chatbot-mcp-servers (current-startup-chatbot context))))))))

(fiveam:test test-auto-startup-chatbot-defaults-to-noop
  (let ((*startup-chatbot* nil)
       (*auto-initialize-startup-mcp-servers-p* nil)
       (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore bot strict-required-p))
             (incf init-calls))))
      (maybe-auto-initialize-startup-chatbot)
      (fiveam:is (= 0 init-calls))
      (fiveam:is-false (startup-chatbot-initialized-p)))))

(fiveam:test test-auto-startup-chatbot-honors-compatibility-flag
  (let ((*startup-chatbot* nil)
        (*auto-initialize-startup-mcp-servers-p* t))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore strict-required-p))
             (setf (chatbot-mcp-servers bot) '(:shared-server))
             bot)))
      (maybe-auto-initialize-startup-chatbot)
      (fiveam:is-true (startup-chatbot-initialized-p))
      (fiveam:is (equal '(:shared-server)
                       (chatbot-mcp-servers *startup-chatbot*))))))

(fiveam:test test-startup-chatbot-exposes-partial-startup-status
  (let ((*startup-chatbot* nil))
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
      (initialize-startup-chatbot)
      (let ((status (startup-chatbot-mcp-status)))
        (fiveam:is-true (startup-chatbot-initialized-p))
        (fiveam:is (typep status 'mcp-startup-status))
        (fiveam:is-true (mcp-startup-status-partial-failure-p status))
        (fiveam:is (= 1 (mcp-startup-status-failed-count status)))))))

(fiveam:test test-startup-chatbot-does-not-initialize-on-strict-required-failure
  (let ((*startup-chatbot* nil))
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
      (fiveam:signals mcp-startup-error
        (initialize-startup-chatbot nil :strict-required-p t)))
    (fiveam:is-false (startup-chatbot-initialized-p))
    (fiveam:is-false (startup-chatbot-mcp-status))))

(fiveam:test test-shutdown-chatbot-preserves-shared-startup-servers
  (let* ((context (make-runtime-context))
         (shared-servers (list :shared-server))
         (startup-bot (make-instance 'chatbot
                                     :mcp-servers shared-servers
                                     :runtime-context context))
         (bot (make-instance 'chatbot
                             :mcp-servers shared-servers
                             :runtime-context context))
         (stopped nil))
    (let ((*stop-mcp-server-function*
            (lambda (server)
              (push server stopped))))
      (setf (runtime-context-startup-chatbot context) startup-bot)
      (unwind-protect
           (progn
             (shutdown-chatbot bot context)
             (fiveam:is (null stopped))
             (fiveam:is (eq shared-servers (chatbot-mcp-servers startup-bot)))
             (shutdown-chatbot startup-bot context)
             (fiveam:is (equal shared-servers stopped))
             (fiveam:is (null (runtime-context-startup-chatbot context))))
        (setf (runtime-context-startup-chatbot context) nil)))))

(fiveam:test test-shutdown-chatbot-stops-only-persona-specific-mcp-servers
  (let* ((context (make-runtime-context))
         (shared-time (make-instance 'mcp-server :name "mcp-server-time"))
         (shared-memory (make-instance 'mcp-server :name "memory"))
         (persona-memory (make-instance 'mcp-server :name "memory"))
         (startup-bot (make-instance 'chatbot
                                     :mcp-servers (list shared-time shared-memory)
                                     :runtime-context context))
         (bot (make-instance 'chatbot
                             :mcp-servers (list shared-time persona-memory)
                             :runtime-context context))
         (stopped nil))
    (let ((*stop-mcp-server-function*
            (lambda (server)
              (push server stopped))))
      (setf (runtime-context-startup-chatbot context) startup-bot)
      (unwind-protect
           (progn
             (shutdown-chatbot bot context)
             (fiveam:is (equal (list persona-memory) stopped))
             (fiveam:is (eq startup-bot (runtime-context-startup-chatbot context))))
        (setf (runtime-context-startup-chatbot context) nil)))))
