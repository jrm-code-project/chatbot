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
             (setf (current-default-conversation) conv)
             (chat "Hello")
             (fiveam:is-true called)
             (fiveam:is (eq conv (runtime-context-default-conversation *default-runtime-context*))))
        (setf *gemini-api-key-function* original-gemini-api-key-function)
        (setf *http-post-function* original-post-function)
        (setf *default-conversation* original-default-conversation)
        (setf (runtime-context-default-conversation *default-runtime-context*) original-context-default)))))

(fiveam:test test-resolve-chat-entry-context-does-not-sync-legacy-default-conversation-into-runtime-context
  (let* ((default-context *default-runtime-context*)
        (legacy-conversation (new-chat))
        (context-conversation (new-chat))
        (original-legacy-conversation *default-conversation*)
        (original-default-conversation (runtime-context-default-conversation default-context)))
    (unwind-protect
        (progn
          (setf *default-conversation* legacy-conversation)
          (setf (runtime-context-default-conversation default-context) context-conversation)
          (fiveam:is (eq default-context (resolve-chat-entry-context nil)))
          (fiveam:is (eq context-conversation
                        (runtime-context-default-conversation default-context)))
          (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation))))

(fiveam:test test-chat-with-canonical-default-conversation-uses-conversation-runtime-context
  (let* ((custom-context (make-runtime-context))
         (conv (new-chat :backend :google :runtime-context custom-context))
         (legacy-http-called-p nil)
         (context-http-called-p nil)
         (original-default-conversation *default-conversation*)
         (original-context-default (runtime-context-default-conversation *default-runtime-context*))
         (original-gemini-api-key-function *gemini-api-key-function*)
         (original-http-post-function *http-post-function*)
         (*minions-data-directory* (merge-pathnames "runtime-context-checkpoints/"
                                                    (uiop:default-temporary-directory)))
         (checkpoint-file (merge-pathnames "DefaultConversation.json" (minions-data-directory))))
    (when (probe-file checkpoint-file)
      (delete-file checkpoint-file))
    (setf (runtime-context-gemini-api-key-function custom-context)
         (lambda ()
           "context-google-api-key"))
    (setf (runtime-context-http-post-function custom-context)
         (lambda (url &rest args)
           (declare (ignore url args))
           (setf context-http-called-p t)
           (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from ambient context\"}], \"role\": \"model\"}}]}" 200)))
    (unwind-protect
        (progn
          (setf (current-default-conversation) conv)
          (setf *gemini-api-key-function*
                (lambda ()
                  (error "ambient legacy gemini key function should not be used")))
          (setf *http-post-function*
                (lambda (url &rest args)
                  (declare (ignore url args))
                  (setf legacy-http-called-p t)
                  (error "ambient legacy http function should not be used")))
          (fiveam:is (string= "Hello from ambient context" (chat "Hello ambient runtime context")))
          (fiveam:is-true context-http-called-p)
          (fiveam:is-false legacy-http-called-p))
      (setf *default-conversation* original-default-conversation)
      (setf (runtime-context-default-conversation *default-runtime-context*) original-context-default)
      (setf *gemini-api-key-function* original-gemini-api-key-function)
      (setf *http-post-function* original-http-post-function)
      (when (probe-file checkpoint-file)
        (delete-file checkpoint-file)))))

(fiveam:test test-minions-data-directory-defaults-to-user-runtime-state
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-minions-default/" temp-dir))
         (*minions-data-directory* nil)
         (*getenv-function* (lambda (name)
                              (declare (ignore name))
                              nil))
         (*user-homedir-pathname-function* (lambda ()
                                             mock-home)))
    (let ((resolved (minions-data-directory)))
      (fiveam:is (string= (namestring (uiop:ensure-directory-pathname
                                       (merge-pathnames ".chatbot/data/minions/" mock-home)))
                          (namestring resolved))))))

(fiveam:test test-minions-data-directory-prefers-environment-override
  (let* ((temp-dir (uiop:default-temporary-directory))
         (env-dir (merge-pathnames "env-minions-root/" temp-dir))
         (*minions-data-directory* nil)
         (*getenv-function* (lambda (name)
                              (if (string= name "CHATBOT_MINIONS_DATA_DIR")
                                  (namestring env-dir)
                                  nil)))
         (*user-homedir-pathname-function* (lambda ()
                                             (merge-pathnames "ignored-home/" temp-dir))))
    (fiveam:is (string= (namestring (uiop:ensure-directory-pathname env-dir))
                       (namestring (minions-data-directory))))))

(fiveam:test test-read-file-forms-as-text-reads-all-forms-with-line-boundaries
  (let* ((temp-dir (uiop:default-temporary-directory))
        (file (merge-pathnames "read-file-forms-as-text.lisp" temp-dir))
        (contents
          (format nil "~%~%(defun alpha (x)~%  (+ x 1))~%~%(defmacro with-beta (value)~%  `(list ,value))~%~%(gamma) (delta)")))
    (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string contents stream))
    (unwind-protect
        (let ((forms (read-file-forms-as-text file))
              (expected-alpha (format nil "(defun alpha (x)~%  (+ x 1))"))
              (expected-beta (format nil "(defmacro with-beta (value)~%  `(list ,value))")))
          (fiveam:is (= 4 (length forms)))
          (fiveam:is (string= expected-alpha (first forms)))
          (fiveam:is (string= expected-beta (second forms)))
          (fiveam:is (string= "(gamma) (delta)" (third forms)))
          (fiveam:is (string= "(gamma) (delta)" (fourth forms))))
      (when (probe-file file)
        (delete-file file)))))

(fiveam:test test-make-runtime-context-inherits-default-conversation-from-canonical-context-not-legacy-global
  (let* ((default-context *default-runtime-context*)
        (legacy-conversation (new-chat))
        (context-conversation (new-chat))
        (original-legacy-conversation *default-conversation*)
        (original-default-conversation (runtime-context-default-conversation default-context)))
    (unwind-protect
        (progn
          (setf *default-conversation* legacy-conversation)
          (setf (runtime-context-default-conversation default-context) context-conversation)
          (let ((context (make-runtime-context)))
            (fiveam:is (eq context-conversation
                           (runtime-context-default-conversation context)))))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation))))

(fiveam:test test-no-arg-current-default-conversation-prefers-default-runtime-context
  (let* ((default-context *default-runtime-context*)
         (legacy-conversation (new-chat))
         (context-conversation (new-chat))
         (original-legacy-conversation *default-conversation*)
         (original-default-conversation (runtime-context-default-conversation default-context)))
    (unwind-protect
         (progn
           (setf *default-conversation* legacy-conversation)
           (setf (runtime-context-default-conversation default-context) context-conversation)
           (fiveam:is (eq context-conversation (current-default-conversation)))
           (fiveam:is (eq context-conversation
                         (runtime-context-default-conversation default-context)))
           (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation))))

(fiveam:test test-no-arg-set-current-default-conversation-updates-default-runtime-context
  (let* ((default-context *default-runtime-context*)
         (conversation (new-chat))
         (original-legacy-conversation *default-conversation*)
         (original-default-conversation (runtime-context-default-conversation default-context)))
    (unwind-protect
         (progn
           (setf (current-default-conversation) conversation)
           (fiveam:is (eq original-legacy-conversation *default-conversation*))
           (fiveam:is (eq conversation
                         (runtime-context-default-conversation default-context))))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation))))

(fiveam:test test-new-chat-does-not-sync-legacy-default-conversation-into-runtime-context
  (let* ((default-context *default-runtime-context*)
         (legacy-conversation (new-chat))
         (context-conversation (new-chat))
         (original-legacy-conversation *default-conversation*)
         (original-default-conversation (runtime-context-default-conversation default-context)))
    (unwind-protect
         (progn
           (setf *default-conversation* legacy-conversation)
           (setf (runtime-context-default-conversation default-context) context-conversation)
           (new-chat)
           (fiveam:is (eq context-conversation
                         (runtime-context-default-conversation default-context)))
           (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation))))

(fiveam:test test-initialize-startup-chatbot-does-not-sync-legacy-default-conversation-into-runtime-context
  (let* ((default-context *default-runtime-context*)
         (legacy-conversation (new-chat))
         (context-conversation (new-chat))
         (original-legacy-conversation *default-conversation*)
         (original-default-conversation (runtime-context-default-conversation default-context))
         (original-context-startup-chatbot (runtime-context-startup-chatbot default-context)))
    (unwind-protect
         (let ((*initialize-mcp-servers-for-chatbot-function*
                (lambda (bot &key strict-required-p)
                  (declare (ignore strict-required-p))
                  bot)))
           (setf *default-conversation* legacy-conversation)
           (setf (runtime-context-default-conversation default-context) context-conversation)
           (setf (runtime-context-startup-chatbot default-context) nil)
           (initialize-startup-chatbot)
           (fiveam:is (eq context-conversation
                         (runtime-context-default-conversation default-context)))
           (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation)
      (setf (runtime-context-startup-chatbot default-context) original-context-startup-chatbot))))

(fiveam:test test-function-seam-helpers-do-not-sync-legacy-default-conversation-into-runtime-context
  (let* ((default-context *default-runtime-context*)
         (legacy-conversation (new-chat))
         (context-conversation (new-chat))
         (original-legacy-conversation *default-conversation*)
         (original-default-conversation (runtime-context-default-conversation default-context))
         (original-getenv-function *getenv-function*)
         (context-getenv-function (lambda (name)
                                    (if (string= name "OPENAI_API_KEY")
                                        "context-env-key"
                                        nil))))
    (unwind-protect
         (progn
           (setf *default-conversation* legacy-conversation)
           (setf (runtime-context-default-conversation default-context) context-conversation)
           (setf (runtime-context-getenv-function default-context) context-getenv-function)
           (fiveam:is (eq context-getenv-function
                         (current-getenv-function default-context)))
           (setf (current-getenv-function default-context)
                 (lambda (name)
                   (if (string= name "OPENAI_API_KEY")
                       "updated-context-env-key"
                       nil)))
           (fiveam:is (eq context-conversation
                         (runtime-context-default-conversation default-context)))
           (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation)
      (setf *getenv-function* original-getenv-function))))

(fiveam:test test-approval-helpers-do-not-sync-legacy-default-conversation-into-runtime-context
  (let* ((default-context *default-runtime-context*)
         (legacy-conversation (new-chat))
         (context-conversation (new-chat))
         (original-legacy-conversation *default-conversation*)
         (original-default-conversation (runtime-context-default-conversation default-context))
         (original-approval-function *filesystem-access-approval-function*)
         (context-approval-function (lambda (&rest ignored)
                                      (declare (ignore ignored))
                                      :context-approval)))
    (unwind-protect
         (progn
           (setf *default-conversation* legacy-conversation)
           (setf (runtime-context-default-conversation default-context) context-conversation)
           (setf (runtime-context-filesystem-access-approval-function default-context)
                 context-approval-function)
           (fiveam:is (eq context-approval-function
                         (current-filesystem-access-approval-function default-context)))
           (setf (current-filesystem-access-approval-function default-context)
                 (lambda (&rest ignored)
                   (declare (ignore ignored))
                   :updated-context-approval))
           (fiveam:is (eq context-conversation
                         (runtime-context-default-conversation default-context)))
           (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation)
      (setf *filesystem-access-approval-function* original-approval-function))))

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

(fiveam:test test-call-with-stream-read-timeout-signals-on-stall
  (let ((start (get-internal-real-time))
       (units internal-time-units-per-second))
    (fiveam:signals error
     (call-with-stream-read-timeout
      (lambda ()
        (sleep 2)
        "never reached")
      :timeout-seconds 1
      :timeout-context "test stream"))
    (fiveam:is (< (/ (- (get-internal-real-time) start) units) 2.0))))

(fiveam:test test-legacy-timeout-globals-do-not-override-default-runtime-context
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
         (progn
           (setf (runtime-context-http-connect-timeout *default-runtime-context*) 17)
           (setf (runtime-context-http-read-timeout *default-runtime-context*) 71)
           (let ((*http-connect-timeout* 21)
                 (*http-read-timeout* 84))
             (fiveam:is (string= "Hello from globals" (chat "Hi" :conversation conv)))
             (fiveam:is (= 17 captured-connect-timeout))
             (fiveam:is (= 71 captured-read-timeout))
             (fiveam:is (= 17 (current-http-connect-timeout)))
             (fiveam:is (= 71 (current-http-read-timeout)))))
      (setf *gemini-api-key-function* original-gemini-api-key-function)
      (setf *http-post-function* original-post-function)
      (setf *http-connect-timeout* original-connect-timeout)
      (setf *http-read-timeout* original-read-timeout)
      (setf (runtime-context-http-connect-timeout *default-runtime-context*) original-context-connect)
      (setf (runtime-context-http-read-timeout *default-runtime-context*) original-context-read))))

(fiveam:test test-default-runtime-context-helpers-no-longer-sync-legacy-globals
  (let ((original-mcp-config-path *mcp-config-path*)
       (original-auto-init *auto-initialize-startup-mcp-servers-p*)
       (original-logging-enabled *logging-enabled-p*)
       (original-log-level *log-level*)
       (original-log-stream *log-stream*)
       (original-connect-timeout *http-connect-timeout*)
       (original-read-timeout *http-read-timeout*)
       (context *default-runtime-context*)
       (original-context-path (runtime-context-mcp-config-path *default-runtime-context*))
       (original-context-auto-init
        (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*))
       (original-context-logging-enabled
        (runtime-context-logging-enabled-p *default-runtime-context*))
       (original-context-log-level (runtime-context-log-level *default-runtime-context*))
       (original-context-log-stream (runtime-context-log-stream *default-runtime-context*))
       (original-context-connect (runtime-context-http-connect-timeout *default-runtime-context*))
       (original-context-read (runtime-context-http-read-timeout *default-runtime-context*))
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
          (fiveam:is (equal original-mcp-config-path *mcp-config-path*))
          (fiveam:is (eql original-auto-init *auto-initialize-startup-mcp-servers-p*))
          (fiveam:is (eql original-logging-enabled *logging-enabled-p*))
          (fiveam:is (eq original-log-level *log-level*))
          (fiveam:is (eq original-log-stream *log-stream*))
          (fiveam:is (= original-connect-timeout *http-connect-timeout*))
          (fiveam:is (= original-read-timeout *http-read-timeout*))
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
      (setf (runtime-context-mcp-config-path context) original-context-path)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p context) original-context-auto-init)
      (setf (runtime-context-logging-enabled-p context) original-context-logging-enabled)
      (setf (runtime-context-log-level context) original-context-log-level)
      (setf (runtime-context-log-stream context) original-context-log-stream)
      (setf (runtime-context-http-connect-timeout context) original-context-connect)
      (setf (runtime-context-http-read-timeout context) original-context-read))))

(fiveam:test test-continue-stateless-tool-recursion-updates-history-and-preserves-order
  (let* ((conversation (new-chat :backend :openai))
        (history-messages (list (list (cons "role" "user")
                                      (cons "content" "hello"))))
        (recursion-messages (list (list (cons "role" "assistant")
                                        (cons "content" nil)
                                        (cons "tool_calls" (list (list (cons "id" "call-1")))))
                                  (list (cons "role" "tool")
                                        (cons "tool_call_id" "call-1")
                                        (cons "content" "tool result"))))
        (captured-history nil)
        (captured-messages nil))
    (fiveam:is
     (equal (append history-messages recursion-messages)
           (continue-stateless-tool-recursion
            conversation
            history-messages
            recursion-messages
            (lambda (updated-history updated-messages)
              (setf captured-history updated-history)
              (setf captured-messages updated-messages)
              updated-history))))
    (fiveam:is (equal (append history-messages recursion-messages)
                     (conversation-messages conversation)))
    (fiveam:is (equal (append history-messages recursion-messages)
                     captured-history))
    (fiveam:is (equal recursion-messages captured-messages))))

(fiveam:test test-emit-chat-response-text-handles-formatting-usage-and-callback
  (reset-global-token-grand-totals)
  (let ((callback-text nil)
       (stream (make-string-output-stream)))
    (let ((*standard-output* stream))
     (fiveam:is (string= "Shared reply"
                         (emit-chat-response-text
                          "Shared reply"
                          :callback (lambda (text)
                                      (setf callback-text text))
                          :usage '(("total_input_tokens" . 2)
                                   ("total_output_tokens" . 3)
                                   ("total_tokens" . 5))))))
    (let ((output (get-output-stream-string stream)))
     (fiveam:is (search "Shared reply" output))
     (fiveam:is (search "[Tokens] prompt: 2" output))
     (fiveam:is (string= "Shared reply" callback-text)))))

(fiveam:test test-emit-chat-response-text-prints-short-thoughts
  (reset-global-token-grand-totals)
  (let ((stream (make-string-output-stream)))
    (let ((*standard-output* stream))
     (emit-chat-response-text
      "Shared reply"
      :usage '(("total_input_tokens" . 2)
               ("total_output_tokens" . 3)
               ("total_thought_tokens" . 4)
               ("total_tokens" . 9))
      :thought-text "Tiny chain of thought"))
    (let ((output (get-output-stream-string stream)))
     (fiveam:is (search "[Thoughts]" output))
     (fiveam:is (search "Tiny chain of thought" output)))))

(fiveam:test test-emit-chat-response-text-does-not-print-long-thoughts
  (reset-global-token-grand-totals)
  (let ((stream (make-string-output-stream)))
    (let ((*standard-output* stream))
     (emit-chat-response-text
      "Shared reply"
      :usage '(("total_input_tokens" . 2)
               ("total_output_tokens" . 3)
               ("total_thought_tokens" . 16)
               ("total_tokens" . 21))
      :thought-text "Do not print this"))
    (let ((output (get-output-stream-string stream)))
     (fiveam:is (null (search "[Thoughts]" output)))
     (fiveam:is (null (search "Do not print this" output))))))

(fiveam:test test-sanitize-chat-response-text-removes-antml-thinking-blocks
  (fiveam:is (string= "Visible reply"
                     (sanitize-chat-response-text
                      "<antmlThinking>hidden</antmlThinking>Visible reply")))
  (fiveam:is (string= "Visible reply"
                     (sanitize-chat-response-text
                      "<andtmlThinking>hidden</antmlThinging>Visible reply")))
  (fiveam:is (string= "Visible reply"
                     (sanitize-chat-response-text
                      "<antmlThinking>Visible reply"))))

(fiveam:test test-finish-stateless-text-turn-emits-and-persists-history
  (let* ((conversation (new-chat :backend :google))
        (history-messages (list (list (cons "role" "user")
                                      (cons "content" "hello"))))
        (stream (make-string-output-stream)))
    (let ((*standard-output* stream))
     (fiveam:is (string= "Shared final"
                         (finish-stateless-text-turn conversation
                                                     history-messages
                                                     "model"
                                                     "Shared final"))))
    (fiveam:is (search "Shared final" (get-output-stream-string stream)))
    (fiveam:is (equal (append history-messages
                             (list (list (cons "role" "model")
                                         (cons "content" "Shared final"))))
                     (conversation-messages conversation)))))

(fiveam:test test-finish-stateless-text-turn-sanitizes-visible-and-stored-text
  (let* ((conversation (new-chat :backend :google))
        (history-messages (list (list (cons "role" "user")
                                      (cons "content" "hello"))))
        (stream (make-string-output-stream)))
    (let ((*standard-output* stream))
     (fiveam:is (string= "Shared final"
                         (finish-stateless-text-turn conversation
                                                     history-messages
                                                     "model"
                                                     "<andtmlThinking>secret</antmlThinging>Shared final"))))
    (let ((output (get-output-stream-string stream)))
     (fiveam:is (search "Shared final" output))
     (fiveam:is-false (search "secret" output)))
    (fiveam:is (equal (append history-messages
                             (list (list (cons "role" "model")
                                         (cons "content" "Shared final"))))
                     (conversation-messages conversation)))))

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
           (fiveam:is (string= "explicit-next.lisp" (current-mcp-config-path context)))
           (fiveam:is-true (current-auto-initialize-startup-mcp-servers-p context))
           (fiveam:is-false (current-logging-enabled-p context))
           (fiveam:is (eq :warn (current-log-level context)))
           (fiveam:is (eq test-stream (current-log-stream context)))
           (fiveam:is (= 19 (current-http-connect-timeout context)))
           (fiveam:is (= 88 (current-http-read-timeout context)))))
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

(fiveam:test test-deprecated-runtime-globals-no-longer-override-current-helpers
  (let ((original-read-timeout *http-read-timeout*)
        (original-context-read (runtime-context-http-read-timeout *default-runtime-context*)))
    (unwind-protect
         (progn
           (setf (runtime-context-http-read-timeout *default-runtime-context*) 88)
           (setf *http-read-timeout* 141)
           (fiveam:is (= 88 (current-http-read-timeout))))
      (setf *http-read-timeout* original-read-timeout)
      (setf (runtime-context-http-read-timeout *default-runtime-context*) original-context-read))))

(fiveam:test test-explicit-runtime-context-does-not-warn-about-legacy-globals
  (let* ((context (make-runtime-context :http-connect-timeout 7)))
    (let ((*error-output* (make-string-output-stream)))
      (call-with-runtime-context
       context
       (lambda ()
         (setf (current-http-connect-timeout context) 12)
         (fiveam:is (= 12 (current-http-connect-timeout context)))))
      (fiveam:is (string= "" (get-output-stream-string *error-output*))))))

(fiveam:test test-no-arg-current-helpers-prefer-active-runtime-context
  (let* ((default-context *default-runtime-context*)
        (original-default-connect-timeout (runtime-context-http-connect-timeout default-context))
        (original-default-startup-chatbot (runtime-context-startup-chatbot default-context))
        (explicit-bot (make-instance 'chatbot))
        (context (make-runtime-context :http-connect-timeout 29
                                      :startup-chatbot explicit-bot)))
    (unwind-protect
        (call-with-runtime-context
         context
         (lambda ()
          (fiveam:is (= 29 (current-http-connect-timeout)))
          (fiveam:is (eq explicit-bot (current-startup-chatbot)))
          (fiveam:is (= original-default-connect-timeout
                        (runtime-context-http-connect-timeout default-context)))
          (fiveam:is (eq original-default-startup-chatbot
                         (runtime-context-startup-chatbot default-context)))))
      (setf (runtime-context-http-connect-timeout default-context) original-default-connect-timeout)
      (setf (runtime-context-startup-chatbot default-context) original-default-startup-chatbot))))

(fiveam:test test-no-arg-transient-helpers-prefer-active-runtime-context-without-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
        (explicit-conversation (new-chat :runtime-context (make-runtime-context)))
        (explicit-planner (new-chat :runtime-context (make-runtime-context)))
        (explicit-parent (new-chat :runtime-context (make-runtime-context)))
        (legacy-conversation (new-chat))
        (legacy-planner (new-chat))
        (legacy-parent (new-chat))
        (original-default-active-conversation (runtime-context-active-conversation default-context))
        (original-default-active-planner (runtime-context-active-planner default-context))
        (original-default-active-planner-parent
          (runtime-context-active-planner-parent-conversation default-context))
        (original-legacy-active-conversation *active-conversation*)
        (original-legacy-active-planner *active-planner*)
        (original-legacy-active-planner-parent *active-planner-parent-conversation*)
        (context (make-runtime-context)))
    (unwind-protect
        (progn
          (setf *active-conversation* legacy-conversation)
          (setf *active-planner* legacy-planner)
          (setf *active-planner-parent-conversation* legacy-parent)
          (call-with-runtime-context
           context
           (lambda ()
             (setf (current-active-conversation) explicit-conversation)
             (setf (current-active-planner) explicit-planner)
             (setf (current-active-planner-parent-conversation) explicit-parent)
             (fiveam:is (eq explicit-conversation (current-active-conversation)))
             (fiveam:is (eq explicit-planner (current-active-planner)))
             (fiveam:is (eq explicit-parent
                           (current-active-planner-parent-conversation)))
             (fiveam:is (eq legacy-conversation *active-conversation*))
             (fiveam:is (eq legacy-planner *active-planner*))
             (fiveam:is (eq legacy-parent *active-planner-parent-conversation*))))
          (fiveam:is (eq explicit-conversation
                        (runtime-context-active-conversation context)))
          (fiveam:is (eq explicit-planner
                        (runtime-context-active-planner context)))
          (fiveam:is (eq explicit-parent
                        (runtime-context-active-planner-parent-conversation context)))
          (fiveam:is (eq legacy-conversation *active-conversation*))
          (fiveam:is (eq legacy-planner *active-planner*))
          (fiveam:is (eq legacy-parent *active-planner-parent-conversation*)))
      (setf (runtime-context-active-conversation default-context) original-default-active-conversation)
      (setf (runtime-context-active-planner default-context) original-default-active-planner)
      (setf (runtime-context-active-planner-parent-conversation default-context)
           original-default-active-planner-parent)
      (setf *active-conversation* original-legacy-active-conversation)
      (setf *active-planner* original-legacy-active-planner)
      (setf *active-planner-parent-conversation* original-legacy-active-planner-parent))))

(fiveam:test test-default-runtime-context-transient-helpers-no-longer-mirror-legacy-globals
  (let* ((default-context *default-runtime-context*)
        (conversation (new-chat))
        (planner (new-chat))
        (parent (new-chat))
        (original-default-active-conversation (runtime-context-active-conversation default-context))
        (original-default-active-planner (runtime-context-active-planner default-context))
        (original-default-active-planner-parent
          (runtime-context-active-planner-parent-conversation default-context))
        (original-legacy-active-conversation *active-conversation*)
        (original-legacy-active-planner *active-planner*)
        (original-legacy-active-planner-parent *active-planner-parent-conversation*))
    (unwind-protect
        (call-with-runtime-context
         default-context
         (lambda ()
           (setf (current-active-conversation) conversation)
           (setf (current-active-planner) planner)
           (setf (current-active-planner-parent-conversation) parent)
           (fiveam:is (eq original-legacy-active-conversation *active-conversation*))
           (fiveam:is (eq original-legacy-active-planner *active-planner*))
           (fiveam:is (eq original-legacy-active-planner-parent
                         *active-planner-parent-conversation*))
           (fiveam:is (eq conversation
                          (runtime-context-active-conversation default-context)))
           (fiveam:is (eq planner
                          (runtime-context-active-planner default-context)))
           (fiveam:is (eq parent
                          (runtime-context-active-planner-parent-conversation
                           default-context)))))
      (setf (runtime-context-active-conversation default-context) original-default-active-conversation)
      (setf (runtime-context-active-planner default-context) original-default-active-planner)
      (setf (runtime-context-active-planner-parent-conversation default-context)
           original-default-active-planner-parent)
      (setf *active-conversation* original-legacy-active-conversation)
      (setf *active-planner* original-legacy-active-planner)
      (setf *active-planner-parent-conversation* original-legacy-active-planner-parent))))

(fiveam:test test-explicit-runtime-context-transient-getters-do-not-fall-back-to-legacy-globals
  (let* ((context (make-runtime-context))
        (legacy-conversation (new-chat :runtime-context context))
        (legacy-planner (new-chat :runtime-context context))
        (legacy-parent (new-chat :runtime-context context))
        (original-legacy-active-conversation *active-conversation*)
        (original-legacy-active-planner *active-planner*)
        (original-legacy-active-planner-parent *active-planner-parent-conversation*))
    (unwind-protect
        (progn
          (setf *active-conversation* legacy-conversation)
          (setf *active-planner* legacy-planner)
          (setf *active-planner-parent-conversation* legacy-parent)
          (call-with-runtime-context
           context
           (lambda ()
             (fiveam:is-false (current-active-conversation))
             (fiveam:is-false (current-active-planner))
             (fiveam:is-false (current-active-planner-parent-conversation)))))
      (setf *active-conversation* original-legacy-active-conversation)
      (setf *active-planner* original-legacy-active-planner)
      (setf *active-planner-parent-conversation* original-legacy-active-planner-parent))))

(fiveam:test test-default-runtime-context-transient-getters-no-longer-fall-back-to-legacy-globals
  (let* ((default-context *default-runtime-context*)
        (legacy-conversation (new-chat))
        (legacy-planner (new-chat))
        (legacy-parent (new-chat))
        (original-default-active-conversation (runtime-context-active-conversation default-context))
        (original-default-active-planner (runtime-context-active-planner default-context))
        (original-default-active-planner-parent
          (runtime-context-active-planner-parent-conversation default-context))
        (original-legacy-active-conversation *active-conversation*)
        (original-legacy-active-planner *active-planner*)
        (original-legacy-active-planner-parent *active-planner-parent-conversation*))
    (unwind-protect
        (progn
          (setf (runtime-context-active-conversation default-context) nil)
          (setf (runtime-context-active-planner default-context) nil)
          (setf (runtime-context-active-planner-parent-conversation default-context) nil)
          (setf *active-conversation* legacy-conversation)
          (setf *active-planner* legacy-planner)
          (setf *active-planner-parent-conversation* legacy-parent)
          (call-with-runtime-context
           default-context
           (lambda ()
             (fiveam:is-false (current-active-conversation))
             (fiveam:is-false (current-active-planner))
             (fiveam:is-false (current-active-planner-parent-conversation))
             (fiveam:is (eq legacy-conversation *active-conversation*))
             (fiveam:is (eq legacy-planner *active-planner*))
             (fiveam:is (eq legacy-parent *active-planner-parent-conversation*)))))
      (setf (runtime-context-active-conversation default-context) original-default-active-conversation)
      (setf (runtime-context-active-planner default-context) original-default-active-planner)
      (setf (runtime-context-active-planner-parent-conversation default-context)
           original-default-active-planner-parent)
      (setf *active-conversation* original-legacy-active-conversation)
      (setf *active-planner* original-legacy-active-planner)
      (setf *active-planner-parent-conversation* original-legacy-active-planner-parent))))

(fiveam:test test-make-runtime-context-inherits-from-canonical-context-not-legacy-globals
  (let* ((default-context *default-runtime-context*)
        (original-default-log-level (runtime-context-log-level default-context))
        (original-default-connect-timeout (runtime-context-http-connect-timeout default-context))
        (original-legacy-log-level *log-level*)
        (original-legacy-connect-timeout *http-connect-timeout*))
    (unwind-protect
        (progn
          (setf (runtime-context-log-level default-context) :warn)
          (setf (runtime-context-http-connect-timeout default-context) 23)
          (setf *log-level* :error)
          (setf *http-connect-timeout* 91)
          (let ((context (make-runtime-context)))
            (fiveam:is (eq :warn (runtime-context-log-level context)))
            (fiveam:is (= 23 (runtime-context-http-connect-timeout context)))))
      (setf (runtime-context-log-level default-context) original-default-log-level)
      (setf (runtime-context-http-connect-timeout default-context) original-default-connect-timeout)
      (setf *log-level* original-legacy-log-level)
      (setf *http-connect-timeout* original-legacy-connect-timeout))))

(fiveam:test test-explicit-runtime-context-getenv-function-does-not-rely-on-legacy-mirroring
  (let* ((context (make-runtime-context :getenv-function
                                       (lambda (name)
                                         (if (string= name "OPENAI_API_KEY")
                                             "context-env-key"
                                             nil))))
        (original-openai-api-key *openai-api-key*)
        (original-getenv-function *getenv-function*))
    (unwind-protect
        (let ((*openai-api-key* nil)
              (*getenv-function* (lambda (name)
                                   (if (string= name "OPENAI_API_KEY")
                                       "legacy-env-key"
                                       nil))))
          (call-with-runtime-context
           context
           (lambda ()
             (fiveam:is (string= "context-env-key" (openai-api-key))))))
      (setf *openai-api-key* original-openai-api-key)
      (setf *getenv-function* original-getenv-function))))

(fiveam:test test-no-arg-function-seam-helpers-prefer-active-runtime-context
  (let* ((context-function (lambda (name)
                            (if (string= name "OPENAI_API_KEY")
                                "context-env-key"
                                nil)))
        (context (make-runtime-context :getenv-function context-function))
        (legacy-function (lambda (name)
                           (if (string= name "OPENAI_API_KEY")
                               "legacy-env-key"
                               nil)))
        (original-openai-api-key *openai-api-key*)
        (original-getenv-function *getenv-function*)
        (legacy-result nil))
    (unwind-protect
       (let ((*openai-api-key* nil)
             (*getenv-function* legacy-function))
         (call-with-runtime-context
          context
          (lambda ()
            (fiveam:is (eq context-function (current-getenv-function)))
            (setf legacy-result (funcall *getenv-function* "OPENAI_API_KEY"))
            (fiveam:is (string= "context-env-key" (openai-api-key)))
            (setf (current-getenv-function)
                  (lambda (name)
                    (if (string= name "OPENAI_API_KEY")
                        "updated-context-env-key"
                        nil)))
            (fiveam:is (string= "updated-context-env-key" (openai-api-key))))))
      (setf *openai-api-key* original-openai-api-key)
      (setf *getenv-function* original-getenv-function))
    (fiveam:is (string= "updated-context-env-key"
                       (funcall (runtime-context-getenv-function context)
                                "OPENAI_API_KEY")))
    (fiveam:is (string= "legacy-env-key" legacy-result))))

(fiveam:test test-explicit-runtime-context-filesystem-approval-does-not-rely-on-legacy-mirroring
  (let* ((context (make-runtime-context :filesystem-access-approval-function
                                       (lambda (&rest ignored)
                                         (declare (ignore ignored))
                                         :context-approval)))
         (original-approval-function *filesystem-access-approval-function*))
    (unwind-protect
         (let ((*filesystem-access-approval-function* (lambda (&rest ignored)
                                                       (declare (ignore ignored))
                                                       :legacy-approval)))
           (fiveam:is (eq :context-approval
                         (funcall (current-filesystem-access-approval-function context)))))
      (setf *filesystem-access-approval-function* original-approval-function))))

(fiveam:test test-no-arg-approval-helper-prefers-active-runtime-context
  (let* ((context (make-runtime-context :filesystem-access-approval-function
                                       (lambda (&rest ignored)
                                         (declare (ignore ignored))
                                         :context-approval)))
        (legacy-approval-function (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    :legacy-approval))
        (original-approval-function *filesystem-access-approval-function*)
        (legacy-result nil))
    (unwind-protect
       (let ((*filesystem-access-approval-function* legacy-approval-function))
         (call-with-runtime-context
          context
          (lambda ()
            (setf legacy-result (funcall *filesystem-access-approval-function*))
            (fiveam:is (eq :context-approval
                           (funcall (current-filesystem-access-approval-function))))
            (setf (current-filesystem-access-approval-function)
                  (lambda (&rest ignored)
                    (declare (ignore ignored))
                    :updated-context-approval))
            (fiveam:is (eq :updated-context-approval
                           (funcall (current-filesystem-access-approval-function)))))))
      (setf *filesystem-access-approval-function* original-approval-function))
    (fiveam:is (eq :updated-context-approval
                  (funcall (runtime-context-filesystem-access-approval-function context))))
    (fiveam:is (eq :legacy-approval legacy-result))))

(fiveam:test test-default-runtime-context-filesystem-approval-prefers-runtime-context
  (let* ((default-context *default-runtime-context*)
        (original-approval-function *filesystem-access-approval-function*)
        (original-default-approval
          (runtime-context-filesystem-access-approval-function default-context)))
    (unwind-protect
       (progn
         (setf (runtime-context-filesystem-access-approval-function default-context)
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 :context-approval))
         (fiveam:is (eq :context-approval
                        (funcall (current-filesystem-access-approval-function default-context))))
         (let ((*filesystem-access-approval-function* (lambda (&rest ignored)
                                                        (declare (ignore ignored))
                                                        :legacy-approval)))
           (call-with-runtime-context
            default-context
            (lambda ()
              (fiveam:is (eq :context-approval
                             (funcall (current-filesystem-access-approval-function))))
              (fiveam:is (eq :context-approval
                             (funcall (current-filesystem-access-approval-function
                                       default-context))))))))
      (setf *filesystem-access-approval-function* original-approval-function)
      (setf (runtime-context-filesystem-access-approval-function default-context)
           original-default-approval))))

(fiveam:test test-default-runtime-context-eval-approval-prefers-runtime-context
  (let* ((default-context *default-runtime-context*)
       (original-approval-function *eval-approval-function*)
       (original-default-approval
         (runtime-context-eval-approval-function default-context)))
    (unwind-protect
      (progn
        (setf (runtime-context-eval-approval-function default-context)
              (lambda (&rest ignored)
                (declare (ignore ignored))
                :context-approval))
        (fiveam:is (eq :context-approval
                       (funcall (current-eval-approval-function default-context))))
        (let ((*eval-approval-function* (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          :legacy-approval)))
          (call-with-runtime-context
           default-context
           (lambda ()
             (fiveam:is (eq :context-approval
                            (funcall (current-eval-approval-function))))
             (fiveam:is (eq :context-approval
                            (funcall (current-eval-approval-function
                                      default-context))))))))
      (setf *eval-approval-function* original-approval-function)
      (setf (runtime-context-eval-approval-function default-context)
          original-default-approval))))

(fiveam:test test-default-runtime-context-no-arg-function-seam-no-longer-falls-back-to-legacy
  (let* ((default-context *default-runtime-context*)
        (original-getenv-function *getenv-function*)
        (original-default-getenv (runtime-context-getenv-function default-context)))
    (unwind-protect
        (progn
          (setf (runtime-context-getenv-function default-context)
               (lambda (name)
                 (if (string= name "OPENAI_API_KEY")
                     "context-env-key"
                     nil)))
          (fiveam:is (string= "context-env-key"
                             (funcall (current-getenv-function default-context)
                                      "OPENAI_API_KEY")))
          (let ((*getenv-function* (lambda (name)
                                    (if (string= name "OPENAI_API_KEY")
                                        "legacy-env-key"
                                        nil))))
           (call-with-runtime-context
            default-context
            (lambda ()
              (fiveam:is (string= "context-env-key"
                                  (funcall (current-getenv-function)
                                           "OPENAI_API_KEY")))))))
      (setf *getenv-function* original-getenv-function)
      (setf (runtime-context-getenv-function default-context)
           original-default-getenv))))

(fiveam:test test-explicit-runtime-context-startup-chatbot-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (legacy-startup-bot (make-instance 'chatbot))
         (context-startup-bot (make-instance 'chatbot))
         (original-legacy-startup-bot *startup-chatbot*)
         (original-default-startup-bot (runtime-context-startup-chatbot default-context))
         (context (make-runtime-context :startup-chatbot context-startup-bot)))
    (unwind-protect
         (progn
           (setf *startup-chatbot* legacy-startup-bot)
           (setf (runtime-context-startup-chatbot default-context) legacy-startup-bot)
           (fiveam:is (eq context-startup-bot
                         (current-startup-chatbot context))))
      (setf *startup-chatbot* original-legacy-startup-bot)
      (setf (runtime-context-startup-chatbot default-context) original-default-startup-bot))))

(fiveam:test test-explicit-runtime-context-default-conversation-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
        (legacy-conversation (new-chat))
        (context-conversation (new-chat))
        (original-legacy-conversation *default-conversation*)
        (original-default-conversation (runtime-context-default-conversation default-context))
        (context (make-runtime-context :default-conversation context-conversation)))
    (unwind-protect
        (progn
          (setf *default-conversation* legacy-conversation)
          (setf (runtime-context-default-conversation default-context) legacy-conversation)
          (fiveam:is (eq context-conversation
                        (current-default-conversation context))))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation))))

(fiveam:test test-call-with-runtime-context-explicit-context-does-not-rebind-legacy-default-conversation
  (let* ((legacy-conversation (new-chat))
         (context-conversation (new-chat))
         (context (make-runtime-context :default-conversation context-conversation))
         (original-legacy-conversation *default-conversation*))
    (unwind-protect
         (progn
           (setf *default-conversation* legacy-conversation)
           (call-with-runtime-context
            context
            (lambda ()
              (fiveam:is (eq context-conversation
                           (current-default-conversation)))
              (fiveam:is (eq legacy-conversation *default-conversation*))
              (setf (current-default-conversation) nil)
              (fiveam:is (null (current-default-conversation)))
              (fiveam:is (eq legacy-conversation *default-conversation*))))
           (fiveam:is (null (runtime-context-default-conversation context)))
           (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation))))

(fiveam:test test-call-with-runtime-context-default-context-does-not-rebind-legacy-default-conversation
  (let* ((default-context *default-runtime-context*)
         (legacy-conversation (new-chat))
         (context-conversation (new-chat))
         (original-legacy-conversation *default-conversation*)
         (original-default-conversation (runtime-context-default-conversation default-context)))
    (unwind-protect
         (progn
           (setf *default-conversation* legacy-conversation)
           (setf (runtime-context-default-conversation default-context) context-conversation)
           (call-with-runtime-context
            default-context
            (lambda ()
              (fiveam:is (eq legacy-conversation *default-conversation*))
              (fiveam:is (eq context-conversation (current-default-conversation)))
              (setf *default-conversation* legacy-conversation)))
           (fiveam:is (eq context-conversation
                        (runtime-context-default-conversation default-context)))
           (fiveam:is (eq legacy-conversation *default-conversation*)))
      (setf *default-conversation* original-legacy-conversation)
      (setf (runtime-context-default-conversation default-context) original-default-conversation))))

(fiveam:test test-call-with-runtime-context-opt-out-does-not-sync-legacy-function-or-approval-seams
  (let* ((default-context *default-runtime-context*)
         (context-getenv-function (lambda (name)
                                   (declare (ignore name))
                                   "context-env-key"))
         (legacy-getenv-function (lambda (name)
                                  (declare (ignore name))
                                  "legacy-env-key"))
         (context-approval-function (lambda (&rest ignored)
                                     (declare (ignore ignored))
                                     :context-approval))
         (legacy-approval-function (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    :legacy-approval))
         (original-default-getenv (runtime-context-getenv-function default-context))
         (original-default-approval
           (runtime-context-filesystem-access-approval-function default-context))
         (original-legacy-getenv *getenv-function*)
         (original-legacy-approval *filesystem-access-approval-function*))
    (unwind-protect
         (progn
           (setf (runtime-context-getenv-function default-context) context-getenv-function)
           (setf (runtime-context-filesystem-access-approval-function default-context)
                context-approval-function)
           (let ((*getenv-function* legacy-getenv-function)
                (*filesystem-access-approval-function* legacy-approval-function))
            (call-with-runtime-context
             default-context
             (lambda ())
             :default-conversation-compatibility-p nil
             :legacy-function-seam-compatibility-p nil))
           (fiveam:is (eq context-getenv-function
                        (runtime-context-getenv-function default-context)))
           (fiveam:is (eq context-approval-function
                        (runtime-context-filesystem-access-approval-function
                         default-context))))
      (setf (runtime-context-getenv-function default-context) original-default-getenv)
      (setf (runtime-context-filesystem-access-approval-function default-context)
           original-default-approval)
      (setf *getenv-function* original-legacy-getenv)
      (setf *filesystem-access-approval-function* original-legacy-approval))))

(fiveam:test test-call-with-runtime-context-default-compatibility-no-longer-syncs-legacy-function-seams
  (let* ((default-context *default-runtime-context*)
         (context-getenv-function (lambda (name)
                                   (declare (ignore name))
                                   "context-env-key"))
         (legacy-getenv-function (lambda (name)
                                  (declare (ignore name))
                                  "legacy-env-key"))
         (context-approval-function (lambda (&rest ignored)
                                     (declare (ignore ignored))
                                     :context-approval))
         (legacy-approval-function (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    :legacy-approval))
         (original-default-getenv (runtime-context-getenv-function default-context))
         (original-default-approval
           (runtime-context-filesystem-access-approval-function default-context))
         (original-legacy-getenv *getenv-function*)
         (original-legacy-approval *filesystem-access-approval-function*))
    (unwind-protect
         (progn
           (setf (runtime-context-getenv-function default-context) context-getenv-function)
           (setf (runtime-context-filesystem-access-approval-function default-context)
                context-approval-function)
           (let ((*getenv-function* legacy-getenv-function)
                (*filesystem-access-approval-function* legacy-approval-function))
            (call-with-runtime-context
             default-context
             (lambda ())))
           (fiveam:is (eq context-getenv-function
                        (runtime-context-getenv-function default-context)))
           (fiveam:is (eq context-approval-function
                        (runtime-context-filesystem-access-approval-function
                         default-context))))
      (setf (runtime-context-getenv-function default-context) original-default-getenv)
      (setf (runtime-context-filesystem-access-approval-function default-context)
           original-default-approval)
      (setf *getenv-function* original-legacy-getenv)
      (setf *filesystem-access-approval-function* original-legacy-approval))))

(fiveam:test test-explicit-runtime-context-mcp-config-path-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (legacy-path "legacy-config.lisp")
         (context-path "context-config.lisp")
         (original-legacy-path *mcp-config-path*)
         (original-default-path (runtime-context-mcp-config-path default-context))
         (context (make-runtime-context :mcp-config-path context-path)))
    (unwind-protect
         (progn
          (setf *mcp-config-path* legacy-path)
          (setf (runtime-context-mcp-config-path default-context) legacy-path)
          (fiveam:is (string= context-path
                              (current-mcp-config-path context))))
      (setf *mcp-config-path* original-legacy-path)
      (setf (runtime-context-mcp-config-path default-context) original-default-path))))

(fiveam:test test-explicit-runtime-context-log-stream-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (legacy-stream (make-string-output-stream))
         (context-stream (make-string-output-stream))
         (original-legacy-stream *log-stream*)
         (original-default-stream (runtime-context-log-stream default-context))
         (context (make-runtime-context :log-stream context-stream)))
    (unwind-protect
         (progn
          (setf *log-stream* legacy-stream)
          (setf (runtime-context-log-stream default-context) legacy-stream)
          (fiveam:is (eq context-stream
                         (current-log-stream context))))
      (setf *log-stream* original-legacy-stream)
      (setf (runtime-context-log-stream default-context) original-default-stream))))

(fiveam:test test-explicit-runtime-context-logging-enabled-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (original-legacy-enabled *logging-enabled-p*)
         (original-default-enabled (runtime-context-logging-enabled-p default-context))
         (context (make-runtime-context :logging-enabled-p nil)))
    (unwind-protect
         (progn
          (setf *logging-enabled-p* t)
          (setf (runtime-context-logging-enabled-p default-context) t)
          (fiveam:is-false (current-logging-enabled-p context)))
      (setf *logging-enabled-p* original-legacy-enabled)
      (setf (runtime-context-logging-enabled-p default-context) original-default-enabled))))

(fiveam:test test-explicit-runtime-context-log-level-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (original-legacy-level *log-level*)
         (original-default-level (runtime-context-log-level default-context))
         (context (make-runtime-context :log-level :warn)))
    (unwind-protect
         (progn
          (setf *log-level* :error)
          (setf (runtime-context-log-level default-context) :error)
          (fiveam:is (eq :warn (current-log-level context))))
      (setf *log-level* original-legacy-level)
      (setf (runtime-context-log-level default-context) original-default-level))))

(fiveam:test test-explicit-runtime-context-http-connect-timeout-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (original-legacy-timeout *http-connect-timeout*)
         (original-default-timeout (runtime-context-http-connect-timeout default-context))
         (context (make-runtime-context :http-connect-timeout 19)))
    (unwind-protect
         (progn
          (setf *http-connect-timeout* 91)
          (setf (runtime-context-http-connect-timeout default-context) 91)
          (fiveam:is (= 19 (current-http-connect-timeout context))))
      (setf *http-connect-timeout* original-legacy-timeout)
      (setf (runtime-context-http-connect-timeout default-context) original-default-timeout))))

(fiveam:test test-explicit-runtime-context-http-read-timeout-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (original-legacy-timeout *http-read-timeout*)
         (original-default-timeout (runtime-context-http-read-timeout default-context))
         (context (make-runtime-context :http-read-timeout 88)))
    (unwind-protect
         (progn
          (setf *http-read-timeout* 141)
          (setf (runtime-context-http-read-timeout default-context) 141)
          (fiveam:is (= 88 (current-http-read-timeout context))))
      (setf *http-read-timeout* original-legacy-timeout)
      (setf (runtime-context-http-read-timeout default-context) original-default-timeout))))

(fiveam:test test-explicit-runtime-context-auto-init-does-not-rely-on-legacy-mirroring
  (let* ((default-context *default-runtime-context*)
         (original-legacy-auto-init *auto-initialize-startup-mcp-servers-p*)
         (original-default-auto-init
          (runtime-context-auto-initialize-startup-mcp-servers-p default-context))
         (context (make-runtime-context :auto-initialize-startup-mcp-servers-p nil)))
    (unwind-protect
         (progn
          (setf *auto-initialize-startup-mcp-servers-p* t)
          (setf (runtime-context-auto-initialize-startup-mcp-servers-p default-context) t)
          (fiveam:is-false (current-auto-initialize-startup-mcp-servers-p context)))
      (setf *auto-initialize-startup-mcp-servers-p* original-legacy-auto-init)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p default-context)
           original-default-auto-init))))

(fiveam:test test-new-chat-without-persona-starts-empty
  (let* ((conv (new-chat))
         (bot (conversation-chatbot conv))
         (default-bot (make-instance 'chatbot)))
    (fiveam:is (typep conv 'conversation))
    (fiveam:is (typep bot 'chatbot))
    (fiveam:is (null (conversation-messages conv)))
    (fiveam:is (null (conversation-interaction-id conv)))
    (fiveam:is-false (chatbot-gemini-fallback-to-google-p bot))
    (fiveam:is-false (chatbot-gemini-fallback-to-google-p default-bot))))

(fiveam:test test-new-chat-subordinates
  (let* ((sub-conv-1 (new-chat))
         (sub-conv-2 (new-chat))
         (conv (new-chat :subordinates (list sub-conv-1 sub-conv-2)))
         (bot (conversation-chatbot conv)))
    (fiveam:is (typep conv 'conversation))
    (fiveam:is (typep bot 'chatbot))
    (fiveam:is (listp (chatbot-subordinates bot)))
    (fiveam:is (= 2 (length (chatbot-subordinates bot))))
    (fiveam:is (eq sub-conv-1 (first (chatbot-subordinates bot))))
    (fiveam:is (eq sub-conv-2 (second (chatbot-subordinates bot))))))

(fiveam:test test-gemini-chat-falls-back-on-interactions-404
  (let ((conv (new-chat :backend :gemini :gemini-fallback-to-google-p t))
       (calls '())
       (fallback-headers nil))
    (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
         (*http-post-function*
            (lambda (url &rest args)
              (push url calls)
              (if (search "/interactions?alt=sse" url)
                  (error "An HTTP request to \"~A\" returned 404 not found.~%~%{\"error\":{\"message\":\"Requested entity was not found.\",\"code\":\"not_found\"}}" url)
                  (progn
                    (setf fallback-headers (getf args :headers))
                    (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from fallback\"}], \"role\": \"model\"}}]}" 200))))))
      (let ((res (chat "Hi fallback" :conversation conv)))
        (fiveam:is (string= "Hello from fallback" res))
        (fiveam:is (= 2 (length calls)))
        (fiveam:is (search "/interactions?alt=sse" (second calls)))
        (fiveam:is (search ":generateContent" (first calls)))
        (fiveam:is-false (search "?key=" (first calls)))
        (fiveam:is (string= "mocked-google-api-key"
                            (cdr (assoc "x-goog-api-key" fallback-headers :test #'string=))))))))

(fiveam:test test-gemini-chat-does-not-fall-back-by-default-on-interactions-404
  (let ((conv (new-chat :backend :gemini))
        (calls '()))
    (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore args))
              (push url calls)
              (error "An HTTP request to \"~A\" returned 404 not found.~%~%{\"error\":{\"message\":\"Requested entity was not found.\",\"code\":\"not_found\"}}" url))))
      (fiveam:signals error
        (chat "Hi fallback" :conversation conv))
      (fiveam:is (= 1 (length calls)))
      (fiveam:is (search "/interactions?alt=sse" (first calls))))))

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
      (let ((second-payload (decode-test-json (first captured-payloads)))
            (first-payload (decode-test-json (second captured-payloads))))
        (assert-json-field= first-payload "input" "First turn")
        (fiveam:is-false (test-json-value-any first-payload
                                              '("previous_interaction_id" :previous-interaction-id)))
        (assert-json-field= second-payload "input" "Second turn")
        (assert-json-field= second-payload "previous_interaction_id" "session-1"))
      (assert-history-sequence (conversation-messages conv)
                               '(("user" "First turn")
                                 ("model" "Hello one")
                                 ("user" "Second turn")
                                 ("model" "Hello two"))))))

(fiveam:test test-compression-clears-interaction-id-when-history-is-rewritten
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 75)
        (conv (new-chat :backend :google))
        (long-text (make-string 160 :initial-element #\A)))
    (setf (conversation-interaction-id conv) "session-1")
    (setf (conversation-messages conv)
         (list (list (cons "role" "user") (cons "content" long-text))
               (list (cons "role" "model") (cons "content" "response 1"))
               (list (cons "role" "user") (cons "content" long-text))
               (list (cons "role" "model") (cons "content" "response 2"))
               (list (cons "role" "user") (cons "content" long-text))
               (list (cons "role" "model") (cons "content" "response 3"))))
    (let ((*gemini-api-key-function* (lambda () "mocked-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url args))
             (values
              "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Digest summary.\"}], \"role\": \"model\"}}]}"
              200))))
      (compress-conversation-context-if-needed conv)
      (fiveam:is (null (conversation-interaction-id conv)))
      (let ((history (conversation-messages conv)))
       (fiveam:is (string= "system" (cdr (assoc "role" (first history) :test #'string=))))
       (fiveam:is (search "Digest summary."
                          (cdr (assoc "content" (first history) :test #'string=))))))))

(fiveam:test test-gemini-chat-dollar-prefix-overrides-model-for-one-turn
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
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-pro-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
                     "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Hello two\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":2,\"total_output_tokens\":1,\"total_tokens\":3}}}"))
                200))))
      (fiveam:is (string= "Hello one" (chat "$First turn" :conversation conv)))
      (fiveam:is (string= "Hello two" (chat "Second turn" :conversation conv)))
      (let ((first-payload (decode-test-json (second captured-payloads)))
            (second-payload (decode-test-json (first captured-payloads))))
        (assert-json-field= first-payload "model" "gemini-pro-latest")
        (assert-json-field= first-payload "input" "First turn")
        (assert-json-field= second-payload "model" "gemini-3.5-flash")
        (assert-json-field= second-payload "input" "Second turn")
        (fiveam:is (string= "gemini-3.5-flash" (chatbot-model (conversation-chatbot conv))))))))

(fiveam:test test-common-lisp-code-to-user-prompt-uses-gemini-flash-latest
  (let ((captured-payload nil))
    (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                           (declare (ignore bot))
                                           nil))
           (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
           (*http-post-function*
             (lambda (url &rest args)
               (declare (ignore url))
               (setf captured-payload (getf args :content))
               (values
                (make-string-input-stream
                 "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Please implement a function that returns the square of a number.\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")
                200))))
      (fiveam:is
       (string= "Please implement a function that returns the square of a number."
                (common-lisp-code-to-user-prompt
                 "(defun square (x)
                    \"Returns the square of X.\"
                    (* x x))")))
      (let ((payload (decode-test-json captured-payload)))
        (assert-json-field= payload "model" "gemini-flash-latest")
        (fiveam:is
         (search "expert Common Lisp developer"
                 (test-json-value-any payload '("input" :input))))
        (fiveam:is
         (search "```commonlisp"
                 (test-json-value-any payload '("input" :input))))
        (fiveam:is
         (search "(defun square (x)"
                 (test-json-value-any payload '("input" :input))))))))

(fiveam:test test-common-lisp-code-to-user-prompt-requires-non-empty-code
  (fiveam:signals error
    (common-lisp-code-to-user-prompt "")))

(fiveam:test test-read-file-forms-to-user-prompts-pairs-generated-prompts-with-form-text
  (let ((captured-payloads nil)
        (responses
          (list
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for alpha\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-2\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for beta\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-2\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")))
    (let* ((temp-dir (uiop:default-temporary-directory))
           (file (merge-pathnames "read-file-forms-to-user-prompts.lisp" temp-dir))
           (contents
             (format nil "~%~%(defun alpha (x)~%  (+ x 1))~%~%(defun beta (y)~%  (* y 2))")))
      (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string contents stream))
      (unwind-protect
           (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                                  (declare (ignore bot))
                                                  nil))
                  (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
                  (*http-post-function*
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (push (getf args :content) captured-payloads)
                      (values (make-string-input-stream (pop responses))
                              200)))
                  (result (read-file-forms-to-user-prompts file))
                  (expected-alpha (format nil "(defun alpha (x)~%  (+ x 1))"))
                  (expected-beta (format nil "(defun beta (y)~%  (* y 2))")))
             (fiveam:is
              (equal (list (list "Prompt for alpha" expected-alpha)
                           (list "Prompt for beta" expected-beta))
                     result))
             (fiveam:is (= 2 (length captured-payloads)))
             (let ((first-payload (decode-test-json (second captured-payloads)))
                   (second-payload (decode-test-json (first captured-payloads))))
               (fiveam:is (search expected-alpha
                                  (test-json-value-any first-payload '("input" :input))))
               (fiveam:is (search expected-beta
                                  (test-json-value-any second-payload '("input" :input))))))
        (when (probe-file file)
          (delete-file file))))))

(fiveam:test test-format-training-example-returns-chatml-json
  (let* ((json (format-training-example "Write a square function."
                                        "(defun square (x) (* x x))"))
         (payload (decode-test-json json))
         (messages (test-json-elements (test-json-value-any payload '("messages" :messages))))
         (user-message (first messages))
         (assistant-message (second messages)))
    (fiveam:is (string= "user"
                        (test-json-value-any user-message '("role" :role))))
    (fiveam:is (string= "Write a square function."
                        (test-json-value-any user-message '("content" :content))))
    (fiveam:is (string= "assistant"
                        (test-json-value-any assistant-message '("role" :role))))
    (fiveam:is (string= "(defun square (x) (* x x))"
                        (test-json-value-any assistant-message '("content" :content))))))

(fiveam:test test-read-file-forms-to-training-examples-formats-generated-pairs
  (let ((responses
          (list
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for alpha\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-2\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for beta\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-2\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")))
    (let* ((temp-dir (uiop:default-temporary-directory))
           (file (merge-pathnames "read-file-forms-to-training-examples.lisp" temp-dir))
           (contents
             (format nil "~%~%(defun alpha (x)~%  (+ x 1))~%~%(defun beta (y)~%  (* y 2))")))
      (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string contents stream))
      (unwind-protect
           (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                                  (declare (ignore bot))
                                                  nil))
                  (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
                  (*http-post-function*
                    (lambda (url &rest args)
                      (declare (ignore url args))
                      (values (make-string-input-stream (pop responses))
                              200)))
                  (examples (read-file-forms-to-training-examples file))
                  (first-payload (decode-test-json (first examples)))
                  (second-payload (decode-test-json (second examples)))
                  (first-messages (test-json-elements (test-json-value-any first-payload '("messages" :messages))))
                  (second-messages (test-json-elements (test-json-value-any second-payload '("messages" :messages))))
                  (expected-alpha (format nil "(defun alpha (x)~%  (+ x 1))"))
                  (expected-beta (format nil "(defun beta (y)~%  (* y 2))")))
             (fiveam:is (= 2 (length examples)))
             (fiveam:is (string= "Prompt for alpha"
                                 (test-json-value-any (first first-messages) '("content" :content))))
             (fiveam:is (string= expected-alpha
                                 (test-json-value-any (second first-messages) '("content" :content))))
             (fiveam:is (string= "Prompt for beta"
                                 (test-json-value-any (first second-messages) '("content" :content))))
             (fiveam:is (string= expected-beta
                                 (test-json-value-any (second second-messages) '("content" :content)))))
        (when (probe-file file)
          (delete-file file))))))

(fiveam:test test-append-file-forms-to-training-examples-appends_jsonl_examples
  (let ((responses
          (list
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for alpha\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-2\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for beta\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-2\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")))
    (let* ((temp-dir (uiop:default-temporary-directory))
           (source-file (merge-pathnames "append-file-forms-to-training-examples.lisp" temp-dir))
           (master-file (merge-pathnames "master-training-examples.jsonl" temp-dir))
           (contents
             (format nil "~%~%(defun alpha (x)~%  (+ x 1))~%~%(defun beta (y)~%  (* y 2))")))
      (with-open-file (stream source-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string contents stream))
      (with-open-file (stream master-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line "{\"messages\":[{\"role\":\"system\",\"content\":\"seed\"}]}" stream))
      (unwind-protect
           (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                                  (declare (ignore bot))
                                                  nil))
                  (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
                  (*http-post-function*
                    (lambda (url &rest args)
                      (declare (ignore url args))
                      (values (make-string-input-stream (pop responses))
                              200)))
                  (examples (append-file-forms-to-training-examples source-file master-file))
                  (lines (cl-ppcre:split "\\r?\\n"
                                         (string-trim '(#\Return #\Linefeed)
                                                      (uiop:read-file-string master-file)))))
             (fiveam:is (= 2 (length examples)))
             (fiveam:is (= 3 (length lines)))
             (fiveam:is (search "\"seed\"" (first lines)))
             (fiveam:is (search "Prompt for alpha" (second lines)))
             (fiveam:is (search "Prompt for beta" (third lines))))
        (when (probe-file source-file)
          (delete-file source-file))
        (when (probe-file master-file)
          (delete-file master-file))))))

(fiveam:test test-append-directory-lisp-files-to-training-examples-walks_recursively
  (let ((responses
          (list
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for alpha\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-2\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for beta\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-2\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")))
    (let* ((temp-dir (uiop:default-temporary-directory))
           (source-dir (merge-pathnames "append-directory-lisp-files/" temp-dir))
           (nested-dir (merge-pathnames "nested/" source-dir))
           (root-file (merge-pathnames "alpha.lisp" source-dir))
           (nested-file (merge-pathnames "beta.lisp" nested-dir))
           (ignored-file (merge-pathnames "ignore.txt" source-dir))
           (master-file (merge-pathnames "master-directory-training-examples.jsonl" temp-dir)))
      (ensure-directories-exist root-file)
      (ensure-directories-exist nested-file)
      (with-open-file (stream root-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (format nil "~%~%(defun alpha (x)~%  (+ x 1))") stream))
      (with-open-file (stream nested-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (format nil "~%~%(defun beta (y)~%  (* y 2))") stream))
      (with-open-file (stream ignored-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string "not lisp" stream))
      (with-open-file (stream master-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line "{\"messages\":[{\"role\":\"system\",\"content\":\"seed\"}]}" stream))
      (unwind-protect
           (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                                  (declare (ignore bot))
                                                  nil))
                  (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
                  (*http-post-function*
                    (lambda (url &rest args)
                      (declare (ignore url args))
                      (values (make-string-input-stream (pop responses))
                              200)))
                  (examples (append-directory-lisp-files-to-training-examples source-dir master-file))
                  (lines (cl-ppcre:split "\\r?\\n"
                                         (string-trim '(#\Return #\Linefeed)
                                                      (uiop:read-file-string master-file)))))
             (fiveam:is (= 2 (length examples)))
             (fiveam:is (= 3 (length lines)))
             (fiveam:is (search "\"seed\"" (first lines)))
             (fiveam:is (search "Prompt for alpha" (second lines)))
             (fiveam:is (search "Prompt for beta" (third lines))))
        (when (probe-file master-file)
          (delete-file master-file))
        (when (uiop:directory-exists-p source-dir)
          (uiop:delete-directory-tree source-dir :validate t))))))

(fiveam:test test-append-wild-lisp-files-to-training-examples-expands_wildcards
  (let ((responses
          (list
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for alpha\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-2\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for beta\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-2\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")))
    (let* ((temp-dir (uiop:default-temporary-directory))
           (source-dir (merge-pathnames "append-wild-lisp-files/" temp-dir))
           (alpha-file (merge-pathnames "alpha.lisp" source-dir))
           (beta-file (merge-pathnames "beta.lisp" source-dir))
           (ignored-file (merge-pathnames "ignore.txt" source-dir))
           (wild-path (merge-pathnames "*.lisp" source-dir))
           (master-file (merge-pathnames "master-wild-training-examples.jsonl" temp-dir))
           (progress-file (merge-pathnames "master-wild-training-examples.progress.sexp" temp-dir)))
      (ensure-directories-exist alpha-file)
      (with-open-file (stream alpha-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (format nil "~%~%(defun alpha (x)~%  (+ x 1))") stream))
      (with-open-file (stream beta-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (format nil "~%~%(defun beta (y)~%  (* y 2))") stream))
      (with-open-file (stream ignored-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string "not lisp" stream))
      (with-open-file (stream master-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line "{\"messages\":[{\"role\":\"system\",\"content\":\"seed\"}]}" stream))
      (unwind-protect
           (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                                  (declare (ignore bot))
                                                  nil))
                  (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
                  (*http-post-function*
                    (lambda (url &rest args)
                      (declare (ignore url args))
                      (values (make-string-input-stream (pop responses))
                              200)))
                  (examples (append-wild-lisp-files-to-training-examples wild-path
                                                                         master-file
                                                                         :progress-pathname progress-file))
                  (lines (cl-ppcre:split "\\r?\\n"
                                         (string-trim '(#\Return #\Linefeed)
                                                      (uiop:read-file-string master-file)))))
             (fiveam:is (= 2 (length examples)))
             (fiveam:is (= 3 (length lines)))
             (fiveam:is (search "\"seed\"" (first lines)))
             (fiveam:is (search "Prompt for alpha" (second lines)))
             (fiveam:is (search "Prompt for beta" (third lines))))
        (when (probe-file progress-file)
          (delete-file progress-file))
        (when (probe-file master-file)
          (delete-file master-file))
        (when (uiop:directory-exists-p source-dir)
          (uiop:delete-directory-tree source-dir :validate t))))))

(fiveam:test test-append-wild-lisp-files-to-training-examples-resumes-from-progress
  (let ((responses
          (list
           "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-2\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Prompt for beta\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-2\",\"model\":\"gemini-flash-latest\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")))
    (let* ((temp-dir (uiop:default-temporary-directory))
           (source-dir (merge-pathnames "append-wild-lisp-files-resume/" temp-dir))
           (alpha-file (merge-pathnames "alpha.lisp" source-dir))
           (beta-file (merge-pathnames "beta.lisp" source-dir))
           (wild-path (merge-pathnames "*.lisp" source-dir))
           (master-file (merge-pathnames "master-wild-training-resume.jsonl" temp-dir))
           (progress-file (merge-pathnames "master-wild-training-resume.progress.sexp" temp-dir))
           (alpha-form (format nil "(defun alpha (x)~%  (+ x 1))"))
           (alpha-example (format-training-example "Prompt for alpha" alpha-form)))
      (ensure-directories-exist alpha-file)
      (with-open-file (stream alpha-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (format nil "~%~%(defun alpha (x)~%  (+ x 1))") stream))
      (with-open-file (stream beta-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (format nil "~%~%(defun beta (y)~%  (* y 2))") stream))
      (with-open-file (stream master-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line "{\"messages\":[{\"role\":\"system\",\"content\":\"seed\"}]}" stream)
        (write-line alpha-example stream))
      (let ((committed-length (master-training-example-file-length master-file)))
        (with-open-file (stream master-file :direction :output :if-exists :append :if-does-not-exist :create)
          (write-line "CORRUPTED_PARTIAL_OUTPUT" stream))
        (write-training-progress-state
         progress-file
         (list :completed-files (list (training-progress-file-key alpha-file))
               :master-length committed-length)))
      (unwind-protect
           (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                                  (declare (ignore bot))
                                                  nil))
                  (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
                  (*http-post-function*
                    (lambda (url &rest args)
                      (declare (ignore url args))
                      (values (make-string-input-stream (pop responses))
                              200)))
                  (examples
                    (append-wild-lisp-files-to-training-examples
                     wild-path
                     master-file
                     :progress-pathname progress-file))
                  (lines (cl-ppcre:split "\\r?\\n"
                                         (string-trim '(#\Return #\Linefeed)
                                                      (uiop:read-file-string master-file)))))
             (fiveam:is (= 1 (length examples)))
             (fiveam:is (= 3 (length lines)))
             (fiveam:is (search "\"seed\"" (first lines)))
             (fiveam:is (search "Prompt for alpha" (second lines)))
             (fiveam:is (search "Prompt for beta" (third lines)))
             (fiveam:is-false (search "CORRUPTED_PARTIAL_OUTPUT"
                                      (uiop:read-file-string master-file))))
        (when (probe-file progress-file)
          (delete-file progress-file))
        (when (probe-file master-file)
          (delete-file master-file))
        (when (uiop:directory-exists-p source-dir)
          (uiop:delete-directory-tree source-dir :validate t))))))

(fiveam:test test-gemini-chat-retries-malformed-response-on-google-gemini-pro-latest
  (let ((conv (new-chat :backend :gemini :include-timestamp-p t :include-model-p t))
        (captured-urls nil)
        (captured-google-payload nil)
        (captured-gemini-payload nil)
        (call-count 0)
        (prompt-count 0))
    (let* ((*prompt-timestamp-function*
             (lambda ()
               (incf prompt-count)
               (cond
                 ((= prompt-count 1) "[08:46 seed]")
                 ((= prompt-count 2) "[08:46 gemini]")
                 (t "[08:46 retry]"))))
           (*get-all-mcp-tools-function* (lambda (bot)
                                          (declare (ignore bot))
                                          nil))
           (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
           (*http-post-function*
             (lambda (url &rest args)
               (incf call-count)
               (push url captured-urls)
               (if (search "/interactions?alt=sse" url)
                   (progn
                     (setf captured-gemini-payload (getf args :content))
                     (values
                      (make-string-input-stream
                       "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"stopReason\":\"MALFORMED_RESPONSE\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")
                      200))
                   (progn
                     (setf captured-google-payload (getf args :content))
                     (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Recovered from Gemini malformed response\"}], \"role\": \"model\"}}]}" 200))))))
      (let ((res (chat "Retry this" :conversation conv)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "Recovered from Gemini malformed response" res))
        (fiveam:is (search "/interactions?alt=sse" (second captured-urls)))
        (fiveam:is (search "/models/gemini-pro-latest:generateContent" (first captured-urls)))
        (let ((gemini-payload (decode-test-json captured-gemini-payload))
              (google-payload (decode-test-json captured-google-payload)))
          (assert-json-field= gemini-payload "input" "[08:46 gemini] [model: gemini-3.5-flash] Retry this")
          (assert-google-message-texts (first (google-payload-contents google-payload))
                                       "user"
                                       '("[08:46 retry] [model: gemini-pro-latest] Retry this"))
          (fiveam:is (notany (lambda (text)
                               (search "[model: gemini-3.5-flash] Retry this" text))
                             (google-payload-texts google-payload))))
        (fiveam:is (null (conversation-interaction-id conv)))
        (let ((stored-history (conversation-messages conv)))
          (assert-history-sequence stored-history
                                   '(("user" "Retry this")
                                     ("model" "Recovered from Gemini malformed response"))))))))

(fiveam:test test-gemini-chat-retries-empty-response-on-google-gemini-pro-latest
  (let ((conv (new-chat :backend :gemini))
        (captured-urls nil)
        (captured-google-payload nil)
        (call-count 0))
    (let* ((*get-all-mcp-tools-function* (lambda (bot)
                                          (declare (ignore bot))
                                          nil))
           (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
           (*http-post-function*
             (lambda (url &rest args)
               (incf call-count)
               (push url captured-urls)
               (if (search "/interactions?alt=sse" url)
                   (values
                    (make-string-input-stream
                     "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":0,\"total_tokens\":1}}}")
                    200)
                   (progn
                     (setf captured-google-payload (getf args :content))
                     (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Recovered from Gemini empty response\"}], \"role\": \"model\"}}]}" 200))))))
      (let ((res (chat "Retry empty" :conversation conv)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "Recovered from Gemini empty response" res))
        (fiveam:is (search "/interactions?alt=sse" (second captured-urls)))
        (fiveam:is (search "/models/gemini-pro-latest:generateContent" (first captured-urls)))
        (let ((google-payload (decode-test-json captured-google-payload)))
          (assert-google-message-texts (first (google-payload-contents google-payload))
                                       "user"
                                       '("Retry empty")))
        (fiveam:is (null (conversation-interaction-id conv)))
        (let ((stored-history (conversation-messages conv)))
          (assert-history-sequence stored-history
                                   '(("user" "Retry empty")
                                     ("model" "Recovered from Gemini empty response"))))))))

(fiveam:test test-gemini-tool-call-errors-are-reported-back-to-the-model
  (let ((conv (new-chat :backend :gemini))
        (captured-payloads nil)
        (call-count 0))
    (let* ((*get-all-mcp-tools-function*
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
              (declare (ignore server arguments))
              (error 'mcp-tool-execution-error
                     :tool-name tool-name
                     :reason "Mock tool failure")))
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
data: {\"event_type\":\"step.start\",\"step\":{\"id\":\"call-1\",\"type\":\"function_call\",\"name\":\"echo_tool\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"arguments\":\"{\\\"value\\\":\\\"payload\\\"}\"}}
data: {\"event_type\":\"step.stop\"}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
                    "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Handled tool error\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":2,\"total_output_tokens\":1,\"total_tokens\":3}}}"))
               200))))
      (fiveam:is (string= "Handled tool error" (chat "Run tool" :conversation conv)))
      (fiveam:is (= 2 (length captured-payloads)))
      (let* ((second-payload (decode-test-json (first captured-payloads)))
             (input (interaction-payload-input second-payload))
             (result-step (first input))
             (result-parts (test-json-elements (test-json-value-any result-step '("result" :result))))
             (result-text (decode-test-json
                           (test-json-value-any (first result-parts) '("text" :text)))))
        (assert-json-field= second-payload "previous_interaction_id" "session-1")
        (assert-json-field= result-step "type" "function_result")
        (assert-json-field= result-step "name" "echo_tool")
        (assert-json-field= result-text "type" "tool_error")
        (assert-json-field= result-text "toolName" "echo_tool")
        (assert-json-field= result-text "message" "Mock tool failure")))))

(fiveam:test test-gemini-tool-recursion-depth-is-capped
  (let ((conv (new-chat :backend :gemini))
        (call-count 0))
    (let* ((*get-all-mcp-tools-function*
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
             (declare (ignore server tool-name arguments))
             "loop result"))
          (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url args))
             (incf call-count)
             (values
              (make-string-input-stream
               "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.start\",\"step\":{\"id\":\"call-1\",\"type\":\"function_call\",\"name\":\"echo_tool\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"arguments\":\"{\\\"value\\\":\\\"payload\\\"}\"}}
data: {\"event_type\":\"step.stop\"}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")
              200))))
      (fiveam:signals chatbot-tool-recursion-limit-error
        (chat "Run tool loop" :conversation conv))
      (fiveam:is (= +max-chatbot-tool-recursion-depth+ call-count)))))

(fiveam:test test-tool-recursion-limit-error-is-continuable
  (let ((seen-condition nil))
    (fiveam:is
     (= (1+ +max-chatbot-tool-recursion-depth+)
       (handler-bind
           ((chatbot-tool-recursion-limit-error
              (lambda (condition)
                (setf seen-condition condition)
                (invoke-restart 'continue))))
         (next-chatbot-tool-recursion-depth :openai +max-chatbot-tool-recursion-depth+))))
    (fiveam:is (typep seen-condition 'chatbot-tool-recursion-limit-error))
    (fiveam:is (eq :openai
                  (chatbot-tool-recursion-limit-error-backend seen-condition)))
    (fiveam:is (= +max-chatbot-tool-recursion-depth+
                 (chatbot-tool-recursion-limit-error-depth seen-condition)))))

(fiveam:test test-gemini-built-in-no-arg-tool-accepts-empty-arguments
  (let ((conv (new-chat :backend :gemini))
        (captured-payloads nil)
        (call-count 0))
    (let* ((bot (conversation-chatbot conv))
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
data: {\"event_type\":\"step.start\",\"step\":{\"id\":\"call-1\",\"type\":\"function_call\",\"name\":\"readSystemInstructions\"}}
data: {\"event_type\":\"step.stop\"}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
                     "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Handled empty args\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":2,\"total_output_tokens\":1,\"total_tokens\":3}}}"))
                200))))
      (setf (chatbot-system-instruction bot) #("Directive one." "Directive two."))
      (setf (chatbot-system-instruction-path bot)
            #p"C:/Users/bitdi/.Personas/Test/system-instructions.md")
      (setf (chatbot-system-instruction-storage-kind bot) :markdown-file)
      (fiveam:is (string= "Handled empty args" (chat "Inspect directives" :conversation conv)))
      (fiveam:is (= 2 call-count))
      (let* ((second-payload (decode-test-json (first captured-payloads)))
             (input (interaction-payload-input second-payload))
             (result-step (first input))
             (result-parts (test-json-elements
                            (test-json-value-any result-step '("result" :result))))
             (result-text (decode-test-json
                           (test-json-value-any (first result-parts) '("text" :text)))))
        (assert-json-field= second-payload "previous_interaction_id" "session-1")
        (assert-json-field= result-step "type" "function_result")
        (assert-json-field= result-step "name" "readSystemInstructions")
        (fiveam:is (equal '("Directive one." "Directive two.")
                          (test-json-elements
                           (test-json-value-any result-text '("paragraphs" :paragraphs)))))))))

(fiveam:test test-text-formatting
  (let ((wrapped (wrap-text "This is a test of the line wrapping utility." :width 15)))
    (fiveam:is (every (lambda (line) (<= (length line) 15)) wrapped))
    (fiveam:is (string= "This is a test" (car wrapped))))
  (let ((wrapped (wrap-text "This is a test of the line wrapping utility."
                            :width 15
                            :initial-prefix "  ")))
    (fiveam:is (every (lambda (line) (<= (length line) 15)) wrapped))
    (fiveam:is (string= "  This is a" (car wrapped)))
    (fiveam:is (string= "test of the" (second wrapped))))
  (let ((output (with-output-to-string (s)
                  (format-paragraphs "Para one.

Para two." :width 40 :stream s))))
    (fiveam:is (search "  Para one." output))
    (fiveam:is (search (format nil "  Para one.~%~%  Para two." ) output)))
  (let* ((fenced-text "Before fence.

```java
public class Example {
    public static void main(String[] args) {}
}
```

After fence.")
         (output (with-output-to-string (s)
                   (format-paragraphs fenced-text :width 20 :stream s))))
    (fiveam:is (search "  Before fence." output))
    (fiveam:is (search "```java" output))
    (fiveam:is (search "public class Example {" output))
    (fiveam:is (search "    public static void main(String[] args) {}" output))
    (fiveam:is (search "}" output))
    (fiveam:is (search "```" output))
    (fiveam:is (search "  After fence." output))
    (fiveam:is-false (search "public class~%Example" output))
    (fiveam:is-false (search "static void~%main" output)))
  (let* ((unordered-bullets "- first bullet item that should stay on one line
- second bullet item")
         (output (with-output-to-string (s)
                   (format-paragraphs unordered-bullets :width 20 :stream s))))
    (fiveam:is (string= (format nil "- first bullet item that should stay on one line~%- second bullet item~%")
                        output))
    (fiveam:is-false (search "first bullet item~%that should stay" output)))
  (let* ((ordered-bullets "1. first ordered item
2. second ordered item")
         (output (with-output-to-string (s)
                   (format-paragraphs ordered-bullets :width 15 :stream s))))
    (fiveam:is (string= (format nil "1. first ordered item~%2. second ordered item~%")
                        output))
    (fiveam:is-false (search "first ordered~%item" output)))
  (let* ((indented-bullets "  * indented bullet entry
    continuation text that should also stay verbatim
  * another bullet")
         (output (with-output-to-string (s)
                   (format-paragraphs indented-bullets :width 18 :stream s))))
    (fiveam:is (string= (format nil "  * indented bullet entry~%    continuation text that should also stay verbatim~%  * another bullet~%")
                        output))
    (fiveam:is-false (search "continuation text~%that should also stay" output))))

(fiveam:test test-log-message-level-filtering
  (let ((output (with-output-to-string (s)
                  (let ((context (make-runtime-context :logging-enabled-p t
                                                       :log-level :warn
                                                       :log-stream s)))
                    (call-with-runtime-context
                     context
                     (lambda ()
                       (log-message :info "skip me")
                       (log-message :error "keep me")))))))
    (fiveam:is (null (search "skip me" output)))
    (fiveam:is (search "keep me" output))))

(fiveam:test test-call-with-logging-settings-scopes-log-level
  (let ((context (make-runtime-context :logging-enabled-p t
                                       :log-level :info)))
    (call-with-runtime-context
     context
     (lambda ()
       (fiveam:is (eq :info (current-log-level)))
       (fiveam:is-true (log-level-enabled-p :info))
       (call-with-logging-settings
        (lambda ()
          (fiveam:is (eq :warn (current-log-level)))
          (fiveam:is-false (log-level-enabled-p :info))
          (fiveam:is-true (log-level-enabled-p :warn)))
        :log-level :warn)
       (fiveam:is (eq :info (current-log-level)))
       (fiveam:is-true (log-level-enabled-p :info))))))

(fiveam:test test-log-backend-response-stats
  (let ((output (with-output-to-string (s)
                  (let ((context (make-runtime-context :logging-enabled-p t
                                                       :log-level :info
                                                       :log-stream s)))
                    (call-with-runtime-context
                     context
                     (lambda ()
                       (log-backend-response-stats
                        :google
                        :http-status 200
                        :response-id "resp-123"
                        :model "gemini-3.5-flash"
                        :finish-reason "STOP"
                        :usage '((:prompt-token-count . 12)
                                 (:cached-content-token-count . 5)
                                 (:candidates-token-count . 7)
                                 (:thoughts-token-count . 3)
                                 (:total-token-count . 22)))))))))
    (fiveam:is (search "Backend response stats" output))
    (fiveam:is (search "backend: google" output))
    (fiveam:is (search "http-status: 200" output))
    (fiveam:is (search "response-id: resp-123" output))
    (fiveam:is (search "prompt-tokens: 12" output))
    (fiveam:is (search "cached-tokens: 5" output))
    (fiveam:is (search "completion-tokens: 7" output))
    (fiveam:is (search "thought-tokens: 3" output))
    (fiveam:is (search "total-tokens: 22" output))))

(fiveam:test test-write-turn-token-summary-supports-interactions-usage-keys
  (reset-global-token-grand-totals)
  (let ((output (with-output-to-string (s)
                  (write-turn-token-summary
                   '(("total_input_tokens" . 12)
                     ("total_cached_tokens" . 5)
                     ("total_output_tokens" . 7)
                     ("total_thought_tokens" . 3)
                     ("total_tokens" . 22))
                   :stream s))))
    (fiveam:is (search "[Tokens] prompt: 12" output))
    (fiveam:is (search "cached: 5" output))
    (fiveam:is (search "completion: 7" output))
    (fiveam:is (search "thought: 3" output))
    (fiveam:is (search "total: 22" output))
    (fiveam:is (search "[Tokens Total] prompt: 12 completion: 7 thought: 3 cached: 5 total: 22" output))))

(fiveam:test test-write-turn-token-summary-uses-embedded-thought-text
  (reset-global-token-grand-totals)
  (let ((output (with-output-to-string (s)
                  (write-turn-token-summary
                   '(("total_input_tokens" . 12)
                     ("total_output_tokens" . 7)
                     ("total_thought_tokens" . 3)
                     ("total_tokens" . 22)
                     ("thoughtText" . "Visible thought"))
                   :stream s))))
    (fiveam:is (search "[Thoughts]" output))
    (fiveam:is (search "Visible thought" output))))

(fiveam:test test-write-turn-token-summary-accumulates-process-wide-grand-totals
  (reset-global-token-grand-totals)
  (let ((first-output (with-output-to-string (s)
                        (write-turn-token-summary
                         '((:prompt-token-count . 12)
                          (:cached-content-token-count . 5)
                          (:candidates-token-count . 7)
                          (:thoughts-token-count . 3)
                          (:total-token-count . 22))
                         :stream s)))
        (second-output (with-output-to-string (s)
                         (write-turn-token-summary
                          '(("total_input_tokens" . 5)
                            ("total_cached_tokens" . 2)
                            ("total_output_tokens" . 2)
                            ("total_thought_tokens" . 1))
                          :stream s))))
    (fiveam:is (search "[Tokens Total] prompt: 12 completion: 7 thought: 3 cached: 5 total: 22" first-output))
    (fiveam:is (search "[Tokens] prompt: 5 completion: 2 thought: 1 cached: 2 total: 8" second-output))
    (fiveam:is (search "[Tokens Total] prompt: 17 completion: 9 thought: 4 cached: 7 total: 30" second-output))
    (fiveam:is (equal '(:prompt 17 :completion 9 :thought 4 :cached 7 :total 30)
                      (current-global-token-grand-totals)))))

(fiveam:test test-write-turn-token-summary-upgrades-legacy-grand-total-state
  (let ((*global-token-grand-totals* (list :prompt 12
                                           :completion 7
                                           :thought 3
                                           :total 22)))
    (let ((output (with-output-to-string (s)
                    (write-turn-token-summary
                     '(("total_input_tokens" . 5)
                       ("total_cached_tokens" . 2)
                       ("total_output_tokens" . 2)
                       ("total_thought_tokens" . 1))
                     :stream s))))
      (fiveam:is (search "[Tokens] prompt: 5 completion: 2 thought: 1 cached: 2 total: 8" output))
      (fiveam:is (search "[Tokens Total] prompt: 17 completion: 9 thought: 4 cached: 2 total: 30" output))
      (fiveam:is (equal '(:prompt 17 :completion 9 :thought 4 :cached 2 :total 30)
                        (current-global-token-grand-totals))))))

(fiveam:test test-accumulate-global-token-grand-totals-is-thread-safe
  (reset-global-token-grand-totals)
  (let* ((thread-count 8)
         (iterations-per-thread 500)
         (usage '((:prompt-token-count . 2)
                  (:candidates-token-count . 3)
                  (:thoughts-token-count . 1)
                  (:total-token-count . 6)))
         (threads
           (loop repeat thread-count
                 collect
                 (sb-thread:make-thread
                  (lambda ()
                    (loop repeat iterations-per-thread
                          do (accumulate-global-token-grand-totals usage)))))))
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (fiveam:is (equal (list :prompt (* thread-count iterations-per-thread 2)
                            :completion (* thread-count iterations-per-thread 3)
                            :thought (* thread-count iterations-per-thread 1)
                            :cached 0
                            :total (* thread-count iterations-per-thread 6))
                      (current-global-token-grand-totals)))))

(fiveam:test test-post-web-request-logging-redacts-secrets
  (let ((captured-url nil)
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
         (let ((log-output
                 (with-output-to-string (s)
                   (let ((context (make-runtime-context :logging-enabled-p t
                                                       :log-level :info
                                                       :log-stream s
                                                       :http-post-function *http-post-function*
                                                       :http-connect-timeout 15
                                                       :http-read-timeout 120)))
                     (call-with-runtime-context
                      context
                      (lambda ()
                        (post-web-request "https://example.com/test?key=secret-value"
                                         '(("Authorization" . "Bearer secret-token")
                                           ("Content-Type" . "application/json"))
                                         "{\"hello\":\"world\"}")))))))
           (fiveam:is (string= "https://example.com/test?key=secret-value" captured-url))
           (fiveam:is (equal '(("Authorization" . "Bearer secret-token")
                              ("Content-Type" . "application/json"))
                             captured-headers))
           (fiveam:is (string= "{\"hello\":\"world\"}" captured-content))
           (fiveam:is (= 15 captured-connect-timeout))
           (fiveam:is (= 120 captured-read-timeout))
           (fiveam:is (search "HTTP POST request" log-output))
           (fiveam:is (search "https://example.com/test?key=[REDACTED]" log-output))
           (fiveam:is-false (search "\"Authorization\":\"[REDACTED]\"" log-output))
           (fiveam:is-false (search "headers:" log-output))
           (fiveam:is-false (search "connect-timeout: 15" log-output))
           (fiveam:is-false (search "read-timeout: 120" log-output))
           (fiveam:is-false (search "want-stream:" log-output))
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

(fiveam:test test-chat-backend-registry
  (fiveam:is (eq 'equal (hash-table-test *chat-backends*)))
  (dolist (backend '(:gemini :google :openai :lm-studio))
    (fiveam:is-true (gethash backend *chat-backends*)))
  (when (asdf:find-system "llambda/chatbot" nil)
    (fiveam:is-true (gethash :llambda *chat-backends*))))

(fiveam:test test-registered-chat-backend-receives-turn-arguments
  (let* ((*chat-backends* (make-hash-table :test 'equal))
         (conversation (new-chat :backend "custom_backend" :model "custom-model"))
         (bot (conversation-chatbot conversation))
         (captured nil)
         (handler
           (lambda (input &key bot conversation callback file-attachments
                               effective-model effective-generation-config)
             (setf captured
                   (list input bot conversation callback file-attachments
                         effective-model effective-generation-config))
             (make-chat-turn-result "custom response"))))
    (register-chat-backend :custom-backend handler)
    (fiveam:is
     (string= "custom response"
              (chat-turn-result-text
               (invoke-chat-backend-turn
                bot conversation "custom prompt" #'identity
                :file-attachments '(:attachment)
                :effective-model "turn-model"
                :effective-generation-config '(:temperature 0.25)))))
    (fiveam:is (equal "custom prompt" (first captured)))
    (fiveam:is (eq bot (second captured)))
    (fiveam:is (eq conversation (third captured)))
    (fiveam:is (eq #'identity (fourth captured)))
    (fiveam:is (equal '(:attachment) (fifth captured)))
    (fiveam:is (string= "turn-model" (sixth captured)))
    (fiveam:is (equal '(:temperature 0.25) (seventh captured)))))

(fiveam:test test-unregistered-chat-backend-signals-clean-error
  (let* ((*chat-backends* (make-hash-table :test 'equal))
         (conversation (new-chat :backend :missing :model "missing-model")))
    (fiveam:signals error
      (invoke-chat-backend-turn
       (conversation-chatbot conversation)
       conversation
       "prompt"
       nil))))

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

(fiveam:test test-gemini-chat-ignores-done-sentinel
  (let ((conv (new-chat :backend :gemini)))
    (let ((*get-all-mcp-tools-function* (lambda (bot)
                                         (declare (ignore bot))
                                         nil))
         (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url args))
             (values
              (make-string-input-stream
               "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Hello done\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}
data: [DONE]")
              200))))
      (fiveam:is (string= "Hello done" (chat "Hi done" :conversation conv))))))

(fiveam:test test-new-chat-reuses-startup-mcp-servers
  (let ((original-startup-chatbot *startup-chatbot*)
        (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*)))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore strict-required-p))
             (setf (chatbot-mcp-servers bot) '(:shared-server))
             (setf (chatbot-mcp-startup-status bot)
                   (make-instance 'mcp-startup-status
                                  :configured-count 1
                                  :successful-count 1))
             bot)))
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (initialize-startup-chatbot)
      (let* ((conv (new-chat))
             (bot (conversation-chatbot conv))
             (startup-bot (ensure-startup-chatbot)))
        (fiveam:is-true (startup-chatbot-initialized-p))
        (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
        (fiveam:is (eq startup-bot
                       (runtime-context-startup-chatbot *default-runtime-context*)))
        (fiveam:is (eq (chatbot-mcp-servers bot)
                       (chatbot-mcp-servers startup-bot))))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot))))

(fiveam:test test-initialize-startup-chatbot-reuses-existing-shared-bot
  (let* ((context (make-runtime-context))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
          (lambda (bot &key strict-required-p)
            (declare (ignore strict-required-p))
            (incf init-calls)
            (setf (chatbot-mcp-servers bot) '(:shared-server))
            (setf (chatbot-mcp-startup-status bot)
                  (make-instance 'mcp-startup-status
                                 :entries nil
                                 :configured-count 1
                                 :successful-count 1
                                 :failed-count 0
                                 :required-failed-count 0))
            bot)))
      (let ((first-bot (initialize-startup-chatbot context))
           (second-bot (initialize-startup-chatbot context)))
       (fiveam:is (= 1 init-calls))
       (fiveam:is (eq first-bot second-bot))
       (fiveam:is (eq first-bot (current-startup-chatbot context))))
      (setf (runtime-context-startup-chatbot context) nil))))

(fiveam:test test-startup-chatbot-initialized-p-requires-ready-startup-state
  (let* ((context (make-runtime-context))
        (statusless-bot (make-instance 'chatbot :runtime-context context))
        (server-only-bot (make-instance 'chatbot
                                        :runtime-context context
                                        :mcp-servers '(:shared-server)))
        (ready-bot (make-instance 'chatbot
                                  :runtime-context context
                                  :mcp-startup-status (make-instance 'mcp-startup-status
                                                                     :entries nil
                                                                     :configured-count 0
                                                                     :successful-count 0
                                                                     :failed-count 0
                                                                     :required-failed-count 0))))
    (setf (runtime-context-startup-chatbot context) statusless-bot)
    (fiveam:is-false (startup-chatbot-initialized-p context))
    (setf (runtime-context-startup-chatbot context) server-only-bot)
    (fiveam:is-false (startup-chatbot-initialized-p context))
    (setf (runtime-context-startup-chatbot context) ready-bot)
    (fiveam:is-true (startup-chatbot-initialized-p context))
    (setf (runtime-context-startup-chatbot context) nil)))

(fiveam:test test-initialize-startup-chatbot-initializes-statusless-shared-bot-once
  (let* ((context (make-runtime-context))
        (init-calls 0)
        (existing-bot (make-instance 'chatbot :runtime-context context)))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore strict-required-p))
             (incf init-calls)
             (setf (chatbot-mcp-servers bot) '(:shared-server))
             (setf (chatbot-mcp-startup-status bot)
                   (make-instance 'mcp-startup-status
                                  :entries nil
                                  :configured-count 0
                                  :successful-count 0
                                  :failed-count 0
                                  :required-failed-count 0))
             bot)))
      (setf (runtime-context-startup-chatbot context) existing-bot)
      (fiveam:is-false (startup-chatbot-initialized-p context))
      (let ((first-bot (initialize-startup-chatbot context))
           (second-bot (initialize-startup-chatbot context)))
        (fiveam:is (= 1 init-calls))
        (fiveam:is (eq existing-bot first-bot))
        (fiveam:is (eq first-bot second-bot))
        (fiveam:is (eq first-bot (current-startup-chatbot context)))
        (fiveam:is-true (startup-chatbot-initialized-p context)))
      (setf (runtime-context-startup-chatbot context) nil))))

(fiveam:test test-initialize-startup-chatbot-reinitializes-server-only-shared-bot
  (let* ((context (make-runtime-context))
        (init-calls 0)
        (existing-bot (make-instance 'chatbot
                                     :runtime-context context
                                     :mcp-servers '(:stale-shared-server))))
    (let ((*initialize-mcp-servers-for-chatbot-function*
          (lambda (bot &key strict-required-p)
            (declare (ignore strict-required-p))
            (incf init-calls)
            (setf (chatbot-mcp-servers bot) '(:shared-server))
            (setf (chatbot-mcp-startup-status bot)
                  (make-instance 'mcp-startup-status
                                 :entries nil
                                 :configured-count 1
                                 :successful-count 1
                                 :failed-count 0
                                 :required-failed-count 0))
            bot)))
      (setf (runtime-context-startup-chatbot context) existing-bot)
      (fiveam:is-false (startup-chatbot-initialized-p context))
      (let ((first-bot (initialize-startup-chatbot context))
           (second-bot (initialize-startup-chatbot context)))
       (fiveam:is (= 1 init-calls))
       (fiveam:is (eq existing-bot first-bot))
       (fiveam:is (eq first-bot second-bot))
       (fiveam:is (equal '(:shared-server) (chatbot-mcp-servers first-bot)))
       (fiveam:is-true (startup-chatbot-initialized-p context)))
      (setf (runtime-context-startup-chatbot context) nil))))

(fiveam:test test-startup-chatbot-mcp-servers-hides-incomplete-server-only-shared-bot
  (let* ((context (make-runtime-context))
        (server-only-bot (make-instance 'chatbot
                                        :runtime-context context
                                        :mcp-servers '(:stale-shared-server)))
        (ready-bot (make-instance 'chatbot
                                  :runtime-context context
                                  :mcp-servers '(:shared-server)
                                  :mcp-startup-status (make-instance 'mcp-startup-status
                                                                     :entries nil
                                                                     :configured-count 1
                                                                     :successful-count 1
                                                                     :failed-count 0
                                                                     :required-failed-count 0))))
    (setf (runtime-context-startup-chatbot context) server-only-bot)
    (fiveam:is (null (startup-chatbot-mcp-servers context)))
    (setf (runtime-context-startup-chatbot context) ready-bot)
    (fiveam:is (equal '(:shared-server) (startup-chatbot-mcp-servers context)))
    (setf (runtime-context-startup-chatbot context) nil)))

(fiveam:test test-explicit-runtime-context-isolates-startup-mcp-servers
  (let* ((context-a (make-runtime-context))
        (context-b (make-runtime-context))
        (original-default-startup-chatbot
          (runtime-context-startup-chatbot *default-runtime-context*)))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore strict-required-p))
             (setf (chatbot-mcp-servers bot)
                   (list (if (eq (chatbot-runtime-context bot) context-a)
                             :context-a-server
                             :context-b-server)))
             (setf (chatbot-mcp-startup-status bot)
                   (make-instance 'mcp-startup-status
                                  :entries nil
                                  :configured-count 1
                                  :successful-count 1
                                  :failed-count 0
                                  :required-failed-count 0))
             bot)))
      (unwind-protect
          (progn
            (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
            (initialize-startup-chatbot context-a)
            (initialize-startup-chatbot context-b)
            (let* ((conv-a (new-chat :runtime-context context-a))
                   (conv-b (new-chat :runtime-context context-b))
                   (conv-default (new-chat)))
              (fiveam:is (equal '(:context-a-server)
                                (chatbot-mcp-servers (conversation-chatbot conv-a))))
              (fiveam:is (equal '(:context-b-server)
                                (chatbot-mcp-servers (conversation-chatbot conv-b))))
              (fiveam:is (null (chatbot-mcp-servers (conversation-chatbot conv-default))))))
        (setf (runtime-context-startup-chatbot *default-runtime-context*)
             original-default-startup-chatbot)))))

(fiveam:test test-new-chat-does-not-inherit-incomplete-server-only-shared-startup-state
  (let* ((context (make-runtime-context))
         (startup-bot (make-instance 'chatbot
                                     :runtime-context context
                                     :mcp-servers '(:stale-shared-server))))
    (setf (runtime-context-startup-chatbot context) startup-bot)
    (unwind-protect
         (let* ((conv (new-chat :runtime-context context))
                (bot (conversation-chatbot conv)))
           (fiveam:is (null (chatbot-mcp-servers bot)))
           (fiveam:is-false (chatbot-mcp-startup-status bot)))
      (setf (runtime-context-startup-chatbot context) nil))))

(fiveam:test test-new-chat-does-not-start-mcp-servers
  (let ((original-startup-chatbot *startup-chatbot*)
        (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore bot strict-required-p))
             (incf init-calls))))
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (let* ((conv (new-chat))
             (bot (conversation-chatbot conv)))
        (fiveam:is (= 0 init-calls))
        (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
        (fiveam:is (null (chatbot-mcp-servers bot))))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot))))

(fiveam:test test-new-chat-auto-initializes-startup-mcp-servers-when-enabled
  (let* ((context (make-runtime-context :auto-initialize-startup-mcp-servers-p t))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
          (lambda (bot &key strict-required-p)
            (declare (ignore strict-required-p))
            (incf init-calls)
            (setf (chatbot-mcp-servers bot) '(:shared-server))
            (setf (chatbot-mcp-startup-status bot)
                  (make-instance 'mcp-startup-status
                                 :configured-count 1
                                 :successful-count 1))
            bot)))
      (let* ((conv (new-chat :runtime-context context))
            (bot (conversation-chatbot conv)))
       (fiveam:is (= 1 init-calls))
       (fiveam:is-true (startup-chatbot-initialized-p context))
       (fiveam:is (eq (current-startup-chatbot context)
                      (runtime-context-startup-chatbot context)))
       (fiveam:is (eq (chatbot-mcp-servers bot)
                      (chatbot-mcp-servers (current-startup-chatbot context))))))))

(fiveam:test test-new-chat-inherits-shared-startup-status-with-zero-servers
  (let* ((context (make-runtime-context :auto-initialize-startup-mcp-servers-p t))
        (startup-status (make-instance 'mcp-startup-status
                                       :entries nil
                                       :configured-count 0
                                       :successful-count 0
                                       :failed-count 0
                                       :required-failed-count 0))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
          (lambda (bot &key strict-required-p)
            (declare (ignore strict-required-p))
            (incf init-calls)
            (setf (chatbot-mcp-servers bot) nil)
            (setf (chatbot-mcp-startup-status bot) startup-status)
            bot)))
      (let* ((conv (new-chat :runtime-context context))
            (bot (conversation-chatbot conv)))
       (fiveam:is (= 1 init-calls))
       (fiveam:is-true (startup-chatbot-initialized-p context))
       (fiveam:is (null (chatbot-mcp-servers bot)))
       (fiveam:is (eq startup-status (chatbot-mcp-startup-status bot)))))))

(fiveam:test test-new-chat-inherits-shared-startup-status-without-successful-servers
  (let* ((context (make-runtime-context :auto-initialize-startup-mcp-servers-p t))
        (startup-status (make-instance 'mcp-startup-status
                                       :entries nil
                                       :configured-count 1
                                       :successful-count 0
                                       :failed-count 1
                                       :required-failed-count 0))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore strict-required-p))
             (incf init-calls)
             (setf (chatbot-mcp-servers bot) nil)
             (setf (chatbot-mcp-startup-status bot) startup-status)
             bot)))
      (let* ((conv (new-chat :runtime-context context))
            (bot (conversation-chatbot conv))
            (status (chatbot-mcp-startup-status bot)))
       (fiveam:is (= 1 init-calls))
       (fiveam:is-true (startup-chatbot-initialized-p context))
       (fiveam:is (null (chatbot-mcp-servers bot)))
       (fiveam:is (eq startup-status status))
       (fiveam:is (= 1 (mcp-startup-status-failed-count status)))))))

(fiveam:test test-auto-startup-chatbot-defaults-to-noop
  (let ((original-startup-chatbot *startup-chatbot*)
       (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*))
       (original-auto-init *auto-initialize-startup-mcp-servers-p*)
        (original-context-auto-init
         (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore bot strict-required-p))
             (incf init-calls))))
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*) nil)
      (maybe-auto-initialize-startup-chatbot)
      (fiveam:is (= 0 init-calls))
      (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
      (fiveam:is (eql original-auto-init *auto-initialize-startup-mcp-servers-p*))
      (fiveam:is-false (startup-chatbot-initialized-p))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot)
      (setf *auto-initialize-startup-mcp-servers-p* original-auto-init)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*)
            original-context-auto-init))))

(fiveam:test test-auto-startup-chatbot-honors-compatibility-flag
  (let ((original-startup-chatbot *startup-chatbot*)
        (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*))
        (original-auto-init *auto-initialize-startup-mcp-servers-p*)
        (original-context-auto-init
         (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*)))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore strict-required-p))
             (setf (chatbot-mcp-servers bot) '(:shared-server))
             (setf (chatbot-mcp-startup-status bot)
                   (make-instance 'mcp-startup-status
                                  :configured-count 1
                                  :successful-count 1))
             bot)))
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*) t)
      (maybe-auto-initialize-startup-chatbot)
      (fiveam:is-true (startup-chatbot-initialized-p))
      (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
      (fiveam:is (eql original-auto-init *auto-initialize-startup-mcp-servers-p*))
      (fiveam:is (equal '(:shared-server)
                       (chatbot-mcp-servers
                        (runtime-context-startup-chatbot *default-runtime-context*))))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot)
      (setf *auto-initialize-startup-mcp-servers-p* original-auto-init)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*)
            original-context-auto-init))))

(fiveam:test test-initialize-startup-chatbot-on-load-initializes-shared-startup-chatbot
  (let ((original-startup-chatbot *startup-chatbot*)
        (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*))
        (original-auto-init *auto-initialize-startup-mcp-servers-p*)
        (original-context-auto-init
         (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*))
        (init-calls 0))
    (let ((*initialize-mcp-servers-for-chatbot-function*
           (lambda (bot &key strict-required-p)
             (declare (ignore strict-required-p))
             (incf init-calls)
             (setf (chatbot-mcp-servers bot) '(:shared-server))
             (setf (chatbot-mcp-startup-status bot)
                   (make-instance 'mcp-startup-status
                                  :entries nil
                                  :configured-count 0
                                  :successful-count 0
                                  :failed-count 0
                                  :required-failed-count 0))
             bot)))
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*) nil)
      (initialize-startup-chatbot-on-load)
      (fiveam:is (= 1 init-calls))
      (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
      (fiveam:is (eql original-auto-init *auto-initialize-startup-mcp-servers-p*))
      (fiveam:is-true (startup-chatbot-initialized-p))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot)
      (setf *auto-initialize-startup-mcp-servers-p* original-auto-init)
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p *default-runtime-context*)
            original-context-auto-init))))

(fiveam:test test-startup-chatbot-exposes-partial-startup-status
  (let ((original-startup-chatbot *startup-chatbot*)
        (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*)))
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
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (initialize-startup-chatbot)
      (let ((status (startup-chatbot-mcp-status)))
        (fiveam:is-true (startup-chatbot-initialized-p))
        (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
        (fiveam:is (typep status 'mcp-startup-status))
        (fiveam:is-true (mcp-startup-status-partial-failure-p status))
        (fiveam:is (= 1 (mcp-startup-status-failed-count status))))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot))))

(fiveam:test test-startup-chatbot-does-not-initialize-on-strict-required-failure
  (let ((original-startup-chatbot *startup-chatbot*)
        (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*)))
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
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (fiveam:signals mcp-startup-error
        (initialize-startup-chatbot nil :strict-required-p t))
      (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
      (fiveam:is-false (startup-chatbot-initialized-p))
      (fiveam:is-false (startup-chatbot-mcp-status))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot))))

(fiveam:test test-startup-chatbot-strict-required-failure-cleans-up-started-servers
  (let ((original-startup-chatbot *startup-chatbot*)
       (original-context-startup-chatbot (runtime-context-startup-chatbot *default-runtime-context*))
       (stopped nil))
    (let ((*read-mcp-config-function*
          (lambda ()
            '((:name "healthy-server"
               :command "sbcl"
               :args ("--script" "healthy.lisp"))
              (:name "required-failure"
               :command "sbcl"
               :args ("--script" "broken.lisp")
               :required t))))
         (*start-mcp-server-function*
          (lambda (name command args)
            (declare (ignore command args))
            (if (string= name "required-failure")
                (error "launch failed")
                (make-instance 'mcp-server :name name))))
         (*mcp-initialize-function* (lambda (server) server))
         (*stop-mcp-server-function*
          (lambda (server)
            (push (mcp-server-name server) stopped))))
      (setf (runtime-context-startup-chatbot *default-runtime-context*) nil)
      (fiveam:signals mcp-startup-error
       (initialize-startup-chatbot nil :strict-required-p t))
      (fiveam:is (equal '("healthy-server") stopped))
      (fiveam:is (eq original-startup-chatbot *startup-chatbot*))
      (fiveam:is-false (startup-chatbot-initialized-p))
      (fiveam:is-false (startup-chatbot-mcp-status))
      (setf *startup-chatbot* original-startup-chatbot)
      (setf (runtime-context-startup-chatbot *default-runtime-context*) original-context-startup-chatbot))))

(fiveam:test test-startup-chatbot-strict-mode-reinitializes-incompatible-shared-bot
  (let* ((context (make-runtime-context))
        (existing-bot (make-instance 'chatbot
                                     :runtime-context context
                                     :mcp-startup-status (make-instance 'mcp-startup-status
                                                                        :entries nil
                                                                        :strict-required-p nil
                                                                        :configured-count 1
                                                                        :successful-count 0
                                                                        :failed-count 1
                                                                        :required-failed-count 1)))
        (init-calls 0)
        (stopped nil))
    (let ((*initialize-mcp-servers-for-chatbot-function*
          (lambda (bot &key strict-required-p)
            (declare (ignore strict-required-p))
            (incf init-calls)
            (setf (chatbot-mcp-servers bot)
                  (list (make-instance 'mcp-server :name "healthy-server")))
            (setf (chatbot-mcp-startup-status bot)
                  (make-instance 'mcp-startup-status
                                 :entries nil
                                 :strict-required-p t
                                 :configured-count 2
                                 :successful-count 1
                                 :failed-count 1
                                 :required-failed-count 1))
            (error 'mcp-startup-error :status (chatbot-mcp-startup-status bot))))
         (*stop-mcp-server-function*
          (lambda (server)
            (push (mcp-server-name server) stopped))))
      (setf (runtime-context-startup-chatbot context) existing-bot)
      (unwind-protect
          (progn
            (fiveam:signals mcp-startup-error
              (initialize-startup-chatbot context :strict-required-p t))
            (fiveam:is (= 1 init-calls))
            (fiveam:is (equal '("healthy-server") stopped))
            (fiveam:is-false (current-startup-chatbot context))
            (fiveam:is-false (chatbot-mcp-startup-status existing-bot))
            (fiveam:is (null (chatbot-mcp-servers existing-bot))))
       (setf (runtime-context-startup-chatbot context) nil)))))

(fiveam:test test-shutdown-chatbot-preserves-shared-startup-servers
  (let* ((context (make-runtime-context))
        (shared-servers (list :shared-server))
        (startup-status (make-instance 'mcp-startup-status
                                       :entries nil
                                       :configured-count 1
                                       :successful-count 1
                                       :failed-count 0
                                       :required-failed-count 0))
         (startup-bot (make-instance 'chatbot
                                     :mcp-servers shared-servers
                                     :mcp-startup-status startup-status
                                     :runtime-context context))
         (bot (make-instance 'chatbot
                             :mcp-servers shared-servers
                             :mcp-startup-status startup-status
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
             (fiveam:is (null (chatbot-mcp-servers bot)))
             (fiveam:is-false (chatbot-mcp-startup-status bot))
             (shutdown-chatbot startup-bot context)
             (fiveam:is (equal shared-servers stopped))
             (fiveam:is-false (chatbot-mcp-startup-status startup-bot))
             (fiveam:is (null (runtime-context-startup-chatbot context))))
        (setf (runtime-context-startup-chatbot context) nil)))))

(fiveam:test test-initialize-startup-chatbot-reinitializes-shutdown-shared-bot
  (let* ((context (make-runtime-context))
         (init-calls 0)
         (startup-bot (make-instance 'chatbot :runtime-context context)))
    (let ((*initialize-mcp-servers-for-chatbot-function*
            (lambda (bot &key strict-required-p)
              (declare (ignore strict-required-p))
              (incf init-calls)
              (setf (chatbot-mcp-servers bot) '(:shared-server))
              (setf (chatbot-mcp-startup-status bot)
                    (make-instance 'mcp-startup-status
                                   :entries nil
                                   :configured-count 1
                                   :successful-count 1
                                   :failed-count 0
                                   :required-failed-count 0))
              bot))
          (*stop-mcp-server-function* (lambda (server)
                                        (declare (ignore server))
                                        nil)))
      (setf (runtime-context-startup-chatbot context) startup-bot)
      (unwind-protect
           (progn
             (initialize-startup-chatbot context)
             (fiveam:is (= 1 init-calls))
             (shutdown-chatbot startup-bot context)
             (fiveam:is-false (chatbot-mcp-startup-status startup-bot))
             (fiveam:is (null (runtime-context-startup-chatbot context)))
             ;; Simulate later reuse of the same chatbot object as the shared startup slot.
             (setf (runtime-context-startup-chatbot context) startup-bot)
             (initialize-startup-chatbot context)
             (fiveam:is (= 2 init-calls))
             (fiveam:is (eq startup-bot (current-startup-chatbot context))))
        (setf (runtime-context-startup-chatbot context) nil)))))

(fiveam:test test-startup-chatbot-shared-servers-p-detects-shared-and-owned-server-sets
  (let* ((context (make-runtime-context))
         (shared-servers (list :shared-server))
         (startup-bot (make-instance 'chatbot
                                    :mcp-servers shared-servers
                                    :runtime-context context))
         (shared-bot (make-instance 'chatbot
                                   :mcp-servers shared-servers
                                   :runtime-context context))
         (owned-bot (make-instance 'chatbot
                                  :mcp-servers (list :owned-server)
                                  :runtime-context context)))
    (setf (runtime-context-startup-chatbot context) startup-bot)
    (unwind-protect
         (progn
           (fiveam:is-true (startup-chatbot-shared-servers-p shared-bot context))
           (fiveam:is-false (startup-chatbot-shared-servers-p owned-bot context))
           (fiveam:is-false (startup-chatbot-shared-servers-p startup-bot context)))
      (setf (runtime-context-startup-chatbot context) nil))))

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

(fiveam:test test-http-backoff-retry-on-transient-errors
  (let ((post-call-count 0)
        (get-call-count 0))
    ;; 1. Test POST request with transient errors that eventually succeeds on 3rd attempt
    (let* ((post-function
             (lambda (url &rest args)
               (declare (ignore url args))
               (incf post-call-count)
               (cond
                 ((<= post-call-count 2)
                  (error "Transient Network timeout (12002)."))
                 (t
                  (values "Success" 200)))))
           (context (make-runtime-context :http-post-function post-function)))
      (call-with-runtime-context
       context
       (lambda ()
         (let ((res (post-web-request "https://api.test/post" nil "payload")))
           (fiveam:is (string= "Success" res))
           (fiveam:is (= 3 post-call-count))))))
    
    ;; 2. Test GET request with non-retryable 404 client error that fails immediately on 1st attempt
    (let* ((get-function
             (lambda (url &rest args)
               (declare (ignore url args))
               (incf get-call-count)
               (error "HTTP 404 Not Found")))
           (context (make-runtime-context :http-get-function get-function)))
      (call-with-runtime-context
       context
       (lambda ()
         (fiveam:signals error
           (get-web-request "https://api.test/get"))
         (fiveam:is (= 1 get-call-count)))))))
