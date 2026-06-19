;;; tests.lisp

(in-package "CHATBOT")

(fiveam:def-suite chatbot-suite :description "Chatbot framework test suite")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-payload-generation
  (let ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :system-instruction "Be helpful"
                            :google-search-p t
                            :code-execution-p nil)))
    (let ((payload (make-interaction-payload bot "Hello" :previous-interaction-id "session-123" :stream t)))
      (fiveam:is (string= "gemini-3.5-flash" (cdr (assoc "model" payload :test #'string=))))
      (fiveam:is (string= "Hello" (cdr (assoc "input" payload :test #'string=))))
      (fiveam:is (eq t (cdr (assoc "stream" payload :test #'string=))))
      (fiveam:is (string= "session-123" (cdr (assoc "previous_interaction_id" payload :test #'string=))))
      (fiveam:is (string= "Be helpful" (cdr (assoc "system_instruction" payload :test #'string=))))
      (let ((tools (cdr (assoc "tools" payload :test #'string=))))
        (fiveam:is (= 1 (length tools)))
        (fiveam:is (string= "google_search" (cdr (assoc "type" (car tools) :test #'string=))))))))

(fiveam:test test-sse-parsing
  (let* ((raw-line "data: {\"event_type\": \"step.delta\", \"delta\": {\"type\": \"text\", \"text\": \"Hello world\"}}")
         (event (parse-sse-event raw-line)))
    (fiveam:is (string= "step.delta" (cdr (assoc :event--type event))))
    (let* ((delta (cdr (assoc :delta event)))
           (text (cdr (assoc :text delta))))
      (fiveam:is (string= "Hello world" text)))))

(fiveam:test test-default-conversation
  (let ((conv (new-chat))
        (called nil))
    (let ((original-post (symbol-function 'dexador:post))
          (original-key-fun (symbol-function 'google:gemini-api-key)))
      (setf (symbol-function 'google:gemini-api-key) (lambda () "mocked-google-api-key"))
      (setf (symbol-function 'dexador:post)
            (lambda (url &rest args)
              (declare (ignore url args))
              (setf called t)
              (values (make-string-input-stream "") 200)))
      (unwind-protect
           (progn
             (setf *default-conversation* conv)
             (chat "Hello")
             (fiveam:is-true called))
        (setf (symbol-function 'google:gemini-api-key) original-key-fun)
        (setf (symbol-function 'dexador:post) original-post)
        (setf *default-conversation* nil)))))

(fiveam:test test-text-formatting
  (let ((wrapped (wrap-text "This is a test of the line wrapping utility." :width 15)))
    (fiveam:is (every (lambda (line) (<= (length line) 15)) wrapped))
    (fiveam:is (string= "This is a test" (car wrapped))))
  (let ((output (with-output-to-string (s)
                  (format-paragraphs "Para one.

Para two." :width 40 :stream s))))
    (fiveam:is (search "Para one." output))
    (fiveam:is (search "Para two." output))))

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
    (fiveam:is (string= "gemini-1.5-flash" (chatbot-model (conversation-chatbot conv-google))))
    
    (fiveam:is (eq :lm-studio (chatbot-backend (conversation-chatbot conv-custom))))
    (fiveam:is (string= "my-model" (chatbot-model (conversation-chatbot conv-custom))))))

(fiveam:test test-openai-api-key-resolution
  (let ((*openai-api-key* "my-explicit-key"))
    (fiveam:is (string= "my-explicit-key" (openai-api-key))))
  (let ((*openai-api-key* nil))
    (let ((original-getenv (symbol-function 'uiop:getenv)))
      (setf (symbol-function 'uiop:getenv)
            (lambda (name)
              (if (string= name "OPENAI_API_KEY")
                  "my-env-key"
                  (funcall original-getenv name))))
      (unwind-protect
           (fiveam:is (string= "my-env-key" (openai-api-key)))
        (setf (symbol-function 'uiop:getenv) original-getenv)))))

(fiveam:test test-openai-chat-flow
  (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
        (captured-payloads nil)
        (original-post (symbol-function 'dexador:post)))
    (setf (symbol-function 'dexador:post)
          (lambda (url &rest args)
            (declare (ignore url))
            (let ((content (getf args :content)))
              (push content captured-payloads))
            (values (make-string-input-stream
                     "data: {\"choices\": [{\"delta\": {\"content\": \"Hello \"}}]}
data: {\"choices\": [{\"delta\": {\"content\": \"OpenAI\"}}]}
data: [DONE]")
                    200)))
    (unwind-protect
         (let ((*openai-api-key* "test-key"))
           ;; Turn 1
           (let ((res1 (chat "Hi there" :conversation conv)))
             (fiveam:is (string= "Hello OpenAI" res1))
             (fiveam:is (= 1 (length captured-payloads)))
             (let* ((payload (cl-json:decode-json-from-string (first captured-payloads)))
                    (messages (cdr (assoc :messages payload))))
               ;; Verify we have system-instruction and current user input
               (fiveam:is (= 2 (length messages)))
               (fiveam:is (string= "system" (cdr (assoc :role (first messages)))))
               (fiveam:is (string= "Be helpful" (cdr (assoc :content (first messages)))))
               (fiveam:is (string= "user" (cdr (assoc :role (second messages)))))
               (fiveam:is (string= "Hi there" (cdr (assoc :content (second messages)))))))
           
           ;; Turn 2
           (let ((res2 (chat "How are you?" :conversation conv)))
             (fiveam:is (string= "Hello OpenAI" res2))
             (fiveam:is (= 2 (length captured-payloads)))
             (let* ((payload (cl-json:decode-json-from-string (first captured-payloads)))
                    (messages (cdr (assoc :messages payload))))
               ;; Verify history + new user input are present
               (fiveam:is (= 4 (length messages)))
               (fiveam:is (string= "system" (cdr (assoc :role (first messages)))))
               (fiveam:is (string= "user" (cdr (assoc :role (second messages)))))
               (fiveam:is (string= "Hi there" (cdr (assoc :content (second messages)))))
               (fiveam:is (string= "assistant" (cdr (assoc :role (third messages)))))
               (fiveam:is (string= "Hello OpenAI" (cdr (assoc :content (third messages)))))
               (fiveam:is (string= "user" (cdr (assoc :role (fourth messages)))))
               (fiveam:is (string= "How are you?" (cdr (assoc :content (fourth messages))))))))
      (setf (symbol-function 'dexador:post) original-post))))

(fiveam:test test-lm-studio-api-key-resolution
  (let ((*lm-studio-api-key* "explicit-lm-key"))
    (fiveam:is (string= "explicit-lm-key" (lm-studio-api-key))))
  (let ((*lm-studio-api-key* nil))
    (let ((original-getenv (symbol-function 'uiop:getenv)))
      (setf (symbol-function 'uiop:getenv)
            (lambda (name)
              (if (string= name "LM_API_TOKEN")
                  "env-lm-key"
                  (funcall original-getenv name))))
      (unwind-protect
           (fiveam:is (string= "env-lm-key" (lm-studio-api-key)))
        (setf (symbol-function 'uiop:getenv) original-getenv)))))

(fiveam:test test-lm-studio-chat-flow
  (let ((conv (new-chat :backend :lm-studio))
        (captured-url nil)
        (captured-headers nil)
        (original-post (symbol-function 'dexador:post)))
    (setf (symbol-function 'dexador:post)
          (lambda (url &rest args)
            (setf captured-url url)
            (setf captured-headers (getf args :headers))
            (values (make-string-input-stream
                     "data: {\"choices\": [{\"delta\": {\"content\": \"Hello LM Studio\"}}]}
data: [DONE]")
                    200)))
    (unwind-protect
         (let ((*lm-studio-api-key* "lm_studio")
               (*lm-studio-base-url* "http://127.0.0.1:8088/v1"))
           (let ((res (chat "Hello local model" :conversation conv)))
             (fiveam:is (string= "Hello LM Studio" res))
             (fiveam:is (string= "http://127.0.0.1:8088/v1/chat/completions" captured-url))
             (fiveam:is (string= "Bearer lm_studio" (cdr (assoc "Authorization" captured-headers :test #'string=))))))
      (setf (symbol-function 'dexador:post) original-post))))

(fiveam:test test-google-chat-flow
  (let ((conv (new-chat :backend :google :system-instruction "Be concise"))
        (captured-url nil)
        (captured-headers nil)
        (captured-content nil)
        (original-post (symbol-function 'dexador:post))
        (original-key-fun (symbol-function 'google:gemini-api-key)))
    (setf (symbol-function 'google:gemini-api-key) (lambda () "mocked-google-api-key"))
    (setf (symbol-function 'dexador:post)
          (lambda (url &rest args)
            (setf captured-url url)
            (setf captured-headers (getf args :headers))
            (setf captured-content (getf args :content))
            (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200)))
    (unwind-protect
         (let* ((callback-called nil)
                (res (chat "Hi Google"
                           :conversation conv
                           :callback (lambda (text)
                                       (setf callback-called text)))))
           (fiveam:is (string= "Hello from Google non-streaming" res))
           (fiveam:is (string= "Hello from Google non-streaming" callback-called))
           (fiveam:is (string= "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=mocked-google-api-key" captured-url))
           (fiveam:is (string= "application/json" (cdr (assoc "Content-Type" captured-headers :test #'string=))))
           
           ;; Decode payload and check properties
           (let* ((payload (cl-json:decode-json-from-string captured-content))
                  (contents (cdr (assoc :contents payload)))
                  (sys-inst (cdr (assoc :system-instruction payload)))
                  (first-msg (car contents))
                  (parts (cdr (assoc :parts first-msg))))
             (fiveam:is (= 1 (length contents)))
             (fiveam:is (string= "user" (cdr (assoc :role first-msg))))
             (fiveam:is (string= "Hi Google" (cdr (assoc :text (car parts)))))
             (fiveam:is (string= "Be concise" (cdr (assoc :text (car (cdr (assoc :parts sys-inst)))))))))
      (setf (symbol-function 'google:gemini-api-key) original-key-fun)
      (setf (symbol-function 'dexador:post) original-post))))

(fiveam:test test-persona-loading
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "test-persona/" personas-dir))
         (original-homedir (symbol-function 'get-user-homedir-pathname)))
    ;; Create mock directories
    (ensure-directories-exist test-persona-dir)
    ;; Write config.lisp and system-instruction.md
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "system-instruction.md" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "You are a helpful test assistant." s))
    
    ;; Mock get-user-homedir-pathname
    (setf (symbol-function 'get-user-homedir-pathname) (lambda () mock-home))
    
    (unwind-protect
         (let ((conv (new-chat-persona "test-persona")))
           (let ((bot (conversation-chatbot conv)))
             (fiveam:is (eq :google (chatbot-backend bot)))
             (fiveam:is (string= "models/gemini-mock-model" (chatbot-model bot)))
             (fiveam:is (not (null (search "You are a helpful test assistant." (chatbot-system-instruction bot)))))))
      ;; Cleanup
      (setf (symbol-function 'get-user-homedir-pathname) original-homedir)
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-loading-unexpected-properties
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-unexpected/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (original-homedir (symbol-function 'get-user-homedir-pathname)))
    ;; Mock get-user-homedir-pathname
    (setf (symbol-function 'get-user-homedir-pathname) (lambda () mock-home))
    
    (unwind-protect
         (progn
           ;; Scenario 1: Well-formed plist but with unexpected keys, plus valid new parameters (google-search-p, code-execution-p)
           (let ((test-persona-dir (merge-pathnames "persona-unexpected/" personas-dir)))
             (ensure-directories-exist test-persona-dir)
             (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                                :direction :output
                                :if-exists :supersede)
               (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api :unexpected-key \"unexpected-val\" :google-search-p t :code-execution-p nil)" s))
             (with-open-file (s (merge-pathnames "system-instruction.md" test-persona-dir)
                                :direction :output
                                :if-exists :supersede)
               (write-line "Test instructions." s))
             (let* ((conv (new-chat-persona "persona-unexpected"))
                    (bot (conversation-chatbot conv)))
               (fiveam:is (eq :google (chatbot-backend bot)))
               (fiveam:is (string= "models/gemini-mock-model" (chatbot-model bot)))
               (fiveam:is-true (chatbot-google-search-p bot))
               (fiveam:is-false (chatbot-code-execution-p bot))))

           ;; Scenario 2: Malformed plist with odd number of elements (which would crash standard GETF)
           (let ((test-persona-dir (merge-pathnames "persona-odd-plist/" personas-dir)))
             (ensure-directories-exist test-persona-dir)
             (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                                :direction :output
                                :if-exists :supersede)
               (write-line "(:model \"models/gemini-mock-model-odd\" :googleapi :google-api :malformed-property)" s))
             (let* ((conv (new-chat-persona "persona-odd-plist"))
                    (bot (conversation-chatbot conv)))
               (fiveam:is (eq :google (chatbot-backend bot)))
               (fiveam:is (string= "models/gemini-mock-model-odd" (chatbot-model bot)))))

           ;; Scenario 3: Malformed config file content (completely invalid syntax, should trigger reader handler-case fallback)
           (let ((test-persona-dir (merge-pathnames "persona-syntax-error/" personas-dir)))
             (ensure-directories-exist test-persona-dir)
             (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                                :direction :output
                                :if-exists :supersede)
               (write-line "this-is-not-a-list" s))
             (let* ((conv (new-chat-persona "persona-syntax-error"))
                    (bot (conversation-chatbot conv)))
               ;; Verify we fall back to defaults without raising an error
               (fiveam:is (eq :gemini (chatbot-backend bot)))
               (fiveam:is (string= "gemini-3.5-flash" (chatbot-model bot))))))
      ;; Cleanup
      (setf (symbol-function 'get-user-homedir-pathname) original-homedir)
      (uiop:delete-directory-tree mock-home :validate t))))

(defun run-all-tests ()
  "Utility to run the chatbot-suite tests and return results."
  (let ((results (fiveam:run 'chatbot-suite)))
    (fiveam:explain! results)
    (every #'fiveam::test-passed-p results)))
