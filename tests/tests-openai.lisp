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

(fiveam:test test-openai-chat-joins-system-instruction-paragraph-vectors
  (let ((captured-payload nil))
    (let* ((*openai-api-key* "test-key")
           (context (make-runtime-context
                     :http-post-function
                     (lambda (url &rest args)
                       (declare (ignore url))
                       (setf captured-payload (getf args :content))
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                               200))))
           (conv (new-chat :backend :openai
                           :system-instruction #("First paragraph." "Second paragraph.")
                           :runtime-context context)))
      (fiveam:is (string= "Hello OpenAI" (chat "Hi there" :conversation conv)))
      (let* ((payload (cl-json:decode-json-from-string captured-payload))
             (messages (cdr (assoc :messages payload)))
             (expected (format nil "First paragraph.~%~%Second paragraph.")))
        (fiveam:is (string= expected
                            (cdr (assoc :content (first messages)))))))))

(fiveam:test test-openai-chat-includes-effective-sampling-parameters
  (let ((captured-payload nil))
    (let* ((*openai-api-key* "test-key")
           (context (make-runtime-context
                     :http-post-function
                     (lambda (url &rest args)
                       (declare (ignore url))
                       (setf captured-payload (getf args :content))
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                               200))))
           (conv (new-chat :backend :openai
                           :temperature 0.3d0
                           :top-p 0.4d0
                           :runtime-context context)))
      (fiveam:is (string= "Hello OpenAI"
                          (chat "Hi there"
                                :conversation conv
                                :temperature 0.8d0
                                :top-p 0.95d0)))
      (fiveam:is (search "\"temperature\":0.8" captured-payload))
      (fiveam:is (search "\"top_p\":0.95" captured-payload))
      (let ((parameters (sampling-parameters conv)))
        (fiveam:is (= 0.3d0 (getf parameters :temperature)))
        (fiveam:is (= 0.4d0 (getf parameters :top-p)))))))

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

(fiveam:test test-openai-chat-includes-transient-file-attachments-without-persisting-them
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "openai-chat-files/" temp-dir))
        (file-path (merge-pathnames "note.txt" root))
        (captured-payloads nil))
    (ensure-directories-exist root)
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (write-string "Alpha attachment" stream))
    (unwind-protect
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
               (conv (new-chat :backend :openai
                               :system-instruction "Be helpful"
                               :runtime-context context)))
          (fiveam:is (string= "Hello OpenAI"
                              (chat "Summarize" :conversation conv :files (list file-path))))
          (fiveam:is (string= "Hello OpenAI"
                              (chat "No files now" :conversation conv)))
          (fiveam:is (= 2 (length captured-payloads)))
          (let* ((first-payload (cl-json:decode-json-from-string (first captured-payloads)))
                 (first-messages (cdr (assoc :messages first-payload)))
                 (first-content (cdr (assoc :content (second first-messages))))
                 (second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                 (second-messages (cdr (assoc :messages second-payload)))
                 (stored-history (conversation-messages conv)))
            (fiveam:is (= 2 (length first-content)))
            (fiveam:is (string= "Summarize"
                                (cdr (assoc :text (first first-content)))))
            (fiveam:is (search "Alpha attachment"
                               (cdr (assoc :text (second first-content)))))
            (fiveam:is (string= "Summarize"
                                (cdr (assoc :content (second second-messages)))))
            (fiveam:is (notany (lambda (message)
                                 (search "Alpha attachment"
                                         (princ-to-string (cdr (assoc :content message)))))
                               second-messages))
            (fiveam:is (= 4 (length stored-history)))
            (fiveam:is (string= "Summarize"
                                (cdr (assoc "content" (first stored-history) :test #'string=))))
            (fiveam:is (notany (lambda (message)
                                 (search "Alpha attachment"
                                         (princ-to-string
                                          (cdr (assoc "content" message :test #'string=)))))
                               stored-history))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-openai-chat-file-alias-matches-singleton-files
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "openai-chat-file-alias/" temp-dir))
         (file-path (merge-pathnames "note.txt" root))
         (captured-payloads nil))
    (ensure-directories-exist root)
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (write-string "Alias attachment" stream))
    (unwind-protect
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
                                   200)))))
           (fiveam:is (string= "Hello OpenAI"
                               (chat "Summarize"
                                    :conversation (new-chat :backend :openai :runtime-context context)
                                    :file file-path)))
           (fiveam:is (string= "Hello OpenAI"
                               (chat "Summarize"
                                    :conversation (new-chat :backend :openai :runtime-context context)
                                    :files (list file-path))))
           (fiveam:is (= 2 (length captured-payloads)))
           (fiveam:is (string= (first captured-payloads)
                               (second captured-payloads))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-send-latest-screenshot-uses-home-relative-matches-and-appends-prompt
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-screenshots/" temp-dir))
        (shots-dir (merge-pathnames "OneDrive/Pictures/Screenshots 1/" mock-home))
        (older (merge-pathnames "older.txt" shots-dir))
        (newer (merge-pathnames "newer.txt" shots-dir))
        (captured-payload nil))
    (ensure-directories-exist shots-dir)
    (with-open-file (stream older :direction :output :if-exists :supersede)
      (write-string "Older screenshot text" stream))
    (sleep 1)
    (with-open-file (stream newer :direction :output :if-exists :supersede)
      (write-string "Newest screenshot text" stream))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home))
              (*screenshot-path* #p"~/Missing/*.txt")
              (*screenshot-path-1* #p"~/OneDrive/Pictures/Screenshots 1/*.txt")
              (*screenshot-prompt* "Describe the screenshot.")
              (*openai-api-key* "test-key"))
          (let* ((context (make-runtime-context
                           :http-post-function
                           (lambda (url &rest args)
                             (declare (ignore url))
                             (setf captured-payload (getf args :content))
                             (values (make-string-input-stream
                                      "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                                     200))))
                 (conv (new-chat :backend :openai :runtime-context context)))
            (fiveam:is (string= "Hello OpenAI"
                                (send-latest-screenshot :n 1
                                                        :prompt "Focus on the HUD."
                                                        :conversation conv)))
            (let* ((payload (cl-json:decode-json-from-string captured-payload))
                   (messages (cdr (assoc :messages payload)))
                   (content (cdr (assoc :content (first messages))))
                   (attachment-text (cdr (assoc :text (second content)))))
              (fiveam:is (= 2 (length content)))
              (fiveam:is (string= "Describe the screenshot. Focus on the HUD."
                                  (cdr (assoc :text (first content)))))
              (fiveam:is (search "Newest screenshot text" attachment-text))
              (fiveam:is-false (search "Older screenshot text" attachment-text)))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-lisp-news-fetches-configured-feed-urls-as_attachments
  (let ((captured-payload nil)
       (captured-urls nil))
    (let* ((*openai-api-key* "test-key")
          (context (make-runtime-context
                    :http-get-function
                    (lambda (url &rest args)
                      (declare (ignore args))
                      (push url captured-urls)
                      (values (cond
                                ((string= url "https://planet.lisp.org/rss20.xml")
                                 "<rss><channel><title>Planet Lisp</title></channel></rss>")
                                ((string= url "https://planet.scheme.org/atom.xml")
                                 "<feed><title>Planet Scheme</title></feed>")
                                (t
                                 (error "Unexpected feed URL: ~A" url)))
                              200
                              '(("content-type" . "application/xml; charset=utf-8"))))
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-payload (getf args :content))
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                              200))))
          (conv (new-chat :backend :openai :runtime-context context)))
      (fiveam:is (string= "Hello OpenAI" (lisp-news :conversation conv)))
      (fiveam:is (equal '("https://planet.lisp.org/rss20.xml"
                         "https://planet.scheme.org/atom.xml")
                       (nreverse captured-urls)))
      (let* ((payload (cl-json:decode-json-from-string captured-payload))
            (messages (cdr (assoc :messages payload)))
            (content (cdr (assoc :content (first messages))))
            (prompt-text (cdr (assoc :text (first content))))
            (lisp-feed-text (cdr (assoc :text (second content))))
            (scheme-feed-text (cdr (assoc :text (third content)))))
       (fiveam:is (string= "What's new in the world of Lisp and Scheme these days?"
                           prompt-text))
       (fiveam:is (search "https://planet.lisp.org/rss20.xml" lisp-feed-text))
       (fiveam:is (search "Planet Lisp" lisp-feed-text))
       (fiveam:is (search "https://planet.scheme.org/atom.xml" scheme-feed-text))
       (fiveam:is (search "Planet Scheme" scheme-feed-text))))))

(fiveam:test test-openai-chat-includes-preloaded-diary-history
  (let ((captured-payload nil))
    (let* ((*openai-api-key* "test-key")
           (context (make-runtime-context
                     :http-post-function
                     (lambda (url &rest args)
                       (declare (ignore url))
                       (setf captured-payload (getf args :content))
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                               200))))
           (conv (new-chat :backend :openai
                           :system-instruction "Be helpful"
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (setf (conversation-persona-diary-entries conv)
            '(((:filename . "1.txt") (:content . "First diary entry."))
              ((:filename . "2.txt") (:content . "Second diary entry."))))
      (chat "First live turn" :conversation conv)
      (let* ((payload (cl-json:decode-json-from-string captured-payload))
             (messages (cdr (assoc :messages payload))))
        (fiveam:is (= 6 (length messages)))
        (fiveam:is (string= (format nil "[Diary: 1.txt]~%First diary entry.")
                            (cdr (assoc :content (fourth messages)))))
        (fiveam:is (string= (format nil "[Diary: 2.txt]~%Second diary entry.")
                            (cdr (assoc :content (fifth messages)))))
        (fiveam:is (string= "First live turn"
                            (cdr (assoc :content (sixth messages)))))))))

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

(fiveam:test test-lm-studio-api-base-url-normalizes-root-and-v1-paths
  (let ((*lm-studio-base-url* "http://127.0.0.1:1234/"))
    (fiveam:is (string= "http://127.0.0.1:1234/v1"
                       (lm-studio-api-base-url))))
  (let ((*lm-studio-base-url* "http://127.0.0.1:1234/v1"))
    (fiveam:is (string= "http://127.0.0.1:1234/v1"
                       (lm-studio-api-base-url)))))

(fiveam:test test-lm-studio-chat-flow
  (let ((captured-url nil)
        (captured-headers nil))
    (let* ((*lm-studio-api-key* "lm_studio")
           (*lm-studio-base-url* "http://127.0.0.1:1234/")
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
        (fiveam:is (string= "http://127.0.0.1:1234/v1/chat/completions" captured-url))
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
            (echo-tool (find "echo_tool"
                             tools
                             :test #'string=
                             :key (lambda (tool)
                                    (cdr (assoc :name (cdr (assoc :function tool)))))))
            (function (cdr (assoc :function echo-tool))))
        (fiveam:is (string= "function" (cdr (assoc :type echo-tool))))
        (fiveam:is (string= "echo_tool" (cdr (assoc :name function))))
        (fiveam:is (string= "Echo tool" (cdr (assoc :description function))))))))

(fiveam:test test-openai-tool-call-errors-are-reported-back-to-the-model
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
             (declare (ignore server arguments))
             (error 'mcp-tool-execution-error
                    :tool-name tool-name
                    :reason "Mock tool failure")))
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
                          "data: {\"choices\": [{\"delta\": {\"content\": \"Handled tool error\"}}]}
data: [DONE]")
                         200)))))
      (let ((res (let ((*openai-api-key* "test-key"))
                  (chat "Run tool" :conversation conv))))
        (fiveam:is (string= "Handled tool error" res))
        (fiveam:is (= 2 (length captured-payloads)))
        (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
              (messages (cdr (assoc :messages second-payload)))
              (tool-msg (fourth messages))
              (tool-content (cdr (assoc :content tool-msg))))
         (fiveam:is (string= "tool" (cdr (assoc :role tool-msg))))
         (fiveam:is (search "\"type\":\"tool_error\"" tool-content))
         (fiveam:is (search "\"toolName\":\"echo_tool\"" tool-content))
         (fiveam:is (search "\"message\":\"Mock tool failure\"" tool-content)))))))

(fiveam:test test-openai-tool-recursion-depth-is-capped
  (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
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
            (declare (ignore server tool-name arguments))
            "loop result"))
         (*http-post-function*
          (lambda (url &rest args)
            (declare (ignore url args))
            (incf call-count)
            (values (make-string-input-stream
                     "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"echo_tool\", \"arguments\": \"{\\\"value\\\":\\\"payload\\\"}\"}}]}}]}
data: [DONE]")
                    200))))
      (fiveam:signals chatbot-tool-recursion-limit-error
        (let ((*openai-api-key* "test-key"))
         (chat "Run tool loop" :conversation conv)))
      (fiveam:is (= +max-chatbot-tool-recursion-depth+ call-count)))))

(fiveam:test test-openai-built-in-read-file-lines-tool-recursion
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "openai-filesystem-tool/" temp-dir))
        (file-path (merge-pathnames "notes.txt" root)))
    (ensure-directories-exist root)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Line one" s)
      (write-line "Line two" s)
      (write-line "Line three" s))
    (unwind-protect
         (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
               (captured-payloads nil)
               (call-count 0))
           (setf (chatbot-filesystem-tools-p (conversation-chatbot conv)) t)
           (setf (chatbot-filesystem-root-directory (conversation-chatbot conv)) root)
           (let ((*http-post-function*
                   (lambda (url &rest args)
                     (declare (ignore url))
                     (incf call-count)
                     (setf captured-payloads
                           (append captured-payloads (list (getf args :content))))
                     (if (= call-count 1)
                         (values (make-string-input-stream
                                  "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"readFileLines\", \"arguments\": \"{\\\"filename\\\":\\\"notes.txt\\\",\\\"beginningLine\\\":2,\\\"endingLine\\\":100}\"}}]}}]}
data: [DONE]")
                                 200)
                         (values (make-string-input-stream
                                  "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                                 200))))
                 (*openai-api-key* "test-key"))
             (let ((res (chat "Read the file" :conversation conv)))
               (fiveam:is (string= "Done" res))
               (fiveam:is (= 2 (length captured-payloads)))
               (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                      (messages (cdr (assoc :messages second-payload))))
                 (fiveam:is (string= (format nil "Line two~%Line three")
                                     (cdr (assoc :content (fourth messages)))))))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-openai-request-prefixes-live-input-with-fresh-timestamp-only
  (let (captured-payload)
    (let* ((context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-payload (getf args :content))
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                              200))))
          (*openai-api-key* "test-key")
          (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
          (conv (new-chat :backend :openai
                          :model "gpt-4o"
                          :include-timestamp-p t
                          :runtime-context context)))
      (setf (conversation-messages conv)
           (list (list (cons "role" "user")
                       (cons "content" "Earlier question"))
                 (list (cons "role" "assistant")
                       (cons "content" "Earlier answer"))))
      (fiveam:is (string= "Hello OpenAI" (chat "What time is it?" :conversation conv)))
      (let* ((payload (cl-json:decode-json-from-string captured-payload))
            (messages (cdr (assoc :messages payload))))
       (fiveam:is (= 3 (length messages)))
       (fiveam:is (string= "Earlier question" (cdr (assoc :content (first messages)))))
       (fiveam:is (string= "Earlier answer" (cdr (assoc :content (second messages)))))
       (fiveam:is (string= "[14:29 26-Jun-2026] What time is it?"
                           (cdr (assoc :content (third messages))))))
      (let ((updated-messages (conversation-messages conv)))
       (fiveam:is (= 4 (length updated-messages)))
       (fiveam:is (string= "What time is it?"
                           (cdr (assoc "content" (third updated-messages) :test #'string=))))))))

(fiveam:test test-openai-request-prefixes-live-input-with-timestamp-and-model-only
  (let (captured-payload)
    (let* ((context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-payload (getf args :content))
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Hello OpenAI\"}}]}
data: [DONE]")
                              200))))
          (*openai-api-key* "test-key")
          (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
          (conv (new-chat :backend :openai
                          :model "gpt-4o"
                          :include-timestamp-p t
                          :include-model-p t
                          :runtime-context context)))
      (setf (conversation-messages conv)
           (list (list (cons "role" "user")
                       (cons "content" "Earlier question"))
                 (list (cons "role" "assistant")
                       (cons "content" "Earlier answer"))))
      (fiveam:is (string= "Hello OpenAI" (chat "What time is it?" :conversation conv)))
      (let* ((payload (cl-json:decode-json-from-string captured-payload))
            (messages (cdr (assoc :messages payload))))
       (fiveam:is (= 3 (length messages)))
       (fiveam:is (string= "Earlier question" (cdr (assoc :content (first messages)))))
       (fiveam:is (string= "Earlier answer" (cdr (assoc :content (second messages)))))
       (fiveam:is (string= "[14:29 26-Jun-2026] [model: gpt-4o] What time is it?"
                           (cdr (assoc :content (third messages))))))
      (let ((updated-messages (conversation-messages conv)))
       (fiveam:is (= 4 (length updated-messages)))
       (fiveam:is (string= "What time is it?"
                           (cdr (assoc "content" (third updated-messages) :test #'string=))))))))

(fiveam:test test-openai-built-in-eval-tool-recursion
  (let (captured-payloads)
    (let ((*eval-approval-function* (lambda (&rest ignored)
                                     (declare (ignore ignored))
                                     t)))
      (let* ((conv (new-chat :backend :openai :system-instruction "Be helpful" :enable-eval-p t))
            (call-count 0))
       (let ((*http-post-function*
              (lambda (url &rest args)
                (declare (ignore url))
                (incf call-count)
                (setf captured-payloads
                      (append captured-payloads (list (getf args :content))))
                (if (= call-count 1)
                    (values (make-string-input-stream
                             "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"eval\", \"arguments\": \"{\\\"expression\\\":\\\"(progn (format t \\\\\\\"hello\\\\\\\") (format *error-output* \\\\\\\"oops\\\\\\\") (values 42 :done))\\\"}\"}}]}}]}
data: [DONE]")
                            200)
                    (values (make-string-input-stream
                             "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                            200))))
             (*openai-api-key* "test-key"))
         (let ((res (chat "Run the eval" :conversation conv)))
           (fiveam:is (string= "Done" res))
           (fiveam:is (= 2 (length captured-payloads)))
           (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                  (messages (cdr (assoc :messages second-payload)))
                  (result (cl-json:decode-json-from-string
                           (cdr (assoc :content (fourth messages))))))
             (fiveam:is (equal '("42" ":DONE")
                               (coerce (cdr (assoc :values result)) 'list)))
             (fiveam:is (string= "hello" (cdr (assoc :stdout result))))
             (fiveam:is (string= "oops" (cdr (assoc :stderr result)))))))))))

(fiveam:test test-openai-built-in-web-search-tool-recursion
  (let (captured-payloads)
    (flet ((mock-response ()
            (let ((response (make-hash-table :test #'eql))
                  (search-info (make-hash-table :test #'eql))
                  (item (make-hash-table :test #'eql)))
              (setf (gethash :total-results search-info) "1")
              (setf (gethash :search-information response) search-info)
              (setf (gethash :title item) "Common Lisp")
              (setf (gethash :link item) "https://lisp-lang.org/")
              (setf (gethash :snippet item) "A programmable programming language.")
              (setf (gethash :items response) (vector item))
              response)))
      (let ((*web-search-function* (lambda (query)
                                    (fiveam:is (string= "common lisp" query))
                                    (mock-response))))
        (let* ((conv (new-chat :backend :openai :system-instruction "Be helpful" :web-tools-p t))
              (call-count 0))
          (let ((*http-post-function*
                (lambda (url &rest args)
                  (declare (ignore url))
                  (incf call-count)
                  (setf captured-payloads
                        (append captured-payloads (list (getf args :content))))
                  (if (= call-count 1)
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"webSearch\", \"arguments\": \"{\\\"query\\\":\\\"common lisp\\\"}\"}}]}}]}
data: [DONE]")
                              200)
                      (values (make-string-input-stream
                               "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                              200))))
               (*openai-api-key* "test-key"))
            (let ((res (chat "Run the search" :conversation conv)))
             (fiveam:is (string= "Done" res))
             (fiveam:is (= 2 (length captured-payloads)))
             (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                    (messages (cdr (assoc :messages second-payload))))
               (fiveam:is (search "Web search query: common lisp"
                                  (cdr (assoc :content (fourth messages)))))
               (fiveam:is (search "https://lisp-lang.org/"
                                  (cdr (assoc :content (fourth messages)))))))))))))

(fiveam:test test-openai-built-in-directory-tool-recursion
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "openai-directory-tool/" temp-dir))
        (docs-dir (merge-pathnames "docs/" root)))
    (ensure-directories-exist docs-dir)
    (with-open-file (s (merge-pathnames "alpha.txt" docs-dir) :direction :output :if-exists :supersede)
      (write-line "Alpha" s))
    (with-open-file (s (merge-pathnames "beta.txt" docs-dir) :direction :output :if-exists :supersede)
      (write-line "Beta" s))
    (with-open-file (s (merge-pathnames "notes.md" docs-dir) :direction :output :if-exists :supersede)
      (write-line "Notes" s))
    (unwind-protect
        (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
              (captured-payloads nil)
              (call-count 0))
          (setf (chatbot-filesystem-tools-p (conversation-chatbot conv)) t)
          (setf (chatbot-filesystem-root-directory (conversation-chatbot conv)) root)
          (let ((*http-post-function*
                 (lambda (url &rest args)
                   (declare (ignore url))
                   (incf call-count)
                   (setf captured-payloads
                         (append captured-payloads (list (getf args :content))))
                   (if (= call-count 1)
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"directory\", \"arguments\": \"{\\\"pathname\\\":\\\"docs\\\",\\\"pattern\\\":\\\"*.txt\\\"}\"}}]}}]}
data: [DONE]")
                               200)
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                               200))))
                (*openai-api-key* "test-key"))
            (let ((res (chat "List the directory" :conversation conv)))
              (fiveam:is (string= "Done" res))
              (fiveam:is (= 2 (length captured-payloads)))
              (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                     (messages (cdr (assoc :messages second-payload))))
                (fiveam:is (equal '("docs/alpha.txt" "docs/beta.txt")
                                  (coerce (cl-json:decode-json-from-string
                                           (cdr (assoc :content (fourth messages))))
                                          'list)))))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-openai-built-in-write-file-tool-recursion
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "openai-write-file-tool/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root)))
    (ensure-directories-exist root)
    (unwind-protect
         (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
               (captured-payloads nil)
               (call-count 0))
           (setf (chatbot-filesystem-tools-p (conversation-chatbot conv)) t)
           (setf (chatbot-filesystem-root-directory (conversation-chatbot conv)) root)
           (let ((*http-post-function*
                 (lambda (url &rest args)
                   (declare (ignore url))
                   (incf call-count)
                   (setf captured-payloads
                         (append captured-payloads (list (getf args :content))))
                   (if (= call-count 1)
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"writeFile\", \"arguments\": \"{\\\"pathname\\\":\\\"notes.txt\\\",\\\"useLfOnly\\\":true,\\\"endWithEol\\\":false,\\\"lines\\\":[\\\"Alpha\\\",\\\"Beta\\\"]}\"}}]}}]}
data: [DONE]")
                               200)
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                               200))))
                (*openai-api-key* "test-key"))
             (let ((res (chat "Write the file" :conversation conv)))
               (fiveam:is (string= "Done" res))
               (fiveam:is (= 2 (length captured-payloads)))
               (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                     (messages (cdr (assoc :messages second-payload))))
                (fiveam:is (string= "Wrote file: notes.txt"
                                    (cdr (assoc :content (fourth messages)))))
                (fiveam:is (string= (format nil "Alpha~%Beta")
                                    (read-test-file-octets-as-string file-path)))))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-openai-built-in-delete-file-tool-recursion
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "openai-delete-file-tool/" temp-dir))
         (file-path (merge-pathnames "notes.txt" root)))
    (ensure-directories-exist root)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Delete me" s))
    (unwind-protect
         (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
               (captured-payloads nil)
               (call-count 0))
           (setf (chatbot-filesystem-tools-p (conversation-chatbot conv)) t)
           (setf (chatbot-filesystem-root-directory (conversation-chatbot conv)) root)
           (let ((*http-post-function*
                 (lambda (url &rest args)
                   (declare (ignore url))
                   (incf call-count)
                   (setf captured-payloads
                         (append captured-payloads (list (getf args :content))))
                   (if (= call-count 1)
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"deleteFile\", \"arguments\": \"{\\\"pathname\\\":\\\"notes.txt\\\"}\"}}]}}]}
data: [DONE]")
                               200)
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                               200))))
                (*openai-api-key* "test-key"))
             (let ((res (chat "Delete the file" :conversation conv)))
               (fiveam:is (string= "Done" res))
               (fiveam:is (= 2 (length captured-payloads)))
               (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                      (messages (cdr (assoc :messages second-payload))))
                 (fiveam:is (string= "Deleted file: notes.txt"
                                     (cdr (assoc :content (fourth messages)))))
                 (fiveam:is-false (probe-file file-path))))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-openai-filesystem-tool-recursion-prompts-and-persists-approval
  (let* ((temp-dir (uiop:default-temporary-directory))
         (root (merge-pathnames "openai-allowlist-root/" temp-dir))
         (outside-dir (merge-pathnames "approved-openai/" temp-dir))
         (file-path (merge-pathnames "notes.txt" outside-dir))
         (allowlist-path (merge-pathnames "filesystem-allowlist.lisp" root))
         (prompted-directory nil))
    (ensure-directories-exist root)
    (ensure-directories-exist outside-dir)
    (with-open-file (s file-path :direction :output :if-exists :supersede)
      (write-line "Line one" s)
      (write-line "Line two" s))
    (unwind-protect
         (let ((conv (new-chat :backend :openai :system-instruction "Be helpful"))
               (captured-payloads nil)
               (call-count 0))
           (setf (chatbot-filesystem-tools-p (conversation-chatbot conv)) t)
           (setf (chatbot-filesystem-root-directory (conversation-chatbot conv)) root)
           (setf (chatbot-filesystem-allowlist-path (conversation-chatbot conv)) allowlist-path)
           (let ((*filesystem-access-approval-function*
                 (lambda (ignored-bot directory tool-name)
                   (declare (ignore ignored-bot))
                   (setf prompted-directory (list (namestring directory) tool-name))
                   t))
                 (*http-post-function*
                 (lambda (url &rest args)
                   (declare (ignore url))
                   (incf call-count)
                   (setf captured-payloads
                         (append captured-payloads (list (getf args :content))))
                   (if (= call-count 1)
                       (values (make-string-input-stream
                                (format nil
                                        "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call-1\", \"function\": {\"name\": \"readFileLines\", \"arguments\": \"{\\\"filename\\\":\\\"~A\\\",\\\"beginningLine\\\":1,\\\"endingLine\\\":10}\"}}]}}]}~%data: [DONE]"
                                        (namestring file-path)))
                               200)
                       (values (make-string-input-stream
                                "data: {\"choices\": [{\"delta\": {\"content\": \"Done\"}}]}
data: [DONE]")
                               200))))
                 (*openai-api-key* "test-key"))
             (let ((res (chat "Read approved file" :conversation conv)))
               (fiveam:is (string= "Done" res))
               (fiveam:is (equal (list (namestring (uiop:ensure-directory-pathname (truename outside-dir)))
                                      "readFileLines")
                                prompted-directory))
               (fiveam:is (equal (list (namestring (uiop:ensure-directory-pathname (truename outside-dir))))
                                (read-test-lisp-form allowlist-path)))
               (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                     (messages (cdr (assoc :messages second-payload))))
                 (fiveam:is (string= (format nil "Line one~%Line two")
                                    (cdr (assoc :content (fourth messages)))))))))
      (uiop:delete-directory-tree outside-dir :validate t)
      (uiop:delete-directory-tree root :validate t))))
