;;; tests-google.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-google-chat-flow
  (let ((captured-url nil)
        (captured-headers nil)
        (captured-content nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context (make-runtime-context
                    :gemini-api-key-function (lambda () "mocked-google-api-key")
                    :http-post-function
                    (lambda (url &rest args)
                      (setf captured-url url)
                      (setf captured-headers (getf args :headers))
                      (setf captured-content (getf args :content))
                      (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
           (conv (new-chat :backend :google :system-instruction "Be concise" :runtime-context context)))
      (let* ((callback-called nil)
            (res (chat "Hi Google"
                       :conversation conv
                       :callback (lambda (text)
                                   (setf callback-called text)))))
        (fiveam:is (string= "Hello from Google non-streaming" res))
        (fiveam:is (string= "Hello from Google non-streaming" callback-called))
        (fiveam:is (string= "https://example.test/gemini/models/gemini-3.5-flash:generateContent" captured-url))
        (fiveam:is (string= "mocked-google-api-key" (cdr (assoc "x-goog-api-key" captured-headers :test #'string=))))
        (fiveam:is (string= "application/json" (cdr (assoc "Content-Type" captured-headers :test #'string=))))
        (let* ((payload (cl-json:decode-json-from-string captured-content))
              (contents (mcp-val :contents payload)))
          (fiveam:is (= 1 (length contents)))
          (assert-google-message-texts (first contents) "user" '("Hi Google"))
          (assert-google-system-instruction-texts payload '("Be concise")))))))

(fiveam:test test-google-chat-preserves-system-instruction-paragraph-vectors
  (let ((captured-content nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
          (context (make-runtime-context
                    :gemini-api-key-function (lambda () "mocked-google-api-key")
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-content (getf args :content))
                      (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
          (conv (new-chat :backend :google
                          :system-instruction #("First paragraph." "Second paragraph.")
                          :runtime-context context)))
      (fiveam:is (string= "Hello from Google non-streaming"
                         (chat "Hi Google" :conversation conv)))
      (let* ((payload (cl-json:decode-json-from-string captured-content))
             (parts (message-part-texts (mcp-val :system-instruction payload))))
        (fiveam:is (equal '("First paragraph." "Second paragraph.")
                          parts)))))) 

(fiveam:test test-google-chat-includes-generation-config
  (let ((captured-content nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context (make-runtime-context
                     :gemini-api-key-function (lambda () "mocked-google-api-key")
                     :http-post-function
                     (lambda (url &rest args)
                       (declare (ignore url))
                       (setf captured-content (getf args :content))
                       (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
           (conv (new-chat :backend :google
                           :temperature 0.6d0
                           :top-p 0.85d0
                           :runtime-context context)))
      (fiveam:is (string= "Hello from Google non-streaming"
                          (chat "Hi Google" :conversation conv)))
      (let ((generation-config (test-json-value-any (decode-test-json captured-content)
                                                    '("generationConfig" :generation-config))))
        (assert-json-field= generation-config "temperature" 0.6d0)
        (assert-json-value= (test-json-value-any generation-config '("topP" "top_p" :top-p))
                            0.85d0)))))

(fiveam:test test-google-chat-flow-supports-models-prefix
  (let ((captured-url nil))
    (let ((*gemini-base-url* "https://example.test/gemini"))
      (let* ((context (make-runtime-context
                       :gemini-api-key-function (lambda () "mocked-google-api-key")
                       :http-post-function
                       (lambda (url &rest args)
                         (declare (ignore args))
                         (setf captured-url url)
                         (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
             (conv (new-chat :backend :google
                             :model "models/gemini-prefixed-model"
                             :runtime-context context)))
        (fiveam:is (string= "Hello from Google non-streaming"
                            (chat "Hi Google" :conversation conv)))
        (fiveam:is (string= "https://example.test/gemini/models/gemini-prefixed-model:generateContent"
                            captured-url))))))

(fiveam:test test-google-chat-dollar-prefix-overrides-model-for-one-turn
  (let ((captured-urls nil)
        (captured-payloads nil)
        (call-count 0))
    (let* ((*gemini-base-url* "https://example.test/gemini")
          (context (make-runtime-context
                   :gemini-api-key-function (lambda () "mocked-google-api-key")
                   :http-post-function
                   (lambda (url &rest args)
                     (incf call-count)
                     (push url captured-urls)
                     (push (getf args :content) captured-payloads)
                     (values
                      (if (= call-count 1)
                          "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"First reply\"}], \"role\": \"model\"}}]}"
                          "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Second reply\"}], \"role\": \"model\"}}]}")
                      200))))
          (conv (new-chat :backend :google :runtime-context context)))
      (fiveam:is (string= "First reply" (chat "$First turn" :conversation conv)))
      (fiveam:is (string= "Second reply" (chat "Second turn" :conversation conv)))
      (fiveam:is (equal '("https://example.test/gemini/models/gemini-pro-latest:generateContent"
                         "https://example.test/gemini/models/gemini-3.5-flash:generateContent")
                       (nreverse captured-urls)))
      (let* ((first-payload (decode-test-json (second captured-payloads)))
             (first-contents (google-payload-contents first-payload))
             (second-payload (decode-test-json (first captured-payloads)))
             (second-contents (google-payload-contents second-payload))
             (stored-history (conversation-messages conv)))
        (assert-google-message-texts (first first-contents) "user" '("First turn"))
        (assert-google-message-texts (third second-contents) "user" '("Second turn"))
        (assert-history-message (first stored-history) "user" "First turn")
        (fiveam:is (string= "gemini-3.5-flash" (chatbot-model (conversation-chatbot conv))))))))

(fiveam:test test-google-chat-preserves-preloaded-history-every-turn
  (let ((captured-payloads nil))
    (let* ((context (make-runtime-context
                    :gemini-api-key-function (lambda () "mocked-google-api-key")
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-payloads
                            (append captured-payloads (list (getf args :content))))
                      (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
           (conv (new-chat :backend :google :system-instruction "Be concise" :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (chat "First live turn" :conversation conv)
      (chat "Second live turn" :conversation conv)
      (fiveam:is (= 2 (length captured-payloads)))
      (let* ((first-payload (decode-test-json (first captured-payloads)))
             (first-contents (google-payload-contents first-payload))
             (second-payload (decode-test-json (second captured-payloads)))
             (second-contents (google-payload-contents second-payload)))
        (fiveam:is (= 1 (length first-contents)))
        (assert-google-message-texts (first first-contents) "user" '("First live turn"))
        (fiveam:is (= 3 (length second-contents)))
        (assert-google-message-texts (first second-contents) "user" '("First live turn"))
        (assert-google-message-texts (second second-contents)
                                    "model"
                                    '("Hello from Google non-streaming"))
        (assert-google-message-texts (third second-contents) "user" '("Second live turn"))
        (fiveam:is (= 4 (length (conversation-messages conv))))
        (fiveam:is (string= "Stored persona memory."
                           (conversation-persona-memory conv)))))))

(fiveam:test test-google-chat-auto-cache-skips-empty-prefix-without-error
  (let ((captured-payload nil)
        (captured-urls nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (push url captured-urls)
                (setf captured-payload (getf args :content))
                (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
           (conv (new-chat :backend :google
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (fiveam:is (string= "Hello from Google non-streaming"
                         (chat "First live turn" :conversation conv)))
      (fiveam:is (= 1 (length captured-urls)))
      (fiveam:is-false (search "/cachedContents" (first captured-urls)))
      (let ((payload (decode-test-json captured-payload)))
        (fiveam:is-false (test-json-value-any payload '("cachedContent" :cached-content)))
        (assert-google-message-texts (first (google-payload-contents payload))
                                    "user"
                                    '("First live turn"))))))

(fiveam:test test-google-content-cache-bindings-use-shared-http-seams
  (let ((create-url nil)
        (create-payload nil)
        (get-url nil)
        (list-url nil)
        (patch-url nil)
        (patch-payload nil)
        (delete-url nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (setf create-url url
                      create-payload (getf args :content))
                (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"3600s\"}" 200))
              :http-get-function
              (lambda (url &rest args)
                (declare (ignore args))
                (cond
                  ((search "pageSize=10&pageToken=next-page" url)
                   (setf list-url url)
                   (values "{\"cachedContents\":[{\"name\":\"cachedContents/cache-1\"}]}" 200))
                  (t
                   (setf get-url url)
                   (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"3600s\"}" 200))))
              :http-patch-function
              (lambda (url &rest args)
                (setf patch-url url
                      patch-payload (getf args :content))
                (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"1800s\"}" 200))
              :http-delete-function
              (lambda (url &rest args)
                (declare (ignore args))
                (setf delete-url url)
                (values "" 204))))
           (conv (new-chat :backend :google
                           :system-instruction "Cache this stable prefix."
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (call-with-runtime-context
       context
       (lambda ()
         (let ((created (create-google-content-cache conv))
               (fetched (get-google-content-cache "cachedContents/cache-1"))
               (listed (list-google-content-caches :page-size 10 :page-token "next-page"))
               (updated (update-google-content-cache-ttl "cachedContents/cache-1" 1800))
               (deleted (delete-google-content-cache "cachedContents/cache-1")))
           (fiveam:is (string= "cachedContents/cache-1" (cached-content-response-name created)))
           (fiveam:is (string= "cachedContents/cache-1" (cached-content-response-name fetched)))
           (fiveam:is (string=
                       "cachedContents/cache-1"
                       (cached-content-response-name
                        (first (test-json-elements
                                (test-json-value-any listed '("cachedContents" :cached-contents)))))))
           (fiveam:is (string= "1800s" (test-json-value-any updated '("ttl" :ttl))))
           (fiveam:is-true deleted))))
      (fiveam:is (string= "https://example.test/gemini/cachedContents" create-url))
      (fiveam:is (string= "https://example.test/gemini/cachedContents/cache-1" get-url))
      (fiveam:is (string= "https://example.test/gemini/cachedContents?pageSize=10&pageToken=next-page" list-url))
      (fiveam:is (string= "https://example.test/gemini/cachedContents/cache-1?updateMask=ttl" patch-url))
      (fiveam:is (string= "https://example.test/gemini/cachedContents/cache-1" delete-url))
      (let ((create-json (decode-test-json create-payload))
            (patch-json (decode-test-json patch-payload)))
        (assert-json-value= (test-json-value-any create-json '("ttl" :ttl)) "3600s")
        (fiveam:is (equal '("Cache this stable prefix.")
                          (message-part-texts (test-json-value-any create-json '("systemInstruction" :system-instruction)))))
        (assert-json-value= (test-json-value-any patch-json '("ttl" :ttl)) "1800s")))))

(fiveam:test test-google-chat-auto-creates-and-reuses-explicit-content-cache
  (let ((captured-urls nil)
        (captured-payloads nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (push url captured-urls)
                (push (getf args :content) captured-payloads)
                (if (search "/cachedContents" url)
                    (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"3600s\"}" 200)
                    (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200)))))
           (conv (new-chat :backend :google
                           :system-instruction "Be concise"
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (fiveam:is (string= "Hello from Google non-streaming"
                         (chat "First live turn" :conversation conv)))
      (fiveam:is (string= "Hello from Google non-streaming"
                         (chat "Second live turn" :conversation conv)))
      (fiveam:is (= 3 (length captured-urls)))
      (fiveam:is (= 1 (count-if (lambda (url) (search "/cachedContents" url)) captured-urls)))
      (fiveam:is (string= "cachedContents/cache-1" (conversation-cached-content-name conv)))
      (fiveam:is (string= "cachedContents/cache-1"
                         (cached-content-response-name (conversation-cached-content-metadata conv))))
      (let* ((first-cache-payload (decode-test-json (third captured-payloads)))
             (first-turn-payload (decode-test-json (second captured-payloads)))
             (second-turn-payload (decode-test-json (first captured-payloads)))
             (first-turn-contents (google-payload-contents first-turn-payload))
             (second-turn-contents (google-payload-contents second-turn-payload)))
        (assert-json-value= (test-json-value-any first-turn-payload '("cachedContent" :cached-content))
                            "cachedContents/cache-1")
        (fiveam:is-false (test-json-value-any first-turn-payload '("systemInstruction" :system-instruction)))
        (fiveam:is (= 1 (length first-turn-contents)))
        (assert-google-message-texts (first first-turn-contents) "user" '("First live turn"))
        (fiveam:is (equal '("Be concise")
                          (message-part-texts (test-json-value-any first-cache-payload '("systemInstruction" :system-instruction)))))
        (assert-json-value= (test-json-value-any second-turn-payload '("cachedContent" :cached-content))
                            "cachedContents/cache-1")
        (fiveam:is-false (test-json-value-any second-turn-payload '("systemInstruction" :system-instruction)))
        (assert-google-message-texts (first second-turn-contents) "user" '("First live turn"))
        (assert-google-message-texts (second second-turn-contents) "model" '("Hello from Google non-streaming"))
        (assert-google-message-texts (third second-turn-contents) "user" '("Second live turn"))))))

(fiveam:test test-google-chat-rebuilds-explicit-content-cache-when-somewhat-stale
  (let ((captured-urls nil)
        (captured-payloads nil)
        (cache-events nil))
    (flet ((utc-rfc3339 (universal-time)
             (multiple-value-bind (second minute hour day month year)
                 (decode-universal-time universal-time 0)
               (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
                       year month day hour minute second))))
      (let* ((*gemini-base-url* "https://example.test/gemini")
             (context
               (make-runtime-context
                :gemini-api-key-function (lambda () "mocked-google-api-key")
                :http-post-function
                (lambda (url &rest args)
                  (push url captured-urls)
                  (push (getf args :content) captured-payloads)
                  (if (search "/cachedContents" url)
                      (progn
                        (push (list :create url) cache-events)
                        (values "{\"name\":\"cachedContents/cache-2\",\"ttl\":\"3600s\",\"expireTime\":\"2030-01-01T00:00:00Z\"}" 200))
                      (progn
                        (push (list :chat url) cache-events)
                        (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
                :http-delete-function
                (lambda (url &rest args)
                  (declare (ignore args))
                  (push (list :delete url) cache-events)
                  (values "" 204))))
             (conv (new-chat :backend :google
                             :system-instruction "Be concise"
                             :content-cache-policy :auto
                             :content-cache-min-tokens 1
                             :runtime-context context)))
        (setf (conversation-persona-memory conv) "Stored persona memory.")
        (let ((descriptor (google-cacheable-prefix-descriptor conv)))
          (setf (conversation-cached-content-name conv) "cachedContents/cache-1")
          (setf (conversation-cached-content-key conv) (getf descriptor :fingerprint))
          (setf (conversation-cached-content-metadata conv)
                `(("name" . "cachedContents/cache-1")
                  ("ttl" . "3600s")
                  ("expireTime" . ,(utc-rfc3339 (+ (get-universal-time) 120)))))
          (setf (conversation-turns-since-cache-reload conv) 5))
        (fiveam:is (string= "Hello from Google non-streaming"
                            (chat "First live turn" :conversation conv)))
        (fiveam:is (= 2 (length captured-urls)))
        (fiveam:is (= 1 (count-if (lambda (url) (search "/cachedContents" url)) captured-urls)))
        (fiveam:is (string= "cachedContents/cache-2" (conversation-cached-content-name conv)))
        (fiveam:is (equal `((:delete "https://example.test/gemini/cachedContents/cache-1")
                            (:create "https://example.test/gemini/cachedContents")
                            (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                          (nreverse cache-events)))
        (assert-json-value= (test-json-value-any (decode-test-json (first captured-payloads))
                                                 '("cachedContent" :cached-content))
                            "cachedContents/cache-2")))))

(fiveam:test test-google-chat-rebuilds-explicit-content-cache-when-previous-cache-already-gone
  (let ((cache-events nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (declare (ignore args))
                (if (search "/cachedContents" url)
                    (progn
                      (push (list :create url) cache-events)
                      (values "{\"name\":\"cachedContents/cache-2\",\"ttl\":\"3600s\",\"expireTime\":\"2030-01-01T00:00:00Z\"}" 200))
                    (progn
                      (push (list :chat url) cache-events)
                      (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
              :http-delete-function
              (lambda (url &rest args)
                (declare (ignore args))
                (push (list :delete url) cache-events)
                (values "" 404))))
           (conv (new-chat :backend :google
                           :system-instruction "Be concise"
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (let ((descriptor (google-cacheable-prefix-descriptor conv)))
        (setf (conversation-cached-content-name conv) "cachedContents/cache-1")
        (setf (conversation-cached-content-key conv) (getf descriptor :fingerprint))
        (setf (conversation-cached-content-metadata conv)
              '(("name" . "cachedContents/cache-1")
                ("ttl" . "3600s")
                ("expireTime" . "2000-01-01T00:00:00Z"))))
      (fiveam:is (string= "Hello from Google non-streaming"
                          (chat "First live turn" :conversation conv)))
      (fiveam:is (string= "cachedContents/cache-2" (conversation-cached-content-name conv)))
      (fiveam:is (equal `((:delete "https://example.test/gemini/cachedContents/cache-1")
                          (:create "https://example.test/gemini/cachedContents")
                          (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                        (nreverse cache-events))))))

(fiveam:test test-google-chat-rebuilds-explicit-content-cache-when-delete-signals-missing-403
  (let ((cache-events nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (declare (ignore args))
                (if (search "/cachedContents" url)
                    (progn
                      (push (list :create url) cache-events)
                      (values "{\"name\":\"cachedContents/cache-2\",\"ttl\":\"3600s\",\"expireTime\":\"2030-01-01T00:00:00Z\"}" 200))
                    (progn
                      (push (list :chat url) cache-events)
                      (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
              :http-delete-function
              (lambda (url &rest args)
                (declare (ignore args))
                (push (list :delete url) cache-events)
                (error "An HTTP request to \"~A\" returned 403 forbidden.~%~%{\"error\":{\"code\":403,\"message\":\"CachedContent not found (or permission denied)\",\"status\":\"PERMISSION_DENIED\"}}"
                       url))))
           (conv (new-chat :backend :google
                           :system-instruction "Be concise"
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (let ((descriptor (google-cacheable-prefix-descriptor conv)))
        (setf (conversation-cached-content-name conv) "cachedContents/cache-1")
        (setf (conversation-cached-content-key conv) (getf descriptor :fingerprint))
        (setf (conversation-cached-content-metadata conv)
              '(("name" . "cachedContents/cache-1")
                ("ttl" . "3600s")
                ("expireTime" . "2000-01-01T00:00:00Z"))))
      (fiveam:is (string= "Hello from Google non-streaming"
                          (chat "First live turn" :conversation conv)))
      (fiveam:is (string= "cachedContents/cache-2" (conversation-cached-content-name conv)))
      (fiveam:is (equal `((:delete "https://example.test/gemini/cachedContents/cache-1")
                          (:create "https://example.test/gemini/cachedContents")
                          (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                        (nreverse cache-events))))))

(fiveam:test test-google-chat-postpones-cache-reload-until-5-turns-pass
  (let ((cache-events nil))
    (flet ((utc-rfc3339 (universal-time)
             (multiple-value-bind (second minute hour day month year)
                 (decode-universal-time universal-time 0)
               (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
                       year month day hour minute second))))
      (let* ((*gemini-base-url* "https://example.test/gemini")
             (context
               (make-runtime-context
                :gemini-api-key-function (lambda () "mocked-google-api-key")
                :http-post-function
                (lambda (url &rest args)
                  (declare (ignore args))
                  (if (search "/cachedContents" url)
                      (progn
                        (push (list :create url) cache-events)
                        (values "{\"name\":\"cachedContents/cache-2\",\"ttl\":\"3600s\",\"expireTime\":\"2030-01-01T00:00:00Z\"}" 200))
                      (progn
                        (push (list :chat url) cache-events)
                        (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
                :http-delete-function
                (lambda (url &rest args)
                  (declare (ignore args))
                  (push (list :delete url) cache-events)
                  (values "" 204))))
             (conv (new-chat :backend :google
                             :system-instruction "Be concise"
                             :content-cache-policy :auto
                             :content-cache-min-tokens 1
                             :runtime-context context)))
        (setf (conversation-persona-memory conv) "Stored persona memory.")
        (let ((descriptor (google-cacheable-prefix-descriptor conv)))
          (setf (conversation-cached-content-name conv) "cachedContents/cache-1")
          (setf (conversation-cached-content-key conv) (getf descriptor :fingerprint))
          (setf (conversation-cached-content-metadata conv)
                `(("name" . "cachedContents/cache-1")
                  ("ttl" . "3600s")
                  ("expireTime" . ,(utc-rfc3339 (+ (get-universal-time) 120)))))) ; somewhat stale (120 seconds left)
        
        ;; At turn start: turns-since-cache-reload = 0. Cache is stale, but we have existing cache.
        ;; First turn
        (chat "Turn 1" :conversation conv)
        (fiveam:is (= 1 (conversation-turns-since-cache-reload conv)))
        (fiveam:is (equal `((:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                          (nreverse cache-events)))
        (setf cache-events nil)
        
        ;; Second turn
        (chat "Turn 2" :conversation conv)
        (fiveam:is (= 2 (conversation-turns-since-cache-reload conv)))
        (fiveam:is (equal `((:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                          (nreverse cache-events)))
        (setf cache-events nil)
        
        ;; Third turn
        (chat "Turn 3" :conversation conv)
        (fiveam:is (= 3 (conversation-turns-since-cache-reload conv)))
        
        ;; Fourth turn
        (chat "Turn 4" :conversation conv)
        (fiveam:is (= 4 (conversation-turns-since-cache-reload conv)))
        
        ;; Fifth turn
        (chat "Turn 5" :conversation conv)
        (fiveam:is (= 5 (conversation-turns-since-cache-reload conv)))
        (fiveam:is (equal `((:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent")
                            (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent")
                            (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                          (nreverse cache-events)))
        (setf cache-events nil)
        
        ;; Sixth turn: turns-since-cache-reload = 5. Cache is stale. Now it should reload!
        (chat "Turn 6" :conversation conv)
        (fiveam:is (= 1 (conversation-turns-since-cache-reload conv))) ; reset to 0 upon reload, then incremented to 1 by applying the turn result
        (fiveam:is (string= "cachedContents/cache-2" (conversation-cached-content-name conv)))
        (fiveam:is (equal `((:delete "https://example.test/gemini/cachedContents/cache-1")
                            (:create "https://example.test/gemini/cachedContents")
                            (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                          (nreverse cache-events)))))))

(fiveam:test test-google-chat-reloads-cache-when-model-mismatch-even-within-5-turns
  (let ((cache-events nil))
    (flet ((utc-rfc3339 (universal-time)
             (multiple-value-bind (second minute hour day month year)
                 (decode-universal-time universal-time 0)
               (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
                       year month day hour minute second))))
      (let* ((*gemini-base-url* "https://example.test/gemini")
             (context
               (make-runtime-context
                :gemini-api-key-function (lambda () "mocked-google-api-key")
                :http-post-function
                (lambda (url &rest args)
                  (declare (ignore args))
                  (if (search "/cachedContents" url)
                      (progn
                        (push (list :create url) cache-events)
                        (values "{\"name\":\"cachedContents/cache-2\",\"model\":\"models/gemini-pro-latest\",\"ttl\":\"3600s\",\"expireTime\":\"2030-01-01T00:00:00Z\"}" 200))
                      (progn
                        (push (list :chat url) cache-events)
                        (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200))))
                :http-delete-function
                (lambda (url &rest args)
                  (declare (ignore args))
                  (push (list :delete url) cache-events)
                  (values "" 204))))
             (conv (new-chat :backend :google
                             :system-instruction "Be concise"
                             :content-cache-policy :auto
                             :content-cache-min-tokens 1
                             :runtime-context context)))
        (setf (conversation-persona-memory conv) "Stored persona memory.")
        (let ((descriptor (google-cacheable-prefix-descriptor conv)))
          (setf (conversation-cached-content-name conv) "cachedContents/cache-1")
          (setf (conversation-cached-content-key conv) (getf descriptor :fingerprint))
          (setf (conversation-cached-content-metadata conv)
                `(("name" . "cachedContents/cache-1")
                  ("model" . "models/gemini-3.5-flash")
                  ("ttl" . "3600s")
                  ("expireTime" . ,(utc-rfc3339 (+ (get-universal-time) 120)))))) ; somewhat stale

        ;; At turn start: turns-since-cache-reload = 0.
        ;; First turn uses dollar-prefix override for model (gemini-pro-latest).
        ;; Under the dollar-prefix cache-bypass policy, using the elevated model
        ;; must not use or replace the cache (meaning no create/delete events happen,
        ;; and the cache name is untouched).
        (chat "$gemini-pro-latest Turn 1" :conversation conv)
        (fiveam:is (= 1 (conversation-turns-since-cache-reload conv)))
        (fiveam:is (string= "cachedContents/cache-1" (conversation-cached-content-name conv)))
        (fiveam:is (equal `((:chat "https://example.test/gemini/models/gemini-pro-latest:generateContent"))
                          (nreverse cache-events)))))))

(fiveam:test test-google-chat-recovers-gracefully-when-cache-not-found-on-chat
  (let ((chat-events nil)
        (chat-payloads nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (let ((payload (getf args :content)))
                  (push payload chat-payloads)
                  (if (search "/cachedContents" url)
                      (progn
                        (push (list :create url) chat-events)
                        (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"3600s\",\"expireTime\":\"2030-01-01T00:00:00Z\"}" 200))
                      (progn
                        (push (list :chat url) chat-events)
                        (if (and payload (search "cache-1" payload))
                            ;; First attempt fails with 404 CachedContent not found (or permission denied)
                            (error "An HTTP request to \"~A\" returned 404 not found.~%~%{\"error\":{\"code\":404,\"message\":\"CachedContent not found\",\"status\":\"NOT_FOUND\"}}" url)
                            ;; Second attempt succeeds without the cached content reference
                            (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello without cache\"}], \"role\": \"model\"}}]}" 200))))))))
           (conv (new-chat :backend :google
                           :system-instruction "Be concise"
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      ;; Pre-seed with existing cache
      (let ((descriptor (google-cacheable-prefix-descriptor conv)))
        (setf (conversation-cached-content-name conv) "cachedContents/cache-1")
        (setf (conversation-cached-content-key conv) (getf descriptor :fingerprint))
        (setf (conversation-cached-content-metadata conv)
              '(("name" . "cachedContents/cache-1")
                ("ttl" . "3600s")
                ("expireTime" . "2030-01-01T00:00:00Z"))))
      (fiveam:is (string= "Hello without cache"
                          (chat "First live turn" :conversation conv)))
      ;; The cache state should be completely cleared after recovering from the error
      (fiveam:is (null (conversation-cached-content-name conv)))
      (fiveam:is (equal `((:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent")
                          (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                        (nreverse chat-events))))))

(fiveam:test test-google-chat-recovers-gracefully-when-permission-denied-on-chat
  (let ((chat-events nil)
        (chat-payloads nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (let ((payload (getf args :content)))
                  (push payload chat-payloads)
                  (if (search "/cachedContents" url)
                      (progn
                        (push (list :create url) chat-events)
                        (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"3600s\",\"expireTime\":\"2030-01-01T00:00:00Z\"}" 200))
                      (progn
                        (push (list :chat url) chat-events)
                        (if (and payload (search "cache-1" payload))
                            ;; First attempt fails with 403 PERMISSION_DENIED
                            (error "An HTTP request to \"~A\" returned 403 forbidden.~%~%{\"error\":{\"code\":403,\"message\":\"The caller does not have permission\",\"status\":\"PERMISSION_DENIED\"}}" url)
                            ;; Second attempt succeeds without the cached content reference
                            (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello without cache\"}], \"role\": \"model\"}}]}" 200))))))))
           (conv (new-chat :backend :google
                           :system-instruction "Be concise"
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      ;; Pre-seed with existing cache
      (let ((descriptor (google-cacheable-prefix-descriptor conv)))
        (setf (conversation-cached-content-name conv) "cachedContents/cache-1")
        (setf (conversation-cached-content-key conv) (getf descriptor :fingerprint))
        (setf (conversation-cached-content-metadata conv)
              '(("name" . "cachedContents/cache-1")
                ("ttl" . "3600s")
                ("expireTime" . "2030-01-01T00:00:00Z"))))
      (fiveam:is (string= "Hello without cache"
                          (chat "First live turn" :conversation conv)))
      ;; The cache state should be completely cleared after recovering from the error
      (fiveam:is (null (conversation-cached-content-name conv)))
      (fiveam:is (equal `((:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent")
                          (:chat "https://example.test/gemini/models/gemini-3.5-flash:generateContent"))
                        (nreverse chat-events))))))

(fiveam:test test-google-chat-moves-tools-into-cached-content
  (let* ((tool '((:name . "lookup_time")
                 (:description . "Looks up the current time")
                 (:input-schema . ((:type . "object")
                                   (:properties . nil)))))
         (captured-urls nil)
         (captured-payloads nil))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (*get-all-mcp-tools-function*
             (lambda (ignored-bot)
               (declare (ignore ignored-bot))
               (list (cons nil tool))))
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (push url captured-urls)
                (push (getf args :content) captured-payloads)
                (if (search "/cachedContents" url)
                    (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"3600s\"}" 200)
                    (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200)))))
           (conv (new-chat :backend :google
                           :system-instruction "Be concise"
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (fiveam:is (string= "Hello from Google non-streaming"
                         (chat "First live turn" :conversation conv)))
      (let* ((cache-payload (decode-test-json (second captured-payloads)))
             (turn-payload (decode-test-json (first captured-payloads)))
             (cache-tools (google-payload-tools cache-payload))
             (request-tools (google-payload-tools turn-payload)))
        (fiveam:is (= 1 (count-if (lambda (url) (search "/cachedContents" url)) captured-urls)))
        (fiveam:is (= 1 (length cache-tools)))
        (fiveam:is (member "lookup_time"
                           (google-tool-names cache-tools)
                           :test #'string=))
        (fiveam:is-false request-tools)
        (assert-json-value= (test-json-value-any turn-payload '("cachedContent" :cached-content))
                            "cachedContents/cache-1")))))

(fiveam:test test-google-chat-reuses-cache-despite-tool-order-churn
  (let ((captured-urls nil)
        (captured-payloads nil)
        (tool-call-count 0))
    (let* ((*gemini-base-url* "https://example.test/gemini")
           (*get-all-mcp-tools-function*
             (lambda (ignored-bot)
               (declare (ignore ignored-bot))
               (incf tool-call-count)
               (let ((alpha '((:name . "alpha_tool")
                              (:description . "Alpha tool")
                              (:input-schema . ((:type . "object")
                                                (:properties . nil)))))
                     (beta '((:name . "beta_tool")
                             (:description . "Beta tool")
                             (:input-schema . ((:type . "object")
                                               (:properties . nil))))))
                 (if (oddp tool-call-count)
                     (list (cons nil beta) (cons nil alpha))
                     (list (cons nil alpha) (cons nil beta))))))
           (context
             (make-runtime-context
              :gemini-api-key-function (lambda () "mocked-google-api-key")
              :http-post-function
              (lambda (url &rest args)
                (push url captured-urls)
                (push (getf args :content) captured-payloads)
                (if (search "/cachedContents" url)
                    (values "{\"name\":\"cachedContents/cache-1\",\"ttl\":\"3600s\"}" 200)
                    (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}" 200)))))
           (conv (new-chat :backend :google
                           :system-instruction "Be concise"
                           :content-cache-policy :auto
                           :content-cache-min-tokens 1
                           :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (fiveam:is (string= "Hello from Google non-streaming"
                         (chat "First live turn" :conversation conv)))
      (fiveam:is (string= "Hello from Google non-streaming"
                         (chat "Second live turn" :conversation conv)))
      (fiveam:is (= 1 (count-if (lambda (url) (search "/cachedContents" url)) captured-urls)))
      (fiveam:is (string= "cachedContents/cache-1" (conversation-cached-content-name conv)))
      (let* ((cache-payload (decode-test-json (third captured-payloads)))
             (cache-tools (google-payload-tools cache-payload)))
        (fiveam:is
         (equal '("alpha_tool" "beta_tool")
                (remove-if-not
                 (lambda (name)
                   (member name '("alpha_tool" "beta_tool") :test #'string=))
                 (google-tool-names cache-tools))))))))

(fiveam:test test-google-chat-includes-transient-file-attachments-without-persisting-them
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "google-chat-files/" temp-dir))
        (file-path (merge-pathnames "note.txt" root))
        (captured-payloads nil))
    (ensure-directories-exist root)
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (write-string "Alpha attachment" stream))
    (unwind-protect
        (let* ((context (make-runtime-context
                         :gemini-api-key-function (lambda () "mocked-google-api-key")
                         :http-post-function
                         (lambda (url &rest args)
                           (declare (ignore url))
                           (setf captured-payloads
                                 (append captured-payloads (list (getf args :content))))
                           (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}"
                                   200))))
               (conv (new-chat :backend :google :runtime-context context)))
          (fiveam:is (string= "Hello from Google non-streaming"
                              (chat "Summarize" :conversation conv :files (list file-path))))
          (fiveam:is (string= "Hello from Google non-streaming"
                              (chat "No files now" :conversation conv)))
          (fiveam:is (= 2 (length captured-payloads)))
          (let* ((first-payload (decode-test-json (first captured-payloads)))
                (first-message (first (google-payload-contents first-payload)))
                (first-parts (google-message-parts first-message))
                (inline-data-part (find-if (lambda (part)
                                             (test-json-value-any part '("inlineData" :inline-data)))
                                           first-parts))
                (inline-data (and inline-data-part
                                  (test-json-value-any inline-data-part
                                                       '("inlineData" :inline-data))))
                (second-payload (decode-test-json (second captured-payloads)))
                (second-message (first (google-payload-contents second-payload)))
                (second-parts (google-message-parts second-message))
                (stored-history (conversation-messages conv)))
            (fiveam:is (= 2 (length first-parts)))
            (assert-json-field= first-message :role "user")
            (assert-json-field= (first first-parts) "text" "Summarize")
            (assert-json-field= inline-data "mimeType" "text/plain")
            (assert-google-message-texts second-message "user" '("Summarize"))
            (fiveam:is (notany (lambda (part)
                                 (test-json-value-any part '("inlineData" :inline-data)))
                               second-parts))
            (fiveam:is (= 4 (length stored-history)))
            (assert-history-message (first stored-history) "user" "Summarize")
            (fiveam:is (notany (lambda (content)
                                 (search "Alpha attachment" content))
                               (history-contents stored-history))))
      (uiop:delete-directory-tree root :validate t)))))

(fiveam:test test-google-chat-continues-without-persona
  (let ((captured-payloads nil)
        (call-count 0))
    (let* ((context (make-runtime-context
                    :gemini-api-key-function (lambda () "mocked-google-api-key")
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (incf call-count)
                      (setf captured-payloads
                            (append captured-payloads (list (getf args :content))))
                      (values
                       (if (= call-count 1)
                           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"First reply\"}], \"role\": \"model\"}}]}"
                           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Second reply\"}], \"role\": \"model\"}}]}")
                       200))))
           (conv (new-chat :backend :google :system-instruction "Be concise" :runtime-context context)))
      (fiveam:is (string= "First reply" (chat "First turn" :conversation conv)))
      (fiveam:is (string= "Second reply" (chat "Second turn" :conversation conv)))
      (fiveam:is (= 2 (length captured-payloads)))
      (let* ((first-payload (decode-test-json (first captured-payloads)))
             (first-contents (google-payload-contents first-payload))
             (second-payload (decode-test-json (second captured-payloads)))
             (second-contents (google-payload-contents second-payload)))
        (fiveam:is (= 1 (length first-contents)))
        (assert-google-message-texts (first first-contents) "user" '("First turn"))
        (fiveam:is (= 3 (length second-contents)))
        (assert-google-message-texts (first second-contents) "user" '("First turn"))
        (assert-google-message-texts (second second-contents) "model" '("First reply"))
        (assert-google-message-texts (third second-contents) "user" '("Second turn"))
        (fiveam:is (= 4 (length (conversation-messages conv))))))))

(fiveam:test test-google-persona-preload-is-sent-as-model-history
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-google-persona/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "google-persona/" personas-dir))
         (captured-content nil))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "compressed-memory.txt" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Stored persona memory." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home))
              (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
              (*http-post-function*
                (lambda (url &rest args)
                  (declare (ignore url))
                  (setf captured-content (getf args :content))
                  (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}"
                          200))))
          (let ((conv (new-chat-persona "google-persona")))
            (fiveam:is (string= "Stored persona memory."
                                (conversation-persona-memory conv)))
            (chat "First live turn" :conversation conv)
            (let* ((payload (decode-test-json captured-content))
                   (contents (google-payload-contents payload)))
              (fiveam:is (= 1 (length contents)))
              (assert-google-message-texts (first contents)
                                           "user"
                                           '("First live turn")))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-google-chat-includes-preloaded-diary-history
  (let ((captured-content nil))
    (let* ((context (make-runtime-context
                    :gemini-api-key-function (lambda () "mocked-google-api-key")
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url))
                      (setf captured-content (getf args :content))
                      (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google non-streaming\"}], \"role\": \"model\"}}]}"
                              200))))
          (conv (new-chat :backend :google
                          :system-instruction "Be concise"
                          :runtime-context context)))
      (setf (conversation-persona-memory conv) "Stored persona memory.")
      (setf (conversation-persona-diary-entries conv)
           '(((:filename . "1.txt") (:content . "First diary entry."))
             ((:filename . "2.txt") (:content . "Second diary entry."))))
      (chat "First live turn" :conversation conv)
      (let* ((payload (decode-test-json captured-content))
            (contents (google-payload-contents payload)))
       (fiveam:is (= 3 (length contents)))
       (assert-google-message-texts (first contents)
                                    "model"
                                    (list (format nil "[Diary: 1.txt]~%First diary entry.")))
       (assert-google-message-texts (second contents)
                                    "model"
                                    (list (format nil "[Diary: 2.txt]~%Second diary entry.")))
       (assert-google-message-texts (third contents)
                                    "user"
                                    '("First live turn"))))))

(fiveam:test test-google-chat-tool-payload-sanitization
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
        (conv (make-instance 'conversation :chatbot bot))
         (tool '((:name . "lookup_time")
                 (:description . "Looks up the current time")
                 (:input-schema . ((:|$schema| . "https://json-schema.org/draft/2020-12/schema")
                                   (:type . "object")
                                   (:properties . nil)))))
         (captured-content nil))
    (let ((*get-all-mcp-tools-function*
            (lambda (ignored-bot)
              (declare (ignore ignored-bot))
              (list (cons nil tool))))
          (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore url))
              (setf captured-content (getf args :content))
              (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Hello from Google tool test\"}], \"role\": \"model\"}}]}" 200))))
      (let ((res (test-chat-google bot "Hi Google" conv nil)))
        (fiveam:is (string= "Hello from Google tool test" res))
        (let* ((payload (decode-test-json captured-content))
               (tools (google-payload-tools payload))
               (declarations (google-tool-declarations tools))
               (lookup-tool (find "lookup_time"
                                  declarations
                                  :test #'string=
                                  :key (lambda (tool)
                                         (test-json-value-any tool '("name" :name)))))
               (parameters (google-tool-parameters lookup-tool))
               (properties (test-json-value-any parameters '("properties" :properties))))
          (fiveam:is (= 1 (length tools)))
          (fiveam:is (null (test-json-value-any (first tools) '("type" :type))))
          (fiveam:is (not (null lookup-tool)))
          (assert-json-field= lookup-tool "name" "lookup_time")
          (fiveam:is (null (test-json-value-any parameters '("$schema" :|$schema|))))
          (fiveam:is (or (null properties)
                         (and (hash-table-p properties)
                              (= 0 (hash-table-count properties)))
                         (and (json-object-alist-p properties)
                              (null properties)))))))))

(fiveam:test test-google-chat-function-call-response
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
         (conv (make-instance 'conversation :chatbot bot))
         (call-count 0)
         (captured-tool-args nil)
         (captured-second-request nil))
    (let ((*find-mcp-server-and-tool-function*
            (lambda (ignored-bot tool-name)
              (declare (ignore ignored-bot))
              (values :mock-server `((:name . ,tool-name)))))
          (*execute-mcp-tool-function*
            (lambda (server tool-name arguments)
              (declare (ignore server tool-name))
              (setf captured-tool-args arguments)
              "The current time is 12:34 PM"))
          (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore url))
              (incf call-count)
              (if (= call-count 1)
                  (values "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"get_current_time\",\"args\":{\"timezone\":\"America/Los_Angeles\"},\"id\":\"bwvnqvbe\"},\"thoughtSignature\":\"sig\"}],\"role\":\"model\"}}]}" 200)
                  (progn
                    (setf captured-second-request (getf args :content))
                    (values "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"It is 12:34 PM in Los Angeles.\"}],\"role\":\"model\"}}]}" 200))))))
      (let ((res (test-chat-google bot "What time is it now?" conv nil)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "America/Los_Angeles" (cdr (assoc :timezone captured-tool-args))))
        (let* ((payload (decode-test-json captured-second-request))
               (contents (google-payload-contents payload))
               (model-message (second contents))
               (function-call-part (google-function-call-part model-message))
               (function-call (assert-google-function-call-part function-call-part
                                                                "get_current_time"
                                                                :thought-signature "sig")))
          (assert-json-field= (test-json-value-any function-call '("args" :args))
                              "timezone"
                              "America/Los_Angeles"))
        (fiveam:is (string= "It is 12:34 PM in Los Angeles." res))))))

(fiveam:test test-google-chat-function-call-errors-are-reported-back-to-the-model
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
         (conv (make-instance 'conversation :chatbot bot))
         (call-count 0)
         (captured-second-request nil))
    (let ((*find-mcp-server-and-tool-function*
            (lambda (ignored-bot tool-name)
              (declare (ignore ignored-bot))
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
              (if (= call-count 1)
                  (values "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"get_current_time\",\"args\":{\"timezone\":\"America/Los_Angeles\"},\"id\":\"bwvnqvbe\"},\"thoughtSignature\":\"sig\"}],\"role\":\"model\"}}]}" 200)
                  (progn
                    (setf captured-second-request (getf args :content))
                    (values "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Handled tool error\"}],\"role\":\"model\"}}]}" 200))))))
      (let ((res (test-chat-google bot "What time is it now?" conv nil)))
        (fiveam:is (= 2 call-count))
        (let* ((payload (decode-test-json captured-second-request))
               (contents (google-payload-contents payload))
               (model-message (second contents))
               (response-message (third contents))
               (function-call-part (google-function-call-part model-message))
               (function-response-part (google-function-response-part response-message))
               (function-response (assert-google-function-response-part function-response-part
                                                                       "get_current_time")))
          (assert-google-function-call-part function-call-part
                                            "get_current_time"
                                            :thought-signature "sig")
          (let ((response (test-json-value-any function-response '("response" :response))))
            (assert-json-field= response "type" "tool_error")
            (assert-json-field= response "toolName" "get_current_time")
            (assert-json-field= response "message" "Mock tool failure")))
        (fiveam:is (string= "Handled tool error" res))))))

(fiveam:test test-google-chat-prints-short-thought-parts
  (reset-global-token-grand-totals)
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
         (conv (make-instance 'conversation :chatbot bot))
         (stream (make-string-output-stream)))
    (let ((*standard-output* stream)
          (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore url args))
              (values "{\"usageMetadata\":{\"promptTokenCount\":2,\"candidatesTokenCount\":3,\"thoughtsTokenCount\":4,\"totalTokenCount\":9},\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Tiny chain of thought\",\"thought\":true},{\"text\":\"Visible answer\"}],\"role\":\"model\"}}]}" 200))))
      (fiveam:is (string= "Visible answer" (test-chat-google bot "Hi Google" conv nil))))
    (let ((output (get-output-stream-string stream)))
      (fiveam:is (search "[Thoughts]" output))
      (fiveam:is (search "Tiny chain of thought" output))
      (fiveam:is (search "Visible answer" output)))))

(fiveam:test test-google-chat-no-arg-function-call-uses-empty-args-object
  (let* ((bot (make-instance 'chatbot
                           :backend :google
                           :model "gemini-3.5-flash"
                           :temperature 0.2d0))
        (conv (make-instance 'conversation :chatbot bot))
        (call-count 0)
        (captured-second-request nil))
    (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url))
             (incf call-count)
             (if (= call-count 1)
                 (values "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"readSamplingParameters\",\"id\":\"call-1\"}}],\"role\":\"model\"}}]}" 200)
                 (progn
                   (setf captured-second-request (getf args :content))
                   (values "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Read sampling ok\"}],\"role\":\"model\"}}]}" 200))))))
      (let ((res (test-chat-google bot "Inspect sampling" conv nil)))
        (fiveam:is (= 2 call-count))
        (let* ((payload (decode-test-json captured-second-request))
               (contents (google-payload-contents payload))
               (model-message (second contents))
               (response-message (third contents))
               (function-call-part (google-function-call-part model-message))
               (function-response-part (google-function-response-part response-message))
               (function-call (assert-google-function-call-part function-call-part
                                                                "readSamplingParameters"))
               (function-response (assert-google-function-response-part function-response-part
                                                                       "readSamplingParameters"))
               (result (decode-test-json
                        (test-json-value-any (test-json-value-any function-response '("response" :response))
                                             '("result" :result)))))
          (fiveam:is (null (test-json-elements (test-json-value-any function-call '("args" :args)))))
          (assert-sampling-parameters result :temperature 0.2d0))
        (fiveam:is (string= "Read sampling ok" res))))))

(fiveam:test test-google-chat-sanitizes-stored-no-arg-function-call-history
  (let* ((stored-history
          (list
           '(("role" . "model")
             ("parts" . #((("functionCall" . (("name" . "readSamplingParameters")
                                              ("args" . nil)))))))
           '(("role" . "user")
             ("parts" . #((("functionResponse" . (("name" . "readSamplingParameters")
                                                  ("response" . (("result" . "{\"temperature\":null,\"topP\":null}")))))))))))
        (bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
        (conv (make-instance 'conversation
                             :chatbot bot
                             :messages stored-history))
        (captured-content nil))
    (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url))
             (setf captured-content (getf args :content))
             (values "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"History sanitized\"}],\"role\":\"model\"}}]}" 200))))
      (let ((res (test-chat-google bot "Next turn" conv nil)))
        (let* ((payload (decode-test-json captured-content))
               (contents (google-payload-contents payload))
               (function-call-part (google-function-call-part (first contents)))
               (function-call (assert-google-function-call-part function-call-part
                                                                "readSamplingParameters")))
          (fiveam:is (null (test-json-elements (test-json-value-any function-call '("args" :args))))))
        (fiveam:is (string= "History sanitized" res))))))

(fiveam:test test-google-chat-tool-recursion-depth-is-capped
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
        (conv (make-instance 'conversation :chatbot bot))
        (call-count 0))
    (let ((*find-mcp-server-and-tool-function*
           (lambda (ignored-bot tool-name)
             (declare (ignore ignored-bot))
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
             (values "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"get_current_time\",\"args\":{\"timezone\":\"America/Los_Angeles\"},\"id\":\"bwvnqvbe\"}}],\"role\":\"model\"}}]}" 200))))
      (fiveam:signals chatbot-tool-recursion-limit-error
        (test-chat-google bot "What time is it now?" conv nil))
      (fiveam:is (= +max-chatbot-tool-recursion-depth+ call-count)))))

(fiveam:test test-google-chat-retries-malformed-response-on-gemini-pro-latest
  (let* ((bot (make-instance 'chatbot
                            :backend :google
                             :model "gemini-3.5-flash"
                             :include-timestamp-p t
                             :include-model-p t))
         (conv (make-instance 'conversation :chatbot bot))
         (call-count 0)
         (captured-urls nil)
         (captured-payloads nil)
         (prompt-count 0))
    (let ((*prompt-timestamp-function*
            (lambda ()
              (incf prompt-count)
              (if (= prompt-count 1)
                  "[08:46 first]"
                  "[08:46 retry]")))
          (*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
            (lambda (url &rest args)
              (incf call-count)
              (push url captured-urls)
              (push (getf args :content) captured-payloads)
              (values
               (if (= call-count 1)
                   "{\"candidates\":[{\"finishReason\":\"MALFORMED_RESPONSE\",\"content\":{\"parts\":[{\"text\":\"Broken\"}],\"role\":\"model\"}}]}"
                   "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Recovered on retry\"}],\"role\":\"model\"}}]}"
                   )
               200))))
      (let ((res (test-chat-google bot "Retry me" conv nil)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "Recovered on retry" res))
        (fiveam:is (equal '("https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash:generateContent"
                            "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro-latest:generateContent")
                          (nreverse captured-urls)))
        (let* ((first-payload (decode-test-json (second captured-payloads)))
               (first-contents (google-payload-contents first-payload))
               (retry-payload (decode-test-json (first captured-payloads)))
               (retry-contents (google-payload-contents retry-payload))
               (stored-history (conversation-messages conv)))
          (assert-google-message-texts (first first-contents)
                                      "user"
                                      (list (format nil "Retry me~%~%=== Dynamic Context ===~%[08:46 first] [model: gemini-3.5-flash]")))
          (assert-google-message-texts (first retry-contents)
                                      "user"
                                      (list (format nil "Retry me~%~%=== Dynamic Context ===~%[08:46 retry] [model: gemini-pro-latest]")))
          (fiveam:is-false
           (search "[model: gemini-3.5-flash]"
                   (first (message-part-texts (first retry-contents)))))
          (assert-history-sequence stored-history
                                   (list (list "user" (format nil "Retry me~%~%=== Dynamic Context ===~%[08:46 retry] [model: gemini-pro-latest]"))
                                         (list "model" "Recovered on retry"))))))))

(fiveam:test test-google-chat-retries-no-text-response-on-gemini-pro-latest
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
         (conv (make-instance 'conversation :chatbot bot))
         (call-count 0)
         (captured-urls nil))
    (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
          (*http-post-function*
            (lambda (url &rest args)
              (declare (ignore args))
              (incf call-count)
              (push url captured-urls)
              (values
               (if (= call-count 1)
                   "{\"candidates\":[{\"content\":{\"parts\":[{}],\"role\":\"model\"}}]}"
                   "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Recovered after empty response\"}],\"role\":\"model\"}}]}"
                   )
               200))))
      (let ((res (test-chat-google bot "Retry empty" conv nil)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "Recovered after empty response" res))
        (fiveam:is (equal '("https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash:generateContent"
                            "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent")
                          (nreverse captured-urls)))))))

(fiveam:test test-google-chat-retries-malformed-function-call-on-gemini-pro-latest
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
        (conv (make-instance 'conversation :chatbot bot))
        (call-count 0)
        (captured-urls nil))
    (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore args))
             (incf call-count)
             (push url captured-urls)
             (values
              (if (= call-count 1)
                  "{\"candidates\":[{\"content\":{},\"finishReason\":\"MALFORMED_FUNCTION_CALL\",\"finishMessage\":\"Malformed function call: Failed to parse function call: Function call is empty - no input to parse.\"}]}"
                  "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Recovered after malformed function call\"}],\"role\":\"model\"}}]}")
              200))))
      (let ((res (test-chat-google bot "Retry malformed function call" conv nil)))
       (fiveam:is (= 2 call-count))
       (fiveam:is (string= "Recovered after malformed function call" res))
       (fiveam:is (equal '("https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash:generateContent"
                           "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro-latest:generateContent")
                         (nreverse captured-urls)))))))

(fiveam:test test-string-to-embedding-vector
  (let ((captured-url nil)
        (captured-payload nil)
        (captured-headers nil))
    (let* ((context (make-runtime-context
                     :gemini-api-key-function (lambda () "mocked-embedding-key")
                     :http-post-function
                     (lambda (url &rest args)
                       (setf captured-url url)
                       (setf captured-payload (getf args :content))
                       (setf captured-headers (getf args :headers))
                       (values "{\"embedding\": {\"values\": [0.1, -0.2, 0.35]}}" 200)))))
      (call-with-runtime-context context
        (lambda ()
          (let ((vec (string->embedding-vector "test message" :model "text-embedding-004")))
            (fiveam:is (equalp #(0.1 -0.2 0.35) vec))
            (fiveam:is (string= "https://generativelanguage.googleapis.com/v1beta/models/text-embedding-004:embedContent" captured-url))
            (let ((decoded (cl-json:decode-json-from-string captured-payload)))
              (fiveam:is (string= "models/text-embedding-004" (cdr (assoc :model decoded)))))
            (fiveam:is (search "test message" captured-payload))
            (fiveam:is (string= "mocked-embedding-key" (cdr (assoc "x-goog-api-key" captured-headers :test #'string=))))))))))

(fiveam:test test-persona-vector-database
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-vector-db/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-vector-db/" personas-dir))
         (memory-path (merge-pathnames "memory.json" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s memory-path :direction :output :if-exists :supersede)
      (write-string "{\"entities\":[{\"name\":\"Joe\",\"entityType\":\"person\",\"observations\":[\"likes Lisp\"]},{\"name\":\"SBCL\",\"entityType\":\"tool\",\"observations\":[\"fast\"]}]}" s))
    (unwind-protect
         (let* ((context (make-runtime-context
                          :gemini-api-key-function (lambda () "mocked-embedding-key")
                          :http-post-function
                          (lambda (url &rest args)
                            (declare (ignore url args))
                            (values "{\"embedding\": {\"values\": [0.5, 0.5]}}" 200)))))
           (call-with-runtime-context context
             (lambda ()
               (let ((*user-homedir-pathname-function* (lambda () mock-home)))
                 ;; 1. Test creation
                 (let ((db (create-persona-vector-database "persona-vector-db")))
                   (fiveam:is (= 2 (length db)))
                   (fiveam:is (equalp #(0.5 0.5) (car (first db))))
                   (fiveam:is (string= "- Joe (person): likes Lisp" (cdr (first db))))
                   
                   ;; 2. Test search
                   (let ((results (search-persona-vector-database "Lisp" db)))
                     (fiveam:is (= 2 (length results)))
                     ;; Cosine similarity between two [0.5, 0.5] vectors is approximately 1.0
                     (fiveam:is (< (abs (- 1.0 (car (first results)))) 1e-5))
                     (fiveam:is (string= "- Joe (person): likes Lisp" (cdr (first results))))))))))
      (uiop:delete-directory-tree mock-home :validate t))))
