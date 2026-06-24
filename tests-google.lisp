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
              (contents (cdr (assoc :contents payload)))
              (sys-inst (cdr (assoc :system-instruction payload)))
              (first-msg (car contents))
              (parts (cdr (assoc :parts first-msg))))
          (fiveam:is (= 1 (length contents)))
          (fiveam:is (string= "user" (cdr (assoc :role first-msg))))
          (fiveam:is (string= "Hi Google" (cdr (assoc :text (car parts)))))
          (fiveam:is (string= "Be concise" (cdr (assoc :text (car (cdr (assoc :parts sys-inst))))))))))))

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
      (let* ((first-payload (cl-json:decode-json-from-string (second captured-payloads)))
            (first-contents (cdr (assoc :contents first-payload)))
            (first-parts (cdr (assoc :parts (first first-contents))))
            (second-payload (cl-json:decode-json-from-string (first captured-payloads)))
            (second-contents (cdr (assoc :contents second-payload)))
            (stored-history (conversation-messages conv)))
        (fiveam:is (string= "First turn" (cdr (assoc :text (first first-parts)))))
        (fiveam:is (string= "Second turn"
                           (cdr (assoc :text (car (cdr (assoc :parts (third second-contents))))))))
        (fiveam:is (string= "First turn"
                           (cdr (assoc "content" (first stored-history) :test #'string=))))
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
      (let* ((first-payload (cl-json:decode-json-from-string (first captured-payloads)))
             (first-contents (cdr (assoc :contents first-payload)))
             (second-payload (cl-json:decode-json-from-string (second captured-payloads)))
             (second-contents (cdr (assoc :contents second-payload))))
        (fiveam:is (= 3 (length first-contents)))
        (fiveam:is (string= "user" (cdr (assoc :role (first first-contents)))))
        (fiveam:is (string= "Please concisely summarize your knowledge graph."
                           (cdr (assoc :text (car (cdr (assoc :parts (first first-contents))))))))
        (fiveam:is (string= "model" (cdr (assoc :role (second first-contents)))))
        (fiveam:is (string= "Stored persona memory."
                           (cdr (assoc :text (car (cdr (assoc :parts (second first-contents))))))))
        (fiveam:is (string= "First live turn"
                           (cdr (assoc :text (car (cdr (assoc :parts (third first-contents))))))))
        (fiveam:is (= 5 (length second-contents)))
        (fiveam:is (string= "Please concisely summarize your knowledge graph."
                           (cdr (assoc :text (car (cdr (assoc :parts (first second-contents))))))))
        (fiveam:is (string= "Stored persona memory."
                           (cdr (assoc :text (car (cdr (assoc :parts (second second-contents))))))))
        (fiveam:is (string= "First live turn"
                           (cdr (assoc :text (car (cdr (assoc :parts (third second-contents))))))))
        (fiveam:is (string= "Hello from Google non-streaming"
                           (cdr (assoc :text (car (cdr (assoc :parts (fourth second-contents))))))))
        (fiveam:is (string= "Second live turn"
                           (cdr (assoc :text (car (cdr (assoc :parts (fifth second-contents))))))))
        (fiveam:is (= 4 (length (conversation-messages conv))))
        (fiveam:is (string= "Stored persona memory."
                           (conversation-persona-memory conv)))))))

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
          (let* ((first-payload (cl-json:decode-json-from-string (first captured-payloads)))
                 (first-contents (cdr (assoc :contents first-payload)))
                 (first-parts (cdr (assoc :parts (first first-contents))))
                 (inline-data (cdr (assoc :inline-data (second first-parts))))
                 (second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                 (second-contents (cdr (assoc :contents second-payload)))
                 (stored-history (conversation-messages conv)))
            (fiveam:is (= 2 (length first-parts)))
            (fiveam:is (string= "Summarize"
                                (cdr (assoc :text (first first-parts)))))
            (fiveam:is (string= "text/plain"
                                (cdr (assoc :mime-type inline-data))))
            (fiveam:is (string= "Summarize"
                                (cdr (assoc :text (car (cdr (assoc :parts (first second-contents))))))))
            (fiveam:is (notany (lambda (content)
                                 (search ":INLINE-DATA" (princ-to-string content)))
                               second-contents))
            (fiveam:is (= 4 (length stored-history)))
            (fiveam:is (string= "Summarize"
                                (cdr (assoc "content" (first stored-history) :test #'string=))))
            (fiveam:is (notany (lambda (message)
                                 (search "Alpha attachment"
                                         (princ-to-string
                                          (cdr (assoc "content" message :test #'string=)))))
                               stored-history))))
      (uiop:delete-directory-tree root :validate t))))

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
      (let* ((first-payload (cl-json:decode-json-from-string (first captured-payloads)))
             (first-contents (cdr (assoc :contents first-payload)))
             (second-payload (cl-json:decode-json-from-string (second captured-payloads)))
             (second-contents (cdr (assoc :contents second-payload))))
        (fiveam:is (= 1 (length first-contents)))
        (fiveam:is (string= "First turn"
                           (cdr (assoc :text
                                       (car (cdr (assoc :parts (first first-contents))))))))
        (fiveam:is (= 3 (length second-contents)))
        (fiveam:is (string= "First turn"
                           (cdr (assoc :text
                                       (car (cdr (assoc :parts (first second-contents))))))))
        (fiveam:is (string= "model" (cdr (assoc :role (second second-contents)))))
        (fiveam:is (string= "First reply"
                           (cdr (assoc :text
                                       (car (cdr (assoc :parts (second second-contents))))))))
        (fiveam:is (string= "Second turn"
                           (cdr (assoc :text
                                       (car (cdr (assoc :parts (third second-contents))))))))
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
             (let* ((payload (cl-json:decode-json-from-string captured-content))
                    (contents (cdr (assoc :contents payload)))
                    (preloaded-model-msg (second contents)))
               (fiveam:is (= 3 (length contents)))
               (fiveam:is (string= "model" (cdr (assoc :role preloaded-model-msg))))
               (fiveam:is (string= "Stored persona memory."
                                   (cdr (assoc :text
                                              (car (cdr (assoc :parts preloaded-model-msg))))))))))
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
      (let* ((payload (cl-json:decode-json-from-string captured-content))
            (contents (cdr (assoc :contents payload))))
       (fiveam:is (= 5 (length contents)))
       (fiveam:is (string= (format nil "[Diary: 1.txt]~%First diary entry.")
                           (cdr (assoc :text (car (cdr (assoc :parts (third contents))))))))
       (fiveam:is (string= (format nil "[Diary: 2.txt]~%Second diary entry.")
                           (cdr (assoc :text (car (cdr (assoc :parts (fourth contents))))))))
       (fiveam:is (string= "First live turn"
                           (cdr (assoc :text (car (cdr (assoc :parts (fifth contents))))))))))))

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
      (let ((res (chat-google bot "Hi Google" conv nil)))
        (fiveam:is (string= "Hello from Google tool test" res))
        (fiveam:is (search "\"functionDeclarations\"" captured-content))
        (fiveam:is (null (search "\"type\":\"function_declarations\"" captured-content)))
        (fiveam:is (null (search "\"$schema\"" captured-content)))
        (fiveam:is (search "\"properties\":{}" captured-content))))))

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
      (let ((res (chat-google bot "What time is it now?" conv nil)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "America/Los_Angeles" (cdr (assoc :timezone captured-tool-args))))
        (fiveam:is (search "\"thoughtSignature\":\"sig\"" captured-second-request))
        (fiveam:is (search "\"functionCall\":{\"name\":\"get_current_time\",\"args\":{\"timezone\":\"America\\/Los_Angeles\"}"
                           captured-second-request))
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
      (let ((res (chat-google bot "What time is it now?" conv nil)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (search "\"thoughtSignature\":\"sig\"" captured-second-request))
        (fiveam:is (search "\"response\":{\"type\":\"tool_error\",\"toolName\":\"get_current_time\",\"message\":\"Mock tool failure\"}"
                           captured-second-request))
        (fiveam:is (string= "Handled tool error" res))))))

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
      (let ((res (chat-google bot "Retry me" conv nil)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "Recovered on retry" res))
        (fiveam:is (equal '("https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash:generateContent"
                            "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro-latest:generateContent")
                          (nreverse captured-urls)))
        (let* ((first-payload (cl-json:decode-json-from-string (second captured-payloads)))
               (first-contents (cdr (assoc :contents first-payload)))
               (retry-payload (cl-json:decode-json-from-string (first captured-payloads)))
               (retry-contents (cdr (assoc :contents retry-payload))))
          (fiveam:is (string= "[08:46 first] [model: gemini-3.5-flash] Retry me"
                              (cdr (assoc :text
                                          (car (cdr (assoc :parts (first first-contents))))))))
          (fiveam:is (string= "[08:46 retry] [model: gemini-pro-latest] Retry me"
                              (cdr (assoc :text
                                          (car (cdr (assoc :parts (first retry-contents))))))))
          (fiveam:is-false
           (search "[model: gemini-3.5-flash]"
                   (cdr (assoc :text
                               (car (cdr (assoc :parts (first retry-contents)))))))))))))

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
      (let ((res (chat-google bot "Retry empty" conv nil)))
        (fiveam:is (= 2 call-count))
        (fiveam:is (string= "Recovered after empty response" res))
        (fiveam:is (equal '("https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash:generateContent"
                            "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro-latest:generateContent")
                          (nreverse captured-urls)))))))
