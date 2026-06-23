;;; tests-payloads.lisp

(in-package "CHATBOT")
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

(fiveam:test test-initial-interaction-payload-includes-preloaded-messages
  (let ((bot (make-instance 'chatbot :model "gemini-3.5-flash")))
    (let* ((payload (make-interaction-payload bot "Hello"
                                             :messages nil
                                             :persona-memory "Stored persona memory."
                                             :stream t))
           (input (cdr (assoc "input" payload :test #'string=)))
           (first-step (aref input 0))
           (second-step (aref input 1))
           (third-step (aref input 2)))
      (fiveam:is (= 3 (length input)))
      (fiveam:is (string= "user_input" (cdr (assoc "type" first-step :test #'string=))))
      (fiveam:is (string= "model_output" (cdr (assoc "type" second-step :test #'string=))))
      (fiveam:is (string= "user_input" (cdr (assoc "type" third-step :test #'string=))))
      (fiveam:is (string= "Stored persona memory."
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" second-step :test #'string=)) 0)
                                      :test #'string=))))
      (fiveam:is (string= "Hello"
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" third-step :test #'string=)) 0)
                                      :test #'string=)))))))

(fiveam:test test-format-prompt-timestamp
  (fiveam:is (string= "[14:29 26-Jun-2026]"
                      (format-prompt-timestamp
                       (encode-universal-time 0 29 14 26 6 2026 0)
                       0))))

(fiveam:test test-format-prompt-model-indicator
  (fiveam:is (string= "[model: gemini-3-flash]"
                      (format-prompt-model-indicator "gemini-3-flash"))))

(fiveam:test test-request-history-prefixes-current-input-with-timestamp-only
  (let* ((bot (make-instance 'chatbot
                             :model "gemini-3.5-flash"
                             :include-timestamp-p t))
         (stored-messages (list (list (cons "role" "user")
                                      (cons "content" "Earlier question"))
                                (list (cons "role" "assistant")
                                      (cons "content" "Earlier answer"))))
         (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
         (request-messages (build-request-history-messages stored-messages
                                                           "Hello"
                                                           :chatbot bot)))
    (fiveam:is (string= "Earlier question"
                        (cdr (assoc "content" (first request-messages) :test #'string=))))
    (fiveam:is (string= "Earlier answer"
                        (cdr (assoc "content" (second request-messages) :test #'string=))))
    (fiveam:is (string= "[14:29 26-Jun-2026] Hello"
                        (cdr (assoc "content" (third request-messages) :test #'string=))))
    (fiveam:is (= 2 (length stored-messages)))
    (fiveam:is (string= "Earlier question"
                        (cdr (assoc "content" (first stored-messages) :test #'string=))))
    (fiveam:is (string= "Earlier answer"
                        (cdr (assoc "content" (second stored-messages) :test #'string=))))))

(fiveam:test test-request-history-prefixes-current-input-with-timestamp-and-model
  (let* ((bot (make-instance 'chatbot
                             :model "gemini-3.5-flash"
                             :include-timestamp-p t
                             :include-model-p t))
         (stored-messages (list (list (cons "role" "user")
                                      (cons "content" "Earlier question"))
                                (list (cons "role" "assistant")
                                      (cons "content" "Earlier answer"))))
         (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
         (request-messages (build-request-history-messages stored-messages
                                                           "Hello"
                                                           :chatbot bot)))
    (fiveam:is (string= "[14:29 26-Jun-2026] [model: gemini-3.5-flash] Hello"
                        (cdr (assoc "content" (third request-messages) :test #'string=))))
    (fiveam:is (= 2 (length stored-messages)))
    (fiveam:is (string= "Earlier question"
                        (cdr (assoc "content" (first stored-messages) :test #'string=))))
    (fiveam:is (string= "Earlier answer"
                        (cdr (assoc "content" (second stored-messages) :test #'string=))))))

(fiveam:test test-initial-interaction-payload-includes-diary-preload
  (let ((bot (make-instance 'chatbot :model "gemini-3.5-flash")))
    (let* ((payload (make-interaction-payload
                     bot
                     "Hello"
                     :messages nil
                     :persona-memory "Stored persona memory."
                     :persona-diary-entries '(((:filename . "1.txt") (:content . "First diary entry."))
                                              ((:filename . "2.txt") (:content . "Second diary entry.")))
                     :stream t))
           (input (cdr (assoc "input" payload :test #'string=)))
           (third-step (aref input 2))
           (fourth-step (aref input 3))
           (fifth-step (aref input 4)))
      (fiveam:is (= 5 (length input)))
      (fiveam:is (string= "model_output" (cdr (assoc "type" third-step :test #'string=))))
      (fiveam:is (string= (format nil "[Diary: 1.txt]~%First diary entry.")
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" third-step :test #'string=)) 0)
                                      :test #'string=))))
      (fiveam:is (string= (format nil "[Diary: 2.txt]~%Second diary entry.")
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" fourth-step :test #'string=)) 0)
                                      :test #'string=))))
      (fiveam:is (string= "Hello"
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" fifth-step :test #'string=)) 0)
                                      :test #'string=)))))))

(fiveam:test test-initial-interaction-payload-prefixes-current-input-with-timestamp
  (let* ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :include-timestamp-p t))
         (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
         (payload (make-interaction-payload bot "Hello" :messages nil :stream t))
         (input (cdr (assoc "input" payload :test #'string=))))
    (fiveam:is (string= "[14:29 26-Jun-2026] Hello" input))))

(fiveam:test test-initial-interaction-payload-prefixes-current-input-with-timestamp-and-model
  (let* ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :include-timestamp-p t
                            :include-model-p t))
         (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
         (payload (make-interaction-payload bot "Hello" :messages nil :stream t))
         (input (cdr (assoc "input" payload :test #'string=))))
    (fiveam:is (string= "[14:29 26-Jun-2026] [model: gemini-3.5-flash] Hello"
                       input))))

(fiveam:test test-sse-parsing
  (let* ((raw-line "data: {\"event_type\": \"step.delta\", \"delta\": {\"type\": \"text\", \"text\": \"Hello world\"}}")
         (event (parse-sse-event raw-line)))
    (fiveam:is (string= "step.delta" (cdr (assoc :event--type event))))
    (let* ((delta (cdr (assoc :delta event)))
           (text (cdr (assoc :text delta))))
      (fiveam:is (string= "Hello world" text)))))

(fiveam:test test-sse-parsing-signals-on-invalid-json
  (fiveam:signals malformed-json-error
    (parse-sse-event "data: {not valid json}")))

(fiveam:test test-gemini-tool-schema-sanitization
  (let* ((bot (make-instance 'chatbot :model "gemini-3.5-flash"))
         (tool '((:name . "lookup_time")
                 (:description . "Looks up the current time")
                 (:input-schema . ((:|$schema| . "https://json-schema.org/draft/2020-12/schema")
                                   (:type . "object")
                                   (:properties . nil))))))
    (let ((*get-all-mcp-tools-function*
            (lambda (ignored-bot)
              (declare (ignore ignored-bot))
              (list (cons nil tool)))))
      (let* ((payload (make-interaction-payload bot "Hello"))
             (tools (cdr (assoc "tools" payload :test #'string=)))
             (first-tool (car tools))
             (parameters (cdr (assoc "parameters" first-tool :test #'string=)))
             (properties (and (hash-table-p parameters)
                              (gethash "properties" parameters))))
        (fiveam:is (string= "function" (cdr (assoc "type" first-tool :test #'string=))))
        (fiveam:is (hash-table-p parameters))
        (fiveam:is (null (gethash "$schema" parameters)))
        (fiveam:is (hash-table-p properties))
        (fiveam:is (= 0 (hash-table-count properties)))))))

(fiveam:test test-gemini-schema-key-normalization
  (let* ((schema '((:type . "object")
                   (:properties
                    (:entity-name ((:type . "string")))
                    (:entity-type ((:type . "string")))
                    (:source--timezone ((:type . "string")))
                    (:next-thought-needed ((:type . "boolean"))))
                   (:required "entityName" "entityType" "source_timezone" "nextThoughtNeeded")))
         (parameters (gemini-tool-parameters schema))
         (properties (gethash "properties" parameters))
         (required (gethash "required" parameters)))
    (fiveam:is (hash-table-p properties))
    (fiveam:is (not (null (gethash "entityName" properties))))
    (fiveam:is (not (null (gethash "entityType" properties))))
    (fiveam:is (not (null (gethash "source_timezone" properties))))
    (fiveam:is (not (null (gethash "nextThoughtNeeded" properties))))
    (fiveam:is (equal '("entityName" "entityType" "source_timezone" "nextThoughtNeeded")
                      (coerce required 'list)))))

(fiveam:test test-interaction-payload-includes-mcp-tools
  (let* ((bot (make-instance 'chatbot :model "gemini-3.5-flash"))
        (tool '((:name . "lookup_time")
                (:description . "Looks up the current time")
                (:input-schema . ((:type . "object")
                                  (:properties . nil))))))
    (let ((*get-all-mcp-tools-function*
           (lambda (ignored-bot)
             (declare (ignore ignored-bot))
             (list (cons nil tool)))))
     (let* ((payload (make-interaction-payload bot "Hello"))
            (tools (cdr (assoc "tools" payload :test #'string=)))
            (first-tool (car tools)))
       (fiveam:is (= 1 (length tools)))
       (fiveam:is (string= "function" (cdr (assoc "type" first-tool :test #'string=))))
       (fiveam:is (string= "lookup_time" (cdr (assoc "name" first-tool :test #'string=))))
       (fiveam:is (string= "Looks up the current time"
                           (cdr (assoc "description" first-tool :test #'string=))))))))

(fiveam:test test-payload-builders-include-read-file-lines-when-enabled
  (let* ((bot (make-instance 'chatbot
                             :model "gemini-3.5-flash"
                             :filesystem-tools-p t))
         (interaction-tools (interaction-request-tools bot))
         (openai-tools (openai-request-tools bot))
         (google-tools (generate-content-request-tools bot)))
    (fiveam:is (find "readFileLines"
                     interaction-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is (find "readFileLines"
                     openai-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name"
                                        (cdr (assoc "function" tool :test #'string=))
                                        :test #'string=)))))
    (fiveam:is (search "\"readFileLines\""
                       (cl-json:encode-json-to-string google-tools)))
    (fiveam:is (find "directory"
                     interaction-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is (find "directory"
                     openai-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name"
                                        (cdr (assoc "function" tool :test #'string=))
                                        :test #'string=)))))
    (fiveam:is (search "\"directory\""
                       (cl-json:encode-json-to-string google-tools)))
    (fiveam:is (find "writeFile"
                     interaction-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is (find "writeFile"
                     openai-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name"
                                        (cdr (assoc "function" tool :test #'string=))
                                        :test #'string=)))))
    (fiveam:is (search "\"writeFile\""
                       (cl-json:encode-json-to-string google-tools)))
    (fiveam:is (find "deleteFile"
                     interaction-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is (find "deleteFile"
                     openai-tools
                     :test #'string=
                     :key (lambda (tool)
                            (cdr (assoc "name"
                                        (cdr (assoc "function" tool :test #'string=))
                                        :test #'string=)))))
    (fiveam:is (search "\"deleteFile\""
                       (cl-json:encode-json-to-string google-tools)))))

(fiveam:test test-payload-builders-exclude-read-file-lines-when-disabled
  (let* ((bot (make-instance 'chatbot
                             :model "gemini-3.5-flash"
                             :filesystem-tools-p nil)))
    (fiveam:is-false (find "readFileLines"
                           (interaction-request-tools bot)
                           :test #'string=
                           :key (lambda (tool)
                                  (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is-false (openai-request-tools bot))
    (fiveam:is-false (generate-content-request-tools bot))))

(fiveam:test test-payload-builders-include-eval-when-enabled
  (let* ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :enable-eval-p t))
        (interaction-tools (interaction-request-tools bot))
        (openai-tools (openai-request-tools bot))
        (google-tools (generate-content-request-tools bot)))
    (fiveam:is (find "eval"
                    interaction-tools
                    :test #'string=
                    :key (lambda (tool)
                           (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is (find "eval"
                    openai-tools
                    :test #'string=
                    :key (lambda (tool)
                           (cdr (assoc "name"
                                       (cdr (assoc "function" tool :test #'string=))
                                       :test #'string=)))))
    (fiveam:is (search "\"eval\""
                      (cl-json:encode-json-to-string google-tools)))))

(fiveam:test test-payload-builders-include-web-grounding-tools-when-enabled
  (let* ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :web-tools-p t))
         (interaction-tools (interaction-request-tools bot))
         (openai-tools (openai-request-tools bot))
         (google-tools (generate-content-request-tools bot)))
    (fiveam:is (find "webSearch"
                     interaction-tools
                     :test #'string=
                     :key (lambda (tool)
                           (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is (find "hyperspecSearch"
                     interaction-tools
                     :test #'string=
                     :key (lambda (tool)
                           (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is (find "webSearch"
                     openai-tools
                     :test #'string=
                     :key (lambda (tool)
                           (cdr (assoc "name"
                                       (cdr (assoc "function" tool :test #'string=))
                                       :test #'string=)))))
    (fiveam:is (find "hyperspecSearch"
                     openai-tools
                     :test #'string=
                     :key (lambda (tool)
                           (cdr (assoc "name"
                                       (cdr (assoc "function" tool :test #'string=))
                                       :test #'string=)))))
    (fiveam:is (search "\"webSearch\""
                      (cl-json:encode-json-to-string google-tools)))
    (fiveam:is (search "\"hyperspecSearch\""
                      (cl-json:encode-json-to-string google-tools)))))
