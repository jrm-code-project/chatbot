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
      (assert-json-field= payload "model" "gemini-3.5-flash")
      (assert-json-field= payload "input" "Hello")
      (assert-json-field= payload "stream" t)
      (assert-json-field= payload "previous_interaction_id" "session-123")
      (assert-json-field= payload "system_instruction" "Be helpful")
      (let ((tools (mcp-val "tools" payload)))
        (fiveam:is (find "google_search"
                         tools
                         :test #'string=
                         :key (lambda (tool)
                                (mcp-val "type" tool))))))))

(fiveam:test test-resolve-effective-generation-config-prefers-turn-overrides
  (let ((bot (make-instance 'chatbot
                           :model "gemini-3.5-flash"
                           :temperature 0.4d0
                           :top-p 0.8d0)))
    (let ((config (resolve-effective-generation-config bot
                                                       :temperature 0.9d0
                                                       :top-p 0.6d0)))
      (fiveam:is (= 0.9d0 (getf config :temperature)))
      (fiveam:is (= 0.6d0 (getf config :top-p))))
    (let ((config (resolve-effective-generation-config bot)))
      (fiveam:is (= 0.4d0 (getf config :temperature)))
      (fiveam:is (= 0.8d0 (getf config :top-p))))))

(fiveam:test test-interaction-payload-includes-generation-config
  (let* ((bot (make-instance 'chatbot :model "gemini-3.5-flash"))
         (payload (make-interaction-payload bot
                                            "Hello"
                                            :effective-generation-config '(:temperature 0.7d0 :top-p 0.9d0)))
         (generation-config (mcp-val "generation_config" payload)))
    (assert-json-field= generation-config "temperature" 0.7d0)
    (assert-json-field= generation-config "top_p" 0.9d0)))

(fiveam:test test-interaction-payload-joins-system-instruction-paragraph-vectors
  (let ((bot (make-instance 'chatbot
                          :model "gemini-3.5-flash"
                          :system-instruction #("First paragraph." "Second paragraph."))))
    (let ((payload (make-interaction-payload bot "Hello" :previous-interaction-id "session-123" :stream t))
          (expected (format nil "First paragraph.~%~%Second paragraph.")))
      (assert-json-field= payload "system_instruction" expected))))

(fiveam:test test-initial-interaction-payload-includes-preloaded-messages
  (let ((bot (make-instance 'chatbot :model "gemini-3.5-flash")))
    (let* ((payload (make-interaction-payload bot "Hello"
                                              :messages nil
                                              :persona-memory "Stored persona memory."
                                              :stream t))
           (input (mcp-val "input" payload)))
       (fiveam:is (stringp input))
       (fiveam:is (string= "Hello" input)))))

(fiveam:test test-resolve-chat-input-files-expands-directories-wildcards-and-deduplicates
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "chat-files-expand/" temp-dir))
        (nested (merge-pathnames "nested/" root))
        (deeper (merge-pathnames "nested\\deeper/" root))
        (alpha (merge-pathnames "alpha.txt" root))
        (beta (merge-pathnames "beta.txt" root))
        (gamma (merge-pathnames "gamma.txt" nested))
        (delta (merge-pathnames "delta.txt" deeper)))
    (ensure-directories-exist deeper)
    (dolist (path (list alpha beta gamma delta))
      (with-open-file (stream path :direction :output :if-exists :supersede)
       (write-line (file-namestring path) stream)))
    (unwind-protect
        (let* ((resolved (resolve-chat-input-files
                          (list alpha
                                (merge-pathnames "*.txt" root)
                                nested)))
               (resolved-names (mapcar #'file-namestring resolved)))
          (fiveam:is (equal '("alpha.txt" "beta.txt" "gamma.txt" "delta.txt")
                            resolved-names)))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-resolve-chat-input-files-expands-home-relative-wildcards
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-chat-files/" temp-dir))
         (shots-dir (merge-pathnames "Pictures/" mock-home))
         (alpha (merge-pathnames "alpha.txt" shots-dir))
         (beta (merge-pathnames "beta.txt" shots-dir)))
    (ensure-directories-exist shots-dir)
    (dolist (path (list alpha beta))
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (write-line (file-namestring path) stream)))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((resolved (resolve-chat-input-files (list #p"~/Pictures/*.txt"))))
            (fiveam:is (equal '("alpha.txt" "beta.txt")
                              (mapcar #'file-namestring resolved)))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-json-encodable-value-preserves-array-of-objects
  (let* ((value '((:observations ((:text . "first"))
                                 ((:text . "second")))))
         (encoded (json-encodable-value value))
         (observations (gethash "observations" encoded)))
    (fiveam:is (hash-table-p encoded))
    (fiveam:is (listp observations))
    (fiveam:is (= 2 (length observations)))
    (fiveam:is (hash-table-p (first observations)))
    (fiveam:is (string= "first" (gethash "text" (first observations))))
    (fiveam:is (hash-table-p (second observations)))
    (fiveam:is (string= "second" (gethash "text" (second observations))))))

(fiveam:test test-openai-request-messages-include-text-file-attachments-as-text-parts
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "chat-files-openai-builder/" temp-dir))
        (file-path (merge-pathnames "note.txt" root)))
    (ensure-directories-exist root)
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (write-string "Alpha attachment" stream))
    (unwind-protect
        (let* ((attachments (prepare-chat-file-attachments (list file-path)))
               (messages (build-openai-request-messages
                          nil
                          nil
                          "Summarize"
                          :chatbot (make-instance 'chatbot :model "gemini-3.5-flash")
                          :file-attachments attachments))
               (content (cdr (assoc "content" (first messages) :test #'string=))))
          (fiveam:is (= 2 (length content)))
          (fiveam:is (string= "text"
                              (cdr (assoc "type" (first content) :test #'string=))))
          (fiveam:is (string= "Summarize"
                              (cdr (assoc "text" (first content) :test #'string=))))
          (fiveam:is (search "Alpha attachment"
                             (cdr (assoc "text" (second content) :test #'string=)))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-generate-content-and-interaction-builders-include-file-blobs
  (let* ((temp-dir (uiop:default-temporary-directory))
        (root (merge-pathnames "chat-files-gemini-builder/" temp-dir))
        (file-path (merge-pathnames "note.txt" root))
        (bot (make-instance 'chatbot :model "gemini-3.5-flash")))
    (ensure-directories-exist root)
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (write-string "Alpha attachment" stream))
    (unwind-protect
        (let* ((attachments (prepare-chat-file-attachments (list file-path)))
               (contents (build-generate-content-request-contents nil
                                                                  "Summarize"
                                                                  :chatbot bot
                                                                  :file-attachments attachments))
               (payload (make-interaction-payload bot
                                                  "Summarize"
                                                  :file-attachments attachments
                                                  :stream t))
               (generate-parts (cdr (assoc "parts" (first contents) :test #'string=)))
               (interaction-input (cdr (assoc "input" payload :test #'string=)))
               (user-step (aref interaction-input 0))
               (interaction-parts (cdr (assoc "content" user-step :test #'string=))))
          (fiveam:is (= 2 (length generate-parts)))
          (fiveam:is (string= "Summarize"
                              (cdr (assoc "text" (aref generate-parts 0) :test #'string=))))
          (let ((inline-data (cdr (assoc "inlineData" (aref generate-parts 1) :test #'string=))))
            (fiveam:is (string= "text/plain"
                                (cdr (assoc "mimeType" inline-data :test #'string=))))
            (fiveam:is (stringp (cdr (assoc "data" inline-data :test #'string=)))))
          (fiveam:is (= 2 (length interaction-parts)))
          (fiveam:is (string= "text"
                              (cdr (assoc "type" (aref interaction-parts 0) :test #'string=))))
          (fiveam:is (string= "document"
                              (cdr (assoc "type" (aref interaction-parts 1) :test #'string=))))
          (fiveam:is (string= "text/plain"
                              (cdr (assoc "mime_type" (aref interaction-parts 1) :test #'string=)))))
      (uiop:delete-directory-tree root :validate t))))

(fiveam:test test-attachment-content-type-info-centralizes-textual-and-interaction-policy
  (let* ((json-path (make-pathname :name "memory" :type "json"))
        (png-path (make-pathname :name "diagram" :type "png"))
        (unknown-path (make-pathname :name "blob" :type "bin"))
        (json-info (attachment-content-type-info :pathname json-path))
        (png-info (attachment-content-type-info :pathname png-path))
        (unknown-info (attachment-content-type-info :pathname unknown-path)))
    (fiveam:is (string= "application/json" (getf json-info :mime-type)))
    (fiveam:is-true (getf json-info :textual-p))
    (fiveam:is (string= "document" (getf json-info :interaction-type)))
    (fiveam:is (string= "image/png" (getf png-info :mime-type)))
    (fiveam:is-false (getf png-info :textual-p))
    (fiveam:is (string= "image" (getf png-info :interaction-type)))
    (fiveam:is (string= "application/octet-stream" (getf unknown-info :mime-type)))
    (fiveam:is-false (getf unknown-info :textual-p))
    (fiveam:is (string= "document" (getf unknown-info :interaction-type)))))

(fiveam:test test-pathname-content-type-rules-group-aliases-without-duplicates
  (let* ((extensions (mapcar #'car +pathname-content-type-policies+))
         (yaml-policy (pathname-content-type-policy (make-pathname :name "config" :type "yaml")))
         (yml-policy (pathname-content-type-policy (make-pathname :name "config" :type "yml")))
         (js-policy (pathname-content-type-policy (make-pathname :name "app" :type "js")))
         (mjs-policy (pathname-content-type-policy (make-pathname :name "app" :type "mjs"))))
    (fiveam:is (= (length extensions)
                  (length (remove-duplicates extensions :test #'string=))))
    (fiveam:is (equal yaml-policy yml-policy))
    (fiveam:is (equal js-policy mjs-policy))
    (fiveam:is (string= "application/yaml" (getf yaml-policy :mime-type)))
    (fiveam:is-true (getf yaml-policy :textual-p))))

(fiveam:test test-textual-mime-fallback-is-derived-from-grouped-rules
  (let ((json-info (attachment-content-type-info :mime-type "application/json"))
        (javascript-info (attachment-content-type-info :mime-type "application/javascript"))
        (binary-info (attachment-content-type-info :mime-type "application/octet-stream")))
    (fiveam:is (member "application/json" +textual-mime-types+ :test #'string=))
    (fiveam:is (member "application/javascript" +textual-mime-types+ :test #'string=))
    (fiveam:is-true (getf json-info :textual-p))
    (fiveam:is-true (getf javascript-info :textual-p))
    (fiveam:is-false (getf binary-info :textual-p))))

(fiveam:test test-pathname-content-type-rule-validation-rejects-malformed-rules
  (fiveam:signals error
    (validate-pathname-content-type-rules
     '((:mime-type "" :extensions ("txt")))))
  (fiveam:signals error
    (validate-pathname-content-type-rules
     '((:mime-type "text/plain" :extensions ()))))
  (fiveam:signals error
    (validate-pathname-content-type-rules
     '((:mime-type "text/plain" :extensions ("txt" "txt")))))
  (fiveam:signals error
    (validate-pathname-content-type-rules
     '((:mime-type "text/plain" :extensions ("txt"))
       (:mime-type "application/json" :extensions ("txt"))))))

(fiveam:test test-format-prompt-timestamp
  (fiveam:is (string= "[14:29 26-Jun-2026]"
                     (format-prompt-timestamp
                       (encode-universal-time 0 29 14 26 6 2026 0)
                       0))))

(fiveam:test test-format-prompt-model-indicator
  (fiveam:is (string= "[model: gemini-3-flash]"
                      (format-prompt-model-indicator "gemini-3-flash"))))

(fiveam:test test-resolve-prompt-model-override-only-for-google-and-gemini
  (let ((gemini-bot (make-instance 'chatbot :backend :gemini :model "gemini-3.5-flash"))
        (google-bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
        (openai-bot (make-instance 'chatbot :backend :openai :model "gpt-4o")))
    (multiple-value-bind (input effective-model)
        (resolve-prompt-model-override gemini-bot "$Hello")
      (fiveam:is (string= "Hello" input))
      (fiveam:is (string= "gemini-pro-latest" effective-model)))
    (multiple-value-bind (input effective-model)
        (resolve-prompt-model-override google-bot "$Hello")
      (fiveam:is (string= "Hello" input))
      (fiveam:is (string= "gemini-pro-latest" effective-model)))
    (multiple-value-bind (input effective-model)
        (resolve-prompt-model-override openai-bot "$Hello")
      (fiveam:is (string= "$Hello" input))
      (fiveam:is (null effective-model)))))

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
    (fiveam:is (string= (format nil "Hello~%~%=== Dynamic Context ===~%[14:29 26-Jun-2026]")
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
    (fiveam:is (string= (format nil "Hello~%~%=== Dynamic Context ===~%[14:29 26-Jun-2026] [model: gemini-3.5-flash]")
                        (cdr (assoc "content" (third request-messages) :test #'string=))))
    (fiveam:is (= 2 (length stored-messages)))
    (fiveam:is (string= "Earlier question"
                        (cdr (assoc "content" (first stored-messages) :test #'string=))))
    (fiveam:is (string= "Earlier answer"
                        (cdr (assoc "content" (second stored-messages) :test #'string=))))))

(fiveam:test test-request-history-prefixes-current-input-with-overridden-model
  (let* ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :include-model-p t))
        (request-messages (build-request-history-messages nil
                                                          "Hello"
                                                          :chatbot bot
                                                          :effective-model "gemini-pro-latest")))
    (fiveam:is (string= (format nil "Hello~%~%=== Dynamic Context ===~%[model: gemini-pro-latest]")
                       (cdr (assoc "content" (first request-messages) :test #'string=))))))

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
           (first-step (aref input 0))
           (second-step (aref input 1))
           (third-step (aref input 2)))
      (fiveam:is (= 3 (length input)))
      (fiveam:is (string= "model_output" (cdr (assoc "type" first-step :test #'string=))))
      (fiveam:is (string= (format nil "[Diary: 1.txt]~%First diary entry.")
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" first-step :test #'string=)) 0)
                                      :test #'string=))))
      (fiveam:is (string= (format nil "[Diary: 2.txt]~%Second diary entry.")
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" second-step :test #'string=)) 0)
                                      :test #'string=))))
      (fiveam:is (string= "Hello"
                          (cdr (assoc "text"
                                      (aref (cdr (assoc "content" third-step :test #'string=)) 0)
                                      :test #'string=)))))))

(fiveam:test test-initial-interaction-payload-prefixes-current-input-with-timestamp
  (let* ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :include-timestamp-p t))
         (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
         (payload (make-interaction-payload bot "Hello" :messages nil :stream t))
         (input (cdr (assoc "input" payload :test #'string=))))
    (fiveam:is (string= (format nil "Hello~%~%=== Dynamic Context ===~%[14:29 26-Jun-2026]") input))))

(fiveam:test test-initial-interaction-payload-prefixes-current-input-with-timestamp-and-model
  (let* ((bot (make-instance 'chatbot
                            :model "gemini-3.5-flash"
                            :include-timestamp-p t
                            :include-model-p t))
         (*prompt-timestamp-function* (lambda () "[14:29 26-Jun-2026]"))
         (payload (make-interaction-payload bot "Hello" :messages nil :stream t))
         (input (cdr (assoc "input" payload :test #'string=))))
    (fiveam:is (string= (format nil "Hello~%~%=== Dynamic Context ===~%[14:29 26-Jun-2026] [model: gemini-3.5-flash]")
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

(fiveam:test test-sse-parsing-ignores-done-sentinel
  (fiveam:is-false (parse-sse-event "data: [DONE]")))

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
             (lookup-tool (find "lookup_time"
                                tools
                                :test #'string=
                                :key (lambda (entry)
                                       (cdr (assoc "name" entry :test #'string=))))
             )
             (parameters (cdr (assoc "parameters" lookup-tool :test #'string=)))
             (properties (and (hash-table-p parameters)
                              (gethash "properties" parameters))))
        (fiveam:is (string= "function" (cdr (assoc "type" lookup-tool :test #'string=))))
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
            (lookup-tool (find "lookup_time"
                               tools
                               :test #'string=
                               :key (lambda (entry)
                                      (cdr (assoc "name" entry :test #'string=))))))
       (fiveam:is (string= "function" (cdr (assoc "type" lookup-tool :test #'string=))))
       (fiveam:is (string= "lookup_time" (cdr (assoc "name" lookup-tool :test #'string=))))
       (fiveam:is (string= "Looks up the current time"
                           (cdr (assoc "description" lookup-tool :test #'string=))))))))

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
    (fiveam:is (member "readFileLines"
                       (google-tool-names google-tools)
                       :test #'string=))
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
    (fiveam:is (member "directory"
                       (google-tool-names google-tools)
                       :test #'string=))
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
    (fiveam:is (member "writeFile"
                       (google-tool-names google-tools)
                       :test #'string=))
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
    (fiveam:is (member "deleteFile"
                       (google-tool-names google-tools)
                       :test #'string=))))

(fiveam:test test-payload-builders-exclude-read-file-lines-when-disabled
  (let* ((bot (make-instance 'chatbot
                             :model "gemini-3.5-flash"
                             :filesystem-tools-p nil)))
    (fiveam:is-false (find "readFileLines"
                           (interaction-request-tools bot)
                           :test #'string=
                           :key (lambda (tool)
                                  (cdr (assoc "name" tool :test #'string=)))))
    (fiveam:is-false (find "readFileLines"
                           (openai-request-tools bot)
                           :test #'string=
                           :key (lambda (tool)
                                  (cdr (assoc "name"
                                              (cdr (assoc "function" tool :test #'string=))
                                              :test #'string=)))))
    (fiveam:is-false (member "readFileLines"
                             (google-tool-names (generate-content-request-tools bot))
                             :test #'string=))))

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
    (fiveam:is (member "eval"
                      (google-tool-names google-tools)
                      :test #'string=))))

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
    (fiveam:is (member "webSearch"
                       (google-tool-names google-tools)
                       :test #'string=))
    (fiveam:is (member "hyperspecSearch"
                       (google-tool-names google-tools)
                       :test #'string=))))

(fiveam:test test-persona-diary-messages-keeps-only-most-recent-8
  (let* ((entries (loop for i from 1 to 12
                        collect `((:filename . ,(format nil "~D.txt" i))
                                  (:content . ,(format nil "Diary entry ~D." i)))))
         (messages (persona-diary-messages entries)))
    (fiveam:is (= 8 (length messages)))
    (fiveam:is (string= (format nil "[Diary: 5.txt]~%Diary entry 5.")
                        (cdr (assoc "content" (first messages) :test #'string=))))
    (fiveam:is (string= (format nil "[Diary: 12.txt]~%Diary entry 12.")
                        (cdr (assoc "content" (car (last messages)) :test #'string=))))))
