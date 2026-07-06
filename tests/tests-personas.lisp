;;; tests-personas.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-persona-preload-uses-model-role-canonically
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-model-role/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "test-persona-model-role/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"gpt-4o\")" s))
    (with-open-file (s (merge-pathnames "compressed-memory.txt" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Stored persona memory." s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let ((conv (new-chat-persona "test-persona-model-role")))
           (fiveam:is (null (conversation-messages conv)))
           (fiveam:is (string= "Stored persona memory."
                               (conversation-persona-memory conv)))
           (fiveam:is (null (conversation-persona-diary-entries conv)))
           (fiveam:is (eq :gemini (chatbot-backend (conversation-chatbot conv))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-resolve-persona-directory-can-create-missing-persona-via-restart
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-create-missing-persona/" temp-dir))
        (expected-dir (merge-pathnames ".Personas/Splat/" mock-home)))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((resolved
                  (handler-bind ((persona-directory-not-found
                                   (lambda (condition)
                                     (declare (ignore condition))
                                     (invoke-restart 'create-persona-directory))))
                    (resolve-persona-directory "Splat"))))
            (fiveam:is (equal expected-dir resolved))
            (fiveam:is (not (null (uiop:directory-exists-p expected-dir))))))
      (when (uiop:directory-exists-p mock-home)
       (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-resolve-persona-directory-can-use-alternate-directory-via-restart
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-alternate-persona/" temp-dir))
        (alternate-dir (merge-pathnames "alternate-persona/" temp-dir)))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((resolved
                  (handler-bind ((persona-directory-not-found
                                   (lambda (condition)
                                     (declare (ignore condition))
                                     (invoke-restart 'use-value alternate-dir))))
                    (resolve-persona-directory "Splat"))))
            (fiveam:is (equal (uiop:ensure-directory-pathname alternate-dir) resolved))
            (fiveam:is (not (null (uiop:directory-exists-p alternate-dir))))))
      (when (uiop:directory-exists-p mock-home)
       (uiop:delete-directory-tree mock-home :validate t))
      (when (uiop:directory-exists-p alternate-dir)
       (uiop:delete-directory-tree alternate-dir :validate t)))))

(fiveam:test test-resolve-persona-directory-can-skip-restoring-missing-persona
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-skip-missing-persona/" temp-dir))
        (expected-dir (merge-pathnames ".Personas/Splat/" mock-home)))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((resolved
                  (handler-bind ((persona-directory-not-found
                                   (lambda (condition)
                                     (declare (ignore condition))
                                     (invoke-restart 'skip-persona-restore))))
                    (resolve-persona-directory "Splat"))))
            (fiveam:is-false resolved)
            (fiveam:is-false (uiop:directory-exists-p expected-dir))))
      (when (uiop:directory-exists-p mock-home)
       (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-new-chat-persona-can-recover-by-creating-missing-directory
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-create-persona-chat/" temp-dir))
        (expected-dir (merge-pathnames ".Personas/Splat/" mock-home)))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((conversation
                  (handler-bind ((persona-directory-not-found
                                   (lambda (condition)
                                     (declare (ignore condition))
                                     (invoke-restart 'create-persona-directory))))
                    (new-chat-persona "Splat"))))
            (fiveam:is (equal expected-dir
                              (resolve-persona-directory "Splat")))
            (fiveam:is (null (conversation-messages conversation)))
            (fiveam:is (eq :gemini
                           (chatbot-backend (conversation-chatbot conversation))))))
      (when (uiop:directory-exists-p mock-home)
       (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-new-chat-persona-can-skip-restoring-missing-persona
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-skip-persona-chat/" temp-dir))
        (expected-dir (merge-pathnames ".Personas/Splat/" mock-home)))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((conversation
                  (handler-bind ((persona-directory-not-found
                                   (lambda (condition)
                                     (declare (ignore condition))
                                     (invoke-restart 'skip-persona-restore))))
                    (new-chat-persona "Splat"))))
            (fiveam:is-false (uiop:directory-exists-p expected-dir))
            (fiveam:is (null (conversation-messages conversation)))
            (fiveam:is-false (chatbot-persona-name (conversation-chatbot conversation)))
            (fiveam:is (eq :gemini
                           (chatbot-backend (conversation-chatbot conversation))))))
      (when (uiop:directory-exists-p mock-home)
       (uiop:delete-directory-tree mock-home :validate t)))))

(fiveam:test test-persona-config-reader-supports-top-level-plist-forms
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-top-level-plist/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "top-level-plist/" personas-dir))
        (config-path (merge-pathnames "config.lisp" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s config-path
                      :direction :output
                      :if-exists :supersede)
      (write-line ";;; -*- Lisp -*-" s)
      (write-line "" s)
      (write-line ":model \"models/gemini-flash-latest\"" s)
      (write-line ":googleapi :google-api" s)
      (write-line ":enable-web-tools t" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((config (read-persona-config config-path))
                (conv (new-chat-persona "top-level-plist")))
            (fiveam:is (string= "models/gemini-flash-latest"
                                (safe-getf config :model)))
            (fiveam:is (eq :google-api (safe-getf config :googleapi)))
            (fiveam:is-true (safe-getf config :enable-web-tools))
            (fiveam:is-false (chatbot-filesystem-tools-p (conversation-chatbot conv)))
            (fiveam:is (eq :google
                           (chatbot-backend (conversation-chatbot conv))))
            (fiveam:is (string= "models/gemini-flash-latest"
                                (chatbot-model (conversation-chatbot conv))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-preload-logs-memory-source
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-preload-log/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-preload-log/" personas-dir)))
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
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let ((output (with-output-to-string (s)
                           (let ((context (make-runtime-context :logging-enabled-p t
                                                                :log-level :info
                                                                :log-stream s)))
                             (call-with-runtime-context
                              context
                              (lambda ()
                                (new-chat-persona "persona-preload-log")))))))
             (fiveam:is (search "Loading persona memory preload" output))
             (fiveam:is (search "source: compressed-memory.txt" output))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-loading
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "test-persona/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "system-instruction.md" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "You are a helpful test assistant." s))
    (with-open-file (s (merge-pathnames "compressed-memory.txt" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "Stored persona memory." s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let ((conv (new-chat-persona "test-persona")))
             (let ((bot (conversation-chatbot conv))
                   (messages (conversation-messages conv)))
               (fiveam:is (eq :google (chatbot-backend bot)))
               (fiveam:is (string= "models/gemini-mock-model" (chatbot-model bot)))
               (fiveam:is (not (null (search "You are a helpful test assistant." (chatbot-system-instruction bot)))))
               (fiveam:is (null messages))
               (fiveam:is (null (conversation-persona-diary-entries conv)))
               (fiveam:is (string= "Stored persona memory."
                                   (conversation-persona-memory conv))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-loading-supports-lm-studio-backend
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-lm-studio/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-lm-studio/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:backend :lm-studio :model \"gemma-4-e4b-uncensored-hauhaucs-aggressive\")" s))
    (with-open-file (s (merge-pathnames "system-instruction.md" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "You are direct and concise." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let ((conv (new-chat-persona "persona-lm-studio")))
            (let ((bot (conversation-chatbot conv)))
              (fiveam:is (eq :lm-studio (chatbot-backend bot)))
              (fiveam:is (string= "gemma-4-e4b-uncensored-hauhaucs-aggressive"
                                  (chatbot-model bot)))
              (fiveam:is (search "You are direct and concise."
                                 (chatbot-system-instruction bot))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-system-instructions-file-loads-paragraph-vector
  (let* ((temp-dir (uiop:default-temporary-directory))
       (mock-home (merge-pathnames "mock-home-system-instructions/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-system-instructions/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "system-instructions" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-string "First instruction paragraph.

Second instruction paragraph." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "persona-system-instructions"))
                 (bot (conversation-chatbot conv))
                 (system-instruction (chatbot-system-instruction bot)))
            (fiveam:is (vectorp system-instruction))
            (fiveam:is (= 2 (length system-instruction)))
            (fiveam:is (string= "First instruction paragraph." (aref system-instruction 0)))
            (fiveam:is (string= "Second instruction paragraph." (aref system-instruction 1)))
            (fiveam:is (= 2 (system-instruction-paragraph-count conv)))
            (fiveam:is (string= "First instruction paragraph."
                                (system-instruction-paragraph conv 0)))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-loads-sampling-parameters
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-persona-sampling/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-sampling/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api :temperature 0.7 :top-p 0.9)" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "persona-sampling"))
                (parameters (sampling-parameters conv)))
            (fiveam:is (< (abs (- (getf parameters :temperature) 0.7d0)) 1d-5))
            (fiveam:is (< (abs (- (getf parameters :top-p) 0.9d0)) 1d-5))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-overrides-agentic-loop-default-profile
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-persona-loop-defaults/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-loop-defaults/" personas-dir))
        (observed-backend nil)
        (observed-model nil)
        (*agentic-loop-chat-function*
         (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt callback file files temperature top-p))
           (setf observed-backend (chatbot-backend (conversation-chatbot conversation)))
           (setf observed-model (chatbot-model (conversation-chatbot conversation)))
           (cl-json:encode-json-to-string '(("status" . "final")
                                            ("summary" . "persona loop defaults applied"))))))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:backend :google" s)
      (write-line " :model \"gemini-3.5-flash\"" s)
      (write-line " :agentic-loop-default-backend \"openai\"" s)
      (write-line " :agentic-loop-default-model \"gpt-4o-mini\")" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((base-context (make-runtime-context :agentic-loop-default-backend :lm-studio
                                                     :agentic-loop-default-model "base-loop-model"))
                 (conv (new-chat-persona "persona-loop-defaults" :runtime-context base-context))
                 (runtime-context (chatbot-runtime-context (conversation-chatbot conv))))
            (fiveam:is (eq :openai (current-agentic-loop-default-backend runtime-context)))
            (fiveam:is (string= "gpt-4o-mini"
                                (current-agentic-loop-default-model runtime-context)))
            (let ((loop (start-agentic-loop conv "Use persona loop defaults" :max-iterations 2)))
              (fiveam:is (eq :completed
                            (wait-for-agentic-loop-status loop
                                                          '(:completed :failed :limit-reached)
                                                          :timeout-seconds 10.0d0)))
              (fiveam:is (eq :openai observed-backend))
              (fiveam:is (string= "gpt-4o-mini" observed-model)))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops)
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-system-instruction-crud-api-updates-paragraph-vectors
  (let* ((temp-dir (uiop:default-temporary-directory))
       (mock-home (merge-pathnames "mock-home-system-instruction-crud/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-system-instruction-crud/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "system-instructions" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-string "First paragraph.

Second paragraph." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
         (let ((conv (new-chat-persona "persona-system-instruction-crud")))
           (fiveam:is (equal '("First paragraph." "Second paragraph.")
                             (coerce (system-instruction-paragraphs-copy conv) 'list)))
           (insert-system-instruction-paragraph conv "Inserted paragraph." :index 1)
           (update-system-instruction-paragraph conv 0 "Updated first paragraph.")
           (delete-system-instruction-paragraph conv 2)
           (fiveam:is (equal '("Updated first paragraph." "Inserted paragraph.")
                             (coerce (system-instruction-paragraphs-copy conv) 'list)))
           (clear-system-instruction-paragraphs conv)
           (fiveam:is (= 0 (system-instruction-paragraph-count conv)))
           (replace-system-instruction-paragraphs conv '("Replacement one." "Replacement two."))
           (fiveam:is (equal '("Replacement one." "Replacement two.")
                             (coerce (system-instruction-paragraphs-copy conv) 'list)))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-split-system-instructions-preserves-fenced-blocks
  (let* ((text (format nil "Intro paragraph.~%~%```markdown~%Paragraph one inside fence.~%~%Paragraph two inside fence.~%```~%~%Outro paragraph."))
        (paragraphs (coerce (split-system-instruction-into-paragraphs text) 'list)))
    (fiveam:is (equal (list "Intro paragraph."
                           (format nil "```markdown~%Paragraph one inside fence.~%~%Paragraph two inside fence.~%```")
                           "Outro paragraph.")
                     paragraphs))))

(fiveam:test test-save-system-instructions-persists-paragraph-file
  (let* ((temp-dir (uiop:default-temporary-directory))
       (mock-home (merge-pathnames "mock-home-save-system-instructions/" temp-dir))
       (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-save-system-instructions/" personas-dir))
        (inst-path (merge-pathnames "system-instructions" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s inst-path
                      :direction :output
                      :if-exists :supersede)
      (write-string "First paragraph.

Second paragraph." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
         (let ((conv (new-chat-persona "persona-save-system-instructions")))
           (replace-system-instruction-paragraphs conv '("Saved one." "Saved two."))
           (fiveam:is (equal inst-path (save-system-instructions conv)))
           (fiveam:is (string= (format nil "Saved one.~%~%Saved two.")
                               (uiop:read-file-string inst-path)))
           (let ((reloaded (new-chat-persona "persona-save-system-instructions")))
             (fiveam:is (equal '("Saved one." "Saved two.")
                               (coerce (system-instruction-paragraphs-copy reloaded) 'list))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-save-system-instructions-persists-markdown-backed-files
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-save-system-instructions-markdown/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-save-system-instructions-markdown/" personas-dir))
        (inst-path (merge-pathnames "system-instructions.md" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s inst-path
                      :direction :output
                      :if-exists :supersede)
      (write-string "Markdown paragraph one.

Markdown paragraph two." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
         (let ((conv (new-chat-persona "persona-save-system-instructions-markdown")))
           (fiveam:is (equal '("Markdown paragraph one." "Markdown paragraph two.")
                             (coerce (system-instruction-paragraphs-copy conv) 'list)))
           (replace-system-instruction-paragraphs conv '("Converted paragraph one."
                                                         "Converted paragraph two."))
           (fiveam:is (equal inst-path (save-system-instructions conv)))
           (fiveam:is (string= (format nil "Converted paragraph one.~%~%Converted paragraph two.")
                               (uiop:read-file-string inst-path)))
           (let ((reloaded (new-chat-persona "persona-save-system-instructions-markdown")))
             (fiveam:is (equal '("Converted paragraph one." "Converted paragraph two.")
                               (coerce (system-instruction-paragraphs-copy reloaded) 'list))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-system-instruction-tools-persist-markdown-backed-persona-files
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-system-instruction-tools/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-system-instruction-tools/" personas-dir))
        (inst-path (merge-pathnames "system-instructions.md" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s inst-path
                      :direction :output
                      :if-exists :supersede)
      (write-string "Paragraph one.

Paragraph two." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
         (let* ((conv (new-chat-persona "persona-system-instruction-tools"))
                (bot (conversation-chatbot conv))
                (tool-names (mapcar (lambda (entry)
                                      (mcp-val :name (cdr entry)))
                                    (default-get-all-builtin-tools bot)))
                (read-payload (cl-json:decode-json-from-string
                               (execute-chatbot-tool-by-name bot "readSystemInstructions" '()))))
           (fiveam:is (member "readSystemInstructions" tool-names :test #'string=))
           (fiveam:is (member "insertSystemInstructionParagraph" tool-names :test #'string=))
           (fiveam:is (member "updateSystemInstructionParagraph" tool-names :test #'string=))
           (fiveam:is (member "deleteSystemInstructionParagraph" tool-names :test #'string=))
           (fiveam:is (member "replaceSystemInstructions" tool-names :test #'string=))
           (fiveam:is (equal '("Paragraph one." "Paragraph two.")
                             (coerce (cdr (assoc :paragraphs read-payload)) 'list)))
           (execute-chatbot-tool-by-name bot
                                         "replaceSystemInstructions"
                                         '(("paragraphs" . #("Tool paragraph one."
                                                             "Tool paragraph two."
                                                             "Tool paragraph three."))))
           (fiveam:is (string= (format nil "Tool paragraph one.~%~%Tool paragraph two.~%~%Tool paragraph three.")
                               (uiop:read-file-string inst-path)))
           (let ((reloaded (new-chat-persona "persona-system-instruction-tools")))
             (fiveam:is (equal '("Tool paragraph one."
                                 "Tool paragraph two."
                                 "Tool paragraph three.")
                               (coerce (system-instruction-paragraphs-copy reloaded) 'list))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-diary-loads-in-sorted-order-at-startup
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-diary-startup/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-diary-startup/" personas-dir))
        (diary-dir (merge-pathnames "Diary/" test-persona-dir)))
    (ensure-directories-exist diary-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "10.txt" diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Tenth diary entry." s))
    (with-open-file (s (merge-pathnames "2.txt" diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Second diary entry." s))
    (with-open-file (s (merge-pathnames "1.txt" diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "First diary entry." s))
    (with-open-file (s (merge-pathnames "Alpha.md" diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Alpha diary entry." s))
    (with-open-file (s (merge-pathnames "beta.md" diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Beta diary entry." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "persona-diary-startup"))
                 (entries (conversation-persona-diary-entries conv)))
            (fiveam:is (= 5 (length entries)))
            (fiveam:is (equal '("1.txt" "2.txt" "10.txt" "Alpha.md" "beta.md")
                              (mapcar (lambda (entry)
                                        (cdr (assoc :filename entry)))
                                      entries)))
            (fiveam:is (string= "First diary entry."
                                (cdr (assoc :content (first entries)))))
            (fiveam:is (string= "Beta diary entry."
                                (cdr (assoc :content (fifth entries)))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-prefers-compressed-diary-over-diary
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-compressed-diary/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-compressed-diary/" personas-dir))
        (diary-dir (merge-pathnames "Diary/" test-persona-dir))
        (compressed-diary-dir (merge-pathnames "CompressedDiary/" test-persona-dir)))
    (ensure-directories-exist diary-dir)
    (ensure-directories-exist compressed-diary-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "1.txt" diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Ordinary diary entry." s))
    (with-open-file (s (merge-pathnames "2.txt" diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Second diary entry." s))
    (with-open-file (s (merge-pathnames "1.txt" compressed-diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Compressed diary entry." s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((conv (new-chat-persona "persona-compressed-diary"))
                  (entries (conversation-persona-diary-entries conv)))
             (fiveam:is (= 2 (length entries)))
             (fiveam:is (string= "Compressed diary entry."
                                 (cdr (assoc :content (first entries)))))
             (fiveam:is (string= "Second diary entry."
                                 (cdr (assoc :content (second entries)))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-auto-compress-diary-on-startup
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-auto-compress-diary/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-auto-compress-diary/" personas-dir))
         (diary-dir (merge-pathnames "Diary/" test-persona-dir))
         (compressed-diary-dir (merge-pathnames "CompressedDiary/" test-persona-dir))
         (captured-urls nil))
    (ensure-directories-exist diary-dir)
    (ensure-directories-exist compressed-diary-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "memory.json" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "{\"entities\":[],\"relations\":[]}" s))
    (with-open-file (s (merge-pathnames "1.txt" diary-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "Ordinary uncompressed diary entry." s))
    (with-open-file (s (merge-pathnames "2.txt" diary-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "This file is already compressed." s))
    (with-open-file (s (merge-pathnames "2.txt" compressed-diary-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "Compressed already-existing content." s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home))
               (*gemini-api-key-function* (lambda () "mocked-api-key"))
               (*read-mcp-config-function*
                 (lambda ()
                   '((:name "memory"
                      :command "npx"
                      :args ("-y" "@modelcontextprotocol/server-memory")
                      :env (("MEMORY_FILE_PATH" . "default-memory.json"))))))
               (*start-mcp-server-function*
                 (lambda (name command args &optional environment)
                   (declare (ignore command args environment))
                   (make-instance 'mcp-server :name name)))
               (*mcp-initialize-function* (lambda (server) server))
               (*mcp-send-request-function*
                 (lambda (server method params &key timeout)
                   (declare (ignore server params timeout))
                   (when (string= "tools/list" method)
                     '((:tools . (((:name . "read_graph"))))))))
               (*persona-memory-compression-thread-function*
                 (lambda (thunk thread-name)
                   (declare (ignore thread-name))
                   (funcall thunk)
                   :ran-inline))
               (*http-post-function*
                 (lambda (url &rest args)
                   (declare (ignore args))
                   (push url captured-urls)
                   (values
                    (make-string-input-stream
                     "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"session-1\"}}
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"Compressed mock response content.\"}}
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"session-1\",\"model\":\"models/gemini-mock-model\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}")
                    200))))
           (let* ((conv (new-chat-persona "persona-auto-compress-diary"))
                  (entries (conversation-persona-diary-entries conv)))
             ;; Because we ran compression inline, Gopher should have automatically compressed 1.txt to CompressedDiary/1.txt
             ;; Verify file exists in CompressedDiary/ with the mocked content
             (let ((compressed-1-path (merge-pathnames "1.txt" compressed-diary-dir))
                   (compressed-2-path (merge-pathnames "2.txt" compressed-diary-dir)))
               (fiveam:is (not (null (probe-file compressed-1-path))))
               (fiveam:is (string= "Compressed mock response content." (uiop:read-file-string compressed-1-path)))
               ;; Verify 2.txt was NOT overwritten
               (fiveam:is (string= "Compressed already-existing content."
                                   (string-right-trim '(#\Space #\Tab #\Return #\Linefeed)
                                                       (uiop:read-file-string compressed-2-path)))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-enables-filesystem-tools
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-filesystem-tools/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-filesystem-tools/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"gpt-4o\" :enable-filesystem-tools t)" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((conv (new-chat-persona "persona-filesystem-tools"))
                 (bot (conversation-chatbot conv)))
            (fiveam:is-true (chatbot-filesystem-tools-p bot))
            (fiveam:is (equal (truename test-persona-dir)
                              (chatbot-filesystem-root-directory bot)))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-enables-turn-timestamps
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-turn-timestamps/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-turn-timestamps/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                     :direction :output
                     :if-exists :supersede)
      (write-line "(:model \"gpt-4o\" :include-timestamp t)" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "persona-turn-timestamps"))
                (bot (conversation-chatbot conv)))
           (fiveam:is-true (chatbot-include-timestamp-p bot))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-enables-model-indicator
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-model-indicator/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-model-indicator/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                     :direction :output
                     :if-exists :supersede)
      (write-line "(:model \"gpt-4o\" :include-model t)" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "persona-model-indicator"))
                 (bot (conversation-chatbot conv)))
            (fiveam:is-true (chatbot-include-model-p bot))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-enables-web-tools
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-web-tools/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-web-tools/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                     :direction :output
                     :if-exists :supersede)
      (write-line "(:model \"gpt-4o\" :enable-web-tools t)" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((conv (new-chat-persona "persona-web-tools"))
                 (bot (conversation-chatbot conv)))
            (fiveam:is-true (chatbot-web-tools-p bot))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-can-enable-gemini-google-fallback
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-gemini-fallback/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-gemini-fallback/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                    :direction :output
                    :if-exists :supersede)
      (write-line "(:model \"gemini-3.5-flash\" :gemini-fallback-to-google-p t)" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((conv (new-chat-persona "persona-gemini-fallback"))
                 (bot (conversation-chatbot conv)))
            (fiveam:is-true (chatbot-gemini-fallback-to-google-p bot))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-defaults-gemini-google-fallback-off
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-gemini-fallback-default/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-gemini-fallback-default/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                   :direction :output
                   :if-exists :supersede)
      (write-line "(:model \"gemini-3.5-flash\")" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((conv (new-chat-persona "persona-gemini-fallback-default"))
                 (bot (conversation-chatbot conv)))
            (fiveam:is-false (chatbot-gemini-fallback-to-google-p bot))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-config-enables-eval-tool
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-eval-tool/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-eval-tool/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                     :direction :output
                     :if-exists :supersede)
      (write-line "(:model \"gpt-4o\" :enable-eval t)" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (let* ((conv (new-chat-persona "persona-eval-tool"))
                 (bot (conversation-chatbot conv)))
            (fiveam:is-true (chatbot-enable-eval-p bot))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-loads-filesystem-allowlist
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-filesystem-allowlist/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-filesystem-allowlist/" personas-dir))
        (outside-dir (merge-pathnames "outside-approved/" temp-dir))
        (allowlist-path (merge-pathnames "filesystem-allowlist.lisp" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (ensure-directories-exist outside-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                     :direction :output
                     :if-exists :supersede)
      (write-line "(:model \"gpt-4o\" :enable-filesystem-tools t)" s))
    (with-open-file (s allowlist-path
                     :direction :output
                     :if-exists :supersede)
      (prin1 (list (namestring outside-dir)) s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "persona-filesystem-allowlist"))
                 (bot (conversation-chatbot conv)))
            (fiveam:is (equal (persona-filesystem-allowlist-path test-persona-dir)
                              (chatbot-filesystem-allowlist-path bot)))
            (fiveam:is (equal (list (uiop:ensure-directory-pathname (truename outside-dir)))
                              (chatbot-filesystem-allowed-directories bot)))))
      (uiop:delete-directory-tree outside-dir :validate t)
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-loading-unexpected-properties
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-unexpected/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home)))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (progn
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
             (let ((test-persona-dir (merge-pathnames "persona-json-memory/" personas-dir)))
               (ensure-directories-exist test-persona-dir)
               (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                                  :direction :output
                                  :if-exists :supersede)
                 (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
               (with-open-file (s (merge-pathnames "memory.json" test-persona-dir)
                                  :direction :output
                                  :if-exists :supersede)
                 (write-line "{\"entities\":[]}" s))
                (let ((*read-mcp-config-function*
                        (lambda ()
                          '((:name "memory"
                             :command "npx"
                             :args ("-y" "@modelcontextprotocol/server-memory")
                             :env (("MEMORY_FILE_PATH" . "default-memory.json"))))))
                      (*start-mcp-server-function*
                        (lambda (name command args &optional environment)
                          (declare (ignore command args environment))
                          (make-instance 'mcp-server :name name)))
                      (*mcp-initialize-function* (lambda (server) server))
                      (*mcp-send-request-function*
                        (lambda (server method params &key timeout)
                          (declare (ignore server params timeout))
                          (when (string= "tools/list" method)
                            '((:tools . (((:name . "read_graph"))))))))
                      (*persona-memory-compression-thread-function*
                        (lambda (thunk thread-name)
                          (declare (ignore thread-name))
                          (funcall thunk)
                          :ran-inline)))
                  (let ((conv (new-chat-persona "persona-json-memory")))
                    (fiveam:is (null (conversation-messages conv)))
                    (fiveam:is (search "{\"entities\":[]}"
                                       (conversation-persona-memory conv))))))
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
             (let ((test-persona-dir (merge-pathnames "persona-syntax-error/" personas-dir)))
               (ensure-directories-exist test-persona-dir)
               (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                                  :direction :output
                                  :if-exists :supersede)
                 (write-line "this-is-not-a-list" s))
               (fiveam:signals error
                 (new-chat-persona "persona-syntax-error")))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-memory-json-starts-persona-memory-server
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-persona-memory-server/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-memory-server/" personas-dir))
        (captured-environment nil)
        (tools-listed-p nil))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "memory.json" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "{\"entities\":[{\"name\":\"Joe\",\"entityType\":\"person\",\"observations\":[\"likes Lisp\"]}],\"relations\":[]}" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home))
              (*start-mcp-server-function*
                (lambda (name command args &optional environment)
                  (fiveam:is (string= "memory" name))
                  (fiveam:is (search "npx" (string-downcase command)))
                  (fiveam:is (equal '("-y" "@modelcontextprotocol/server-memory") args))
                  (setf captured-environment environment)
                  (make-instance 'mcp-server :name name)))
              (*mcp-initialize-function* (lambda (server) server))
              (*mcp-send-request-function*
                (lambda (server method params &key timeout)
                  (declare (ignore server params timeout))
                  (when (string= "tools/list" method)
                    (setf tools-listed-p t)
                    '((:tools . (((:name . "read_graph"))
                                 ((:name . "search_nodes"))
                                 ((:name . "add_observations"))))))))
              (*persona-memory-compression-thread-function*
                (lambda (thunk thread-name)
                  (declare (ignore thread-name))
                  (funcall thunk)
                  :ran-inline)))
          (let* ((memory-path (merge-pathnames "memory.json" test-persona-dir))
                 (conv (new-chat-persona "persona-memory-server"))
                 (bot (conversation-chatbot conv))
                (memory-preload (decode-test-json (conversation-persona-memory conv)))
                (preload-entities (test-json-elements
                                   (test-json-value-any memory-preload '("entities" :entities))))
                (preload-relations (test-json-elements
                                    (test-json-value-any memory-preload '("relations" :relations))))
                (memory-file-records (decode-test-json-lines
                                      (uiop:read-file-string memory-path)))
                (entity-record (first memory-file-records)))
           (fiveam:is (= 1 (length preload-entities)))
           (fiveam:is (null preload-relations))
           (assert-json-field= (first preload-entities) "name" "Joe")
           (assert-json-field= (first preload-entities) "entityType" "person")
           (fiveam:is (equal '("likes Lisp")
                             (test-json-elements
                              (test-json-value-any (first preload-entities)
                                                   '("observations" :observations)))))
           (fiveam:is (= 1 (length (chatbot-mcp-servers bot))))
           (fiveam:is (string= "memory"
                               (mcp-server-name (car (chatbot-mcp-servers bot)))))
           (fiveam:is (not (null tools-listed-p)))
           (fiveam:is (mcp-server-tool-list-cache-valid-p
                       (car (chatbot-mcp-servers bot))))
           (fiveam:is (string= (namestring memory-path)
                               (cdr (assoc "MEMORY_FILE_PATH"
                                           captured-environment
                                           :test #'string=))))
           (fiveam:is (= 1 (length memory-file-records)))
           (assert-json-field= entity-record "type" "entity")
           (assert-json-field= entity-record "name" "Joe")
           (assert-json-field= entity-record "entityType" "person")
           (fiveam:is (equal '("likes Lisp")
                             (test-json-elements
                              (test-json-value-any entity-record
                                                   '("observations" :observations)))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-memory-json-errors-when-memory-server-has-no-graph-tools
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-persona-memory-no-tools/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-memory-no-tools/" personas-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "memory.json" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "{\"entities\":[],\"relations\":[]}" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home))
              (*start-mcp-server-function*
                (lambda (name command args &optional environment)
                  (declare (ignore name command args environment))
                  (make-instance 'mcp-server :name "memory")))
              (*mcp-initialize-function* (lambda (server) server))
              (*mcp-send-request-function*
                (lambda (server method params &key timeout)
                  (declare (ignore server params timeout))
                  (when (string= "tools/list" method)
                    '((:tools . (((:name . "unrelated_tool"))))))))
              (*persona-memory-compression-thread-function*
                (lambda (thunk thread-name)
                  (declare (ignore thunk thread-name))
                  :not-started)))
          (fiveam:signals error
            (new-chat-persona "persona-memory-no-tools")))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-save-compressed-persona-memory-from-graph-json
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-compressed-memory-graph/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-compressed-memory-graph/" personas-dir))
        (memory-path (merge-pathnames "memory.json" test-persona-dir))
        (compressed-path (merge-pathnames "compressed-memory.txt" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s memory-path :direction :output :if-exists :supersede)
      (write-string "{\"entities\":[{\"name\":\"Joe\",\"entityType\":\"person\",\"observations\":[\"likes Lisp\",\"uses Emacs\"]},{\"name\":\"SBCL\",\"entityType\":\"tool\",\"observations\":[\"fast compiler\"]}],\"relations\":[{\"from\":\"Joe\",\"to\":\"SBCL\",\"relationType\":\"uses\"}]}" s))
    (unwind-protect
       (let ((*user-homedir-pathname-function* (lambda () mock-home)))
         (fiveam:is (equal compressed-path
                           (save-compressed-persona-memory "persona-compressed-memory-graph")))
         (fiveam:is (string= (format nil "Entities:~%- Joe (person): likes Lisp; uses Emacs~%- SBCL (tool): fast compiler~%~%Relations:~%- Joe -uses-> SBCL")
                             (uiop:read-file-string compressed-path))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-save-compressed-persona-memory-from-jsonl
  (let* ((temp-dir (uiop:default-temporary-directory))
        (mock-home (merge-pathnames "mock-home-compressed-memory-jsonl/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-compressed-memory-jsonl/" personas-dir))
        (memory-path (merge-pathnames "memory.json" test-persona-dir))
        (compressed-path (merge-pathnames "compressed-memory.txt" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s memory-path :direction :output :if-exists :supersede)
      (write-line "{\"type\":\"entity\",\"name\":\"Joe\",\"entityType\":\"person\",\"observations\":[\"likes Lisp\"]}" s)
      (write-line "{\"type\":\"relation\",\"from\":\"Joe\",\"to\":\"Common Lisp\",\"relationType\":\"studies\"}" s))
    (unwind-protect
       (let ((*user-homedir-pathname-function* (lambda () mock-home)))
         (fiveam:is (equal compressed-path
                           (save-compressed-persona-memory "persona-compressed-memory-jsonl")))
         (fiveam:is (string= (format nil "Entities:~%- Joe (person): likes Lisp~%~%Relations:~%- Joe -studies-> Common Lisp")
                             (uiop:read-file-string compressed-path))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-save-compressed-persona-memory-from-jsonl-with-inferred-and-empty-records
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-compressed-memory-jsonl-inferred/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-compressed-memory-jsonl-inferred/" personas-dir))
         (memory-path (merge-pathnames "memory.json" test-persona-dir))
         (compressed-path (merge-pathnames "compressed-memory.txt" test-persona-dir)))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s memory-path :direction :output :if-exists :supersede)
      (write-line "{}" s)
      (write-line "{\"name\":\"Joe\",\"entityType\":\"person\",\"observations\":[\"likes Lisp\"]}" s)
      (write-line "{\"from\":\"Joe\",\"to\":\"Common Lisp\",\"relationType\":\"studies\"}" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home)))
           (fiveam:is (equal compressed-path
                             (save-compressed-persona-memory "persona-compressed-memory-jsonl-inferred")))
           (fiveam:is (string= (format nil "Entities:~%- Joe (person): likes Lisp~%~%Relations:~%- Joe -studies-> Common Lisp")
                               (uiop:read-file-string compressed-path))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-new-chat-persona-starts-background-memory-compression
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-persona-memory-compression-thread/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-memory-compression-thread/" personas-dir))
         (compressed-path (merge-pathnames "compressed-memory.txt" test-persona-dir))
         (captured-thread-name nil))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "memory.json" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "{\"entities\":[{\"name\":\"Joe\",\"entityType\":\"person\",\"observations\":[\"likes Lisp\"]}],\"relations\":[{\"from\":\"Joe\",\"to\":\"SBCL\",\"relationType\":\"uses\"}]}" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home))
              (*read-mcp-config-function*
                (lambda ()
                  '((:name "memory"
                     :command "npx"
                     :args ("-y" "@modelcontextprotocol/server-memory")
                     :env (("MEMORY_FILE_PATH" . "default-memory.json"))))))
              (*start-mcp-server-function*
                (lambda (name command args &optional environment)
                  (declare (ignore command args environment))
                  (make-instance 'mcp-server :name name)))
              (*mcp-initialize-function* (lambda (server) server))
              (*mcp-send-request-function*
                (lambda (server method params &key timeout)
                  (declare (ignore server params timeout))
                  (when (string= "tools/list" method)
                    '((:tools . (((:name . "read_graph"))))))))
              (*persona-memory-compression-thread-function*
                (lambda (thunk thread-name)
                  (setf captured-thread-name thread-name)
                  (funcall thunk)
                  :ran-inline)))
          (new-chat-persona "persona-memory-compression-thread")
          (fiveam:is (search "Persona-Memory-Compression-persona-memory-compression-thread"
                             captured-thread-name))
          (fiveam:is (string= (format nil "Entities:~%- Joe (person): likes Lisp~%~%Relations:~%- Joe -uses-> SBCL")
                              (uiop:read-file-string compressed-path))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-new-chat-persona-skips-background-memory-compression-without-memory-json
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-persona-no-memory-compression-thread/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (test-persona-dir (merge-pathnames "persona-no-memory-compression-thread/" personas-dir))
         (compression-started-p nil))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "compressed-memory.txt" test-persona-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "Existing compressed memory." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home))
              (*persona-memory-compression-thread-function*
                (lambda (thunk thread-name)
                  (declare (ignore thunk thread-name))
                  (setf compression-started-p t)
                  :unexpected)))
          (new-chat-persona "persona-no-memory-compression-thread")
          (fiveam:is-false compression-started-p))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-memory-server-replaces-shared-memory-server
  (let* ((temp-dir (uiop:default-temporary-directory))
       (mock-home (merge-pathnames "mock-home-persona-memory-shared/" temp-dir))
        (personas-dir (merge-pathnames ".Personas/" mock-home))
        (test-persona-dir (merge-pathnames "persona-memory-shared/" personas-dir))
        (context (make-runtime-context))
        (shared-time (make-instance 'mcp-server :name "mcp-server-time"))
        (shared-memory (make-instance 'mcp-server :name "memory"))
        (startup-bot (make-instance 'chatbot
                                    :mcp-servers (list shared-time shared-memory)
                                    :runtime-context context))
        (persona-memory-server (make-instance 'mcp-server :name "memory")))
    (ensure-directories-exist test-persona-dir)
    (with-open-file (s (merge-pathnames "config.lisp" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "(:model \"models/gemini-mock-model\" :googleapi :google-api)" s))
    (with-open-file (s (merge-pathnames "memory.json" test-persona-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "{\"entities\":[],\"relations\":[]}" s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home))
              (*read-mcp-config-function*
                (lambda ()
                  '((:name "memory"
                     :command "npx"
                     :args ("-y" "@modelcontextprotocol/server-memory")
                     :env (("MEMORY_FILE_PATH" . "default-memory.json"))))))
              (*start-mcp-server-function*
                (lambda (name command args &optional environment)
                  (declare (ignore name command args environment))
                  persona-memory-server))
              (*mcp-initialize-function* (lambda (server) server))
              (*mcp-send-request-function*
                (lambda (server method params &key timeout)
                  (declare (ignore server params timeout))
                  (when (string= "tools/list" method)
                    '((:tools . (((:name . "read_graph"))))))))
              (*persona-memory-compression-thread-function*
                (lambda (thunk thread-name)
                  (declare (ignore thread-name))
                  (funcall thunk)
                  :ran-inline)))
          (setf (runtime-context-startup-chatbot context) startup-bot)
          (let* ((conv (new-chat-persona "persona-memory-shared" :runtime-context context))
                 (servers (chatbot-mcp-servers (conversation-chatbot conv))))
            (fiveam:is (= 2 (length servers)))
            (fiveam:is (eq shared-time (first servers)))
            (fiveam:is (eq persona-memory-server (second servers)))
            (fiveam:is-false (member shared-memory servers :test #'eq))))
      (setf (runtime-context-startup-chatbot context) nil)
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-persona-subordinates-spawning
  (let* ((temp-dir (uiop:default-temporary-directory))
         (mock-home (merge-pathnames "mock-home-subordinates/" temp-dir))
         (personas-dir (merge-pathnames ".Personas/" mock-home))
         (parent-dir (merge-pathnames "parent-persona/" personas-dir))
         (sub1-dir (merge-pathnames "sub-persona-1/" personas-dir))
         (sub2-dir (merge-pathnames "sub-persona-2/" personas-dir)))
    (ensure-directories-exist parent-dir)
    (ensure-directories-exist sub1-dir)
    (ensure-directories-exist sub2-dir)
    (with-open-file (s (merge-pathnames "config.lisp" parent-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"parent-model\" :subordinates (\"sub-persona-1\" \"sub-persona-2\"))" s))
    (with-open-file (s (merge-pathnames "config.lisp" sub1-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"sub1-model\")" s))
    (with-open-file (s (merge-pathnames "config.lisp" sub2-dir)
                       :direction :output
                       :if-exists :supersede)
      (write-line "(:model \"sub2-model\")" s))
    (unwind-protect
         (let ((*user-homedir-pathname-function* (lambda () mock-home))
               (*gemini-api-key-function* (lambda () "mocked-api-key"))
               (*http-post-function*
                 (lambda (url &rest args)
                   (declare (ignore url args))
                   (let ((payload
                          (format nil
                                   "data: ~A~%data: ~A~%data: ~A"
                                   (cl-json:encode-json-to-string
                                    '(("event_type" . "interaction.created")
                                      ("interaction" . (("id" . "session-1")))))
                                   (cl-json:encode-json-to-string
                                    `(("event_type" . "step.delta")
                                      ("delta" . (("type" . "text")
                                                  ("text" . ,(format nil
                                                                     "{\"reply\":~A,\"spawn\":null}"
                                                                     (cl-json:encode-json-to-string
                                                                      "Subordinate persona replied.")))))))
                                   (cl-json:encode-json-to-string
                                    '(("event_type" . "interaction.completed")
                                      ("interaction" . (("id" . "session-1")
                                                        ("model" . "sub1-model")
                                                        ("usage" . (("total_input_tokens" . 1)
                                                                    ("total_output_tokens" . 1)
                                                                    ("total_tokens" . 2))))))))))
                    (values (make-string-input-stream payload) 200)))))
           (let* ((conv (new-chat-persona "parent-persona"))
                  (bot (conversation-chatbot conv))
                  (subs (chatbot-subordinates bot)))
             (fiveam:is (typep conv 'conversation))
             (fiveam:is (typep bot 'chatbot))
             (fiveam:is (= 2 (length subs)))
             (let* ((sub-conv-1 (first subs))
                    (sub-bot-1 (conversation-chatbot sub-conv-1))
                    (sub-conv-2 (second subs))
                    (sub-bot-2 (conversation-chatbot sub-conv-2)))
               (fiveam:is (typep sub-conv-1 'conversation))
               (fiveam:is (typep sub-bot-1 'chatbot))
               (fiveam:is (string= "sub-persona-1" (chatbot-persona-name sub-bot-1)))
               (fiveam:is (string= "sub1-model" (chatbot-model sub-bot-1)))
               (fiveam:is (typep sub-conv-2 'conversation))
               (fiveam:is (typep sub-bot-2 'chatbot))
               (fiveam:is (string= "sub-persona-2" (chatbot-persona-name sub-bot-2)))
               (fiveam:is (string= "sub2-model" (chatbot-model sub-bot-2)))
               ;; Verify that the promptSubordinate tool is registered and can be executed
               (multiple-value-bind (source tool) (find-chatbot-tool bot "promptSubordinate")
                 (fiveam:is (eq :built-in source))
                 (fiveam:is (string= "promptSubordinate" (mcp-val :name tool))))
               ;; Execute the promptSubordinate tool
               (let ((result (execute-chatbot-tool-by-name bot "promptSubordinate"
                                                           '(("name" . "sub-persona-1")
                                                             ("prompt" . "hello")))))
                 (fiveam:is (string= "Subordinate persona replied." result)))
               ;; Verify that matching is case-insensitive
               (let ((result (execute-chatbot-tool-by-name bot "promptSubordinate"
                                                           '(("name" . "SUB-PERSONA-1")
                                                             ("prompt" . "hello")))))
                 (fiveam:is (string= "Subordinate persona replied." result)))
               ;; Verify error on non-existent subordinate name
               (fiveam:signals error
                 (execute-chatbot-tool-by-name bot "promptSubordinate"
                                               '(("name" . "unknown-persona")
                                                 ("prompt" . "hello")))))))
      (uiop:delete-directory-tree mock-home :validate t))))

(fiveam:test test-context-pruning
  (let* ((*context-pruning-threshold-characters* 8000)
         (conv (new-chat :backend :google))
         (bot (conversation-chatbot conv))
         (long-text (make-string 3000 :initial-element #\A))
         (calls '()))
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
              (declare (ignore args))
              (push url calls)
              (values
               "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Mocked response.\"}], \"role\": \"model\"}}]}"
               200))))
      (let ((response (chat "Hello latest prompt" :conversation conv)))
        (fiveam:is (string= "Mocked response." response))
        ;; Verify that the history was successfully pruned and a State Digest was inserted
        (let ((history (conversation-messages conv)))
          (fiveam:is (<= (length history) 8))
          (fiveam:is (string= "system" (cdr (assoc "role" (first history) :test #'string=))))
          (fiveam:is (search "State Digest" (cdr (assoc "content" (first history) :test #'string=))))
          (fiveam:is (string= "Mocked response."
                              (cdr (assoc "content" (car (last history)) :test #'string=)))))))))

(fiveam:test test-context-pruning-default-window-targets-200k-budget
  (let ((*context-pruning-threshold-characters* 800000)
        (*context-pruning-estimated-max-tokens* 200000)
        (*context-pruning-estimated-target-tokens* 150000))
    (fiveam:is (= 200000 (configured-context-pruning-max-tokens)))
    (fiveam:is (= 200000 (effective-context-pruning-max-tokens)))
    (fiveam:is (= 150000 (effective-context-pruning-target-tokens)))))

(fiveam:test test-context-pruning-adaptive-threshold-stays-within-configured-budget
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 75)
        (conv (new-chat :backend :google))
        (large-history
          (list (list (cons "role" "user")
                      (cons "content" (make-string 160 :initial-element #\A)))
                (list (cons "role" "model")
                      (cons "content" (make-string 160 :initial-element #\B))))))
    (update-adaptive-context-pruning-max-tokens conv large-history)
    (fiveam:is (= 100 (conversation-adaptive-context-pruning-max-tokens conv)))
    (fiveam:is (= 100 (effective-context-pruning-max-tokens conv)))
    (setf (conversation-adaptive-context-pruning-max-tokens conv) 180)
    (fiveam:is (= 100 (effective-context-pruning-max-tokens conv)))
    (fiveam:is (= 75 (effective-context-pruning-target-tokens conv)))))

(fiveam:test test-context-pruning-runs-after-turn-completes
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 75)
        (conv (new-chat :backend :google))
        (near-limit-text (make-string 120 :initial-element #\A))
        (responses
          (list
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Primary response.\"}], \"role\": \"model\"}}]}"
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Digest summary.\"}], \"role\": \"model\"}}]}"))
        (calls '()))
    (setf (conversation-messages conv)
         (list (list (cons "role" "user") (cons "content" near-limit-text))
               (list (cons "role" "model") (cons "content" "response 1"))
               (list (cons "role" "user") (cons "content" near-limit-text))
               (list (cons "role" "model") (cons "content" "response 2"))
               (list (cons "role" "user") (cons "content" near-limit-text))
               (list (cons "role" "model") (cons "content" "response 3"))))
    (let ((*gemini-api-key-function* (lambda () "mocked-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore args))
             (push url calls)
             (values (pop responses) 200))))
      (let ((response (chat "Hello latest prompt" :conversation conv)))
       (fiveam:is (string= "Primary response." response))
       (fiveam:is (= 2 (length calls)))
       (let ((history (conversation-messages conv)))
         (fiveam:is (string= "system" (cdr (assoc "role" (first history) :test #'string=))))
         (fiveam:is (search "Digest summary."
                            (cdr (assoc "content" (first history) :test #'string=))))
         (fiveam:is (string= "Primary response."
                             (cdr (assoc "content" (car (last history)) :test #'string=))))
         (fiveam:is (<= (estimated-history-token-count history) 75)))))))

(fiveam:test test-context-pruning-adapts-threshold-after-compression
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 20)
        (conv (new-chat :backend :google))
        (responses
          (list
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Digest summary.\"}], \"role\": \"model\"}}]}"
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Retry digest.\"}], \"role\": \"model\"}}]}"
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Follow-up digest.\"}], \"role\": \"model\"}}]}")))
    (setf (conversation-messages conv)
         (loop for index from 1 to 30
               append (list (list (cons "role" "user")
                                  (cons "content" (format nil "user-~2,'0D" index)))
                            (list (cons "role" "model")
                                  (cons "content" (format nil "model-~2,'0D" index))))))
    (let ((*gemini-api-key-function* (lambda () "mocked-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url args))
             (values (or (pop responses)
                         "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Fallback digest.\"}], \"role\": \"model\"}}]}")
                     200))))
      (compress-conversation-context-if-needed conv)
      (let* ((compressed-history (conversation-messages conv))
            (compressed-total-tokens
              (estimated-conversation-context-token-count conv compressed-history))
            (adaptive-threshold
              (conversation-adaptive-context-pruning-max-tokens conv))
            (needed-extra-tokens (1+ (- adaptive-threshold compressed-total-tokens)))
            (extra-message-tokens (ceiling needed-extra-tokens 2))
            (extra-text (make-string (* 4 extra-message-tokens) :initial-element #\B))
            (expanded-history
              (append compressed-history
                      (list (list (cons "role" "user") (cons "content" extra-text))
                            (list (cons "role" "model") (cons "content" extra-text)))))
            (expanded-total-tokens
              (estimated-conversation-context-token-count conv expanded-history))
            (recompressed-history
              (compressed-conversation-history-if-needed conv expanded-history)))
        (fiveam:is (= (* 2 compressed-total-tokens) adaptive-threshold))
        (fiveam:is (< adaptive-threshold *context-pruning-estimated-max-tokens*))
        (fiveam:is (> expanded-total-tokens adaptive-threshold))
        (fiveam:is (< expanded-total-tokens *context-pruning-estimated-max-tokens*))
        (fiveam:is (not (equal expanded-history recompressed-history)))
        (fiveam:is (string= "system" (cdr (assoc "role" (first recompressed-history) :test #'string=))))
        (fiveam:is (search "State Digest"
                          (cdr (assoc "content" (first recompressed-history) :test #'string=))))))))

(fiveam:test test-context-pruning-counts-system-instruction-tokens
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 75)
        (conv (new-chat :backend :google
                        :system-instruction (make-string 320 :initial-element #\S)))
        (medium-text (make-string 24 :initial-element #\A))
        (responses
          (list
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Primary response.\"}], \"role\": \"model\"}}]}"
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Digest summary.\"}], \"role\": \"model\"}}]}"))
        (calls '()))
    (setf (conversation-messages conv)
         (list (list (cons "role" "user") (cons "content" medium-text))
               (list (cons "role" "model") (cons "content" "response 1"))
               (list (cons "role" "user") (cons "content" medium-text))
               (list (cons "role" "model") (cons "content" "response 2"))
               (list (cons "role" "user") (cons "content" medium-text))
               (list (cons "role" "model") (cons "content" "response 3"))))
    (fiveam:is (< (estimated-history-token-count (conversation-messages conv)) 100))
    (let ((*gemini-api-key-function* (lambda () "mocked-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore args))
             (push url calls)
             (values (pop responses) 200))))
      (let ((response (chat "Hello latest prompt" :conversation conv)))
        (fiveam:is (string= "Primary response." response))
        (fiveam:is (= 2 (length calls)))
        (let ((history (conversation-messages conv)))
         (fiveam:is (string= "system" (cdr (assoc "role" (first history) :test #'string=))))
         (fiveam:is (search "Digest summary."
                            (cdr (assoc "content" (first history) :test #'string=))))
         (fiveam:is (string= "Primary response."
                             (cdr (assoc "content" (car (last history)) :test #'string=)))))))))

(fiveam:test test-context-token-breakdown-separates-digest-history
  (let* ((conv (new-chat :backend :google
                        :system-instruction (make-string 40 :initial-element #\S)))
        (history (list (make-context-digest-message "Earlier digest state.")
                       (list (cons "role" "user")
                             (cons "content" "Fresh question."))))
        (breakdown (conversation-context-token-breakdown conv history)))
    (fiveam:is (> (getf breakdown :fixed-context-tokens) 0))
    (fiveam:is (> (getf breakdown :history-tokens) 0))
    (fiveam:is (> (getf breakdown :digest-message-tokens) 0))
    (fiveam:is (> (getf breakdown :non-digest-history-tokens) 0))
    (fiveam:is (= (getf breakdown :total-tokens)
                 (+ (getf breakdown :fixed-context-tokens)
                    (getf breakdown :history-tokens))))))

(fiveam:test test-summarize-old-history-source-text-unwraps-existing-digest
  (let ((source-text
         (summarize-old-history-source-text
          (list (make-context-digest-message "Prior digest summary.")
                (list (cons "role" "user")
                      (cons "content" "Need follow-up work."))))))
    (fiveam:is (search "Existing State Digest content to preserve and refine:" source-text))
    (fiveam:is (search "Prior digest summary." source-text))
    (fiveam:is (search "user: Need follow-up work." source-text))
    (fiveam:is-false (search "[State Digest of previous turns:" source-text))))

(fiveam:test test-summarize-old-history-bounds-digest-size
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 20)
        (*context-pruning-max-digest-tokens* 8)
        (conversation (new-chat :backend :google))
        (long-summary (make-string 200 :initial-element #\D)))
    (let ((*gemini-api-key-function* (lambda () "mocked-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore url args))
             (values
              (format nil "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"~A\"}], \"role\": \"model\"}}]}" long-summary)
              200))))
      (let ((digest (summarize-old-history
                    (list (list (cons "role" "user") (cons "content" "First turn"))
                          (list (cons "role" "model") (cons "content" "Second turn")))
                    conversation)))
        (fiveam:is (stringp digest))
        (fiveam:is (<= (estimate-text-token-count digest) 8))))))

(fiveam:test test-context-pruning-does-not-trigger-when-fixed-context-alone-exceeds-budget
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 75)
        (conv (new-chat :backend :google
                        :system-instruction (make-string 420 :initial-element #\S)))
        (responses
          (list
           "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Primary response.\"}], \"role\": \"model\"}}]}"))
        (calls '()))
    (setf (conversation-messages conv)
         (list (list (cons "role" "user") (cons "content" "tiny history"))
               (list (cons "role" "model") (cons "content" "small reply"))))
    (let ((*gemini-api-key-function* (lambda () "mocked-api-key"))
         (*http-post-function*
           (lambda (url &rest args)
             (declare (ignore args))
             (push url calls)
             (values (pop responses) 200))))
      (let ((response (chat "Hello latest prompt" :conversation conv)))
        (fiveam:is (string= "Primary response." response))
        (fiveam:is (= 1 (length calls)))
        (let ((history (conversation-messages conv)))
         (fiveam:is (string= "user" (cdr (assoc "role" (first history) :test #'string=))))
         (fiveam:is-false (search "State Digest"
                                  (cdr (assoc "content" (first history) :test #'string=))))
         (fiveam:is (string= "Primary response."
                             (cdr (assoc "content" (car (last history)) :test #'string=)))))))))

(fiveam:test test-context-pruning-uses-estimated-token-window
  (let* ((*context-pruning-threshold-characters* nil)
        (*context-pruning-estimated-max-tokens* 100)
        (*context-pruning-estimated-target-tokens* 75)
        (conv (new-chat :backend :google))
        (long-text (make-string 160 :initial-element #\A))
        (calls '()))
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
             (declare (ignore args))
             (push url calls)
             (values
              "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Mocked response.\"}], \"role\": \"model\"}}]}"
              200))))
      (let ((response (chat "Hello latest prompt" :conversation conv)))
        (fiveam:is (string= "Mocked response." response))
        (let ((history (conversation-messages conv)))
         (fiveam:is (<= (length history) 8))
         (fiveam:is (string= "system" (cdr (assoc "role" (first history) :test #'string=))))
         (fiveam:is (search "State Digest" (cdr (assoc "content" (first history) :test #'string=))))
         (fiveam:is (<= (estimated-history-token-count history) 75))
         (fiveam:is (string= "Mocked response."
                             (cdr (assoc "content" (car (last history)) :test #'string=)))))))))

(fiveam:test test-context-pruning-kept-suffix-starts-at-user-boundary
  (let* ((*context-pruning-threshold-characters* nil)
         (*context-pruning-estimated-max-tokens* 12)
         (*context-pruning-estimated-target-tokens* 10)
         (history (list (list (cons "role" "user") (cons "content" (make-string 40 :initial-element #\A)))
                       (list (cons "role" "model") (cons "content" "m1"))
                       (list (cons "role" "user") (cons "content" "u2"))
                       (list (cons "role" "model") (cons "content" "m2"))
                       (list (cons "role" "user") (cons "content" "u3"))
                       (list (cons "role" "model") (cons "content" "m3"))))
         (kept (select-recent-messages-for-pruning history)))
    (fiveam:is (string= "user" (cdr (assoc "role" (first kept) :test #'string=))))
    (fiveam:is (equal '(("role" . "user") ("content" . "u2"))
                     (first kept)))))
