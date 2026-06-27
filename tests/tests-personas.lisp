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
         (test-persona-dir (merge-pathnames "persona-preload-log/" personas-dir))
         (*logging-enabled-p* t)
         (*log-level* :info))
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
                           (let ((*log-stream* s))
                             (new-chat-persona "persona-preload-log")))))
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
           "FINAL: persona loop defaults applied")))
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
                             (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
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
    (with-open-file (s (merge-pathnames "1.txt" compressed-diary-dir)
                      :direction :output
                      :if-exists :supersede)
      (write-line "Compressed diary entry." s))
    (unwind-protect
        (let ((*user-homedir-pathname-function* (lambda () mock-home)))
          (let* ((conv (new-chat-persona "persona-compressed-diary"))
                 (entries (conversation-persona-diary-entries conv)))
            (fiveam:is (= 1 (length entries)))
            (fiveam:is (string= "Compressed diary entry."
                                (cdr (assoc :content (first entries)))))))
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
        (captured-environment nil))
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
              (*read-mcp-config-function*
                (lambda ()
                  '((:name "memory"
                     :command "npx"
                     :args ("-y" "@modelcontextprotocol/server-memory")
                     :env (("MEMORY_FILE_PATH" . "default-memory.json"))))))
              (*start-mcp-server-function*
                (lambda (name command args &optional environment)
                  (declare (ignore command args))
                  (setf captured-environment environment)
                  (make-instance 'mcp-server :name name)))
              (*mcp-initialize-function* (lambda (server) server))
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
