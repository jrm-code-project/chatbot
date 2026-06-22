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
           (fiveam:is (eq :gemini (chatbot-backend (conversation-chatbot conv))))))
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
               (fiveam:is (string= "Stored persona memory."
                                   (conversation-persona-memory conv))))))
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
               (let ((conv (new-chat-persona "persona-json-memory")))
                 (fiveam:is (null (conversation-messages conv)))
                 (fiveam:is (search "{\"entities\":[]}"
                                    (conversation-persona-memory conv)))))
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
