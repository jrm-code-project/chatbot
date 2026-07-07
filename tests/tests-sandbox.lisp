;;; tests-sandbox.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-build-persona-system-instruction-renders-structured-sections
  (let ((instruction
          (build-persona-system-instruction
           :role "Gorgon"
           :tone "brutal but precise"
           :context '("You are reviewing experimental code.")
           :directives '("Find memory leaks." "Challenge weak assumptions.")
           :constraints '("Be concise."))))
    (fiveam:is (search "You are Gorgon." instruction))
    (fiveam:is (search "Adopt this tone: brutal but precise." instruction))
    (fiveam:is (search "Context:" instruction))
    (fiveam:is (search "  - You are reviewing experimental code." instruction))
    (fiveam:is (search "Directives:" instruction))
    (fiveam:is (search "  - Find memory leaks." instruction))
    (fiveam:is (search "Constraints:" instruction))))

(fiveam:test test-explicit-persona-registry-isolates_same_names_from_global_state
  (let ((registry-a (make-persona-registry))
       (registry-b (make-persona-registry)))
    (unwind-protect
        (progn
          (clear-personas)
          (let ((alpha-a (spawn-persona "Alpha"
                                        :backend :google
                                        :model "gemini-3.5-flash"
                                        :registry registry-a))
                (alpha-b (spawn-persona "Alpha"
                                        :backend :google
                                        :model "gemini-3.5-flash"
                                        :registry registry-b)))
            (fiveam:is (eq alpha-a (find-persona "Alpha" :registry registry-a)))
            (fiveam:is (eq alpha-b (find-persona "Alpha" :registry registry-b)))
            (fiveam:is (not (eq alpha-a alpha-b)))
            (fiveam:is (equal '("Alpha") (list-personas :registry registry-a)))
            (fiveam:is (equal '("Alpha") (list-personas :registry registry-b)))
            (fiveam:is (null (list-personas)))
            (fiveam:is (null (find-persona "Alpha")))
            (fiveam:is (eq alpha-b (remove-persona "Alpha" :registry registry-b)))
            (fiveam:is (null (find-persona "Alpha" :registry registry-b)))
            (fiveam:is (equal '("Alpha") (list-personas :registry registry-a)))))
     (clear-personas)
     (clear-personas :registry registry-a)
     (clear-personas :registry registry-b))))

(fiveam:test test-spawn-persona-registers-list-finds-and-clears
  (unwind-protect
      (progn
         (clear-personas)
         (let ((persona (spawn-persona "Alpha"
                                       :backend :google
                                       :model "gemini-3.5-flash"
                                       :role "reviewer"
                                       :tone "calm"
                                       :temperature 0.4d0
                                       :top-p 0.7d0)))
           (fiveam:is (typep persona 'persona))
           (fiveam:is (equal '("Alpha") (list-personas)))
           (fiveam:is (eq persona (find-persona "Alpha")))
           (let ((instruction
                   (chatbot-system-instruction
                    (conversation-chatbot (persona-conversation persona)))))
             (fiveam:is (search "You are reviewer." instruction))
             (fiveam:is (search "Adopt this tone: calm." instruction)))
           (fiveam:is (string= "Alpha"
                               (chatbot-checkpoint-name
                                (conversation-chatbot (persona-conversation persona)))))
           (fiveam:is (= 0.4d0 (getf (sampling-parameters persona) :temperature)))
           (fiveam:is (= 0.7d0 (getf (sampling-parameters persona) :top-p)))
           (fiveam:signals error
             (spawn-persona "Alpha" :backend :google :model "gemini-3.5-flash"))))
    (clear-personas)))

(fiveam:test test-query-all-checkpoints-sandbox-personas-separately
  (let* ((checkpoint-root (merge-pathnames "sandbox-persona-checkpoints/"
                                          (uiop:default-temporary-directory)))
        (*minions-data-directory* checkpoint-root))
    (ensure-directories-exist checkpoint-root)
    (unwind-protect
        (progn
          (clear-personas)
          (let* ((alpha-context (make-runtime-context
                                 :gemini-api-key-function (lambda () "mocked-google-api-key")
                                 :http-post-function
                                 (lambda (url &rest args)
                                   (declare (ignore url args))
                                   (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
                 (beta-context (make-runtime-context
                                :gemini-api-key-function (lambda () "mocked-google-api-key")
                                :http-post-function
                                (lambda (url &rest args)
                                  (declare (ignore url args))
                                  (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200))))
                 (alpha (spawn-persona "Alpha" :backend :google :runtime-context alpha-context))
                 (beta (spawn-persona "Beta" :backend :google :runtime-context beta-context))
                 (alpha-checkpoint (merge-pathnames "Alpha.json" (minions-data-directory)))
                 (beta-checkpoint (merge-pathnames "Beta.json" (minions-data-directory)))
                 (default-checkpoint (merge-pathnames "DefaultConversation.json" (minions-data-directory))))
            (when (probe-file alpha-checkpoint)
              (delete-file alpha-checkpoint))
            (when (probe-file beta-checkpoint)
              (delete-file beta-checkpoint))
            (when (probe-file default-checkpoint)
              (delete-file default-checkpoint))
            (query-all "Kickoff" :personas (list alpha beta))
            (fiveam:is (string= "Alpha" (chatbot-persona-name (conversation-chatbot (persona-conversation alpha)))))
            (fiveam:is (string= "Beta" (chatbot-persona-name (conversation-chatbot (persona-conversation beta)))))
            (fiveam:is (string= "Alpha" (chatbot-checkpoint-name (conversation-chatbot (persona-conversation alpha)))))
            (fiveam:is (string= "Beta" (chatbot-checkpoint-name (conversation-chatbot (persona-conversation beta)))))
            (fiveam:is (probe-file alpha-checkpoint))
            (fiveam:is (probe-file beta-checkpoint))
            (fiveam:is-false (probe-file default-checkpoint))))
     (clear-personas)
     (when (probe-file checkpoint-root)
       (uiop:delete-directory-tree checkpoint-root :validate t)))))

(fiveam:test test-query-all-can_draw_default_personas_from_explicit_registry
  (let ((registry (make-persona-registry))
      (alpha-payloads nil)
       (beta-payloads nil))
    (unwind-protect
        (let* ((alpha-context (make-runtime-context
                               :gemini-api-key-function (lambda () "mocked-google-api-key")
                               :http-post-function
                               (lambda (url &rest args)
                                 (declare (ignore url))
                                 (push (getf args :content) alpha-payloads)
                                 (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
               (beta-context (make-runtime-context
                              :gemini-api-key-function (lambda () "mocked-google-api-key")
                              :http-post-function
                              (lambda (url &rest args)
                                (declare (ignore url))
                                (push (getf args :content) beta-payloads)
                                (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200)))))
          (spawn-persona "Alpha" :backend :google :runtime-context alpha-context :registry registry)
          (spawn-persona "Beta" :backend :google :runtime-context beta-context :registry registry)
          (let ((results (query-all "Kickoff" :registry registry)))
            (fiveam:is (equal '(("Alpha" . "Alpha reply") ("Beta" . "Beta reply"))
                              (mapcar (lambda (result)
                                        (cons (getf result :name)
                                              (getf result :response)))
                                      results))))
          (fiveam:is (null (list-personas)))
          (fiveam:is (= 1 (length alpha-payloads)))
          (fiveam:is (= 1 (length beta-payloads))))
     (clear-personas :registry registry)
     (clear-personas))))

(fiveam:test test-reset-persona-clears-isolated-history
  (let ((captured-payloads nil))
    (unwind-protect
         (progn
           (clear-personas)
           (let* ((context (make-runtime-context
                            :gemini-api-key-function (lambda () "mocked-google-api-key")
                            :http-post-function
                            (lambda (url &rest args)
                              (declare (ignore url))
                              (setf captured-payloads
                                    (append captured-payloads (list (getf args :content))))
                              (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Reply\"}], \"role\": \"model\"}}]}" 200))))
                  (persona (spawn-persona "Resettable"
                                          :backend :google
                                          :runtime-context context)))
             (query-all "First prompt" :personas (list persona))
             (fiveam:is (= 2 (length (persona-history persona))))
             (reset-persona "Resettable")
             (fiveam:is (null (persona-history persona)))
             (fiveam:is (null (conversation-messages (persona-conversation persona))))
             (query-all "Second prompt" :personas (list persona))
             (let* ((second-payload (cl-json:decode-json-from-string (second captured-payloads)))
                   (second-contents (google-payload-contents second-payload)))
               (fiveam:is (= 1 (length second-contents)))
               (assert-google-message-texts (first second-contents)
                                            "user"
                                            '("Second prompt")))))
      (clear-personas))))

(fiveam:test test-query-all-keeps_histories_isolated_and_prints_headers
  (let ((alpha-payloads nil)
        (beta-payloads nil))
    (unwind-protect
         (progn
           (clear-personas)
           (let* ((alpha-context (make-runtime-context
                                  :gemini-api-key-function (lambda () "mocked-google-api-key")
                                  :http-post-function
                                  (lambda (url &rest args)
                                    (declare (ignore url))
                                    (push (getf args :content) alpha-payloads)
                                    (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
                  (beta-context (make-runtime-context
                                 :gemini-api-key-function (lambda () "mocked-google-api-key")
                                 :http-post-function
                                 (lambda (url &rest args)
                                   (declare (ignore url))
                                   (push (getf args :content) beta-payloads)
                                   (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200))))
                  (alpha (spawn-persona "Alpha"
                                        :backend :google
                                        :runtime-context alpha-context))
                  (beta (spawn-persona "Beta"
                                       :backend :google
                                       :runtime-context beta-context)))
             (let ((*standard-output* (make-string-output-stream)))
               (let ((results (query-all "Kickoff" :personas (list alpha beta))))
                 (fiveam:is (equal '(("Alpha" . "Alpha reply") ("Beta" . "Beta reply"))
                                   (mapcar (lambda (result)
                                             (cons (getf result :name)
                                                   (getf result :response)))
                                           results))))
               (let ((output (get-output-stream-string *standard-output*)))
                 (fiveam:is (search "[Alpha]" output))
                 (fiveam:is (search "Alpha reply" output))
                 (fiveam:is (search "[Beta]" output))
                 (fiveam:is (search "Beta reply" output))))
             (query-all "Second turn" :personas (list alpha beta))
             (let* ((alpha-second (cl-json:decode-json-from-string (first alpha-payloads)))
                   (alpha-texts (google-payload-texts alpha-second))
                    (beta-second (cl-json:decode-json-from-string (first beta-payloads)))
                   (beta-texts (google-payload-texts beta-second)))
               (fiveam:is (member "Kickoff" alpha-texts :test #'string=))
               (fiveam:is (member "Alpha reply" alpha-texts :test #'string=))
               (fiveam:is-false (member "Beta reply" alpha-texts :test #'string=))
               (fiveam:is (member "Kickoff" beta-texts :test #'string=))
               (fiveam:is (member "Beta reply" beta-texts :test #'string=))
               (fiveam:is-false (member "Alpha reply" beta-texts :test #'string=)))))
      (clear-personas))))

(fiveam:test test-query-all-replays-gemini-history-without-previous-interaction-id
  (let ((payloads nil))
    (flet ((gemini-stream (id text)
             (make-string-input-stream
              (format nil
                      "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"~A\"}}~%
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"~A\"}}~%
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"~A\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
                      id
                      text
                      id))))
      (unwind-protect
           (progn
             (clear-personas)
             (let* ((context (make-runtime-context
                              :gemini-api-key-function (lambda () "mocked-google-api-key")
                              :http-post-function
                              (lambda (url &rest args)
                                (declare (ignore url))
                                (push (getf args :content) payloads)
                                (values (gemini-stream "persona-session" "Gemini reply") 200))))
                    (persona (spawn-persona "Gem" :backend :gemini :runtime-context context)))
               (query-all "First turn" :personas (list persona))
               (query-all "Second turn" :personas (list persona))
               (let* ((second-payload (decode-test-json (first payloads)))
                      (input (interaction-payload-input second-payload)))
                 (fiveam:is-false (test-json-value-any second-payload '("previous_interaction_id" :previous-interaction-id)))
                 (fiveam:is (equal '("user_input" "model_output" "user_input")
                                   (mapcar (lambda (step)
                                             (test-json-value-any step '("type" :type)))
                                           input)))
                 (fiveam:is (equal '(("First turn")
                                     ("Gemini reply")
                                     ("Second turn"))
                                   (mapcar #'interaction-step-content-texts input))))))
        (clear-personas)))))

(fiveam:test test-remove-persona-and-reset-all-personas-support_repl_workflows
  (let ((alpha-context (make-runtime-context
                       :gemini-api-key-function (lambda () "mocked-google-api-key")
                       :http-post-function
                       (lambda (url &rest args)
                         (declare (ignore url args))
                         (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
        (beta-context (make-runtime-context
                      :gemini-api-key-function (lambda () "mocked-google-api-key")
                      :http-post-function
                      (lambda (url &rest args)
                        (declare (ignore url args))
                        (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200)))))
    (unwind-protect
         (progn
           (clear-personas)
           (let ((alpha (spawn-persona "Alpha" :backend :google :runtime-context alpha-context))
                 (beta (spawn-persona "Beta" :backend :google :runtime-context beta-context)))
             (query-all "Kickoff" :personas (list alpha beta))
             (fiveam:is (= 2 (reset-all-personas)))
             (fiveam:is (null (persona-history alpha)))
             (fiveam:is (null (persona-history beta)))
             (fiveam:is (eq beta (remove-persona "Beta")))
             (fiveam:is (equal '("Alpha") (list-personas)))
             (fiveam:is (null (remove-persona "Missing")))))
      (clear-personas))))

(fiveam:test test-stock-persona-catalog-and-spawn-helper
  (unwind-protect
       (progn
          (clear-personas)
          (fiveam:is (member :r-lee-ermey-drill-sergeant (list-stock-personas)))
          (fiveam:is (member :richard-feynman (list-stock-personas)))
          (let* ((drill (spawn-stock-persona :r-lee-ermey-drill-sergeant
                                             :model "gemini-3.5-flash"))
                 (feynman (spawn-stock-persona :richard-feynman
                                               :name "Feynman"
                                               :model "gemini-3.5-flash"))
                 (drill-instruction (chatbot-system-instruction
                                     (conversation-chatbot (persona-conversation drill))))
                 (feynman-instruction (chatbot-system-instruction
                                       (conversation-chatbot (persona-conversation feynman)))))
            (fiveam:is (equal '("Drill Sergeant" "Feynman")
                              (list-personas)))
            (fiveam:is (search "R. Lee Ermey" drill-instruction))
            (fiveam:is (search "Marine Corps drill sergeant" drill-instruction))
            (fiveam:is (search "Richard Feynman" feynman-instruction))
            (fiveam:is (search "thought experiments" feynman-instruction))
            (fiveam:signals error
              (spawn-stock-persona :unknown-stock-persona))))
    (clear-personas)))

(fiveam:test test-show-personas-prints_registry_summary
  (unwind-protect
        (progn
         (clear-personas)
         (spawn-persona "Alpha" :backend :google :model "gemini-3.5-flash")
         (let ((stream (make-string-output-stream)))
           (show-personas :stream stream)
           (let ((output (get-output-stream-string stream)))
             (fiveam:is (search "Alpha" output))
             (fiveam:is (search "BACKEND: GOOGLE" (string-upcase output)))
             (fiveam:is (search "model: gemini-3.5-flash" output)))))
    (clear-personas)))

(fiveam:test test-defpersona-replaces_existing_persona_and_uses_body_directives
  (unwind-protect
       (progn
         (clear-personas)
         (defpersona sparky (:backend :google
                             :model "gemini-3.5-flash"
                             :role "engineer")
           "Prefer practical solutions."
           "State tradeoffs explicitly.")
         (let* ((persona (find-persona "Sparky"))
                (instruction (chatbot-system-instruction
                             (conversation-chatbot (persona-conversation persona)))))
           (fiveam:is (typep persona 'persona))
           (fiveam:is (search "You are engineer." instruction))
           (fiveam:is (search "Prefer practical solutions." instruction))
           (fiveam:is (search "State tradeoffs explicitly." instruction)))
         (defpersona sparky (:backend :google
                             :model "gemini-3.5-flash"
                             :role "critic")
           "Focus on failure modes.")
         (let* ((persona (find-persona "Sparky"))
                (instruction (chatbot-system-instruction
                             (conversation-chatbot (persona-conversation persona)))))
           (fiveam:is (search "You are critic." instruction))
           (fiveam:is (search "Focus on failure modes." instruction))
           (fiveam:is-false (search "You are engineer." instruction))))
    (clear-personas)))

(fiveam:test test-run-arena-relays-one_persona_output_into_the_next_persona_prompt
  (let ((alpha-payloads nil)
        (beta-payloads nil))
    (unwind-protect
         (progn
           (clear-personas)
           (let* ((alpha-context (make-runtime-context
                                 :gemini-api-key-function (lambda () "mocked-google-api-key")
                                 :http-post-function
                                 (lambda (url &rest args)
                                   (declare (ignore url))
                                   (push (getf args :content) alpha-payloads)
                                   (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
                 (beta-context (make-runtime-context
                                :gemini-api-key-function (lambda () "mocked-google-api-key")
                                :http-post-function
                                (lambda (url &rest args)
                                  (declare (ignore url))
                                  (push (getf args :content) beta-payloads)
                                  (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200))))
                 (alpha (spawn-persona "Alpha"
                                       :backend :google
                                       :runtime-context alpha-context))
                 (beta (spawn-persona "Beta"
                                      :backend :google
                                      :runtime-context beta-context)))
             (let ((*standard-output* (make-string-output-stream)))
               (let ((results (run-arena "Kickoff prompt" :personas (list alpha beta) :rounds 1)))
                 (fiveam:is (equal '(("Alpha" . "Alpha reply") ("Beta" . "Beta reply"))
                                  (mapcar (lambda (result)
                                            (cons (getf result :name)
                                                  (getf result :response)))
                                          results))))
               (let ((output (get-output-stream-string *standard-output*)))
                 (fiveam:is (search "[User]" output))
                 (fiveam:is (search "[Alpha]" output))
                 (fiveam:is (search "Alpha reply" output))
                 (fiveam:is (search "[Beta]" output))
                 (fiveam:is (search "Beta reply" output))
                 (fiveam:is (search "[Your turn]" output))))
             (let* ((beta-first-payload (cl-json:decode-json-from-string (first beta-payloads)))
                   (beta-texts (google-payload-texts beta-first-payload)))
               (fiveam:is (find-if (lambda (text)
                                     (search "Alpha said: Alpha reply" text))
                                   beta-texts))
               (fiveam:is (find-if (lambda (text)
                                     (search "What is your response, Beta?" text))
                                   beta-texts)))))
      (clear-personas))))

(fiveam:test test-run-arena-supports_multiple_rounds_and_preserves_turn_order
  (let ((alpha-prompts nil)
        (beta-prompts nil))
    (unwind-protect
         (progn
           (clear-personas)
           (let* ((alpha-context (make-runtime-context
                                 :gemini-api-key-function (lambda () "mocked-google-api-key")
                                 :http-post-function
                                 (lambda (url &rest args)
                                   (declare (ignore url))
                                   (let ((prompt-texts (google-payload-texts
                                                        (decode-test-json (getf args :content)))))
                                     (push (format nil "~{~A~^ ~}" prompt-texts) alpha-prompts))
                                   (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
                 (beta-context (make-runtime-context
                                :gemini-api-key-function (lambda () "mocked-google-api-key")
                                :http-post-function
                                (lambda (url &rest args)
                                  (declare (ignore url))
                                  (let ((prompt-texts (google-payload-texts
                                                       (decode-test-json (getf args :content)))))
                                    (push (format nil "~{~A~^ ~}" prompt-texts) beta-prompts))
                                  (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200))))
                 (alpha (spawn-persona "Alpha"
                                       :backend :google
                                       :runtime-context alpha-context))
                 (beta (spawn-persona "Beta"
                                      :backend :google
                                      :runtime-context beta-context)))
             (let ((results (run-arena "Seed" :personas (list alpha beta) :rounds 2)))
               (fiveam:is (= 4 (length results))))
             (fiveam:is (= 2 (length alpha-prompts)))
             (fiveam:is (= 2 (length beta-prompts)))
             (fiveam:is (search "Seed" (second alpha-prompts)))
             (fiveam:is (search "Alpha reply" (second beta-prompts)))
             (fiveam:is (search "Beta reply" (first alpha-prompts)))))
      (clear-personas))))
