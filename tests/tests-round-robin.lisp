;;; tests-round-robin.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-round-robin-session-validates-participants
  (let* ((conv-a (new-chat :backend :google))
         (conv-b (new-chat :backend :google))
         (participant-a (make-round-robin-participant :name "Alpha" :conversation conv-a))
         (participant-b (make-round-robin-participant :name "Beta" :conversation conv-b)))
    (fiveam:signals error
      (new-round-robin-chat (list participant-a)))
    (fiveam:signals error
      (new-round-robin-chat
       (list participant-a
             (make-round-robin-participant :name "Alpha" :conversation conv-b))))
    (fiveam:is (typep (new-round-robin-chat (list participant-a participant-b))
                      'round-robin-session))))

(fiveam:test test-round-robin-history-maps-own-prior-turns-to-assistant-role
  (let* ((transcript (list (round-robin-transcript-entry "User" :user "Hello")
                           (round-robin-transcript-entry "Alpha" :bot "Alpha one")
                           (round-robin-transcript-entry "Beta" :bot "Beta one")))
         (history (build-round-robin-history-messages transcript "Alpha")))
    (assert-history-sequence
     history
     '(("user" "User: Hello")
       ("assistant" "Alpha: Alpha one")
       ("user" "Beta: Beta one")))))

(fiveam:test test-round-robin-state-machine-enforces-legal-transition-order
  (let* ((session (new-round-robin-chat
                   (list (make-round-robin-participant :name "Alpha"
                                                       :conversation (new-chat :backend :google))
                         (make-round-robin-participant :name "Beta"
                                                       :conversation (new-chat :backend :google)))))
         (participant-count (length (round-robin-session-participants session)))
         (initial-state (round-robin-session-state session)))
    (fiveam:is (equal '(:phase :awaiting-user-input :next-participant-index 0)
                      initial-state))
    (let ((after-user (advance-round-robin-session-state initial-state
                                                         :user-entry-recorded
                                                         participant-count)))
      (fiveam:is (equal '(:phase :participant-ready :next-participant-index 0)
                        after-user))
      (let ((run-alpha (plan-round-robin-session-step after-user participant-count)))
        (fiveam:is (eq :run-participant (getf run-alpha :kind)))
        (fiveam:is (= 0 (getf run-alpha :participant-index)))
        (fiveam:is (equal '(:phase :participant-running :next-participant-index 0)
                          (getf run-alpha :next-state)))
        (let* ((after-alpha (advance-round-robin-session-state (getf run-alpha :next-state)
                                                               :participant-response-recorded
                                                               participant-count))
               (advance-to-beta (plan-round-robin-session-step after-alpha participant-count))
               (beta-ready (getf advance-to-beta :next-state))
               (run-beta (plan-round-robin-session-step beta-ready participant-count))
               (after-beta (advance-round-robin-session-state (getf run-beta :next-state)
                                                              :participant-response-recorded
                                                              participant-count))
               (finish-turn (plan-round-robin-session-step after-beta participant-count))
               (reset-state (getf finish-turn :next-state)))
          (fiveam:is (equal '(:phase :participant-complete :next-participant-index 0)
                            after-alpha))
          (fiveam:is (eq :advance (getf advance-to-beta :kind)))
          (fiveam:is (equal '(:phase :participant-ready :next-participant-index 1)
                            beta-ready))
          (fiveam:is (eq :run-participant (getf run-beta :kind)))
          (fiveam:is (= 1 (getf run-beta :participant-index)))
          (fiveam:is (equal '(:phase :participant-complete :next-participant-index 1)
                            after-beta))
          (fiveam:is (eq :advance (getf finish-turn :kind)))
          (fiveam:is (equal '(:phase :turn-complete :next-participant-index 0)
                            reset-state))
          (fiveam:is (eq :finish-turn
                         (getf (plan-round-robin-session-step reset-state participant-count)
                               :kind))))))
    (fiveam:signals error
      (advance-round-robin-session-state initial-state
                                         :participant-response-recorded
                                         participant-count))))

(fiveam:test test-round-robin-clones-conversation-through-shared-copy-helpers
  (let* ((runtime-context (make-runtime-context))
         (allowed-directories (list #p"C:/tmp/allowed-a/" #p"C:/tmp/allowed-b/"))
         (mcp-servers (list :shared-tool-server))
         (source-conversation
           (make-instance 'conversation
                          :chatbot (make-instance 'chatbot
                                                  :backend :google
                                                  :model "gemini-3.5-flash"
                                                  :system-instruction "Be skeptical."
                                                  :system-instruction-path #p"C:/tmp/system-instruction.md"
                                                  :system-instruction-storage-kind :markdown-file
                                                  :temperature 0.25d0
                                                  :top-p 0.75d0
                                                  :google-search-p t
                                                  :gemini-fallback-to-google-p t
                                                  :web-tools-p t
                                                  :code-execution-p t
                                                  :include-timestamp-p t
                                                  :include-model-p t
                                                  :enable-eval-p t
                                                  :filesystem-tools-p t
                                                  :filesystem-root-directory #p"C:/tmp/persona/"
                                                  :filesystem-allowed-directories allowed-directories
                                                  :filesystem-allowlist-path #p"C:/tmp/filesystem-allowlist.lisp"
                                                  :mcp-servers mcp-servers
                                                  :mcp-startup-status :ready
                                                  :runtime-context runtime-context)
                          :persona-memory "Compressed persona memory."
                          :persona-diary-entries '(((:filename . "001.txt")
                                                    (:content . "First diary entry.")))
                          :interaction-id nil
                          :messages nil))
         (session (new-round-robin-chat
                   (list (make-round-robin-participant :name "Alpha"
                                                       :conversation source-conversation)
                         (make-round-robin-participant :name "Beta"
                                                       :conversation (new-chat :backend :google)))))
         (cloned-conversation
           (round-robin-participant-conversation
            (first (round-robin-session-participants session))))
         (source-bot (conversation-chatbot source-conversation))
         (cloned-bot (conversation-chatbot cloned-conversation)))
    (fiveam:is (not (eq source-conversation cloned-conversation)))
    (fiveam:is (not (eq source-bot cloned-bot)))
    (fiveam:is (eq runtime-context (chatbot-runtime-context cloned-bot)))
    (fiveam:is (eq mcp-servers (chatbot-mcp-servers cloned-bot)))
    (fiveam:is (equal allowed-directories
                      (chatbot-filesystem-allowed-directories cloned-bot)))
    (fiveam:is (string= (chatbot-model source-bot)
                        (chatbot-model cloned-bot)))
    (fiveam:is (string= (chatbot-system-instruction source-bot)
                        (chatbot-system-instruction cloned-bot)))
    (fiveam:is (eq (chatbot-system-instruction-storage-kind source-bot)
                   (chatbot-system-instruction-storage-kind cloned-bot)))
    (fiveam:is (= (chatbot-temperature source-bot)
                  (chatbot-temperature cloned-bot)))
    (fiveam:is (= (chatbot-top-p source-bot)
                  (chatbot-top-p cloned-bot)))
    (fiveam:is-true (chatbot-google-search-p cloned-bot))
    (fiveam:is-true (chatbot-gemini-fallback-to-google-p cloned-bot))
    (fiveam:is-true (chatbot-web-tools-p cloned-bot))
    (fiveam:is-true (chatbot-code-execution-p cloned-bot))
    (fiveam:is-true (chatbot-include-timestamp-p cloned-bot))
    (fiveam:is-true (chatbot-include-model-p cloned-bot))
    (fiveam:is-true (chatbot-enable-eval-p cloned-bot))
    (fiveam:is-true (chatbot-filesystem-tools-p cloned-bot))
    (fiveam:is (equal (conversation-persona-memory source-conversation)
                      (conversation-persona-memory cloned-conversation)))
    (fiveam:is (equal (conversation-persona-diary-entries source-conversation)
                      (conversation-persona-diary-entries cloned-conversation)))
    (fiveam:is (null (conversation-messages cloned-conversation)))
    (fiveam:is (null (conversation-interaction-id cloned-conversation)))))

(fiveam:test test-round-robin-chat-sequentially-feeds-next-participant
  (let ((first-payload nil)
        (second-payload nil))
    (let* ((context-a (make-runtime-context
                       :gemini-api-key-function (lambda () "mocked-google-api-key")
                       :http-post-function
                       (lambda (url &rest args)
                         (declare (ignore url))
                         (setf first-payload (getf args :content))
                         (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
           (context-b (make-runtime-context
                       :gemini-api-key-function (lambda () "mocked-google-api-key")
                       :http-post-function
                       (lambda (url &rest args)
                         (declare (ignore url))
                         (setf second-payload (getf args :content))
                         (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200))))
           (session
             (new-round-robin-chat
              (list (make-round-robin-participant
                     :name "Alpha"
                     :conversation (new-chat :backend :google :runtime-context context-a))
                    (make-round-robin-participant
                     :name "Beta"
                     :conversation (new-chat :backend :google :runtime-context context-b))))))
      (let ((results (round-robin-chat "Kickoff" :session session)))
        (fiveam:is (equal '(("Alpha" . "Alpha reply") ("Beta" . "Beta reply"))
                          (mapcar (lambda (result)
                                    (cons (getf result :name)
                                          (getf result :response)))
                                  results)))
        (fiveam:is (eq :awaiting-user-input (round-robin-session-phase session)))
        (fiveam:is (= 0 (round-robin-session-next-participant-index session)))
        (let* ((decoded-first (cl-json:decode-json-from-string first-payload))
               (decoded-second (cl-json:decode-json-from-string second-payload))
               (first-contents (google-payload-contents decoded-first))
               (second-contents (google-payload-contents decoded-second)))
          (fiveam:is (= 1 (length first-contents)))
          (assert-google-message-texts (first first-contents)
                                       "user"
                                       '("User: Kickoff"))
          (fiveam:is (= 2 (length second-contents)))
          (assert-google-message-texts (first second-contents)
                                       "user"
                                       '("User: Kickoff"))
          (assert-google-message-texts (second second-contents)
                                       "user"
                                       '("Alpha: Alpha reply")))))))

(fiveam:test test-round-robin-chat-prints-speaker-headings-and-user-turn-marker
  (let* ((context-a (make-runtime-context
                     :gemini-api-key-function (lambda () "mocked-google-api-key")
                     :http-post-function
                     (lambda (url &rest args)
                       (declare (ignore url args))
                       (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Alpha reply\"}], \"role\": \"model\"}}]}" 200))))
         (context-b (make-runtime-context
                     :gemini-api-key-function (lambda () "mocked-google-api-key")
                     :http-post-function
                     (lambda (url &rest args)
                       (declare (ignore url args))
                       (values "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Beta reply\"}], \"role\": \"model\"}}]}" 200))))
         (session
           (new-round-robin-chat
            (list (make-round-robin-participant
                   :name "Alpha"
                   :conversation (new-chat :backend :google :runtime-context context-a))
                  (make-round-robin-participant
                   :name "Beta"
                   :conversation (new-chat :backend :google :runtime-context context-b)))
            :user-name "Operator")))
    (let ((*standard-output* (make-string-output-stream)))
      (round-robin-chat "Kickoff" :session session)
      (let ((output (get-output-stream-string *standard-output*)))
        (fiveam:is (search "[Operator]" output))
        (fiveam:is (search "Kickoff" output))
        (fiveam:is (search "[Alpha]" output))
        (fiveam:is (search "Alpha reply" output))
        (fiveam:is (search "[Beta]" output))
        (fiveam:is (search "Beta reply" output))
        (fiveam:is (search "[Your turn]" output))))))

(fiveam:test test-round-robin-gemini-resets-interaction-id-between-rounds
  (let ((alpha-payloads nil)
        (beta-payloads nil))
    (flet ((gemini-stream (id text)
             (make-string-input-stream
              (format nil
                      "data: {\"event_type\":\"interaction.created\",\"interaction\":{\"id\":\"~A\"}}~%
data: {\"event_type\":\"step.delta\",\"delta\":{\"type\":\"text\",\"text\":\"~A\"}}~%
data: {\"event_type\":\"interaction.completed\",\"interaction\":{\"id\":\"~A\",\"model\":\"gemini-3.5-flash\",\"usage\":{\"total_input_tokens\":1,\"total_output_tokens\":1,\"total_tokens\":2}}}"
                      id
                      text
                      id))))
      (let* ((context-a (make-runtime-context
                         :gemini-api-key-function (lambda () "mocked-google-api-key")
                         :http-post-function
                         (lambda (url &rest args)
                           (declare (ignore url))
                           (push (getf args :content) alpha-payloads)
                           (values (gemini-stream "alpha-session" "Alpha reply") 200))))
             (context-b (make-runtime-context
                         :gemini-api-key-function (lambda () "mocked-google-api-key")
                         :http-post-function
                         (lambda (url &rest args)
                           (declare (ignore url))
                           (push (getf args :content) beta-payloads)
                           (values (gemini-stream "beta-session" "Beta reply") 200))))
             (session
               (new-round-robin-chat
                (list (make-round-robin-participant
                       :name "Alpha"
                       :conversation (new-chat :backend :gemini :runtime-context context-a))
                      (make-round-robin-participant
                       :name "Beta"
                       :conversation (new-chat :backend :gemini :runtime-context context-b))))))
        (round-robin-chat "First turn" :session session)
        (round-robin-chat "Second turn" :session session)
        (fiveam:is (= 2 (length alpha-payloads)))
        (let* ((second-alpha-payload (decode-test-json (first alpha-payloads)))
               (input (interaction-payload-input second-alpha-payload)))
          (fiveam:is-false (test-json-value-any second-alpha-payload
                                                '("previous_interaction_id" :previous-interaction-id)))
          (fiveam:is (equal '("user_input" "model_output" "user_input" "user_input")
                            (mapcar (lambda (step)
                                      (test-json-value-any step '("type" :type)))
                                    input)))
          (fiveam:is (equal '(("User: First turn")
                              ("Alpha: Alpha reply")
                              ("Beta: Beta reply")
                              ("User: Second turn"))
                            (mapcar #'interaction-step-content-texts input))))))))
