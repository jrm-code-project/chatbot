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
    (fiveam:is (= 3 (length history)))
    (fiveam:is (string= "user" (cdr (assoc "role" (first history) :test #'string=))))
    (fiveam:is (string= "User: Hello"
                        (cdr (assoc "content" (first history) :test #'string=))))
    (fiveam:is (string= "assistant" (cdr (assoc "role" (second history) :test #'string=))))
    (fiveam:is (string= "Alpha: Alpha one"
                        (cdr (assoc "content" (second history) :test #'string=))))
    (fiveam:is (string= "user" (cdr (assoc "role" (third history) :test #'string=))))
    (fiveam:is (string= "Beta: Beta one"
                        (cdr (assoc "content" (third history) :test #'string=))))))

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
        (let* ((decoded-first (cl-json:decode-json-from-string first-payload))
               (decoded-second (cl-json:decode-json-from-string second-payload))
               (first-contents (cdr (assoc :contents decoded-first)))
               (second-contents (cdr (assoc :contents decoded-second))))
          (fiveam:is (= 1 (length first-contents)))
          (fiveam:is (string= "User: Kickoff"
                              (cdr (assoc :text
                                          (car (cdr (assoc :parts (first first-contents))))))))
          (fiveam:is (= 2 (length second-contents)))
          (fiveam:is (string= "User: Kickoff"
                              (cdr (assoc :text
                                          (car (cdr (assoc :parts (first second-contents))))))))
          (fiveam:is (string= "Alpha: Alpha reply"
                              (cdr (assoc :text
                                          (car (cdr (assoc :parts (second second-contents)))))))))))))

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
        (let ((second-alpha-payload (first alpha-payloads)))
          (fiveam:is-false (search "\"previous_interaction_id\"" second-alpha-payload))
          (fiveam:is (search "\"type\":\"model_output\"" second-alpha-payload))
          (fiveam:is (search "Beta: Beta reply" second-alpha-payload))
          (fiveam:is (search "User: Second turn" second-alpha-payload)))))))
