;;; tests-fact-cards.lisp - tests for per-turn Dense Fact Card distillation and embedding

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(defun fact-card-test-turn (user-text model-text &optional (turn-number 0))
  "Returns a TURN structure containing one user/model exchange for fact-card tests."
  (first (partition-messages-into-turns
          (list (test-message "user" user-text turn-number)
                (test-message "model" model-text turn-number)))))

(fiveam:test test-turn-user-prompt-text-and-model-response-text-extract-correctly
  "TURN-USER-PROMPT-TEXT and TURN-MODEL-RESPONSE-TEXT pull the right message content."
  (let ((a-turn (fact-card-test-turn "I tried Wellbutrin once." "Norepinephrine-dopamine spike...")))
    (fiveam:is (string= "I tried Wellbutrin once." (turn-user-prompt-text a-turn)))
    (fiveam:is (string= "Norepinephrine-dopamine spike..." (turn-model-response-text a-turn)))))

(fiveam:test test-turn-model-response-text-concatenates-multiple-model-messages
  "TURN-MODEL-RESPONSE-TEXT concatenates every model-authored message in a turn, in order."
  (let* ((messages (list (test-message "user" "hi" 0)
                         (test-message "model" "first part" 0)
                         (test-message "model" "second part" 0)))
         (a-turn (first (partition-messages-into-turns messages))))
    (fiveam:is (string= (format nil "first part~%second part") (turn-model-response-text a-turn)))))

(fiveam:test test-turn-fact-card-prompt-includes-instruction-and-content
  "TURN-FACT-CARD-PROMPT embeds the instruction and the turn's raw user/model text."
  (let* ((a-turn (fact-card-test-turn "User text here" "Model text here"))
         (prompt (turn-fact-card-prompt a-turn)))
    (fiveam:is (search +fact-card-instruction+ prompt))
    (fiveam:is (search "User: User text here" prompt))
    (fiveam:is (search "Model: Model text here" prompt))))

(fiveam:test test-persist-turn-fact-card-embeds-into-persona-facts-collection
  "PERSIST-TURN-FACT-CARD embeds the fact card text into the <persona>_Facts collection."
  (let* ((mock-get-called-p nil)
         (mock-embed-called-p nil)
         (add-payloads nil)
         (context (make-test-backend-runtime-context nil))
         (a-turn (fact-card-test-turn "hi" "hello" 7)))
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (cond
              ((search "/heartbeat" url) "{\"nanosecond heartbeat\": 1}")
              ((search "/collections/V_Facts" url)
               "{\"name\": \"V_Facts\", \"id\": \"v-facts-uuid\", \"metadata\": null}")
              (t (error "Unexpected GET URL: ~A" url)))))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (cond
              ((search "embedContent" url)
               (setf mock-embed-called-p t)
               "{\"embedding\": {\"values\": [0.1, 0.2, 0.3]}}")
              ((search "/add" url)
               (push (cl-json:decode-json-from-string (getf args :content)) add-payloads)
               "")
              (t (error "Unexpected POST URL: ~A" url)))))
    (call-with-runtime-context context
      (lambda ()
        (persist-turn-fact-card "V" a-turn "The user experienced severe dysphoric rage.")))
    (fiveam:is (not (null mock-get-called-p)))
    (fiveam:is (not (null mock-embed-called-p)))
    (fiveam:is (= 1 (length add-payloads)))
    (let ((payload (first add-payloads)))
      (fiveam:is (equal '("fact-turn-7") (coerce (cdr (assoc :ids payload)) 'list)))
      (fiveam:is (equal '("The user experienced severe dysphoric rage.")
                       (coerce (cdr (assoc :documents payload)) 'list))))))

(fiveam:test test-persist-turn-fact-card-skips-when-fact-card-text-is-empty
  "PERSIST-TURN-FACT-CARD is a no-op when given empty or nil fact card text."
  (let ((context (make-test-backend-runtime-context nil))
        (a-turn (fact-card-test-turn "hi" "hello" 0)))
    (setf (runtime-context-http-get-function context)
          (lambda (&rest args) (declare (ignore args)) (error "Unexpected GET.")))
    (setf (runtime-context-http-post-function context)
          (lambda (&rest args) (declare (ignore args)) (error "Unexpected POST.")))
    (call-with-runtime-context context
      (lambda ()
        (persist-turn-fact-card "V" a-turn "")
        (persist-turn-fact-card "V" a-turn nil)))))

(fiveam:test test-generate-and-persist-turn-fact-card-runs-full-pipeline
  "GENERATE-AND-PERSIST-TURN-FACT-CARD distills a turn via the cheap model and embeds it."
  (let* ((mock-get-called-p nil)
         (mock-embed-called-p nil)
         (add-payloads nil)
         (generate-content-called-p nil)
         (bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash" :persona-name "V"))
         (context (make-test-backend-runtime-context nil))
         (a-turn (fact-card-test-turn "I tried Wellbutrin once." "Norepinephrine-dopamine spike." 42)))
    (setf (chatbot-runtime-context bot) context)
    (setf (runtime-context-http-get-function context)
          (lambda (url &rest args)
            (declare (ignore args))
            (setf mock-get-called-p t)
            (cond
              ((search "/heartbeat" url) "{\"nanosecond heartbeat\": 1}")
              ((search "/collections/V_Facts" url)
               "{\"name\": \"V_Facts\", \"id\": \"v-facts-uuid\", \"metadata\": null}")
              (t (error "Unexpected GET URL: ~A" url)))))
    (setf (runtime-context-http-post-function context)
          (lambda (url &rest args)
            (cond
              ((search "embedContent" url)
               (setf mock-embed-called-p t)
               "{\"embedding\": {\"values\": [0.1, 0.2]}}")
              ((search "generateContent" url)
               (setf generate-content-called-p t)
               (values
                "{\"usageMetadata\": {\"promptTokenCount\": 1, \"candidatesTokenCount\": 1, \"totalTokenCount\": 2}, \"candidates\": [{\"content\": {\"parts\": [{\"text\": \"Dense fact card text.\"}], \"role\": \"model\"}}]}"
                200))
              ((search "/add" url)
               (push (cl-json:decode-json-from-string (getf args :content)) add-payloads)
               "")
              (t (error "Unexpected POST URL: ~A" url)))))
    (setf (runtime-context-gemini-api-key-function context) (lambda () "mocked-api-key"))
    (call-with-runtime-context context
      (lambda ()
        (generate-and-persist-turn-fact-card bot "V" a-turn)))
    (fiveam:is (not (null generate-content-called-p)))
    (fiveam:is (not (null mock-get-called-p)))
    (fiveam:is (not (null mock-embed-called-p)))
    (fiveam:is (= 1 (length add-payloads)))
    (let ((payload (first add-payloads)))
      (fiveam:is (equal '("fact-turn-42") (coerce (cdr (assoc :ids payload)) 'list)))
      (fiveam:is (equal '("Dense fact card text.")
                       (coerce (cdr (assoc :documents payload)) 'list))))))

(fiveam:test test-maybe-generate-turn-fact-card-in-background-skips-without-persona-name
  "MAYBE-GENERATE-TURN-FACT-CARD-IN-BACKGROUND is a no-op when the conversation's bot has no persona name."
  (let* ((*fact-cards-enabled-p* t)
         (bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
         (messages (list (test-message "user" "hi" 0) (test-message "model" "hello" 0)))
         (conv (make-instance 'conversation :chatbot bot :messages messages)))
    (fiveam:is-false (chatbot-persona-name bot))
    (fiveam:is-false (maybe-generate-turn-fact-card-in-background conv))))

(fiveam:test test-maybe-generate-turn-fact-card-in-background-skips-without-turns
  "MAYBE-GENERATE-TURN-FACT-CARD-IN-BACKGROUND is a no-op when the conversation has no completed turns."
  (let* ((*fact-cards-enabled-p* t)
         (bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash" :persona-name "V"))
         (conv (make-instance 'conversation :chatbot bot)))
    (fiveam:is-false (maybe-generate-turn-fact-card-in-background conv))))

(fiveam:test test-maybe-generate-turn-fact-card-in-background-respects-kill-switch
  "MAYBE-GENERATE-TURN-FACT-CARD-IN-BACKGROUND is a no-op when *FACT-CARDS-ENABLED-P* is NIL,
even for an otherwise-eligible persona conversation."
  (let* ((*fact-cards-enabled-p* nil)
         (bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash" :persona-name "V"))
         (messages (list (test-message "user" "hi" 0) (test-message "model" "hello" 0)))
         (conv (make-instance 'conversation :chatbot bot :messages messages)))
    (fiveam:is-false (maybe-generate-turn-fact-card-in-background conv))))
