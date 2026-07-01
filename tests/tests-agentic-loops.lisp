;;; tests-agentic-loops.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(defun test-agentic-loop-response (status summary)
  "Returns one strict JSON control response for agentic loop tests."
  (cl-json:encode-json-to-string `(("status" . ,status)
                                   ("summary" . ,summary))))

(fiveam:test test-parse-agentic-loop-control-response-accepts-provider-wrapper
  (let ((control (parse-agentic-loop-control-response
                  "<antml:thinking>draft</antml:thinking>{\"status\":\"continue\",\"summary\":\"next step\"}")))
    (fiveam:is (string= "continue" (getf control :status)))
    (fiveam:is (string= "next step" (getf control :summary)))))

(defun wait-for-agentic-loop-status (loop statuses &key (timeout-seconds 3.0d0))
  "Polls LOOP until its status is one of STATUSES or TIMEOUT-SECONDS elapses."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop
      when (member (agentic-loop-status loop) statuses)
        do (return (agentic-loop-status loop))
      when (> (get-internal-real-time) deadline)
        do (return nil)
      do (sleep 0.01))))

(fiveam:test test-start-agentic-loop-completes-and-records-result
  (let ((*agentic-loop-chat-function*
         (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt conversation callback file files temperature top-p))
           (test-agentic-loop-response "final" "loop finished")))
        (conversation (new-chat :backend :openai)))
    (unwind-protect
         (let ((loop (start-agentic-loop conversation "Finish the task" :max-iterations 3)))
           (fiveam:is (eq :completed
                          (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
           (fiveam:is (= 1 (agentic-loop-current-iteration loop)))
           (fiveam:is (string= "loop finished" (agentic-loop-result-summary loop)))
           (fiveam:is (= 1 (length (agentic-loop-step-history loop)))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-start-agentic-loop-pauses-for-approval-and-resumes
  (let* ((context (make-runtime-context
                   :eval-approval-function
                   (lambda (&rest ignored)
                     (declare (ignore ignored))
                     (error "Loop runtime context should override eval approval."))))
         (conversation (new-chat :backend :openai
                                 :enable-eval-p t
                                 :runtime-context context))
         (calls 0)
         (*agentic-loop-chat-function*
           (lambda (prompt &key conversation callback file files temperature top-p)
             (declare (ignore prompt callback file files temperature top-p))
             (incf calls)
             (approve-chatbot-eval-expression
              (conversation-chatbot conversation)
              "(+ 1 2)"
              "eval")
             (test-agentic-loop-response "final" "eval complete"))))
    (unwind-protect
         (let ((loop (start-agentic-loop conversation "Run eval" :max-iterations 2)))
           (fiveam:is (eq :awaiting-approval
                          (wait-for-agentic-loop-status loop '(:awaiting-approval :completed :failed))))
           (fiveam:is (equal :eval (getf (agentic-loop-pending-approval loop) :kind)))
           (fiveam:is (string= "eval" (getf (agentic-loop-pending-approval loop) :tool-name)))
           (resume-agentic-loop (agentic-loop-id loop) :approve t)
           (fiveam:is (eq :completed
                          (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
           (fiveam:is (= 1 calls))
           (fiveam:is (string= "eval complete" (agentic-loop-result-summary loop))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-abort-agentic-loop-interrupts-in-flight-step-cooperatively
  (let* ((entered-step-p nil)
         (release-step-p nil)
         (*agentic-loop-chat-function*
          (lambda (prompt &key conversation callback file files temperature top-p)
            (declare (ignore prompt conversation callback file files temperature top-p))
            (setf entered-step-p t)
            (loop until release-step-p
                  do (sleep 0.01))
            (test-agentic-loop-response "final" "this response must not win after abort")))
         (conversation (new-chat :backend :openai)))
    (unwind-protect
         (let ((loop (start-agentic-loop conversation "Interrupt the running step" :max-iterations 3)))
           (loop repeat 100
                 until entered-step-p
                 do (sleep 0.01))
           (fiveam:is (not (null entered-step-p)))
           (abort-agentic-loop (agentic-loop-id loop) :force t)
           (setf release-step-p t)
           (fiveam:is (eq :interrupted
                          (wait-for-agentic-loop-status loop '(:interrupted :completed :failed :limit-reached))))
           (loop repeat 100
                 while (agentic-loop-thread-alive-p loop)
                 do (sleep 0.01))
           (fiveam:is (string= "Interrupted." (agentic-loop-result-summary loop)))
           (fiveam:is (string= "Interrupted." (agentic-loop-last-error loop)))
           (fiveam:is (= 1 (length (agentic-loop-step-history loop))))
           (fiveam:is (eq :interrupted
                          (getf (first (agentic-loop-step-history loop)) :status))))
      (setf release-step-p t)
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-start-agentic-loop-applies-backend-and-model-overrides
  (let* ((observed-backend nil)
         (observed-model nil)
         (*agentic-loop-chat-function*
          (lambda (prompt &key conversation callback file files temperature top-p)
            (declare (ignore prompt callback file files temperature top-p))
            (setf observed-backend (chatbot-backend (conversation-chatbot conversation)))
            (setf observed-model (chatbot-model (conversation-chatbot conversation)))
            (test-agentic-loop-response "final" "override applied")))
         (conversation (new-chat :backend :google :model "gemini-3.5-flash")))
    (unwind-protect
         (let ((loop (start-agentic-loop conversation
                                        "Use another provider"
                                        :backend :openai
                                        :model "gpt-4o-mini"
                                        :max-iterations 2)))
           (fiveam:is (eq :completed
                         (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
           (fiveam:is (eq :openai observed-backend))
           (fiveam:is (string= "gpt-4o-mini" observed-model))
           (fiveam:is (eq :openai (getf (agentic-loop-execution-profile loop) :backend)))
           (fiveam:is (string= "gpt-4o-mini" (getf (agentic-loop-execution-profile loop) :model))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-start-agentic-loop-uses-runtime-context-default-backend-and-model
  (let* ((observed-backend nil)
         (observed-model nil)
         (*agentic-loop-chat-function*
          (lambda (prompt &key conversation callback file files temperature top-p)
            (declare (ignore prompt callback file files temperature top-p))
            (setf observed-backend (chatbot-backend (conversation-chatbot conversation)))
            (setf observed-model (chatbot-model (conversation-chatbot conversation)))
            (test-agentic-loop-response "final" "default profile applied")))
         (context (make-runtime-context :agentic-loop-default-backend :openai
                                        :agentic-loop-default-model "gpt-4o-mini"))
         (conversation (new-chat :backend :google
                                 :model "gemini-3.5-flash"
                                 :runtime-context context)))
    (unwind-protect
         (let ((loop (start-agentic-loop conversation
                                        "Use the configured default loop profile"
                                        :max-iterations 2)))
           (fiveam:is (eq :completed
                          (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
           (fiveam:is (eq :openai observed-backend))
           (fiveam:is (string= "gpt-4o-mini" observed-model))
           (fiveam:is (eq :openai (getf (agentic-loop-execution-profile loop) :backend)))
           (fiveam:is (string= "gpt-4o-mini" (getf (agentic-loop-execution-profile loop) :model))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-start-agentic-loop-explicit-overrides-win-over-defaults
  (let* ((observed-backend nil)
         (observed-model nil)
         (*agentic-loop-chat-function*
          (lambda (prompt &key conversation callback file files temperature top-p)
            (declare (ignore prompt callback file files temperature top-p))
            (setf observed-backend (chatbot-backend (conversation-chatbot conversation)))
            (setf observed-model (chatbot-model (conversation-chatbot conversation)))
            (test-agentic-loop-response "final" "explicit profile applied")))
         (context (make-runtime-context :agentic-loop-default-backend :openai
                                        :agentic-loop-default-model "gpt-4o-mini"))
         (conversation (new-chat :backend :google
                                 :model "gemini-3.5-flash"
                                 :runtime-context context)))
    (unwind-protect
         (let ((loop (start-agentic-loop conversation
                                        "Override the configured default loop profile"
                                        :backend :lm-studio
                                        :model "custom-loop-model"
                                        :max-iterations 2)))
           (fiveam:is (eq :completed
                          (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
           (fiveam:is (eq :lm-studio observed-backend))
           (fiveam:is (string= "custom-loop-model" observed-model))
           (fiveam:is (eq :lm-studio (getf (agentic-loop-execution-profile loop) :backend)))
           (fiveam:is (string= "custom-loop-model" (getf (agentic-loop-execution-profile loop) :model))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-start-agentic-loop-tool-uses-active-conversation
  (let ((*agentic-loop-chat-function*
         (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt conversation callback file files temperature top-p))
           (test-agentic-loop-response "final" "started by tool")))
        (conversation (new-chat :backend :openai)))
    (unwind-protect
         (let* ((*active-conversation* conversation)
                (json (default-execute-builtin-chatbot-tool
                       (conversation-chatbot conversation)
                       "startAgenticLoop"
                       '(("goal" . "Tool-launched goal")
                        ("maxIterations" . 2)
                        ("backend" . "openai")
                        ("model" . "gpt-4o"))))
                (payload (parse-json-or-error json :context "agentic loop tool result"))
                (loop-id (mcp-val :id payload))
                (loop (find-agentic-loop loop-id))
               (execution-profile (json-object-field payload :execution-profile)))
           (fiveam:is (typep loop 'agentic-loop))
           (fiveam:is (string= "Tool-launched goal" (agentic-loop-goal loop)))
           (fiveam:is (eq :openai (getf (agentic-loop-execution-profile loop) :backend)))
           (fiveam:is (string= "gpt-4o" (getf (agentic-loop-execution-profile loop) :model)))
           (assert-json-field= execution-profile :backend "openai")
           (assert-json-field= execution-profile :model "gpt-4o"))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-start-agentic-loop-tool-uses-runtime-context-default-profile
  (let* ((*agentic-loop-chat-function*
          (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt conversation callback file files temperature top-p))
           (test-agentic-loop-response "final" "started by tool with defaults")))
         (context (make-runtime-context :agentic-loop-default-backend :openai
                                       :agentic-loop-default-model "gpt-4.1-mini"))
         (conversation (new-chat :backend :google
                                :model "gemini-3.5-flash"
                                :runtime-context context)))
    (unwind-protect
         (let* ((*active-conversation* conversation)
               (json (default-execute-builtin-chatbot-tool
                      (conversation-chatbot conversation)
                      "startAgenticLoop"
                      '(("goal" . "Tool-launched goal with defaults")
                        ("maxIterations" . 2))))
               (payload (parse-json-or-error json :context "agentic loop tool result"))
               (loop-id (mcp-val :id payload))
               (loop (find-agentic-loop loop-id))
               (execution-profile (json-object-field payload :execution-profile)))
           (fiveam:is (typep loop 'agentic-loop))
           (fiveam:is (eq :openai (getf (agentic-loop-execution-profile loop) :backend)))
           (fiveam:is (string= "gpt-4.1-mini" (getf (agentic-loop-execution-profile loop) :model)))
           (assert-json-field= execution-profile :backend "openai")
           (assert-json-field= execution-profile :model "gpt-4.1-mini"))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))


(fiveam:test test-monitor-restarts-failed-agentic-loop-within-budget
  (let* ((*agentic-loop-supervisor-max-restarts* 1)
        (*agentic-loop-supervisor-restart-backoff-seconds* 0.0d0)
        (calls 0)
        (*agentic-loop-chat-function*
         (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt conversation callback file files temperature top-p))
           (incf calls)
           (if (= calls 1)
               (error "Malformed response from minion.")
               (test-agentic-loop-response "final" "recovered after restart"))))
        (conversation (new-chat :backend :openai)))
    (unwind-protect
        (let ((loop (start-agentic-loop conversation "Recover from failure" :max-iterations 2)))
          (fiveam:is (eq :failed
                         (wait-for-agentic-loop-status loop '(:failed :completed :limit-reached))))
          (fiveam:is (= 1 calls))
          (monitor-agentic-loops-once)
          (monitor-agentic-loops-once)
          (fiveam:is (eq :completed
                         (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
          (fiveam:is (= 2 calls))
          (fiveam:is (= 1 (agentic-loop-supervisor-restart-count loop)))
          (fiveam:is (string= "recovered after restart" (agentic-loop-result-summary loop))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-monitor-restarts-agentic-loop-after-invalid-structured-output
  (let* ((*agentic-loop-supervisor-max-restarts* 1)
         (*agentic-loop-supervisor-restart-backoff-seconds* 0.0d0)
         (calls 0)
         (*agentic-loop-chat-function*
          (lambda (prompt &key conversation callback file files temperature top-p)
            (declare (ignore prompt conversation callback file files temperature top-p))
            (incf calls)
            (if (= calls 1)
                "Here is your code!"
                (test-agentic-loop-response "final" "recovered after invalid structured output"))))
         (conversation (new-chat :backend :openai)))
    (unwind-protect
        (let ((loop (start-agentic-loop conversation "Recover from invalid structured output" :max-iterations 2)))
          (fiveam:is (eq :failed
                         (wait-for-agentic-loop-status loop '(:failed :completed :limit-reached))))
          (fiveam:is (search "agentic loop control response" (agentic-loop-last-error loop)))
          (monitor-agentic-loops-once)
          (monitor-agentic-loops-once)
          (fiveam:is (eq :completed
                         (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
          (fiveam:is (= 2 calls))
          (fiveam:is (= 1 (agentic-loop-supervisor-restart-count loop)))
          (fiveam:is (string= "recovered after invalid structured output"
                              (agentic-loop-result-summary loop))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-monitor-leaves-exhausted-failed-loop-terminal
  (let* ((*agentic-loop-supervisor-max-restarts* 1)
        (*agentic-loop-supervisor-restart-backoff-seconds* 0.0d0)
        (calls 0)
        (*agentic-loop-chat-function*
         (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt conversation callback file files temperature top-p))
           (incf calls)
           "Here is your code!"))
        (conversation (new-chat :backend :openai)))
    (unwind-protect
       (let ((loop (start-agentic-loop conversation "Exhaust retries once" :max-iterations 2)))
         (fiveam:is (eq :failed
                        (wait-for-agentic-loop-status loop '(:failed :completed :limit-reached))))
         (loop repeat 50
               until (and (= calls 2)
                          (eq :failed (agentic-loop-status loop)))
               do (monitor-agentic-loops-once)
                  (sleep 0.01))
         (fiveam:is (eq :failed (agentic-loop-status loop)))
         (fiveam:is (= 2 calls))
         (fiveam:is (= 1 (agentic-loop-supervisor-restart-count loop)))
         (let ((finished-at (agentic-loop-finished-at loop))
               (last-error (agentic-loop-last-error loop)))
           (sleep 0.01)
           (monitor-agentic-loops-once)
           (fiveam:is (eq :failed (agentic-loop-status loop)))
           (fiveam:is (= 2 calls))
           (fiveam:is (= 1 (agentic-loop-supervisor-restart-count loop)))
           (fiveam:is (eql finished-at (agentic-loop-finished-at loop)))
           (fiveam:is (string= last-error (agentic-loop-last-error loop)))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-monitor-restarts-timed-out-agentic-loop-step
  (let* ((*agentic-loop-supervisor-timeout-seconds* 0.05d0)
        (*agentic-loop-supervisor-max-restarts* 1)
        (*agentic-loop-supervisor-restart-backoff-seconds* 0.0d0)
        (calls 0)
        (*agentic-loop-chat-function*
         (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt conversation callback file files temperature top-p))
           (incf calls)
           (if (= calls 1)
               (loop do (sleep 0.01))
               (test-agentic-loop-response "final" "completed after timeout restart"))))
        (conversation (new-chat :backend :openai)))
    (unwind-protect
        (let ((loop (start-agentic-loop conversation "Recover from timeout" :max-iterations 2)))
          (fiveam:is (eq :running
                         (wait-for-agentic-loop-status loop '(:running :completed :failed :limit-reached))))
          (sleep 0.08)
          (loop repeat 50
                until (or (= calls 2)
                          (eq :completed (agentic-loop-status loop)))
                do (monitor-agentic-loops-once)
                   (sleep 0.01))
          (fiveam:is (eq :completed
                         (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
          (fiveam:is (= 2 calls))
          (fiveam:is (= 1 (agentic-loop-supervisor-restart-count loop)))
          (fiveam:is (string= "completed after timeout restart" (agentic-loop-result-summary loop))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))


(fiveam:test test-agentic-loop-registry-context-isolation
  (let* ((context-a (make-runtime-context))
        (context-b (make-runtime-context))
         (conv-a (new-chat :backend :openai :runtime-context context-a))
         (conv-b (new-chat :backend :openai :runtime-context context-b))
         (*agentic-loop-chat-function*
          (lambda (prompt &key conversation callback file files temperature top-p)
            (declare (ignore prompt conversation callback file files temperature top-p))
            (test-agentic-loop-response "final" "ok"))))
    (unwind-protect
         (let ((loop-a (start-agentic-loop conv-a "Goal A" :max-iterations 2))
               (loop-b (start-agentic-loop conv-b "Goal B" :max-iterations 2)))
           (fiveam:is (typep (find-agentic-loop (agentic-loop-id loop-a) context-a) 'agentic-loop))
           (fiveam:is (null (find-agentic-loop (agentic-loop-id loop-a) context-b)))
           (fiveam:is (typep (find-agentic-loop (agentic-loop-id loop-b) context-b) 'agentic-loop))
           (fiveam:is (null (find-agentic-loop (agentic-loop-id loop-b) context-a)))
           (fiveam:is (= 1 (length (list-agentic-loops context-a))))
           (fiveam:is (= 1 (length (list-agentic-loops context-b)))))
      (abort-agentic-loops :force t :context context-a)
      (abort-agentic-loops :force t :context context-b)
      (clear-agentic-loops context-a)
      (clear-agentic-loops context-b))))

(fiveam:test test-active-thread-reaper-garbage-collection
  (let* ((context (make-runtime-context))
         (conv (new-chat :backend :openai :runtime-context context))
         (loop-id 9999)
         (loop (make-instance 'agentic-loop
                              :id loop-id
                              :goal "Dummy goal"
                              :conversation conv
                              :runtime-context context)))
    ;; Set loop to completed and finished older than 5 minutes (301 seconds ago)
    (setf (agentic-loop-status loop) :completed)
    (setf (agentic-loop-finished-at loop) (- (get-high-precision-timestamp) 301))
    
    ;; Register it
    (register-agentic-loop loop)
    (fiveam:is (typep (find-agentic-loop loop-id) 'agentic-loop))
    
    ;; Spawn a mock worker thread that hangs (representing an orphaned/hung worker)
    (let ((hung-thread (sb-thread:make-thread (lambda () (sleep 10))
                                             :name (format nil "Agentic-Loop-Worker-~A" loop-id))))
      (unwind-protect
           (progn
             (fiveam:is-true (sb-thread:thread-alive-p hung-thread))
             ;; Run reaper sweep
             (reap-orphaned-threads-and-sockets)
             ;; Verify loop is pruned from the registry
             (fiveam:is (null (find-agentic-loop loop-id)))
             ;; Verify hung orphaned thread was detected and terminated by the reaper
             (sleep 0.1)
             (fiveam:is-false (sb-thread:thread-alive-p hung-thread)))
        (when (sb-thread:thread-alive-p hung-thread)
          (sb-thread:terminate-thread hung-thread))))))
