;;; tests-agentic-loops.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

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
           "FINAL: loop finished"))
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
             "FINAL: eval complete")))
    (unwind-protect
         (let ((loop (start-agentic-loop conversation "Run eval" :max-iterations 2)))
           (fiveam:is (eq :awaiting-approval
                          (wait-for-agentic-loop-status loop '(:awaiting-approval :completed :failed))))
           (fiveam:is (equal :eval (getf (agentic-loop-pending-approval loop) :kind)))
           (fiveam:is (string= "eval" (getf (agentic-loop-pending-approval loop) :tool-name)))
           (resume-agentic-loop (agentic-loop-id loop) :approve t)
           (fiveam:is (eq :completed
                          (wait-for-agentic-loop-status loop '(:completed :failed :limit-reached))))
           (fiveam:is (= 2 calls))
           (fiveam:is (string= "eval complete" (agentic-loop-result-summary loop))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))

(fiveam:test test-start-agentic-loop-tool-uses-active-conversation
  (let ((*agentic-loop-chat-function*
         (lambda (prompt &key conversation callback file files temperature top-p)
           (declare (ignore prompt conversation callback file files temperature top-p))
           "FINAL: started by tool"))
        (conversation (new-chat :backend :openai)))
    (unwind-protect
         (let* ((*active-conversation* conversation)
                (json (default-execute-builtin-chatbot-tool
                       (conversation-chatbot conversation)
                       "startAgenticLoop"
                       '(("goal" . "Tool-launched goal")
                         ("maxIterations" . 2))))
                (payload (parse-json-or-error json :context "agentic loop tool result"))
                (loop-id (mcp-val :id payload))
                (loop (find-agentic-loop loop-id)))
           (fiveam:is (typep loop 'agentic-loop))
           (fiveam:is (string= "Tool-launched goal" (agentic-loop-goal loop))))
      (abort-agentic-loops :force t)
      (clear-agentic-loops))))
