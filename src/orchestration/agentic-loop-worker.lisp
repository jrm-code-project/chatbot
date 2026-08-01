;;; -*- Lisp -*-
;;; agentic-loop-worker.lisp - autonomous background loop workers and life-cycle operations

(in-package "CHATBOT")

(defun run-agentic-loop-step (loop)
  "Runs one autonomous iteration for LOOP."
  (let* ((conversation (agentic-loop-conversation loop))
         (prompt (or (agentic-loop-pending-step-prompt loop)
                     (build-agentic-loop-step-prompt loop)))
         (snapshot (snapshot-conversation-state conversation))
         (iteration (1+ (agentic-loop-current-iteration loop))))
    (setf (agentic-loop-pending-step-prompt loop) prompt)
    (setf (agentic-loop-active-step-started-at loop) (get-high-precision-timestamp))
    (setf (agentic-loop-active-step-snapshot loop) snapshot)
    (handler-case
        (progn
          (ensure-agentic-loop-not-interrupted loop)
          (let ((response (funcall (or (agentic-loop-chat-function-override loop)
                                      #'chat)
                                  prompt
                                  :conversation conversation)))
            (ensure-agentic-loop-not-interrupted loop)
            (apply-agentic-loop-step-state
             loop
             (make-agentic-loop-response-state iteration prompt response))))
      (agentic-loop-interrupted (condition)
        (restore-conversation-state conversation snapshot)
        (apply-agentic-loop-step-state
         loop
         (make-agentic-loop-interruption-state loop iteration prompt condition))))))

(defun agentic-loop-watchdog-restart-allowed-p (loop)
  "Returns true when LOOP still has restart budget remaining."
  (< (agentic-loop-supervisor-restart-count loop)
     (agentic-loop-supervisor-max-restarts loop)))

(defun agentic-loop-watchdog-backoff-elapsed-p (loop now)
  "Returns true when LOOP's restart backoff window has elapsed at NOW."
  (let ((not-before (agentic-loop-supervisor-restart-not-before loop)))
    (or (null not-before)
       (>= now not-before))))

(defun agentic-loop-watchdog-restart-scheduled-p (loop)
  "Returns true when LOOP already has a watchdog restart scheduled."
  (or (eq (agentic-loop-status loop) :pending)
      (agentic-loop-supervisor-restart-not-before loop)))

(defun agentic-loop-watchdog-timeout-expired-p (loop now)
  "Returns true when LOOP's current step has exceeded its watchdog timeout."
  (let ((timeout-seconds (agentic-loop-supervisor-timeout-seconds loop))
       (started-at (agentic-loop-active-step-started-at loop)))
    (and (eq (agentic-loop-status loop) :running)
        started-at
        timeout-seconds
        (> (- now started-at) timeout-seconds))))

(defun terminate-agentic-loop-worker-thread (loop)
  "Forcefully terminates LOOP's worker thread when it is still alive."
  (let ((thread (agentic-loop-thread loop)))
    (when (and thread (sb-thread:thread-alive-p thread))
      (handler-case
          (sb-thread:terminate-thread thread)
        (sb-thread:interrupt-thread-error ()
          nil))))
  (setf (agentic-loop-thread loop) nil)
  loop)

(defun restore-agentic-loop-active-snapshot (loop)
  "Restores LOOP's conversation to the most recent safe snapshot when available."
  (let ((snapshot (agentic-loop-active-step-snapshot loop)))
    (when snapshot
      (restore-conversation-state (agentic-loop-conversation loop) snapshot)))
  loop)

(defun mark-agentic-loop-supervisor-failed (loop reason)
  "Marks LOOP as permanently failed under watchdog supervision with REASON."
  (terminate-agentic-loop-worker-thread loop)
  (restore-agentic-loop-active-snapshot loop)
  (clear-agentic-loop-active-step-state loop)
  (sb-thread:with-mutex ((agentic-loop-lock loop))
    (setf (agentic-loop-status loop) :failed)
    (setf (agentic-loop-last-error loop) reason)
    (setf (agentic-loop-result-summary loop) reason)
    (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))
    (setf (agentic-loop-pending-approval loop) nil)
    (setf (agentic-loop-pending-approval-decision loop) nil))
  (agentic-loop-log :error loop "watchdog exhausted restart budget"
                   :context `(("reason" . ,reason)
                              ("restart-count" . ,(agentic-loop-supervisor-restart-count loop))))
  loop)

(defun schedule-agentic-loop-watchdog-restart (loop reason &key terminate-thread-p)
  "Schedules LOOP for watchdog-managed restart.
Returns :SCHEDULED when a restart was newly queued, :NOOP when one was already
queued, and :EXHAUSTED when LOOP has no restart budget left."
  (let ((result nil)
        (prompt nil)
        (restart-count nil)
        (max-restarts nil))
    (sb-thread:with-mutex ((agentic-loop-lock loop))
      (cond
        ((agentic-loop-watchdog-restart-scheduled-p loop)
         (setf result :noop))
        ((not (agentic-loop-watchdog-restart-allowed-p loop))
         (setf result :exhausted))
        (t
         (setf prompt (agentic-loop-pending-step-prompt loop))
         (incf (agentic-loop-supervisor-restart-count loop))
         (setf restart-count (agentic-loop-supervisor-restart-count loop))
         (setf max-restarts (agentic-loop-supervisor-max-restarts loop))
         (setf (agentic-loop-status loop) :pending)
         (setf (agentic-loop-last-error loop) reason)
         (setf (agentic-loop-result-summary loop) reason)
         (setf (agentic-loop-finished-at loop) nil)
         (setf (agentic-loop-pending-approval loop) nil)
         (setf (agentic-loop-pending-approval-decision loop) nil)
         (setf (agentic-loop-supervisor-restart-not-before loop)
               (+ (get-high-precision-timestamp)
                  (agentic-loop-supervisor-restart-backoff-seconds loop)))
         (setf result :scheduled))))
    (when (eq result :scheduled)
      (when terminate-thread-p
        (terminate-agentic-loop-worker-thread loop))
      (restore-agentic-loop-active-snapshot loop)
      (clear-agentic-loop-active-step-state loop)
      (append-agentic-loop-step-record
       loop
       (make-agentic-loop-step-record (1+ (agentic-loop-current-iteration loop))
                                      :interrupted
                                      :prompt prompt
                                      :note reason))
      (agentic-loop-log :warn loop "watchdog scheduled restart"
                        :context `(("reason" . ,reason)
                                   ("restart-count" . ,restart-count)
                                   ("max-restarts" . ,max-restarts))))
    result))

(defun ensure-agentic-loop-watchdog-restart (loop reason &key terminate-thread-p)
  "Restarts LOOP under watchdog policy when allowed, otherwise leaves it failed."
  (case (schedule-agentic-loop-watchdog-restart loop reason :terminate-thread-p terminate-thread-p)
    (:scheduled loop)
    (:noop loop)
    (t
     (mark-agentic-loop-supervisor-failed loop reason))))

(defun run-agentic-loop-worker (loop)
  "Runs LOOP to completion, pause, interruption, or failure."
  (unwind-protect
       (call-with-runtime-context
        (agentic-loop-runtime-context loop)
        (lambda ()
          (handler-case
              (loop
                while (eq (agentic-loop-status loop) :running)
                do (when (>= (agentic-loop-current-iteration loop)
                             (agentic-loop-max-iterations loop))
                     (setf (agentic-loop-status loop) :limit-reached)
                     (setf (agentic-loop-result-summary loop)
                           "Maximum iterations reached.")
                     (return))
                   (case (run-agentic-loop-step loop)
                     (:completed (return))
                     (:paused (return))
                     (:interrupted (return))
                     (:continue nil)
                     (t (return))))
            (agentic-loop-interrupted (condition)
              (unless (eq (agentic-loop-status loop) :interrupted)
                (setf (agentic-loop-status loop) :interrupted))
              (setf (agentic-loop-last-error loop) (agentic-loop-interrupted-reason condition))
              (setf (agentic-loop-result-summary loop) (agentic-loop-interrupted-reason condition))
              (agentic-loop-log :warn loop "interrupted"
                               :context `(("reason" . ,(agentic-loop-interrupted-reason condition)))))
            (error (condition)
              (setf (agentic-loop-status loop) :failed)
              (setf (agentic-loop-last-error loop) (princ-to-string condition))
              (append-agentic-loop-step-record
               loop
               (make-agentic-loop-step-record (1+ (agentic-loop-current-iteration loop))
                                              :failed
                                              :note (princ-to-string condition)))
              (agentic-loop-log :error loop "failed"
                                :context `(("error" . ,(princ-to-string condition)))))))
        :default-conversation-compatibility-p nil
        :legacy-function-seam-compatibility-p nil)
    (progn
      (when (eq (agentic-loop-status loop) :running)
        (setf (agentic-loop-status loop) :completed))
      (clear-agentic-loop-active-step-state loop)
      (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))
      (setf (agentic-loop-thread loop) nil))))

(defun spawn-agentic-loop-thread (loop)
  "Spawns LOOP's background worker thread."
  (setf (agentic-loop-status loop) :running)
  (unless (agentic-loop-started-at loop)
    (setf (agentic-loop-started-at loop) (get-high-precision-timestamp)))
  (let ((thread (sb-thread:make-thread
                 (lambda ()
                   (run-agentic-loop-worker loop))
                 :name (format nil "Agentic-Loop-Worker-~A" (agentic-loop-id loop)))))
    (setf (agentic-loop-thread loop) thread)
    (register-supervised-thread (current-resource-supervisor) thread)
    (agentic-loop-log :info loop "started")
    loop))

(defun start-agentic-loop (conversation goal &key (max-iterations 10) backend model isolate-p)
  "Clones CONVERSATION and starts an autonomous loop for GOAL."
  (unless (typep conversation 'conversation)
    (error "Agentic loops require a CHATBOT conversation."))
  (let* ((source-bot (conversation-chatbot conversation))
         (template-context (or (chatbot-runtime-context source-bot)
                               (resolve-runtime-context nil)
                               *default-runtime-context*))
         (loop-conversation (clone-conversation-for-agentic-loop conversation :isolate-p isolate-p))
         (loop (make-instance 'agentic-loop
                              :id (next-agentic-loop-id)
                              :goal goal
                              :max-iterations max-iterations
                              :conversation loop-conversation
                              :runtime-context template-context
                              :chat-function (resolve-agentic-loop-chat-function))))
    (setf (agentic-loop-execution-profile loop)
          (apply-agentic-loop-execution-profile loop-conversation
                                                :backend backend
                                                :model model))
    (let ((loop-context (make-agentic-loop-runtime-context loop template-context loop-conversation)))
      (setf (agentic-loop-runtime-context loop) loop-context)
      (setf (chatbot-runtime-context (conversation-chatbot loop-conversation)) loop-context))
    (register-agentic-loop loop)
    (spawn-agentic-loop-thread loop)
    (start-agentic-loop-monitor)
    loop))

(defun interrupt-agentic-loop-instance (loop &key force)
  "Interrupts LOOP in place."
  (declare (ignore force))
  (sb-thread:with-mutex ((agentic-loop-lock loop))
    (setf (agentic-loop-last-error loop) "Interrupted.")
    (setf (agentic-loop-result-summary loop) "Interrupted.")
    (unless (member (agentic-loop-status loop) '(:completed :failed :limit-reached))
     (setf (agentic-loop-status loop) :interrupted))
    (setf (agentic-loop-pending-approval-decision loop) :deny)
    (sb-thread:condition-broadcast (agentic-loop-approval-waitqueue loop)))
  (unless (agentic-loop-thread-alive-p loop)
    (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))
    (setf (agentic-loop-thread loop) nil))
  (agentic-loop-log :warn loop "interrupted")
  loop)

(defun abort-agentic-loop (loop-id &key force context)
  "Interrupts the autonomous loop identified by LOOP-ID."
  (interrupt-agentic-loop-instance
   (or (find-agentic-loop loop-id context)
      (error "Unknown agentic loop id: ~A" loop-id))
   :force force))

(defun abort-agentic-loops (&key force context)
  "Interrupts all registered autonomous loops."
  (dolist (loop (list-agentic-loops context) t)
    (abort-agentic-loop (agentic-loop-id loop) :force force :context context)))

(defun resume-agentic-loop-instance (loop &key approve)
  "Resumes paused LOOP after an explicit approval decision."
  (let ((loop-id (agentic-loop-id loop)))
    (unless (eq (agentic-loop-status loop) :awaiting-approval)
      (error "Agentic loop ~A is not awaiting approval." loop-id))
    (unless (agentic-loop-pending-approval loop)
      (error "Agentic loop ~A has no pending approval." loop-id))
    (unless (agentic-loop-thread-alive-p loop)
      (error "Agentic loop ~A is paused but has no live worker thread." loop-id))
    (if approve
        (progn
          (setf (agentic-loop-finished-at loop) nil)
          (sb-thread:with-mutex ((agentic-loop-lock loop))
            (setf (agentic-loop-pending-approval-decision loop) t)
            (setf (agentic-loop-status loop) :running)
            (sb-thread:condition-broadcast (agentic-loop-approval-waitqueue loop))))
        (progn
          (setf (agentic-loop-last-error loop) "Approval denied by user.")
          (setf (agentic-loop-result-summary loop) "Approval denied by user.")
          (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))
          (sb-thread:with-mutex ((agentic-loop-lock loop))
            (setf (agentic-loop-pending-approval-decision loop) :deny)
            (setf (agentic-loop-status loop) :interrupted)
            (sb-thread:condition-broadcast (agentic-loop-approval-waitqueue loop)))))
    loop))

(defun resume-agentic-loop (loop-id &key approve context)
  "Resumes a paused autonomous loop after an explicit approval decision."
  (resume-agentic-loop-instance
   (or (find-agentic-loop loop-id context)
       (error "Unknown agentic loop id: ~A" loop-id))
   :approve approve))
