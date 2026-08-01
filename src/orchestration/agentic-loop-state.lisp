;;; -*- Lisp -*-
;;; agentic-loop-state.lisp - autonomous background loop definitions and state management

(in-package "CHATBOT")

(defun get-high-precision-timestamp ()
  "Returns a double-float timestamp in seconds since Unix epoch (using sb-ext:get-time-of-day) or process internal time if on non-SBCL."
  #+sbcl
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ sec (float (/ usec 1000000) 1.0d0)))
  #-sbcl
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1.0d0))

(defvar *agentic-loop-registry* (make-hash-table))
(defvar *agentic-loop-registry-lock* (sb-thread:make-mutex :name "agentic-loop-registry-lock"))
(defvar *agentic-loop-id-counter* 0)
(defvar *agentic-loop-id-lock* (sb-thread:make-mutex :name "agentic-loop-id-lock"))
(defparameter *agentic-loop-chat-function* nil
  "Optional test seam overriding the chat function used by agentic loops.")

(defparameter *agentic-loop-supervisor-timeout-seconds* 180.0d0
  "Maximum seconds an in-flight loop step may run before the watchdog restarts it.")

(defparameter *agentic-loop-supervisor-max-restarts* 2
  "Maximum watchdog-managed restarts for one agentic loop before it is left failed.")

(defparameter *agentic-loop-supervisor-restart-backoff-seconds* 1.0d0
  "Seconds the watchdog waits before respawning a restarted loop.")

(define-condition agentic-loop-approval-required (error)
  ((loop-id :initarg :loop-id :reader agentic-loop-approval-required-loop-id)
   (kind :initarg :kind :reader agentic-loop-approval-required-kind)
   (tool-name :initarg :tool-name :reader agentic-loop-approval-required-tool-name)
   (resource :initarg :resource :reader agentic-loop-approval-required-resource))
  (:report (lambda (condition stream)
             (format stream
                     "Agentic loop ~A requires ~A approval for ~A: ~A"
                     (agentic-loop-approval-required-loop-id condition)
                     (agentic-loop-approval-required-kind condition)
                     (agentic-loop-approval-required-tool-name condition)
                     (agentic-loop-approval-required-resource condition)))))

(define-condition agentic-loop-interrupted (error)
  ((loop-id :initarg :loop-id :reader agentic-loop-interrupted-loop-id)
   (reason :initarg :reason :reader agentic-loop-interrupted-reason))
  (:report (lambda (condition stream)
             (format stream
                     "Agentic loop ~A interrupted: ~A"
                     (agentic-loop-interrupted-loop-id condition)
                     (agentic-loop-interrupted-reason condition)))))

(defclass agentic-loop ()
  ((id
    :initarg :id
    :reader agentic-loop-id)
   (goal
    :initarg :goal
    :reader agentic-loop-goal)
   (max-iterations
    :initarg :max-iterations
    :reader agentic-loop-max-iterations
    :initform 10)
   (current-iteration
    :initarg :current-iteration
    :accessor agentic-loop-current-iteration
    :initform 0)
   (status
    :initarg :status
    :accessor agentic-loop-status
    :initform :pending)
   (thread
    :initarg :thread
    :accessor agentic-loop-thread
    :initform nil)
   (conversation
    :initarg :conversation
    :reader agentic-loop-conversation)
   (runtime-context
    :initarg :runtime-context
    :accessor agentic-loop-runtime-context)
   (chat-function
    :initarg :chat-function
    :reader agentic-loop-chat-function-override
    :initform nil)
   (execution-profile
    :initarg :execution-profile
    :accessor agentic-loop-execution-profile
    :initform nil)
   (step-history
    :initarg :step-history
    :accessor agentic-loop-step-history
    :initform nil)
   (result-summary
    :initarg :result-summary
    :accessor agentic-loop-result-summary
    :initform nil)
   (last-error
    :initarg :last-error
    :accessor agentic-loop-last-error
    :initform nil)
   (pending-approval
    :initarg :pending-approval
    :accessor agentic-loop-pending-approval
    :initform nil)
   (pending-approval-decision
    :initarg :pending-approval-decision
    :accessor agentic-loop-pending-approval-decision
    :initform nil)
   (approval-waitqueue
    :initarg :approval-waitqueue
    :reader agentic-loop-approval-waitqueue
    :initform (sb-thread:make-waitqueue :name "agentic-loop-approval-waitqueue"))
   (pending-step-prompt
    :initarg :pending-step-prompt
    :accessor agentic-loop-pending-step-prompt
    :initform nil)
   (active-step-started-at
    :initarg :active-step-started-at
    :accessor agentic-loop-active-step-started-at
    :initform nil)
   (active-step-snapshot
    :initarg :active-step-snapshot
    :accessor agentic-loop-active-step-snapshot
    :initform nil)
   (supervisor-restart-count
    :initarg :supervisor-restart-count
    :accessor agentic-loop-supervisor-restart-count
    :initform 0)
   (supervisor-max-restarts
    :initarg :supervisor-max-restarts
    :accessor agentic-loop-supervisor-max-restarts
    :initform *agentic-loop-supervisor-max-restarts*)
   (supervisor-restart-backoff-seconds
    :initarg :supervisor-restart-backoff-seconds
    :accessor agentic-loop-supervisor-restart-backoff-seconds
    :initform *agentic-loop-supervisor-restart-backoff-seconds*)
   (supervisor-restart-not-before
    :initarg :supervisor-restart-not-before
    :accessor agentic-loop-supervisor-restart-not-before
    :initform nil)
   (supervisor-timeout-seconds
    :initarg :supervisor-timeout-seconds
    :accessor agentic-loop-supervisor-timeout-seconds
    :initform *agentic-loop-supervisor-timeout-seconds*)
   (created-at
    :initarg :created-at
    :reader agentic-loop-created-at
    :initform (get-high-precision-timestamp))
   (started-at
    :initarg :started-at
    :accessor agentic-loop-started-at
    :initform nil)
   (finished-at
    :initarg :finished-at
    :accessor agentic-loop-finished-at
    :initform nil)
   (lock
    :initarg :lock
    :reader agentic-loop-lock
    :initform (sb-thread:make-mutex :name "agentic-loop-lock"))))

(defun next-agentic-loop-id ()
  "Returns the next unique autonomous loop identifier."
  (sb-thread:with-mutex (*agentic-loop-id-lock*)
    (incf *agentic-loop-id-counter*)))

(defun snapshot-conversation-state (conversation)
  "Returns a restorable snapshot of CONVERSATION state."
  (list :messages (and (conversation-messages conversation)
                       (copy-tree (conversation-messages conversation)))
        :interaction-id (conversation-interaction-id conversation)))

(defun restore-conversation-state (conversation snapshot)
  "Restores CONVERSATION from SNAPSHOT."
  (setf (conversation-messages conversation) (getf snapshot :messages))
  (setf (conversation-interaction-id conversation) (getf snapshot :interaction-id))
  conversation)

(defun register-agentic-loop (loop)
  "Registers LOOP in the autonomous loop registry of its runtime context."
  (let* ((context (agentic-loop-runtime-context loop))
         (registry (runtime-context-agentic-loop-registry context))
         (lock (runtime-context-agentic-loop-registry-lock context)))
    (sb-thread:with-mutex (lock)
      (setf (gethash (agentic-loop-id loop) registry) loop))
    (sb-thread:with-mutex (*agentic-loop-registry-lock*)
      (setf (gethash (agentic-loop-id loop) *agentic-loop-registry*) loop))
    (register-runtime-worker-entry
     (make-runtime-worker-entry
      :worker-id (format nil "loop:~A" (agentic-loop-id loop))
      :kind :loop
      :loop loop)
     context)
    loop))

(defun runtime-context-agentic-worker-loops (context)
  "Returns autonomous loops registered in CONTEXT's unified worker registry."
  (loop for entry in (list-runtime-worker-entries context)
       when (eq (runtime-worker-entry-kind entry) :loop)
         collect (runtime-worker-entry-loop entry)))

(defun find-agentic-loop (loop-id &optional context)
  "Returns the autonomous loop identified by LOOP-ID, or NIL."
  (if context
     (let* ((resolved-context (resolve-runtime-context context))
            (entry (find-runtime-worker-entry
                    (format nil "loop:~A" loop-id)
                    resolved-context)))
       (or (and entry
                (eq (runtime-worker-entry-kind entry) :loop)
                (runtime-worker-entry-loop entry))
           (let ((registry (runtime-context-agentic-loop-registry resolved-context))
                 (lock (runtime-context-agentic-loop-registry-lock resolved-context)))
             (sb-thread:with-mutex (lock)
               (gethash loop-id registry)))))
     (sb-thread:with-mutex (*agentic-loop-registry-lock*)
       (gethash loop-id *agentic-loop-registry*))))

(defun list-agentic-loops (&optional context)
  "Returns all registered autonomous loops ordered by id."
  (let ((loops
         (if context
             (runtime-context-agentic-worker-loops context)
             (sb-thread:with-mutex (*agentic-loop-registry-lock*)
               (loop for loop being the hash-values of *agentic-loop-registry*
                     collect loop)))))
    (sort loops #'< :key #'agentic-loop-id)))

(defun clear-agentic-loops (&optional context)
  "Clears the autonomous loop registry."
  (if context
     (let* ((resolved-context (resolve-runtime-context context))
            (loops (runtime-context-agentic-worker-loops resolved-context))
            (loop-ids (mapcar #'agentic-loop-id loops))
            (registry (runtime-context-agentic-loop-registry resolved-context))
            (lock (runtime-context-agentic-loop-registry-lock resolved-context)))
       (sb-thread:with-mutex (lock)
         (clrhash registry))
       (sb-thread:with-mutex (*agentic-loop-registry-lock*)
         (dolist (loop-id loop-ids)
            (remhash loop-id *agentic-loop-registry*)
            (remove-runtime-worker-entry (format nil "loop:~A" loop-id)
                                         resolved-context)))
        t)
      (progn
        (let ((loops nil))
         (sb-thread:with-mutex (*agentic-loop-registry-lock*)
           (setf loops (loop for loop being the hash-values of *agentic-loop-registry*
                             collect loop))
           (clrhash *agentic-loop-registry*))
         (dolist (loop loops)
           (remove-runtime-worker-entry
            (format nil "loop:~A" (agentic-loop-id loop))
            (agentic-loop-runtime-context loop))))
        t)))

(defun agentic-loop-thread-alive-p (loop)
  "Returns true when LOOP still has a live worker thread."
  (let ((thread (agentic-loop-thread loop)))
    (and thread
         (sb-thread:thread-alive-p thread))))

(defun resolve-agentic-loop-chat-function ()
  "Returns the chat function used for autonomous iterations."
  (or *agentic-loop-chat-function* #'chat))

(defun agentic-loop-history-summary (loop)
  "Returns a compact text summary of LOOP's previous steps."
  (with-output-to-string (stream)
    (dolist (entry (reverse (agentic-loop-step-history loop)))
      (format stream "~%Step ~D (~A): ~A"
              (getf entry :iteration)
              (string-downcase (string (getf entry :status)))
              (or (getf entry :response)
                  (getf entry :note)
                  "")))))

(defun build-agentic-loop-step-prompt (loop)
  "Builds the next autonomous prompt for LOOP."
  (format nil
          "Autonomous goal: ~A~%Iteration: ~D of ~D.~%~A~%Use available tools when helpful. You MUST reply with ONLY one strict JSON object in exactly this schema: {\"status\":\"continue\",\"summary\":\"concise progress update and next step\"} or {\"status\":\"final\",\"summary\":\"final result\"}. Do not add commentary before or after the JSON. The status field must be either continue or final, and the summary field must be a non-empty string."
          (agentic-loop-goal loop)
          (1+ (agentic-loop-current-iteration loop))
          (agentic-loop-max-iterations loop)
          (let ((history (agentic-loop-history-summary loop)))
            (if (string= history "")
                "Previous steps: none."
                (format nil "Previous steps:~A" history)))))

(defun parse-agentic-loop-control-response (response)
  "Parses one strict structured loop control RESPONSE."
  (let* ((payload (parse-structured-json-response-or-error
                   response
                   :context "agentic loop control response"))
         (context "agentic loop control response"))
    (unless (json-object-alist-p payload)
      (error "Invalid ~A payload: expected a JSON object." context))
    (ensure-json-object-only-keys payload '("status" "summary") '() context)
    (let ((status-raw (mcp-val "status" payload))
          (summary (require-non-empty-json-string (mcp-val "summary" payload) "summary" context)))
      (unless (stringp status-raw)
        (error "Invalid ~A payload: status must be a string." context))
      (let ((status (string-downcase status-raw)))
        (unless (member status '("continue" "final") :test #'string=)
          (error "Invalid ~A payload: status must be either continue or final." context))
        (list :status status
              :summary summary)))))

(defun agentic-loop-pending-approval-plist (kind tool-name resource)
  "Returns the stored pending approval representation."
  (list :kind kind
        :tool-name tool-name
        :resource (typecase resource
                    (pathname (namestring resource))
                    (t (princ-to-string resource)))))

(defun agentic-loop-consume-approval-decision (loop kind tool-name resource)
  "Consumes any matching stored approval decision for LOOP."
  (sb-thread:with-mutex ((agentic-loop-lock loop))
    (let ((pending (agentic-loop-pending-approval loop))
          (decision (agentic-loop-pending-approval-decision loop))
          (resource-name (typecase resource
                           (pathname (namestring resource))
                           (t (princ-to-string resource)))))
      (when (and pending
                 (not (null decision))
                 (eq (getf pending :kind) kind)
                 (string= (getf pending :tool-name) tool-name)
                 (string= (getf pending :resource) resource-name))
        (setf (agentic-loop-pending-approval-decision loop) nil)
        decision))))

(defun signal-agentic-loop-approval (loop kind tool-name resource)
  "Signals that LOOP requires KIND approval for RESOURCE."
  (error 'agentic-loop-approval-required
         :loop-id (agentic-loop-id loop)
         :kind kind
         :tool-name tool-name
         :resource resource))

(defun interrupt-agentic-loop-error (loop reason)
  "Signals a loop interruption error for LOOP with REASON."
  (error 'agentic-loop-interrupted
         :loop-id (agentic-loop-id loop)
         :reason reason))

(defun agentic-loop-interruption-reason (loop)
  "Returns the current interruption reason recorded for LOOP."
  (or (agentic-loop-last-error loop)
      (agentic-loop-result-summary loop)
      "Interrupted."))

(defun ensure-agentic-loop-not-interrupted (loop)
  "Signals interruption when LOOP has already been asked to stop."
  (when (eq (agentic-loop-status loop) :interrupted)
    (interrupt-agentic-loop-error loop
                                 (agentic-loop-interruption-reason loop))))

(defun agentic-loop-approval-wrapper (loop kind resource)
  "Returns an approval function wrapper for LOOP."
  (lambda (bot raw-resource tool-name)
    (declare (ignore bot))
    (let ((resolved-resource (funcall resource raw-resource)))
      (ensure-agentic-loop-not-interrupted loop)
      (sb-thread:with-mutex ((agentic-loop-lock loop))
        (setf (agentic-loop-status loop) :awaiting-approval)
        (setf (agentic-loop-pending-approval loop)
              (agentic-loop-pending-approval-plist kind tool-name resolved-resource))
        (setf (agentic-loop-pending-approval-decision loop) nil)
        (loop
          for decision = (agentic-loop-pending-approval-decision loop)
          do (cond
               ((eq decision t)
                (setf (agentic-loop-pending-approval-decision loop) nil)
                (setf (agentic-loop-pending-approval loop) nil)
                (setf (agentic-loop-status loop) :running)
                (return t))
               ((eq decision :deny)
                (setf (agentic-loop-pending-approval-decision loop) nil)
                (setf (agentic-loop-pending-approval loop) nil)
                (setf (agentic-loop-status loop) :interrupted)
                (interrupt-agentic-loop-error loop "Approval denied by user."))
               ((eq (agentic-loop-status loop) :interrupted)
                (setf (agentic-loop-pending-approval loop) nil)
                (interrupt-agentic-loop-error loop
                                              (or (agentic-loop-result-summary loop)
                                                  "Interrupted.")))
               (t
                (sb-thread:condition-wait (agentic-loop-approval-waitqueue loop)
                                          (agentic-loop-lock loop)))))))))

(defun make-agentic-loop-runtime-context (loop template-context conversation)
  "Returns a loop-specific runtime context derived from TEMPLATE-CONTEXT."
  (let ((loop-context
          (runtime-context-with-logging-settings template-context
                                                 :log-level :warn)))
    (clone-runtime-context
     loop-context
     :default-conversation conversation
     :filesystem-access-approval-function
     (agentic-loop-approval-wrapper loop
                                    :filesystem
                                    (lambda (directory)
                                      (uiop:ensure-directory-pathname (truename directory))))
     :eval-approval-function
     (agentic-loop-approval-wrapper loop
                                    :eval
                                    (lambda (expression)
                                      expression)))))

(defun make-agentic-loop-step-record (iteration status &key prompt response note)
  "Returns one structured autonomous step record."
  (list :iteration iteration
        :status status
        :prompt prompt
        :response response
        :note note
        :timestamp (get-high-precision-timestamp)))

(defun append-agentic-loop-step-record (loop record)
  "Appends RECORD to LOOP history."
  (setf (agentic-loop-step-history loop)
        (append (agentic-loop-step-history loop) (list record))))

(defun agentic-loop-terminal-status-p (status)
  "Returns true when STATUS is terminal for an autonomous loop."
  (member status '(:completed :failed :limit-reached :interrupted)))

(defun agentic-loop-live-status-p (status)
  "Returns true when STATUS expects a live worker thread."
  (member status '(:running :awaiting-approval)))

(defun make-agentic-loop-response-state (iteration prompt response)
  "Returns the next loop state implied by RESPONSE."
  (let* ((control (parse-agentic-loop-control-response response))
         (final-p (string= "final" (getf control :status))))
    (list :current-iteration iteration
          :pending-step-prompt nil
          :pending-approval nil
          :pending-approval-decision nil
          :record (make-agentic-loop-step-record iteration :completed
                                                 :prompt prompt
                                                 :response response)
          :status (if final-p :completed :running)
          :result-summary (and final-p
                               (getf control :summary))
          :outcome (if final-p :completed :continue))))

(defun make-agentic-loop-interruption-state (loop iteration prompt condition)
  "Returns the next loop state implied by an interruption CONDITION."
  (list :current-iteration (agentic-loop-current-iteration loop)
        :pending-step-prompt nil
        :pending-approval (agentic-loop-pending-approval loop)
        :pending-approval-decision nil
        :record (make-agentic-loop-step-record iteration :interrupted
                                               :prompt prompt
                                               :note (princ-to-string condition))
        :outcome :interrupted))

(defun clear-agentic-loop-active-step-state (loop)
  "Clears LOOP's in-flight step bookkeeping."
  (setf (agentic-loop-active-step-started-at loop) nil)
  (setf (agentic-loop-active-step-snapshot loop) nil)
  loop)

(defun apply-agentic-loop-step-state (loop state)
  "Applies one computed step STATE to LOOP and returns the step outcome keyword."
  (setf (agentic-loop-current-iteration loop) (getf state :current-iteration))
  (setf (agentic-loop-pending-step-prompt loop) (getf state :pending-step-prompt))
  (setf (agentic-loop-pending-approval loop) (getf state :pending-approval))
  (setf (agentic-loop-pending-approval-decision loop)
        (getf state :pending-approval-decision))
  (append-agentic-loop-step-record loop (getf state :record))
  (when (member :status state)
    (setf (agentic-loop-status loop) (getf state :status)))
  (when (member :result-summary state)
    (setf (agentic-loop-result-summary loop) (getf state :result-summary)))
  (clear-agentic-loop-active-step-state loop)
  (getf state :outcome))

(defun agentic-loop-log (level loop message &key context)
  "Emits one concise loop lifecycle log entry."
  (log-message level
               (format nil "Agentic loop ~A ~A" (agentic-loop-id loop) message)
               :context context))

(defun agentic-loop-public-alist (loop)
  "Returns LOOP state as a JSON-encodable alist."
  (let* ((conversation (agentic-loop-conversation loop))
        (bot (conversation-chatbot conversation))
        (backend-name (string-downcase
                       (string (or (getf (agentic-loop-execution-profile loop) :backend)
                                   (chatbot-backend bot)))))
        (model-name (or (getf (agentic-loop-execution-profile loop) :model)
                        (chatbot-model bot)
                        :null)))
    `(("id" . ,(agentic-loop-id loop))
      ("workerId" . ,(format nil "loop:~A" (agentic-loop-id loop)))
      ("kind" . ,(runtime-worker-kind-public-name :loop))
      ("name" . ,(format nil "Loop ~A" (agentic-loop-id loop)))
      ("goal" . ,(agentic-loop-goal loop))
      ("status" . ,(string-downcase (string (agentic-loop-status loop))))
      ("parentName" . ,(or (chatbot-parent-name bot) :null))
      ("depth" . ,(chatbot-depth bot))
      ("tokenBudget" . ,(or (chatbot-token-budget bot) :null))
      ("spentTokens" . ,(chatbot-spent-tokens bot))
      ("executionProfile" . (("backend" . ,backend-name)
                             ("model" . ,model-name)))
      ("maxIterations" . ,(agentic-loop-max-iterations loop))
      ("currentIteration" . ,(agentic-loop-current-iteration loop))
      ("resultSummary" . ,(or (agentic-loop-result-summary loop) :null))
      ("lastError" . ,(or (agentic-loop-last-error loop) :null))
      ("threadAlive" . ,(if (agentic-loop-thread-alive-p loop) t :false))
      ("createdAt" . ,(agentic-loop-created-at loop))
      ("startedAt" . ,(or (agentic-loop-started-at loop) :null))
      ("finishedAt" . ,(or (agentic-loop-finished-at loop) :null))
      ("pendingApproval" . ,(or (agentic-loop-pending-approval loop) :null))
      ("stepHistory" . ,(coerce (mapcar (lambda (entry)
                                          `(("iteration" . ,(getf entry :iteration))
                                            ("status" . ,(string-downcase (string (getf entry :status))))
                                            ("prompt" . ,(or (getf entry :prompt) :null))
                                            ("response" . ,(or (getf entry :response) :null))
                                            ("note" . ,(or (getf entry :note) :null))
                                            ("timestamp" . ,(getf entry :timestamp))))
                                        (agentic-loop-step-history loop))
                                    'vector)))))

(defun agentic-loop-public-json (loop)
  "Returns LOOP state as a JSON string."
  (cl-json:encode-json-to-string (agentic-loop-public-alist loop)))

(defun agentic-loop-list-json (&optional context)
  "Returns all loop states as a JSON string."
  (cl-json:encode-json-to-string
   `(("loops" . ,(coerce (mapcar #'agentic-loop-public-alist
                                (list-agentic-loops context))
                        'vector)))))
