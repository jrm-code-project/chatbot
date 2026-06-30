;;; -*- Lisp -*-
;;; agentic-loops.lisp - autonomous background loop orchestration

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
(defvar *agentic-loop-chat-function* nil
  "Optional test seam overriding the chat function used by agentic loops.")

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
    :accessor agentic-loop-id)
   (goal
    :initarg :goal
    :accessor agentic-loop-goal)
   (max-iterations
    :initarg :max-iterations
    :accessor agentic-loop-max-iterations
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
    :accessor agentic-loop-conversation)
   (runtime-context
    :initarg :runtime-context
    :accessor agentic-loop-runtime-context)
   (chat-function
    :initarg :chat-function
    :accessor agentic-loop-chat-function-override
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
    :accessor agentic-loop-approval-waitqueue
    :initform (sb-thread:make-waitqueue :name "agentic-loop-approval-waitqueue"))
   (pending-step-prompt
    :initarg :pending-step-prompt
    :accessor agentic-loop-pending-step-prompt
    :initform nil)
   (created-at
    :initarg :created-at
    :accessor agentic-loop-created-at
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
    :accessor agentic-loop-lock
    :initform (sb-thread:make-mutex :name "agentic-loop-lock"))))

(defun next-agentic-loop-id ()
  "Returns the next unique autonomous loop identifier."
  (sb-thread:with-mutex (*agentic-loop-id-lock*)
    (incf *agentic-loop-id-counter*)))

(defun clone-runtime-context (context &rest initarg-overrides)
  "Returns a shallow clone of CONTEXT with INITARG-OVERRIDES applied."
  (apply #'make-instance 'runtime-context
         (merge-initarg-overrides (copy-initargs-for-instance context)
                                  initarg-overrides)))

(defun clone-conversation-for-agentic-loop (conversation &key isolate-p)
  "Returns a loop-owned clone of CONVERSATION and its chatbot, optionally isolated."
  (let ((chatbot (conversation-chatbot conversation)))
    (if isolate-p
        (clone-conversation conversation
                            :chatbot (clone-chatbot chatbot
                                                    :system-instruction nil
                                                    :persona-memory nil
                                                    :persona-diary-entries nil)
                            :messages nil
                            :interaction-id nil)
        (clone-conversation conversation
                            :chatbot (clone-chatbot chatbot)
                            :messages (and (conversation-messages conversation)
                                           (copy-tree (conversation-messages conversation)))
                            :interaction-id (conversation-interaction-id conversation)))))

(defun apply-agentic-loop-execution-profile (conversation &key backend model)
  "Applies backend/model overrides to CONVERSATION and returns the effective profile."
  (let* ((bot (conversation-chatbot conversation))
         (runtime-context (chatbot-runtime-context bot))
         (current-backend (chatbot-backend bot))
         (default-backend-raw (current-agentic-loop-default-backend runtime-context))
         (default-model-raw (current-agentic-loop-default-model runtime-context))
         (default-backend (when default-backend-raw
                            (normalize-chatbot-backend default-backend-raw
                                                       "agentic loop default")))
         (default-model (when default-model-raw
                          (require-non-empty-string default-model-raw
                                                    "Default agentic loop model")))
         (effective-backend (cond
                              (backend
                               (normalize-chatbot-backend backend "agentic loop"))
                              (default-backend
                               default-backend)
                              (t
                               current-backend)))
         (effective-model (cond
                            (model
                             (require-non-empty-string model "Agentic loop model"))
                            (backend
                             (backend-default-model effective-backend))
                            (default-backend
                             (or default-model
                                 (backend-default-model effective-backend)))
                            (default-model
                             default-model)
                            (t
                             (or (chatbot-model bot)
                                 (backend-default-model effective-backend))))))
    (setf (chatbot-backend bot) effective-backend)
    (setf (chatbot-model bot) effective-model)
    (list :backend effective-backend
          :model effective-model)))

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
    loop))

(defun find-agentic-loop (loop-id &optional context)
  "Returns the autonomous loop identified by LOOP-ID, or NIL."
  (if context
      (let* ((resolved-context (resolve-runtime-context context))
             (registry (runtime-context-agentic-loop-registry resolved-context))
             (lock (runtime-context-agentic-loop-registry-lock resolved-context)))
        (sb-thread:with-mutex (lock)
          (gethash loop-id registry)))
      (sb-thread:with-mutex (*agentic-loop-registry-lock*)
        (gethash loop-id *agentic-loop-registry*))))

(defun list-agentic-loops (&optional context)
  "Returns all registered autonomous loops ordered by id."
  (let ((loops
          (if context
              (let* ((resolved-context (resolve-runtime-context context))
                     (registry (runtime-context-agentic-loop-registry resolved-context))
                     (lock (runtime-context-agentic-loop-registry-lock resolved-context)))
                (sb-thread:with-mutex (lock)
                  (loop for loop being the hash-values of registry
                        collect loop)))
              (sb-thread:with-mutex (*agentic-loop-registry-lock*)
                (loop for loop being the hash-values of *agentic-loop-registry*
                      collect loop)))))
    (sort loops #'< :key #'agentic-loop-id)))

(defun clear-agentic-loops (&optional context)
  "Clears the autonomous loop registry."
  (if context
      (let* ((resolved-context (resolve-runtime-context context))
             (registry (runtime-context-agentic-loop-registry resolved-context))
             (lock (runtime-context-agentic-loop-registry-lock resolved-context))
             (loop-ids nil))
        (sb-thread:with-mutex (lock)
          (setf loop-ids
                (loop for loop being the hash-values of registry
                      collect (agentic-loop-id loop)))
          (clrhash registry))
        (sb-thread:with-mutex (*agentic-loop-registry-lock*)
          (dolist (loop-id loop-ids)
            (remhash loop-id *agentic-loop-registry*)))
        t)
      (progn
        (sb-thread:with-mutex (*agentic-loop-registry-lock*)
          (clrhash *agentic-loop-registry*))
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
          "Autonomous goal: ~A~%Iteration: ~D of ~D.~%~A~%Use available tools when helpful. If the goal is complete or you cannot make further progress, reply with `FINAL:` followed by the result. Otherwise reply with `CONTINUE:` followed by a concise summary of what you learned and the next step to take."
          (agentic-loop-goal loop)
          (1+ (agentic-loop-current-iteration loop))
          (agentic-loop-max-iterations loop)
          (let ((history (agentic-loop-history-summary loop)))
            (if (string= history "")
                "Previous steps: none."
                (format nil "Previous steps:~A" history)))))

(defun trim-agentic-loop-response (response)
  "Returns RESPONSE trimmed for control prefix checks."
  (string-trim '(#\Space #\Tab #\Return #\Linefeed) (or response "")))

(defun agentic-loop-final-response-p (response)
  "Returns true when RESPONSE indicates the autonomous loop is done."
  (let ((trimmed (trim-agentic-loop-response response)))
    (or (alexandria:starts-with-subseq "FINAL:" trimmed)
        (not (alexandria:starts-with-subseq "CONTINUE:" trimmed)))))

(defun agentic-loop-final-response-text (response)
  "Returns the human-meaningful completion text from RESPONSE."
  (let ((trimmed (trim-agentic-loop-response response)))
    (if (alexandria:starts-with-subseq "FINAL:" trimmed)
        (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                     (subseq trimmed (length "FINAL:")))
        trimmed)))

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
  (clone-runtime-context
   template-context
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
                                    expression))))

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

(defun agentic-loop-log (level loop message &key context)
  "Emits one concise loop lifecycle log entry."
  (log-message level
               (format nil "Agentic loop ~A ~A" (agentic-loop-id loop) message)
               :context context))

(defun agentic-loop-public-alist (loop)
  "Returns LOOP state as a JSON-encodable alist."
  (let ((backend-name (string-downcase
                       (string (or (getf (agentic-loop-execution-profile loop) :backend)
                                   (chatbot-backend (conversation-chatbot (agentic-loop-conversation loop)))))))
        (model-name (or (getf (agentic-loop-execution-profile loop) :model)
                        (chatbot-model (conversation-chatbot (agentic-loop-conversation loop)))
                        :null)))
    `(("id" . ,(agentic-loop-id loop))
      ("goal" . ,(agentic-loop-goal loop))
      ("status" . ,(string-downcase (string (agentic-loop-status loop))))
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

(defun run-agentic-loop-step (loop)
  "Runs one autonomous iteration for LOOP."
  (let* ((conversation (agentic-loop-conversation loop))
         (prompt (or (agentic-loop-pending-step-prompt loop)
                     (build-agentic-loop-step-prompt loop)))
         (snapshot (snapshot-conversation-state conversation))
         (iteration (1+ (agentic-loop-current-iteration loop))))
    (setf (agentic-loop-pending-step-prompt loop) prompt)
    (handler-case
        (progn
          (ensure-agentic-loop-not-interrupted loop)
          (let ((response (funcall (or (agentic-loop-chat-function-override loop)
                                      #'chat)
                                  prompt
                                  :conversation conversation)))
            (ensure-agentic-loop-not-interrupted loop)
            (incf (agentic-loop-current-iteration loop))
            (setf (agentic-loop-pending-step-prompt loop) nil)
            (setf (agentic-loop-pending-approval loop) nil)
            (setf (agentic-loop-pending-approval-decision loop) nil)
            (append-agentic-loop-step-record
             loop
             (make-agentic-loop-step-record iteration :completed
                                           :prompt prompt
                                           :response response))
            (if (agentic-loop-final-response-p response)
                (progn
                  (setf (agentic-loop-status loop) :completed)
                  (setf (agentic-loop-result-summary loop)
                       (agentic-loop-final-response-text response))
                  :completed)
                :continue)))
      (agentic-loop-interrupted (condition)
        (restore-conversation-state conversation snapshot)
        (setf (agentic-loop-pending-step-prompt loop) nil)
        (setf (agentic-loop-pending-approval-decision loop) nil)
        (append-agentic-loop-step-record
         loop
         (make-agentic-loop-step-record iteration :interrupted
                                        :prompt prompt
                                        :note (princ-to-string condition)))
        :interrupted))))

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
                                :context `(("error" . ,(princ-to-string condition))))))))
    (progn
      (when (eq (agentic-loop-status loop) :running)
        (setf (agentic-loop-status loop) :completed))
      (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))
      (setf (agentic-loop-thread loop) nil))))

(defun spawn-agentic-loop-thread (loop)
  "Spawns LOOP's background worker thread."
  (setf (agentic-loop-status loop) :running)
  (unless (agentic-loop-started-at loop)
    (setf (agentic-loop-started-at loop) (get-high-precision-timestamp)))
  (setf (agentic-loop-thread loop)
        (sb-thread:make-thread
         (lambda ()
           (run-agentic-loop-worker loop))
         :name (format nil "Agentic-Loop-~A" (agentic-loop-id loop))))
  (agentic-loop-log :info loop "started")
  loop)

(defun start-agentic-loop (conversation goal &key (max-iterations 10) backend model isolate-p)
  "Clones CONVERSATION and starts an autonomous loop for GOAL."
  (unless (typep conversation 'conversation)
    (error "Agentic loops require a CHATBOT conversation."))
  (let* ((source-bot (conversation-chatbot conversation))
         (template-context (or (chatbot-runtime-context source-bot)
                               (resolve-runtime-context nil :sync-from-globals-p t)
                               *default-runtime-context*))
         (loop-conversation (clone-conversation-for-agentic-loop conversation :isolate-p isolate-p))
         (loop (make-instance 'agentic-loop
                              :id (next-agentic-loop-id)
                              :goal goal
                              :max-iterations max-iterations
                              :conversation loop-conversation
                              :runtime-context template-context
                              :chat-function (resolve-agentic-loop-chat-function))))
    ;; Force sterile instructions only when isolate-p is requested
    (when isolate-p
      (setf (chatbot-system-instruction (conversation-chatbot loop-conversation))
            "You are an autonomous agent. Focus purely on achieving the specified goal. Do not output conversational filler."))
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

(defun abort-agentic-loop (loop-id &key force context)
  "Interrupts the autonomous loop identified by LOOP-ID."
  (declare (ignore force))
  (let ((loop (or (find-agentic-loop loop-id context)
                  (error "Unknown agentic loop id: ~A" loop-id))))
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
    loop))

(defun abort-agentic-loops (&key force context)
  "Interrupts all registered autonomous loops."
  (dolist (loop (list-agentic-loops context) t)
    (abort-agentic-loop (agentic-loop-id loop) :force force :context context)))

(defun resume-agentic-loop (loop-id &key approve context)
  "Resumes a paused autonomous loop after an explicit approval decision."
  (let ((loop (or (find-agentic-loop loop-id context)
                  (error "Unknown agentic loop id: ~A" loop-id))))
    (unless (eq (agentic-loop-status loop) :awaiting-approval)
      (error "Agentic loop ~A is not awaiting approval." loop-id))
    (unless (agentic-loop-pending-approval loop)
      (error "Agentic loop ~A has no pending approval." loop-id))
    (if approve
        (progn
          (unless (agentic-loop-thread-alive-p loop)
            (error "Agentic loop ~A is paused but has no live worker thread." loop-id))
          (setf (agentic-loop-finished-at loop) nil)
          (sb-thread:with-mutex ((agentic-loop-lock loop))
            (setf (agentic-loop-pending-approval-decision loop) t)
            (setf (agentic-loop-status loop) :running)
            (sb-thread:condition-broadcast (agentic-loop-approval-waitqueue loop))))
        (progn
          (unless (agentic-loop-thread-alive-p loop)
            (error "Agentic loop ~A is paused but has no live worker thread." loop-id))
          (setf (agentic-loop-last-error loop) "Approval denied by user.")
          (setf (agentic-loop-result-summary loop) "Approval denied by user.")
          (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))
          (sb-thread:with-mutex ((agentic-loop-lock loop))
            (setf (agentic-loop-pending-approval-decision loop) :deny)
            (setf (agentic-loop-status loop) :interrupted)
            (sb-thread:condition-broadcast (agentic-loop-approval-waitqueue loop)))))
    loop))



(defvar *agentic-loop-monitor-thread* nil
  "The active background thread monitoring agentic loops.")
(defvar *agentic-loop-monitor-active* nil
  "Flag indicating whether the agentic loop monitor should continue running.")
(defvar *agentic-loop-monitor-lock* (sb-thread:make-mutex :name "agentic-loop-monitor-lock")
  "Mutex protecting the monitor thread activation state.")

(defun monitor-agentic-loops-once (&optional context)
  "Scans all registered loops and pushes stuck, pending, or zombie loops into valid states."
  (dolist (loop (list-agentic-loops context))
    (let ((status (agentic-loop-status loop))
          (alive (agentic-loop-thread-alive-p loop)))
      (cond
        ;; 1. Stuck in :pending (registered but worker thread never spawned/started)
        ((eq status :pending)
         (log-message :info (format nil "Monitor: Spawning pending loop ~A" (agentic-loop-id loop)))
         (spawn-agentic-loop-thread loop))

        ;; 2. Zombie state: status is :running or :awaiting-approval, but the worker thread is dead.
        ;; We push it to :failed (aborted) to ensure it doesn't stay stuck forever.
        ((and (member status '(:running :awaiting-approval)) (not alive))
         (log-message :warn (format nil "Monitor: Detected zombie loop ~A (status: ~A, thread dead)" 
                                    (agentic-loop-id loop) status))
         (sb-thread:with-mutex ((agentic-loop-lock loop))
           (setf (agentic-loop-status loop) :failed)
           (setf (agentic-loop-last-error loop) "Worker thread terminated unexpectedly.")
           (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))))

        ;; 3. Invalid/unrecognized states that are not completed or aborted should be pushed to failed.
        ((not (member status '(:pending :running :awaiting-approval :completed :failed :limit-reached :interrupted)))
         (log-message :error (format nil "Monitor: Detected loop ~A in invalid state: ~A. Aborting." 
                                     (agentic-loop-id loop) status))
         (sb-thread:with-mutex ((agentic-loop-lock loop))
           (setf (agentic-loop-status loop) :failed)
           (setf (agentic-loop-last-error loop) (format nil "Unrecognized loop status: ~A" status))
           (setf (agentic-loop-finished-at loop) (get-high-precision-timestamp))))))))

(defvar *reaper-interval-seconds* 600
  "Interval in seconds between thread and memory reaper sweeps (default 10 minutes).")

(defvar *last-reaper-execution-time* 0
  "Timestamp of the last thread and memory reaper sweep.")

(defun reap-orphaned-threads-and-sockets ()
  "Garbage-collects terminal loops from the registry and terminates orphaned background threads."
  (let ((all-threads (sb-thread:list-all-threads)))
    ;; 1. Reap terminal agentic loops from global registry to free memory
    (sb-thread:with-mutex (*agentic-loop-registry-lock*)
      (let ((ids-to-remove nil))
        (maphash (lambda (id loop)
                   (let ((status (agentic-loop-status loop))
                         (finished (agentic-loop-finished-at loop)))
                     ;; If the loop is finished (terminal status) and has been finished for more than 5 minutes
                     (when (and (member status '(:completed :failed :limit-reached :interrupted))
                                finished
                                (> (- (get-high-precision-timestamp) finished) 300))
                       (push id ids-to-remove))))
                 *agentic-loop-registry*)
        (dolist (id ids-to-remove)
          (log-message :info (format nil "Reaper: Pruning completed loop ~A from registry." id))
          (remhash id *agentic-loop-registry*))))
    
    ;; 2. Detect and terminate orphaned/hung worker threads
    (dolist (thread all-threads)
      (let ((name (sb-thread:thread-name thread)))
        (when (and name (alexandria:starts-with-subseq "Agentic-Loop-Worker-" name))
          (let* ((id-str (subseq name (length "Agentic-Loop-Worker-")))
                 (id (parse-integer id-str :junk-allowed t)))
            (when id
              ;; Look up in registry. If the loop is not registered, or is in terminal state, terminate!
              (let ((loop (sb-thread:with-mutex (*agentic-loop-registry-lock*)
                            (gethash id *agentic-loop-registry*))))
                (when (or (null loop)
                          (member (agentic-loop-status loop) '(:completed :failed :limit-reached :interrupted)))
                  (log-message :warn (format nil "Reaper: Terminating orphaned worker thread: ~A" name))
                  (handler-case (sb-thread:terminate-thread thread)
                    (error () nil)))))))))))

(defun run-agentic-loop-monitor ()
  "The execution loop for the background monitor."
  (loop
    while *agentic-loop-monitor-active*
    do (handler-case
           (progn
             (monitor-agentic-loops-once)
             ;; Run reaper sweep if interval has elapsed
             (let ((now (get-high-precision-timestamp)))
               (when (>= (- now *last-reaper-execution-time*) *reaper-interval-seconds*)
                 (setf *last-reaper-execution-time* now)
                 (reap-orphaned-threads-and-sockets)))
             (sleep 5))
         (error (condition)
           (log-message :error (format nil "Agentic loop monitor error: ~A" condition))
           (sleep 5)))))

(defun start-agentic-loop-monitor ()
  "Starts the background monitor thread."
  (sb-thread:with-mutex (*agentic-loop-monitor-lock*)
    (unless *agentic-loop-monitor-active*
      (setf *agentic-loop-monitor-active* t)
      (setf *agentic-loop-monitor-thread*
            (sb-thread:make-thread #'run-agentic-loop-monitor
                                   :name "Agentic-Loop-Monitor"))
      (log-message :info "Agentic loop monitor started.")))
  *agentic-loop-monitor-thread*)

(defun stop-agentic-loop-monitor ()
  "Stops the background monitor thread."
  (sb-thread:with-mutex (*agentic-loop-monitor-lock*)
    (when *agentic-loop-monitor-active*
      (setf *agentic-loop-monitor-active* nil)
      (let ((thread *agentic-loop-monitor-thread*))
        (when (and thread (sb-thread:thread-alive-p thread))
          (sb-thread:join-thread thread :timeout 5)
          (when (sb-thread:thread-alive-p thread)
            (sb-thread:terminate-thread thread))))
      (setf *agentic-loop-monitor-thread* nil)
      (log-message :info "Agentic loop monitor stopped.")))
  t)
