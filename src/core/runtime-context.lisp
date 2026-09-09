;;;

(in-package "CHATBOT")

;;; The RUNTIME-CONTEXT object: ambient special variables, construction,
;;; resolution, generic per-slot CURRENT-* accessors, the unified worker
;;; registry, and CALL-WITH-RUNTIME-CONTEXT dynamic binding.

(declaim (special *active-runtime-context*
                  *default-runtime-context*
                  *active-resource-supervisor*))

(defvar *active-resource-supervisor* nil
  "The resource supervisor currently bound dynamically, when any.")

(defun current-resource-supervisor ()
  "Returns the active resource supervisor if bound, otherwise the fallback context supervisor."
  (or *active-resource-supervisor*
      (and *active-runtime-context*
           (runtime-context-supervisor *active-runtime-context*))
      (and *default-runtime-context*
           (runtime-context-supervisor *default-runtime-context*))))

(defvar *active-runtime-context* nil
  "Runtime context currently bound by CALL-WITH-RUNTIME-CONTEXT, when any.")

(defvar *default-runtime-context* nil
  "Canonical runtime context used for legacy no-context entry points.")

(defvar *active-conversation* nil
  "Deprecated compatibility alias for the active conversation.
Runtime code no longer consults or mirrors this special; use
CURRENT-ACTIVE-CONVERSATION with an explicit runtime context instead.")

(defun runtime-context-accessor-value (context accessor)
  "Reads ACCESSOR from CONTEXT."
  (funcall accessor context))

(defun set-runtime-context-accessor-value (context accessor value)
  "Stores VALUE on CONTEXT through ACCESSOR."
  (funcall (fdefinition (list 'setf accessor)) value context))

(defun (setf runtime-context-accessor-value) (value context accessor)
  "Stores VALUE on CONTEXT through ACCESSOR using SETF."
  (set-runtime-context-accessor-value context accessor value))

(defun eager-mcp-startup-enabled-p ()
  "Returns true when eager shared MCP startup is enabled via environment."
  (let ((value (funcall *getenv-function* "CHATBOT_EAGER_MCP_STARTUP")))
    (and value
         (member (string-downcase value)
                 '("1" "true" "yes" "on")
                 :test #'string=))))

(defun make-runtime-context (&key (mcp-config-path nil mcp-config-path-p)
                                  (startup-chatbot nil startup-chatbot-p)
                                  (auto-initialize-startup-mcp-servers-p nil auto-init-p)
                                  (logging-enabled-p nil logging-enabled-p-p)
                                  (log-level nil log-level-p)
                                  (log-stream nil log-stream-p)
                                  (http-connect-timeout nil http-connect-timeout-p)
                                  (http-read-timeout nil http-read-timeout-p)
                                  (getenv-function nil getenv-function-p)
                                  (http-post-function nil http-post-function-p)
                                  (http-get-function nil http-get-function-p)
                                  (http-patch-function nil http-patch-function-p)
                                  (http-delete-function nil http-delete-function-p)
                                  (gemini-api-key-function nil gemini-api-key-function-p)
                                  (filesystem-access-approval-function nil filesystem-access-approval-function-p)
                                  (eval-approval-function nil eval-approval-function-p)
                                  (default-conversation nil default-conversation-p)
                                  (agentic-loop-default-backend nil agentic-loop-default-backend-p)
                                  (agentic-loop-default-model nil agentic-loop-default-model-p)
                                  (active-conversation nil active-conversation-p)
                                  (active-planner nil active-planner-p)
                                  (active-planner-parent-conversation nil active-planner-parent-conversation-p))
  "Constructs the preferred public container for shared Chatbot runtime state.
Use this with explicit :RUNTIME-CONTEXT arguments instead of mutating the
compatibility-only ambient special variables."
  (let ((template (resolve-runtime-context nil)))
    (flet ((inherit (provided-p explicit-value template-reader fallback-value)
             (if provided-p
                 explicit-value
                 (if template
                     (funcall template-reader template)
                     fallback-value))))
      (make-instance 'runtime-context
                     :mcp-config-path (inherit mcp-config-path-p
                                               mcp-config-path
                                               #'runtime-context-mcp-config-path
                                               nil)
                     :startup-chatbot (if startup-chatbot-p
                                          startup-chatbot
                                          nil)
                     :auto-initialize-startup-mcp-servers-p
                     (inherit auto-init-p
                              auto-initialize-startup-mcp-servers-p
                              #'runtime-context-auto-initialize-startup-mcp-servers-p
                              (eager-mcp-startup-enabled-p))
                     :logging-enabled-p (inherit logging-enabled-p-p
                                                 logging-enabled-p
                                                 #'runtime-context-logging-enabled-p
                                                 t)
                     :log-level (inherit log-level-p
                                         log-level
                                         #'runtime-context-log-level
                                         :info)
                     :log-stream (inherit log-stream-p
                                          log-stream
                                          #'runtime-context-log-stream
                                          *error-output*)
                     :http-connect-timeout (inherit http-connect-timeout-p
                                                    http-connect-timeout
                                                    #'runtime-context-http-connect-timeout
                                                    15)
                     :http-read-timeout (inherit http-read-timeout-p
                                                 http-read-timeout
                                                 #'runtime-context-http-read-timeout
                                                 120)
                     :getenv-function (inherit getenv-function-p
                                               getenv-function
                                               #'runtime-context-getenv-function
                                               *getenv-function*)
                     :http-post-function (inherit http-post-function-p
                                                  http-post-function
                                                  #'runtime-context-http-post-function
                                                  *http-post-function*)
                     :http-get-function (inherit http-get-function-p
                                                 http-get-function
                                                 #'runtime-context-http-get-function
                                                 *http-get-function*)
                     :http-patch-function (inherit http-patch-function-p
                                                   http-patch-function
                                                   #'runtime-context-http-patch-function
                                                   *http-patch-function*)
                     :http-delete-function (inherit http-delete-function-p
                                                    http-delete-function
                                                    #'runtime-context-http-delete-function
                                                    *http-delete-function*)
                     :gemini-api-key-function (inherit gemini-api-key-function-p
                                                       gemini-api-key-function
                                                       #'runtime-context-gemini-api-key-function
                                                       *gemini-api-key-function*)
                     :filesystem-access-approval-function
                     (inherit filesystem-access-approval-function-p
                              filesystem-access-approval-function
                              #'runtime-context-filesystem-access-approval-function
                              *filesystem-access-approval-function*)
                     :eval-approval-function (inherit eval-approval-function-p
                                                      eval-approval-function
                                                      #'runtime-context-eval-approval-function
                                                      *eval-approval-function*)
                     :default-conversation (inherit default-conversation-p
                                                   default-conversation
                                                   #'runtime-context-default-conversation
                                                   nil)
                     :agentic-loop-default-backend
                     (inherit agentic-loop-default-backend-p
                              agentic-loop-default-backend
                              #'runtime-context-agentic-loop-default-backend
                              nil)
                     :agentic-loop-default-model
                     (inherit agentic-loop-default-model-p
                              agentic-loop-default-model
                              #'runtime-context-agentic-loop-default-model
                              nil)
                     :active-conversation (if active-conversation-p
                                             active-conversation
                                             nil)
                     :active-planner (if active-planner-p
                                         active-planner
                                         nil)
                     :active-planner-parent-conversation
                     (if active-planner-parent-conversation-p
                         active-planner-parent-conversation
                         nil)))))

(defun default-runtime-context-p (context)
  "Returns true when CONTEXT is the canonical default runtime context."
  (and context
       (eq context *default-runtime-context*)))

(defun active-runtime-context-p (context)
  "Returns true when CONTEXT is the currently bound runtime context."
  (and context
       (eq context *active-runtime-context*)))

(defun legacy-global-value (symbol)
  "Returns SYMBOL's current ambient legacy-global value."
  (symbol-value symbol))

(defun resolve-runtime-context (context)
  "Returns CONTEXT, otherwise the active context, otherwise the canonical default context."
  (or context
      *active-runtime-context*
      *default-runtime-context*))

(defun normalize-runtime-worker-kind (kind)
  "Returns KIND normalized to one canonical worker kind keyword."
  (let ((normalized
         (cond
           ((keywordp kind) kind)
           ((symbolp kind) (intern (string-upcase (symbol-name kind)) "KEYWORD"))
           ((stringp kind) (intern (string-upcase kind) "KEYWORD"))
           (t kind))))
    (case normalized
      (:subordinate :delegated)
      (:autonomous :loop)
      ((:delegated :planner :loop) normalized)
      (t
       (error "Unsupported runtime worker kind: ~A" kind)))))

(defun runtime-worker-kind-public-name (kind)
  "Returns the public API name for KIND."
  (case (normalize-runtime-worker-kind kind)
    (:loop "autonomous")
    (t
     (string-downcase
      (symbol-name (normalize-runtime-worker-kind kind))))))

(defun subordinate-runtime-worker-kind-p (kind)
  "Returns true when KIND identifies a delegated or planner worker."
  (member (normalize-runtime-worker-kind kind)
         '(:delegated :planner)))

(defun make-runtime-worker-entry (&key worker-id kind conversation loop owner-bot)
  "Returns one unified runtime worker entry."
  (list :worker-id worker-id
       :kind (normalize-runtime-worker-kind kind)
       :conversation conversation
       :loop loop
       :owner-bot owner-bot))

(defun runtime-worker-entry-worker-id (entry)
  "Returns ENTRY's worker id."
  (getf entry :worker-id))

(defun runtime-worker-entry-kind (entry)
  "Returns ENTRY's worker kind."
  (getf entry :kind))

(defun runtime-worker-entry-conversation (entry)
  "Returns ENTRY's subordinate/planner conversation, or NIL."
  (getf entry :conversation))

(defun runtime-worker-entry-loop (entry)
  "Returns ENTRY's autonomous loop, or NIL."
  (getf entry :loop))

(defun runtime-worker-entry-owner-bot (entry)
  "Returns ENTRY's owning chatbot, or NIL."
  (getf entry :owner-bot))

(defun register-runtime-worker-entry (entry &optional context)
  "Stores ENTRY in CONTEXT's unified worker registry."
  (let* ((resolved-context (resolve-runtime-context context))
        (registry (runtime-context-worker-registry resolved-context))
        (lock (runtime-context-worker-registry-lock resolved-context)))
    (sb-thread:with-mutex (lock)
     (setf (gethash (runtime-worker-entry-worker-id entry) registry) entry))
    entry))

(defun remove-runtime-worker-entry (worker-id &optional context)
  "Removes WORKER-ID from CONTEXT's unified worker registry."
  (let* ((resolved-context (resolve-runtime-context context))
        (registry (runtime-context-worker-registry resolved-context))
        (lock (runtime-context-worker-registry-lock resolved-context)))
    (sb-thread:with-mutex (lock)
     (remhash worker-id registry)))
  worker-id)

(defun find-runtime-worker-entry (worker-id &optional context)
  "Returns the unified worker entry identified by WORKER-ID, or NIL."
  (let* ((resolved-context (resolve-runtime-context context))
        (registry (runtime-context-worker-registry resolved-context))
        (lock (runtime-context-worker-registry-lock resolved-context)))
    (sb-thread:with-mutex (lock)
     (gethash worker-id registry))))

(defun list-runtime-worker-entries (&optional context)
  "Returns all unified worker entries in CONTEXT."
  (let* ((resolved-context (resolve-runtime-context context))
        (registry (runtime-context-worker-registry resolved-context))
        (lock (runtime-context-worker-registry-lock resolved-context)))
    (sb-thread:with-mutex (lock)
     (loop for entry being the hash-values of registry
           collect entry))))

(defmacro define-runtime-context-accessor (name accessor)
  `(progn
     (defun ,name (&optional context)
       (let ((resolved (resolve-runtime-context context)))
         (and resolved (runtime-context-accessor-value resolved ',accessor))))
     (defun (setf ,name) (value &optional context)
       (let ((resolved (resolve-runtime-context context)))
         (when resolved
           (setf (runtime-context-accessor-value resolved ',accessor) value)))
       value)))

(define-runtime-context-accessor current-default-conversation runtime-context-default-conversation)
(define-runtime-context-accessor current-mcp-config-path runtime-context-mcp-config-path)
(define-runtime-context-accessor current-startup-chatbot runtime-context-startup-chatbot)
(define-runtime-context-accessor current-auto-initialize-startup-mcp-servers-p runtime-context-auto-initialize-startup-mcp-servers-p)
(define-runtime-context-accessor current-logging-enabled-p runtime-context-logging-enabled-p)
(define-runtime-context-accessor current-log-level runtime-context-log-level)
(define-runtime-context-accessor current-log-stream runtime-context-log-stream)
(define-runtime-context-accessor current-http-connect-timeout runtime-context-http-connect-timeout)
(define-runtime-context-accessor current-http-read-timeout runtime-context-http-read-timeout)
(define-runtime-context-accessor current-agentic-loop-default-backend runtime-context-agentic-loop-default-backend)
(define-runtime-context-accessor current-agentic-loop-default-model runtime-context-agentic-loop-default-model)

(define-runtime-context-accessor current-getenv-function runtime-context-getenv-function)
(define-runtime-context-accessor current-http-post-function runtime-context-http-post-function)
(define-runtime-context-accessor current-http-get-function runtime-context-http-get-function)
(define-runtime-context-accessor current-http-patch-function runtime-context-http-patch-function)
(define-runtime-context-accessor current-http-delete-function runtime-context-http-delete-function)
(define-runtime-context-accessor current-gemini-api-key-function runtime-context-gemini-api-key-function)

(define-runtime-context-accessor current-filesystem-access-approval-function runtime-context-filesystem-access-approval-function)
(define-runtime-context-accessor current-eval-approval-function runtime-context-eval-approval-function)
(define-runtime-context-accessor current-shell-approval-function runtime-context-shell-approval-function)

(define-runtime-context-accessor current-active-conversation runtime-context-active-conversation)
(define-runtime-context-accessor current-active-planner runtime-context-active-planner)
(define-runtime-context-accessor current-active-planner-parent-conversation runtime-context-active-planner-parent-conversation)

(defun call-with-runtime-context (context thunk
                                 &key
                                   (default-conversation-compatibility-p t)
                                   (legacy-function-seam-compatibility-p t))
  "Calls THUNK with the resolved runtime context active.
Function seams and approval seams now resolve through the active runtime
context directly."
  (declare (ignore default-conversation-compatibility-p
                    legacy-function-seam-compatibility-p))
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((null resolved-context)
       (funcall thunk))
      ((active-runtime-context-p resolved-context)
       (funcall thunk))
      (t
       (let ((*active-runtime-context* resolved-context)
             (*active-resource-supervisor* (runtime-context-supervisor resolved-context)))
         (funcall thunk))))))

(setf *default-runtime-context* (make-runtime-context))
