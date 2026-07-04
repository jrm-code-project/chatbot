;;; -*- Lisp -*-
;;; chat-entry.lisp - public chat entry context routing

(in-package "CHATBOT")

(defun require-chat-conversation (conversation context)
  "Returns the effective conversation for CONTEXT or signals when none is available."
  (or conversation
      (current-default-conversation context)
      (error "No conversation provided and the ambient default conversation is NIL. Please specify a conversation or set *default-conversation*.")))

(defun route-chat-turn-conversation (conversation input context)
  "Returns CONVERSATION or the active planner conversation for INPUT when planning mode is active."
  (let ((planner-conversation (current-active-planner context)))
    (if (and planner-conversation
             (not (eq conversation planner-conversation)))
        (progn
          (log-message :info "Routing chat input to active planner minion in Planning Mode"
                       :context `(("input" . ,input)))
          planner-conversation)
        conversation)))

(defun resolve-chat-conversation (conversation context &key input)
  "Returns the effective conversation for CONTEXT, optionally planner-routed for INPUT."
  (let ((effective-conversation (require-chat-conversation conversation context)))
    (if input
        (route-chat-turn-conversation effective-conversation input context)
        effective-conversation)))

(defun chat-conversation-runtime-context (conversation fallback-context)
  "Returns CONVERSATION's runtime context, or FALLBACK-CONTEXT when absent."
  (or (and conversation
           (chatbot-runtime-context (conversation-chatbot conversation)))
      fallback-context))

(defun resolve-chat-entry-context (conversation)
  "Returns the initial runtime context for a public chat entry."
  (or (chat-conversation-runtime-context conversation nil)
      (resolve-runtime-context nil)))

(defun call-with-active-chat-conversation (conversation context thunk)
  "Calls THUNK with CONVERSATION recorded as the current active conversation for CONTEXT."
  (let ((previous-active-conversation (current-active-conversation context)))
    (setf (current-active-conversation context) conversation)
    (unwind-protect
         (funcall thunk)
      (setf (current-active-conversation context) previous-active-conversation))))

(defun execute-chat-entry-shell (conversation context thunk)
  "Runs THUNK with CONVERSATION bound as the active conversation inside CONTEXT."
  (call-with-runtime-context
   context
   (lambda ()
     (let* ((active-conversation (resolve-chat-conversation conversation context))
            (active-context (chat-conversation-runtime-context active-conversation context)))
       (call-with-runtime-context
        active-context
        (lambda ()
          (call-with-active-chat-conversation
           active-conversation
           active-context
           (lambda ()
             (funcall thunk active-conversation active-context)))))))))
