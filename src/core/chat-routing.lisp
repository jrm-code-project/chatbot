;;; -*- Lisp -*-
;;; chat-routing.lisp - conversation resolution and planner routing

(in-package "CHATBOT")

(defun require-chat-conversation (conversation context)
  "Returns the effective conversation for CONTEXT or signals when none is available."
  (or conversation
      (current-default-conversation context)
      (error "No conversation provided and the canonical default conversation is NIL. Please specify a conversation or set CURRENT-DEFAULT-CONVERSATION.")))

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
