;;; -*- Lisp -*-
;;; backend-registry.lisp - pluggable chat backend dispatch

(in-package "CHATBOT")

(defvar *chat-backends* (make-hash-table :test 'equal)
  "Backend keywords mapped to chat handler function designators.")

(defun register-chat-backend (keyword handler-fn)
  "Registers HANDLER-FN as the chat handler for backend KEYWORD.
Handlers receive the prompt as their first argument, followed by keyword
arguments :BOT, :CONVERSATION, :CALLBACK, :FILE-ATTACHMENTS,
:EFFECTIVE-MODEL, and :EFFECTIVE-GENERATION-CONFIG."
  (check-type keyword keyword)
  (check-type handler-fn (or function symbol))
  (setf (gethash keyword *chat-backends*) handler-fn))
