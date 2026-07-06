;;; -*- Lisp -*-
;;; agentic-loop-startup.lisp - startup shaping for autonomous background loops

(in-package "CHATBOT")

(defvar *agentic-loop-start-history-message-limit* 6
  "Maximum number of recent stored messages retained when cloning a conversation for an agentic loop.")

(defvar *agentic-loop-start-system-instruction*
  (format nil
          "You are an autonomous agent executing the current goal. Focus on the goal, use available tools when helpful, follow configured safety and approval constraints, and reply tersely without conversational filler.~%~%~A"
          +agentic-operational-directive+)
  "Compact system instruction used for agentic-loop startup clones.")

(defvar *isolated-agentic-loop-start-system-instruction*
  (format nil
          "You are a background process. Reply in JSON.~%~%~A"
          +agentic-operational-directive+)
  "Brutally sterile system instruction used for isolated agentic-loop startup clones.")

(defun trim-agentic-loop-start-history (messages)
  "Returns an aggressively trimmed recent suffix of MESSAGES for loop startup."
  (let* ((history-length (length messages))
         (limited-history (if (<= history-length *agentic-loop-start-history-message-limit*)
                              messages
                              (nthcdr (- history-length *agentic-loop-start-history-message-limit*)
                                      messages)))
         (first-user-index (position-if (lambda (message)
                                          (string= "user" (cdr (assoc "role" message :test #'equal))))
                                        limited-history))
         (aligned-history (if (and first-user-index
                                   (> first-user-index 0))
                              (nthcdr first-user-index limited-history)
                              limited-history)))
    (and aligned-history
         (copy-tree aligned-history))))

(defun clone-chatbot-for-agentic-loop (chatbot &key isolate-p)
  "Returns a loop-owned CHATBOT clone with loop-specific startup instructions."
  (clone-chatbot chatbot
                 :system-instruction (if isolate-p
                                         *isolated-agentic-loop-start-system-instruction*
                                         *agentic-loop-start-system-instruction*)
                 :system-instruction-path nil
                 :system-instruction-storage-kind :transient))

(defun clone-conversation-for-agentic-loop (conversation &key isolate-p)
  "Returns a loop-owned clone of CONVERSATION and its chatbot, optionally isolated."
  (let ((chatbot (conversation-chatbot conversation)))
    (if isolate-p
        (clone-conversation conversation
                            :chatbot (clone-chatbot-for-agentic-loop chatbot :isolate-p t)
                            :persona-memory nil
                            :persona-diary-entries nil
                            :messages nil
                            :interaction-id nil)
        (let* ((source-messages (conversation-messages conversation))
               (trimmed-messages (trim-agentic-loop-start-history source-messages))
               (loop-chatbot (clone-chatbot-for-agentic-loop chatbot)))
          (clone-conversation conversation
                              :chatbot loop-chatbot
                              :persona-memory nil
                              :persona-diary-entries nil
                              :messages trimmed-messages
                              :interaction-id nil)))))

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
