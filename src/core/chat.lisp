;;; -*- Lisp -*-
;;; chat.lisp - backend dispatch entry point

(in-package "CHATBOT")

(defun annotate-chat-turn-result (result conversation)
  "Returns RESULT annotated with its target CONVERSATION."
  (append (list :conversation conversation) result))

(defun dispatch-chat-turn (conversation input callback
                           &key file-attachments effective-model effective-generation-config)
  "Dispatches one prepared chat turn for CONVERSATION's backend."
  (let* ((bot (conversation-chatbot conversation))
         (result
           (case (chatbot-backend bot)
             (:gemini
              (chat-gemini bot
                           input
                           conversation
                           callback
                           :file-attachments file-attachments
                           :effective-model effective-model
                           :effective-generation-config effective-generation-config
                           :return-turn-result-p t))
             (:openai
              (chat-openai bot input conversation callback
                           :file-attachments file-attachments
                           :effective-generation-config effective-generation-config
                           :return-turn-result-p t))
             (:lm-studio
              (chat-openai bot input conversation callback
                           :file-attachments file-attachments
                           :effective-generation-config effective-generation-config
                           :return-turn-result-p t))
             (:google
              (chat-google bot
                           input
                           conversation
                           callback
                           :file-attachments file-attachments
                           :effective-model effective-model
                           :effective-generation-config effective-generation-config
                           :return-turn-result-p t))
             (t
              (error "Unknown chatbot backend: ~S" (chatbot-backend bot))))))
    (annotate-chat-turn-result result conversation)))

(defun chat-turn (input &key conversation callback file files
                       (temperature nil temperature-specified-p)
                       (top-p nil top-p-specified-p)
                       ((:temperature-specified-p explicit-temperature-specified-p) temperature-specified-p)
                       ((:top-p-specified-p explicit-top-p-specified-p) top-p-specified-p)
                       context)
  "Runs one chat turn and returns a normalized turn result without mutating the source conversation."
  (let ((conversation (or conversation
                          (current-default-conversation context))))
    (unless conversation
      (error "No conversation provided and the ambient default conversation is NIL. Please specify a conversation or set *default-conversation*."))
    (let ((planner-conversation (current-active-planner context)))
      (when (and planner-conversation
                 (not (eq conversation planner-conversation)))
        (log-message :info "Routing chat input to active planner minion in Planning Mode"
                     :context `(("input" . ,input)))
        (setf conversation planner-conversation)))
    (let* ((bot (conversation-chatbot conversation))
           (effective-files (append (when file
                                      (list file))
                                    files))
           (file-attachments (and effective-files
                                  (prepare-chat-file-attachments effective-files)))
           (effective-generation-config
             (apply #'resolve-effective-generation-config
                    bot
                    (append (when explicit-temperature-specified-p
                              (list :temperature temperature))
                           (when explicit-top-p-specified-p
                              (list :top-p top-p)))))
           (pruned-messages (prune-conversation-context-if-needed conversation))
           (turn-conversation (clone-conversation conversation
                                                 :messages pruned-messages)))
      (multiple-value-bind (effective-input effective-model)
          (resolve-prompt-model-override bot input)
        (annotate-chat-turn-result
         (dispatch-chat-turn turn-conversation
                             effective-input
                             callback
                             :file-attachments file-attachments
                             :effective-model effective-model
                             :effective-generation-config effective-generation-config)
         conversation)))))

(defun chat (input &key conversation callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Sends user input to the active conversation using the appropriate backend API.
If a callback is provided, each text token is passed to it in real-time.
Returns the complete response text."
  (let ((context (and conversation
                      (chatbot-runtime-context (conversation-chatbot conversation)))))
    (call-with-runtime-context
     context
     (lambda ()
       (let ((active-conversation (or conversation
                                      (current-default-conversation context)))
             (previous-active-conversation (current-active-conversation context)))
         (unless active-conversation
           (error "No conversation provided and the ambient default conversation is NIL. Please specify a conversation or set *default-conversation*."))
         (setf (current-active-conversation context) active-conversation)
         (unwind-protect
              (let* ((result (chat-turn input
                                        :conversation active-conversation
                                        :callback callback
                                        :file file
                                        :files files
                                        :temperature temperature
                                        :temperature-specified-p temperaturep
                                        :top-p top-p
                                        :top-p-specified-p top-pp
                                        :context context))
                     (effective-conversation (chat-turn-result-conversation result))
                     (d-bot (conversation-chatbot effective-conversation))
                     (original-name (chatbot-persona-name d-bot)))
                (apply-chat-turn-result result effective-conversation)
                (unless original-name
                  (setf (chatbot-persona-name d-bot) "DefaultConversation"))
                (log-message :info "Checkpointing conversation after chat"
                             :context `(("name" . ,(chatbot-persona-name d-bot))))
                (unwind-protect
                     (save-minion-state effective-conversation)
                  (unless original-name
                    (setf (chatbot-persona-name d-bot) nil)))
                (chat-turn-result-text result))
           (setf (current-active-conversation context) previous-active-conversation)))))))
