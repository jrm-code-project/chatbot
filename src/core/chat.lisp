;;; -*- Lisp -*-
;;; chat.lisp - backend dispatch entry point

(in-package "CHATBOT")

(defun dispatch-chat-turn (conversation input callback
                           &key file-attachments effective-model effective-generation-config)
  "Dispatches one prepared chat turn for CONVERSATION's backend."
  (let ((bot (conversation-chatbot conversation)))
    (case (chatbot-backend bot)
      (:gemini
       (chat-gemini bot
                    input
                    conversation
                    callback
                    :file-attachments file-attachments
                    :effective-model effective-model
                    :effective-generation-config effective-generation-config))
      (:openai
       (chat-openai bot input conversation callback
                    :file-attachments file-attachments
                    :effective-generation-config effective-generation-config))
      (:lm-studio
       (chat-openai bot input conversation callback
                    :file-attachments file-attachments
                    :effective-generation-config effective-generation-config))
      (:google
       (chat-google bot
                    input
                    conversation
                    callback
                    :file-attachments file-attachments
                    :effective-model effective-model
                    :effective-generation-config effective-generation-config))
      (t
       (error "Unknown chatbot backend: ~S" (chatbot-backend bot))))))

(defun chat (input &key conversation callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Sends user input to the active conversation using the appropriate backend API.
If a callback is provided, each text token is passed to it in real-time.
Returns the complete response text."
  (let ((context (and conversation
                      (chatbot-runtime-context (conversation-chatbot conversation)))))
    (call-with-runtime-context
     context
     (lambda ()
       (let ((conversation (or conversation
                               (current-default-conversation context))))
         (unless conversation
           (error "No conversation provided and the ambient default conversation is NIL. Please specify a conversation or set *default-conversation*."))
         (let* ((bot (conversation-chatbot conversation))
                (effective-files (append (when file
                                          (list file))
                                         files))
                (file-attachments (and effective-files
                                       (prepare-chat-file-attachments effective-files)))
                (effective-generation-config
                  (apply #'resolve-effective-generation-config
                         bot
                         (append (when temperaturep
                                   (list :temperature temperature))
                                 (when top-pp
                                   (list :top-p top-p))))))
           (multiple-value-bind (effective-input effective-model)
               (resolve-prompt-model-override bot input)
             (let ((*active-conversation* conversation))
               (dispatch-chat-turn conversation
                                   effective-input
                                   callback
                                   :file-attachments file-attachments
                                   :effective-model effective-model
                                   :effective-generation-config effective-generation-config)))))))))
