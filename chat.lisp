;;; -*- Lisp -*-
;;; chat.lisp - backend dispatch entry point

(in-package "CHATBOT")

(defun chat (input &key conversation callback file files)
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
                                       (prepare-chat-file-attachments effective-files))))
       (case (chatbot-backend bot)
         (:gemini
          (multiple-value-bind (effective-input effective-model)
                (resolve-prompt-model-override bot input)
            (chat-gemini bot
                           effective-input
                           conversation
                           callback
                           :file-attachments file-attachments
                           :effective-model effective-model)))
         (:openai
          (chat-openai bot input conversation callback :file-attachments file-attachments))
         (:lm-studio
          (chat-openai bot input conversation callback :file-attachments file-attachments))
         (:google
          (multiple-value-bind (effective-input effective-model)
                (resolve-prompt-model-override bot input)
            (chat-google bot
                           effective-input
                           conversation
                           callback
                           :file-attachments file-attachments
                           :effective-model effective-model)))
         (t
          (error "Unknown chatbot backend: ~S" (chatbot-backend bot))))))))))
