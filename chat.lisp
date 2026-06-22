;;; -*- Lisp -*-
;;; chat.lisp - backend dispatch entry point

(in-package "CHATBOT")

(defun chat (input &key conversation callback)
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
         (let ((bot (conversation-chatbot conversation)))
       (case (chatbot-backend bot)
         (:gemini
          (chat-gemini bot input conversation callback))
         (:openai
          (chat-openai bot input conversation callback))
         (:lm-studio
          (chat-openai bot input conversation callback))
         (:google
          (chat-google bot input conversation callback))
         (t
          (error "Unknown chatbot backend: ~S" (chatbot-backend bot))))))))))
