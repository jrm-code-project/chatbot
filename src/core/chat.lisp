;;; -*- Lisp -*-
;;; chat.lisp - backend dispatch entry point

(in-package "CHATBOT")

(defun annotate-chat-turn-result (result conversation)
  "Returns RESULT annotated with its target CONVERSATION."
  (append (list :conversation conversation) result))

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

(defun call-with-active-chat-conversation (conversation context thunk)
  "Calls THUNK with CONVERSATION recorded as the current active conversation for CONTEXT."
  (let ((previous-active-conversation (current-active-conversation context)))
    (setf (current-active-conversation context) conversation)
    (unwind-protect
         (funcall thunk)
      (setf (current-active-conversation context) previous-active-conversation))))

(defun apply-and-checkpoint-chat-turn-result (result)
  "Applies RESULT to its target conversation, checkpoints it, and returns the final text."
  (let ((effective-conversation (chat-turn-result-conversation result)))
    (apply-chat-turn-result result effective-conversation)
    (checkpoint-conversation-after-chat effective-conversation)
    (chat-turn-result-text result)))

(defun chat-backend-dispatch-key (bot)
  "Returns the backend dispatch key for BOT."
  (chatbot-backend bot))

(defun invoke-gemini-chat-turn (bot conversation input callback file-attachments effective-model effective-generation-config)
  "Invokes one Gemini backend turn."
  (chat-gemini bot
               input
               conversation
               callback
               :file-attachments file-attachments
               :effective-model effective-model
               :effective-generation-config effective-generation-config
               :return-turn-result-p t))

(defun invoke-openai-chat-turn (bot conversation input callback file-attachments effective-generation-config)
  "Invokes one OpenAI-compatible backend turn."
  (chat-openai bot input conversation callback
               :file-attachments file-attachments
               :effective-generation-config effective-generation-config
               :return-turn-result-p t))

(defun invoke-google-chat-turn (bot conversation input callback file-attachments effective-model effective-generation-config)
  "Invokes one Google generateContent backend turn."
  (chat-google bot
               input
               conversation
               callback
               :file-attachments file-attachments
               :effective-model effective-model
               :effective-generation-config effective-generation-config
               :return-turn-result-p t))

(defun invoke-chat-backend-turn (bot conversation input callback
                                 &key file-attachments effective-model effective-generation-config)
  "Invokes the backend-specific chat turn for BOT."
  (case (chat-backend-dispatch-key bot)
    (:gemini
     (invoke-gemini-chat-turn bot conversation input callback
                              file-attachments effective-model effective-generation-config))
    ((:openai :lm-studio)
     (invoke-openai-chat-turn bot conversation input callback
                              file-attachments effective-generation-config))
    (:google
     (invoke-google-chat-turn bot conversation input callback
                              file-attachments effective-model effective-generation-config))
    (t
     (error "Unknown chatbot backend: ~S" (chatbot-backend bot)))))

(defun dispatch-chat-turn (conversation input callback
                           &key file-attachments effective-model effective-generation-config)
  "Dispatches one prepared chat turn for CONVERSATION's backend."
  (let* ((bot (conversation-chatbot conversation))
         (result
           (invoke-chat-backend-turn bot
                                     conversation
                                     input
                                     callback
                                     :file-attachments file-attachments
                                     :effective-model effective-model
                                     :effective-generation-config effective-generation-config)))
    (annotate-chat-turn-result result conversation)))

(defun chat-turn (input &key conversation callback file files
                       (temperature nil temperature-specified-p)
                       (top-p nil top-p-specified-p)
                       ((:temperature-specified-p explicit-temperature-specified-p) temperature-specified-p)
                       ((:top-p-specified-p explicit-top-p-specified-p) top-p-specified-p)
                       context)
  "Runs one chat turn and returns a normalized turn result without mutating the source conversation."
  (let* ((base-conversation (require-chat-conversation conversation context))
         (conversation (route-chat-turn-conversation base-conversation input context))
         (bot (conversation-chatbot conversation))
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
       conversation))))

(defun chat (input &key conversation callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Sends user input to the active conversation using the appropriate backend API.
If a callback is provided, each text token is passed to it in real-time.
Returns the complete response text."
  (let ((context (and conversation
                     (chatbot-runtime-context (conversation-chatbot conversation)))))
    (call-with-runtime-context
     context
     (lambda ()
       (let ((active-conversation (require-chat-conversation conversation context)))
         (call-with-active-chat-conversation
          active-conversation
          context
          (lambda ()
            (apply-and-checkpoint-chat-turn-result
             (chat-turn input
                        :conversation active-conversation
                        :callback callback
                        :file file
                        :files files
                        :temperature temperature
                        :temperature-specified-p temperaturep
                        :top-p top-p
                        :top-p-specified-p top-pp
                        :context context)))))))))
