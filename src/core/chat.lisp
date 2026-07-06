;;; -*- Lisp -*-
;;; chat.lisp - backend dispatch entry point

(in-package "CHATBOT")

(defun annotate-chat-turn-result (result conversation &key provider-conversation)
  "Returns RESULT annotated with its target CONVERSATION."
  (append (list :conversation conversation)
          (when provider-conversation
            (list :provider-conversation provider-conversation))
          result))

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

(defun chat-turn-effective-files (file files)
  "Returns the normalized per-turn file list."
  (append (when file
            (list file))
          files))

(defun chat-turn-prepared-state (conversation input file files bot
                                 explicit-temperature-specified-p temperature
                                 explicit-top-p-specified-p top-p)
  "Returns the pure prepared state for one chat turn."
  (let* ((effective-files (chat-turn-effective-files file files))
         (file-attachments (and effective-files
                                (prepare-chat-file-attachments effective-files)))
         (effective-generation-config
           (apply #'resolve-effective-generation-config
                  bot
                  (append (when explicit-temperature-specified-p
                            (list :temperature temperature))
                          (when explicit-top-p-specified-p
                            (list :top-p top-p)))))
         (turn-conversation (clone-conversation conversation)))
    (multiple-value-bind (effective-input effective-model)
        (resolve-prompt-model-override bot input)
      (list :turn-conversation turn-conversation
           :effective-input effective-input
            :effective-model effective-model
            :file-attachments file-attachments
            :effective-generation-config effective-generation-config))))

(defun chat-turn (input &key conversation callback file files
                       (temperature nil temperature-specified-p)
                       (top-p nil top-p-specified-p)
                       ((:temperature-specified-p explicit-temperature-specified-p) temperature-specified-p)
                       ((:top-p-specified-p explicit-top-p-specified-p) top-p-specified-p)
                       context)
  "Runs one chat turn and returns a normalized turn result without mutating the source conversation."
  (let* ((conversation (resolve-chat-conversation conversation context :input input))
         (bot (conversation-chatbot conversation))
         (prepared-state (chat-turn-prepared-state conversation
                                                  input
                                                  file
                                                  files
                                                  bot
                                                  explicit-temperature-specified-p
                                                  temperature
                                                  explicit-top-p-specified-p
                                                  top-p)))
    (let* ((provider-result
             (dispatch-chat-turn (getf prepared-state :turn-conversation)
                                 (getf prepared-state :effective-input)
                                 callback
                                 :file-attachments (getf prepared-state :file-attachments)
                                 :effective-model (getf prepared-state :effective-model)
                                 :effective-generation-config (getf prepared-state :effective-generation-config)))
           (provider-conversation (chat-turn-result-conversation provider-result)))
      (annotate-chat-turn-result provider-result
                                conversation
                                :provider-conversation provider-conversation))))

(defun chat (input &key conversation callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Sends user input to the active conversation using the appropriate backend API.
If a callback is provided, each text token is passed to it in real-time.
Returns the complete response text."
  (let ((context (resolve-chat-entry-context conversation)))
    (execute-chat-entry-shell
     conversation
     context
     (lambda (active-conversation active-context)
       (finalize-chat-turn-result
        (chat-turn input
                   :conversation active-conversation
                   :callback callback
                   :file file
                   :files files
                   :temperature temperature
                   :temperature-specified-p temperaturep
                   :top-p top-p
                   :top-p-specified-p top-pp
                   :context active-context))))))
