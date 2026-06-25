;;; -*- Lisp -*-
;;; round-robin.lisp - multi-chat round-robin orchestration

(in-package "CHATBOT")

(defun round-robin-transcript-entry (speaker-name speaker-kind content)
  "Returns one shared round-robin transcript entry."
  (list :speaker-name speaker-name
        :speaker-kind speaker-kind
        :content content))

(defun round-robin-entry-speaker-name (entry)
  "Returns ENTRY's speaker name."
  (getf entry :speaker-name))

(defun round-robin-entry-speaker-kind (entry)
  "Returns ENTRY's speaker kind."
  (getf entry :speaker-kind))

(defun round-robin-entry-content (entry)
  "Returns ENTRY's raw content."
  (getf entry :content))

(defun labeled-round-robin-entry-content (entry &key content)
  "Returns ENTRY content prefixed with its speaker identity."
  (format nil "~A: ~A"
          (round-robin-entry-speaker-name entry)
          (or content (round-robin-entry-content entry))))

(defun validate-round-robin-participant-name (name)
  "Returns NAME as a non-empty string for round-robin use."
  (let ((name-string (and name (string name))))
    (unless (and name-string
                 (string/= "" (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                           name-string)))
      (error "Round-robin participant names must be non-empty strings."))
    name-string))

(defun validate-round-robin-source-conversation (conversation name)
  "Ensures CONVERSATION is safe to use as a new round-robin participant source."
  (unless (typep conversation 'conversation)
    (error "Round-robin participant ~A must use a CHATBOT conversation." name))
  (when (conversation-messages conversation)
    (error "Round-robin participant ~A must start from a fresh conversation with no stored turn history." name))
  (when (conversation-interaction-id conversation)
    (error "Round-robin participant ~A must start from a fresh conversation with no active Gemini interaction." name))
  conversation)

(defun clone-chatbot-for-round-robin (bot)
  "Returns a shallow clone of BOT suitable for round-robin session-local state."
  (make-instance 'chatbot
                 :model (chatbot-model bot)
                 :backend (chatbot-backend bot)
                 :system-instruction (chatbot-system-instruction bot)
                 :system-instruction-path (chatbot-system-instruction-path bot)
                 :system-instruction-storage-kind (chatbot-system-instruction-storage-kind bot)
                 :temperature (chatbot-temperature bot)
                 :top-p (chatbot-top-p bot)
                 :google-search-p (chatbot-google-search-p bot)
                 :gemini-fallback-to-google-p (chatbot-gemini-fallback-to-google-p bot)
                 :web-tools-p (chatbot-web-tools-p bot)
                 :code-execution-p (chatbot-code-execution-p bot)
                 :include-timestamp-p (chatbot-include-timestamp-p bot)
                 :include-model-p (chatbot-include-model-p bot)
                 :enable-eval-p (chatbot-enable-eval-p bot)
                 :filesystem-tools-p (chatbot-filesystem-tools-p bot)
                 :filesystem-root-directory (chatbot-filesystem-root-directory bot)
                 :filesystem-allowed-directories (chatbot-filesystem-allowed-directories bot)
                 :filesystem-allowlist-path (chatbot-filesystem-allowlist-path bot)
                 :mcp-servers (chatbot-mcp-servers bot)
                 :mcp-startup-status (chatbot-mcp-startup-status bot)
                 :runtime-context (chatbot-runtime-context bot)))

(defun clone-conversation-for-round-robin (conversation)
  "Returns a round-robin-local clone of CONVERSATION."
  (make-instance 'conversation
                 :chatbot (clone-chatbot-for-round-robin
                           (conversation-chatbot conversation))
                 :persona-memory (conversation-persona-memory conversation)
                 :persona-diary-entries (conversation-persona-diary-entries conversation)
                 :interaction-id nil
                 :messages nil))

(defun make-round-robin-participant (&key name conversation)
  "Returns one named round-robin participant wrapper."
  (make-instance 'round-robin-participant
                 :name (validate-round-robin-participant-name name)
                 :conversation conversation))

(defun validate-round-robin-participants (participants)
  "Ensures PARTICIPANTS form a valid ordered round-robin participant list."
  (unless (and (listp participants)
               (>= (length participants) 2))
    (error "Round-robin chat requires at least two chatbot participants."))
  (let ((names nil))
    (dolist (participant participants)
      (unless (typep participant 'round-robin-participant)
        (error "Round-robin participants must be created with MAKE-ROUND-ROBIN-PARTICIPANT."))
      (let ((name (round-robin-participant-name participant))
            (conversation (round-robin-participant-conversation participant)))
        (validate-round-robin-source-conversation conversation name)
        (when (member name names :test #'string=)
          (error "Round-robin participant names must be unique: ~A" name))
        (push name names)))))

(defun new-round-robin-chat (participants &key (user-name "User"))
  "Creates a new round-robin session over ordered PARTICIPANTS."
  (validate-round-robin-participants participants)
  (make-instance 'round-robin-session
                 :participants
                 (mapcar (lambda (participant)
                           (make-instance 'round-robin-participant
                                          :name (round-robin-participant-name participant)
                                          :conversation
                                          (clone-conversation-for-round-robin
                                           (round-robin-participant-conversation participant))))
                         participants)
                 :user-name (validate-round-robin-participant-name user-name)
                 :transcript nil))

(defun append-round-robin-transcript-entry (session speaker-name speaker-kind content)
  "Appends one ENTRY to SESSION transcript and returns it."
  (let ((entry (round-robin-transcript-entry speaker-name speaker-kind content)))
    (setf (round-robin-session-transcript session)
          (append (round-robin-session-transcript session)
                  (list entry)))
    entry))

(defun build-round-robin-history-messages (transcript participant-name)
  "Returns provider-neutral history for PARTICIPANT-NAME from shared TRANSCRIPT."
  (mapcar (lambda (entry)
            (list (cons "role"
                        (if (and (eq (round-robin-entry-speaker-kind entry) :bot)
                                 (string= participant-name
                                          (round-robin-entry-speaker-name entry)))
                            "assistant"
                            "user"))
                  (cons "content" (labeled-round-robin-entry-content entry))))
          transcript))

(defun split-round-robin-transcript-for-live-turn (transcript)
  "Returns the prefix and latest entry from TRANSCRIPT."
  (unless transcript
    (error "Round-robin transcript is empty."))
  (values (butlast transcript 1)
          (car (last transcript))))

(defun prepare-round-robin-live-input (participant latest-entry)
  "Returns the labeled live input and any effective model override for PARTICIPANT."
  (let* ((conversation (round-robin-participant-conversation participant))
         (bot (conversation-chatbot conversation))
         (content (round-robin-entry-content latest-entry)))
    (if (eq (round-robin-entry-speaker-kind latest-entry) :user)
        (multiple-value-bind (effective-content effective-model)
            (resolve-prompt-model-override bot content)
          (values (labeled-round-robin-entry-content latest-entry :content effective-content)
                  effective-model))
        (values (labeled-round-robin-entry-content latest-entry)
                nil))))

(defun print-round-robin-user-turn-marker ()
  "Prints the prompt marker indicating the user may speak again."
  (write-line "[Your turn]")

  nil)

(defun round-robin-chat (input &key session callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Runs one full round-robin turn for INPUT across SESSION participants."
  (unless (typep session 'round-robin-session)
    (error "ROUND-ROBIN-CHAT requires a :SESSION created by NEW-ROUND-ROBIN-CHAT."))
  (let* ((effective-files (append (when file
                                    (list file))
                                  files))
         (file-attachments (and effective-files
                                (prepare-chat-file-attachments effective-files)))
         (results nil))
    (print-chat-speaker-block (round-robin-session-user-name session) input)
    (append-round-robin-transcript-entry session
                                         (round-robin-session-user-name session)
                                         :user
                                         input)
    (dolist (participant (round-robin-session-participants session))
      (multiple-value-bind (history-transcript latest-entry)
          (split-round-robin-transcript-for-live-turn
           (round-robin-session-transcript session))
        (let* ((conversation (round-robin-participant-conversation participant))
               (bot (conversation-chatbot conversation))
               (effective-generation-config
                 (apply #'resolve-effective-generation-config
                        bot
                        (append (when temperaturep
                                  (list :temperature temperature))
                                (when top-pp
                                  (list :top-p top-p))))))
          (setf (conversation-messages conversation)
                (build-round-robin-history-messages history-transcript
                                                    (round-robin-participant-name participant)))
          (when (eq (chatbot-backend bot) :gemini)
            (setf (conversation-interaction-id conversation) nil))
          (multiple-value-bind (live-input effective-model)
              (prepare-round-robin-live-input participant latest-entry)
            (print-chat-speaker-header (round-robin-participant-name participant))
            (let ((response
                    (call-with-runtime-context
                     (chatbot-runtime-context bot)
                     (lambda ()
                       (dispatch-chat-turn conversation
                                           live-input
                                           callback
                                           :file-attachments (and (eq (round-robin-entry-speaker-kind latest-entry) :user)
                                                                  file-attachments)
                                           :effective-model effective-model
                                           :effective-generation-config effective-generation-config)))))
              (terpri)
              (terpri)
              (append-round-robin-transcript-entry session
                                                   (round-robin-participant-name participant)
                                                   :bot
                                                   response)
              (push (list :name (round-robin-participant-name participant)
                          :response response)
                    results))))))
    (print-round-robin-user-turn-marker)
    (nreverse results)))
