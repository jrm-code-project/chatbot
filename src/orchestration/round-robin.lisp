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

(defun clone-conversation-for-round-robin (conversation)
  "Returns a round-robin-local clone of CONVERSATION."
  (clone-conversation conversation
                      :chatbot (clone-chatbot
                                (conversation-chatbot conversation))
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
  (reduce (lambda (names participant)
            (unless (typep participant 'round-robin-participant)
              (error "Round-robin participants must be created with MAKE-ROUND-ROBIN-PARTICIPANT."))
            (let ((name (round-robin-participant-name participant))
                  (conversation (round-robin-participant-conversation participant)))
              (validate-round-robin-source-conversation conversation name)
              (when (member name names :test #'string=)
                (error "Round-robin participant names must be unique: ~A" name))
              (append names (list name))))
          participants
          :initial-value nil)
  nil)

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

(defun valid-round-robin-session-phase-p (phase)
  "Returns true when PHASE is one of the explicit round-robin FSM states."
  (member phase '(:awaiting-user-input
                  :participant-ready
                  :participant-running
                  :participant-complete
                  :turn-complete)))

(defun make-round-robin-session-state (&key phase next-participant-index)
  "Returns one explicit round-robin orchestration state plist."
  (list :phase phase
        :next-participant-index next-participant-index))

(defun round-robin-session-state (session)
  "Returns SESSION's current orchestration state as a pure plist."
  (make-round-robin-session-state
   :phase (round-robin-session-phase session)
   :next-participant-index (round-robin-session-next-participant-index session)))

(defun apply-round-robin-session-state (session state)
  "Applies STATE to SESSION and returns SESSION."
  (setf (round-robin-session-phase session) (getf state :phase))
  (setf (round-robin-session-next-participant-index session)
        (getf state :next-participant-index))
  session)

(defun validate-round-robin-session-state (state participant-count)
  "Ensures STATE is valid for a round-robin session with PARTICIPANT-COUNT participants."
  (let ((phase (getf state :phase))
        (next-participant-index (getf state :next-participant-index)))
    (unless (valid-round-robin-session-phase-p phase)
      (error "Round-robin session is in an invalid phase: ~A" phase))
    (unless (and (integerp next-participant-index)
                 (<= 0 next-participant-index))
      (error "Round-robin session has an invalid next participant index: ~A"
             next-participant-index))
    (when (and (> participant-count 0)
               (member phase '(:participant-ready :participant-running :participant-complete))
               (>= next-participant-index participant-count))
      (error "Round-robin phase ~A cannot reference participant index ~A with only ~A participants."
             phase
             next-participant-index
             participant-count))
    state))

(defun advance-round-robin-session-state (state event participant-count)
  "Returns the next pure round-robin state implied by EVENT."
  (validate-round-robin-session-state state participant-count)
  (let ((phase (getf state :phase))
        (next-participant-index (getf state :next-participant-index)))
    (case event
      (:user-entry-recorded
       (unless (eq phase :awaiting-user-input)
         (error "Cannot record round-robin user input while session phase is ~A." phase))
       (make-round-robin-session-state :phase :participant-ready
                                      :next-participant-index 0))
      (:participant-response-recorded
       (unless (eq phase :participant-running)
         (error "Cannot record round-robin participant output while session phase is ~A." phase))
       (make-round-robin-session-state :phase :participant-complete
                                      :next-participant-index next-participant-index))
      (t
       (error "Unsupported round-robin state event: ~A" event)))))

(defun plan-round-robin-session-step (state participant-count)
  "Returns the next orchestration step for STATE as a pure plan plist."
  (validate-round-robin-session-state state participant-count)
  (let ((phase (getf state :phase))
        (next-participant-index (getf state :next-participant-index)))
    (case phase
      (:awaiting-user-input
       (list :kind :await-user-input
             :next-state state))
      (:participant-ready
       (list :kind :run-participant
             :participant-index next-participant-index
             :next-state (make-round-robin-session-state
                          :phase :participant-running
                          :next-participant-index next-participant-index)))
      (:participant-running
       (error "Round-robin participant ~D is already running." next-participant-index))
      (:participant-complete
       (if (< (1+ next-participant-index) participant-count)
           (list :kind :advance
                 :next-state (make-round-robin-session-state
                              :phase :participant-ready
                              :next-participant-index (1+ next-participant-index)))
           (list :kind :advance
                 :next-state (make-round-robin-session-state
                              :phase :turn-complete
                              :next-participant-index 0))))
      (:turn-complete
       (list :kind :finish-turn
             :next-state (make-round-robin-session-state
                          :phase :awaiting-user-input
                          :next-participant-index 0)))
      (t
       (error "Unsupported round-robin session phase: ~A" phase)))))

(defun run-round-robin-participant-turn (session participant latest-entry history-transcript callback
                                         file-attachments effective-generation-config)
  "Runs one participant turn using the current transcript context."
  (let* ((conversation (round-robin-participant-conversation participant))
         (bot (conversation-chatbot conversation)))
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
                 (let ((result
                        (dispatch-chat-turn conversation
                                           live-input
                                           callback
                                           :file-attachments (and (eq (round-robin-entry-speaker-kind latest-entry) :user)
                                                                  file-attachments)
                                           :effective-model effective-model
                                           :effective-generation-config effective-generation-config)))
                   (apply-chat-turn-result result conversation))))))
        (terpri)
        (terpri)
        (append-round-robin-transcript-entry session
                                           (round-robin-participant-name participant)
                                           :bot
                                           response)
        (list :name (round-robin-participant-name participant)
              :response response)))))

(defun round-robin-chat (input &key session callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Runs one full round-robin turn for INPUT across SESSION participants."
  (unless (typep session 'round-robin-session)
    (error "ROUND-ROBIN-CHAT requires a :SESSION created by NEW-ROUND-ROBIN-CHAT."))
  (let* ((participants (round-robin-session-participants session))
         (participant-count (length participants))
         (effective-files (append (when file
                                   (list file))
                                  files))
         (file-attachments (and effective-files
                                (prepare-chat-file-attachments effective-files))))
    (validate-round-robin-session-state (round-robin-session-state session)
                                       participant-count)
    (print-chat-speaker-block (round-robin-session-user-name session) input)
    (append-round-robin-transcript-entry session
                                       (round-robin-session-user-name session)
                                       :user
                                       input)
    (apply-round-robin-session-state
     session
     (advance-round-robin-session-state (round-robin-session-state session)
                                       :user-entry-recorded
                                       participant-count))
    (let ((results nil))
      (loop
        for state = (round-robin-session-state session)
        for plan = (plan-round-robin-session-step state participant-count)
        do (case (getf plan :kind)
             (:await-user-input
              (return (nreverse results)))
             (:advance
              (apply-round-robin-session-state session (getf plan :next-state)))
             (:finish-turn
              (apply-round-robin-session-state session (getf plan :next-state))
              (print-round-robin-user-turn-marker)
              (return (nreverse results)))
             (:run-participant
              (let* ((participant-index (getf plan :participant-index))
                     (participant (nth participant-index participants)))
                (apply-round-robin-session-state session (getf plan :next-state))
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
                    (push (run-round-robin-participant-turn session
                                                           participant
                                                           latest-entry
                                                           history-transcript
                                                           callback
                                                           file-attachments
                                                           effective-generation-config)
                          results)
                    (apply-round-robin-session-state
                     session
                     (advance-round-robin-session-state (round-robin-session-state session)
                                                       :participant-response-recorded
                                                       participant-count))))))
             (t
              (error "Unsupported round-robin session plan kind: ~A" (getf plan :kind))))))))
