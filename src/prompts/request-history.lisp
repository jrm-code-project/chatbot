;;; -*- Lisp -*-
;;; request-history.lisp - synthetic preload and stateless history helpers

(in-package "CHATBOT")

(defparameter +max-chatbot-tool-recursion-depth+ 64
  "Maximum number of recursive tool-calling backend continuations allowed in one turn.")

(define-condition chatbot-tool-recursion-limit-error (error)
  ((backend :initarg :backend :reader chatbot-tool-recursion-limit-error-backend)
   (depth :initarg :depth :reader chatbot-tool-recursion-limit-error-depth)
   (max-depth :initarg :max-depth :reader chatbot-tool-recursion-limit-error-max-depth))
  (:report (lambda (condition stream)
             (format stream
                     "Tool recursion depth limit exceeded for backend ~A at depth ~D (max ~D)."
                     (chatbot-tool-recursion-limit-error-backend condition)
                     (chatbot-tool-recursion-limit-error-depth condition)
                     (chatbot-tool-recursion-limit-error-max-depth condition)))))

(defun ensure-chatbot-tool-recursion-depth (backend recursion-depth
                                            &optional (max-depth +max-chatbot-tool-recursion-depth+))
  "Signals a clean condition when RECURSION-DEPTH has reached MAX-DEPTH."
  (when (>= recursion-depth max-depth)
    (cerror "Continue despite the tool recursion depth limit."
            'chatbot-tool-recursion-limit-error
            :backend backend
            :depth recursion-depth
            :max-depth max-depth))
  recursion-depth)

(defun next-chatbot-tool-recursion-depth (backend recursion-depth
                                          &optional (max-depth +max-chatbot-tool-recursion-depth+))
  "Returns the next recursion depth after first enforcing MAX-DEPTH."
  (1+ (ensure-chatbot-tool-recursion-depth backend recursion-depth max-depth)))

(defun append-user-input-to-conversation-messages (messages input)
  "Returns MESSAGES with the current user INPUT appended when present."
  (if input
      (append messages (list (list (cons "role" "user")
                                   (cons "content" input))))
      messages))

(defun stateless-history-messages (current-messages input)
  "Returns the stored stateless backend history for the current turn."
  (append-user-input-to-conversation-messages current-messages input))

(defun extend-stateless-history (history-messages &rest messages)
  "Appends MESSAGES to HISTORY-MESSAGES for a stateless backend turn."
  (append history-messages messages))

(defun update-conversation-stateless-history (history-messages &rest messages)
  "Returns the stateless backend recursive history after appending MESSAGES."
  (apply #'extend-stateless-history history-messages messages))

(defun continue-stateless-tool-recursion (&rest args)
  "Threads recursion messages into stateless history and preserves the legacy mutating form.

Accepted signatures:
  (HISTORY-MESSAGES RECURSION-MESSAGES CONTINUATION)
  (CONVERSATION HISTORY-MESSAGES RECURSION-MESSAGES CONTINUATION)"
  (destructuring-bind (conversation history-messages recursion-messages continuation)
      (if (= (length args) 3)
          (list nil (first args) (second args) (third args))
          args)
    (let ((updated-history (apply #'update-conversation-stateless-history
                                  history-messages
                                  recursion-messages)))
      (when conversation
        (setf (conversation-messages conversation) updated-history))
      (funcall continuation updated-history recursion-messages))))

(defun make-chat-turn-result (text &key messages interaction-id usage thought-text conversation)
  "Returns the normalized result of one chat turn."
  (list :text (sanitize-chat-response-text text)
        :messages messages
        :interaction-id interaction-id
        :usage usage
        :thought-text thought-text
        :conversation conversation))

(defun chat-turn-result-text (result)
  "Returns RESULT's final response text."
  (getf result :text))

(defun chat-turn-result-messages (result)
  "Returns RESULT's updated conversation history."
  (getf result :messages))

(defun chat-turn-result-interaction-id (result)
  "Returns RESULT's updated Gemini interaction id."
  (getf result :interaction-id))

(defun chat-turn-result-usage (result)
  "Returns RESULT's usage metadata."
  (getf result :usage))

(defun chat-turn-result-thought-text (result)
  "Returns RESULT's thought text."
  (getf result :thought-text))

(defun chat-turn-result-conversation (result)
  "Returns RESULT's target conversation."
  (getf result :conversation))

(defun chat-turn-result-provider-conversation (result)
  "Returns RESULT's backend-owned working conversation, when present."
  (getf result :provider-conversation))

(defun sync-chat-turn-provider-state (source target)
  "Copies provider-owned state from SOURCE to TARGET when both conversations exist."
  (when (and source target)
    (setf (conversation-cached-content-name target)
          (conversation-cached-content-name source))
    (setf (conversation-cached-content-key target)
          (conversation-cached-content-key source))
    (setf (conversation-cached-content-metadata target)
          (conversation-cached-content-metadata source))
    (setf (conversation-turns-since-cache-reload target)
          (conversation-turns-since-cache-reload source))))

(defun apply-chat-turn-result (result &optional conversation)
  "Applies RESULT to CONVERSATION, defaulting to RESULT's target conversation."
  (let ((target (or conversation
                    (chat-turn-result-conversation result))))
    (when target
      (sync-chat-turn-provider-state
       (chat-turn-result-provider-conversation result)
       target)
      (setf (conversation-messages target)
            (chat-turn-result-messages result))
      (setf (conversation-interaction-id target)
            (chat-turn-result-interaction-id result))
      (when (conversation-cached-content-name target)
        (incf (conversation-turns-since-cache-reload target))))
    (chat-turn-result-text result)))

(defun emit-chat-response-text (text &key callback usage thought-text)
  "Formats TEXT for display, writes token USAGE when present, optionally calls CALLBACK, and returns TEXT."
  (let ((sanitized-text (sanitize-chat-response-text text)))
    (write-line "---" *standard-output*)
    (format-paragraphs sanitized-text :width 80)
    (write-line "---" *standard-output*)
    (when usage
      (write-turn-token-summary usage :thought-text thought-text))
    (when callback
      (funcall callback sanitized-text))
    sanitized-text))

(defun finish-stateless-text-turn (&rest args)
  "Emits final text and returns a normalized turn result.

Accepted signatures:
  (HISTORY-MESSAGES ROLE TEXT &key CALLBACK USAGE THOUGHT-TEXT INTERACTION-ID)
  (CONVERSATION HISTORY-MESSAGES ROLE TEXT &key CALLBACK USAGE THOUGHT-TEXT INTERACTION-ID)"
  (let* ((legacy-call-p (and (>= (length args) 4)
                             (typep (first args) 'conversation)))
         (conversation (and legacy-call-p (first args)))
         (history-messages (if legacy-call-p (second args) (first args)))
         (role (if legacy-call-p (third args) (second args)))
         (text (if legacy-call-p (fourth args) (third args)))
         (options (if legacy-call-p (nthcdr 4 args) (nthcdr 3 args)))
         (callback (getf options :callback))
         (usage (getf options :usage))
         (thought-text (getf options :thought-text))
         (sanitized-text (sanitize-chat-response-text text))
         (interaction-id (or (getf options :interaction-id)
                             (and conversation
                                  (conversation-interaction-id conversation))))
         (result
           (progn
             (emit-chat-response-text sanitized-text :callback callback :usage usage :thought-text thought-text)
             (make-chat-turn-result
              sanitized-text
              :messages
              (update-conversation-stateless-history
               history-messages
               (list (cons "role" role)
                     (cons "content" sanitized-text)))
              :interaction-id interaction-id
              :usage usage
              :thought-text thought-text
              :conversation conversation))))
    (when conversation
      (return-from finish-stateless-text-turn
        (apply-chat-turn-result result conversation)))
    result))

(defun persona-memory-messages (persona-memory)
  "Returns provider-neutral synthetic history representing PERSONA-MEMORY."
  (when persona-memory
    (list (list (cons "role" "user")
                (cons "content" "Please concisely summarize your knowledge graph."))
          (list (cons "role" "model")
                (cons "content" persona-memory)))))

(defun persona-diary-entry-message-text (entry)
  "Formats a persona diary ENTRY as model-visible preload text."
  (let ((filename (cdr (assoc :filename entry)))
        (content (cdr (assoc :content entry))))
    (if filename
        (format nil "[Diary: ~A]~%~A" filename content)
        content)))

(defun persona-diary-messages (entries)
  "Returns provider-neutral synthetic history representing ordered diary ENTRIES.
Uses only the most recent (last) 8 diary entries."
  (when entries
    (let ((recent-entries (last entries 8)))
      (mapcar (lambda (entry)
                (list (cons "role" "model")
                      (cons "content" (persona-diary-entry-message-text entry))))
              recent-entries))))

(defun build-request-history-messages (messages input &key chatbot persona-memory persona-diary-entries effective-model)
  "Builds request history by prepending persona preload ahead of ordinary MESSAGES and INPUT."
  (append ;(persona-memory-messages persona-memory)
          (persona-diary-messages persona-diary-entries)
          (append-user-input-to-conversation-messages
           messages
           (decorate-live-user-input chatbot input :effective-model effective-model))))
