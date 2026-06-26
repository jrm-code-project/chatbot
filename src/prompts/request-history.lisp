;;; -*- Lisp -*-
;;; request-history.lisp - synthetic preload and stateless history helpers

(in-package "CHATBOT")

(defparameter +max-chatbot-tool-recursion-depth+ 16
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
    (error 'chatbot-tool-recursion-limit-error
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

(defun update-conversation-stateless-history (conversation history-messages &rest messages)
  "Stores the stateless backend recursive history on CONVERSATION and returns it."
  (let ((updated-history (apply #'extend-stateless-history history-messages messages)))
    (setf (conversation-messages conversation) updated-history)
    updated-history))

(defun continue-stateless-tool-recursion (conversation history-messages recursion-messages continuation)
  "Stores RECURSION-MESSAGES on CONVERSATION history, then calls CONTINUATION."
  (let ((updated-history (apply #'update-conversation-stateless-history
                                conversation
                                history-messages
                                recursion-messages)))
    (funcall continuation updated-history recursion-messages)))

(defun emit-chat-response-text (text &key callback usage)
  "Formats TEXT for display, writes token USAGE when present, optionally calls CALLBACK, and returns TEXT."
  (format-paragraphs text :width 80)
  (when usage
    (write-turn-token-summary usage))
  (when callback
    (funcall callback text))
  text)

(defun finish-stateless-text-turn (conversation history-messages role text &key callback usage)
  "Emits final TEXT for a stateless backend turn, persists it on CONVERSATION, and returns TEXT."
  (emit-chat-response-text text :callback callback :usage usage)
  (update-conversation-stateless-history
   conversation
   history-messages
   (list (cons "role" role)
         (cons "content" text)))
  text)

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
  "Returns provider-neutral synthetic history representing ordered diary ENTRIES."
  (when entries
    (mapcar (lambda (entry)
              (list (cons "role" "model")
                    (cons "content" (persona-diary-entry-message-text entry))))
            entries)))

(defun build-request-history-messages (messages input &key chatbot persona-memory persona-diary-entries effective-model)
  "Builds request history by prepending persona preload ahead of ordinary MESSAGES and INPUT."
  (append (persona-memory-messages persona-memory)
          (persona-diary-messages persona-diary-entries)
          (append-user-input-to-conversation-messages
           messages
           (decorate-live-user-input chatbot input :effective-model effective-model))))
