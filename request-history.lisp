;;; -*- Lisp -*-
;;; request-history.lisp - synthetic preload and stateless history helpers

(in-package "CHATBOT")

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
