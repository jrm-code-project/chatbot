;;; -*- Lisp -*-
;;; openai-payloads.lisp - OpenAI-compatible payload translation

(in-package "CHATBOT")

(defun openai-role-for-message (role)
  "Normalizes a stored conversation ROLE for OpenAI-compatible chat completions."
  (if (and role (string= (string-downcase role) "model")) "assistant" role))

(defun openai-live-user-message (chatbot input file-attachments)
  "Builds the transient current user message for OpenAI-compatible requests."
  (let ((decorated-input (decorate-live-user-input chatbot input)))
    (cond
      (file-attachments
       (let ((content nil))
         (when (and (stringp decorated-input)
                    (string/= decorated-input ""))
           (push `(("type" . "text") ("text" . ,decorated-input)) content))
         (dolist (attachment file-attachments)
           (setf content (append content (openai-file-content-parts attachment))))
         (when content
           (list (cons "role" "user")
                 (cons "content" content)))))
      ((and (stringp decorated-input)
            (string/= decorated-input ""))
       (list (cons "role" "user")
             (cons "content" decorated-input)))
      (t nil))))

(defun build-openai-request-messages (system-inst messages input &key chatbot persona-memory persona-diary-entries file-attachments)
  "Builds the OpenAI chat-completions message list for the current turn."
  (let ((history (mapcar (lambda (message)
                           (let ((role (cdr (assoc "role" message :test #'string=))))
                             (if role
                                 (acons "role"
                                        (openai-role-for-message role)
                                        (remove "role" message :key #'car :test #'string=))
                                 message)))
                         (append (persona-memory-messages persona-memory)
                                 (persona-diary-messages persona-diary-entries)
                                 messages)))
        (current-user-message (openai-live-user-message chatbot input file-attachments)))
    (if system-inst
        (append (list (list (cons "role" "system")
                            (cons "content" system-inst)))
                history
                (when current-user-message
                  (list current-user-message)))
        (append history
                (when current-user-message
                  (list current-user-message))))))

(defun openai-request-tools (chatbot)
  "Builds the OpenAI-compatible tool list for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
    (when mcp-tools
      (mapcar (lambda (pair)
                (translate-mcp-tool-to-openai (cdr pair)))
              mcp-tools))))
