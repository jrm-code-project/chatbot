;;; -*- Lisp -*-
;;; gemini-payloads.lisp - Gemini Interactions payload translation

(in-package "CHATBOT")

(defun interaction-live-user-input-parts (chatbot input file-attachments &key effective-model)
  "Builds the Interactions API content parts for the current live user turn."
  (let ((parts nil)
        (decorated-input (decorate-live-user-input chatbot input
                                                  :effective-model effective-model)))
    (when (and (stringp decorated-input)
               (string/= decorated-input ""))
      (push `(("type" . "text") ("text" . ,decorated-input)) parts))
    (dolist (attachment file-attachments)
      (push (make-interaction-file-part attachment) parts))
    (nreverse parts)))

(defun interaction-live-user-input-value (chatbot input file-attachments &key effective-model)
  "Builds the Interactions API input value for the current live user turn."
  (if file-attachments
      (coerce (interaction-live-user-input-parts chatbot
                                                 input
                                                 file-attachments
                                                 :effective-model effective-model)
              'vector)
      (decorate-live-user-input chatbot input :effective-model effective-model)))

(defun interaction-request-tools (chatbot)
  "Builds the Gemini Interactions tool list for CHATBOT, including built-in and MCP tools."
  (let ((tools nil))
    (when (chatbot-google-search-p chatbot)
      (push '(("type" . "google_search")) tools))
    (when (chatbot-code-execution-p chatbot)
      (push '(("type" . "code_execution")) tools))
    (let ((chatbot-tools (get-all-chatbot-tools chatbot)))
      (when chatbot-tools
        (dolist (pair chatbot-tools)
          (let* ((mcp-tool (cdr pair))
                 (name (mcp-val :name mcp-tool))
                 (description (mcp-val :description mcp-tool))
                 (input-schema (mcp-val :input-schema mcp-tool)))
            (push `(("type" . "function")
                    ("name" . ,name)
                    ("description" . ,(or description ""))
                    ("parameters" . ,(gemini-tool-parameters input-schema)))
                  tools)))))
    (nreverse tools)))

(defun conversation-message->interaction-step (message)
  "Converts a stored conversation message to an Interactions API step."
  (let ((role (cdr (assoc "role" message :test #'string=)))
        (content (cdr (assoc "content" message :test #'string=))))
    (when (and role content)
      `(("type" . ,(if (assistant-like-role-p role) "model_output" "user_input"))
        ("content" . ,(vector `(("type" . "text") ("text" . ,content))))))))

(defun build-initial-interaction-input (messages input &key chatbot persona-memory persona-diary-entries file-attachments effective-model)
  "Builds the first-turn Interactions API input with any preloaded history."
  (let* ((history-messages (append (persona-memory-messages persona-memory)
                                   (persona-diary-messages persona-diary-entries)))
         (request-input (interaction-live-user-input-parts chatbot
                                                          input
                                                          file-attachments
                                                          :effective-model effective-model)))
    (if (and (null (append history-messages messages))
             (null file-attachments)
             (stringp input))
        (decorate-live-user-input chatbot input :effective-model effective-model)
        (if (or (append history-messages messages) request-input)
            (coerce
             (append
              (remove nil (mapcar #'conversation-message->interaction-step
                                  (append history-messages messages)))
              (when request-input
                (list `(("type" . "user_input")
                        ("content" . ,(coerce request-input 'vector))))))
             'vector)
            (decorate-live-user-input chatbot input :effective-model effective-model)))))

(defun make-interaction-payload (chatbot input &key previous-interaction-id (stream t) messages persona-memory persona-diary-entries file-attachments effective-model)
  "Creates a JSON-serializable alist payload for the Gemini Interactions API."
  (let ((payload (list (cons "model" (or effective-model
                                        (chatbot-model chatbot)))
                      (cons "input" (if previous-interaction-id
                                         (if (and file-attachments
                                                  (stringp input))
                                             (interaction-live-user-input-value chatbot
                                                                                input
                                                                                file-attachments
                                                                                :effective-model effective-model)
                                             (decorate-live-user-input chatbot input :effective-model effective-model))
                                         (build-initial-interaction-input messages
                                                                          input
                                                                          :chatbot chatbot
                                                                          :persona-memory persona-memory
                                                                          :persona-diary-entries persona-diary-entries
                                                                          :file-attachments file-attachments
                                                                          :effective-model effective-model)))
                       (cons "stream" (if stream t :false))
                       (cons "store" t))))
    (when previous-interaction-id
      (push (cons "previous_interaction_id" previous-interaction-id) payload))
    (when (chatbot-system-instruction chatbot)
      (push (cons "system_instruction" (chatbot-system-instruction chatbot)) payload))
    (let ((tools (interaction-request-tools chatbot)))
      (when tools
        (push (cons "tools" tools) payload)))
    (nreverse payload)))
