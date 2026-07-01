;;; -*- Lisp -*-
;;; gemini-payloads.lisp - Gemini Interactions payload translation

(in-package "CHATBOT")

(defun interaction-live-user-input-parts (chatbot input file-attachments &key effective-model)
  "Builds the Interactions API content parts for the current live user turn."
  (let* ((decorated-input (decorate-live-user-input chatbot input
                                                    :effective-model effective-model))
         (text-parts (when (and (stringp decorated-input)
                                (string/= decorated-input ""))
                       (list `(("type" . "text") ("text" . ,decorated-input)))))
         (attachment-parts (mapcar #'make-interaction-file-part file-attachments)))
    (append text-parts attachment-parts)))

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
  (append (when (chatbot-google-search-p chatbot)
            (list '(("type" . "google_search"))))
          (when (chatbot-code-execution-p chatbot)
            (list '(("type" . "code_execution"))))
          (mapcar (lambda (pair)
                    (let* ((mcp-tool (cdr pair))
                           (name (mcp-val :name mcp-tool))
                           (description (mcp-val :description mcp-tool))
                           (input-schema (mcp-val :input-schema mcp-tool)))
                      `(("type" . "function")
                        ("name" . ,name)
                        ("description" . ,(or description ""))
                        ("parameters" . ,(gemini-tool-parameters input-schema)))))
                  (or (get-all-chatbot-tools chatbot) nil))))

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

(defun make-interaction-payload (chatbot input &key previous-interaction-id (stream t) messages persona-memory persona-diary-entries file-attachments effective-model effective-generation-config)
  "Creates a JSON-serializable alist payload for the Gemini Interactions API."
  (let* ((model (or effective-model
                    (chatbot-model chatbot)))
         (request-input
           (if previous-interaction-id
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
         (generation-config
           (when (or (getf effective-generation-config :temperature)
                     (getf effective-generation-config :top-p))
             (remove nil
                     (list (when (getf effective-generation-config :temperature)
                             (cons "temperature" (getf effective-generation-config :temperature)))
                           (when (getf effective-generation-config :top-p)
                             (cons "top_p" (getf effective-generation-config :top-p)))))))
         (tools (interaction-request-tools chatbot)))
    (append (list (cons "store" t)
                  (cons "stream" (if stream t :false))
                  (cons "input" request-input)
                  (cons "model" model))
            (when previous-interaction-id
              (list (cons "previous_interaction_id" previous-interaction-id)))
            (when (chatbot-system-instruction chatbot)
              (list (cons "system_instruction"
                          (system-instruction-text (chatbot-system-instruction chatbot)))))
            (when generation-config
              (list (cons "generation_config" generation-config)))
            (when tools
              (list (cons "tools" tools))))))
