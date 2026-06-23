;;; -*- Lisp -*-
;;; payloads.lisp - role normalization and provider payload builders

(in-package "CHATBOT")

(defun assistant-like-role-p (role)
  "Returns true when ROLE is an assistant/model response role."
  (and role
       (member (string-downcase role) '("assistant" "model") :test #'string=)))

(defun generate-content-role-for-message (role)
  "Normalizes a stored conversation ROLE for Google generateContent."
  (if (assistant-like-role-p role) "model" "user"))

(defun openai-role-for-message (role)
  "Normalizes a stored conversation ROLE for OpenAI-compatible chat completions."
  (if (and role (string= (string-downcase role) "model")) "assistant" role))

(defun append-user-input-to-conversation-messages (messages input)
  "Returns MESSAGES with the current user INPUT appended when present."
  (if input
      (append messages (list (list (cons "role" "user")
                                   (cons "content" input))))
      messages))

(defun persona-memory-messages (persona-memory)
  "Returns provider-neutral synthetic history representing PERSONA-MEMORY."
  (when persona-memory
    (list (list (cons "role" "user")
                (cons "content" "Please concisely summarize your knowledge graph."))
          (list (cons "role" "model")
                (cons "content" persona-memory)))))

(defun build-request-history-messages (messages input &key persona-memory)
  "Builds request history by prepending PERSONA-MEMORY ahead of ordinary MESSAGES and INPUT."
  (append (persona-memory-messages persona-memory)
          (append-user-input-to-conversation-messages messages input)))

(defun build-openai-request-messages (system-inst messages input &key persona-memory)
  "Builds the OpenAI chat-completions message list for the current turn."
  (let ((history (mapcar (lambda (message)
                          (let ((role (cdr (assoc "role" message :test #'string=))))
                            (if role
                                 (acons "role"
                                        (openai-role-for-message role)
                                        (remove "role" message :key #'car :test #'string=))
                                 message)))
                         (build-request-history-messages messages input :persona-memory persona-memory))))
    (if system-inst
        (append (list (list (cons "role" "system")
                           (cons "content" system-inst)))
                history)
        history)))

(defun interaction-request-tools (chatbot)
  "Builds the Gemini Interactions tool list for CHATBOT, including MCP tools."
  (let ((tools nil))
    (when (chatbot-google-search-p chatbot)
      (push '(("type" . "google_search")) tools))
    (when (chatbot-code-execution-p chatbot)
      (push '(("type" . "code_execution")) tools))
    (let ((mcp-tools (get-all-mcp-tools chatbot)))
      (when mcp-tools
        (dolist (pair mcp-tools)
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

(defun openai-request-tools (chatbot)
  "Builds the OpenAI-compatible tool list for CHATBOT from MCP tools."
  (let ((mcp-tools (get-all-mcp-tools chatbot)))
    (when mcp-tools
      (mapcar (lambda (pair)
               (translate-mcp-tool-to-openai (cdr pair)))
             mcp-tools))))

(defun generate-content-request-tools (chatbot)
  "Builds the Google generateContent tools payload for CHATBOT from MCP tools."
  (let ((mcp-tools (get-all-mcp-tools chatbot)))
    (when mcp-tools
      (list `(("functionDeclarations" . ,(coerce
                                         (mapcar (lambda (pair)
                                                   (translate-mcp-tool-to-gemini-fn (cdr pair)))
                                                 mcp-tools)
                                         'vector)))))))

(defun conversation-message->interaction-step (message)
  "Converts a stored conversation message to an Interactions API step."
  (let ((role (cdr (assoc "role" message :test #'string=)))
        (content (cdr (assoc "content" message :test #'string=))))
    (when (and role content)
      `(("type" . ,(if (assistant-like-role-p role) "model_output" "user_input"))
        ("content" . ,(vector `(("type" . "text") ("text" . ,content))))))))

(defun build-initial-interaction-input (messages input &key persona-memory)
  "Builds the first-turn Interactions API input with any preloaded history."
  (let ((history-messages (persona-memory-messages persona-memory)))
    (if (and (append history-messages messages) (stringp input))
      (coerce
       (append
        (remove nil (mapcar #'conversation-message->interaction-step
                            (append history-messages messages)))
        (list `(("type" . "user_input")
                ("content" . ,(vector `(("type" . "text") ("text" . ,input)))))))
       'vector)
      input)))

(defun make-interaction-payload (chatbot input &key previous-interaction-id (stream t) messages persona-memory)
  "Creates a JSON-serializable alist payload for the Gemini Interactions API."
  (let ((payload (list (cons "model" (chatbot-model chatbot))
                      (cons "input" (if previous-interaction-id
                                        input
                                        (build-initial-interaction-input messages input :persona-memory persona-memory)))
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
