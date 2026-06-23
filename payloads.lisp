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

(defparameter +prompt-timestamp-month-abbreviations+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun format-prompt-timestamp (universal-time &optional time-zone)
  "Formats UNIVERSAL-TIME as a prompt prefix like [14:29 26-Jun-2026]."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time time-zone)
    (declare (ignore second))
    (format nil "[~2,'0D:~2,'0D ~2,'0D-~A-~4,'0D]"
            hour
            minute
            day
            (svref +prompt-timestamp-month-abbreviations+ (1- month))
            year)))

(defun default-prompt-timestamp-function ()
  "Returns the current local prompt timestamp string."
  (format-prompt-timestamp (get-universal-time)))

(defvar *prompt-timestamp-function* #'default-prompt-timestamp-function
  "Function used to generate the current prompt timestamp string.")

(defun format-prompt-model-indicator (model)
  "Formats MODEL as a prompt prefix like [model: gemini-3-flash]."
  (format nil "[model: ~A]" model))

(defun decorate-live-user-input (chatbot input)
  "Decorates string INPUT with transient prompt prefixes requested by CHATBOT."
  (if (and chatbot
           (stringp input))
      (let ((parts nil))
        (when (chatbot-include-timestamp-p chatbot)
          (push (funcall *prompt-timestamp-function*) parts))
        (when (chatbot-include-model-p chatbot)
          (push (format-prompt-model-indicator (chatbot-model chatbot)) parts))
        (if parts
            (format nil "~{~A~^ ~} ~A" (nreverse parts) input)
            input))
      input))

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

(defun build-request-history-messages (messages input &key chatbot persona-memory persona-diary-entries)
  "Builds request history by prepending persona preload ahead of ordinary MESSAGES and INPUT."
  (append (persona-memory-messages persona-memory)
          (persona-diary-messages persona-diary-entries)
          (append-user-input-to-conversation-messages
           messages
           (decorate-live-user-input chatbot input))))

(defun build-openai-request-messages (system-inst messages input &key chatbot persona-memory persona-diary-entries)
  "Builds the OpenAI chat-completions message list for the current turn."
  (let ((history (mapcar (lambda (message)
                          (let ((role (cdr (assoc "role" message :test #'string=))))
                            (if role
                                 (acons "role"
                                        (openai-role-for-message role)
                                        (remove "role" message :key #'car :test #'string=))
                                 message)))
                         (build-request-history-messages messages
                                                         input
                                                         :chatbot chatbot
                                                         :persona-memory persona-memory
                                                         :persona-diary-entries persona-diary-entries))))
    (if system-inst
        (append (list (list (cons "role" "system")
                           (cons "content" system-inst)))
                history)
        history)))

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

(defun openai-request-tools (chatbot)
  "Builds the OpenAI-compatible tool list for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
    (when mcp-tools
      (mapcar (lambda (pair)
               (translate-mcp-tool-to-openai (cdr pair)))
             mcp-tools))))

(defun generate-content-request-tools (chatbot)
  "Builds the Google generateContent tools payload for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
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

(defun build-initial-interaction-input (messages input &key chatbot persona-memory persona-diary-entries)
  "Builds the first-turn Interactions API input with any preloaded history."
  (let* ((history-messages (append (persona-memory-messages persona-memory)
                                  (persona-diary-messages persona-diary-entries)))
        (request-input (decorate-live-user-input chatbot input)))
    (if (and (append history-messages messages) (stringp request-input))
      (coerce
      (append
       (remove nil (mapcar #'conversation-message->interaction-step
                           (append history-messages messages)))
       (list `(("type" . "user_input")
               ("content" . ,(vector `(("type" . "text") ("text" . ,request-input)))))))
      'vector)
      request-input)))

(defun make-interaction-payload (chatbot input &key previous-interaction-id (stream t) messages persona-memory persona-diary-entries)
  "Creates a JSON-serializable alist payload for the Gemini Interactions API."
  (let ((payload (list (cons "model" (chatbot-model chatbot))
                      (cons "input" (if previous-interaction-id
                                        (decorate-live-user-input chatbot input)
                                        (build-initial-interaction-input messages
                                                                         input
                                                                         :chatbot chatbot
                                                                         :persona-memory persona-memory
                                                                         :persona-diary-entries persona-diary-entries)))
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
