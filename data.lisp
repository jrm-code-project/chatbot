;;;

(in-package "CHATBOT")

(defclass runtime-context ()
  ((mcp-config-path
    :initarg :mcp-config-path
    :accessor runtime-context-mcp-config-path
    :initform nil
    :documentation "Optional override path for MCP configuration.")
   (startup-chatbot
    :initarg :startup-chatbot
    :accessor runtime-context-startup-chatbot
    :initform nil
    :documentation "Shared chatbot instance owning MCP servers for this runtime context.")
   (auto-initialize-startup-mcp-servers-p
    :initarg :auto-initialize-startup-mcp-servers-p
    :accessor runtime-context-auto-initialize-startup-mcp-servers-p
    :initform nil
    :documentation "Whether startup MCP servers should be initialized automatically for this context.")
   (logging-enabled-p
    :initarg :logging-enabled-p
    :accessor runtime-context-logging-enabled-p
    :initform t
    :documentation "Whether logging is enabled for this runtime context.")
   (log-level
    :initarg :log-level
    :accessor runtime-context-log-level
    :initform :info
    :documentation "Minimum log level for this runtime context.")
   (log-stream
    :initarg :log-stream
    :accessor runtime-context-log-stream
    :initform *error-output*
    :documentation "Destination stream for logs in this runtime context.")
   (http-connect-timeout
    :initarg :http-connect-timeout
    :accessor runtime-context-http-connect-timeout
    :initform 15
    :documentation "HTTP connect timeout in seconds for this runtime context.")
   (http-read-timeout
    :initarg :http-read-timeout
    :accessor runtime-context-http-read-timeout
    :initform 120
    :documentation "HTTP read timeout in seconds for this runtime context.")
   (getenv-function
    :initarg :getenv-function
    :accessor runtime-context-getenv-function
    :initform #'uiop:getenv
    :documentation "Environment lookup function for this runtime context.")
   (http-post-function
    :initarg :http-post-function
    :accessor runtime-context-http-post-function
    :initform #'dexador:post
    :documentation "HTTP POST function for this runtime context.")
   (gemini-api-key-function
    :initarg :gemini-api-key-function
    :accessor runtime-context-gemini-api-key-function
    :initform #'google:gemini-api-key
    :documentation "Gemini API key lookup function for this runtime context.")
   (filesystem-access-approval-function
    :initarg :filesystem-access-approval-function
    :accessor runtime-context-filesystem-access-approval-function
    :initform nil
    :documentation "Function used to approve persona filesystem access outside the current allowlist.")
   (eval-approval-function
    :initarg :eval-approval-function
    :accessor runtime-context-eval-approval-function
    :initform nil
    :documentation "Function used to approve evaluation of a specific persona eval tool expression.")
   (default-conversation
    :initarg :default-conversation
    :accessor runtime-context-default-conversation
    :initform nil
    :documentation "Default conversation associated with this runtime context.")))

(defclass chatbot ()
  ((model
    :initarg :model
    :accessor chatbot-model
    :initform nil
    :documentation "The model name used for the chatbot.")
   (backend
    :initarg :backend
    :accessor chatbot-backend
    :initform :gemini
    :documentation "The backend to use for the conversation (:gemini, :openai, or :google).")
   (system-instruction
    :initarg :system-instruction
    :accessor chatbot-system-instruction
    :initform nil
    :documentation "Optional system instructions directing the chatbot behavior.")
   (google-search-p
    :initarg :google-search-p
    :accessor chatbot-google-search-p
    :initform nil
    :documentation "Flag to enable Google Search Grounding tool.")
   (web-tools-p
    :initarg :web-tools-p
    :accessor chatbot-web-tools-p
    :initform nil
    :documentation "Flag to enable built-in web grounding search tools for this chatbot.")
   (code-execution-p
    :initarg :code-execution-p
    :accessor chatbot-code-execution-p
    :initform nil
    :documentation "Flag to enable sandboxed Code Execution tool.")
   (include-timestamp-p
    :initarg :include-timestamp-p
    :accessor chatbot-include-timestamp-p
    :initform nil
    :documentation "Flag to prepend a fresh timestamp to each live user prompt.")
   (include-model-p
    :initarg :include-model-p
    :accessor chatbot-include-model-p
    :initform nil
    :documentation "Flag to prepend the active model name to each live user prompt.")
   (enable-eval-p
    :initarg :enable-eval-p
    :accessor chatbot-enable-eval-p
    :initform nil
    :documentation "Flag to enable the built-in eval tool for this chatbot.")
   (filesystem-tools-p
    :initarg :filesystem-tools-p
    :accessor chatbot-filesystem-tools-p
    :initform nil
    :documentation "Flag to enable built-in filesystem tools for this chatbot.")
   (filesystem-root-directory
    :initarg :filesystem-root-directory
    :accessor chatbot-filesystem-root-directory
    :initform nil
    :documentation "Root directory within which built-in filesystem tools may operate.")
   (filesystem-allowed-directories
    :initarg :filesystem-allowed-directories
    :accessor chatbot-filesystem-allowed-directories
    :initform nil
    :documentation "Additional allowed directories a persona may traverse with built-in filesystem tools.")
   (filesystem-allowlist-path
    :initarg :filesystem-allowlist-path
    :accessor chatbot-filesystem-allowlist-path
    :initform nil
    :documentation "Persona-owned file path used to persist the filesystem allowlist.")
   (mcp-servers
    :initarg :mcp-servers
    :accessor chatbot-mcp-servers
    :initform nil
    :documentation "List of active connected MCP servers for this chatbot.")
   (mcp-startup-status
    :initarg :mcp-startup-status
    :accessor chatbot-mcp-startup-status
    :initform nil
    :documentation "Structured MCP startup status for this chatbot, when initialization has been attempted.")
   (runtime-context
    :initarg :runtime-context
    :accessor chatbot-runtime-context
    :initform nil
    :documentation "Optional runtime context carrying shared configuration and startup state.")))

(defclass conversation ()
  ((chatbot
    :initarg :chatbot
    :accessor conversation-chatbot
    :documentation "Reference to the chatbot instance powering this conversation.")
   (persona-memory
    :initarg :persona-memory
    :accessor conversation-persona-memory
    :initform nil
    :documentation "Optional preloaded persona memory kept separate from ordinary conversation turns.")
   (persona-diary-entries
    :initarg :persona-diary-entries
    :accessor conversation-persona-diary-entries
    :initform nil
    :documentation "Optional ordered persona diary preload entries kept separate from ordinary conversation turns.")
   (interaction-id
    :initarg :interaction-id
    :accessor conversation-interaction-id
    :initform nil
    :documentation "Stateful Gemini Interaction ID for multi-turn conversations.")
   (messages
    :initarg :messages
    :accessor conversation-messages
    :initform nil
    :documentation "Accumulated conversation messages for stateless backends (like OpenAI).")))
