;;;

(in-package "CHATBOT")

;;; Variables for the Chatbot framework

(defvar *gemini-base-url* "https://generativelanguage.googleapis.com/v1beta"
  "The base REST endpoint for the Gemini Interactions API.")

(defvar *openai-base-url* "https://api.openai.com/v1"
  "The base REST endpoint for the OpenAI-compliant API.")

(defvar *openai-api-key* nil
  "The API key for the OpenAI-compliant API. If nil, looks up the OPENAI_API_KEY environment variable.")

(defvar *getenv-function* #'uiop:getenv
  "Function used to read environment variables.")

(defvar *gemini-api-key-function* #'google:gemini-api-key
  "Function used to resolve the Gemini API key.")

(defvar *user-homedir-pathname-function* #'user-homedir-pathname
  "Function used to resolve the current user's home directory pathname.")

(defvar *read-mcp-config-function* nil
  "Optional test seam for reading MCP configuration.")

(defvar *start-mcp-server-function* nil
  "Optional test seam for launching an MCP server.")

(defvar *stop-mcp-server-function* nil
  "Optional test seam for stopping an MCP server.")

(defvar *mcp-send-request-function* nil
  "Optional test seam for sending an MCP JSON-RPC request.")

(defvar *mcp-initialize-function* nil
  "Optional test seam for performing the MCP initialize handshake.")

(defvar *mcp-call-tool-function* nil
  "Optional test seam for invoking an MCP tool call.")

(defvar *initialize-mcp-servers-for-chatbot-function* nil
  "Optional test seam for startup MCP initialization orchestration.")

(defvar *get-all-mcp-tools-function* nil
  "Optional test seam for enumerating all MCP tools for a chatbot.")

(defvar *find-mcp-server-and-tool-function* nil
  "Optional test seam for resolving an MCP tool by name.")

(defvar *execute-mcp-tool-function* nil
  "Optional test seam for executing an MCP tool and returning text content.")

(defun require-non-empty-string (value context)
  "Returns VALUE when it is a non-empty string, otherwise signals an error for CONTEXT."
  (unless (and (stringp value)
               (string/= value ""))
    (error "~A must be a non-empty string." context))
  value)

(defvar *backend-default-models*
  '((:gemini . "gemini-3.5-flash")
    (:google . "gemini-3.5-flash")
    (:openai . "gpt-4o")
    (:lm-studio . "gemma-4-e4b-uncensored-hauhaucs-aggressive"))
  "Default model names keyed by backend.")

(defun backend-default-model (backend)
  "Returns the configured default model for BACKEND.
Unknown backends fall back to the Gemini default."
  (let* ((gemini-default (cdr (assoc :gemini *backend-default-models*)))
         (resolved (or (cdr (assoc backend *backend-default-models*))
                       gemini-default)))
    (require-non-empty-string resolved (format nil "Default model for backend ~A" backend))))

(defun openai-api-key ()
  "Returns the OpenAI API key. First checks *openai-api-key*, then the OPENAI_API_KEY environment variable."
  (or *openai-api-key*
      (funcall *getenv-function* "OPENAI_API_KEY")))

(defvar *lm-studio-base-url* "http://127.0.0.1:8088/v1"
  "The base REST endpoint for the local LM Studio API.")

(defvar *lm-studio-default-api-key* "lm_studio"
  "Fallback API key used when LM Studio credentials are otherwise unset.")

(defvar *lm-studio-api-key* nil
  "The API key for the LM Studio API.")

(defun lm-studio-api-key ()
  "Returns the LM Studio API key. First checks *lm-studio-api-key*, then the LM_API_TOKEN environment variable."
  (or *lm-studio-api-key*
      (funcall *getenv-function* "LM_API_TOKEN")
      (require-non-empty-string *lm-studio-default-api-key* "LM Studio default API key")))

(defun gemini-api-key ()
  "Returns the Gemini API key using the current runtime seam."
  (funcall *gemini-api-key-function*))

(defvar *gemini-api-revision* "2026-05-20"
  "API revision header value used for Gemini Interactions requests.")

(defun gemini-api-revision ()
  "Returns the configured Gemini API revision header value."
  (require-non-empty-string *gemini-api-revision* "Gemini API revision"))

(defvar *mcp-config-path* nil
  "Compatibility-only ambient override path for the MCP configuration file.
Prefer MAKE-RUNTIME-CONTEXT with :MCP-CONFIG-PATH and explicit :RUNTIME-CONTEXT
arguments on public entry points.")

(defvar *warn-on-legacy-runtime-globals-p*
  (let ((value (funcall *getenv-function* "CHATBOT_WARN_LEGACY_RUNTIME_GLOBALS")))
    (and value
         (member (string-downcase value)
                 '("1" "true" "yes" "on")
                 :test #'string=)))
  "When true, emit one-time migration warnings when compatibility-only ambient
runtime globals are used to override the default runtime context.")

(defvar *legacy-runtime-global-warnings-issued* nil
  "Internal list of compatibility globals already warned about in this image.")

(defvar *legacy-runtime-global-replacements*
  '((*mcp-config-path* . "MAKE-RUNTIME-CONTEXT with :MCP-CONFIG-PATH and explicit :RUNTIME-CONTEXT arguments")
    (*auto-initialize-startup-mcp-servers-p* . "MAKE-RUNTIME-CONTEXT with :AUTO-INITIALIZE-STARTUP-MCP-SERVERS-P")
    (*default-conversation* . "passing :CONVERSATION explicitly or using MAKE-RUNTIME-CONTEXT")
    (*logging-enabled-p* . "MAKE-RUNTIME-CONTEXT with :LOGGING-ENABLED-P")
    (*log-level* . "MAKE-RUNTIME-CONTEXT with :LOG-LEVEL")
    (*log-stream* . "MAKE-RUNTIME-CONTEXT with :LOG-STREAM")
    (*http-connect-timeout* . "MAKE-RUNTIME-CONTEXT with :HTTP-CONNECT-TIMEOUT")
    (*http-read-timeout* . "MAKE-RUNTIME-CONTEXT with :HTTP-READ-TIMEOUT"))
  "Compatibility-only ambient globals mapped to their preferred replacements.")

(defvar *http-post-function* #'dexador:post
  "Function used to perform HTTP POST requests.")

(defun eager-mcp-startup-enabled-p ()
  "Returns true when eager shared MCP startup is enabled via environment."
  (let ((value (funcall *getenv-function* "CHATBOT_EAGER_MCP_STARTUP")))
    (and value
         (member (string-downcase value)
                 '("1" "true" "yes" "on")
                 :test #'string=))))

(defvar *startup-chatbot* nil
  "Compatibility-only ambient shared chatbot instance used to own startup MCP servers.
Prefer INITIALIZE-STARTUP-CHATBOT and explicit runtime contexts over mutating this
special directly.")

(defvar *auto-initialize-startup-mcp-servers-p* (eager-mcp-startup-enabled-p)
  "Compatibility-only ambient flag for eager shared MCP startup.
Prefer MAKE-RUNTIME-CONTEXT with :AUTO-INITIALIZE-STARTUP-MCP-SERVERS-P.
Defaults to the CHATBOT_EAGER_MCP_STARTUP environment variable for migration compatibility.")

(defvar *logging-enabled-p* t
  "Compatibility-only ambient flag controlling Chatbot logging.
Prefer MAKE-RUNTIME-CONTEXT with :LOGGING-ENABLED-P.")

(defvar *log-level* :info
  "Compatibility-only ambient minimum log level.
Prefer MAKE-RUNTIME-CONTEXT with :LOG-LEVEL.")

(defvar *log-stream* *error-output*
  "Compatibility-only ambient destination stream for Chatbot log output.
Prefer MAKE-RUNTIME-CONTEXT with :LOG-STREAM.")

(defvar *http-connect-timeout* 15
  "Compatibility-only ambient HTTP connection timeout in seconds.
Prefer MAKE-RUNTIME-CONTEXT with :HTTP-CONNECT-TIMEOUT.")

(defvar *http-read-timeout* 120
  "Compatibility-only ambient HTTP response timeout in seconds.
Prefer MAKE-RUNTIME-CONTEXT with :HTTP-READ-TIMEOUT.")

(defvar *default-conversation* nil
  "Compatibility-only ambient default conversation used by CHAT when none is specified.
Prefer passing :CONVERSATION explicitly or using a runtime context.")

(defvar *default-runtime-context* nil
  "Canonical runtime context used for legacy no-context entry points.")

(defvar *active-runtime-context* nil
  "Runtime context currently bound by CALL-WITH-RUNTIME-CONTEXT, when any.")

(defmethod initialize-instance :after ((bot chatbot) &key)
  "Applies backend-sensitive defaults for chatbot instances created without an explicit model."
  (when (null (chatbot-model bot))
    (setf (chatbot-model bot)
          (backend-default-model (chatbot-backend bot)))))

(defun make-runtime-context (&key mcp-config-path
                                  startup-chatbot
                                  (auto-initialize-startup-mcp-servers-p *auto-initialize-startup-mcp-servers-p*)
                                  (logging-enabled-p *logging-enabled-p*)
                                  (log-level *log-level*)
                                  (log-stream *log-stream*)
                                  (http-connect-timeout *http-connect-timeout*)
                                  (http-read-timeout *http-read-timeout*)
                                  (getenv-function *getenv-function*)
                                  (http-post-function *http-post-function*)
                                  (gemini-api-key-function *gemini-api-key-function*)
                                  default-conversation)
  "Constructs the preferred public container for shared Chatbot runtime state.
Use this with explicit :RUNTIME-CONTEXT arguments instead of mutating the
compatibility-only ambient special variables."
  (make-instance 'runtime-context
                 :mcp-config-path mcp-config-path
                 :startup-chatbot startup-chatbot
                 :auto-initialize-startup-mcp-servers-p auto-initialize-startup-mcp-servers-p
                 :logging-enabled-p logging-enabled-p
                 :log-level log-level
                 :log-stream log-stream
                 :http-connect-timeout http-connect-timeout
                 :http-read-timeout http-read-timeout
                 :getenv-function getenv-function
                 :http-post-function http-post-function
                 :gemini-api-key-function gemini-api-key-function
                 :default-conversation default-conversation))

(defun sync-runtime-context-from-legacy-globals (context)
  "Copies legacy global runtime values into CONTEXT."
  (labels ((same-runtime-state-p (left right)
             (or (eq left right)
                 (equal left right)))
           (maybe-warn-legacy-runtime-global-usage (symbol current-value context-value)
             (when (and *warn-on-legacy-runtime-globals-p*
                        (not (same-runtime-state-p current-value context-value))
                        (not (member symbol *legacy-runtime-global-warnings-issued* :test #'eq)))
               (push symbol *legacy-runtime-global-warnings-issued*)
               (format *error-output*
                       "[CHATBOT WARN] ~A is a compatibility-only ambient runtime global; prefer ~A.~%"
                       symbol
                       (or (cdr (assoc symbol *legacy-runtime-global-replacements* :test #'eq))
                           "MAKE-RUNTIME-CONTEXT and explicit :RUNTIME-CONTEXT usage")))))
    (maybe-warn-legacy-runtime-global-usage '*mcp-config-path*
                                            *mcp-config-path*
                                            (runtime-context-mcp-config-path context))
    (maybe-warn-legacy-runtime-global-usage '*auto-initialize-startup-mcp-servers-p*
                                            *auto-initialize-startup-mcp-servers-p*
                                            (runtime-context-auto-initialize-startup-mcp-servers-p context))
    (maybe-warn-legacy-runtime-global-usage '*default-conversation*
                                            *default-conversation*
                                            (runtime-context-default-conversation context))
    (maybe-warn-legacy-runtime-global-usage '*logging-enabled-p*
                                            *logging-enabled-p*
                                            (runtime-context-logging-enabled-p context))
    (maybe-warn-legacy-runtime-global-usage '*log-level*
                                            *log-level*
                                            (runtime-context-log-level context))
    (maybe-warn-legacy-runtime-global-usage '*log-stream*
                                            *log-stream*
                                            (runtime-context-log-stream context))
    (maybe-warn-legacy-runtime-global-usage '*http-connect-timeout*
                                            *http-connect-timeout*
                                            (runtime-context-http-connect-timeout context))
    (maybe-warn-legacy-runtime-global-usage '*http-read-timeout*
                                            *http-read-timeout*
                                            (runtime-context-http-read-timeout context)))
  (setf (runtime-context-mcp-config-path context) *mcp-config-path*)
  (setf (runtime-context-startup-chatbot context) *startup-chatbot*)
  (setf (runtime-context-auto-initialize-startup-mcp-servers-p context) *auto-initialize-startup-mcp-servers-p*)
  (setf (runtime-context-logging-enabled-p context) *logging-enabled-p*)
  (setf (runtime-context-log-level context) *log-level*)
  (setf (runtime-context-log-stream context) *log-stream*)
  (setf (runtime-context-http-connect-timeout context) *http-connect-timeout*)
  (setf (runtime-context-http-read-timeout context) *http-read-timeout*)
  (setf (runtime-context-getenv-function context) *getenv-function*)
  (setf (runtime-context-http-post-function context) *http-post-function*)
  (setf (runtime-context-gemini-api-key-function context) *gemini-api-key-function*)
  (setf (runtime-context-default-conversation context) *default-conversation*)
  context)

(defun sync-legacy-globals-from-runtime-context (context)
  "Copies runtime values from CONTEXT back into the legacy globals."
  (setf *mcp-config-path* (runtime-context-mcp-config-path context))
  (setf *startup-chatbot* (runtime-context-startup-chatbot context))
  (setf *auto-initialize-startup-mcp-servers-p* (runtime-context-auto-initialize-startup-mcp-servers-p context))
  (setf *logging-enabled-p* (runtime-context-logging-enabled-p context))
  (setf *log-level* (runtime-context-log-level context))
  (setf *log-stream* (runtime-context-log-stream context))
  (setf *http-connect-timeout* (runtime-context-http-connect-timeout context))
  (setf *http-read-timeout* (runtime-context-http-read-timeout context))
  (setf *getenv-function* (runtime-context-getenv-function context))
  (setf *http-post-function* (runtime-context-http-post-function context))
  (setf *gemini-api-key-function* (runtime-context-gemini-api-key-function context))
  (setf *default-conversation* (runtime-context-default-conversation context))
  context)

(defun default-runtime-context-p (context)
  "Returns true when CONTEXT is the canonical default runtime context."
  (and context
       (eq context *default-runtime-context*)))

(defun maybe-sync-legacy-globals-from-default-runtime-context (context)
  "Copies CONTEXT back to legacy globals when it is the default runtime context."
  (when (default-runtime-context-p context)
    (sync-legacy-globals-from-runtime-context context))
  context)

(defun active-runtime-context-p (context)
  "Returns true when CONTEXT is the currently bound runtime context."
  (and context
       (eq context *active-runtime-context*)))

(defun resolve-runtime-context (context &key sync-from-globals-p)
  "Returns CONTEXT or the canonical default runtime context when CONTEXT is nil."
  (let ((resolved (or context *default-runtime-context*)))
    (when (and sync-from-globals-p
               (default-runtime-context-p resolved))
      (sync-runtime-context-from-legacy-globals resolved))
    resolved))

(defun current-default-conversation (&optional context)
  "Returns the ambient default conversation for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *default-conversation*
             (runtime-context-default-conversation resolved-context)))))

(defun (setf current-default-conversation) (value &optional context)
  "Sets the ambient default conversation for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-default-conversation resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *default-conversation* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-mcp-config-path (&optional context)
  "Returns the ambient MCP configuration override path for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *mcp-config-path*
             (runtime-context-mcp-config-path resolved-context)))))

(defun (setf current-mcp-config-path) (value &optional context)
  "Sets the ambient MCP configuration override path for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-mcp-config-path resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *mcp-config-path* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-startup-chatbot (&optional context)
  "Returns the shared startup chatbot for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *startup-chatbot*
             (runtime-context-startup-chatbot resolved-context)))))

(defun (setf current-startup-chatbot) (value &optional context)
  "Sets the shared startup chatbot for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-startup-chatbot resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *startup-chatbot* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-auto-initialize-startup-mcp-servers-p (&optional context)
  "Returns whether startup MCP auto-initialization is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *auto-initialize-startup-mcp-servers-p*
             (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context)))))

(defun (setf current-auto-initialize-startup-mcp-servers-p) (value &optional context)
  "Sets whether startup MCP auto-initialization is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *auto-initialize-startup-mcp-servers-p* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-logging-enabled-p (&optional context)
  "Returns whether logging is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *logging-enabled-p*
             (runtime-context-logging-enabled-p resolved-context)))))

(defun (setf current-logging-enabled-p) (value &optional context)
  "Sets whether logging is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-logging-enabled-p resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *logging-enabled-p* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-log-level (&optional context)
  "Returns the current log level for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *log-level*
             (runtime-context-log-level resolved-context)))))

(defun (setf current-log-level) (value &optional context)
  "Sets the current log level for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-log-level resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *log-level* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-log-stream (&optional context)
  "Returns the current log stream for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *log-stream*
             (runtime-context-log-stream resolved-context)))))

(defun (setf current-log-stream) (value &optional context)
  "Sets the current log stream for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-log-stream resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *log-stream* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-http-connect-timeout (&optional context)
  "Returns the current HTTP connect timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *http-connect-timeout*
             (runtime-context-http-connect-timeout resolved-context)))))

(defun (setf current-http-connect-timeout) (value &optional context)
  "Sets the current HTTP connect timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-http-connect-timeout resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *http-connect-timeout* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun current-http-read-timeout (&optional context)
  "Returns the current HTTP read timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (and resolved-context
         (if (active-runtime-context-p resolved-context)
             *http-read-timeout*
             (runtime-context-http-read-timeout resolved-context)))))

(defun (setf current-http-read-timeout) (value &optional context)
  "Sets the current HTTP read timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
    (when resolved-context
      (setf (runtime-context-http-read-timeout resolved-context) value)
      (when (active-runtime-context-p resolved-context)
        (setf *http-read-timeout* value))
      (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
    value))

(defun call-with-runtime-context (context thunk)
  "Calls THUNK with legacy special variables rebound from CONTEXT when CONTEXT is non-nil."
  (let* ((resolved-context (resolve-runtime-context context :sync-from-globals-p t))
         (default-context-p (default-runtime-context-p resolved-context)))
    (if (null resolved-context)
        (funcall thunk)
        (let (result)
          (setf result
                (let ((*active-runtime-context* resolved-context)
                      (*mcp-config-path* (runtime-context-mcp-config-path resolved-context))
                     (*startup-chatbot* (runtime-context-startup-chatbot resolved-context))
                     (*auto-initialize-startup-mcp-servers-p* (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context))
                     (*logging-enabled-p* (runtime-context-logging-enabled-p resolved-context))
                     (*log-level* (runtime-context-log-level resolved-context))
                     (*log-stream* (runtime-context-log-stream resolved-context))
                     (*http-connect-timeout* (runtime-context-http-connect-timeout resolved-context))
                     (*http-read-timeout* (runtime-context-http-read-timeout resolved-context))
                     (*getenv-function* (runtime-context-getenv-function resolved-context))
                     (*http-post-function* (runtime-context-http-post-function resolved-context))
                     (*gemini-api-key-function* (runtime-context-gemini-api-key-function resolved-context))
                     (*default-conversation* (runtime-context-default-conversation resolved-context)))
                 (unwind-protect
                      (funcall thunk)
                   (setf (runtime-context-mcp-config-path resolved-context) *mcp-config-path*)
                   (setf (runtime-context-startup-chatbot resolved-context) *startup-chatbot*)
                   (setf (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context) *auto-initialize-startup-mcp-servers-p*)
                   (setf (runtime-context-logging-enabled-p resolved-context) *logging-enabled-p*)
                   (setf (runtime-context-log-level resolved-context) *log-level*)
                   (setf (runtime-context-log-stream resolved-context) *log-stream*)
                   (setf (runtime-context-http-connect-timeout resolved-context) *http-connect-timeout*)
                   (setf (runtime-context-http-read-timeout resolved-context) *http-read-timeout*)
                   (setf (runtime-context-getenv-function resolved-context) *getenv-function*)
                   (setf (runtime-context-http-post-function resolved-context) *http-post-function*)
                   (setf (runtime-context-gemini-api-key-function resolved-context) *gemini-api-key-function*)
                   (setf (runtime-context-default-conversation resolved-context) *default-conversation*))))
          (when default-context-p
            (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
          result))))

(setf *default-runtime-context* (make-runtime-context))
