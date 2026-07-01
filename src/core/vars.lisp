;;;

(in-package "CHATBOT")

;;; Variables for the Chatbot framework

(defvar *gemini-base-url* "https://generativelanguage.googleapis.com/v1beta"
  "The base REST endpoint for the Gemini Interactions API.")

(defparameter +planner-system-instruction+
  "You are an architectural planner. You cannot execute code. Your job is to collaborate with the user to outline steps required to achieve a goal. Ask clarifying questions until the requirements are unambiguous. Format the final output as a detailed Markdown list/document. When approved by the user, use the `submitPlan` tool to submit the plan.")

(defvar *openai-base-url* "https://api.openai.com/v1"
  "The base REST endpoint for the OpenAI-compliant API.")

(defvar *openai-api-key* nil
  "The API key for the OpenAI-compliant API. If nil, looks up the OPENAI_API_KEY environment variable.")

(defvar *getenv-function* #'uiop:getenv
  "Function used to read environment variables.")

(defvar *gemini-api-key-function* #'google:gemini-api-key
  "Function used to resolve the Gemini API key.")

(defvar *web-search-function* #'google:web-search
  "Function used by the built-in web grounding search tool.")

(defvar *hyperspec-search-function* #'google:hyperspec-search
  "Function used by the built-in HyperSpec grounding search tool.")

(defun default-filesystem-access-approval-function (bot directory tool-name)
  "Prompts the user to approve BOT access to DIRECTORY for TOOL-NAME."
  (declare (ignore bot))
  (y-or-n-p "~&Allow ~A to access directory ~A and remember it for this persona? "
            tool-name
            (namestring directory)))

(defvar *filesystem-access-approval-function* #'default-filesystem-access-approval-function
  "Function used to approve persona filesystem access outside the current allowlist.")

(defvar *bypass-eval-approval-p* nil
  "When T, bypasses interactive evaluation approval and automatically returns T.")

(defun default-eval-approval-function (bot source tool-name)
  "Prompts the user to approve evaluating SOURCE for TOOL-NAME."
  (declare (ignore bot))
  (or *bypass-eval-approval-p*
      (y-or-n-p "~&Allow ~A to evaluate this expression?~%~A~% "
                tool-name
                source)))

(defvar *eval-approval-function* #'default-eval-approval-function
  "Function used to approve evaluation of a specific expression for the eval tool.")

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

(defvar *mcp-debug-p* nil
  "Global flag controlling whether verbose MCP JSON-RPC and lifecycle debug messages are logged.")

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

(defun default-persona-memory-compression-thread-function (thunk thread-name)
  "Starts a background thread for persona memory compression."
  (sb-thread:make-thread thunk :name thread-name))

(defvar *persona-memory-compression-thread-function*
  #'default-persona-memory-compression-thread-function
  "Function used to start background persona memory compression threads.")

(defvar *global-token-grand-totals* nil
  "Process-wide cumulative token totals shared across unrelated chats.")

(defvar *global-token-grand-totals-lock*
  (sb-thread:make-mutex :name "global-token-grand-totals-lock")
  "Mutex protecting process-wide token grand total updates.")

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

(defun normalize-chatbot-backend (backend context &key allow-nil-p)
  "Normalizes BACKEND to a supported backend keyword for CONTEXT."
  (when (null backend)
    (if allow-nil-p
        (return-from normalize-chatbot-backend nil)
        (error "~A backend is required." context)))
  (let ((normalized
          (typecase backend
            (keyword backend)
            (string
             (let ((downcased (string-downcase backend)))
               (cond
                 ((string= downcased "gemini") :gemini)
                 ((string= downcased "google") :google)
                 ((string= downcased "openai") :openai)
                 ((or (string= downcased "lm-studio")
                      (string= downcased "lm_studio"))
                  :lm-studio)
                 (t nil))))
            (t nil))))
    (unless (member normalized '(:gemini :google :openai :lm-studio))
      (error "Unsupported ~A backend: ~S" context backend))
    normalized))

(defun openai-api-key ()
  "Returns the OpenAI API key. First checks *openai-api-key*, then the OPENAI_API_KEY environment variable."
  (or *openai-api-key*
      (funcall (current-getenv-function) "OPENAI_API_KEY")))

(defvar *lm-studio-base-url* "http://127.0.0.1:1234"
  "The host root for the local LM Studio API.")

(defun lm-studio-api-base-url ()
  "Returns the normalized OpenAI-compatible LM Studio API base URL."
  (let ((base-url (string-right-trim "/" *lm-studio-base-url*)))
    (if (alexandria:ends-with-subseq "/v1" base-url)
        base-url
        (concatenate 'string base-url "/v1"))))

(defvar *lm-studio-default-api-key* "lm_studio"
  "Fallback API key used when LM Studio credentials are otherwise unset.")

(defvar *lm-studio-api-key* nil
  "The API key for the LM Studio API.")

(defvar *lm-studio-http-read-timeout* 600
  "Minimum HTTP response timeout in seconds for the LM Studio backend.")

(defun lm-studio-api-key ()
  "Returns the LM Studio API key. First checks *lm-studio-api-key*, then the LM_API_TOKEN environment variable."
  (or *lm-studio-api-key*
      (funcall (current-getenv-function) "LM_API_TOKEN")
      (require-non-empty-string *lm-studio-default-api-key* "LM Studio default API key")))

(defun backend-http-read-timeout (backend)
  "Returns the effective HTTP read timeout for BACKEND."
  (let ((default-timeout (current-http-read-timeout)))
    (if (eq backend :lm-studio)
        (max default-timeout *lm-studio-http-read-timeout*)
        default-timeout)))

(defun gemini-api-key ()
  "Returns the Gemini API key using the current runtime seam."
  (funcall (current-gemini-api-key-function)))

(defvar *gemini-api-revision* "2026-05-20"
  "API revision header value used for Gemini Interactions requests.")

(defun gemini-api-revision ()
  "Returns the configured Gemini API revision header value."
  (require-non-empty-string *gemini-api-revision* "Gemini API revision"))

(defvar *mcp-config-path* nil
  "Deprecated compatibility alias for the MCP configuration override path.
Runtime contexts no longer consult this special; use MAKE-RUNTIME-CONTEXT with
:MCP-CONFIG-PATH and explicit :RUNTIME-CONTEXT arguments instead.")

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
    (*http-read-timeout* . "MAKE-RUNTIME-CONTEXT with :HTTP-READ-TIMEOUT")
    (*agentic-loop-default-backend* . "MAKE-RUNTIME-CONTEXT with :AGENTIC-LOOP-DEFAULT-BACKEND")
    (*agentic-loop-default-model* . "MAKE-RUNTIME-CONTEXT with :AGENTIC-LOOP-DEFAULT-MODEL")
    (*filesystem-access-approval-function* . "MAKE-RUNTIME-CONTEXT with :FILESYSTEM-ACCESS-APPROVAL-FUNCTION")
    (*eval-approval-function* . "MAKE-RUNTIME-CONTEXT with :EVAL-APPROVAL-FUNCTION"))
  "Compatibility-only ambient globals mapped to their preferred replacements.")

(defvar *http-post-function* #'dexador:post
  "Function used to perform HTTP POST requests.")

(defvar *http-get-function* #'dexador:get
  "Function used to perform HTTP GET requests.")

(defun eager-mcp-startup-enabled-p ()
  "Returns true when eager shared MCP startup is enabled via environment."
  (let ((value (funcall *getenv-function* "CHATBOT_EAGER_MCP_STARTUP")))
    (and value
         (member (string-downcase value)
                 '("1" "true" "yes" "on")
                 :test #'string=))))

(defvar *startup-chatbot* nil
  "Deprecated compatibility alias for the shared startup chatbot.
Runtime contexts own startup MCP state; use INITIALIZE-STARTUP-CHATBOT and
explicit runtime contexts instead of mutating this special directly.")

(defvar *auto-initialize-startup-mcp-servers-p* (eager-mcp-startup-enabled-p)
  "Deprecated compatibility alias for eager shared MCP startup.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:AUTO-INITIALIZE-STARTUP-MCP-SERVERS-P instead.")

(defvar *logging-enabled-p* t
  "Deprecated compatibility alias controlling Chatbot logging.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:LOGGING-ENABLED-P.")

(defvar *log-level* :info
  "Deprecated compatibility alias for the Chatbot minimum log level.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with :LOG-LEVEL.")

(defvar *log-stream* *error-output*
  "Deprecated compatibility alias for the Chatbot log output stream.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with :LOG-STREAM.")

(defvar *http-connect-timeout* 15
  "Deprecated compatibility alias for the HTTP connection timeout.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:HTTP-CONNECT-TIMEOUT.")

(defvar *http-read-timeout* 120
  "Deprecated compatibility alias for the HTTP response timeout.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:HTTP-READ-TIMEOUT.")

(defvar *default-conversation* nil
  "Compatibility-only ambient default conversation used by CHAT when none is specified.
Prefer passing :CONVERSATION explicitly or using a runtime context.")

(defvar *agentic-loop-default-backend* nil
  "Deprecated compatibility alias for the agentic-loop default backend.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:AGENTIC-LOOP-DEFAULT-BACKEND.")

(defvar *agentic-loop-default-model* nil
  "Deprecated compatibility alias for the agentic-loop default model.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:AGENTIC-LOOP-DEFAULT-MODEL.")

(defvar *default-runtime-context* nil
  "Canonical runtime context used for legacy no-context entry points.")

(defvar *active-runtime-context* nil
  "Runtime context currently bound by CALL-WITH-RUNTIME-CONTEXT, when any.")

(defvar *active-conversation* nil
  "Conversation currently being processed by CHAT, when any.")

(defparameter *runtime-context-legacy-global-specs*
  '((:symbol *default-conversation*
     :accessor runtime-context-default-conversation
     :warn-p t))
  "Authoritative compatibility-global mirror specification for runtime contexts.")

(defun runtime-context-legacy-global-symbol (spec)
  "Returns the compatibility global symbol from SPEC."
  (getf spec :symbol))

(defun runtime-context-legacy-global-accessor (spec)
  "Returns the runtime-context accessor symbol from SPEC."
  (getf spec :accessor))

(defun runtime-context-legacy-global-warn-p (spec)
  "Returns whether SPEC should emit compatibility warnings."
  (getf spec :warn-p))

(defun runtime-context-accessor-value (context accessor)
  "Reads ACCESSOR from CONTEXT."
  (funcall accessor context))

(defun set-runtime-context-accessor-value (context accessor value)
  "Stores VALUE on CONTEXT through ACCESSOR."
  (funcall (fdefinition (list 'setf accessor)) value context))

(defun runtime-context-legacy-global-symbols ()
  "Returns the ordered compatibility-global symbol list mirrored by runtime contexts."
  (mapcar #'runtime-context-legacy-global-symbol
          *runtime-context-legacy-global-specs*))

(defun runtime-context-legacy-global-values (context)
  "Returns the ordered runtime-context values corresponding to the mirrored globals."
  (mapcar (lambda (spec)
            (runtime-context-accessor-value context
                                            (runtime-context-legacy-global-accessor spec)))
          *runtime-context-legacy-global-specs*))

(defmethod initialize-instance :after ((bot chatbot) &key)
  "Applies backend-sensitive defaults for chatbot instances created without an explicit model."
  (when (null (chatbot-model bot))
    (setf (chatbot-model bot)
          (backend-default-model (chatbot-backend bot)))))

(defun make-runtime-context (&key (mcp-config-path nil mcp-config-path-p)
                                  (startup-chatbot nil startup-chatbot-p)
                                  (auto-initialize-startup-mcp-servers-p nil auto-init-p)
                                  (logging-enabled-p nil logging-enabled-p-p)
                                  (log-level nil log-level-p)
                                  (log-stream nil log-stream-p)
                                  (http-connect-timeout nil http-connect-timeout-p)
                                  (http-read-timeout nil http-read-timeout-p)
                                  (getenv-function nil getenv-function-p)
                                  (http-post-function nil http-post-function-p)
                                  (http-get-function nil http-get-function-p)
                                  (gemini-api-key-function nil gemini-api-key-function-p)
                                  (filesystem-access-approval-function nil filesystem-access-approval-function-p)
                                  (eval-approval-function nil eval-approval-function-p)
                                  (default-conversation nil default-conversation-p)
                                  (agentic-loop-default-backend nil agentic-loop-default-backend-p)
                                  (agentic-loop-default-model nil agentic-loop-default-model-p)
                                  (active-conversation nil active-conversation-p)
                                  (active-planner nil active-planner-p)
                                  (active-planner-parent-conversation nil active-planner-parent-conversation-p))
  "Constructs the preferred public container for shared Chatbot runtime state.
Use this with explicit :RUNTIME-CONTEXT arguments instead of mutating the
compatibility-only ambient special variables."
  (let ((template (resolve-runtime-context nil)))
    (flet ((inherit (provided-p explicit-value template-reader fallback-value)
             (if provided-p
                 explicit-value
                 (if template
                     (funcall template-reader template)
                     fallback-value))))
      (make-instance 'runtime-context
                     :mcp-config-path (inherit mcp-config-path-p
                                               mcp-config-path
                                               #'runtime-context-mcp-config-path
                                               nil)
                     :startup-chatbot (if startup-chatbot-p
                                          startup-chatbot
                                          nil)
                     :auto-initialize-startup-mcp-servers-p
                     (inherit auto-init-p
                              auto-initialize-startup-mcp-servers-p
                              #'runtime-context-auto-initialize-startup-mcp-servers-p
                              (eager-mcp-startup-enabled-p))
                     :logging-enabled-p (inherit logging-enabled-p-p
                                                 logging-enabled-p
                                                 #'runtime-context-logging-enabled-p
                                                 t)
                     :log-level (inherit log-level-p
                                         log-level
                                         #'runtime-context-log-level
                                         :info)
                     :log-stream (inherit log-stream-p
                                          log-stream
                                          #'runtime-context-log-stream
                                          *error-output*)
                     :http-connect-timeout (inherit http-connect-timeout-p
                                                    http-connect-timeout
                                                    #'runtime-context-http-connect-timeout
                                                    15)
                     :http-read-timeout (inherit http-read-timeout-p
                                                 http-read-timeout
                                                 #'runtime-context-http-read-timeout
                                                 120)
                     :getenv-function (inherit getenv-function-p
                                               getenv-function
                                               #'runtime-context-getenv-function
                                               *getenv-function*)
                     :http-post-function (inherit http-post-function-p
                                                  http-post-function
                                                  #'runtime-context-http-post-function
                                                  *http-post-function*)
                     :http-get-function (inherit http-get-function-p
                                                 http-get-function
                                                 #'runtime-context-http-get-function
                                                 *http-get-function*)
                     :gemini-api-key-function (inherit gemini-api-key-function-p
                                                       gemini-api-key-function
                                                       #'runtime-context-gemini-api-key-function
                                                       *gemini-api-key-function*)
                     :filesystem-access-approval-function
                     (inherit filesystem-access-approval-function-p
                              filesystem-access-approval-function
                              #'runtime-context-filesystem-access-approval-function
                              *filesystem-access-approval-function*)
                     :eval-approval-function (inherit eval-approval-function-p
                                                      eval-approval-function
                                                      #'runtime-context-eval-approval-function
                                                      *eval-approval-function*)
                     :default-conversation (if default-conversation-p
                                               default-conversation
                                               *default-conversation*)
                     :agentic-loop-default-backend
                     (inherit agentic-loop-default-backend-p
                              agentic-loop-default-backend
                              #'runtime-context-agentic-loop-default-backend
                              nil)
                     :agentic-loop-default-model
                     (inherit agentic-loop-default-model-p
                              agentic-loop-default-model
                              #'runtime-context-agentic-loop-default-model
                              nil)
                     :active-conversation (if active-conversation-p
                                             active-conversation
                                             nil)
                     :active-planner (if active-planner-p
                                         active-planner
                                         nil)
                     :active-planner-parent-conversation
                     (if active-planner-parent-conversation-p
                         active-planner-parent-conversation
                         nil)))))

(defun sync-runtime-context-from-legacy-globals (context &key (warn-p t))
  "Copies legacy global runtime values into CONTEXT."
  (labels ((same-runtime-state-p (left right)
             (or (eq left right)
                 (equal left right)))
           (maybe-warn-legacy-runtime-global-usage (spec current-value context-value)
             (let ((symbol (runtime-context-legacy-global-symbol spec)))
               (when (and warn-p
                          (runtime-context-legacy-global-warn-p spec)
                          *warn-on-legacy-runtime-globals-p*
                          (not (same-runtime-state-p current-value context-value))
                          (not (member symbol *legacy-runtime-global-warnings-issued* :test #'eq)))
                 (push symbol *legacy-runtime-global-warnings-issued*)
                 (format *error-output*
                         "[CHATBOT WARN] ~A is a compatibility-only ambient runtime global; prefer ~A.~%"
                         symbol
                         (or (cdr (assoc symbol *legacy-runtime-global-replacements* :test #'eq))
                             "MAKE-RUNTIME-CONTEXT and explicit :RUNTIME-CONTEXT usage"))))))
    (dolist (spec *runtime-context-legacy-global-specs*)
      (let* ((symbol (runtime-context-legacy-global-symbol spec))
             (accessor (runtime-context-legacy-global-accessor spec))
             (current-value (symbol-value symbol))
             (context-value (runtime-context-accessor-value context accessor)))
        (maybe-warn-legacy-runtime-global-usage spec current-value context-value)
        (set-runtime-context-accessor-value context accessor current-value))))
  context)

(defun sync-legacy-globals-from-runtime-context (context)
  "Copies runtime values from CONTEXT back into the legacy globals."
  (dolist (spec *runtime-context-legacy-global-specs*)
    (let ((symbol (runtime-context-legacy-global-symbol spec))
          (accessor (runtime-context-legacy-global-accessor spec)))
      (setf (symbol-value symbol)
            (runtime-context-accessor-value context accessor))))
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

(defun legacy-global-value (symbol)
  "Returns SYMBOL's current ambient legacy-global value."
  (symbol-value symbol))

(defun resolve-runtime-context (context &key sync-from-globals-p)
  "Returns CONTEXT, otherwise the active context, otherwise the canonical default context."
  (let ((resolved (or context
                     *active-runtime-context*
                     *default-runtime-context*)))
    (when (and sync-from-globals-p
               (default-runtime-context-p resolved))
      (sync-runtime-context-from-legacy-globals resolved))
    resolved))

(defun mirrored-runtime-context-value (context accessor legacy-symbol)
  "Returns the mirrored legacy/runtime-context value for ACCESSOR and LEGACY-SYMBOL."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-accessor-value resolved-context accessor))
      (context
       (when (default-runtime-context-p resolved-context)
         (set-runtime-context-accessor-value resolved-context accessor
                                           (legacy-global-value legacy-symbol)))
       (and resolved-context
            (runtime-context-accessor-value resolved-context accessor)))
      (t
       (when (default-runtime-context-p resolved-context)
         (set-runtime-context-accessor-value resolved-context accessor
                                           (legacy-global-value legacy-symbol)))
       (legacy-global-value legacy-symbol)))))

(defun set-mirrored-runtime-context-value (value context accessor legacy-symbol)
  "Stores VALUE through the mirrored legacy/runtime-context bridge."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (set-runtime-context-accessor-value resolved-context accessor value))
      (context
       (when resolved-context
         (set-runtime-context-accessor-value resolved-context accessor value)
         (when (default-runtime-context-p resolved-context)
           (setf (symbol-value legacy-symbol) value))))
      (t
       (setf (symbol-value legacy-symbol) value)
       (when (default-runtime-context-p resolved-context)
         (set-runtime-context-accessor-value resolved-context accessor value)))))
  value)

(defun runtime-context-function-value (context accessor legacy-symbol &key default-uses-legacy-p)
  "Returns the function seam value for ACCESSOR and LEGACY-SYMBOL."
  (if context
      (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
        (if resolved-context
            (if (or (active-runtime-context-p resolved-context)
                   (and default-uses-legacy-p
                        (default-runtime-context-p resolved-context)))
               (legacy-global-value legacy-symbol)
               (runtime-context-accessor-value resolved-context accessor))
            (legacy-global-value legacy-symbol)))
      (legacy-global-value legacy-symbol)))

(defun set-runtime-context-function-value (value context accessor legacy-symbol &key default-uses-legacy-p)
  "Stores VALUE through the function seam bridge for ACCESSOR and LEGACY-SYMBOL."
  (let ((resolved-context (and context
                              (resolve-runtime-context context :sync-from-globals-p t))))
    (if resolved-context
        (progn
          (set-runtime-context-accessor-value resolved-context accessor value)
          (when (or (active-runtime-context-p resolved-context)
                   (and default-uses-legacy-p
                        (default-runtime-context-p resolved-context)))
            (setf (symbol-value legacy-symbol) value)))
        (setf (symbol-value legacy-symbol) value)))
  value)

(defmacro define-mirrored-runtime-context-helper (name accessor legacy-symbol getter-doc setter-doc)
  `(progn
     (defun ,name (&optional context)
       ,getter-doc
       (mirrored-runtime-context-value context ',accessor ',legacy-symbol))
     (defun (setf ,name) (value &optional context)
       ,setter-doc
       (set-mirrored-runtime-context-value value context ',accessor ',legacy-symbol))))

(defmacro define-runtime-context-function-helper (name accessor legacy-symbol getter-doc setter-doc
                                                 &key default-uses-legacy-p)
  `(progn
     (defun ,name (&optional context)
       ,getter-doc
       (runtime-context-function-value context
                                      ',accessor
                                      ',legacy-symbol
                                      :default-uses-legacy-p ,default-uses-legacy-p))
     (defun (setf ,name) (value &optional context)
       ,setter-doc
       (set-runtime-context-function-value value
                                          context
                                          ',accessor
                                          ',legacy-symbol
                                          :default-uses-legacy-p ,default-uses-legacy-p))))

(defun runtime-context-owned-value (context accessor)
  "Returns ACCESSOR from the resolved runtime context, preferring the active context."
  (let ((resolved-context (resolve-runtime-context context)))
    (and resolved-context
        (runtime-context-accessor-value resolved-context accessor))))

(defun set-runtime-context-owned-value (value context accessor legacy-symbol)
  "Stores VALUE through ACCESSOR on the resolved runtime context.
When the canonical default runtime context is updated, keep LEGACY-SYMBOL in sync
as a compatibility alias."
  (let ((resolved-context (or (resolve-runtime-context context)
                             *default-runtime-context*)))
    (when resolved-context
      (set-runtime-context-accessor-value resolved-context accessor value)
      (when (default-runtime-context-p resolved-context)
       (setf (symbol-value legacy-symbol) value))))
  value)

(defmacro define-context-owned-runtime-context-helper (name accessor legacy-symbol getter-doc setter-doc)
  `(progn
     (defun ,name (&optional context)
       ,getter-doc
       (runtime-context-owned-value context ',accessor))
     (defun (setf ,name) (value &optional context)
       ,setter-doc
       (set-runtime-context-owned-value value context ',accessor ',legacy-symbol))))

(defun transient-runtime-context-value (context accessor legacy-symbol)
  "Returns transient runtime state from CONTEXT when present, otherwise the legacy global."
  (let ((resolved-context (resolve-runtime-context context)))
    (if resolved-context
        (or (runtime-context-accessor-value resolved-context accessor)
            (let ((legacy-value (legacy-global-value legacy-symbol)))
              (and (typep legacy-value 'conversation)
                   (eq (chatbot-runtime-context (conversation-chatbot legacy-value))
                       resolved-context)
                   legacy-value)))
        (legacy-global-value legacy-symbol))))

(defun set-transient-runtime-context-value (value context accessor legacy-symbol)
  "Stores transient runtime state in both CONTEXT and the legacy compatibility global."
  (let ((resolved-context (resolve-runtime-context context)))
    (when resolved-context
      (set-runtime-context-accessor-value resolved-context accessor value))
    (setf (symbol-value legacy-symbol) value))
  value)

(defmacro define-transient-runtime-context-helper (name accessor legacy-symbol getter-doc setter-doc)
  `(progn
     (defun ,name (&optional context)
       ,getter-doc
       (transient-runtime-context-value context ',accessor ',legacy-symbol))
     (defun (setf ,name) (value &optional context)
       ,setter-doc
       (set-transient-runtime-context-value value context ',accessor ',legacy-symbol))))

(define-mirrored-runtime-context-helper current-default-conversation
  runtime-context-default-conversation
  *default-conversation*
  "Returns the ambient default conversation for CONTEXT."
  "Sets the ambient default conversation for CONTEXT.")

(define-context-owned-runtime-context-helper current-mcp-config-path
  runtime-context-mcp-config-path
  *mcp-config-path*
  "Returns the ambient MCP configuration override path for CONTEXT."
  "Sets the ambient MCP configuration override path for CONTEXT.")

(define-context-owned-runtime-context-helper current-startup-chatbot
  runtime-context-startup-chatbot
  *startup-chatbot*
  "Returns the shared startup chatbot for CONTEXT."
  "Sets the shared startup chatbot for CONTEXT.")

(define-context-owned-runtime-context-helper current-auto-initialize-startup-mcp-servers-p
  runtime-context-auto-initialize-startup-mcp-servers-p
  *auto-initialize-startup-mcp-servers-p*
  "Returns whether startup MCP auto-initialization is enabled for CONTEXT."
  "Sets whether startup MCP auto-initialization is enabled for CONTEXT.")

(define-context-owned-runtime-context-helper current-logging-enabled-p
  runtime-context-logging-enabled-p
  *logging-enabled-p*
  "Returns whether logging is enabled for CONTEXT."
  "Sets whether logging is enabled for CONTEXT.")

(define-context-owned-runtime-context-helper current-log-level
  runtime-context-log-level
  *log-level*
  "Returns the current log level for CONTEXT."
  "Sets the current log level for CONTEXT.")

(define-context-owned-runtime-context-helper current-log-stream
  runtime-context-log-stream
  *log-stream*
  "Returns the current log stream for CONTEXT."
  "Sets the current log stream for CONTEXT.")

(define-context-owned-runtime-context-helper current-http-connect-timeout
  runtime-context-http-connect-timeout
  *http-connect-timeout*
  "Returns the current HTTP connect timeout for CONTEXT."
  "Sets the current HTTP connect timeout for CONTEXT.")

(define-context-owned-runtime-context-helper current-http-read-timeout
  runtime-context-http-read-timeout
  *http-read-timeout*
  "Returns the current HTTP read timeout for CONTEXT."
  "Sets the current HTTP read timeout for CONTEXT.")

(define-context-owned-runtime-context-helper current-agentic-loop-default-backend
  runtime-context-agentic-loop-default-backend
  *agentic-loop-default-backend*
  "Returns the default backend for new agentic loops in CONTEXT."
  "Sets the default backend for new agentic loops in CONTEXT.")

(define-context-owned-runtime-context-helper current-agentic-loop-default-model
  runtime-context-agentic-loop-default-model
  *agentic-loop-default-model*
  "Returns the default model for new agentic loops in CONTEXT."
  "Sets the default model for new agentic loops in CONTEXT.")

(define-runtime-context-function-helper current-getenv-function
  runtime-context-getenv-function
  *getenv-function*
  "Returns the current environment lookup function for CONTEXT."
  "Sets the current environment lookup function for CONTEXT.")

(define-runtime-context-function-helper current-http-post-function
  runtime-context-http-post-function
  *http-post-function*
  "Returns the current HTTP POST function for CONTEXT."
  "Sets the current HTTP POST function for CONTEXT.")

(define-runtime-context-function-helper current-http-get-function
  runtime-context-http-get-function
  *http-get-function*
  "Returns the current HTTP GET function for CONTEXT."
  "Sets the current HTTP GET function for CONTEXT.")

(define-runtime-context-function-helper current-gemini-api-key-function
  runtime-context-gemini-api-key-function
  *gemini-api-key-function*
  "Returns the current Gemini API key lookup function for CONTEXT."
  "Sets the current Gemini API key lookup function for CONTEXT.")

(define-runtime-context-function-helper current-filesystem-access-approval-function
  runtime-context-filesystem-access-approval-function
  *filesystem-access-approval-function*
  "Returns the current filesystem access approval function for CONTEXT."
  "Sets the current filesystem access approval function for CONTEXT."
  :default-uses-legacy-p t)

(define-runtime-context-function-helper current-eval-approval-function
  runtime-context-eval-approval-function
  *eval-approval-function*
  "Returns the current eval approval function for CONTEXT."
  "Sets the current eval approval function for CONTEXT."
  :default-uses-legacy-p t)

(define-transient-runtime-context-helper current-active-conversation
  runtime-context-active-conversation
  *active-conversation*
  "Returns the transient active conversation for CONTEXT."
  "Sets the transient active conversation for CONTEXT.")

(define-transient-runtime-context-helper current-active-planner
  runtime-context-active-planner
  *active-planner*
  "Returns the transient active planner conversation for CONTEXT."
  "Sets the transient active planner conversation for CONTEXT.")

(define-transient-runtime-context-helper current-active-planner-parent-conversation
  runtime-context-active-planner-parent-conversation
  *active-planner-parent-conversation*
  "Returns the transient planner parent conversation for CONTEXT."
  "Sets the transient planner parent conversation for CONTEXT.")

(defun call-with-runtime-context (context thunk)
  "Calls THUNK with the resolved runtime context active.
Only *DEFAULT-CONVERSATION* still requires legacy special rebinding; all other
runtime settings are read from the runtime context directly."
  (let* ((resolved-context (resolve-runtime-context context :sync-from-globals-p t))
         (default-context-p (default-runtime-context-p resolved-context)))
    (if (null resolved-context)
        (funcall thunk)
        (if (active-runtime-context-p resolved-context)
            (funcall thunk)
            (let (result)
              (setf result
                    (let ((*active-runtime-context* resolved-context))
                     (let ((*getenv-function* (if default-context-p
                                                  *getenv-function*
                                                  (runtime-context-getenv-function resolved-context)))
                           (*http-post-function* (if default-context-p
                                                     *http-post-function*
                                                     (runtime-context-http-post-function resolved-context)))
                           (*http-get-function* (if default-context-p
                                                    *http-get-function*
                                                    (runtime-context-http-get-function resolved-context)))
                           (*gemini-api-key-function* (if default-context-p
                                                          *gemini-api-key-function*
                                                          (runtime-context-gemini-api-key-function resolved-context)))
                           (*filesystem-access-approval-function* (if default-context-p
                                                                      *filesystem-access-approval-function*
                                                                      (runtime-context-filesystem-access-approval-function resolved-context)))
                           (*eval-approval-function* (if default-context-p
                                                         *eval-approval-function*
                                                         (runtime-context-eval-approval-function resolved-context))))
                       (progv (runtime-context-legacy-global-symbols)
                              (runtime-context-legacy-global-values resolved-context)
                         (unwind-protect
                              (funcall thunk)
                           (sync-runtime-context-from-legacy-globals resolved-context :warn-p nil)
                           (setf (runtime-context-getenv-function resolved-context) *getenv-function*)
                           (setf (runtime-context-http-post-function resolved-context) *http-post-function*)
                           (setf (runtime-context-http-get-function resolved-context) *http-get-function*)
                           (setf (runtime-context-gemini-api-key-function resolved-context) *gemini-api-key-function*)
                           (setf (runtime-context-filesystem-access-approval-function resolved-context)
                                 *filesystem-access-approval-function*)
                           (setf (runtime-context-eval-approval-function resolved-context)
                                 *eval-approval-function*))))))
              (when default-context-p
                (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
              result)))))

(setf *default-runtime-context* (make-runtime-context))

(defvar *max-minion-depth* 3
  "The global maximum nesting depth allowed for the minion hierarchy.")

(defvar *context-pruning-threshold-characters* 64000
  "The total character length of the conversation history above which auto-pruning is triggered.")

(defvar *active-planner* nil
  "Tracks the active planner minion conversation, or NIL if not in Planner Mode.")

(defvar *active-planner-parent-conversation* nil
  "Tracks the parent conversation that spawned the active planner minion.")
