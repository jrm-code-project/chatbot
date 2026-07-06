;;;

(in-package "CHATBOT")

;;; Variables for the Chatbot framework

(declaim (special *mcp-config-path*
                  *warn-on-legacy-runtime-globals-p*
                  *legacy-runtime-global-warnings-issued*
                  *legacy-runtime-global-replacements*
                  *startup-chatbot*
                  *auto-initialize-startup-mcp-servers-p*
                  *logging-enabled-p*
                  *log-level*
                  *log-stream*
                  *http-connect-timeout*
                  *http-read-timeout*
                  *http-patch-function*
                  *http-delete-function*
                  *default-conversation*
                  *agentic-loop-default-backend*
                  *agentic-loop-default-model*
                  *default-runtime-context*))

(defparameter *gemini-base-url* "https://generativelanguage.googleapis.com/v1beta"
  "The base REST endpoint for the Gemini Interactions API.")

(defparameter +agentic-operational-directive+
  "**OPERATIONAL DIRECTIVE:** You are an autonomous agentic loop spawned to achieve a specific goal. You are expected to be thorough, methodical, and persistent. **MANDATORY MINIMUM THRESHOLD:** You are strictly forbidden from concluding your task in fewer than three (3) iterations. Do not attempt a \"one-and-done\" lazy execution. **SEQUENTIAL REASONING:** You must use sequential thinking mechanisms (such as a structured Plan -> Execute -> Evaluate cycle) for every phase of your operation.
* **Iteration 1:** Analyze the goal, formulate a concrete plan, and execute the first logical step (e.g., read a file, execute a search, write initial code).
* **Iteration 2:** Evaluate the results of Iteration 1. Identify errors, gather missing context, or execute the next phase of the plan.
* **Iteration 3+:** Verify the final outcome against the original goal, test the code, or refine the data. Do not signal completion or ask for approval until you have thoroughly iterated, tested, and verified your work against the goal. Show your work.
**TOOL EXECUTION MANDATE (NO HALLUCINATIONS):** You are strictly forbidden from hallucinating actions or faking results. If your step requires gathering data or writing to the disk, you MUST explicitly invoke the corresponding tool (e.g., `search_nodes`, `writeFile`).
  * Do NOT claim you performed an action if you have not explicitly executed the tool call.
  * **SIMULTANEOUS FIRING REQUIRED:** When you invoke a tool, you MUST simultaneously output the required `{\"status\":\"continue\", \"summary\":\"...\"}` JSON object in your text response.
  * Do NOT wait for the tool execution result to return the JSON. The JSON summary should simply state which tool you are currently firing and what you expect to do with the result in the next iteration.
  * Summarizing work you did not physically perform via a tool call is a critical failure. Pull the trigger; do not just describe the bullet.")

(defparameter +planner-system-instruction+
  (format nil
          "You are an architectural planner. You cannot execute code. Your job is to collaborate with the user to outline steps required to achieve a goal. Ask clarifying questions until the requirements are unambiguous. Format the final output as a detailed Markdown list/document. When approved by the user, use the `submitPlan` tool to submit the plan.~%~%~A"
          +agentic-operational-directive+))

(defparameter *openai-base-url* "https://api.openai.com/v1"
  "The base REST endpoint for the OpenAI-compliant API.")

(defparameter *openai-api-key* nil
  "The API key for the OpenAI-compliant API. If nil, looks up the OPENAI_API_KEY environment variable.")

(defparameter *getenv-function* #'uiop:getenv
  "Function used to read environment variables.")

(defparameter *gemini-api-key-function* #'google:gemini-api-key
  "Function used to resolve the Gemini API key.")

(defparameter *web-search-function* #'google:web-search
  "Function used by the built-in web grounding search tool.")

(defparameter *hyperspec-search-function* #'google:hyperspec-search
  "Function used by the built-in HyperSpec grounding search tool.")

(defun default-filesystem-access-approval-function (bot directory tool-name)
  "Prompts the user to approve BOT access to DIRECTORY for TOOL-NAME."
  (declare (ignore bot))
  (y-or-n-p "~&Allow ~A to access directory ~A and remember it for this persona? "
            tool-name
            (namestring directory)))

(defparameter *filesystem-access-approval-function* #'default-filesystem-access-approval-function
  "Function used to approve persona filesystem access outside the current allowlist.")

(defparameter *bypass-eval-approval-p* nil
  "When T, bypasses interactive evaluation approval and automatically returns T.")

(defun default-eval-approval-function (bot source tool-name)
  "Prompts the user to approve evaluating SOURCE for TOOL-NAME."
  (declare (ignore bot))
  (or *bypass-eval-approval-p*
      (y-or-n-p "~&Allow ~A to evaluate this expression?~%~A~% "
                tool-name
                source)))

(defparameter *eval-approval-function* #'default-eval-approval-function
  "Function used to approve evaluation of a specific expression for the eval tool.")

(defparameter *user-homedir-pathname-function* #'user-homedir-pathname
  "Function used to resolve the current user's home directory pathname.")

(defparameter *read-mcp-config-function* nil
  "Optional test seam for reading MCP configuration.")

(defparameter *start-mcp-server-function* nil
  "Optional test seam for launching an MCP server.")

(defparameter *stop-mcp-server-function* nil
  "Optional test seam for stopping an MCP server.")

(defparameter *mcp-send-request-function* nil
  "Optional test seam for sending an MCP JSON-RPC request.")

(defparameter *mcp-debug-p* nil
  "Global flag controlling whether verbose MCP JSON-RPC and lifecycle debug messages are logged.")

(defparameter *mcp-initialize-function* nil
  "Optional test seam for performing the MCP initialize handshake.")

(defparameter *mcp-call-tool-function* nil
  "Optional test seam for invoking an MCP tool call.")

(defparameter *initialize-mcp-servers-for-chatbot-function* nil
  "Optional test seam for startup MCP initialization orchestration.")

(defparameter *get-all-mcp-tools-function* nil
  "Optional test seam for enumerating all MCP tools for a chatbot.")

(defparameter *find-mcp-server-and-tool-function* nil
  "Optional test seam for resolving an MCP tool by name.")

(defparameter *execute-mcp-tool-function* nil
  "Optional test seam for executing an MCP tool and returning text content.")

(defun default-persona-memory-compression-thread-function (thunk thread-name)
  "Starts a background thread for persona memory compression."
  (sb-thread:make-thread thunk :name thread-name))

(defparameter *persona-memory-compression-thread-function*
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

(defparameter *backend-default-models*
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

(defparameter *lm-studio-base-url* "http://127.0.0.1:1234"
  "The host root for the local LM Studio API.")

(defun lm-studio-api-base-url ()
  "Returns the normalized OpenAI-compatible LM Studio API base URL."
  (let ((base-url (string-right-trim "/" *lm-studio-base-url*)))
    (if (alexandria:ends-with-subseq "/v1" base-url)
        base-url
        (concatenate 'string base-url "/v1"))))

(defparameter *lm-studio-default-api-key* "lm_studio"
  "Fallback API key used when LM Studio credentials are otherwise unset.")

(defparameter *lm-studio-api-key* nil
  "The API key for the LM Studio API.")

(defparameter *lm-studio-http-read-timeout* 600
  "Minimum HTTP response timeout in seconds for the LM Studio backend.")

(defparameter +default-content-cache-policy+ :auto
  "Default content-caching policy for chatbots.")

(defparameter *default-content-cache-ttl-seconds* 3600
  "Default TTL in seconds for newly created explicit Gemini content caches.")

(defparameter *default-content-cache-min-tokens* 2048
  "Default estimated token threshold before automatic explicit cache creation is attempted.")

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

(defun normalize-content-cache-policy (policy &key allow-nil-p)
  "Returns POLICY normalized to a supported content-caching keyword."
  (when (null policy)
    (if allow-nil-p
        (return-from normalize-content-cache-policy nil)
        (error "Content cache policy is required.")))
  (let ((normalized
          (typecase policy
            (keyword policy)
            (string (intern (string-upcase policy) "KEYWORD"))
            (t nil))))
    (unless (member normalized '(:auto :off))
      (error "Unsupported content cache policy: ~S" policy))
    normalized))

(defun normalize-content-cache-ttl-seconds (ttl-seconds &key allow-nil-p)
  "Returns TTL-SECONDS validated as a positive integer or NIL."
  (when (null ttl-seconds)
    (if allow-nil-p
        (return-from normalize-content-cache-ttl-seconds nil)
        (error "Content cache TTL must not be NIL.")))
  (unless (and (integerp ttl-seconds)
               (> ttl-seconds 0))
    (error "Content cache TTL must be a positive integer number of seconds: ~S" ttl-seconds))
  ttl-seconds)

(defun normalize-content-cache-min-tokens (min-tokens &key allow-nil-p)
  "Returns MIN-TOKENS validated as a positive integer or NIL."
  (when (null min-tokens)
    (if allow-nil-p
        (return-from normalize-content-cache-min-tokens nil)
        (error "Content cache minimum token threshold must not be NIL.")))
  (unless (and (integerp min-tokens)
               (> min-tokens 0))
    (error "Content cache minimum token threshold must be a positive integer: ~S" min-tokens))
  min-tokens)

(defun gemini-api-key ()
  "Returns the Gemini API key using the current runtime seam."
  (funcall (current-gemini-api-key-function)))

(defparameter *gemini-api-revision* "2026-05-20"
  "API revision header value used for Gemini Interactions requests.")

(defun gemini-api-revision ()
  "Returns the configured Gemini API revision header value."
  (require-non-empty-string *gemini-api-revision* "Gemini API revision"))

(defparameter *http-post-function* #'dexador:post
  "Function used to perform HTTP POST requests.")

(defparameter *http-get-function* #'dexador:get
  "Function used to perform HTTP GET requests.")

(defparameter *http-patch-function* #'dexador:patch
  "Function used to perform HTTP PATCH requests.")

(defparameter *http-delete-function* #'dexador:delete
  "Function used to perform HTTP DELETE requests.")

(defun eager-mcp-startup-enabled-p ()
  "Returns true when eager shared MCP startup is enabled via environment."
  (let ((value (funcall *getenv-function* "CHATBOT_EAGER_MCP_STARTUP")))
    (and value
         (member (string-downcase value)
                 '("1" "true" "yes" "on")
                 :test #'string=))))

(defvar *active-runtime-context* nil
  "Runtime context currently bound by CALL-WITH-RUNTIME-CONTEXT, when any.")

(defvar *default-runtime-context* nil
  "Canonical runtime context used for legacy no-context entry points.")

(defvar *active-conversation* nil
  "Conversation currently being processed by CHAT, when any.")

(defun runtime-context-accessor-value (context accessor)
  "Reads ACCESSOR from CONTEXT."
  (funcall accessor context))

(defun set-runtime-context-accessor-value (context accessor value)
  "Stores VALUE on CONTEXT through ACCESSOR."
  (funcall (fdefinition (list 'setf accessor)) value context))

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
                     :default-conversation (inherit default-conversation-p
                                                   default-conversation
                                                   #'runtime-context-default-conversation
                                                   nil)
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

(defun default-runtime-context-p (context)
  "Returns true when CONTEXT is the canonical default runtime context."
  (and context
       (eq context *default-runtime-context*)))

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
      (sync-default-conversation-from-legacy-global resolved))
    resolved))

(defun runtime-context-function-seam-value (context accessor legacy-symbol)
  "Returns the function seam value for ACCESSOR and LEGACY-SYMBOL."
  (let ((resolved-context (and context
                              (resolve-runtime-context context))))
    (cond
      ((and (null context)
            *active-runtime-context*
            (not (default-runtime-context-p *active-runtime-context*)))
       (runtime-context-accessor-value *active-runtime-context* accessor))
      (resolved-context
       (runtime-context-accessor-value resolved-context accessor))
      (t
       (legacy-global-value legacy-symbol)))))

(defun set-runtime-context-function-seam-value (value context accessor legacy-symbol)
  "Stores VALUE through the function seam bridge for ACCESSOR and LEGACY-SYMBOL."
  (let ((resolved-context (and context
                              (resolve-runtime-context context))))
    (cond
      ((and (null context)
            *active-runtime-context*
            (not (default-runtime-context-p *active-runtime-context*)))
       (set-runtime-context-accessor-value *active-runtime-context* accessor value))
      (resolved-context
       (set-runtime-context-accessor-value resolved-context accessor value))
      (t
       (setf (symbol-value legacy-symbol) value)
       (when (default-runtime-context-p *default-runtime-context*)
         (set-runtime-context-accessor-value *default-runtime-context* accessor value)))))
  value)

(defmacro define-runtime-context-function-seam-helper (name accessor legacy-symbol getter-doc setter-doc)
  `(progn
     (defun ,name (&optional context)
       ,getter-doc
       (runtime-context-function-seam-value context
                                          ',accessor
                                          ',legacy-symbol))
     (defun (setf ,name) (value &optional context)
       ,setter-doc
       (set-runtime-context-function-seam-value value
                                              context
                                              ',accessor
                                              ',legacy-symbol))))

(defun runtime-context-approval-function-value (context accessor legacy-symbol)
  "Returns the approval function seam value for ACCESSOR and LEGACY-SYMBOL."
  (let ((resolved-context (and context
                              (resolve-runtime-context context))))
    (cond
      ((and (null context)
            *active-runtime-context*
            (not (default-runtime-context-p *active-runtime-context*)))
       (runtime-context-accessor-value *active-runtime-context* accessor))
      (resolved-context
       (if (and (active-runtime-context-p resolved-context)
                (default-runtime-context-p resolved-context))
           (legacy-global-value legacy-symbol)
           (runtime-context-accessor-value resolved-context accessor)))
      (t
       (legacy-global-value legacy-symbol)))))

(defun set-runtime-context-approval-function-value (value context accessor legacy-symbol)
  "Stores approval VALUE through the runtime-context bridge.
Default-context active execution still mirrors the legacy special binding for
compatibility with approval overrides."
  (let ((resolved-context (and context
                              (resolve-runtime-context context))))
    (cond
      ((and (null context)
            *active-runtime-context*
            (not (default-runtime-context-p *active-runtime-context*)))
       (set-runtime-context-accessor-value *active-runtime-context* accessor value))
      (resolved-context
       (progn
         (set-runtime-context-accessor-value resolved-context accessor value)
         (when (and (active-runtime-context-p resolved-context)
                    (default-runtime-context-p resolved-context))
           (setf (symbol-value legacy-symbol) value))))
      (t
       (setf (symbol-value legacy-symbol) value)
       (when (default-runtime-context-p *default-runtime-context*)
         (set-runtime-context-accessor-value *default-runtime-context* accessor value)))))
  value)

(defmacro define-runtime-context-approval-function-helper (name accessor legacy-symbol getter-doc setter-doc)
  `(progn
     (defun ,name (&optional context)
       ,getter-doc
       (runtime-context-approval-function-value context
                                              ',accessor
                                              ',legacy-symbol))
     (defun (setf ,name) (value &optional context)
       ,setter-doc
       (set-runtime-context-approval-function-value value
                                                  context
                                                  ',accessor
                                                  ',legacy-symbol))))

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
  "Returns transient runtime state from CONTEXT, falling back to legacy globals only
for default-context compatibility."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and resolved-context
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-accessor-value resolved-context accessor))
      (resolved-context
       (or (runtime-context-accessor-value resolved-context accessor)
           (legacy-global-value legacy-symbol)))
      (t
       (legacy-global-value legacy-symbol)))))

(defun set-transient-runtime-context-value (value context accessor legacy-symbol)
  "Stores transient runtime state in CONTEXT, mirroring to legacy globals only for
the canonical default runtime context."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            *active-runtime-context*
            (not (default-runtime-context-p *active-runtime-context*)))
       (set-runtime-context-accessor-value *active-runtime-context* accessor value))
      (resolved-context
       (set-runtime-context-accessor-value resolved-context accessor value)
       (when (default-runtime-context-p resolved-context)
         (setf (symbol-value legacy-symbol) value)))
      (t
       (setf (symbol-value legacy-symbol) value)
       (when (default-runtime-context-p *default-runtime-context*)
         (set-runtime-context-accessor-value *default-runtime-context* accessor value)))))
  value)

(defmacro define-transient-runtime-context-helper (name accessor legacy-symbol getter-doc setter-doc)
  `(progn
     (defun ,name (&optional context)
       ,getter-doc
       (transient-runtime-context-value context ',accessor ',legacy-symbol))
     (defun (setf ,name) (value &optional context)
       ,setter-doc
       (set-transient-runtime-context-value value context ',accessor ',legacy-symbol))))

(defun current-default-conversation (&optional context)
  "Returns the ambient default conversation for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((null resolved-context)
       *default-conversation*)
      ((default-runtime-context-p resolved-context)
       (sync-default-conversation-from-legacy-global resolved-context)
       (runtime-context-default-conversation resolved-context))
      (t
       (runtime-context-default-conversation resolved-context)))))

(defun (setf current-default-conversation) (value &optional context)
  "Sets the ambient default conversation for CONTEXT."
  (let ((resolved-context (or (resolve-runtime-context context)
                              *default-runtime-context*)))
    (when resolved-context
      (setf (runtime-context-default-conversation resolved-context) value)
      (when (default-runtime-context-p resolved-context)
        (setf *default-conversation* value))))
  value)

(defun call-with-default-conversation-compatibility (context thunk)
  "Calls THUNK with CONTEXT's default conversation mirrored through the legacy special.
Only the canonical default runtime context still requires this compatibility shell."
  (let ((*default-conversation*
         (runtime-context-default-conversation context)))
    (unwind-protect
         (funcall thunk)
      (setf (runtime-context-default-conversation context)
           *default-conversation*))))

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

(define-runtime-context-function-seam-helper current-getenv-function
  runtime-context-getenv-function
  *getenv-function*
  "Returns the current environment lookup function for CONTEXT."
  "Sets the current environment lookup function for CONTEXT.")

(define-runtime-context-function-seam-helper current-http-post-function
  runtime-context-http-post-function
  *http-post-function*
  "Returns the current HTTP POST function for CONTEXT."
  "Sets the current HTTP POST function for CONTEXT.")

(define-runtime-context-function-seam-helper current-http-get-function
  runtime-context-http-get-function
  *http-get-function*
  "Returns the current HTTP GET function for CONTEXT."
  "Sets the current HTTP GET function for CONTEXT.")

(define-runtime-context-function-seam-helper current-http-patch-function
  runtime-context-http-patch-function
  *http-patch-function*
  "Returns the current HTTP PATCH function for CONTEXT."
  "Sets the current HTTP PATCH function for CONTEXT.")

(define-runtime-context-function-seam-helper current-http-delete-function
  runtime-context-http-delete-function
  *http-delete-function*
  "Returns the current HTTP DELETE function for CONTEXT."
  "Sets the current HTTP DELETE function for CONTEXT.")

(define-runtime-context-function-seam-helper current-gemini-api-key-function
  runtime-context-gemini-api-key-function
  *gemini-api-key-function*
  "Returns the current Gemini API key lookup function for CONTEXT."
  "Sets the current Gemini API key lookup function for CONTEXT.")

(define-runtime-context-approval-function-helper current-filesystem-access-approval-function
  runtime-context-filesystem-access-approval-function
  *filesystem-access-approval-function*
  "Returns the current filesystem access approval function for CONTEXT."
  "Sets the current filesystem access approval function for CONTEXT.")

(define-runtime-context-approval-function-helper current-eval-approval-function
  runtime-context-eval-approval-function
  *eval-approval-function*
  "Returns the current eval approval function for CONTEXT."
  "Sets the current eval approval function for CONTEXT.")

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

(defun call-with-runtime-context (context thunk
                                 &key
                                   (default-conversation-compatibility-p t)
                                   (legacy-function-seam-compatibility-p t))
  "Calls THUNK with the resolved runtime context active.
Only the canonical default runtime context still requires *DEFAULT-CONVERSATION*
legacy rebinding. Function seams now resolve through the active runtime context
directly, and legacy function/approval seam synchronization only runs when that
default-context compatibility is still desired."
  (let* ((resolved-context (resolve-runtime-context
                           context
                           :sync-from-globals-p default-conversation-compatibility-p))
         (default-context-p (default-runtime-context-p resolved-context))
         (default-conversation-compatibility-active-p
           (and default-context-p default-conversation-compatibility-p))
         (legacy-function-seam-compatibility-active-p
          (and default-context-p legacy-function-seam-compatibility-p)))
    (cond
      ((null resolved-context)
       (funcall thunk))
      ((active-runtime-context-p resolved-context)
       (funcall thunk))
      (t
       (let ((result
               (let ((*active-runtime-context* resolved-context))
                 (unwind-protect
                      (if default-conversation-compatibility-active-p
                          (call-with-default-conversation-compatibility resolved-context thunk)
                          (funcall thunk))
                   (when legacy-function-seam-compatibility-active-p
                     (setf (runtime-context-getenv-function resolved-context) *getenv-function*)
                     (setf (runtime-context-http-post-function resolved-context) *http-post-function*)
                     (setf (runtime-context-http-get-function resolved-context) *http-get-function*)
                     (setf (runtime-context-http-patch-function resolved-context) *http-patch-function*)
                     (setf (runtime-context-http-delete-function resolved-context) *http-delete-function*)
                     (setf (runtime-context-gemini-api-key-function resolved-context) *gemini-api-key-function*)
                     (setf (runtime-context-filesystem-access-approval-function resolved-context)
                           *filesystem-access-approval-function*)
                     (setf (runtime-context-eval-approval-function resolved-context)
                           *eval-approval-function*))))))
         (when default-conversation-compatibility-active-p
           (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
         result)))))

(setf *default-runtime-context* (make-runtime-context))

(defparameter *max-minion-depth* 3
  "The global maximum nesting depth allowed for the minion hierarchy.")

(defparameter *context-pruning-estimated-max-tokens* 200000
  "Estimated prompt-token ceiling above which completed conversation history is auto-compressed.")

(defparameter *context-pruning-estimated-target-tokens* 150000
  "Estimated prompt-token target after compressing oversized conversation history.")

(defparameter *context-pruning-threshold-characters* 800000
  "Compatibility character ceiling for auto-pruning, aligned with the default estimated token window.")

(defvar *active-planner* nil
  "Tracks the active planner minion conversation, or NIL if not in Planner Mode.")

(defvar *active-planner-parent-conversation* nil
  "Tracks the parent conversation that spawned the active planner minion.")
