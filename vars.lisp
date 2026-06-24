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

(defun default-eval-approval-function (bot source tool-name)
  "Prompts the user to approve evaluating SOURCE for TOOL-NAME."
  (declare (ignore bot))
  (y-or-n-p "~&Allow ~A to evaluate this expression?~%~A~% "
            tool-name
            source))

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

(defvar *global-token-grand-totals* nil
  "Process-wide cumulative token totals shared across unrelated chats.")

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
      (funcall (current-getenv-function) "OPENAI_API_KEY")))

(defvar *lm-studio-base-url* "http://127.0.0.1:8088/v1"
  "The base REST endpoint for the local LM Studio API.")

(defvar *lm-studio-default-api-key* "lm_studio"
  "Fallback API key used when LM Studio credentials are otherwise unset.")

(defvar *lm-studio-api-key* nil
  "The API key for the LM Studio API.")

(defun lm-studio-api-key ()
  "Returns the LM Studio API key. First checks *lm-studio-api-key*, then the LM_API_TOKEN environment variable."
  (or *lm-studio-api-key*
      (funcall (current-getenv-function) "LM_API_TOKEN")
      (require-non-empty-string *lm-studio-default-api-key* "LM Studio default API key")))

(defun gemini-api-key ()
  "Returns the Gemini API key using the current runtime seam."
  (funcall (current-gemini-api-key-function)))

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
    (*http-read-timeout* . "MAKE-RUNTIME-CONTEXT with :HTTP-READ-TIMEOUT")
    (*filesystem-access-approval-function* . "MAKE-RUNTIME-CONTEXT with :FILESYSTEM-ACCESS-APPROVAL-FUNCTION")
    (*eval-approval-function* . "MAKE-RUNTIME-CONTEXT with :EVAL-APPROVAL-FUNCTION"))
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

(defparameter *runtime-context-legacy-global-specs*
  '()
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
                                  (gemini-api-key-function nil gemini-api-key-function-p)
                                  (filesystem-access-approval-function nil filesystem-access-approval-function-p)
                                  (eval-approval-function nil eval-approval-function-p)
                                  (default-conversation nil default-conversation-p))
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
                                               *mcp-config-path*)
                     :startup-chatbot (if startup-chatbot-p
                                          startup-chatbot
                                          nil)
                     :auto-initialize-startup-mcp-servers-p
                     (inherit auto-init-p
                              auto-initialize-startup-mcp-servers-p
                              #'runtime-context-auto-initialize-startup-mcp-servers-p
                              *auto-initialize-startup-mcp-servers-p*)
                     :logging-enabled-p (inherit logging-enabled-p-p
                                                 logging-enabled-p
                                                 #'runtime-context-logging-enabled-p
                                                 *logging-enabled-p*)
                     :log-level (inherit log-level-p
                                         log-level
                                         #'runtime-context-log-level
                                         *log-level*)
                     :log-stream (inherit log-stream-p
                                          log-stream
                                          #'runtime-context-log-stream
                                          *log-stream*)
                     :http-connect-timeout (inherit http-connect-timeout-p
                                                    http-connect-timeout
                                                    #'runtime-context-http-connect-timeout
                                                    *http-connect-timeout*)
                     :http-read-timeout (inherit http-read-timeout-p
                                                 http-read-timeout
                                                 #'runtime-context-http-read-timeout
                                                 *http-read-timeout*)
                     :getenv-function (inherit getenv-function-p
                                               getenv-function
                                               #'runtime-context-getenv-function
                                               *getenv-function*)
                     :http-post-function (inherit http-post-function-p
                                                  http-post-function
                                                  #'runtime-context-http-post-function
                                                  *http-post-function*)
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

(defun resolve-runtime-context (context &key sync-from-globals-p)
  "Returns CONTEXT, otherwise the active context, otherwise the canonical default context."
  (let ((resolved (or context
                     *active-runtime-context*
                     *default-runtime-context*)))
    (when (and sync-from-globals-p
               (default-runtime-context-p resolved))
      (sync-runtime-context-from-legacy-globals resolved))
    resolved))

(defun current-default-conversation (&optional context)
  "Returns the ambient default conversation for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-default-conversation resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-default-conversation resolved-context) *default-conversation*))
       (and resolved-context
            (runtime-context-default-conversation resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-default-conversation resolved-context) *default-conversation*))
       *default-conversation*))))

(defun (setf current-default-conversation) (value &optional context)
  "Sets the ambient default conversation for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-default-conversation resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-default-conversation resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *default-conversation* value))))
      (t
       (setf *default-conversation* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-default-conversation resolved-context) value))))
    value))

(defun current-mcp-config-path (&optional context)
  "Returns the ambient MCP configuration override path for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-mcp-config-path resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-mcp-config-path resolved-context) *mcp-config-path*))
       (and resolved-context
            (runtime-context-mcp-config-path resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-mcp-config-path resolved-context) *mcp-config-path*))
       *mcp-config-path*))))

(defun (setf current-mcp-config-path) (value &optional context)
  "Sets the ambient MCP configuration override path for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-mcp-config-path resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-mcp-config-path resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *mcp-config-path* value))))
      (t
       (setf *mcp-config-path* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-mcp-config-path resolved-context) value))))
    value))

(defun current-startup-chatbot (&optional context)
  "Returns the shared startup chatbot for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-startup-chatbot resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-startup-chatbot resolved-context) *startup-chatbot*))
       (and resolved-context
            (runtime-context-startup-chatbot resolved-context)))
      (t *startup-chatbot*))))

(defun (setf current-startup-chatbot) (value &optional context)
  "Sets the shared startup chatbot for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-startup-chatbot resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-startup-chatbot resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *startup-chatbot* value))))
      (t
       (setf *startup-chatbot* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-startup-chatbot resolved-context) value))))
    value))

(defun current-auto-initialize-startup-mcp-servers-p (&optional context)
  "Returns whether startup MCP auto-initialization is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context)
               *auto-initialize-startup-mcp-servers-p*))
       (and resolved-context
            (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context)
               *auto-initialize-startup-mcp-servers-p*))
       *auto-initialize-startup-mcp-servers-p*))))

(defun (setf current-auto-initialize-startup-mcp-servers-p) (value &optional context)
  "Sets whether startup MCP auto-initialization is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *auto-initialize-startup-mcp-servers-p* value))))
      (t
       (setf *auto-initialize-startup-mcp-servers-p* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-auto-initialize-startup-mcp-servers-p resolved-context) value))))
    value))

(defun current-logging-enabled-p (&optional context)
  "Returns whether logging is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-logging-enabled-p resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-logging-enabled-p resolved-context) *logging-enabled-p*))
       (and resolved-context
            (runtime-context-logging-enabled-p resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-logging-enabled-p resolved-context) *logging-enabled-p*))
       *logging-enabled-p*))))

(defun (setf current-logging-enabled-p) (value &optional context)
  "Sets whether logging is enabled for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-logging-enabled-p resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-logging-enabled-p resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *logging-enabled-p* value))))
      (t
       (setf *logging-enabled-p* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-logging-enabled-p resolved-context) value))))
    value))

(defun current-log-level (&optional context)
  "Returns the current log level for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-log-level resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-log-level resolved-context) *log-level*))
       (and resolved-context
            (runtime-context-log-level resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-log-level resolved-context) *log-level*))
       *log-level*))))

(defun (setf current-log-level) (value &optional context)
  "Sets the current log level for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-log-level resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-log-level resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *log-level* value))))
      (t
       (setf *log-level* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-log-level resolved-context) value))))
    value))

(defun current-log-stream (&optional context)
  "Returns the current log stream for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-log-stream resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-log-stream resolved-context) *log-stream*))
       (and resolved-context
            (runtime-context-log-stream resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-log-stream resolved-context) *log-stream*))
       *log-stream*))))

(defun (setf current-log-stream) (value &optional context)
  "Sets the current log stream for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-log-stream resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-log-stream resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *log-stream* value))))
      (t
       (setf *log-stream* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-log-stream resolved-context) value))))
    value))

(defun current-http-connect-timeout (&optional context)
  "Returns the current HTTP connect timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-http-connect-timeout resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-http-connect-timeout resolved-context) *http-connect-timeout*))
       (and resolved-context
            (runtime-context-http-connect-timeout resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-http-connect-timeout resolved-context) *http-connect-timeout*))
       *http-connect-timeout*))))

(defun (setf current-http-connect-timeout) (value &optional context)
  "Sets the current HTTP connect timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-http-connect-timeout resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-http-connect-timeout resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *http-connect-timeout* value))))
      (t
       (setf *http-connect-timeout* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-http-connect-timeout resolved-context) value))))
    value))

(defun current-http-read-timeout (&optional context)
  "Returns the current HTTP read timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (runtime-context-http-read-timeout resolved-context))
      (context
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-http-read-timeout resolved-context) *http-read-timeout*))
       (and resolved-context
            (runtime-context-http-read-timeout resolved-context)))
      (t
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-http-read-timeout resolved-context) *http-read-timeout*))
       *http-read-timeout*))))

(defun (setf current-http-read-timeout) (value &optional context)
  "Sets the current HTTP read timeout for CONTEXT."
  (let ((resolved-context (resolve-runtime-context context)))
    (cond
      ((and (null context)
            resolved-context
            (active-runtime-context-p resolved-context)
            (not (default-runtime-context-p resolved-context)))
       (setf (runtime-context-http-read-timeout resolved-context) value))
      (context
       (when resolved-context
         (setf (runtime-context-http-read-timeout resolved-context) value)
         (when (default-runtime-context-p resolved-context)
           (setf *http-read-timeout* value))))
      (t
       (setf *http-read-timeout* value)
       (when (default-runtime-context-p resolved-context)
         (setf (runtime-context-http-read-timeout resolved-context) value))))
    value))

(defun current-getenv-function (&optional context)
  "Returns the current environment lookup function for CONTEXT."
  (if context
      (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
       (if resolved-context
           (if (active-runtime-context-p resolved-context)
               *getenv-function*
               (runtime-context-getenv-function resolved-context))
           *getenv-function*))
      *getenv-function*))

(defun (setf current-getenv-function) (value &optional context)
  "Sets the current environment lookup function for CONTEXT."
  (let ((resolved-context (and context
                              (resolve-runtime-context context :sync-from-globals-p t))))
    (if resolved-context
       (progn
         (setf (runtime-context-getenv-function resolved-context) value)
         (when (active-runtime-context-p resolved-context)
           (setf *getenv-function* value)))
       (setf *getenv-function* value))
    value))

(defun current-http-post-function (&optional context)
  "Returns the current HTTP POST function for CONTEXT."
  (if context
      (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
       (if resolved-context
           (if (active-runtime-context-p resolved-context)
               *http-post-function*
               (runtime-context-http-post-function resolved-context))
           *http-post-function*))
      *http-post-function*))

(defun (setf current-http-post-function) (value &optional context)
  "Sets the current HTTP POST function for CONTEXT."
  (let ((resolved-context (and context
                              (resolve-runtime-context context :sync-from-globals-p t))))
    (if resolved-context
       (progn
         (setf (runtime-context-http-post-function resolved-context) value)
         (when (active-runtime-context-p resolved-context)
           (setf *http-post-function* value)))
       (setf *http-post-function* value))
    value))

(defun current-gemini-api-key-function (&optional context)
  "Returns the current Gemini API key lookup function for CONTEXT."
  (if context
      (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
       (if resolved-context
           (if (active-runtime-context-p resolved-context)
               *gemini-api-key-function*
               (runtime-context-gemini-api-key-function resolved-context))
           *gemini-api-key-function*))
      *gemini-api-key-function*))

(defun (setf current-gemini-api-key-function) (value &optional context)
  "Sets the current Gemini API key lookup function for CONTEXT."
  (let ((resolved-context (and context
                              (resolve-runtime-context context :sync-from-globals-p t))))
    (if resolved-context
       (progn
         (setf (runtime-context-gemini-api-key-function resolved-context) value)
         (when (active-runtime-context-p resolved-context)
           (setf *gemini-api-key-function* value)))
       (setf *gemini-api-key-function* value))
    value))

(defun current-filesystem-access-approval-function (&optional context)
  "Returns the current filesystem access approval function for CONTEXT."
  (if context
      (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
        (and resolved-context
             (if (or (active-runtime-context-p resolved-context)
                     (default-runtime-context-p resolved-context))
                 *filesystem-access-approval-function*
                 (runtime-context-filesystem-access-approval-function resolved-context))))
      *filesystem-access-approval-function*))

(defun (setf current-filesystem-access-approval-function) (value &optional context)
  "Sets the current filesystem access approval function for CONTEXT."
  (let ((resolved-context (and context
                               (resolve-runtime-context context :sync-from-globals-p t))))
    (if resolved-context
        (progn
          (setf (runtime-context-filesystem-access-approval-function resolved-context) value)
          (when (or (active-runtime-context-p resolved-context)
                    (default-runtime-context-p resolved-context))
            (setf *filesystem-access-approval-function* value)))
        (setf *filesystem-access-approval-function* value))
    value))

(defun current-eval-approval-function (&optional context)
  "Returns the current eval approval function for CONTEXT."
  (if context
      (let ((resolved-context (resolve-runtime-context context :sync-from-globals-p t)))
        (and resolved-context
             (if (or (active-runtime-context-p resolved-context)
                     (default-runtime-context-p resolved-context))
                 *eval-approval-function*
                 (runtime-context-eval-approval-function resolved-context))))
      *eval-approval-function*))

(defun (setf current-eval-approval-function) (value &optional context)
  "Sets the current eval approval function for CONTEXT."
  (let ((resolved-context (and context
                               (resolve-runtime-context context :sync-from-globals-p t))))
    (if resolved-context
        (progn
          (setf (runtime-context-eval-approval-function resolved-context) value)
          (when (or (active-runtime-context-p resolved-context)
                    (default-runtime-context-p resolved-context))
            (setf *eval-approval-function* value)))
        (setf *eval-approval-function* value))
    value))

(defun call-with-runtime-context (context thunk)
  "Calls THUNK with legacy special variables rebound from CONTEXT when CONTEXT is non-nil."
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
                           (*gemini-api-key-function* (if default-context-p
                                                          *gemini-api-key-function*
                                                          (runtime-context-gemini-api-key-function resolved-context))))
                       (progv (runtime-context-legacy-global-symbols)
                              (runtime-context-legacy-global-values resolved-context)
                         (unwind-protect
                              (funcall thunk)
                           (sync-runtime-context-from-legacy-globals resolved-context :warn-p nil)
                           (setf (runtime-context-getenv-function resolved-context) *getenv-function*)
                           (setf (runtime-context-http-post-function resolved-context) *http-post-function*)
                           (setf (runtime-context-gemini-api-key-function resolved-context) *gemini-api-key-function*))))))
              (when default-context-p
                (maybe-sync-legacy-globals-from-default-runtime-context resolved-context))
              result)))))

(setf *default-runtime-context* (make-runtime-context))
