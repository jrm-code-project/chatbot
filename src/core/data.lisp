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
   (http-get-function
    :initarg :http-get-function
    :accessor runtime-context-http-get-function
    :initform #'dexador:get
    :documentation "HTTP GET function for this runtime context.")
   (http-patch-function
    :initarg :http-patch-function
    :accessor runtime-context-http-patch-function
    :initform #'dexador:patch
    :documentation "HTTP PATCH function for this runtime context.")
   (http-delete-function
    :initarg :http-delete-function
    :accessor runtime-context-http-delete-function
    :initform #'dexador:delete
    :documentation "HTTP DELETE function for this runtime context.")
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
    :documentation "Default conversation associated with this runtime context.")
   (agentic-loop-default-backend
    :initarg :agentic-loop-default-backend
    :accessor runtime-context-agentic-loop-default-backend
    :initform nil
    :documentation "Optional default backend for new agentic loops in this runtime context.")
   (agentic-loop-default-model
    :initarg :agentic-loop-default-model
    :accessor runtime-context-agentic-loop-default-model
    :initform nil
    :documentation "Optional default model for new agentic loops in this runtime context.")
   (agentic-loop-registry
    :initarg :agentic-loop-registry
    :accessor runtime-context-agentic-loop-registry
    :initform (make-hash-table)
    :documentation "The active agentic loops registered in this runtime context.")
   (agentic-loop-registry-lock
    :initarg :agentic-loop-registry-lock
    :accessor runtime-context-agentic-loop-registry-lock
    :initform (sb-thread:make-mutex :name "agentic-loop-registry-lock")
    :documentation "Lock protecting agentic-loop-registry updates.")
   (worker-registry
    :initarg :worker-registry
    :accessor runtime-context-worker-registry
    :initform (make-hash-table :test 'equal)
    :documentation "Unified worker registry keyed by workerId for delegated, planner, and autonomous workers.")
   (worker-registry-lock
    :initarg :worker-registry-lock
    :accessor runtime-context-worker-registry-lock
    :initform (sb-thread:make-mutex :name "worker-registry-lock")
    :documentation "Lock protecting unified worker-registry updates.")
   (active-conversation
    :initarg :active-conversation
    :accessor runtime-context-active-conversation
    :initform nil
    :documentation "Conversation currently being processed in this runtime context.")
   (active-planner
    :initarg :active-planner
    :accessor runtime-context-active-planner
    :initform nil
    :documentation "Planner conversation currently intercepting chat turns in this runtime context.")
   (active-planner-parent-conversation
    :initarg :active-planner-parent-conversation
    :accessor runtime-context-active-planner-parent-conversation
    :initform nil
    :documentation "Parent conversation associated with the active planner in this runtime context.")))

(defparameter +default-gemini-fallback-to-google-p+ nil
  "Authoritative default for the Gemini Interactions compatibility fallback.")

(defclass chatbot ()
  ((persona-name
    :initarg :persona-name
    :accessor chatbot-persona-name
    :initform nil
    :documentation "Optional name of the persona associated with this chatbot.")
   (checkpoint-name
   :initarg :checkpoint-name
   :accessor chatbot-checkpoint-name
   :initform nil
   :documentation "Optional explicit persistence identifier used for checkpoint filenames and restore identity.")
   (model
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
    :documentation "Optional system instructions directing the chatbot behavior, stored as either a string or a vector of paragraph strings.")
   (system-instruction-path
    :initarg :system-instruction-path
    :accessor chatbot-system-instruction-path
    :initform nil
    :documentation "Optional backing file pathname for system instructions.")
   (system-instruction-storage-kind
    :initarg :system-instruction-storage-kind
    :accessor chatbot-system-instruction-storage-kind
    :initform :transient
    :documentation "Persistence kind for system instructions (:transient, :markdown-file, or :paragraph-file).")
   (temperature
    :initarg :temperature
    :accessor chatbot-temperature
    :initform nil
    :documentation "Optional default sampling temperature for this chatbot. NIL uses the provider default.")
   (top-p
    :initarg :top-p
    :accessor chatbot-top-p
    :initform nil
    :documentation "Optional default nucleus sampling top-p for this chatbot. NIL uses the provider default.")
   (content-cache-policy
    :initarg :content-cache-policy
    :accessor chatbot-content-cache-policy
    :initform :auto
    :documentation "Content-caching policy for this chatbot (:auto or :off).")
   (content-cache-ttl-seconds
    :initarg :content-cache-ttl-seconds
    :accessor chatbot-content-cache-ttl-seconds
    :initform nil
    :documentation "Optional TTL in seconds for newly created explicit content caches.")
   (content-cache-min-tokens
    :initarg :content-cache-min-tokens
    :accessor chatbot-content-cache-min-tokens
    :initform nil
    :documentation "Optional minimum estimated token threshold before automatic cache creation is attempted.")
   (google-search-p
    :initarg :google-search-p
    :accessor chatbot-google-search-p
    :initform nil
    :documentation "Flag to enable Google Search Grounding tool.")
   (gemini-fallback-to-google-p
    :initarg :gemini-fallback-to-google-p
    :accessor chatbot-gemini-fallback-to-google-p
    :initform +default-gemini-fallback-to-google-p+
    :documentation "Compatibility flag allowing Gemini Interactions requests to fall back to Google generateContent on 404-style endpoint errors.")
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
   (enable-git-tools-p
    :initarg :enable-git-tools-p
    :accessor chatbot-enable-git-tools-p
    :initform nil
    :documentation "Flag to enable built-in git tools for this chatbot.")
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
   (filesystem-read-only-p
    :initarg :filesystem-read-only-p
    :accessor chatbot-filesystem-read-only-p
    :initform nil
    :documentation "Flag indicating whether filesystem tools are restricted to read-only access (directory and readFileLines).")
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
   (subordinates
    :initarg :subordinates
    :accessor chatbot-subordinates
    :initform nil
    :documentation "A list of conversation objects or NIL.")
   (parent-name
    :initarg :parent-name
    :accessor chatbot-parent-name
    :initform nil
    :documentation "Optional name of the parent chatbot that spawned this minion.")
   (depth
    :initarg :depth
    :accessor chatbot-depth
    :initform 1
    :documentation "The hierarchical depth of this minion, where 1 is top-level.")
   (token-budget
    :initarg :token-budget
    :accessor chatbot-token-budget
    :initform nil
    :documentation "The token budget representing the allowed usage limit for this minion.")
   (spent-tokens
    :initarg :spent-tokens
    :accessor chatbot-spent-tokens
    :initform 0
    :documentation "The spent token usage tracked per prompt execution.")
   (scoped-directory
    :initarg :scoped-directory
    :accessor chatbot-scoped-directory
    :initform nil
    :documentation "The localized sandbox directory where built-in filesystem tools may operate.")
   (planner-p
    :initarg :planner-p
    :accessor chatbot-planner-p
    :initform nil
    :documentation "Flag indicating whether this chatbot is running as a Planner minion.")
   (runtime-context
    :initarg :runtime-context
    :accessor chatbot-runtime-context
    :initform nil
    :documentation "Optional runtime context carrying shared configuration and startup state.")
   (task-journal
    :initarg :task-journal
    :accessor chatbot-task-journal
    :initform (make-hash-table :test 'equal)
    :documentation "In-memory idempotency journal for agentic task executions keyed by immutable input.")
   (task-journal-lock
    :initarg :task-journal-lock
    :accessor chatbot-task-journal-lock
    :initform (sb-thread:make-mutex :name "chatbot-task-journal-lock")
    :documentation "Mutex protecting task-journal reads, writes, and wait coordination.")))

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
   (adaptive-context-pruning-max-tokens
    :initarg :adaptive-context-pruning-max-tokens
    :accessor conversation-adaptive-context-pruning-max-tokens
    :initform nil
    :documentation "Optional per-conversation estimated total-token ceiling updated after history compression.")
   (messages
    :initarg :messages
    :accessor conversation-messages
    :initform nil
    :documentation "Accumulated conversation messages for stateless backends (like OpenAI).")
   (cached-content-name
    :initarg :cached-content-name
    :accessor conversation-cached-content-name
    :initform nil
    :documentation "Active explicit Gemini cached-content resource name currently associated with this conversation.")
   (cached-content-key
    :initarg :cached-content-key
    :accessor conversation-cached-content-key
    :initform nil
    :documentation "Stable fingerprint for the explicit cached-content context currently associated with this conversation.")
   (cached-content-metadata
    :initarg :cached-content-metadata
    :accessor conversation-cached-content-metadata
    :initform nil
    :documentation "Optional raw cached-content metadata retained for observability and later reuse decisions.")))

(defclass round-robin-participant ()
  ((name
    :initarg :name
    :reader round-robin-participant-name
    :documentation "Human-readable name identifying this chatbot in round-robin transcripts.")
   (conversation
    :initarg :conversation
    :reader round-robin-participant-conversation
    :documentation "Conversation state backing this participant's turns.")))

(defclass round-robin-session ()
  ((participants
    :initarg :participants
    :reader round-robin-session-participants
    :documentation "Ordered round-robin participants.")
   (user-name
    :initarg :user-name
    :reader round-robin-session-user-name
    :initform "User"
    :documentation "Display name used for the human participant in the shared transcript.")
   (transcript
    :initarg :transcript
    :accessor round-robin-session-transcript
    :initform nil
    :documentation "Chronological shared transcript entries for the round-robin session.")
   (phase
    :initarg :phase
    :accessor round-robin-session-phase
    :initform :awaiting-user-input
    :documentation "Explicit orchestration state for round-robin scheduling.")
   (next-participant-index
    :initarg :next-participant-index
    :accessor round-robin-session-next-participant-index
    :initform 0
    :documentation "Index of the next participant allowed to run by the round-robin state machine.")))

(defclass persona ()
  ((name
    :initarg :name
    :reader persona-name
    :documentation "Unique runtime identifier for an active sandbox persona.")
   (conversation
    :initarg :conversation
    :reader persona-conversation
    :documentation "Conversation state backing this sandbox persona.")
   (history
    :initarg :history
    :accessor persona-history
    :initform nil
    :documentation "Provider-neutral user/assistant history replayed for sandbox persona turns.")
   (prompt-options
    :initarg :prompt-options
    :accessor persona-prompt-options
    :initform nil
    :documentation "Structured prompt-building options captured when this persona was spawned.")))

(defclass persona-registry ()
  ((personas
     :initarg :personas
     :reader persona-registry-personas
     :initform (make-hash-table :test 'equal)
     :documentation "Hash table mapping active sandbox persona names to PERSONA instances.")
   (lock
     :initarg :lock
     :reader persona-registry-lock
     :initform (sb-thread:make-mutex :name "persona-registry-lock")
     :documentation "Mutex protecting PERSONAS updates and reads.")))

(defun make-persona-registry ()
  "Returns a fresh explicit sandbox persona registry."
  (make-instance 'persona-registry))

(defun ensure-class-finalized (class)
  "Returns CLASS after finalizing it when required for slot introspection."
  (unless (sb-mop:class-finalized-p class)
    (sb-mop:finalize-inheritance class))
  class)

(defun copy-initargs-for-instance (instance &key ignored-slots)
  "Returns INSTANCE state as initargs, omitting any IGNORED-SLOTS by slot name."
  (let* ((class (ensure-class-finalized (class-of instance)))
         (ignored-slots (copy-list ignored-slots)))
    (mapcan (lambda (slot)
              (let* ((slot-name (sb-mop:slot-definition-name slot))
                     (initargs (sb-mop:slot-definition-initargs slot)))
                (unless (or (null initargs)
                            (member slot-name ignored-slots))
                  (let ((value (when (slot-boundp instance slot-name)
                                 (slot-value instance slot-name))))
                    (list (first initargs) value)))))
            (sb-mop:class-slots class))))

(defun merge-initarg-overrides (base-initargs override-initargs)
  "Returns BASE-INITARGS with OVERRIDE-INITARGS replacing duplicate initargs."
  (labels ((plist-pairs (plist)
             (if (endp plist)
                 nil
                 (cons (list (first plist) (second plist))
                       (plist-pairs (cddr plist))))))
    (let* ((base-pairs (plist-pairs base-initargs))
           (override-pairs (plist-pairs override-initargs))
           (override-keys (mapcar #'first override-pairs)))
      (append (mapcan (lambda (pair)
                        (unless (member (first pair) override-keys)
                          pair))
                      base-pairs)
              override-initargs))))

(defun clone-chatbot (bot &rest initarg-overrides)
  "Returns a shallow clone of BOT with INITARG-OVERRIDES applied."
  (apply #'make-instance 'chatbot
         (merge-initarg-overrides (copy-initargs-for-instance bot
                                                             :ignored-slots '(task-journal
                                                                              task-journal-lock))
                                  initarg-overrides)))

(defun clone-runtime-context (context &rest initarg-overrides)
  "Returns a shallow clone of CONTEXT with INITARG-OVERRIDES applied."
  (apply #'make-instance 'runtime-context
         (merge-initarg-overrides (copy-initargs-for-instance context)
                                  initarg-overrides)))

(defun clone-conversation (conversation &rest initarg-overrides)
  "Returns a shallow clone of CONVERSATION with INITARG-OVERRIDES applied."
  (apply #'make-instance 'conversation
         (merge-initarg-overrides (copy-initargs-for-instance conversation)
                                  initarg-overrides)))

(defun system-instruction-fence-line-p (line)
  "Returns true when LINE begins a Markdown triple-backtick fence."
  (alexandria:starts-with-subseq
   "```"
   (string-left-trim '(#\Space #\Tab) line)))

(defun split-system-instruction-into-paragraphs (text)
  "Splits TEXT into trimmed paragraphs, preserving blank lines inside fenced blocks."
  (labels ((state-value (state key)
             (getf state key))
           (make-state (&key paragraphs current-lines in-fence-p)
             (list :paragraphs paragraphs
                   :current-lines current-lines
                   :in-fence-p in-fence-p))
           (append-current-line (state line)
             (make-state :paragraphs (state-value state :paragraphs)
                         :current-lines (append (state-value state :current-lines)
                                                (list line))
                         :in-fence-p (state-value state :in-fence-p)))
           (flush-paragraph (state)
             (let ((current-lines (state-value state :current-lines)))
               (if (null current-lines)
                   state
                   (let* ((paragraph (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                                  (format nil "~{~A~^~%~}" current-lines)))
                          (paragraphs (state-value state :paragraphs)))
                     (make-state :paragraphs (if (string= paragraph "")
                                                 paragraphs
                                                 (append paragraphs (list paragraph)))
                                 :current-lines nil
                                 :in-fence-p (state-value state :in-fence-p))))))
           (accumulate-line (state line)
             (cond
               ((state-value state :in-fence-p)
                (let ((updated-state (append-current-line state line)))
                  (if (system-instruction-fence-line-p line)
                      (make-state :paragraphs (state-value updated-state :paragraphs)
                                  :current-lines (state-value updated-state :current-lines)
                                  :in-fence-p nil)
                      updated-state)))
               ((system-instruction-fence-line-p line)
                (make-state :paragraphs (state-value state :paragraphs)
                            :current-lines (append (state-value state :current-lines)
                                                   (list line))
                            :in-fence-p t))
               ((string= "" (string-trim '(#\Space #\Tab #\Return) line))
                (flush-paragraph state))
               (t
                (append-current-line state line)))))
    (let* ((final-state (reduce #'accumulate-line
                                (cl-ppcre:split "\\r?\\n" text)
                                :initial-value (make-state :paragraphs nil
                                                           :current-lines nil
                                                           :in-fence-p nil)))
           (paragraphs (state-value (flush-paragraph final-state) :paragraphs)))
      (coerce paragraphs 'vector))))

(defun system-instruction-paragraphs (system-instruction)
  "Returns SYSTEM-INSTRUCTION as a vector of paragraphs."
  (cond
    ((null system-instruction) nil)
    ((stringp system-instruction) (vector system-instruction))
    ((vectorp system-instruction) system-instruction)
    ((listp system-instruction) (coerce system-instruction 'vector))
    (t (vector system-instruction))))

(defun system-instruction-text (system-instruction)
  "Returns SYSTEM-INSTRUCTION as a single string separated by blank lines."
  (let ((paragraphs (system-instruction-paragraphs system-instruction)))
    (when (and paragraphs (> (length paragraphs) 0))
     (format nil "~{~A~^~%~%~}" (coerce paragraphs 'list)))))

(defun system-instruction-text-parts (system-instruction)
  "Returns SYSTEM-INSTRUCTION as a vector of provider text parts."
  (let ((paragraphs (system-instruction-paragraphs system-instruction)))
    (when (and paragraphs (> (length paragraphs) 0))
     (coerce (loop for paragraph across paragraphs
                   collect (list (cons "text" paragraph)))
             'vector))))

(defun system-instruction-owner (target)
  "Returns the chatbot owning TARGET's system instructions."
  (cond
    ((typep target 'chatbot) target)
    ((typep target 'conversation) (conversation-chatbot target))
    ((typep target 'persona) (conversation-chatbot (persona-conversation target)))
    (t (error "System instruction target must be a chatbot or conversation: ~S" target))))

(defun sampling-parameter-owner (target)
  "Returns the chatbot owning TARGET's sampling parameters."
  (cond
    ((typep target 'chatbot) target)
    ((typep target 'conversation) (conversation-chatbot target))
    ((typep target 'persona) (conversation-chatbot (persona-conversation target)))
    (t (error "Sampling parameter target must be a chatbot or conversation: ~S" target))))

(defun normalize-chatbot-temperature (temperature &key allow-nil-p)
  "Returns TEMPERATURE normalized for chatbot storage."
  (when (null temperature)
    (if allow-nil-p
        (return-from normalize-chatbot-temperature nil)
        (error "Temperature must not be NIL.")))
  (unless (realp temperature)
    (error "Temperature must be a real number: ~S" temperature))
  (let ((normalized (float temperature 1.0d0)))
    (unless (<= 0.0d0 normalized 2.0d0)
      (error "Temperature must be between 0.0 and 2.0 inclusive: ~S" temperature))
    normalized))

(defun normalize-chatbot-top-p (top-p &key allow-nil-p)
  "Returns TOP-P normalized for chatbot storage."
  (when (null top-p)
    (if allow-nil-p
        (return-from normalize-chatbot-top-p nil)
        (error "Top-p must not be NIL.")))
  (unless (realp top-p)
    (error "Top-p must be a real number: ~S" top-p))
  (let ((normalized (float top-p 1.0d0)))
    (unless (and (> normalized 0.0d0)
                 (<= normalized 1.0d0))
      (error "Top-p must be greater than 0.0 and at most 1.0: ~S" top-p))
    normalized))

(defun sampling-parameters (target)
  "Returns TARGET's current sampling parameters as a plist."
  (let ((owner (sampling-parameter-owner target)))
    (list :temperature (chatbot-temperature owner)
          :top-p (chatbot-top-p owner))))

(defun set-sampling-parameters (target &key (temperature nil temperaturep) (top-p nil top-pp))
  "Updates TARGET's chatbot sampling defaults and returns the current plist."
  (unless (or temperaturep top-pp)
    (error "At least one of :temperature or :top-p must be provided."))
  (let ((owner (sampling-parameter-owner target)))
    (when temperaturep
      (setf (chatbot-temperature owner)
            (normalize-chatbot-temperature temperature :allow-nil-p t)))
    (when top-pp
      (setf (chatbot-top-p owner)
            (normalize-chatbot-top-p top-p :allow-nil-p t)))
    (sampling-parameters owner)))

(defun reset-sampling-parameters (target)
  "Clears TARGET's chatbot sampling defaults and returns the current plist."
  (let ((owner (sampling-parameter-owner target)))
    (setf (chatbot-temperature owner) nil)
    (setf (chatbot-top-p owner) nil)
    (sampling-parameters owner)))

(defun normalize-system-instruction-paragraph (paragraph)
  "Returns PARAGRAPH normalized for system instruction storage."
  (unless (stringp paragraph)
    (error "System instruction paragraphs must be strings: ~S" paragraph))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) paragraph)))
    (unless (string/= trimmed "")
     (error "System instruction paragraphs must not be empty."))
    trimmed))

(defun normalize-system-instruction-paragraph-sequence (paragraphs)
  "Returns PARAGRAPHS as a normalized vector of paragraph strings."
  (let ((paragraph-vector (or (system-instruction-paragraphs paragraphs)
                             #())))
    (coerce (loop for paragraph across paragraph-vector
                 collect (normalize-system-instruction-paragraph paragraph))
           'vector)))

(defun current-system-instruction-paragraphs (target)
  "Returns TARGET's current system instructions as a paragraph vector."
  (let ((raw (chatbot-system-instruction (system-instruction-owner target))))
    (cond
     ((null raw) #())
     ((stringp raw) (split-system-instruction-into-paragraphs raw))
     (t (or (system-instruction-paragraphs raw) #())))))

(defun %set-system-instruction-paragraphs (target paragraphs)
  "Stores PARAGRAPHS on TARGET's owning chatbot and returns the stored vector."
  (let* ((owner (system-instruction-owner target))
        (normalized (normalize-system-instruction-paragraph-sequence paragraphs)))
    (setf (chatbot-system-instruction owner) normalized)
    normalized))

(defun system-instruction-paragraph-count (target)
  "Returns the number of system instruction paragraphs stored on TARGET."
  (length (current-system-instruction-paragraphs target)))

(defun system-instruction-paragraphs-copy (target)
  "Returns a copy of TARGET's system instruction paragraph vector."
  (copy-seq (current-system-instruction-paragraphs target)))

(defun checked-system-instruction-index (target index &key allow-end-p)
  "Validates INDEX for TARGET and returns it."
  (unless (and (integerp index) (<= 0 index))
    (error "System instruction index must be a non-negative integer: ~S" index))
  (let ((count (system-instruction-paragraph-count target)))
    (unless (if allow-end-p
               (<= index count)
               (< index count))
     (error "System instruction index ~A is out of bounds for ~A paragraphs."
            index
            count))
    index))

(defun system-instruction-paragraph (target index)
  "Returns the paragraph at INDEX from TARGET's system instructions."
  (let ((paragraphs (current-system-instruction-paragraphs target)))
    (aref paragraphs
         (checked-system-instruction-index target index))))

(defun insert-system-instruction-paragraph (target paragraph &key index)
  "Inserts PARAGRAPH into TARGET's system instructions and returns the updated vector."
  (let* ((paragraphs (current-system-instruction-paragraphs target))
        (insert-index (if index
                          (checked-system-instruction-index target index :allow-end-p t)
                          (length paragraphs)))
        (normalized (normalize-system-instruction-paragraph paragraph)))
    (%set-system-instruction-paragraphs
     target
     (append (subseq (coerce paragraphs 'list) 0 insert-index)
            (list normalized)
            (subseq (coerce paragraphs 'list) insert-index)))))

(defun update-system-instruction-paragraph (target index paragraph)
  "Replaces the paragraph at INDEX on TARGET and returns the updated vector."
  (let* ((paragraphs (system-instruction-paragraphs-copy target))
        (resolved-index (checked-system-instruction-index target index))
        (normalized (normalize-system-instruction-paragraph paragraph)))
    (setf (aref paragraphs resolved-index) normalized)
    (%set-system-instruction-paragraphs target paragraphs)))

(defun delete-system-instruction-paragraph (target index)
  "Deletes the paragraph at INDEX from TARGET and returns the updated vector."
  (let* ((paragraphs (current-system-instruction-paragraphs target))
        (resolved-index (checked-system-instruction-index target index))
        (paragraph-list (coerce paragraphs 'list)))
    (%set-system-instruction-paragraphs
     target
     (append (subseq paragraph-list 0 resolved-index)
            (subseq paragraph-list (1+ resolved-index))))))

(defun clear-system-instruction-paragraphs (target)
  "Clears all system instruction paragraphs from TARGET."
  (%set-system-instruction-paragraphs target #()))

(defun replace-system-instruction-paragraphs (target paragraphs)
  "Replaces TARGET's system instruction paragraphs with PARAGRAPHS."
  (%set-system-instruction-paragraphs target paragraphs))

(defun save-system-instructions (target)
  "Persists TARGET's paragraph-vector system instructions back to its backing file."
  (let* ((owner (system-instruction-owner target))
        (path (chatbot-system-instruction-path owner))
        (storage-kind (chatbot-system-instruction-storage-kind owner))
        (contents (or (system-instruction-text (chatbot-system-instruction owner))
                      "")))
    (unless path
     (error "System instructions do not have a backing file to save."))
    (unless (member storage-kind '(:paragraph-file :markdown-file))
     (error "System instructions backed by ~A cannot be saved with SAVE-SYSTEM-INSTRUCTIONS; migrate to a system-instructions file first."
            storage-kind))
    (with-open-file (stream path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
     (write-string contents stream))
    path))
