;;; -*- Lisp -*-
;;; conversations.lisp - conversation constructors and persona entry points

(in-package "CHATBOT")

(defparameter +state-digest-message-prefix+ "[State Digest of previous turns: "
  "Prefix used for synthetic digest messages inserted during context compression.")

(defparameter +state-digest-message-suffix+ "]"
  "Suffix used for synthetic digest messages inserted during context compression.")

(defparameter *context-pruning-max-digest-tokens* 2048
  "Absolute upper bound for synthetic state-digest size after pruning.")

(defparameter +transient-plan-system-instruction-prefix+ "[EXECUTING PLAN FROM "
  "Marker prefix for transient system-instruction paragraphs loaded from plans.")

(defun read-persona-config (config-path)
  "Reads and validates a persona config form from CONFIG-PATH."
  (handler-case
      (with-open-file (stream config-path :direction :input)
        (let* ((eof-marker (gensym "EOF"))
               (forms (loop for form = (read stream nil eof-marker)
                            until (eq form eof-marker)
                            collect form))
               (config (cond
                         ((null forms) :eof)
                         ((and (= 1 (length forms))
                               (listp (car forms)))
                          (car forms))
                         (t forms))))
          (when (eq config :eof)
            (error "Persona config file is empty: ~A" config-path))
          (unless (listp config)
            (error "Persona config must be a property list: ~A" config-path))
          (unless (and config (keywordp (car config)))
            (error "Persona config must start with a keyword property: ~A" config-path))
          config))
    (error (e)
      (error "Invalid persona config in ~A: ~A" config-path e))))

(defun persona-system-instruction-path (persona-dir)
  "Returns the preferred persona system-instruction file pathname when present."
  (or (probe-file (merge-pathnames "system-instructions" persona-dir))
      (probe-file (merge-pathnames "system-instruction.md" persona-dir))
      (probe-file (merge-pathnames "system-instructions.md" persona-dir))))

(defun read-persona-system-instruction (inst-path)
  "Reads INST-PATH using the appropriate internal representation."
  (let ((contents (uiop:read-file-string inst-path)))
    (if (string= "system-instructions" (file-namestring inst-path))
        (split-system-instruction-into-paragraphs contents)
        contents)))

(defun persona-system-instruction-storage-kind (inst-path)
  "Returns the storage kind implied by INST-PATH."
  (if (and inst-path
           (string= "system-instructions" (file-namestring inst-path)))
      :paragraph-file
      :markdown-file))

(defun persona-config-backend (config)
  "Returns the backend keyword implied by persona CONFIG."
  (let ((backend (safe-getf config :backend)))
    (cond
      ((null backend)
       (if (eq (safe-getf config :googleapi) :google-api)
          :google
          :gemini))
      (t
       (normalize-chatbot-backend backend "persona")))))

(defun persona-config-agentic-loop-default-backend (config)
  "Returns the optional default agentic loop backend implied by persona CONFIG."
  (normalize-chatbot-backend (safe-getf config :agentic-loop-default-backend)
                            "persona agentic loop default"
                            :allow-nil-p t))

(defun persona-config-agentic-loop-default-model (config)
  "Returns the optional default agentic loop model implied by persona CONFIG."
  (let ((model (safe-getf config :agentic-loop-default-model)))
    (when model
      (require-non-empty-string model "Persona agentic loop default model"))))

(defun persona-config-runtime-context (config runtime-context)
  "Returns the effective runtime context for a persona using CONFIG and RUNTIME-CONTEXT."
  (let* ((base-context (resolve-runtime-context runtime-context))
        (loop-default-backend (persona-config-agentic-loop-default-backend config))
        (loop-default-model (persona-config-agentic-loop-default-model config)))
    (if (or loop-default-backend loop-default-model)
       (call-with-runtime-context
        base-context
        (lambda ()
          (make-runtime-context :agentic-loop-default-backend loop-default-backend
                                :agentic-loop-default-model loop-default-model))
        :default-conversation-compatibility-p nil
        :legacy-function-seam-compatibility-p nil)
       base-context)))

(defun new-chat (&key model system-instruction system-instruction-path (system-instruction-storage-kind :transient) temperature top-p (content-cache-policy +default-content-cache-policy+) (content-cache-ttl-seconds *default-content-cache-ttl-seconds*) (content-cache-min-tokens *default-content-cache-min-tokens*) google-search-p (gemini-fallback-to-google-p +default-gemini-fallback-to-google-p+) web-tools-p code-execution-p include-timestamp-p include-model-p enable-eval-p (enable-git-tools-p nil) filesystem-tools-p filesystem-root-directory filesystem-allowed-directories filesystem-allowlist-path (backend :gemini) runtime-context subordinates persona-name persona-source-name checkpoint-name parent-name (depth 1) token-budget (spent-tokens 0) scoped-directory filesystem-read-only-p planner-p cached-content-name cached-content-key cached-content-metadata)
  "Creates a new chatbot instance and returns an initialized conversation object.
If model is NIL, a sensible default model is chosen based on the backend.
Personas are optional; use NEW-CHAT-PERSONA only when you want persona-specific
configuration, instructions, or preloaded memory."
  (let ((resolved-context (resolve-runtime-context runtime-context)))
    (call-with-runtime-context
     resolved-context
     (lambda ()
      (maybe-auto-initialize-startup-chatbot resolved-context)
      (let* ((chosen-model (or model
                               (backend-default-model backend)))
             (bot (make-instance 'chatbot
                                 :persona-name persona-name
                                 :persona-source-name persona-source-name
                                 :checkpoint-name (or checkpoint-name persona-name)
                                 :model chosen-model
                                 :backend backend
                                 :system-instruction system-instruction
                                 :system-instruction-path system-instruction-path
                                 :system-instruction-storage-kind system-instruction-storage-kind
                                 :temperature (normalize-chatbot-temperature temperature :allow-nil-p t)
                                 :top-p (normalize-chatbot-top-p top-p :allow-nil-p t)
                                 :content-cache-policy (normalize-content-cache-policy content-cache-policy)
                                 :content-cache-ttl-seconds (normalize-content-cache-ttl-seconds content-cache-ttl-seconds :allow-nil-p t)
                                 :content-cache-min-tokens (normalize-content-cache-min-tokens content-cache-min-tokens :allow-nil-p t)
                                 :google-search-p google-search-p
                                 :gemini-fallback-to-google-p gemini-fallback-to-google-p
                                 :web-tools-p web-tools-p
                                 :code-execution-p code-execution-p
                                 :include-timestamp-p include-timestamp-p
                                 :include-model-p include-model-p
                                 :enable-eval-p enable-eval-p :enable-git-tools-p enable-git-tools-p
                                 :filesystem-tools-p filesystem-tools-p
                                 :filesystem-root-directory (or scoped-directory filesystem-root-directory)
                                 :filesystem-allowed-directories filesystem-allowed-directories
                                 :filesystem-allowlist-path filesystem-allowlist-path
                                 :runtime-context resolved-context
                                 :subordinates subordinates
                                 :parent-name parent-name
                                 :depth depth
                                 :token-budget token-budget
                                 :spent-tokens spent-tokens
                                 :scoped-directory (or scoped-directory filesystem-root-directory)
                                 :filesystem-read-only-p filesystem-read-only-p
                                 :planner-p planner-p)))
        (let ((startup-servers (startup-chatbot-mcp-servers resolved-context))
              (startup-status (startup-chatbot-mcp-status resolved-context)))
          (when startup-servers
            (setf (chatbot-mcp-servers bot) startup-servers))
          (when startup-status
            (setf (chatbot-mcp-startup-status bot) startup-status)))
        (make-instance 'conversation
                       :chatbot bot
                       :cached-content-name cached-content-name
                       :cached-content-key cached-content-key
                       :cached-content-metadata cached-content-metadata)))
     :default-conversation-compatibility-p nil
     :legacy-function-seam-compatibility-p nil)))

(defun new-chat-persona (persona-name &key runtime-context parent-name (depth 1) token-budget (spent-tokens 0) scoped-directory (web-tools-p nil web-tools-supplied-p) (enable-git-tools-p nil enable-git-tools-supplied-p) (filesystem-tools-p nil filesystem-tools-supplied-p) (filesystem-read-only-p nil filesystem-read-only-supplied-p) (planner-p nil planner-supplied-p) (load-configured-subordinates-p t))
  "Creates a new chat session for a given chatbot persona.
The persona's configuration is read from ~/.Personas/<persona-name>/config.lisp
and the system instructions are loaded from the persona's system-instruction file set.
Use NEW-CHAT instead when no persona should be loaded."
  (let ((persona-dir (resolve-persona-directory persona-name)))
    (if (null persona-dir)
        (progn
         (log-message :warn "Skipping restore for missing persona"
                      :context `(("persona" . ,(princ-to-string persona-name))))
         (new-chat :runtime-context runtime-context
                   :persona-source-name persona-name
                   :checkpoint-name persona-name
                   :parent-name parent-name
                   :depth depth
                   :token-budget token-budget
                   :spent-tokens spent-tokens
                   :scoped-directory scoped-directory
                   :filesystem-read-only-p (if filesystem-read-only-supplied-p filesystem-read-only-p nil)
                   :planner-p (if planner-supplied-p planner-p nil)))
        (let* ((config-path (probe-file (merge-pathnames "config.lisp" persona-dir)))
              (inst-path (persona-system-instruction-path persona-dir)))
         (let* ((config (when config-path
                          (read-persona-config config-path)))
                (system-instruction (when inst-path
                                      (read-persona-system-instruction inst-path)))
                (model (safe-getf config :model))
                (temperature (safe-getf config :temperature))
                (top-p (safe-getf config :top-p))
                (googleapi (safe-getf config :googleapi))
                (google-search-p (safe-getf config :google-search-p))
                (gemini-fallback-to-google-p (safe-getf config :gemini-fallback-to-google-p))
                (config-web-tools-p (safe-getf config :enable-web-tools))
                (code-execution-p (safe-getf config :code-execution-p))
                (include-timestamp-p (safe-getf config :include-timestamp))
                (include-model-p (safe-getf config :include-model))
                (enable-eval-p (safe-getf config :enable-eval))
                (config-enable-git-tools-p (safe-getf config :enable-git-tools)) (config-filesystem-tools-p (safe-getf config :enable-filesystem-tools))
                (backend (persona-config-backend config))
                (persona-runtime-context (persona-config-runtime-context config runtime-context)))
           (declare (ignore googleapi))
           (let ((conversation
                   (preload-persona-conversation-diary
                    (preload-persona-conversation-memory
                     (new-chat :backend backend
                               :model model
                               :system-instruction system-instruction
                               :system-instruction-path inst-path
                               :system-instruction-storage-kind (persona-system-instruction-storage-kind inst-path)
                               :temperature temperature
                               :top-p top-p
                               :google-search-p google-search-p
                               :gemini-fallback-to-google-p gemini-fallback-to-google-p
                               :web-tools-p (if web-tools-supplied-p web-tools-p config-web-tools-p)
                               :code-execution-p code-execution-p
                               :include-timestamp-p include-timestamp-p
                               :include-model-p include-model-p
                               :enable-eval-p enable-eval-p :enable-git-tools-p enable-git-tools-p
                               :enable-git-tools-p (if enable-git-tools-supplied-p enable-git-tools-p config-enable-git-tools-p) :filesystem-tools-p (if filesystem-tools-supplied-p filesystem-tools-p config-filesystem-tools-p)
                               :filesystem-root-directory (or scoped-directory persona-dir)
                               :filesystem-allowed-directories (persona-filesystem-allowlist-directories persona-dir)
                               :filesystem-allowlist-path (persona-filesystem-allowlist-path persona-dir)
                               :runtime-context persona-runtime-context
                               :subordinates (and load-configured-subordinates-p
                                                  (loop for sub-persona in (safe-getf config :subordinates)
                                                        collect (new-chat-persona sub-persona :runtime-context runtime-context)))
                               :persona-name persona-name
                               :persona-source-name persona-name
                               :parent-name parent-name
                               :depth depth
                               :token-budget token-budget
                               :spent-tokens spent-tokens
                               :scoped-directory (or scoped-directory persona-dir)
                               :filesystem-read-only-p (if filesystem-read-only-supplied-p filesystem-read-only-p nil)
                               :planner-p (if planner-supplied-p planner-p nil))
                     persona-dir)
                    persona-dir)))
             (setf conversation (attach-persona-memory-mcp-server conversation persona-dir))
             (start-persona-memory-compression-thread conversation persona-dir)
             conversation))))))

(defparameter *minions-data-directory* nil
  "Seam to override the dynamic minions storage directory in unit tests.")

(defun configured-minions-data-directory ()
  "Returns the configured minions storage directory from the environment, or NIL."
  (let ((configured (funcall *getenv-function* "CHATBOT_MINIONS_DATA_DIR")))
    (when (and configured (string/= configured ""))
      (uiop:ensure-directory-pathname configured))))

(defun default-minions-data-directory ()
  "Returns the default per-user runtime directory for minion checkpoint states."
  (uiop:ensure-directory-pathname
   (merge-pathnames (make-pathname :directory '(:relative ".chatbot" "data" "minions"))
                    (funcall *user-homedir-pathname-function*))))

(defun minions-data-directory ()
  "Returns the directory where minion checkpoint states are persisted."
  (or *minions-data-directory*
      (configured-minions-data-directory)
      (default-minions-data-directory)))

(defun conversation-checkpoint-name (conversation)
  "Returns the persistence name used when checkpointing CONVERSATION."
  (or (chatbot-checkpoint-name (conversation-chatbot conversation))
      (chatbot-persona-name (conversation-chatbot conversation))
      "DefaultConversation"))

(defun save-minion-state (conversation &key checkpoint-name)
  "Serializes the critical state and telemetry of CONVERSATION to disk."
  (let* ((bot (conversation-chatbot conversation))
         (name (or checkpoint-name
                  (chatbot-checkpoint-name bot)
                  (chatbot-persona-name bot))))
    (when name
      (let* ((dir (minions-data-directory))
             (file-path (merge-pathnames (format nil "~A.json" name) dir))
             (state-plist (conversation-persistence-state conversation :name name)))
        (ensure-directories-exist file-path)
        (with-open-file (stream file-path
                              :direction :output
                              :if-exists :supersede
                               :if-does-not-exist :create)
          (write-string (cl-json:encode-json-to-string state-plist) stream))
        (log-message :info "Freeze-dried minion state"
                     :context `(("name" . ,name) ("file" . ,(namestring file-path))))
        (namestring file-path)))))

(defun checkpoint-conversation-after-chat (conversation)
  "Persists CONVERSATION using the standard post-chat checkpoint naming policy."
  (let ((checkpoint-name (conversation-checkpoint-name conversation)))
    (log-message :info "Checkpointing conversation after chat"
                :context `(("name" . ,checkpoint-name)))
    (save-minion-state conversation :checkpoint-name checkpoint-name)))

(defun finalize-chat-turn-result (result &optional conversation)
  "Applies RESULT, performs post-response compression, checkpoints, and returns the final text."
  (let ((effective-conversation (or conversation
                                    (chat-turn-result-conversation result))))
    (let ((text (apply-chat-turn-result result effective-conversation)))
      (when effective-conversation
        (compress-conversation-context-if-needed effective-conversation)
        (checkpoint-conversation-after-chat effective-conversation))
      text)))

(defun parse-minion-state-file (file runtime-context)
  "Returns FILE decoded to the normalized restore schema, or NIL after logging a warning."
  (handler-case
      (decode-persisted-conversation-state
       (cl-json:decode-json-from-string (uiop:read-file-string file))
       :runtime-context runtime-context
       :append-recovery-handshake-p t)
    (error (e)
      (log-message :warn "Failed to parse minion state file"
                 :context `(("file" . ,(namestring file))
                            ("error" . ,(princ-to-string e))))
      nil)))

(defun load-sorted-minion-states (directory runtime-context)
  "Returns DIRECTORY's normalized minion restore specs sorted shallowest-first."
  (let ((files (uiop:directory-files directory "*.json")))
    (sort (remove nil
                  (mapcar (lambda (file)
                            (parse-minion-state-file file runtime-context))
                          files))
          #'<
          :key (lambda (state) (getf state :depth)))))

(defun restoration-planner-p (restoration)
  "Returns true when RESTORATION represents a planner worker."
  (eq (getf restoration :worker-kind) :planner))

(defun restoration-content-cache-policy (restoration)
  "Returns the persisted content-cache policy or the repository default."
  (or (getf restoration :content-cache-policy)
      +default-content-cache-policy+))

(defun restoration-content-cache-ttl-seconds (restoration)
  "Returns the persisted content-cache TTL or the repository default."
  (or (getf restoration :content-cache-ttl-seconds)
      *default-content-cache-ttl-seconds*))

(defun restoration-content-cache-min-tokens (restoration)
  "Returns the persisted content-cache minimum token floor or the repository default."
  (or (getf restoration :content-cache-min-tokens)
      *default-content-cache-min-tokens*))

(defun apply-restored-chatbot-state (bot restoration persona-source-name)
  "Applies persisted chatbot state from RESTORATION onto BOT."
  (setf (chatbot-checkpoint-name bot)
        (getf restoration :checkpoint-name)
        (chatbot-persona-name bot)
        (getf restoration :persona-name)
        (chatbot-persona-source-name bot)
        persona-source-name
        (chatbot-backend bot)
        (getf restoration :backend)
        (chatbot-model bot)
        (getf restoration :model)
        (chatbot-system-instruction bot)
        (getf restoration :system-instruction)
        (chatbot-system-instruction-path bot)
        (getf restoration :system-instruction-path)
        (chatbot-system-instruction-storage-kind bot)
        (getf restoration :system-instruction-storage-kind)
        (chatbot-temperature bot)
        (getf restoration :temperature)
        (chatbot-top-p bot)
        (getf restoration :top-p)
        (chatbot-parent-name bot)
        (getf restoration :parent-name)
        (chatbot-depth bot)
        (getf restoration :depth)
        (chatbot-token-budget bot)
        (getf restoration :token-budget)
        (chatbot-spent-tokens bot)
        (getf restoration :spent-tokens)
        (chatbot-content-cache-policy bot)
        (restoration-content-cache-policy restoration)
        (chatbot-content-cache-ttl-seconds bot)
        (restoration-content-cache-ttl-seconds restoration)
        (chatbot-content-cache-min-tokens bot)
        (restoration-content-cache-min-tokens restoration)
        (chatbot-google-search-p bot)
        (getf restoration :google-search-p)
        (chatbot-gemini-fallback-to-google-p bot)
        (getf restoration :gemini-fallback-to-google-p)
        (chatbot-web-tools-p bot)
        (getf restoration :web-tools-p)
        (chatbot-code-execution-p bot)
        (getf restoration :code-execution-p)
        (chatbot-include-timestamp-p bot)
        (getf restoration :include-timestamp-p)
        (chatbot-include-model-p bot)
        (getf restoration :include-model-p)
        (chatbot-enable-eval-p bot)
        (getf restoration :enable-eval-p)
        (chatbot-enable-git-tools-p bot)
        (getf restoration :enable-git-tools-p)
        (chatbot-filesystem-tools-p bot)
        (getf restoration :filesystem-tools-p)
        (chatbot-filesystem-root-directory bot)
        (getf restoration :filesystem-root-directory)
        (chatbot-filesystem-allowed-directories bot)
        (getf restoration :filesystem-allowed-directories)
        (chatbot-filesystem-allowlist-path bot)
        (getf restoration :filesystem-allowlist-path)
        (chatbot-filesystem-read-only-p bot)
        (getf restoration :filesystem-read-only-p)
        (chatbot-scoped-directory bot)
        (getf restoration :scoped-directory)
        (chatbot-planner-p bot)
        (restoration-planner-p restoration))
  bot)

(defun apply-restored-conversation-state (conversation restoration)
  "Applies persisted conversation state from RESTORATION onto CONVERSATION."
  (setf (conversation-interaction-id conversation)
        (getf restoration :interaction-id)
        (conversation-adaptive-context-pruning-max-tokens conversation)
        (getf restoration :adaptive-context-pruning-max-tokens)
        (conversation-cached-content-name conversation)
        (getf restoration :cached-content-name)
        (conversation-cached-content-key conversation)
        (getf restoration :cached-content-key)
        (conversation-cached-content-metadata conversation)
        (getf restoration :cached-content-metadata)
        (conversation-messages conversation)
        (getf restoration :history))
  conversation)

(defun instantiate-conversation-from-restored-state (restoration)
  "Returns one restored conversation from normalized RESTORATION."
  (let* ((persona-source-name (getf restoration :persona-source-name))
         (restored-conv
           (if persona-source-name
               (new-chat-persona persona-source-name
                                 :runtime-context (getf restoration :runtime-context)
                                 :parent-name (getf restoration :parent-name)
                                 :depth (getf restoration :depth)
                                 :token-budget (getf restoration :token-budget)
                                 :spent-tokens (getf restoration :spent-tokens)
                                 :scoped-directory (getf restoration :scoped-directory)
                                 :planner-p (restoration-planner-p restoration)
                                 :load-configured-subordinates-p nil)
               (new-chat :backend (getf restoration :backend)
                         :model (getf restoration :model)
                         :system-instruction (getf restoration :system-instruction)
                         :system-instruction-path (getf restoration :system-instruction-path)
                         :system-instruction-storage-kind
                         (getf restoration :system-instruction-storage-kind)
                         :temperature (getf restoration :temperature)
                         :top-p (getf restoration :top-p)
                         :checkpoint-name (getf restoration :checkpoint-name)
                         :content-cache-policy (restoration-content-cache-policy restoration)
                         :content-cache-ttl-seconds
                         (restoration-content-cache-ttl-seconds restoration)
                         :content-cache-min-tokens
                         (restoration-content-cache-min-tokens restoration)
                         :google-search-p (getf restoration :google-search-p)
                         :gemini-fallback-to-google-p (getf restoration :gemini-fallback-to-google-p)
                         :web-tools-p (getf restoration :web-tools-p)
                         :code-execution-p (getf restoration :code-execution-p)
                         :include-timestamp-p (getf restoration :include-timestamp-p)
                         :include-model-p (getf restoration :include-model-p)
                         :enable-eval-p (getf restoration :enable-eval-p)
                         :enable-git-tools-p (getf restoration :enable-git-tools-p)
                         :filesystem-tools-p (getf restoration :filesystem-tools-p)
                         :filesystem-root-directory (getf restoration :filesystem-root-directory)
                         :filesystem-allowed-directories
                         (getf restoration :filesystem-allowed-directories)
                         :filesystem-allowlist-path (getf restoration :filesystem-allowlist-path)
                         :filesystem-read-only-p (getf restoration :filesystem-read-only-p)
                         :parent-name (getf restoration :parent-name)
                         :depth (getf restoration :depth)
                         :token-budget (getf restoration :token-budget)
                         :spent-tokens (getf restoration :spent-tokens)
                         :planner-p (restoration-planner-p restoration)
                         :scoped-directory (getf restoration :scoped-directory)
                         :runtime-context (getf restoration :runtime-context)
                         :cached-content-name (getf restoration :cached-content-name)
                         :cached-content-key (getf restoration :cached-content-key)
                         :cached-content-metadata (getf restoration :cached-content-metadata)))))
    (apply-restored-chatbot-state (conversation-chatbot restored-conv)
                                 restoration
                                 persona-source-name)
    (apply-restored-conversation-state restored-conv restoration)))

(defun instantiate-restored-minion (restoration)
  "Returns one restored subordinate conversation from RESTORATION."
  (let* ((name (getf restoration :name))
         (sub-conv (instantiate-conversation-from-restored-state restoration))
         (sub-bot (conversation-chatbot sub-conv)))
    (when name
      (terminate-active-threads-by-name name)
      (setf (chatbot-checkpoint-name sub-bot) name)
      (setf (chatbot-persona-name sub-bot)
           (or (getf restoration :persona-name)
               name)))
    sub-conv))

(defun restoration-parent-is-root-p (restoration root-bot)
  "Returns true when RESTORATION should attach directly beneath ROOT-BOT."
  (let ((parent-name (getf restoration :parent-name)))
    (or (null parent-name)
        (string= parent-name "")
        (string-equal parent-name (chatbot-persona-name root-bot)))))

(defun restored-root-parent-conversation (root-bot)
  "Returns the best available restored planner parent conversation for ROOT-BOT."
  (let* ((context (chatbot-runtime-context root-bot))
         (active (and context (current-active-conversation context)))
         (default (and context (current-default-conversation context))))
    (cond
      ((and active
           (eq (conversation-chatbot active) root-bot))
       active)
      ((and default
           (eq (conversation-chatbot default) root-bot))
       default)
      (t
       nil))))

(defun activate-restored-planner (restoration sub-conv parent-conv)
  "Restores planner runtime activation state for SUB-CONV when required."
  (when (eq (getf restoration :worker-kind) :planner)
    (let ((context (chatbot-runtime-context (conversation-chatbot sub-conv))))
      (setf (current-active-planner context) sub-conv)
      (setf (current-active-planner-parent-conversation context) parent-conv)))
  sub-conv)

(defun attach-restored-minion (root-bot restored-convs restoration sub-conv)
  "Attaches SUB-CONV according to RESTORATION using RESTORED-CONVS for parent lookup."
  (let ((name (getf restoration :name))
        (parent-name (getf restoration :parent-name)))
    (when name
      (setf (gethash name restored-convs) sub-conv))
    (if (restoration-parent-is-root-p restoration root-bot)
        (progn
         (attach-subordinate-conversation root-bot sub-conv)
         (activate-restored-planner restoration
                                    sub-conv
                                    (restored-root-parent-conversation root-bot)))
        (let ((parent-conv (gethash parent-name restored-convs)))
         (if parent-conv
             (progn
               (attach-subordinate-conversation (conversation-chatbot parent-conv) sub-conv)
               (activate-restored-planner restoration sub-conv parent-conv))
             (log-message :warn "Orphaned minion: parent not found"
                          :context `(("name" . ,name)
                                     ("parent" . ,parent-name))))))))

(defun terminate-active-threads-by-name (name-substring)
  "Finds and terminates any active SBCL threads whose name contains NAME-SUBSTRING case-insensitively."
  #+sbcl
  (let ((threads (sb-thread:list-all-threads))
        (current sb-thread:*current-thread*))
    (dolist (thread threads)
      (unless (eq thread current)
        (let ((name (sb-thread:thread-name thread)))
          (when (and name (search name-substring name :test #'char-equal))
            (handler-case
                (progn
                  (log-message :info "MCRS: Terminating pre-existing thread to prevent leak"
                               :context `(("thread-name" . ,name) ("minion" . ,name-substring)))
                  (sb-thread:terminate-thread thread))
              (error (e)
                (log-message :warn "MCRS: Failed to terminate thread"
                             :context `(("thread-name" . ,name) ("error" . ,(princ-to-string e))))))))))))

(defun restore-minions (root-bot)
  "Scans data/minions/ directory and reconstructs the minion hierarchy under ROOT-BOT."
  (let ((dir (minions-data-directory)))
    (when (uiop:directory-exists-p dir)
      (let ((restored-convs (make-hash-table :test #'equal)))
        (dolist (restoration
                 (load-sorted-minion-states dir (chatbot-runtime-context root-bot)))
          (let ((sub-conv (instantiate-restored-minion restoration)))
            (attach-restored-minion root-bot restored-convs restoration sub-conv))))
      (log-message :info "MCRS: Restoration bootloader completed successfully."))))

(defun summarize-old-history (messages conversation)
  "Sends the old conversation history to the LLM to generate a concise State Digest."
  (let* ((bot (conversation-chatbot conversation))
         (max-digest-tokens (effective-state-digest-max-tokens conversation))
         (history-text (summarize-old-history-source-text messages))
         (prompt
           (format nil
                   "Please read the following conversation history and write a highly concise, dense State Digest summarizing all key factual information, state, progress, and memories from it. Consolidate any prior digest content instead of repeating wrapper text. Output only the State Digest, nothing else, and keep it under approximately ~D tokens.~%~%~A"
                   max-digest-tokens
                   history-text))
         ;; Use a clean, stateless conversation to avoid nested pruning loops
         (conv (new-chat :backend (chatbot-backend bot)
                         :model (chatbot-model bot)
                         :runtime-context (chatbot-runtime-context bot)))
         (summary
           (call-with-runtime-context
            (chatbot-runtime-context bot)
            (lambda ()
              (multiple-value-bind (effective-input effective-model)
                  (resolve-prompt-model-override (conversation-chatbot conv) prompt)
                (let ((result (dispatch-chat-turn conv
                                                effective-input
                                                nil
                                                :effective-model effective-model
                                                :effective-generation-config
                                                (resolve-effective-generation-config (conversation-chatbot conv)))))
                  (apply-chat-turn-result result conv)))))))
    (let ((bounded-summary
            (limit-text-to-estimated-token-budget summary max-digest-tokens)))
      (if (string/= bounded-summary "")
          bounded-summary
          "Compressed prior turns."))))

(defun message-content-string (message)
  "Returns MESSAGE content as a printable string for pruning heuristics."
  (let ((content (cdr (assoc "content" message :test #'string=))))
    (typecase content
      (null "")
      (string content)
      (t (princ-to-string content)))))

(defun estimate-text-token-count (text)
  "Returns a coarse token estimate for TEXT using a 4-characters-per-token heuristic."
  (ceiling (/ (length text) 4.0)))

(defun estimate-message-token-count (message)
  "Returns a coarse token estimate for one conversation MESSAGE."
  (estimate-text-token-count (message-content-string message)))

(defun message-role-string (message)
  "Returns MESSAGE's role as a lowercase string when present."
  (let ((role (cdr (assoc "role" message :test #'string=))))
    (and role (string-downcase role))))

(defun state-digest-message-p (message)
  "Returns true when MESSAGE is one synthetic compression digest."
  (let ((content (cdr (assoc "content" message :test #'string=))))
    (and (string= "system" (or (message-role-string message) ""))
         (stringp content)
         (alexandria:starts-with-subseq +state-digest-message-prefix+ content)
         (alexandria:ends-with-subseq +state-digest-message-suffix+ content))))

(defun message-state-digest-text (message)
  "Returns the inner digest text carried by one synthetic digest MESSAGE."
  (when (state-digest-message-p message)
    (let* ((content (cdr (assoc "content" message :test #'string=)))
           (start (length +state-digest-message-prefix+))
           (end (- (length content) (length +state-digest-message-suffix+))))
      (subseq content start end))))

(defun summarize-old-history-source-text (messages)
  "Returns the digest prompt source text for compressed old-history MESSAGES.
Existing synthetic digests are unwrapped so later summarization consolidates their
content instead of recursively digesting the wrapper text."
  (let ((prior-digests nil)
        (raw-lines nil))
    (dolist (message messages)
      (let ((digest-text (message-state-digest-text message)))
        (if digest-text
            (push digest-text prior-digests)
            (push (format nil "~A: ~A"
                          (or (cdr (assoc "role" message :test #'string=))
                              "unknown")
                          (message-content-string message))
                  raw-lines))))
    (with-output-to-string (stream)
      (when prior-digests
        (format stream "Existing State Digest content to preserve and refine:~%~{~A~%~^~%~}"
                (nreverse prior-digests)))
      (when raw-lines
        (when prior-digests
          (format stream "~%~%"))
        (format stream "Additional conversation turns to merge into the digest:~%~{~A~%~}"
                (nreverse raw-lines))))))

(defun limit-text-to-estimated-token-budget (text max-tokens)
  "Returns TEXT trimmed to MAX-TOKENS using the repository's coarse token heuristic."
  (let* ((normalized (string-trim '(#\Space #\Tab #\Return #\Linefeed) (or text "")))
         (max-characters (* 4 (max 1 max-tokens))))
    (if (<= (estimate-text-token-count normalized) max-tokens)
        normalized
        (string-right-trim
         '(#\Space #\Tab #\Return #\Linefeed)
         (subseq normalized 0 (min (length normalized) max-characters))))))

(defun effective-state-digest-max-tokens (conversation-or-bot)
  "Returns the estimated token budget available for one synthetic digest."
  (let* ((conversation (and (typep conversation-or-bot 'conversation)
                            conversation-or-bot))
         (target-history-tokens
           (if conversation
               (effective-history-compression-target-tokens conversation)
               (effective-context-pruning-target-tokens)))
         (scaled-budget (max 1 (floor target-history-tokens 2))))
    (min *context-pruning-max-digest-tokens* scaled-budget)))

(defun estimated-history-token-count (messages)
  "Returns a coarse token estimate for MESSAGES."
  (loop for message in messages
        sum (estimate-message-token-count message)))

(defun estimated-digest-message-token-count (messages)
  "Returns the estimated token usage attributable to synthetic digest messages."
  (loop for message in messages
        when (state-digest-message-p message)
          sum (estimate-message-token-count message)))

(defun estimate-optional-text-token-count (text)
  "Returns a coarse token estimate for TEXT, or zero when TEXT is absent."
  (if (and text (stringp text) (string/= text ""))
      (estimate-text-token-count text)
      0))

(defun estimated-fixed-conversation-context-token-count (conversation)
  "Returns the estimated non-history prompt tokens carried by CONVERSATION."
  (let* ((bot (conversation-chatbot conversation))
         (system-instruction-tokens
           (estimate-optional-text-token-count
            (system-instruction-text (chatbot-system-instruction bot)))))
    system-instruction-tokens))

(defun estimated-conversation-context-token-count (conversation &optional (history (conversation-messages conversation)))
  "Returns the estimated total prompt tokens for CONVERSATION using HISTORY."
  (+ (estimated-fixed-conversation-context-token-count conversation)
     (estimated-history-token-count history)))

(defun conversation-context-token-breakdown (conversation &optional (history (conversation-messages conversation)))
  "Returns a plist breaking CONVERSATION context into fixed, history, digest, and total tokens."
  (let* ((fixed-context-tokens (estimated-fixed-conversation-context-token-count conversation))
         (history-tokens (estimated-history-token-count history))
         (digest-message-tokens (estimated-digest-message-token-count history)))
    (list :fixed-context-tokens fixed-context-tokens
          :history-tokens history-tokens
          :digest-message-tokens digest-message-tokens
          :non-digest-history-tokens (max 0 (- history-tokens digest-message-tokens))
          :total-tokens (+ fixed-context-tokens history-tokens))))

(defun configured-context-pruning-max-tokens ()
  "Returns the configured estimated max-token ceiling before per-conversation adaptation."
  (let ((max-tokens *context-pruning-estimated-max-tokens*)
        (char-threshold *context-pruning-threshold-characters*))
    (if (and char-threshold (> char-threshold 0))
        (min max-tokens
             (estimate-text-token-count (make-string char-threshold :initial-element #\X)))
        max-tokens)))

(defun update-adaptive-context-pruning-max-tokens (conversation history)
  "Updates CONVERSATION with a per-conversation compression ceiling no higher than the configured budget."
  (let ((compressed-total-tokens (estimated-conversation-context-token-count conversation history)))
    (setf (conversation-adaptive-context-pruning-max-tokens conversation)
          (min (configured-context-pruning-max-tokens)
               (max 1 (* 2 compressed-total-tokens))))))

(defun effective-history-compression-max-tokens (conversation)
  "Returns the estimated history-token budget available before compression should trigger."
  (max 0
       (- (effective-context-pruning-max-tokens conversation)
          (estimated-fixed-conversation-context-token-count conversation))))

(defun effective-history-compression-target-tokens (conversation)
  "Returns the estimated history-token budget to aim for after compression."
  (max 0
       (- (effective-context-pruning-target-tokens conversation)
          (estimated-fixed-conversation-context-token-count conversation))))

(defun effective-context-pruning-max-tokens (&optional conversation)
  "Returns the effective estimated max-token ceiling, including per-conversation adaptation."
  (let ((configured-max-tokens (configured-context-pruning-max-tokens)))
    (if conversation
        (let ((adaptive-max-tokens
                (conversation-adaptive-context-pruning-max-tokens conversation)))
          (if adaptive-max-tokens
              (min configured-max-tokens adaptive-max-tokens)
              configured-max-tokens))
        configured-max-tokens)))

(defun effective-context-pruning-target-tokens (&optional conversation)
  "Returns the effective estimated post-compression target token count."
  (let* ((max-tokens (effective-context-pruning-max-tokens conversation))
         (configured-target *context-pruning-estimated-target-tokens*))
    (min configured-target
         (max 1 (floor (* max-tokens 0.9))))))

(defun select-recent-messages-for-pruning (history &key (target-tokens (effective-context-pruning-target-tokens)))
  "Returns the newest raw messages to keep after compressing HISTORY."
  (let ((minimum-keep-count 4)
        (kept nil)
        (kept-tokens 0))
    (dolist (message (reverse history))
      (let ((message-tokens (estimate-message-token-count message)))
        (when (or (< (length kept) minimum-keep-count)
                  (<= (+ kept-tokens message-tokens) target-tokens))
          (push message kept)
          (incf kept-tokens message-tokens))))
    (let* ((first-user-index
             (position-if (lambda (message)
                            (string= "user" (or (message-role-string message) "")))
                          kept)))
      (cond
        ((or (null kept) (zerop first-user-index))
         kept)
        (first-user-index
         (subseq kept first-user-index))
        (t
         kept)))))

(defun make-context-digest-message (digest)
  "Returns a synthetic system message containing DIGEST."
  (list (cons "role" "system")
        (cons "content" (format nil "[State Digest of previous turns: ~A]" digest))))

(defun build-compressed-history-from-raw-messages (history raw-messages digest)
  "Returns HISTORY compressed with DIGEST plus RAW-MESSAGES, or HISTORY when no reduction occurs."
  (let* ((keep-count (length raw-messages))
         (history-len (length history)))
    (if (<= history-len keep-count)
        history
        (append (list (make-context-digest-message digest))
               raw-messages))))

(defun compressed-conversation-history-if-needed (conversation &optional (history (conversation-messages conversation)))
  "Returns CONVERSATION history compressed when its estimated prompt context exceeds the configured limit."
  (let* ((fixed-context-tokens (estimated-fixed-conversation-context-token-count conversation))
         (history-tokens (estimated-history-token-count history))
         (estimated-total-tokens (estimated-conversation-context-token-count conversation history))
         (max-tokens (effective-context-pruning-max-tokens conversation))
         (target-tokens (effective-context-pruning-target-tokens conversation))
         (history-max-tokens (effective-history-compression-max-tokens conversation))
         (history-target-tokens (effective-history-compression-target-tokens conversation)))
    (if (or (<= estimated-total-tokens max-tokens)
            (<= history-tokens history-max-tokens)
            (<= history-max-tokens 0))
        history
        (labels ((compress-with-raw-target (raw-target-tokens)
                  (let* ((raw-messages (select-recent-messages-for-pruning history
                                                                           :target-tokens raw-target-tokens))
                         (keep-count (length raw-messages))
                         (history-len (length history)))
                    (if (<= history-len keep-count)
                        (list :history history
                              :old-messages nil
                              :raw-messages raw-messages
                              :digest nil)
                        (let* ((old-messages (subseq history 0 (- history-len keep-count)))
                              (digest (summarize-old-history old-messages conversation)))
                          (list :history (build-compressed-history-from-raw-messages history raw-messages digest)
                                :old-messages old-messages
                                :raw-messages raw-messages
                                :digest digest))))))
         (let* ((initial-pass (compress-with-raw-target history-target-tokens))
                (initial-history (getf initial-pass :history))
                (initial-digest (getf initial-pass :digest))
                (initial-digest-message (and initial-digest
                                             (make-context-digest-message initial-digest)))
                (initial-estimated-tokens (estimated-conversation-context-token-count conversation initial-history))
                (retry-raw-target-tokens
                  (and initial-digest-message
                       (> initial-estimated-tokens target-tokens)
                       (max 1
                            (- history-target-tokens
                               (estimate-message-token-count initial-digest-message)))))
                (final-pass
                  (if (and retry-raw-target-tokens
                           (< retry-raw-target-tokens history-target-tokens))
                      (compress-with-raw-target retry-raw-target-tokens)
                      initial-pass))
                (compressed-history (getf final-pass :history))
                (next-adaptive-max-tokens
                  (min (configured-context-pruning-max-tokens)
                       (max 1
                            (* 2
                               (estimated-conversation-context-token-count conversation
                                                                         compressed-history)))))
                (old-messages (getf final-pass :old-messages))
                (raw-messages (getf final-pass :raw-messages))
                (digest (getf final-pass :digest))
                (breakdown (conversation-context-token-breakdown conversation compressed-history)))
           (when digest
             (log-message :info "Compressed conversation history context after completed turn"
                          :context `(("estimated-total-tokens" . ,(princ-to-string estimated-total-tokens))
                                     ("history-tokens" . ,(princ-to-string history-tokens))
                                     ("fixed-context-tokens" . ,(princ-to-string fixed-context-tokens))
                                     ("history-max-tokens" . ,(princ-to-string history-max-tokens))
                                     ("history-target-tokens" . ,(princ-to-string history-target-tokens))
                                     ("effective-max-tokens" . ,(princ-to-string max-tokens))
                                     ("effective-target-tokens" . ,(princ-to-string target-tokens))
                                     ("next-effective-max-tokens" . ,(princ-to-string next-adaptive-max-tokens))
                                     ("compressed-total-tokens" . ,(princ-to-string (getf breakdown :total-tokens)))
                                     ("compressed-history-tokens" . ,(princ-to-string (getf breakdown :history-tokens)))
                                     ("compressed-digest-message-tokens" . ,(princ-to-string (getf breakdown :digest-message-tokens)))
                                     ("compressed-non-digest-history-tokens" . ,(princ-to-string (getf breakdown :non-digest-history-tokens)))
                                     ("old-messages-count" . ,(princ-to-string (length old-messages)))
                                     ("kept-messages-count" . ,(princ-to-string (length raw-messages)))
                                     ("digest-length" . ,(princ-to-string (length digest))))))
           compressed-history)))))

(defun compress-conversation-context-if-needed (conversation)
  "Applies post-response compression to CONVERSATION when its stored history is oversized."
  (let* ((original-history (conversation-messages conversation))
         (compressed-history
          (compressed-conversation-history-if-needed conversation original-history)))
    (setf (conversation-messages conversation) compressed-history)
    (unless (eq compressed-history original-history)
      (update-adaptive-context-pruning-max-tokens conversation compressed-history)
      (setf (conversation-interaction-id conversation) nil))
    compressed-history))

(defun prune-conversation-context-if-needed (conversation)
  "Returns CONVERSATION's compressed history for compatibility with older callers."
  (compressed-conversation-history-if-needed conversation))

(defun transient-plan-system-instruction-paragraph-p (paragraph)
  "Returns true when PARAGRAPH is one previously loaded transient plan paragraph."
  (and (stringp paragraph)
       (alexandria:starts-with-subseq +transient-plan-system-instruction-prefix+ paragraph)))

(defun replace-transient-plan-system-instruction (bot paragraph)
  "Stores PARAGRAPH on BOT while replacing any older transient loaded-plan paragraph."
  (let* ((current-paragraphs (coerce (current-system-instruction-paragraphs bot) 'list))
         (preserved-paragraphs
           (remove-if #'transient-plan-system-instruction-paragraph-p current-paragraphs)))
    (replace-system-instruction-paragraphs bot (append preserved-paragraphs (list paragraph)))))

(defun load-plan-to-system-instructions (bot filename)
  "Reads the generated Markdown plan from FILENAME and stores it on BOT's transient system-instruction."
  (let* ((filepath (merge-pathnames filename (uiop:getcwd)))
         (content (and (probe-file filepath) (uiop:read-file-string filepath))))
    (unless content
      (error "Plan file not found: ~A" filename))
    (let ((plan-inst (format nil "~&[EXECUTING PLAN FROM ~A]:~%~A" filename content)))
      (replace-transient-plan-system-instruction bot plan-inst)
      (log-message :info "Ingested plan as transient system instruction"
                   :context `(("file" . ,filename)))
      (format nil "Plan from ~A successfully loaded as a transient system instruction." filename))))

(defun restore-conversation-from-checkpoint (filename &key runtime-context)
  "Loads the conversation checkpoint from FILENAME (in the minions-data-directory) and returns a restored conversation instance."
  (let* ((dir (minions-data-directory))
         (file-path (merge-pathnames filename dir)))
    (unless (probe-file file-path)
      (error "Checkpoint file not found: ~A" (namestring file-path)))
    (let* ((raw-text (uiop:read-file-string file-path))
           (restoration
             (decode-persisted-conversation-state
              (cl-json:decode-json-from-string raw-text)
              :runtime-context runtime-context)))
      (unless (getf restoration :checkpoint-name)
       (setf (getf restoration :checkpoint-name) (pathname-name file-path)))
      (let ((conv (instantiate-conversation-from-restored-state restoration)))
       (log-message :info "Restored conversation from checkpoint"
                    :context `(("file" . ,(namestring file-path))))
       conv))))
