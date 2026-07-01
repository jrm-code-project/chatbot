;;; -*- Lisp -*-
;;; conversations.lisp - conversation constructors and persona entry points

(in-package "CHATBOT")

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
  (let* ((base-context (resolve-runtime-context runtime-context :sync-from-globals-p t))
        (loop-default-backend (persona-config-agentic-loop-default-backend config))
        (loop-default-model (persona-config-agentic-loop-default-model config)))
    (if (or loop-default-backend loop-default-model)
       (call-with-runtime-context
        base-context
        (lambda ()
          (make-runtime-context :agentic-loop-default-backend loop-default-backend
                                :agentic-loop-default-model loop-default-model)))
       base-context)))

(defun new-chat (&key model system-instruction system-instruction-path (system-instruction-storage-kind :transient) temperature top-p google-search-p (gemini-fallback-to-google-p +default-gemini-fallback-to-google-p+) web-tools-p code-execution-p include-timestamp-p include-model-p enable-eval-p (enable-git-tools-p nil) filesystem-tools-p filesystem-root-directory filesystem-allowed-directories filesystem-allowlist-path (backend :gemini) runtime-context subordinates persona-name parent-name (depth 1) token-budget (spent-tokens 0) scoped-directory filesystem-read-only-p planner-p)
  "Creates a new chatbot instance and returns an initialized conversation object.
If model is NIL, a sensible default model is chosen based on the backend.
Personas are optional; use NEW-CHAT-PERSONA only when you want persona-specific
configuration, instructions, or preloaded memory."
  (let ((resolved-context (resolve-runtime-context runtime-context :sync-from-globals-p t)))
    (call-with-runtime-context
     resolved-context
     (lambda ()
      (maybe-auto-initialize-startup-chatbot resolved-context)
      (let* ((chosen-model (or model
                               (backend-default-model backend)))
             (bot (make-instance 'chatbot
                                 :persona-name persona-name
                                 :model chosen-model
                                 :backend backend
                                 :system-instruction system-instruction
                                 :system-instruction-path system-instruction-path
                                 :system-instruction-storage-kind system-instruction-storage-kind
                                 :temperature (normalize-chatbot-temperature temperature :allow-nil-p t)
                                 :top-p (normalize-chatbot-top-p top-p :allow-nil-p t)
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
        (when (startup-chatbot-mcp-servers resolved-context)
          (setf (chatbot-mcp-servers bot)
                (startup-chatbot-mcp-servers resolved-context))
          (setf (chatbot-mcp-startup-status bot)
                (startup-chatbot-mcp-status resolved-context)))
        (make-instance 'conversation :chatbot bot))))))

(defun new-chat-persona (persona-name &key runtime-context parent-name (depth 1) token-budget (spent-tokens 0) scoped-directory (web-tools-p nil web-tools-supplied-p) (enable-git-tools-p nil enable-git-tools-supplied-p) (filesystem-tools-p nil filesystem-tools-supplied-p) (filesystem-read-only-p nil filesystem-read-only-supplied-p) (planner-p nil planner-supplied-p))
  "Creates a new chat session for a given chatbot persona.
The persona's configuration is read from ~/.Personas/<persona-name>/config.lisp
and the system instructions are loaded from the persona's system-instruction file set.
Use NEW-CHAT instead when no persona should be loaded."
  (let* ((persona-dir (resolve-persona-directory persona-name))
        (config-path (probe-file (merge-pathnames "config.lisp" persona-dir)))
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
                          :subordinates (loop for sub-persona in (safe-getf config :subordinates)
                                              collect (new-chat-persona sub-persona :runtime-context runtime-context))
                          :persona-name persona-name
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
        conversation))))

(defvar *minions-data-directory* nil
  "Seam to override the dynamic minions storage directory in unit tests.")

(defun minions-data-directory ()
  "Returns the directory where minion checkpoint states are persisted."
  (or *minions-data-directory*
      (let* ((base-dir (or (and (find-package "ASDF")
                                (asdf:system-source-directory "chatbot"))
                           (uiop:getcwd)))
             (path (merge-pathnames "data/minions/" base-dir)))
        (uiop:ensure-directory-pathname path))))

(defun save-minion-state (conversation)
  "Serializes the critical state and telemetry of CONVERSATION to disk."
  (let* ((bot (conversation-chatbot conversation))
         (name (chatbot-persona-name bot)))
    (when name
      (let* ((dir (minions-data-directory))
             (file-path (merge-pathnames (format nil "~A.json" name) dir))
             (state-plist
              (list :name name
                    :backend (string-downcase (symbol-name (chatbot-backend bot)))
                    :model (or (chatbot-model bot) "")
                    :parent-name (chatbot-parent-name bot)
                    :depth (chatbot-depth bot)
                    :token-budget (chatbot-token-budget bot)
                    :spent-tokens (chatbot-spent-tokens bot)
                    :scoped-directory (and (chatbot-scoped-directory bot)
                                           (namestring (chatbot-scoped-directory bot)))
                    :system-instruction (let ((inst (chatbot-system-instruction bot)))
                                          (cond
                                            ((null inst) "")
                                            ((stringp inst) inst)
                                            ((vectorp inst) (coerce inst 'list))
                                            (t "")))
                    :interaction-id (or (conversation-interaction-id conversation) "")
                    :messages (conversation-messages conversation))))
        (ensure-directories-exist file-path)
        (with-open-file (stream file-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
          (write-string (cl-json:encode-json-to-string state-plist) stream))
        (log-message :info "Freeze-dried minion state"
                     :context `(("name" . ,name) ("file" . ,(namestring file-path))))
        (namestring file-path)))))

(defun get-string-plist-value (plist key)
  "Gets the value associated with KEY (a string) in a string-keyed PLIST."
  (loop for (k v) on plist by #'cddr
        when (string-equal k key)
        return v))

(defun get-message-field (msg key-kw key-str)
  "Safely retrieves a field value from MSG (supporting keyword alist, string alist, or plist)."
  (cond
    ((listp msg)
     (let ((assoc-val (or (assoc key-kw msg) (assoc key-str msg :test #'string-equal))))
       (if (consp assoc-val)
           (cdr assoc-val)
           (get-string-plist-value msg key-str))))
    (t nil)))

(defun parse-minion-state-file (file)
  "Returns FILE decoded from JSON, or NIL after logging a warning."
  (handler-case
      (cl-json:decode-json-from-string (uiop:read-file-string file))
    (error (e)
      (log-message :warn "Failed to parse minion state file"
                  :context `(("file" . ,(namestring file))
                             ("error" . ,(princ-to-string e))))
      nil)))

(defun minion-state-depth (state)
  "Returns STATE's depth, defaulting to 1."
  (or (get-string-plist-value state "depth") 1))

(defun load-sorted-minion-states (directory)
  "Returns DIRECTORY's minion states sorted shallowest-first."
  (sort (remove nil
               (mapcar #'parse-minion-state-file
                       (uiop:directory-files directory "*.json")))
        #'<
        :key #'minion-state-depth))

(defun normalize-restored-system-instruction (raw-system-instruction)
  "Returns RAW-SYSTEM-INSTRUCTION normalized for NEW-CHAT."
  (cond
    ((null raw-system-instruction) nil)
    ((stringp raw-system-instruction) raw-system-instruction)
    ((listp raw-system-instruction) (coerce raw-system-instruction 'vector))
    (t nil)))

(defun normalize-restored-message (msg)
  "Returns MSG normalized to the internal role/content alist shape."
  (list (cons "role" (get-message-field msg :role "role"))
        (cons "content" (get-message-field msg :content "content"))))

(defun restored-minion-history (messages)
  "Returns normalized restored MESSAGES with the crash-recovery handshake appended."
  (let ((normalized-messages
         (if (and messages (listp messages))
             (mapcar #'normalize-restored-message messages)
             nil)))
    (append normalized-messages
           (list (list (cons "role" "user")
                       (cons "content"
                             "[SYSTEM: Recovered from unexpected shutdown. Please review your context and resume your last uncompleted task.]"))))))

(defun minion-restoration-spec (state root-bot)
  "Returns a normalized restoration spec for one saved minion STATE."
  (let* ((name (get-string-plist-value state "name"))
        (backend-str (get-string-plist-value state "backend"))
        (scoped-dir-str (get-string-plist-value state "scopedDirectory")))
    (list :name name
         :backend (if (and backend-str (string/= backend-str ""))
                      (intern (string-upcase backend-str) "KEYWORD")
                      :gemini)
         :model (get-string-plist-value state "model")
         :parent-name (get-string-plist-value state "parentName")
         :depth (minion-state-depth state)
         :token-budget (get-string-plist-value state "tokenBudget")
         :spent-tokens (or (get-string-plist-value state "spentTokens") 0)
         :scoped-directory (and scoped-dir-str
                                (uiop:ensure-directory-pathname scoped-dir-str))
         :system-instruction
         (normalize-restored-system-instruction
          (get-string-plist-value state "systemInstruction"))
         :interaction-id (get-string-plist-value state "interactionId")
         :history (restored-minion-history (get-string-plist-value state "messages"))
         :runtime-context (chatbot-runtime-context root-bot))))

(defun instantiate-restored-minion (restoration)
  "Returns one restored subordinate conversation from RESTORATION."
  (let* ((name (getf restoration :name))
        (sub-conv (new-chat :backend (getf restoration :backend)
                            :model (getf restoration :model)
                            :system-instruction (getf restoration :system-instruction)
                            :parent-name (getf restoration :parent-name)
                            :depth (getf restoration :depth)
                            :token-budget (getf restoration :token-budget)
                            :spent-tokens (getf restoration :spent-tokens)
                            :scoped-directory (getf restoration :scoped-directory)
                            :runtime-context (getf restoration :runtime-context)))
        (sub-bot (conversation-chatbot sub-conv))
        (interaction-id (getf restoration :interaction-id)))
    (when name
      (terminate-active-threads-by-name name)
      (setf (chatbot-persona-name sub-bot) name))
    (when (and interaction-id (string/= interaction-id ""))
      (setf (conversation-interaction-id sub-conv) interaction-id))
    (setf (conversation-messages sub-conv) (getf restoration :history))
    sub-conv))

(defun restoration-parent-is-root-p (restoration root-bot)
  "Returns true when RESTORATION should attach directly beneath ROOT-BOT."
  (let ((parent-name (getf restoration :parent-name)))
    (or (null parent-name)
        (string= parent-name "")
        (string-equal parent-name (chatbot-persona-name root-bot)))))

(defun attach-restored-minion (root-bot restored-convs restoration sub-conv)
  "Attaches SUB-CONV according to RESTORATION using RESTORED-CONVS for parent lookup."
  (let ((name (getf restoration :name))
        (parent-name (getf restoration :parent-name)))
    (when name
      (setf (gethash name restored-convs) sub-conv))
    (if (restoration-parent-is-root-p restoration root-bot)
        (attach-subordinate-conversation root-bot sub-conv)
        (let ((parent-conv (gethash parent-name restored-convs)))
         (if parent-conv
             (attach-subordinate-conversation (conversation-chatbot parent-conv) sub-conv)
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
                 (mapcar (lambda (state)
                           (minion-restoration-spec state root-bot))
                         (load-sorted-minion-states dir)))
          (let ((sub-conv (instantiate-restored-minion restoration)))
            (attach-restored-minion root-bot restored-convs restoration sub-conv))))
      (log-message :info "MCRS: Restoration bootloader completed successfully."))))

(defun summarize-old-history (messages bot)
  "Sends the old conversation history to the LLM to generate a concise State Digest."
  (let* ((history-text
          (with-output-to-string (stream)
            (dolist (msg messages)
              (format stream "~A: ~A~%"
                      (cdr (assoc "role" msg :test #'string=))
                      (cdr (assoc "content" msg :test #'string=))))))
         (prompt (format nil "Please read the following conversation history and write a highly concise, dense 'State Digest' summarizing all key factual information, state, progress, and memories from it. Output only the State Digest, nothing else: ~%~%~A" history-text))
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
    summary))

(defun prune-conversation-context-if-needed (conversation)
  "Returns CONVERSATION's effective history after pruning oversized context when needed."
  (let* ((bot (conversation-chatbot conversation))
         (history (conversation-messages conversation))
         (total-len (loop for msg in history
                          sum (length (cdr (assoc "content" msg :test #'string=))))))
    (if (<= total-len *context-pruning-threshold-characters*)
        history
        (let* ((keep-count 4)
               (history-len (length history)))
          (if (<= history-len keep-count)
              history
              (let* ((old-messages (subseq history 0 (- history-len keep-count)))
                     (raw-messages (subseq history (- history-len keep-count)))
                     (digest (summarize-old-history old-messages bot))
                     (digest-msg (list (cons "role" "system")
                                      (cons "content" (format nil "[State Digest of previous turns: ~A]" digest))))
                     (pruned-history (append (list digest-msg) raw-messages)))
                (log-message :info "Pruned and compressed conversation history context"
                            :context `(("old-messages-count" . ,(princ-to-string (length old-messages)))
                                       ("digest-length" . ,(princ-to-string (length digest)))))
                pruned-history))))))

(defun load-plan-to-system-instructions (bot filename)
  "Reads the generated Markdown plan from FILENAME and appends it to BOT's transient system-instruction."
  (let* ((filepath (merge-pathnames filename (uiop:getcwd)))
         (content (and (probe-file filepath) (uiop:read-file-string filepath))))
    (unless content
      (error "Plan file not found: ~A" filename))
    (let* ((curr (chatbot-system-instruction bot))
           (plan-inst (format nil "~&[EXECUTING PLAN FROM ~A]:~%~A" filename content)))
      (setf (chatbot-system-instruction bot)
            (cond
              ((null curr) plan-inst)
              ((stringp curr) (format nil "~A~%~A" curr plan-inst))
              ((vectorp curr)
               (coerce (append (coerce curr 'list) (list plan-inst)) 'vector))
              (t plan-inst)))
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
           (state (cl-json:decode-json-from-string raw-text))
           (backend-str (get-string-plist-value state "backend"))
           (backend-kw (if (and backend-str (string/= backend-str ""))
                           (intern (string-upcase backend-str) "KEYWORD")
                           :gemini))
           (model (get-string-plist-value state "model"))
           (system-instruction-raw (get-string-plist-value state "systemInstruction"))
           (system-instruction (cond
                                 ((null system-instruction-raw) nil)
                                 ((stringp system-instruction-raw) system-instruction-raw)
                                 ((listp system-instruction-raw) (coerce system-instruction-raw 'vector))
                                 (t nil)))
           (interaction-id (get-string-plist-value state "interactionId"))
           (messages (get-string-plist-value state "messages")))
      (let ((conv (new-chat :backend backend-kw
                            :model (and (string/= model "") model)
                            :system-instruction system-instruction
                            :runtime-context runtime-context)))
        (setf (conversation-interaction-id conv) (and (string/= interaction-id "") interaction-id))
        (when (and messages (listp messages))
          (setf (conversation-messages conv)
                (mapcar (lambda (msg)
                          (list (cons "role" (get-message-field msg :role "role"))
                                (cons "content" (get-message-field msg :content "content"))))
                        messages)))
        (log-message :info "Restored conversation from checkpoint"
                     :context `(("file" . ,(namestring file-path))))
        conv))))
