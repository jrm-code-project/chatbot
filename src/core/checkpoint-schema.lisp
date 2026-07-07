(in-package "CHATBOT")

(defun get-string-plist-value (plist key)
  "Gets the value associated with KEY (a string) in a string-keyed PLIST."
  (loop for (k v) on plist by #'cddr
        when (string-equal k key)
        return v))

(defun empty-string-p (value)
  "Returns true when VALUE is the empty string."
  (and (stringp value)
       (string= value "")))

(defun persisted-message-field (msg key-kw key-str)
  "Safely retrieves a field value from MSG (supporting keyword alist, string alist, or plist)."
  (cond
    ((listp msg)
     (let ((assoc-val (or (assoc key-kw msg)
                          (assoc key-str msg :test #'string-equal))))
       (if (consp assoc-val)
           (cdr assoc-val)
           (get-string-plist-value msg key-str))))
    (t nil)))

(defun persisted-backend-keyword (backend-str)
  "Returns BACKEND-STR normalized to a backend keyword."
  (if (and backend-str
           (not (empty-string-p backend-str)))
      (intern (string-upcase backend-str) "KEYWORD")
      :gemini))

(defun persisted-system-instruction-value (system-instruction)
  "Returns SYSTEM-INSTRUCTION normalized for persisted JSON state."
  (cond
    ((null system-instruction) "")
    ((stringp system-instruction) system-instruction)
    ((vectorp system-instruction) (coerce system-instruction 'list))
    (t "")))

(defun normalize-persisted-system-instruction (raw-system-instruction)
  "Returns RAW-SYSTEM-INSTRUCTION normalized for NEW-CHAT."
  (cond
    ((null raw-system-instruction) nil)
    ((empty-string-p raw-system-instruction) nil)
    ((stringp raw-system-instruction) raw-system-instruction)
    ((listp raw-system-instruction) (coerce raw-system-instruction 'vector))
    (t nil)))

(defun persisted-pathname-value (path)
  "Returns PATH normalized for persisted JSON state."
  (if path
     (namestring path)
     ""))

(defun normalize-persisted-pathname (raw-path &key directory-p)
  "Returns RAW-PATH normalized to a pathname, or NIL."
  (when (and raw-path
            (not (empty-string-p raw-path)))
    (if directory-p
       (uiop:ensure-directory-pathname raw-path)
       (pathname raw-path))))

(defun persisted-pathname-list-value (paths)
  "Returns PATHS normalized for persisted JSON state."
  (if paths
     (mapcar #'namestring paths)
     nil))

(defun normalize-persisted-pathname-list (raw-paths &key directory-p)
  "Returns RAW-PATHS normalized to a pathname list."
  (if (and raw-paths (listp raw-paths))
     (mapcar (lambda (raw-path)
               (normalize-persisted-pathname raw-path :directory-p directory-p))
             raw-paths)
     nil))

(defun normalize-persisted-system-instruction-storage-kind (raw-kind)
  "Returns RAW-KIND normalized to one canonical storage-kind keyword."
  (cond
    ((or (null raw-kind)
        (empty-string-p raw-kind))
    :transient)
    ((keywordp raw-kind)
    raw-kind)
    ((symbolp raw-kind)
    (intern (string-upcase (symbol-name raw-kind)) "KEYWORD"))
    ((stringp raw-kind)
    (intern (string-upcase raw-kind) "KEYWORD"))
    (t
    :transient)))

(defun persisted-real-value (value)
  "Returns VALUE normalized for persisted JSON state."
  (if (realp value)
     (princ-to-string value)
     nil))

(defun normalize-persisted-real-value (raw-value)
  "Returns RAW-VALUE normalized to a real number, or NIL."
  (cond
    ((null raw-value) nil)
    ((realp raw-value) (float raw-value 1.0d0))
    ((and (stringp raw-value)
         (not (empty-string-p raw-value)))
    (let ((parsed (read-from-string raw-value)))
      (when (realp parsed)
        (float parsed 1.0d0))))
    (t nil)))

(defun normalize-persisted-content-cache-policy (raw-policy)
  "Returns RAW-POLICY normalized for NEW-CHAT content-cache policy."
  (cond
    ((or (null raw-policy)
        (empty-string-p raw-policy))
    nil)
    ((string-equal raw-policy "auto") :auto)
    ((string-equal raw-policy "off") :off)
    (t nil)))

(defun normalize-persisted-message (msg)
  "Returns MSG normalized to the internal role/content alist shape."
  (list (cons "role" (persisted-message-field msg :role "role"))
        (cons "content" (persisted-message-field msg :content "content"))))

(defun normalize-persisted-history (messages)
  "Returns MESSAGES normalized to the internal role/content alist shape."
  (if (and messages (listp messages))
      (mapcar #'normalize-persisted-message messages)
      nil))

(defun recovery-handshake-message ()
  "Returns the synthetic crash-recovery handshake message."
  (list (cons "role" "user")
        (cons "content"
              "[SYSTEM: Recovered from unexpected shutdown. Please review your context and resume your last uncompleted task.]")))

(defun maybe-append-recovery-handshake (history append-recovery-handshake-p)
  "Returns HISTORY, optionally appending the synthetic recovery handshake."
  (if append-recovery-handshake-p
      (append history (list (recovery-handshake-message)))
      history))

(defun conversation-persistence-worker-kind (conversation)
  "Returns CONVERSATION's persisted worker kind string, or NIL."
  (let ((bot (conversation-chatbot conversation)))
    (cond
      ((chatbot-planner-p bot)
       "planner")
      ((chatbot-parent-name bot)
       "delegated")
      (t
       nil))))

(defun persisted-worker-kind-keyword (kind)
  "Returns KIND decoded to one canonical worker kind keyword, or NIL."
  (cond
    ((null kind) nil)
    ((keywordp kind)
     (case kind
       (:subordinate :delegated)
       (:autonomous :loop)
       ((:delegated :planner :loop) kind)
       (t nil)))
    ((symbolp kind)
     (persisted-worker-kind-keyword (symbol-name kind)))
    ((stringp kind)
     (let ((normalized (string-downcase kind)))
       (cond
         ((string= normalized "subordinate") :delegated)
         ((string= normalized "delegated") :delegated)
         ((string= normalized "planner") :planner)
         ((string= normalized "autonomous") :loop)
         ((string= normalized "loop") :loop)
         (t nil))))
    (t
     nil)))

(defun conversation-persistence-state (conversation &key name)
  "Returns CONVERSATION serialized to the shared persisted-state schema."
  (let ((bot (conversation-chatbot conversation)))
    (list :name (or name
                    (chatbot-checkpoint-name bot)
                    (chatbot-persona-name bot))
          :persona-name (or (chatbot-persona-name bot) "")
          :persona-source-name (or (chatbot-persona-source-name bot) "")
          :backend (string-downcase (symbol-name (chatbot-backend bot)))
          :model (or (chatbot-model bot) "")
          :system-instruction-path
          (persisted-pathname-value (chatbot-system-instruction-path bot))
          :system-instruction-storage-kind
          (string-downcase
           (symbol-name (chatbot-system-instruction-storage-kind bot)))
          :temperature (persisted-real-value (chatbot-temperature bot))
          :top-p (persisted-real-value (chatbot-top-p bot))
          :parent-name (chatbot-parent-name bot)
          :depth (chatbot-depth bot)
          :token-budget (chatbot-token-budget bot)
          :spent-tokens (chatbot-spent-tokens bot)
          :content-cache-policy (string-downcase (symbol-name (chatbot-content-cache-policy bot)))
          :content-cache-ttl-seconds (chatbot-content-cache-ttl-seconds bot)
          :content-cache-min-tokens (chatbot-content-cache-min-tokens bot)
          :google-search-p (chatbot-google-search-p bot)
          :gemini-fallback-to-google-p (chatbot-gemini-fallback-to-google-p bot)
          :web-tools-p (chatbot-web-tools-p bot)
          :code-execution-p (chatbot-code-execution-p bot)
          :include-timestamp-p (chatbot-include-timestamp-p bot)
          :include-model-p (chatbot-include-model-p bot)
          :enable-eval-p (chatbot-enable-eval-p bot)
          :enable-git-tools-p (chatbot-enable-git-tools-p bot)
          :filesystem-tools-p (chatbot-filesystem-tools-p bot)
          :filesystem-root-directory
          (persisted-pathname-value (chatbot-filesystem-root-directory bot))
          :filesystem-allowed-directories
          (persisted-pathname-list-value (chatbot-filesystem-allowed-directories bot))
          :filesystem-allowlist-path
          (persisted-pathname-value (chatbot-filesystem-allowlist-path bot))
          :filesystem-read-only-p (chatbot-filesystem-read-only-p bot)
          :scoped-directory (and (chatbot-scoped-directory bot)
                                 (namestring (chatbot-scoped-directory bot)))
          :system-instruction (persisted-system-instruction-value
                               (chatbot-system-instruction bot))
          :worker-kind (conversation-persistence-worker-kind conversation)
          :adaptive-context-pruning-max-tokens
          (conversation-adaptive-context-pruning-max-tokens conversation)
          :interaction-id (or (conversation-interaction-id conversation) "")
          :cached-content-name (or (conversation-cached-content-name conversation) "")
          :cached-content-key (or (conversation-cached-content-key conversation) "")
          :cached-content-metadata (or (conversation-cached-content-metadata conversation) nil)
          :messages (conversation-messages conversation))))

(defun decode-persisted-conversation-state (state &key runtime-context append-recovery-handshake-p)
  "Returns STATE decoded from the shared persisted-state schema."
  (let* ((name (get-string-plist-value state "name"))
         (persona-name (get-string-plist-value state "personaName"))
         (persona-source-name (get-string-plist-value state "personaSourceName"))
         (backend-str (get-string-plist-value state "backend"))
         (model (get-string-plist-value state "model"))
         (system-instruction-path (get-string-plist-value state "systemInstructionPath"))
         (parent-name (get-string-plist-value state "parentName"))
         (scoped-dir-str (get-string-plist-value state "scopedDirectory"))
         (spent-tokens (get-string-plist-value state "spentTokens"))
         (history (normalize-persisted-history
                   (get-string-plist-value state "messages"))))
    (list :name (if (empty-string-p name) nil name)
          :checkpoint-name (if (empty-string-p name) nil name)
          :persona-name (if (empty-string-p persona-name) nil persona-name)
          :persona-source-name (if (empty-string-p persona-source-name) nil persona-source-name)
          :backend (persisted-backend-keyword backend-str)
          :model (if (empty-string-p model) nil model)
          :system-instruction-path
          (normalize-persisted-pathname system-instruction-path)
          :system-instruction-storage-kind
          (normalize-persisted-system-instruction-storage-kind
           (get-string-plist-value state "systemInstructionStorageKind"))
          :temperature (normalize-persisted-real-value
                        (get-string-plist-value state "temperature"))
          :top-p (normalize-persisted-real-value
                  (get-string-plist-value state "topP"))
          :parent-name (if (empty-string-p parent-name) nil parent-name)
          :depth (or (get-string-plist-value state "depth") 1)
          :token-budget (get-string-plist-value state "tokenBudget")
          :spent-tokens (or spent-tokens 0)
          :content-cache-policy (normalize-persisted-content-cache-policy
                                 (get-string-plist-value state "contentCachePolicy"))
          :content-cache-ttl-seconds (get-string-plist-value state "contentCacheTtlSeconds")
          :content-cache-min-tokens (get-string-plist-value state "contentCacheMinTokens")
          :google-search-p (get-string-plist-value state "googleSearchP")
          :gemini-fallback-to-google-p (get-string-plist-value state "geminiFallbackToGoogleP")
          :web-tools-p (get-string-plist-value state "webToolsP")
          :code-execution-p (get-string-plist-value state "codeExecutionP")
          :include-timestamp-p (get-string-plist-value state "includeTimestampP")
          :include-model-p (get-string-plist-value state "includeModelP")
          :enable-eval-p (get-string-plist-value state "enableEvalP")
          :enable-git-tools-p (get-string-plist-value state "enableGitToolsP")
          :filesystem-tools-p (get-string-plist-value state "filesystemToolsP")
          :filesystem-root-directory
          (normalize-persisted-pathname
           (get-string-plist-value state "filesystemRootDirectory")
           :directory-p t)
          :filesystem-allowed-directories
          (normalize-persisted-pathname-list
           (get-string-plist-value state "filesystemAllowedDirectories")
           :directory-p t)
          :filesystem-allowlist-path
          (normalize-persisted-pathname
           (get-string-plist-value state "filesystemAllowlistPath"))
          :filesystem-read-only-p (get-string-plist-value state "filesystemReadOnlyP")
          :scoped-directory (and scoped-dir-str
                                 (not (empty-string-p scoped-dir-str))
                                 (uiop:ensure-directory-pathname scoped-dir-str))
          :system-instruction
          (normalize-persisted-system-instruction
           (get-string-plist-value state "systemInstruction"))
          :worker-kind
          (persisted-worker-kind-keyword
           (get-string-plist-value state "workerKind"))
          :adaptive-context-pruning-max-tokens
          (get-string-plist-value state "adaptiveContextPruningMaxTokens")
          :interaction-id (let ((interaction-id (get-string-plist-value state "interactionId")))
                            (if (empty-string-p interaction-id)
                                nil
                                interaction-id))
          :cached-content-name (let ((name (get-string-plist-value state "cachedContentName")))
                                 (if (empty-string-p name)
                                     nil
                                     name))
          :cached-content-key (let ((key (get-string-plist-value state "cachedContentKey")))
                                (if (empty-string-p key)
                                    nil
                                    key))
          :cached-content-metadata (get-string-plist-value state "cachedContentMetadata")
          :history (maybe-append-recovery-handshake history append-recovery-handshake-p)
          :runtime-context runtime-context)))
