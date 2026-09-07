;;; -*- Lisp -*-
;;; conversation-persistence.lisp - minion state checkpointing and restoration

(in-package "CHATBOT")

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

(defun save-minion-state (conversation &key checkpoint-name)
  "Serializes the critical state and telemetry of CONVERSATION to disk atomically."
  (let* ((bot (conversation-chatbot conversation))
         (name (or checkpoint-name
                   (chatbot-checkpoint-name bot))))
    (unless (and name (stringp name) (string/= name ""))
      (error "Attempted to save minion state without a valid checkpoint name."))
    (let* ((dir (minions-data-directory))
           (file-path (merge-pathnames (format nil "~A.json" name) dir))
           (tmp-path (make-pathname :type "tmp" :defaults file-path))
           (state-plist (conversation-persistence-state conversation :name name)))
        (ensure-directories-exist tmp-path)
        (with-open-file (stream tmp-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
          (write-string (cl-json:encode-json-to-string state-plist) stream))
        (uiop:rename-file-overwriting-target tmp-path file-path)
        (log-message :info "Freeze-dried minion state"
                     :context `(("name" . ,name) ("file" . ,(namestring file-path))))
        (namestring file-path))))

(defun checkpoint-conversation-after-chat (conversation)
  "Persists CONVERSATION using the standard post-chat checkpoint naming policy."
  (let ((checkpoint-name (conversation-checkpoint-name conversation)))
    (log-message :info "Checkpointing conversation after chat"
                :context `(("name" . ,checkpoint-name)))
    (save-minion-state conversation :checkpoint-name checkpoint-name)))

(defun finalize-chat-turn-result (result &optional conversation)
  "Applies RESULT, performs post-response compression, checkpoints, speaks the response, and returns the final text."
  (let ((effective-conversation (or conversation
                                    (chat-turn-result-conversation result))))
    (let ((text (apply-chat-turn-result result effective-conversation)))
      (when effective-conversation
        (decrement-prompt-decorations effective-conversation)
        (compress-conversation-context-if-needed effective-conversation)
        (checkpoint-conversation-after-chat effective-conversation))
      (speak-chat-response-in-background text)
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
        (chatbot-include-elapsed-time-p bot)
        (getf restoration :include-elapsed-time-p)
        (chatbot-enable-eval-p bot)
        (getf restoration :enable-eval-p)
        (chatbot-enable-shell-p bot)
        (getf restoration :enable-shell-p)
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
        (chatbot-inbox-s3-path bot)
        (getf restoration :inbox-s3-path)
        (chatbot-planner-p bot)
        (restoration-planner-p restoration))
  bot)

(defun apply-restored-conversation-state (conversation restoration)
  "Applies persisted conversation state from RESTORATION onto CONVERSATION."
  (setf (conversation-checkpoint-name conversation)
        (getf restoration :checkpoint-name))
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
        (conversation-turns-since-cache-reload conversation)
        (getf restoration :turns-since-cache-reload 0)
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
                         :include-elapsed-time-p (getf restoration :include-elapsed-time-p)
                         :enable-eval-p (getf restoration :enable-eval-p)
                         :enable-git-tools-p (getf restoration :enable-git-tools-p)
                         :filesystem-tools-p (getf restoration :filesystem-tools-p)
                         :filesystem-root-directory (getf restoration :filesystem-root-directory)
                         :filesystem-allowed-directories
                         (getf restoration :filesystem-allowed-directories)
                         :filesystem-allowlist-path (getf restoration :filesystem-allowlist-path)
                         :filesystem-read-only-p (getf restoration :filesystem-read-only-p)
                         :inbox-s3-path (getf restoration :inbox-s3-path)
                         :parent-name (getf restoration :parent-name)
                         :depth (getf restoration :depth)
                         :token-budget (getf restoration :token-budget)
                         :spent-tokens (getf restoration :spent-tokens)
                         :planner-p (restoration-planner-p restoration)
                         :scoped-directory (getf restoration :scoped-directory)
                         :runtime-context (getf restoration :runtime-context)
                         :cached-content-name (getf restoration :cached-content-name)
                         :cached-content-key (getf restoration :cached-content-key)
                         :cached-content-metadata (getf restoration :cached-content-metadata)
                         :turns-since-cache-reload (getf restoration :turns-since-cache-reload 0)))))
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
