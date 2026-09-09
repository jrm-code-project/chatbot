;;;

(in-package "CHATBOT")

;;; Chatbot construction/teardown lifecycle: the CHATBOT class's
;;; INITIALIZE-INSTANCE :AFTER method (which builds the composed
;;; sub-component objects from flat keyword args), conversation checkpoint
;;; naming, context-pruning tuning constants, and the WITH-CHATBOT-LIFECYCLE
;;; convenience macro.

(defmethod initialize-instance :after ((bot chatbot) &rest initargs &key
                                       identity llm-config prompt-config cache-config tool-config mcp-state minion-state
                                       persona-name persona-source-name checkpoint-name
                                       model (backend nil backend-supplied-p) temperature top-p
                                       system-instruction system-instruction-path system-instruction-storage-kind
                                       include-timestamp-p include-model-p include-elapsed-time-p gemini-fallback-to-google-p
                                       content-cache-policy content-cache-ttl-seconds content-cache-min-tokens
                                       google-search-p web-tools-p code-execution-p enable-eval-p enable-shell-p enable-git-tools-p
                                       filesystem-tools-p filesystem-root-directory filesystem-allowed-directories
                                       filesystem-allowlist-path filesystem-read-only-p scoped-directory inbox-s3-path
                                       mcp-servers mcp-startup-status
                                       subordinates parent-name (depth nil depth-supplied-p) token-budget spent-tokens planner-p
                                       task-journal task-journal-lock
                                       &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value bot 'identity)
        (or identity
            (make-instance 'chatbot-identity
                           :persona-name persona-name
                           :persona-source-name persona-source-name
                           :checkpoint-name (or checkpoint-name "DefaultConversation"))))
  (setf (slot-value bot 'llm-config)
        (or llm-config
            (make-instance 'chatbot-llm-config
                           :model model
                           :backend (if backend-supplied-p backend :gemini)
                           :temperature temperature
                           :top-p top-p)))
  (setf (slot-value bot 'prompt-config)
        (or prompt-config
            (make-instance 'chatbot-prompt-config
                           :system-instruction system-instruction
                           :system-instruction-path system-instruction-path
                           :system-instruction-storage-kind (or system-instruction-storage-kind :transient)
                           :include-timestamp-p include-timestamp-p
                           :include-model-p include-model-p
                           :include-elapsed-time-p include-elapsed-time-p
                           :gemini-fallback-to-google-p (or gemini-fallback-to-google-p +default-gemini-fallback-to-google-p+))))
  (setf (slot-value bot 'cache-config)
        (or cache-config
            (make-instance 'chatbot-cache-config
                           :content-cache-policy (or content-cache-policy :auto)
                           :content-cache-ttl-seconds content-cache-ttl-seconds
                           :content-cache-min-tokens content-cache-min-tokens)))
  (setf (slot-value bot 'tool-config)
        (or tool-config
            (make-instance 'chatbot-tool-config
                           :google-search-p google-search-p
                           :web-tools-p web-tools-p
                           :code-execution-p code-execution-p
                           :enable-eval-p enable-eval-p
                           :enable-shell-p enable-shell-p
                           :enable-git-tools-p enable-git-tools-p
                           :filesystem-tools-p filesystem-tools-p
                           :filesystem-root-directory filesystem-root-directory
                           :filesystem-allowed-directories filesystem-allowed-directories
                           :filesystem-allowlist-path filesystem-allowlist-path
                           :filesystem-read-only-p filesystem-read-only-p
                           :scoped-directory scoped-directory
                           :inbox-s3-path inbox-s3-path)))
  (setf (slot-value bot 'mcp-state)
        (or mcp-state
            (make-instance 'chatbot-mcp-state
                           :mcp-servers mcp-servers
                           :mcp-startup-status mcp-startup-status)))
  (setf (slot-value bot 'minion-state)
        (or minion-state
            (make-instance 'chatbot-minion-state
                           :subordinates subordinates
                           :parent-name parent-name
                           :depth (if depth-supplied-p depth 1)
                           :token-budget token-budget
                           :spent-tokens (or spent-tokens 0)
                           :planner-p planner-p
                           :task-journal (or task-journal (make-hash-table :test 'equal))
                           :task-journal-lock (or task-journal-lock (sb-thread:make-mutex :name "chatbot-task-journal-lock")))))
  ;; Applies backend-sensitive defaults for chatbot instances created without an explicit model.
  (setf (chatbot-backend bot)
        (normalize-chatbot-backend (chatbot-backend bot) "chatbot"))
  (when (null (chatbot-model bot))
    (setf (chatbot-model bot)
          (backend-default-model (chatbot-backend bot)))))

(defun conversation-checkpoint-name (conversation)
  "Returns the validated persistence name used when checkpointing CONVERSATION."
  (let ((name (or (and (slot-boundp conversation 'checkpoint-name)
                       (%conversation-checkpoint-name conversation))
                  (chatbot-checkpoint-name (conversation-chatbot conversation)))))
    (unless (and name (stringp name) (string/= name ""))
      (error "Conversation is missing an explicit checkpoint name identifier."))
    (let ((bot-name (chatbot-checkpoint-name (conversation-chatbot conversation))))
      (unless (and bot-name (stringp bot-name) (string/= bot-name ""))
        (error "Chatbot is missing an explicit checkpoint name identifier.")))
    name))

(defun (setf conversation-checkpoint-name) (new-value conversation)
  "Sets the persistence name used when checkpointing CONVERSATION after validation."
  (unless (and new-value (stringp new-value) (string/= new-value ""))
    (error "Checkpoint name must be a non-empty string."))
  (setf (%conversation-checkpoint-name conversation) new-value))

(defparameter *max-minion-depth* 3
  "The global maximum nesting depth allowed for the minion hierarchy.")

(defparameter *context-pruning-estimated-max-tokens* 200000
  "Estimated prompt-token ceiling above which completed conversation history is auto-compressed.
This is the fallback ceiling used when the conversation's model is unknown or is not a
Gemini Pro-tier model; see *gemini-pro-context-price-cliff-tokens* for the Pro-specific ceiling.")

(defparameter *context-pruning-estimated-target-tokens* 150000
  "Estimated prompt-token target after compressing oversized conversation history.
This is the fallback target used when the conversation's model is unknown or is not a
Gemini Pro-tier model; see *context-pruning-pro-target-ratio* for the Pro-specific target.")

(defparameter *context-pruning-threshold-characters* 300000
  "Compatibility character ceiling for auto-pruning, aligned with the default estimated token window.")

(defparameter *gemini-pro-context-price-cliff-tokens* 200000
  "Prompt-token count at which Gemini Pro-tier pricing doubles for the entire request
(per Google's published pricing: prompts over this size are billed at the higher tier,
not just the excess). Used to keep Pro conversations well clear of this cliff.")

(defparameter *gemini-pro-context-pruning-safety-margin-tokens* 60000
  "Safety margin subtracted from *gemini-pro-context-price-cliff-tokens* when computing the
auto-compression ceiling for Gemini Pro-tier conversations, so compression fires with
enough headroom to never let a request cross the price cliff.")

(defparameter *context-pruning-pro-target-ratio* 0.35
  "Fraction of the effective max-token ceiling that Gemini Pro-tier conversations are
compressed down to. Kept aggressively low (relative to the ~0.75 ratio implied by the
generic defaults) because Pro's per-token price is high and this target governs the
average history size resent on every subsequent turn.")

(defmacro with-chatbot-lifecycle ((bot &rest initargs) &body body)
  "Evaluates BODY with BOT bound to a newly created chatbot, ensuring full shutdown on exit."
  (let ((context-var (gensym "CONTEXT")))
    `(let* ((,context-var (make-instance 'runtime-context))
            (,bot (make-instance 'chatbot :runtime-context ,context-var ,@initargs)))
       (unwind-protect
            (call-with-runtime-context ,context-var
              (lambda () ,@body))
         (shutdown-chatbot ,bot)))))
