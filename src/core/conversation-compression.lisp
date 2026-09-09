;;; -*- Lisp -*-
;;; conversation-compression.lisp - conversation context compression and digest generation

(in-package "CHATBOT")

(defparameter +state-digest-message-prefix+ "[State Digest of previous turns: "
  "Prefix used for synthetic digest messages inserted during context compression.")

(defparameter +state-digest-message-suffix+ "]"
  "Suffix used for synthetic digest messages inserted during context compression.")

(defparameter *context-pruning-max-digest-tokens* 2048
  "Absolute upper bound for synthetic state-digest size after pruning.")

(defparameter +transient-plan-system-instruction-prefix+ "[EXECUTING PLAN FROM "
  "Marker prefix for transient system-instruction paragraphs loaded from plans.")

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
         ;; Use a clean, stateless conversation to avoid nested pruning loops.
         ;; Always summarize with a cheap model, regardless of the parent
         ;; conversation's (potentially expensive) model, since digest
         ;; generation is the largest single input this system ever sends.
         (conv (new-chat :backend (chatbot-backend bot)
                         :model (cheap-summarization-model (chatbot-backend bot) (chatbot-model bot))
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
  (reduce #'+ messages :key #'estimate-message-token-count :initial-value 0))

(defun estimated-digest-message-token-count (messages)
  "Returns the estimated token usage attributable to synthetic digest messages."
  (reduce #'+ (remove-if-not #'state-digest-message-p messages)
          :key #'estimate-message-token-count
          :initial-value 0))

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

(defun configured-context-pruning-max-tokens (&optional model)
  "Returns the configured estimated max-token ceiling before per-conversation adaptation.
When MODEL names a Gemini Pro-tier model, the ceiling is additionally clamped to stay a
safety margin below the Pro price cliff (*gemini-pro-context-price-cliff-tokens*), since
a request that crosses that cliff is billed at roughly double the per-token rate."
  (let ((max-tokens (if (and model (gemini-pro-model-p model))
                        (min *context-pruning-estimated-max-tokens*
                             (max 1 (- *gemini-pro-context-price-cliff-tokens*
                                       *gemini-pro-context-pruning-safety-margin-tokens*)))
                        *context-pruning-estimated-max-tokens*))
        (char-threshold *context-pruning-threshold-characters*))
    (if (and char-threshold (> char-threshold 0))
        (min max-tokens
             (estimate-text-token-count (make-string char-threshold :initial-element #\X)))
        max-tokens)))

(defun update-adaptive-context-pruning-max-tokens (conversation history)
  "Updates CONVERSATION with a per-conversation compression ceiling no higher than the configured budget."
  (let ((compressed-total-tokens (estimated-conversation-context-token-count conversation history))
        (model (chatbot-model (conversation-chatbot conversation))))
    (setf (conversation-adaptive-context-pruning-max-tokens conversation)
          (min (configured-context-pruning-max-tokens model)
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
  (let* ((model (and conversation (chatbot-model (conversation-chatbot conversation))))
         (configured-max-tokens (configured-context-pruning-max-tokens model)))
    (if conversation
        (let ((adaptive-max-tokens
                (conversation-adaptive-context-pruning-max-tokens conversation)))
          (if adaptive-max-tokens
              (min configured-max-tokens adaptive-max-tokens)
              configured-max-tokens))
        configured-max-tokens)))

(defun effective-context-pruning-target-tokens (&optional conversation)
  "Returns the effective estimated post-compression target token count.
For Gemini Pro-tier conversations, the target is a small fraction
(*context-pruning-pro-target-ratio*) of the effective max ceiling, kept aggressively low
because Pro's high per-token price makes the average resent-history size across a
session's life the dominant cost driver. Other conversations use the generic configured
target."
  (let* ((max-tokens (effective-context-pruning-max-tokens conversation))
         (model (and conversation (chatbot-model (conversation-chatbot conversation))))
         (configured-target
           (if (and model (gemini-pro-model-p model))
               (max 1 (floor (* max-tokens *context-pruning-pro-target-ratio*)))
               *context-pruning-estimated-target-tokens*)))
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
                  (min (configured-context-pruning-max-tokens (chatbot-model (conversation-chatbot conversation)))
                       (max 1
                            (* 2
                               (estimated-conversation-context-token-count conversation
                                                                         compressed-history)))))
                (old-messages (getf final-pass :old-messages))
                (raw-messages (getf final-pass :raw-messages))
                (digest (getf final-pass :digest))
                (breakdown (conversation-context-token-breakdown conversation compressed-history))
                (main-model (chatbot-model (conversation-chatbot conversation)))
                (digest-model (cheap-summarization-model (chatbot-backend (conversation-chatbot conversation))
                                                          main-model))
                (main-model-price (estimated-gemini-model-input-price-per-token main-model))
                (digest-model-price (estimated-gemini-model-input-price-per-token digest-model))
                (old-messages-tokens (estimated-history-token-count old-messages))
                ;; Estimated per-turn savings from a smaller resent history going forward.
                (estimated-per-turn-dollars-saved
                  (and digest main-model-price
                       (* old-messages-tokens main-model-price)))
                ;; Estimated $ avoided by summarizing on a cheap model instead of the
                ;; parent conversation's (potentially expensive) model.
                (estimated-digest-call-dollars-saved
                  (and digest main-model-price digest-model-price
                       (* old-messages-tokens (- main-model-price digest-model-price)))))
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
                                     ("digest-length" . ,(princ-to-string (length digest)))
                                     ("digest-model" . ,digest-model)
                                     ("estimated-per-turn-dollars-saved"
                                      . ,(if estimated-per-turn-dollars-saved
                                             (format nil "~,4F" estimated-per-turn-dollars-saved)
                                             "unknown"))
                                     ("estimated-digest-call-dollars-saved"
                                      . ,(if estimated-digest-call-dollars-saved
                                             (format nil "~,4F" estimated-digest-call-dollars-saved)
                                             "unknown")))))
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


(defun scrub-conversation-tool-responses (&key (conversation (resolve-chat-conversation nil nil))
                                               (max-characters 1000)
                                               (keep-recent-messages 10))
  "Surgically scrubs large tool execution results from CONVERSATION's history.
Replaces the tool-result text of any tool response older than the most recent
KEEP-RECENT-MESSAGES stored messages whose length exceeds MAX-CHARACTERS with a
concise summary marker. Handles both Gemini/Google-style \"parts\"-based
functionResponse messages (covering both the success \"result\" payload and
the error \"message\" payload) and OpenAI/Grok/LM-Studio-style flat
\"tool\"-role \"content\" messages.
When any response is scrubbed, also clears CONVERSATION's cached interaction
id: Gemini's Interactions API only resends the full message history on the
first turn of an interaction chain and otherwise relies on server-side state
via previous_interaction_id, so without this reset a Gemini-backed
conversation would keep sending the untrimmed original history to the model
regardless of what was scrubbed here (mirroring the same reset performed by
COMPRESS-CONVERSATION-CONTEXT-IF-NEEDED).
Returns the count of pruned responses and the total characters saved."
  (let* ((conv (resolve-chat-conversation conversation nil))
         (msgs (conversation-messages conv))
         (total (length msgs))
         (cutoff (max 0 (- total keep-recent-messages)))
         (scrubbed-count 0)
         (saved-chars 0))
    (labels ((alist-replace (alist key new-value)
               "Returns ALIST with KEY's value replaced by NEW-VALUE, preserving all other entries and their order."
               (mapcar (lambda (cell)
                        (if (string= (car cell) key) (cons key new-value) cell))
                      alist))
             (scrub-text-value (value tool-name)
               "Returns VALUE, or a pruned-marker replacement (tallying SCRUBBED-COUNT/SAVED-CHARS) when
VALUE is a string longer than MAX-CHARACTERS."
               (if (and (stringp value) (> (length value) max-characters))
                   (let* ((orig-len (length value))
                          (new-value (format nil "[Tool response pruned: ~A, originally ~D chars]"
                                             (or tool-name "unknown") orig-len)))
                     (incf scrubbed-count)
                     (incf saved-chars (- orig-len (length new-value)))
                     new-value)
                   value))
             (function-response-value-key (response)
               "Returns whichever of \"result\" (success) or \"message\" (error) is present in RESPONSE."
               (cond
                 ((assoc "result" response :test #'string=) "result")
                 ((assoc "message" response :test #'string=) "message")
                 (t nil)))
             (scrub-function-response-part (part)
               "Scrubs a Gemini/Google-style functionResponse PART's \"result\" or \"message\" payload text."
               (let ((fn-resp (cdr (assoc "functionResponse" part :test #'string=))))
                 (if (null fn-resp)
                     part
                     (let* ((name (cdr (assoc "name" fn-resp :test #'string=)))
                            (response (cdr (assoc "response" fn-resp :test #'string=)))
                            (value-key (function-response-value-key response)))
                       (if (null value-key)
                           part
                           (let* ((value (cdr (assoc value-key response :test #'string=)))
                                  (new-value (scrub-text-value value name)))
                             (if (eq new-value value)
                                 part
                                 (let* ((new-response (alist-replace response value-key new-value))
                                        (new-fn-resp (alist-replace fn-resp "response" new-response)))
                                   (list (cons "functionResponse" new-fn-resp))))))))))
             (scrub-tool-role-message (msg)
               "Scrubs an OpenAI/Grok/LM-Studio-style \"tool\"-role MSG's \"content\" text."
               (let* ((content (cdr (assoc "content" msg :test #'string=)))
                      (name (cdr (assoc "name" msg :test #'string=)))
                      (new-content (scrub-text-value content name)))
                 (if (eq new-content content)
                     msg
                     (alist-replace msg "content" new-content))))
             (scrub-msg (msg idx)
               (if (>= idx cutoff)
                   msg
                   (let ((parts (cdr (assoc "parts" msg :test #'string=)))
                        (role (cdr (assoc "role" msg :test #'string=))))
                     (cond
                       ((vectorp parts)
                        (alist-replace msg "parts" (map 'vector #'scrub-function-response-part parts)))
                       ((and (stringp role) (string= role "tool"))
                        (scrub-tool-role-message msg))
                       (t msg))))))
      (let ((new-msgs (loop for m in msgs for i from 0 collect (scrub-msg m i))))
        (setf (conversation-messages conv) new-msgs)
        (when (plusp scrubbed-count)
          (setf (conversation-interaction-id conv) nil))
        (values scrubbed-count saved-chars)))))

(defun trim-context (&key (conversation (resolve-chat-conversation nil nil))
                          (max-characters 1000)
                          (keep-recent-messages 10))
  "Convenience REPL wrapper for SCRUB-CONVERSATION-TOOL-RESPONSES."
  (multiple-value-bind (scrubbed saved)
      (scrub-conversation-tool-responses :conversation conversation
                                         :max-characters max-characters
                                         :keep-recent-messages keep-recent-messages)
    (format nil "Trimmed context: ~D tool response~:P scrubbed, ~D character~:P (~,1F KB) saved."
            scrubbed saved (/ saved 1024.0))))
