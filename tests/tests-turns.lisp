;;; tests-turns.lisp - tests for the formal conversation "turn" concept

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(defun test-message (role content &optional turn-number)
  "Returns a minimal role/content message alist, tagged with TURN-NUMBER when supplied."
  (let ((message (list (cons "role" role) (cons "content" content))))
    (if turn-number
        (message-with-turn-number message turn-number)
        message)))

(fiveam:test test-tag-message-starting-new-turn-begins-at-zero
  "A turn-starting tag on an empty history begins numbering at 0."
  (let ((tagged (tag-message-starting-new-turn (test-message "user" "hi") nil)))
    (fiveam:is (= 0 (message-turn-number tagged)))))

(fiveam:test test-tag-message-starting-new-turn-increments
  "A turn-starting tag increments relative to the prior last message's turn number."
  (let* ((history (list (test-message "user" "hi" 0) (test-message "model" "hello" 0)))
         (tagged (tag-message-starting-new-turn (test-message "user" "again") history)))
    (fiveam:is (= 1 (message-turn-number tagged)))))

(fiveam:test test-tag-message-continuing-turn-matches-current
  "A turn-continuing tag matches the current last message's turn number."
  (let* ((history (list (test-message "user" "hi" 3)))
         (tagged (tag-message-continuing-turn (test-message "model" "hello") history)))
    (fiveam:is (= 3 (message-turn-number tagged)))))

(fiveam:test test-tag-messages-continuing-turn-tags-batch
  "A batch of continuation messages are all tagged with the same current turn number."
  (let* ((history (list (test-message "user" "hi" 2)))
         (tagged (tag-messages-continuing-turn
                  (list (test-message "assistant" "call") (test-message "tool" "result"))
                  history)))
    (fiveam:is (every (lambda (message) (= 2 (message-turn-number message))) tagged))))

(fiveam:test test-partition-messages-into-turns-groups-contiguous-runs
  "Messages sharing a turn tag are grouped into one turn structure, in order."
  (let* ((messages (list (test-message "user" "hi" 0)
                         (test-message "model" "hello" 0)
                         (test-message "user" "bye" 1)
                         (test-message "model" "goodbye" 1)))
         (turns (partition-messages-into-turns messages)))
    (fiveam:is (= 2 (length turns)))
    (fiveam:is (= 0 (turn-number (first turns))))
    (fiveam:is (= 2 (length (turn-messages (first turns)))))
    (fiveam:is (= 1 (turn-number (second turns))))
    (fiveam:is (= 2 (length (turn-messages (second turns)))))))

(fiveam:test test-turn-contains-function-call-and-response
  "A single turn legitimately contains its function call/response exchange plus final text."
  (let* ((messages (list (test-message "user" "do the thing" 0)
                         (test-message "assistant" "calling tool" 0)
                         (test-message "tool" "tool result" 0)
                         (test-message "model" "done" 0)))
         (turns (partition-messages-into-turns messages)))
    (fiveam:is (= 1 (length turns)))
    (fiveam:is (= 4 (length (turn-messages (first turns)))))))

(fiveam:test test-conversation-turn-count-excludes-untagged-digest-messages
  "CONVERSATION-TURN-COUNT excludes untagged pseudo-turns such as compression digests."
  (let* ((bot (make-instance 'chatbot :model "gemini-2.5-flash" :backend :google))
         (messages (list (test-message "system" "[State Digest: ...]")
                         (test-message "user" "hi" 0)
                         (test-message "model" "hello" 0)))
         (conv (make-instance 'conversation :chatbot bot :messages messages)))
    (fiveam:is (= 1 (conversation-turn-count conv)))
    (fiveam:is (= 2 (length (conversation-turns conv))))))

(fiveam:test test-get-conversation-turn-finds-numbered-turn
  "GET-CONVERSATION-TURN retrieves the turn with the requested number, or NIL."
  (let* ((bot (make-instance 'chatbot :model "gemini-2.5-flash" :backend :google))
         (messages (list (test-message "user" "hi" 0)
                         (test-message "model" "hello" 0)
                         (test-message "user" "bye" 1)
                         (test-message "model" "goodbye" 1)))
         (conv (make-instance 'conversation :chatbot bot :messages messages)))
    (fiveam:is (= 2 (length (turn-messages (get-conversation-turn conv 1)))))
    (fiveam:is-false (get-conversation-turn conv 99))))

(fiveam:test test-backfill-message-turn-numbers-is-noop-when-fully-tagged
  "BACKFILL-MESSAGE-TURN-NUMBERS returns an already-tagged history unchanged."
  (let ((messages (list (test-message "user" "hi" 0) (test-message "model" "hello" 0))))
    (fiveam:is (equal messages (backfill-message-turn-numbers messages)))))

(fiveam:test test-backfill-message-turn-numbers-assigns-legacy-gemini-history
  "Legacy untagged history is backfilled using genuine-user-turn detection, keeping a
Gemini functionResponse message (role user) attached to its preceding turn rather than
starting a new one."
  (let* ((messages (list (test-message "user" "do the thing")
                         (list (cons "role" "model")
                               (cons "parts" (list (list (cons "functionCall" "tool")))))
                         (list (cons "role" "user")
                               (cons "parts" (list (list (cons "functionResponse" "result")))))
                         (test-message "model" "done")
                         (test-message "user" "next request")
                         (test-message "model" "next response")))
         (backfilled (backfill-message-turn-numbers messages))
         (turns (partition-messages-into-turns backfilled)))
    (fiveam:is (= 2 (length turns)))
    (fiveam:is (= 4 (length (turn-messages (first turns)))))
    (fiveam:is (= 2 (length (turn-messages (second turns)))))))

(fiveam:test test-select-recent-messages-for-pruning-keeps-whole-turns
  "Pruning never splits a turn's internal messages between kept and dropped halves."
  (let* ((big-text (make-string 5000 :initial-element #\a))
         (turn-0-messages (list (test-message "user" big-text 0)
                                (test-message "assistant" "calling tool" 0)
                                (test-message "tool" "tool result" 0)
                                (test-message "model" big-text 0)))
         (messages (append turn-0-messages
                           (list (test-message "user" big-text 1)
                                 (test-message "model" big-text 1)
                                 (test-message "user" big-text 2)
                                 (test-message "model" big-text 2))))
         (kept (select-recent-messages-for-pruning messages :target-tokens 100))
         (kept-turns (partition-messages-into-turns kept)))
    ;; Turn 0 either survives whole (all 4 of its messages) or is entirely dropped;
    ;; it must never appear as a partial (split) run of messages.
    (let ((kept-turn-0 (find 0 kept-turns :key #'turn-number)))
      (when kept-turn-0
        (fiveam:is (= (length turn-0-messages) (length (turn-messages kept-turn-0))))))
    ;; At least the configured minimum of 2 most recent turns must survive.
    (fiveam:is (>= (count-if #'turn-number kept-turns) 2))))

(fiveam:test test-checkpoint-normalize-preserves-turn-tag
  "Round-tripping a message through checkpoint normalization preserves its turn tag."
  (let* ((persisted (list (cons :role "user") (cons :content "hi") (cons :turn 2)))
         (normalized (normalize-persisted-message persisted)))
    (fiveam:is (= 2 (message-turn-number normalized)))))

(fiveam:test test-checkpoint-normalize-history-backfills-legacy-messages
  "NORMALIZE-PERSISTED-HISTORY backfills turn numbers for legacy checkpoints lacking them."
  (let* ((persisted (list (list (cons :role "user") (cons :content "hi"))
                          (list (cons :role "model") (cons :content "hello"))))
         (normalized (normalize-persisted-history persisted)))
    (fiveam:is (every #'message-turn-number normalized))))

(fiveam:test test-conversation-chat-transcript-wraps-conversation-turns
  "CONVERSATION-CHAT-TRANSCRIPT is a CHAT-TRANSCRIPT wrapping CONVERSATION-TURNS."
  (let* ((bot (make-instance 'chatbot :model "gemini-2.5-flash" :backend :google))
         (messages (list (test-message "user" "hi" 0)
                         (test-message "model" "hello" 0)
                         (test-message "user" "bye" 1)
                         (test-message "model" "goodbye" 1)))
         (conv (make-instance 'conversation :chatbot bot :messages messages))
         (a-chat-transcript (conversation-chat-transcript conv)))
    (fiveam:is (chat-transcript-p a-chat-transcript))
    (fiveam:is (equal (mapcar #'turn-number (conversation-turns conv))
                     (mapcar #'turn-number (chat-transcript-turns a-chat-transcript))))
    (fiveam:is (equal (mapcar #'turn-messages (conversation-turns conv))
                     (mapcar #'turn-messages (chat-transcript-turns a-chat-transcript))))
    (fiveam:is (= 2 (chat-transcript-turn-count a-chat-transcript)))
    (fiveam:is (= 1 (turn-number (get-chat-transcript-turn a-chat-transcript 1))))
    (fiveam:is-false (get-chat-transcript-turn a-chat-transcript 99))))

(fiveam:test test-chat-transcript-excludes-untagged-digest-messages
  "CHAT-TRANSCRIPT-TURN-COUNT excludes untagged pseudo-turns such as compression digests."
  (let* ((bot (make-instance 'chatbot :model "gemini-2.5-flash" :backend :google))
         (messages (list (test-message "system" "[State Digest: ...]")
                         (test-message "user" "hi" 0)
                         (test-message "model" "hello" 0)))
         (conv (make-instance 'conversation :chatbot bot :messages messages))
         (a-chat-transcript (conversation-chat-transcript conv)))
    (fiveam:is (= 1 (chat-transcript-turn-count a-chat-transcript)))
    (fiveam:is (= 2 (length (chat-transcript-turns a-chat-transcript))))))

(fiveam:test test-chat-transcript-gains-one-turn-per-live-model-interaction
  "Each live interaction with the model appends exactly one new turn to the transcript."
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-3.5-flash"))
         (conv (make-instance 'conversation :chatbot bot))
         (reply-text "placeholder"))
    (flet ((mocked-google-response (url &rest args)
             (declare (ignore url args))
             (values (format nil "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"~A\"}], \"role\": \"model\"}}]}"
                            reply-text)
                     200)))
      (let ((*gemini-api-key-function* (lambda () "mocked-google-api-key"))
            (*http-post-function* #'mocked-google-response))
        (fiveam:is (= 0 (chat-transcript-turn-count (conversation-chat-transcript conv))))
        (setf reply-text "First reply")
        (test-chat-google bot "First prompt" conv nil)
        (fiveam:is (= 1 (chat-transcript-turn-count (conversation-chat-transcript conv))))
        (setf reply-text "Second reply")
        (test-chat-google bot "Second prompt" conv nil)
        (fiveam:is (= 2 (chat-transcript-turn-count (conversation-chat-transcript conv))))
        (let ((turns (chat-transcript-turns (conversation-chat-transcript conv))))
          (fiveam:is (equal '(0 1) (mapcar #'turn-number turns))))))))

(fiveam:test test-recent-turns-messages-is-noop-within-limit
  "RECENT-TURNS-MESSAGES returns MESSAGES unchanged when at most MAX-TURNS turns are present."
  (let ((messages (list (test-message "user" "hi" 0) (test-message "model" "hello" 0))))
    (fiveam:is (equal messages (recent-turns-messages messages 16)))))

(fiveam:test test-recent-turns-messages-drops-oldest-turns-beyond-the-limit
  "RECENT-TURNS-MESSAGES keeps only the MAX-TURNS most recent numbered turns."
  (let* ((messages (loop for n from 0 below 20
                        append (list (test-message "user" (format nil "prompt ~D" n) n)
                                   (test-message "model" (format nil "reply ~D" n) n))))
         (kept (recent-turns-messages messages 16))
         (kept-turns (partition-messages-into-turns kept)))
    (fiveam:is (= 16 (length kept-turns)))
    (fiveam:is (equal (loop for n from 4 below 20 collect n)
                     (mapcar #'turn-number kept-turns)))))

(fiveam:test test-recent-turns-messages-always-keeps-untagged-digest-messages
  "RECENT-TURNS-MESSAGES preserves untagged compression-digest messages regardless of MAX-TURNS."
  (let* ((messages (append (list (test-message "system" "[State Digest: ...]"))
                          (loop for n from 0 below 20
                                append (list (test-message "user" (format nil "prompt ~D" n) n)
                                           (test-message "model" (format nil "reply ~D" n) n)))))
         (kept (recent-turns-messages messages 16))
         (kept-turns (partition-messages-into-turns kept)))
    (fiveam:is (null (turn-number (first kept-turns))))
    (fiveam:is (= 16 (count-if #'turn-number kept-turns)))))

(fiveam:test test-recent-turns-messages-defaults-to-max-context-turns
  "RECENT-TURNS-MESSAGES defaults its limit to *MAX-CONTEXT-TURNS*."
  (let* ((messages (loop for n from 0 below 20
                        append (list (test-message "user" (format nil "prompt ~D" n) n)
                                   (test-message "model" (format nil "reply ~D" n) n))))
         (*max-context-turns* 3)
         (kept-turns (partition-messages-into-turns (recent-turns-messages messages))))
    (fiveam:is (= 3 (length kept-turns)))
    (fiveam:is (equal '(17 18 19) (mapcar #'turn-number kept-turns)))))

(fiveam:test test-messages-without-turn-tags-strips-the-turn-key
  "MESSAGES-WITHOUT-TURN-TAGS removes the internal \"turn\" bookkeeping field
from every message, leaving all other fields untouched."
  (let* ((tagged (list (test-message "user" "hi" 3) (test-message "model" "hello" 3)))
         (stripped (messages-without-turn-tags tagged)))
    (dolist (message stripped)
      (fiveam:is-false (assoc "turn" message :test #'string=)))
    (fiveam:is (string= "hi" (cdr (assoc "content" (first stripped) :test #'string=))))))

(fiveam:test test-google-tool-recursion-state-never-leaks-turn-tag-into-request-contents
  "GOOGLE-TOOL-RECURSION-STATE's :REQUEST-CONTENTS never carries the internal
\"turn\" bookkeeping field, since providers reject unrecognized fields."
  (let* ((bot (make-instance 'chatbot :backend :google :model "gemini-flash-latest" :persona-name "V"))
         (history (list (test-message "user" "hi" 0)))
         (conv (make-instance 'conversation :chatbot bot :messages history))
         (outcome (list :request-contents (list (list (cons "role" "user") (cons "content" "hi")))
                       :file-attachments nil
                       :effective-model "gemini-flash-latest"
                       :effective-generation-config nil
                       :cached-content-name nil
                       :bypass-cache-p t
                       :malformed-response-fallback-attempted-p nil))
         (tagged-recursion-messages
           (tag-messages-continuing-turn
            (list (test-message "model" "functionCall-ish") (test-message "user" "functionResponse-ish"))
            history))
         (state (google-tool-recursion-state bot conv outcome history tagged-recursion-messages)))
    (dolist (message (getf state :request-contents))
      (fiveam:is-false (assoc "turn" message :test #'string=)))))

(fiveam:test test-openai-tool-recursion-state-never-leaks-turn-tag-into-request-messages
  "OPENAI-TOOL-RECURSION-STATE's :REQUEST-MESSAGES never carries the internal
\"turn\" bookkeeping field, since providers reject unrecognized fields."
  (let* ((bot (make-instance 'chatbot :backend :openai :model "gpt-5" :persona-name "V"))
         (history (list (test-message "user" "hi" 0)))
         (conv (make-instance 'conversation :chatbot bot :messages history))
         (outcome (list :request-messages (list (list (cons "role" "user") (cons "content" "hi")))
                       :file-attachments nil
                       :effective-generation-config nil
                       :malformed-response-retry-attempted-p nil))
         (tagged-recursion-messages
           (tag-messages-continuing-turn
            (list (test-message "assistant" "tool-call-ish") (test-message "tool" "tool-result-ish"))
            history))
         (state (openai-tool-recursion-state bot conv outcome history tagged-recursion-messages)))
    (dolist (message (getf state :request-messages))
      (fiveam:is-false (assoc "turn" message :test #'string=)))))
