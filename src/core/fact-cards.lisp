;;; -*- Lisp -*-
;;; fact-cards.lisp - per-turn Dense Fact Card distillation and ChromaDB embedding
;;;
;;; After a turn is finalized (its live user prompt and the model's full
;;; response, including any function calls, are both present in
;;; CONVERSATION-MESSAGES), a cheap, fast model distills it into a short,
;;; self-contained "Dense Fact Card" -- a few dense sentences capturing the
;;; durable facts worth remembering -- which is then embedded into the
;;; persona's "<persona>_Facts" ChromaDB collection. Generation and embedding
;;; run in a supervised background thread so they never delay the response
;;; the user is waiting for.

(in-package "CHATBOT")

(defparameter *fact-cards-enabled-p* t
  "Master kill-switch for post-turn Dense Fact Card generation and embedding.
When NIL, MAYBE-GENERATE-TURN-FACT-CARD-IN-BACKGROUND returns immediately without checking
ChromaDB reachability or spawning a background thread. The test suite binds this to NIL so
ordinary chat-turn tests never race a real background LLM/ChromaDB call against their own
mocked HTTP response queues.")

(defun fact-card-message-content-string (message)
  "Returns MESSAGE content as a printable string for fact-card distillation."
  (let ((content (cdr (assoc "content" message :test #'string=))))
    (typecase content
      (null "")
      (string content)
      (t (princ-to-string content)))))

(defun turn-user-prompt-text (a-turn)
  "Returns the live user prompt text opening A-TURN, or \"\" when absent."
  (let ((message (find "user" (turn-messages a-turn)
                       :key #'message-role-string :test #'string=)))
    (if message (fact-card-message-content-string message) "")))

(defun turn-model-response-text (a-turn)
  "Returns A-TURN's model-authored text, concatenated in order, or \"\" when absent."
  (let ((model-messages (remove-if-not
                         (lambda (message)
                           (member (message-role-string message) '("model" "assistant") :test #'string=))
                         (turn-messages a-turn))))
    (format nil "~{~A~^~%~}" (mapcar #'fact-card-message-content-string model-messages))))

(defparameter +fact-card-instruction+
  (concatenate 'string
               "Read the following single conversation turn and write one dense, "
               "self-contained sentence (or two, only if necessary) capturing every "
               "durable fact, event, preference, or detail worth remembering "
               "long-term. Write only the fact card text, with no headers, labels, "
               "or commentary.")
  "The instruction prefix used when prompting the cheap model to distill a turn into a Dense Fact Card.")

(defun turn-fact-card-prompt (a-turn)
  "Returns the micro-model prompt used to distill A-TURN into a Dense Fact Card."
  (format nil "~A~%~%User: ~A~%~%Model: ~A"
          +fact-card-instruction+
          (turn-user-prompt-text a-turn)
          (turn-model-response-text a-turn)))

(defun generate-turn-fact-card (bot a-turn)
  "Returns a Dense Fact Card for A-TURN, generated using BOT's cheap summarization model."
  (let* ((prompt (turn-fact-card-prompt a-turn))
         (conv (new-chat :backend (cheap-summarization-backend (chatbot-backend bot))
                         :model (cheap-summarization-model (cheap-summarization-backend (chatbot-backend bot))
                                                           (chatbot-model bot))
                         :runtime-context (chatbot-runtime-context bot))))
    (multiple-value-bind (effective-input effective-model)
        (resolve-prompt-model-override (conversation-chatbot conv) prompt)
      (let ((result (dispatch-chat-turn conv
                                        effective-input
                                        nil
                                        :effective-model effective-model
                                        :effective-generation-config
                                        (resolve-effective-generation-config (conversation-chatbot conv)))))
        (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                     (apply-chat-turn-result result conv))))))

(defun persist-turn-fact-card (persona-name a-turn fact-card-text)
  "Embeds FACT-CARD-TEXT for A-TURN into the PERSONA-NAME \"_Facts\" ChromaDB collection."
  (when (and persona-name (stringp fact-card-text) (string/= "" fact-card-text) (chroma-alive-p))
    (let* ((collection-name (format nil "~A_Facts" (string persona-name)))
           (collection (or (chroma-get-collection collection-name)
                           (chroma-create-collection collection-name :get-or-create t))))
      (if (null collection)
          (log-message :warn "Could not create or get ChromaDB collection"
                       :context `(("collection" . ,collection-name)))
          (let* ((collection-id (cdr (assoc :id collection)))
                 (id (format nil "fact-turn-~A" (turn-timestamp a-turn)))
                 (vector (string->embedding-vector fact-card-text :model "gemini-embedding-2"))
                 (metadata `((:turn--timestamp . ,(turn-timestamp a-turn)))))
            (log-message :info "Persisting turn fact card to ChromaDB"
                         :context `(("persona" . ,persona-name)
                                    ("turn" . ,(turn-timestamp a-turn))))
            (chroma-add collection-id (list id)
                        :embeddings (list vector)
                        :documents (list fact-card-text)
                        :metadatas (list metadata)))))))

(defun fact-card-query-result->entry (result)
  "Returns a (:turn-timestamp :document :distance) plist for one fact-card query RESULT,
or NIL when RESULT carries no turn-timestamp metadata."
  (let* ((meta (getf result :metadata))
         (turn-timestamp (cdr (assoc :turn--timestamp meta))))
    (when turn-timestamp
      (list :turn-timestamp turn-timestamp
            :document (getf result :document)
            :distance (getf result :distance)))))

(defun retrieve-relevant-turn-fact-cards (persona-name query-text &optional (limit 8))
  "Returns up to LIMIT fact-card entries (:turn-timestamp :document :distance)
from PERSONA-NAME's ChromaDB Facts collection most relevant to QUERY-TEXT by
cosine similarity, deduplicated by turn timestamp and sorted in ascending
(chronological) timestamp order, so callers can prepend them chronologically.
Returns NIL when PERSONA-NAME or QUERY-TEXT is absent, ChromaDB is
unreachable, or no Facts collection yet exists for this persona."
  (handler-case
      (when (and persona-name (stringp query-text) (string/= "" query-text) (chroma-alive-p))
        (let* ((collection-name (format nil "~A_Facts" (string persona-name)))
               (collection (chroma-get-collection collection-name)))
          (when collection
            (let* ((collection-id (cdr (assoc :id collection)))
                   (query-vector (string->embedding-vector query-text :model "gemini-embedding-2"))
                   (query-resp (chroma-query collection-id (list query-vector) :n-results limit))
                   (entries (remove nil (mapcar #'fact-card-query-result->entry
                                               (extract-chroma-query-results query-resp)))))
              (sort (delete-duplicates entries :key (lambda (entry) (getf entry :turn-timestamp)) :test #'= :from-end t)
                    #'< :key (lambda (entry) (getf entry :turn-timestamp)))))))
    (error (e)
      (log-message :warn "Failed to retrieve relevant turn fact cards"
                   :context `(("persona" . ,persona-name) ("error" . ,(princ-to-string e))))
      nil)))

(defun fact-card-preload-message (entry)
  "Returns a synthetic preload message presenting one retrieved fact-card ENTRY."
  (list (cons "role" "model")
        (cons "content" (format nil "[Relevant Memory] ~A~%~A"
                               (format-prompt-timestamp (floor (getf entry :turn-timestamp)))
                               (getf entry :document)))))

(defun relevant-turn-fact-card-messages (persona-name query-text recent-messages &optional (limit 8))
  "Returns up to LIMIT synthetic preload messages for the fact cards most
relevant to QUERY-TEXT, in ascending (chronological) turn-timestamp order. Any
fact card whose turn timestamp is already present in RECENT-MESSAGES is
omitted, so a retrieved memory is never sent twice alongside its own literal
turn."
  (let ((excluded-turn-timestamps (remove-duplicates (remove nil (mapcar #'message-turn-timestamp recent-messages)))))
    (mapcar #'fact-card-preload-message
            (remove-if (lambda (entry) (member (getf entry :turn-timestamp) excluded-turn-timestamps :test #'=))
                       (retrieve-relevant-turn-fact-cards persona-name query-text limit)))))

(defun generate-and-persist-turn-fact-card (bot persona-name a-turn)
  "Generates and persists one Dense Fact Card for A-TURN, logging and swallowing any error."
  (handler-case
      (persist-turn-fact-card persona-name a-turn (generate-turn-fact-card bot a-turn))
    (error (e)
      (log-message :warn "Failed to generate turn fact card"
                   :context `(("persona" . ,persona-name)
                              ("turn" . ,(and a-turn (turn-timestamp a-turn)))
                              ("error" . ,(princ-to-string e)))))))

(defun maybe-generate-turn-fact-card-in-background (conversation)
  "Spawns a supervised background thread that distills CONVERSATION's most
recently completed turn into a Dense Fact Card and embeds it into the
persona's ChromaDB Facts collection. A no-op when CONVERSATION has no
persona identity, no completed turns, ChromaDB is unreachable, or
*FACT-CARDS-ENABLED-P* is NIL. The ChromaDB reachability check runs
synchronously, under the caller's own active runtime context, so tests
and environments without ChromaDB (or without network mocks wired into a
background thread) never spawn a thread or attempt any I/O."
  (let* ((bot (conversation-chatbot conversation))
         (persona-name (chatbot-persona-name bot))
         (a-turn (car (last (conversation-turns conversation)))))
    (when (and *fact-cards-enabled-p* persona-name a-turn (turn-timestamp a-turn) (chroma-alive-p))
      (let* ((captured-context (resolve-runtime-context nil))
             (thread (sb-thread:make-thread
                     (lambda ()
                       (call-with-runtime-context
                        captured-context
                        (lambda ()
                          (generate-and-persist-turn-fact-card bot persona-name a-turn))))
                     :name "chatbot-fact-card")))
        (register-supervised-thread (current-resource-supervisor) thread)
        thread))))
