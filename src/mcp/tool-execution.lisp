;;; -*- Lisp -*-
;;; tool-execution.lisp - MCP and built-in chatbot tool execution

(in-package "CHATBOT")

(defun execute-chatbot-tool-by-name (bot tool-name arguments)
  "Finds TOOL-NAME for BOT and executes it with ARGUMENTS."
  (multiple-value-bind (source tool) (find-chatbot-tool bot tool-name)
    (unless source
      (error "Tool not found: ~A" tool-name))
    (execute-chatbot-tool bot
                          source
                          tool-name
                          (normalize-chatbot-tool-arguments source tool arguments))))

(defun execute-chatbot-tool-by-name-json-arguments (bot tool-name arguments-json context)
  "Parses ARGUMENTS-JSON for TOOL-NAME in CONTEXT and executes the tool for BOT."
  (execute-chatbot-tool-by-name
   bot
   tool-name
   (if (or (null arguments-json)
           (string= (string-trim '(#\Space #\Tab #\Return #\Linefeed) arguments-json) ""))
       (empty-json-object)
       (parse-json-or-error arguments-json :context context))))

(defun chatbot-tool-error-message (condition)
  "Returns the most useful human-readable message for CONDITION."
  (if (typep condition 'mcp-tool-execution-error)
      (mcp-tool-execution-error-reason condition)
      (princ-to-string condition)))

(defun chatbot-tool-error-payload (tool-name condition)
  "Returns a JSON-serializable payload describing a tool execution failure."
  `(("type" . "tool_error")
    ("toolName" . ,tool-name)
    ("message" . ,(chatbot-tool-error-message condition))))

(defun chatbot-tool-error-text (tool-name condition)
  "Returns a JSON string describing a tool execution failure for LLM-visible text fields."
  (cl-json:encode-json-to-string (chatbot-tool-error-payload tool-name condition)))

(defun map-chatbot-json-tool-call-results (bot tool-calls context-builder result-builder
                                               &key error-builder)
  "Executes JSON-argument TOOL-CALLS for BOT and returns builder outputs in order.

When ERROR-BUILDER is provided, tool execution errors are converted into result
entries instead of aborting the full turn. If ERROR-BUILDER is NIL, errors are sandboxed."
  (mapcar (lambda (tool-call)
            (let* ((id (cdr (assoc :id tool-call)))
                   (name (cdr (assoc :name tool-call)))
                   (arguments-json (coerce (cdr (assoc :arguments tool-call)) 'string)))
              (handler-case
                  (let ((res-text (execute-chatbot-tool-by-name-json-arguments
                                   bot
                                   name
                                   arguments-json
                                   (funcall context-builder name tool-call))))
                    (funcall result-builder id name arguments-json res-text tool-call))
                (error (condition)
                  (if (or (typep condition 'agentic-loop-approval-required)
                          (typep condition 'agentic-loop-interrupted))
                      (error condition)
                      (if error-builder
                          (funcall error-builder id name arguments-json condition tool-call)
                          (funcall result-builder id name arguments-json
                                   (chatbot-tool-error-text name condition)
                                   tool-call)))))))
          tool-calls))

(defun extract-observations-from-tool (tool-name arguments)
  "Extracts a list of plist records containing :entity-name, :entity-type, and :text (observation string)
from the MCP arguments for create_entities and add_observations."
  (let ((results nil))
    (cond
      ((string-equal tool-name "create_entities")
       (let ((entities (cdr (assoc :entities arguments))))
         (loop for entity across (or (and (vectorp entities) entities) (list entities))
               do (let* ((name (cdr (assoc :name entity)))
                         (type (or (cdr (assoc :entity--type entity))
                                   (cdr (assoc :entity-type entity))))
                         (obs (cdr (assoc :observations entity))))
                    (loop for ob across (or (and (vectorp obs) obs) (list obs))
                          when (and (stringp ob) (string/= ob ""))
                          do (push (list :entity-name name :entity-type type :text ob) results))))))
      ((string-equal tool-name "add_observations")
       (let ((observations (cdr (assoc :observations arguments))))
         (loop for obs-entry across (or (and (vectorp observations) observations) (list observations))
               do (let* ((name (or (cdr (assoc :entity--name obs-entry))
                                   (cdr (assoc :entity-name obs-entry))))
                         (contents (cdr (assoc :contents obs-entry))))
                    (loop for content across (or (and (vectorp contents) contents) (list contents))
                          when (and (stringp content) (string/= content ""))
                          do (push (list :entity-name name :text content) results)))))))
    (nreverse results)))

(defun construct-complete-sentence (entity-name entity-type observation)
  "Uses Gemini to construct a grammatically correct, natural-sounding complete sentence
from an entity name, its optional type, and an observation fact."
  (handler-case
      (let* ((api-key (gemini-api-key))
             (headers (list (cons "x-goog-api-key" api-key)
                            (cons "Content-Type" "application/json")))
             (url (format nil "~A/models/gemini-2.5-flash:generateContent" *gemini-base-url*))
             (prompt (format nil "Construct a single, grammatically correct, natural-sounding complete sentence about the entity \"~A\"~@[ of type \"~A\"~] based on this observation fact: \"~A\".
Respond with ONLY the sentence. Do not include any explanations, markdown, or extra text.
Example:
Entity: user-authentication
Fact: Uses JWT for authentication
Sentence: User-authentication uses JWT for authentication.

Sentence:" entity-name entity-type observation))
             (payload (cl-json:encode-json-to-string
                       `((:contents . ,(vector `((:parts . ,(vector `((:text . ,prompt))))))))))
             (response-json (post-web-request url headers payload))
             (response (cl-json:decode-json-from-string response-json))
             (candidates (cdr (assoc :candidates response)))
             (first-candidate (first candidates))
             (content-obj (cdr (assoc :content first-candidate)))
             (parts (cdr (assoc :parts content-obj)))
             (first-part (first parts))
             (raw-text (cdr (assoc :text first-part)))
             (sentence (string-trim '(#\Space #\Tab #\Return #\Linefeed #\") raw-text)))
        (if (and (stringp sentence) (string/= sentence ""))
            sentence
            (format nil "~A (~A): ~A" entity-name (or entity-type "Entity") observation)))
    (error ()
      ;; Fallback if API fails
      (format nil "~A (~A): ~A" entity-name (or entity-type "Entity") observation))))

(defun sync-knowledge-graph-observations (bot tool-name arguments)
  "Intercepts knowledge graph mutations and syncs new observations to the ChromaDB <persona>_Memory collection as complete sentences."
  (handler-case
      (let ((persona (chatbot-persona-name bot)))
        (when (and persona (member tool-name '("add_observations" "create_entities") :test #'string-equal))
          (let ((observations (extract-observations-from-tool tool-name arguments)))
            (when (and observations (chroma-alive-p))
              (let* ((collection-name (format nil "~A_Memory" (string persona)))
                     (collection (or (chroma-get-collection collection-name)
                                     (chroma-create-collection collection-name :get-or-create t))))
                (when collection
                  (let ((collection-id (cdr (assoc :id collection))))
                    (dolist (obs observations)
                      (let* ((entity-name (getf obs :entity-name))
                             (entity-type (getf obs :entity-type))
                             (fact (getf obs :text))
                             ;; 1. Construct complete sentence
                             (sentence (construct-complete-sentence entity-name entity-type fact))
                             ;; 2. Generate embedding
                             (vector (string->embedding-vector sentence :model "gemini-embedding-2"))
                             ;; 3. Generate unique ID
                             (id (format nil "mem-~A-~X" (get-universal-time) (random #x1000000)))
                             ;; 4. Formulate metadata
                             (metadata `((:entity . ,entity-name)
                                         (:entity--type . ,(or entity-type ""))
                                         (:raw--observation . ,fact))))
                        (log-message :info "Syncing KG observation to memory collection"
                                     :context `(("persona" . ,persona)
                                                ("id" . ,id)
                                                ("sentence" . ,sentence)))
                        (chroma-add collection-id (list id)
                                    :embeddings (list vector)
                                    :documents (list sentence)
                                    :metadatas (list metadata)))))))))))
    (error (e)
      (log-message :warn "Failed to sync knowledge graph observations to ChromaDB"
                   :context `(("error" . ,(princ-to-string e)))))))

(defun execute-chatbot-tool (bot source tool-name arguments)
  "Executes SOURCE as either a built-in or MCP tool for BOT."
  (call-with-runtime-context
   (chatbot-runtime-context bot)
   (lambda ()
     (if (eq source :built-in)
         (default-execute-builtin-chatbot-tool bot tool-name arguments)
         (let ((result (execute-mcp-tool source tool-name arguments)))
           ;; Sync observations to ChromaDB after successful execution
           (sync-knowledge-graph-observations bot tool-name arguments)
           result)))
   :default-conversation-compatibility-p nil
   :legacy-function-seam-compatibility-p nil))
