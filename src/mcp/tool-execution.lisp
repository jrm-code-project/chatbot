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

(defun ensure-system-instruction-tool-path (bot tool-name)
  "Returns BOT's system-instruction path or signals an execution error."
  (or (chatbot-system-instruction-path bot)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "System-instruction tools require a persona-backed system instruction file.")))

(defun system-instruction-storage-kind-name (storage-kind)
  "Returns a lowercase string name for STORAGE-KIND."
  (string-downcase (string storage-kind)))

(defun system-instruction-tool-result (bot &key saved)
  "Returns the current system-instruction paragraph state as JSON text."
  (let ((payload `(("paragraphs" . ,(current-system-instruction-paragraphs bot))
                   ("count" . ,(system-instruction-paragraph-count bot))
                   ("storageKind" . ,(system-instruction-storage-kind-name
                                      (chatbot-system-instruction-storage-kind bot)))
                   ("path" . ,(if (chatbot-system-instruction-path bot)
                                  (namestring (chatbot-system-instruction-path bot))
                                  :null))
                   ,@(when saved '(("saved" . t))))))
    (cl-json:encode-json-to-string payload)))

(defun sampling-parameters-tool-result (bot &key saved)
  "Returns the current runtime sampling parameters as JSON text."
  (let ((parameters (sampling-parameters bot)))
    (cl-json:encode-json-to-string
     `(("temperature" . ,(or (getf parameters :temperature) :null))
       ("topP" . ,(or (getf parameters :top-p) :null))
       ,@(when saved '(("saved" . t)))))))

(defun save-system-instructions-or-tool-error (bot tool-name)
  "Saves BOT's system instructions, mapping failures to tool errors."
  (handler-case
      (save-system-instructions bot)
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (princ-to-string e)))))

(defun whitespace-only-stream-remaining-p (stream)
  "Returns true when the remainder of STREAM contains only whitespace."
  (let ((eof-marker (gensym "EOF")))
    (loop for char = (read-char stream nil eof-marker)
          until (eq char eof-marker)
          always (find char '(#\Space #\Tab #\Newline #\Return #\Page)))))

(defun read-eval-tool-form (expression tool-name)
  "Reads exactly one Lisp form from EXPRESSION with reader eval disabled."
  (handler-case
      (let ((*read-eval* nil)
            (*package* (find-package "CHATBOT")))
        (with-input-from-string (stream expression)
          (let ((eof-marker (gensym "EOF")))
            (let ((form (read stream nil eof-marker)))
              (when (eq form eof-marker)
                (error 'mcp-tool-execution-error
                       :tool-name tool-name
                       :reason "expression must contain one s-expression."))
              (unless (whitespace-only-stream-remaining-p stream)
                (error 'mcp-tool-execution-error
                       :tool-name tool-name
                       :reason "expression must contain exactly one s-expression."))
              form))))
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Failed to parse expression: ~A" e)))))

(defun approve-chatbot-eval-expression (bot expression tool-name)
  "Requests approval to evaluate EXPRESSION for BOT."
  (let ((approval-function (current-eval-approval-function
                            (chatbot-runtime-context bot))))
    (unless approval-function
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "No eval approval function is configured."))
    (unless (funcall approval-function bot expression tool-name)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "Evaluation denied by user."))
    t))

(defun eval-tool-result-json (values stdout stderr)
  "Returns a stable JSON result string for VALUES, STDOUT, and STDERR."
  (cl-json:encode-json-to-string
   `(("values" . ,(coerce (mapcar #'prin1-to-string values) 'vector))
     ("stdout" . ,stdout)
     ("stderr" . ,stderr))))

(defun hash-table-value-any (table keys)
  "Returns the first value present in TABLE for any of KEYS."
  (when (hash-table-p table)
    (dolist (key keys nil)
      (multiple-value-bind (value foundp) (gethash key table)
        (when foundp
          (return value))))))

(defun normalize-grounding-search-response (response tool-name)
  "Returns RESPONSE as a hash table or signals an execution error."
  (unless (hash-table-p response)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Search returned an unexpected response shape."))
  response)

(defun grounding-search-items (response)
  "Returns the result items vector/list from RESPONSE."
  (let ((items (hash-table-value-any response '(:items "items"))))
    (cond
      ((vectorp items) (coerce items 'list))
      ((listp items) items)
      (t nil))))

(defun format-grounding-search-results (label query response tool-name)
  "Formats grounding RESPONSE into stable text for LABEL and QUERY."
  (let* ((normalized (normalize-grounding-search-response response tool-name))
         (items (grounding-search-items normalized))
         (search-info (hash-table-value-any normalized '(:search-information "searchInformation" :search--information)))
         (total-results (or (hash-table-value-any search-info '(:total-results "totalResults" :total--results))
                            (and items (princ-to-string (length items)))
                            "0")))
    (with-output-to-string (stream)
      (format stream "~A query: ~A~%Total results: ~A" label query total-results)
      (if items
          (loop for item in items
                for index from 1
                for title = (or (hash-table-value-any item '(:title "title"))
                                "(untitled)")
                for link = (or (hash-table-value-any item '(:link "link"))
                               "(no link)")
                for snippet = (or (hash-table-value-any item '(:snippet "snippet"))
                                  "")
                do (format stream "~%~%~D. ~A~%URL: ~A" index title link)
                   (when (string/= snippet "")
                     (format stream "~%Snippet: ~A" snippet)))
          (format stream "~%~%No results found.")))))

(defun run-grounding-search (tool-name label function query)
  "Runs grounding search FUNCTION for QUERY and formats a stable tool result."
  (handler-case
      (format-grounding-search-results label
                                       query
                                       (funcall function query)
                                       tool-name)
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Search failed for query ~S: ~A" query e)))))

(defvar *eval-tool-timeout-seconds* 60
  "Maximum number of seconds an approved eval tool expression may run.")

(defun execute-approved-eval-expression (expression form tool-name)
  "Evaluates FORM and returns a structured JSON string with values and captured output."
  (let ((stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream)))
    (let ((*standard-output* stdout-stream)
          (*error-output* stderr-stream)
          (*package* (find-package "CHATBOT")))
      (handler-case
          (let ((values (trivial-timeout:with-timeout (*eval-tool-timeout-seconds*)
                          (multiple-value-list (eval form)))))
            (eval-tool-result-json values
                                   (get-output-stream-string stdout-stream)
                                   (get-output-stream-string stderr-stream)))
        (trivial-timeout:timeout-error ()
          (let ((stdout (get-output-stream-string stdout-stream))
                (stderr (get-output-stream-string stderr-stream)))
            (error 'mcp-tool-execution-error
                   :tool-name tool-name
                   :reason (format nil "Evaluation timed out after ~D seconds.~@[~%stdout:~%~A~]~@[~%stderr:~%~A~]"
                                   *eval-tool-timeout-seconds*
                                   (and (string/= stdout "") stdout)
                                   (and (string/= stderr "") stderr)))))
        (error (e)
          (let ((stdout (get-output-stream-string stdout-stream))
                (stderr (get-output-stream-string stderr-stream)))
            (error 'mcp-tool-execution-error
                   :tool-name tool-name
                   :reason (format nil "Evaluation failed for expression ~S: ~A~@[~%stdout:~%~A~]~@[~%stderr:~%~A~]"
                                   expression
                                   e
                                   (and (string/= stdout "") stdout)
                                   (and (string/= stderr "") stderr)))))))))

(defun format-delegation-instruction (name depth remaining-budget)
  (format nil "~&[DELEGATION CAPABILITIES]
You are a minion named '~A' at hierarchical depth ~D.
You have been allocated a token budget of ~A.
You are capable of delegating tasks to your own subordinate minions if needed.
To delegate a task, output a command in this exact format in your response:
[SPAWN-SUB: name=\"child_name\", budget=number]
Where child_name is the unique name of the child minion to spawn, and budget is the number of tokens allocated to it.
Ensure the child_name is unique and budget is within your remaining budget of ~A.
Once spawned, you can talk to the child using the 'promptSubordinate' tool.
Do not output anything else in the spawn command line itself, but continue your response normally."
          name
          depth
          (or remaining-budget "unbounded")
          (or remaining-budget "unbounded")))

(defun append-delegation-instructions (bot name depth remaining-budget)
  (let* ((model (and (slot-boundp bot 'model) (chatbot-model bot)))
         (is-qwen (and (stringp model) (search "qwen" model :test #'char-equal)))
         (inst (if is-qwen
                   (format nil (concatenate 'string
                                            "~&[CRITICAL OPERATION DIRECTIVE]~%"
                                            "You are a worker minion named '~A'.~%"
                                            "You must DIRECTLY write Lisp code and execute tasks yourself.~%"
                                            "Do NOT output [SPAWN-SUB] commands. Do NOT try to delegate tasks.~%"
                                            "Solve all requests entirely within your own response.")
                               name)
                   (format-delegation-instruction name depth remaining-budget)))
         (curr (chatbot-system-instruction bot)))
    (setf (chatbot-system-instruction bot)
          (cond
            ((null curr) inst)
            ((stringp curr) (format nil "~A~%~A" curr inst))
            ((vectorp curr)
             (coerce (append (coerce curr 'list) (list inst)) 'vector))
            (t inst)))))

(defun subordinate-conversation-name (conversation)
  "Returns CONVERSATION's subordinate name."
  (chatbot-persona-name (conversation-chatbot conversation)))

(defun find-subordinate-conversation (bot name)
  "Returns BOT's subordinate conversation named NAME, or NIL."
  (find name
        (chatbot-subordinates bot)
        :key #'subordinate-conversation-name
        :test #'string-equal))

(defun append-conversation-user-message (conversation text)
  "Appends a transient user TEXT message to CONVERSATION and returns CONVERSATION."
  (setf (conversation-messages conversation)
        (append (conversation-messages conversation)
               (list (list (cons "role" "user")
                           (cons "content" text)))))
  conversation)

(defun attach-subordinate-conversation (bot sub-conv)
  "Attaches SUB-CONV beneath BOT and returns SUB-CONV."
  (setf (chatbot-subordinates bot)
        (append (chatbot-subordinates bot) (list sub-conv)))
  sub-conv)

(defun spawn-subordinate-conversation (bot name child-depth child-dir
                                      &key persona-name backend-kw model
                                        system-instruction requested-budget web-tools-p)
  "Creates and bootstraps one subordinate conversation for BOT."
  (let* ((sub-conv
           (if persona-name
              (new-chat-persona persona-name
                                :runtime-context (chatbot-runtime-context bot)
                                :parent-name (chatbot-persona-name bot)
                                :depth child-depth
                                :token-budget requested-budget
                                :scoped-directory child-dir
                                :web-tools-p web-tools-p
                                :filesystem-tools-p t
                                :filesystem-read-only-p t)
              (new-chat :backend backend-kw
                        :model model
                        :system-instruction system-instruction
                        :runtime-context (chatbot-runtime-context bot)
                        :parent-name (chatbot-persona-name bot)
                        :depth child-depth
                        :token-budget requested-budget
                        :scoped-directory child-dir
                        :web-tools-p web-tools-p
                        :filesystem-tools-p t
                        :filesystem-read-only-p t)))
         (child-bot (conversation-chatbot sub-conv)))
    (setf (chatbot-persona-name child-bot) name)
    (append-delegation-instructions child-bot name child-depth requested-budget)
    (save-minion-state sub-conv)
    sub-conv))

(defun minion-public-info (conversation)
  "Returns CONVERSATION's public minion metadata as an alist."
  (let ((sub-bot (conversation-chatbot conversation)))
    `((:name . ,(chatbot-persona-name sub-bot))
      (:backend . ,(string-downcase (symbol-name (chatbot-backend sub-bot))))
      (:model . ,(or (chatbot-model sub-bot) "default")))))

(defun parse-and-execute-spawn-trigger (bot response)
  "Parses RESPONSE for a spawn trigger [SPAWN-SUB: name=\"child_name\", budget=1000].
If found, extracts those parameters, validates, and spawns the child under BOT."
  (let ((pattern "\\[SPAWN-SUB:\\s*name=\"([^\"]+)\"\\s*,\\s*budget=(\\d+)\\]"))
    (cl-ppcre:register-groups-bind (child-name budget-str) (pattern response)
      (let* ((budget (parse-integer budget-str :junk-allowed t))
            ;; Ensure name is unique
            (existing (find-subordinate-conversation bot child-name)))
        (cond
          (existing nil)
          
          ;; 1. Depth guard
          ((> (1+ (chatbot-depth bot)) *max-minion-depth*)
           (format nil "~%[SYSTEM ERROR: Spawn failed: Maximum nesting depth (~D) exceeded.]" *max-minion-depth*))
          
          ;; 2. Budget guard
          ((and (chatbot-token-budget bot)
                (> budget (- (chatbot-token-budget bot) (chatbot-spent-tokens bot))))
           (format nil "~%[SYSTEM ERROR: Spawn failed: Requested budget (~D) exceeds remaining budget (~D).]"
                   budget (- (chatbot-token-budget bot) (chatbot-spent-tokens bot))))
          
          (t
           ;; Deduct budget from parent
           (when (chatbot-token-budget bot)
             (incf (chatbot-spent-tokens bot) budget))
           ;; 3. Sandbox inheritance
           (let* ((parent-dir (or (chatbot-scoped-directory bot)
                                  (chatbot-filesystem-root-directory bot)
                                  (uiop:default-temporary-directory)))
                  (child-dir (merge-pathnames (format nil "minion-sandbox-~A/" child-name) parent-dir)))
             (ensure-directories-exist child-dir)
             ;; Spawn the child
             (let* ((child-depth (1+ (chatbot-depth bot)))
                    (sub-conv
                      (spawn-subordinate-conversation
                       bot
                       child-name
                       child-depth
                       child-dir
                       :backend-kw (chatbot-backend bot)
                       :model (chatbot-model bot)
                       :requested-budget budget
                       :web-tools-p (chatbot-web-tools-p bot))))
               (attach-subordinate-conversation bot sub-conv)
               (format nil "~%[SYSTEM INFO: Successfully spawned subordinate minion '~A' with budget ~D at depth ~D.]"
                       child-name budget child-depth)))))))))

(defun recursively-dismiss-conversation (conv)
  "Recursively dismisses all subordinate conversations of CONV."
  (let* ((bot (conversation-chatbot conv))
         (name (chatbot-persona-name bot)))
    (dolist (sub (chatbot-subordinates bot))
      (recursively-dismiss-conversation sub))
    ;; Delete checkpoint state file if it exists
    (when name
      (let* ((dir (minions-data-directory))
             (file-path (merge-pathnames (format nil "~A.json" name) dir)))
        (when (probe-file file-path)
          (delete-file file-path))))
    (setf (chatbot-subordinates bot) nil)))

(defun generate-timestamped-plan-filename ()
  "Generates a string filename matching plans/plan-YYYYMMDD-HHMM.md."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec))
    (format nil "plans/plan-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D.md"
            year month day hour min)))

(defvar *builtin-tools* (make-hash-table :test 'equal)
  "Registry of built-in chatbot tools, mapping tool-name strings to handler functions.")

(defmacro define-builtin-tool (tool-name (bot-var arguments-var) &body body)
  "Defines a handler for a built-in tool and registers it in *builtin-tools*.
The handler takes BOT and ARGUMENTS. TOOL-NAME is implicitly bound lexically for use in errors."
  `(setf (gethash ,tool-name *builtin-tools*)
         (lambda (,bot-var tool-name ,arguments-var)
           (declare (ignorable ,bot-var tool-name ,arguments-var))
           ,@body)))

(define-builtin-tool "promptSubordinate" (bot arguments)
  (let* ((name (normalize-builtin-tool-string-argument
                   (or (mcp-val "name" arguments)
                       (mcp-val :name arguments))
                   "name"
                   tool-name))
           (prompt (normalize-builtin-tool-string-argument
                    (or (mcp-val "prompt" arguments)
                        (mcp-val :prompt arguments))
                    "prompt"
                    tool-name))
           (sub-conv (find-subordinate-conversation bot name)))
       (unless sub-conv
         (error 'mcp-tool-execution-error
                :tool-name tool-name
                :reason (format nil "Subordinate persona not found: ~A" name)))
       (let* ((response (chat prompt :conversation sub-conv))
              (sub-bot (conversation-chatbot sub-conv))
              (spawn-msg (parse-and-execute-spawn-trigger sub-bot response)))
         ;; Auto-save state
         (save-minion-state sub-conv)
         (if spawn-msg
             (format nil "~A~%~A" response spawn-msg)
             response))))

(define-builtin-tool "spawnMinion" (bot arguments)
  (let* ((name (normalize-builtin-tool-string-argument
                   (or (mcp-val "name" arguments)
                       (mcp-val :name arguments))
                   "name"
                   tool-name))
            (persona-name (let ((val (or (mcp-val "personaName" arguments)
                                         (mcp-val :persona-name arguments)
                                         (mcp-val :personaName arguments))))
                            (and val (string/= val "") val)))
            (backend-str (let ((val (or (mcp-val "backend" arguments)
                                        (mcp-val :backend arguments))))
                           (and val (string/= val "") val)))
            (backend-kw (if backend-str
                            (intern (string-upcase backend-str) "KEYWORD")
                            :gemini))
            (model (let ((val (or (mcp-val "model" arguments)
                                  (mcp-val :model arguments))))
                     (and val (string/= val "") val)))
            (system-instruction (let ((val (or (mcp-val "systemInstruction" arguments)
                                               (mcp-val :system-instruction arguments)
                                               (mcp-val :systemInstruction arguments))))
                                  (and val (string/= val "") val)))
            (requested-budget (let ((val (or (mcp-val "budget" arguments)
                                             (mcp-val :budget arguments))))
                                (and val (numberp val) val)))
            (web-tools-p (let ((cell (or (assoc "webTools" arguments :test #'string-equal)
                                         (assoc :web-tools-p arguments :test #'eq)
                                         (assoc :webTools arguments :test #'eq))))
                           (if cell
                               (cdr cell)
                               (chatbot-web-tools-p bot))))) ; Inherit from parent if omitted
       ;; Ensure name is unique
       (when (find name (chatbot-subordinates bot)
                   :key #'subordinate-conversation-name
                   :test #'string-equal)
         (error 'mcp-tool-execution-error
                :tool-name tool-name
                :reason (format nil "A minion or subordinate named '~A' already exists." name)))
       
       ;; Constraint Validation
       ;; 1. Depth limit guard
       (let ((parent-depth (chatbot-depth bot)))
         (when (>= (1+ parent-depth) *max-minion-depth*)
           (error 'mcp-tool-execution-error
                  :tool-name tool-name
                  :reason (format nil "Spawn failed: Maximum nesting depth (~D) exceeded." *max-minion-depth*))))
       
       ;; 2. Budget validation guard
       (let ((parent-budget (chatbot-token-budget bot))
             (parent-spent (chatbot-spent-tokens bot)))
         (when (and parent-budget requested-budget)
           (let ((remaining (- parent-budget parent-spent)))
             (when (> requested-budget remaining)
               (error 'mcp-tool-execution-error
                      :tool-name tool-name
                      :reason (format nil "Spawn failed: Requested budget (~A) exceeds parent's remaining budget (~A)."
                                      requested-budget remaining)))))
         ;; Deduct budget from parent
         (when requested-budget
           (incf (chatbot-spent-tokens bot) requested-budget)))
       
       ;; 3. Sandbox inheritance
       (let* ((parent-dir (or (chatbot-scoped-directory bot)
                              (chatbot-filesystem-root-directory bot)
                              (uiop:default-temporary-directory)))
              (child-dir (merge-pathnames (format nil "minion-sandbox-~A/" name) parent-dir)))
         (ensure-directories-exist child-dir)
          
         ;; Spawn minion conversation
         (let* ((child-depth (1+ (chatbot-depth bot)))
                (sub-conv
                  (spawn-subordinate-conversation
                   bot
                   name
                   child-depth
                   child-dir
                   :persona-name persona-name
                   :backend-kw backend-kw
                   :model model
                   :system-instruction system-instruction
                   :requested-budget requested-budget
                   :web-tools-p web-tools-p)))
           (attach-subordinate-conversation bot sub-conv)
           (format nil "Minion '~A' spawned successfully." name)))))

(define-builtin-tool "listMinions" (bot arguments)
  (cl-json:encode-json-to-string
   (coerce (mapcar #'minion-public-info
                  (chatbot-subordinates bot))
          'vector)))

(define-builtin-tool "dismissMinion" (bot arguments)
  (let* ((name (normalize-builtin-tool-string-argument
                   (or (mcp-val "name" arguments)
                       (mcp-val :name arguments))
                   "name"
                   tool-name))
            (target (find-subordinate-conversation bot name)))
       (unless target
         (error 'mcp-tool-execution-error
                :tool-name tool-name
                :reason (format nil "Minion '~A' not found." name)))
       ;; Recursively dismiss all of its subordinates first
       (recursively-dismiss-conversation target)
       ;; Now remove target from parent's list
       (setf (chatbot-subordinates bot)
             (remove target (chatbot-subordinates bot) :test #'eq))
       (format nil "Minion '~A' and all of its subordinates dismissed successfully." name)))

(define-builtin-tool "webSearch" (bot arguments)
  (unless (chatbot-web-tools-p bot)
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "Web grounding tools are not enabled."))
     (run-grounding-search tool-name
                           "Web search"
                           *web-search-function*
                           (normalize-builtin-tool-string-argument
                            (or (mcp-val "query" arguments)
                                (mcp-val :query arguments))
                            "query"
                            tool-name)))

(define-builtin-tool "hyperspecSearch" (bot arguments)
  (unless (chatbot-web-tools-p bot)
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "Web grounding tools are not enabled."))
     (run-grounding-search tool-name
                           "HyperSpec search"
                           *hyperspec-search-function*
                           (normalize-builtin-tool-string-argument
                            (or (mcp-val "query" arguments)
                                (mcp-val :query arguments))
                            "query"
                            tool-name)))

(define-builtin-tool "gitCall" (bot arguments)
  (unless (chatbot-enable-git-tools-p bot)
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "Git tool is not enabled."))
     (let* ((args-list (or (mcp-val "args" arguments)
                           (mcp-val :args arguments)))
            (args (loop for arg in args-list
                        collect (typecase arg
                                  (string arg)
                                  (t (format nil "~A" arg)))))
            (dir (or (chatbot-scoped-directory bot)
                     (namestring (uiop:getcwd)))))
       (multiple-value-bind (stdout stderr exit-code)
           (uiop:run-program (cons "git" args)
                             :directory dir
                             :output :string
                             :error-output :string
                             :ignore-error-status t)
         (format nil (concatenate 'string
                                  "~&[Git Executed]~%"
                                  "Command: git ~{~A ~}~%"
                                  "Directory: ~A~%"
                                  "Exit Code: ~D~@[~%"
                                  "STDOUT:~%"
                                  "~A~]~@[~%"
                                  "STDERR:~%"
                                  "~A~]")
                 args dir exit-code
                 (and (string/= stdout "") stdout)
                 (and (string/= stderr "") stderr)))))

(define-builtin-tool "eval" (bot arguments)
  (unless (chatbot-enable-eval-p bot)
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "Eval tool is not enabled."))
     (let* ((expression (normalize-builtin-tool-string-argument
                         (or (mcp-val "expression" arguments)
                             (mcp-val :expression arguments))
                         "expression"
                         tool-name))
            (form (read-eval-tool-form expression tool-name)))
       (approve-chatbot-eval-expression bot expression tool-name)
       (execute-approved-eval-expression expression form tool-name)))

(define-builtin-tool "readSamplingParameters" (bot arguments)
  (sampling-parameters-tool-result bot))

(define-builtin-tool "startAgenticLoop" (bot arguments)
  (unless (current-active-conversation (chatbot-runtime-context bot))
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "No active conversation is bound for autonomous loop startup."))
     (let* ((goal (normalize-builtin-tool-string-argument
                   (or (mcp-val "goal" arguments)
                       (mcp-val :goal arguments))
                   "goal"
                   tool-name))
            (max-iterations (let ((raw (or (mcp-val "maxIterations" arguments)
                                           (mcp-val :max-iterations arguments))))
                              (if raw
                                  (normalize-builtin-tool-integer-argument raw "maxIterations" tool-name)
                                  10)))
            (backend (let ((raw (or (mcp-val "backend" arguments)
                                    (mcp-val :backend arguments))))
                       (when raw
                         (normalize-builtin-tool-string-argument raw "backend" tool-name))))
            (model (let ((raw (or (mcp-val "model" arguments)
                                  (mcp-val :model arguments))))
                     (when raw
                       (normalize-builtin-tool-string-argument raw "model" tool-name))))
            (isolate-p (let ((raw (or (mcp-val "isolate" arguments)
                                      (mcp-val :isolate arguments))))
                         (and raw (eq raw t))))
            (loop (start-agentic-loop (current-active-conversation (chatbot-runtime-context bot))
                                      goal
                                      :max-iterations max-iterations
                                      :backend backend
                                      :model model
                                      :isolate-p isolate-p)))
       (agentic-loop-public-json loop)))

(define-builtin-tool "listAgenticLoops" (bot arguments)
  (agentic-loop-list-json))

(define-builtin-tool "readAgenticLoop" (bot arguments)
  (let* ((loop-id (normalize-builtin-tool-integer-argument
                      (or (mcp-val "loopId" arguments)
                          (mcp-val :loop-id arguments))
                      "loopId"
                      tool-name))
            (loop (or (find-agentic-loop loop-id)
                      (error 'mcp-tool-execution-error
                             :tool-name tool-name
                             :reason (format nil "Unknown agentic loop id: ~A" loop-id)))))
       (agentic-loop-public-json loop)))

(define-builtin-tool "abortAgenticLoop" (bot arguments)
  (multiple-value-bind (force-foundp force-value)
         (builtin-tool-argument arguments "force" :force)
       (let* ((loop-id (normalize-builtin-tool-integer-argument
                        (or (mcp-val "loopId" arguments)
                            (mcp-val :loop-id arguments))
                        "loopId"
                        tool-name))
              (force (if force-foundp
                         (normalize-builtin-tool-boolean-argument force-foundp
                                                                  force-value
                                                                  "force"
                                                                  tool-name)
                         nil))
              (loop (abort-agentic-loop loop-id :force force)))
         (agentic-loop-public-json loop))))

(define-builtin-tool "resumeAgenticLoop" (bot arguments)
  (multiple-value-bind (approve-foundp approve-value)
         (builtin-tool-argument arguments "approve" :approve)
       (let* ((loop-id (normalize-builtin-tool-integer-argument
                        (or (mcp-val "loopId" arguments)
                            (mcp-val :loop-id arguments))
                        "loopId"
                        tool-name))
              (approve (normalize-builtin-tool-boolean-argument approve-foundp
                                                                approve-value
                                                                "approve"
                                                                tool-name))
              (loop (resume-agentic-loop loop-id :approve approve)))
         (agentic-loop-public-json loop))))

(define-builtin-tool "setSamplingParameters" (bot arguments)
  (multiple-value-bind (temperature-foundp temperature-value)
         (builtin-tool-argument arguments "temperature" :temperature)
       (multiple-value-bind (top-p-foundp top-p-value)
           (builtin-tool-argument arguments "topP" :top-p :top_p)
         (unless (or temperature-foundp top-p-foundp)
           (error 'mcp-tool-execution-error
                  :tool-name tool-name
                  :reason "At least one of temperature or topP is required."))
         (handler-case
             (progn
               (apply #'set-sampling-parameters
                      bot
                      (append (when temperature-foundp
                                (list :temperature
                                      (normalize-builtin-tool-real-argument temperature-value "temperature" tool-name :allow-nil-p t)))
                              (when top-p-foundp
                                (list :top-p
                                      (normalize-builtin-tool-real-argument top-p-value "topP" tool-name :allow-nil-p t)))))
               (sampling-parameters-tool-result bot :saved t))
           (error (e)
             (error 'mcp-tool-execution-error
                    :tool-name tool-name
                    :reason (princ-to-string e)))))))

(define-builtin-tool "resetSamplingParameters" (bot arguments)
  (reset-sampling-parameters bot)
     (sampling-parameters-tool-result bot :saved t))

(define-builtin-tool "readFileLines" (bot arguments)
  (let* ((filename (mcp-val :filename arguments))
            (beginning-line (normalize-builtin-tool-integer-argument
                             (or (mcp-val "beginningLine" arguments)
                                 (mcp-val :beginning-line arguments))
                             "beginningLine"
                             tool-name))
            (ending-line (normalize-builtin-tool-integer-argument
                          (or (mcp-val "endingLine" arguments)
                              (mcp-val :ending-line arguments))
                          "endingLine"
                          tool-name))
            (path (resolve-filesystem-tool-path bot filename tool-name)))
       (read-file-lines-subset path beginning-line ending-line tool-name)))

(define-builtin-tool "readSystemInstructions" (bot arguments)
  (system-instruction-tool-result bot))

(define-builtin-tool "insertSystemInstructionParagraph" (bot arguments)
  (insert-system-instruction-paragraph
   bot
   (normalize-builtin-tool-string-argument
    (or (mcp-val "paragraph" arguments)
        (mcp-val :paragraph arguments))
    "paragraph"
    tool-name)
   :index (normalize-builtin-tool-integer-argument
           (or (mcp-val "index" arguments)
               (mcp-val :index arguments))
           "index"
           tool-name))
  (when (chatbot-system-instruction-path bot)
    (save-system-instructions-or-tool-error bot tool-name))
  (system-instruction-tool-result bot :saved (and (chatbot-system-instruction-path bot) t)))

(define-builtin-tool "updateSystemInstructionParagraph" (bot arguments)
  (update-system-instruction-paragraph
   bot
   (normalize-builtin-tool-integer-argument
    (or (mcp-val "index" arguments)
        (mcp-val :index arguments))
    "index"
    tool-name)
   (normalize-builtin-tool-string-argument
    (or (mcp-val "paragraph" arguments)
        (mcp-val :paragraph arguments))
    "paragraph"
    tool-name))
  (when (chatbot-system-instruction-path bot)
    (save-system-instructions-or-tool-error bot tool-name))
  (system-instruction-tool-result bot :saved (and (chatbot-system-instruction-path bot) t)))

(define-builtin-tool "deleteSystemInstructionParagraph" (bot arguments)
  (delete-system-instruction-paragraph
   bot
   (normalize-builtin-tool-integer-argument
    (or (mcp-val "index" arguments)
        (mcp-val :index arguments))
    "index"
    tool-name))
  (when (chatbot-system-instruction-path bot)
    (save-system-instructions-or-tool-error bot tool-name))
  (system-instruction-tool-result bot :saved (and (chatbot-system-instruction-path bot) t)))

(define-builtin-tool "replaceSystemInstructions" (bot arguments)
  (multiple-value-bind (paragraphs-foundp paragraphs-value)
      (builtin-tool-argument arguments "paragraphs" :paragraphs)
    (replace-system-instruction-paragraphs
     bot
     (normalize-builtin-tool-string-sequence-argument
      paragraphs-foundp
      paragraphs-value
      "paragraphs"
      tool-name)))
  (when (chatbot-system-instruction-path bot)
    (save-system-instructions-or-tool-error bot tool-name))
  (system-instruction-tool-result bot :saved (and (chatbot-system-instruction-path bot) t)))

(define-builtin-tool "directory" (bot arguments)
  (multiple-value-bind (directory-path root)
         (resolve-filesystem-tool-directory bot
                                            (or (mcp-val "pathname" arguments)
                                                (mcp-val :pathname arguments))
                                            tool-name)
       (directory-tool-result directory-path
                              root
                              (or (mcp-val "pattern" arguments)
                                  (mcp-val :pattern arguments))
                              tool-name)))

(define-builtin-tool "writeFile" (bot arguments)
  (multiple-value-bind (pathname-foundp pathname)
         (builtin-tool-argument arguments "pathname" :pathname)
       (declare (ignore pathname-foundp))
       (multiple-value-bind (use-lf-only-foundp use-lf-only-value)
           (builtin-tool-argument arguments "useLfOnly" :use-lf-only)
         (multiple-value-bind (end-with-eol-foundp end-with-eol-value)
             (builtin-tool-argument arguments "endWithEol" :end-with-eol)
           (multiple-value-bind (lines-foundp lines-value)
               (builtin-tool-argument arguments "lines" :lines)
             (multiple-value-bind (target-path root)
                 (resolve-filesystem-tool-target-path bot
                                                      (normalize-builtin-tool-string-argument pathname "pathname" tool-name)
                                                      tool-name)
               (write-file-tool-result target-path
                                       root
                                       (normalize-builtin-tool-string-sequence-argument lines-foundp
                                                                                        lines-value
                                                                                        "lines"
                                                                                        tool-name)
                                       (normalize-builtin-tool-boolean-argument use-lf-only-foundp
                                                                                use-lf-only-value
                                                                                "useLfOnly"
                                                                                tool-name)
                                       (normalize-builtin-tool-boolean-argument end-with-eol-foundp
                                                                                end-with-eol-value
                                                                                "endWithEol"
                                                                                tool-name))))))))

(define-builtin-tool "deleteFile" (bot arguments)
  (multiple-value-bind (pathname-foundp pathname)
         (builtin-tool-argument arguments "pathname" :pathname)
       (unless pathname-foundp
         (error 'mcp-tool-execution-error
                :tool-name tool-name
                :reason "pathname is required."))
       (let* ((path (resolve-filesystem-tool-path bot pathname tool-name))
              (root (chatbot-filesystem-root-truename bot tool-name)))
         (delete-file-tool-result path root))))

(define-builtin-tool "submitPlan" (bot arguments)
  (let* ((plan-content (normalize-builtin-tool-string-argument
                           (or (mcp-val "plan_content" arguments)
                               (mcp-val "planContent" arguments)
                               (mcp-val :plan-content arguments)
                               (mcp-val :plan_content arguments))
                           "planContent"
                           tool-name))
            (filename (generate-timestamped-plan-filename)))
       (ensure-directories-exist filename)
       (with-open-file (stream filename
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
         (write-string plan-content stream))
       (format t "~&[PLAN SUBMITTED]~%Filename: ~A~%~%Content:~%~A~%" filename plan-content)
       ;; Toggle state
       (setf (current-active-planner (chatbot-runtime-context bot)) nil)
       ;; Inject transient system message to V (the parent conversation)
       (let ((parent-conv (current-active-planner-parent-conversation (chatbot-runtime-context bot))))
         (when parent-conv
           (setf (conversation-messages parent-conv)
                 (append (conversation-messages parent-conv)
                         (list (list (cons "role" "user")
                                     (cons "content" (format nil "[System: Plan saved to ~A]" filename))))))))
       (format nil "Plan saved successfully to ~A and exited Planner Mode." filename)))

(define-builtin-tool "abortPlan" (bot arguments)
  (let ((reason (or (mcp-val "reason" arguments)
                       (mcp-val :reason arguments)
                       "No reason provided.")))
       (format t "~&[PLAN ABORTED]~%Reason: ~A~%" reason)
       ;; Toggle state
       (setf (current-active-planner (chatbot-runtime-context bot)) nil)
       ;; Inject transient system message to V (the parent conversation)
       (let ((parent-conv (current-active-planner-parent-conversation (chatbot-runtime-context bot))))
         (when parent-conv
           (setf (conversation-messages parent-conv)
                 (append (conversation-messages parent-conv)
                         (list (list (cons "role" "user")
                                     (cons "content" "[System: Planner mode aborted.]")))))))
       (format nil "Planner mode aborted: ~A" reason)))

(define-builtin-tool "invokePlanner" (bot arguments)
  (let* ((context-summary (normalize-builtin-tool-string-argument
                              (or (mcp-val "context_summary" arguments)
                                  (mcp-val "contextSummary" arguments)
                                  (mcp-val :context-summary arguments)
                                  (mcp-val :context_summary arguments))
                              "contextSummary"
                              tool-name))
            ;; Resolve the parent conversation
            (parent-conv (or (current-active-conversation (chatbot-runtime-context bot))
                             (make-instance 'conversation :chatbot bot)))
            ;; Spawn the Planner minion chatbot
            (planner-conv (new-chat :backend (chatbot-backend bot)
                                    :model (chatbot-model bot)
                                    :system-instruction +planner-system-instruction+
                                    :parent-name (chatbot-persona-name bot)
                                    :depth (1+ (chatbot-depth bot))
                                    :planner-p t
                                    :runtime-context (chatbot-runtime-context bot))))
       (setf (chatbot-persona-name (conversation-chatbot planner-conv)) "Planner")
       ;; Link as subordinate
       (setf (chatbot-subordinates bot)
             (append (chatbot-subordinates bot) (list planner-conv)))
       ;; Setup Planner Mode active state to suspend V's REPL context
       (setf (current-active-planner (chatbot-runtime-context bot)) planner-conv)
       (setf (current-active-planner-parent-conversation (chatbot-runtime-context bot)) parent-conv)
       ;; Inject context summary into initial prompt
       (let ((initial-prompt (format nil "Planning Session Initiated.~%Context/Goal Summary: ~A" context-summary)))
         (append-conversation-user-message planner-conv initial-prompt))
       (format nil "Planner minion successfully spawned and Planner Mode activated with goal: ~A" context-summary)))

(defun default-execute-builtin-chatbot-tool (bot tool-name arguments)
  "Executes a built-in tool for BOT using the *builtin-tools* registry."
  (let ((handler (gethash tool-name *builtin-tools*)))
    (if handler
        (funcall handler bot tool-name arguments)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason "Unknown built-in tool."))))

(defun execute-chatbot-tool (bot source tool-name arguments)
  "Executes SOURCE as either a built-in or MCP tool for BOT."
  (if (eq source :built-in)
      (default-execute-builtin-chatbot-tool bot tool-name arguments)
      (execute-mcp-tool source tool-name arguments)))

(defun default-find-mcp-server-and-tool (bot tool-name)
  "Find the connected MCP server and tool definition that matches tool-name."
  (dolist (server (chatbot-mcp-servers bot))
    (handler-case
        (let* ((response (mcp-list-tools server))
               (tools (mcp-val :tools response)))
          (dolist (tool tools)
            (let ((name (mcp-val :name tool)))
              (when (string= name tool-name)
                (return-from default-find-mcp-server-and-tool (values server tool))))))
      (error (e)
        (error 'mcp-tool-lookup-error
               :tool-name tool-name
               :server-name (mcp-server-name server)
               :reason (princ-to-string e)))))
  (values nil nil))

(defun find-mcp-server-and-tool (bot tool-name)
  "Finds an MCP tool by name, honoring the configured test seam when present."
  (if *find-mcp-server-and-tool-function*
      (funcall *find-mcp-server-and-tool-function* bot tool-name)
      (default-find-mcp-server-and-tool bot tool-name)))

(defun mcp-result-items (value)
  "Returns VALUE as a proper list when it represents a JSON array."
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun mcp-structured-content (response)
  "Returns structured tool result content when present on RESPONSE."
  (or (mcp-val "structuredContent" response)
      (mcp-val :structured-content response)
      (mcp-val :structured_content response)))

(defun mcp-jsonish-value->string (value)
  "Renders VALUE as a textual tool result."
  (typecase value
    (null "null")
    (string value)
    (t (cl-json:encode-json-to-string value))))

(defun mcp-tool-result-error-p (response)
  "Returns true when RESPONSE reports a tool-level error."
  (let ((flag (or (mcp-val "isError" response)
                  (mcp-val :is-error response)
                  (mcp-val :is_error response))))
    (not (null flag))))

(defun mcp-tool-result-fallback-payload (response)
  "Returns the most useful non-text payload available on RESPONSE, or NIL."
  (let ((structured-content (mcp-structured-content response))
        (content (mcp-val :content response)))
    (cond
      (structured-content structured-content)
      (content content)
      ((or (mcp-val "result" response)
           (mcp-val :result response))
       (or (mcp-val "result" response)
           (mcp-val :result response)))
      (response response)
      (t nil))))

(defun default-execute-mcp-tool (server tool-name arguments)
  "Calls the tool on the given MCP server and returns the result content string."
  (handler-case
      (let* ((response (mcp-call-tool server tool-name arguments))
             (content (mcp-result-items (mcp-val :content response)))
             (result-texts nil))
        (when (mcp-tool-result-error-p response)
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (mcp-jsonish-value->string response)))
        (dolist (item content)
          (let ((type (mcp-val :type item))
                (text (mcp-val :text item)))
            (when (and (string= type "text") text)
              (push text result-texts))))
        (if result-texts
            (format nil "~{~A~^~%~}" (nreverse result-texts))
            (let ((fallback (mcp-tool-result-fallback-payload response)))
              (if fallback
                  (mcp-jsonish-value->string fallback)
                  "Tool completed successfully."))))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (princ-to-string e)))))

(defun execute-mcp-tool (server tool-name arguments)
  "Executes an MCP tool, honoring the configured test seam when present."
  (if *execute-mcp-tool-function*
      (funcall *execute-mcp-tool-function* server tool-name arguments)
      (default-execute-mcp-tool server tool-name arguments)))
