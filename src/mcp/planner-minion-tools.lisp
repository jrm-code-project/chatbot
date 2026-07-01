;;; -*- Lisp -*-
;;; planner-minion-tools.lisp - built-in chatbot planner and minion helpers

(in-package "CHATBOT")

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

(defun make-chatbot-task-journal-entry ()
  "Returns one fresh in-flight idempotency journal entry."
  (list :status :running
        :result nil
        :waitqueue (sb-thread:make-waitqueue :name "chatbot-task-journal-entry")))

(defun chatbot-task-journal-entry-status (entry)
  "Returns ENTRY's execution status."
  (getf entry :status))

(defun chatbot-task-journal-entry-result (entry)
  "Returns ENTRY's cached task result."
  (getf entry :result))

(defun chatbot-task-journal-entry-waitqueue (entry)
  "Returns ENTRY's completion waitqueue."
  (getf entry :waitqueue))

(defun complete-chatbot-task-journal-entry (entry result)
  "Marks ENTRY complete with RESULT and returns ENTRY."
  (setf (getf entry :status) :completed)
  (setf (getf entry :result) result)
  entry)

(defun call-with-idempotent-chatbot-task (bot task-key thunk)
  "Runs THUNK exactly once per immutable TASK-KEY for BOT, reusing the first completed result."
  (let ((journal (chatbot-task-journal bot))
        (lock (chatbot-task-journal-lock bot)))
    (block done
      (loop
        with claimed-entry = nil
        do (sb-thread:with-mutex (lock)
             (let ((entry (gethash task-key journal)))
               (cond
                 ((null entry)
                  (setf claimed-entry (make-chatbot-task-journal-entry))
                  (setf (gethash task-key journal) claimed-entry))
                 ((eq (chatbot-task-journal-entry-status entry) :completed)
                  (return-from done (chatbot-task-journal-entry-result entry)))
                 ((eq (chatbot-task-journal-entry-status entry) :running)
                  (sb-thread:condition-wait
                   (chatbot-task-journal-entry-waitqueue entry)
                   lock))
                 (t
                  (remhash task-key journal)))))
           (when claimed-entry
             (handler-case
                 (let ((result (funcall thunk)))
                   (sb-thread:with-mutex (lock)
                     (complete-chatbot-task-journal-entry claimed-entry result)
                     (sb-thread:condition-broadcast
                      (chatbot-task-journal-entry-waitqueue claimed-entry)))
                   (return-from done result))
               (error (condition)
                 (sb-thread:with-mutex (lock)
                   (remhash task-key journal)
                   (sb-thread:condition-broadcast
                    (chatbot-task-journal-entry-waitqueue claimed-entry)))
                 (error condition))))))))

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
             (existing (find-subordinate-conversation bot child-name)))
        (cond
          (existing nil)
          ((> (1+ (chatbot-depth bot)) *max-minion-depth*)
           (format nil "~%[SYSTEM ERROR: Spawn failed: Maximum nesting depth (~D) exceeded.]" *max-minion-depth*))
          ((and (chatbot-token-budget bot)
                (> budget (- (chatbot-token-budget bot) (chatbot-spent-tokens bot))))
           (format nil "~%[SYSTEM ERROR: Spawn failed: Requested budget (~D) exceeds remaining budget (~D).]"
                   budget (- (chatbot-token-budget bot) (chatbot-spent-tokens bot))))
          (t
           (when (chatbot-token-budget bot)
             (incf (chatbot-spent-tokens bot) budget))
           (let* ((parent-dir (or (chatbot-scoped-directory bot)
                                  (chatbot-filesystem-root-directory bot)
                                  (uiop:default-temporary-directory)))
                  (child-dir (merge-pathnames (format nil "minion-sandbox-~A/" child-name) parent-dir)))
             (ensure-directories-exist child-dir)
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

(defun execute-prompt-subordinate-tool (bot arguments tool-name)
  "Runs the built-in promptSubordinate tool."
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
         (sub-conv (find-subordinate-conversation bot name))
         (task-key `(:kind :prompt-subordinate
                    :name ,name
                    :prompt ,prompt)))
    (unless sub-conv
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Subordinate persona not found: ~A" name)))
    (call-with-idempotent-chatbot-task
     bot
     task-key
     (lambda ()
       (let* ((response (chat prompt :conversation sub-conv))
              (sub-bot (conversation-chatbot sub-conv))
              (spawn-msg (parse-and-execute-spawn-trigger sub-bot response)))
         (save-minion-state sub-conv)
         (if spawn-msg
             (format nil "~A~%~A" response spawn-msg)
             response))))))

(defun execute-spawn-minion-tool (bot arguments tool-name)
  "Runs the built-in spawnMinion tool."
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
                            (chatbot-web-tools-p bot))))
         (task-key `(:kind :spawn-minion
                    :name ,name
                    :persona-name ,persona-name
                    :backend ,backend-kw
                    :model ,model
                    :system-instruction ,system-instruction
                    :requested-budget ,requested-budget
                    :web-tools-p ,web-tools-p)))
    (call-with-idempotent-chatbot-task
     bot
     task-key
     (lambda ()
       (when (find name (chatbot-subordinates bot)
                  :key #'subordinate-conversation-name
                  :test #'string-equal)
         (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "A minion or subordinate named '~A' already exists." name)))
       (let ((parent-depth (chatbot-depth bot)))
         (when (>= (1+ parent-depth) *max-minion-depth*)
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "Spawn failed: Maximum nesting depth (~D) exceeded." *max-minion-depth*))))
       (let ((parent-budget (chatbot-token-budget bot))
            (parent-spent (chatbot-spent-tokens bot)))
         (when (and parent-budget requested-budget)
          (let ((remaining (- parent-budget parent-spent)))
            (when (> requested-budget remaining)
              (error 'mcp-tool-execution-error
                     :tool-name tool-name
                     :reason (format nil "Spawn failed: Requested budget (~A) exceeds parent's remaining budget (~A)."
                                     requested-budget remaining)))))
         (when requested-budget
          (incf (chatbot-spent-tokens bot) requested-budget)))
       (let* ((parent-dir (or (chatbot-scoped-directory bot)
                             (chatbot-filesystem-root-directory bot)
                             (uiop:default-temporary-directory)))
             (child-dir (merge-pathnames (format nil "minion-sandbox-~A/" name) parent-dir)))
         (ensure-directories-exist child-dir)
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
          (format nil "Minion '~A' spawned successfully." name)))))))

(defun execute-list-minions-tool (bot)
  "Runs the built-in listMinions tool."
  (cl-json:encode-json-to-string
   (coerce (mapcar #'minion-public-info
                   (chatbot-subordinates bot))
           'vector)))

(defun execute-dismiss-minion-tool (bot arguments tool-name)
  "Runs the built-in dismissMinion tool."
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
    (recursively-dismiss-conversation target)
    (setf (chatbot-subordinates bot)
          (remove target (chatbot-subordinates bot) :test #'eq))
    (format nil "Minion '~A' and all of its subordinates dismissed successfully." name)))

(defun execute-submit-plan-tool (bot arguments tool-name)
  "Runs the built-in submitPlan tool."
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
    (setf (current-active-planner (chatbot-runtime-context bot)) nil)
    (let ((parent-conv (current-active-planner-parent-conversation (chatbot-runtime-context bot))))
      (when parent-conv
        (setf (conversation-messages parent-conv)
              (append (conversation-messages parent-conv)
                      (list (list (cons "role" "user")
                                  (cons "content" (format nil "[System: Plan saved to ~A]" filename))))))))
    (format nil "Plan saved successfully to ~A and exited Planner Mode." filename)))

(defun execute-abort-plan-tool (bot arguments)
  "Runs the built-in abortPlan tool."
  (let ((reason (or (mcp-val "reason" arguments)
                    (mcp-val :reason arguments)
                    "No reason provided.")))
    (format t "~&[PLAN ABORTED]~%Reason: ~A~%" reason)
    (setf (current-active-planner (chatbot-runtime-context bot)) nil)
    (let ((parent-conv (current-active-planner-parent-conversation (chatbot-runtime-context bot))))
      (when parent-conv
        (setf (conversation-messages parent-conv)
              (append (conversation-messages parent-conv)
                      (list (list (cons "role" "user")
                                  (cons "content" "[System: Planner mode aborted.]")))))))
    (format nil "Planner mode aborted: ~A" reason)))

(defun execute-invoke-planner-tool (bot arguments tool-name)
  "Runs the built-in invokePlanner tool."
  (let* ((context-summary (normalize-builtin-tool-string-argument
                           (or (mcp-val "context_summary" arguments)
                               (mcp-val "contextSummary" arguments)
                               (mcp-val :context-summary arguments)
                               (mcp-val :context_summary arguments))
                           "contextSummary"
                           tool-name))
         (parent-conv (or (current-active-conversation (chatbot-runtime-context bot))
                          (make-instance 'conversation :chatbot bot)))
         (planner-conv (new-chat :backend (chatbot-backend bot)
                                 :model (chatbot-model bot)
                                 :system-instruction +planner-system-instruction+
                                 :parent-name (chatbot-persona-name bot)
                                 :depth (1+ (chatbot-depth bot))
                                 :planner-p t
                                 :runtime-context (chatbot-runtime-context bot))))
    (setf (chatbot-persona-name (conversation-chatbot planner-conv)) "Planner")
    (setf (chatbot-subordinates bot)
          (append (chatbot-subordinates bot) (list planner-conv)))
    (setf (current-active-planner (chatbot-runtime-context bot)) planner-conv)
    (setf (current-active-planner-parent-conversation (chatbot-runtime-context bot)) parent-conv)
    (let ((initial-prompt (format nil "Planning Session Initiated.~%Context/Goal Summary: ~A" context-summary)))
      (append-conversation-user-message planner-conv initial-prompt))
    (format nil "Planner minion successfully spawned and Planner Mode activated with goal: ~A" context-summary)))
