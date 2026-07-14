;;; -*- Lisp -*-
;;; agentic-loop-tools.lisp - built-in chatbot agentic loop helpers

(in-package "CHATBOT")

(defun worker-id-loop-p (worker-id)
  "Returns true when WORKER-ID identifies an autonomous loop worker."
  (and (stringp worker-id)
       (alexandria:starts-with-subseq "loop:" worker-id)))

(defun worker-id-subordinate-p (worker-id)
  "Returns true when WORKER-ID identifies a subordinate worker."
  (and (stringp worker-id)
       (or (alexandria:starts-with-subseq "minion:" worker-id)
           (alexandria:starts-with-subseq "delegated:" worker-id)
           (alexandria:starts-with-subseq "planner:" worker-id))))

(defun worker-id-suffix (worker-id prefix)
  "Returns WORKER-ID without PREFIX."
  (subseq worker-id (length prefix)))

(defun require-worker-id-argument (arguments tool-name)
  "Returns the required unified worker id from ARGUMENTS."
  (normalize-builtin-tool-string-argument
   (or (mcp-val "workerId" arguments)
       (mcp-val :worker-id arguments)
       (mcp-val :workerId arguments))
   "workerId"
   tool-name))

(defun make-subordinate-worker-ref (conversation)
  "Returns one shared worker reference for subordinate CONVERSATION."
  (make-runtime-worker-entry
   :worker-id (subordinate-conversation-worker-id conversation)
   :kind (subordinate-conversation-worker-kind conversation)
   :conversation conversation))

(defun make-loop-worker-ref (loop)
  "Returns one shared worker reference for autonomous LOOP."
  (make-runtime-worker-entry
   :worker-id (loop-worker-id (agentic-loop-id loop))
   :kind :loop
   :loop loop))

(defun worker-ref-kind (worker-ref)
  "Returns WORKER-REF's kind keyword."
  (runtime-worker-entry-kind worker-ref))

(defun worker-ref-conversation (worker-ref)
  "Returns WORKER-REF's subordinate conversation, or NIL."
  (runtime-worker-entry-conversation worker-ref))

(defun worker-ref-loop (worker-ref)
  "Returns WORKER-REF's autonomous loop, or NIL."
  (runtime-worker-entry-loop worker-ref))

(defun worker-ref-id (worker-ref)
  "Returns WORKER-REF's unified identifier."
  (runtime-worker-entry-worker-id worker-ref))

(defun worker-ref-public-alist (worker-ref)
  "Returns WORKER-REF as a public worker metadata alist."
  (case (worker-ref-kind worker-ref)
    ((:delegated :planner)
     (minion-public-info (worker-ref-conversation worker-ref)))
    (:loop
     (agentic-loop-public-alist (worker-ref-loop worker-ref)))
    (t
     (error "Unsupported worker reference kind: ~A"
            (worker-ref-kind worker-ref)))))

(defun worker-ref-public-kind (worker-ref)
  "Returns WORKER-REF's public kind string."
  (runtime-worker-kind-public-name (worker-ref-kind worker-ref)))

(defun filter-worker-refs-by-kind (worker-refs allowed-kinds)
  "Returns WORKER-REFS whose public kind is present in ALLOWED-KINDS."
  (if (null allowed-kinds)
      worker-refs
      (remove-if-not (lambda (worker-ref)
                      (member (worker-ref-public-kind worker-ref)
                              allowed-kinds
                              :test #'string=))
                    worker-refs)))

(defun list-visible-worker-refs (bot)
  "Returns shared worker references visible from BOT."
  (let ((context (chatbot-runtime-context bot)))
    (remove-if-not
     (lambda (entry)
       (or (eq (worker-ref-kind entry) :loop)
           (eq (runtime-worker-entry-owner-bot entry) bot)))
     (list-runtime-worker-entries context))))

(defun list-visible-workers (bot)
  "Returns unified public worker metadata visible from BOT."
  (mapcar #'worker-ref-public-alist
          (list-visible-worker-refs bot)))

(defun find-subordinate-worker-conversation (bot worker-id)
  "Returns BOT's direct subordinate conversation identified by WORKER-ID."
  (let ((entry (find-runtime-worker-entry worker-id (chatbot-runtime-context bot))))
    (and entry
         (subordinate-runtime-worker-kind-p (worker-ref-kind entry))
         (eq (runtime-worker-entry-owner-bot entry) bot)
         (worker-ref-conversation entry))))

(defun worker-public-json (worker-alist)
  "Returns WORKER-ALIST encoded as JSON."
  (cl-json:encode-json-to-string worker-alist))

(defun worker-list-json (bot &key kinds (envelope-key "workers"))
  "Returns visible workers for BOT encoded as JSON."
  (let* ((worker-refs (if bot
                          (list-visible-worker-refs bot)
                          (mapcar #'make-loop-worker-ref
                                  (list-agentic-loops))))
         (worker-refs (filter-worker-refs-by-kind worker-refs kinds))
         (workers (mapcar #'worker-ref-public-alist worker-refs)))
    (cl-json:encode-json-to-string
     `((,envelope-key . ,(coerce workers 'vector))))))

(defun loop-worker-id (loop-id)
  "Returns the unified worker id for LOOP-ID."
  (format nil "loop:~A" loop-id))

(defun require-loop-id-argument (arguments tool-name)
  "Returns the required legacy loop id argument."
  (normalize-builtin-tool-integer-argument
   (or (mcp-val "loopId" arguments)
       (mcp-val :loop-id arguments))
   "loopId"
   tool-name))

(defun require-worker-id-for-subordinate-name (bot arguments tool-name)
  "Returns the worker id for the legacy subordinate NAME argument."
  (let* ((name (normalize-builtin-tool-string-argument
                (or (mcp-val "name" arguments)
                    (mcp-val :name arguments))
                "name"
                tool-name))
         (conversation (find-subordinate-conversation bot name)))
    (unless conversation
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Subordinate persona not found: ~A" name)))
    (subordinate-conversation-worker-id conversation)))

(defun resolve-worker-ref (bot worker-id tool-name)
  "Returns the shared worker reference for WORKER-ID."
  (let* ((context (or (and bot (chatbot-runtime-context bot))
                     (resolve-runtime-context nil)))
        (entry (and context
                    (find-runtime-worker-entry worker-id context))))
    (cond
      (entry entry)
      ((worker-id-loop-p worker-id)
      (let ((loop-id (parse-integer (worker-id-suffix worker-id "loop:") :junk-allowed nil)))
        (make-loop-worker-ref
         (require-agentic-loop-by-id loop-id tool-name))))
      ((and bot (worker-id-subordinate-p worker-id))
      (let ((conversation (find-subordinate-worker-conversation bot worker-id)))
        (unless conversation
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "Unknown worker id: ~A" worker-id)))
        (make-subordinate-worker-ref conversation)))
      (t
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Unsupported worker id: ~A" worker-id))))))

(defun resolve-worker-public-alist (bot worker-id &optional (tool-name "worker"))
  "Returns the unified public worker alist for WORKER-ID."
  (worker-ref-public-alist
   (resolve-worker-ref bot worker-id tool-name)))

(defun require-agentic-loop-active-conversation (bot tool-name)
  "Returns BOT's active conversation or signals a tool execution error."
  (or (current-active-conversation (chatbot-runtime-context bot))
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "No active conversation is bound for autonomous loop startup.")))

(defun optional-agentic-loop-string-argument (arguments string-key keyword-key field-name tool-name)
  "Returns the optional string value for FIELD-NAME when present in ARGUMENTS."
  (let ((raw (or (mcp-val string-key arguments)
                 (mcp-val keyword-key arguments))))
    (when raw
      (normalize-builtin-tool-string-argument raw field-name tool-name))))

(defun require-agentic-loop-by-id (loop-id tool-name)
  "Returns the registered agentic loop for LOOP-ID or signals a tool execution error."
  (or (find-agentic-loop loop-id)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Unknown agentic loop id: ~A" loop-id))))

(defun execute-start-agentic-loop-tool (bot arguments tool-name)
  "Runs the built-in startAgenticLoop tool."
  (execute-spawn-worker-tool
   bot
   (append '(("mode" . "autonomous")) arguments)
   tool-name))

(defun execute-list-agentic-loops-tool ()
  "Runs the built-in listAgenticLoops tool."
  (worker-list-json nil :kinds '("autonomous") :envelope-key "loops"))

(defun execute-read-agentic-loop-tool (arguments tool-name)
  "Runs the built-in readAgenticLoop tool."
  (worker-public-json
   (resolve-worker-public-alist nil
                                (loop-worker-id
                                 (require-loop-id-argument arguments tool-name)))))

(defun execute-abort-agentic-loop-tool (arguments tool-name)
  "Runs the built-in abortAgenticLoop tool."
  (execute-abort-worker-tool
   nil
   `(("workerId" . ,(loop-worker-id (require-loop-id-argument arguments tool-name)))
     ,@(let ((entry (assoc "force" arguments :test #'string-equal)))
         (if entry
             (list (cons "force" (cdr entry)))
             (let ((keyword-entry (assoc :force arguments :test #'eq)))
               (if keyword-entry
                   (list (cons "force" (cdr keyword-entry)))
                   nil)))))
   tool-name))

(defun execute-resume-agentic-loop-tool (arguments tool-name)
  "Runs the built-in resumeAgenticLoop tool."
  (multiple-value-bind (approve-foundp approve-value)
      (builtin-tool-argument arguments "approve" :approve)
    (let ((approve (normalize-builtin-tool-boolean-argument approve-foundp
                                                            approve-value
                                                            "approve"
                                                            tool-name)))
      (execute-resume-worker-tool
       `(("workerId" . ,(loop-worker-id (require-loop-id-argument arguments tool-name)))
         ("approve" . ,approve))
       tool-name))))

(defun execute-list-minions-via-workers-tool (bot)
  "Runs the legacy listMinions tool via unified worker metadata."
  (let ((workers (mapcar #'worker-ref-public-alist
                         (filter-worker-refs-by-kind
                          (list-visible-worker-refs bot)
                          '("delegated" "planner")))))
    (cl-json:encode-json-to-string (coerce workers 'vector))))

(defun prompt-subordinate-worker-conversation (bot conversation prompt)
  "Prompts subordinate CONVERSATION from BOT with PROMPT."
  (let* ((name (subordinate-conversation-name conversation))
         (task-key `(:kind :prompt-subordinate
                    :name ,name
                    :prompt ,prompt)))
    (call-with-idempotent-chatbot-task
     bot
     task-key
     (lambda ()
       (let* ((response (chat prompt :conversation conversation))
              (sub-bot (conversation-chatbot conversation))
              (control (parse-subordinate-control-response
                        response
                        :backend (chatbot-backend sub-bot)))
              (spawn-msg (maybe-execute-subordinate-spawn-request sub-bot
                                                                  (getf control :spawn))))
         (save-minion-state conversation)
         (if spawn-msg
             (format nil "~A~%~A" (getf control :reply) spawn-msg)
             (getf control :reply)))))))

(defun dismiss-subordinate-worker-conversation (bot conversation)
  "Dismisses subordinate CONVERSATION from BOT."
  (let ((name (subordinate-conversation-name conversation)))
    (recursively-dismiss-conversation conversation)
    (setf (chatbot-subordinates bot)
          (remove conversation (chatbot-subordinates bot) :test #'eq))
    (format nil "Minion '~A' and all of its subordinates dismissed successfully." name)))

(defun execute-prompt-worker-ref (bot worker-ref prompt tool-name)
  "Prompts WORKER-REF using PROMPT."
  (case (worker-ref-kind worker-ref)
    ((:delegated :planner)
     (prompt-subordinate-worker-conversation bot
                                             (worker-ref-conversation worker-ref)
                                             prompt))
    (t
     (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason "Only delegated/planner workers can be prompted directly."))))

(defun execute-abort-worker-ref (bot worker-ref tool-name force)
  "Aborts or dismisses WORKER-REF."
  (declare (ignore tool-name))
  (case (worker-ref-kind worker-ref)
    (:loop
     (agentic-loop-public-json
      (interrupt-agentic-loop-instance (worker-ref-loop worker-ref)
                                       :force force)))
    ((:delegated :planner)
     (dismiss-subordinate-worker-conversation bot
                                              (worker-ref-conversation worker-ref))
     (worker-public-json
      `(("workerId" . ,(worker-ref-id worker-ref))
        ("status" . "dismissed"))))
    (t
     (error "Unsupported worker reference kind: ~A"
            (worker-ref-kind worker-ref)))))

(defun execute-resume-worker-ref (worker-ref approve tool-name)
  "Resumes WORKER-REF when supported."
  (case (worker-ref-kind worker-ref)
    (:loop
     (agentic-loop-public-json
      (resume-agentic-loop-instance (worker-ref-loop worker-ref)
                                    :approve approve)))
    (t
     (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason "Only autonomous workers support resume."))))

(defun execute-prompt-subordinate-via-workers-tool (bot arguments tool-name)
  "Runs the legacy promptSubordinate tool via the unified worker prompt path."
  (execute-prompt-worker-ref
   bot
   (resolve-worker-ref bot
                       (require-worker-id-for-subordinate-name bot arguments tool-name)
                       tool-name)
   (normalize-builtin-tool-string-argument
    (or (mcp-val "prompt" arguments)
        (mcp-val :prompt arguments))
    "prompt"
    tool-name)
   tool-name))

(defun execute-dismiss-minion-via-workers-tool (bot arguments tool-name)
  "Runs the legacy dismissMinion tool via the unified worker abort path."
  (let ((worker-id (require-worker-id-for-subordinate-name bot arguments tool-name)))
    (dismiss-subordinate-worker-conversation
     bot
     (worker-ref-conversation
      (resolve-worker-ref bot worker-id tool-name)))))

(defun execute-spawn-minion-via-workers-tool (bot arguments tool-name)
  "Runs the legacy spawnMinion tool via the unified worker spawn path."
  (let ((name (normalize-builtin-tool-string-argument
               (or (mcp-val "name" arguments)
                   (mcp-val :name arguments))
               "name"
               tool-name)))
    (execute-spawn-worker-tool
     bot
     (append '(("mode" . "delegated")) arguments)
     tool-name)
    (format nil "Minion '~A' spawned successfully." name)))

(defun execute-invoke-planner-via-workers-tool (bot arguments tool-name)
  "Runs the legacy invokePlanner tool via the unified worker spawn path."
  (let ((goal (normalize-builtin-tool-string-argument
               (or (mcp-val "context_summary" arguments)
                   (mcp-val "contextSummary" arguments)
                   (mcp-val :context-summary arguments)
                   (mcp-val :context_summary arguments))
               "contextSummary"
               tool-name)))
    (execute-spawn-worker-tool
     bot
     `(("mode" . "planner")
       ("goal" . ,goal))
     tool-name)
    (format nil "Planner minion successfully spawned and Planner Mode activated with goal: ~A"
            goal)))

(defun execute-list-workers-tool (bot)
  "Runs the built-in listWorkers tool."
  (worker-list-json bot))

(defun execute-read-worker-tool (bot arguments tool-name)
  "Runs the built-in readWorker tool."
  (worker-public-json
   (resolve-worker-public-alist bot
                                (require-worker-id-argument arguments tool-name)
                                tool-name)))

(defun execute-prompt-worker-tool (bot arguments tool-name)
  "Runs the built-in promptWorker tool."
  (execute-prompt-worker-ref
   bot
   (resolve-worker-ref bot
                       (require-worker-id-argument arguments tool-name)
                       tool-name)
   (normalize-builtin-tool-string-argument
    (or (mcp-val "prompt" arguments)
        (mcp-val :prompt arguments))
    "prompt"
    tool-name)
   tool-name))

(defun execute-abort-worker-tool (bot arguments tool-name)
  "Runs the built-in abortWorker tool."
  (execute-abort-worker-ref
   bot
   (resolve-worker-ref bot
                       (require-worker-id-argument arguments tool-name)
                       tool-name)
   tool-name
   (let ((raw (or (mcp-val "force" arguments)
                  (mcp-val :force arguments))))
     (and raw (eq raw t)))))

(defun execute-resume-worker-tool (arguments tool-name)
  "Runs the built-in resumeWorker tool."
  (let ((approve-foundp nil)
        (approve-value nil))
    (multiple-value-setq (approve-foundp approve-value)
      (builtin-tool-argument arguments "approve" :approve))
    (execute-resume-worker-ref
     (resolve-worker-ref nil
                         (require-worker-id-argument arguments tool-name)
                         tool-name)
     (normalize-builtin-tool-boolean-argument approve-foundp
                                              approve-value
                                              "approve"
                                              tool-name)
     tool-name)))

(defun execute-spawn-worker-tool (bot arguments tool-name)
  "Runs the built-in spawnWorker tool."
  (let* ((mode-raw (or (mcp-val "mode" arguments)
                       (mcp-val :mode arguments)
                       "delegated"))
         (mode (string-downcase
                (normalize-builtin-tool-string-argument mode-raw "mode" tool-name))))
    (cond
      ((string= mode "autonomous")
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
              (backend (optional-agentic-loop-string-argument arguments "backend" :backend "backend" tool-name))
              (model (optional-agentic-loop-string-argument arguments "model" :model "model" tool-name))
              (isolate-p (let ((raw (or (mcp-val "isolate" arguments)
                                        (mcp-val :isolate arguments))))
                           (and raw (eq raw t))))
              (conversation (require-agentic-loop-active-conversation bot tool-name))
              (loop (start-agentic-loop conversation
                                        goal
                                        :max-iterations max-iterations
                                        :backend backend
                                        :model model
                                        :isolate-p isolate-p)))
         (agentic-loop-public-json loop)))
      ((string= mode "planner")
       (let ((planner
               (spawn-planner-worker-conversation
                bot
                (normalize-builtin-tool-string-argument
                 (or (mcp-val "goal" arguments)
                     (mcp-val "contextSummary" arguments)
                     (mcp-val "context_summary" arguments)
                     (mcp-val :goal arguments))
                 "goal"
                 tool-name)
                :tool-name tool-name)))
         (worker-public-json (minion-public-info planner))))
      (t
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
         (worker-public-json
          (minion-public-info
           (call-with-idempotent-chatbot-task
            bot
            task-key
            (lambda ()
              (spawn-delegated-worker-conversation
               bot
               name
               :persona-name persona-name
               :backend-kw backend-kw
               :model model
               :system-instruction system-instruction
               :requested-budget requested-budget
               :web-tools-p web-tools-p
               :tool-name tool-name))))))))))
