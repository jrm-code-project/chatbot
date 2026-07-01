;;; -*- Lisp -*-
;;; agentic-loop-tools.lisp - built-in chatbot agentic loop helpers

(in-package "CHATBOT")

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
  (let* ((conversation (require-agentic-loop-active-conversation bot tool-name))
         (goal (normalize-builtin-tool-string-argument
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
         (loop (start-agentic-loop conversation
                                   goal
                                   :max-iterations max-iterations
                                   :backend backend
                                   :model model
                                   :isolate-p isolate-p)))
    (agentic-loop-public-json loop)))

(defun execute-list-agentic-loops-tool ()
  "Runs the built-in listAgenticLoops tool."
  (agentic-loop-list-json))

(defun execute-read-agentic-loop-tool (arguments tool-name)
  "Runs the built-in readAgenticLoop tool."
  (let* ((loop-id (normalize-builtin-tool-integer-argument
                   (or (mcp-val "loopId" arguments)
                       (mcp-val :loop-id arguments))
                   "loopId"
                   tool-name))
         (loop (require-agentic-loop-by-id loop-id tool-name)))
    (agentic-loop-public-json loop)))

(defun execute-abort-agentic-loop-tool (arguments tool-name)
  "Runs the built-in abortAgenticLoop tool."
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

(defun execute-resume-agentic-loop-tool (arguments tool-name)
  "Runs the built-in resumeAgenticLoop tool."
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
