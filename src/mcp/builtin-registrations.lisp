;;; -*- Lisp -*-
;;; builtin-registrations.lisp - built-in chatbot tool registrations

(in-package "CHATBOT")

(define-builtin-tool "promptSubordinate" (bot arguments)
  (execute-prompt-subordinate-tool bot arguments tool-name))

(define-builtin-tool "spawnMinion" (bot arguments)
  (execute-spawn-minion-tool bot arguments tool-name))

(define-builtin-tool "listMinions" (bot arguments)
  (declare (ignore arguments))
  (execute-list-minions-tool bot))

(define-builtin-tool "dismissMinion" (bot arguments)
  (execute-dismiss-minion-tool bot arguments tool-name))

(define-builtin-tool "webSearch" (bot arguments)
  (execute-web-search-tool bot arguments tool-name))

(define-builtin-tool "hyperspecSearch" (bot arguments)
  (execute-hyperspec-search-tool bot arguments tool-name))

(define-builtin-tool "gitCall" (bot arguments)
  (execute-git-call-tool bot arguments tool-name))

(define-builtin-tool "eval" (bot arguments)
  (execute-eval-tool bot arguments tool-name))

(define-builtin-tool "readSamplingParameters" (bot arguments)
  (declare (ignore arguments))
  (execute-read-sampling-parameters-tool bot))

(define-builtin-tool "startAgenticLoop" (bot arguments)
  (execute-start-agentic-loop-tool bot arguments tool-name))

(define-builtin-tool "listAgenticLoops" (bot arguments)
  (declare (ignore bot arguments))
  (execute-list-agentic-loops-tool))

(define-builtin-tool "readAgenticLoop" (bot arguments)
  (declare (ignore bot))
  (execute-read-agentic-loop-tool arguments tool-name))

(define-builtin-tool "abortAgenticLoop" (bot arguments)
  (declare (ignore bot))
  (execute-abort-agentic-loop-tool arguments tool-name))

(define-builtin-tool "resumeAgenticLoop" (bot arguments)
  (declare (ignore bot))
  (execute-resume-agentic-loop-tool arguments tool-name))

(define-builtin-tool "setSamplingParameters" (bot arguments)
  (execute-set-sampling-parameters-tool bot arguments tool-name))

(define-builtin-tool "resetSamplingParameters" (bot arguments)
  (declare (ignore arguments))
  (execute-reset-sampling-parameters-tool bot))

(define-builtin-tool "readFileLines" (bot arguments)
  (execute-read-file-lines-tool bot arguments tool-name))

(define-builtin-tool "readSystemInstructions" (bot arguments)
  (declare (ignore arguments))
  (execute-read-system-instructions-tool bot))

(define-builtin-tool "insertSystemInstructionParagraph" (bot arguments)
  (execute-insert-system-instruction-paragraph-tool bot arguments tool-name))

(define-builtin-tool "updateSystemInstructionParagraph" (bot arguments)
  (execute-update-system-instruction-paragraph-tool bot arguments tool-name))

(define-builtin-tool "deleteSystemInstructionParagraph" (bot arguments)
  (execute-delete-system-instruction-paragraph-tool bot arguments tool-name))

(define-builtin-tool "replaceSystemInstructions" (bot arguments)
  (execute-replace-system-instructions-tool bot arguments tool-name))

(define-builtin-tool "directory" (bot arguments)
  (execute-directory-tool bot arguments tool-name))

(define-builtin-tool "writeFile" (bot arguments)
  (execute-write-file-tool bot arguments tool-name))

(define-builtin-tool "updateScratchpad" (bot arguments)
  (execute-update-scratchpad-tool bot arguments tool-name))

(define-builtin-tool "deleteFile" (bot arguments)
  (execute-delete-file-tool bot arguments tool-name))

(define-builtin-tool "submitPlan" (bot arguments)
  (execute-submit-plan-tool bot arguments tool-name))

(define-builtin-tool "abortPlan" (bot arguments)
  (execute-abort-plan-tool bot arguments))

(define-builtin-tool "invokePlanner" (bot arguments)
  (execute-invoke-planner-tool bot arguments tool-name))
