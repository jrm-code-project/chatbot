;;;

(in-package "CHATBOT")

;;; This file previously held all Chatbot-framework special variables and
;;; runtime-context/lifecycle logic. It has since been split into cohesive
;;; single-concern files (see chatbot.asd for the full list: agentic-directives,
;;; provider-config, tool-approval, mcp-vars, process-globals, runtime-context,
;;; chatbot-lifecycle). This file is retained only as a load-order anchor so
;;; that existing :depends-on ("vars") declarations continue to transitively
;;; pull in all of the split-out files.
