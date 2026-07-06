;;; -*- Lisp -*-
;;; runtime-compatibility.lisp - deprecated ambient runtime globals and bridges

(in-package "CHATBOT")

(defparameter *mcp-config-path* nil
  "Deprecated compatibility alias for the MCP configuration override path.
Runtime contexts no longer consult this special; use MAKE-RUNTIME-CONTEXT with
:MCP-CONFIG-PATH and explicit :RUNTIME-CONTEXT arguments instead.")

(defparameter *warn-on-legacy-runtime-globals-p*
  (let ((value (funcall *getenv-function* "CHATBOT_WARN_LEGACY_RUNTIME_GLOBALS")))
    (and value
         (member (string-downcase value)
                 '("1" "true" "yes" "on")
                 :test #'string=)))
  "When true, emit one-time migration warnings when compatibility-only ambient
runtime globals are used to override the default runtime context.")

(defvar *legacy-runtime-global-warnings-issued* nil
  "Internal list of compatibility globals already warned about in this image.")

(defparameter *legacy-runtime-global-replacements*
  '((*mcp-config-path* . "MAKE-RUNTIME-CONTEXT with :MCP-CONFIG-PATH and explicit :RUNTIME-CONTEXT arguments")
    (*auto-initialize-startup-mcp-servers-p* . "MAKE-RUNTIME-CONTEXT with :AUTO-INITIALIZE-STARTUP-MCP-SERVERS-P")
    (*default-conversation* . "passing :CONVERSATION explicitly or using MAKE-RUNTIME-CONTEXT")
    (*logging-enabled-p* . "MAKE-RUNTIME-CONTEXT with :LOGGING-ENABLED-P")
    (*log-level* . "MAKE-RUNTIME-CONTEXT with :LOG-LEVEL")
    (*log-stream* . "MAKE-RUNTIME-CONTEXT with :LOG-STREAM")
    (*http-connect-timeout* . "MAKE-RUNTIME-CONTEXT with :HTTP-CONNECT-TIMEOUT")
    (*http-read-timeout* . "MAKE-RUNTIME-CONTEXT with :HTTP-READ-TIMEOUT")
    (*agentic-loop-default-backend* . "MAKE-RUNTIME-CONTEXT with :AGENTIC-LOOP-DEFAULT-BACKEND")
    (*agentic-loop-default-model* . "MAKE-RUNTIME-CONTEXT with :AGENTIC-LOOP-DEFAULT-MODEL")
    (*filesystem-access-approval-function* . "MAKE-RUNTIME-CONTEXT with :FILESYSTEM-ACCESS-APPROVAL-FUNCTION")
    (*eval-approval-function* . "MAKE-RUNTIME-CONTEXT with :EVAL-APPROVAL-FUNCTION"))
  "Compatibility-only ambient globals mapped to their preferred replacements.")

(defvar *startup-chatbot* nil
  "Deprecated compatibility alias for the shared startup chatbot.
Runtime contexts own startup MCP state; use INITIALIZE-STARTUP-CHATBOT and
explicit runtime contexts instead of mutating this special directly.")

(defparameter *auto-initialize-startup-mcp-servers-p* (eager-mcp-startup-enabled-p)
  "Deprecated compatibility alias for eager shared MCP startup.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:AUTO-INITIALIZE-STARTUP-MCP-SERVERS-P instead.")

(defparameter *logging-enabled-p* t
  "Deprecated compatibility alias controlling Chatbot logging.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:LOGGING-ENABLED-P.")

(defparameter *log-level* :info
  "Deprecated compatibility alias for the Chatbot minimum log level.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with :LOG-LEVEL.")

(defparameter *log-stream* *error-output*
  "Deprecated compatibility alias for the Chatbot log output stream.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with :LOG-STREAM.")

(defparameter *http-connect-timeout* 15
  "Deprecated compatibility alias for the HTTP connection timeout.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:HTTP-CONNECT-TIMEOUT.")

(defparameter *http-read-timeout* 120
  "Deprecated compatibility alias for the HTTP response timeout.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:HTTP-READ-TIMEOUT.")

(defvar *default-conversation* nil
  "Compatibility-only ambient default conversation used by CHAT when none is specified.
Prefer passing :CONVERSATION explicitly or using a runtime context.")

(defparameter *agentic-loop-default-backend* nil
  "Deprecated compatibility alias for the agentic-loop default backend.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:AGENTIC-LOOP-DEFAULT-BACKEND.")

(defparameter *agentic-loop-default-model* nil
  "Deprecated compatibility alias for the agentic-loop default model.
Runtime contexts own this setting; use MAKE-RUNTIME-CONTEXT with
:AGENTIC-LOOP-DEFAULT-MODEL.")

(defun same-runtime-state-p (left right)
  "Returns true when LEFT and RIGHT represent the same runtime state."
  (or (eq left right)
      (equal left right)))

(defun maybe-warn-legacy-default-conversation-usage (current-value context-value warn-p)
  "Warns once when *DEFAULT-CONVERSATION* diverges from CONTEXT-VALUE."
  (when (and warn-p
             *warn-on-legacy-runtime-globals-p*
             (not (same-runtime-state-p current-value context-value))
             (not (member '*default-conversation*
                          *legacy-runtime-global-warnings-issued*
                          :test #'eq)))
    (push '*default-conversation* *legacy-runtime-global-warnings-issued*)
    (format *error-output*
            "[CHATBOT WARN] ~A is a compatibility-only ambient runtime global; prefer ~A.~%"
            '*default-conversation*
            (or (cdr (assoc '*default-conversation*
                            *legacy-runtime-global-replacements*
                            :test #'eq))
                "MAKE-RUNTIME-CONTEXT and explicit :RUNTIME-CONTEXT usage"))))

(defun sync-default-conversation-from-legacy-global (context &key (warn-p t))
  "Copies the ambient *DEFAULT-CONVERSATION* compatibility value into CONTEXT."
  (let ((current-value *default-conversation*)
        (context-value (runtime-context-default-conversation context)))
    (maybe-warn-legacy-default-conversation-usage current-value context-value warn-p)
    (setf (runtime-context-default-conversation context) current-value))
  context)

(defun sync-legacy-default-conversation-from-runtime-context (context)
  "Copies CONTEXT's default conversation back into the legacy compatibility global."
  (setf *default-conversation* (runtime-context-default-conversation context))
  context)

(defun maybe-sync-legacy-globals-from-default-runtime-context (context)
  "Copies CONTEXT back to legacy globals when it is the default runtime context."
  (when (default-runtime-context-p context)
    (sync-legacy-default-conversation-from-runtime-context context))
  context)
