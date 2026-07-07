;;; -*- Lisp -*-
;;; runtime-compatibility.lisp - deprecated ambient runtime globals and bridges

(in-package "CHATBOT")

(defparameter *mcp-config-path* nil
  "Deprecated compatibility alias for the MCP configuration override path.
Runtime contexts no longer consult this special; use MAKE-RUNTIME-CONTEXT with
:MCP-CONFIG-PATH and explicit :RUNTIME-CONTEXT arguments instead.")

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

(defun sync-legacy-default-conversation-from-runtime-context (context)
  "Copies CONTEXT's default conversation back into the legacy compatibility global."
  (setf *default-conversation* (runtime-context-default-conversation context))
  context)

(defun maybe-sync-legacy-globals-from-default-runtime-context (context)
  "Copies CONTEXT back to legacy globals when it is the default runtime context."
  (when (default-runtime-context-p context)
    (sync-legacy-default-conversation-from-runtime-context context))
  context)
