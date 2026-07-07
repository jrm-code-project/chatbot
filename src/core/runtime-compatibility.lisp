;;; -*- Lisp -*-
;;; runtime-compatibility.lisp - deprecated ambient runtime globals and bridges

(in-package "CHATBOT")

(defparameter *mcp-config-path* nil
  "Deprecated compatibility alias for the MCP configuration override path.
Runtime contexts no longer consult this special; use MAKE-RUNTIME-CONTEXT with
:MCP-CONFIG-PATH and explicit :RUNTIME-CONTEXT arguments instead.")

(defvar *startup-chatbot* nil
  "Deprecated compatibility alias for the shared startup chatbot.
Runtime code no longer consults or mirrors this special; use
INITIALIZE-STARTUP-CHATBOT and explicit runtime contexts instead.")

(defparameter *auto-initialize-startup-mcp-servers-p* (eager-mcp-startup-enabled-p)
  "Deprecated compatibility alias for eager shared MCP startup.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :AUTO-INITIALIZE-STARTUP-MCP-SERVERS-P instead.")

(defparameter *logging-enabled-p* t
  "Deprecated compatibility alias controlling Chatbot logging.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :LOGGING-ENABLED-P.")

(defparameter *log-level* :info
  "Deprecated compatibility alias for the Chatbot minimum log level.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :LOG-LEVEL.")

(defparameter *log-stream* *error-output*
  "Deprecated compatibility alias for the Chatbot log output stream.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :LOG-STREAM.")

(defparameter *http-connect-timeout* 15
  "Deprecated compatibility alias for the HTTP connection timeout.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :HTTP-CONNECT-TIMEOUT.")

(defparameter *http-read-timeout* 120
  "Deprecated compatibility alias for the HTTP response timeout.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :HTTP-READ-TIMEOUT.")

(defvar *default-conversation* nil
  "Deprecated compatibility alias for the old ambient default conversation.
Runtime code no longer consults or mirrors this special; use
CURRENT-DEFAULT-CONVERSATION or an explicit runtime context instead.")

(defparameter *agentic-loop-default-backend* nil
  "Deprecated compatibility alias for the agentic-loop default backend.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :AGENTIC-LOOP-DEFAULT-BACKEND.")

(defparameter *agentic-loop-default-model* nil
  "Deprecated compatibility alias for the agentic-loop default model.
Runtime code no longer consults or mirrors this special; use
MAKE-RUNTIME-CONTEXT with :AGENTIC-LOOP-DEFAULT-MODEL.")
