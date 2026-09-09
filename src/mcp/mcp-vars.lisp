;;;

(in-package "CHATBOT")

;;; MCP (Model Context Protocol) test seams and the disabled-tool-name mechanism.
;;; These are optional function-pointer parameters that tests substitute with
;;; fakes; production code resolves the real implementations in src/mcp/mcp.lisp.

(defparameter *read-mcp-config-function* nil
  "Optional test seam for reading MCP configuration.")

(defparameter *start-mcp-server-function* nil
  "Optional test seam for launching an MCP server.")

(defparameter *stop-mcp-server-function* nil
  "Optional test seam for stopping an MCP server.")

(defparameter *mcp-send-request-function* nil
  "Optional test seam for sending an MCP JSON-RPC request.")

(defparameter *mcp-debug-p* nil
  "Global flag controlling whether verbose MCP JSON-RPC and lifecycle debug messages are logged.")

(defparameter *mcp-initialize-function* nil
  "Optional test seam for performing the MCP initialize handshake.")

(defparameter *mcp-call-tool-function* nil
  "Optional test seam for invoking an MCP tool call.")

(defparameter *initialize-mcp-servers-for-chatbot-function* nil
  "Optional test seam for startup MCP initialization orchestration.")

(defparameter *get-all-mcp-tools-function* nil
  "Optional test seam for enumerating all MCP tools for a chatbot.")

(defparameter *find-mcp-server-and-tool-function* nil
  "Optional test seam for resolving an MCP tool by name.")

(defparameter *execute-mcp-tool-function* nil
  "Optional test seam for executing an MCP tool and returning text content.")

(defparameter *disabled-mcp-tool-names* '("read_graph")
  "Names of remote MCP tools that are hidden from the model and refused at execution time.
read_graph (from the memory MCP server) dumps a persona's entire knowledge graph and was
found to dramatically bloat conversation context; use the built-in queryMemory,
search_nodes, or open_nodes tools instead for scoped lookups.")

(defun mcp-tool-name-disabled-p (tool-name)
  "Returns true when TOOL-NAME is in *DISABLED-MCP-TOOL-NAMES* (case-insensitive)."
  (and tool-name
       (member tool-name *disabled-mcp-tool-names* :test #'string-equal)
       t))
