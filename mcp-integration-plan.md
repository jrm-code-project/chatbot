# MCP Integration Plan

## Objective
Implement a Model Context Protocol (MCP) client within the Chatbot framework to allow LLMs to seamlessly access external tools and data sources. The client will communicate with MCP servers over JSON-RPC 2.0 via standard input/output.

## Key Files & Context
- **New File:** `infrastructure/mcp.lisp` (To be added to `chatbot.asd`)
- **Modified Files:** `infrastructure/data.lisp`, `infrastructure/macros.lisp`, `infrastructure/vars.lisp`, `chatbot.asd`

## Proposed Architecture
1. **Configuration Management:**
   - Configuration will be loaded from a cross-platform location using `uiop:xdg-config-home`. On Windows, this resolves to `~/AppData/Local/mcp/mcp.lisp`, and on Unix systems to `~/.config/mcp/mcp.lisp`.
   - The file will contain an s-expression defining servers, their execution commands, and arguments.
2. **Process Management & JSON-RPC (Asynchronous/Threaded):**
   - We will utilize SBCL native threads (`sb-thread`) to handle threading, avoiding the need for third-party threading dependencies.
   - For each configured MCP server, we will launch a subprocess using `uiop:launch-program` with `:input :stream` and `:output :stream`.
   - A dedicated reader thread will be spawned per server to parse incoming JSON-RPC responses asynchronously. This prevents blocking the main REPL and handles asynchronous notifications.
3. **Tool Injection & Execution Pipeline:**
   - The `chatbot` class will be extended to store a list of connected `mcp-server` objects.
   - `make-interaction-payload` (for Gemini/Google) and the equivalent OpenAI payload builders will dynamically query connected MCP servers (via the `tools/list` JSON-RPC method) and inject them into the LLM API requests.
   - The message processing loops for all supported backends (`chat-gemini`, `chat-google`, and `chat-openai`) will be updated to intercept `function_call` (or tool call) events. When intercepted, the framework will:
     - Route the call to the appropriate MCP server via the `tools/call` JSON-RPC method.
     - Wait for the threaded listener to resolve the result.
     - Send the result back to the respective LLM backend to continue the conversation.

## Implementation Steps
**Phase 1: Foundation & Process Management**
1. Update `chatbot.asd` to include the new `mcp.lisp` file.
2. Create `mcp.lisp` and define `mcp-server` CLOS classes.
3. Implement `read-mcp-config` to locate and parse the `mcp.lisp` configuration file.
4. Implement process spawning (`uiop:launch-program`) and the threaded JSON-RPC message listener using `cl-json` and `sb-thread` primitives.

**Phase 2: Protocol Handlers**
1. Implement synchronous wrapper functions that send JSON-RPC requests (e.g., `initialize`, `tools/list`, `tools/call`) and block via `sb-thread:make-mailbox` or other native SBCL synchronization primitives until the listener thread signals a response.
2. Translate standard MCP `tools/list` JSON schemas into the Gemini/Google API tool format and the OpenAI tool format.

**Phase 3: Core Framework Integration**
1. Add an `mcp-servers` slot to the `chatbot` class.
2. Modify payload generators in `misc.lisp` and `macros.lisp` to inject the translated MCP tools for all backends.
3. Update `chat-gemini`, `chat-google`, and `chat-openai` in `macros.lisp` to parse and handle function/tool calls triggered by the LLM, seamlessly wrapping the `tools/call` logic.

## Verification & Testing
- **Unit Tests:** Add tests for JSON-RPC parsing, config resolution (`get-mcp-config-path`), and tool payload translation.
- **Integration Test:** Create a mock script (e.g., `mock-mcp-server.py`) and verify that `uiop:launch-program` correctly binds streams, that `tools/list` is successfully parsed, and that a `tools/call` returns the expected value using native `sb-thread` interactions.

## Migration & Rollback
- Ensure backward compatibility for users without `mcp.lisp` or MCP configurations (the tool list will simply be empty).
- The threading components will be isolated within the MCP logic to prevent regressions in standard chatbot usage.
