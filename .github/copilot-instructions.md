# Copilot instructions for `chatbot`

## Build and test commands

This repository is a Common Lisp ASDF system named `chatbot`. Quicklisp and SBCL are assumed.

| Task | Command |
| --- | --- |
| Load the system | `sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:load-system :chatbot)'` |
| Load the test system | `sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:load-system "chatbot/tests")'` |
| Run the full test suite | `sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:test-system :chatbot)'` |
| Run the full test suite via wrapper script | `sbcl --non-interactive --load run-tests.lisp` |
| Run one test | `sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:load-system "chatbot/tests")' --eval '(fiveam:run (quote chatbot::test-default-conversation))'` |

Single tests are FiveAM test symbols loaded by the `chatbot/tests` ASDF system; replace `chatbot::test-default-conversation` with the specific test name you need.

## High-level architecture

- `chatbot.asd` now defines two systems: runtime code lives in `chatbot`, and FiveAM files live in `chatbot/tests`. Keep runtime files out of the test system, keep test files out of the runtime system, and declare explicit `:depends-on` edges in `chatbot.asd`.
- Runtime source files now live under `src/` by concern, and FiveAM files live under `tests/`.
- `src/core/data.lisp` defines the two central objects: `chatbot` holds backend/model/tool configuration, and `conversation` holds the runtime state for a session.
- `src/core/vars.lisp` is the shared runtime configuration layer: backend base URLs, API key resolution, logging controls, timeout defaults, the MCP config override, the eager-start compatibility flag, and the shared `*default-conversation*` / `*startup-chatbot*` globals all live there.
- Shared helpers are split by concern instead of living in one utility file: `src/utils/json-utils.lisp` handles JSON/schema/plist helpers, `src/utils/logging.lisp` handles logging and token summaries, `src/utils/http-utils.lisp` wraps outbound HTTP, `src/utils/text-utils.lisp` owns SSE parsing and text formatting, and the attachment pipeline is split across `src/attachments/attachment-paths.lisp`, `src/attachments/attachment-mime.lisp`, and `src/attachments/attachments.lisp`.
- Runtime flow is also split by concern: `src/attachments/attachment-paths.lisp` owns transient file pathname expansion and deduplication, `src/attachments/attachment-mime.lisp` owns MIME/content-typing policy, `src/attachments/attachments.lisp` owns binary reading plus attachment encoding, `src/prompts/prompt-decoration.lisp` owns transient timestamp/model prompt prefixes, `src/prompts/request-history.lisp` owns persona preload plus shared stateless history shaping, `src/payloads/openai-payloads.lisp` owns OpenAI-compatible request translation, `src/payloads/google-payloads.lisp` owns Google `generateContent` translation, `src/payloads/gemini-payloads.lisp` owns Gemini Interactions translation, `src/payloads/payloads.lisp` now holds only shared payload helpers, `src/personas/personas.lisp` handles persona loading, `src/core/conversations.lisp` defines `new-chat` / `new-chat-persona`, each backend has its own file under `src/backends/`, and `src/core/chat.lisp` is the dispatch entry point.
- Backend state is intentionally different by provider:
  - Gemini Interactions uses `conversation-interaction-id` for multi-turn state and streams SSE events.
  - OpenAI and LM Studio rebuild history from `conversation-messages` every turn and stream chat-completions deltas.
  - Google `generateContent` is non-streaming and also relies on explicit `conversation-messages`.
- Gemini's legacy 404 fallback to Google `generateContent` is now an explicit compatibility flag on the chatbot (`:gemini-fallback-to-google-p`, default `nil`) rather than an unconditional silent switch.
- Tool-calling is recursive in all backends: the model response is parsed for tool requests, the matching MCP tool is executed, the tool result is converted back into the provider-specific message shape, and the backend function calls itself again to continue the turn.
- `src/mcp/mcp.lisp` owns Model Context Protocol integration. Shared MCP startup is now explicit: call `chatbot:initialize-startup-chatbot` when you want configured servers started and cached on `*startup-chatbot*`. `new-chat` reuses that shared server list if it already exists, but a plain system load no longer starts external MCP processes unless the compatibility flag `CHATBOT_EAGER_MCP_STARTUP=1` (or `chatbot:*auto-initialize-startup-mcp-servers-p*`) is enabled.
- MCP server definitions come from an s-expression config file, not JSON. `src/mcp/mcp.lisp` supports both plist-style server definitions and a custom nested-list form, launches each server as a subprocess, performs the initialize handshake, caches `tools/list`, and invalidates that cache when the server reports tool-list changes.
- Persona preload is implemented as synthetic conversation history, not a separate memory store. `new-chat-persona` loads `config.lisp`, `system-instruction.md` / `system-instructions.md`, optional `compressed-memory.txt` or `memory.json`, and any sorted diary files from `CompressedDiary/` when present or `Diary/` otherwise, then seeds the conversation with synthetic preload history before the first live turn. When `memory.json` is present, persona startup may also attach a persona-scoped `memory` MCP server so its knowledge-graph tools operate on that persona file. Persona config may also enable built-in filesystem tools with `:enable-filesystem-tools t`, may enable built-in web grounding tools with `:enable-web-tools t` (`webSearch` via `GOOGLE:web-search` and `hyperspecSearch` via `GOOGLE:hyperspec-search`), may enable the built-in `eval` tool with `:enable-eval t`, and may enable prompt timestamping with `:include-timestamp t` and prompt model labels with `:include-model t`. The eval tool requires per-request user approval for the exact submitted expression and returns captured values/stdout/stderr. Timestamp and model prefixes are transient request-time prompt decoration, not stored conversation history. `chat` also accepts transient per-turn `:files` attachments plus a convenience `:file` alias for one pathname: files, directories, and wildcards are expanded into a deduplicated file list, Gemini/Google receive MIME blobs when possible, OpenAI-compatible requests fall back to text parts (or image data URLs for images), and the attachments are not saved in `conversation-messages`. The persona root is always allowed, and additional approved directories are persisted in a separate persona-owned `filesystem-allowlist.lisp` file.

## Key conventions

- Every source file lives in `(in-package "CHATBOT")`.
- `src/core/package.lisp` shadowing-imports `DEFUN`, `FUNCALL`, `LET*`, `MULTIPLE-VALUE-BIND`, `LET`, `NAMED-LAMBDA`, and `COMPOSE` from helper libraries. Inside this package, do not assume you are using the plain `CL` variants of those names.
- When seeding or inspecting stored conversation history, assistant-side preload messages should use the `"model"` role. The backend adapters normalize roles for OpenAI and Google when building requests.
- Route outbound HTTP calls through `post-web-request` in `src/utils/http-utils.lisp` so logging, secret redaction, and the repository timeout defaults (`15s` connect, `120s` read) stay consistent.
- MCP tool exposure is backend-specific. Reuse the existing translation helpers in `src/mcp/mcp.lisp` plus the schema/payload helpers in `src/utils/json-utils.lisp` / `src/payloads/payloads.lisp` (`translate-mcp-tool-to-openai`, `translate-mcp-tool-to-gemini-fn`, `gemini-tool-parameters`, `make-interaction-payload`) instead of hand-building tool payloads in new code.
