# Technical Debt Register

This document prioritizes the highest-value technical debt currently visible in the `chatbot` codebase. It focuses on debt that increases change risk, operational fragility, or maintenance cost.

This register was refreshed on **2026-07-31** after a full codebase scan, test-system execution, and the integration of the "Sticky Warmth" Protocol and surgical `editFile` tool.

## Priority scale

- **P0** — high-risk architectural or operational debt that can cause broad regressions
- **P1** — important maintainability debt that slows feature work or makes bugs likely
- **P2** — worthwhile cleanup that should follow once higher-risk items are under control

## Prioritized debt

| Priority | Area | Debt | Evidence | Why it matters | Recommended direction |
| --- | --- | --- | --- | --- | --- |
| **P1** | Tool Execution Layering | **Layering Violation in Tool Execution.** | `execute-chatbot-tool` intercepts specific tool calls to perform hardcoded side effects (ChromaDB sync) and even makes out-of-band LLM calls with hardcoded models/URLs. | Breaks separation of concerns, making tool execution hard to test and rigid. | Extract side-effects into specialized, decoupled event listeners or interceptors instead of hardcoding them in the core tool dispatcher. |
| **P1** | Duplicate code trees | **The repository contains a mirrored sandbox code tree that can drift from `src/`.** | `minion-sandbox-Gopher\` contains tracked copies of major runtime modules such as `core\chat.lisp`, `core\conversations.lisp`, `mcp\tool-execution.lisp`, and backend files. | Near-copies multiply maintenance cost, create confusion about active code paths, and raise the risk of fixing bugs in one tree while leaving them in the other. | Generate the sandbox tree from `src\` dynamically during tests, shrink it to true fixtures, or remove it entirely if it is no longer required. |
| **P1** | Chat orchestration | **Top-level chat flow is narrower, but dispatch/preparation and entry-shell routing still span multiple core seams.** | `src/core/chat.lisp` holds `dispatch-chat-turn` and `chat-turn`, but routing and entry context logic are scattered across `chat-routing.lisp` and `chat-entry.lisp`. | Any change to routing or provider dispatch still touches multiple central orchestration modules, raising regression risk for standard chat turns. | Continue the shell/core split: extract remaining compatibility-only concerns into smaller layers so public chat orchestration becomes mostly pure delegation. |
| **P1** | Provider backends | **Provider turn submission functions remain stateful and mixed-responsibility.** | Streaming parsing, payload building, retry policy, tool-call detection, and outcome normalization are tightly coupled within `backend-google.lisp`, `backend-openai.lisp`, and `backend-gemini.lisp`. | High-traffic provider code is difficult to refactor or test in isolation, increasing the risk of regressions for edge-case tool-calling or streaming scenarios. | Keep extracting provider-specific phases into smaller, pure helpers for request assembly, response parsing, tool-call extraction, and outcome normalization. |
| **P1** | Checkpoint restore semantics | **Checkpoint restore does not capture the full conversation-construction contract.** | `apply-restored-chatbot-state` and `apply-restored-conversation-state` perform post-restore patching, but not every constructor input or side-effect policy is explicitly modeled in the persisted schema. | Save/restore depends on implicit constructor behavior, which can easily drift as conversation and chatbot classes evolve. | Move toward an explicit restore contract: persist all constructor inputs and recovery policy decisions that materially affect runtime behavior. |
| **P1** | Persona startup and recovery policy | **`new-chat-persona` mixes normal persona construction with debugger-driven recovery and fallback policy.** | `src/personas/personas.lisp` and `src/core/conversations.lisp` interleave "create / redirect / skip restore" policy with core conversation setup. | Main conversation constructors decide both how personas are loaded and what to do when they cannot be loaded, making behavior dependent on interactive restart choices. | Split persona lookup/recovery planning from conversation construction. Keep `new-chat-persona` focused on the happy path, and move fallbacks to a dedicated resolution layer. |
| **P1** | Persona memory storage format | **Persona memory loading accumulates compatibility heuristics instead of enforcing one canonical schema.** | `personas.lisp` accepts graph JSON, JSONL records, embedded graph objects, and type-less records to support legacy persona files. | Every new edge case adds more implicit parsing rules to a critical startup path, increasing the risk of silent misclassifications or surprising migrations. | Define a versioned on-disk schema for persona memory, enforce a single canonical write format, and run one-time migrations for legacy files. |
| **P1** | Data Structures | **Ad-hoc Data Structures and JSON Handling.** | Pervasive use of plists and `assoc` for complex data instead of structured types. | Makes the codebase brittle, hard to type-check, and difficult to refactor safely. | Migrate from ad-hoc plists to explicit `defclass` or `defstruct` types for core data boundaries. |
| **P1** | Test structure | **Several test files are large enough to be hard to evolve safely.** | `tests/tests-runtime.lisp` (~121 KB), `tests/tests-mcp.lisp` (~116 KB), and `tests/tests-personas.lisp` (~90 KB). | Giant integration-heavy test files make it harder to find coverage gaps, isolate failures, or add narrowly scoped regression checks. | Split test files by behavior slice rather than subsystem umbrella (e.g., separate runtime-context, MCP lifecycle, and checkpoint restore tests). |
| **P2** | Utility I/O | **Inefficient Utility I/O.** | Some utilities, such as `read-file-forms-as-text`, perform redundant file operations (O(N^2) open/read behavior). | Slows down file processing, especially on larger files or slow disks. | Optimize file reading utilities to perform a single pass over the file stream. |
| **P2** | Runtime artifacts in the repository | **Legacy tracked minion state files still exist in git.** | `data/minions\` contains tracked JSON state files such as `Planner.json` and `Gopher.json`. | Runtime state artifacts in the repository confuse ownership boundaries and add unnecessary churn to the git workspace. | Remove or relocate the remaining tracked state files, or reclassify a minimal subset as intentional fixtures in a dedicated test-data area. |

## Recently Retired & Resolved Debt (July 2026)

The following high-value technical debt items have been fully resolved and retired through recent architectural improvements:

1. **Legacy Ambient Globals & Compatibility Seams (Resolved - July 2026)**
   * *The Debt*: Monolithic dynamic specials and ambient compatibility seams coexisted awkwardly with the newer, isolated `runtime-context` structures, leading to risk of state leakage, connection/socket pooling drift, and fragile test suites.
   * *The Fix*: Fully retired the compatibility bridge, deleted `runtime-compatibility.lisp`, removed all 16 deprecated ambient specials, and refactored all config getters/setters (`current-*`) into lightweight, direct `defmethod`/`defun` context accessors.

2. **'God Object' Data Models (Resolved - July 2026)**
   * *The Debt*: Monolithic `chatbot` and `conversation` classes housed 40+ slots each, cross-cutting multiple distinct concerns (identities, LLM, prompt, cache, tools, MCP, minions) and increasing modification friction and regression risk.
   * *The Fix*: Decomposed both massive classes into highly cohesive, dedicated component classes (e.g., `chatbot-identity`, `chatbot-llm-config`, `conversation-history`, etc.) managed via composition. Implemented complete backwards-compatible forwarding accessors and streamlined copy-constructor flatteners.

3. **"Sticky Warmth" Protocol (SWP) for Flash/Pro Failover (Resolved - July 2026)**
   * *The Debt*: Previously, the client performed single-turn failovers from Flash to Pro and immediately bounced back. This triggered Pro's expensive cold-start ingestion fee on every transient failure, while completely trashing the context cache on both models.
   * *The Fix*: Implemented the **Sticky Warmth Protocol (SWP)** state machine (`:flash-warm`, `:pro-sticky`, and `:transition`). Successful failovers now lock the session to Pro for consecutive turns, allowing us to leverage Pro's warm-cached context rate. Downgrades back to Flash are safely deferred until a short, low-risk prompt is processed.
   
4. **Identical History Preservation for Context Caching (Resolved - July 2026)**
   * *The Debt*: Providers sent decorated user messages (with timestamps and memories) to the API but saved only raw messages in the conversation history. This meant the previous turn's message in the prompt prefix altered on every turn, completely invalidating Gemini's automatic context caching.
   * *The Fix*: Restructured prompt decoration to put raw user input at the very beginning of the turn's prompt, appending all dynamic, transient parts (timestamps, memories) in a trailing suffix block. Backends now save fully decorated user messages to the history so that prompt prefixes remain 100% identical and cacheable across consecutive turns.

5. **High-Performance Surgical `editFile` Tool (Resolved - July 2026)**
   * *The Debt*: To make file modifications, we previously relied on reading, rewriting, and re-transmitting entire file contents. This was highly token-inefficient, slow, and prone to accidental deletions or formatting losses.
   * *The Fix*: Implemented the `editFile` built-in tool supporting both **Search & Replace Block Mode** (guaranteeing atomic, single-match correctness) and **Line-Range Mode** (surgically modifying specified lines while preserving line-ending formats). This tool enables precision, lightning-fast file updates with minimal token overhead.

6. **ChromaDB KG Sync Type Safety (Resolved - July 2026)**
   * *The Debt*: In `extract-observations-from-tool`, parsing JSON-decoded arrays of entities/observations assumed they would always be represented as vectors. When certain configurations decoded them as lists of alists, `loop ... across` raised a severe type error.
   * *The Fix*: Introduced the `normalize-to-list` utility function that safely coerces vectors, list-of-alists, single alists, single strings, and NIL into flat lists, allowing robust iteration via standard list loops.

7. **Lisp Compilation Warning Elimination (Resolved - July 2026)**
   * *The Debt*: Stale accessor call sites (`chatbot-content-cache-ttl`), deprecated configuration parameters (`:content-cache-ttl`), unused bindings in test suites, and timezone type parsing notes created compilation noise.
   * *The Fix*: Cleaned all stale call sites, aligned all tests to the `-seconds` API, and resolved all unused test bindings. The repository is now completely warning-light during routine load and test executions.

8. **MCP Lifecycle & Supervisor Context Manager (Resolved - July 2026)**
   * *The Debt*: Subprocess and thread lifecycle management was highly fragile; background threads (`Agentic-Loop-Worker-*` and server processes) required manual cleanup, and could leak into zombie states on abnormal exit.
   * *The Fix*: Introduced a robust `resource-supervisor` that tracks all spawned UIOP processes and SBCL threads on a given `runtime-context`. Connected `shutdown-chatbot` to forcefully sweep and terminate these resources, and introduced the `with-chatbot-lifecycle` macro wrapping standard chatbot operations in a guaranteed `unwind-protect` cleanup block.

9. **Checkpoint Identity Decoupling (Resolved - July 2026)**
   * *The Debt*: Checkpoint naming was implicitly coupled to incidental `chatbot-persona-name` metadata, causing separate concurrent instances of the same persona to collide, overwrite, and corrupt each other's `.json` files.
   * *The Fix*: Decoupled checkpoint identifiers, making checkpoint names an explicit, validated parameter supplied during construction, and added `:checkpoint-name` support to the `new-chat-persona` bootstrapper.

10. **Source Layout Hotspot File Splitting (Resolved - July 2026)**
    * *The Debt*: Core orchestration logic was concentrated in several massive "hotspot" files (`src/core/conversations.lisp` and `src/orchestration/agentic-loops.lisp`), which mixed multiple lifecycle phases and caused severe merge friction.
    * *The Fix*: Structural split of both monolithic files into 6 distinct, highly cohesive files (`conversation-constructors`, `conversation-persistence`, `conversation-compression`, `agentic-loop-state`, `agentic-loop-worker`, and `agentic-loop-monitor`) and aligned compilation topology in `chatbot.asd`.
