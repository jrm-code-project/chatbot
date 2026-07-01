# Technical Debt Register

This document prioritizes the highest-value technical debt currently visible in the `chatbot` codebase. It focuses on debt that increases change risk, operational fragility, or maintenance cost.

## Priority scale

- **P0** — high-risk architectural or operational debt that can cause broad regressions
- **P1** — important maintainability debt that slows feature work or makes bugs likely
- **P2** — worthwhile cleanup that should follow once higher-risk items are under control

## Prioritized debt

| Priority | Area | Debt | Evidence | Why it matters | Recommended direction |
| --- | --- | --- | --- | --- | --- |
| **P0** | Runtime context and configuration | **Legacy ambient globals still coexist with the newer runtime-context model.** | `src/core/vars.lisp` contains the deprecated compatibility globals, sync bridges, helper-generating macros, and `call-with-runtime-context`; see `sync-runtime-context-from-legacy-globals`, `sync-legacy-globals-from-runtime-context`, `mirrored-runtime-context-value`, `set-mirrored-runtime-context-value`, and `call-with-runtime-context`. | This keeps behavior dependent on ambient mutable state and compatibility mirroring, which makes bugs non-local and complicates concurrent or nested execution. The main loop is cleaner than before, but the compatibility layer is still large and central. | Finish the runtime-context migration: pick a date to stop adding new legacy-global readers, reduce bridge entry points, then remove or isolate the deprecated aliases behind one compatibility module. |
| **P0** | MCP lifecycle | **Subprocess and thread lifecycle management is still abrupt and fragile.** | `src/mcp/mcp-lifecycle.lisp` starts reader and stderr threads in `default-start-mcp-server` and stops them in `default-stop-mcp-server` with `terminate-thread`, stream closes, and `terminate-process`. `src/mcp/mcp-startup.lisp` mixes config parsing, startup, error handling, shared startup state, and shutdown. | This is the highest operational-risk code in the repo: process teardown races, blocked I/O, and partial startup failures are hard to reproduce and easy to break. It also couples startup policy to lifecycle mechanics. | Split lifecycle into smaller layers: command/environment resolution, process launch, reader supervision, startup orchestration, and shutdown policy. Replace abrupt termination paths with more explicit shutdown handshakes where possible. |
| **P1** | Chat orchestration | **Top-level chat flow still couples dispatch, context routing, pruning, persistence, and checkpointing.** | `src/core/chat.lisp`: `dispatch-chat-turn`, `chat-turn`, and `chat`. `chat` still owns active-conversation binding, result application, default naming, and `save-minion-state`. | The pure-turn refactor improved this area, but the shell still contains several responsibilities. Any future change to persistence, routing, or provider dispatch still touches a high-traffic path. | Continue the shell/core split: extract checkpoint naming/persistence policy and planner-routing policy out of `chat`, so `chat` becomes only orchestration plus effect application. |
| **P1** | Provider backends | **Provider turn submission functions remain long, stateful, and mixed-responsibility.** | `src/backends/backend-google.lisp`: `submit-google-turn`; `src/backends/backend-openai.lisp`: `submit-openai-turn`; similarly complex Gemini flow in `src/backends/backend-gemini.lisp`. | Streaming parsing, payload building, retry policy, tool-call detection, usage extraction, and final-outcome normalization all live in a few functions. That raises regression risk for tool-calling and provider edge cases. | Extract provider-specific phases into smaller helpers: request assembly, response parsing, tool-call extraction, retry decision, and outcome normalization. Keep `run-provider-turn-loop` as the shared orchestration backbone. |
| **P1** | Tool execution and orchestration | **The MCP tool execution layer is still too large and central.** | `src/mcp/tool-execution.lisp` spans builtin argument normalization, filesystem policy, planner/minion orchestration, and the main `execute-chatbot-tool` dispatch. | This file has become the coordination hub for many unrelated concerns. That makes every new tool or orchestration change riskier and harder to review. | Split by concern: builtin validation/coercion, filesystem tools, persona/system-instruction tools, planner/minion tools, and shared execution plumbing. |
| **P1** | Conversation restoration and personas | **Conversation construction still carries a lot of policy and side effects.** | `src/core/conversations.lisp`: `new-chat-persona`, `restore-minions`, `summarize-old-history`, `prune-conversation-context-if-needed`. `src/personas/personas.lisp` still mixes pure shaping with background compression and MCP-server attachment flow. | Recent refactors made these paths more functional, but persona loading and minion restore still combine configuration, state shaping, startup side effects, and persistence behavior. | Keep peeling apart planning from effects: separate persona config resolution, preload shaping, restore planning, and side-effect application into distinct helpers/modules. |
| **P2** | Duplicate code trees | **The repository contains a mirrored sandbox code tree that can drift from `src/`.** | `minion-sandbox-Gopher\` contains 24 tracked files, while `src\` contains 39 tracked files. The mirrored tree duplicates major runtime modules such as `core\chat.lisp`, `mcp\tool-execution.lisp`, and backend files. | Near-copies multiply maintenance cost and create a real risk of fixing behavior in one tree but not the other. | Either generate the sandbox tree from a single source, replace it with fixtures/test artifacts, or delete it if it is no longer required. |
| **P2** | Runtime artifacts in the repository | **Persisted minion state files are tracked in git.** | `data/minions\` contains many tracked JSON state files such as `Planner.json`, `Gopher.json`, `Documentation.json`, and others. `save-minion-state` in `src/core/conversations.lisp` writes into this directory. | Checked-in runtime state creates noise, invites accidental coupling between code and local state, and can confuse tests and reviews. | Move persistent runtime artifacts out of the tracked repository tree or formalize them as fixtures in a separate test-data area with clear ownership. |
| **P2** | Test coverage shape | **The highest-risk lifecycle and concurrency paths appear less directly exercised than core request/response behavior.** | The suite is broad (`tests/tests-*.lisp`), but the riskiest areas are teardown races, background thread/process cleanup, and shared-state interaction across runtime contexts. | The code most likely to fail in production is not the easiest to validate with ordinary unit tests. Without stronger lifecycle tests, regressions may only appear under real runtime conditions. | Add focused tests around MCP shutdown, thread cleanup, and recovery behavior, ideally with deterministic seams already present in `vars.lisp`. |

## Recently improved, but not finished

These areas should not be treated as fresh debt, but as **partially retired debt** that still has follow-through work left:

1. **Pure chat loop migration**
   - The normalized turn-result flow and thinner `chat` shell were a major improvement.
   - Remaining debt: compatibility routing, checkpoint persistence, and planner routing still sit close to the main entry path.

2. **Agentic loop and minion functional cleanup**
   - Loop-step state and minion restoration are clearer than before.
   - Remaining debt: orchestration still depends on global registries, threads, and runtime-context bridging.

3. **Reader vs accessor cleanup**
   - Immutable CLOS slots were tightened in several classes.
   - Remaining debt: large classes and lifecycle-heavy modules still expose broad mutable surfaces overall.

4. **MCP startup/config separation**
   - MCP configuration discovery, parsing, and built-in definition lookup now live in `src/mcp/mcp-config.lisp`, and `src/mcp/mcp-startup.lisp` is narrower than before.
   - Remaining debt: process supervision, shutdown behavior, and startup-policy coupling still need further lifecycle decomposition.

5. **Tool-execution filesystem split**
   - Built-in tool argument normalization now lives in `src/mcp/tool-arguments.lisp`, and filesystem tool helpers now live in `src/mcp/filesystem-tools.lisp`.
   - Remaining debt: system-instruction tools, eval/grounding helpers, and planner/minion orchestration still keep `src/mcp/tool-execution.lisp` broader than ideal.

## Suggested fix order

1. **Retire the legacy-global/runtime-context bridge** enough that new work no longer depends on it.
2. **Refactor MCP lifecycle and startup/shutdown mechanics** into smaller, testable layers.
3. **Break up provider submission functions** and the large `tool-execution` hub.
4. **Remove duplicate runtime trees and tracked state artifacts** so the repository shape becomes easier to trust.
5. **Deepen lifecycle and concurrency tests** after the high-risk modules are decomposed.
