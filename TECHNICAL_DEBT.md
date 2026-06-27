# Technical Debt

This file records the highest-signal technical debt items found during a repo walk on 2026-06-27.

1. **Runtime-context migration is still paying a heavy compatibility tax**
   - **Why this is debt:** The codebase now has a first-class `runtime-context`, but the implementation still mirrors a large set of compatibility globals, syncs values in both directions, and rebinds them through `progv`. That keeps old entry points working, but it also means every new runtime setting has to be threaded through a dual-path API and reasoned about in both ambient-global and explicit-context modes.
   - **Evidence:** `src/core/vars.lisp:156-186`, `src/core/vars.lisp:244-322`, `src/core/vars.lisp:394-705`, `src/core/package.lisp:56-72`
   - **Remediation direction:** Continue migrating public entry points to explicit runtime contexts, then prune the mirrored-global bridge and collapse the legacy sync helpers once the compatibility surface is small enough.

2. **Agentic-loop orchestration still mixes process-global state with forceful thread control**
   - **Why this is debt:** The new loop engine works, but loop registration is still process-global rather than owned by a runtime context, and emergency interruption still falls back to `sb-thread:terminate-thread`. That weakens isolation between independent runtimes and leaves the hardest cancellation path dependent on abrupt thread termination rather than purely cooperative shutdown.
   - **Evidence:** `src/orchestration/agentic-loops.lisp:204-227`, `src/orchestration/agentic-loops.lisp:318-364`, `src/orchestration/agentic-loops.lisp:522-563`
   - **Remediation direction:** Move loop registries under runtime-context ownership, add clearer lifecycle cleanup, and shrink the situations that require hard thread termination.

3. **MCP transport/process lifecycle still bypasses the newer logging and orchestration boundaries**
   - **Why this is debt:** MCP and MCP-startup diagnostics now write through the shared logging stream helper instead of open-coded `format t` calls, which reduced one of the biggest integration mismatches. The remaining debt is structural: `mcp.lisp` still combines transport, process supervision, request bookkeeping, and shutdown behavior, so the MCP layer is still more tightly coupled than the rest of the runtime architecture.
   - **Evidence:** `src/utils/logging.lisp:28-45`, `src/mcp/mcp.lisp:141-381`, `src/mcp/mcp-startup.lisp:241-322`
   - **Remediation direction:** Finish separating startup/configuration from transport/process supervision and continue reducing the responsibilities still concentrated in `mcp.lisp`.

4. **Provider turn orchestration still duplicates the request/tool recursion loop across backends**
   - **Why this is debt:** Final response handling and some payload helpers are shared, but the high-risk control flow is still repeated provider by provider: request submission, response parsing, tool-call extraction, continuation, and backend-specific retry/fallback behavior all remain separate. That keeps provider code understandable in isolation, but it also means cross-cutting fixes still have to be reimplemented three times.
   - **Evidence:** `src/backends/backend-openai.lisp:6-157`, `src/backends/backend-google.lisp:27-204`, `src/backends/backend-gemini.lisp:14-183`
   - **Remediation direction:** Extract a shared turn runner that centralizes recursive tool continuation and completion/error finalization while leaving payload translation and provider-specific wire formats in the backend modules.

5. **Tests remain tightly coupled to literal payload and JSON structure**
   - **Why this is debt:** Shared payload assertion helpers now also cover Google recursive tool-call request assertions, Google tool-sanitization request payloads, several OpenAI attachment, preload-history, and tool-error continuation payload tests, sandbox persona/arena request-history assertions, round-robin Google/Gemini history assertions, key Gemini runtime request/fallback payload assertions including no-arg built-in tool continuations, Google payload-builder tool declaration checks, and MCP request JSON-shape assertions, in addition to representative interaction-payload and sampling-parameter coverage. That reduces more of the direct string-fragment and nested `assoc`/shape assertions, but many tests still depend on exact serialized fragments and payload layout, so benign internal representation changes can still trigger disproportionate test churn.
   - **Evidence:** `tests/tests.lisp`, `tests/tests-openai.lisp`, `tests/tests-google.lisp`, `tests/tests-payloads.lisp`, `tests/tests-mcp.lisp`
   - **Remediation direction:** Continue migrating remaining payload-heavy tests to semantic helpers and reserve exact structural comparisons for a smaller set of translation-focused golden tests.
