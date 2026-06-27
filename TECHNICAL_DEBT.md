# Technical Debt

This file records the highest-signal technical debt items found during a repo walk on 2026-06-27.

1. **Runtime-context migration is still paying a heavy compatibility tax**
   - **Why this is debt:** The codebase now has a first-class `runtime-context`, but the implementation still mirrors a large set of compatibility globals, syncs values in both directions, and rebinds them through `progv`. That keeps old entry points working, but it also means every new runtime setting has to be threaded through a dual-path API and reasoned about in both ambient-global and explicit-context modes.
   - **Evidence:** `src/core/vars.lisp:156-186`, `src/core/vars.lisp:244-322`, `src/core/vars.lisp:394-705`, `src/core/package.lisp:56-72`
   - **Remediation direction:** Continue migrating public entry points to explicit runtime contexts, then prune the mirrored-global bridge and collapse the legacy sync helpers once the compatibility surface is small enough.

2. **Provider turn orchestration still duplicates the request/tool recursion loop across backends**
   - **Why this is debt:** Final response handling and some payload helpers are shared, but the high-risk control flow is still repeated provider by provider: request submission, response parsing, tool-call extraction, continuation, and backend-specific retry/fallback behavior all remain separate. That keeps provider code understandable in isolation, but it also means cross-cutting fixes still have to be reimplemented three times.
   - **Evidence:** `src/backends/backend-openai.lisp:6-157`, `src/backends/backend-google.lisp:27-204`, `src/backends/backend-gemini.lisp:14-183`
   - **Remediation direction:** Extract a shared turn runner that centralizes recursive tool continuation and completion/error finalization while leaving payload translation and provider-specific wire formats in the backend modules.
