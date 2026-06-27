# Technical Debt

This file records the highest-signal technical debt items found during a repo walk on 2026-06-27.

1. **Runtime-context migration is still paying a heavy compatibility tax**
   - **Why this is debt:** The codebase now has a first-class `runtime-context`, but the implementation still mirrors a large set of compatibility globals, syncs values in both directions, and rebinds them through `progv`. That keeps old entry points working, but it also means every new runtime setting has to be threaded through a dual-path API and reasoned about in both ambient-global and explicit-context modes.
   - **Evidence:** `src/core/vars.lisp:156-186`, `src/core/vars.lisp:244-322`, `src/core/vars.lisp:394-705`, `src/core/package.lisp:56-72`
   - **Remediation direction:** Continue migrating public entry points to explicit runtime contexts, then prune the mirrored-global bridge and collapse the legacy sync helpers once the compatibility surface is small enough.
