# Technical Debt

This file records the highest-signal technical debt items found during a repo walk on 2026-06-25.

1. **`src/mcp/mcp.lisp` is still an oversized integration hub**
   - **Why this is debt:** The MCP layer still mixes transport/process lifecycle, request correlation, protocol parsing, startup orchestration, built-in tool definitions, tool discovery, and shutdown behavior in one file. That makes unrelated changes collide and keeps the MCP surface difficult to reason about.
   - **Evidence:** `src/mcp/mcp.lisp:17-67`, `src/mcp/mcp.lisp:714-780`, `src/mcp/mcp.lisp:801-980`
   - **Remediation direction:** Split the file into protocol/client transport, startup/configuration, built-in tool catalog, and tool execution/registry modules.

2. **Provider turn orchestration is duplicated across OpenAI, Google, and Gemini**
   - **Why this is debt:** Each backend still owns its own request loop, callback emission, tool-call accumulation, retry handling, and recursion continuation. The shared helpers reduce some duplication, but the main control flow can still drift provider-by-provider.
   - **Evidence:** `src/backends/backend-openai.lisp:6-163`, `src/backends/backend-google.lisp:43-213`, `src/backends/backend-gemini.lisp:25-189`
   - **Remediation direction:** Extract a shared turn runner that centralizes recursion, callback delivery, and final history updates while leaving payload translation provider-specific.

3. **The runtime-context migration still carries a large legacy-global compatibility layer**
   - **Why this is debt:** `runtime-context` is the preferred API, but many compatibility globals remain alive and are mirrored into and out of the default context. The repeated getter/setter bridge code increases surface area, duplicates state, and makes migration bugs more likely.
   - **Evidence:** `src/core/vars.lisp:141-225`, `src/core/vars.lisp:354-433`, `src/core/vars.lisp:530-780`
   - **Remediation direction:** Finish the migration to explicit runtime contexts, collapse the duplicated bridge accessors, and leave a thinner compatibility shim at the outer API boundary.

4. **Round-robin cloning manually copies chatbot slots and will drift as the model grows**
   - **Why this is debt:** `clone-chatbot-for-round-robin` reconstructs a `chatbot` instance by enumerating many slots by hand. Every new chatbot field now requires remembering to update clone logic, which is easy to miss and hard to notice until a feature behaves differently inside round-robin sessions.
   - **Evidence:** `src/core/data.lisp:82-187`, `src/orchestration/round-robin.lisp:49-82`
   - **Remediation direction:** Introduce a dedicated copy/clone constructor for `chatbot` and `conversation`, or define a smaller immutable configuration object that round-robin can reuse safely.

5. **Sandbox persona orchestration still depends on process-global mutable state**
   - **Why this is debt:** Active personas live in a global hash table guarded by a global mutex, and tests repeatedly have to clear that shared registry to stay isolated. That works for REPL workflows, but it couples unrelated callers and makes concurrent or embedded use harder.
   - **Evidence:** `src/orchestration/sandbox-personas.lisp:6-12`, `src/orchestration/sandbox-personas.lisp:98-123`, `src/orchestration/sandbox-personas.lisp:218-231`, `tests/tests-sandbox.lisp:22-45`, `tests/tests-sandbox.lisp:179-191`
   - **Remediation direction:** Move sandbox persona state behind an explicit registry/session object and keep the current globals as a convenience wrapper rather than the primary storage model.

6. **Tests are still tightly coupled to internal payload shape and literal JSON layout**
   - **Why this is debt:** Many tests assert exact nested alists, vectors, and serialized JSON fragments. That gives strong translation coverage, but it makes benign internal refactors expensive because representation changes can break large numbers of tests without changing observable behavior.
   - **Evidence:** `tests/tests-openai.lisp:16-63`, `tests/tests-google.lisp:6-38`, `tests/tests-payloads.lisp:6-78`, `tests/tests-payloads.lisp:133-211`
   - **Remediation direction:** Add higher-level payload assertion helpers and reserve exact structure checks for a smaller set of translation-specific golden tests.

7. **Repository hygiene is noisy: scratch files, backup files, and plan docs live at the repo root**
   - **Why this is debt:** One-off investigation scripts and editor backup files live beside real source and test entry points. That makes ownership less clear, raises the odds of stale artifacts being mistaken for supported code, and adds cognitive overhead for contributors.
   - **Evidence:** `chatbot.asd:21-73`, `test-boolean.lisp:1-6`, `test-boolean-s.lisp:1-8`, `test-boolean-alist.lisp:1-6`, `chatbot.asd~`, `attachment-mime.lisp~`, `mcp.lisp~`, `git-backup-plan.md`, `mcp-integration-plan.md`
   - **Remediation direction:** Move intentional scratch artifacts under a clearly named `dev/` or `scratch/` area, delete stale backups, and keep root-level docs limited to maintained project documentation.
