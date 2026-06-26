# Technical Debt

This file records the highest-signal technical debt items found during a repo walk on 2026-06-26.

1. **`src/mcp/mcp.lisp` is still an oversized integration hub**
   - **Why this is debt:** The built-in tool catalog and tool-registry helpers have now been extracted to `src/mcp/builtin-tools.lisp` and `src/mcp/tool-registry.lisp`, but `mcp.lisp` still mixes transport/process lifecycle, config parsing, startup orchestration, tool-call wrappers, built-in argument normalization, and built-in tool execution in one 1,500+ line file. It is trending in the right direction, but still owns too many unrelated MCP concerns.
   - **Evidence:** `src/mcp/mcp.lisp:17-67`, `src/mcp/mcp.lisp:551-760`, `src/mcp/mcp.lisp:763-836`, `src/mcp/mcp.lisp:843-1564`, `src/mcp/builtin-tools.lisp:1-166`, `src/mcp/tool-registry.lisp:1-134`
   - **Remediation direction:** Continue the split by carving out protocol/client transport, startup/configuration, and built-in tool execution/validation modules so `mcp.lisp` stops being the catch-all for every MCP concern.

2. **Provider turn orchestration still duplicates the request and tool loop across OpenAI, Google, and Gemini**
   - **Why this is debt:** Final text emission and stateless-history persistence are now shared, but the heavy control flow is still copied three ways: request construction, response parsing, tool-call capture, retry/fallback decisions, and recursive continuation all remain backend-specific. That means every behavioral fix still has to be re-threaded provider by provider.
   - **Evidence:** `src/backends/backend-openai.lisp:6-157`, `src/backends/backend-google.lisp:27-204`, `src/backends/backend-gemini.lisp:14-183`
   - **Remediation direction:** Build on the shared finalizers by extracting a shared turn runner that centralizes recursion, callback delivery, and success/failure finalization while leaving payload translation provider-specific.

3. **Tests are still tightly coupled to internal payload shape and literal JSON layout**
   - **Why this is debt:** Many tests still assert exact nested alists, vectors, and serialized JSON fragments rather than observable backend behavior. That gives strong translation coverage, but it also means benign internal representation changes trigger wide test churn even when external behavior is unchanged.
   - **Evidence:** `tests/tests-openai.lisp:16-63`, `tests/tests-google.lisp:6-38`, `tests/tests-payloads.lisp:6-78`, `tests/tests-mcp.lisp:43-73`
   - **Remediation direction:** Add higher-level payload assertion helpers and reserve exact structure checks for a smaller set of translation-specific golden tests.

4. **Repository hygiene is noisy: scratch files, backup files, and plan docs live at the repo root**
   - **Why this is debt:** One-off investigation scripts, editor backups, and plan documents still sit next to supported source and entry points. That makes root-level ownership less clear, increases the odds of stale artifacts being mistaken for maintained code, and adds cognitive overhead for every repo walk.
   - **Evidence:** `chatbot.asd:21-72`, `test-boolean.lisp`, `test-boolean-s.lisp`, `test-boolean-alist.lisp`, `chatbot.asd~`, `attachment-mime.lisp~`, `mcp.lisp~`, `git-backup-plan.md`, `mcp-integration-plan.md`
   - **Remediation direction:** Move intentional scratch artifacts under a clearly named `dev/` or `scratch/` area, delete stale backups, and keep root-level docs limited to maintained project documentation.
