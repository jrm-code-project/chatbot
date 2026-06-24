# Technical Debt

This file records the highest-signal technical debt items found during a repo walk on 2026-06-23.

1. **File attachment MIME and pathname handling is ad hoc**
   - **Why this is debt:** Pathname expansion, content-type policy, and attachment encoding now live in separate modules, attachment MIME/textual/provider-kind decisions are centralized behind one helper, grouped alias rules reduce duplicate table entries, textual MIME fallback derives from those same grouped rules, and malformed rule entries now fail fast. The remaining debt is that the policy still depends on a manually curated extension/rule list, so breadth can drift as new file types and edge cases appear.
   - **Evidence:** `attachment-paths.lisp:1-51`, `attachment-mime.lisp:6-153`, `attachments.lisp:48-64`
   - **Remediation direction:** Keep using the centralized helper and grouped rule expansion, but replace or back the manual rule list with a broader shared lookup strategy and add new-type coverage as attachment formats expand.

2. **Backend turn orchestration is duplicated across providers**
   - **Why this is debt:** OpenAI, Google, and Gemini each still implement their own turn loop, callback dispatch, and provider-specific recursion flow. Stateless history updates, OpenAI/Google stateless tool-recursion continuation, direct tool lookup/execute, JSON tool-argument execution, and the ordered OpenAI/Gemini JSON tool-call mapping path are now shared, but the larger request/response orchestration still lives separately in each backend and can drift.
   - **Evidence:** `backend-openai.lisp:6-136`, `backend-google.lisp:13-117`, `backend-gemini.lisp:10-117`, `request-history.lisp:13-33`, `mcp.lisp:890-920`
   - **Remediation direction:** Extract a shared turn runner / tool recursion layer and keep backend files focused on request/response translation.

3. **Gemini compatibility fallback still exists as a code path**
   - **Why this is debt:** The Interactions-to-`generateContent` fallback is now explicit and opt-in, which is safer, but the compatibility branch still exists and keeps two Gemini-family execution paths alive in the same backend.
   - **Evidence:** `data.lisp:93-102`, `conversations.lisp:30-57`, `conversations.lisp:81-110`, `backend-gemini.lisp:6-95`
   - **Remediation direction:** Remove the compatibility path entirely once Interactions support is considered stable for supported deployments.

4. **`mcp.lisp` is an oversized integration hub**
   - **Why this is debt:** The MCP layer combines process lifecycle, protocol request tracking, response parsing, startup status modeling, config resolution, and tool cache invalidation in one large file. That increases coupling between unrelated concerns.
   - **Evidence:** `mcp.lisp:17-167`, `mcp.lisp:169-220`, `mcp.lisp:249-420`
   - **Remediation direction:** Split the module into protocol, process management, startup/configuration, and tool registry concerns.

5. **Tests are tightly coupled to internal payload structure**
   - **Why this is debt:** Many tests assert exact nested alist/vector shapes and literal JSON payload fragments. That gives good coverage, but it also makes refactors expensive because internal representation changes can break large numbers of tests without changing behavior.
   - **Evidence:** `tests-openai.lisp:16-63`, `tests-google.lisp:6-37`, `tests-payloads.lisp:6-43`, `tests-payloads.lisp:93-129`
   - **Remediation direction:** Add higher-level contract helpers for payload matching and reserve exact-structure assertions for a smaller set of translation-specific tests.

6. **Scratch scripts live in the repo root outside the ASDF systems**
   - **Why this is debt:** Files like `test-boolean.lisp`, `test-boolean-s.lisp`, and `test-boolean-alist.lisp` appear to be one-off experiments. They are not part of the declared runtime or test systems, which makes ownership and long-term intent unclear.
   - **Evidence:** `chatbot.asd:21-58`, `test-boolean.lisp:1-6`, `test-boolean-s.lisp:1-8`, `test-boolean-alist.lisp:1-6`
   - **Remediation direction:** Either delete these scripts, move them under a clearly named `scratch/` or `dev/` area, or convert the useful cases into real FiveAM tests.
