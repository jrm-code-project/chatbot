# IMMEDIATE GOALS

Current focus: **Continue splitting `src/mcp/mcp.lisp`**, now that tool execution has moved out.

Completed in this round:

1. [x] Map the remaining responsibility clusters inside `src/mcp/mcp.lisp` and choose the next extraction seam.
2. [x] Extract one cohesive MCP concern into its own `src/mcp/` module and wire it into `chatbot.asd`.
3. [x] Update callers and shared helpers so behavior stays unchanged after the split.
4. [x] Add or adjust regression coverage for the extracted seam and rerun the existing test suite.
5. [x] Refresh `TECHNICAL_DEBT.md` so it reflects the new remaining MCP surface accurately.

Next immediate goals for the same short-term objective:

1. [ ] Extract MCP startup/configuration orchestration into its own `src/mcp/` module.
2. [ ] Rewire `new-chat`, shared startup helpers, and ASDF load order around that startup module.
3. [ ] Add focused regression coverage for startup status, strict-required failures, and shared-server reuse after the split.
4. [ ] Reassess whether the remaining `src/mcp/mcp.lisp` is small enough or needs one more transport-focused extraction.
