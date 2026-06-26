# SHORT-TERM GOALS

These goals are ordered by urgency. When `IMMEDIATE-GOALS.md` becomes empty, select the highest-priority unfinished item here and break it into a new immediate-goal list.

Current decomposition target: **LONG-TERM GOAL 1** — add functionality that makes `chatbot` a superior chatbot suite.

1. [ ] Build a first-class evaluation harness for comparing backends, personas, tools, and prompts.
   - Add workflows and reporting that make it easy to measure answer quality, latency, tool success, and regressions across configurations.
2. [ ] Improve persona and configuration ergonomics so new chatbots are easy to create, inspect, and tune.
   - Reduce setup friction for persona files, backend/model selection, capability flags, and sampling controls.
3. [ ] Expand practical built-in tools and integrations that make the suite more useful out of the box.
   - Prioritize high-leverage capabilities such as better filesystem workflows, web/research helpers, evaluation tools, and richer automation seams.
4. [ ] Strengthen backend and MCP reliability so the suite behaves predictably under real usage.
   - Improve startup clarity, transport robustness, error surfacing, and recovery behavior across providers and tool servers.
5. [ ] Finish splitting `src/mcp/mcp.lisp` so startup/configuration, transport, and built-in execution stop living in one file.
6. [ ] Extract a shared provider turn runner so OpenAI, Google, and Gemini stop duplicating recursion and tool-loop control flow.
7. [ ] Add higher-level test helpers so backend and MCP tests assert behavior more often than literal payload layout.
8. [ ] Clean root-level scratch, backup, and planning artifacts into clearer maintained locations.
