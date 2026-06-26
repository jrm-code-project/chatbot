# Chatbot Framework - GEMINI Instructions & Context

This file serves as a reference for the development, architecture, environment setup, and coding conventions of the Chatbot framework codebase.

---

## 1. Project Overview

The **Chatbot** project is a Common Lisp framework designed for building conversational agents. It aims to provide flexible abstractions for managing conversational state, integrating with external APIs/services, and defining rich agent behaviors.

### Technology Stack
- **Language:** Common Lisp (compatible with Steel Bank Common Lisp - SBCL)
- **Build System:** ASDF (Another System Definition Facility)
- **Primary Package / Scope:** `CHATBOT`
- **Key Dependencies:**
  - `alexandria`: General-purpose utilities.
  - `cl-json` & `jsonx`: JSON parsing, serialization, and extended JSON manipulation.
  - `cl-ppcre`: Regular expressions.
  - `dexador`: HTTP client for web integration.
  - `fold`, `function`, `named-let`, `series`: Functional programming paradigms, generator-like series operations, and recursive naming.
  - `trivial-timeout`: Safe timeout wrappers.
  - `fiveam`: Unit testing framework (build/test-time dependency).

### System & Architecture
The system definition is managed in `chatbot.asd` under the systems `"chatbot"` and `"chatbot/tests"`. Runtime code is organized under `src/` by concern:

- **`src/core/`**: `package.lisp`, `data.lisp`, `vars.lisp`, `conversations.lisp`, and `chat.lisp`.
- **`src/utils/`**: shared helpers such as `json-utils.lisp`, `logging.lisp`, `http-utils.lisp`, and `text-utils.lisp`.
- **`src/attachments/`**: attachment path expansion, MIME policy, and attachment encoding.
- **`src/prompts/`**: prompt decoration and request-history shaping.
- **`src/payloads/`**: shared payload helpers plus provider-specific payload builders.
- **`src/personas/`**: persona loading and persona startup helpers.
- **`src/mcp/`**: Model Context Protocol support.
- **`src/backends/`**: Gemini, Google, and OpenAI-compatible backend dispatch.
- **`src/orchestration/`**: round-robin chat and sandbox persona orchestration.
- **`src/tools/`**: convenience helpers like screenshot and news tools.

FiveAM regression tests live under **`tests/`** and are loaded by the `chatbot/tests` ASDF system.

---

## 2. Building and Running

### Prerequisites
- **Lisp Implementation:** SBCL (v2.6.5 or later recommended).
- **Quicklisp:** Required to resolve external dependencies.

### Environment Setup
To load the Chatbot system locally, register its directory path with ASDF:

1. **Option A: ASDF Central Registry (Ad-hoc)**
   Load your Lisp REPL and push the project's root directory to `asdf:*central-registry*`:
   ```lisp
   (push #p"d:/repositories/Chatbot/" asdf:*central-registry*)
   ```

2. **Option B: Quicklisp Local Projects (Persistent)**
   Create a symbolic link or move the project folder directly into your Quicklisp local-projects directory:
   ```powershell
   # Windows PowerShell example
   New-Item -ItemType SymbolicLink -Path "$Home\quicklisp\local-projects\Chatbot" -Value "d:\repositories\Chatbot"
   ```

### Loading the Project
With the registry configured, load the project in SBCL:
```lisp
;; Start SBCL and load the Chatbot system using Quicklisp
(ql:quickload :chatbot)
```

### Running Tests
The system uses the **FiveAM** library for regression and unit testing. To run the full suite:
```lisp
(asdf:test-system :chatbot)
```

---

## 3. API Entry Points

The framework exports two primary functions/symbols from the `CHATBOT` package to drive conversation flows:

1. **`NEW-CHAT`**: Initialize a new conversation or session with an instance of a chatbot.
2. **`CHAT`**: Send a message / turn to an active conversation and retrieve the response.

---

## 4. Development Conventions

To maintain consistency and avoid common Common Lisp pitfalls, adhere to the following development conventions:

### File Headers & Package Declarations
- Source files should specify the package context at the top of the file:
  ```lisp
  (in-package "CHATBOT")
  ```
- File encodings or mode markers (e.g., `;;; -*- Lisp -*-`) are placed at the very top of infrastructure files (like `src/core/package.lisp`).

### Architectural Practices & Style
1. **Shadowing Imports:**
   Be aware of package shadowing in `src/core/package.lisp`. Several standard CL macros/functions or external utility names (like `COMPOSE`, `LET`, `LET*`, `FUNCALL`, `DEFUN`, `MULTIPLE-VALUE-BIND`) are imported from helper libraries (`SERIES`, `NAMED-LET`, `FUNCTION`) rather than the standard `CL` package. Always use these shadowed forms when working within the `"CHATBOT"` package.
2. **CLOS Classes:**
   - Define custom structs/classes inside `src/core/data.lisp` or domain-specific files.
   - Utilize standard slot specifications (`:initarg`, `:accessor`, `:initform`) to ensure type-safe and clean object management.
3. **Adding New Files:**
   - When introducing a new source file, place it in the appropriate `src/` subdirectory and declare it under the `:components` list inside `chatbot.asd`.
   - Ensure you declare dependencies using `:depends-on ("package" ...)` to allow ASDF to resolve compilation orders correctly.
