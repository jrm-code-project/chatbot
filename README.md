# Chatbot

`chatbot` is a Common Lisp framework for building chats with Gemini, Google, OpenAI-compatible backends, persona-backed conversations, and small multi-agent sandboxes.

The main public entry points are:

| Entry point | Use it for |
| --- | --- |
| `CHATBOT:NEW-CHAT` | Start a plain conversation with a selected backend/model |
| `CHATBOT:NEW-CHAT-PERSONA` | Start a conversation from a persona directory in `~/.Personas/` |
| `CHATBOT:CHAT` | Send one turn to a conversation |
| `CHATBOT:SPAWN-PERSONA` | Create a sandbox persona in the active registry |
| `CHATBOT:QUERY-ALL` | Ask several sandbox personas the same question independently |
| `CHATBOT:RUN-ARENA` | Run a debate/forum where personas answer each other in sequence |

## Requirements

- SBCL
- Quicklisp
- The repository available to ASDF

One way to load the system:

```lisp
(require :asdf)
(asdf:load-system :chatbot)
```

If you prefer Quicklisp:

```lisp
(ql:quickload :chatbot)
```

## Fast start: plain Gemini chat

`NEW-CHAT` defaults to the Gemini backend when you do not specify one.

```lisp
(let ((conversation (chatbot:new-chat)))
  (chatbot:chat "Hello." :conversation conversation))
```

If you want to pick a model explicitly:

```lisp
(let ((conversation (chatbot:new-chat :model "gemini-3.5-flash")))
  (chatbot:chat "Summarize the idea of a monad." :conversation conversation))
```

## Main conversation API

### `CHATBOT:NEW-CHAT`

Creates a fresh conversation object.

Common options:

- `:backend` - one of `:gemini`, `:google`, `:openai`, or `:lm-studio`
- `:model` - override the default model for that backend
- `:system-instruction` - set a custom system prompt
- `:temperature` / `:top-p` - set default sampling behavior

Example:

```lisp
(let ((conversation
        (chatbot:new-chat
         :backend :google
         :model "gemini-3.5-flash"
         :system-instruction "Be concise."
         :temperature 0.4d0)))
  (chatbot:chat "Explain tail recursion." :conversation conversation))
```

### `CHATBOT:CHAT`

Sends one turn to a conversation and returns the model's complete reply as a string.

```lisp
(let ((conversation (chatbot:new-chat)))
  (chatbot:chat "First question" :conversation conversation)
  (chatbot:chat "Second question" :conversation conversation))
```

Useful per-turn options:

- `:conversation` - which conversation to use
- `:callback` - receive streamed text chunks when the backend supports it
- `:file` / `:files` - attach files for the current turn only
- `:temperature` / `:top-p` - override sampling for just this turn

## Using `*DEFAULT-CONVERSATION*`

If you want `CHAT` to use a default conversation when `:conversation` is omitted:

```lisp
(let ((conversation (chatbot:new-chat)))
  (setf chatbot:*default-conversation* conversation)
  (chatbot:chat "Hello through the default conversation.")
  (chatbot:chat "And another turn."))
```

This is convenient at the REPL, but explicit `:conversation` arguments are usually clearer in larger programs.

## Persona-backed chats

### `CHATBOT:NEW-CHAT-PERSONA`

Loads a persona from `~/.Personas/<name>/`.

Typical persona files may include:

- `config.lisp`
- `system-instruction.md` or `system-instructions.md`
- `compressed-memory.txt` or `memory.json`
- diary files in `CompressedDiary/` or `Diary/`

Example:

```lisp
(let ((conversation (chatbot:new-chat-persona "Janus")))
  (chatbot:chat "What are you optimizing for today?" :conversation conversation))
```

Another example:

```lisp
(let ((conversation (chatbot:new-chat-persona "V")))
  (chatbot:chat "Review this plan and be blunt." :conversation conversation))
```

## Common usage patterns

### Plain Gemini chat with a custom instruction

```lisp
(let ((conversation
        (chatbot:new-chat
         :system-instruction "Explain things clearly and use examples.")))
  (chatbot:chat "What is continuation-passing style?" :conversation conversation))
```

### OpenAI-compatible chat

```lisp
(let ((conversation
        (chatbot:new-chat
         :backend :openai
         :model "gpt-4o")))
  (chatbot:chat "Write a short release note." :conversation conversation))
```

### One-turn file attachment

```lisp
(let ((conversation (chatbot:new-chat)))
  (chatbot:chat
   "Summarize this file."
   :conversation conversation
   :file #p"d:/path/to/file.txt"))
```

## Multi-agent usage

### Round-robin chat

Round-robin chat lets one user and two or more chatbot conversations take turns in a fixed order.

```lisp
(let* ((v-conversation (chatbot:new-chat-persona "V"))
       (janus-conversation (chatbot:new-chat-persona "Janus"))
       (session
         (chatbot:new-round-robin-chat
          (list (chatbot:make-round-robin-participant
                 :name "V"
                 :conversation v-conversation)
                (chatbot:make-round-robin-participant
                 :name "Janus"
                 :conversation janus-conversation))
          :user-name "Joe")))
  (chatbot:round-robin-chat
   "Discuss the best way to structure a debugging session."
   :session session))
```

### Sandbox personas

Sandbox personas are runtime-managed agents that keep their own isolated history.

### Spawn one ad hoc persona

```lisp
(chatbot:spawn-persona
 "Critic"
 :backend :google
 :model "gemini-3.5-flash"
 :role "a brutally honest reviewer"
 :tone "direct"
 :directives '("Find weak assumptions." "Suggest stronger alternatives."))
```

### Define one quickly at the REPL

```lisp
(chatbot:defpersona sparky (:backend :google
                           :model "gemini-3.5-flash"
                           :role "engineer")
  "Prefer practical solutions."
  "State tradeoffs explicitly.")
```

Helpful registry functions:

- `CHATBOT:LIST-PERSONAS`
- `CHATBOT:SHOW-PERSONAS`
- `CHATBOT:FIND-PERSONA`
- `CHATBOT:REMOVE-PERSONA`
- `CHATBOT:RESET-PERSONA`
- `CHATBOT:RESET-ALL-PERSONAS`
- `CHATBOT:CLEAR-PERSONAS`

### Ask several personas the same question

```lisp
(chatbot:query-all
 "What is the biggest design risk here?"
 :personas (list (chatbot:find-persona "Sparky")
                 (chatbot:find-persona "Critic")))
```

### Run a debate/forum

`RUN-ARENA` passes each persona's response into the next persona as labeled input.

```lisp
(chatbot:clear-personas)

(let* ((drill (chatbot:spawn-stock-persona
               :r-lee-ermey-drill-sergeant
               :model "gemini-3.5-flash"))
       (v (chatbot:spawn-persona
           "V"
           :persona-name "V"))
       (feynman (chatbot:spawn-stock-persona
                 :richard-feynman
                 :name "Feynman"
                 :model "gemini-3.5-flash"))
       (janus (chatbot:spawn-persona
               "Janus"
               :persona-name "Janus")))
  (chatbot:run-arena
   "Debate the best approach to designing a resilient Lisp agent sandbox."
   :personas (list drill v feynman janus)
   :rounds 2))
```

### Built-in stock personas

Available stock personas:

- `:r-lee-ermey-drill-sergeant`
- `:richard-feynman`

List them programmatically:

```lisp
(chatbot:list-stock-personas)
```

## Optional shared MCP startup

Loading the system does not automatically start shared MCP servers.

If you want configured startup MCP servers initialized and cached:

```lisp
(chatbot:initialize-startup-chatbot)
```

You can check whether that shared startup chatbot is initialized:

```lisp
(chatbot:startup-chatbot-initialized-p)
```

## Running tests

Run the full test suite from the repository root with:

```powershell
sbcl --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :chatbot)"
```

Or use the wrapper:

```powershell
sbcl --non-interactive --load run-tests.lisp
```
