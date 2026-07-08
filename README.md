# Chatbot REPL Interface

This document describes the core top-level functions used to interact with the Common Lisp Chatbot environment from the REPL, as well as how to configure persistent personas.

## Quick Chat Functions

The most common way to interact with the chatbot is using the `chat` function.

### `chat`

Sends a prompt to the active conversation and returns the response string.

**Basic Usage:**
```commonlisp
(chat "Hello, who are you?")
```

**Keyword Arguments:**
* `:conversation` - Target a specific conversation object instead of the default active conversation.
* `:file` / `:files` - Attach local files or remote URLs to the prompt.
* `:temperature` / `:top-p` - Override the sampling parameters for this specific turn.
* `:callback` - Provide a function to receive text tokens as they stream in real-time.

### `new-chat`

Creates a fresh conversation state. Useful when you want to clear the history without loading a specific persona.

**Basic Usage:**
```commonlisp
(new-chat :backend :gemini :model "gemini-1.5-pro")
```

## Persona Configuration

A "persona" is a persistent configuration that defines a chatbot's backend, model, system instructions, and optional tools. Personas are stored in your home directory under `~/.Personas/`.

### Directory Structure

To create a persona named `MyPersona`, create the following directory structure:

```
~/.Personas/MyPersona/
    config.lisp
    system-instructions.md (or system-instructions)
```

### `config.lisp`

This file contains a single Lisp property list (plist) defining the persona's settings.

**Example `config.lisp`:**
```commonlisp
(:backend :gemini
 :model "gemini-1.5-pro"
 :temperature 0.7
 :enable-filesystem-tools t
 :enable-eval t
 :enable-web-tools t)
```

### `system-instructions.md`

This file contains the plain text Markdown instructions that tell the AI who it is and how to behave.

## Interacting with Personas

### `new-chat-persona`

Loads a persona from the `~/.Personas/` directory and creates a new conversation using its configuration.

**Basic Usage:**
```commonlisp
(new-chat-persona "MyPersona")
```

Once loaded, subsequent calls to `chat` will automatically route to this newly instantiated persona conversation unless overridden.

### The Sandbox Registry (Optional)

You can also spawn personas into a named registry for easy multi-agent or background tasking.

* **`spawn-persona`**: Loads a persona and binds it to a specific registry name.
* **`list-personas`**: Lists all active personas in the registry.
* **`remove-persona`**: Removes a persona from the registry.

## Advanced Orchestration

### `round-robin-chat`

Passes a prompt sequentially to a configured list of participants, allowing multiple personas to weigh in on the same conversation thread.

### `run-arena`

Forces multiple personas to debate or respond to *each other's* outputs for a specified number of rounds.
