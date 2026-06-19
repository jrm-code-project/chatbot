;;;

(in-package "CHATBOT")

(defclass chatbot ()
  ((model
    :initarg :model
    :accessor chatbot-model
    :initform "gemini-3.5-flash"
    :documentation "The model name used for the chatbot.")
   (backend
    :initarg :backend
    :accessor chatbot-backend
    :initform :gemini
    :documentation "The backend to use for the conversation (:gemini, :openai, or :google).")
   (system-instruction
    :initarg :system-instruction
    :accessor chatbot-system-instruction
    :initform nil
    :documentation "Optional system instructions directing the chatbot behavior.")
   (google-search-p
    :initarg :google-search-p
    :accessor chatbot-google-search-p
    :initform nil
    :documentation "Flag to enable Google Search Grounding tool.")
   (code-execution-p
    :initarg :code-execution-p
    :accessor chatbot-code-execution-p
    :initform nil
    :documentation "Flag to enable sandboxed Code Execution tool.")))

(defclass conversation ()
  ((chatbot
    :initarg :chatbot
    :accessor conversation-chatbot
    :documentation "Reference to the chatbot instance powering this conversation.")
   (interaction-id
    :initarg :interaction-id
    :accessor conversation-interaction-id
    :initform nil
    :documentation "Stateful Gemini Interaction ID for multi-turn conversations.")
   (messages
    :initarg :messages
    :accessor conversation-messages
    :initform nil
    :documentation "Accumulated conversation messages for stateless backends (like OpenAI).")))

