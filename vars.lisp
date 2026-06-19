;;;

(in-package "CHATBOT")

;;; Variables for the Chatbot framework

(defvar *gemini-base-url* "https://generativelanguage.googleapis.com/v1beta"
  "The base REST endpoint for the Gemini Interactions API.")

(defvar *openai-base-url* "https://api.openai.com/v1"
  "The base REST endpoint for the OpenAI-compliant API.")

(defvar *openai-api-key* nil
  "The API key for the OpenAI-compliant API. If nil, looks up the OPENAI_API_KEY environment variable.")

(defun openai-api-key ()
  "Returns the OpenAI API key. First checks *openai-api-key*, then the OPENAI_API_KEY environment variable."
  (or *openai-api-key*
      (uiop:getenv "OPENAI_API_KEY")))

(defvar *lm-studio-base-url* "http://127.0.0.1:8088/v1"
  "The base REST endpoint for the local LM Studio API.")

(defvar *lm-studio-api-key* "lm_studio"
  "The API key for the LM Studio API.")

(defun lm-studio-api-key ()
  "Returns the LM Studio API key. First checks *lm-studio-api-key*, then the LM_API_TOKEN environment variable."
  (or *lm-studio-api-key*
      (uiop:getenv "LM_API_TOKEN")
      "lm_studio"))

(defvar *default-conversation* nil
  "The default conversation instance used by CHAT if none is specified.")


