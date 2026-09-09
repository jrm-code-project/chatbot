;;;

(in-package "CHATBOT")

;;; Provider configuration: base URLs, credentials, default models, and the
;;; HTTP/cache-policy normalization helpers shared by all LLM backends.

(defparameter *gemini-base-url* "https://generativelanguage.googleapis.com/v1beta"
  "The base REST endpoint for the Gemini Interactions API.")

(defparameter *openai-base-url* "https://api.openai.com/v1"
  "The base REST endpoint for the OpenAI-compliant API.")

(defparameter *openai-api-key* nil
  "The API key for the OpenAI-compliant API. If nil, looks up the OPENAI_API_KEY environment variable.")

(defparameter *grok-base-url* "https://api.x.ai/"
  "The base REST endpoint for the Grok API.")

(defparameter *grok-api-key* nil
  "The API key for the Grok API. If nil, reads from AppData/Local/config/X/api-key or checks GROK_API_KEY environment variable.")

(defparameter *texttospeech-base-url* "https://texttospeech.googleapis.com/v1"
  "The base REST endpoint for the Google Cloud Text-to-Speech API.")

(defparameter *texttospeech-api-key* nil
  "The API key for the Google Cloud Text-to-Speech API. If nil, reads from AppData/Local/config/googleapis/texttospeech/apikey.
When no key can be found, text-to-speech playback is skipped (not an error).")

(defparameter *getenv-function* #'uiop:getenv
  "Function used to read environment variables.")

(defparameter *gemini-api-key-function* #'google:gemini-api-key
  "Function used to resolve the Gemini API key.")

(defparameter *web-search-function* #'google:web-search
  "Function used by the built-in web grounding search tool.")

(defparameter *hyperspec-search-function* #'google:hyperspec-search
  "Function used by the built-in HyperSpec grounding search tool.")

(defparameter *user-homedir-pathname-function* #'user-homedir-pathname
  "Function used to resolve the current user's home directory pathname.")

(defun require-non-empty-string (value context)
  "Returns VALUE when it is a non-empty string, otherwise signals an error for CONTEXT."
  (unless (and (stringp value)
               (string/= value ""))
    (error "~A must be a non-empty string." context))
  value)

(defparameter *backend-default-models*
  '((:gemini . "gemini-3.5-flash")
    (:google . "gemini-3.5-flash")
    (:openai . "gpt-4o")
    (:lm-studio . "gemma-4-e4b-uncensored-hauhaucs-aggressive")
    (:grok . "grok-2-latest"))
  "Default model names keyed by backend.")

(defun backend-default-model (backend)
  "Returns the configured default model for BACKEND.
Unknown backends fall back to the Gemini default."
  (let* ((gemini-default (cdr (assoc :gemini *backend-default-models*)))
         (resolved (or (cdr (assoc backend *backend-default-models*))
                       gemini-default)))
    (require-non-empty-string resolved (format nil "Default model for backend ~A" backend))))

(defparameter *cheap-summarization-models*
  '((:gemini . "gemini-flash-lite-latest")
    (:google . "gemini-flash-lite-latest")
    (:openai . "gpt-4o-mini")
    (:grok . "grok-2-latest"))
  "Cheap model names, keyed by backend, used for internal history-digest
summarization instead of the parent conversation's (potentially expensive)
model. Backends without a configured entry fall back to the caller-supplied
default model.")

(defun cheap-summarization-model (backend default-model)
  "Returns the configured cheap summarization model for BACKEND, or
DEFAULT-MODEL when BACKEND has no cheaper model configured."
  (or (cdr (assoc backend *cheap-summarization-models*))
      default-model))

(defun normalize-chatbot-backend (backend context &key allow-nil-p)
  "Normalizes BACKEND to a backend keyword for CONTEXT."
  (when (null backend)
    (if allow-nil-p
        (return-from normalize-chatbot-backend nil)
        (error "~A backend is required." context)))
  (let ((normalized
          (typecase backend
            (keyword backend)
            (string
             (unless (string= backend "")
               (intern (substitute #\- #\_ (string-upcase backend)) "KEYWORD")))
            (t nil))))
    (unless normalized
      (error "Invalid ~A backend: ~S" context backend))
    normalized))

(defun openai-api-key ()
  "Returns the OpenAI API key. First checks *openai-api-key*, then the OPENAI_API_KEY environment variable."
  (or *openai-api-key*
      (funcall (current-getenv-function) "OPENAI_API_KEY")))

(defun grok-api-key-file-path ()
  "Constructs the target path for the Grok API key stored in AppData/Local/config/X/api-key."
  (let* ((local-app-data (funcall (current-getenv-function) "LOCALAPPDATA"))
         (home (funcall *user-homedir-pathname-function*)))
    (if (and local-app-data (string/= local-app-data ""))
        (merge-pathnames "config/X/api-key" (uiop:ensure-directory-pathname local-app-data))
        (merge-pathnames "AppData/Local/config/X/api-key" home))))

(defun grok-api-key ()
  "Returns the Grok API key. First checks *grok-api-key*, then reads from AppData/Local/config/X/api-key,
and falls back to the GROK_API_KEY environment variable."
  (or *grok-api-key*
      (let ((path (grok-api-key-file-path)))
        (if (probe-file path)
            (string-trim '(#\Space #\Tab #\Return #\Linefeed) (uiop:read-file-string path))
            (funcall (current-getenv-function) "GROK_API_KEY")))))

(defun texttospeech-api-key-file-path ()
  "Constructs the target path for the Text-to-Speech API key stored in AppData/Local/config/googleapis/texttospeech/apikey."
  (let* ((local-app-data (funcall (current-getenv-function) "LOCALAPPDATA"))
         (home (funcall *user-homedir-pathname-function*)))
    (if (and local-app-data (string/= local-app-data ""))
        (merge-pathnames "config/googleapis/texttospeech/apikey" (uiop:ensure-directory-pathname local-app-data))
        (merge-pathnames "AppData/Local/config/googleapis/texttospeech/apikey" home))))

(defun texttospeech-api-key ()
  "Returns the Text-to-Speech API key, or NIL when unconfigured.
First checks *texttospeech-api-key*, then reads from AppData/Local/config/googleapis/texttospeech/apikey.
Unlike other provider API keys, a missing key is not an error: callers should log and skip
text-to-speech playback when this returns NIL."
  (or *texttospeech-api-key*
      (let ((path (texttospeech-api-key-file-path)))
        (when (probe-file path)
          (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) (uiop:read-file-string path))))
            (and (string/= trimmed "") trimmed))))))

(defun grok-api-base-url ()
  "Returns the normalized OpenAI-compatible Grok API base URL."
  (let ((base-url (string-right-trim "/" *grok-base-url*)))
    (if (alexandria:ends-with-subseq "/v1" base-url)
        base-url
        (concatenate 'string base-url "/v1"))))

(defun generate-unique-grok-conv-id ()
  "Generates a lightweight, unique session ID for Grok prefix caching."
  (format nil "grok-session-~X-~X"
          (get-universal-time)
          (random #xFFFFFFFF)))

(defparameter *lm-studio-base-url* "http://127.0.0.1:1234"
  "The host root for the local LM Studio API.")

(defun lm-studio-api-base-url ()
  "Returns the normalized OpenAI-compatible LM Studio API base URL."
  (let ((base-url (string-right-trim "/" *lm-studio-base-url*)))
    (if (alexandria:ends-with-subseq "/v1" base-url)
        base-url
        (concatenate 'string base-url "/v1"))))

(defparameter *lm-studio-default-api-key* "lm_studio"
  "Fallback API key used when LM Studio credentials are otherwise unset.")

(defparameter *lm-studio-api-key* nil
  "The API key for the LM Studio API.")

(defparameter *lm-studio-http-read-timeout* 600
  "Minimum HTTP response timeout in seconds for the LM Studio backend.")

(defparameter *google-http-read-timeout* 150
  "Minimum HTTP response timeout in seconds for the Gemini and Google backends.")

(defparameter +default-content-cache-policy+ :auto
  "Default content-caching policy for chatbots.")

(defparameter *default-content-cache-ttl-seconds* 3600
  "Default TTL in seconds for newly created explicit Gemini content caches.")

(defparameter *default-content-cache-min-tokens* 2048
  "Default estimated token threshold before automatic explicit cache creation is attempted.")

(defun lm-studio-api-key ()
  "Returns the LM Studio API key. First checks *lm-studio-api-key*, then the LM_API_TOKEN environment variable."
  (or *lm-studio-api-key*
      (funcall (current-getenv-function) "LM_API_TOKEN")
      (require-non-empty-string *lm-studio-default-api-key* "LM Studio default API key")))

(defun backend-http-read-timeout (backend)
  "Returns the effective HTTP read timeout for BACKEND."
  (let ((default-timeout (current-http-read-timeout)))
    (cond
      ((eq backend :lm-studio)
       (max default-timeout *lm-studio-http-read-timeout*))
      ((member backend '(:gemini :google))
       (max default-timeout *google-http-read-timeout*))
      (t default-timeout))))

(defun normalize-content-cache-policy (policy &key allow-nil-p)
  "Returns POLICY normalized to a supported content-caching keyword."
  (when (null policy)
    (if allow-nil-p
        (return-from normalize-content-cache-policy nil)
        (error "Content cache policy is required.")))
  (let ((normalized
          (typecase policy
            (keyword policy)
            (string (intern (string-upcase policy) "KEYWORD"))
            (t nil))))
    (unless (member normalized '(:auto :off))
      (error "Unsupported content cache policy: ~S" policy))
    normalized))

(defun normalize-content-cache-ttl-seconds (ttl-seconds &key allow-nil-p)
  "Returns TTL-SECONDS validated as a positive integer or NIL."
  (when (null ttl-seconds)
    (if allow-nil-p
        (return-from normalize-content-cache-ttl-seconds nil)
        (error "Content cache TTL must not be NIL.")))
  (unless (and (integerp ttl-seconds)
               (> ttl-seconds 0))
    (error "Content cache TTL must be a positive integer number of seconds: ~S" ttl-seconds))
  ttl-seconds)

(defun normalize-content-cache-min-tokens (min-tokens &key allow-nil-p)
  "Returns MIN-TOKENS validated as a positive integer or NIL."
  (when (null min-tokens)
    (if allow-nil-p
        (return-from normalize-content-cache-min-tokens nil)
        (error "Content cache minimum token threshold must not be NIL.")))
  (unless (and (integerp min-tokens)
               (> min-tokens 0))
    (error "Content cache minimum token threshold must be a positive integer: ~S" min-tokens))
  min-tokens)

(defun gemini-api-key ()
  "Returns the Gemini API key using the current runtime seam."
  (funcall (current-gemini-api-key-function)))

(defparameter *gemini-api-revision* "2026-05-20"
  "API revision header value used for Gemini Interactions requests.")

(defun gemini-api-revision ()
  "Returns the configured Gemini API revision header value."
  (require-non-empty-string *gemini-api-revision* "Gemini API revision"))

(defparameter *http-post-function* #'dexador:post
  "Function used to perform HTTP POST requests.")

(defparameter *http-get-function* #'dexador:get
  "Function used to perform HTTP GET requests.")

(defparameter *http-patch-function* #'dexador:patch
  "Function used to perform HTTP PATCH requests.")

(defparameter *http-delete-function* #'dexador:delete
  "Function used to perform HTTP DELETE requests.")
