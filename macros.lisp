;;;

(in-package "CHATBOT")

;;; Main entry points for the Chatbot framework

(defun new-chat (&key model system-instruction google-search-p code-execution-p (backend :gemini))
  "Creates a new chatbot instance and returns an initialized conversation object.
If model is NIL, a sensible default model is chosen based on the backend."
  (let* ((default-model (case backend
                          (:openai "gpt-4o")
                          (:lm-studio "gemma-4-e4b-uncensored-hauhaucs-aggressive")
                          (:google "gemini-1.5-flash")
                          (t "gemini-3.5-flash")))
         (chosen-model (or model default-model))
         (bot (make-instance 'chatbot
                             :model chosen-model
                             :backend backend
                             :system-instruction system-instruction
                             :google-search-p google-search-p
                             :code-execution-p code-execution-p)))
    (make-instance 'conversation :chatbot bot)))

(defun get-user-homedir-pathname ()
  "Wrapper around user-homedir-pathname to allow package-lock-safe testing/mocking."
  (user-homedir-pathname))

(defun new-chat-persona (persona-name)
  "Creates a new chat session for a given chatbot persona.
The persona's configuration is read from ~/.Personas/<persona-name>/config.lisp
and the system instructions are loaded from the persona's system-instruction.md file."
  (let* ((homedir (get-user-homedir-pathname))
         (name-str (string persona-name))
         (persona-dir (or (uiop:directory-exists-p (merge-pathnames (make-pathname :directory (list :relative ".Personas" name-str)) homedir))
                          (uiop:directory-exists-p (merge-pathnames (make-pathname :directory (list :relative ".Personas" (string-downcase name-str))) homedir))
                          (error "Persona directory not found: ~~/.Personas/~A" name-str)))
         (config-path (probe-file (merge-pathnames "config.lisp" persona-dir)))
         (inst-path (or (probe-file (merge-pathnames "system-instruction.md" persona-dir))
                        (probe-file (merge-pathnames "system-instructions.md" persona-dir)))))
    (let* ((config (when config-path
                     (handler-case
                         (with-open-file (stream config-path :direction :input)
                           (read stream nil nil))
                       (error () nil))))
           (system-instruction (when inst-path
                                 (uiop:read-file-string inst-path)))
           (model (safe-getf config :model))
           (googleapi (safe-getf config :googleapi))
           (google-search-p (safe-getf config :google-search-p))
           (code-execution-p (safe-getf config :code-execution-p))
           (backend (cond
                      ((eq googleapi :google-api) :google)
                      (t :gemini))))
      (new-chat :backend backend
                :model model
                :system-instruction system-instruction
                :google-search-p google-search-p
                :code-execution-p code-execution-p))))

(defun chat-gemini (bot input conversation callback)
  "Sends user input to the active conversation using the Gemini Interactions API."
  (let ((api-key (google:gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (google:gemini-api-key) is configured."))
    (let* ((payload-alist (make-interaction-payload
                            bot
                            input
                            :previous-interaction-id (conversation-interaction-id conversation)
                            :stream t))
           (payload-json (cl-json:encode-json-to-string payload-alist))
           (url (concatenate 'string *gemini-base-url* "/interactions?alt=sse"))
           (headers (list (cons "x-goog-api-key" api-key)
                          (cons "Api-Revision" "2026-05-20")
                          (cons "Content-Type" "application/json")))
           (full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
      (handler-case
          (multiple-value-bind (stream status)
              (dexador:post url :headers headers :content payload-json :want-stream t)
            (if (= status 200)
                (unwind-protect
                     (loop for line = (read-sse-line stream)
                           until (eq line :eof)
                           do (let ((event (parse-sse-event line)))
                                (when event
                                  (let ((event-type (cdr (assoc :event--type event))))
                                    (cond
                                      ((string= event-type "step.delta")
                                       (let* ((delta (cdr (assoc :delta event)))
                                              (delta-type (cdr (assoc :type delta)))
                                              (delta-text (cdr (assoc :text delta))))
                                         (when (and (string= delta-type "text") (stringp delta-text))
                                           (loop for char across delta-text
                                                 do (vector-push-extend char full-text))
                                           (when callback
                                             (funcall callback delta-text)))))
                                      ((or (string= event-type "interaction.created")
                                           (string= event-type "interaction.completed"))
                                       (let* ((interaction (cdr (assoc :interaction event)))
                                              (id (cdr (assoc :id interaction))))
                                         (when id
                                           (setf (conversation-interaction-id conversation) id))))
                                      ((string= event-type "interaction.status_update")
                                       (let ((id (cdr (assoc :interaction--id event))))
                                         (when id
                                           (setf (conversation-interaction-id conversation) id)))))))))
                  (close stream))
                (error "API responded with HTTP status ~A" status)))
        (error (e)
          (error "Gemini Chat Error: ~A" e)))
      (format-paragraphs full-text :width 80)
      (coerce full-text 'string))))

(defun chat-openai (bot input conversation callback)
  "Sends user input to the active conversation using an OpenAI-compliant chat completions API."
  (let* ((backend (chatbot-backend bot))
         (api-key (if (eq backend :lm-studio)
                      (lm-studio-api-key)
                      (openai-api-key)))
         (base-url (if (eq backend :lm-studio)
                       *lm-studio-base-url*
                       *openai-base-url*)))
    (unless (and api-key (string/= api-key ""))
      (error "~A API Key is not set." (if (eq backend :lm-studio) "LM Studio" "OpenAI")))
    (let* ((system-inst (chatbot-system-instruction bot))
           (current-messages (conversation-messages conversation))
           (messages (cond
                       (current-messages current-messages)
                       (system-inst (list (list (cons "role" "system") (cons "content" system-inst))))
                       (t nil))))
      (setf messages (append messages (list (list (cons "role" "user") (cons "content" input)))))
      (let* ((payload-alist (list (cons "model" (chatbot-model bot))
                                  (cons "messages" messages)
                                  (cons "stream" t)))
             (payload-json (cl-json:encode-json-to-string payload-alist))
             (url (concatenate 'string base-url "/chat/completions"))
             (headers (list (cons "Authorization" (concatenate 'string "Bearer " api-key))
                            (cons "Content-Type" "application/json")))
             (full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
        (handler-case
            (multiple-value-bind (stream status)
                (dexador:post url :headers headers :content payload-json :want-stream t)
              (if (= status 200)
                  (unwind-protect
                       (loop for line = (read-sse-line stream)
                             until (or (eq line :eof)
                                       (and (stringp line)
                                            (alexandria:starts-with-subseq "data: [DONE]" line)))
                             do (let ((event (parse-sse-event line)))
                                  (when event
                                    (let* ((choices (cdr (assoc :choices event)))
                                           (first-choice (car choices))
                                           (delta (cdr (assoc :delta first-choice)))
                                           (delta-text (cdr (assoc :content delta))))
                                      (when (and (stringp delta-text) (string/= delta-text ""))
                                        (loop for char across delta-text
                                              do (vector-push-extend char full-text))
                                        (when callback
                                          (funcall callback delta-text)))))))
                    (close stream))
                  (error "API responded with HTTP status ~A" status)))
          (error (e)
            (error "OpenAI Chat Error: ~A" e)))
        (format-paragraphs full-text :width 80)
        (let ((final-str (coerce full-text 'string)))
          (setf (conversation-messages conversation)
                (append messages (list (list (cons "role" "assistant") (cons "content" final-str)))))
          final-str)))))

(defun chat-google (bot input conversation callback)
  "Sends user input to the active conversation using Google's non-streaming generateContent API."
  (let ((api-key (google:gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (google:gemini-api-key) is configured."))
    (let* ((system-inst (chatbot-system-instruction bot))
           (current-messages (conversation-messages conversation))
           (messages (append current-messages (list (list (cons "role" "user") (cons "content" input)))))
           (contents (coerce
                      (mapcar (lambda (msg)
                                (let ((role (cdr (assoc "role" msg :test #'string=)))
                                      (content (cdr (assoc "content" msg :test #'string=))))
                                  (list (cons "role" (if (string= role "assistant") "model" "user"))
                                        (cons "parts" (vector (list (cons "text" content)))))))
                              messages)
                      'vector))
           (payload-alist (list (cons "contents" contents)))
           (url (concatenate 'string *gemini-base-url* "/models/" (chatbot-model bot) ":generateContent?key=" api-key))
           (headers (list (cons "Content-Type" "application/json"))))
      (when system-inst
        (setf payload-alist
              (append payload-alist
                      (list (cons "systemInstruction"
                                  (list (cons "parts"
                                              (vector (list (cons "text" system-inst))))))))))
      (let ((payload-json (cl-json:encode-json-to-string payload-alist)))
        (handler-case
            (multiple-value-bind (response-body status)
                (dexador:post url :headers headers :content payload-json)
              (if (= status 200)
                  (let* ((response-alist (cl-json:decode-json-from-string response-body))
                         (candidates (cdr (assoc :candidates response-alist)))
                         (first-candidate (car candidates))
                         (content (cdr (assoc :content first-candidate)))
                         (parts (cdr (assoc :parts content)))
                         (first-part (car parts))
                         (final-str (cdr (assoc :text first-part))))
                    (unless final-str
                      (error "No text returned from Gemini API response: ~A" response-body))
                    (format-paragraphs final-str :width 80)
                    (when callback
                      (funcall callback final-str))
                    (setf (conversation-messages conversation)
                          (append messages (list (list (cons "role" "assistant") (cons "content" final-str)))))
                    final-str)
                  (error "API responded with HTTP status ~A" status)))
          (error (e)
            (error "Google Chat Error: ~A" e)))))))

(defun chat (input &key (conversation *default-conversation*) callback)
  "Sends user input to the active conversation using the appropriate backend API.
If a callback is provided, each text token is passed to it in real-time.
Returns the complete response text."
  (unless conversation
    (error "No conversation provided and *default-conversation* is NIL. Please specify a conversation or set *default-conversation*."))
  (let ((bot (conversation-chatbot conversation)))
    (case (chatbot-backend bot)
      (:gemini
       (chat-gemini bot input conversation callback))
      (:openai
       (chat-openai bot input conversation callback))
      (:lm-studio
       (chat-openai bot input conversation callback))
      (:google
       (chat-google bot input conversation callback))
      (t
       (error "Unknown chatbot backend: ~S" (chatbot-backend bot))))))


