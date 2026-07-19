;;; -*- Lisp -*-
;;; backend-google.lisp - Google generateContent flow

(in-package "CHATBOT")

(defun generate-content-model-name (model)
  "Returns MODEL in the form expected by the generateContent URL path."
  (if (and (stringp model)
           (alexandria:starts-with-subseq "models/" model))
      (subseq model (length "models/"))
      model))

(defun malformed-response-stop-reason-p (stop-reason)
  "Returns true when STOP-REASON indicates the provider malformed the response."
  (and stop-reason
       (member (princ-to-string stop-reason)
               '("MALFORMED_RESPONSE" "MALFORMED_FUNCTION_CALL")
               :test #'string-equal)))

(defun response-stop-reason (value)
  "Returns a stop or finish reason from VALUE when present."
  (or (mcp-val :finish-reason value)
      (mcp-val :finish_reason value)
      (mcp-val "finishReason" value)
      (mcp-val :stop-reason value)
      (mcp-val :stop_reason value)
      (mcp-val "stopReason" value)))

(defun google-json-true-p (value)
  "Returns true when VALUE represents a JSON true boolean."
  (typecase value
    (null nil)
    (number (not (zerop value)))
    (string (not (member (string-downcase value)
                         '("" "0" "false" "nil")
                         :test #'string=)))
    (symbol (not (member value '(nil false :false) :test #'eq)))
    (t t)))

(defun google-part-text (part)
  "Returns the text payload stored in PART, when present."
  (cdr (assoc :text part)))

(defun google-part-function-call (part)
  "Returns the function-call payload stored in PART, when present."
  (or (cdr (assoc :function-call part))
      (cdr (assoc :function--call part))))

(defun google-part-thought-signature (part)
  "Returns the thought signature stored in PART, when present."
  (or (cdr (assoc :thought-signature part))
      (cdr (assoc :thoughtSignature part :test #'string=))))

(defun google-part-thought-p (part)
  "Returns true when PART is marked as a thought fragment."
  (google-json-true-p
   (or (cdr (assoc :thought part))
       (cdr (assoc "thought" part :test #'string=)))))

(defun join-google-part-texts (parts)
  "Returns a single string combining non-empty text fragments from PARTS."
  (let ((texts (remove nil
                       (mapcar #'google-part-text parts)
                       :test #'equal)))
    (when texts
      (format nil "~{~A~^~%~%~}" texts))))

(defun retry-on-google-gemini-pro-latest (bot input conversation callback
                                            &key file-attachments request-contents history-messages
                                              effective-model
                                              effective-generation-config
                                              use-stronger-model-p
                                              return-turn-result-p
                                              (recursion-depth 0))
  "Resubmits the current turn through the Google backend."
  (declare (ignore request-contents history-messages))
  (let* ((current-model (or effective-model (chatbot-model bot)))
         (target-model (if use-stronger-model-p
                           (stronger-model current-model)
                           +google-gemini-model-override-model+)))
    (chat-google bot
                 input
                 conversation
                 callback
                 :file-attachments file-attachments
                 :effective-model target-model
                 :effective-generation-config effective-generation-config
                 :malformed-response-fallback-attempted-p t
                 :return-turn-result-p return-turn-result-p
                 :recursion-depth recursion-depth
                 :bypass-cache-p t)))

(defun google-request-state (bot input conversation file-attachments effective-model effective-generation-config
                              &key request-contents history-messages malformed-response-fallback-attempted-p cached-content-name bypass-cache-p)
  "Builds the provider-runner state for a Google generateContent turn."
  (let* ((current-messages (conversation-messages conversation))
         (persona-memory (conversation-persona-memory conversation))
         (persona-diary-entries (conversation-persona-diary-entries conversation))
         (resolved-cached-content-name
           (and (not bypass-cache-p)
                (or cached-content-name
                    (ensure-google-conversation-content-cache conversation
                                                             :effective-model effective-model)))))
    (list :input input
          :file-attachments file-attachments
          :effective-model effective-model
          :effective-generation-config effective-generation-config
          :malformed-response-fallback-attempted-p malformed-response-fallback-attempted-p
          :cached-content-name resolved-cached-content-name
          :bypass-cache-p bypass-cache-p
          :history-messages (or history-messages
                              (stateless-history-messages current-messages input))
          :request-contents (or request-contents
                               (build-generate-content-request-contents current-messages
                                                                        input
                                                                        :chatbot bot
                                                                        :persona-memory persona-memory
                                                                        :persona-diary-entries persona-diary-entries
                                                                        :file-attachments file-attachments
                                                                        :effective-model effective-model
                                                                        :omit-preloaded-history-p (and resolved-cached-content-name t))))))

(defun google-model-override-active-p (bot effective-model)
  "Returns true when the malformed-response fallback override model is already active."
  (string-equal (or effective-model
                   (chatbot-model bot))
                +google-gemini-model-override-model+))

(defun google-generation-config-alist (effective-generation-config)
  "Returns the Google generationConfig alist implied by EFFECTIVE-GENERATION-CONFIG."
  (remove nil
          (list (when (getf effective-generation-config :temperature)
                  (cons "temperature" (getf effective-generation-config :temperature)))
                (when (getf effective-generation-config :top-p)
                  (cons "topP" (getf effective-generation-config :top-p))))))

(defun google-request-payload-alist (bot request-contents effective-generation-config &key cached-content-name)
  "Returns the generateContent payload alist for BOT."
  (let* ((system-inst (chatbot-system-instruction bot))
         (gemini-tools (generate-content-request-tools bot))
         (generation-config (google-generation-config-alist effective-generation-config)))
    (append (list (cons "contents" (coerce request-contents 'vector)))
            (when cached-content-name
              (list (cons "cachedContent" cached-content-name)))
            (when (and system-inst
                       (null cached-content-name))
              (list (cons "systemInstruction"
                         (list (cons "parts"
                                     (system-instruction-text-parts system-inst))))))
            (when (and gemini-tools
                       (null cached-content-name))
              (list (cons "tools" gemini-tools)))
            (when generation-config
              (list (cons "generationConfig" generation-config))))))

(defun google-request-url (bot effective-model)
  "Returns the generateContent URL for BOT and EFFECTIVE-MODEL."
  (concatenate 'string
               *gemini-base-url*
               "/models/"
               (generate-content-model-name (or effective-model
                                               (chatbot-model bot)))
               ":generateContent"))

(defun google-request-headers (api-key)
  "Returns the Google request headers for API-KEY."
  (list (cons "x-goog-api-key" api-key)
        (cons "Content-Type" "application/json")))

(defun google-response-primary-parts (response-alist)
  "Returns the primary candidate parts from RESPONSE-ALIST."
  (let* ((candidates (cdr (assoc :candidates response-alist)))
         (first-candidate (car candidates))
         (content (cdr (assoc :content first-candidate))))
    (cdr (assoc :parts content))))

(defun parse-google-response (response-body)
  "Returns RESPONSE-BODY decoded into normalized Google response fields."
  (let* ((response-alist (cl-json:decode-json-from-string response-body))
         (candidates (cdr (assoc :candidates response-alist)))
         (first-candidate (car candidates))
         (parts (google-response-primary-parts response-alist))
         (function-call-part (find-if #'google-part-function-call parts))
         (fn-call (and function-call-part
                      (google-part-function-call function-call-part))))
    (list :usage (cdr (assoc :usage-metadata response-alist))
          :response-id (cdr (assoc :response-id response-alist))
          :model-version (cdr (assoc :model-version response-alist))
          :finish-reason (response-stop-reason first-candidate)
          :parts parts
          :function-call fn-call
          :thought-signature (and function-call-part
                                 (google-part-thought-signature function-call-part))
          :thought-text (join-google-part-texts
                        (remove-if-not #'google-part-thought-p parts))
          :final-text (join-google-part-texts
                      (remove-if #'google-part-thought-p parts)))))

(defun google-tool-call-outcome (state fn-call thought-signature)
  "Returns the provider tool outcome for one Google FN-CALL."
  (let ((name (cdr (assoc :name fn-call)))
        (raw-args (cdr (assoc :args fn-call))))
    (make-provider-turn-tool-outcome
     (list (list (cons :id nil)
                 (cons :name name)
                 (cons :arguments (coerce (cl-json:encode-json-to-string
                                          (if raw-args
                                              (json-encodable-value raw-args)
                                              (empty-json-object)))
                                         'simple-string))
                 (cons :raw-args raw-args)
                 (cons :thought-signature thought-signature)))
     :history-messages (getf state :history-messages)
     :request-contents (getf state :request-contents)
     :file-attachments (getf state :file-attachments)
     :effective-model (getf state :effective-model)
     :effective-generation-config (getf state :effective-generation-config)
     :cached-content-name (getf state :cached-content-name)
     :bypass-cache-p (getf state :bypass-cache-p)
     :malformed-response-fallback-attempted-p
     (getf state :malformed-response-fallback-attempted-p))))

(defun google-tool-arguments-log-label (name tool-call)
  "Returns the debug label for one Google tool request."
  (declare (ignore tool-call))
  (format nil "Google tool arguments for ~A" name))

(defun google-function-response-message (name response-payload)
  "Returns one Google functionResponse user message for NAME and RESPONSE-PAYLOAD."
  `(("role" . "user")
    ("parts" . ,(vector
                (list (cons "functionResponse"
                            `(("name" . ,name)
                              ("response" . ,response-payload))))))))

(defun google-tool-success-message (id name args-str res-text tool-call)
  "Returns the Google tool success response message."
  (declare (ignore id args-str tool-call))
  (google-function-response-message name `(("result" . ,res-text))))

(defun google-tool-error-message (id name args-str condition tool-call)
  "Returns the Google tool error response message."
  (declare (ignore id args-str tool-call))
  (google-function-response-message name
                                   (chatbot-tool-error-payload name condition)))

(defun google-tool-call-model-message (tool-call)
  "Returns the Google model-side functionCall message for TOOL-CALL."
  (let* ((name (cdr (assoc :name tool-call)))
        (raw-args (cdr (assoc :raw-args tool-call)))
        (payload-args (if raw-args
                          (json-encodable-value raw-args)
                          (empty-json-object)))
        (thought-signature (cdr (assoc :thought-signature tool-call))))
    `(("role" . "model")
     ("parts" . ,(vector
                  (append
                   `(("functionCall" . (("name" . ,name)
                                        ("args" . ,payload-args))))
                   (when thought-signature
                     `(("thoughtSignature" . ,thought-signature)))))))))

(defun google-tool-recursion-messages (tool-calls tool-results)
  "Returns the Google recursion messages formed from TOOL-CALLS and TOOL-RESULTS."
  (append (list (google-tool-call-model-message (car tool-calls)))
         tool-results))

(defun google-tool-recursion-state (bot conversation outcome recursive-history recursion-messages)
  "Returns the next Google request state after one tool-recursion round."
  (google-request-state
   bot
   nil
   conversation
   (getf outcome :file-attachments)
   (getf outcome :effective-model)
   (getf outcome :effective-generation-config)
   :request-contents (append (getf outcome :request-contents)
                            recursion-messages)
   :history-messages recursive-history
   :cached-content-name (getf outcome :cached-content-name)
   :bypass-cache-p (getf outcome :bypass-cache-p)
   :malformed-response-fallback-attempted-p
   (getf outcome :malformed-response-fallback-attempted-p)))

(defun continue-google-provider-tool-recursion (bot conversation outcome next-depth step)
  "Continues the Google turn loop after the model requested tool execution."
  (continue-stateless-provider-tool-recursion
   bot
   (getf outcome :history-messages)
   (provider-turn-outcome-tool-calls outcome)
   #'google-tool-arguments-log-label
   #'google-tool-success-message
   #'google-tool-recursion-messages
   (lambda (recursive-history recursion-messages)
     (funcall step
             (google-tool-recursion-state
              bot
              conversation
              outcome
              recursive-history
              recursion-messages)
             next-depth))
   :error-builder #'google-tool-error-message))

(defun google-final-text-retryable-p (bot state final-text)
  "Returns true when FINAL-TEXT should trigger the gemini-pro-latest retry path."
  (and (or (null final-text)
           (string= final-text ""))
       (not (getf state :malformed-response-fallback-attempted-p))
       (not (google-model-override-active-p bot (getf state :effective-model)))))

(defun google-response->provider-outcome (bot state response-body status parsed-response)
  "Returns the provider outcome implied by PARSED-RESPONSE."
  (let ((usage (getf parsed-response :usage))
        (response-id (getf parsed-response :response-id))
        (model-version (getf parsed-response :model-version))
        (finish-reason (getf parsed-response :finish-reason))
        (fn-call (getf parsed-response :function-call))
        (thought-signature (getf parsed-response :thought-signature))
        (thought-text (getf parsed-response :thought-text))
        (final-text (getf parsed-response :final-text)))
    (log-backend-response-stats
     :google
     :http-status status
     :response-id response-id
     :model model-version
     :finish-reason finish-reason
     :usage usage)
    (cond
      ((and (not (getf state :malformed-response-fallback-attempted-p))
            (not (google-model-override-active-p bot (getf state :effective-model)))
            (malformed-response-stop-reason-p finish-reason))
       (make-provider-turn-retry-outcome :reason :malformed-response))
      (fn-call
       (google-tool-call-outcome state fn-call thought-signature))
      ((google-final-text-retryable-p bot state final-text)
       (make-provider-turn-retry-outcome :reason :empty-response))
      ((or (null final-text)
           (string= final-text ""))
       (error "No text returned from Gemini API response: ~A" response-body))
      (t
       (make-provider-turn-final-outcome final-text
                                        :usage usage
                                        :thought-text thought-text
                                        :cached-content-name (getf state :cached-content-name))))))

(defun google-api-key-or-error ()
  "Returns the configured Google API key or signals when it is missing."
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    api-key))

(defun google-turn-request-payload-json (bot state)
  "Returns the encoded generateContent request payload for STATE."
  (cl-json:encode-json-to-string
   (google-request-payload-alist bot
                                 (getf state :request-contents)
                                 (getf state :effective-generation-config)
                                 :cached-content-name (getf state :cached-content-name))))

(defun google-turn-request-details (bot state)
  "Returns the request details plist for one Google generateContent turn."
  (let ((api-key (google-api-key-or-error)))
    (list :payload-json (google-turn-request-payload-json bot state)
          :url (google-request-url bot (getf state :effective-model))
          :headers (google-request-headers api-key))))

(defun post-google-turn-request (request-details)
  "Executes one Google generateContent request from REQUEST-DETAILS."
  (multiple-value-bind (response-body status)
      (post-web-request (getf request-details :url)
                        (getf request-details :headers)
                        (getf request-details :payload-json))
    (unless (= status 200)
      (error "API responded with HTTP status ~A" status))
    (list :response-body response-body
          :status status)))

(defun google-http-response->provider-outcome (bot state http-response)
  "Returns the provider outcome implied by HTTP-RESPONSE for STATE."
  (let ((response-body (getf http-response :response-body))
        (status (getf http-response :status)))
    (google-response->provider-outcome
     bot
     state
     response-body
     status
     (parse-google-response response-body))))

(defun submit-google-turn (bot response-body-parser state)
  "Submits one Google generateContent turn and returns a normalized outcome."
  (declare (ignore response-body-parser))
  (google-http-response->provider-outcome
   bot
   state
   (post-google-turn-request
    (google-turn-request-details bot state))))

(defun chat-google (bot input conversation callback
                   &key file-attachments request-contents history-messages effective-model effective-generation-config
                     malformed-response-fallback-attempted-p return-turn-result-p
                     (recursion-depth 0)
                     bypass-cache-p)
  "Sends user input to the active conversation using Google's non-streaming generateContent API."
  (let ((result
          (run-provider-turn-loop
           :google
           (google-request-state bot input conversation file-attachments effective-model effective-generation-config
                                :request-contents request-contents
                                :history-messages history-messages
                                :malformed-response-fallback-attempted-p malformed-response-fallback-attempted-p
                                :bypass-cache-p bypass-cache-p)
           (lambda (state current-depth)
             (declare (ignore current-depth))
             (submit-google-turn bot nil state))
           :retry-turn
           (lambda (state outcome current-depth step)
             (declare (ignore step))
             (let ((reason (getf outcome :reason)))
               (retry-on-google-gemini-pro-latest
                bot
                (getf state :input)
                conversation
                callback
                :file-attachments (getf state :file-attachments)
                :request-contents (getf state :request-contents)
                :history-messages (getf state :history-messages)
                :effective-generation-config (getf state :effective-generation-config)
                :effective-model (getf state :effective-model)
                :use-stronger-model-p (eq reason :empty-response)
                :return-turn-result-p t
                :recursion-depth current-depth)))
           :continue-with-tools
           (lambda (state outcome next-depth step)
             (declare (ignore state))
             (continue-google-provider-tool-recursion
              bot conversation outcome next-depth step))
           :finalize-turn
           (lambda (state outcome)
             (finish-stateless-text-turn (getf state :history-messages)
                                        "model"
                                        (provider-turn-outcome-text outcome)
                                        :callback callback
                                        :usage (provider-turn-outcome-usage outcome)
                                        :thought-text (provider-turn-outcome-thought-text outcome)
                                        :interaction-id (conversation-interaction-id conversation)))
           :error-handler
           (lambda (state condition current-depth)
             (if (and (getf state :cached-content-name)
                      (google-caching-error-p condition))
                 (progn
                   (log-message :warn "Google content cache not found or permission denied; retrying turn without explicit cache."
                                :context `(("name" . ,(getf state :cached-content-name))
                                           ("error" . ,(princ-to-string condition))))
                   (clear-google-conversation-content-cache-state conversation)
                   (chat-google bot
                                input
                                conversation
                                callback
                                :file-attachments file-attachments
                                :effective-model effective-model
                                :effective-generation-config effective-generation-config
                                :malformed-response-fallback-attempted-p malformed-response-fallback-attempted-p
                                :return-turn-result-p return-turn-result-p
                                :recursion-depth current-depth
                                :bypass-cache-p t))
                 (error "Google Chat Error: ~A" condition)))
           :initial-recursion-depth recursion-depth)))
    (if return-turn-result-p
        result
        (apply-chat-turn-result result conversation))))

(defun google-chat-backend-handler (input &key bot conversation callback file-attachments
                                               effective-model effective-generation-config)
  "Runs one registered Google generateContent backend turn."
  (chat-google bot input conversation callback
               :file-attachments file-attachments
               :effective-model effective-model
               :effective-generation-config effective-generation-config
               :return-turn-result-p t
               :bypass-cache-p (and effective-model t)))

(register-chat-backend :google #'google-chat-backend-handler)
