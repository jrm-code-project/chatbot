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
       (string-equal (princ-to-string stop-reason) "MALFORMED_RESPONSE")))

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
                                              effective-generation-config
                                              return-turn-result-p
                                              (recursion-depth 0))
  "Resubmits the current turn through the Google backend on gemini-pro-latest."
  (declare (ignore request-contents history-messages))
  (chat-google bot
               input
               conversation
               callback
               :file-attachments file-attachments
               :effective-model +google-gemini-model-override-model+
               :effective-generation-config effective-generation-config
               :malformed-response-fallback-attempted-p t
               :return-turn-result-p return-turn-result-p
               :recursion-depth recursion-depth))

(defun google-request-state (bot input conversation file-attachments effective-model effective-generation-config
                              &key request-contents history-messages malformed-response-fallback-attempted-p)
  "Builds the provider-runner state for a Google generateContent turn."
  (let* ((current-messages (conversation-messages conversation))
         (persona-memory (conversation-persona-memory conversation))
         (persona-diary-entries (conversation-persona-diary-entries conversation)))
    (list :input input
          :file-attachments file-attachments
          :effective-model effective-model
          :effective-generation-config effective-generation-config
          :malformed-response-fallback-attempted-p malformed-response-fallback-attempted-p
          :history-messages (or history-messages
                              (stateless-history-messages current-messages input))
          :request-contents (or request-contents
                               (build-generate-content-request-contents current-messages
                                                                        input
                                                                        :chatbot bot
                                                                        :persona-memory persona-memory
                                                                        :persona-diary-entries persona-diary-entries
                                                                        :file-attachments file-attachments
                                                                        :effective-model effective-model)))))

(defun google-model-override-active-p (bot effective-model)
  "Returns true when the malformed-response fallback override model is already active."
  (string-equal (or effective-model
                   (chatbot-model bot))
                +google-gemini-model-override-model+))

(defun submit-google-turn (bot response-body-parser state)
  "Submits one Google generateContent turn and returns a normalized outcome."
  (declare (ignore response-body-parser))
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    (let* ((system-inst (chatbot-system-instruction bot))
           (request-contents (getf state :request-contents))
           (contents (coerce request-contents 'vector))
           (effective-model (getf state :effective-model))
           (effective-generation-config (getf state :effective-generation-config))
           (gemini-tools (generate-content-request-tools bot))
           (payload-alist (list (cons "contents" contents)))
           (url (concatenate 'string
                            *gemini-base-url*
                            "/models/"
                            (generate-content-model-name (or effective-model
                                                             (chatbot-model bot)))
                            ":generateContent"))
           (headers (list (cons "x-goog-api-key" api-key)
                         (cons "Content-Type" "application/json"))))
      (when system-inst
        (setf payload-alist
              (append payload-alist
                     (list (cons "systemInstruction"
                                 (list (cons "parts"
                                             (system-instruction-text-parts system-inst))))))))
      (when gemini-tools
        (setf payload-alist
              (append payload-alist (list (cons "tools" gemini-tools)))))
      (when (or (getf effective-generation-config :temperature)
                (getf effective-generation-config :top-p))
        (setf payload-alist
              (append payload-alist
                     (list (cons "generationConfig"
                                 (remove nil
                                         (list (when (getf effective-generation-config :temperature)
                                                 (cons "temperature" (getf effective-generation-config :temperature)))
                                               (when (getf effective-generation-config :top-p)
                                                 (cons "topP" (getf effective-generation-config :top-p))))))))))
      (let ((payload-json (cl-json:encode-json-to-string payload-alist)))
        (multiple-value-bind (response-body status)
            (post-web-request url headers payload-json)
          (unless (= status 200)
            (error "API responded with HTTP status ~A" status))
          (let* ((response-alist (cl-json:decode-json-from-string response-body))
                 (usage (cdr (assoc :usage-metadata response-alist)))
                 (response-id (cdr (assoc :response-id response-alist)))
                 (model-version (cdr (assoc :model-version response-alist)))
                 (candidates (cdr (assoc :candidates response-alist)))
                 (first-candidate (car candidates))
                 (finish-reason (cdr (assoc :finish-reason first-candidate)))
                 (content (cdr (assoc :content first-candidate)))
                 (parts (cdr (assoc :parts content)))
                 (function-call-part (find-if #'google-part-function-call parts))
                 (fn-call (and function-call-part
                              (google-part-function-call function-call-part)))
                 (thought-signature (and function-call-part
                                        (google-part-thought-signature function-call-part)))
                 (thought-text (join-google-part-texts
                               (remove-if-not #'google-part-thought-p parts)))
                 (final-str (join-google-part-texts
                            (remove-if #'google-part-thought-p parts))))
            (log-backend-response-stats
             :google
             :http-status status
             :response-id response-id
             :model model-version
             :finish-reason finish-reason
             :usage usage)
            (cond
              ((and (not (getf state :malformed-response-fallback-attempted-p))
                   (not (google-model-override-active-p bot effective-model))
                   (malformed-response-stop-reason-p finish-reason))
               (make-provider-turn-retry-outcome :reason :malformed-response))
              (fn-call
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
                  :request-contents request-contents
                  :file-attachments (getf state :file-attachments)
                  :effective-model effective-model
                  :effective-generation-config effective-generation-config
                  :malformed-response-fallback-attempted-p
                  (getf state :malformed-response-fallback-attempted-p))))
              ((and (or (null final-str)
                       (string= final-str ""))
                   (not (getf state :malformed-response-fallback-attempted-p))
                   (not (google-model-override-active-p bot effective-model)))
               (make-provider-turn-retry-outcome :reason :empty-response))
              ((or (null final-str)
                   (string= final-str ""))
               (error "No text returned from Gemini API response: ~A" response-body))
              (t
               (make-provider-turn-final-outcome final-str
                                                :usage usage
                                                :thought-text thought-text)))))))))

(defun chat-google (bot input conversation callback
                   &key file-attachments request-contents history-messages effective-model effective-generation-config
                     malformed-response-fallback-attempted-p return-turn-result-p
                     (recursion-depth 0))
  "Sends user input to the active conversation using Google's non-streaming generateContent API."
  (let ((result
          (run-provider-turn-loop
           :google
           (google-request-state bot input conversation file-attachments effective-model effective-generation-config
                                :request-contents request-contents
                                :history-messages history-messages
                                :malformed-response-fallback-attempted-p malformed-response-fallback-attempted-p)
           (lambda (state current-depth)
             (declare (ignore current-depth))
             (submit-google-turn bot nil state))
           :retry-turn
           (lambda (state outcome current-depth step)
             (declare (ignore outcome step))
             (retry-on-google-gemini-pro-latest
              bot
              (getf state :input)
              conversation
              callback
              :file-attachments (getf state :file-attachments)
              :request-contents (getf state :request-contents)
              :history-messages (getf state :history-messages)
              :effective-generation-config (getf state :effective-generation-config)
              :return-turn-result-p t
              :recursion-depth current-depth))
           :continue-with-tools
           (lambda (state outcome next-depth step)
             (declare (ignore state))
             (continue-stateless-provider-tool-recursion
              bot
              (getf outcome :history-messages)
              (provider-turn-outcome-tool-calls outcome)
              (lambda (name tool-call)
                (declare (ignore tool-call))
                (format nil "Google tool arguments for ~A" name))
              (lambda (id name args-str res-text tool-call)
                (declare (ignore id args-str))
                (let ((response-payload `(("result" . ,res-text))))
                  `(("role" . "user")
                   ("parts" . ,(vector
                                (list (cons "functionResponse"
                                            `(("name" . ,name)
                                              ("response" . ,response-payload)))))))))
              (lambda (tool-calls tool-results)
                (let* ((tool-call (car tool-calls))
                      (name (cdr (assoc :name tool-call)))
                      (raw-args (cdr (assoc :raw-args tool-call)))
                      (payload-args (if raw-args
                                        (json-encodable-value raw-args)
                                        (empty-json-object)))
                      (thought-signature (cdr (assoc :thought-signature tool-call)))
                      (model-msg `(("role" . "model")
                                   ("parts" . ,(vector
                                                (append
                                                 `(("functionCall" . (("name" . ,name) ("args" . ,payload-args))))
                                                 (when thought-signature
                                                   `(("thoughtSignature" . ,thought-signature)))))))))
                  (append (list model-msg)
                         tool-results)))
              (lambda (recursive-history recursion-messages)
                (funcall step
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
                         :malformed-response-fallback-attempted-p
                         (getf outcome :malformed-response-fallback-attempted-p))
                        next-depth))
              :error-builder
              (lambda (id name args-str condition tool-call)
                (declare (ignore id args-str tool-call))
                (let ((response-payload (chatbot-tool-error-payload name condition)))
                  `(("role" . "user")
                   ("parts" . ,(vector
                                (list (cons "functionResponse"
                                            `(("name" . ,name)
                                              ("response" . ,response-payload)))))))))))
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
             (declare (ignore state current-depth))
             (error "Google Chat Error: ~A" condition))
           :initial-recursion-depth recursion-depth)))
    (if return-turn-result-p
        result
        (apply-chat-turn-result result conversation))))
