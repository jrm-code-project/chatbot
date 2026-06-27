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
               :recursion-depth recursion-depth))

(defun chat-google (bot input conversation callback
                   &key file-attachments request-contents history-messages effective-model effective-generation-config
                     malformed-response-fallback-attempted-p (recursion-depth 0))
  "Sends user input to the active conversation using Google's non-streaming generateContent API."
  (ensure-chatbot-tool-recursion-depth :google recursion-depth)
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    (let* ((system-inst (chatbot-system-instruction bot))
           (current-messages (conversation-messages conversation))
           (persona-memory (conversation-persona-memory conversation))
           (persona-diary-entries (conversation-persona-diary-entries conversation))
           (history-messages (or history-messages
                                 (stateless-history-messages current-messages input)))
           (contents-list (or request-contents
                              (build-generate-content-request-contents current-messages
                                                                       input
                                                                       :chatbot bot
                                                                       :persona-memory persona-memory
                                                                       :persona-diary-entries persona-diary-entries
                                                                       :file-attachments file-attachments
                                                                       :effective-model effective-model)))
           (contents (coerce contents-list 'vector))
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
        (handler-case
            (funcall
             (lambda ()
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
                     ((and (not malformed-response-fallback-attempted-p)
                           (not (string-equal (or effective-model
                                                  (chatbot-model bot))
                                              +google-gemini-model-override-model+))
                           (malformed-response-stop-reason-p finish-reason))
                      (retry-on-google-gemini-pro-latest
                       bot
                       input
                       conversation
                       callback
                       :file-attachments file-attachments
                       :request-contents request-contents
                       :history-messages history-messages
                       :effective-generation-config effective-generation-config
                       :recursion-depth recursion-depth))
                     (fn-call
                      (let* ((name (cdr (assoc :name fn-call)))
                             (raw-args (cdr (assoc :args fn-call)))
                             (payload-args (if raw-args
                                               (json-encodable-value raw-args)
                                               (empty-json-object)))
                             (model-msg `(("role" . "model")
                                          ("parts" . ,(vector
                                                       (append
                                                        `(("functionCall" . (("name" . ,name) ("args" . ,payload-args))))
                                                        (when thought-signature
                                                          `(("thoughtSignature" . ,thought-signature))))))))
                             (response-payload
                               (handler-case
                                   `(("result" . ,(execute-chatbot-tool-by-name bot name raw-args)))
                                 (agentic-loop-interrupted (e)
                                   (error e))
                                 (error (condition)
                                   (chatbot-tool-error-payload name condition))))
                             (resp-msg `(("role" . "user")
                                         ("parts" . ,(vector
                                                      (list (cons "functionResponse"
                                                                  `(("name" . ,name)
                                                                    ("response" . ,response-payload))))))))
                             (recursion-messages (list model-msg resp-msg)))
                        (continue-stateless-tool-recursion
                         conversation
                         history-messages
                         recursion-messages
                         (lambda (recursive-history recursion-messages)
                           (chat-google bot
                                        nil
                                        conversation
                                        callback
                                        :history-messages recursive-history
                                        :request-contents (append contents-list recursion-messages)
                                        :file-attachments file-attachments
                                        :effective-model effective-model
                                        :effective-generation-config effective-generation-config
                                        :malformed-response-fallback-attempted-p malformed-response-fallback-attempted-p
                                        :recursion-depth (next-chatbot-tool-recursion-depth
                                                          :google
                                                          recursion-depth))))))
                     (t
                      (unless (and (stringp final-str)
                                   (string/= final-str ""))
                        (if (and (not malformed-response-fallback-attempted-p)
                                 (not (string-equal (or effective-model
                                                        (chatbot-model bot))
                                                    +google-gemini-model-override-model+)))
                            (return-from chat-google
                              (retry-on-google-gemini-pro-latest
                               bot
                               input
                               conversation
                               callback
                               :file-attachments file-attachments
                               :request-contents request-contents
                               :history-messages history-messages
                               :effective-generation-config effective-generation-config
                               :recursion-depth recursion-depth))
                            (error "No text returned from Gemini API response: ~A" response-body)))
                      (finish-stateless-text-turn conversation
                                                  history-messages
                                                  "model"
                                                  final-str
                                                  :callback callback
                                                  :usage usage
                                                  :thought-text thought-text)))))))
          (agentic-loop-interrupted (e)
            (error e))
          (chatbot-tool-recursion-limit-error (e)
            (error e))
          (error (e)
            (error "Google Chat Error: ~A" e)))))))
