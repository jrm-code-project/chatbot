;;; -*- Lisp -*-
;;; backend-google.lisp - Google generateContent flow

(in-package "CHATBOT")

(defun generate-content-model-name (model)
  "Returns MODEL in the form expected by the generateContent URL path."
  (if (and (stringp model)
           (alexandria:starts-with-subseq "models/" model))
      (subseq model (length "models/"))
      model))

(defun chat-google (bot input conversation callback)
  "Sends user input to the active conversation using Google's non-streaming generateContent API."
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    (let* ((system-inst (chatbot-system-instruction bot))
           (current-messages (conversation-messages conversation))
           (persona-memory (conversation-persona-memory conversation))
           (messages (build-request-history-messages current-messages input
                                                     :persona-memory persona-memory))
           (contents (coerce
                      (mapcar (lambda (msg)
                                (let ((role (cdr (assoc "role" msg :test #'string=)))
                                      (content (cdr (assoc "content" msg :test #'string=)))
                                      (parts (cdr (assoc "parts" msg :test #'string=))))
                                  (cond
                                    (parts
                                     (list (cons "role" (generate-content-role-for-message role))
                                           (cons "parts" parts)))
                                    (t
                                     (list (cons "role" (generate-content-role-for-message role))
                                           (cons "parts" (vector (list (cons "text" content)))))))))
                              messages)
                      'vector))
           (gemini-tools (generate-content-request-tools bot))
           (payload-alist (list (cons "contents" contents)))
           (url (concatenate 'string
                             *gemini-base-url*
                             "/models/"
                             (generate-content-model-name (chatbot-model bot))
                             ":generateContent?key="
                             api-key))
           (headers (list (cons "Content-Type" "application/json"))))
      (when system-inst
        (setf payload-alist
              (append payload-alist
                      (list (cons "systemInstruction"
                                  (list (cons "parts"
                                              (vector (list (cons "text" system-inst))))))))))
      (when gemini-tools
        (setf payload-alist (append payload-alist (list (cons "tools" gemini-tools)))))
      (let ((payload-json (cl-json:encode-json-to-string payload-alist)))
        (handler-case
            (multiple-value-bind (response-body status)
                (post-web-request url headers payload-json)
              (if (= status 200)
                  (let* ((response-alist (cl-json:decode-json-from-string response-body))
                         (usage (cdr (assoc :usage-metadata response-alist)))
                         (response-id (cdr (assoc :response-id response-alist)))
                         (model-version (cdr (assoc :model-version response-alist)))
                         (candidates (cdr (assoc :candidates response-alist)))
                         (first-candidate (car candidates))
                         (finish-reason (cdr (assoc :finish-reason first-candidate)))
                         (content (cdr (assoc :content first-candidate)))
                         (parts (cdr (assoc :parts content)))
                         (first-part (car parts))
                         (fn-call (or (cdr (assoc :function-call first-part))
                                      (cdr (assoc :function--call first-part))))
                         (thought-signature (or (cdr (assoc :thought-signature first-part))
                                                (cdr (assoc :thoughtSignature first-part :test #'string=))))
                         (final-str (cdr (assoc :text first-part))))
                    (log-backend-response-stats
                     :google
                     :http-status status
                     :response-id response-id
                     :model model-version
                     :finish-reason finish-reason
                     :usage usage)
                    (cond
                      (fn-call
                       (let* ((name (cdr (assoc :name fn-call)))
                              (args (cdr (assoc :args fn-call)))
                              (model-msg `(("role" . "model")
                                           ("parts" . ,(vector
                                                        (append
                                                         `(("functionCall" . (("name" . ,name) ("args" . ,args))))
                                                         (when thought-signature
                                                           `(("thoughtSignature" . ,thought-signature)))))))))
                         (multiple-value-bind (srv tool) (find-mcp-server-and-tool bot name)
                           (declare (ignore tool))
                           (unless srv
                             (error "MCP tool not found: ~A" name))
                           (let* ((res-text (execute-mcp-tool srv name args))
                                  (resp-msg `(("role" . "user")
                                              ("parts" . ,(vector `(("functionResponse" . (("name" . ,name) ("response" . (("result" . ,res-text)))))))))))
                             (setf (conversation-messages conversation)
                                   (append (append-user-input-to-conversation-messages current-messages input)
                                           (list model-msg resp-msg)))
                             (chat-google bot nil conversation callback)))))
                      (t
                       (unless final-str
                         (error "No text returned from Gemini API response: ~A" response-body))
                       (format-paragraphs final-str :width 80)
                       (write-turn-token-summary usage)
                       (when callback
                         (funcall callback final-str))
                       (setf (conversation-messages conversation)
                             (append (append-user-input-to-conversation-messages current-messages input)
                                     (list (list (cons "role" "model") (cons "content" final-str)))))
                       final-str)))
                  (error "API responded with HTTP status ~A" status)))
          (error (e)
            (error "Google Chat Error: ~A" e)))))))
