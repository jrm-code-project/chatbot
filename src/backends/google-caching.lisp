;;; -*- Lisp -*-
;;; google-caching.lisp - explicit Gemini cachedContents bindings and policy helpers

(in-package "CHATBOT")

(defun google-explicit-content-cache-supported-p (bot)
  "Returns true when BOT's backend supports explicit Gemini cachedContents usage."
  (eq (chatbot-backend bot) :google))

(defun google-content-cache-model-resource-name (model)
  "Returns MODEL normalized to the cachedContents resource-name format."
  (let ((resolved-model (require-non-empty-string model "Cached-content model")))
    (if (alexandria:starts-with-subseq "models/" resolved-model)
        resolved-model
        (format nil "models/~A" resolved-model))))

(defun google-content-cache-url (&optional cached-content-name)
  "Returns the cachedContents collection URL or one specific CACHED-CONTENT-NAME URL."
  (let ((collection-url (concatenate 'string *gemini-base-url* "/cachedContents")))
    (cond
      ((null cached-content-name) collection-url)
      ((or (alexandria:starts-with-subseq "cachedContents/" cached-content-name)
           (alexandria:starts-with-subseq "projects/" cached-content-name))
       (format nil "~A/~A" *gemini-base-url* cached-content-name))
      (t
       (format nil "~A/~A" collection-url cached-content-name)))))

(defun google-content-cache-headers (api-key)
  "Returns the standard cachedContents request headers."
  (list (cons "x-goog-api-key" api-key)
        (cons "Content-Type" "application/json")))

(defun google-content-cache-api-key-or-error ()
  "Returns the configured Gemini API key for cachedContents calls."
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    api-key))

(defun format-google-content-cache-ttl (ttl-seconds)
  "Returns TTL-SECONDS formatted for the cachedContents API."
  (format nil "~As"
          (normalize-content-cache-ttl-seconds ttl-seconds)))

(defun generate-content-message-text-token-count (message)
  "Returns an estimated token count for one generateContent-format MESSAGE."
  (or (loop for part in (let ((parts (cdr (assoc "parts" message :test #'string=))))
                          (if (vectorp parts)
                              (coerce parts 'list)
                              parts))
            for text = (cdr (assoc "text" part :test #'string=))
            when (and text (stringp text))
              sum (estimate-text-token-count text))
      0))

(defun google-cacheable-prefix-contents (conversation)
  "Returns the reusable generateContent prefix contents for CONVERSATION."
  (let* ((bot (conversation-chatbot conversation)))
    (generate-content-cacheable-prefix-contents
     bot
     (conversation-persona-memory conversation)
     (conversation-persona-diary-entries conversation))))

(defun google-cacheable-prefix-token-count (conversation)
  "Returns the estimated token count of CONVERSATION's reusable explicit-cache prefix."
  (let* ((bot (conversation-chatbot conversation))
         (system-instruction-tokens
           (estimate-optional-text-token-count
            (system-instruction-text (chatbot-system-instruction bot))))
         (content-tokens
           (or (loop for message in (google-cacheable-prefix-contents conversation)
                     sum (generate-content-message-text-token-count message))
               0)))
    (+ (or system-instruction-tokens 0)
       content-tokens)))

(defun google-cacheable-prefix-descriptor (conversation &key effective-model)
  "Returns the explicit-cache descriptor for CONVERSATION, or NIL when caching should be skipped."
  (let* ((bot (conversation-chatbot conversation))
         (policy (chatbot-content-cache-policy bot))
         (prefix-contents (google-cacheable-prefix-contents conversation))
         (system-instruction (chatbot-system-instruction bot))
         (gemini-tools (generate-content-request-tools bot))
         (estimated-tokens (google-cacheable-prefix-token-count conversation))
         (minimum-tokens (or (chatbot-content-cache-min-tokens bot)
                             *default-content-cache-min-tokens*)))
    (when (and (google-explicit-content-cache-supported-p bot)
               (eq policy :auto)
               (or system-instruction prefix-contents)
               (>= estimated-tokens minimum-tokens))
      (let* ((model-name (google-content-cache-model-resource-name
                          (or effective-model
                              (chatbot-model bot))))
             (body
               (append (list (cons "model" model-name))
                       (when prefix-contents
                         (list (cons "contents" (coerce prefix-contents 'vector))))
                       (when system-instruction
                         (list (cons "systemInstruction"
                                     (list (cons "parts"
                                                 (system-instruction-text-parts system-instruction))))))
                       (when gemini-tools
                         (list (cons "tools" gemini-tools)))))
             (fingerprint (cl-json:encode-json-to-string body)))
        (list :body body
              :fingerprint fingerprint
              :estimated-tokens estimated-tokens)))))

(defun cached-content-response-name (response)
  "Returns the resource name from one decoded cachedContents RESPONSE."
  (or (cdr (assoc :name response))
      (cdr (assoc "name" response :test #'string=))))

(defun create-google-content-cache (conversation &key effective-model)
  "Creates one explicit Gemini cachedContents resource for CONVERSATION."
  (let* ((descriptor (or (google-cacheable-prefix-descriptor conversation
                                                             :effective-model effective-model)
                         (error "Conversation does not have an eligible reusable prefix to cache.")))
         (bot (conversation-chatbot conversation))
         (api-key (google-content-cache-api-key-or-error))
         (ttl-seconds (or (chatbot-content-cache-ttl-seconds bot)
                          *default-content-cache-ttl-seconds*))
         (payload (append (copy-tree (getf descriptor :body))
                          (list (cons "ttl" (format-google-content-cache-ttl ttl-seconds))))))
    (multiple-value-bind (response-body status)
        (post-web-request (google-content-cache-url)
                          (google-content-cache-headers api-key)
                          (cl-json:encode-json-to-string payload))
      (unless (member status '(200 201))
        (error "Cached-content create responded with HTTP status ~A" status))
      (let ((response (cl-json:decode-json-from-string response-body)))
        (unless (cached-content-response-name response)
          (error "Cached-content create response did not include a resource name."))
        response))))

(defun get-google-content-cache (cached-content-name)
  "Returns the decoded cachedContents resource named CACHED-CONTENT-NAME."
  (let ((api-key (google-content-cache-api-key-or-error)))
    (multiple-value-bind (response-body status)
        (get-web-request (google-content-cache-url cached-content-name)
                         :headers (google-content-cache-headers api-key))
      (unless (= status 200)
        (error "Cached-content get responded with HTTP status ~A" status))
      (cl-json:decode-json-from-string response-body))))

(defun list-google-content-caches (&key page-size page-token)
  "Returns the decoded cachedContents list response."
  (let* ((api-key (google-content-cache-api-key-or-error))
         (query-parts
           (remove nil
                   (list (when page-size
                           (format nil "pageSize=~A" page-size))
                         (when page-token
                           (format nil "pageToken=~A" page-token)))))
         (url (if query-parts
                  (format nil "~A?~{~A~^&~}" (google-content-cache-url) query-parts)
                  (google-content-cache-url))))
    (multiple-value-bind (response-body status)
        (get-web-request url
                         :headers (google-content-cache-headers api-key))
      (unless (= status 200)
        (error "Cached-content list responded with HTTP status ~A" status))
      (cl-json:decode-json-from-string response-body))))

(defun update-google-content-cache-ttl (cached-content-name ttl-seconds)
  "Updates CACHED-CONTENT-NAME with a new TTL and returns the decoded resource."
  (let* ((api-key (google-content-cache-api-key-or-error))
         (url (format nil "~A?updateMask=ttl"
                      (google-content-cache-url cached-content-name)))
         (payload (cl-json:encode-json-to-string
                   (list (cons "ttl" (format-google-content-cache-ttl ttl-seconds))))))
    (multiple-value-bind (response-body status)
        (patch-web-request url
                           (google-content-cache-headers api-key)
                           payload)
      (unless (= status 200)
        (error "Cached-content update responded with HTTP status ~A" status))
      (cl-json:decode-json-from-string response-body))))

(defun delete-google-content-cache (cached-content-name)
  "Deletes CACHED-CONTENT-NAME and returns true when the API succeeds."
  (let ((api-key (google-content-cache-api-key-or-error)))
    (multiple-value-bind (response-body status)
        (delete-web-request (google-content-cache-url cached-content-name)
                            :headers (google-content-cache-headers api-key))
      (declare (ignore response-body))
      (unless (member status '(200 204))
        (error "Cached-content delete responded with HTTP status ~A" status))
      t)))

(defun ensure-google-conversation-content-cache (conversation &key effective-model)
  "Returns CONVERSATION's explicit cached-content name, creating it when policy requires."
  (let* ((descriptor (google-cacheable-prefix-descriptor conversation
                                                         :effective-model effective-model))
         (existing-name (conversation-cached-content-name conversation))
         (existing-key (conversation-cached-content-key conversation)))
    (when descriptor
      (if (and existing-name
               existing-key
               (string= existing-key (getf descriptor :fingerprint)))
          existing-name
          (let* ((response (create-google-content-cache conversation
                                                        :effective-model effective-model))
                 (name (cached-content-response-name response)))
            (setf (conversation-cached-content-name conversation) name)
            (setf (conversation-cached-content-key conversation)
                  (getf descriptor :fingerprint))
            (setf (conversation-cached-content-metadata conversation) response)
            (log-message :info "Created explicit Gemini content cache"
                         :context `(("name" . ,name)
                                    ("estimated-prefix-tokens" . ,(princ-to-string (getf descriptor :estimated-tokens)))))
            name)))))
