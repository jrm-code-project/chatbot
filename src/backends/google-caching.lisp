;;; -*- Lisp -*-
;;; google-caching.lisp - explicit Gemini cachedContents bindings and policy helpers

(in-package "CHATBOT")

(defparameter *google-content-cache-stale-refresh-fraction* 0.25d0
  "Fraction of a cache TTL remaining below which Google explicit caches are considered stale.")

(defparameter *google-content-cache-stale-refresh-min-remaining-seconds* 600
  "Minimum remaining lifetime that marks a Google explicit cache as stale enough to refresh.")

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

(defun canonical-content-cache-fingerprint-value (value)
  "Returns a stable Lisp representation of VALUE suitable for cache fingerprinting."
  (cond
    ((hash-table-p value)
     (list :object
           (sort (let (entries)
                   (maphash (lambda (key nested-value)
                              (push (cons (json-key-string key)
                                          (canonical-content-cache-fingerprint-value nested-value))
                                    entries))
                            value)
                   entries)
                 #'string<
                 :key #'car)))
    ((json-object-alist-p value)
     (list :object
           (sort (mapcar (lambda (entry)
                           (cons (json-key-string (car entry))
                                 (canonical-content-cache-fingerprint-value (cdr entry))))
                         value)
                 #'string<
                 :key #'car)))
    ((vectorp value)
     (list :array
           (map 'list #'canonical-content-cache-fingerprint-value value)))
    ((listp value)
     (list :array
           (mapcar #'canonical-content-cache-fingerprint-value value)))
    (t value)))

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
             (fingerprint (prin1-to-string
                           (canonical-content-cache-fingerprint-value body))))
        (list :body body
              :fingerprint fingerprint
              :estimated-tokens estimated-tokens)))))

(defun cached-content-response-name (response)
  "Returns the resource name from one decoded cachedContents RESPONSE."
  (or (cdr (assoc :name response))
      (cdr (assoc "name" response :test #'string=))))

(defun cached-content-response-field (response key)
  "Returns KEY from one decoded cachedContents RESPONSE."
  (or (cdr (assoc key response))
      (cdr (assoc (string-downcase (symbol-name key)) response :test #'string=))
      (cdr (assoc (case key
                    (:expire-time "expireTime")
                    (:update-time "updateTime")
                    (:create-time "createTime")
                    (t (string-capitalize (string-downcase (symbol-name key)))))
                  response
                  :test #'string=))))

(defun parse-google-cache-duration-seconds (value)
  "Returns VALUE parsed as a cache TTL in seconds when possible."
  (typecase value
    (null nil)
    (integer value)
    (real (truncate value))
    (string
     (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) value)))
       (cond
         ((string= trimmed "") nil)
         ((alexandria:ends-with-subseq "s" trimmed)
          (truncate (read-from-string (subseq trimmed 0 (1- (length trimmed))))))
         (t
          (truncate (read-from-string trimmed))))))
    (t nil)))

(defun parse-rfc3339-universal-time (timestamp)
  "Returns TIMESTAMP parsed from a basic RFC 3339 string, or NIL."
  (when (and (stringp timestamp)
             (>= (length timestamp) 20))
    (let* ((fractional-start (position #\. timestamp :start 19))
           (timezone-start (or (position #\Z timestamp :start 19)
                               (position #\+ timestamp :start 19)
                               (position #\- timestamp :start 19)))
           (timezone-start (or timezone-start (length timestamp)))
           (core (subseq timestamp 0 19))
           (timezone-fragment
             (subseq timestamp
                     (or (and fractional-start
                              (< fractional-start timezone-start)
                              timezone-start)
                         timezone-start))))
      (flet ((segment (start end)
               (parse-integer core :start start :end end)))
        (let* ((year (segment 0 4))
               (month (segment 5 7))
               (day (segment 8 10))
               (hour (segment 11 13))
               (minute (segment 14 16))
               (second (segment 17 19))
               (timezone
                 (cond
                   ((or (string= timezone-fragment "")
                        (string= timezone-fragment "Z"))
                    0)
                   ((>= (length timezone-fragment) 6)
                    (let* ((sign (char timezone-fragment 0))
                           (offset-hours (parse-integer timezone-fragment :start 1 :end 3))
                           (offset-minutes (parse-integer timezone-fragment :start 4 :end 6))
                           (offset (+ offset-hours (/ offset-minutes 60.0d0))))
                      (case sign
                        (#\+ (- offset))
                        (#\- offset)
                        (t 0))))
                   (t 0))))
          (encode-universal-time second minute hour day month year timezone))))))

(defun google-content-cache-ttl-seconds (conversation metadata)
  "Returns the configured or reported TTL in seconds for CONVERSATION's cached content."
  (or (parse-google-cache-duration-seconds
       (cached-content-response-field metadata :ttl))
      (chatbot-content-cache-ttl-seconds (conversation-chatbot conversation))
      *default-content-cache-ttl-seconds*))

(defun google-content-cache-stale-threshold-seconds (conversation metadata)
  "Returns the remaining-lifetime threshold under which cached content should refresh."
  (let ((ttl-seconds (google-content-cache-ttl-seconds conversation metadata)))
    (max *google-content-cache-stale-refresh-min-remaining-seconds*
         (floor (* ttl-seconds *google-content-cache-stale-refresh-fraction*)))))

(defun google-content-cache-somewhat-stale-p (conversation metadata)
  "Returns true when METADATA says CONVERSATION's cache is approaching expiry."
  (let ((expire-time (parse-rfc3339-universal-time
                      (cached-content-response-field metadata :expire-time))))
    (when expire-time
      (<= (- expire-time (get-universal-time))
          (google-content-cache-stale-threshold-seconds conversation metadata)))))

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

(defun google-content-cache-missing-text-p (text)
  "Returns true when TEXT describes an already-absent cachedContents resource."
  (let ((message (string-downcase (or text ""))))
    (or (search "cachedcontent not found" message)
       (search "cached content not found" message)
       (search "\"message\": \"cachedcontent not found" message)
       (and (search "permission_denied" message)
            (search "not found" message))
       (and (search "permission denied" message)
            (search "not found" message)))))

(defun google-content-cache-missing-condition-p (condition)
  "Returns true when CONDITION represents an already-absent cachedContents resource."
  (google-content-cache-missing-text-p (princ-to-string condition)))

(defun delete-google-content-cache (cached-content-name &key missing-ok-p)
  "Deletes CACHED-CONTENT-NAME and returns true when the API succeeds.
When MISSING-OK-P is true, already-absent cache responses are tolerated,
including Google cachedContents DELETE replies that surface the miss as 404
or as 403/PERMISSION_DENIED."
  (let ((api-key (google-content-cache-api-key-or-error)))
    (handler-case
       (multiple-value-bind (response-body status)
           (delete-web-request (google-content-cache-url cached-content-name)
                               :headers (google-content-cache-headers api-key))
         (unless (or (member status '(200 204))
                     (and missing-ok-p
                          (or (= status 404)
                              (and (= status 403)
                                   (google-content-cache-missing-text-p response-body)))))
           (error "Cached-content delete responded with HTTP status ~A" status))
         t)
      (error (condition)
       (if (and missing-ok-p
                (google-content-cache-missing-condition-p condition))
           t
           (error condition))))))

(defun clear-google-conversation-content-cache-state (conversation)
  "Clears CONVERSATION's remembered explicit cached-content state."
  (setf (conversation-cached-content-name conversation) nil)
  (setf (conversation-cached-content-key conversation) nil)
  (setf (conversation-cached-content-metadata conversation) nil)
  conversation)

(defun ensure-google-conversation-content-cache (conversation &key effective-model)
  "Returns CONVERSATION's explicit cached-content name, creating it when policy requires."
  (let* ((descriptor (google-cacheable-prefix-descriptor conversation
                                                         :effective-model effective-model))
         (existing-name (conversation-cached-content-name conversation))
         (existing-key (conversation-cached-content-key conversation))
         (existing-metadata (conversation-cached-content-metadata conversation)))
    (when descriptor
      (if (and existing-name
              existing-key
              (string= existing-key (getf descriptor :fingerprint))
              (not (google-content-cache-somewhat-stale-p conversation existing-metadata)))
         existing-name
         (let ((replacement-p (and existing-name t)))
           (when existing-name
             (delete-google-content-cache existing-name :missing-ok-p t)
             (clear-google-conversation-content-cache-state conversation))
           (let* ((response (create-google-content-cache conversation
                                                         :effective-model effective-model))
                  (name (cached-content-response-name response)))
             (setf (conversation-cached-content-name conversation) name)
             (setf (conversation-cached-content-key conversation)
                   (getf descriptor :fingerprint))
             (setf (conversation-cached-content-metadata conversation) response)
             (log-message :info
                          (if replacement-p
                              "Replaced explicit Gemini content cache"
                              "Created explicit Gemini content cache")
                          :context `(("name" . ,name)
                                     ("estimated-prefix-tokens" . ,(princ-to-string (getf descriptor :estimated-tokens)))))
             name))))))
