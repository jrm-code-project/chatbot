;;; -*- Lisp -*-
;;; http-utils.lisp - outbound HTTP helpers

(in-package "CHATBOT")

(defun sensitive-header-name-p (name)
  "Returns true when NAME refers to a sensitive HTTP header."
  (member (string-downcase name)
          '("authorization" "x-goog-api-key" "api-key")
          :test #'string=))

(defun sanitize-url-for-log (url)
  "Redacts common API key query parameters from URL."
  (cl-ppcre:regex-replace-all
   "((?:\\?|&)(?:key|api_key|api-key)=)[^&]+"
   url
   "\\1[REDACTED]"))

(defun sanitize-headers-for-log (headers)
  "Redacts sensitive HTTP header values for logging."
  (mapcar (lambda (header)
            (let ((name (car header))
                  (value (cdr header)))
              (cons name
                    (if (sensitive-header-name-p name)
                        "[REDACTED]"
                        value))))
          headers))

(defun retryable-http-error-p (condition)
  "Returns true if CONDITION is a transient network or server error that is safe to retry."
  (let ((msg (string-downcase (princ-to-string condition))))
    (not (or (search "404" msg)
             (search "400" msg)
             (search "401" msg)
             (search "403" msg)
             (search "405" msg)))))

(defmacro with-exponential-backoff-retry ((&key (max-retries 3) (base-delay-ms 100)) &body body)
  "Executes BODY, retrying up to MAX-RETRIES times with exponential back-off on transient errors."
  (let ((retry-var (gensym "RETRY"))
        (delay-var (gensym "DELAY"))
        (err-var (gensym "ERR")))
    `(let ((,delay-var ,base-delay-ms))
       (loop for ,retry-var from 0 to ,max-retries
             do (handler-case
                    (return (progn ,@body))
                  (error (,err-var)
                    (if (and (< ,retry-var ,max-retries)
                             (retryable-http-error-p ,err-var))
                        (let ((sleep-secs (/ ,delay-var 1000.0)))
                          (log-message :warn "MCRS: HTTP call failed, retrying with back-off"
                                       :context `(("attempt" . ,(princ-to-string (1+ ,retry-var)))
                                                  ("error" . ,(princ-to-string ,err-var))
                                                  ("sleep-seconds" . ,(princ-to-string sleep-secs))))
                          (sleep sleep-secs)
                          (setf ,delay-var (* ,delay-var 2)))
                        (error ,err-var))))))))

(defun post-web-request (url headers content &key want-stream connect-timeout read-timeout)
  "Logs and sends an outbound HTTP POST request."
  (let ((connect-timeout (or connect-timeout
                             (current-http-connect-timeout)))
        (read-timeout (or read-timeout
                          (current-http-read-timeout))))
    (log-message :info "HTTP POST request"
                 :context `(("url" . ,(sanitize-url-for-log url))))
    (let ((http-post-function (current-http-post-function)))
      (with-exponential-backoff-retry (:max-retries 3 :base-delay-ms 100)
        (if want-stream
            (funcall http-post-function url
                     :headers headers
                     :content content
                     :connect-timeout connect-timeout
                     :read-timeout read-timeout
                     :want-stream t)
            (funcall http-post-function url
                     :headers headers
                     :content content
                     :connect-timeout connect-timeout
                     :read-timeout read-timeout))))))

(defun get-web-request (url &key headers want-stream connect-timeout read-timeout)
  "Logs and sends an outbound HTTP GET request."
  (let ((connect-timeout (or connect-timeout
                             (current-http-connect-timeout)))
        (read-timeout (or read-timeout
                          (current-http-read-timeout))))
    (log-message :info "HTTP GET request"
                 :context `(("url" . ,(sanitize-url-for-log url))))
    (let ((http-get-function (current-http-get-function)))
      (with-exponential-backoff-retry (:max-retries 3 :base-delay-ms 100)
        (if want-stream
            (funcall http-get-function url
                     :headers headers
                     :connect-timeout connect-timeout
                     :read-timeout read-timeout
                     :want-stream t)
            (funcall http-get-function url
                     :headers headers
                     :connect-timeout connect-timeout
                     :read-timeout read-timeout))))))

(defun patch-web-request (url headers content &key connect-timeout read-timeout)
  "Logs and sends an outbound HTTP PATCH request."
  (let ((connect-timeout (or connect-timeout
                            (current-http-connect-timeout)))
        (read-timeout (or read-timeout
                         (current-http-read-timeout))))
    (log-message :info "HTTP PATCH request"
                :context `(("url" . ,(sanitize-url-for-log url))))
    (let ((http-patch-function (current-http-patch-function)))
      (with-exponential-backoff-retry (:max-retries 3 :base-delay-ms 100)
        (funcall http-patch-function url
                :headers headers
                :content content
                :connect-timeout connect-timeout
                :read-timeout read-timeout)))))

(defun delete-web-request (url &key headers connect-timeout read-timeout)
  "Logs and sends an outbound HTTP DELETE request."
  (let ((connect-timeout (or connect-timeout
                             (current-http-connect-timeout)))
        (read-timeout (or read-timeout
                          (current-http-read-timeout))))
    (log-message :info "HTTP DELETE request"
                 :context `(("url" . ,(sanitize-url-for-log url))))
    (let ((http-delete-function (current-http-delete-function)))
      (with-exponential-backoff-retry (:max-retries 3 :base-delay-ms 100)
        (funcall http-delete-function url
                 :headers headers
                 :connect-timeout connect-timeout
                 :read-timeout read-timeout)))))

(defun classify-http-response (status body)
  "Classifies an HTTP response status and body, returning a reason keyword:
  - :not-found
  - :permission-denied
  - :invalid-argument
  - :other"
  (let ((status-num (or status 0))
        (body-str (string-downcase (or body ""))))
    (cond
      ((or (eql status-num 404)
           (search "cachedcontent not found" body-str)
           (search "cached content not found" body-str)
           (and (search "permission_denied" body-str) (search "not found" body-str))
           (and (search "permission denied" body-str) (search "not found" body-str)))
       :not-found)
      ((or (eql status-num 403)
           (search "permission_denied" body-str)
           (search "permission denied" body-str))
       :permission-denied)
      ((or (eql status-num 400)
           (search "invalid argument" body-str))
       :invalid-argument)
      (t :other))))

(defun classify-http-error (condition)
  "Analyzes CONDITION (which can be a Dexador HTTP error or generic Lisp error) and returns a plist:
  (:status <http-status> :body <body> :reason <reason-keyword>)."
  (let ((status nil)
        (body nil))
    (typecase condition
      (dexador:http-request-failed
       (setf status (dexador:response-status condition)
             body (dexador:response-body condition)))
      (error
       ;; Fallback: attempt to parse printed message if we don't have a direct Dexador condition object
       (let ((msg (string-downcase (princ-to-string condition))))
         (setf body msg)
         (cond
           ((search "404" msg) (setf status 404))
           ((search "403" msg) (setf status 403))
           ((search "400" msg) (setf status 400))))))

    (list :status status
          :body body
          :reason (classify-http-response status body))))
