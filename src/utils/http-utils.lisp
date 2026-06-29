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

(defun post-web-request (url headers content &key want-stream)
  "Logs and sends an outbound HTTP POST request."
  (let ((connect-timeout (current-http-connect-timeout))
        (read-timeout (current-http-read-timeout)))
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

(defun get-web-request (url &key headers want-stream)
  "Logs and sends an outbound HTTP GET request."
  (let ((connect-timeout (current-http-connect-timeout))
        (read-timeout (current-http-read-timeout)))
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
