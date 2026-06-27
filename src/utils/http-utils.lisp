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

(defun post-web-request (url headers content &key want-stream)
  "Logs and sends an outbound HTTP POST request."
  (let ((connect-timeout (current-http-connect-timeout))
        (read-timeout (current-http-read-timeout)))
    (log-message :info "HTTP POST request"
                 :context `(("url" . ,(sanitize-url-for-log url))))
    (let ((http-post-function (current-http-post-function)))
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
                 :read-timeout read-timeout)))))

(defun get-web-request (url &key headers want-stream)
  "Logs and sends an outbound HTTP GET request."
  (let ((connect-timeout (current-http-connect-timeout))
        (read-timeout (current-http-read-timeout)))
    (log-message :info "HTTP GET request"
                :context `(("url" . ,(sanitize-url-for-log url))))
    (let ((http-get-function (current-http-get-function)))
      (if want-stream
         (funcall http-get-function url
                  :headers headers
                  :connect-timeout connect-timeout
                  :read-timeout read-timeout
                  :want-stream t)
         (funcall http-get-function url
                  :headers headers
                  :connect-timeout connect-timeout
                  :read-timeout read-timeout)))))
