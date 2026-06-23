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
    (declare (ignore content))
    (log-message :info "HTTP POST request"
                 :context `(("url" . ,(sanitize-url-for-log url))
                            ("headers" . ,(cl-json:encode-json-to-string
                                           (sanitize-headers-for-log headers)))
                            ("connect-timeout" . ,(princ-to-string connect-timeout))
                            ("read-timeout" . ,(princ-to-string read-timeout))
                            ("want-stream" . ,(if want-stream "true" "false"))))
    (if want-stream
        (funcall *http-post-function* url
                 :headers headers
                 :content content
                 :connect-timeout connect-timeout
                 :read-timeout read-timeout
                 :want-stream t)
        (funcall *http-post-function* url
                 :headers headers
                 :content content
                 :connect-timeout connect-timeout
                 :read-timeout read-timeout))))
