;;; -*- Lisp -*-
;;; payloads.lisp - role normalization and provider payload builders

(in-package "CHATBOT")

(defun assistant-like-role-p (role)
  "Returns true when ROLE is an assistant/model response role."
  (and role
       (member (string-downcase role) '("assistant" "model") :test #'string=)))

(defun resolve-effective-generation-config (chatbot &key (temperature nil temperaturep) (top-p nil top-pp))
  "Returns the effective generation config plist for CHATBOT and this turn."
  (let* ((default-parameters (and chatbot (sampling-parameters chatbot)))
         (resolved-temperature (if temperaturep
                                   (normalize-chatbot-temperature temperature :allow-nil-p t)
                                   (getf default-parameters :temperature)))
         (resolved-top-p (if top-pp
                             (normalize-chatbot-top-p top-p :allow-nil-p t)
                             (getf default-parameters :top-p))))
    (list :temperature resolved-temperature
          :top-p resolved-top-p)))
