;;; -*- Lisp -*-
;;; payloads.lisp - role normalization and provider payload builders

(in-package "CHATBOT")

(defun assistant-like-role-p (role)
  "Returns true when ROLE is an assistant/model response role."
  (and role
       (member (string-downcase role) '("assistant" "model") :test #'string=)))
