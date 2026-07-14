;;; -*- Lisp -*-
;;; backend-llambda-loader.lisp - optional native llambda backend integration

(in-package "CHATBOT")

(defun load-optional-llambda-backend ()
  "Loads llambda's chatbot adapter when its ASDF system is discoverable."
  (let ((system (asdf:find-system "llambda/chatbot" nil)))
    (when system
      (load (asdf:system-relative-pathname system "chatbot-backend.lisp")))))

(load-optional-llambda-backend)
