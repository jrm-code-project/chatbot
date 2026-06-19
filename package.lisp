;;; -*- Lisp -*-

(defpackage "CHATBOT"
  (:shadowing-import-from "FUNCTION" "COMPOSE")
  (:shadowing-import-from "NAMED-LET" "LET" "NAMED-LAMBDA")
  (:shadowing-import-from "SERIES" "DEFUN" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:use "ALEXANDRIA" "CL" "FOLD" "FUNCTION" "JSONX" "NAMED-LET" "PROMISE" "SERIES" "TRIVIAL-TIMEOUT")
  (:export "CHAT"
           "NEW-CHAT"
           "NEW-CHAT-PERSONA"
           "*DEFAULT-CONVERSATION*"
           "*OPENAI-BASE-URL*"
           "*OPENAI-API-KEY*"
           "OPENAI-API-KEY"
           "*LM-STUDIO-BASE-URL*"
           "*LM-STUDIO-API-KEY*"
           "LM-STUDIO-API-KEY"))

