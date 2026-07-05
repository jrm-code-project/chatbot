;;; -*- Lisp -*-
;;; prompt-tools.lisp - convenience helpers for prompt reconstruction tasks

(in-package "CHATBOT")

(defparameter +common-lisp-code-to-user-prompt-instructions+
  "You are an expert Common Lisp developer. You will be provided with a block of Common Lisp code (a function, macro, or class). Your task is to write the user prompt that would logically request this exact code. If the code contains a docstring or comments, use them to understand the intent, but DO NOT copy them verbatim into the prompt. The prompt should sound like a programmer asking a senior engineer for a specific implementation. Be precise about the required inputs, outputs, and any specific constraints (e.g., 'Make sure it is tail-recursive' or 'Avoid variable capture'). Return ONLY the generated prompt string."
  "Instructions sent by COMMON-LISP-CODE-TO-USER-PROMPT.")

(defun common-lisp-code-to-user-prompt-request (code)
  "Returns the specialized reconstruction request for Common Lisp CODE."
  (format nil "~A~%~%```commonlisp~%~A~%```"
          +common-lisp-code-to-user-prompt-instructions+
          (require-non-empty-string code "CODE")))

(defun common-lisp-code-to-user-prompt (code &key callback runtime-context)
  "Returns a user prompt reconstructed from Common Lisp CODE using gemini-flash-latest."
  (let ((conversation (new-chat :backend :gemini
                                :model "gemini-flash-latest"
                                :runtime-context runtime-context)))
    (chat (common-lisp-code-to-user-prompt-request code)
          :conversation conversation
          :callback callback)))
