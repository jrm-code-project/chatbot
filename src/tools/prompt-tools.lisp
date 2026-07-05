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

(defun read-file-forms-to-user-prompts (pathname &key callback runtime-context)
  "Returns a list of (PROMPT FORM) lists for every readable top-level form in PATHNAME."
  (mapcar (lambda (form-text)
            (list (common-lisp-code-to-user-prompt form-text
                                                   :callback callback
                                                   :runtime-context runtime-context)
                  form-text))
          (read-file-forms-as-text pathname)))

(defun format-training-example (user-prompt assistant-code)
  "Formats USER-PROMPT and ASSISTANT-CODE as a single-line ChatML JSON string."
  (cl-json:encode-json-to-string
   `((:messages . (((:role . "user")
                    (:content . ,(require-non-empty-string user-prompt "USER-PROMPT")))
                   ((:role . "assistant")
                    (:content . ,(require-non-empty-string assistant-code "ASSISTANT-CODE"))))))))

(defun read-file-forms-to-training-examples (pathname &key callback runtime-context)
  "Returns ChatML JSON training examples for every readable top-level form in PATHNAME."
  (mapcar (lambda (prompt-and-form)
            (format-training-example (first prompt-and-form)
                                     (second prompt-and-form)))
          (read-file-forms-to-user-prompts pathname
                                           :callback callback
                                           :runtime-context runtime-context)))

(defun append-file-forms-to-training-examples (source-pathname master-pathname
                                               &key callback runtime-context)
  "Appends training examples from SOURCE-PATHNAME to MASTER-PATHNAME and returns the new examples."
  (let ((examples (read-file-forms-to-training-examples source-pathname
                                                        :callback callback
                                                        :runtime-context runtime-context)))
    (with-open-file (stream master-pathname
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (dolist (example examples)
        (write-line example stream)))
    examples))

(defun lisp-source-file-p (pathname)
  "Returns true when PATHNAME names a Lisp source file handled by training export."
  (let ((type (pathname-type pathname)))
    (and type
         (string-equal type "lisp"))))

(defun append-directory-lisp-files-to-training-examples (directory-pathname master-pathname
                                                         &key callback runtime-context)
  "Recursively appends training examples for all Lisp files under DIRECTORY-PATHNAME to MASTER-PATHNAME."
  (mapcan (lambda (pathname)
            (append-file-forms-to-training-examples pathname
                                                    master-pathname
                                                    :callback callback
                                                    :runtime-context runtime-context))
          (remove-if-not #'lisp-source-file-p
                         (expand-chat-input-directory-files directory-pathname))))

(defun append-wild-lisp-files-to-training-examples (wild-pathname master-pathname
                                                    &key callback runtime-context)
  "Appends training examples for Lisp files matching WILD-PATHNAME to MASTER-PATHNAME."
  (mapcan (lambda (pathname)
            (append-file-forms-to-training-examples pathname
                                                    master-pathname
                                                    :callback callback
                                                    :runtime-context runtime-context))
          (remove-if-not #'lisp-source-file-p
                         (expand-chat-input-file-spec wild-pathname))))
