;;; -*- Lisp -*-
;;; google-payloads.lisp - Google generateContent payload translation

(in-package "CHATBOT")

(defun generate-content-role-for-message (role)
  "Normalizes a stored conversation ROLE for Google generateContent."
  (if (assistant-like-role-p role) "model" "user"))

(defun generate-content-live-user-message (chatbot input file-attachments)
  "Builds the transient current user message for generateContent requests."
  (let ((parts nil)
        (decorated-input (decorate-live-user-input chatbot input)))
    (when (and (stringp decorated-input)
               (string/= decorated-input ""))
      (push (list (cons "text" decorated-input)) parts))
    (dolist (attachment file-attachments)
      (push (make-generate-content-file-part attachment) parts))
    (if parts
        (list (cons "role" "user")
              (cons "parts" (coerce (nreverse parts) 'vector)))
        nil)))

(defun build-generate-content-request-contents (messages input &key chatbot persona-memory persona-diary-entries file-attachments)
  "Builds the Google generateContent contents list for the current turn."
  (let ((history (build-request-history-messages messages
                                                 nil
                                                 :chatbot chatbot
                                                 :persona-memory persona-memory
                                                 :persona-diary-entries persona-diary-entries))
        (current-user-message (generate-content-live-user-message chatbot input file-attachments)))
    (append
     (mapcar (lambda (msg)
               (let ((role (cdr (assoc "role" msg :test #'string=)))
                     (content (cdr (assoc "content" msg :test #'string=)))
                     (parts (cdr (assoc "parts" msg :test #'string=))))
                 (cond
                   (parts
                    (list (cons "role" (generate-content-role-for-message role))
                          (cons "parts" parts)))
                   (t
                    (list (cons "role" (generate-content-role-for-message role))
                          (cons "parts" (vector (list (cons "text" content)))))))))
             history)
     (when current-user-message
       (list current-user-message)))))

(defun generate-content-request-tools (chatbot)
  "Builds the Google generateContent tools payload for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
    (when mcp-tools
      (list `(("functionDeclarations" . ,(coerce
                                          (mapcar (lambda (pair)
                                                    (translate-mcp-tool-to-gemini-fn (cdr pair)))
                                                  mcp-tools)
                                          'vector)))))))
