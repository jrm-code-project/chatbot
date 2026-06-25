;;; -*- Lisp -*-
;;; google-payloads.lisp - Google generateContent payload translation

(in-package "CHATBOT")

(defun generate-content-role-for-message (role)
  "Normalizes a stored conversation ROLE for Google generateContent."
  (if (assistant-like-role-p role) "model" "user"))

(defun sanitize-generate-content-function-call (function-call)
  "Returns FUNCTION-CALL with nil args normalized to an empty JSON object."
  (let* ((name-entry (or (assoc "name" function-call :test #'string=)
                         (assoc :name function-call)))
         (args-entry (or (assoc "args" function-call :test #'string=)
                         (assoc :args function-call)))
         (id-entry (or (assoc "id" function-call :test #'string=)
                       (assoc :id function-call)))
         (raw-args (and args-entry (cdr args-entry)))
         (payload-args (if args-entry
                           (if raw-args
                               (json-encodable-value raw-args)
                               (empty-json-object))
                           nil)))
    (remove nil
            (list (when name-entry
                    (cons "name" (cdr name-entry)))
                  (when args-entry
                    (cons "args" payload-args))
                  (when id-entry
                    (cons "id" (cdr id-entry)))))))

(defun sanitize-generate-content-part (part)
  "Returns PART with Google function-call fields normalized for generateContent."
  (let* ((function-call-entry (or (assoc "functionCall" part :test #'string=)
                                  (assoc :functionCall part)
                                  (assoc :function-call part)
                                  (assoc :function--call part)))
         (sanitized-function-call
           (and function-call-entry
                (sanitize-generate-content-function-call (cdr function-call-entry)))))
    (if sanitized-function-call
        (append
         (remove-if (lambda (entry)
                      (member (car entry)
                              '("functionCall" :functionCall :function-call :function--call)
                              :test #'equal))
                    part)
         (list (cons "functionCall" sanitized-function-call)))
        part)))

(defun sanitize-generate-content-parts (parts)
  "Returns PARTS with provider-specific function-call payloads normalized."
  (typecase parts
    (vector (map 'vector #'sanitize-generate-content-part parts))
    (list (mapcar #'sanitize-generate-content-part parts))
    (t parts)))

(defun generate-content-live-user-message (chatbot input file-attachments &key effective-model)
  "Builds the transient current user message for generateContent requests."
  (let ((parts nil)
        (decorated-input (decorate-live-user-input chatbot input
                                                  :effective-model effective-model)))
    (when (and (stringp decorated-input)
               (string/= decorated-input ""))
      (push (list (cons "text" decorated-input)) parts))
    (dolist (attachment file-attachments)
      (push (make-generate-content-file-part attachment) parts))
    (if parts
        (list (cons "role" "user")
              (cons "parts" (coerce (nreverse parts) 'vector)))
        nil)))

(defun build-generate-content-request-contents (messages input &key chatbot persona-memory persona-diary-entries file-attachments effective-model)
  "Builds the Google generateContent contents list for the current turn."
  (let ((history (build-request-history-messages messages
                                                 nil
                                                 :chatbot chatbot
                                                 :persona-memory persona-memory
                                                 :persona-diary-entries persona-diary-entries
                                                 :effective-model effective-model))
        (current-user-message (generate-content-live-user-message chatbot
                                                                 input
                                                                 file-attachments
                                                                 :effective-model effective-model)))
    (append
     (mapcar (lambda (msg)
               (let ((role (cdr (assoc "role" msg :test #'string=)))
                     (content (cdr (assoc "content" msg :test #'string=)))
                     (parts (cdr (assoc "parts" msg :test #'string=))))
                 (cond
                   (parts
                    (list (cons "role" (generate-content-role-for-message role))
                          (cons "parts" (sanitize-generate-content-parts parts))))
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
