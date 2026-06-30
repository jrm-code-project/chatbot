;;; -*- Lisp -*-
;;; openai-payloads.lisp - OpenAI-compatible payload translation

(in-package "CHATBOT")

(defun openai-role-for-message (role)
  "Normalizes a stored conversation ROLE for OpenAI-compatible chat completions."
  (if (and role (string= (string-downcase role) "model")) "assistant" role))

(defun openai-live-user-message (chatbot input file-attachments)
  "Builds the transient current user message for OpenAI-compatible requests."
  (let ((decorated-input (decorate-live-user-input chatbot input)))
    (cond
      (file-attachments
       (let ((content nil))
         (when (and (stringp decorated-input)
                    (string/= decorated-input ""))
           (push `(("type" . "text") ("text" . ,decorated-input)) content))
         (dolist (attachment file-attachments)
           (setf content (append content (openai-file-content-parts attachment))))
         (when content
           (list (cons "role" "user")
                 (cons "content" content)))))
      ((and (stringp decorated-input)
            (string/= decorated-input ""))
       (list (cons "role" "user")
             (cons "content" decorated-input)))
      (t nil))))

(defun openai-normalize-message-content (content)
  "Ensures CONTENT is either a plain string or an array of valid OpenAI content parts.
Converts any Gemini/Google-style nested parts or non-string structures into clean plain text strings."
  (cond
    ((null content) "")
    ((stringp content) content)
    ((and (listp content)
          (consp (car content))
          (stringp (caar content))) ; Is it an alist (e.g., single Gemini/Google part, or single object)?
     (let ((text-val (cdr (assoc "text" content :test #'string=)))
           (fn-call (cdr (assoc "functionCall" content :test #'string=)))
           (fn-resp (cdr (assoc "functionResponse" content :test #'string=))))
       (cond
         (text-val text-val)
         (fn-call (format nil "[Tool Call: ~A]" (cdr (assoc "name" fn-call :test #'string=))))
         (fn-resp (format nil "[Tool Response: ~A]" (cl-json:encode-json-to-string fn-resp)))
         (t (cl-json:encode-json-to-string content)))))
    ((listp content) ; Is it a list of parts?
     (with-output-to-string (s)
       (dolist (part content)
         (let ((normalized (openai-normalize-message-content part)))
           (when (string/= normalized "")
             (format s "~A~%" normalized))))))
    ((vectorp content) ; Is it a vector of parts?
     (openai-normalize-message-content (coerce content 'list)))
    (t (princ-to-string content))))

(defun build-openai-request-messages (system-inst messages input &key chatbot persona-memory persona-diary-entries file-attachments)
  "Builds the OpenAI chat-completions message list for the current turn."
  (let ((history (mapcar (lambda (message)
                           (let* ((role (cdr (assoc "role" message :test #'string=)))
                                  (content (cdr (assoc "content" message :test #'string=)))
                                  (normalized-content (openai-normalize-message-content content))
                                  (clean-msg (remove "role" (remove "content" message :key #'car :test #'string=) :key #'car :test #'string=)))
                             (if role
                                 (acons "role"
                                        (openai-role-for-message role)
                                        (acons "content" normalized-content clean-msg))
                                 message)))
                         (append (persona-memory-messages persona-memory)
                                 (persona-diary-messages persona-diary-entries)
                                 messages)))
        (current-user-message (openai-live-user-message chatbot input file-attachments)))
    (if system-inst
        (append (list (list (cons "role" "system")
                            (cons "content" (openai-normalize-message-content (system-instruction-text system-inst)))))
                history
                (when current-user-message
                  (list current-user-message)))
        (append history
                (when current-user-message
                  (list current-user-message))))))

(defun openai-request-tools (chatbot)
  "Builds the OpenAI-compatible tool list for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
    (when mcp-tools
      (mapcar (lambda (pair)
                (translate-mcp-tool-to-openai (cdr pair)))
              mcp-tools))))
