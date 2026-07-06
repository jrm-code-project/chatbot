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

(defun history-message->generate-content-message (message)
  "Converts one stored provider-neutral MESSAGE into a generateContent message."
  (let ((role (cdr (assoc "role" message :test #'string=)))
        (content (cdr (assoc "content" message :test #'string=)))
        (parts (cdr (assoc "parts" message :test #'string=))))
    (cond
      (parts
       (list (cons "role" (generate-content-role-for-message role))
             (cons "parts" (sanitize-generate-content-parts parts))))
      (t
       (list (cons "role" (generate-content-role-for-message role))
             (cons "parts" (vector (list (cons "text" content)))))))))

(defun generate-content-cacheable-prefix-contents (chatbot persona-memory persona-diary-entries)
  "Returns the reusable generateContent prefix contents implied by CHATBOT preload."
  (mapcar #'history-message->generate-content-message
          (append (persona-memory-messages persona-memory)
                  (persona-diary-messages persona-diary-entries))))

(defun build-generate-content-request-contents (messages input &key chatbot persona-memory persona-diary-entries file-attachments effective-model omit-preloaded-history-p)
  "Builds the Google generateContent contents list for the current turn."
  (let ((history (build-request-history-messages messages
                                                 nil
                                                 :chatbot chatbot
                                                 :persona-memory (unless omit-preloaded-history-p
                                                                  persona-memory)
                                                 :persona-diary-entries (unless omit-preloaded-history-p
                                                                         persona-diary-entries)
                                                 :effective-model effective-model))
        (current-user-message (generate-content-live-user-message chatbot
                                                                 input
                                                                 file-attachments
                                                                 :effective-model effective-model)))
    (append
     (mapcar #'history-message->generate-content-message history)
     (when current-user-message
       (list current-user-message)))))

(defun generate-content-request-tools (chatbot)
  "Builds the Google generateContent tools payload for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
    (when mcp-tools
      (list `(("functionDeclarations" . ,(coerce
                                          (sort (mapcar (lambda (pair)
                                                          (translate-mcp-tool-to-gemini-fn (cdr pair)))
                                                        mcp-tools)
                                                #'string<
                                                :key (lambda (tool)
                                                       (mcp-val :name tool)))
                                          'vector)))))))
