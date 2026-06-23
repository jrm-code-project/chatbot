;;; -*- Lisp -*-
;;; payloads.lisp - role normalization and provider payload builders

(in-package "CHATBOT")

(defun assistant-like-role-p (role)
  "Returns true when ROLE is an assistant/model response role."
  (and role
       (member (string-downcase role) '("assistant" "model") :test #'string=)))

(defun generate-content-role-for-message (role)
  "Normalizes a stored conversation ROLE for Google generateContent."
  (if (assistant-like-role-p role) "model" "user"))

(defun openai-role-for-message (role)
  "Normalizes a stored conversation ROLE for OpenAI-compatible chat completions."
  (if (and role (string= (string-downcase role) "model")) "assistant" role))

(defparameter +prompt-timestamp-month-abbreviations+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun format-prompt-timestamp (universal-time &optional time-zone)
  "Formats UNIVERSAL-TIME as a prompt prefix like [14:29 26-Jun-2026]."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time time-zone)
    (declare (ignore second))
    (format nil "[~2,'0D:~2,'0D ~2,'0D-~A-~4,'0D]"
            hour
            minute
            day
            (svref +prompt-timestamp-month-abbreviations+ (1- month))
            year)))

(defun default-prompt-timestamp-function ()
  "Returns the current local prompt timestamp string."
  (format-prompt-timestamp (get-universal-time)))

(defvar *prompt-timestamp-function* #'default-prompt-timestamp-function
  "Function used to generate the current prompt timestamp string.")

(defun format-prompt-model-indicator (model)
  "Formats MODEL as a prompt prefix like [model: gemini-3-flash]."
  (format nil "[model: ~A]" model))

(defun decorate-live-user-input (chatbot input)
  "Decorates string INPUT with transient prompt prefixes requested by CHATBOT."
  (if (and chatbot
           (stringp input))
      (let ((parts nil))
        (when (chatbot-include-timestamp-p chatbot)
          (push (funcall *prompt-timestamp-function*) parts))
        (when (chatbot-include-model-p chatbot)
          (push (format-prompt-model-indicator (chatbot-model chatbot)) parts))
        (if parts
            (format nil "~{~A~^ ~} ~A" (nreverse parts) input)
            input))
      input))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter +pathname-mime-types+
  '(("txt" . "text/plain")
    ("text" . "text/plain")
    ("md" . "text/markdown")
    ("markdown" . "text/markdown")
    ("csv" . "text/csv")
    ("tsv" . "text/tab-separated-values")
    ("json" . "application/json")
    ("xml" . "application/xml")
    ("yaml" . "application/yaml")
    ("yml" . "application/yaml")
    ("html" . "text/html")
    ("htm" . "text/html")
    ("css" . "text/css")
    ("js" . "application/javascript")
    ("mjs" . "application/javascript")
    ("ts" . "application/typescript")
    ("tsx" . "application/typescript")
    ("jsx" . "application/javascript")
    ("lisp" . "text/x-common-lisp")
    ("lsp" . "text/x-common-lisp")
    ("cl" . "text/x-common-lisp")
    ("asd" . "text/x-common-lisp")
    ("org" . "text/plain")
    ("pdf" . "application/pdf")
    ("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp")
    ("svg" . "image/svg+xml")
    ("bmp" . "image/bmp")
    ("mp3" . "audio/mpeg")
    ("wav" . "audio/wav")
    ("flac" . "audio/flac")
    ("ogg" . "audio/ogg")
    ("m4a" . "audio/mp4")
    ("mp4" . "video/mp4")
    ("mov" . "video/quicktime")
    ("webm" . "video/webm")
    ("avi" . "video/x-msvideo")))

(defparameter +textual-mime-types+
  '("application/json"
    "application/xml"
    "application/yaml"
    "application/javascript"
    "application/typescript"
    "text/x-common-lisp"))

(defun pathname-mime-type (pathname)
  "Infers a MIME type for PATHNAME from its extension."
  (let* ((type (pathname-type pathname))
        (extension (and type (string-downcase type))))
    (or (and extension
            (cdr (assoc extension +pathname-mime-types+ :test #'string=)))
       "application/octet-stream")))

(defun textual-mime-type-p (mime-type)
  "Returns true when MIME-TYPE should fall back to inline text parts."
  (or (alexandria:starts-with-subseq "text/" mime-type)
      (member mime-type +textual-mime-types+ :test #'string=)))

(defun interaction-content-type-for-mime-type (mime-type)
  "Returns the Interactions API content type for MIME-TYPE."
  (cond
    ((alexandria:starts-with-subseq "image/" mime-type) "image")
    ((alexandria:starts-with-subseq "audio/" mime-type) "audio")
    ((alexandria:starts-with-subseq "video/" mime-type) "video")
    (t "document")))

(defun read-file-octets (pathname)
  "Reads PATHNAME as a vector of octets."
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
          (octets (make-array size :element-type '(unsigned-byte 8)))
          (count (read-sequence octets stream)))
      (if (= count size)
         octets
         (subseq octets 0 count)))))

(defun base64-encode-octets (octets)
  "Encodes OCTETS as a base64 string."
  (with-output-to-string (stream)
    (loop for index from 0 below (length octets) by 3
         for remaining = (- (length octets) index)
         for first = (aref octets index)
         for second = (if (> remaining 1) (aref octets (1+ index)) 0)
         for third = (if (> remaining 2) (aref octets (+ index 2)) 0)
         for chunk = (logior (ash first 16)
                             (ash second 8)
                             third)
         do (write-char (char +base64-alphabet+ (ldb (byte 6 18) chunk)) stream)
            (write-char (char +base64-alphabet+ (ldb (byte 6 12) chunk)) stream)
            (write-char (if (> remaining 1)
                            (char +base64-alphabet+ (ldb (byte 6 6) chunk))
                            #\=)
                        stream)
            (write-char (if (> remaining 2)
                            (char +base64-alphabet+ (ldb (byte 6 0) chunk))
                            #\=)
                        stream))))

(defun decode-octets-as-utf-8 (octets)
  "Decodes OCTETS as UTF-8 when supported, otherwise as Latin-1."
  #+sbcl
  (sb-ext:octets-to-string octets :external-format :utf-8)
  #-sbcl
  (coerce (map 'list #'code-char octets) 'string))

(defun normalize-chat-file-spec (file-spec)
  "Normalizes FILE-SPEC into a pathname."
  (etypecase file-spec
    (pathname file-spec)
    (string (pathname file-spec))))

(defun expand-chat-input-directory-files (directory)
  "Recursively expands DIRECTORY into a stable list of file pathnames."
  (let* ((resolved-directory (uiop:ensure-directory-pathname (truename directory)))
        (files (stable-sort (copy-list (uiop:directory-files resolved-directory))
                            #'string<
                            :key #'namestring))
        (subdirectories (stable-sort (copy-list (uiop:subdirectories resolved-directory))
                                     #'string<
                                     :key #'namestring)))
    (append files
           (mapcan #'expand-chat-input-directory-files subdirectories))))

(defun expand-chat-input-file-spec (file-spec)
  "Expands FILE-SPEC into zero or more concrete file pathnames."
  (let ((pathname (normalize-chat-file-spec file-spec)))
    (cond
      ((wild-pathname-p pathname)
      (let ((matches (stable-sort (copy-list (cl:directory pathname))
                                  #'string<
                                  :key #'namestring)))
        (unless matches
          (error "No files matched wildcard pathname ~A." pathname))
        (mapcan #'expand-chat-input-file-spec matches)))
      ((uiop:directory-exists-p pathname)
      (expand-chat-input-directory-files pathname))
      ((probe-file pathname)
      (let ((resolved (probe-file pathname)))
        (if (uiop:directory-exists-p resolved)
            (expand-chat-input-directory-files resolved)
            (list (truename resolved)))))
      (t
      (error "File path not found: ~A" pathname)))))

(defun resolve-chat-input-files (files)
  "Resolves FILES into a stable deduplicated list of concrete file pathnames."
  (let ((seen (make-hash-table :test 'equal))
       (result nil))
    (dolist (file-spec files (nreverse result))
      (dolist (pathname (expand-chat-input-file-spec file-spec))
       (let* ((resolved (truename pathname))
              (key (string-downcase (namestring resolved))))
         (unless (gethash key seen)
           (setf (gethash key seen) t)
           (push resolved result)))))))

(defun make-chat-file-attachment (pathname)
  "Reads PATHNAME and prepares one transient prompt attachment descriptor."
  (let* ((resolved (truename pathname))
        (mime-type (pathname-mime-type resolved))
        (octets (read-file-octets resolved))
        (base64 (base64-encode-octets octets)))
    (list (cons :pathname resolved)
         (cons :pathname-string (namestring resolved))
         (cons :display-name (file-namestring resolved))
         (cons :mime-type mime-type)
         (cons :interaction-type (interaction-content-type-for-mime-type mime-type))
         (cons :size-bytes (length octets))
         (cons :base64-data base64)
         (cons :text-fallback
               (when (textual-mime-type-p mime-type)
                 (decode-octets-as-utf-8 octets))))))

(defun prepare-chat-file-attachments (files)
  "Expands FILES and reads the resulting files into transient attachment descriptors."
  (unless (listp files)
    (error ":files must be a list of file or directory pathnames."))
  (mapcar #'make-chat-file-attachment
         (resolve-chat-input-files files)))

(defun attachment-openai-text (attachment)
  "Builds the text fallback content for ATTACHMENT."
  (let ((pathname-string (cdr (assoc :pathname-string attachment)))
       (mime-type (cdr (assoc :mime-type attachment)))
       (text-fallback (cdr (assoc :text-fallback attachment)))
       (base64-data (cdr (assoc :base64-data attachment))))
    (if text-fallback
       (format nil "[Attached file: ~A (~A)]~%~A"
               pathname-string
               mime-type
               text-fallback)
       (format nil "[Attached file: ~A (~A, base64)]~%~A"
               pathname-string
               mime-type
               base64-data))))

(defun make-interaction-file-part (attachment)
  "Converts ATTACHMENT into an Interactions API content part."
  `(("type" . ,(cdr (assoc :interaction-type attachment)))
    ("data" . ,(cdr (assoc :base64-data attachment)))
    ("mime_type" . ,(cdr (assoc :mime-type attachment)))))

(defun make-generate-content-file-part (attachment)
  "Converts ATTACHMENT into a generateContent inlineData part."
  `(("inlineData" . (("mimeType" . ,(cdr (assoc :mime-type attachment)))
                    ("data" . ,(cdr (assoc :base64-data attachment)))))))

(defun openai-file-content-parts (attachment)
  "Converts ATTACHMENT into one or more OpenAI-compatible content parts."
  (let ((mime-type (cdr (assoc :mime-type attachment))))
    (cond
      ((alexandria:starts-with-subseq "image/" mime-type)
      (list `(("type" . "text")
              ("text" . ,(format nil "[Attached image: ~A (~A)]"
                                 (cdr (assoc :pathname-string attachment))
                                 mime-type)))
            `(("type" . "image_url")
              ("image_url" . (("url" . ,(format nil "data:~A;base64,~A"
                                                mime-type
                                                (cdr (assoc :base64-data attachment)))))))))
      (t
      (list `(("type" . "text")
              ("text" . ,(attachment-openai-text attachment))))))))

(defun interaction-live-user-input-parts (chatbot input file-attachments)
  "Builds the Interactions API content parts for the current live user turn."
  (let ((parts nil)
       (decorated-input (decorate-live-user-input chatbot input)))
    (when (and (stringp decorated-input)
              (string/= decorated-input ""))
      (push `(("type" . "text") ("text" . ,decorated-input)) parts))
    (dolist (attachment file-attachments)
      (push (make-interaction-file-part attachment) parts))
    (nreverse parts)))

(defun interaction-live-user-input-value (chatbot input file-attachments)
  "Builds the Interactions API input value for the current live user turn."
  (if file-attachments
      (coerce (interaction-live-user-input-parts chatbot input file-attachments) 'vector)
      (decorate-live-user-input chatbot input)))

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

(defun append-user-input-to-conversation-messages (messages input)
  "Returns MESSAGES with the current user INPUT appended when present."
  (if input
      (append messages (list (list (cons "role" "user")
                                   (cons "content" input))))
      messages))

(defun persona-memory-messages (persona-memory)
  "Returns provider-neutral synthetic history representing PERSONA-MEMORY."
  (when persona-memory
    (list (list (cons "role" "user")
                (cons "content" "Please concisely summarize your knowledge graph."))
          (list (cons "role" "model")
                (cons "content" persona-memory)))))

(defun persona-diary-entry-message-text (entry)
  "Formats a persona diary ENTRY as model-visible preload text."
  (let ((filename (cdr (assoc :filename entry)))
        (content (cdr (assoc :content entry))))
    (if filename
        (format nil "[Diary: ~A]~%~A" filename content)
        content)))

(defun persona-diary-messages (entries)
  "Returns provider-neutral synthetic history representing ordered diary ENTRIES."
  (when entries
    (mapcar (lambda (entry)
              (list (cons "role" "model")
                    (cons "content" (persona-diary-entry-message-text entry))))
            entries)))

(defun build-request-history-messages (messages input &key chatbot persona-memory persona-diary-entries)
  "Builds request history by prepending persona preload ahead of ordinary MESSAGES and INPUT."
  (append (persona-memory-messages persona-memory)
          (persona-diary-messages persona-diary-entries)
          (append-user-input-to-conversation-messages
           messages
           (decorate-live-user-input chatbot input))))

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

(defun build-openai-request-messages (system-inst messages input &key chatbot persona-memory persona-diary-entries file-attachments)
  "Builds the OpenAI chat-completions message list for the current turn."
  (let ((history (mapcar (lambda (message)
                          (let ((role (cdr (assoc "role" message :test #'string=))))
                            (if role
                                (acons "role"
                                       (openai-role-for-message role)
                                       (remove "role" message :key #'car :test #'string=))
                                message)))
                         (append (persona-memory-messages persona-memory)
                                (persona-diary-messages persona-diary-entries)
                                messages)))
        (current-user-message (openai-live-user-message chatbot input file-attachments)))
    (if system-inst
        (append (list (list (cons "role" "system")
                           (cons "content" system-inst)))
                history
                (when current-user-message
                  (list current-user-message)))
        (append history
                (when current-user-message
                  (list current-user-message))))))

(defun interaction-request-tools (chatbot)
  "Builds the Gemini Interactions tool list for CHATBOT, including built-in and MCP tools."
  (let ((tools nil))
    (when (chatbot-google-search-p chatbot)
      (push '(("type" . "google_search")) tools))
    (when (chatbot-code-execution-p chatbot)
      (push '(("type" . "code_execution")) tools))
    (let ((chatbot-tools (get-all-chatbot-tools chatbot)))
      (when chatbot-tools
        (dolist (pair chatbot-tools)
         (let* ((mcp-tool (cdr pair))
                (name (mcp-val :name mcp-tool))
                (description (mcp-val :description mcp-tool))
                (input-schema (mcp-val :input-schema mcp-tool)))
           (push `(("type" . "function")
                   ("name" . ,name)
                   ("description" . ,(or description ""))
                   ("parameters" . ,(gemini-tool-parameters input-schema)))
                 tools)))))
    (nreverse tools)))

(defun openai-request-tools (chatbot)
  "Builds the OpenAI-compatible tool list for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
    (when mcp-tools
      (mapcar (lambda (pair)
               (translate-mcp-tool-to-openai (cdr pair)))
             mcp-tools))))

(defun generate-content-request-tools (chatbot)
  "Builds the Google generateContent tools payload for CHATBOT from built-in and MCP tools."
  (let ((mcp-tools (get-all-chatbot-tools chatbot)))
    (when mcp-tools
      (list `(("functionDeclarations" . ,(coerce
                                         (mapcar (lambda (pair)
                                                   (translate-mcp-tool-to-gemini-fn (cdr pair)))
                                                 mcp-tools)
                                         'vector)))))))

(defun conversation-message->interaction-step (message)
  "Converts a stored conversation message to an Interactions API step."
  (let ((role (cdr (assoc "role" message :test #'string=)))
        (content (cdr (assoc "content" message :test #'string=))))
    (when (and role content)
      `(("type" . ,(if (assistant-like-role-p role) "model_output" "user_input"))
        ("content" . ,(vector `(("type" . "text") ("text" . ,content))))))))

(defun build-initial-interaction-input (messages input &key chatbot persona-memory persona-diary-entries file-attachments)
  "Builds the first-turn Interactions API input with any preloaded history."
  (let* ((history-messages (append (persona-memory-messages persona-memory)
                                  (persona-diary-messages persona-diary-entries)))
        (request-input (interaction-live-user-input-parts chatbot input file-attachments)))
    (if (and (null (append history-messages messages))
            (null file-attachments)
            (stringp input))
       (decorate-live-user-input chatbot input)
       (if (or (append history-messages messages) request-input)
       (coerce
        (append
         (remove nil (mapcar #'conversation-message->interaction-step
                             (append history-messages messages)))
         (when request-input
           (list `(("type" . "user_input")
                   ("content" . ,(coerce request-input 'vector))))))
        'vector)
           (decorate-live-user-input chatbot input)))))

(defun make-interaction-payload (chatbot input &key previous-interaction-id (stream t) messages persona-memory persona-diary-entries file-attachments)
  "Creates a JSON-serializable alist payload for the Gemini Interactions API."
  (let ((payload (list (cons "model" (chatbot-model chatbot))
                      (cons "input" (if previous-interaction-id
                                        (if (and file-attachments
                                                 (stringp input))
                                            (interaction-live-user-input-value chatbot input file-attachments)
                                            (decorate-live-user-input chatbot input))
                                        (build-initial-interaction-input messages
                                                                         input
                                                                         :chatbot chatbot
                                                                         :persona-memory persona-memory
                                                                         :persona-diary-entries persona-diary-entries
                                                                         :file-attachments file-attachments)))
                      (cons "stream" (if stream t :false))
                      (cons "store" t))))
    (when previous-interaction-id
      (push (cons "previous_interaction_id" previous-interaction-id) payload))
    (when (chatbot-system-instruction chatbot)
      (push (cons "system_instruction" (chatbot-system-instruction chatbot)) payload))
    (let ((tools (interaction-request-tools chatbot)))
      (when tools
        (push (cons "tools" tools) payload)))
    (nreverse payload)))
