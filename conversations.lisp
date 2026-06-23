;;; -*- Lisp -*-
;;; conversations.lisp - conversation constructors and persona entry points

(in-package "CHATBOT")

(defun read-persona-config (config-path)
  "Reads and validates a persona config form from CONFIG-PATH."
  (handler-case
      (with-open-file (stream config-path :direction :input)
        (let* ((eof-marker (gensym "EOF"))
               (forms (loop for form = (read stream nil eof-marker)
                            until (eq form eof-marker)
                            collect form))
               (config (cond
                         ((null forms) :eof)
                         ((and (= 1 (length forms))
                               (listp (car forms)))
                          (car forms))
                         (t forms))))
          (when (eq config :eof)
            (error "Persona config file is empty: ~A" config-path))
          (unless (listp config)
            (error "Persona config must be a property list: ~A" config-path))
          (unless (and config (keywordp (car config)))
            (error "Persona config must start with a keyword property: ~A" config-path))
          config))
    (error (e)
      (error "Invalid persona config in ~A: ~A" config-path e))))

(defun new-chat (&key model system-instruction google-search-p web-tools-p code-execution-p include-timestamp-p include-model-p enable-eval-p filesystem-tools-p filesystem-root-directory filesystem-allowed-directories filesystem-allowlist-path (backend :gemini) runtime-context)
  "Creates a new chatbot instance and returns an initialized conversation object.
If model is NIL, a sensible default model is chosen based on the backend.
Personas are optional; use NEW-CHAT-PERSONA only when you want persona-specific
configuration, instructions, or preloaded memory."
  (let ((resolved-context (resolve-runtime-context runtime-context :sync-from-globals-p t)))
    (call-with-runtime-context
     resolved-context
     (lambda ()
      (maybe-auto-initialize-startup-chatbot resolved-context)
      (let* ((chosen-model (or model
                               (backend-default-model backend)))
             (bot (make-instance 'chatbot
                                 :model chosen-model
                                 :backend backend
                                 :system-instruction system-instruction
                                 :google-search-p google-search-p
                                 :web-tools-p web-tools-p
                                 :code-execution-p code-execution-p
                                 :include-timestamp-p include-timestamp-p
                                 :include-model-p include-model-p
                                 :enable-eval-p enable-eval-p
                                 :filesystem-tools-p filesystem-tools-p
                                 :filesystem-root-directory filesystem-root-directory
                                 :filesystem-allowed-directories filesystem-allowed-directories
                                 :filesystem-allowlist-path filesystem-allowlist-path
                                 :runtime-context resolved-context)))
        (when (startup-chatbot-mcp-servers resolved-context)
          (setf (chatbot-mcp-servers bot)
                (startup-chatbot-mcp-servers resolved-context))
          (setf (chatbot-mcp-startup-status bot)
                (startup-chatbot-mcp-status resolved-context)))
        (make-instance 'conversation :chatbot bot))))))

(defun new-chat-persona (persona-name &key runtime-context)
  "Creates a new chat session for a given chatbot persona.
The persona's configuration is read from ~/.Personas/<persona-name>/config.lisp
and the system instructions are loaded from the persona's system-instruction.md file.
Use NEW-CHAT instead when no persona should be loaded."
  (let* ((homedir (get-user-homedir-pathname))
         (name-str (string persona-name))
         (persona-dir (or (uiop:directory-exists-p (merge-pathnames (make-pathname :directory (list :relative ".Personas" name-str)) homedir))
                          (uiop:directory-exists-p (merge-pathnames (make-pathname :directory (list :relative ".Personas" (string-downcase name-str))) homedir))
                          (error "Persona directory not found: ~~/.Personas/~A" name-str)))
        (config-path (probe-file (merge-pathnames "config.lisp" persona-dir)))
        (inst-path (or (probe-file (merge-pathnames "system-instruction.md" persona-dir))
                       (probe-file (merge-pathnames "system-instructions.md" persona-dir)))))
    (let* ((config (when config-path
                     (read-persona-config config-path)))
           (system-instruction (when inst-path
                                 (uiop:read-file-string inst-path)))
           (model (safe-getf config :model))
           (googleapi (safe-getf config :googleapi))
           (google-search-p (safe-getf config :google-search-p))
           (web-tools-p (safe-getf config :enable-web-tools))
           (code-execution-p (safe-getf config :code-execution-p))
           (include-timestamp-p (safe-getf config :include-timestamp))
           (include-model-p (safe-getf config :include-model))
           (enable-eval-p (safe-getf config :enable-eval))
           (filesystem-tools-p (safe-getf config :enable-filesystem-tools))
           (backend (cond
                      ((eq googleapi :google-api) :google)
                      (t :gemini))))
      (let ((conversation
              (preload-persona-conversation-diary
               (preload-persona-conversation-memory
                (new-chat :backend backend
                          :model model
                          :system-instruction system-instruction
                          :google-search-p google-search-p
                          :web-tools-p web-tools-p
                          :code-execution-p code-execution-p
                          :include-timestamp-p include-timestamp-p
                          :include-model-p include-model-p
                          :enable-eval-p enable-eval-p
                          :filesystem-tools-p filesystem-tools-p
                          :filesystem-root-directory persona-dir
                          :filesystem-allowed-directories (persona-filesystem-allowlist-directories persona-dir)
                          :filesystem-allowlist-path (persona-filesystem-allowlist-path persona-dir)
                          :runtime-context runtime-context)
                persona-dir)
               persona-dir)))
        (attach-persona-memory-mcp-server conversation persona-dir)))))
