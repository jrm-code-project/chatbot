;;; -*- Lisp -*-
;;; chatbot-state-tools.lisp - built-in chatbot state tool helpers

(in-package "CHATBOT")

(defun ensure-system-instruction-tool-path (bot tool-name)
  "Returns BOT's system-instruction path or signals an execution error."
  (or (chatbot-system-instruction-path bot)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "System-instruction tools require a persona-backed system instruction file.")))

(defun system-instruction-storage-kind-name (storage-kind)
  "Returns a lowercase string name for STORAGE-KIND."
  (string-downcase (string storage-kind)))

(defun system-instruction-tool-result (bot &key saved)
  "Returns the current system-instruction paragraph state as JSON text."
  (let ((payload `(("paragraphs" . ,(current-system-instruction-paragraphs bot))
                   ("count" . ,(system-instruction-paragraph-count bot))
                   ("storageKind" . ,(system-instruction-storage-kind-name
                                      (chatbot-system-instruction-storage-kind bot)))
                   ("path" . ,(if (chatbot-system-instruction-path bot)
                                  (namestring (chatbot-system-instruction-path bot))
                                  :null))
                   ,@(when saved '(("saved" . t))))))
    (cl-json:encode-json-to-string payload)))

(defun sampling-parameters-tool-result (bot &key saved)
  "Returns the current runtime sampling parameters as JSON text."
  (let ((parameters (sampling-parameters bot)))
    (cl-json:encode-json-to-string
     `(("temperature" . ,(or (getf parameters :temperature) :null))
       ("topP" . ,(or (getf parameters :top-p) :null))
       ,@(when saved '(("saved" . t)))))))

(defun save-system-instructions-or-tool-error (bot tool-name)
  "Saves BOT's system instructions, mapping failures to tool errors."
  (handler-case
      (save-system-instructions bot)
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (princ-to-string e)))))

(defun system-instruction-tool-saved-p (bot)
  "Returns true when BOT has a persisted system-instruction backing file."
  (and (chatbot-system-instruction-path bot) t))

(defun maybe-save-system-instructions (bot tool-name)
  "Persists BOT's system instructions when they have a backing file."
  (when (chatbot-system-instruction-path bot)
    (save-system-instructions-or-tool-error bot tool-name)))

(defun execute-read-sampling-parameters-tool (bot)
  "Returns BOT's current sampling-parameters tool payload."
  (sampling-parameters-tool-result bot))

(defun execute-set-sampling-parameters-tool (bot arguments tool-name)
  "Updates BOT's sampling parameters from ARGUMENTS."
  (multiple-value-bind (temperature-foundp temperature-value)
      (builtin-tool-argument arguments "temperature" :temperature)
    (multiple-value-bind (top-p-foundp top-p-value)
        (builtin-tool-argument arguments "topP" :top-p :top_p)
      (unless (or temperature-foundp top-p-foundp)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason "At least one of temperature or topP is required."))
      (handler-case
          (progn
            (apply #'set-sampling-parameters
                   bot
                   (append (when temperature-foundp
                             (list :temperature
                                   (normalize-builtin-tool-real-argument temperature-value "temperature" tool-name :allow-nil-p t)))
                           (when top-p-foundp
                             (list :top-p
                                   (normalize-builtin-tool-real-argument top-p-value "topP" tool-name :allow-nil-p t)))))
            (sampling-parameters-tool-result bot :saved t))
        (error (e)
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (princ-to-string e)))))))

(defun execute-reset-sampling-parameters-tool (bot)
  "Clears BOT's sampling parameter overrides."
  (reset-sampling-parameters bot)
  (sampling-parameters-tool-result bot :saved t))

(defun execute-read-system-instructions-tool (bot)
  "Returns BOT's system-instruction tool payload."
  (system-instruction-tool-result bot))

(defun execute-insert-system-instruction-paragraph-tool (bot arguments tool-name)
  "Inserts one system-instruction paragraph for BOT."
  (insert-system-instruction-paragraph
   bot
   (normalize-builtin-tool-string-argument
    (or (mcp-val "paragraph" arguments)
        (mcp-val :paragraph arguments))
    "paragraph"
    tool-name)
   :index (normalize-builtin-tool-integer-argument
           (or (mcp-val "index" arguments)
               (mcp-val :index arguments))
           "index"
           tool-name))
  (maybe-save-system-instructions bot tool-name)
  (system-instruction-tool-result bot :saved (system-instruction-tool-saved-p bot)))

(defun execute-update-system-instruction-paragraph-tool (bot arguments tool-name)
  "Updates one system-instruction paragraph for BOT."
  (update-system-instruction-paragraph
   bot
   (normalize-builtin-tool-integer-argument
    (or (mcp-val "index" arguments)
        (mcp-val :index arguments))
    "index"
    tool-name)
   (normalize-builtin-tool-string-argument
    (or (mcp-val "paragraph" arguments)
        (mcp-val :paragraph arguments))
    "paragraph"
    tool-name))
  (maybe-save-system-instructions bot tool-name)
  (system-instruction-tool-result bot :saved (system-instruction-tool-saved-p bot)))

(defun execute-delete-system-instruction-paragraph-tool (bot arguments tool-name)
  "Deletes one system-instruction paragraph for BOT."
  (delete-system-instruction-paragraph
   bot
   (normalize-builtin-tool-integer-argument
    (or (mcp-val "index" arguments)
        (mcp-val :index arguments))
    "index"
    tool-name))
  (maybe-save-system-instructions bot tool-name)
  (system-instruction-tool-result bot :saved (system-instruction-tool-saved-p bot)))

(defun execute-replace-system-instructions-tool (bot arguments tool-name)
  "Replaces BOT's full system-instruction paragraph set."
  (multiple-value-bind (paragraphs-foundp paragraphs-value)
      (builtin-tool-argument arguments "paragraphs" :paragraphs)
    (replace-system-instruction-paragraphs
     bot
     (normalize-builtin-tool-string-sequence-argument
      paragraphs-foundp
      paragraphs-value
      "paragraphs"
      tool-name)))
  (maybe-save-system-instructions bot tool-name)
  (system-instruction-tool-result bot :saved (system-instruction-tool-saved-p bot)))
