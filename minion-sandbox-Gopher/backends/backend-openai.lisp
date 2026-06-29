;;; -*- Lisp -*-
;;; backend-openai.lisp - OpenAI-compatible streaming chat flow

(in-package "CHATBOT")

(defun openai-request-state (bot input conversation file-attachments effective-generation-config
                               &key request-messages history-messages)
  "Builds the provider-runner state for an OpenAI-compatible turn."
  (let* ((system-inst (chatbot-system-instruction bot))
         (current-messages (conversation-messages conversation))
         (persona-memory (conversation-persona-memory conversation))
         (persona-diary-entries (conversation-persona-diary-entries conversation)))
    (list :input input
          :file-attachments file-attachments
          :effective-generation-config effective-generation-config
          :history-messages (or history-messages
                               (stateless-history-messages current-messages input))
          :request-messages (or request-messages
                                (build-openai-request-messages system-inst current-messages input
                                                               :chatbot bot
                                                               :persona-memory persona-memory
                                                               :persona-diary-entries persona-diary-entries
                                                               :file-attachments file-attachments)))))

(defun openai-tool-result-message (id name args-str res-text tool-call)
  "Returns the OpenAI-compatible tool role message for a successful tool call."
  (declare (ignore args-str tool-call))
  `(("role" . "tool")
    ("tool_call_id" . ,id)
    ("name" . ,name)
    ("content" . ,res-text)))

(defun openai-tool-error-message (id name args-str condition tool-call)
  "Returns the OpenAI-compatible tool role message for a failed tool call."
  (declare (ignore args-str tool-call))
  `(("role" . "tool")
    ("tool_call_id" . ,id)
    ("name" . ,name)
    ("content" . ,(chatbot-tool-error-text name condition))))

(defun openai-assistant-tool-call-message (tool-calls)
  "Returns the OpenAI-compatible assistant tool call envelope for TOOL-CALLS."
  (let ((assistant-tool-calls
          (mapcar (lambda (tool-call)
                    (let ((id (cdr (assoc :id tool-call)))
                          (name (cdr (assoc :name tool-call)))
                          (args (coerce (cdr (assoc :arguments tool-call)) 'string)))
                      `(("id" . ,id)
                        ("type" . "function")
                        ("function" . (("name" . ,name)
                                       ("arguments" . ,args))))))
                  tool-calls)))
    `(("role" . "assistant")
      ("content" . nil)
      ("tool_calls" . ,assistant-tool-calls))))

(defun submit-openai-turn (bot callback state)
  "Submits one OpenAI-compatible streaming turn and returns a normalized outcome."
  (let* ((backend (chatbot-backend bot))
         (api-key (if (eq backend :lm-studio)
                      (lm-studio-api-key)
                      (openai-api-key)))
         (base-url (if (eq backend :lm-studio)
                       (lm-studio-api-base-url)
                       *openai-base-url*)))
    (unless (and api-key (string/= api-key ""))
      (error "~A API Key is not set." (if (eq backend :lm-studio) "LM Studio" "OpenAI")))
    (let* ((request-messages (getf state :request-messages))
           (effective-generation-config (getf state :effective-generation-config))
           (openai-tools (openai-request-tools bot))
           (stream-read-timeout (current-http-read-timeout))
           (payload-alist (list (cons "model" (chatbot-model bot))
                                (cons "messages" request-messages)
                                (cons "stream" t))))
      (when (getf effective-generation-config :temperature)
        (push (cons "temperature" (getf effective-generation-config :temperature)) payload-alist))
      (when (getf effective-generation-config :top-p)
        (push (cons "top_p" (getf effective-generation-config :top-p)) payload-alist))
      (when openai-tools
        (push (cons "tools" openai-tools) payload-alist))
      (let* ((payload-json (cl-json:encode-json-to-string payload-alist))
             (url (concatenate 'string base-url "/chat/completions"))
             (headers (list (cons "Authorization" (concatenate 'string "Bearer " api-key))
                            (cons "Content-Type" "application/json")))
             (full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
             (accumulated-tool-calls (make-hash-table :test 'equal)))
        (multiple-value-bind (stream status)
            (post-web-request url headers payload-json :want-stream t)
          (unless (= status 200)
            (error "API responded with HTTP status ~A" status))
          (unwind-protect
               (loop for line = (read-sse-line stream
                                               :timeout-seconds stream-read-timeout
                                               :timeout-context "OpenAI streaming response")
                     until (or (eq line :eof)
                               (and (stringp line)
                                    (alexandria:starts-with-subseq "data: [DONE]" line)))
                     do (let ((event (parse-sse-event line)))
                          (when event
                            (let* ((choices (cdr (assoc :choices event)))
                                   (first-choice (car choices))
                                   (delta (cdr (assoc :delta first-choice)))
                                   (tool-calls (cdr (assoc :tool--calls delta)))
                                   (delta-text (cdr (assoc :content delta))))
                              (when (and (stringp delta-text) (string/= delta-text ""))
                                (loop for char across delta-text
                                      do (vector-push-extend char full-text))
                                (when callback
                                  (funcall callback delta-text)))
                              (when tool-calls
                                (dolist (tool-call tool-calls)
                                  (let* ((index (cdr (assoc :index tool-call)))
                                         (id (cdr (assoc :id tool-call)))
                                         (function (cdr (assoc :function tool-call)))
                                         (name (cdr (assoc :name function)))
                                         (args (cdr (assoc :arguments function)))
                                         (existing (gethash index accumulated-tool-calls)))
                                    (unless existing
                                      (setf existing (list (cons :id nil)
                                                           (cons :name nil)
                                                           (cons :arguments (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))))
                                      (setf (gethash index accumulated-tool-calls) existing))
                                    (when id
                                      (setf (cdr (assoc :id existing)) id))
                                    (when name
                                      (setf (cdr (assoc :name existing)) name))
                                    (when args
                                      (loop for char across args
                                            do (vector-push-extend char (cdr (assoc :arguments existing))))))))))))
            (close stream)))
        (let ((tool-calls nil))
          (maphash (lambda (key value)
                     (declare (ignore key))
                     (push value tool-calls))
                   accumulated-tool-calls)
          (setf tool-calls (nreverse tool-calls))
          (if tool-calls
              (make-provider-turn-tool-outcome tool-calls
                                               :history-messages (getf state :history-messages)
                                               :request-messages request-messages
                                               :file-attachments (getf state :file-attachments)
                                               :effective-generation-config effective-generation-config)
              (make-provider-turn-final-outcome (coerce full-text 'string))))))))

(defun chat-openai (bot input conversation callback
                    &key file-attachments request-messages history-messages effective-generation-config (recursion-depth 0))
  "Sends user input to the active conversation using an OpenAI-compliant chat completions API."
  (run-provider-turn-loop
   :openai
   (openai-request-state bot input conversation file-attachments effective-generation-config
                         :request-messages request-messages
                         :history-messages history-messages)
   (lambda (state current-depth)
     (declare (ignore current-depth))
     (submit-openai-turn bot callback state))
   :continue-with-tools
   (lambda (state outcome next-depth step)
     (declare (ignore state))
     (continue-stateless-provider-tool-recursion
      bot
      conversation
      (getf outcome :history-messages)
      (provider-turn-outcome-tool-calls outcome)
      (lambda (name tool-call)
        (declare (ignore tool-call))
        (format nil "OpenAI tool arguments for ~A" name))
      #'openai-tool-result-message
      (lambda (tool-calls ordered-tool-responses)
        (append (list (openai-assistant-tool-call-message tool-calls))
                ordered-tool-responses))
      (lambda (recursive-history recursion-messages)
        (funcall step
                 (openai-request-state bot
                                       nil
                                       conversation
                                       (getf outcome :file-attachments)
                                       (getf outcome :effective-generation-config)
                                       :history-messages recursive-history
                                       :request-messages (append (getf outcome :request-messages)
                                                                 recursion-messages))
                 next-depth))
      :error-builder #'openai-tool-error-message))
   :finalize-turn
   (lambda (state outcome)
     (finish-stateless-text-turn conversation
                                 (getf state :history-messages)
                                 "assistant"
                                 (provider-turn-outcome-text outcome)))
   :error-handler
   (lambda (state condition current-depth)
     (declare (ignore state current-depth))
     (error "OpenAI Chat Error: ~A" condition))
   :initial-recursion-depth recursion-depth))
