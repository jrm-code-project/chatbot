;;; -*- Lisp -*-
;;; backend-openai.lisp - OpenAI-compatible streaming chat flow

(in-package "CHATBOT")

(defun openai-request-state (bot input conversation file-attachments effective-generation-config
                               &key request-messages history-messages malformed-response-retry-attempted-p)
  "Builds the provider-runner state for an OpenAI-compatible turn."
  (let* ((system-inst (chatbot-system-instruction bot))
         (current-messages (conversation-messages conversation))
         (persona-memory (conversation-persona-memory conversation))
         (persona-diary-entries (conversation-persona-diary-entries conversation)))
    (list :input input
          :file-attachments file-attachments
          :effective-generation-config effective-generation-config
          :malformed-response-retry-attempted-p malformed-response-retry-attempted-p
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

(defun openai-request-target (bot)
  "Returns BOT's resolved backend, API key, base URL, and backend label."
  (let ((backend (chatbot-backend bot)))
    (list :backend backend
          :api-key (if (eq backend :lm-studio)
                       (lm-studio-api-key)
                       (openai-api-key))
          :base-url (if (eq backend :lm-studio)
                        (lm-studio-api-base-url)
                        *openai-base-url*)
          :backend-label (if (eq backend :lm-studio) "LM Studio" "OpenAI"))))

(defun openai-request-payload-alist (bot request-messages effective-generation-config)
  "Returns the chat completions payload alist for BOT."
  (let ((openai-tools (openai-request-tools bot)))
    (append (list (cons "model" (chatbot-model bot))
                  (cons "messages" request-messages)
                  (cons "stream" t))
            (when (getf effective-generation-config :temperature)
              (list (cons "temperature" (getf effective-generation-config :temperature))))
            (when (getf effective-generation-config :top-p)
              (list (cons "top_p" (getf effective-generation-config :top-p))))
            (when openai-tools
              (list (cons "tools" openai-tools))))))

(defun openai-request-url (base-url)
  "Returns the chat completions URL under BASE-URL."
  (concatenate 'string base-url "/chat/completions"))

(defun openai-request-headers (api-key)
  "Returns the standard OpenAI-compatible request headers for API-KEY."
  (list (cons "Authorization" (concatenate 'string "Bearer " api-key))
        (cons "Content-Type" "application/json")))

(defun openai-empty-tool-call-entry ()
  "Returns a fresh mutable tool-call accumulator entry."
  (list (cons :id nil)
        (cons :name nil)
        (cons :arguments (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))))

(defun ensure-openai-tool-call-entry (accumulated-tool-calls index)
  "Returns the accumulator entry for INDEX in ACCUMULATED-TOOL-CALLS, creating it when absent."
  (or (gethash index accumulated-tool-calls)
      (setf (gethash index accumulated-tool-calls)
            (openai-empty-tool-call-entry))))

(defun append-string-to-buffer (buffer text)
  "Appends TEXT to adjustable character BUFFER."
  (loop for char across text
        do (vector-push-extend char buffer))
  buffer)

(defun accumulate-openai-tool-call (accumulated-tool-calls tool-call)
  "Merges one streaming TOOL-CALL delta into ACCUMULATED-TOOL-CALLS."
  (let* ((index (cdr (assoc :index tool-call)))
         (id (cdr (assoc :id tool-call)))
         (function (cdr (assoc :function tool-call)))
         (name (cdr (assoc :name function)))
         (args (cdr (assoc :arguments function)))
         (existing (ensure-openai-tool-call-entry accumulated-tool-calls index)))
    (when id
      (setf (cdr (assoc :id existing)) id))
    (when name
      (setf (cdr (assoc :name existing)) name))
    (when args
      (append-string-to-buffer (cdr (assoc :arguments existing)) args))
    existing))

(defun handle-openai-stream-delta (delta callback full-text accumulated-tool-calls)
  "Applies one streaming DELTA to FULL-TEXT and ACCUMULATED-TOOL-CALLS."
  (let ((tool-calls (cdr (assoc :tool--calls delta)))
        (delta-text (cdr (assoc :content delta))))
    (when (and (stringp delta-text) (string/= delta-text ""))
      (append-string-to-buffer full-text delta-text)
      (when callback
        (funcall callback delta-text)))
    (when tool-calls
      (dolist (tool-call tool-calls)
        (accumulate-openai-tool-call accumulated-tool-calls tool-call)))))

(defun parse-openai-stream-event (event callback full-text accumulated-tool-calls)
  "Applies one parsed SSE EVENT to FULL-TEXT and ACCUMULATED-TOOL-CALLS."
  (let* ((choices (cdr (assoc :choices event)))
         (first-choice (car choices))
         (delta (cdr (assoc :delta first-choice))))
    (when delta
      (handle-openai-stream-delta delta callback full-text accumulated-tool-calls))))

(defun collect-openai-stream-state (stream callback stream-read-timeout)
  "Consumes STREAM and returns the accumulated full text and tool calls."
  (let ((full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (accumulated-tool-calls (make-hash-table :test 'equal)))
    (unwind-protect
         (loop for line = (read-sse-line stream
                                         :timeout-seconds stream-read-timeout
                                         :timeout-context "OpenAI streaming response")
               until (or (eq line :eof)
                         (and (stringp line)
                              (alexandria:starts-with-subseq "data: [DONE]" line)))
               do (let ((event (parse-sse-event line)))
                    (when event
                      (parse-openai-stream-event event
                                                 callback
                                                 full-text
                                                 accumulated-tool-calls))))
      (close stream))
    (list :text (coerce full-text 'string)
          :accumulated-tool-calls accumulated-tool-calls)))

(defun accumulated-openai-tool-calls (accumulated-tool-calls)
  "Returns ACCUMULATED-TOOL-CALLS as the list shape expected by provider outcomes."
  (let ((tool-calls nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (push value tool-calls))
             accumulated-tool-calls)
    (nreverse tool-calls)))

(defun openai-final-text-retryable-p (state text)
  "Returns true when TEXT should trigger one retry for malformed OpenAI-compatible output."
  (and (not (getf state :malformed-response-retry-attempted-p))
       (or (null text)
           (string= text "")
           (markup-only-text-p text))))

(defun openai-stream-state->provider-outcome (state stream-state)
  "Returns the provider outcome implied by STREAM-STATE."
  (let* ((tool-calls (accumulated-openai-tool-calls
                     (getf stream-state :accumulated-tool-calls)))
         (text (getf stream-state :text)))
    (if tool-calls
        (make-provider-turn-tool-outcome tool-calls
                                        :history-messages (getf state :history-messages)
                                        :request-messages (getf state :request-messages)
                                        :file-attachments (getf state :file-attachments)
                                        :effective-generation-config (getf state :effective-generation-config))
        (if (openai-final-text-retryable-p state text)
            (make-provider-turn-retry-outcome :reason :malformed-response)
            (make-provider-turn-final-outcome text)))))

(defun openai-api-key-or-error (request-target)
  "Returns REQUEST-TARGET's configured API key or signals when it is missing."
  (let ((api-key (getf request-target :api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "~A API Key is not set." (getf request-target :backend-label)))
    api-key))

(defun openai-turn-request-payload-json (bot state)
  "Returns the encoded chat completions request payload for STATE."
  (cl-json:encode-json-to-string
   (openai-request-payload-alist bot
                                 (getf state :request-messages)
                                 (getf state :effective-generation-config))))

(defun openai-turn-request-details (bot state)
  "Returns the request details plist for one OpenAI-compatible turn."
  (let* ((request-target (openai-request-target bot))
         (api-key (openai-api-key-or-error request-target)))
    (list :payload-json (openai-turn-request-payload-json bot state)
          :url (openai-request-url (getf request-target :base-url))
          :headers (openai-request-headers api-key)
          :stream-read-timeout (current-http-read-timeout))))

(defun post-openai-turn-request (request-details)
  "Executes one OpenAI-compatible streaming request from REQUEST-DETAILS."
  (multiple-value-bind (stream status)
      (post-web-request (getf request-details :url)
                        (getf request-details :headers)
                        (getf request-details :payload-json)
                        :want-stream t)
    (unless (= status 200)
      (error "API responded with HTTP status ~A" status))
    stream))

(defun submit-openai-turn (bot callback state)
  "Submits one OpenAI-compatible streaming turn and returns a normalized outcome."
  (let ((request-details (openai-turn-request-details bot state)))
    (openai-stream-state->provider-outcome
     state
     (collect-openai-stream-state
      (post-openai-turn-request request-details)
      callback
      (getf request-details :stream-read-timeout)))))

(defun chat-openai (bot input conversation callback
                    &key file-attachments request-messages history-messages effective-generation-config
                      return-turn-result-p
                      (recursion-depth 0))
  "Sends user input to the active conversation using an OpenAI-compliant chat completions API."
  (let ((result
          (run-provider-turn-loop
           :openai
           (openai-request-state bot input conversation file-attachments effective-generation-config
                                 :request-messages request-messages
                                 :history-messages history-messages)
           (lambda (state current-depth)
             (declare (ignore current-depth))
             (submit-openai-turn bot callback state))
           :retry-turn
           (lambda (state outcome current-depth step)
             (declare (ignore outcome))
             (let ((retry-state (copy-list state)))
               (setf (getf retry-state :malformed-response-retry-attempted-p) t)
               (funcall step retry-state current-depth)))
           :continue-with-tools
           (lambda (state outcome next-depth step)
             (declare (ignore state))
             (continue-stateless-provider-tool-recursion
              bot
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
             (finish-stateless-text-turn (getf state :history-messages)
                                         "assistant"
                                         (provider-turn-outcome-text outcome)
                                         :usage (provider-turn-outcome-usage outcome)
                                         :thought-text (provider-turn-outcome-thought-text outcome)
                                         :interaction-id (conversation-interaction-id conversation)))
           :error-handler
           (lambda (state condition current-depth)
             (declare (ignore state current-depth))
             (error "OpenAI Chat Error: ~A" condition))
           :initial-recursion-depth recursion-depth)))
    (if return-turn-result-p
        result
        (apply-chat-turn-result result conversation))))
