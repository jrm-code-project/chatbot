;;; -*- Lisp -*-
;;; backend-openai.lisp - OpenAI-compatible streaming chat flow

(in-package "CHATBOT")

(defun chat-openai (bot input conversation callback
                    &key file-attachments request-messages history-messages effective-generation-config (recursion-depth 0))
  "Sends user input to the active conversation using an OpenAI-compliant chat completions API."
  (ensure-chatbot-tool-recursion-depth :openai recursion-depth)
  (let* ((backend (chatbot-backend bot))
         (api-key (if (eq backend :lm-studio)
                      (lm-studio-api-key)
                      (openai-api-key)))
         (base-url (if (eq backend :lm-studio)
                       (lm-studio-api-base-url)
                       *openai-base-url*)))
    (unless (and api-key (string/= api-key ""))
      (error "~A API Key is not set." (if (eq backend :lm-studio) "LM Studio" "OpenAI")))
    (let* ((system-inst (chatbot-system-instruction bot))
           (current-messages (conversation-messages conversation))
           (persona-memory (conversation-persona-memory conversation))
           (persona-diary-entries (conversation-persona-diary-entries conversation))
           (history-messages (or history-messages
                                 (stateless-history-messages current-messages input)))
           (request-messages (or request-messages
                                 (build-openai-request-messages system-inst current-messages input
                                                                :chatbot bot
                                                                :persona-memory persona-memory
                                                                :persona-diary-entries persona-diary-entries
                                                                :file-attachments file-attachments)))
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
        (handler-case
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
                                    (dolist (tc tool-calls)
                                      (let* ((index (cdr (assoc :index tc)))
                                             (id (cdr (assoc :id tc)))
                                             (function (cdr (assoc :function tc)))
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
          (chatbot-tool-recursion-limit-error (e)
            (error e))
          (error (e)
            (error "OpenAI Chat Error: ~A" e)))
        (let ((tcs nil))
          (maphash (lambda (k v)
                     (declare (ignore k))
                     (push v tcs))
                   accumulated-tool-calls)
          (setf tcs (nreverse tcs))
          (if (null tcs)
              (let ((final-str (coerce full-text 'string)))
                (finish-stateless-text-turn conversation
                                            history-messages
                                            "assistant"
                                            final-str))
              (let* ((assistant-tool-calls
                      (mapcar (lambda (tc)
                                (let ((id (cdr (assoc :id tc)))
                                      (name (cdr (assoc :name tc)))
                                       (args (coerce (cdr (assoc :arguments tc)) 'string)))
                                   `(("id" . ,id)
                                     ("type" . "function")
                                     ("function" . (("name" . ,name)
                                                    ("arguments" . ,args))))))
                               tcs))
                     (assistant-msg `(("role" . "assistant")
                                      ("content" . nil)
                                      ("tool_calls" . ,assistant-tool-calls)))
                     (ordered-tool-responses
                       (map-chatbot-json-tool-call-results
                        bot
                        tcs
                        (lambda (name tool-call)
                          (declare (ignore tool-call))
                          (format nil "OpenAI tool arguments for ~A" name))
                        (lambda (id name args-str res-text tool-call)
                          (declare (ignore args-str tool-call))
                          `(("role" . "tool")
                            ("tool_call_id" . ,id)
                            ("name" . ,name)
                            ("content" . ,res-text)))
                        :error-builder
                        (lambda (id name args-str condition tool-call)
                          (declare (ignore args-str tool-call))
                          `(("role" . "tool")
                            ("tool_call_id" . ,id)
                            ("name" . ,name)
                            ("content" . ,(chatbot-tool-error-text name condition)))))))
                (let ((recursion-messages (append (list assistant-msg)
                                                  ordered-tool-responses)))
                  (continue-stateless-tool-recursion
                   conversation
                   history-messages
                   recursion-messages
                   (lambda (recursive-history recursion-messages)
                     (chat-openai bot
                                  nil
                                  conversation
                                  callback
                                  :history-messages recursive-history
                                  :request-messages (append request-messages recursion-messages)
                                  :file-attachments file-attachments
                                  :effective-generation-config effective-generation-config
                                  :recursion-depth (next-chatbot-tool-recursion-depth
                                                    :openai
                                                   recursion-depth))))))))))))
