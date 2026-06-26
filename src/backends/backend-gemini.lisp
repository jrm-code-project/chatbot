;;; -*- Lisp -*-
;;; backend-gemini.lisp - Gemini Interactions API flow

(in-package "CHATBOT")

(defun gemini-fallback-to-google-enabled-p (bot)
  "Returns true when BOT should fall back to Google generateContent on Interactions 404 errors."
  (chatbot-gemini-fallback-to-google-p bot))

(defun restore-gemini-interaction-id (conversation original-interaction-id)
  "Restores CONVERSATION's Gemini interaction state to ORIGINAL-INTERACTION-ID."
  (setf (conversation-interaction-id conversation) original-interaction-id))

(defun retry-gemini-turn-on-google-gemini-pro-latest (bot input conversation callback
                                                           &key file-attachments effective-generation-config (recursion-depth 0))
  "Resubmits a Gemini turn through the Google backend on gemini-pro-latest."
  (retry-on-google-gemini-pro-latest bot
                                     input
                                     conversation
                                     callback
                                     :file-attachments file-attachments
                                     :effective-generation-config effective-generation-config
                                     :recursion-depth recursion-depth))

(defun chat-gemini (bot input conversation callback &key file-attachments effective-model effective-generation-config (recursion-depth 0))
  "Sends user input to the active conversation using the Gemini Interactions API."
  (ensure-chatbot-tool-recursion-depth :gemini recursion-depth)
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    (let* ((original-interaction-id (conversation-interaction-id conversation))
           (payload-alist (make-interaction-payload
                           bot
                           input
                           :messages (conversation-messages conversation)
                           :persona-memory (conversation-persona-memory conversation)
                           :persona-diary-entries (conversation-persona-diary-entries conversation)
                           :previous-interaction-id original-interaction-id
                           :file-attachments file-attachments
                           :effective-model effective-model
                           :effective-generation-config effective-generation-config
                           :stream t))
           (payload-json (cl-json:encode-json-to-string payload-alist))
           (url (concatenate 'string *gemini-base-url* "/interactions?alt=sse"))
           (headers (list (cons "x-goog-api-key" api-key)
                          (cons "Api-Revision" (gemini-api-revision))
                          (cons "Content-Type" "application/json")))
           (stream-read-timeout (current-http-read-timeout))
           (full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
           (active-fn-call nil)
           (function-calls-to-run nil)
           (completed-usage nil)
           (completed-stop-reason nil)
           (completed-interaction-p nil))
      (handler-case
          (funcall
           (lambda ()
             (multiple-value-bind (stream status)
                (post-web-request url headers payload-json :want-stream t)
              (unless (= status 200)
                (error "API responded with HTTP status ~A" status))
              (unwind-protect
                   (loop for line = (read-sse-line stream
                                                  :timeout-seconds stream-read-timeout
                                                  :timeout-context "Gemini streaming response")
                         until (eq line :eof)
                         do (let ((event (parse-sse-event line)))
                              (when event
                                (let ((event-type (cdr (assoc :event--type event))))
                                  (cond
                                    ((string= event-type "step.start")
                                     (let* ((step (cdr (assoc :step event)))
                                            (type (cdr (assoc :type step)))
                                            (id (cdr (assoc :id step)))
                                            (name (cdr (assoc :name step))))
                                       (when (string= type "function_call")
                                         (setf active-fn-call
                                               (list (cons :id id)
                                                     (cons :name name)
                                                     (cons :arguments (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))))))
                                    ((string= event-type "step.delta")
                                     (let* ((delta (cdr (assoc :delta event)))
                                            (delta-type (cdr (assoc :type delta)))
                                            (delta-text (cdr (assoc :text delta)))
                                            (delta-args (cdr (assoc :arguments delta))))
                                       (cond
                                         ((and (string= delta-type "text") (stringp delta-text))
                                          (loop for char across delta-text
                                                do (vector-push-extend char full-text))
                                          (when callback
                                            (funcall callback delta-text)))
                                         ((and active-fn-call delta-args)
                                          (loop for char across delta-args
                                                do (vector-push-extend char (cdr (assoc :arguments active-fn-call))))))))
                                    ((string= event-type "step.stop")
                                     (when active-fn-call
                                       (push active-fn-call function-calls-to-run)
                                       (setf active-fn-call nil)))
                                    ((or (string= event-type "interaction.created")
                                         (string= event-type "interaction.completed"))
                                     (let* ((interaction (cdr (assoc :interaction event)))
                                            (id (cdr (assoc :id interaction))))
                                       (when id
                                         (setf (conversation-interaction-id conversation) id))
                                       (when (string= event-type "interaction.completed")
                                         (setf completed-interaction-p t)
                                         (setf completed-usage (cdr (assoc :usage interaction)))
                                         (setf completed-stop-reason (response-stop-reason interaction))
                                         (log-backend-response-stats
                                          :gemini
                                          :http-status status
                                          :interaction-id id
                                          :model (cdr (assoc :model interaction))
                                          :finish-reason completed-stop-reason
                                          :usage completed-usage))))
                                    ((string= event-type "interaction.status_update")
                                     (let ((id (cdr (assoc :interaction--id event))))
                                       (when id
                                         (setf (conversation-interaction-id conversation) id)))))))))
                (close stream)))))
        (chatbot-tool-recursion-limit-error (e)
          (restore-gemini-interaction-id conversation original-interaction-id)
          (error e))
        (error (e)
          (let ((message (string-downcase (princ-to-string e))))
            (restore-gemini-interaction-id conversation original-interaction-id)
            (if (and (gemini-fallback-to-google-enabled-p bot)
                     (search "/interactions?alt=sse" message)
                     (search "404" message)
                     (search "not found" message))
                (return-from chat-gemini
                  (chat-google bot
                               input
                               conversation
                               callback
                               :file-attachments file-attachments
                               :effective-model effective-model
                               :effective-generation-config effective-generation-config
                               :recursion-depth recursion-depth))
                (error "Gemini Chat Error: ~A" e)))))
      (if function-calls-to-run
          (let ((results
                  (map-chatbot-json-tool-call-results
                   bot
                   (nreverse function-calls-to-run)
                   (lambda (name tool-call)
                     (declare (ignore tool-call))
                     (format nil "Gemini tool arguments for ~A" name))
                   (lambda (id name args-str res-text tool-call)
                     (declare (ignore args-str tool-call))
                     `(("type" . "function_result")
                       ("name" . ,name)
                       ("call_id" . ,id)
                       ("result" . ,(list `(("type" . "text") ("text" . ,res-text))))))
                   :error-builder
                   (lambda (id name args-str condition tool-call)
                     (declare (ignore args-str tool-call))
                     `(("type" . "function_result")
                       ("name" . ,name)
                       ("call_id" . ,id)
                       ("result" . ,(list `(("type" . "text")
                                            ("text" . ,(chatbot-tool-error-text name condition))))))))))
            (chat-gemini bot
                         results
                         conversation
                         callback
                         :effective-model effective-model
                         :effective-generation-config effective-generation-config
                         :recursion-depth (next-chatbot-tool-recursion-depth
                                           :gemini
                                           recursion-depth)))
          (progn
            (when (and (stringp input)
                       completed-interaction-p
                       (or (malformed-response-stop-reason-p completed-stop-reason)
                           (= 0 (length full-text))))
              (restore-gemini-interaction-id conversation original-interaction-id)
              (return-from chat-gemini
                (retry-gemini-turn-on-google-gemini-pro-latest
                 bot
                 input
                 conversation
                 callback
                 :file-attachments file-attachments
                 :effective-generation-config effective-generation-config
                 :recursion-depth recursion-depth)))
            (emit-chat-response-text (coerce full-text 'string)
                                     :usage completed-usage))))))
