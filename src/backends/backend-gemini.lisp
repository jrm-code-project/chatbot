;;; -*- Lisp -*-
;;; backend-gemini.lisp - Gemini Interactions API flow

(in-package "CHATBOT")

(defun gemini-thought-delta-type-p (delta-type)
  "Returns true when DELTA-TYPE represents streamed thought text."
  (member delta-type
          '("reasoning" "thinking" "thought")
          :test #'string-equal))

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

(defun gemini-request-state (input conversation file-attachments effective-model effective-generation-config)
  "Builds the provider-runner state for a Gemini Interactions turn."
  (list :input input
        :file-attachments file-attachments
        :effective-model effective-model
        :effective-generation-config effective-generation-config
        :original-interaction-id (conversation-interaction-id conversation)))

(defun gemini-tool-result-message (id name args-str res-text tool-call)
  "Returns the Gemini Interactions function_result payload for a successful tool call."
  (declare (ignore args-str tool-call))
  `(("type" . "function_result")
    ("name" . ,name)
    ("call_id" . ,id)
    ("result" . ,(list `(("type" . "text")
                         ("text" . ,res-text))))))

(defun gemini-tool-error-message (id name args-str condition tool-call)
  "Returns the Gemini Interactions function_result payload for a failed tool call."
  (declare (ignore args-str tool-call))
  `(("type" . "function_result")
    ("name" . ,name)
    ("call_id" . ,id)
    ("result" . ,(list `(("type" . "text")
                         ("text" . ,(chatbot-tool-error-text name condition)))))))

(defun submit-gemini-turn (bot conversation callback state)
  "Submits one Gemini Interactions turn and returns a normalized outcome."
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    (let* ((original-interaction-id (getf state :original-interaction-id))
           (payload-alist (make-interaction-payload
                           bot
                           (getf state :input)
                           :messages (conversation-messages conversation)
                           :persona-memory (conversation-persona-memory conversation)
                           :persona-diary-entries (conversation-persona-diary-entries conversation)
                           :previous-interaction-id original-interaction-id
                           :file-attachments (getf state :file-attachments)
                           :effective-model (getf state :effective-model)
                           :effective-generation-config (getf state :effective-generation-config)
                           :stream t))
           (payload-json (cl-json:encode-json-to-string payload-alist))
           (url (concatenate 'string *gemini-base-url* "/interactions?alt=sse"))
           (headers (list (cons "x-goog-api-key" api-key)
                          (cons "Api-Revision" (gemini-api-revision))
                          (cons "Content-Type" "application/json")))
           (stream-read-timeout (current-http-read-timeout))
           (full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
           (full-thought-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
           (active-fn-call nil)
           (function-calls-to-run nil)
           (completed-usage nil)
           (completed-stop-reason nil)
           (completed-interaction-p nil))
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
                                                  (cons :arguments (make-array 0 :element-type 'character
                                                                               :fill-pointer 0
                                                                               :adjustable t)))))))
                                 ((string= event-type "step.delta")
                                  (let* ((delta (cdr (assoc :delta event)))
                                         (delta-type (cdr (assoc :type delta)))
                                         (delta-text (cdr (assoc :text delta)))
                                         (delta-args (cdr (assoc :arguments delta))))
                                    (cond
                                      ((and (string= delta-type "text")
                                            (stringp delta-text))
                                       (loop for char across delta-text
                                             do (vector-push-extend char full-text))
                                       (when callback
                                         (funcall callback delta-text)))
                                      ((and (gemini-thought-delta-type-p delta-type)
                                            (stringp delta-text))
                                       (loop for char across delta-text
                                             do (vector-push-extend char full-thought-text)))
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
      (cond
        (function-calls-to-run
         (make-provider-turn-tool-outcome
          (nreverse function-calls-to-run)
          :effective-model (getf state :effective-model)
          :effective-generation-config (getf state :effective-generation-config)))
        ((and (stringp (getf state :input))
              completed-interaction-p
              (or (malformed-response-stop-reason-p completed-stop-reason)
                  (= 0 (length full-text))))
         (make-provider-turn-retry-outcome))
        (t
         (make-provider-turn-final-outcome (coerce full-text 'string)
                                           :usage completed-usage
                                           :thought-text (coerce full-thought-text 'string)))))))

(defun chat-gemini (bot input conversation callback &key file-attachments effective-model effective-generation-config (recursion-depth 0))
  "Sends user input to the active conversation using the Gemini Interactions API."
  (run-provider-turn-loop
   :gemini
   (gemini-request-state input conversation file-attachments effective-model effective-generation-config)
   (lambda (state current-depth)
     (declare (ignore current-depth))
     (submit-gemini-turn bot conversation callback state))
   :retry-turn
   (lambda (state outcome current-depth step)
     (declare (ignore outcome step))
     (restore-gemini-interaction-id conversation (getf state :original-interaction-id))
     (retry-gemini-turn-on-google-gemini-pro-latest
      bot
      (getf state :input)
      conversation
      callback
      :file-attachments (getf state :file-attachments)
      :effective-generation-config (getf state :effective-generation-config)
      :recursion-depth current-depth))
   :continue-with-tools
   (lambda (state outcome next-depth step)
     (declare (ignore state))
     (let ((results
             (provider-tool-call-results
              bot
              (provider-turn-outcome-tool-calls outcome)
              (lambda (name tool-call)
                (declare (ignore tool-call))
                (format nil "Gemini tool arguments for ~A" name))
              #'gemini-tool-result-message
              :error-builder #'gemini-tool-error-message)))
       (funcall step
                (gemini-request-state results
                                      conversation
                                      nil
                                      (getf outcome :effective-model)
                                      (getf outcome :effective-generation-config))
                next-depth)))
   :finalize-turn
   (lambda (state outcome)
     (declare (ignore state))
     (emit-chat-response-text (provider-turn-outcome-text outcome)
                              :usage (provider-turn-outcome-usage outcome)
                              :thought-text (provider-turn-outcome-thought-text outcome)))
   :error-handler
   (lambda (state condition current-depth)
     (declare (ignore current-depth))
     (let ((message (string-downcase (princ-to-string condition))))
       (restore-gemini-interaction-id conversation (getf state :original-interaction-id))
       (if (and (gemini-fallback-to-google-enabled-p bot)
                (search "/interactions?alt=sse" message)
                (search "404" message)
                (search "not found" message))
           (chat-google bot
                        (getf state :input)
                        conversation
                        callback
                        :file-attachments (getf state :file-attachments)
                        :effective-model (getf state :effective-model)
                        :effective-generation-config (getf state :effective-generation-config)
                        :recursion-depth recursion-depth)
           (error "Gemini Chat Error: ~A" condition))))
   :initial-recursion-depth recursion-depth))
