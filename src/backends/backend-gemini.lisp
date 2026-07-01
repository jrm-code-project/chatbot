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

(defun retry-gemini-turn-on-google-gemini-pro-latest (bot input conversation callback
                                                           &key file-attachments effective-generation-config
                                                             return-turn-result-p
                                                             (recursion-depth 0))
  "Resubmits a Gemini turn through the Google backend on gemini-pro-latest."
  (retry-on-google-gemini-pro-latest bot
                                     input
                                     conversation
                                     callback
                                     :file-attachments file-attachments
                                     :effective-generation-config effective-generation-config
                                     :return-turn-result-p return-turn-result-p
                                     :recursion-depth recursion-depth))

(defun gemini-request-state (input conversation file-attachments effective-model effective-generation-config
                                  &key messages persona-memory persona-diary-entries
                                    original-interaction-id current-interaction-id)
  "Builds the provider-runner state for a Gemini Interactions turn."
  (list :input input
        :file-attachments file-attachments
        :effective-model effective-model
        :effective-generation-config effective-generation-config
        :messages (or messages (conversation-messages conversation))
        :persona-memory (or persona-memory (conversation-persona-memory conversation))
        :persona-diary-entries (or persona-diary-entries (conversation-persona-diary-entries conversation))
        :original-interaction-id (or original-interaction-id
                                     (conversation-interaction-id conversation))
        :current-interaction-id (or current-interaction-id
                                    (conversation-interaction-id conversation))))

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

(defun gemini-request-url ()
  "Returns the Gemini Interactions streaming endpoint."
  (concatenate 'string *gemini-base-url* "/interactions?alt=sse"))

(defun gemini-request-headers (api-key)
  "Returns the Gemini Interactions request headers for API-KEY."
  (list (cons "x-goog-api-key" api-key)
        (cons "Api-Revision" (gemini-api-revision))
        (cons "Content-Type" "application/json")))

(defun gemini-append-buffer-text (buffer text)
  "Appends TEXT to adjustable character BUFFER."
  (loop for char across text
        do (vector-push-extend char buffer))
  buffer)

(defun gemini-empty-function-call (id name)
  "Returns a fresh function-call accumulator for ID and NAME."
  (list (cons :id id)
        (cons :name name)
        (cons :arguments (make-array 0 :element-type 'character
                                    :fill-pointer 0
                                    :adjustable t))))

(defun gemini-initial-stream-state (current-interaction-id)
  "Returns the initial mutable state used while consuming a Gemini SSE stream."
  (list :current-interaction-id current-interaction-id
        :full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
        :full-thought-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
        :active-fn-call nil
        :function-calls-to-run nil
        :completed-usage nil
        :completed-stop-reason nil
        :completed-interaction-p nil))

(defun gemini-stream-state-current-interaction-id (stream-state)
  "Returns STREAM-STATE's latest interaction id."
  (getf stream-state :current-interaction-id))

(defun gemini-stream-state-full-text (stream-state)
  "Returns STREAM-STATE's accumulated visible text buffer."
  (getf stream-state :full-text))

(defun gemini-stream-state-full-thought-text (stream-state)
  "Returns STREAM-STATE's accumulated thought text buffer."
  (getf stream-state :full-thought-text))

(defun gemini-stream-state-active-fn-call (stream-state)
  "Returns STREAM-STATE's in-progress function call accumulator."
  (getf stream-state :active-fn-call))

(defun gemini-stream-state-function-calls-to-run (stream-state)
  "Returns STREAM-STATE's completed function-call accumulator list."
  (getf stream-state :function-calls-to-run))

(defun gemini-stream-state-completed-usage (stream-state)
  "Returns STREAM-STATE's completed usage payload."
  (getf stream-state :completed-usage))

(defun gemini-stream-state-completed-stop-reason (stream-state)
  "Returns STREAM-STATE's completed stop reason."
  (getf stream-state :completed-stop-reason))

(defun gemini-stream-state-completed-interaction-p (stream-state)
  "Returns whether STREAM-STATE observed interaction.completed."
  (getf stream-state :completed-interaction-p))

(defun gemini-handle-step-start (stream-state event)
  "Updates STREAM-STATE for one Gemini step.start EVENT."
  (let* ((step (cdr (assoc :step event)))
         (type (cdr (assoc :type step)))
         (id (cdr (assoc :id step)))
         (name (cdr (assoc :name step))))
    (when (string= type "function_call")
      (setf (getf stream-state :active-fn-call)
           (gemini-empty-function-call id name))))
  stream-state)

(defun gemini-handle-step-delta (stream-state delta callback)
  "Updates STREAM-STATE for one Gemini step DELTA."
  (let* ((delta-type (cdr (assoc :type delta)))
         (delta-text (cdr (assoc :text delta)))
         (delta-args (cdr (assoc :arguments delta)))
         (active-fn-call (gemini-stream-state-active-fn-call stream-state)))
    (cond
      ((and (string= delta-type "text")
           (stringp delta-text))
       (gemini-append-buffer-text (gemini-stream-state-full-text stream-state) delta-text)
       (when callback
         (funcall callback delta-text)))
      ((and (gemini-thought-delta-type-p delta-type)
           (stringp delta-text))
       (gemini-append-buffer-text (gemini-stream-state-full-thought-text stream-state) delta-text))
      ((and active-fn-call delta-args)
       (gemini-append-buffer-text (cdr (assoc :arguments active-fn-call)) delta-args))))
  stream-state)

(defun gemini-handle-step-stop (stream-state)
  "Moves any active Gemini function call from in-progress to ready-to-run."
  (let ((active-fn-call (gemini-stream-state-active-fn-call stream-state)))
    (when active-fn-call
      (push active-fn-call (getf stream-state :function-calls-to-run))
      (setf (getf stream-state :active-fn-call) nil)))
  stream-state)

(defun gemini-handle-interaction-event (stream-state event event-type status)
  "Updates STREAM-STATE for one Gemini interaction EVENT of EVENT-TYPE."
  (let* ((interaction (cdr (assoc :interaction event)))
         (id (cdr (assoc :id interaction))))
    (when id
      (setf (getf stream-state :current-interaction-id) id))
    (when (string= event-type "interaction.completed")
      (setf (getf stream-state :completed-interaction-p) t)
      (setf (getf stream-state :completed-usage) (cdr (assoc :usage interaction)))
      (setf (getf stream-state :completed-stop-reason) (response-stop-reason interaction))
      (log-backend-response-stats
       :gemini
       :http-status status
       :interaction-id id
       :model (cdr (assoc :model interaction))
       :finish-reason (gemini-stream-state-completed-stop-reason stream-state)
       :usage (gemini-stream-state-completed-usage stream-state))))
  stream-state)

(defun gemini-handle-status-update (stream-state event)
  "Updates STREAM-STATE for one Gemini interaction.status_update EVENT."
  (let ((id (cdr (assoc :interaction--id event))))
    (when id
      (setf (getf stream-state :current-interaction-id) id)))
  stream-state)

(defun gemini-handle-stream-event (stream-state event callback status)
  "Updates STREAM-STATE for one parsed Gemini SSE EVENT."
  (let ((event-type (cdr (assoc :event--type event))))
    (cond
      ((string= event-type "step.start")
       (gemini-handle-step-start stream-state event))
      ((string= event-type "step.delta")
       (gemini-handle-step-delta stream-state (cdr (assoc :delta event)) callback))
      ((string= event-type "step.stop")
       (gemini-handle-step-stop stream-state))
      ((or (string= event-type "interaction.created")
           (string= event-type "interaction.completed"))
       (gemini-handle-interaction-event stream-state event event-type status))
      ((string= event-type "interaction.status_update")
       (gemini-handle-status-update stream-state event))
      (t stream-state))))

(defun collect-gemini-stream-state (stream callback stream-read-timeout status current-interaction-id)
  "Consumes STREAM and returns the accumulated Gemini stream state."
  (let ((stream-state (gemini-initial-stream-state current-interaction-id)))
    (unwind-protect
         (loop for line = (read-sse-line stream
                                        :timeout-seconds stream-read-timeout
                                        :timeout-context "Gemini streaming response")
              until (eq line :eof)
              do (let ((event (parse-sse-event line)))
                   (when event
                     (gemini-handle-stream-event stream-state event callback status))))
      (close stream))
    stream-state))

(defun gemini-tool-call-outcome (state stream-state)
  "Returns the provider tool outcome implied by STREAM-STATE."
  (make-provider-turn-tool-outcome
   (nreverse (gemini-stream-state-function-calls-to-run stream-state))
   :interaction-id (gemini-stream-state-current-interaction-id stream-state)
   :effective-model (getf state :effective-model)
   :effective-generation-config (getf state :effective-generation-config)))

(defun gemini-response-retryable-p (state stream-state)
  "Returns true when STREAM-STATE should trigger the Google retry path."
  (and (stringp (getf state :input))
       (gemini-stream-state-completed-interaction-p stream-state)
       (or (malformed-response-stop-reason-p
           (gemini-stream-state-completed-stop-reason stream-state))
           (= 0 (length (gemini-stream-state-full-text stream-state))))))

(defun gemini-stream-state->provider-outcome (state stream-state)
  "Returns the provider outcome implied by STREAM-STATE."
  (cond
    ((gemini-stream-state-function-calls-to-run stream-state)
     (gemini-tool-call-outcome state stream-state))
    ((gemini-response-retryable-p state stream-state)
     (make-provider-turn-retry-outcome
      :interaction-id (gemini-stream-state-current-interaction-id stream-state)))
    (t
     (make-provider-turn-final-outcome
      (coerce (gemini-stream-state-full-text stream-state) 'string)
      :interaction-id (gemini-stream-state-current-interaction-id stream-state)
      :usage (gemini-stream-state-completed-usage stream-state)
      :thought-text (coerce (gemini-stream-state-full-thought-text stream-state) 'string)))))

(defun submit-gemini-turn (bot callback state)
  "Submits one Gemini Interactions turn and returns a normalized outcome."
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    (let* ((current-interaction-id (getf state :current-interaction-id))
           (payload-alist (make-interaction-payload
                           bot
                           (getf state :input)
                           :messages (getf state :messages)
                           :persona-memory (getf state :persona-memory)
                           :persona-diary-entries (getf state :persona-diary-entries)
                           :previous-interaction-id current-interaction-id
                           :file-attachments (getf state :file-attachments)
                           :effective-model (getf state :effective-model)
                           :effective-generation-config (getf state :effective-generation-config)
                           :stream t))
           (payload-json (cl-json:encode-json-to-string payload-alist))
           (url (gemini-request-url))
           (headers (gemini-request-headers api-key))
           (stream-read-timeout (current-http-read-timeout)))
      (multiple-value-bind (stream status)
          (post-web-request url headers payload-json :want-stream t)
        (unless (= status 200)
          (error "API responded with HTTP status ~A" status))
        (gemini-stream-state->provider-outcome
         state
         (collect-gemini-stream-state
          stream
          callback
          stream-read-timeout
          status
          current-interaction-id))))))

(defun chat-gemini (bot input conversation callback &key file-attachments effective-model effective-generation-config
                                                     return-turn-result-p
                                                     (recursion-depth 0))
  "Sends user input to the active conversation using the Gemini Interactions API."
  (let ((result
          (run-provider-turn-loop
           :gemini
           (gemini-request-state input conversation file-attachments effective-model effective-generation-config)
           (lambda (state current-depth)
             (declare (ignore current-depth))
             (submit-gemini-turn bot callback state))
           :retry-turn
           (lambda (state outcome current-depth step)
             (declare (ignore outcome step))
             (retry-gemini-turn-on-google-gemini-pro-latest
              bot
              (getf state :input)
              conversation
              callback
              :file-attachments (getf state :file-attachments)
              :effective-generation-config (getf state :effective-generation-config)
              :return-turn-result-p t
              :recursion-depth current-depth))
           :continue-with-tools
           (lambda (state outcome next-depth step)
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
                                              (getf outcome :effective-generation-config)
                                              :messages (getf state :messages)
                                              :persona-memory (getf state :persona-memory)
                                              :persona-diary-entries (getf state :persona-diary-entries)
                                              :original-interaction-id (getf state :original-interaction-id)
                                              :current-interaction-id (getf outcome :interaction-id))
                        next-depth)))
           :finalize-turn
           (lambda (state outcome)
             (declare (ignore state))
             (emit-chat-response-text (provider-turn-outcome-text outcome)
                                      :usage (provider-turn-outcome-usage outcome)
                                      :thought-text (provider-turn-outcome-thought-text outcome))
             (make-chat-turn-result
              (provider-turn-outcome-text outcome)
              :messages (conversation-messages conversation)
              :interaction-id (getf outcome :interaction-id)
              :usage (provider-turn-outcome-usage outcome)
              :thought-text (provider-turn-outcome-thought-text outcome)))
           :error-handler
           (lambda (state condition current-depth)
             (declare (ignore current-depth))
             (let ((message (string-downcase (princ-to-string condition))))
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
                                :return-turn-result-p t
                                :recursion-depth recursion-depth)
                   (error "Gemini Chat Error: ~A" condition))))
           :initial-recursion-depth recursion-depth)))
    (if return-turn-result-p
        result
        (apply-chat-turn-result result conversation))))
