;;; -*- Lisp -*-
;;; backend-gemini.lisp - Gemini Interactions API flow

(in-package "CHATBOT")

(defun chat-gemini (bot input conversation callback)
  "Sends user input to the active conversation using the Gemini Interactions API."
  (let ((api-key (gemini-api-key)))
    (unless (and api-key (string/= api-key ""))
      (error "Gemini API Key is not set. Please ensure (gemini-api-key) is configured."))
    (let* ((payload-alist (make-interaction-payload
                           bot
                           input
                           :messages (conversation-messages conversation)
                           :persona-memory (conversation-persona-memory conversation)
                           :previous-interaction-id (conversation-interaction-id conversation)
                           :stream t))
           (payload-json (cl-json:encode-json-to-string payload-alist))
           (url (concatenate 'string *gemini-base-url* "/interactions?alt=sse"))
           (headers (list (cons "x-goog-api-key" api-key)
                          (cons "Api-Revision" (gemini-api-revision))
                          (cons "Content-Type" "application/json")))
           (full-text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
           (active-fn-call nil)
           (function-calls-to-run nil)
           (completed-usage nil))
      (handler-case
          (multiple-value-bind (stream status)
              (post-web-request url headers payload-json :want-stream t)
            (if (= status 200)
                (unwind-protect
                     (loop for line = (read-sse-line stream)
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
                                           (setf completed-usage (cdr (assoc :usage interaction)))
                                           (log-backend-response-stats
                                            :gemini
                                            :http-status status
                                            :interaction-id id
                                            :model (cdr (assoc :model interaction))
                                            :usage completed-usage))))
                                      ((string= event-type "interaction.status_update")
                                       (let ((id (cdr (assoc :interaction--id event))))
                                         (when id
                                           (setf (conversation-interaction-id conversation) id)))))))))
                  (close stream))
                (error "API responded with HTTP status ~A" status)))
        (error (e)
          (let ((message (string-downcase (princ-to-string e))))
            (if (and (search "/interactions?alt=sse" message)
                     (search "404" message)
                     (search "not found" message))
                (return-from chat-gemini (chat-google bot input conversation callback))
                (error "Gemini Chat Error: ~A" e)))))
      (if function-calls-to-run
          (let ((results nil))
            (dolist (fc (nreverse function-calls-to-run))
              (let* ((id (cdr (assoc :id fc)))
                     (name (cdr (assoc :name fc)))
                     (args-str (coerce (cdr (assoc :arguments fc)) 'string))
                     (args (parse-json-or-error args-str :context (format nil "Gemini tool arguments for ~A" name))))
                (multiple-value-bind (srv tool) (find-mcp-server-and-tool bot name)
                  (declare (ignore tool))
                  (unless srv
                    (error "MCP tool not found: ~A" name))
                  (let ((res-text (execute-mcp-tool srv name args)))
                    (push `(("type" . "function_result")
                            ("name" . ,name)
                            ("call_id" . ,id)
                            ("result" . ,(list `(("type" . "text") ("text" . ,res-text)))))
                          results)))))
            (chat-gemini bot (nreverse results) conversation callback))
          (progn
            (format-paragraphs full-text :width 80)
            (write-turn-token-summary completed-usage)
            (coerce full-text 'string))))))
