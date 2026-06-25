;;; -*- Lisp -*-
;;; sandbox-personas.lisp - runtime persona registry and sandbox orchestration

(in-package "CHATBOT")

(defparameter *active-personas* (make-hash-table :test 'equal)
  "Registry mapping active sandbox persona names to PERSONA instances.")

(defparameter *active-personas-lock*
  (sb-thread:make-mutex :name "active-personas-lock")
  "Mutex protecting the active sandbox persona registry.")

(defparameter +stock-persona-definitions+
  '((:r-lee-ermey-drill-sergeant
     :display-name "Drill Sergeant"
     :role "R. Lee Ermey channeling a relentless Marine Corps drill sergeant"
     :tone "intense, abrasive, profane, and commanding, but still ultimately useful"
     :directives ("Drive the user hard and do not coddle them."
                  "Use short, forceful, memorable phrasing."
                  "Translate weak ideas into concrete action items."
                  "When giving feedback, be brutally direct but still technically correct.")
     :constraints ("Do not encourage illegal, dangerous, or abusive conduct."
                   "Stay focused on helping the user accomplish the task."
                   "Do not break character unless explicitly asked."))
    (:richard-feynman
     :display-name "Richard Feynman"
     :role "Richard Feynman as a brilliant physics teacher and skeptical explainer"
     :tone "curious, plainspoken, incisive, playful, and deeply explanatory"
     :directives ("Explain ideas in simple language before introducing formalism."
                  "Use concrete examples and thought experiments."
                  "Question hidden assumptions and expose confusion clearly."
                  "Prefer understanding over jargon or prestige.")
     :constraints ("Do not claim certainty beyond the evidence."
                   "Admit when something is unknown or underspecified."
                   "Keep the focus on clear reasoning, not mystique.")))
  "Built-in sandbox persona templates keyed by keyword identifier.")

(defun validate-persona-name (name)
  "Returns NAME as a non-empty string suitable for the sandbox registry."
  (let ((name-string (and name (string name))))
    (unless (and name-string
                 (string/= "" (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                           name-string)))
      (error "Persona names must be non-empty strings."))
    name-string))

(defun normalize-stock-persona-key (designator)
  "Returns DESIGNATOR normalized to a stock persona keyword."
  (etypecase designator
    (keyword designator)
    (symbol (intern (string-upcase (symbol-name designator)) "KEYWORD"))
    (string (intern (string-upcase designator) "KEYWORD"))))

(defun stock-persona-definition (designator)
  "Returns the stock persona definition for DESIGNATOR or signals an error."
  (let ((key (normalize-stock-persona-key designator)))
    (or (assoc key +stock-persona-definitions+)
        (error "Unknown stock persona ~A. Available stock personas: ~{~A~^, ~}."
               key
               (list-stock-personas)))))

(defun normalize-persona-text-list (value field-name)
  "Returns VALUE normalized to a list of strings for FIELD-NAME."
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((listp value)
     (mapcar (lambda (item)
               (if (stringp item)
                   item
                   (error "~A entries must be strings: ~S" field-name item)))
             value))
    (t
     (error "~A must be NIL, a string, or a list of strings: ~S" field-name value))))

(defun format-persona-bullet-section (title items)
  "Returns TITLE and ITEMS as a bullet-list paragraph block."
  (when items
    (format nil "~A~%~{  - ~A~%~}" title items)))

(defun build-persona-system-instruction (&key role tone directives constraints context)
  "Builds a sandbox persona system instruction string from structured options."
  (let* ((directive-list (normalize-persona-text-list directives ":DIRECTIVES"))
         (constraint-list (normalize-persona-text-list constraints ":CONSTRAINTS"))
         (context-list (normalize-persona-text-list context ":CONTEXT"))
         (paragraphs
           (remove nil
                   (list (when role
                           (format nil "You are ~A." role))
                         (when tone
                           (format nil "Adopt this tone: ~A." tone))
                         (format-persona-bullet-section "Context:" context-list)
                         (format-persona-bullet-section "Directives:" directive-list)
                         (format-persona-bullet-section "Constraints:" constraint-list)))))
    (when paragraphs
      (format nil "~{~A~^~%~%~}" paragraphs))))

(defun register-active-persona (persona)
  "Registers PERSONA in the global sandbox registry."
  (sb-thread:with-mutex (*active-personas-lock*)
    (let ((name (persona-name persona)))
      (when (gethash name *active-personas*)
        (error "A sandbox persona named ~A is already active." name))
      (setf (gethash name *active-personas*) persona)))
  persona)

(defun active-persona-objects ()
  "Returns the registered sandbox personas in stable name order."
  (sb-thread:with-mutex (*active-personas-lock*)
    (sort (loop for persona being the hash-values of *active-personas*
                collect persona)
          #'string<
          :key #'persona-name)))

(defun require-active-persona (designator)
  "Returns the active sandbox persona named by DESIGNATOR or signals an error."
  (typecase designator
    (persona designator)
    (t
     (let ((name (validate-persona-name designator)))
       (or (sb-thread:with-mutex (*active-personas-lock*)
             (gethash name *active-personas*))
           (error "No active sandbox persona named ~A." name))))))

(defun spawn-persona (name
                      &key persona-name model system-instruction role tone directives constraints context
                        temperature top-p (backend :gemini) runtime-context)
  "Creates, registers, and returns a new active sandbox persona."
  (let* ((registry-name (validate-persona-name name))
         (generated-system-instruction
           (or system-instruction
               (build-persona-system-instruction
                :role role
                :tone tone
                :directives directives
                :constraints constraints
                :context context)))
         (conversation
           (if persona-name
               (new-chat-persona persona-name :runtime-context runtime-context)
               (new-chat :backend backend
                         :model model
                         :system-instruction generated-system-instruction
                         :temperature temperature
                         :top-p top-p
                         :runtime-context runtime-context)))
         (persona (make-instance 'persona
                                 :name registry-name
                                 :conversation conversation
                                 :history nil
                                 :prompt-options (remove nil
                                                         (list (when role (cons :role role))
                                                               (when tone (cons :tone tone))
                                                               (when directives (cons :directives directives))
                                                               (when constraints (cons :constraints constraints))
                                                               (when context (cons :context context)))))))
    (when generated-system-instruction
      (let ((bot (conversation-chatbot conversation)))
        (setf (chatbot-system-instruction bot) generated-system-instruction)
        (setf (chatbot-system-instruction-path bot) nil)
        (setf (chatbot-system-instruction-storage-kind bot) :transient)))
    (when (or temperature top-p)
      (apply #'set-sampling-parameters
             persona
             (append (when temperature
                       (list :temperature temperature))
                     (when top-p
                       (list :top-p top-p)))))
    (register-active-persona persona)))

(defun find-persona (name)
  "Returns the active sandbox persona named NAME, or NIL when absent."
  (let ((validated-name (validate-persona-name name)))
    (sb-thread:with-mutex (*active-personas-lock*)
      (gethash validated-name *active-personas*))))

(defun remove-persona (designator)
  "Removes DESIGNATOR from the active sandbox persona registry and returns it when present."
  (let ((name (validate-persona-name (if (typep designator 'persona)
                                         (persona-name designator)
                                         designator))))
    (sb-thread:with-mutex (*active-personas-lock*)
      (multiple-value-bind (persona presentp)
          (gethash name *active-personas*)
        (when presentp
          (remhash name *active-personas*))
        persona))))

(defun list-personas ()
  "Returns the active sandbox persona names in stable order."
  (mapcar #'persona-name (active-persona-objects)))

(defun show-personas (&key (stream *standard-output*))
  "Prints the active sandbox personas and returns their names."
  (let ((personas (active-persona-objects)))
    (if personas
        (dolist (persona personas)
          (let* ((conversation (persona-conversation persona))
                 (bot (conversation-chatbot conversation))
                 (history-turns (/ (length (persona-history persona)) 2)))
            (format stream "~A~%" (persona-name persona))
            (format stream "  backend: ~A~%" (chatbot-backend bot))
            (format stream "  model: ~A~%" (chatbot-model bot))
            (format stream "  history-turns: ~D~%" history-turns)
            (terpri stream)))
        (write-line "No active personas." stream))
    (list-personas)))

(defun reset-persona (designator)
  "Clears DESIGNATOR's sandbox history and provider conversation state."
  (let* ((persona (require-active-persona designator))
         (conversation (persona-conversation persona)))
    (setf (persona-history persona) nil)
    (setf (conversation-messages conversation) nil)
    (setf (conversation-interaction-id conversation) nil)
    persona))

(defun reset-all-personas ()
  "Clears history and provider state for every active sandbox persona."
  (let ((count 0))
    (dolist (persona (active-persona-objects))
      (reset-persona persona)
      (incf count))
    count))

(defun clear-personas ()
  "Purges the active sandbox persona registry and returns the number removed."
  (sb-thread:with-mutex (*active-personas-lock*)
    (let ((count (hash-table-count *active-personas*)))
      (clrhash *active-personas*)
      count)))

(defun defpersona-macro-name-string (name)
  "Returns the runtime persona name string implied by DEFPERSONA NAME."
  (typecase name
    (symbol (string-capitalize (string-downcase (symbol-name name))))
    (string name)
    (t (error "DEFPERSONA name must be a symbol or string literal: ~S" name))))

(defmacro defpersona (name options &body directives)
  "Defines or replaces an active sandbox persona with concise REPL syntax.

Example:
  (defpersona sparky (:backend :google :role \"engineer\")
    \"Prefer direct implementations.\"
    \"Point out quick experiments.\")"
  (unless (listp options)
    (error "DEFPERSONA options must be a property list literal: ~S" options))
  (unless (evenp (length options))
    (error "DEFPERSONA options must contain an even number of forms: ~S" options))
  (when (member :directives options)
    (error "DEFPERSONA uses body forms for directives; omit :DIRECTIVES from the options plist."))
  (let ((name-string (defpersona-macro-name-string name)))
    `(progn
       (remove-persona ,name-string)
       (spawn-persona ,name-string
                      ,@options
                      ,@(when directives
                          `(:directives (list ,@directives)))))))

(defun list-stock-personas ()
  "Returns the available stock sandbox persona keys."
  (mapcar #'car +stock-persona-definitions+))

(defun spawn-stock-persona (designator &key name model temperature top-p (backend :google) runtime-context)
  "Spawns one built-in stock sandbox persona by DESIGNATOR."
  (let* ((definition (stock-persona-definition designator))
         (properties (cdr definition))
         (display-name (or name (getf properties :display-name))))
    (spawn-persona display-name
                   :backend backend
                   :model model
                   :role (getf properties :role)
                   :tone (getf properties :tone)
                   :directives (getf properties :directives)
                   :constraints (getf properties :constraints)
                   :temperature temperature
                   :top-p top-p
                   :runtime-context runtime-context)))

(defun normalize-query-personas (personas)
  "Returns PERSONAS normalized to a non-empty list of PERSONA instances."
  (let ((resolved
          (cond
            ((null personas)
             (active-persona-objects))
            ((typep personas 'persona)
             (list personas))
            ((listp personas)
             (mapcar #'require-active-persona personas))
            (t
             (list (require-active-persona personas))))))
    (unless resolved
      (error "No active sandbox personas are available."))
    resolved))

(defun extend-persona-history (history input response)
  "Returns HISTORY extended with one user INPUT and assistant RESPONSE pair."
  (append history
          (list (list (cons "role" "user")
                      (cons "content" input))
                (list (cons "role" "assistant")
                      (cons "content" response)))))

(defun query-one-persona (persona prompt &key callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Runs PROMPT through PERSONA, preserving isolated sandbox history."
  (let* ((conversation (persona-conversation persona))
         (bot (conversation-chatbot conversation))
         (history-copy (copy-tree (persona-history persona))))
    (setf (conversation-messages conversation) history-copy)
    (when (eq (chatbot-backend bot) :gemini)
      (setf (conversation-interaction-id conversation) nil))
    (multiple-value-bind (effective-input ignored-effective-model)
        (resolve-prompt-model-override bot prompt)
      (declare (ignore ignored-effective-model))
      (let* ((response
               (call-with-runtime-context
                (chatbot-runtime-context bot)
                (lambda ()
                  (apply #'chat
                         prompt
                         :conversation conversation
                         :callback callback
                         (append (when file
                                   (list :file file))
                                 (when files
                                   (list :files files))
                                 (when temperaturep
                                   (list :temperature temperature))
                                 (when top-pp
                                   (list :top-p top-p)))))))
             (updated-history
               (case (chatbot-backend bot)
                 (:gemini
                  (extend-persona-history (persona-history persona)
                                          effective-input
                                          response))
                 (t
                  (copy-tree (conversation-messages conversation))))))
        (setf (persona-history persona) updated-history)
        (setf (conversation-messages conversation) updated-history)
        response))))

(defun query-all (prompt &key personas callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Sends PROMPT to each selected sandbox persona and returns ordered results."
  (let ((results nil))
    (dolist (persona (normalize-query-personas personas))
      (print-chat-speaker-header (persona-name persona))
      (let ((response
              (apply #'query-one-persona
                     persona
                     prompt
                     :callback callback
                     (append (when file
                               (list :file file))
                             (when files
                               (list :files files))
                             (when temperaturep
                               (list :temperature temperature))
                             (when top-pp
                               (list :top-p top-p))))))
        (terpri)
        (terpri)
        (push (list :name (persona-name persona)
                    :response response)
              results)))
    (nreverse results)))

(defun format-arena-relay-prompt (speaker response listener)
  "Returns the labeled relay prompt passed from SPEAKER to LISTENER."
  (format nil "~A said: ~A~%~%What is your response, ~A?"
          speaker
          response
          listener))

(defun print-arena-user-turn-marker ()
  "Prints the prompt marker indicating the arena is waiting on the user."
  (write-line "[Your turn]")
  nil)

(defun run-arena (prompt &key personas (rounds 1) callback file files (temperature nil temperaturep) (top-p nil top-pp))
  "Runs a multi-round sandbox arena where each persona responds to the previous turn."
  (unless (and (integerp rounds)
               (> rounds 0))
    (error "Arena :rounds must be a positive integer."))
  (let* ((ordered-personas (normalize-query-personas personas))
         (persona-count (length ordered-personas))
         (total-turns (* rounds persona-count))
         (results nil)
         (current-prompt prompt))
    (unless (>= persona-count 2)
      (error "RUN-ARENA requires at least two personas."))
    (print-chat-speaker-block "User" prompt)
    (loop for turn-index from 0 below total-turns
          for persona = (nth (mod turn-index persona-count) ordered-personas)
          do (let ((response
                     (query-one-persona persona
                                        current-prompt
                                        :callback callback
                                        :file file
                                        :files files
                                        :temperature (when temperaturep temperature)
                                        :top-p (when top-pp top-p))))
               (print-chat-speaker-block (persona-name persona) response)
               (push (list :round (1+ (floor turn-index persona-count))
                           :turn (1+ turn-index)
                           :name (persona-name persona)
                           :prompt current-prompt
                           :response response)
                     results)
               (setf current-prompt
                     (format-arena-relay-prompt (persona-name persona)
                                                response
                                                (persona-name (nth (mod (1+ turn-index) persona-count)
                                                                   ordered-personas))))))
    (print-arena-user-turn-marker)
    (nreverse results)))
