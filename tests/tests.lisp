;;; tests.lisp

(in-package "CHATBOT")

(fiveam:def-suite chatbot-suite :description "Chatbot framework test suite")
(fiveam:in-suite chatbot-suite)

(defparameter *test-live-http-post-function* *http-post-function*)
(defparameter *test-live-http-get-function* *http-get-function*)
(defparameter *test-live-http-patch-function* *http-patch-function*)
(defparameter *test-live-http-delete-function* *http-delete-function*)
(defparameter *test-default-getenv-function* *getenv-function*)
(defparameter *test-default-gemini-api-key-function* *gemini-api-key-function*)

(defun select-test-function-seam (legacy-value legacy-default context-value prefer-context-p)
  "Selects the context seam for explicit contexts, otherwise a dynamic test seam."
  (cond
    ((and prefer-context-p
          (not (eq context-value legacy-default)))
     context-value)
    ((not (eq legacy-value legacy-default))
     legacy-value)
    (t context-value)))

(defun test-unmocked-http-function (operation)
  "Returns a function that rejects unmocked test HTTP traffic."
  (lambda (&rest arguments)
    (declare (ignore arguments))
    (error "Test attempted an unmocked HTTP ~A request." operation)))

(defun test-http-function-seam (operation legacy-value live-value context-value prefer-context-p)
  "Returns a mocked HTTP seam or a function that rejects live test traffic."
  (let ((selected (select-test-function-seam legacy-value live-value context-value
                                              prefer-context-p)))
    (if (eq selected live-value)
        (test-unmocked-http-function operation)
        selected)))

(defun make-test-backend-runtime-context (conversation)
  "Returns a child runtime context carrying active test mocks for one backend call."
  (let* ((conversation-context
           (and conversation
                (chatbot-runtime-context (conversation-chatbot conversation))))
         (prefer-context-p
           (and conversation-context
                (not (eq conversation-context *default-runtime-context*)))))
    (make-runtime-context
     :startup-chatbot (current-startup-chatbot)
     :getenv-function
     (select-test-function-seam *getenv-function*
                                *test-default-getenv-function*
                                (current-getenv-function)
                                prefer-context-p)
     :http-post-function
     (test-http-function-seam "POST" *http-post-function*
                              *test-live-http-post-function*
                              (current-http-post-function)
                              prefer-context-p)
     :http-get-function
     (test-http-function-seam "GET" *http-get-function*
                              *test-live-http-get-function*
                              (current-http-get-function)
                              prefer-context-p)
     :http-patch-function
     (test-http-function-seam "PATCH" *http-patch-function*
                              *test-live-http-patch-function*
                              (current-http-patch-function)
                              prefer-context-p)
     :http-delete-function
     (test-http-function-seam "DELETE" *http-delete-function*
                              *test-live-http-delete-function*
                              (current-http-delete-function)
                              prefer-context-p)
     :gemini-api-key-function
     (select-test-function-seam *gemini-api-key-function*
                                *test-default-gemini-api-key-function*
                                (current-gemini-api-key-function)
                                prefer-context-p)
     :default-conversation (current-default-conversation)
     :active-conversation conversation
     :active-planner (current-active-planner)
     :active-planner-parent-conversation (current-active-planner-parent-conversation))))

(defun make-test-chat-backends ()
  "Returns a backend registry whose handlers install request-local test seams."
  (let ((registry (make-hash-table :test 'equal)))
    (maphash
     (lambda (keyword handler)
       (let ((delegate handler))
         (setf (gethash keyword registry)
               (lambda (input &rest arguments)
                 (let ((context
                         (make-test-backend-runtime-context
                          (getf arguments :conversation))))
                   (call-with-runtime-context
                    context
                    (lambda ()
                      (apply delegate input arguments))))))))
     *chat-backends*)
    registry))

(defun test-chat-google (bot input conversation callback &rest arguments)
  "Calls CHAT-GOOGLE with dynamically bound test seams in a request context."
  (call-with-runtime-context
   (make-test-backend-runtime-context conversation)
   (lambda ()
     (apply #'chat-google bot input conversation callback arguments))))

(defun test-results-passed-p (results)
  "Returns true when RESULTS contain only passing checks.
Skipped checks still count as a non-successful suite run so this preserves the
existing RUN-ALL-TESTS contract while using FiveAM's public result API."
  (multiple-value-bind (successfulp failed-tests skipped-tests)
      (fiveam:results-status results)
    (declare (ignore failed-tests))
    (and successfulp
         (null skipped-tests))))

(setf (fdefinition 'run-all-tests)
      (lambda ()
        "Utility to run the chatbot-suite tests and return results."
        (let ((*bypass-eval-approval-p* t)
              (*chat-backends* (make-test-chat-backends)))
          (let ((results (fiveam:run 'chatbot-suite)))
            (fiveam:explain! results)
            (test-results-passed-p results)))))

(defun test-json-elements (value)
  "Returns VALUE as a proper list for JSON-style vectors or lists."
  (if (vectorp value)
      (coerce value 'list)
      value))

(defun decode-test-json (value)
  "Returns VALUE decoded from JSON when it is a string, otherwise VALUE."
  (if (stringp value)
      (cl-json:decode-json-from-string value)
      value))

(defun decode-test-json-lines (text)
  "Returns decoded JSON objects from newline-delimited TEXT."
  (loop for line in (cl-ppcre:split "\\r?\\n" text)
        for trimmed = (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)
        unless (string= trimmed "")
          collect (decode-test-json trimmed)))

(defun normalized-test-json-key-name (key)
  "Returns KEY normalized for punctuation-insensitive JSON assertions."
  (coerce
   (remove-if-not #'alphanumericp
                  (string-downcase
                   (typecase key
                     (string key)
                     (symbol (symbol-name key))
                     (t (princ-to-string key)))))
   'string))

(defun normalize-test-json-value (value)
  "Normalizes decoded JSON literals and numbers for semantic assertions."
  (let ((printed (string-downcase (princ-to-string value))))
    (cond
      ((search "json-literal true" printed) t)
      ((or (search "json-literal false" printed)
           (search "json-literal null" printed))
       nil)
      (t value))))

(defun assert-json-value= (actual expected)
  "Asserts semantic equality between decoded JSON ACTUAL and EXPECTED."
  (let ((actual (normalize-test-json-value actual))
        (expected (normalize-test-json-value expected)))
    (if (and (numberp actual)
             (numberp expected))
        (fiveam:is (<= (abs (- (coerce actual 'double-float)
                               (coerce expected 'double-float)))
                       1d-6))
        (fiveam:is (equal actual expected)))))

(defun test-json-value-any (object keys)
  "Returns the first value found in OBJECT for any of KEYS."
  (let* ((entries (test-json-elements object))
         (normalized-keys (mapcar #'normalized-test-json-key-name keys)))
    (loop for entry in entries
          for entry-key = (normalized-test-json-key-name (car entry))
          when (member entry-key normalized-keys :test #'string=)
            return (cdr entry))))

(defun assert-json-field= (object key expected)
  "Asserts that OBJECT contains KEY with EXPECTED value."
  (assert-json-value= (test-json-value-any object (list key)) expected))

(defun assert-role/content (message role content)
  "Asserts that MESSAGE has ROLE and CONTENT."
  (assert-json-field= message :role role)
  (assert-json-field= message :content content))

(defun assert-role/content-sequence (messages expected)
  "Asserts MESSAGES match EXPECTED (ROLE CONTENT) pairs in order."
  (let ((message-list (test-json-elements messages)))
    (fiveam:is (= (length expected) (length message-list)))
    (loop for message in message-list
          for (role content) in expected
          do (assert-role/content message role content))))

(defun message-part-texts (message)
  "Returns the text values from MESSAGE parts."
  (mapcar (lambda (part)
            (mcp-val :text part))
          (test-json-elements (mcp-val :parts message))))

(defun message-content-parts (message)
  "Returns MESSAGE content as normalized content parts when it is part-based."
  (let ((content (test-json-value-any message '(:content "content"))))
    (and (or (vectorp content)
             (listp content))
         (test-json-elements content))))

(defun message-content-texts (message)
  "Returns text values from MESSAGE content parts."
  (mapcar (lambda (part)
            (test-json-value-any part '(:text "text")))
          (message-content-parts message)))

(defun message-all-texts (message)
  "Returns all human-readable text strings carried by MESSAGE."
  (let ((content (test-json-value-any message '(:content "content"))))
    (cond
      ((stringp content) (list content))
      (t (remove nil (message-content-texts message))))))

(defun messages-all-texts (messages)
  "Returns all human-readable text strings across MESSAGES."
  (mapcan #'message-all-texts (test-json-elements messages)))

(defun assert-message-content-texts (message role expected-texts)
  "Asserts MESSAGE has ROLE and EXPECTED-TEXTS content parts."
  (assert-json-field= message :role role)
  (fiveam:is (equal expected-texts
                    (message-content-texts message))))

(defun assert-history-message (message role content)
  "Asserts stored conversation MESSAGE has ROLE and CONTENT."
  (assert-json-field= message :role role)
  (assert-json-field= message :content content))

(defun assert-history-sequence (messages expected)
  "Asserts stored conversation MESSAGES match EXPECTED (ROLE CONTENT) pairs."
  (let ((message-list (test-json-elements messages)))
    (fiveam:is (= (length expected) (length message-list)))
    (loop for message in message-list
          for (role content) in expected
          do (assert-history-message message role content))))

(defun history-contents (messages)
  "Returns the stored content strings from conversation MESSAGES."
  (mapcar (lambda (message)
            (test-json-value-any message '(:content "content")))
          (test-json-elements messages)))

(defun google-payload-contents (payload)
  "Returns Google-format request contents from PAYLOAD."
  (test-json-elements (test-json-value-any payload '(:contents "contents"))))

(defun google-payload-texts (payload)
  "Returns all text parts from Google-format PAYLOAD contents."
  (mapcan #'message-part-texts
          (google-payload-contents payload)))

(defun google-message-parts (message)
  "Returns the normalized part list from a Google-format MESSAGE."
  (test-json-elements (test-json-value-any message '(:parts "parts"))))

(defun google-function-call-part (message)
  "Returns the functionCall part from a Google-format MESSAGE."
  (find-if (lambda (part)
             (test-json-value-any part '("functionCall" :function-call :function--call)))
           (google-message-parts message)))

(defun google-function-response-part (message)
  "Returns the functionResponse part from a Google-format MESSAGE."
  (find-if (lambda (part)
             (test-json-value-any part '("functionResponse" :function-response)))
           (google-message-parts message)))

(defun assert-google-function-call-part (part name &key thought-signature)
  "Asserts semantic fields on Google functionCall PART."
  (let ((function-call (test-json-value-any part '("functionCall" :function-call :function--call))))
    (assert-json-field= function-call "name" name)
    (when thought-signature
      (assert-json-field= part "thoughtSignature" thought-signature))
    function-call))

(defun assert-google-function-response-part (part name)
  "Asserts semantic fields on Google functionResponse PART."
  (let ((function-response (test-json-value-any part '("functionResponse" :function-response))))
    (assert-json-field= function-response "name" name)
    function-response))

(defun interaction-payload-input (payload)
  "Returns Interactions-format input steps from PAYLOAD."
  (test-json-elements (test-json-value-any payload '(:input "input"))))

(defun interaction-tool-names (tools)
  "Returns the tool names from Interactions-format TOOLS."
  (mapcar (lambda (tool)
            (test-json-value-any tool '("name" :name)))
          (test-json-elements tools)))

(defun openai-tool-names (tools)
  "Returns the function names from OpenAI-format TOOLS."
  (mapcar (lambda (tool)
            (test-json-value-any
             (test-json-value-any tool '("function" :function))
             '("name" :name)))
          (test-json-elements tools)))

(defun google-tool-declarations (tools)
  "Returns the flattened function declaration list from Google-format TOOLS."
  (mapcan (lambda (tool-group)
            (test-json-elements
             (test-json-value-any tool-group
                                  '("functionDeclarations" :function-declarations))))
          (test-json-elements tools)))

(defun google-payload-tools (payload)
  "Returns Google-format tool groups from PAYLOAD."
  (test-json-elements (test-json-value-any payload '(:tools "tools"))))

(defun google-tool-parameters (tool)
  "Returns the parameters object from a Google tool declaration."
  (test-json-value-any tool '("parameters" :parameters)))

(defun google-tool-names (tools)
  "Returns the function declaration names from Google-format TOOLS."
  (mapcar (lambda (tool)
            (test-json-value-any tool '("name" :name)))
          (google-tool-declarations tools)))

(defun interaction-step-content-texts (step)
  "Returns text values from an Interactions API STEP content list."
  (mapcar (lambda (part)
            (test-json-value-any part '(:text "text")))
          (test-json-elements (test-json-value-any step '(:content "content")))))

(defun assert-google-message-texts (message role expected-texts)
  "Asserts Google-format MESSAGE has ROLE and EXPECTED-TEXTS parts."
  (assert-json-field= message :role role)
  (fiveam:is (equal expected-texts
                    (message-part-texts message))))

(defun assert-google-system-instruction-texts (payload expected-texts)
  "Asserts PAYLOAD system instruction parts match EXPECTED-TEXTS."
  (fiveam:is (equal expected-texts
                    (message-part-texts (mcp-val :system-instruction payload)))))

(defun json-object-field (object key)
  "Returns KEY from decoded JSON OBJECT."
  (test-json-value-any object (list key)))

(defun assert-sampling-parameters (object &key (temperature nil temperature-supplied-p)
                                              (top-p nil top-p-supplied-p)
                                              (saved nil saved-supplied-p))
  "Asserts semantic sampling-parameter fields on OBJECT or a JSON string."
  (let ((payload (decode-test-json object)))
    (when temperature-supplied-p
      (assert-json-field= payload "temperature" temperature))
    (when top-p-supplied-p
      (assert-json-value= (test-json-value-any payload '("topP" "top_p" :top-p))
                          top-p))
    (when saved-supplied-p
      (assert-json-field= payload "saved" saved))))
