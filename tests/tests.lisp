;;; tests.lisp

(in-package "CHATBOT")

(fiveam:def-suite chatbot-suite :description "Chatbot framework test suite")
(fiveam:in-suite chatbot-suite)

(defun test-results-passed-p (results)
  "Returns true when RESULTS contain only passing checks.
Skipped checks still count as a non-successful suite run so this preserves the
existing RUN-ALL-TESTS contract while using FiveAM's public result API."
  (multiple-value-bind (successfulp failed-tests skipped-tests)
      (fiveam:results-status results)
    (declare (ignore failed-tests))
    (and successfulp
         (null skipped-tests))))

(defun run-all-tests ()
  "Utility to run the chatbot-suite tests and return results."
  (let ((results (fiveam:run 'chatbot-suite)))
    (fiveam:explain! results)
    (test-results-passed-p results)))

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

(defun assert-google-message-texts (message role expected-texts)
  "Asserts Google-format MESSAGE has ROLE and EXPECTED-TEXTS parts."
  (assert-json-field= message :role role)
  (fiveam:is (equal expected-texts
                    (message-part-texts message))))

(defun assert-google-system-instruction-texts (payload expected-texts)
  "Asserts PAYLOAD system instruction parts match EXPECTED-TEXTS."
  (fiveam:is (equal expected-texts
                    (message-part-texts (mcp-val :system-instruction payload)))))

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
