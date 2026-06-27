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

(defun assert-json-field= (object key expected)
  "Asserts that OBJECT contains KEY with EXPECTED value."
  (fiveam:is (equal expected (mcp-val key object))))

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
