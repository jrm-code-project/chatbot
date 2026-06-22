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
