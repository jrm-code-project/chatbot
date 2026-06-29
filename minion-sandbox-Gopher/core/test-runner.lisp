;;; -*- Lisp -*-
;;; test-runner.lisp - runtime entry point for the FiveAM suite

(in-package "CHATBOT")

(defun run-all-tests ()
  "Loads the chatbot/tests system and runs the full FiveAM suite."
  (asdf:load-system "chatbot/tests")
  (let ((runner (find-symbol "RUN-ALL-TESTS" "CHATBOT")))
    (unless runner
      (error "CHATBOT::RUN-ALL-TESTS is unavailable after loading chatbot/tests."))
    (funcall runner)))
