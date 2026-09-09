;;;

(in-package "CHATBOT")

;;; Interactive tool-execution approval gating: prompts the user with an
;;; affirmative/negative/hint-string tri-state response ("Abort-with-Hint")
;;; before running shell commands, evaluating expressions, or granting
;;; filesystem access outside the current allowlist.

(defun classify-approval-response (raw)
  "Classifies RAW approval input as T (yes), NIL (no), or the trimmed hint string when it is neither."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) (or raw ""))))
    (cond
      ((member trimmed '("" "y" "Y" "yes" "Yes" "YES") :test #'string=) t)
      ((member trimmed '("n" "N" "no" "No" "NO") :test #'string=) nil)
      (t trimmed))))

(defun read-approval-response (control &rest args)
  "Prompts with CONTROL/ARGS followed by \" [Y/N]\", reads a line of input, and
returns T for an affirmative response, NIL for a negative response, or the raw
trimmed response string (an 'abort-with-hint') when the response is neither."
  (format *query-io* "~&~? [Y/N] " control args)
  (force-output *query-io*)
  (classify-approval-response (read-line *query-io* nil "")))

(defun default-filesystem-access-approval-function (bot directory tool-name)
  "Prompts the user to approve BOT access to DIRECTORY for TOOL-NAME.
Returns T when approved, NIL when denied, or a hint string when the user
typed something other than a plain yes/no response."
  (declare (ignore bot))
  (read-approval-response "Allow ~A to access directory ~A and remember it for this persona?"
                          tool-name
                          (namestring directory)))

(defparameter *filesystem-access-approval-function* #'default-filesystem-access-approval-function
  "Function used to approve persona filesystem access outside the current allowlist.")

(defparameter *bypass-eval-approval-p* nil
  "When T, bypasses interactive evaluation approval and automatically returns T.")

(defun default-eval-approval-function (bot source tool-name)
  "Prompts the user to approve evaluating SOURCE for TOOL-NAME.
Returns T when approved, NIL when denied, or a hint string when the user
typed something other than a plain yes/no response."
  (declare (ignore bot tool-name))
  (or *bypass-eval-approval-p*
      (read-approval-response "Evaluate ~A?" source)))

(defparameter *eval-approval-function* #'default-eval-approval-function
  "Function used to approve evaluation of a specific expression for the eval tool.")

(defparameter *bypass-shell-approval-p* nil
  "When T, bypasses interactive shell approval and automatically returns T.")

(defun default-shell-approval-function (bot command tool-name)
  "Prompts the user to approve executing COMMAND for TOOL-NAME.
Returns T when approved, NIL when denied, or a hint string when the user
typed something other than a plain yes/no response."
  (declare (ignore bot tool-name))
  (or *bypass-shell-approval-p*
      (read-approval-response "Run this shell command ~S?" command)))

(defparameter *shell-approval-function* #'default-shell-approval-function
  "Function used to approve running a specific shell command for the shell tool.")

(defun tool-approval-denied-reason (approval generic-reason)
  "Returns the tool-execution-error reason for a denied APPROVAL.
When APPROVAL is a non-empty hint string, appends it to the aborted-execution
message; otherwise returns GENERIC-REASON."
  (if (and (stringp approval) (plusp (length approval)))
      (format nil "Execution aborted by user. User hint: ~A" approval)
      generic-reason))
