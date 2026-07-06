;;; -*- Lisp -*-
;;; eval-grounding-tools.lisp - built-in chatbot eval and grounding helpers

(in-package "CHATBOT")

(defun whitespace-only-stream-remaining-p (stream)
  "Returns true when the remainder of STREAM contains only whitespace."
  (let ((eof-marker (gensym "EOF")))
    (loop for char = (read-char stream nil eof-marker)
          until (eq char eof-marker)
          always (find char '(#\Space #\Tab #\Newline #\Return #\Page)))))

(defun read-eval-tool-form (expression tool-name)
  "Reads exactly one Lisp form from EXPRESSION with reader eval disabled."
  (handler-case
      (let ((*read-eval* nil)
            (*package* (find-package "CHATBOT")))
        (with-input-from-string (stream expression)
          (let ((eof-marker (gensym "EOF")))
            (let ((form (read stream nil eof-marker)))
              (when (eq form eof-marker)
                (error 'mcp-tool-execution-error
                       :tool-name tool-name
                       :reason "expression must contain one s-expression."))
              (unless (whitespace-only-stream-remaining-p stream)
                (error 'mcp-tool-execution-error
                       :tool-name tool-name
                       :reason "expression must contain exactly one s-expression."))
              form))))
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Failed to parse expression: ~A" e)))))

(defun approve-chatbot-eval-expression (bot expression tool-name)
  "Requests approval to evaluate EXPRESSION for BOT."
  (let ((approval-function (current-eval-approval-function
                            (chatbot-runtime-context bot))))
    (unless approval-function
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "No eval approval function is configured."))
    (unless (funcall approval-function bot expression tool-name)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "Evaluation denied by user."))
    t))

(defun eval-tool-result-json (values stdout stderr)
  "Returns a stable JSON result string for VALUES, STDOUT, and STDERR."
  (cl-json:encode-json-to-string
   `(("values" . ,(coerce (mapcar #'prin1-to-string values) 'vector))
     ("stdout" . ,stdout)
     ("stderr" . ,stderr))))

(defun hash-table-value-any (table keys)
  "Returns the first value present in TABLE for any of KEYS."
  (when (hash-table-p table)
    (dolist (key keys nil)
      (multiple-value-bind (value foundp) (gethash key table)
        (when foundp
          (return value))))))

(defun normalize-grounding-search-response (response tool-name)
  "Returns RESPONSE as a hash table or signals an execution error."
  (unless (hash-table-p response)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Search returned an unexpected response shape."))
  response)

(defun grounding-search-items (response)
  "Returns the result items vector/list from RESPONSE."
  (let ((items (hash-table-value-any response '(:items "items"))))
    (cond
      ((vectorp items) (coerce items 'list))
      ((listp items) items)
      (t nil))))

(defun format-grounding-search-results (label query response tool-name)
  "Formats grounding RESPONSE into stable text for LABEL and QUERY."
  (let* ((normalized (normalize-grounding-search-response response tool-name))
         (items (grounding-search-items normalized))
         (search-info (hash-table-value-any normalized '(:search-information "searchInformation" :search--information)))
         (total-results (or (hash-table-value-any search-info '(:total-results "totalResults" :total--results))
                            (and items (princ-to-string (length items)))
                            "0")))
    (with-output-to-string (stream)
      (format stream "~A query: ~A~%Total results: ~A" label query total-results)
      (if items
          (loop for item in items
                for index from 1
                for title = (or (hash-table-value-any item '(:title "title"))
                                "(untitled)")
                for link = (or (hash-table-value-any item '(:link "link"))
                               "(no link)")
                for snippet = (or (hash-table-value-any item '(:snippet "snippet"))
                                  "")
                do (format stream "~%~%~D. ~A~%URL: ~A" index title link)
                   (when (string/= snippet "")
                     (format stream "~%Snippet: ~A" snippet)))
          (format stream "~%~%No results found.")))))

(defun run-grounding-search (tool-name label function query)
  "Runs grounding search FUNCTION for QUERY and formats a stable tool result."
  (handler-case
      (format-grounding-search-results label
                                       query
                                       (funcall function query)
                                       tool-name)
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Search failed for query ~S: ~A" query e)))))

(defparameter *eval-tool-timeout-seconds* 60
  "Maximum number of seconds an approved eval tool expression may run.")

(defun execute-approved-eval-expression (expression form tool-name)
  "Evaluates FORM and returns a structured JSON string with values and captured output."
  (let ((stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream)))
    (let ((*standard-output* stdout-stream)
          (*error-output* stderr-stream)
          (*package* (find-package "CHATBOT")))
      (handler-case
          (let ((values (trivial-timeout:with-timeout (*eval-tool-timeout-seconds*)
                          (multiple-value-list (eval form)))))
            (eval-tool-result-json values
                                   (get-output-stream-string stdout-stream)
                                   (get-output-stream-string stderr-stream)))
        (trivial-timeout:timeout-error ()
          (let ((stdout (get-output-stream-string stdout-stream))
                (stderr (get-output-stream-string stderr-stream)))
            (error 'mcp-tool-execution-error
                   :tool-name tool-name
                   :reason (format nil "Evaluation timed out after ~D seconds.~@[~%stdout:~%~A~]~@[~%stderr:~%~A~]"
                                   *eval-tool-timeout-seconds*
                                   (and (string/= stdout "") stdout)
                                   (and (string/= stderr "") stderr)))))
        (error (e)
          (let ((stdout (get-output-stream-string stdout-stream))
                (stderr (get-output-stream-string stderr-stream)))
            (error 'mcp-tool-execution-error
                   :tool-name tool-name
                   :reason (format nil "Evaluation failed for expression ~S: ~A~@[~%stdout:~%~A~]~@[~%stderr:~%~A~]"
                                   expression
                                   e
                                   (and (string/= stdout "") stdout)
                                   (and (string/= stderr "") stderr)))))))))

(defun execute-web-search-tool (bot arguments tool-name)
  "Runs the built-in web grounding search tool."
  (unless (chatbot-web-tools-p bot)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Web grounding tools are not enabled."))
  (run-grounding-search tool-name
                        "Web search"
                        *web-search-function*
                        (normalize-builtin-tool-string-argument
                         (or (mcp-val "query" arguments)
                             (mcp-val :query arguments))
                         "query"
                         tool-name)))

(defun execute-hyperspec-search-tool (bot arguments tool-name)
  "Runs the built-in HyperSpec grounding search tool."
  (unless (chatbot-web-tools-p bot)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Web grounding tools are not enabled."))
  (run-grounding-search tool-name
                        "HyperSpec search"
                        *hyperspec-search-function*
                        (normalize-builtin-tool-string-argument
                         (or (mcp-val "query" arguments)
                             (mcp-val :query arguments))
                         "query"
                         tool-name)))

(defun execute-eval-tool (bot arguments tool-name)
  "Runs the built-in eval tool after approval."
  (unless (chatbot-enable-eval-p bot)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "Eval tool is not enabled."))
  (let* ((expression (normalize-builtin-tool-string-argument
                      (or (mcp-val "expression" arguments)
                          (mcp-val :expression arguments))
                      "expression"
                      tool-name))
         (form (read-eval-tool-form expression tool-name)))
    (approve-chatbot-eval-expression bot expression tool-name)
    (execute-approved-eval-expression expression form tool-name)))
