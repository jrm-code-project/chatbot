;;; -*- Lisp -*-
;;; tool-execution.lisp - MCP and built-in chatbot tool execution

(in-package "CHATBOT")

(defun execute-chatbot-tool-by-name (bot tool-name arguments)
  "Finds TOOL-NAME for BOT and executes it with ARGUMENTS."
  (multiple-value-bind (source tool) (find-chatbot-tool bot tool-name)
    (unless source
      (error "Tool not found: ~A" tool-name))
    (execute-chatbot-tool bot
                          source
                          tool-name
                          (normalize-chatbot-tool-arguments source tool arguments))))

(defun execute-chatbot-tool-by-name-json-arguments (bot tool-name arguments-json context)
  "Parses ARGUMENTS-JSON for TOOL-NAME in CONTEXT and executes the tool for BOT."
  (execute-chatbot-tool-by-name
   bot
   tool-name
   (if (or (null arguments-json)
           (string= (string-trim '(#\Space #\Tab #\Return #\Linefeed) arguments-json) ""))
       (empty-json-object)
       (parse-json-or-error arguments-json :context context))))

(defun chatbot-tool-error-message (condition)
  "Returns the most useful human-readable message for CONDITION."
  (if (typep condition 'mcp-tool-execution-error)
      (mcp-tool-execution-error-reason condition)
      (princ-to-string condition)))

(defun chatbot-tool-error-payload (tool-name condition)
  "Returns a JSON-serializable payload describing a tool execution failure."
  `(("type" . "tool_error")
    ("toolName" . ,tool-name)
    ("message" . ,(chatbot-tool-error-message condition))))

(defun chatbot-tool-error-text (tool-name condition)
  "Returns a JSON string describing a tool execution failure for LLM-visible text fields."
  (cl-json:encode-json-to-string (chatbot-tool-error-payload tool-name condition)))

(defun map-chatbot-json-tool-call-results (bot tool-calls context-builder result-builder
                                               &key error-builder)
  "Executes JSON-argument TOOL-CALLS for BOT and returns builder outputs in order.

When ERROR-BUILDER is provided, tool execution errors are converted into result
entries instead of aborting the full turn."
  (let ((results nil))
    (dolist (tool-call tool-calls (nreverse results))
      (let* ((id (cdr (assoc :id tool-call)))
             (name (cdr (assoc :name tool-call)))
             (arguments-json (coerce (cdr (assoc :arguments tool-call)) 'string)))
        (handler-case
            (let ((res-text (execute-chatbot-tool-by-name-json-arguments
                             bot
                             name
                             arguments-json
                             (funcall context-builder name tool-call))))
              (push (funcall result-builder id name arguments-json res-text tool-call) results))
          (error (condition)
            (if (typep condition 'agentic-loop-approval-required)
                (error condition)
                (if error-builder
                (push (funcall error-builder id name arguments-json condition tool-call) results)
                (error condition)))))))))

(defun normalize-builtin-tool-integer-argument (value argument-name tool-name)
  "Normalizes VALUE to an integer argument or signals an execution error."
  (let ((normalized
          (typecase value
            (integer value)
            (real (truncate value))
            (string (parse-integer value :junk-allowed t))
            (t nil))))
    (unless normalized
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be an integer." argument-name)))
    normalized))

(defun normalize-builtin-tool-string-argument (value argument-name tool-name)
  "Normalizes VALUE to a non-empty string argument or signals an execution error."
  (unless (and (stringp value) (string/= value ""))
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A must be a non-empty string." argument-name)))
  value)

(defun normalize-builtin-tool-real-argument (value argument-name tool-name &key allow-nil-p)
  "Normalizes VALUE to a real argument or signals an execution error."
  (when (null value)
    (if allow-nil-p
        (return-from normalize-builtin-tool-real-argument nil)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "~A is required." argument-name))))
  (let ((normalized
          (typecase value
            (real (float value 1.0d0))
            (string
             (let* ((*read-eval* nil)
                    (trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) value)))
               (handler-case
                   (multiple-value-bind (parsed position)
                       (read-from-string trimmed nil nil)
                     (if (and parsed
                              (realp parsed)
                              (= position (length trimmed)))
                         (float parsed 1.0d0)
                         nil))
                 (error () nil))))
            (t nil))))
    (unless normalized
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be a real number." argument-name)))
    normalized))

(defun builtin-tool-argument (arguments &rest keys)
  "Looks up the first present argument among KEYS in ARGUMENTS."
  (dolist (key keys (values nil nil))
    (let ((entry (mcp-assoc key arguments)))
      (when entry
        (return (values t (cdr entry)))))))

(defun normalize-builtin-tool-boolean-argument (foundp value argument-name tool-name)
  "Normalizes VALUE to a boolean argument or signals an execution error."
  (unless foundp
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A is required." argument-name)))
  (let ((printed (string-downcase (princ-to-string value))))
    (cond
      ((or (eq value t)
           (eq value :true)
           (string= printed "t")
           (string= printed "true")
           (search "json-literal true" printed))
       t)
      ((or (null value)
           (eq value :false)
           (string= printed "nil")
           (string= printed "false")
           (search "json-literal false" printed))
       nil)
      ((symbolp value)
       (cond
         ((string-equal (symbol-name value) "true") t)
         ((string-equal (symbol-name value) "false") nil)
         (t
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "~A must be a boolean." argument-name)))))
      ((stringp value)
       (cond
         ((string-equal value "true") t)
         ((string-equal value "false") nil)
         (t
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (format nil "~A must be a boolean." argument-name)))))
      (t
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason (format nil "~A must be a boolean." argument-name))))))

(defun normalize-builtin-tool-string-sequence-argument (foundp value argument-name tool-name)
  "Normalizes VALUE to a list of strings or signals an execution error."
  (unless foundp
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason (format nil "~A is required." argument-name)))
  (let ((elements (cond
                    ((vectorp value) (coerce value 'list))
                    ((listp value) value)
                    (t nil))))
    (unless elements
      (when (or (vectorp value) (listp value))
        (return-from normalize-builtin-tool-string-sequence-argument nil))
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must be an array of strings." argument-name)))
    (unless (every #'stringp elements)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "~A must contain only strings." argument-name)))
    elements))

(defun ensure-system-instruction-tool-path (bot tool-name)
  "Returns BOT's system-instruction path or signals an execution error."
  (or (chatbot-system-instruction-path bot)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "System-instruction tools require a persona-backed system instruction file.")))

(defun system-instruction-storage-kind-name (storage-kind)
  "Returns a lowercase string name for STORAGE-KIND."
  (string-downcase (string storage-kind)))

(defun system-instruction-tool-result (bot &key saved)
  "Returns the current system-instruction paragraph state as JSON text."
  (let ((payload `(("paragraphs" . ,(current-system-instruction-paragraphs bot))
                   ("count" . ,(system-instruction-paragraph-count bot))
                   ("storageKind" . ,(system-instruction-storage-kind-name
                                      (chatbot-system-instruction-storage-kind bot)))
                   ("path" . ,(namestring (chatbot-system-instruction-path bot)))
                   ,@(when saved '(("saved" . t))))))
    (cl-json:encode-json-to-string payload)))

(defun sampling-parameters-tool-result (bot &key saved)
  "Returns the current runtime sampling parameters as JSON text."
  (let ((parameters (sampling-parameters bot)))
    (cl-json:encode-json-to-string
     `(("temperature" . ,(or (getf parameters :temperature) :null))
       ("topP" . ,(or (getf parameters :top-p) :null))
       ,@(when saved '(("saved" . t)))))))

(defun save-system-instructions-or-tool-error (bot tool-name)
  "Saves BOT's system instructions, mapping failures to tool errors."
  (handler-case
      (save-system-instructions bot)
    (mcp-tool-execution-error (e)
      (error e))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (princ-to-string e)))))

(defun chatbot-filesystem-root-truename (bot tool-name)
  "Returns the validated filesystem root directory for BOT."
  (let ((root (chatbot-filesystem-root-directory bot)))
    (unless root
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "Filesystem tools require a configured root directory."))
    (uiop:ensure-directory-pathname (truename root))))

(defun filesystem-path-within-directory-p (path directory)
  "Returns true when PATH is equal to or nested under DIRECTORY."
  (let ((path-name (string-downcase (namestring (truename path))))
        (directory-name (string-downcase (namestring (uiop:ensure-directory-pathname (truename directory))))))
    (alexandria:starts-with-subseq directory-name path-name)))

(defun canonicalize-allowed-filesystem-directories (directories)
  "Returns DIRECTORIES deduplicated and collapsed by ancestor coverage."
  (let ((sorted (sort (remove-duplicates
                      (mapcar #'uiop:ensure-directory-pathname directories)
                      :test (lambda (left right)
                              (string-equal (namestring left) (namestring right))))
                     #'<
                     :key (lambda (directory)
                            (length (namestring directory))))))
    (let ((result nil))
      (dolist (directory sorted (nreverse result))
        (unless (some (lambda (approved)
                        (filesystem-path-within-directory-p directory approved))
                      result)
          (push directory result))))))

(defun chatbot-effective-filesystem-allowed-directories (bot tool-name)
  "Returns all effective allowed directories for BOT, including the persona root."
  (canonicalize-allowed-filesystem-directories
   (cons (chatbot-filesystem-root-truename bot tool-name)
        (or (chatbot-filesystem-allowed-directories bot) nil))))

(defun persist-chatbot-filesystem-allowlist (bot)
  "Persists BOT's explicit filesystem allowlist when it has a configured allowlist file."
  (let ((allowlist-path (chatbot-filesystem-allowlist-path bot)))
    (when allowlist-path
      (with-open-file (stream allowlist-path
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
        (prin1 (mapcar #'namestring
                      (canonicalize-allowed-filesystem-directories
                       (or (chatbot-filesystem-allowed-directories bot) nil)))
              stream)))))

(defun approve-chatbot-filesystem-directory (bot directory tool-name)
  "Requests approval for DIRECTORY and persists it for BOT when granted."
  (let ((approval-function (current-filesystem-access-approval-function
                            (chatbot-runtime-context bot))))
    (unless approval-function
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason "No filesystem access approval function is configured."))
    (unless (funcall approval-function bot directory tool-name)
      (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason (format nil "Access to directory denied: ~A" directory)))
    (setf (chatbot-filesystem-allowed-directories bot)
          (canonicalize-allowed-filesystem-directories
           (cons directory (or (chatbot-filesystem-allowed-directories bot) nil))))
    (persist-chatbot-filesystem-allowlist bot)
    directory))

(defun ensure-filesystem-tool-directory-authorized (bot directory tool-name)
  "Ensures DIRECTORY is covered by BOT's allowlist, prompting for approval when needed."
  (let ((normalized-directory (uiop:ensure-directory-pathname (truename directory))))
    (if (some (lambda (allowed-directory)
                (filesystem-path-within-directory-p normalized-directory allowed-directory))
              (chatbot-effective-filesystem-allowed-directories bot tool-name))
        normalized-directory
        (approve-chatbot-filesystem-directory bot normalized-directory tool-name))))

(defun filesystem-containing-directory (path)
  "Returns the containing directory pathname for PATH."
  (uiop:ensure-directory-pathname
   (make-pathname :name nil :type nil :defaults path)))

(defun resolve-filesystem-tool-existing-path (bot raw-path tool-name argument-name missing-reason &key directory-target-p)
  "Resolves RAW-PATH and authorizes it for BOT."
  (let ((root (chatbot-filesystem-root-truename bot tool-name)))
    (let* ((validated-path (normalize-builtin-tool-string-argument raw-path argument-name tool-name))
           (requested (pathname validated-path))
           (candidate (if (uiop:absolute-pathname-p requested)
                          requested
                          (merge-pathnames requested root)))
           (resolved (probe-file candidate)))
      (unless resolved
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "~A: ~A" missing-reason validated-path)))
      (ensure-filesystem-tool-directory-authorized
       bot
       (if directory-target-p
           (uiop:ensure-directory-pathname resolved)
           (filesystem-containing-directory resolved))
       tool-name)
      resolved)))

(defun resolve-filesystem-tool-path (bot filename tool-name)
  "Resolves FILENAME for BOT inside the allowed filesystem root."
  (resolve-filesystem-tool-existing-path bot
                                         filename
                                         tool-name
                                         "filename"
                                         "File not found"))

(defun resolve-filesystem-tool-target-path (bot pathname tool-name)
  "Resolves PATHNAME for BOT as a write target inside the allowed filesystem root."
  (let* ((root (chatbot-filesystem-root-truename bot tool-name))
         (validated-path (normalize-builtin-tool-string-argument pathname "pathname" tool-name))
         (requested (pathname validated-path))
         (candidate (if (uiop:absolute-pathname-p requested)
                        requested
                        (merge-pathnames requested root)))
         (parent-candidate (uiop:ensure-directory-pathname
                            (make-pathname :name nil :type nil :defaults candidate)))
         (parent-resolved (probe-file parent-candidate)))
    (when (and (null (pathname-name candidate))
               (null (pathname-type candidate)))
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Pathname must name a file: ~A" pathname)))
    (unless parent-resolved
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Parent directory not found: ~A" parent-candidate)))
    (let* ((parent-truename (uiop:ensure-directory-pathname parent-resolved))
           (safe-parent (ensure-filesystem-tool-directory-authorized bot parent-truename tool-name))
           (resolved-target (merge-pathnames (make-pathname :name (pathname-name candidate)
                                                           :type (pathname-type candidate)
                                                           :version (pathname-version candidate))
                                            safe-parent)))
      (values resolved-target root))))

(defun resolve-filesystem-tool-directory (bot pathname tool-name)
  "Resolves PATHNAME for BOT as an existing directory inside the allowed filesystem root."
  (let* ((root (chatbot-filesystem-root-truename bot tool-name))
         (resolved (resolve-filesystem-tool-existing-path bot
                                                          pathname
                                                          tool-name
                                                          "pathname"
                                                          "Directory not found"
                                                          :directory-target-p t)))
    (unless (uiop:directory-exists-p resolved)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Path is not a directory: ~A" pathname)))
    (values (uiop:ensure-directory-pathname resolved) root)))

(defun read-file-lines-subset (path beginning-line ending-line tool-name)
  "Returns the inclusive line subset from PATH."
  (when (< beginning-line 1)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "beginningLine must be >= 1."))
  (when (< ending-line beginning-line)
    (error 'mcp-tool-execution-error
           :tool-name tool-name
           :reason "endingLine must be >= beginningLine."))
  (with-open-file (stream path :direction :input)
    (let ((lines nil)
          (line-count 0)
          (eof-marker (gensym "EOF")))
      (loop for line = (read-line stream nil eof-marker)
            until (eq line eof-marker)
            do (incf line-count)
               (when (<= beginning-line line-count ending-line)
                 (push line lines)))
      (when (< line-count beginning-line)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "beginningLine ~D is past end of file (~D lines)."
                               beginning-line
                               line-count)))
      (format nil "~{~A~^~%~}" (nreverse lines)))))

(defun validate-directory-tool-pattern (pattern tool-name)
  "Validates PATTERN for the built-in directory tool."
  (let ((validated-pattern (normalize-builtin-tool-string-argument pattern "pattern" tool-name))
        (pattern-path (pathname pattern)))
    (when (uiop:absolute-pathname-p pattern-path)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "pattern must be relative to pathname."))
    (when (pathname-directory pattern-path)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason "pattern must not contain directory components."))
    validated-pattern))

(defun directory-tool-result (directory-path root pattern tool-name)
  "Returns a stable JSON array string of matching files for DIRECTORY-PATH and PATTERN."
  (declare (ignore tool-name))
  (let* ((validated-pattern (validate-directory-tool-pattern pattern "directory"))
         (matches (remove-if #'uiop:directory-exists-p
                             (cl:directory (merge-pathnames (pathname validated-pattern)
                                                            directory-path))))
         (relative-paths (sort (mapcar (lambda (path)
                                         (enough-namestring path root))
                                       matches)
                               #'string-lessp
                               :key #'string-downcase)))
    (cl-json:encode-json-to-string (coerce relative-paths 'vector))))

(defun write-file-content-from-lines (lines use-lf-only end-with-eol)
  "Serializes LINES using the requested line-ending controls."
  (if (null lines)
      ""
      (let ((separator (if use-lf-only
                           (string #\Linefeed)
                           (format nil "~C~C" #\Return #\Linefeed))))
        (with-output-to-string (stream)
          (loop for line in lines
                for firstp = t then nil
                do (unless firstp
                     (write-string separator stream))
                   (write-string line stream))
          (when end-with-eol
            (write-string separator stream))))))

(defun write-file-tool-result (target-path root lines use-lf-only end-with-eol)
  "Writes LINES to TARGET-PATH and returns a stable success string."
  (with-open-file (stream target-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (write-file-content-from-lines lines use-lf-only end-with-eol)
                  stream))
  (format nil "Wrote file: ~A" (enough-namestring target-path root)))

(defun delete-file-tool-result (path root)
  "Deletes PATH and returns a stable success string."
  (when (uiop:directory-exists-p path)
    (error 'mcp-tool-execution-error
           :tool-name "deleteFile"
           :reason (format nil "Path is not a file: ~A" path)))
  (delete-file path)
  (format nil "Deleted file: ~A" (enough-namestring path root)))

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

(defvar *eval-tool-timeout-seconds* 60
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

(defun default-execute-builtin-chatbot-tool (bot tool-name arguments)
  "Executes a built-in tool for BOT."
  (cond
    ((string= tool-name "webSearch")
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
    ((string= tool-name "hyperspecSearch")
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
    ((string= tool-name "eval")
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
    ((string= tool-name "readSamplingParameters")
     (sampling-parameters-tool-result bot))
    ((string= tool-name "startAgenticLoop")
     (unless *active-conversation*
       (error 'mcp-tool-execution-error
              :tool-name tool-name
              :reason "No active conversation is bound for autonomous loop startup."))
     (let* ((goal (normalize-builtin-tool-string-argument
                   (or (mcp-val "goal" arguments)
                       (mcp-val :goal arguments))
                   "goal"
                   tool-name))
            (max-iterations (let ((raw (or (mcp-val "maxIterations" arguments)
                                           (mcp-val :max-iterations arguments))))
                              (if raw
                                  (normalize-builtin-tool-integer-argument raw "maxIterations" tool-name)
                                  10)))
            (loop (start-agentic-loop *active-conversation*
                                      goal
                                      :max-iterations max-iterations)))
       (agentic-loop-public-json loop)))
    ((string= tool-name "listAgenticLoops")
     (agentic-loop-list-json))
    ((string= tool-name "readAgenticLoop")
     (let* ((loop-id (normalize-builtin-tool-integer-argument
                      (or (mcp-val "loopId" arguments)
                          (mcp-val :loop-id arguments))
                      "loopId"
                      tool-name))
            (loop (or (find-agentic-loop loop-id)
                      (error 'mcp-tool-execution-error
                             :tool-name tool-name
                             :reason (format nil "Unknown agentic loop id: ~A" loop-id)))))
       (agentic-loop-public-json loop)))
    ((string= tool-name "abortAgenticLoop")
     (multiple-value-bind (force-foundp force-value)
         (builtin-tool-argument arguments "force" :force)
       (let* ((loop-id (normalize-builtin-tool-integer-argument
                        (or (mcp-val "loopId" arguments)
                            (mcp-val :loop-id arguments))
                        "loopId"
                        tool-name))
              (force (if force-foundp
                         (normalize-builtin-tool-boolean-argument force-foundp
                                                                  force-value
                                                                  "force"
                                                                  tool-name)
                         nil))
              (loop (abort-agentic-loop loop-id :force force)))
         (agentic-loop-public-json loop))))
    ((string= tool-name "resumeAgenticLoop")
     (multiple-value-bind (approve-foundp approve-value)
         (builtin-tool-argument arguments "approve" :approve)
       (let* ((loop-id (normalize-builtin-tool-integer-argument
                        (or (mcp-val "loopId" arguments)
                            (mcp-val :loop-id arguments))
                        "loopId"
                        tool-name))
              (approve (normalize-builtin-tool-boolean-argument approve-foundp
                                                                approve-value
                                                                "approve"
                                                                tool-name))
              (loop (resume-agentic-loop loop-id :approve approve)))
         (agentic-loop-public-json loop))))
    ((string= tool-name "setSamplingParameters")
     (multiple-value-bind (temperature-foundp temperature-value)
         (builtin-tool-argument arguments "temperature" :temperature)
       (multiple-value-bind (top-p-foundp top-p-value)
           (builtin-tool-argument arguments "topP" :top-p :top_p)
         (unless (or temperature-foundp top-p-foundp)
           (error 'mcp-tool-execution-error
                  :tool-name tool-name
                  :reason "At least one of temperature or topP is required."))
         (handler-case
             (progn
               (apply #'set-sampling-parameters
                      bot
                      (append (when temperature-foundp
                                (list :temperature
                                      (normalize-builtin-tool-real-argument temperature-value "temperature" tool-name :allow-nil-p t)))
                              (when top-p-foundp
                                (list :top-p
                                      (normalize-builtin-tool-real-argument top-p-value "topP" tool-name :allow-nil-p t)))))
               (sampling-parameters-tool-result bot :saved t))
           (error (e)
             (error 'mcp-tool-execution-error
                    :tool-name tool-name
                    :reason (princ-to-string e)))))))
    ((string= tool-name "resetSamplingParameters")
     (reset-sampling-parameters bot)
     (sampling-parameters-tool-result bot :saved t))
    ((string= tool-name "readFileLines")
     (let* ((filename (mcp-val :filename arguments))
            (beginning-line (normalize-builtin-tool-integer-argument
                             (or (mcp-val "beginningLine" arguments)
                                 (mcp-val :beginning-line arguments))
                             "beginningLine"
                             tool-name))
            (ending-line (normalize-builtin-tool-integer-argument
                          (or (mcp-val "endingLine" arguments)
                              (mcp-val :ending-line arguments))
                          "endingLine"
                          tool-name))
            (path (resolve-filesystem-tool-path bot filename tool-name)))
       (read-file-lines-subset path beginning-line ending-line tool-name)))
    ((string= tool-name "readSystemInstructions")
     (ensure-system-instruction-tool-path bot tool-name)
     (system-instruction-tool-result bot))
    ((string= tool-name "insertSystemInstructionParagraph")
     (ensure-system-instruction-tool-path bot tool-name)
     (insert-system-instruction-paragraph
      bot
      (normalize-builtin-tool-string-argument
       (or (mcp-val "paragraph" arguments)
           (mcp-val :paragraph arguments))
       "paragraph"
       tool-name)
      :index (normalize-builtin-tool-integer-argument
              (or (mcp-val "index" arguments)
                  (mcp-val :index arguments))
              "index"
              tool-name))
     (save-system-instructions-or-tool-error bot tool-name)
     (system-instruction-tool-result bot :saved t))
    ((string= tool-name "updateSystemInstructionParagraph")
     (ensure-system-instruction-tool-path bot tool-name)
     (update-system-instruction-paragraph
      bot
      (normalize-builtin-tool-integer-argument
       (or (mcp-val "index" arguments)
           (mcp-val :index arguments))
       "index"
       tool-name)
      (normalize-builtin-tool-string-argument
       (or (mcp-val "paragraph" arguments)
           (mcp-val :paragraph arguments))
       "paragraph"
       tool-name))
     (save-system-instructions-or-tool-error bot tool-name)
     (system-instruction-tool-result bot :saved t))
    ((string= tool-name "deleteSystemInstructionParagraph")
     (ensure-system-instruction-tool-path bot tool-name)
     (delete-system-instruction-paragraph
      bot
      (normalize-builtin-tool-integer-argument
       (or (mcp-val "index" arguments)
           (mcp-val :index arguments))
       "index"
       tool-name))
     (save-system-instructions-or-tool-error bot tool-name)
     (system-instruction-tool-result bot :saved t))
    ((string= tool-name "replaceSystemInstructions")
     (ensure-system-instruction-tool-path bot tool-name)
     (multiple-value-bind (paragraphs-foundp paragraphs-value)
         (builtin-tool-argument arguments "paragraphs" :paragraphs)
       (replace-system-instruction-paragraphs
        bot
        (normalize-builtin-tool-string-sequence-argument
         paragraphs-foundp
         paragraphs-value
         "paragraphs"
         tool-name)))
     (save-system-instructions-or-tool-error bot tool-name)
     (system-instruction-tool-result bot :saved t))
    ((string= tool-name "directory")
     (multiple-value-bind (directory-path root)
         (resolve-filesystem-tool-directory bot
                                            (or (mcp-val "pathname" arguments)
                                                (mcp-val :pathname arguments))
                                            tool-name)
       (directory-tool-result directory-path
                              root
                              (or (mcp-val "pattern" arguments)
                                  (mcp-val :pattern arguments))
                              tool-name)))
    ((string= tool-name "writeFile")
     (multiple-value-bind (pathname-foundp pathname)
         (builtin-tool-argument arguments "pathname" :pathname)
       (declare (ignore pathname-foundp))
       (multiple-value-bind (use-lf-only-foundp use-lf-only-value)
           (builtin-tool-argument arguments "useLfOnly" :use-lf-only)
         (multiple-value-bind (end-with-eol-foundp end-with-eol-value)
             (builtin-tool-argument arguments "endWithEol" :end-with-eol)
           (multiple-value-bind (lines-foundp lines-value)
               (builtin-tool-argument arguments "lines" :lines)
             (multiple-value-bind (target-path root)
                 (resolve-filesystem-tool-target-path bot
                                                      (normalize-builtin-tool-string-argument pathname "pathname" tool-name)
                                                      tool-name)
               (write-file-tool-result target-path
                                       root
                                       (normalize-builtin-tool-string-sequence-argument lines-foundp
                                                                                        lines-value
                                                                                        "lines"
                                                                                        tool-name)
                                       (normalize-builtin-tool-boolean-argument use-lf-only-foundp
                                                                                use-lf-only-value
                                                                                "useLfOnly"
                                                                                tool-name)
                                       (normalize-builtin-tool-boolean-argument end-with-eol-foundp
                                                                                end-with-eol-value
                                                                                "endWithEol"
                                                                                tool-name))))))))
    ((string= tool-name "deleteFile")
     (multiple-value-bind (pathname-foundp pathname)
         (builtin-tool-argument arguments "pathname" :pathname)
       (unless pathname-foundp
         (error 'mcp-tool-execution-error
                :tool-name tool-name
                :reason "pathname is required."))
       (let* ((path (resolve-filesystem-tool-path bot pathname tool-name))
              (root (chatbot-filesystem-root-truename bot tool-name)))
         (delete-file-tool-result path root))))
    (t
     (error 'mcp-tool-execution-error
            :tool-name tool-name
            :reason "Unknown built-in tool."))))

(defun execute-chatbot-tool (bot source tool-name arguments)
  "Executes SOURCE as either a built-in or MCP tool for BOT."
  (if (eq source :built-in)
      (default-execute-builtin-chatbot-tool bot tool-name arguments)
      (execute-mcp-tool source tool-name arguments)))

(defun default-find-mcp-server-and-tool (bot tool-name)
  "Find the connected MCP server and tool definition that matches tool-name."
  (dolist (server (chatbot-mcp-servers bot))
    (handler-case
        (let* ((response (mcp-list-tools server))
               (tools (mcp-val :tools response)))
          (dolist (tool tools)
            (let ((name (mcp-val :name tool)))
              (when (string= name tool-name)
                (return-from default-find-mcp-server-and-tool (values server tool))))))
      (error (e)
        (error 'mcp-tool-lookup-error
               :tool-name tool-name
               :server-name (mcp-server-name server)
               :reason (princ-to-string e)))))
  (values nil nil))

(defun find-mcp-server-and-tool (bot tool-name)
  "Finds an MCP tool by name, honoring the configured test seam when present."
  (if *find-mcp-server-and-tool-function*
      (funcall *find-mcp-server-and-tool-function* bot tool-name)
      (default-find-mcp-server-and-tool bot tool-name)))

(defun mcp-result-items (value)
  "Returns VALUE as a proper list when it represents a JSON array."
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun mcp-structured-content (response)
  "Returns structured tool result content when present on RESPONSE."
  (or (mcp-val "structuredContent" response)
      (mcp-val :structured-content response)
      (mcp-val :structured_content response)))

(defun mcp-jsonish-value->string (value)
  "Renders VALUE as a textual tool result."
  (typecase value
    (null "null")
    (string value)
    (t (cl-json:encode-json-to-string value))))

(defun mcp-tool-result-error-p (response)
  "Returns true when RESPONSE reports a tool-level error."
  (let ((flag (or (mcp-val "isError" response)
                  (mcp-val :is-error response)
                  (mcp-val :is_error response))))
    (not (null flag))))

(defun mcp-tool-result-fallback-payload (response)
  "Returns the most useful non-text payload available on RESPONSE, or NIL."
  (let ((structured-content (mcp-structured-content response))
        (content (mcp-val :content response)))
    (cond
      (structured-content structured-content)
      (content content)
      ((or (mcp-val "result" response)
           (mcp-val :result response))
       (or (mcp-val "result" response)
           (mcp-val :result response)))
      (response response)
      (t nil))))

(defun default-execute-mcp-tool (server tool-name arguments)
  "Calls the tool on the given MCP server and returns the result content string."
  (handler-case
      (let* ((response (mcp-call-tool server tool-name arguments))
             (content (mcp-result-items (mcp-val :content response)))
             (result-texts nil))
        (when (mcp-tool-result-error-p response)
          (error 'mcp-tool-execution-error
                 :tool-name tool-name
                 :reason (mcp-jsonish-value->string response)))
        (dolist (item content)
          (let ((type (mcp-val :type item))
                (text (mcp-val :text item)))
            (when (and (string= type "text") text)
              (push text result-texts))))
        (if result-texts
            (format nil "~{~A~^~%~}" (nreverse result-texts))
            (let ((fallback (mcp-tool-result-fallback-payload response)))
              (if fallback
                  (mcp-jsonish-value->string fallback)
                  "Tool completed successfully."))))
    (error (e)
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (princ-to-string e)))))

(defun execute-mcp-tool (server tool-name arguments)
  "Executes an MCP tool, honoring the configured test seam when present."
  (if *execute-mcp-tool-function*
      (funcall *execute-mcp-tool-function* server tool-name arguments)
      (default-execute-mcp-tool server tool-name arguments)))
