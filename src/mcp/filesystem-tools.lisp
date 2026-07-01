;;; -*- Lisp -*-
;;; filesystem-tools.lisp - built-in chatbot filesystem tool helpers

(in-package "CHATBOT")

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

(defun logical-pathname-within-directory-p (path directory)
  "Checks if PATH is logically within DIRECTORY without requiring PATH to exist on disk."
  (let* ((clean-path (pathname (uiop:native-namestring (uiop:ensure-directory-pathname path))))
         (clean-dir (pathname (uiop:native-namestring (uiop:ensure-directory-pathname directory))))
         (dir-dir (pathname-directory clean-dir))
         (path-dir (pathname-directory clean-path)))
    (and (equalp (pathname-device clean-path) (pathname-device clean-dir))
         (eq (car dir-dir) (car path-dir))
         (>= (length path-dir) (length dir-dir))
         (equal (subseq path-dir 0 (length dir-dir)) dir-dir))))

(defun canonicalize-allowed-filesystem-directories (directories)
  "Returns DIRECTORIES deduplicated and collapsed by ancestor coverage."
  (let ((sorted (sort (remove-duplicates
                       (mapcar #'uiop:ensure-directory-pathname directories)
                       :test (lambda (left right)
                               (string-equal (namestring left) (namestring right))))
                      #'<
                      :key (lambda (directory)
                             (length (namestring directory))))))
    (reduce (lambda (approved directory)
              (if (some (lambda (existing)
                          (filesystem-path-within-directory-p directory existing))
                        approved)
                  approved
                  (append approved (list directory))))
            sorted
            :initial-value nil)))

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
                            (make-pathname :name nil :type nil :defaults candidate))))
    (when (and (null (pathname-name candidate))
               (null (pathname-type candidate)))
      (error 'mcp-tool-execution-error
             :tool-name tool-name
             :reason (format nil "Pathname must name a file: ~A" pathname)))
    (let ((allowed-dirs (chatbot-effective-filesystem-allowed-directories bot tool-name)))
      (unless (some (lambda (allowed-dir)
                      (logical-pathname-within-directory-p parent-candidate allowed-dir))
                    allowed-dirs)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "Target parent directory is outside sandbox root: ~A" parent-candidate))))
    (ensure-directories-exist parent-candidate)
    (let* ((parent-resolved (probe-file parent-candidate))
           (parent-truename (uiop:ensure-directory-pathname parent-resolved))
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
    (let* ((eof-marker (gensym "EOF"))
           (lines (loop for line = (read-line stream nil eof-marker)
                        until (eq line eof-marker)
                        collect line))
           (line-count (length lines)))
      (when (< line-count beginning-line)
        (error 'mcp-tool-execution-error
               :tool-name tool-name
               :reason (format nil "beginningLine ~D is past end of file (~D lines)."
                               beginning-line
                               line-count)))
      (format nil "~{~A~^~%~}"
              (subseq lines
                      (1- beginning-line)
                      (min ending-line line-count))))))

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
      (let* ((separator (if use-lf-only
                            (string #\Linefeed)
                            (format nil "~C~C" #\Return #\Linefeed)))
             (content (reduce (lambda (left right)
                                (concatenate 'string left separator right))
                              (rest lines)
                              :initial-value (first lines))))
        (if end-with-eol
            (concatenate 'string content separator)
            content))))

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
