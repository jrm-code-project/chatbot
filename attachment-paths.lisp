;;; -*- Lisp -*-
;;; attachment-paths.lisp - transient chat file path expansion and normalization

(in-package "CHATBOT")

(defun home-relative-chat-pathname-p (pathname)
  "Returns true when PATHNAME starts with a user-home marker."
  (let ((namestring (namestring pathname)))
    (or (alexandria:starts-with-subseq "~/" namestring)
        (alexandria:starts-with-subseq "~\\" namestring)
        (string= "~" namestring))))

(defun expand-chat-home-pathname (pathname)
  "Expands a ~/ relative PATHNAME against the configured user home directory."
  (if (home-relative-chat-pathname-p pathname)
      (let* ((namestring (namestring pathname))
             (home (funcall *user-homedir-pathname-function*))
             (relative (cond
                         ((string= "~" namestring) "")
                         ((or (alexandria:starts-with-subseq "~/" namestring)
                              (alexandria:starts-with-subseq "~\\" namestring))
                          (subseq namestring 2))
                         (t namestring))))
        (merge-pathnames (pathname relative) home))
      pathname))

(defun normalize-chat-file-spec (file-spec)
  "Normalizes FILE-SPEC into a pathname."
  (expand-chat-home-pathname
   (etypecase file-spec
     (pathname file-spec)
     (string (pathname file-spec)))))

(defun expand-chat-input-directory-files (directory)
  "Recursively expands DIRECTORY into a stable list of file pathnames."
  (let* ((resolved-directory (uiop:ensure-directory-pathname (truename directory)))
         (files (stable-sort (copy-list (uiop:directory-files resolved-directory))
                             #'string<
                             :key #'namestring))
         (subdirectories (stable-sort (copy-list (uiop:subdirectories resolved-directory))
                                      #'string<
                                      :key #'namestring)))
    (append files
            (mapcan #'expand-chat-input-directory-files subdirectories))))

(defun expand-chat-input-file-spec (file-spec)
  "Expands FILE-SPEC into zero or more concrete file pathnames."
  (let ((pathname (normalize-chat-file-spec file-spec)))
    (cond
      ((wild-pathname-p pathname)
       (let ((matches (stable-sort (copy-list (cl:directory pathname))
                                   #'string<
                                   :key #'namestring)))
         (unless matches
           (error "No files matched wildcard pathname ~A." pathname))
         (mapcan #'expand-chat-input-file-spec matches)))
      ((uiop:directory-exists-p pathname)
       (expand-chat-input-directory-files pathname))
      ((probe-file pathname)
       (let ((resolved (probe-file pathname)))
         (if (uiop:directory-exists-p resolved)
             (expand-chat-input-directory-files resolved)
             (list (truename resolved)))))
      (t
       (error "File path not found: ~A" pathname)))))

(defun deduplicate-chat-input-files (pathnames)
  "Returns PATHNAMES with duplicate concrete files removed, preserving order."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (pathname pathnames (nreverse result))
      (let* ((resolved (truename pathname))
             (key (string-downcase (namestring resolved))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push resolved result))))))

(defun resolve-chat-input-files (files)
  "Resolves FILES into a stable deduplicated list of concrete file pathnames."
  (deduplicate-chat-input-files
   (mapcan #'expand-chat-input-file-spec files)))

(defun matching-chat-input-files (file-spec)
  "Expands FILE-SPEC into zero or more concrete files.
Unlike EXPAND-CHAT-INPUT-FILE-SPEC, unmatched wildcards return NIL."
  (let ((pathname (normalize-chat-file-spec file-spec)))
    (cond
      ((wild-pathname-p pathname)
       (mapcan #'expand-chat-input-file-spec
               (stable-sort (copy-list (cl:directory pathname))
                            #'string<
                            :key #'namestring)))
      ((uiop:directory-exists-p pathname)
       (expand-chat-input-directory-files pathname))
      ((probe-file pathname)
       (let ((resolved (probe-file pathname)))
         (if (uiop:directory-exists-p resolved)
             (expand-chat-input-directory-files resolved)
             (list (truename resolved)))))
      (t nil))))

(defun chat-input-file-newer-p (left right)
  "Returns true when LEFT should sort ahead of RIGHT by write time."
  (let ((left-date (or (file-write-date left) 0))
        (right-date (or (file-write-date right) 0)))
    (if (/= left-date right-date)
        (> left-date right-date)
        (string-lessp (namestring left)
                      (namestring right)))))

(defun latest-chat-matching-files (file-specs &key (n 1))
  "Returns up to N newest files matching FILE-SPECS, or NIL when none match."
  (unless (listp file-specs)
    (error "FILE-SPECS must be a list of file or directory pathnames."))
  (unless (and (integerp n) (>= n 0))
    (error "N must be a non-negative integer."))
  (let* ((matches (deduplicate-chat-input-files
                   (mapcan #'matching-chat-input-files file-specs)))
         (sorted (stable-sort (copy-list matches)
                              #'chat-input-file-newer-p)))
    (subseq sorted 0 (min n (length sorted)))))
