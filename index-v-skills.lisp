;;; -*- Lisp -*-
;;; index-v-skills.lisp - Script to create V_Skills collection and index existing V skills

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(push (uiop:getcwd) asdf:*central-registry*)

(format t "Loading chatbot system...~%")
(finish-output)
(ql:quickload "chatbot" :silent t)

(in-package "CHATBOT")

(defun run-indexing ()
  "Main function to coordinate V skills indexing."
  (format t "Checking if ChromaDB is running...~%")
  (finish-output)
  (if (not (chroma-alive-p))
      (progn
        (format t "ChromaDB is not running on ~A:~D. Skipping V_Skills indexing.~%"
                *chroma-host* *chroma-port*)
        (finish-output)
        (uiop:quit 0))
      (format t "ChromaDB is running! Proceeding with indexing.~%"))

  (let* ((persona-dir (resolve-persona-directory "V"))
         (skills-dir (merge-pathnames "Skills/" persona-dir))
         (files (and (uiop:directory-exists-p skills-dir) (uiop:directory-files skills-dir)))
         (total (length files)))

    (format t "Found ~D existing skills for V.~%" total)
    (finish-output)

    ;; Ensure skills collection exists
    (ensure-persona-skills-collection "V")

    (dolist (file files)
      (let* ((skill-name (pathname-name file))
             (content (uiop:read-file-string file)))
        (when skill-name
          (format t "  -> Indexing skill: ~A... " skill-name)
          (finish-output)
          (multiple-value-bind (response status)
              (save-persona-skill "V" skill-name content :description (format nil "V's ~A skill" skill-name))
            (declare (ignore response))
            (if (eq status :host-unavailable)
                (progn
                  (format t "failed (host unavailable).~%")
                  (finish-output)
                  (uiop:quit 1))
                (progn
                  (format t "success.~%")
                  (finish-output)))))))

    (format t "Successfully completed indexing of all ~D skills into ChromaDB collection V_Skills!~%" total)
    (finish-output)
    (uiop:quit 0)))

(run-indexing)
