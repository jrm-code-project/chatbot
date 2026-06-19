#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(let ((repos-dir (cond
                   ((uiop:directory-exists-p "/mnt/d/repositories/") "/mnt/d/repositories/")
                   ((uiop:directory-exists-p "D:/repositories/") "D:/repositories/"))))
  (when repos-dir
    (asdf:initialize-source-registry
     `(:source-registry (:tree ,repos-dir) :inherit-configuration))))

(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :chatbot)
(if (chatbot::run-all-tests)
    (uiop:quit 0)
    (uiop:quit 1))
