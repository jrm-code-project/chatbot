;;; -*- Lisp -*-
;;; personas.lisp - persona file loading and preload helpers

(in-package "CHATBOT")

(defun get-user-homedir-pathname ()
  "Wrapper around user-homedir-pathname to allow package-lock-safe testing/mocking."
  (funcall *user-homedir-pathname-function*))

(defun persona-preload-memory-text (persona-dir)
  "Returns persona memory text from compressed-memory.txt or memory.json."
  (let ((compressed-path (probe-file (merge-pathnames "compressed-memory.txt" persona-dir)))
        (json-path (probe-file (merge-pathnames "memory.json" persona-dir))))
    (cond
      (compressed-path
       (log-message :info "Loading persona memory preload"
                    :context `(("source" . "compressed-memory.txt")
                               ("path" . ,(namestring compressed-path))))
       (string-right-trim '(#\Space #\Tab #\Return #\Linefeed)
                          (uiop:read-file-string compressed-path)))
      (json-path
       (log-message :info "Loading persona memory preload"
                    :context `(("source" . "memory.json")
                               ("path" . ,(namestring json-path))))
       (uiop:read-file-string json-path))
      (t nil))))

(defun preload-persona-conversation-memory (conversation persona-dir)
  "Stores persona preload memory separately from ordinary conversation turns."
  (let ((memory-text (persona-preload-memory-text persona-dir)))
    (when memory-text
      (setf (conversation-persona-memory conversation) memory-text))
    conversation))
