;;;

(in-package "CHATBOT")

;;; Miscellaneous process-wide globals: background-thread startup seam,
;;; cumulative token accounting, and interaction call-duration tracking.

(defun default-persona-memory-compression-thread-function (thunk thread-name)
  "Starts a background thread for persona memory compression."
  (sb-thread:make-thread thunk :name thread-name))

(defparameter *persona-memory-compression-thread-function*
  #'default-persona-memory-compression-thread-function
  "Function used to start background persona memory compression threads.")

(defvar *global-token-grand-totals* nil
  "Process-wide cumulative token totals shared across unrelated chats.")

(defvar *global-token-grand-totals-lock*
  (sb-thread:make-mutex :name "global-token-grand-totals-lock")
  "Mutex protecting process-wide token grand total updates.")

(defvar *last-interaction-model-call-duration* nil
  "The accumulated duration (in seconds) of model calls during the current interaction.")
