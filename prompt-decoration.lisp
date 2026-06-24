;;; -*- Lisp -*-
;;; prompt-decoration.lisp - transient prompt prefix formatting

(in-package "CHATBOT")

(defparameter +prompt-timestamp-month-abbreviations+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun format-prompt-timestamp (universal-time &optional time-zone)
  "Formats UNIVERSAL-TIME as a prompt prefix like [14:29 26-Jun-2026]."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time time-zone)
    (declare (ignore second))
    (format nil "[~2,'0D:~2,'0D ~2,'0D-~A-~4,'0D]"
            hour
            minute
            day
            (svref +prompt-timestamp-month-abbreviations+ (1- month))
            year)))

(defun default-prompt-timestamp-function ()
  "Returns the current local prompt timestamp string."
  (format-prompt-timestamp (get-universal-time)))

(defvar *prompt-timestamp-function* #'default-prompt-timestamp-function
  "Function used to generate the current prompt timestamp string.")

(defun format-prompt-model-indicator (model)
  "Formats MODEL as a prompt prefix like [model: gemini-3-flash]."
  (format nil "[model: ~A]" model))

(defun decorate-live-user-input (chatbot input)
  "Decorates string INPUT with transient prompt prefixes requested by CHATBOT."
  (if (and chatbot
           (stringp input))
      (let ((parts nil))
        (when (chatbot-include-timestamp-p chatbot)
          (push (funcall *prompt-timestamp-function*) parts))
        (when (chatbot-include-model-p chatbot)
          (push (format-prompt-model-indicator (chatbot-model chatbot)) parts))
        (if parts
            (format nil "~{~A~^ ~} ~A" (nreverse parts) input)
            input))
      input))
