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

(defparameter +google-gemini-model-override-marker+ #\$
  "Leading prompt marker that requests the Gemini Pro override model for one turn.")

(defparameter +google-gemini-model-override-model+ "gemini-pro-latest"
  "Temporary model used when a Google or Gemini prompt starts with the override marker.")

(defun format-prompt-model-indicator (model)
  "Formats MODEL as a prompt prefix like [model: gemini-3-flash]."
  (format nil "[model: ~A]" model))

(defun resolve-prompt-model-override (chatbot input)
  "Returns INPUT with any supported per-turn model override marker removed.

When INPUT starts with the override marker for the Google or Gemini backends,
also returns the effective model name to use for that turn."
  (let ((backend (and chatbot (chatbot-backend chatbot))))
    (if (and (stringp input)
             (> (length input) 0)
             (char= (char input 0) +google-gemini-model-override-marker+)
             (member backend '(:gemini :google)))
        (values (subseq input 1) +google-gemini-model-override-model+)
        (values input nil))))

(defun decorate-live-user-input (chatbot input &key effective-model)
  "Decorates string INPUT with transient prompt prefixes requested by CHATBOT."
  (if (and chatbot
           (stringp input))
      (let ((parts nil))
        (when (chatbot-include-timestamp-p chatbot)
          (push (funcall *prompt-timestamp-function*) parts))
        (when (chatbot-include-model-p chatbot)
          (push (format-prompt-model-indicator (or effective-model
                                                  (chatbot-model chatbot)))
                parts))
        (if parts
            (format nil "~{~A~^ ~} ~A" (nreverse parts) input)
            input))
      input))
