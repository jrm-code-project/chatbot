;;; -*- Lisp -*-
;;; narc.lisp - Narc prompt and related utilities

(in-package "CHATBOT")

(defparameter *narc-grab-script*
  #p"C:/Users/bitdi/.Personas/V/narc_grab_frame.ps1"
  "Path to the background PotPlayer frame capture script.")

(defparameter *narc-script-path*
  #p"C:/Users/bitdi/.Personas/V/narc_capture.ps1"
  "Path to the background PotPlayer trigger and OCR extractor.")

(defun narc-capture-text ()
  "Silently triggers PotPlayer to snap a frame from the Narc, 
runs Windows OCR, and returns the transcribed text as a string."
  (with-output-to-string (s)
    (uiop:run-program
     (list "powershell" "-NoProfile" "-ExecutionPolicy" "Bypass"
           "-File" (uiop:native-namestring *narc-script-path*))
     :output s
     :error-output nil)))

(defun narc-capture-image-path ()
  "Triggers PotPlayer to snap the current Narc frame and returns
the absolute pathname to the newly captured JPEG file."
  (let ((output
          (string-trim
           '(#\Space #\Tab #\Newline #\Return)
           (with-output-to-string (s)
             (uiop:run-program
              (list "powershell" "-NoProfile" "-ExecutionPolicy" "Bypass"
                    "-File" (uiop:native-namestring *narc-grab-script*))
              :output s
              :error-output nil)))))
    (pathname output)))

(defun narc-peek (&optional prompt)
  "Peeks at the Narc screen.
If PROMPT is nil, prints the transcribed screen text.
If PROMPT is provided, sends the transcribed text along with your question 
directly to V for analysis."
  (let ((screen-text (narc-capture-text)))
    (if (or (null prompt) (string= prompt ""))
        (progn
          (format t "~%=== NARC SCREEN TEXT ===~%~A~%========================~%" screen-text)
          screen-text)
        ;; Call your conversational REPL function here:
        (chat prompt :extra screen-text))))

(defun narc-peek-raw (&key (prompt "What is on the screen right now?"))
  "Captures a raw JPEG of the Narc and sends it directly to V's vision model.
If PROMPT is not specified, defaults to asking for a general screen breakdown."
  (let ((image-path (narc-capture-image-path)))
    (format t "~&[NARC] Captured frame: ~A~%" image-path)
    ;; Dispatches the raw image and prompt to V's multimodal interface:
    (chat prompt :file image-path)))
