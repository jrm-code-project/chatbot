;;; -*- Lisp -*-
;;; screenshot-tools.lisp - convenience helpers for screenshot-driven prompts

(in-package "CHATBOT")

(defparameter *screenshot-path*
  #p"~/AppData/Roaming/PotPlayerMini64/Capture/*.jpg"
  "Primary wildcard pathname used by SEND-LATEST-SCREENSHOT.")

(defparameter *screenshot-path-1*
  #p"~/OneDrive/Pictures/Screenshots 1/*.*"
  "Secondary wildcard pathname used by SEND-LATEST-SCREENSHOT.")

(defparameter *screenshot-prompt*
  "Examine the attached screenshot(s) and provide a comprehensive, detailed
description of all visual elements, layout structures, and contextual details.
Transcribe all visible text with verbatim accuracy. Deliver the analysis in a
highly engaging, witty, and humorous tone, ensuring your personality enriches
the description without sacrificing clarity or detail."
  "Base prompt used by SEND-LATEST-SCREENSHOT.")

(defun screenshot-file-specs ()
  "Returns the configured screenshot wildcard pathnames."
  (remove nil (list *screenshot-path*
                    *screenshot-path-1*)))

(defun screenshot-request-prompt (prompt)
  "Builds the final screenshot analysis prompt."
  (unless (stringp prompt)
    (error "PROMPT must be a string."))
  (let ((extra (string-trim '(#\Space #\Tab #\Return #\Linefeed) prompt)))
    (if (string= extra "")
        *screenshot-prompt*
        (format nil "~A ~A" *screenshot-prompt* extra))))

(defun send-latest-screenshot (&key (n 1) (prompt "") conversation callback)
  "Sends the N newest configured screenshots as transient chat attachments."
  (unless (and (integerp n) (> n 0))
    (error "N must be a positive integer."))
  (let ((files (latest-chat-matching-files (screenshot-file-specs) :n n)))
    (unless files
      (error "No screenshots found matching the configured screenshot paths."))
    (chat (screenshot-request-prompt prompt)
          :conversation conversation
          :callback callback
          :files files)))
