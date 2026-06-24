;;; -*- Lisp -*-
;;; attachments.lisp - transient chat file expansion and attachment encoding

(in-package "CHATBOT")

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun read-file-octets (pathname)
  "Reads PATHNAME as a vector of octets."
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (octets (make-array size :element-type '(unsigned-byte 8)))
           (count (read-sequence octets stream)))
      (if (= count size)
          octets
          (subseq octets 0 count)))))

(defun base64-encode-octets (octets)
  "Encodes OCTETS as a base64 string."
  (with-output-to-string (stream)
    (loop for index from 0 below (length octets) by 3
          for remaining = (- (length octets) index)
          for first = (aref octets index)
          for second = (if (> remaining 1) (aref octets (1+ index)) 0)
          for third = (if (> remaining 2) (aref octets (+ index 2)) 0)
          for chunk = (logior (ash first 16)
                              (ash second 8)
                              third)
          do (write-char (char +base64-alphabet+ (ldb (byte 6 18) chunk)) stream)
             (write-char (char +base64-alphabet+ (ldb (byte 6 12) chunk)) stream)
             (write-char (if (> remaining 1)
                             (char +base64-alphabet+ (ldb (byte 6 6) chunk))
                             #\=)
                         stream)
             (write-char (if (> remaining 2)
                             (char +base64-alphabet+ (ldb (byte 6 0) chunk))
                             #\=)
                         stream))))

(defun decode-octets-as-utf-8 (octets)
  "Decodes OCTETS as UTF-8 when supported, otherwise as Latin-1."
  #+sbcl
  (sb-ext:octets-to-string octets :external-format :utf-8)
  #-sbcl
  (coerce (map 'list #'code-char octets) 'string))

(defun make-chat-file-attachment (pathname)
  "Reads PATHNAME and prepares one transient prompt attachment descriptor."
  (let* ((resolved (truename pathname))
         (content-type-info (attachment-content-type-info :pathname resolved))
         (mime-type (getf content-type-info :mime-type))
         (octets (read-file-octets resolved))
         (base64 (base64-encode-octets octets)))
    (list (cons :pathname resolved)
          (cons :pathname-string (namestring resolved))
          (cons :display-name (file-namestring resolved))
          (cons :mime-type mime-type)
          (cons :interaction-type (getf content-type-info :interaction-type))
          (cons :size-bytes (length octets))
          (cons :base64-data base64)
          (cons :text-fallback
                (when (getf content-type-info :textual-p)
                  (decode-octets-as-utf-8 octets))))))

(defun prepare-chat-file-attachments (files)
  "Expands FILES and reads the resulting files into transient attachment descriptors."
  (unless (listp files)
    (error ":files must be a list of file or directory pathnames."))
  (mapcar #'make-chat-file-attachment
          (resolve-chat-input-files files)))

(defun attachment-openai-text (attachment)
  "Builds the text fallback content for ATTACHMENT."
  (let ((pathname-string (cdr (assoc :pathname-string attachment)))
        (mime-type (cdr (assoc :mime-type attachment)))
        (text-fallback (cdr (assoc :text-fallback attachment)))
        (base64-data (cdr (assoc :base64-data attachment))))
    (if text-fallback
        (format nil "[Attached file: ~A (~A)]~%~A"
                pathname-string
                mime-type
                text-fallback)
        (format nil "[Attached file: ~A (~A, base64)]~%~A"
                pathname-string
                mime-type
                base64-data))))

(defun make-interaction-file-part (attachment)
  "Converts ATTACHMENT into an Interactions API content part."
  `(("type" . ,(cdr (assoc :interaction-type attachment)))
    ("data" . ,(cdr (assoc :base64-data attachment)))
    ("mime_type" . ,(cdr (assoc :mime-type attachment)))))

(defun make-generate-content-file-part (attachment)
  "Converts ATTACHMENT into a generateContent inlineData part."
  `(("inlineData" . (("mimeType" . ,(cdr (assoc :mime-type attachment)))
                     ("data" . ,(cdr (assoc :base64-data attachment)))))))

(defun openai-file-content-parts (attachment)
  "Converts ATTACHMENT into one or more OpenAI-compatible content parts."
  (let ((mime-type (cdr (assoc :mime-type attachment))))
    (cond
      ((alexandria:starts-with-subseq "image/" mime-type)
       (list `(("type" . "text")
               ("text" . ,(format nil "[Attached image: ~A (~A)]"
                                  (cdr (assoc :pathname-string attachment))
                                  mime-type)))
             `(("type" . "image_url")
               ("image_url" . (("url" . ,(format nil "data:~A;base64,~A"
                                                 mime-type
                                                 (cdr (assoc :base64-data attachment)))))))))
      (t
       (list `(("type" . "text")
               ("text" . ,(attachment-openai-text attachment))))))))
