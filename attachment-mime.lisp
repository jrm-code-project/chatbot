;;; -*- Lisp -*-
;;; attachment-mime.lisp - attachment MIME and content-typing policy

(in-package "CHATBOT")

(defun validate-pathname-content-type-rule (rule)
  "Returns RULE after rejecting malformed grouped content-type declarations."
  (let ((mime-type (getf rule :mime-type))
        (extensions (getf rule :extensions)))
    (unless (and (stringp mime-type)
                 (> (length mime-type) 0))
      (error "Attachment content-type rule is missing a MIME type: ~S" rule))
    (unless (and (listp extensions)
                 extensions)
      (error "Attachment content-type rule is missing extensions: ~S" rule))
    (dolist (extension extensions)
      (unless (and (stringp extension)
                   (> (length extension) 0))
        (error "Attachment content-type rule has an invalid extension: ~S" rule)))
    (when (/= (length extensions)
              (length (remove-duplicates extensions :test #'string=)))
      (error "Attachment content-type rule repeats an extension: ~S" rule))
    rule))

(defun validate-pathname-content-type-rules (rules)
  "Returns RULES after rejecting malformed or overlapping grouped rules."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (rule rules)
      (validate-pathname-content-type-rule rule)
      (dolist (extension (getf rule :extensions))
        (when (gethash extension seen)
          (error "Duplicate attachment content-type rule for extension: ~A" extension))
        (setf (gethash extension seen) t))))
  rules)

(defparameter +pathname-content-type-rules+
  (validate-pathname-content-type-rules
   '((:mime-type "text/plain" :extensions ("txt" "text" "org"))
     (:mime-type "text/markdown" :extensions ("md" "markdown"))
     (:mime-type "text/csv" :extensions ("csv"))
     (:mime-type "text/tab-separated-values" :extensions ("tsv"))
     (:mime-type "application/json" :extensions ("json") :textual-p t)
     (:mime-type "application/xml" :extensions ("xml") :textual-p t)
     (:mime-type "application/yaml" :extensions ("yaml" "yml") :textual-p t)
     (:mime-type "text/html" :extensions ("html" "htm"))
     (:mime-type "text/css" :extensions ("css"))
     (:mime-type "application/javascript" :extensions ("js" "mjs" "jsx") :textual-p t)
     (:mime-type "application/typescript" :extensions ("ts" "tsx") :textual-p t)
     (:mime-type "text/plain" :extensions ("lisp" "lsp" "cl" "asd") :textual-p t)
     (:mime-type "application/pdf" :extensions ("pdf"))
     (:mime-type "image/png" :extensions ("png"))
     (:mime-type "image/jpeg" :extensions ("jpg" "jpeg"))
     (:mime-type "image/gif" :extensions ("gif"))
     (:mime-type "image/webp" :extensions ("webp"))
     (:mime-type "image/svg+xml" :extensions ("svg"))
     (:mime-type "image/bmp" :extensions ("bmp"))
     (:mime-type "audio/mpeg" :extensions ("mp3"))
     (:mime-type "audio/wav" :extensions ("wav"))
     (:mime-type "audio/flac" :extensions ("flac"))
     (:mime-type "audio/ogg" :extensions ("ogg"))
     (:mime-type "audio/mp4" :extensions ("m4a"))
     (:mime-type "video/mp4" :extensions ("mp4"))
     (:mime-type "video/quicktime" :extensions ("mov"))
     (:mime-type "video/webm" :extensions ("webm"))
     (:mime-type "video/x-msvideo" :extensions ("avi")))))

(defun validate-pathname-content-type-policies (policies)
  "Returns POLICIES after rejecting duplicate extension entries."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (policy policies)
      (let ((extension (car policy)))
        (when (gethash extension seen)
          (error "Duplicate attachment content-type policy for extension: ~A" extension))
        (setf (gethash extension seen) t))))
  policies)

(defun build-pathname-content-type-policies ()
  "Expands grouped content-type rules into extension-indexed policies."
  (validate-pathname-content-type-policies
   (loop for rule in +pathname-content-type-rules+
         append
         (let ((mime-type (getf rule :mime-type))
               (extensions (getf rule :extensions))
               (textual-p-provided (member :textual-p rule))
               (textual-p (getf rule :textual-p)))
           (mapcar (lambda (extension)
                     (cons extension
                           (append (list :mime-type mime-type)
                                   (when textual-p-provided
                                     (list :textual-p textual-p)))))
                   extensions)))))

(defparameter +pathname-content-type-policies+
  (build-pathname-content-type-policies))

(defun build-textual-mime-types ()
  "Derives MIME types with explicit textual fallback from grouped content-type rules."
  (remove-duplicates
   (loop for rule in +pathname-content-type-rules+
         when (getf rule :textual-p)
         collect (getf rule :mime-type))
   :test #'string=))

(defparameter +textual-mime-types+
  (build-textual-mime-types))

(defun pathname-content-type-policy (pathname)
  "Returns the declared content-type policy for PATHNAME, if any."
  (let* ((type (pathname-type pathname))
         (extension (and type (string-downcase type))))
    (and extension
         (cdr (assoc extension +pathname-content-type-policies+ :test #'string=)))))

(defun mime-type-interaction-content-type (mime-type)
  "Returns the Interactions API content type for MIME-TYPE."
  (cond
    ((alexandria:starts-with-subseq "image/" mime-type) "image")
    ((alexandria:starts-with-subseq "audio/" mime-type) "audio")
    ((alexandria:starts-with-subseq "video/" mime-type) "video")
    (t "document")))

(defun default-textual-mime-type-p (mime-type)
  "Returns the fallback textual policy for MIME-TYPE when no explicit override exists."
  (or (alexandria:starts-with-subseq "text/" mime-type)
      (member mime-type +textual-mime-types+ :test #'string=)))

(defun attachment-content-type-info (&key pathname mime-type)
  "Returns the canonical attachment content-type descriptor.
The result is a plist with :MIME-TYPE, :TEXTUAL-P, and :INTERACTION-TYPE."
  (let* ((policy (and pathname
                      (pathname-content-type-policy pathname)))
         (resolved-mime-type (or mime-type
                                 (getf policy :mime-type)
                                 "application/octet-stream")))
    (list :mime-type resolved-mime-type
          :textual-p (if policy
                         (if (member :textual-p policy)
                             (getf policy :textual-p)
                             (default-textual-mime-type-p resolved-mime-type))
                         (default-textual-mime-type-p resolved-mime-type))
          :interaction-type (mime-type-interaction-content-type resolved-mime-type))))

(defun pathname-mime-type (pathname)
  "Infers a MIME type for PATHNAME from its extension."
  (getf (attachment-content-type-info :pathname pathname)
        :mime-type))

(defun textual-mime-type-p (mime-type)
  "Returns true when MIME-TYPE should fall back to inline text parts."
  (getf (attachment-content-type-info :mime-type mime-type)
        :textual-p))

(defun interaction-content-type-for-mime-type (mime-type)
  "Returns the Interactions API content type for MIME-TYPE."
  (getf (attachment-content-type-info :mime-type mime-type)
        :interaction-type))
