;;; -*- Lisp -*-
;;; inbox-polling.lisp - S3-backed persona inbound-email polling
;;;
;;; A persona whose config sets :inbox-s3-path may receive email notifications
;;; (e.g. delivered to that S3 prefix by AWS SES) that get folded into the
;;; conversation as synthetic system messages ahead of the next live turn.
;;; The AWS CLI calls are the imperative edge; MIME parsing and history
;;; injection are pure functions of their inputs.

(in-package "CHATBOT")

(defun s3-ls-line-key (line)
  "Returns the S3 object key (last whitespace-separated token) from one `aws s3 ls` output LINE, or NIL."
  (let* ((tokens (remove "" (cl-ppcre:split "\\s+" (string-trim '(#\Space #\Tab #\Return) line))
                         :test #'string=)))
    (car (last tokens))))

(defun s3-command-failure-context (exit-code stderr &rest extra-context)
  "Returns a log-message context alist describing a failed AWS CLI invocation, including trimmed STDERR."
  (append extra-context
          (list (cons "exit-code" exit-code)
                (cons "stderr" (string-trim '(#\Space #\Tab #\Newline #\Return) (or stderr ""))))))

(defun list-s3-inbox-keys (s3-path)
  "Returns the list of object keys currently present under S3-PATH via `aws s3 ls`."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (format nil "aws s3 ls ~A" s3-path)
                        :force-shell t
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (if (zerop exit-code)
        (remove nil
                (mapcar #'s3-ls-line-key
                        (cl-ppcre:split "\\r?\\n" stdout)))
        (progn
          (log-message :warn "Failed to list S3 inbox"
                       :context (s3-command-failure-context exit-code stderr (cons "path" s3-path)))
          nil))))

(defun fetch-s3-object-text (s3-path key)
  "Returns the text contents of KEY under S3-PATH, or NIL on failure."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (format nil "aws s3 cp ~A~A -" s3-path key)
                        :force-shell t
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (if (zerop exit-code)
        stdout
        (progn
          (log-message :warn "Failed to fetch S3 inbox object"
                       :context (s3-command-failure-context exit-code stderr (cons "path" s3-path) (cons "key" key)))
          nil))))

(defun delete-s3-object (s3-path key)
  "Deletes KEY under S3-PATH so it is not processed again. Returns true on success."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (format nil "aws s3 rm ~A~A" s3-path key)
                        :force-shell t
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore stdout))
    (if (zerop exit-code)
        t
        (progn
          (log-message :warn "Failed to delete S3 inbox object"
                       :context (s3-command-failure-context exit-code stderr (cons "path" s3-path) (cons "key" key)))
          nil))))

(defun fetch-and-clear-inbox (s3-path)
  "Imperatively pulls raw email bodies from the S3 prefix S3-PATH and deletes them so they aren't re-processed.
Returns a list of raw MIME strings in arrival (listing) order. S3-PATH must end in a trailing slash."
  (loop for key in (list-s3-inbox-keys s3-path)
        for raw = (fetch-s3-object-text s3-path key)
        when raw
          do (delete-s3-object s3-path key)
          and collect raw))

;;; ---------------------------------------------------------------------
;;; Security gateway configuration
;;; ---------------------------------------------------------------------

(defparameter +default-inbox-allowed-sender+ "eval.apply@gmail.com"
  "The only sender address permitted to inject inbox email content into a persona's conversation.
Any email whose From: address does not strictly match this value is quarantined (discarded, and never
passed on to MIME parsing, sanitization, or the LLM context).")

(defparameter +inbox-email-max-body-length+ 4000
  "Maximum sanitized email body length (characters) forwarded to the LLM context.
Guards against context-bombing via oversized inbound email bodies.")

(defparameter +inbox-email-truncation-marker+ "[MESSAGE TRUNCATED FOR SECURITY]"
  "Marker appended to an inbox email body when it is truncated by +INBOX-EMAIL-MAX-BODY-LENGTH+.")

(defparameter +inbox-email-max-raw-body-length+ 200000
  "Hard cap on raw MIME body length considered before MIME parsing/sanitization, bounding the
worst-case cost of regex-based parsing against an oversized or adversarial payload.")

;;; ---------------------------------------------------------------------
;;; RFC 5322 header parsing (pure)
;;; ---------------------------------------------------------------------

(defun split-mime-headers-and-body (raw-mime)
  "Returns (VALUES HEADERS-TEXT BODY-TEXT) split at the first blank line in RAW-MIME.
When no blank line is found, the entire message is treated as headerless, with BODY-TEXT
equal to RAW-MIME and HEADERS-TEXT empty."
  (multiple-value-bind (match-start match-end)
      (cl-ppcre:scan "\\r?\\n\\r?\\n" raw-mime)
    (if match-start
        (values (subseq raw-mime 0 match-start) (subseq raw-mime match-end))
        (values "" raw-mime))))

(defun unfold-mime-header-lines (headers-text)
  "Returns HEADERS-TEXT with RFC 5322 folded continuation lines (leading whitespace) joined onto the previous line."
  (cl-ppcre:regex-replace-all "\\r?\\n[ \\t]+" headers-text " "))

(defun parse-mime-headers (headers-text)
  "Returns an alist of (lowercase-header-name . trimmed-value) parsed from one MIME HEADERS-TEXT block."
  (loop for line in (cl-ppcre:split "\\r?\\n" (unfold-mime-header-lines headers-text))
        for colon-pos = (position #\: line)
        when (and colon-pos (plusp colon-pos))
          collect (cons (string-downcase (string-trim '(#\Space #\Tab) (subseq line 0 colon-pos)))
                        (string-trim '(#\Space #\Tab) (subseq line (1+ colon-pos))))))

(defun mime-header-value (headers name)
  "Returns the value of header NAME (case-insensitive) from parsed HEADERS, or NIL when absent."
  (cdr (assoc (string-downcase name) headers :test #'string=)))

(defun parse-mime-content-type (content-type-value)
  "Returns (VALUES MEDIA-TYPE BOUNDARY) parsed from a Content-Type header CONTENT-TYPE-VALUE.
MEDIA-TYPE is lowercased (e.g. \"text/plain\", \"multipart/alternative\") and defaults to
\"text/plain\" when CONTENT-TYPE-VALUE is NIL. BOUNDARY is the (unquoted) multipart boundary
token, or NIL when absent."
  (if (null content-type-value)
      (values "text/plain" nil)
      (let* ((first-segment (car (cl-ppcre:split ";" content-type-value :limit 2)))
             (media-type (string-downcase (string-trim '(#\Space #\Tab) first-segment)))
             (boundary (cl-ppcre:register-groups-bind (quoted unquoted)
                           ("(?i)boundary=\"([^\"]*)\"|(?i)boundary=([^;\\s]+)" content-type-value)
                         (or quoted unquoted))))
        (values media-type boundary))))

(defun mime-transfer-encoding (headers)
  "Returns the downcased Content-Transfer-Encoding value from HEADERS, defaulting to \"7bit\"."
  (let ((value (mime-header-value headers "content-transfer-encoding")))
    (if value (string-downcase (string-trim '(#\Space #\Tab) value)) "7bit")))

;;; ---------------------------------------------------------------------
;;; Content-Transfer-Encoding decoding (pure)
;;; ---------------------------------------------------------------------

(defun quoted-printable-decode-octets (text)
  "Returns a (VECTOR (UNSIGNED-BYTE 8)) of raw octets decoded from quoted-printable TEXT (RFC 2045),
with soft line-break sequences (\"=\" at end of line) removed first."
  (let* ((unfolded (cl-ppcre:regex-replace-all "=\\r?\\n" text ""))
         (len (length unfolded))
         (octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (do ((i 0))
        ((>= i len) octets)
      (let ((ch (char unfolded i)))
        (if (and (char= ch #\=) (<= (+ i 3) len))
            (let ((hex (subseq unfolded (1+ i) (+ i 3))))
              (handler-case
                  (progn (vector-push-extend (parse-integer hex :radix 16) octets)
                         (incf i 3))
                (error ()
                  (vector-push-extend (char-code ch) octets)
                  (incf i))))
            (progn
              (vector-push-extend (char-code ch) octets)
              (incf i)))))))

(defun decode-quoted-printable (text)
  "Pure function decoding a quoted-printable-encoded TEXT string (RFC 2045) as UTF-8."
  (sb-ext:octets-to-string (quoted-printable-decode-octets text) :external-format :utf-8))

(defparameter +inbox-base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The standard base64 alphabet used to decode base64 MIME part bodies.")

(defun decode-base64-octets (text)
  "Returns a (VECTOR (UNSIGNED-BYTE 8)) of raw octets decoded from base64-encoded TEXT.
Whitespace, padding ('=') and any other characters outside the base64 alphabet are ignored."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (bits 0)
        (bit-count 0))
    (loop for ch across text
          for value = (position ch +inbox-base64-alphabet+)
          when value
            do (setf bits (logior (ash bits 6) value))
               (incf bit-count 6)
               (when (>= bit-count 8)
                 (decf bit-count 8)
                 (vector-push-extend (ldb (byte 8 bit-count) bits) octets)))
    octets))

(defun decode-mime-base64 (text)
  "Pure function decoding base64-encoded MIME TEXT as UTF-8."
  (sb-ext:octets-to-string (decode-base64-octets text) :external-format :utf-8))

(defun decode-mime-part-body (body-text transfer-encoding)
  "Returns BODY-TEXT decoded according to TRANSFER-ENCODING (\"quoted-printable\", \"base64\", or passthrough)."
  (cond
    ((string= transfer-encoding "quoted-printable") (decode-quoted-printable body-text))
    ((string= transfer-encoding "base64") (decode-mime-base64 body-text))
    (t body-text)))

;;; ---------------------------------------------------------------------
;;; MIME part selection: prefer text/plain, fall back to stripped text/html (pure)
;;; ---------------------------------------------------------------------

(defun split-mime-multipart-body (body-text boundary)
  "Returns the list of raw part strings (each one part's headers followed by its body) found in
BODY-TEXT delimited by multipart BOUNDARY. Preamble text before the first delimiter and epilogue
text after the closing delimiter are discarded."
  (let* ((open-marker (format nil "--~A" boundary))
         (close-marker (format nil "--~A--" boundary))
         (lines (cl-ppcre:split "\\r?\\n" body-text))
         (parts nil)
         (current nil)
         (in-part-p nil))
    (flet ((flush-current ()
             (when in-part-p
               (push (format nil "~{~A~^~%~}" (nreverse current)) parts))
             (setf current nil)))
      (dolist (raw-line lines)
        (let ((line (string-right-trim '(#\Space #\Tab) raw-line)))
          (cond
            ((string= line close-marker)
             (flush-current)
             (setf in-part-p nil))
            ((string= line open-marker)
             (flush-current)
             (setf in-part-p t))
            (in-part-p
             (push raw-line current))
            (t nil)))))
    (nreverse parts)))

(defparameter +html-entity-replacements+
  '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
    ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'"))
  "Minimal HTML entity decode table applied after HTML tag stripping.")

(defun strip-html-tags (html)
  "Pure function returning HTML with script/style block contents, all remaining tags, and common
entities removed, leaving plain readable text. Used when an email offers only a text/html body."
  (let* ((no-scripts (cl-ppcre:regex-replace-all "(?is)<script.*?</script>" html ""))
         (no-styles (cl-ppcre:regex-replace-all "(?is)<style.*?</style>" no-scripts ""))
         (no-tags (cl-ppcre:regex-replace-all "(?s)<[^>]*>" no-styles " "))
         (decoded (reduce (lambda (text entity-pair)
                            (cl-ppcre:regex-replace-all
                             (cl-ppcre:quote-meta-chars (car entity-pair)) text (cdr entity-pair)))
                          +html-entity-replacements+
                          :initial-value no-tags)))
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (cl-ppcre:regex-replace-all "[ \\t]+" decoded " "))))

(defun mime-part-text-and-type (part-text)
  "Returns (VALUES DECODED-BODY MEDIA-TYPE BOUNDARY) for one raw MIME PART-TEXT (headers+body)."
  (multiple-value-bind (headers-text body-text) (split-mime-headers-and-body part-text)
    (let ((headers (parse-mime-headers headers-text)))
      (multiple-value-bind (media-type boundary)
          (parse-mime-content-type (mime-header-value headers "content-type"))
        (values (decode-mime-part-body body-text (mime-transfer-encoding headers))
                media-type
                boundary)))))

(defun select-preferred-mime-part-list (part-texts)
  "Returns the best available plain-text body found among sibling MIME PART-TEXTS.
Recursively descends into nested multipart parts (e.g. multipart/alternative nested inside
multipart/mixed), and strictly prefers any text/plain part over a text/html part."
  (let ((plain-text nil)
        (html-text nil))
    (dolist (part-text part-texts)
      (multiple-value-bind (decoded-body media-type nested-boundary) (mime-part-text-and-type part-text)
        (cond
          ((and nested-boundary (alexandria:starts-with-subseq "multipart/" media-type))
           (let ((nested-text (select-preferred-mime-part-list
                                (split-mime-multipart-body decoded-body nested-boundary))))
             (when (and nested-text (null plain-text))
               (setf plain-text nested-text))))
          ((string= media-type "text/plain")
           (unless plain-text (setf plain-text decoded-body)))
          ((string= media-type "text/html")
           (unless html-text (setf html-text decoded-body))))))
    (or plain-text
        (and html-text (strip-html-tags html-text)))))

(defun extract-preferred-email-body (headers body-text)
  "Returns the best-effort plain-text body extracted from one top-level MIME message given its
top-level HEADERS alist and raw BODY-TEXT. Always prioritizes text/plain content over text/html;
when only HTML is available, all HTML markup is aggressively stripped first."
  (multiple-value-bind (media-type boundary)
      (parse-mime-content-type (mime-header-value headers "content-type"))
    (cond
      ((and boundary (alexandria:starts-with-subseq "multipart/" media-type))
       (or (select-preferred-mime-part-list (split-mime-multipart-body body-text boundary)) ""))
      ((string= media-type "text/html")
       (strip-html-tags (decode-mime-part-body body-text (mime-transfer-encoding headers))))
      (t
       (decode-mime-part-body body-text (mime-transfer-encoding headers))))))

;;; ---------------------------------------------------------------------
;;; Prompt-injection sanitization and length limitation (pure)
;;; ---------------------------------------------------------------------

(defun sanitize-prompt-injection-tags (text)
  "Pure function aggressively neutralizing any XML/HTML-like pseudo-tags (e.g. <system>,
</instruction>, <script>) that could be used to inject instructions into a downstream LLM.
Any substring matching '<' ... '>' is removed entirely, including across newlines."
  (cl-ppcre:regex-replace-all "(?s)<[^>]*>" text ""))

(defun truncate-for-security (text &optional (max-length +inbox-email-max-body-length+))
  "Returns TEXT truncated to MAX-LENGTH characters, with +INBOX-EMAIL-TRUNCATION-MARKER+ appended
when truncation occurred. Guards against context-bombing via oversized email bodies."
  (if (> (length text) max-length)
      (format nil "~A~%~A" (subseq text 0 max-length) +inbox-email-truncation-marker+)
      text))

;;; ---------------------------------------------------------------------
;;; Sender whitelist enforcement / quarantine (pure address check, imperative logging)
;;; ---------------------------------------------------------------------

(defun extract-email-address (header-value)
  "Returns the bare email address extracted from a From:-style HEADER-VALUE such as
\"Display Name <addr@example.com>\" or a bare \"addr@example.com\", lowercased for comparison.
Returns NIL when HEADER-VALUE is NIL."
  (when header-value
    (string-downcase
     (or (cl-ppcre:register-groups-bind (addr) ("<\\s*([^<>\\s]+)\\s*>" header-value) addr)
         (string-trim '(#\Space #\Tab) header-value)))))

(defun email-sender-whitelisted-p (from-header-value &optional (allowed-sender +default-inbox-allowed-sender+))
  "Returns true when FROM-HEADER-VALUE resolves to exactly ALLOWED-SENDER (case-insensitive)."
  (let ((address (extract-email-address from-header-value)))
    (and address (string= address (string-downcase allowed-sender)))))

(defun quarantine-rejected-inbox-email (from-header-value raw-mime)
  "Imperatively logs a rejected inbox email (failing sender whitelist enforcement) to the quarantine
log, without ever forwarding its body to the LLM context. Always returns NIL."
  (log-message :warn "Quarantined inbox email from non-whitelisted sender"
               :context (list (cons "from" (or from-header-value "Unknown"))
                              (cons "raw-length" (length raw-mime))))
  nil)

;;; ---------------------------------------------------------------------
;;; Top-level security gateway
;;; ---------------------------------------------------------------------

(defun parse-raw-ses-email (raw-mime &key (allowed-sender +default-inbox-allowed-sender+))
  "Security gateway for one raw SES/MIME email: strictly enforces ALLOWED-SENDER (discarding and
quarantine-logging anything else without inspecting its body), prefers text/plain over text/html,
aggressively sanitizes pseudo-tag prompt-injection attempts, and truncates the result to bound
context size. Returns a (:FROM :SUBJECT :BODY) plist for one accepted email, or NIL when the
email is rejected/quarantined."
  (multiple-value-bind (headers-text body-text) (split-mime-headers-and-body raw-mime)
    (let* ((headers (parse-mime-headers headers-text))
           (from (mime-header-value headers "from"))
           (subject (or (mime-header-value headers "subject") "No Subject")))
      (if (not (email-sender-whitelisted-p from allowed-sender))
          (quarantine-rejected-inbox-email from raw-mime)
          (let* ((bounded-body (subseq body-text 0 (min (length body-text) +inbox-email-max-raw-body-length+)))
                 (extracted-text (extract-preferred-email-body headers bounded-body))
                 (sanitized-text (sanitize-prompt-injection-tags extracted-text))
                 (final-text (truncate-for-security
                              (string-trim '(#\Space #\Tab #\Newline #\Return) sanitized-text))))
            (list :from from :subject subject :body final-text))))))

(defun make-inbox-notification-message (parsed-email)
  "Returns one synthetic system-role conversation message for PARSED-EMAIL."
  (list (cons "role" "system")
        (cons "content"
              (format nil "INBOUND EMAIL NOTIFICATION~%From: ~A~%Subject: ~A~%~%~A"
                      (getf parsed-email :from)
                      (getf parsed-email :subject)
                      (getf parsed-email :body)))))

(defun inject-inbox-messages (messages parsed-emails)
  "Pure function returning MESSAGES with one synthetic notification message appended per entry in PARSED-EMAILS."
  (append messages (mapcar #'make-inbox-notification-message parsed-emails)))

(defun poll-and-inject-persona-inbox (bot messages)
  "Returns MESSAGES with any pending S3 inbox emails for BOT folded in as synthetic system messages.
Emails failing the sender whitelist are quarantined (logged and discarded) rather than injected.
Does nothing (returns MESSAGES unchanged) when BOT has no configured inbox-s3-path."
  (let ((s3-path (chatbot-inbox-s3-path bot)))
    (if s3-path
        (let ((parsed-emails (remove nil (mapcar #'parse-raw-ses-email (fetch-and-clear-inbox s3-path)))))
          (if parsed-emails
              (inject-inbox-messages messages parsed-emails)
              messages))
        messages)))

(defun maybe-poll-persona-inbox (conversation)
  "Mutates CONVERSATION in place, folding in any pending inbox emails for its chatbot.
Intended to run once per chat turn, immediately before dispatching the turn to a backend."
  (let ((bot (conversation-chatbot conversation)))
    (when (chatbot-inbox-s3-path bot)
      (setf (conversation-messages conversation)
            (poll-and-inject-persona-inbox bot (conversation-messages conversation)))))
  conversation)
