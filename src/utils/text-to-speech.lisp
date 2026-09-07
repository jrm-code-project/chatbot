;;; -*- Lisp -*-
;;; text-to-speech.lisp - post-turn text-to-speech synthesis and playback

(in-package "CHATBOT")

(defparameter *texttospeech-enabled-p* t
  "Master kill-switch for post-turn Text-to-Speech synthesis and playback.
When NIL, SPEAK-CHAT-RESPONSE returns immediately without deriving an API key, probing the
filesystem, or making any network call. The test suite binds this to NIL so tests never make
a real call to texttospeech.googleapis.com, regardless of any API key discoverable on disk.")

(defparameter *texttospeech-voice-name* "en-US-Journey-O"
  "The Google Cloud Text-to-Speech voice used for chat response playback.")

(defparameter *texttospeech-voice-language-code* "en-US"
  "The BCP-47 language code paired with *texttospeech-voice-name*.")

(defparameter *texttospeech-audio-encoding* "MP3"
  "The requested Text-to-Speech output audio encoding.")

(defun texttospeech-request-url ()
  "Returns the Text-to-Speech synthesize REST endpoint."
  (concatenate 'string *texttospeech-base-url* "/text:synthesize"))

(defun texttospeech-request-headers (api-key)
  "Returns the Text-to-Speech request headers for API-KEY."
  (list (cons "X-Goog-Api-Key" api-key)
        (cons "Content-Type" "application/json")))

(defun texttospeech-request-payload-json (text)
  "Returns the encoded Text-to-Speech synthesize request body for TEXT,
pinned to *texttospeech-voice-name*/*texttospeech-voice-language-code*."
  (cl-json:encode-json-to-string
   (list (cons "input" (list (cons "text" text)))
         (cons "voice" (list (cons "languageCode" *texttospeech-voice-language-code*)
                             (cons "name" *texttospeech-voice-name*)))
         (cons "audioConfig" (list (cons "audioEncoding" *texttospeech-audio-encoding*))))))

(defun synthesize-speech-mp3-octets (text api-key)
  "Synthesizes TEXT to speech via the Text-to-Speech API and returns the raw MP3 octets."
  (multiple-value-bind (response-body status)
      (post-web-request (texttospeech-request-url)
                        (texttospeech-request-headers api-key)
                        (texttospeech-request-payload-json text))
    (unless (eql status 200)
      (error "Text-to-Speech API responded with HTTP status ~A" status))
    (let* ((response-alist (cl-json:decode-json-from-string response-body))
           (audio-content (cdr (assoc :audio-content response-alist))))
      (unless (and audio-content (stringp audio-content) (string/= audio-content ""))
        (error "Text-to-Speech API response did not include audioContent."))
      (decode-base64-octets audio-content))))

(defun texttospeech-temp-mp3-path ()
  "Returns a fresh, unique temporary .mp3 pathname."
  (merge-pathnames (format nil "chatbot-tts-~A-~A.mp3"
                          (get-universal-time)
                          (random #xFFFFFF))
                   (uiop:default-temporary-directory)))

(defun write-mp3-to-temp-file (octets)
  "Writes OCTETS to a new temporary .mp3 file and returns its pathname."
  (let ((path (texttospeech-temp-mp3-path)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (write-sequence octets stream))
    path))

(defun default-play-audio-file (path)
  "Invokes the OS default multimedia player on PATH."
  (let ((namestring (namestring path)))
    (if (uiop:os-windows-p)
        (uiop:run-program (format nil "start \"\" \"~A\"" namestring)
                          :force-shell t
                          :output nil
                          :error-output nil
                          :ignore-error-status t)
        (uiop:run-program (list "xdg-open" namestring)
                          :output nil
                          :error-output nil
                          :ignore-error-status t))))

(defparameter *play-audio-file-function* #'default-play-audio-file
  "Function used to play a synthesized speech file. Seam for tests.")

(defun play-audio-file (path)
  "Plays the audio file at PATH using the configured player function."
  (funcall *play-audio-file-function* path))

(defun synthesize-speech-mp3-octets-with-timing (text api-key)
  "Synthesizes TEXT via API-KEY, returning (VALUES OCTETS ELAPSED-SECONDS) and info-logging the duration."
  (let* ((start-time (get-internal-real-time))
         (octets (synthesize-speech-mp3-octets text api-key))
         (end-time (get-internal-real-time))
         (elapsed-seconds (/ (- end-time start-time) (float internal-time-units-per-second))))
    (log-message :info "Text-to-speech synthesis completed"
               :context `(("elapsed-seconds" . ,(format nil "~,3F" elapsed-seconds))))
    (values octets elapsed-seconds)))

(defun speak-chat-response (text)
  "Synthesizes TEXT with the en-US-Journey-O voice and plays it back, when a Text-to-Speech
API key is configured. Logs and skips silently when *TEXTTOSPEECH-ENABLED-P* is NIL or no key
is found; synthesis or playback failures are logged and swallowed so they never interrupt the
surrounding chat turn."
  (handler-case
      (cond
        ((not *texttospeech-enabled-p*)
         (log-message :info "Skipping text-to-speech playback: text-to-speech is disabled."))
        (t
         (let ((api-key (texttospeech-api-key)))
           (cond
             ((not (and api-key (string/= api-key "")))
              (log-message :info "Skipping text-to-speech playback: no Text-to-Speech API key configured."))
             ((not (and (stringp text)
                       (string/= (string-trim '(#\Space #\Tab #\Newline #\Return) text) "")))
              nil)
             (t
              (let* ((octets (synthesize-speech-mp3-octets-with-timing text api-key))
                     (path (write-mp3-to-temp-file octets)))
                (play-audio-file path)))))))
    (error (e)
      (log-message :warn "Text-to-speech playback failed"
                 :context `(("error" . ,(princ-to-string e)))))))

(defun speak-chat-response-in-background (text)
  "Starts a supervised background thread that synthesizes and plays TEXT, without blocking the caller.
Captures the calling thread's active runtime context, Text-to-Speech enablement flag, API key, and
player seam before spawning, since a fresh SBCL thread does not inherit the caller's dynamic bindings
-- without this capture, the background thread would silently fall back to global defaults (e.g. a
real API key discovered on disk) instead of honoring the caller's (or a test's) scoped overrides."
  (unless *texttospeech-enabled-p*
    (log-message :info "Skipping text-to-speech playback: text-to-speech is disabled.")
    (return-from speak-chat-response-in-background nil))
  (let* ((captured-context (resolve-runtime-context nil))
         (captured-enabled-p *texttospeech-enabled-p*)
         (captured-api-key *texttospeech-api-key*)
         (captured-play-function *play-audio-file-function*)
         (thread (sb-thread:make-thread
                 (lambda ()
                   (call-with-runtime-context
                    captured-context
                    (lambda ()
                      (let ((*texttospeech-enabled-p* captured-enabled-p)
                            (*texttospeech-api-key* captured-api-key)
                            (*play-audio-file-function* captured-play-function))
                        (speak-chat-response text)))))
                 :name "chatbot-tts-playback")))
    (register-supervised-thread (current-resource-supervisor) thread)
    thread))
