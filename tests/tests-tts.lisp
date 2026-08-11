;;; tests-tts.lisp

(in-package "CHATBOT")
(fiveam:in-suite chatbot-suite)

(fiveam:test test-texttospeech-api-key-returns-nil-when-unconfigured
  (let* ((*texttospeech-api-key* nil)
         (context (make-runtime-context
                  :getenv-function (lambda (name)
                                     (declare (ignore name))
                                     nil))))
    (call-with-runtime-context
     context
     (lambda ()
       (let ((*user-homedir-pathname-function* (lambda () #p"/non-existent-home-tts/")))
         (fiveam:is (null (texttospeech-api-key))))))))

(fiveam:test test-texttospeech-api-key-reads-from-config-file
  (let* ((*texttospeech-api-key* nil)
         (temp-dir (uiop:temporary-directory))
         (appdata-dir (merge-pathnames "tts-mock-localappdata/" temp-dir))
         (config-dir (merge-pathnames "config/googleapis/texttospeech/" appdata-dir))
         (key-file (merge-pathnames "apikey" config-dir)))
    (ensure-directories-exist config-dir)
    (with-open-file (s key-file :direction :output :if-exists :supersede)
      (write-line "  configured-tts-key " s))
    (unwind-protect
        (let ((context (make-runtime-context
                       :getenv-function (lambda (name)
                                          (if (string= name "LOCALAPPDATA")
                                              (namestring appdata-dir)
                                              nil)))))
          (call-with-runtime-context
           context
           (lambda ()
             (fiveam:is (string= "configured-tts-key" (texttospeech-api-key))))))
      (uiop:delete-directory-tree appdata-dir :validate t))))

(fiveam:test test-texttospeech-request-payload-json-pins-studio-o-voice
  (let* ((payload (cl-json:decode-json-from-string (texttospeech-request-payload-json "Hello there")))
         (voice (mcp-val :voice payload))
         (input (mcp-val :input payload))
         (audio-config (mcp-val :audio-config payload)))
    (fiveam:is (string= "Hello there" (mcp-val :text input)))
    (fiveam:is (string= "en-US-Studio-O" (mcp-val :name voice)))
    (fiveam:is (string= "en-US" (mcp-val :language-code voice)))
    (fiveam:is (string= "MP3" (mcp-val :audio-encoding audio-config)))))

(fiveam:test test-synthesize-speech-mp3-octets-decodes-audio-content
  (let ((captured-url nil)
        (captured-headers nil))
    (let ((context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (setf captured-url url)
                      (setf captured-headers (getf args :headers))
                      (values (cl-json:encode-json-to-string
                              (list (cons "audioContent" "aGVsbG8=")))
                             200)))))
      (call-with-runtime-context
       context
       (lambda ()
         (let ((octets (synthesize-speech-mp3-octets "Hello there" "mocked-tts-key")))
           (fiveam:is (string= "https://texttospeech.googleapis.com/v1/text:synthesize" captured-url))
           (fiveam:is (string= "mocked-tts-key"
                              (cdr (assoc "X-Goog-Api-Key" captured-headers :test #'string=))))
           (fiveam:is (equalp #(104 101 108 108 111) octets))))))))

(fiveam:test test-speak-chat-response-skips-when-disabled
  (let* ((play-called-p nil)
         (*texttospeech-enabled-p* nil)
         (*texttospeech-api-key* "configured-key")
         (*play-audio-file-function* (lambda (path)
                                      (declare (ignore path))
                                      (setf play-called-p t))))
    (speak-chat-response "This should not be spoken.")
    (fiveam:is (not play-called-p))))

(fiveam:test test-speak-chat-response-skips-when-no-api-key-configured
  (let* ((play-called-p nil)
         (*texttospeech-enabled-p* t)
         (*texttospeech-api-key* nil)
         (*play-audio-file-function* (lambda (path)
                                      (declare (ignore path))
                                      (setf play-called-p t)))
         (context (make-runtime-context
                  :getenv-function (lambda (name)
                                     (declare (ignore name))
                                     nil))))
    (call-with-runtime-context
     context
     (lambda ()
       (let ((*user-homedir-pathname-function* (lambda () #p"/non-existent-home-tts-skip/")))
         (speak-chat-response "This should not be spoken."))))
    (fiveam:is (not play-called-p))))

(fiveam:test test-speak-chat-response-synthesizes-and-plays-when-key-configured
  (let* ((play-called-with-path nil)
         (*texttospeech-enabled-p* t)
         (*texttospeech-api-key* "configured-key")
         (*play-audio-file-function* (lambda (path)
                                      (setf play-called-with-path path))))
    (let ((context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url args))
                      (values (cl-json:encode-json-to-string
                              (list (cons "audioContent" "aGVsbG8=")))
                             200)))))
      (call-with-runtime-context
       context
       (lambda ()
         (speak-chat-response "Speak this."))))
    (fiveam:is (not (null play-called-with-path)))
    (unwind-protect
        (fiveam:is (equalp #(104 101 108 108 111)
                          (with-open-file (stream play-called-with-path
                                                  :direction :input
                                                  :element-type '(unsigned-byte 8))
                            (let ((buffer (make-array (file-length stream)
                                                     :element-type '(unsigned-byte 8))))
                              (read-sequence buffer stream)
                              buffer))))
      (ignore-errors (delete-file play-called-with-path)))))

(fiveam:test test-speak-chat-response-swallows-synthesis-errors
  (let ((play-called-p nil)
        (*texttospeech-enabled-p* t)
        (*texttospeech-api-key* "configured-key")
        (*play-audio-file-function* (lambda (path)
                                      (declare (ignore path))
                                      (setf play-called-p t))))
    (let ((context (make-runtime-context
                    :http-post-function
                    (lambda (url &rest args)
                      (declare (ignore url args))
                      (values "{}" 500)))))
      (call-with-runtime-context
       context
       (lambda ()
         (fiveam:finishes (speak-chat-response "Speak this.")))))
    (fiveam:is (not play-called-p))))

(fiveam:test test-speak-chat-response-in-background-captures-caller-settings
  ;; A fresh SBCL thread does not inherit the caller's dynamic bindings, so
  ;; SPEAK-CHAT-RESPONSE-IN-BACKGROUND must capture the caller's active runtime
  ;; context, enablement flag, API key, and player seam and re-establish them inside
  ;; the spawned thread. This guards against silently falling back to global defaults
  ;; (e.g. a real on-disk API key) when a caller has scoped its own overrides -- and
  ;; is what lets the test suite's global *texttospeech-enabled-p* NIL default actually
  ;; take effect inside background TTS threads spawned by ordinary chat-flow tests.
  (let ((play-called-with-path nil)
        (post-called-p nil)
        (*texttospeech-enabled-p* t)
        (*texttospeech-api-key* "configured-key"))
    (let ((context (make-runtime-context
                   :http-post-function
                   (lambda (url &rest args)
                     (declare (ignore url args))
                     (setf post-called-p t)
                     (values (cl-json:encode-json-to-string
                             (list (cons "audioContent" "aGVsbG8=")))
                            200)))))
      (call-with-runtime-context
       context
       (lambda ()
         (let* ((*play-audio-file-function* (lambda (path)
                                              (setf play-called-with-path path)))
                (thread (speak-chat-response-in-background "Speak this in the background.")))
           (sb-thread:join-thread thread :timeout 5)))))
    (fiveam:is (eq t post-called-p))
    (fiveam:is (not (null play-called-with-path)))
    (when play-called-with-path
      (ignore-errors (delete-file play-called-with-path)))))

(fiveam:test test-speak-chat-response-in-background-noop-when-disabled
  (let ((*texttospeech-enabled-p* nil))
    (fiveam:is (null (speak-chat-response-in-background "Should never spawn a thread.")))))
