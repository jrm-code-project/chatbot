;;;; chatbot.asd

(defsystem "chatbot"
  :description "A chatbot framework for building conversational agents"
  :author "Joe Marshall <eval.apply@gmail.com>"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-json"
               "cl-ppcre"
               "dexador"
               "fold"
               "function"
               "google"
               "jsonx"
               "named-let"
               "series"
               "str"
               "trivial-timeout"
               "uiop")
  :in-order-to ((test-op (test-op "chatbot/tests")))
  :components
  ((:module "infrastructure"
    :pathname ""
    :components
    ((:file "package")
     (:file "data"            :depends-on ("package"))
     (:file "vars"            :depends-on ("package" "data"))
     (:file "json-utils"      :depends-on ("package"))
     (:file "logging"         :depends-on ("package" "vars"))
     (:file "http-utils"      :depends-on ("package" "vars" "logging"))
     (:file "text-utils"      :depends-on ("package" "json-utils"))
     (:file "mcp"             :depends-on ("package" "vars" "data" "json-utils"))
     (:file "attachment-paths" :depends-on ("package" "vars"))
     (:file "attachment-mime" :depends-on ("package"))
     (:file "prompt-decoration" :depends-on ("package" "data"))
     (:file "request-history" :depends-on ("package" "data" "prompt-decoration"))
     (:file "attachments"     :depends-on ("package" "attachment-paths" "attachment-mime"))
     (:file "payloads"        :depends-on ("package" "data" "json-utils" "mcp" "attachments" "prompt-decoration" "request-history"))
     (:file "openai-payloads" :depends-on ("package" "data" "mcp" "attachments" "prompt-decoration" "request-history" "payloads"))
     (:file "google-payloads" :depends-on ("package" "data" "mcp" "attachments" "prompt-decoration" "request-history" "payloads"))
     (:file "gemini-payloads" :depends-on ("package" "data" "mcp" "attachments" "prompt-decoration" "request-history" "payloads"))
     (:file "personas"        :depends-on ("package" "logging"))
     (:file "conversations"   :depends-on ("package" "vars" "data" "json-utils" "personas" "mcp"))
     (:file "backend-gemini"  :depends-on ("package" "vars" "data" "logging" "http-utils" "text-utils" "payloads" "gemini-payloads" "mcp"))
     (:file "backend-openai"  :depends-on ("package" "vars" "data" "http-utils" "text-utils" "payloads" "openai-payloads" "mcp"))
     (:file "backend-google"  :depends-on ("package" "vars" "data" "logging" "http-utils" "payloads" "google-payloads" "mcp"))
     (:file "chat"            :depends-on ("package" "vars" "data" "attachments" "backend-gemini" "backend-openai" "backend-google"))
     (:file "screenshot-tools" :depends-on ("package" "attachment-paths" "chat"))))))

(defsystem "chatbot/tests"
  :description "FiveAM test suite for the Chatbot framework"
  :depends-on ("chatbot" "fiveam")
  :perform (test-op (operation component)
            (declare (ignore operation component))
            (unless (funcall (find-symbol "RUN-ALL-TESTS" "CHATBOT"))
              (error "Chatbot test suite failed.")))
  :components
  ((:module "infrastructure"
    :pathname ""
    :components
    ((:file "tests")
     (:file "tests-payloads"  :depends-on ("tests"))
     (:file "tests-runtime"   :depends-on ("tests"))
     (:file "tests-openai"    :depends-on ("tests"))
     (:file "tests-google"    :depends-on ("tests"))
     (:file "tests-personas"  :depends-on ("tests"))
     (:file "tests-mcp"       :depends-on ("tests"))))))
