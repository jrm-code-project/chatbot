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
  ((:module "src"
    :pathname "src/"
    :components
    ((:file "package"            :pathname "core/package")
     (:file "data"               :pathname "core/data" :depends-on ("package"))
     (:file "vars"               :pathname "core/vars" :depends-on ("package" "data"))
     (:file "json-utils"         :pathname "utils/json-utils" :depends-on ("package"))
     (:file "logging"            :pathname "utils/logging" :depends-on ("package" "vars"))
     (:file "http-utils"         :pathname "utils/http-utils" :depends-on ("package" "vars" "logging"))
     (:file "text-utils"         :pathname "utils/text-utils" :depends-on ("package" "json-utils"))
     (:file "builtin-tools"      :pathname "mcp/builtin-tools" :depends-on ("package" "data"))
     (:file "tool-registry"      :pathname "mcp/tool-registry" :depends-on ("package" "vars" "data" "json-utils" "builtin-tools"))
     (:file "mcp"                :pathname "mcp/mcp" :depends-on ("package" "vars" "data" "json-utils" "builtin-tools" "tool-registry"))
     (:file "attachment-paths"   :pathname "attachments/attachment-paths" :depends-on ("package" "vars"))
     (:file "attachment-mime"    :pathname "attachments/attachment-mime" :depends-on ("package"))
     (:file "prompt-decoration"  :pathname "prompts/prompt-decoration" :depends-on ("package" "data"))
     (:file "request-history"    :pathname "prompts/request-history" :depends-on ("package" "data" "prompt-decoration"))
     (:file "attachments"        :pathname "attachments/attachments" :depends-on ("package" "attachment-paths" "attachment-mime"))
     (:file "payloads"           :pathname "payloads/payloads" :depends-on ("package" "data" "json-utils" "mcp" "attachments" "prompt-decoration" "request-history"))
     (:file "openai-payloads"    :pathname "payloads/openai-payloads" :depends-on ("package" "data" "mcp" "attachments" "prompt-decoration" "request-history" "payloads"))
     (:file "google-payloads"    :pathname "payloads/google-payloads" :depends-on ("package" "data" "mcp" "attachments" "prompt-decoration" "request-history" "payloads"))
     (:file "gemini-payloads"    :pathname "payloads/gemini-payloads" :depends-on ("package" "data" "mcp" "attachments" "prompt-decoration" "request-history" "payloads"))
     (:file "personas"           :pathname "personas/personas" :depends-on ("package" "logging"))
     (:file "conversations"      :pathname "core/conversations" :depends-on ("package" "vars" "data" "json-utils" "personas" "mcp"))
     (:file "backend-gemini"     :pathname "backends/backend-gemini" :depends-on ("package" "vars" "data" "logging" "http-utils" "text-utils" "payloads" "gemini-payloads" "mcp"))
     (:file "backend-openai"     :pathname "backends/backend-openai" :depends-on ("package" "vars" "data" "http-utils" "text-utils" "payloads" "openai-payloads" "mcp"))
     (:file "backend-google"     :pathname "backends/backend-google" :depends-on ("package" "vars" "data" "logging" "http-utils" "payloads" "google-payloads" "mcp"))
     (:file "chat"               :pathname "core/chat" :depends-on ("package" "vars" "data" "attachments" "backend-gemini" "backend-openai" "backend-google"))
     (:file "round-robin"        :pathname "orchestration/round-robin" :depends-on ("package" "data" "attachments" "prompt-decoration" "text-utils" "chat"))
     (:file "sandbox-personas"   :pathname "orchestration/sandbox-personas" :depends-on ("package" "data" "conversations" "prompt-decoration" "text-utils" "chat"))
     (:file "screenshot-tools"   :pathname "tools/screenshot-tools" :depends-on ("package" "attachment-paths" "chat"))
     (:file "news-tools"         :pathname "tools/news-tools" :depends-on ("package" "chat"))))))

(defsystem "chatbot/tests"
  :description "FiveAM test suite for the Chatbot framework"
  :depends-on ("chatbot" "fiveam")
  :perform (test-op (operation component)
            (declare (ignore operation component))
            (unless (funcall (find-symbol "RUN-ALL-TESTS" "CHATBOT"))
              (error "Chatbot test suite failed.")))
  :components
  ((:module "tests"
    :pathname "tests/"
    :components
    ((:file "tests")
     (:file "tests-payloads"    :depends-on ("tests"))
     (:file "tests-runtime"     :depends-on ("tests"))
     (:file "tests-round-robin" :depends-on ("tests"))
     (:file "tests-sandbox"     :depends-on ("tests"))
     (:file "tests-openai"      :depends-on ("tests"))
     (:file "tests-google"      :depends-on ("tests"))
     (:file "tests-personas"    :depends-on ("tests"))
     (:file "tests-mcp"         :depends-on ("tests"))))))
