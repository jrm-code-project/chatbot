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
    (:file "runtime-compatibility" :pathname "core/runtime-compatibility" :depends-on ("package" "vars"))
    (:file "json-utils"         :pathname "utils/json-utils" :depends-on ("package"))
     (:file "logging"            :pathname "utils/logging" :depends-on ("package" "vars"))
     (:file "http-utils"         :pathname "utils/http-utils" :depends-on ("package" "vars" "logging"))
     (:file "text-utils"         :pathname "utils/text-utils" :depends-on ("package" "json-utils"))
     (:file "builtin-tools"      :pathname "mcp/builtin-tools" :depends-on ("package" "data"))
     (:file "mcp-dispatch"       :pathname "mcp/mcp-dispatch" :depends-on ("package" "vars" "data" "json-utils" "builtin-tools"))
     (:file "tool-registry"      :pathname "mcp/tool-registry" :depends-on ("package" "vars" "data" "json-utils" "builtin-tools" "mcp-dispatch"))
     (:file "mcp"                :pathname "mcp/mcp" :depends-on ("package" "vars" "data" "json-utils" "builtin-tools" "tool-registry"))
     (:file "mcp-lifecycle"      :pathname "mcp/mcp-lifecycle" :depends-on ("package" "vars" "data" "mcp"))
     (:file "mcp-config"         :pathname "mcp/mcp-config" :depends-on ("package" "vars" "data" "json-utils" "mcp-lifecycle"))
     (:file "mcp-startup"        :pathname "mcp/mcp-startup" :depends-on ("package" "vars" "data" "mcp-config"))
     (:file "builtin-dispatch"   :pathname "mcp/builtin-dispatch" :depends-on ("package" "mcp"))
     (:file "tool-arguments"     :pathname "mcp/tool-arguments" :depends-on ("package" "mcp"))
     (:file "filesystem-tools"   :pathname "mcp/filesystem-tools" :depends-on ("package" "vars" "data" "json-utils" "mcp" "tool-arguments"))
     (:file "chatbot-state-tools" :pathname "mcp/chatbot-state-tools" :depends-on ("package" "data" "json-utils" "mcp" "tool-arguments"))
     (:file "eval-grounding-tools" :pathname "mcp/eval-grounding-tools" :depends-on ("package" "vars" "json-utils" "mcp" "tool-arguments"))
     (:file "git-tools"          :pathname "mcp/git-tools" :depends-on ("package" "data" "mcp"))
     (:file "planner-minion-tools" :pathname "mcp/planner-minion-tools" :depends-on ("package" "vars" "data" "json-utils" "mcp" "tool-arguments"))
     (:file "agentic-loops"      :pathname "orchestration/agentic-loops" :depends-on ("package" "vars" "data" "logging"))
     (:file "agentic-loop-tools" :pathname "mcp/agentic-loop-tools" :depends-on ("package" "vars" "data" "mcp" "tool-arguments" "agentic-loops"))
     (:file "builtin-registrations" :pathname "mcp/builtin-registrations" :depends-on ("package" "vars" "data" "json-utils" "builtin-dispatch" "mcp" "tool-arguments" "filesystem-tools" "chatbot-state-tools" "eval-grounding-tools" "git-tools" "planner-minion-tools" "agentic-loops" "agentic-loop-tools"))
     (:file "tool-execution"     :pathname "mcp/tool-execution" :depends-on ("package" "vars" "data" "json-utils" "builtin-tools" "builtin-dispatch" "mcp-dispatch" "tool-registry" "mcp" "tool-arguments" "filesystem-tools" "chatbot-state-tools" "eval-grounding-tools" "git-tools" "planner-minion-tools" "agentic-loops" "agentic-loop-tools" "builtin-registrations"))
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
     (:file "conversations"      :pathname "core/conversations" :depends-on ("package" "vars" "data" "json-utils" "personas" "mcp" "request-history"))
     (:file "turn-runner"        :pathname "backends/turn-runner" :depends-on ("package" "vars" "data" "request-history" "tool-execution"))
     (:file "backend-gemini"     :pathname "backends/backend-gemini" :depends-on ("package" "vars" "data" "logging" "http-utils" "text-utils" "payloads" "gemini-payloads" "mcp" "tool-execution" "turn-runner"))
     (:file "backend-openai"     :pathname "backends/backend-openai" :depends-on ("package" "vars" "data" "http-utils" "text-utils" "payloads" "openai-payloads" "mcp" "tool-execution" "turn-runner"))
     (:file "backend-google"     :pathname "backends/backend-google" :depends-on ("package" "vars" "data" "logging" "http-utils" "payloads" "google-payloads" "mcp" "tool-execution" "turn-runner"))
     (:file "chat"               :pathname "core/chat" :depends-on ("package" "vars" "data" "attachments" "conversations" "backend-gemini" "backend-openai" "backend-google"))
     (:file "test-runner"        :pathname "core/test-runner" :depends-on ("package"))
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
     (:file "tests-mcp"         :depends-on ("tests"))
     (:file "tests-agentic-loops" :depends-on ("tests"))))))
