;;;; chatbot.asd

(defsystem "chatbot"
  :description "A chatbot framework for building conversational agents"
  :author "Joe Marshall <eval.apply@gmail.com>"
  :license "MIT"
  :defsystem-depends-on ("fiveam")
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
  :components
  ((:module "infrastructure"
    :pathname ""
    :components
    ((:file "data"      :depends-on ("package"))
     (:file "macros"    :depends-on ("package" "vars" "data" "misc"))
     (:file "misc"      :depends-on ("package"))
     (:file "package")
     (:file "vars"      :depends-on ("package"))
     (:file "tests"     :depends-on ("package" "macros" "misc"))))))
