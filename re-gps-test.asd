#|
  This file is a part of re-gps project.
|#

(defsystem "re-gps-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("re-gps"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "re-gps"))))
  :description "Test system for re-gps"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
