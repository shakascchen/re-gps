#|
  This file is a part of re-gps project.
|#

(defsystem "re-gps"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util2")
                 (:file "op2")
                 (:file "domains1")
                 (:file "domains2")
                 (:file "debug")
                 (:file "gps2")
                 (:file "re-gps"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "re-gps-test"))))
