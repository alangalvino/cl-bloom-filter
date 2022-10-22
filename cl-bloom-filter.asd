(defsystem "cl-bloom-filter"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "helpers")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-bloom-filter/tests"))))

(defsystem "cl-bloom-filter/tests"
  :author ""
  :license ""
  :depends-on ("cl-bloom-filter"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main")
                 (:file "helpers"))))
  :description "Test system for cl-bloom-filter"
  :perform (test-op (op c) (symbol-call :rove :run c)))
