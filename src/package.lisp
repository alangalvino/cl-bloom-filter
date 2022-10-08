(defpackage cl-bloom-filter
  (:nicknames :bf)
  (:use :cl)
  (:export
   :make-bloom-filter
   :effective-fp-rate
   :add
   :lookup))
