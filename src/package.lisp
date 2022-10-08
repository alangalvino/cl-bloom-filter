(defpackage cl-bloom-filter
  (:nicknames :bloom :bf)
  (:use :cl)
  (:export
   :make-bloom-filter
   :add
   :lookup))
