(defpackage cl-bloom-filter
  (:nicknames :bf)
  (:use :cl)
  (:export
   ;; bloom filter internal methods
   :make-bloom-filter
   :hash-function
   :expected-fp-rate
   :expected-number-of-elements
   :number-of-elements
   :numbef-of-hash-functions
   ;; bloom filter public methods
   :effective-fp-rate
   :add
   :lookup))
