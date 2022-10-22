(defpackage cl-bloom-filter
  (:nicknames :bf)
  (:use :cl)
  (:export
   ;; bloom filter private functions (exporting only for unit testing)
   :%optimal-number-of-bits
   :%optimal-number-of-hash-functions
   :%fp-rate
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
