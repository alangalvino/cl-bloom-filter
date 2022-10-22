(in-package :cl-bloom-filter)

;;; Bloom Filter Helper Functions

;; m  -> number of bits
;; n  -> number of elements
;; k -> number of hash functions
;; fp -> false positive rate
;;
;; All equations based on https://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives

;; optimal(m) = - (n * ln(e)) / (ln(2)) ^ 2
(defun %optimal-number-of-bits (n fp)
  (ceiling (- (/ (* n (log fp)) (* (log 2) (log 2))))))

;; optimal(k) = (m / n) * ln(2)
(defun %optimal-number-of-hash-functions (n m)
  (max 1 (round (* (/ m n) (log 2)))))

;; fp = (1 - (e ^ (- (k * n) / m))^k 
(defun %fp-rate (n m k)
  (expt (- 1 (expt (exp 1) (- (/ (* k n) m)))) k))

