(defpackage cl-bloom-filter
  (:use :cl))
(in-package :cl-bloom-filter)

;;; Bloom Filter Helper Functions

;; m  -> number of bits
;; n  -> expected number of insertions
;; fp -> false positive rate
;;
;; Based on https://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives
;; optimal(m) = -(n*ln(e))/(ln(2))^2
;;
(defun %optimal-number-of-bits (n fp)
  (ceiling (- (/ (* n (log fp)) (* (log 2) (log 2))))))

;; n -> expected number of insertions 
;; m -> number of bits
;; k -> number of hash functions
;;
;; Based on https://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives
;; optimal(k) = (m/n)*ln(2)
;;
(defun %optimal-number-of-hash-functions (n m)
  (max 1 (round (* (/ m n) (log 2)))))

;;; Bloom Filter Class Definition

(defclass bloom-filter ()
  ((capacity
    :initarg :capacity
    :initform 10000
    :accessor capacity)
   (fp-rate
    :initarg :fp-rate
    :initform 1/1000
    :accessor fp-rate)
   (hash-function
    :initarg :hash-function
    :initform 'sxhash
    :accessor hash-function)
   (hash-set
    :accessor hash-set)
   (number-of-hash-functions
    :accessor number-of-hash-functions)))

;; n  -> expected number of insertions 
;; m  -> number of bits
;; k  -> number of hash functions
;; fp -> false positive rate
(defmethod initialize-instance :after ((bloom-filter bloom-filter) &key)
  (let* ((n (capacity bloom-filter))
         (fp (fp-rate bloom-filter))
         (m (%optimal-number-of-bits n fp))
         (k (%optimal-number-of-hash-functions n m)))
    (setf (slot-value bloom-filter 'number-of-hash-functions) k)
    (setf (slot-value bloom-filter 'hash-set) (make-array m :element-type 'bit))))

;;; Bloom Filter Methods

(defgeneric add (bloom-filter item) (:documentation "Add item to the bloom filter"))

(defgeneric lookup (bloom-filter item) (:documentation "Check if item is member of the bloom filter"))

(defmethod %size ((bloom-filter bloom-filter))
  (length (hash-set bloom-filter)))

(defmethod %set-bit ((bloom-filter bloom-filter) hashed-item)
  (setf (aref (hash-set bloom-filter) hashed-item) 1))

(defmethod %get-bit ((bloom-filter bloom-filter) hashed-item)
  (aref (hash-set bloom-filter) hashed-item))

(defmethod add ((bloom-filter bloom-filter) item)
  (let* ((number-of-hash-functions (number-of-hash-functions bloom-filter))
         (size (%size bloom-filter))
         (hash-function (hash-function bloom-filter))
         (hash1 (funcall hash-function item))
         (hash2 (funcall hash-function hash1)))
    (do* ((i 1 (+ i 1)))
         ((> i number-of-hash-functions) t)
      ;; Based on Less Hashing paper: https://www.eecs.harvard.edu/~michaelm/postscripts/rsa2008.pdf
      (let ((hashed-item (mod (abs (+ hash1 (* i hash2))) size)))      
        (%set-bit bloom-filter hashed-item)))))

(defmethod lookup ((bloom-filter bloom-filter) item)
  (let* ((number-of-hash-functions (number-of-hash-functions bloom-filter))
         (size (%size bloom-filter))
         (hash-function (hash-function bloom-filter))
         (hash1 (funcall hash-function item))
         (hash2 (funcall hash-function hash1)))
    (do* ((i 1 (+ i 1)))
         ((> i number-of-hash-functions) t)
      ;; Based on Less Hashing paper: https://www.eecs.harvard.edu/~michaelm/postscripts/rsa2008.pdf
      (let ((hashed-item (mod (abs (+ hash1 (* i hash2))) size)))
        (if (equal 0 (%get-bit bloom-filter hashed-item))
            (return))))))

#+nil
(defvar abloom-filter (make-instance 'bloom-filter))

#+nil
(add abloom-filter 331137)

#+nil
(check abloom-filter 331137)
