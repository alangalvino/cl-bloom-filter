(in-package :cl-bloom-filter)

;;; Bloom Filter Class Definition

(defclass bloom-filter ()
  ((capacity
    :initarg :capacity
    :initform 10000
    :accessor capacity)
   (expected-fp-rate
    :initarg :expected-fp-rate
    :initform 1/1000
    :accessor expected-fp-rate)
   (hash-function
    :initarg :hash-function
    :initform 'sxhash
    :accessor hash-function)
   (hash-set
    :accessor hash-set)
   (number-of-hash-functions
    :accessor number-of-hash-functions)
   (number-of-elements
    :initform 0
    :accessor number-of-elements)))

(defmethod initialize-instance :after ((bloom-filter bloom-filter) &key)
  (let* ((n (capacity bloom-filter))
         (fp (expected-fp-rate bloom-filter))
         (m (%optimal-number-of-bits n fp))
         (k (%optimal-number-of-hash-functions n m)))
    (setf (slot-value bloom-filter 'number-of-hash-functions) k)
    (setf (slot-value bloom-filter 'hash-set) (make-array m :element-type 'bit))))

(defmethod add :after ((bloom-filter bloom-filter) item)
  (incf (slot-value bloom-filter 'number-of-elements)))

;;; Bloom Filter Methods

(defgeneric effective-fp-rate (bloom-filter) (:documentation "Effective false positive rate"))

(defgeneric add (bloom-filter item) (:documentation "Add item to the bloom filter"))

(defgeneric lookup (bloom-filter item) (:documentation "Check if item is member of the bloom filter"))

(defmethod %size ((bloom-filter bloom-filter))
  (length (hash-set bloom-filter)))

(defmethod %set-bit ((bloom-filter bloom-filter) hashed-item)
  (setf (aref (hash-set bloom-filter) hashed-item) 1))

(defmethod %get-bit ((bloom-filter bloom-filter) hashed-item)
  (aref (hash-set bloom-filter) hashed-item))

(defmethod effective-fp-rate ((bloom-filter bloom-filter))
  (let* ((k (number-of-hash-functions bloom-filter))
         (m (%size bloom-filter))
         (n (number-of-elements bloom-filter)))
    (%fp-rate n m k)))

(defmethod add ((bloom-filter bloom-filter) item)
  (let* ((number-of-hash-functions (number-of-hash-functions bloom-filter))
         (size (%size bloom-filter))
         (hash-function (hash-function bloom-filter))
         (hash1 (funcall hash-function item))
         (hash2 (funcall hash-function hash1)))
    (do* ((i 1 (+ i 1)))
         ((> i number-of-hash-functions) t)
      ;; Using only two hash functions
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
      ;; Using only two hash functions
      ;; Based on Less Hashing paper: https://www.eecs.harvard.edu/~michaelm/postscripts/rsa2008.pdf
      (let ((hashed-item (mod (abs (+ hash1 (* i hash2))) size)))
        (if (equal 0 (%get-bit bloom-filter hashed-item))
            (return))))))

#+nil
(defvar abloom-filter (make-instance 'bloom:bloom-filter))

#+nil
(bloom:add abloom-filter 331137)

#+nil
(bloom:lookup abloom-filter 331137)
