(in-package :cl-bloom-filter)

;;; Class Definition

(defclass bloom-filter ()
  ((expected-number-of-elements
    :initarg :expected-number-of-elements
    :accessor expected-number-of-elements)
   (expected-fp-rate
    :initarg :expected-fp-rate
    :accessor expected-fp-rate)
   (hash-function
    :initarg :hash-function
    :accessor hash-function)
   (hash-set
    :accessor hash-set)
   (number-of-hash-functions
    :accessor number-of-hash-functions)
   (number-of-elements
    :initform 0
    :accessor number-of-elements)))

(defmethod initialize-instance :after ((bloom-filter bloom-filter) &key)
  (let* ((n (expected-number-of-elements bloom-filter))
         (fp (expected-fp-rate bloom-filter))
         (m (%optimal-number-of-bits n fp))
         (k (%optimal-number-of-hash-functions n m)))
    (setf (slot-value bloom-filter 'number-of-hash-functions) k)
    (setf (slot-value bloom-filter 'hash-set) (make-array m :element-type 'bit))))

(defmethod add :after ((bloom-filter bloom-filter) item)
  (incf (slot-value bloom-filter 'number-of-elements)))

(defun make-bloom-filter (&key (hash-function 'sxhash) (expected-fp-rate 1/100) (expected-number-of-elements 10000))
  (check-type hash-function symbol)
  (check-type expected-fp-rate number)
  (check-type expected-number-of-elements integer)
  (assert (< 0 expected-fp-rate 1.0))
  (assert (> expected-number-of-elements 0))
  (make-instance 'bloom-filter :hash-function hash-function
                               :expected-fp-rate expected-fp-rate
                               :expected-number-of-elements expected-number-of-elements))

;;; Facade

(defgeneric effective-fp-rate (bloom-filter) (:documentation "Effective false positive rate"))

(defgeneric add (bloom-filter item) (:documentation "Add item to the bloom filter"))

(defgeneric lookup (bloom-filter item) (:documentation "Check if item is member of the bloom filter"))

;;; Private Methods

(defmethod %size ((bloom-filter bloom-filter))
  (length (hash-set bloom-filter)))

(defmethod %set-bit ((bloom-filter bloom-filter) hashed-item)
  (setf (aref (hash-set bloom-filter) hashed-item) 1))

(defmethod %get-bit ((bloom-filter bloom-filter) hashed-item)
  (aref (hash-set bloom-filter) hashed-item))

(defmacro %do-for-each-item-digest (bloom-filter item &body body)
  `(let* ((number-of-hash-functions (number-of-hash-functions ,bloom-filter))
          (size (%size ,bloom-filter))
          (hash-function (hash-function ,bloom-filter))
          (hash1 (funcall hash-function ,item))
          (hash2 (funcall hash-function hash1)))
     (do* ((i 1 (+ i 1)))
          ((> i number-of-hash-functions) t)
       ;; using only two hash functions for performance purposes
       ;; based on Less Hashing paper: https://www.eecs.harvard.edu/~michaelm/postscripts/rsa2008.pdf
       (let ((item-digest (mod (abs (+ hash1 (* i hash2))) size)))      
         ,@body))))

;;; Public Methods

(defmethod effective-fp-rate ((bloom-filter bloom-filter))
  (let* ((k (number-of-hash-functions bloom-filter))
         (m (%size bloom-filter))
         (n (number-of-elements bloom-filter)))
    (%fp-rate n m k)))

(defmethod add ((bloom-filter bloom-filter) item)
  (%do-for-each-item-digest bloom-filter item
    (%set-bit bloom-filter item-digest)))

(defmethod lookup ((bloom-filter bloom-filter) item)
  (%do-for-each-item-digest bloom-filter item
    (if (equal 0 (%get-bit bloom-filter item-digest))
        (return))))

;;; How to Use

#+nil
(defvar abloom-filter (bf:make-bloom-filter))

#+nil
(bf:add abloom-filter 331137)

#+nil
(bf:lookup abloom-filter 331137)
