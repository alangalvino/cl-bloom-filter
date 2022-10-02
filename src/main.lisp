(defpackage cl-bloom-filter
  (:use :cl))
(in-package :cl-bloom-filter)

;;; Bloom Filter Class Definition

(defclass bloom-filter ()
  ((hash-functions
    :initarg :hash-functions
    :initform '(sxhash)
    :accessor hash-functions)
   (size
    :initarg size
    :initform 8000
    :accessor size)
   false-positive-rate
   (hash-set
    :accessor hash-set)))

(defmethod initialize-instance :after ((bloom-filter bloom-filter) &key)
  (setf (slot-value bloom-filter 'hash-set) (make-array (size bloom-filter) :element-type 'bit)))

;;; Bloom Filter Methods

(defgeneric add (bloom-filter item) (:documentation "Add item to the bloom filter"))

(defgeneric lookup (bloom-filter item) (:documentation "Check if item is on the bloom filter"))

(defmethod %hash ((bloom-filter bloom-filter) hash-function item)
  (let ((hashed-item (funcall hash-function item)))
    (floor (mod hashed-item (size bloom-filter)))))

(defmethod %set-bit ((bloom-filter bloom-filter) hashed-item)
  (setf (aref (hash-set bloom-filter) hashed-item) 1))

(defmethod %get-bit ((bloom-filter bloom-filter) hashed-item)
  (aref (hash-set bloom-filter) hashed-item))

(defmethod add ((bloom-filter bloom-filter) (item number))
  (dolist (hash-function (hash-functions bloom-filter))
    (let ((hashed-item (%hash bloom-filter hash-function item)))
      (%set-bit bloom-filter hashed-item))))

(defmethod check ((bloom-filter bloom-filter) (item number))
  (dolist (hash-function (hash-functions bloom-filter) t)
    (let ((hashed-item (%hash bloom-filter hash-function item)))
      (if (equal 0 (%get-bit bloom-filter hashed-item))
          (return)))))

#+nil
(defvar abloom-filter (make-instance 'bloom-filter))

#+nil
(add abloom-filter 331137)

#+nil
(check abloom-filter 331137)
