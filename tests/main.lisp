(defpackage cl-bloom-filter/tests/main
  (:use :cl
        :cl-bloom-filter
        :rove))
(in-package :cl-bloom-filter/tests/main)

;;; NOTE: To run this test file, execute `(asdf:test-system :cl-bloom-filter)' in your Lisp.

(deftest test-make-bloom-filter
  (testing "should signal an error when hash-function is not a symbol"
    (ok (signals (bf:make-bloom-filter :hash-function (random most-positive-double-float))))
    (ok (signals (bf:make-bloom-filter :hash-function (cons 1 2))))
    (ok (signals (bf:make-bloom-filter :hash-function "a string"))))
  (testing "should signal an error when expected-fp-rate is not a float between 0 and 1"
    (ok (signals (bf:make-bloom-filter :expected-fp-rate (cons 1 2))))
    (ok (signals (bf:make-bloom-filter :expected-fp-rate "a string")))
    (ok (signals (bf:make-bloom-filter :expected-fp-rate (+ 1 (random most-positive-double-float)))))
    (ok (signals (bf:make-bloom-filter :expected-fp-rate (- (random most-positive-double-float))))))
  (testing "should signal an error when expected-number-of-elements is not a non zero positive integer"
    (ok (signals (bf:make-bloom-filter :expected-number-of-elements (cons 1 2))))
    (ok (signals (bf:make-bloom-filter :expected-number-of-elements "a string")))
    (ok (signals (bf:make-bloom-filter :expected-number-of-elements (- (random most-positive-fixnum))))))
  (testing "should set hash-function properly"
    (let* ((any-hash-function 'any-hash-function)
           (bloom-filter (bf:make-bloom-filter :hash-function any-hash-function)))
      (ok (equal (bf:hash-function bloom-filter) any-hash-function))))
  (testing "should set expected-fp-rate properly"
    (let* ((a-float-number (random 1.0))
           (bloom-filter (bf:make-bloom-filter :expected-fp-rate a-float-number)))
      (ok (equal (bf:expected-fp-rate bloom-filter) a-float-number))))
  (testing "should set expected-number-of-elements properly"
    ;; I'm keeping a small number of expected elements (one million) to not overflow the kernel heap
    (let* ((non-zero-positive-integer (+ 1 (random 1000000)))
           (bloom-filter (bf:make-bloom-filter :expected-number-of-elements non-zero-positive-integer)))
      (ok (equal (bf:expected-number-of-elements bloom-filter) non-zero-positive-integer)))))

(deftest test-effective-fp-rate-method
  (testing "should be 0.0 when bloom filter is empty"
    (let ((bloom-filter (bf:make-bloom-filter)))
      (ok (equal (bf:effective-fp-rate bloom-filter) 0.0)))))

(deftest test-add-method
  (testing "should add an item to the bloom filter"
    (let ((item "just an item")
          (bloom-filter (bf:make-bloom-filter)))
      (ok (equal (bf:add bloom-filter item) t))))
  (testing "should increment number-of-element for each add call"
    (let ((number-of-addings (+ 1 (random 10000)))
          (bloom-filter (bf:make-bloom-filter)))
      (do* ((i 1 (+ i 1)))
           ((> i number-of-addings))
        (bf:add bloom-filter i))
      (ok (equal (bf:number-of-elements bloom-filter) number-of-addings)))))

(deftest test-lookup-method
  (testing "should return t when item is present"
    (let* ((item "just an item")
           (bloom-filter (bf:make-bloom-filter)))
      (bf:add bloom-filter item)
      (ok (equal (bf:lookup bloom-filter item) t))))
  (testing "should return nil when item is not present"
    (let ((item "just an item")
          (bloom-filter (bf:make-bloom-filter)))
      (ok (equal (bf:lookup bloom-filter item) nil)))))
