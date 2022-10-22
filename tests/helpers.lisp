(defpackage cl-bloom-filter/tests/helpers
  (:use :cl
   :cl-bloom-filter
        :rove))
(in-package :cl-bloom-filter/tests/helpers)

(defun float-equal (a b &optional (precision 1s-6))
  (< (- a (+ b single-float-epsilon)) precision))

;; magic numbers in these tests were calculated via https://hur.st/bloomfilter

(deftest test-optimal-number-of-bits
  (testing "should be 0 when number of elements is 0"
    (ok (equal 0 (%optimal-number-of-bits 0 (random 1.0)))))
  (testing "should throw division-by-zero signal when fp rate is 0"
    (ok (signals (%optimal-number-of-bits (random 1000) 0) 'division-by-zero)))
  (testing "should be 57511 when number of elements is 4000 and fp rare is 0.001"
    (ok (equal 57511 (%optimal-number-of-bits 4000 0.001))))
  (testing "should be 76780 when number of elements is 10000 and fp rare is 0.025"
    (ok (equal 76780 (%optimal-number-of-bits 10000 0.025)))))

(deftest test-optimal-number-of-hash-functions
  (testing "should be 1 when number of bits is 0"
    (ok (equal 1 (%optimal-number-of-hash-functions (random 1000) 0))))
  (testing "should throw division-by-zero signal when number of elements is 0"
    (ok (signals (%optimal-number-of-hash-functions 0 (random 1000)) 'division-by-zero)))
  (testing "should be 10 when number of elements is 10000 and number of bits is 143776"
    (ok (equal 10 (%optimal-number-of-hash-functions 10000 143776))))
  (testing "should be 5 when number of elements is 5050 and number of bits is 38774"
    (ok (equal 5 (%optimal-number-of-hash-functions 5050 38774)))))

(deftest test-fp-rate
  (testing "should be 1.0 when number of bits is 1 and number of elements and hash functions are 0"
    (ok (equal 1.0 (%fp-rate 0 1 0))))
  (testing "should throw division-by-zero signal when number of bits is 0"
    (ok (signals (%fp-rate (random 1000) 0 (random 1000)) 'division-by-zero)))
  (testing "should be 0.014995649 when number of bits is 8742 and number of elements is 1000 and hash functions is 6"
    (ok (float-equal 0.014995649 (%fp-rate 1000 8742 6))))
  (testing "should be 0.025107341 when number of bits is 15356 and number of elements is 2000 and hash functions is 5"
    (ok (float-equal 0.025107341 (%fp-rate 2000 15356 5)))))
