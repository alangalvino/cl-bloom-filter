(defpackage cl-bloom-filter/tests/main
  (:use :cl
        :cl-bloom-filter
        :rove))
(in-package :cl-bloom-filter/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-bloom-filter)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
