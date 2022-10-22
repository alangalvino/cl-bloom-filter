# cl-bloom-filter
![run-tests workflow](https://github.com/alangalvino/cl-bloom-filter/workflows/.github/workflows/run-tests.yml/badge.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)

Just another Common Lisp bloom filter implementation, enjoy it!

## Usage

Basic usage:

```lisp
(defvar abloom-filter (bf:make-bloom-filter))

;; adding an item
(bf:add abloom-filter 42)

;; checking if an item is member
(bf:lookup abloom-filter 42)

;; asking the current false positie rate
(bf:effective-fp-rate abloom-filter)
```

Using other hash function:

```lisp
;; for this to work you must add cl-murmurhash as a dependency
(defvar abloom-filter (bf:make-bloom-filter :hash-function 'murmurhash))
```

Creating a bloom-filter with specific values:

```lisp
(defvar abloom-filter (bf:make-bloom-filter :hash-function 'sxhash
                                            :expected-number-of-elements 100
                                            :expected-fp-rate 1/100))
```
