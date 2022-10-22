# cl-bloom-filter

Just another Common Lisp bloom filter implementation, enjoy it!

## Usage

Creating a bloom-filter with default values:

```lisp
(defvar abloom-filter (bf:make-bloom-filter))
```

Adding an item:

```lisp
(bf:add abloom-filter 42)
```

Checking if an item is member of the bloom-filter:

```lisp
(bf:lookup abloom-filter 42)
```

Asking what's the current false positive rate:

```lisp
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