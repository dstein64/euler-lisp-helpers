(in-package #:common-lisp-user)

(defpackage :euler-lisp-helpers
  (:use #:common-lisp)
  (:export #:sieve-of-eratosthenes
	   #:factor
	   #:prime-p
	   #:primes-a-b
	   #:make-prime-iter
	   #:nth-prime
	   #:make-dec-iter
	   #:make-dec-string
	   #:permutations
	   #:make-perm-iter
	   #:digits))
