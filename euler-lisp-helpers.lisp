(in-package :euler-lisp-helpers)

;;; The Sieve of Eratosthenes code is from rosettacode.org
;;; http://rosettacode.org/wiki/Sieve_of_Eratosthenes#Common_Lisp
(defun sieve-of-eratosthenes (maximum)
  "Return a list of all prime numbers up to maximum."
  (let ((composites (make-array (1+ maximum)
				:element-type 'bit
				:initial-element 0)))
    (loop for candidate from 2 to maximum
       when (zerop (bit composites candidate))
       collect candidate
       and do (loop for composite from (expt candidate 2) to maximum by candidate
		 do (setf (bit composites composite) 1)))))

;;; The Prime Decomposition code is from rosettacode.org
;;; http://rosettacode.org/wiki/Prime_decomposition#Common_Lisp
(defun factor (n)
  "Return a list of factors of N."
  (when (> n 1)
    (loop with max-d = (isqrt n)
	  for d = 2 then (if (evenp d) (+ d 1) (+ d 2)) do
	  (cond ((> d max-d) (return (list n))) ; n is prime
		((zerop (rem n d)) (return (cons d (factor (truncate n d)))))))))

;; This can also be implemented using a sieve up to n, and then
;; checking if the last element equals n.
(defun prime-p (n)
  "Checks whether n is prime."
  (cond ((= n 1) nil)
	((= n 2) t)
	((evenp n) nil)
	(t (let ((limit (isqrt n)))
	     (do ((i 3 (incf i 2)))
		 ((> i limit) t)
	       (if (zerop (mod n i)) (return-from prime-p 'nil)))))))

(defun primes-a-b (a b)
  "Return a list of primes from a to b."
  (let ((primes (sieve-of-eratosthenes b)))
    (do ((first (car primes) (car (setf primes (cdr primes)))))
	((>= first a) primes))))

(defun make-prime-iter ()
  "Returns a function that returns the next prime each time that it's
  called."
  (let* ((max 1000000)
	 (primes (sieve-of-eratosthenes max)))
    (labels ((iter ()
	       (cond ((null primes)
		      (setf primes
			    (primes-a-b (1+ max)
					(setf max (* 2 max))))
		      (iter))
		     (t (pop primes)))))
      #'iter)))

(defun nth-prime (n)
  "Return the nth prime"
  (let ((prime-iter (make-prime-iter)))
    (dotimes (x (1- n) (funcall prime-iter))
      (funcall prime-iter))))

(defun make-dec-iter (rational)
  "Return a function that returns the next decimal place of a rational
  each time it is called. It will return the left hand side of the
  decimal on the first call. Note: The returned lhs will have no sign,
  even if the argument is negative."
  #'(lambda ()
      (multiple-value-bind (num rem) (truncate (abs rational))
	(setf rational (* 10 rem))
	num)))

(defun make-dec-string (rational &optional (digits 0))
  "Return the string representation of a rational number, with the
  specified number of decimal places."
  (with-output-to-string (s)
    (loop with iter = (make-dec-iter rational)
       for i to digits
       when (and (zerop i) (minusp rational)) do (format s "-")
       do (format s "~a" (funcall iter))
	 (when (and (zerop i) (plusp digits))
	   (format s ".")))))

(defun permutations (lst)
  (if (null lst)
      '(())
      (mapcan #'(lambda (elt)
		  (mapcar #'(lambda (perm)
			      (cons elt perm))
			  (permutations (remove elt lst :count 1))))
	      lst)))

(defun make-perm-iter (lst)
  (let ((perms (permutations lst)))
    #'(lambda ()
	(pop perms))))

(defun digits (num)
  (map 'list #'(lambda (char)
		 (parse-integer (string char)))
       (prin1-to-string num)))

(defun int-power-of-two-p (int)
  (if (zerop int)
      nil
      (zerop (logand int (1- int)))))
