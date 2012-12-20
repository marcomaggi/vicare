;;;
;;;Part of: Vicare Scheme
;;;Contents: test for general SRFI 98
;;;Date: Thu Dec 20, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright 2009 Derick Eddington.
;;;
;;;My MIT-style license  is in the file named LICENSE  from the original
;;;collection this file is distributed with.
;;;


#!r6rs
(import (rename (rnrs)
		(for-all andmap))
  (srfi :98)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 98, OS environment variables\n")


(check
    (list? (get-environment-variables))
  => #t)

(check
    (andmap (lambda (a)
	      (and (pair? a)
		   (string? (car a))
		   (positive? (string-length (car a)))
		   (string? (cdr a))))
	    (get-environment-variables))
  => #t)

(check
    (andmap (lambda (a)
	      (let ((v (get-environment-variable (car a))))
		(and (string? v)
		     (string=? v (cdr a)))))
	    (get-environment-variables))
  => #t)

;;;(assert (not (assoc "BLAH" (get-environment-variables))))

(check
    (get-environment-variable "BLAH")
  => #f)


;;;; done

(check-report)

;;; end of file
