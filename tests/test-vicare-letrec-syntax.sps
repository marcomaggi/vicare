;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for LETREC and LETREC* syntaxes
;;;Date: Wed Oct 31, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare checks)
  (ikarus system $compiler))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare LETREC and LETREC* syntaxes\n")


;;;; helpers

(define-syntax check-syntax-violation
  (syntax-rules ()
    ((_ ?expected-result ?body)
     (check
	 (guard (E
		 ((syntax-violation? E)
;;;		    (check-pretty-print E)
		  (syntax-violation-subform E))
		 (else E))
	   (eval (quote ?body)
		 (environment '(vicare))))
       => (quote ?expected-result)))))


(parametrise ((check-test-name		'basic)
	      ($current-letrec-pass	'basic))

  (check-syntax-violation ciao_0
    (let ()
      (define b (ciao))
      (define (ciao)
	123)
      #t))

  (check-syntax-violation 123
    (let ()
      (define (ciao)
	123)
      (define b (ciao))
      b))

  (check-syntax-violation 123
    (letrec* ((ciao (lambda (x)
		      (when x
			(ciao))
		      123))
	      (b    (ciao #f)))
      b))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'check-syntax-violation 'scheme-indent-function 1)
;; End:
