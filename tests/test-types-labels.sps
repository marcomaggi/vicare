;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: demo for DEFINE-LABEL parser
;;;Date: Mon Apr 25, 2016
;;;
;;;Abstract
;;;
;;;	This file demonstrates how to use the facilities of the expander to implement
;;;	a parser  for the syntax  DEFINE-LABEL.  An adapted  version of this  code is
;;;	also included in the expander.
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (demo-define-label)
  (options typed-language)
  (import (vicare)
    (vicare language-extensions labels)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** demo Vicare: DEFINE-LABEL parser\n")


;;;; helpers

(define-type <all-fixnums>
  (or <non-negative-fixnum> <negative-fixnum>))

;; (debug-print
;;  (type-annotation-super-and-sub? <all-fixnums> <positive-fixnum>))


(parametrise ((check-test-name	'basic))

  (check
      (internal-body
	(define-label <my-fixnum>
	  (parent <all-fixnums>))
	(values (is-a? 123 <my-fixnum>)
		(is-a? "ciao" <my-fixnum>)))
    => #t #f)

  (internal-body
    (define-label <comparison-fixnum>
      (parent <all-fixnums>)
      (type-predicate
	(lambda (parent-pred)
	  (lambda ({_ <boolean>} obj)
	    (and (parent-pred obj)
		 (fx<=? obj +1)
		 (fx>=? obj -1)))))
      (method ({foo <string>} {O <comparison-fixnum>})
	(number->string O)))

    (check-for-true	(type-annotation-super-and-sub? <fixnum> <comparison-fixnum>))
    (check-for-false	(type-annotation-super-and-sub? <comparison-fixnum> <fixnum>))

    (check-for-false	(type-annotation-super-and-sub? <positive-fixnum> <comparison-fixnum>))

    (check-for-true	(is-a? +1 <comparison-fixnum>))
    (check-for-true	(is-a?  0 <comparison-fixnum>))
    (check-for-true	(is-a? -1 <comparison-fixnum>))
    (check-for-false	(is-a? +2 <comparison-fixnum>))
    (check-for-false	(is-a? -2 <comparison-fixnum>))

    (check-for-true	(is-a? (unsafe-cast-signature (<comparison-fixnum>) 0) <comparison-fixnum>))

    (check-for-true	(is-a? (unsafe-cast-signature (<top>) +1) <comparison-fixnum>))
    (check-for-true	(is-a? (unsafe-cast-signature (<top>)  0) <comparison-fixnum>))
    (check-for-true	(is-a? (unsafe-cast-signature (<top>) -1) <comparison-fixnum>))
    (check-for-false	(is-a? (unsafe-cast-signature (<top>) +2) <comparison-fixnum>))
    (check-for-false	(is-a? (unsafe-cast-signature (<top>) -2) <comparison-fixnum>))

    (void))

  (void))


(parametrise ((check-test-name	'methods))

  (check
      (internal-body
	(define-label <my-fixnum>
	  (parent <all-fixnums>)
	  (method (doit {O <my-fixnum>})
	    999))
	(define {O <my-fixnum>}
	  123)
	(values (.hash O) (.doit O)))
    => 123 999)

  (internal-body
    (define-label <comparison-fixnum>
      (parent <all-fixnums>)
      (type-predicate
	(lambda (parent-pred)
	  (lambda ({_ <boolean>} obj)
	    (and (parent-pred obj)
		 (fx<=? obj +1)
		 (fx>=? obj -1)))))
      (method ({foo <string>} {O <comparison-fixnum>})
	(number->string O)))

    (check
	(let (({O <comparison-fixnum>} 1))
	  (.foo O))
      => "1")

    (check
	(.foo (unsafe-cast-signature (<comparison-fixnum>) +1))
      => "1")

    (void))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
