;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for comparison procedures
;;;Date: Tue Apr 26, 2016
;;;
;;;Abstract
;;;
;;;
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
(program (test-vicare-comparison-procedures)
  (options typed-language)
  (import (vicare)
    (vicare system comparison-procedures)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare features: built-in comparison procedures\n")


;;;; helpers

(define-syntax check-for-greater
  (syntax-rules ()
    ((_ ?expr)
     (check ?expr => +1))
    ))

(define-syntax check-for-lesser
  (syntax-rules ()
    ((_ ?expr)
     (check ?expr => -1))
    ))

(define-syntax check-for-equal
  (syntax-rules ()
    ((_ ?expr)
     (check ?expr => 0))
    ))


(parametrise ((check-test-name	'maker))

  (define compar-fx
    (make-comparison-procedure fixnum? fx=? fx<?))

  (check-for-lesser	(compar-fx -1 +1))
  (check-for-equal	(compar-fx +1 +1))
  (check-for-greater	(compar-fx +1 -1))

  (void))


(parametrise ((check-test-name	'numerics))

  ;; fixnums
  (check-for-lesser	(compar-fixnum -1	+1))
  (check-for-greater	(compar-fixnum +1	-1))
  (check-for-equal	(compar-fixnum +1	+1))
  (check-for-equal	(compar-fixnum -1	-1))
  (check-for-equal	(compar-fixnum 0	0))

  ;; bignums
  (check-for-lesser	(compar-bignum (greatest-negative-bignum)	(least-positive-bignum)))
  (check-for-greater	(compar-bignum (least-positive-bignum)		(greatest-negative-bignum)))

  ;; exact integers
  (check-for-lesser	(compar-exact-integer -1	+1))
  (check-for-greater	(compar-exact-integer +1	-1))
  (check-for-equal	(compar-exact-integer +1	+1))
  (check-for-equal	(compar-exact-integer -1	-1))
  (check-for-equal	(compar-exact-integer 0		0))

  (void))


(parametrise ((check-test-name	'textual))

  ;; chars
  (check-for-lesser	(compar-char #\A	#\B))
  (check-for-greater	(compar-char #\B	#\A))
  (check-for-equal	(compar-char #\A	#\A))

  ;; strings
  (check-for-lesser	(compar-string "A"	"B"))
  (check-for-greater	(compar-string "B"	"A"))
  (check-for-equal	(compar-string "A"	"A"))

  ;; symbols
  (check-for-lesser	(compar-symbol 'A	'B))
  (check-for-greater	(compar-symbol 'B	'A))
  (check-for-equal	(compar-symbol 'A	'A))

;;; --------------------------------------------------------------------

  (internal-body
    (define* (compar-sym {A symbol?} {B symbol?})
      (cond ((symbol<? A B)	-1)
	    ((symbol=? A B)	0)
	    (else		+1)))

    (check-for-lesser	(compar-sym 'A	'B))
    (check-for-greater	(compar-sym 'B	'A))
    (check-for-equal	(compar-sym 'A	'A))

    #| end of INTERNAL-BODY |# )

  (void))


(parametrise ((check-test-name	'misc))

  ;; booleans
  (check-for-lesser	(compar-boolean #f	#t))
  (check-for-greater	(compar-boolean #t	#f))
  (check-for-equal	(compar-boolean #t	#t))
  (check-for-equal	(compar-boolean #f	#f))

  ;; transcoders
  (internal-body
    (define A
      (make-transcoder (utf-8-codec)))
    (define B
      (make-transcoder (utf-16-codec)))
    (check-for-lesser	(compar-transcoder A B))
    (check-for-greater	(compar-transcoder B A))
    (check-for-equal	(compar-transcoder A A))
    #| end of INTERNAL-BODY |# )

  ;; pointers
  (check-for-lesser	(compar-pointer (integer->pointer 1)	(integer->pointer 2)))
  (check-for-greater	(compar-pointer (integer->pointer 2)	(integer->pointer 1)))
  (check-for-equal	(compar-pointer (integer->pointer 1)	(integer->pointer 1)))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
