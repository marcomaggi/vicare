;;;
;;;Part of: Vicare Scheme
;;;Contents: built-in comparison procedures
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
(library (ikarus.comparison-procedures)
  (export
    make-comparison-procedure

    ;; numeric comparison
    compar-fixnum
    compar-bignum
    compar-exact-integer
    compar-ratnum
    compar-exact-real
    compar-flonum
    compar-real

    ;; textual comparison
    compar-char
    compar-string
    compar-string-ci
    compar-symbol

    ;; misc comparison
    compar-boolean
    compar-transcoder
    compar-pointer

    #| end of EXPORT |# )
  (import (except (vicare)
		  make-comparison-procedure

		  ;; numeric comparison
		  compar-fixnum
		  compar-bignum
		  compar-exact-integer
		  compar-ratnum
		  compar-exact-real
		  compar-flonum
		  compar-real

		  ;; textual comparison
		  compar-char
		  compar-string
		  compar-string-ci
		  compar-symbol

		  ;; misc comparison
		  compar-boolean
		  compar-transcoder
		  compar-pointer

		  #| end of EXCEPT |# )
    (only (vicare system $transcoders)
	  $transcoder->data))


;;;; helpers

(define-syntax define-compar
  (syntax-rules ()
    ((_ ?who ?type-predicate ?equal-to ?less-than)
     (define* (?who {A ?type-predicate} {B ?type-predicate})
       (cond ((?less-than A B)	-1)
	     ((?equal-to  A B)	0)
	     (else		+1))))
    ))

(define (exact-real? obj)
  (or (exact-integer? obj)
      (ratnum?        obj)))


;;;; maker

(define* (make-comparison-procedure {type-pred procedure?} {equal-to procedure?} {less-than procedure?})
  (lambda* ({A type-pred} {B type-pred})
    (cond ((less-than    A B)	-1)
	  ((equal-to	 A B)	0)
	  (else			+1))))


;;;; numeric comparison

(define-compar compar-fixnum		fixnum?		fx=?		fx<?)
(define-compar compar-bignum		bignum?		=		<)
(define-compar compar-exact-integer	exact-integer?	=		<)
(define-compar compar-ratnum		ratnum?		=		<)
(define-compar compar-exact-real	exact-real?	=		<)
(define-compar compar-flonum		flonum?		fl=?		fl<?)
(define-compar compar-real		real?		=		<)


;;;; textual comparison

(define-compar compar-char		char?		char=?		char<?)
(define-compar compar-string		string?		string=?	string<?)
(define-compar compar-string-ci		string?		string-ci=?	string-ci<?)

(define* (compar-symbol {A symbol?} {B symbol?})
  (cond ((eq? A B)		0)
	((symbol<? A B)		-1)
	(else			+1)))


;;;; misc comparison

(define-compar compar-pointer		pointer?	pointer=?		pointer<?)
(define-compar compar-transcoder	transcoder?	transcoder=?		transcoder<?)

(define* (compar-boolean {A boolean?} {B boolean?})
  (cond ((and A B)	0)
	(A		+1)
	(B		-1)
	(else		0)))


;;;; done

;; #!vicare
;; (define end-of-file-dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.comparison-procedures end")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
