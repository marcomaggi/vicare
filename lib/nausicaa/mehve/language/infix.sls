;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: infix syntax for Mehve
;;;Date: Fri Nov 29, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa mehve language infix)
  (export infix)
  (import (except (nausicaa)
		  ;; redefined by numeric predicates
		  = < > <= >=
		  zero? positive? negative? non-negative? non-positive?
		  odd? even?
		  finite? infinite? nan?

		  ;; redefined by arithmetics
		  + - * / abs
		  div div0 mod mod0
		  div-and-mod div0-and-mod0

		  ;; redefined by transcendental
		  expt sqrt cbrt square cube exp log
		  sin cos tan asin acos atan
		  sinh cosh tanh asinh acosh atanh

		  ;; redefined by parts
		  numerator denominator rationalize sign
		  floor ceiling truncate round
		  real-part imag-part magnitude angle
		  make-rectangular make-polar complex-conjugate

		  ;; redefined by input/output
		  display write

		  ;; redefined by infix
		  infix)
    (vicare language-extensions infix parser-utils)
    (vicare language-extensions infix tokens)
    (nausicaa mehve language numerics-predicates)
    (nausicaa mehve language numerics-arithmetics)
    (nausicaa mehve language numerics-parts)
    (nausicaa mehve language numerics-transcendental))


(define-syntax infix
  (let ()

    (define-syntax case-stx
      (syntax-rules (else)
	((_ ?atom ((?s ...) ?e ...) ... (else ?b ...))
	 (cond ((memv-stx ?atom (syntax ?s) ...) ?e ...) ... (else ?b ...)))))

    (define-syntax memv-stx
      (syntax-rules ()
	((_ ?atom ?stx)
	 (free-identifier=? ?atom ?stx))
	((_ ?atom ?stx ...)
	 (or (free-identifier=? ?atom ?stx) ...))))

    ;; Arithmetic operations.
    (define tok.add		(make-<lexical-token> 'ADD #'+))
    (define tok.sub		(make-<lexical-token> 'SUB #'-))
    (define tok.mul		(make-<lexical-token> 'MUL #'*))
    (define tok./		(make-<lexical-token> 'DIV #'/))
    (define tok.mod		(make-<lexical-token> 'MOD #'mod))
    (define tok.mod0		(make-<lexical-token> 'MOD #'mod0))
    (define tok.div		(make-<lexical-token> 'DIV #'div))
    (define tok.div0		(make-<lexical-token> 'DIV #'div0))
    (define tok.expt		(make-<lexical-token> 'EXPT #'expt))

    ;; Comparison operators.
    (define tok.lt		(make-<lexical-token> 'LT #'<))
    (define tok.gt		(make-<lexical-token> 'GT #'>))
    (define tok.le		(make-<lexical-token> 'LE #'<=))
    (define tok.ge		(make-<lexical-token> 'GE #'>=))
    (define tok.eq		(make-<lexical-token> 'EQ #'=))

    ;; (define tok.incr!	(make-<lexical-token> 'INCR	(cons #'pre-incr! #'post-incr!)))
    ;; (define tok.decr!	(make-<lexical-token> 'DECR	(cons #'pre-decr! #'post-decr!)))

    (define (atom->token atom kont)
      (case-stx atom
	((+)		tok.add)
	((-)		tok.sub)
	((*)		tok.mul)
	((/)		tok./)
	((mod)		tok.mod)
	((mod0)		tok.mod0)
	((expt)		tok.expt)
	((div)		tok.div)
	((div0)		tok.div0)
	((<)		tok.lt)
	((>)		tok.gt)
	((<=)		tok.le)
	((>=)		tok.ge)
	((=)		tok.eq)
	;; ((incr!)	tok.incr!)
	;; ((decr!)	tok.decr!)
	(else		(kont atom))))

    (make-infix-transformer atom->token #'begin)))


;;;; done

)

;;; end of file
