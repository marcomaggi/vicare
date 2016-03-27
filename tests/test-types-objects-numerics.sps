;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics types
;;;Date: Sun Mar 27, 2016
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
(program (test-types-numerics-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for numerics types\n")


(parametrise ((check-test-name		'exactness))

  (check-for-true	(is-a? 123				<exact>))
  (check-for-true	(is-a? (least-positive-bignum)		<exact>))
  (check-for-true	(is-a? (greatest-negative-bignum)	<exact>))
  (check-for-true	(is-a? 1/2				<exact>))
  (check-for-true	(is-a? 1+2i				<exact>))
  (check-for-false	(is-a? 1.2				<exact>))
  (check-for-false	(is-a? 1.2+3.4i				<exact>))
  (check-for-false	(is-a? 1.2+3i				<exact>))
  (check-for-false	(is-a? 1+3.4i				<exact>))

  (check-for-false	(is-a? 123				<inexact>))
  (check-for-false	(is-a? (least-positive-bignum)		<inexact>))
  (check-for-false	(is-a? (greatest-negative-bignum)	<inexact>))
  (check-for-false	(is-a? 1/2				<inexact>))
  (check-for-false	(is-a? 1+2i				<inexact>))
  (check-for-true	(is-a? 1.2				<inexact>))
  (check-for-true	(is-a? 1.2+3.4i				<inexact>))
  (check-for-true	(is-a? 1.2+3i				<inexact>))
  (check-for-true	(is-a? 1+3.4i				<inexact>))

  (void))


(parametrise ((check-test-name		'sign))

  (check-for-false	(is-a? 0				<positive>))
  (check-for-true	(is-a? +1				<positive>))
  (check-for-true	(is-a? (least-positive-bignum)		<positive>))
  (check-for-true	(is-a? +1/2				<positive>))
  (check-for-true	(is-a? +1.2				<positive>))
  (check-for-true	(is-a? +0.0				<positive>))

  (check-for-false	(is-a? 0				<negative>))
  (check-for-false	(is-a? +1				<negative>))
  (check-for-false	(is-a? (least-positive-bignum)		<negative>))
  (check-for-false	(is-a? +1/2				<negative>))
  (check-for-false	(is-a? +1.2				<negative>))
  (check-for-false	(is-a? +0.0				<negative>))

  (check-for-true	(is-a? -1				<negative>))
  (check-for-true	(is-a? (greatest-negative-bignum)	<negative>))
  (check-for-true	(is-a? -1/2				<negative>))
  (check-for-true	(is-a? -1.2				<negative>))
  (check-for-true	(is-a? -0.0				<negative>))

  (check-for-false	(is-a? -1				<positive>))
  (check-for-false	(is-a? (greatest-negative-bignum)	<positive>))
  (check-for-false	(is-a? -1/2				<positive>))
  (check-for-false	(is-a? -1.2				<positive>))
  (check-for-false	(is-a? -0.0				<positive>))

  (check-for-true	(is-a? 0				<non-negative>))
  (check-for-true	(is-a? +1				<non-negative>))
  (check-for-true	(is-a? (least-positive-bignum)		<non-negative>))
  (check-for-true	(is-a? +1/2				<non-negative>))
  (check-for-true	(is-a? +1.2				<non-negative>))
  (check-for-true	(is-a? +0.0				<non-negative>))

  (check-for-false	(is-a? -0.0				<non-negative>))
  (check-for-false	(is-a? -1				<non-negative>))
  (check-for-false	(is-a? (greatest-negative-bignum)	<non-negative>))
  (check-for-false	(is-a? -1/2				<non-negative>))
  (check-for-false	(is-a? -1.2				<non-negative>))

;;; --------------------------------------------------------------------
;;; run-time predicate

  (let-syntax ((body (syntax-rules ()
		       ((_ ?string ?type)
			(let ((port (open-string-input-port ?string)))
			  (is-a? (read port) ?type)))
		       )))

    (check-for-true		(body "+1" <positive>))
    (check-for-false		(body "+1" <negative>))
    (check-for-true		(body "-1" <negative>))
    (check-for-false		(body "-1" <positive>))

    (check-for-true		(body (number->string (least-positive-bignum))		<positive>))
    (check-for-false		(body (number->string (least-positive-bignum))		<negative>))
    (check-for-true		(body (number->string (greatest-negative-bignum))	<negative>))
    (check-for-false		(body (number->string (greatest-negative-bignum))	<positive>))

    (check-for-true		(body "+1.0" <positive>))
    (check-for-false		(body "+1.0" <negative>))
    (check-for-true		(body "-1.0" <negative>))
    (check-for-false		(body "-1.0" <positive>))

    (check-for-true		(body "+0.0" <positive>))
    (check-for-false		(body "+0.0" <negative>))
    (check-for-true		(body "-0.0" <negative>))
    (check-for-false		(body "-0.0" <positive>))

    (check-for-true		(body "+1/2" <positive>))
    (check-for-false		(body "+1/2" <negative>))
    (check-for-true		(body "-1/2" <negative>))
    (check-for-false		(body "-1/2" <positive>))

    (check-for-true		(body "+0.0"	<non-negative>))
    (check-for-true		(body "+1"	<non-negative>))
    (check-for-true		(body "+1.0"	<non-negative>))
    (check-for-true		(body "+1/2"	<non-negative>))
    (check-for-true		(body "0"	<non-negative>))
    (check-for-false		(body "-0.0"	<non-negative>))
    (check-for-false		(body "-1"	<non-negative>))
    (check-for-false		(body "-1.0"	<non-negative>))
    (check-for-false		(body "-1/2"	<non-negative>))

    #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? 0	<zero>))
  (check-for-true	(is-a? +0.0	<zero>))
  (check-for-true	(is-a? -0.0	<zero>))
  (check-for-false	(is-a? 1	<zero>))
  (check-for-false	(is-a? 1.1	<zero>))
  (check-for-false	(is-a? 1/2	<zero>))
  (check-for-false	(is-a? 1+2i	<zero>))
  (check-for-false	(is-a? 1.0+2.0i	<zero>))

  (let-syntax ((body (syntax-rules ()
		       ((_ ?string)
			(let ((port (open-string-input-port ?string)))
			  (is-a? (read port) <zero>)))
		       )))

    (check-for-true	(body "0"))
    (check-for-true	(body "+0.0"))
    (check-for-true	(body "-0.0"))
    (check-for-false	(body "1"))
    (check-for-false	(body "1.1"))
    (check-for-false	(body "1/2"))
    (check-for-false	(body "1+2i"))
    (check-for-false	(body "1.0+2.0i"))

    #| end of LET-SYNTAX |# )

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
