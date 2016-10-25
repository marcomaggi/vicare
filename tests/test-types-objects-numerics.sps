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
    (prefix (vicare expander) xp::)
    (only (vicare expander)
	  type-annotation=?
	  type-annotation-super-and-sub?
	  type-annotation-common-ancestor
	  type-annotation-ancestors
	  type-annotation-syntax
	  type-annotation-matching
	  type-signature-super-and-sub?
	  type-signature-common-ancestor
	  type-signature-matching
	  type-signature-union)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for numerics types\n")


;;;; helpers

(define-constant EVAL-ENVIRONMENT
  (environment '(vicare)))

(define (%type-signature->sexp sig)
  (syntax->datum (xp::type-signature.syntax-object sig)))

(define-syntax-rule (%eval ?form)
  (eval (quote ?form)
	EVAL-ENVIRONMENT
	(expander-options typed-language)
	(compiler-options)))

(define-syntax check-expand-time-signature-violation
  (syntax-rules (=>)
    ((_ ?input-form => ?expected-signature-sexp ?returned-signature-sexp)
     (check
	 (try
	     (eval (quote ?input-form)
		   EVAL-ENVIRONMENT
		   (expander-options typed-language)
		   (compiler-options))
	   (catch E
	     ((xp::&expand-time-type-signature-violation)
	      #;(print-condition E)
	      (values (syntax->datum (xp::type-signature.syntax-object (xp::condition-expected-type-signature E)))
		      (syntax->datum (xp::type-signature.syntax-object (xp::condition-returned-type-signature E)))))
	     (else
	      (values E #f))))
       => (quote ?expected-signature-sexp) (quote ?returned-signature-sexp)))
    ))


(parametrise ((check-test-name		'bytes))

  (check-for-true	(xp::type-annotation-super-and-sub? <byte> <byte>))
  (check-for-true	(xp::type-annotation-super-and-sub? <byte> <zero-byte>))
  (check-for-true	(xp::type-annotation-super-and-sub? <byte> <positive-byte>))
  (check-for-true	(xp::type-annotation-super-and-sub? <byte> <negative-byte>))
  (check-for-true	(xp::type-annotation-super-and-sub? <byte> <non-positive-byte>))
  (check-for-true	(xp::type-annotation-super-and-sub? <byte> <non-negative-byte>))

  (check-for-true	(xp::type-annotation-super-and-sub? <fixnum>			<byte>))
  (check-for-false	(xp::type-annotation-super-and-sub? <zero-fixnum>		<byte>))
  (check-for-false	(xp::type-annotation-super-and-sub? <positive-fixnum>		<byte>))
  (check-for-false	(xp::type-annotation-super-and-sub? <negative-fixnum>		<byte>))
  (check-for-false	(xp::type-annotation-super-and-sub? <non-positive-fixnum>	<byte>))
  (check-for-false	(xp::type-annotation-super-and-sub? <non-negative-fixnum>	<byte>))

  (check-for-false	(xp::type-annotation-super-and-sub? <byte>		<fixnum>))
  (check-for-false	(xp::type-annotation-super-and-sub? <byte>		<zero-fixnum>))
  (check-for-false	(xp::type-annotation-super-and-sub? <byte>		<positive-fixnum>))
  (check-for-false	(xp::type-annotation-super-and-sub? <byte>		<negative-fixnum>))
  (check-for-false	(xp::type-annotation-super-and-sub? <byte>		<non-positive-fixnum>))
  (check-for-false	(xp::type-annotation-super-and-sub? <byte>		<non-negative-fixnum>))

;;; --------------------------------------------------------------------

  (check (xp::type-annotation-matching <byte>			<byte>)			=> 'exact-match)
  (check (xp::type-annotation-matching <byte>			<zero-byte>)		=> 'exact-match)
  (check (xp::type-annotation-matching <byte>			<positive-byte>)	=> 'exact-match)
  (check (xp::type-annotation-matching <byte>			<negative-byte>)	=> 'exact-match)
  (check (xp::type-annotation-matching <byte>			<non-positive-byte>)	=> 'exact-match)
  (check (xp::type-annotation-matching <byte>			<non-negative-byte>)	=> 'exact-match)

  (check (xp::type-annotation-matching <zero-byte>		<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <positive-byte>		<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <negative-byte>		<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <non-positive-byte>	<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <non-negative-byte>	<byte>)			=> 'possible-match)

  (check (xp::type-annotation-matching <byte>			<fixnum>)		=> 'possible-match)
  (check (xp::type-annotation-matching <byte>			<zero-fixnum>)		=> 'possible-match)
  (check (xp::type-annotation-matching <byte>			<positive-fixnum>)	=> 'possible-match)
  (check (xp::type-annotation-matching <byte>			<negative-fixnum>)	=> 'possible-match)
  (check (xp::type-annotation-matching <byte>			<non-positive-fixnum>)	=> 'possible-match)
  (check (xp::type-annotation-matching <byte>			<non-negative-fixnum>)	=> 'possible-match)

  (check (xp::type-annotation-matching <fixnum>			<byte>)			=> 'exact-match)
  (check (xp::type-annotation-matching <zero-fixnum>		<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <positive-fixnum>	<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <negative-fixnum>	<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <non-positive-fixnum>	<byte>)			=> 'possible-match)
  (check (xp::type-annotation-matching <non-negative-fixnum>	<byte>)			=> 'possible-match)

  (check (xp::type-annotation-matching <exact-integer>			<byte>)		=> 'exact-match)
  (check (xp::type-annotation-matching <positive-exact-integer>		<byte>)		=> 'possible-match)
  (check (xp::type-annotation-matching <negative-exact-integer>		<byte>)		=> 'possible-match)
  (check (xp::type-annotation-matching <non-positive-exact-integer>	<byte>)		=> 'possible-match)
  (check (xp::type-annotation-matching <non-negative-exact-integer>	<byte>)		=> 'possible-match)

  (check (xp::type-annotation-matching <byte>		<exact-integer>)		=> 'possible-match)
  (check (xp::type-annotation-matching <byte>		<positive-exact-integer>)	=> 'possible-match)
  (check (xp::type-annotation-matching <byte>		<negative-exact-integer>)	=> 'possible-match)
  (check (xp::type-annotation-matching <byte>		<non-positive-exact-integer>)	=> 'possible-match)
  (check (xp::type-annotation-matching <byte>		<non-negative-exact-integer>)	=> 'possible-match)

  (check (xp::type-annotation-matching <string>		<byte>)			=> 'no-match)
  (check (xp::type-annotation-matching <byte>		<string>)		=> 'no-match)

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? 0 <byte>))
  (check-for-true	(is-a? +10 <byte>))
  (check-for-true	(is-a? -10 <byte>))
  (check-for-true	(is-a? +127 <byte>))
  (check-for-true	(is-a? -128 <byte>))

  (check-for-false	(is-a? +128 <byte>))
  (check-for-false	(is-a? -129 <byte>))

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? (cast-signature (<top>) 0) <byte>))
  (check-for-true	(is-a? (cast-signature (<top>) +10) <byte>))
  (check-for-true	(is-a? (cast-signature (<top>) -10) <byte>))
  (check-for-true	(is-a? (cast-signature (<top>) +127) <byte>))
  (check-for-true	(is-a? (cast-signature (<top>) -128) <byte>))

  (check-for-false	(is-a? (cast-signature (<top>) +128) <byte>))
  (check-for-false	(is-a? (cast-signature (<top>) -129) <byte>))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define ({doit <byte>} {O <byte>})
	  O)

	(doit 123))
    => 123)

  (check
      (internal-body
	(define ({doit <byte>} {O <byte>})
	  (add1 O))

	(doit 123))
    => 124)

;;; --------------------------------------------------------------------
;;; expand-time type violations

  (check
      (try
	  (%eval (let (({O <byte>} "ciao"))
		   O))
	(catch E
	  ((xp::&expand-time-type-signature-violation)
	   #;(print-condition E)
	   (values (syntax->datum (xp::type-signature.syntax-object (xp::condition-expected-type-signature E)))
		   (syntax->datum (xp::type-signature.syntax-object (xp::condition-returned-type-signature E)))))
	  (else
	   (values #f E))))
    => '(<byte>) '(<nestring>))

  (check
      (try
	  (%eval (internal-body
		   (define ({doit <byte>} {O <byte>})
		     O)
		   (doit "ciao")))
	(catch E
	  ((xp::&expand-time-type-signature-violation)
	   #;(print-condition E)
	   (syntax->datum (xp::type-signature.syntax-object (xp::condition-application-operands-signature E))))
	  (else E)))
    => '(<nestring>))

;;; --------------------------------------------------------------------
;;; run-time type violations

  (check
      (try
	  (let (({O <byte>} 1000))
	    O)
	(catch E
	  ((&expression-return-value-violation)
	   (condition-irritants E))
	  (else E)))
    => '((is-a? _ <byte>) 1000))

  ;;The  value  1000  is  a  "<positive-fixnum>",  while  "<byte>"  is  derived  from
  ;;"<fixnum>".
  ;;
  (check
      (try
	  (internal-body
	    (define ({doit <byte>} {O <byte>})
	      O)
	    (doit 1000))
	(catch E
	  ((&procedure-signature-argument-violation)
	   (values (procedure-signature-argument-violation.failed-expression E)
		   (condition-irritants E)))
	  (else
	   (values #f E))))
    => '(is-a? _ <byte>) '(1000))

  (check
      (try
	  (internal-body
	    (define ({doit <byte>} {O <byte>})
	      (add1 O))
	    (doit 127))
	(catch E
	  ((&expression-return-value-violation)
	   #;(print-condition E)
	   (condition-irritants E))
	  (else E)))
    => '((is-a? _ <byte>) 128))

  (void))


(parametrise ((check-test-name		'octets))

  (check-for-true	(xp::type-annotation-super-and-sub? <octet> <octet>))
  (check-for-true	(xp::type-annotation-super-and-sub? <octet> <zero-octet>))
  (check-for-true	(xp::type-annotation-super-and-sub? <octet> <positive-octet>))

  (check-for-true	(xp::type-annotation-super-and-sub? <fixnum>			<octet>))
  (check-for-false	(xp::type-annotation-super-and-sub? <zero-fixnum>		<octet>))
  (check-for-false	(xp::type-annotation-super-and-sub? <positive-fixnum>		<octet>))

  (check-for-false	(xp::type-annotation-super-and-sub? <octet>		<fixnum>))
  (check-for-false	(xp::type-annotation-super-and-sub? <octet>		<zero-fixnum>))
  (check-for-false	(xp::type-annotation-super-and-sub? <octet>		<positive-fixnum>))

;;; --------------------------------------------------------------------

  (check (xp::type-annotation-matching <octet>			<octet>)		=> 'exact-match)
  (check (xp::type-annotation-matching <octet>			<zero-octet>)		=> 'exact-match)
  (check (xp::type-annotation-matching <octet>			<positive-octet>)	=> 'exact-match)

  (check (xp::type-annotation-matching <zero-octet>		<octet>)		=> 'possible-match)
  (check (xp::type-annotation-matching <positive-octet>		<octet>)		=> 'possible-match)

  (check (xp::type-annotation-matching <octet>			<fixnum>)		=> 'possible-match)
  (check (xp::type-annotation-matching <octet>			<zero-fixnum>)		=> 'possible-match)
  (check (xp::type-annotation-matching <octet>			<positive-fixnum>)	=> 'possible-match)

  (check (xp::type-annotation-matching <fixnum>			<octet>)		=> 'exact-match)
  (check (xp::type-annotation-matching <zero-fixnum>		<octet>)		=> 'possible-match)
  (check (xp::type-annotation-matching <positive-fixnum>	<octet>)		=> 'possible-match)
  (check (xp::type-annotation-matching <negative-fixnum>	<octet>)		=> 'no-match)
  (check (xp::type-annotation-matching <non-positive-fixnum>	<octet>)		=> 'possible-match)
  (check (xp::type-annotation-matching <non-negative-fixnum>	<octet>)		=> 'exact-match)

  (check (xp::type-annotation-matching <exact-integer>			<octet>)	=> 'exact-match)
  (check (xp::type-annotation-matching <positive-exact-integer>		<octet>)	=> 'possible-match)
  (check (xp::type-annotation-matching <negative-exact-integer>		<octet>)	=> 'no-match)
  (check (xp::type-annotation-matching <non-positive-exact-integer>	<octet>)	=> 'possible-match)
  (check (xp::type-annotation-matching <non-negative-exact-integer>	<octet>)	=> 'exact-match)

  (check (xp::type-annotation-matching <octet>		<exact-integer>)		=> 'possible-match)
  (check (xp::type-annotation-matching <octet>		<positive-exact-integer>)	=> 'possible-match)
  (check (xp::type-annotation-matching <octet>		<negative-exact-integer>)	=> 'no-match)
  (check (xp::type-annotation-matching <octet>		<non-positive-exact-integer>)	=> 'possible-match)
  (check (xp::type-annotation-matching <octet>		<non-negative-exact-integer>)	=> 'possible-match)

  (check (xp::type-annotation-matching <string>		<octet>)			=> 'no-match)
  (check (xp::type-annotation-matching <octet>		<string>)			=> 'no-match)

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? 0 <octet>))
  (check-for-true	(is-a? +10 <octet>))
  (check-for-true	(is-a? +255 <octet>))

  (check-for-false	(is-a? +256 <octet>))
  (check-for-false	(is-a? -1 <octet>))

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? (cast-signature (<top>) 0) <octet>))
  (check-for-true	(is-a? (cast-signature (<top>) +10) <octet>))
  (check-for-true	(is-a? (cast-signature (<top>) +255) <octet>))

  (check-for-false	(is-a? (cast-signature (<top>) +256) <octet>))
  (check-for-false	(is-a? (cast-signature (<top>) -1) <octet>))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define ({doit <octet>} {O <octet>})
	  O)

	(doit 123))
    => 123)

  (check
      (internal-body
	(define ({doit <octet>} {O <octet>})
	  (add1 O))

	(doit 123))
    => 124)

;;; --------------------------------------------------------------------

  ;;The  value  1000  is  a  "<positive-fixnum>",  while  "<byte>"  is  derived  from
  ;;"<fixnum>".
  ;;
  (check
      (try
	  (internal-body
	    (define ({doit <octet>} {O <octet>})
	      O)
	    (doit 1000))
	(catch E
	  ((&procedure-signature-argument-violation)
	   (values (procedure-signature-argument-violation.failed-expression E)
		   (condition-irritants E)))
	  (else
	   (values #f E))))
    => '(is-a? _ <octet>) '(1000))

  (check
      (try
	  (internal-body
	    (define ({doit <octet>} {O <octet>})
	      (add1 O))
	    (doit 255))
	(catch E
	  ((&expression-return-value-violation)
	   #;(print-condition E)
	   (condition-irritants E))
	  (else E)))
    => '((is-a? _ <octet>) 256))

  (void))


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
  (check-for-false	(is-a? +0.0				<positive>))
  (check-for-false	(is-a? -0.0				<positive>))
  (check-for-false	(is-a? -1				<positive>))
  (check-for-false	(is-a? (greatest-negative-bignum)	<positive>))
  (check-for-false	(is-a? -1/2				<positive>))
  (check-for-false	(is-a? -1.2				<positive>))

  (check-for-true	(is-a? +1				<positive>))
  (check-for-true	(is-a? (least-positive-bignum)		<positive>))
  (check-for-true	(is-a? +1/2				<positive>))
  (check-for-true	(is-a? +1.2				<positive>))

  (check-for-false	(is-a? 0				<negative>))
  (check-for-false	(is-a? +0.0				<negative>))
  (check-for-false	(is-a? -0.0				<negative>))
  (check-for-false	(is-a? +1				<negative>))
  (check-for-false	(is-a? (least-positive-bignum)		<negative>))
  (check-for-false	(is-a? +1/2				<negative>))
  (check-for-false	(is-a? +1.2				<negative>))

  (check-for-true	(is-a? -1				<negative>))
  (check-for-true	(is-a? (greatest-negative-bignum)	<negative>))
  (check-for-true	(is-a? -1/2				<negative>))
  (check-for-true	(is-a? -1.2				<negative>))

  (check-for-true	(is-a? 0				<non-positive>))
  (check-for-true	(is-a? +0.0				<non-positive>))
  (check-for-true	(is-a? -0.0				<non-positive>))
  (check-for-true	(is-a? -1				<non-positive>))
  (check-for-true	(is-a? (greatest-negative-bignum)	<non-positive>))
  (check-for-true	(is-a? -1/2				<non-positive>))
  (check-for-true	(is-a? -1.2				<non-positive>))

  (check-for-false	(is-a? +1				<non-positive>))
  (check-for-false	(is-a? (least-positive-bignum)		<non-positive>))
  (check-for-false	(is-a? +1/2				<non-positive>))
  (check-for-false	(is-a? +1.2				<non-positive>))

  (check-for-true	(is-a? 0				<non-negative>))
  (check-for-true	(is-a? +0.0				<non-negative>))
  (check-for-true	(is-a? -0.0				<non-negative>))
  (check-for-true	(is-a? +1				<non-negative>))
  (check-for-true	(is-a? (least-positive-bignum)		<non-negative>))
  (check-for-true	(is-a? +1/2				<non-negative>))
  (check-for-true	(is-a? +1.2				<non-negative>))

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

    (check-for-false		(body "+0.0" <positive>))
    (check-for-false		(body "+0.0" <negative>))
    (check-for-false		(body "-0.0" <negative>))
    (check-for-false		(body "-0.0" <positive>))

    (check-for-true		(body "+1/2" <positive>))
    (check-for-false		(body "+1/2" <negative>))
    (check-for-true		(body "-1/2" <negative>))
    (check-for-false		(body "-1/2" <positive>))

    (check-for-true		(body "0"	<non-positive>))
    (check-for-true		(body "+0.0"	<non-positive>))
    (check-for-true		(body "-0.0"	<non-positive>))
    (check-for-false		(body "+1"	<non-positive>))
    (check-for-false		(body "+1.0"	<non-positive>))
    (check-for-false		(body "+1/2"	<non-positive>))
    (check-for-true		(body "-1"	<non-positive>))
    (check-for-true		(body "-1.0"	<non-positive>))
    (check-for-true		(body "-1/2"	<non-positive>))

    (check-for-true		(body "0"	<non-negative>))
    (check-for-true		(body "+0.0"	<non-negative>))
    (check-for-true		(body "-0.0"	<non-negative>))
    (check-for-true		(body "+1"	<non-negative>))
    (check-for-true		(body "+1.0"	<non-negative>))
    (check-for-true		(body "+1/2"	<non-negative>))
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
