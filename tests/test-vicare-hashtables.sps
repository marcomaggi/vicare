;;;
;;;Part of: vicare scheme
;;;Contents: tests for hashtables
;;;Date: Thu Mar 12, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: hashtables\n")


(parametrise ((check-test-name	'hash-functions))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?fun ?obj)
       (check
	   (non-negative-exact-integer? (?fun ?obj))
	 => #t))
      ))

;;; --------------------------------------------------------------------

  (doit string-hash "ciao")

  (doit string-ci-hash "Ciao")

;;; --------------------------------------------------------------------

  (doit symbol-hash 'ciao)

;;; --------------------------------------------------------------------

  (doit bytevector-hash '#vu8(1 2 3 4))

;;; --------------------------------------------------------------------

  (doit fixnum-hash 123)
  (doit fixnum-hash (greatest-fixnum))
  (doit fixnum-hash (least-fixnum))

;;; --------------------------------------------------------------------

  (doit exact-integer-hash 123)
  (doit exact-integer-hash (greatest-fixnum))
  (doit exact-integer-hash (least-fixnum))
  (doit exact-integer-hash (greatest-negative-bignum))
  (doit exact-integer-hash (least-positive-bignum))

;;; --------------------------------------------------------------------

  (doit flonum-hash +0.0)
  (doit flonum-hash -0.0)
  (doit flonum-hash +1.0)
  (doit flonum-hash -1.0)
  (doit flonum-hash +1.2)
  (doit flonum-hash -1.2)
  (doit flonum-hash +inf.0)
  (doit flonum-hash -inf.0)
  (doit flonum-hash -nan.0)

;;; --------------------------------------------------------------------

  (doit number-hash 123)
  (doit number-hash (greatest-fixnum))
  (doit number-hash (least-fixnum))
  (doit number-hash (greatest-negative-bignum))
  (doit number-hash (least-positive-bignum))
  (doit number-hash +0.0)
  (doit number-hash -0.0)
  (doit number-hash +1.0)
  (doit number-hash -1.0)
  (doit number-hash +1.2)
  (doit number-hash -1.2)
  (doit number-hash +inf.0)
  (doit number-hash -inf.0)
  (doit number-hash -nan.0)
  (doit number-hash +1/2)
  (doit number-hash -1/2)
  (doit number-hash +1+2i)
  (doit number-hash +1-2i)
  (doit number-hash +1.0+2i)
  (doit number-hash +1-2.0i)
  (doit number-hash +1.0+2.0i)
  (doit number-hash -1.0-2.0i)

;;; --------------------------------------------------------------------

  (doit char-hash #\a)

  (doit char-ci-hash #\a)

;;; --------------------------------------------------------------------

  (doit boolean-hash #t)
  (doit boolean-hash #f)

;;; --------------------------------------------------------------------

  (doit void-hash (void))
  (doit eof-object-hash (eof-object))
  (doit would-block-hash (would-block-object))

;;; --------------------------------------------------------------------

  (internal-body
    (define-struct a-struct
      (a b c))
    (doit struct-hash (make-a-struct 1 2 3)))

;;; --------------------------------------------------------------------

  (internal-body
    (define-record-type a-record
      (fields a b c))
    (doit record-hash (make-a-record 1 2 3)))

;;; --------------------------------------------------------------------

  (doit object-hash "ciao")
  (doit object-hash "Ciao")
  (doit object-hash 'ciao)
  (doit object-hash '#vu8(1 2 3 4))
  (doit object-hash #t)
  (doit object-hash #f)
  (doit object-hash 123)
  (doit object-hash (greatest-fixnum))
  (doit object-hash (least-fixnum))
  (doit object-hash (greatest-negative-bignum))
  (doit object-hash (least-positive-bignum))
  (doit object-hash +0.0)
  (doit object-hash -0.0)
  (doit object-hash +1.0)
  (doit object-hash -1.0)
  (doit object-hash +1.2)
  (doit object-hash -1.2)
  (doit object-hash +inf.0)
  (doit object-hash -inf.0)
  (doit object-hash -nan.0)
  (doit object-hash +1/2)
  (doit object-hash -1/2)
  (doit object-hash +1+2i)
  (doit object-hash +1-2i)
  (doit object-hash +1.0+2i)
  (doit object-hash +1-2.0i)
  (doit object-hash +1.0+2.0i)
  (doit object-hash -1.0-2.0i)
  (doit object-hash #\a)
  (doit object-hash (void))
  (doit object-hash (eof-object))
  (doit object-hash (would-block-object))
  (internal-body
    (define-struct a-struct
      (a b c))
    (doit object-hash (make-a-struct 1 2 3)))
  (internal-body
    (define-record-type a-record
      (fields a b c))
    (doit object-hash (make-a-record 1 2 3)))

;;; --------------------------------------------------------------------

  (doit equal-hash "ciao")
  (doit equal-hash "Ciao")
  (doit equal-hash 'ciao)
  (doit equal-hash '#vu8(1 2 3 4))
  (doit equal-hash #t)
  (doit equal-hash #f)
  (doit equal-hash 123)
  (doit equal-hash (greatest-fixnum))
  (doit equal-hash (least-fixnum))
  (doit equal-hash (greatest-negative-bignum))
  (doit equal-hash (least-positive-bignum))
  (doit equal-hash +0.0)
  (doit equal-hash -0.0)
  (doit equal-hash +1.0)
  (doit equal-hash -1.0)
  (doit equal-hash +1.2)
  (doit equal-hash -1.2)
  (doit equal-hash +inf.0)
  (doit equal-hash -inf.0)
  (doit equal-hash -nan.0)
  (doit equal-hash +1/2)
  (doit equal-hash -1/2)
  (doit equal-hash +1+2i)
  (doit equal-hash +1-2i)
  (doit equal-hash +1.0+2i)
  (doit equal-hash +1-2.0i)
  (doit equal-hash +1.0+2.0i)
  (doit equal-hash -1.0-2.0i)
  (doit equal-hash #\a)
  (doit equal-hash (void))
  (doit equal-hash (eof-object))
  (doit equal-hash (would-block-object))
  (internal-body
    (define-struct a-struct
      (a b c))
    (doit equal-hash (make-a-struct 1 2 3)))
  (internal-body
    (define-record-type a-record
      (fields a b c))
    (doit equal-hash (make-a-record 1 2 3)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
