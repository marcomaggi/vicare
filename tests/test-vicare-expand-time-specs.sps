;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for the expander, type specifications
;;;Date: Fri Feb  7, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (for (prefix (vicare expander object-type-specs) typ.)
    expand)
  (vicare expander tags)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expand-time type specifications\n")


(parametrise ((check-test-name	'spec-inspection))

  (check
      (let ()
  	(define-syntax (get-name stx)
  	  (syntax-case stx ()
  	    ((_ ?type-id)
	     (begin
	       #`(quote #,(typ.object-type-spec-type-id (typ.identifier-object-type-spec #'?type-id)))))
  	    ))
  	(get-name <vector>))
    => '<vector>)

  #t)


(parametrise ((check-test-name	'type-descriptor))

  (check-for-true
   (typ.object-type-spec? (type-descriptor <vector>)))

  (check-for-true
   (typ.object-type-spec? (type-descriptor <fixnum>)))

  (check-for-true
   (typ.object-type-spec? (type-descriptor <exact-integer>)))

  #t)


(parametrise ((check-test-name	'is-a))

  (check
      (values (is-a? 123 <fixnum>)
	      (is-a? "123" <fixnum>))
    => #t #f)

  (check
      (values (is-a? 123 <exact-integer>)
	      (is-a? (least-positive-bignum) <exact-integer>)
	      (is-a? "123" <exact-integer>))
    => #t #t #f)

  (check
      (values (is-a? '#(1 2 3) <vector>)
	      (is-a? "#(1 2 3)" <vector>))
    => #t #f)

  #t)


(parametrise ((check-test-name	'slots))

  (check
      (values (slot-ref '(1 . 2) car <pair>)
	      (slot-ref '(1 . 2) cdr <pair>))
    => 1 2)

  (check
      (let ((P (cons 1 2)))
  	(slot-set! P car <pair> 10)
  	(slot-set! P cdr <pair> 20)
  	(values (slot-ref P car <pair>)
  		(slot-ref P cdr <pair>)))
    => 10 20)

;;; --------------------------------------------------------------------

  (check
      (values ((slot-ref <> car <pair>) '(1 . 2))
	      ((slot-ref <> cdr <pair>) '(1 . 2)))
    => 1 2)

  (check
      (let ((P (cons 1 2)))
	((slot-set! <> car <pair> <>) P 10)
	((slot-set! <> cdr <pair> <>) P 20)
	(values ((slot-ref <> car <pair>) P)
		((slot-ref <> cdr <pair>) P)))
    => 10 20)

  #t)


;;;; done

(check-report)

;;; end of file
