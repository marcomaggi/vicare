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
  (vicare expander object-spec)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expand-time type specifications\n")


;;;; type specifications

(define* (fixnum (obj fixnum?))
  obj)

(define* (exact-integer (obj exact-integer?))
  obj)

(eval-for-expand
  (set-identifier-object-spec! #'fixnum
    (make-object-spec 'fixnum #'fixnum #'fixnum?))

  (set-identifier-object-spec! #'exact-integer
    (make-object-spec 'exact-integer #'exact-integer #'exact-integer?))

  (set-identifier-object-spec! #'vector
    (make-object-spec 'vector #'vector #'vector?))

  #| end of eval-for-expand |# )


(parametrise ((check-test-name	'spec-inspection))

  #;(check
      (let ()
  	(define-syntax (get-name stx)
  	  (syntax-case stx ()
  	    ((_ ?type-id)
  	     (begin
  	       (debug-print (object-spec-name (identifier-object-spec #'?type-id))
  			    (syntax->datum #`(quote #,(object-spec-name (identifier-object-spec #'?type-id)))))
  	       #`(quote #,(object-spec-name (identifier-object-spec #'?type-id)))))
  	    ))
  	(get-name vector))
    => 'vector)

  #t)


(parametrise ((check-test-name	'type-descriptor))

  (check-for-true
   (object-spec? (type-descriptor vector)))

  (check-for-true
   (object-spec? (type-descriptor fixnum)))

  (check-for-true
   (object-spec? (type-descriptor exact-integer)))

  #t)


(parametrise ((check-test-name	'is-a))

  (check
      (values (is-a? 123 fixnum)
	      (is-a? "123" fixnum))
    => #t #f)

  (check
      (values (is-a? 123 exact-integer)
	      (is-a? (least-positive-bignum) exact-integer)
	      (is-a? "123" exact-integer))
    => #t #t #f)

  (check
      (values (is-a? '#(1 2 3) vector)
	      (is-a? "#(1 2 3)" vector))
    => #t #f)

  #t)


;;;; done

(check-report)

;;; end of file
