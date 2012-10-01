;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for arguments validation library
;;;Date: Mon Oct  1, 2012
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
  (vicare syntactic-extensions)
  (vicare arguments validation)
  (prefix (vicare posix) px.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare arguments validation library\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax doit
  (syntax-rules ()
    ((_ ?print ?validator . ?objs)
     (catch ?print
       (let ((who 'test))
	 (with-arguments-validation (who)
	     ((?validator . ?objs))
	   #t))))))


(parametrise ((check-test-name	'config))

  (check
      (eval 'config.arguments-validation
	    (environment '(prefix (vicare installation-configuration)
				  config.)))
    => #t)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "yes" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #t)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "no" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #f)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "1" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #t)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "0" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #f)

  (px.setenv "VICARE_ARGUMENTS_VALIDATION" "yes" #t))


(parametrise ((check-test-name	'validate-fixnums))

;;; fixnum

  (check
      (doit #f fixnum 123)
    => #t)

  (check
      (doit #f fixnum 'ciao)
    => '(ciao))

;;; --------------------------------------------------------------------
;;; positive-fixnum

  (check
      (doit #f positive-fixnum 123)
    => #t)

  (check
      (doit #f positive-fixnum 'ciao)
    => '(ciao))

  (check
      (doit #f positive-fixnum 0)
    => '(0))

  (check
      (doit #f positive-fixnum -1)
    => '(-1))

;;; --------------------------------------------------------------------
;;; negative-fixnum

  (check
      (doit #f negative-fixnum -123)
    => #t)

  (check
      (doit #f negative-fixnum 'ciao)
    => '(ciao))

  (check
      (doit #f negative-fixnum 0)
    => '(0))

  (check
      (doit #f negative-fixnum +1)
    => '(+1))

;;; --------------------------------------------------------------------
;;; non-positive-fixnum

  (check
      (doit #f non-positive-fixnum -123)
    => #t)

  (check
      (doit #f non-positive-fixnum 'ciao)
    => '(ciao))

  (check
      (doit #f non-positive-fixnum 0)
    => #t)

  (check
      (doit #f non-positive-fixnum +1)
    => '(+1))

;;; --------------------------------------------------------------------
;;; non-negative-fixnum

  (check
      (doit #f non-negative-fixnum +123)
    => #t)

  (check
      (doit #f non-negative-fixnum 'ciao)
    => '(ciao))

  (check
      (doit #f non-negative-fixnum 0)
    => #t)

  (check
      (doit #f non-negative-fixnum -1)
    => '(-1))

;;; --------------------------------------------------------------------
;;; fixnum-in-inclusive-range

  (check
      (doit #f fixnum-in-inclusive-range +123 100 200)
    => #t)

  (check
      (doit #f fixnum-in-inclusive-range +100 100 200)
    => #t)

  (check
      (doit #f fixnum-in-inclusive-range +200 100 200)
    => #t)

  (check
      (doit #f fixnum-in-inclusive-range 'ciao 100 200)
    => '(ciao))

  (check
      (doit #f fixnum-in-inclusive-range 0 100 200)
    => '(0))

;;; --------------------------------------------------------------------
;;; fixnum-in-exclusive-range

  (check
      (doit #f fixnum-in-exclusive-range +123 100 200)
    => #t)

  (check
      (doit #f fixnum-in-exclusive-range +100 100 200)
    => '(100))

  (check
      (doit #f fixnum-in-exclusive-range +200 100 200)
    => '(200))

  (check
      (doit #f fixnum-in-exclusive-range 'ciao 100 200)
    => '(ciao))

  (check
      (doit #f fixnum-in-exclusive-range 0 100 200)
    => '(0))

;;; --------------------------------------------------------------------
;;; even-fixnum

  (check
      (doit #f even-fixnum 2)
    => #t)

  (check
      (doit #f even-fixnum 3)
    => '(3))

  (check
      (doit #f even-fixnum -2)
    => #t)

  (check
      (doit #f even-fixnum -3)
    => '(-3))

  (check
      (doit #f even-fixnum 'ciao)
    => '(ciao))

  (check
      (doit #f even-fixnum 0)
    => #t)

;;; --------------------------------------------------------------------
;;; odd-fixnum

  (check
      (doit #f odd-fixnum 2)
    => '(2))

  (check
      (doit #f odd-fixnum 3)
    => #t)

  (check
      (doit #f odd-fixnum -2)
    => '(-2))

  (check
      (doit #f odd-fixnum -3)
    => #t)

  (check
      (doit #f odd-fixnum 'ciao)
    => '(ciao))

  (check
      (doit #f odd-fixnum 0)
    => '(0))

  #t)


(parametrise ((check-test-name	'validate-exact-integer))

;;; exact-integer

  (check
      (doit #f exact-integer 123)
    => #t)

  (check
      (doit #f exact-integer 'ciao)
    => '(ciao))

;;; --------------------------------------------------------------------
;;; positive-exact-integer

  (check
      (doit #f positive-exact-integer 123)
    => #t)

  (check
      (doit #f positive-exact-integer 'ciao)
    => '(ciao))

  (check
      (doit #f positive-exact-integer 0)
    => '(0))

  (check
      (doit #f positive-exact-integer -1)
    => '(-1))

;;; --------------------------------------------------------------------
;;; negative-exact-integer

  (check
      (doit #f negative-exact-integer -123)
    => #t)

  (check
      (doit #f negative-exact-integer 'ciao)
    => '(ciao))

  (check
      (doit #f negative-exact-integer 0)
    => '(0))

  (check
      (doit #f negative-exact-integer +1)
    => '(+1))

;;; --------------------------------------------------------------------
;;; non-positive-exact-integer

  (check
      (doit #f non-positive-exact-integer -123)
    => #t)

  (check
      (doit #f non-positive-exact-integer 'ciao)
    => '(ciao))

  (check
      (doit #f non-positive-exact-integer 0)
    => #t)

  (check
      (doit #f non-positive-exact-integer +1)
    => '(+1))

;;; --------------------------------------------------------------------
;;; non-negative-exact-integer

  (check
      (doit #f non-negative-exact-integer +123)
    => #t)

  (check
      (doit #f non-negative-exact-integer 'ciao)
    => '(ciao))

  (check
      (doit #f non-negative-exact-integer 0)
    => #t)

  (check
      (doit #f non-negative-exact-integer -1)
    => '(-1))

;;; --------------------------------------------------------------------
;;; exact-integer-in-inclusive-range

  (check
      (doit #f exact-integer-in-inclusive-range +123 100 200)
    => #t)

  (check
      (doit #f exact-integer-in-inclusive-range +100 100 200)
    => #t)

  (check
      (doit #f exact-integer-in-inclusive-range +200 100 200)
    => #t)

  (check
      (doit #f exact-integer-in-inclusive-range 'ciao 100 200)
    => '(ciao))

  (check
      (doit #f exact-integer-in-inclusive-range 0 100 200)
    => '(0))

;;; --------------------------------------------------------------------
;;; exact-integer-in-exclusive-range

  (check
      (doit #f exact-integer-in-exclusive-range +123 100 200)
    => #t)

  (check
      (doit #f exact-integer-in-exclusive-range +100 100 200)
    => '(100))

  (check
      (doit #f exact-integer-in-exclusive-range +200 100 200)
    => '(200))

  (check
      (doit #f exact-integer-in-exclusive-range 'ciao 100 200)
    => '(ciao))

  (check
      (doit #f exact-integer-in-exclusive-range 0 100 200)
    => '(0))

;;; --------------------------------------------------------------------
;;; even-exact-integer

  (check
      (doit #f even-exact-integer 2)
    => #t)

  (check
      (doit #f even-exact-integer 3)
    => '(3))

  (check
      (doit #f even-exact-integer -2)
    => #t)

  (check
      (doit #f even-exact-integer -3)
    => '(-3))

  (check
      (doit #f even-exact-integer 'ciao)
    => '(ciao))

  (check
      (doit #f even-exact-integer 0)
    => #t)

;;; --------------------------------------------------------------------
;;; odd-exact-integer

  (check
      (doit #f odd-exact-integer 2)
    => '(2))

  (check
      (doit #f odd-exact-integer 3)
    => #t)

  (check
      (doit #f odd-exact-integer -2)
    => '(-2))

  (check
      (doit #f odd-exact-integer -3)
    => #t)

  (check
      (doit #f odd-exact-integer 'ciao)
    => '(ciao))

  (check
      (doit #f odd-exact-integer 0)
    => '(0))

  #t)


;;;; done

(check-report)

;;; end of file
