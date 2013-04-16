;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for errno related features
;;;Date: Tue Oct  2, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (vicare platform errno)
  (prefix (vicare platform constants)
	  plat.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: errno features\n")


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

(define-syntax catch-assertion
  (syntax-rules ()
    ((_ ?print-message . ?body)
     (guard (E
	     ((assertion-violation? E)
	      (when ?print-message
		(check-pretty-print (condition-message E)))
	      (condition-irritants E))
	     (else E))
       (let () . ?body)))))


(parametrise ((check-test-name	'case-errno))

  (check
      (case-errno plat.EPERM
	((EPERM)	1)
	((ENOMEM)	2)
	((EAGAIN)	3))
    => 1)

  (check
      (case-errno plat.EPERM
	((EPERM)	1)
	((ENOMEM)	2)
	((EAGAIN)	3)
	(else		#f))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (case-errno plat.EPERM
	((ENOMEM EPERM)	1)
	((EAGAIN)	3))
    => 1)

  (check
      (case-errno plat.EPERM
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (case-errno plat.EAGAIN
	((ENOMEM EPERM)	1)
	((EAGAIN)	3))
    => 3)

  (check
      (case-errno plat.EAGAIN
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(case-errno plat.EFAULT
	  ((ENOMEM EPERM)	1)
	  ((EAGAIN)		3)))
    => (list plat.EFAULT))

  (check
      (case-errno plat.EFAULT
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

;;;Syntax error "unknown symbolic error code"
;;;
  #;(case-errno plat.EFAULT
  ((ENOMEM EPERM)	1)
  ((ciao)		2))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'case-errno	'scheme-indent-function 1)
;; eval: (put 'catch		'scheme-indent-function 1)
;; eval: (put 'catch-assertion	'scheme-indent-function 1)
;; End:
