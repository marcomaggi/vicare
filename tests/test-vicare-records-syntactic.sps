;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for R6RS records, syntactic layer
;;;Date: Thu Mar 22, 2012
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare R6RS records, syntactic layer\n")


;;;; syntax helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))


(parametrise ((check-test-name	'misc))

  (let ()
    (define-record-type <alpha>
      (nongenerative ciao-hello-ciao-1)
      (fields a))

    (check
	(record-rtd (make-<alpha> 1))
      => (record-type-descriptor <alpha>))

    #f)

  #t)


(parametrise ((check-test-name	'bugs))

  (check
      (catch #f
	(let ()
	  (define-record-type alpha
	    (fields a)
	    (protocol (lambda (maker)
			(newline))))
	  (make-alpha 1)))
    => (list (void)))

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
