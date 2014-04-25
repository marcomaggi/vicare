;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test for issue #70
;;;Date: Fri Apr 25, 2014
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare checks))
(options tagged-language)

(check-set-mode! 'report-failed)
(check-display "*** testing issue 70: rogue object reference generated when returning multiple values from optimised R6RS record accessors")


;;;; tests

(check
    (call-with-values
	(lambda ()
	  (define-record-type alpha
	    (fields a b c  d e f))
	  (define O
	    (make-alpha 1 2 3 4 5 6))
	  (values (O a) (O b) (O c) (O d) (O e) (O f)))
      (lambda args args))
  => '(1 2 3 4 5 6))

#;(begin
  (print-gensym #f)
  (pretty-width 120)
  (debug-print 'optimisation
	       (optimisation-of
		(call-with-values
		    (lambda ()
		      (define-record-type alpha
			(fields a b c  d e f))
		      (define O
			(make-alpha 1 2 3 4 5 6))
		      (values (O a) (O b) (O c) (O d) (O e) (O f)))
		  (lambda args (debug-print 'here args))))))


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; End:
