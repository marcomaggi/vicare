;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 15
;;;Date: Mon Jun  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (ikarus)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 15\n")


(check		;modify the binding of the raised object
    (guard (E ((begin
		 (set! E 456)
		 #f))
	      ((begin
;;;		      (check-display E)(newline)
		 (= E 456))
	       #t)
	      (else #f))
      (raise 123))
  => #t)

(check		;modify the  binding of  the raised object,  but reraise
		;the original object
    (guard (D (else (condition-message D)))
      (guard (E ((begin
		   (set! E #f)
		   #f)))
	(raise (condition (make-violation)
			  (make-message-condition "I am an error")))))
  => "I am an error")


;;;; done

(check-report)

;;; end of file
