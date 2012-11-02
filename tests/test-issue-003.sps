;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 003
;;;Date: Fri Nov  2, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue #3\n")


;;The bug  will appear only when  the options '--debug -O2'  or '--debug
;;-O1' are used.
;;
(check
    (call-with-string-output-port
	(lambda (port)
	  (define (b)
	    (display "ciao\n" port))
	  (quasiquote (,(b)))
	  (flush-output-port port)))
  => "ciao\n")


;;;; done

(check-report)

;;; end of file
