;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 7
;;;Date: Mon Jun  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 7\n")


;;;; code

;;Note: it does NOT crash when using 1057111.
(define m 1057112)
;; (check-display m)(newline)
;; (check-display "#x")(check-display (number->string m 16))(newline)
;; (check-display "#b")(check-display (number->string m 2))(newline)

(check
    (guard (E ((assertion-violation? E)
;;;	       (pretty-print (condition-message E))
	       #t)
	      (else E))
      (apply + (make-list m 1)))
  => #t)


;;;; done

(check-report)

;;; end of file
