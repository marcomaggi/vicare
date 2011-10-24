;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for symbols
;;;Date: Wed Oct 19, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (vicare) #;(ikarus)
		(parameterize	parametrise))
  (checks))

(print-unicode #f)
(check-set-mode! 'report-failed)
(display "*** testing Vicare symbols\n")


(parametrise ((check-test-name	'plists))

  (putprop 'ciao 'british 'hello)
  (putprop 'ciao 'spanish 'hola)

  (check
      (getprop 'ciao 'british)
    => 'hello)

  (check
      (getprop 'ciao 'spanish)
    => 'hola)

  (check
      (property-list 'ciao)
    => '((british . hello)
	 (spanish . hola)))

  (remprop 'ciao 'british)

  (check
      (getprop 'ciao 'british)
    => #f)

  (check
      (property-list 'ciao)
    => '((spanish . hola)))

  #t)


;;;; done

(check-report)

;;; end of file
