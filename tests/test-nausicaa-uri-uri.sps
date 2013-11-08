;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for URI objects
;;;Date: Fri Nov  8, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (prefix (nausicaa net addresses uri) uri.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: URI objects\n")


(parametrise ((check-test-name	'scheme))

  (check
      (let (((O uri.<scheme>) '#ve(ascii "http")))
        (O uri-representation))
    => '#ve(ascii "http:"))

  (check
      (let (((O uri.<scheme>) '#ve(ascii "http")))
	(receive (port getter)
	    (open-bytevector-output-port)
	  (O put-uri-representation port)
	  (getter)))
    => '#ve(ascii "http:"))

  (check-for-true
   (let (((O uri.<scheme>) '#ve(ascii "http")))
     (fixnum? (O hash))))

;;; --------------------------------------------------------------------

  (check
      (try
	  (let (((O uri.<scheme>) '#vu8()))
	    #f)
	(catch E
	  (&tagged-binding-violation
	   #t)
	  (else E)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
