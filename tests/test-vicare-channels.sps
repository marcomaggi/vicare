;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for channels library
;;;Date: Fri Apr  5, 2013
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


#!r6rs
(import (vicare)
  (prefix (vicare net channels)
	  chan.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare channels library\n")


(parametrise ((check-test-name	'struct))

  (check
      (let* ((port (open-bytevector-input-port '#vu8(1 2 3)))
	     (chan (chan.open-channel port)))
        (chan.channel? chan))
    => #t)

  (check
      (let* ((port (open-bytevector-input-port '#vu8(1 2 3)))
	     (chan (chan.open-channel port)))
        (chan.close-channel chan))
    => (void))

  #t)


;;;; done

(check-report)

;;; end of file
