;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: test file for (vicare linux), timerfd API
;;;Date: Mon Jul  2, 2012
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
  (vicare platform-constants)
  (only (vicare syntactic-extensions)
	unwind-protect)
  (prefix (vicare posix) px.)
  (prefix (vicare linux) lx.))

(set-port-buffer-mode! (current-output-port) (buffer-mode none))
(display "*** testing timerfd API library\n")


;;;; follow time going by

(define (%print-remaining-time fd)
  (pretty-print (lx.struct-itimerspec-it_value
		 (lx.timerfd-gettime fd))))

(define (%sleep-one-second)
  (px.nanosleep 1 0))

(let ((fd (lx.timerfd-create CLOCK_REALTIME TFD_NONBLOCK)))
  (unwind-protect
      (let ( ;; one event every 3 seconds
	    (period (lx.make-struct-timespec 3 0))
	    ;; the first event after 1 nanosecond
	    (offset (lx.make-struct-timespec 0 1)))
	(lx.timerfd-settime fd 0 (lx.make-struct-itimerspec period offset))
	(do ((i 0 (fx+ 1 i)))
	    ((fx= i 6))
	  (printf "fd readable: ~a\n" (px.select-fd-readable? fd 0 0))
	  (printf "number of expirations: ~a\n" (lx.timerfd-read fd))
	  (%print-remaining-time fd)
	  (%sleep-one-second))
	#f)
    (px.close fd)))


;;;; done

;;; end of file
