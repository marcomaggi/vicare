;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare posix) time and alarm functions
;;;Date: Tue Jul 17, 2012
;;;
;;;Abstract
;;;
;;;	This file is  standalone because it makes use of  the signal BUB
;;;	API.
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (prefix (vicare posix)
	  px.)
  (vicare platform constants)
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, time and alarms\n")


(parametrise ((check-test-name	'time))

  (when #f
    (check-pretty-print (list 'clock (px.clock)))
    (check-pretty-print (list 'time  (px.time)))
    (check-pretty-print (list 'timeofday  (px.gettimeofday))))

;;; --------------------------------------------------------------------

  (check
      (px.struct-tms? (px.times))
    => #t)

  (when #f
    (check-pretty-print (px.times)))

;;; --------------------------------------------------------------------

  (when #f
    (check-pretty-print (px.localtime (px.time)))
    (check-pretty-print (px.gmtime    (px.time))))

  (check
      (let ((T (px.time)))
	(equal? T (px.timelocal (px.localtime T))))
    => #t)

  (check
      (let ((T (px.time)))
	(equal? T (px.timegm (px.gmtime T))))
    => #t)

  (when #f
    (check-pretty-print
     (list 'strftime (px.strftime/string "%a %h %d %H:%M:%S %Y" (px.localtime (px.time))))))

;;; --------------------------------------------------------------------

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (let ((new (px.make-struct-itimerval
			(px.make-struct-timeval 0 0)
			(px.make-struct-timeval 0 1000))))
	      (px.setitimer ITIMER_REAL new))
	    (px.nanosleep 1 500)
	    (px.signal-bub-acquire)
	    (px.signal-bub-delivered? SIGALRM))
	(px.signal-bub-final))
    => #t)

  (check
      (let ((rv (px.getitimer ITIMER_REAL)))
	(list (px.struct-timeval-tv_sec (px.struct-itimerval-it_interval rv))
	      (px.struct-timeval-tv_usec (px.struct-itimerval-it_interval rv))
	      (px.struct-timeval-tv_sec (px.struct-itimerval-it_value rv))
	      (px.struct-timeval-tv_usec (px.struct-itimerval-it_value rv))))
    => '(0 0 0 0))

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.alarm 1)
	    (px.nanosleep 1 5)
	    (px.signal-bub-acquire)
	    (px.signal-bub-delivered? SIGALRM))
	(px.signal-bub-final))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
