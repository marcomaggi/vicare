;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the simple event loop (SEL)
;;;Date: Tue Feb 21, 2012
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
  (prefix (vicare posix) px.)
  (prefix (vicare simple-event-loop) sel.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare simple event loop\n")


(parametrise ((check-test-name	'fds))

  (check
      (with-result
       (let-values (((master slave) (px.socketpair PF_LOCAL SOCK_DGRAM 0)))
	 (unwind-protect
	     (begin
	       (sel.writable master
			     (lambda ()
(check-pretty-print 'writable)
			       (add-result '(master send "helo\n"))
			       (px.write master (ascii->string "helo\n"))))
	       (sel.readable slave
			     (lambda ()
(check-pretty-print 'readable)
			       (let ((buf (make-bytevector 5)))
				 (px.read slave buf 5)
				 (add-result `(slave recv ,(ascii->string buf)))
				 (sel.leave-asap)
				 )))
	       (sel.enter))
	   (px.shutdown master SHUT_RDWR)
	   (px.shutdown slave  SHUT_RDWR))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
