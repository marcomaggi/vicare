;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: daemonise a process
;;;Date: Thu May  9, 2013
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
(library (vicare posix daemonisations)
  (export daemonise)
  (import (vicare)
    (prefix (vicare posix)
	    px.)
    (vicare platform constants))


(define (daemonise)
  ;;Daemonise  the  current process.   If  successful:  return a  fixnum
  ;;representing the new process group ID, else raise an exception.
  ;;
  (unless (= 1 (px.getppid))
    (with-compensations
      (compensate
	  (px.signal-bub-init)
	(with
	 (px.signal-bub-final)))
      (%exit-parent-keep-child)
      (%change-directory-to-root)
      (%set-umask-to-zero)
      (%replace-standard-ports)
      (%detach-from-terminal-and-become-session-leader))))

(define (%exit-parent-keep-child)
  (let ((pid (px.fork)))
    (unless (zero? pid)
      ;;We are in the parent.
      (exit 0))
    ;;We are in the child.
    (void)))

(define (%change-directory-to-root)
  (px.chdir "/"))

(define (%set-umask-to-zero)
  (px.umask 0))

(define (%replace-standard-ports)
  (let ((fd (px.open "/dev/null"
		     (fxior O_EXCL O_RDWR)
		     (fxior S_IRUSR S_IWUSR))))
    (px.close 0)
    (px.dup2 fd 0)
    (px.close 1)
    (px.dup2 fd 1)
    (px.close 2)
    (px.dup2 fd 2)
    (px.close fd)))

(define (%detach-from-terminal-and-become-session-leader)
  (px.setsid))


;;;; done

)

;;; end of file
