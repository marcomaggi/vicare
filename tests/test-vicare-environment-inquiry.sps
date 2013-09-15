;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for environment inquiry functions
;;;Date: Sun Sep 15, 2013
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: environment inquiry functions\n")


(parametrise ((check-test-name	'uname))

  (when #t
    (pretty-print (uname)))

  (check-for-true
      (utsname? (uname)))

  (check-for-true
   (string? (utsname-sysname (uname))))

  (check-for-true
   (string? (utsname-nodename (uname))))

  (check-for-true
   (string? (utsname-release (uname))))

  (check-for-true
   (string? (utsname-version (uname))))

  (check-for-true
   (string? (utsname-machine (uname))))

  #t)


(parametrise ((check-test-name	'procs))

  (when #t
    (printf "implmentation name and version: ~s ~s\n"
	    (implementation-name)
	    (implementation-version))
    (printf "CPU architecture: ~s, machine name: ~s\n"
	    (cpu-architecture)
	    (machine-name))
    (printf "OS name and version: ~s, ~s\n"
	    (os-name)
	    (os-version))
    (flush-output-port (current-output-port)))

  (check
      (implementation-name)
    => "vicare-scheme")

  (check
      (string? (implementation-version))
    => #t)

  (check-for-true
   (string? (cpu-architecture)))

  (check-for-true
   (string? (machine-name)))

  (check-for-true
   (string? (os-name)))

  (check-for-true
   (string? (os-version)))

  #t)


;;;; done

(check-report)

;;; end of file
