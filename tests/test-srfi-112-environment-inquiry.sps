;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 112
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
  (prefix (srfi :112) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: 112, environment inquiry\n")


(parametrise ((check-test-name	'base))

  (when #t
    (printf "implmentation name and version: ~s ~s\n"
	    (srfi.implementation-name)
	    (srfi.implementation-version))
    (printf "CPU architecture: ~s, machine name: ~s\n"
	    (srfi.cpu-architecture)
	    (srfi.machine-name))
    (printf "OS name and version: ~s, ~s\n"
	    (srfi.os-name)
	    (srfi.os-version))
    (flush-output-port (current-output-port)))

  (check
      (srfi.implementation-name)
    => "vicare-scheme")

  (check
      (string? (srfi.implementation-version))
    => #t)

  (check-for-true
   (string? (srfi.cpu-architecture)))

  (check-for-true
   (string? (srfi.machine-name)))

  (check-for-true
   (string? (srfi.os-name)))

  (check-for-true
   (string? (srfi.os-version)))

  #t)


;;;; done

(check-report)

;;; end of file
