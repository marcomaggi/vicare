;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: test file for (vicare gcc)
;;;Date: Tue Dec  6, 2011
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


#!r6rs
(import (rename (vicare)
		(parameterize parametrise))
  (checks)
  (prefix (vicare gcc) gcc.))

(check-set-mode! 'report-failed)
(display "*** testing Vicare GCC library\n")

;;(gcc.initialise "gcc" "/home/marco/var/tmp/")
(gcc.initialise "/usr/local/bin/gcc" "/home/marco/var/tmp/")


(parametrise ((check-test-name	'base))

  (check
      (let ()
        (gcc.define-c-function signed-int incr (signed-int)
           "#include <stdio.h>
            int incr (int a) {
              return 1+a;
            }")
	(incr 1))
    => 2)

  #t)


;;;; done

(check-report)

;;; end of file
