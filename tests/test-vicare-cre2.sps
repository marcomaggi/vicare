;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: built in binding to CRE2
;;;Date: Fri Jan  6, 2012
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
(import (rename (vicare)
		(parameterize	parametrise))
  (prefix (vicare cre2) cre2.)
  (checks))

(unless (cre2.cre2-enabled?)
  (exit 0))

(check-set-mode! 'report-failed)
(display "*** testing Vicare CRE2 binding\n")


(parametrise ((check-test-name	'base))

 #; (check
      (let ()
        )
    => )

  #t)


;;;; done

(check-report)

;;; end of file
