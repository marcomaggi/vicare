;;;
;;;Part of: Vicare Scheme
;;;Contents: configuration options
;;;Date: Mon Jun  4, 2012
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
(library (vicare options)
  (export
    print-loaded-libraries)
  (import (rnrs))

  (define print-loaded-libraries
    (let ((bool #f))
      (case-lambda
       (()
	bool)
       ((value)
	(set! bool (and value #t))))))

  )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
