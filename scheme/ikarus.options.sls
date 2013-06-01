;;;
;;;Part of: Vicare Scheme
;;;Contents: configuration options
;;;Date: Mon Jun  4, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    print-loaded-libraries
    report-errors-at-runtime
    strict-r6rs)
  (import (rnrs))

  (define-syntax define-boolean-option
    (syntax-rules ()
      ((_ ?who ?default)
       (define ?who
	 (let ((bool ?default))
	   (case-lambda
	    (()
	     bool)
	    ((value)
	     (set! bool (and value #t)))))))
      ))

  (define-boolean-option print-loaded-libraries   #f)
  (define-boolean-option report-errors-at-runtime #f)
  (define-boolean-option strict-r6rs              #f)
  )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
