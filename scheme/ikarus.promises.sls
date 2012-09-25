;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (ikarus promises)
  (export force make-promise)
  (import (except (ikarus)
		  force make-promise)
    (vicare syntactic-extensions))

  (define-argument-validation (procedure who obj)
    (procedure? obj)
    (assertion-violation who "expected procedure as argument" obj))

  (define (force x)
    (define who 'force)
    (with-arguments-validation (who)
	((procedure  x))
      (x)))

  (define (make-promise proc)
    (define who 'make-promise)
    (with-arguments-validation (who)
	((procedure  proc))
      (let ((results #f))
	(lambda ()
	  (if results
	      (apply values results)
            (call-with-values proc
              (lambda x*
                (if results
                    (apply values results)
		  (begin
		    (set! results x*)
		    (apply values x*))))))))))

  #| end of library |# )

;;; end of file
