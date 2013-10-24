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
  (export
    force
    (rename (public-make-promise make-promise))
    promise?)
  (import (except (ikarus)
		  force
		  make-promise
		  promise?)
    (vicare language-extensions syntaxes)
    (rename (vicare arguments validation)
	    (promise.vicare-arguments-validation
	     promise-struct.vicare-arguments-validation)))


(define-struct promise
  (proc results))

(define (force P)
  (define who 'force)
  (with-arguments-validation (who)
      ((promise-struct	P))
    (if ($promise-results P)
	(apply values ($promise-results P))
      (call-with-values ($promise-proc P)
	(lambda x*
	  (if ($promise-results P)
	      (apply values ($promise-results P))
	    (begin
	      ($set-promise-results! P x*)
	      (apply values x*))))))))

(define (public-make-promise proc)
  (define who 'make-promise)
  (with-arguments-validation (who)
      ((procedure  proc))
    (make-promise proc #f)))


;;;; done

)

;;; end of file
