;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheke
;;;Contents: tests for booleans
;;;Date: Sat Mar 28, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (vicare system $booleans)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare booleans\n")


;;;; syntax helpers

(define-syntax check-procedure-arguments-violation
  (syntax-rules ()
    ((_ ?body)
     (check-for-true
      (guard (E ((procedure-argument-violation? E)
		 (when #f
		   (check-pretty-print (condition-message E))
		   (check-pretty-print (condition-irritants E)))
		 #t)
		(else E))
	?body)))))

(define-syntax catch-expand-time-type-mismatch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((internal-body
		  (import (prefix (vicare expander object-type-specs) typ.))
		  (typ.expand-time-type-signature-violation? E))
		(when print?
		  (check-pretty-print (condition-message E)))
		(syntax->datum (syntax-violation-subform E)))
	       (else E))
       (eval '(begin . ?body)
	     (environment '(vicare)
			  '(vicare system $strings)))))))


(parametrise ((check-test-name	'boolean-cmp))

  (check-for-true	(boolean!=? #f #t))
  (check-for-true	(boolean!=? #t #f))
  (check-for-false	(boolean!=? #f #f))
  (check-for-false	(boolean!=? #t #t))

;;; --------------------------------------------------------------------

  (check-for-true	(boolean<? #f #t))
  (check-for-false	(boolean<? #t #f))
  (check-for-false	(boolean<? #f #f))
  (check-for-false	(boolean<? #t #t))

;;; --------------------------------------------------------------------

  (check-for-false	(boolean>? #f #t))
  (check-for-true	(boolean>? #t #f))
  (check-for-false	(boolean>? #f #f))
  (check-for-false	(boolean>? #t #t))

;;; --------------------------------------------------------------------

  (check-for-true	(boolean<=? #f #t))
  (check-for-false	(boolean<=? #t #f))
  (check-for-true	(boolean<=? #f #f))
  (check-for-true	(boolean<=? #t #t))

;;; --------------------------------------------------------------------

  (check-for-false	(boolean>=? #f #t))
  (check-for-true	(boolean>=? #t #f))
  (check-for-true	(boolean>=? #f #f))
  (check-for-true	(boolean>=? #t #t))

;;; --------------------------------------------------------------------

  (check-for-false	($boolean= #f #t))
  (check-for-false	($boolean= #t #f))
  (check-for-true	($boolean= #f #f))
  (check-for-true	($boolean= #t #t))

;;; --------------------------------------------------------------------

  (check-for-true	($boolean!= #f #t))
  (check-for-true	($boolean!= #t #f))
  (check-for-false	($boolean!= #f #f))
  (check-for-false	($boolean!= #t #t))

;;; --------------------------------------------------------------------

  (check-for-true	($boolean< #f #t))
  (check-for-false	($boolean< #t #f))
  (check-for-false	($boolean< #f #f))
  (check-for-false	($boolean< #t #t))

;;; --------------------------------------------------------------------

  (check-for-false	($boolean> #f #t))
  (check-for-true	($boolean> #t #f))
  (check-for-false	($boolean> #f #f))
  (check-for-false	($boolean> #t #t))

;;; --------------------------------------------------------------------

  (check-for-true	($boolean<= #f #t))
  (check-for-false	($boolean<= #t #f))
  (check-for-true	($boolean<= #f #f))
  (check-for-true	($boolean<= #t #t))

;;; --------------------------------------------------------------------

  (check-for-false	($boolean>= #f #t))
  (check-for-true	($boolean>= #t #f))
  (check-for-true	($boolean>= #f #f))
  (check-for-true	($boolean>= #t #t))

  #t)


(parametrise ((check-test-name	'min-max))

  (check (boolean-min #f)		=> #f)

  (check (boolean-min #f #f)		=> #f)
  (check (boolean-min #t #t)		=> #t)
  (check (boolean-min #f #t)		=> #f)
  (check (boolean-min #t #f)		=> #f)

  (check (boolean-min #f #t #t)		=> #f)
  (check (boolean-min #t #f #t)		=> #f)
  (check (boolean-min #t #t #f)		=> #f)

  (check (boolean-min #f #f #t)		=> #f)
  (check (boolean-min #t #f #f)		=> #f)
  (check (boolean-min #f #t #f)		=> #f)

;;; --------------------------------------------------------------------

  (check (boolean-max #f)		=> #f)

  (check (boolean-max #f #f)		=> #f)
  (check (boolean-max #t #t)		=> #t)
  (check (boolean-max #f #t)		=> #t)
  (check (boolean-max #t #f)		=> #t)

  (check (boolean-max #f #t #t)		=> #t)
  (check (boolean-max #t #f #t)		=> #t)
  (check (boolean-max #t #t #f)		=> #t)

  (check (boolean-max #f #f #t)		=> #t)
  (check (boolean-max #t #f #f)		=> #t)
  (check (boolean-max #f #t #f)		=> #t)

;;; --------------------------------------------------------------------

  (check ($boolean-min #f #f)		=> #f)
  (check ($boolean-min #t #t)		=> #t)
  (check ($boolean-min #f #t)		=> #f)
  (check ($boolean-min #t #f)		=> #f)

;;; --------------------------------------------------------------------

  (check ($boolean-max #f #f)		=> #f)
  (check ($boolean-max #t #t)		=> #t)
  (check ($boolean-max #f #t)		=> #t)
  (check ($boolean-max #t #f)		=> #t)

  #t)


;;;; done

(check-report)

;;; end of file
