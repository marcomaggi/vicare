;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for keyword objects
;;;Date: Sat Mar 10, 2012
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
(import (vicare)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare keywords\n")


(check
    (keyword? 123)
  => #f)

(check
    (let ((K (symbol->keyword 'ciao)))
      (keyword? K))
  => #t)

(check
    (keyword->symbol (symbol->keyword 'ciao))
  => 'ciao)

;;; --------------------------------------------------------------------

(check
    (keyword=? (symbol->keyword 'ciao)
	       (symbol->keyword 'ciao))
  => #t)

(check
    (keyword=? (symbol->keyword 'ciao)
	       (symbol->keyword 'hello))
  => #f)

(check
    (let ((K (symbol->keyword 'ciao)))
      (keyword=? K K))
  => #t)

(check
    (keyword=? (symbol->keyword 'ciao)
	       'ciao)
  => #f)

(check
    (keyword=? 'ciao
	       (symbol->keyword 'ciao))
  => #f)

;;; --------------------------------------------------------------------
;;; EQ? comparison

(check
    (eq? (symbol->keyword 'ciao)
	 (symbol->keyword 'ciao))
  => #f)

(check
    (eq? (symbol->keyword 'ciao)
	 (symbol->keyword 'hello))
  => #f)

(check
    (let ((K (symbol->keyword 'ciao)))
      (eq? K K))
  => #t)

(check
    (eq? (symbol->keyword 'ciao)
	 'ciao)
  => #f)

(check
    (eq? 'ciao
	 (symbol->keyword 'ciao))
  => #f)

;;; --------------------------------------------------------------------
;;; EQV? comparison

(check
    (eqv? (symbol->keyword 'ciao)
	  (symbol->keyword 'ciao))
  => #t)

(check
    (eqv? (symbol->keyword 'ciao)
	  (symbol->keyword 'hello))
  => #f)

(check
    (let ((K (symbol->keyword 'ciao)))
      (eqv? K K))
  => #t)

(check
    (eqv? (symbol->keyword 'ciao)
	  'ciao)
  => #f)

(check
    (eqv? 'ciao
	  (symbol->keyword 'ciao))
  => #f)

;;; --------------------------------------------------------------------
;;; EQUAL? comparison

(check
    (equal? (symbol->keyword 'ciao)
	    (symbol->keyword 'ciao))
  => #t)

(check
    (equal? (symbol->keyword 'ciao)
	    (symbol->keyword 'hello))
  => #f)

(check
    (let ((K (symbol->keyword 'ciao)))
      (equal? K K))
  => #t)

(check
    (equal? (symbol->keyword 'ciao)
	    'ciao)
  => #f)

(check
    (equal? 'ciao
	    (symbol->keyword 'ciao))
  => #f)

;;; --------------------------------------------------------------------

(check
    (keyword-hash (symbol->keyword 'ciao))
  => (symbol-hash 'ciao))

;;; --------------------------------------------------------------------

(check
    (let-values (((port getter)
		  (open-string-output-port)))
      (display (symbol->keyword 'ciao) port)
      (getter))
  => "#[keyword ciao]")


;;;; done

(check-report)

;;; end of file
