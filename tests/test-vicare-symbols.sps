;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for symbols
;;;Date: Wed Oct 19, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(print-unicode #f)
(check-set-mode! 'report-failed)
(check-display "*** testing Vicare symbols\n")


;;;; syntax helpers

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


(parametrise ((check-test-name	'symbol-cmp))

  (check-for-false
   (symbol<? 'abcd 'abcd))

  (check-for-true
   (symbol<? 'abc 'abcd))

  (check-for-false
   (symbol<? 'abcd 'abc))

  (check-for-true
   (symbol<? 'ABcd 'abcd))

  (check-for-false
   (symbol<? 'abcd 'ABcd))

  (check-for-false
   (symbol<? 'abcd 'a2cd))

  (check-for-true
   (symbol<? 'abc 'abcd 'abcde))

  (check-for-false
   (symbol<? 'abc 'abcde 'abcd))

;;; --------------------------------------------------------------------

  (check-for-true
   (symbol<=? 'abcd 'abcd))

  (check-for-true
   (symbol<=? 'abc 'abcd))

  (check-for-false
   (symbol<=? 'abcd 'abc))

  (check-for-true
   (symbol<=? 'ABcd 'abcd))

  (check-for-false
   (symbol<=? 'abcd 'a2cd))

  (check-for-true
   (symbol<=? 'abc 'abcd 'abcde))

  (check-for-false
   (symbol<=? 'abc 'abcde 'abcd))

;;; --------------------------------------------------------------------

  (check-for-false
   (symbol>? 'abcd 'abcd))

  (check-for-true
   (symbol>? 'abcd 'abc))

  (check-for-false
   (symbol>? 'abc 'abcd))

  (check-for-false
   (symbol>? 'ABcd 'abcd))

  (check-for-true
   (symbol>? 'abcd 'ABcd))

  (check-for-false
   (symbol>? 'a2cd 'abcd))

  (check-for-true
   (symbol>? 'abcde 'abcd 'abc))

  (check-for-false
   (symbol>? 'abcd 'abcde 'abc))

;;; --------------------------------------------------------------------

  (check-for-true
   (symbol>=? 'abcd 'abcd))

  (check-for-true
   (symbol>=? 'abcd 'abc))

  (check-for-false
   (symbol>=? 'abc 'abcd))

  (check-for-true
   (symbol>=? 'abcd 'abcd))

  (check-for-false
   (symbol>=? 'a2cd 'abcd))

  (check-for-true
   (symbol>=? 'abcde 'abcd 'abc))

  (check-for-false
   (symbol>=? 'abcd 'abcde 'abc))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check-for-procedure-argument-violation
      (symbol<? 123 'abc)
    => '(symbol<? ((symbol? sym1) 123)))

  (check-for-procedure-argument-violation
      (symbol<? 'abc 123)
    => '(symbol<? ((symbol? sym2) 123)))

  (check-for-procedure-argument-violation
      (symbol<? 'abc 'def 123)
    => '(symbol<? ((symbol? sym3) 123)))

  (check-for-procedure-argument-violation
      (symbol<? 'abc 'def 'ghi 123)
    => '(symbol<? ((list-of-symbols? sym*) (123))))

;;;

  (check-for-procedure-argument-violation
      (symbol<=? 123 'abc)
    => '(symbol<=? ((symbol? sym1) 123)))

  (check-for-procedure-argument-violation
      (symbol<=? 'abc 123)
    => '(symbol<=? ((symbol? sym2) 123)))

  (check-for-procedure-argument-violation
      (symbol<=? 'abc 'def 123)
    => '(symbol<=? ((symbol? sym3) 123)))

  (check-for-procedure-argument-violation
      (symbol<=? 'abc 'def 'ghi 123)
    => '(symbol<=? ((list-of-symbols? sym*) (123))))

;;;

  (check-for-procedure-argument-violation
      (symbol>? 123 'abc)
    => '(symbol>? ((symbol? sym1) 123)))

  (check-for-procedure-argument-violation
      (symbol>? 'abc 123)
    => '(symbol>? ((symbol? sym2) 123)))

  (check-for-procedure-argument-violation
      (symbol>? 'abc 'def 123)
    => '(symbol>? ((symbol? sym3) 123)))

  (check-for-procedure-argument-violation
      (symbol>? 'abc 'def 'ghi 123)
    => '(symbol>? ((list-of-symbols? sym*) (123))))

;;;

  (check-for-procedure-argument-violation
      (symbol>=? 123 'abc)
    => '(symbol>=? ((symbol? sym1) 123)))

  (check-for-procedure-argument-violation
      (symbol>=? 'abc 123)
    => '(symbol>=? ((symbol? sym2) 123)))

  (check-for-procedure-argument-violation
      (symbol>=? 'abc 'def 123)
    => '(symbol>=? ((symbol? sym3) 123)))

  (check-for-procedure-argument-violation
      (symbol>=? 'abc 'def 'ghi 123)
    => '(symbol>=? ((list-of-symbols? sym*) (123))))

  #t)

(parametrise ((check-test-name	'plists))

  (putprop 'ciao 'british 'hello)
  (putprop 'ciao 'spanish 'hola)

  (check
      (getprop 'ciao 'british)
    => 'hello)

  (check
      (getprop 'ciao 'spanish)
    => 'hola)

  (check
      (property-list 'ciao)
    => '((british . hello)
	 (spanish . hola)))

  (remprop 'ciao 'british)

  (check
      (getprop 'ciao 'british)
    => #f)

  (check
      (property-list 'ciao)
    => '((spanish . hola)))

  #t)


;;;; done

(check-report)

;;; end of file
