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
;;;Copyright (C) 2011, 2012, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-symbols)
  (options strict-r6rs)
  (import (vicare)
    (vicare checks))

(print-unicode #f)
(check-set-mode! 'report-failed)
(check-display "*** testing Vicare symbols\n")


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
		  (import (prefix (vicare expander tag-type-specs) typ.))
		  (typ.expand-time-type-signature-violation? E))
		(when print?
		  (check-pretty-print (condition-message E)))
		(syntax->datum (syntax-violation-subform E)))
	       (else E))
       (eval '(begin . ?body)
	     (environment '(vicare)
			  '(vicare system $strings)))))))

(define-syntax check-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((procedure-signature-argument-violation? E)
		    #;(print-condition E)
		    (procedure-signature-argument-violation.offending-value E))
		   ((procedure-arguments-consistency-violation? E)
		    #;(print-condition E)
		    (condition-irritants E))
		   ((procedure-argument-violation? E)
		    (when #f
		      (debug-print (condition-message E)))
		    (let ((D (cdr (condition-irritants E))))
		      (if (pair? D)
			  (car D)
			(condition-irritants E))))
		   ((assertion-violation? E)
		    (condition-irritants E))
		   (else
		    (print-condition E)
		    E))
	   ?body)
       => ?result))))


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
    => '(symbol<? (123)))

  (check-for-procedure-argument-violation
      (symbol<? 'abc 123)
    => '(symbol<? (123)))

  (check-for-procedure-argument-violation
      (symbol<? 'abc 'def 123)
    => '(symbol<? (123)))

  (check-for-procedure-argument-violation
      (symbol<? 'abc 'def 'ghi 123)
    => '(symbol<? (123)))

  (check-argument-violation
      (symbol<? 'abc 'def 'ghi 'lmn 123)
    => 123)

;;;

  (check-for-procedure-argument-violation
      (symbol<=? 123 'abc)
    => '(symbol<=? (123)))

  (check-for-procedure-argument-violation
      (symbol<=? 'abc 123)
    => '(symbol<=? (123)))

  (check-for-procedure-argument-violation
      (symbol<=? 'abc 'def 123)
    => '(symbol<=? (123)))

  (check-for-procedure-argument-violation
      (symbol<=? 'abc 'def 'ghi 123)
    => '(symbol<=? (123)))

  (check-argument-violation
      (symbol<=? 'abc 'def 'ghi 'lmn 123)
    => 123)

;;;

  (check-for-procedure-argument-violation
      (symbol>? 123 'abc)
    => '(symbol>? (123)))

  (check-for-procedure-argument-violation
      (symbol>? 'abc 123)
    => '(symbol>? (123)))

  (check-for-procedure-argument-violation
      (symbol>? 'abc 'def 123)
    => '(symbol>? (123)))

  (check-for-procedure-argument-violation
      (symbol>? 'abc 'def 'ghi 123)
    => '(symbol>? (123)))

  (check-argument-violation
      (symbol>? 'abc 'def 'ghi 'lmn 123)
    => 123)
;;;

  (check-for-procedure-argument-violation
      (symbol>=? 123 'abc)
    => '(symbol>=? (123)))

  (check-for-procedure-argument-violation
      (symbol>=? 'abc 123)
    => '(symbol>=? (123)))

  (check-for-procedure-argument-violation
      (symbol>=? 'abc 'def 123)
    => '(symbol>=? (123)))

  (check-argument-violation
      (symbol>=? 'abc 'def 'ghi 123)
    => 123)

  (check-argument-violation
      (symbol>=? 'abc 'def 'ghi 'lmn 123)
    => 123)
  #t)


(parametrise ((check-test-name	'symbol-inequal))

  (check
      (symbol!=? '\x00; '\x00;)
    => #f)

  (check
      (symbol!=? '\x00; '\x00; '\x00;)
    => #f)

  (check
      (symbol!=? 'a 'a)
    => #f)

  (check
      (symbol!=? 'a 'a 'a)
    => #f)

  (check
      (symbol!=? 'abc 'abc)
    => #f)

  (check
      (symbol!=? 'abc 'abc 'abc)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (symbol!=? 'a '\x00;)
    => #t)

  (check
      (symbol!=? '\x00; 'a)
    => #t)

  (check
      (symbol!=? 'a '\x00; '\x00;)
    => #f)

  (check
      (symbol!=? '\x00; 'a '\x00;)
    => #f)

  (check
      (symbol!=? '\x00; '\x00; 'a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (symbol!=? 'abc 'def)
    => #t)

  (check
      (symbol!=? 'abc 'a)
    => #t)

  (check
      (symbol!=? 'a 'abc)
    => #t)

  (check
      (symbol!=? 'a 'abc 'abc)
    => #f)

  (check
      (symbol!=? 'abc 'a 'abc)
    => #f)

  (check
      (symbol!=? 'abc 'abc 'a)
    => #f)

  (check
      (symbol!=? 'abc 'def 'ghi)
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation

  (check-procedure-arguments-violation
   (symbol!=? 123 '\x00;))

  (check-procedure-arguments-violation
   (symbol!=? '\x00; 123))

  (check-procedure-arguments-violation
   (symbol!=? '\x00; '\x00; 123))

  #t)


(parametrise ((check-test-name	'min-max))

  (check (symbol-min 'A)		=> 'A)

  (check (symbol-min 'a 'a)		=> 'a)
  (check (symbol-min 'a 'b)		=> 'a)
  (check (symbol-min 'b 'a)		=> 'a)

  (check (symbol-min 'a 'b 'c)		=> 'a)

  (check (symbol-min 'a 'b 'c 'd)	=> 'a)

  (check (symbol-min 'a 'b 'c 'd 'e)	=> 'a)

;;; --------------------------------------------------------------------

  (check (symbol-max 'A)		=> 'A)

  (check (symbol-max 'a 'a)		=> 'a)
  (check (symbol-max 'a 'b)		=> 'b)
  (check (symbol-max 'b 'a)		=> 'b)

  (check (symbol-max 'a 'b 'c)		=> 'c)

  (check (symbol-max 'a 'b 'c 'd)	=> 'd)

  (check (symbol-max 'a 'b 'c 'd 'e)	=> 'e)

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

#| end of program |# )

;;; end of file
