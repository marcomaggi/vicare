;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <list> type
;;;Date: Sat Oct 24, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-types-list-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) xp.)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <list> objects\n")


;;;; helpers

(define-constant ENVIRONMENT
  (environment '(vicare)))

(define-syntax-rule (%eval ?sexp)
  (eval (quasiquote ?sexp) ENVIRONMENT))


(parametrise ((check-test-name	'predicate))

  (check-for-false	(is-a? '(1 . 2) <list>))
  (check-for-false	(is-a? 123 <list>))

  (check-for-true	(is-a? '(1) <list>))

  (check-for-true	(let (({O <list>} '(1 2)))
			  (is-a? O <list>)))

  (check-for-true	(let (({O <top>} '(1  2)))
			  (is-a? O <list>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <list>)))

  (void))


(parametrise ((check-test-name	'constructor))

  (check
      (new <list> 1 2)
    => '(1 2))

  (check
      (xp.type-signature-tags (type-of (new <list> (read) (read))))
    (=> syntax=?)
    (list #'<list>))

  #t)


(parametrise ((check-test-name	'methods))

  (check
      (.car (new <list> 1 2))
    => 1)

  (check
      (.cdr (new <list> 1 2))
    => '(2))

  (void))


(parametrise ((check-test-name	'late-binding))

  (check
      (method-call-late-binding 'car (new <list> 1 2))
    => 1)

  (check
      (method-call-late-binding 'cdr (new <list> 1 2))
    => '(2))

  #t)


(parametrise ((check-test-name	'chars))

;;; predicate

  (check-for-true	(is-a? '() <char*>))
  (check-for-true	(is-a? '(#\a #\b) <char*>))
  (check-for-false	(is-a? '(1 2) <char*>))

;;; --------------------------------------------------------------------
;;; constructor

  (check
      (new <char*>)
    => '())

  (check
      (new <char*> #\a #\b)
    => '(#\a #\b))

  (check
      (xp.type-signature-tags (type-of (new <char*> #\a #\b)))
    (=> syntax=?)
    (list #'<char*>))

  ;;Expand-time signature violation.  First operand.
  ;;
  (check
      (try
	  (%eval (new <char*> 1))
	(catch E
	  ((xp.&expand-time-type-signature-violation)
	   (list (xp.condition-argument-type-syntactic-identifier E)
		 (xp.condition-operand-type-syntactic-identifier  E)
		 (xp.condition-argument-index                     E)))
	  (else E)))
    (=> syntax=?)
    (list #'<char> #'<fixnum> 0))

  ;;Expand-time signature violation.  Second operand.
  ;;
  (check
      (try
	  (%eval (new <char*> #\a 1))
	(catch E
	  ((xp.&expand-time-type-signature-violation)
	   (list (xp.condition-argument-type-syntactic-identifier E)
		 (xp.condition-operand-type-syntactic-identifier  E)
		 (xp.condition-argument-index                     E)))
	  (else E)))
    (=> syntax=?)
    (list #'<char> #'<fixnum> 1))

  ;;Run-time validation.
  ;;
  (check
      (let ((port (open-string-input-port "#\\a #\\b")))
	(new <char*> (read port) (read port)))
    => '(#\a #\b))

  ;;Run-time validation.  Operands with signature "(<top>)".
  ;;
  (check
      (let ((port (open-string-input-port "#\\a #\\b")))
	(define ({read-it <top>})
	  (read port))
	(new <char*> (read-it) (read-it)))
    => '(#\a #\b))

  ;;Run-time validation.  Operands with signature "<list>".
  ;;
  (check
      (let ((port (open-string-input-port "#\\a #\\b")))
	(define ({read-it . <list>})
	  (read port))
	(new <char*> (read-it) (read-it)))
    => '(#\a #\b))

  ;;Run-time validation.  Bad first operand.
  ;;
  (check
      (try
	  (let ((port (open-string-input-port "123 #\\b")))
	    (new <char*> (read port) (read port)))
	(catch E
	  ((&procedure-signature-argument-violation)
	   (list (procedure-signature-argument-violation.one-based-argument-index E)
		 (procedure-signature-argument-violation.offending-value          E)
		 (procedure-signature-argument-violation.failed-expression        E)))
	  (else E)))
    => '(0 123 (is-a? _ <char>)))

  ;;Run-time validation.  Bad second operand.
  ;;
  (check
      (try
	  (let ((port (open-string-input-port "#\\a 123")))
	    (new <char*> (read port) (read port)))
	(catch E
	  ((&procedure-signature-argument-violation)
	   (list (procedure-signature-argument-violation.one-based-argument-index E)
		 (procedure-signature-argument-violation.offending-value          E)
		 (procedure-signature-argument-violation.failed-expression        E)))
	  (else E)))
    => '(1 123 (is-a? _ <char>)))

;;; --------------------------------------------------------------------
;;; methods

  (check
      (.car (new <char*> #\a #\b))
    => #\a)

  (check
      (.cdr (new <char*>  #\a #\b))
    => '(#\b))

;;; --------------------------------------------------------------------
;;; late binding

  (check
      (method-call-late-binding 'car (new <char*> #\a #\b))
    => #\a)

  (check
      (method-call-late-binding 'cdr (new <char*> #\a #\b))
    => '(#\b))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
