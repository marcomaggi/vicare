;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <list> type
;;;Date: Sat Oct 24, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <list> objects\n")


;;;; helpers

(library (types-of-lists)
  (export
    <list-of-chars>
    <list-of-strings>)
  (import (vicare))
  (define-type <list-of-chars>		(list-of <char>))
  (define-type <list-of-strings>	(list-of <string>))
  #| end of LIBRARY |# )

(import (types-of-lists))

(define-constant ENVIRONMENT
  (environment '(vicare)
	       '(types-of-lists)))

(define-syntax-rule (%eval ?sexp)
  (eval (quasiquote ?sexp) ENVIRONMENT))

(define-syntax check-expand-time-signature-violation
  (syntax-rules (=>)
    ((_ ?input-form => ?expected-signature-sexp ?returned-signature-sexp)
     (check
	 (try
	     (%eval ?input-form)
	   (catch E
	     ((expander::&expand-time-type-signature-violation)
	      #;(print-condition E)
	      (values (syntax->datum (expander::type-signature.tags (expander::condition-expected-type-signature E)))
		      (syntax->datum (expander::type-signature.tags (expander::condition-returned-type-signature E)))))
	     (else E)))
       => (quote ?expected-signature-sexp) (quote ?returned-signature-sexp)))
    ))


(parametrise ((check-test-name	'type-of))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expression ?expected-tags)
       (check
	   ;;The  return value  of  a  TYPE-OF use  expansion  and  evaluation is  an
	   ;;instance of "<type-signature>".
	   (.tags (type-of ?expression))
	 (=> syntax=?)
	 ;;When the expression is a CONDITION application: the expected tags value is
	 ;;a list with a single item.
	 ?expected-tags))
      ))

;;; --------------------------------------------------------------------

  (doit (list 1)
	#'((list <positive-fixnum>)))

  (doit (list 1 2 3)
	#'((list <positive-fixnum> <positive-fixnum> <positive-fixnum>)))

  (doit (list 1 "ciao" 'ciao)
	#'((list <positive-fixnum> <string> <symbol>)))

  (doit (list 1 '(2 3))
	#'((list <positive-fixnum> (list <positive-fixnum> <positive-fixnum>))))

  (doit (list 1 '())
	#'((list <positive-fixnum> <null>)))

  (void))


(parametrise ((check-test-name	'type-tags))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?type-annotation ?expected-tags)
       ;;Here we test only type signature describing a single value.
       (check
	   (.tags (new expander::<type-signature> #'(?type-annotation)))
	 (=> syntax=?)
	 #'(?expected-tags)))
      ))

;;; --------------------------------------------------------------------

  (doit <list>
	<list>)

  (doit (list)
  	<null>)

  (doit (list <fixnum>)
  	(list <fixnum>))

  (doit (list <fixnum> <flonum> <string>)
  	(list <fixnum> <flonum> <string>))

  (void))


(parametrise ((check-test-name	'predicate))

  (check-for-false	(is-a? '(1 . 2) <list>))
  (check-for-true	(is-a? '() <list>))
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
      (.tags (type-of (new <list> (read) (read))))
    (=> syntax=?)
    #'((list <top> <top>)))

  (check
      (.tags (type-of (new <list>)))
    (=> syntax=?)
    (list #'<null>))

;;; --------------------------------------------------------------------

  (check
      (.tags (type-of (list)))
    (=> syntax=?)
    (list #'<null>))

  (check
      (.tags (type-of (list 1)))
    (=> syntax=?)
    #'((list <positive-fixnum>)))

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

  (define-type <list-of-chars>
    (list-of <char>))

;;; --------------------------------------------------------------------
;;; predicate

  (check-for-true	(is-a? '() <list-of-chars>))
  (check-for-true	(is-a? '(#\a #\b) <list-of-chars>))
  (check-for-false	(is-a? '(1 2) <list-of-chars>))

;;; --------------------------------------------------------------------
;;; constructor

  (check
      (new <list-of-chars>)
    => '())

  (check
      (new <list-of-chars> #\a #\b)
    => '(#\a #\b))

  (check
      (expander::type-signature.tags (type-of (new <list-of-chars> #\a #\b)))
    (=> syntax=?)
    #'((list <char> <char>)))

  ;;Expand-time signature violation.  First operand.
  ;;
  (check-expand-time-signature-violation
      (new <list-of-chars> 1)
    => (<list-of-chars>) ((list <positive-fixnum>)))

  ;;Expand-time signature violation.  Second operand.
  ;;
  (check-expand-time-signature-violation
      (new <list-of-chars> #\a 1)
    => (<list-of-chars>) ((list <char> <positive-fixnum>)))

  ;;Run-time validation.
  ;;
  (check
      (let ((port (open-string-input-port "#\\a #\\b")))
	(new <list-of-chars> (read port) (read port)))
    => '(#\a #\b))

  ;;Run-time validation.  Operands with signature "(<top>)".
  ;;
  (check
      (let ((port (open-string-input-port "#\\a #\\b")))
	(define ({read-it <top>})
	  (read port))
	(new <list-of-chars> (read-it) (read-it)))
    => '(#\a #\b))

  ;;Run-time validation.  Operands with signature "<list>".
  ;;
  (check
      (let ((port (open-string-input-port "#\\a #\\b")))
	(define ({read-it . <list>})
	  (read port))
	(new <list-of-chars> (read-it) (read-it)))
    => '(#\a #\b))

  ;;Run-time validation.  Bad first operand.
  ;;
  (check
      (try
	  (let ((port (open-string-input-port "123 #\\b")))
	    (new <list-of-chars> (read port) (read port)))
	(catch E
	  ((&expression-return-value-violation)
	   (condition-irritants E))
	  (else E)))
    => '((is-a? _ <list-of-chars>) (123 #\b)))

  ;;Run-time validation.  Bad second operand.
  ;;
  (check
      (try
	  (let ((port (open-string-input-port "#\\a 123")))
	    (new <list-of-chars> (read port) (read port)))
	(catch E
	  ((&expression-return-value-violation)
	   (condition-irritants E))
	  (else E)))
    => '((is-a? _ <list-of-chars>) (#\a 123)))

;;; --------------------------------------------------------------------
;;; methods

  (check
      (.car (new <list-of-chars> #\a #\b))
    => #\a)

  (check
      (.cdr (new <list-of-chars>  #\a #\b))
    => '(#\b))

;;; --------------------------------------------------------------------
;;; late binding

  (check
      (method-call-late-binding 'car (new <list-of-chars> #\a #\b))
    => #\a)

  (check
      (method-call-late-binding 'cdr (new <list-of-chars> #\a #\b))
    => '(#\b))

  #t)


(parametrise ((check-test-name	'strings))

  (define-type <list-of-strings>
    (list-of <string>))

;;; --------------------------------------------------------------------
;;; predicate

  (check-for-true	(is-a? '() <list-of-strings>))
  (check-for-true	(is-a? '("a") <list-of-strings>))
  (check-for-true	(is-a? '("a" "b") <list-of-strings>))
  (check-for-false	(is-a? '(1 2) <list-of-strings>))

;;; --------------------------------------------------------------------
;;; constructor

  (check
      (new <list-of-strings>)
    => '())

  (check
      (new <list-of-strings> "a" "b")
    => '("a" "b"))

  (check
      (expander::type-signature.tags (type-of (new <list-of-strings> "a" "b")))
    (=> syntax=?)
    #'((list <string> <string>)))

  ;;Expand-time signature violation.  First operand.
  ;;
  (check-expand-time-signature-violation
      (new <list-of-strings> 1)
    => (<list-of-strings>) ((list <positive-fixnum>)))

  ;;Expand-time signature violation.  Second operand.
  ;;
  (check-expand-time-signature-violation
      (new <list-of-strings> "a" 1)
    => (<list-of-strings>) ((list <string> <positive-fixnum>)))

  ;;Run-time validation.
  ;;
  (check
      (let ((port (open-string-input-port "\"a\" \"b\"")))
	(new <list-of-strings> (read port) (read port)))
    => '("a" "b"))

  ;;Run-time validation.  Operands with signature "(<top>)".
  ;;
  (check
      (let ((port (open-string-input-port "\"a\" \"b\"")))
	(define ({read-it <top>})
	  (read port))
	(new <list-of-strings> (read-it) (read-it)))
    => '("a" "b"))

  ;;Run-time validation.  Operands with signature "<list>".
  ;;
  (check
      (let ((port (open-string-input-port "\"a\" \"b\"")))
	(define ({read-it . <list>})
	  (read port))
	(new <list-of-strings> (read-it) (read-it)))
    => '("a" "b"))

  ;;Run-time validation.  Bad first operand.
  ;;
  (check
      (try
	  (let ((port (open-string-input-port "123 \"b\"")))
	    (new <list-of-strings> (read port) (read port)))
	(catch E
	  ((&expression-return-value-violation)
	   (condition-irritants E))
	  (else E)))
    => '((is-a? _ <list-of-strings>) (123 "b")))

  ;;Run-time validation.  Bad second operand.
  ;;
  (check
      (try
	  (let ((port (open-string-input-port "\"a\" 123")))
	    (new <list-of-strings> (read port) (read port)))
	(catch E
	  ((&expression-return-value-violation)
	   (condition-irritants E))
	  (else E)))
    => '((is-a? _ <list-of-strings>) ("a" 123)))

;;; --------------------------------------------------------------------
;;; methods

  (check
      (.car (new <list-of-strings> "a" "b"))
    => "a")

  (check
      (.cdr (new <list-of-strings>  "a" "b"))
    => '("b"))

;;; --------------------------------------------------------------------
;;; late binding

  (check
      (method-call-late-binding 'car (new <list-of-strings> "a" "b"))
    => "a")

  (check
      (method-call-late-binding 'cdr (new <list-of-strings> "a" "b"))
    => '("b"))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
