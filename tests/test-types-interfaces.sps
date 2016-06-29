;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for DEFINE-INTERFACE
;;;Date: Sat Jun 25, 2016
;;;
;;;Abstract
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-types-interfaces)
  (options typed-language)
  (import (vicare)
    (vicare language-extensions interfaces)
    (vicare language-extensions instantiable-bodies)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** test Vicare typed language: interfaces and define-interface\n")


;;;; helpers

(define-type <all-fixnums>
  (or <non-negative-fixnum> <negative-fixnum>))

(define-syntax matching
  (syntax-rules (=>)
    ((_ ?one ?two => ?expected)
     (check
	 (type-annotation-matching ?one ?two)
       => (quote ?expected)))
    ))

(define (%eval sexp)
  (with-exception-handler
      (lambda (E)
	(unless (warning? E)
	  (raise-continuable E)))
    (lambda ()
      (eval sexp THE-ENVIRONMENT
	    (expander-options typed-language)
	    (compiler-options)))))

(define THE-ENVIRONMENT
  (environment '(vicare)
	       '(vicare language-extensions interfaces)))

(define (%print-message bool str)
  (when bool
    (fprintf (current-error-port) "~a\n" str)))



(parametrise ((check-test-name	'basic))

  ;;Basic example.
  ;;
  (check
      (internal-body
	(define-interface <Arith>
	  (method-prototype add
	    (lambda () => (<number>))))

	(define-record-type <duo>
	  (implements <Arith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define (fun {O <Arith>})
	  (.add O))

	(fun (new <duo>  1 2)))
    => 3)

  ;;Interface with no method prototypes.
  ;;
  (check
      (internal-body
	(define-interface <Stringer>
	  (method (to-string)
	    (with-output-to-string
	      (lambda ()
		(display this)))))

	(define-record-type <duo>
	  (implements <Stringer>)
	  (fields one two)
	  (custom-printer
	    (lambda ({this <duo>} port sub-printer)
	      (display "#[duo "    port)
	      (display (.one this) port)
	      (display #\space     port)
	      (display (.two this) port)
	      (display #\]         port))))

	(define (fun {O <Stringer>})
	  (.to-string O))

	(fun (new <duo>  1 2)))
    => "#[duo 1 2]")

  (void))


(parametrise ((check-test-name	'errors))

  ;;Attempt to instantiate interface.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface <Arith>
		      (method-prototype add
			(lambda () => (<number>))))

		    (new <Arith>)))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '<Arith>)

  ;;Run-time type validation failure cause by APPLY.
  ;;
  (check
      (internal-body
	(define-interface <Arith>
	  (method-prototype add
	    (lambda () => (<number>))))

	(define-record-type <duo>
	  (implements <Arith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define (fun {O <Arith>})
	  (.add O))

	(try
	    (apply fun (list (new <duo>  1 2)))
	  (catch E
	    ((&procedure-signature-argument-violation)
	     (procedure-signature-argument-violation.failed-expression E))
	    (else E))))
    => '(is-a? _ <Arith>))

  ;;The argument of IMPLEMENTS is not an identifier.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <duo>
		      (implements 123)
		      (fields one two)
		      (method ({add <number>})
			(+ (.one this) (.two this))))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 123)

  ;;Interface implemented twice.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface <Arith>
		      (method-prototype add
			(lambda () => (<number>))))

		    (define-record-type <duo>
		      (implements <Arith>)
		      (implements <Arith>)
		      (fields one two)
		      (method ({add <number>})
			(+ (.one this) (.two this))))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '<Arith>)

  ;;The argument of IMPLEMENTS is not an interface name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <duo>
		      (implements <fixnum>)
		      (fields one two)
		      (method ({add <number>})
			(+ (.one this) (.two this))))))
	(catch E
	  ((&assertion)
	   (%print-message #f (condition-message E))
	   (syntax->datum (car (condition-irritants E))))
	  (else E)))
    => '<fixnum>)

  (void))


(parametrise ((check-test-name	'multiple-implementations))

  ;;Two record-types in a hierarchy both implement the same interface.
  ;;
  (check
      (internal-body
	(define-interface <Arith>
	  (method-prototype add
	    (lambda () => (<number>))))

	(define-record-type <duo>
	  (implements <Arith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define-record-type <trio>
	  (parent <duo>)
	  (implements <Arith>)
	  (fields three)
	  (method ({add <number>})
	    (+ (.one this) (.two this) (.three this))))

	(define (fun {O <Arith>})
	  (.add O))

	(values (fun (new <duo>  1 2))
		(fun (new <trio> 1 2 3))))
    => 3 6)

  (void))


(parametrise ((check-test-name	'nongenerative))

  ;;NONGENERATIVE clause with explicit UID.
  ;;
  (check
      (internal-body
	(define-interface <Arith>
	  (nongenerative test-1:<Arith>)
	  (method-prototype add
	    (lambda () => (<number>))))

	(define-record-type <duo>
	  (implements <Arith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define (fun {O <Arith>})
	  (.add O))

	(fun (new <duo> 1 2)))
    => 3)

  ;;NONGENERATIVE clause with implicit UID.
  ;;
  (check
      (internal-body
	(define-interface <Arith>
	  (nongenerative)
	  (method-prototype add
	    (lambda () => (<number>))))

	(define-record-type <duo>
	  (implements <Arith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define (fun {O <Arith>})
	  (.add O))

	(fun (new <duo> 1 2)))
    => 3)


  (void))


(parametrise ((check-test-name	'type-descriptor))

  (import (prefix (vicare system type-descriptors)
		  td::))

  (check
      (internal-body
	(define-interface <Stuff>
	  (method-prototype red
	    (lambda () => (<top>)))
	  (method (blue)
	    2))

	(define itd
	  (type-descriptor <Stuff>))

	(values (td::interface-type-descr? itd)
		(td::interface-type-descr.type-name itd)))
    => #t '<Stuff>)

  (check
      (internal-body
	(define-interface <Stuff>
	  (method-prototype red
	    (lambda () => (<top>)))
	  (method (blue)
	    2))

	(define itd
	  (type-descriptor <Stuff>))

	(procedure? (td::interface-type-descr.method-retriever itd)))
    => #t)

  (void))


(parametrise ((check-test-name	'misc))

  (define-interface <Sequence>
    (method-prototype length	(lambda () => (<non-negative-exact-integer>)))
    (method-prototype first	(lambda () => (<char>)))
    (method-prototype ref	(lambda (<non-negative-exact-integer>) => (<char>)))
    (method ({just-length <non-negative-exact-integer>})
      (.length this))
    (case-method just-item
      (({_ <char>})
       (.ref this 0))
      (({_ <char>} {idx <non-negative-exact-integer>})
       (.ref this idx)))
    (method/overload (doit)
      this))

  (define-record-type <str>
    (implements <Sequence>)
    (fields {str <string>})
    (method ({length <non-negative-fixnum>})
      (string-length (.str this)))
    (method ({first  <char>})
      (string-ref    (.str this) 0))
    (method ({ref    <char>} {idx <non-negative-exact-integer>})
      (string-ref    (.str this) idx)))

  (define (fun {O <Sequence>})
    (.length O))

;;; --------------------------------------------------------------------

  (check (type-annotation-matching <Sequence> <Sequence>)	=> 'exact-match)
  (check (type-annotation-matching <Sequence> <str>)		=> 'exact-match)
  (check (type-annotation-matching <str> <Sequence>)		=> 'no-match)
  (check (type-annotation-matching <Sequence> <fixnum>)		=> 'no-match)
  (check (type-annotation-matching <fixnum> <Sequence>)		=> 'no-match)

  (check-for-true	(type-annotation-super-and-sub? <Sequence> <Sequence>))
  (check-for-true	(type-annotation-super-and-sub? <Sequence> <str>))
  (check-for-false	(type-annotation-super-and-sub? <str> <Sequence>))
  (check-for-false	(type-annotation-super-and-sub? <Sequence> <fixnum>))
  (check-for-false	(type-annotation-super-and-sub? <fixnum> <Sequence>))

;;; --------------------------------------------------------------------

  (check
      (let ((O (new <str> "ciao")))
	(fun O))
    => 4)

  (void))


(parametrise ((check-test-name	'instantiable-bodies))

  ;;Generic interfaces through instantiable bodies.
  ;;
  (check
      (internal-body
	(define-instantiable-body define-iface-arith
	  (define-interface <Iface>
	    (method-prototype add
	      (lambda () => (<type-name>)))))

	(define-iface-arith
	  ((<Iface>		<NumberArith>)
	   (<type-name>		<number>)))

	(define-iface-arith
	  ((<Iface>		<StringArith>)
	   (<type-name>		<string>)))

	(define-record-type <duo>
	  (implements <NumberArith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define (nfun {O <NumberArith>})
	  (.add O))

	(define-record-type <string-duo>
	  (implements <StringArith>)
	  (fields one two)
	  (method ({add <string>})
	    (string-append (.one this) (.two this))))

	(define (sfun {O <StringArith>})
	  (.add O))

	(values (nfun (new <duo> 1 2))
		(sfun (new <string-duo> "hel" "lo"))))
    => 3 "hello")

;;; --------------------------------------------------------------------

  ;;Generic interfaces through instantiable bodies, overloaded functions.
  ;;
  (check
      (internal-body
	(define-instantiable-body define-iface-arith
	  (define-interface <Iface>
	    (method-prototype add
	      (lambda () => (<type-name>)))))

	(define-iface-arith
	  ((<Iface>		<NumberArith>)
	   (<type-name>		<number>)))

	(define-iface-arith
	  ((<Iface>		<StringArith>)
	   (<type-name>		<string>)))

	(define-record-type <duo>
	  (implements <NumberArith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define/overload (fun {O <NumberArith>})
	  (.add O))

	(define-record-type <string-duo>
	  (implements <StringArith>)
	  (fields one two)
	  (method ({add <string>})
	    (string-append (.one this) (.two this))))

	(define/overload (fun {O <StringArith>})
	  (.add O))

	(values (fun (new <duo> 1 2))
		(fun (new <string-duo> "hel" "lo"))))
    => 3 "hello")

  (void))


(parametrise ((check-test-name	'doc))

  ;;No-interfaces example
  ;;

  (internal-body

    (define-record-type <a-vector>
      (fields {vec <nevector>})
      (method ({first <top>})
	(vector-ref (.vec this) 0)))

    (define-record-type <a-string>
      (fields {vec <nestring>})
      (method ({first <top>})
	(string-ref (.vec this) 0)))

    (define-record-type <a-list>
      (fields {vec <nelist>})
      (method ({first <top>})
	(car (.vec this))))

    (define (fun O)
      (.first O))

    (check
	(let ((O (new <a-vector> '#(1 2 3))))
	  (fun O))
      => 1)

    (check
	(let ((O (new <a-string> "ABC")))
	  (fun O))
      => #\A)

    (check
	(let ((O (new <a-list> '(a b c))))
	  (fun O))
      => 'a)

    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------

  ;;The same as above but with interfaces.
  ;;
  (internal-body

    (define-interface <Sequence>
      (method-prototype first
	(lambda () => (<top>))))

    (define-record-type <a-vector>
      (implements <Sequence>)
      (fields {vec <nevector>})
      (method ({first <top>})
	(vector-ref (.vec this) 0)))

    (define-record-type <a-string>
      (implements <Sequence>)
      (fields {vec <nestring>})
      (method ({first <top>})
	(string-ref (.vec this) 0)))

    (define-record-type <a-list>
      (implements <Sequence>)
      (fields {vec <nelist>})
      (method ({first <top>})
	(car (.vec this))))

    (define (fun {O <Sequence>})
      (.first O))

    (check
	(let ((O (new <a-vector> '#(1 2 3))))
	  (fun O))
      => 1)

    (check
	(let ((O (new <a-string> "ABC")))
	  (fun O))
      => #\A)

    (check
	(let ((O (new <a-list> '(a b c))))
	  (fun O))
      => 'a)

    ;;Operand does not implement the interface.
    ;;
    ;; (check
    ;; 	(let ((O '#(1 2 3)))
    ;; 	  (fun O))
    ;;   => #\A)

    ;;Run-time operand validation is impossible.
    ;;
    ;; (check
    ;; 	(let ((O (new <a-string> "ABC")))
    ;; 	  (apply fun (list O)))
    ;;   => #\A)

    #| end of INTERNAL-BODY |# )

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
