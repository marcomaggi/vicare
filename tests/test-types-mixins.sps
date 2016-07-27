;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for DEFINE-MIXIN-TYPE
;;;Date: Wed Jun  1, 2016
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
(program (test-teyps-mixins)
  (options typed-language)
  (import (vicare)
    (vicare language-extensions mixins)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** test Vicare typed language: DEFINE-MIXIN-TYPES\n")


;;;; helpers

(define-syntax (get-spec stx)
  (syntax-case stx ()
    ((_ ?mixin-name)
     (with-syntax ((OBJ (retrieve-expand-time-value #'?mixin-name)))
       (syntax (quote OBJ))))
    ))

(define-syntax check-for-syntax-violation-subform
  (syntax-rules (=>)
    ((_ ?def => ?expected)
     (check
	 (try
	     (%eval-def (quote ?def))
	   (catch E
	     ((&syntax)
	      (when #t
		(fprintf (current-error-port) "~a\n" (condition-message E)))
	      (syntax-violation-subform E))
	     (else E)))
       (=> expander::syntax=?)
       (syntax ?expected)))
    ))

(define-constant EVAL-ENV
  (environment '(vicare)
	       '(vicare language-extensions mixins)))

(define (%eval-def sexp)
  (with-exception-handler
      (lambda (E)
	(unless (warning? E)
	  (raise-continuable E)))
    (lambda ()
      (eval `(internal-body ,sexp (sentinel))
	    EVAL-ENV
	    (expander-options typed-language)
	    (compiler-options)))))

(define (%eval sexp)
  (with-exception-handler
      (lambda (E)
	(unless (warning? E)
	  (raise-continuable E)))
    (lambda ()
      (eval sexp
	    EVAL-ENV
	    (expander-options typed-language)
	    (compiler-options)))))

(define (%print-message bool str)
  (when bool
    (fprintf (current-error-port) "~a\n" str)))


(parametrise ((check-test-name	'definition-basics))

  (check
      (internal-body
	(define-mixin-type <stuff>)
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff> (ciao))
    => ((ciao)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'nongenerative))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (nongenerative))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (nongenerative)))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (nongenerative stuff))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (nongenerative stuff)))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(nongenerative 123))
    => (nongenerative 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'define-type-descriptors))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (define-type-descriptors))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (define-type-descriptors)))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(define-type-descriptors 123))
    => (define-type-descriptors 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'strip-angular-parentheses))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (strip-angular-parentheses))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (strip-angular-parentheses)))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(strip-angular-parentheses 123))
    => (strip-angular-parentheses 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'sealed))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (sealed #t))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (sealed #t)))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (sealed #f))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (sealed #f)))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(sealed 123))
    => (sealed 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'opaque))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (opaque #t))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (opaque #t)))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (opaque #f))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff> (opaque #f)))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(opaque 123))
    => (opaque 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'protocol))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b)))))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b))))))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(protocol 1 2 3))
    => (protocol 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'super-protocol))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (super-protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b)))))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (super-protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b))))))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(super-protocol 1 2 3))
    => (super-protocol 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'fields))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields a b c))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (fields a b c)))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields {a <fixnum>} b {c <string>})
	  )
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (fields {a <fixnum>} b {c <string>})
	  ))

  ;;Collapsing multiple clauses.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields a b)
	  (fields c))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (fields a b c)))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields (mutable a)
		  (immutable b)
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (fields (mutable a)
		  (immutable b)
		  c)
	  ))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields (mutable {a <fixnum>})
		  (immutable {b <string>})
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (fields (mutable {a <fixnum>})
		  (immutable {b <string>})
		  c)
	  ))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields (mutable a <stuff>-a <stuff>-a-set!)
		  (immutable b <stuff>-b)
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (fields (mutable a <stuff>-a <stuff>-a-set!)
		  (immutable b <stuff>-b)
		  c)
	  ))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields (mutable {a <fixnum>} <stuff>-a <stuff>-a-set!)
		  (immutable {b <string>} <stuff>-b)
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (fields (mutable {a <fixnum>} <stuff>-a <stuff>-a-set!)
		  (immutable {b <string>} <stuff>-b)
		  c)
	  ))

;;; --------------------------------------------------------------------

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields 1))
    => 1)

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields a 1 c))
    => 1)

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (ciao a)))
    => (ciao a))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (mutable 1)))
    => (mutable 1))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (immutable 1)))
    => (immutable 1))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (mutable {1 <fixnum>})))
    => (mutable {1 <fixnum>}))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (immutable {1 <fixnum>})))
    => (immutable {1 <fixnum>}))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (immutable {a <fixnum>} 123)))
    => (immutable {a <fixnum>} 123))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (mutable {a <fixnum>} 123 <stuff>-a-set!)))
    => (mutable {a <fixnum>} 123 <stuff>-a-set!))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(fields (mutable {a <fixnum>} <stuff>-a 123)))
    => (mutable {a <fixnum>} <stuff>-a 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'custom-printer))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (custom-printer
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (custom-printer
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(custom-printer 1 2 3))
    => (custom-printer 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'type-predicate))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (type-predicate
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (type-predicate
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(type-predicate 1 2 3))
    => (type-predicate 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'equality-predicate))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (equality-predicate
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (equality-predicate
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(equality-predicate 1 2 3))
    => (equality-predicate 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'comparison-procedure))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (comparison-procedure
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (comparison-procedure
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(comparison-procedure 1 2 3))
    => (comparison-procedure 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'hash-function))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (hash-function
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (hash-function
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(hash-function 1 2 3))
    => (hash-function 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'method))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (method (doit)
	    (display this)))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (method (doit)
	    (display this))))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (method (doit)
	    (display this))
	  (method (done)
	    (write this)))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (method (doit)
	    (display this))
	  (method (done)
	    (write this))))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'method))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (method (doit)
	    (display this)))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (method (doit)
	    (display this))))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (method (doit)
	    (display this))
	  (method (done)
	    (write this)))
	(get-spec <stuff>))
    => '(define-mixin-type <stuff>
	  (method (doit)
	    (display this))
	  (method (done)
	    (write this))))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'implements))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (implements <Thing>)
	  (fields a b c)
	  (method (doit)
	    (display this)))

	(define-mixin-type <fluff>
	  (mixins <stuff>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (implements <Thing>)
	  (fields a b c)
	  (method (doit)
	    (display this))))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (implements <Thing>)
	  (fields a b c)
	  (method (doit)
	    (display this)))

	(define-mixin-type <fluff>
	  (implements <Thong>)
	  (mixins <stuff>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (implements <Thong> <Thing>)
	  (fields a b c)
	  (method (doit)
	    (display this))))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (implements <One> <Two>)
	  (implements <Three> <Four>)
	  (fields a b c)
	  (method (doit)
	    (display this)))

	(define-mixin-type <fluff>
	  (mixins <stuff>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (implements <One> <Two> <Three> <Four>)
	  (fields a b c)
	  (method (doit)
	    (display this))))

  (void))


(parametrise ((check-test-name	'mixins))

  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields a b c)
	  (method (doit)
	    (display this)))

	(define-mixin-type <fluff>
	  (mixins <stuff>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (fields a b c)
	  (method (doit)
	    (display this))))

  ;;Two mixins.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff-1>
	  (fields a b c))

	(define-mixin-type <stuff-2>
	  (method (doit)
	    (display this)))

	(define-mixin-type <fluff>
	  (mixins <stuff-1> <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (fields a b c)
	  (method (doit)
	    (display this))))

  ;;Two mixins with fields.
  ;;
  (check
      (internal-body
	(define-mixin-type <fields-1>
	  (fields a)
	  (fields b))

	(define-mixin-type <fields-2>
	  (mixins <fields-1>)
	  (fields c)
	  (fields d))

	(get-spec <fields-2>))
    => '(define-mixin-type <fields-2>
	  (fields a b c d)))

  ;;Multiple mixins with fields.
  ;;
  (check
      (internal-body
	(define-mixin-type <fields-1>
	  (fields a b))

	(define-mixin-type <fields-2>
	  (mixins <fields-1>)
	  (fields c d))

	(define-mixin-type <fields-3>
	  (mixins <fields-2>)
	  (fields e f))

	(get-spec <fields-3>))
    => '(define-mixin-type <fields-3>
	  (fields a b c d e f)))

  ;;Two mixins imported with one MIXINS clause.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin-type <stuff-2>
	  (fields c d)
	  (method (two) 2))

	(define-mixin-type <fluff>
	  (mixins <stuff-1> <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;Two mixins imported with two MIXINS clauses.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin-type <stuff-2>
	  (fields c d)
	  (method (two) 2))

	(define-mixin-type <fluff>
	  (mixins <stuff-1>)
	  (mixins <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;One mixin imported at the beginning of the clauses.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin-type <stuff-2>
	  (mixins <stuff-1>)
	  (fields c d)
	  (method (two) 2))

	(get-spec <stuff-2>))
    => '(define-mixin-type <stuff-2>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;One mixin imported at the end of the clauses.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff-1>
	  (fields c d)
	  (method (two) 2))

	(define-mixin-type <stuff-2>
	  (fields a b)
	  (method (one) 1)
	  (mixins <stuff-1>))

	(get-spec <stuff-2>))
    => '(define-mixin-type <stuff-2>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;Chain of imported mixins.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin-type <stuff-2>
	  (mixins <stuff-1>)
	  (fields c d)
	  (method (two) 2))

	(define-mixin-type <fluff>
	  (mixins <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin-type <fluff>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

;;; --------------------------------------------------------------------

  (check-for-syntax-violation-subform
      (define-mixin-type <stuff>
	(mixins 123))
    => (mixins 123))

  ;;Importing twice the same mixin is not an error right now, but it should.
  ;;
  (check
      (internal-body
	(define-mixin-type <a>
	  (fields a))

	(define-mixin-type <b>
	  (mixins <a> <a>))

	(get-spec <b>))
    => '(define-mixin-type <b>
	  (fields a a)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'records))

  ;;Single mixin with fields.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields one two))

	(define-record-type <duo>
	  (mixins <stuff>))

	(define O
	  (new <duo> 1 2))

	(values (.one O)
		(.two O)))
    => 1 2)

  ;;Multiple mixins with fields.
  ;;
  (check
      (internal-body
	(define-mixin-type <fields-1>
	  (fields a b))

	(define-mixin-type <fields-2>
	  (fields c d))

	(define-mixin-type <fields-3>
	  (fields e f))

	(define-record-type <alpha>
	  (mixins <fields-1> <fields-2> <fields-3>))

	(define O
	  (new <alpha> 1 2 3 4 5 6))

	(values (.a O) (.b O)
		(.c O) (.d O)
		(.e O) (.f O)))
    => 1 2 3 4 5 6)

  ;;Multiple mixins with fields.
  ;;
  (check
      (internal-body
	(define-mixin-type <fields-1>
	  (fields a b))

	(define-mixin-type <fields-2>
	  (mixins <fields-1>)
	  (fields c d))

	(define-mixin-type <fields-3>
	  (mixins <fields-2>)
	  (fields e f))

	(define-record-type <alpha>
	  (mixins <fields-3>))

	(define O
	  (new <alpha> 1 2 3 4 5 6))

	(values (.a O) (.b O)
		(.c O) (.d O)
		(.e O) (.f O)))
    => 1 2 3 4 5 6)

  ;;Actually using imported methods.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields one two)
	  (method (add)
	    (+ (.one this) (.two this))))

	(define-record-type <duo>
	  (mixins <stuff>))

	(define O
	  (new <duo> 1 2))

	(values (.one O)
		(.two O)
		(.add O)))
    => 1 2 3)


  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'protection-levels))

  ;;Public wrapping syntax.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff>
	  (public
	    (fields a)
	    (method (doit)
	      (.a this))))

	(define-record-type <blue>
	  (mixins <stuff>))

	(.doit (new <blue> 1)))
    => 1)

  ;;Nested syntaxes.
  ;;
  (check
      (internal-body
	(define-mixin-type <stuff>
	  (fields private a)
	  (method protected (one)
		  (.a this))
	  (method public (two)
		  (values (.one this) 2)))

	(define-record-type <blue>
	  (mixins <stuff>))

	(define O
	  (new <blue> 1))

	(.two O))
    => 1 2)

;;; --------------------------------------------------------------------
;;; definition errors

  ;;Protection   level  syntax   around   clause  not   accepting  protection   level
  ;;specification.
  ;;
  (begin
    (check
	(try
	    (%eval-def '(define-mixin-type <stuff>
			  (public (nongenerative))))
	  (catch E
	    ((&syntax)
	     (%print-message #t (condition-message E))
	     (syntax->datum (syntax-violation-subform E)))
	    (else E)))
      => '(nongenerative))
    (check
	(try
	    (%eval-def '(define-mixin-type <stuff>
			  (protected (nongenerative))))
	  (catch E
	    ((&syntax)
	     (%print-message #t (condition-message E))
	     (syntax->datum (syntax-violation-subform E)))
	    (else E)))
      => '(nongenerative))
    (check
	(try
	    (%eval-def '(define-mixin-type <stuff>
			  (private (nongenerative))))
	  (catch E
	    ((&syntax)
	     (%print-message #t (condition-message E))
	     (syntax->datum (syntax-violation-subform E)))
	    (else E)))
      => '(nongenerative))
    #| end of BEGIN |# )

  ;;Double protection level specification.
  ;;
  (check
      (try
	  (%eval-def '(define-mixin-type <stuff>
			(public
			  (fields public a))))
	(catch E
	  ((&syntax)
	   (%print-message #t (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(fields public public a))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'check-for-syntax-violation-subform 'scheme-indent-function 1)
;; End:
