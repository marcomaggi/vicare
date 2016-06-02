;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for DEFINE-MIXINS
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
    #;(prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** test Vicare typed language: DEFINE-MIXINS\n")


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
       (=> syntax=?)
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
      (eval `(internal-body ,sexp (void))
	    EVAL-ENV
	    (expander-options typed-language)
	    (compiler-options)))))


(parametrise ((check-test-name	'definition-basics))

  (check
      (internal-body
	(define-mixin <stuff>)
	(get-spec <stuff>))
    => '(define-mixin <stuff>))

  (check-for-syntax-violation-subform
      (define-mixin <stuff> (ciao))
    => ((ciao)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'nongenerative))

  (check
      (internal-body
	(define-mixin <stuff>
	  (nongenerative))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (nongenerative)))

  (check
      (internal-body
	(define-mixin <stuff>
	  (nongenerative stuff))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (nongenerative stuff)))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(nongenerative 123))
    => (nongenerative 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'define-type-descriptors))

  (check
      (internal-body
	(define-mixin <stuff>
	  (define-type-descriptors))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (define-type-descriptors)))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(define-type-descriptors 123))
    => (define-type-descriptors 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'strip-angular-parentheses))

  (check
      (internal-body
	(define-mixin <stuff>
	  (strip-angular-parentheses))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (strip-angular-parentheses)))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(strip-angular-parentheses 123))
    => (strip-angular-parentheses 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'sealed))

  (check
      (internal-body
	(define-mixin <stuff>
	  (sealed #t))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (sealed #t)))

  (check
      (internal-body
	(define-mixin <stuff>
	  (sealed #f))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (sealed #f)))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(sealed 123))
    => (sealed 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'opaque))

  (check
      (internal-body
	(define-mixin <stuff>
	  (opaque #t))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (opaque #t)))

  (check
      (internal-body
	(define-mixin <stuff>
	  (opaque #f))
	(get-spec <stuff>))
    => '(define-mixin <stuff> (opaque #f)))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(opaque 123))
    => (opaque 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'protocol))

  (check
      (internal-body
	(define-mixin <stuff>
	  (protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b)))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b))))))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(protocol 1 2 3))
    => (protocol 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'super-protocol))

  (check
      (internal-body
	(define-mixin <stuff>
	  (super-protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b)))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (super-protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record a b))))))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(super-protocol 1 2 3))
    => (super-protocol 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'fields))

  (check
      (internal-body
	(define-mixin <stuff>
	  (fields a b c))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (fields a b c)))

  (check
      (internal-body
	(define-mixin <stuff>
	  (fields {a <fixnum>} b {c <string>})
	  )
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (fields {a <fixnum>} b {c <string>})
	  ))

  ;;Collapsing multiple clauses.
  ;;
  (check
      (internal-body
	(define-mixin <stuff>
	  (fields a b)
	  (fields c))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (fields a b c)))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-mixin <stuff>
	  (fields (mutable a)
		  (immutable b)
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (fields (mutable a)
		  (immutable b)
		  c)
	  ))

  (check
      (internal-body
	(define-mixin <stuff>
	  (fields (mutable {a <fixnum>})
		  (immutable {b <string>})
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (fields (mutable {a <fixnum>})
		  (immutable {b <string>})
		  c)
	  ))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-mixin <stuff>
	  (fields (mutable a <stuff>-a <stuff>-a-set!)
		  (immutable b <stuff>-b)
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (fields (mutable a <stuff>-a <stuff>-a-set!)
		  (immutable b <stuff>-b)
		  c)
	  ))

  (check
      (internal-body
	(define-mixin <stuff>
	  (fields (mutable {a <fixnum>} <stuff>-a <stuff>-a-set!)
		  (immutable {b <string>} <stuff>-b)
		  c)
	  )
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (fields (mutable {a <fixnum>} <stuff>-a <stuff>-a-set!)
		  (immutable {b <string>} <stuff>-b)
		  c)
	  ))

;;; --------------------------------------------------------------------

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields 1))
    => 1)

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields a 1 c))
    => 1)

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (ciao a)))
    => (ciao a))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (mutable 1)))
    => 1)

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (immutable 1)))
    => 1)

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (mutable {1 <fixnum>})))
    => {1 <fixnum>})

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (immutable {1 <fixnum>})))
    => {1 <fixnum>})

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (immutable {a <fixnum>} 123)))
    => (immutable {a <fixnum>} 123))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (mutable {a <fixnum>} 123 <stuff>-a-set!)))
    => (mutable {a <fixnum>} 123 <stuff>-a-set!))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(fields (mutable {a <fixnum>} <stuff>-a 123)))
    => (mutable {a <fixnum>} <stuff>-a 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'custom-printer))

  (check
      (internal-body
	(define-mixin <stuff>
	  (custom-printer
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (custom-printer
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(custom-printer 1 2 3))
    => (custom-printer 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'type-predicate))

  (check
      (internal-body
	(define-mixin <stuff>
	  (type-predicate
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (type-predicate
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(type-predicate 1 2 3))
    => (type-predicate 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'equality-predicate))

  (check
      (internal-body
	(define-mixin <stuff>
	  (equality-predicate
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (equality-predicate
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(equality-predicate 1 2 3))
    => (equality-predicate 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'comparison-procedure))

  (check
      (internal-body
	(define-mixin <stuff>
	  (comparison-procedure
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (comparison-procedure
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(comparison-procedure 1 2 3))
    => (comparison-procedure 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'hash-function))

  (check
      (internal-body
	(define-mixin <stuff>
	  (hash-function
	    (lambda (upper)
	      (lambda args args))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (hash-function
	    (lambda (upper)
	      (lambda args args)))))

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(hash-function 1 2 3))
    => (hash-function 1 2 3))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'method))

  (check
      (internal-body
	(define-mixin <stuff>
	  (method (doit {O <stuff>})
	    (display O)))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (method (doit {O <stuff>})
	    (display O))))

  (check
      (internal-body
	(define-mixin <stuff>
	  (method (doit {O <stuff>})
	    (display O))
	  (method (done {O <stuff>})
	    (write O)))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (method (doit {O <stuff>})
	    (display O))
	  (method (done {O <stuff>})
	    (write O))))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'method/overload))

  (check
      (internal-body
	(define-mixin <stuff>
	  (method/overload (doit {O <stuff>})
	    (display O)))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (method/overload (doit {O <stuff>})
	    (display O))))

  (check
      (internal-body
	(define-mixin <stuff>
	  (method/overload (doit {O <stuff>})
	    (display O))
	  (method/overload (done {O <stuff>})
	    (write O)))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (method/overload (doit {O <stuff>})
	    (display O))
	  (method/overload (done {O <stuff>})
	    (write O))))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'case-method))

  (check
      (internal-body
	(define-mixin <stuff>
	  (case-method doit
	    (({O <stuff>})
	     (display O))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (case-method doit
	    (({O <stuff>})
	     (display O)))))

  (check
      (internal-body
	(define-mixin <stuff>
	  (case-method doit
	    (({O <stuff>})
	     (display O))
	    (({O <stuff>} {P <port>})
	     (display O P))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (case-method doit
	    (({O <stuff>})
	     (display O))
	    (({O <stuff>} {P <port>})
	     (display O P)))))

  (check
      (internal-body
	(define-mixin <stuff>
	  (case-method doit
	    (({O <stuff>})
	     (display O)))
	  (case-method done
	    (({O <stuff>})
	     (write O))))
	(get-spec <stuff>))
    => '(define-mixin <stuff>
	  (case-method doit
	    (({O <stuff>})
	     (display O)))
	  (case-method done
	    (({O <stuff>})
	     (write O)))))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'mixins))

  (check
      (internal-body
	(define-mixin <stuff>
	  (fields a b c)
	  (method (doit {O <stuff>})
	    (display O)))

	(define-mixin <fluff>
	  (mixins <stuff>))

	(get-spec <fluff>))
    => '(define-mixin <fluff>
	  (fields a b c)
	  (method (doit {O <fluff>})
	    (display O))))

  ;;Two mixins.
  ;;
  (check
      (internal-body
	(define-mixin <stuff-1>
	  (fields a b c))

	(define-mixin <stuff-2>
	  (method (doit {O <stuff-2>})
	    (display O)))

	(define-mixin <fluff>
	  (mixins <stuff-1> <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin <fluff>
	  (fields a b c)
	  (method (doit {O <fluff>})
	    (display O))))

  ;;Two mixins with fields.
  ;;
  (check
      (internal-body
	(define-mixin <fields-1>
	  (fields a)
	  (fields b))

	(define-mixin <fields-2>
	  (mixins <fields-1>)
	  (fields c)
	  (fields d))

	(get-spec <fields-2>))
    => '(define-mixin <fields-2>
	  (fields a b c d)))

  ;;Multiple mixins with fields.
  ;;
  (check
      (internal-body
	(define-mixin <fields-1>
	  (fields a b))

	(define-mixin <fields-2>
	  (mixins <fields-1>)
	  (fields c d))

	(define-mixin <fields-3>
	  (mixins <fields-2>)
	  (fields e f))

	(get-spec <fields-3>))
    => '(define-mixin <fields-3>
	  (fields a b c d e f)))

  ;;Two mixins imported with one MIXINS clause.
  ;;
  (check
      (internal-body
	(define-mixin <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin <stuff-2>
	  (fields c d)
	  (method (two) 2))

	(define-mixin <fluff>
	  (mixins <stuff-1> <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin <fluff>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;Two mixins imported with two MIXINS clauses.
  ;;
  (check
      (internal-body
	(define-mixin <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin <stuff-2>
	  (fields c d)
	  (method (two) 2))

	(define-mixin <fluff>
	  (mixins <stuff-1>)
	  (mixins <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin <fluff>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;One mixin imported at the beginning of the clauses.
  ;;
  (check
      (internal-body
	(define-mixin <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin <stuff-2>
	  (mixins <stuff-1>)
	  (fields c d)
	  (method (two) 2))

	(get-spec <stuff-2>))
    => '(define-mixin <stuff-2>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;One mixin imported at the end of the clauses.
  ;;
  (check
      (internal-body
	(define-mixin <stuff-1>
	  (fields c d)
	  (method (two) 2))

	(define-mixin <stuff-2>
	  (fields a b)
	  (method (one) 1)
	  (mixins <stuff-1>))

	(get-spec <stuff-2>))
    => '(define-mixin <stuff-2>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

  ;;Chain of imported mixins.
  ;;
  (check
      (internal-body
	(define-mixin <stuff-1>
	  (fields a b)
	  (method (one) 1))

	(define-mixin <stuff-2>
	  (mixins <stuff-1>)
	  (fields c d)
	  (method (two) 2))

	(define-mixin <fluff>
	  (mixins <stuff-2>))

	(get-spec <fluff>))
    => '(define-mixin <fluff>
	  (fields a b c d)
	  (method (one) 1)
	  (method (two) 2)))

;;; --------------------------------------------------------------------

  (check-for-syntax-violation-subform
      (define-mixin <stuff>
	(mixins 123))
    => (mixins 123))

  ;;Importing twice the same mixin is not an error right now, but it should.
  ;;
  (check
      (internal-body
	(define-mixin <a>
	  (fields a))

	(define-mixin <b>
	  (mixins <a> <a>))

	(get-spec <b>))
    => '(define-mixin <b>
	  (fields a a)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'records))

  ;;Single mixin with fields.
  ;;
  (check
      (internal-body
	(define-mixin <stuff>
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
	(define-mixin <fields-1>
	  (fields a b))

	(define-mixin <fields-2>
	  (fields c d))

	(define-mixin <fields-3>
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
	(define-mixin <fields-1>
	  (fields a b))

	(define-mixin <fields-2>
	  (mixins <fields-1>)
	  (fields c d))

	(define-mixin <fields-3>
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

  #| end of PARAMETRISE |# )


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'check-for-syntax-violation-subform 'scheme-indent-function 1)
;; End:
