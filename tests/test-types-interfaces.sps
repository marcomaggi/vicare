;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for DEFINE-INTERFACE-TYPE
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
    (prefix (vicare system type-descriptors) td::)
    (vicare language-extensions interfaces)
    (vicare language-extensions instantiable-bodies)
    (only (vicare expander)
	  type-annotation=?
	  type-annotation-super-and-sub?
	  type-annotation-common-ancestor
	  type-annotation-ancestors
	  type-annotation-syntax
	  type-annotation-matching
	  type-signature-super-and-sub?
	  type-signature-common-ancestor
	  type-signature-matching
	  type-signature-union)
    (prefix (vicare expander) xp::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** test Vicare typed language: interfaces and define-interface-type\n")


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
	       '(vicare language-extensions interfaces)
	       '(only (vicare expander)
		      type-annotation=?
		      type-annotation-super-and-sub?
		      type-annotation-common-ancestor
		      type-annotation-ancestors
		      type-annotation-syntax
		      type-annotation-matching
		      type-signature-super-and-sub?
		      type-signature-common-ancestor
		      type-signature-matching
		      type-signature-union)))

(define (%print-message bool str)
  (when bool
    (fprintf (current-error-port) "~a\n" str)))


(parametrise ((check-test-name	'interface-parent))

  ;;Two levels.
  ;;
  (check
      (internal-body
	(define-interface-type <parentI>
	  (method-prototype doit
	    (lambda (<number>) => (<string>))))

	(define-interface-type <childI>
	  (parent <parentI>))

	(values (type-annotation-super-and-sub? <parentI> <childI>)
		(type-annotation-super-and-sub? <childI> <parentI>)
		(type-annotation-matching <parentI> <childI>)
		(type-annotation-matching <childI> <parentI>)))
    => #t #f 'exact-match 'no-match)

  ;;Three levels.
  ;;
  (check
      (internal-body
	(define-interface-type <granpaI>
	  (method-prototype doit
	    (lambda (<number>) => (<string>))))

	(define-interface-type <dadI>
	  (parent <granpaI>))

	(define-interface-type <childI>
	  (parent <dadI>))

	(values (type-annotation-super-and-sub? <granpaI> <granpaI>)
		(type-annotation-super-and-sub? <granpaI> <dadI>)
		(type-annotation-super-and-sub? <granpaI> <childI>)
		(type-annotation-super-and-sub? <dadI> <granpaI>)
		(type-annotation-super-and-sub? <dadI> <dadI>)
		(type-annotation-super-and-sub? <dadI> <childI>)
		(type-annotation-super-and-sub? <childI> <granpaI>)
		(type-annotation-super-and-sub? <childI> <dadI>)
		(type-annotation-super-and-sub? <childI> <childI>)
		(type-annotation-matching <granpaI> <granpaI>)
		(type-annotation-matching <granpaI> <dadI>)
		(type-annotation-matching <granpaI> <childI>)
		(type-annotation-matching <dadI> <granpaI>)
		(type-annotation-matching <dadI> <dadI>)
		(type-annotation-matching <dadI> <childI>)
		(type-annotation-matching <childI> <granpaI>)
		(type-annotation-matching <childI> <dadI>)
		(type-annotation-matching <childI> <childI>)))
    => #t #t #t
    #f #t #t
    #f #f #t
    'exact-match 'exact-match 'exact-match
    'no-match 'exact-match 'exact-match
    'no-match 'no-match 'exact-match)

  (void))


(parametrise ((check-test-name	'interface-implements))

  (check
      (type-annotation-super-and-sub? (lambda (<nestring>) => (<number>))
				      (lambda (<string>) => (<fixnum>)))
    => #t)

;;; --------------------------------------------------------------------

  ;;An interface-type implements another interface-type.
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype doit
	    (lambda (<nestring>) => (<number>))))

	(define-interface-type <ITwo>
	  (implements <IOne>)
	  (method-prototype doit
	    (lambda (<string>) => (<fixnum>))))

	(values (type-annotation-super-and-sub? <IOne> <ITwo>)
		(type-annotation-super-and-sub? <ITwo> <IOne>)
		(type-annotation-matching <IOne> <ITwo>)
		(type-annotation-matching <ITwo> <IOne>)))
    => #t #f 'exact-match 'no-match)

  ;;An  interface-type implements  another interface-type.   <B> implements  <I>, the
  ;;parent of <B> has the method prototypes.
  ;;
  (check
      (internal-body
	(define-interface-type <I>
	  (method-prototype doit
	    (lambda (<string>) => (<number>))))

	(define-interface-type <A>
	  (method-prototype doit
	    (lambda (<string>) => (<number>))))

	(define-interface-type <B>
	  (parent <A>)
	  (implements <I>))

	(values (type-annotation-super-and-sub? <I> <A>)
		(type-annotation-super-and-sub? <I> <B>)
		(type-annotation-super-and-sub? <A> <I>)
		(type-annotation-super-and-sub? <B> <I>)
		(type-annotation-matching <I> <A>)
		(type-annotation-matching <I> <B>)
		(type-annotation-matching <A> <I>)
		(type-annotation-matching <B> <I>)))
    => #f #t #f #f 'no-match 'exact-match 'no-match 'no-match)

  ;;An interface-type  implements another interface-type.  <A>  implements <ISub>, it
  ;;has all the methods in <superI> and <ISub>.
  ;;
  (check
      (internal-body
	(define-interface-type <ISuper>
	  (method-prototype super-doit
	    (lambda (<string>) => (<number>))))

	(define-interface-type <ISub>
	  (parent <ISuper>)
	  (method-prototype sub-doit
	    (lambda (<string>) => (<fixnum>))))

	(define-interface-type <A>
	  (implements <ISub>)
	  (method-prototype super-doit
	    (lambda (<string>) => (<number>)))
	  (method-prototype sub-doit
	    (lambda (<string>) => (<fixnum>)))
	  #| end of DEFINE-INTERFACE-TYPE |# )

	(values (type-annotation-super-and-sub? <ISuper> <A>)
		(type-annotation-super-and-sub? <ISub>   <A>)
		(type-annotation-super-and-sub? <A> <ISuper>)
		(type-annotation-super-and-sub? <A> <ISub>)
		(type-annotation-matching <ISuper> <A>)
		(type-annotation-matching <ISub>   <A>)
		(type-annotation-matching <A> <ISuper>)
		(type-annotation-matching <A> <ISub>)))
    => #t #t #f #f 'exact-match 'exact-match 'no-match 'no-match)

  ;;An interface-type implements another interface-type.  <B> implements <ISub>.  <B>
  ;;and its parent <A> have has all the methods in <ISuper> and <ISub>.
  ;;
  (check
      (internal-body
	(define-interface-type <ISuper>
	  (method-prototype super-doit
	    (lambda (<string>) => (<number>))))

	(define-interface-type <ISub>
	  (parent <ISuper>)
	  (method-prototype sub-doit
	    (lambda (<string>) => (<fixnum>))))

	(define-interface-type <A>
	  (method-prototype super-doit
	    (lambda (<string>) => (<number>)))
	  #| end of DEFINE-INTERFACE-TYPE |# )

	(define-interface-type <B>
	  (parent <A>)
	  (implements <ISub>)
	  (method-prototype sub-doit
	    (lambda (<string>) => (<fixnum>)))
	  #| end of DEFINE-INTERFACE-TYPE |# )

	(values (type-annotation-super-and-sub? <ISuper> <A>)
		(type-annotation-super-and-sub? <ISuper> <B>)
		(type-annotation-super-and-sub? <ISub>   <A>)
		(type-annotation-super-and-sub? <ISub>   <B>)
		(type-annotation-super-and-sub? <A> <ISuper>)
		(type-annotation-super-and-sub? <A> <ISub>)
		(type-annotation-super-and-sub? <B> <ISuper>)
		(type-annotation-super-and-sub? <B> <ISub>)
		(type-annotation-matching <ISuper> <A>)
		(type-annotation-matching <ISuper> <B>)
		(type-annotation-matching <ISub>   <A>)
		(type-annotation-matching <ISub>   <B>)
		(type-annotation-matching <A> <ISuper>)
		(type-annotation-matching <A> <ISub>)
		(type-annotation-matching <B> <ISuper>)
		(type-annotation-matching <B> <ISub>)))
    => #f #t #f #t #f #f #f #f
    'no-match 'exact-match 'no-match 'exact-match
    'no-match 'no-match 'no-match 'no-match)

  ;;An interface-type implements  another interface-type and the  interfaces that the
  ;;latter implements.
  ;;
  (check
      (internal-body
	(define-interface-type <A>
	  (method-prototype red		(lambda () => (<fixnum>)))
	  #| end of interface |# )

	(define-interface-type <B>
	  (implements <A>)
	  (method-prototype red		(lambda () => (<fixnum>)))
	  (method-prototype blue	(lambda () => (<symbol>)))
	  #| end of interface |# )

	(define-interface-type <C>
	  (implements <B>)
	  (method-prototype red		(lambda () => (<fixnum>)))
	  (method-prototype blue	(lambda () => (<symbol>)))
	  #| end of interface |# )

	(values (type-annotation-super-and-sub? <A> <B>)
		(type-annotation-super-and-sub? <A> <C>)
		(type-annotation-super-and-sub? <B> <A>)
		(type-annotation-super-and-sub? <B> <C>)
		(type-annotation-super-and-sub? <C> <A>)
		(type-annotation-super-and-sub? <C> <B>)
		))
    => #t #t #f #t #f #f)

  ;;The  interface-type  "<IThree>"  implements   the  interface-types  "<IOne>"  and
  ;;"<ITwo>".   "<IThree>"  implements  the  "composite"  method  from  "<IOne>"  and
  ;;"<ITwo>" with multiple METHOD-PROTOTYPE clauses.
  ;;
  ;;                 <IOne>
  ;;                    ^
  ;;                    |
  ;;   <IThree> +++> <ITwo>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype doit
	    (lambda (<fixnum>) => (<string>))))

	(define-interface-type <ITwo>
	  (parent <IOne>)
	  (method-prototype doit
	    (lambda (<flonum>) => (<string>))))

	(define-interface-type <IThree>
	  (implements <ITwo>)
	  (method-prototype doit (lambda (<fixnum>) => (<string>)))
	  (method-prototype doit (lambda (<flonum>) => (<string>))))

	(values (type-annotation-super-and-sub? <IOne> <IThree>)
		(type-annotation-super-and-sub? <IOne> <ITwo>)
		(type-annotation-super-and-sub? <ITwo> <IThree>)
		(type-annotation-super-and-sub? <IThree> <IOne>)
		(type-annotation-super-and-sub? <ITwo> <IOne>)
		(type-annotation-super-and-sub? <IThree> <ITwo>)))
    => #t #t #t #f #f #f)

  ;;The  interface-type  "<IThree>"  implements   the  interface-types  "<IOne>"  and
  ;;"<ITwo>".   "<IThree>"  implements  the  "composite"  method  from  "<IOne>"  and
  ;;"<ITwo>" with a single METHOD-PROTOTYPE clause.
  ;;
  ;;                 <IOne>
  ;;                    ^
  ;;                    |
  ;;   <IThree> +++> <ITwo>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype doit
	    (lambda (<fixnum>) => (<string>))))

	(define-interface-type <ITwo>
	  (parent <IOne>)
	  (method-prototype doit
	    (lambda (<flonum>) => (<string>))))

	(define-interface-type <IThree>
	  (implements <ITwo>)
	  (method-prototype doit (case-lambda
				   ((<fixnum>) => (<string>))
				   ((<flonum>) => (<string>)))))

	(values (type-annotation-super-and-sub? <IOne> <IThree>)
		(type-annotation-super-and-sub? <IOne> <ITwo>)
		(type-annotation-super-and-sub? <ITwo> <IThree>)
		(type-annotation-super-and-sub? <IThree> <IOne>)
		(type-annotation-super-and-sub? <ITwo> <IOne>)
		(type-annotation-super-and-sub? <IThree> <ITwo>)))
    => #t #t #t #f #f #f)

;;; --------------------------------------------------------------------
;;; errors

  ;;An interface-type fails to implement another interface-type: missing method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method-prototype doit
			(lambda (<string>) => (<number>))))

		    (define-interface-type <ITwo>
		      (implements <IOne>))

		    (type-annotation-super-and-sub? <IOne> <ITwo>)))
	(catch E
	  ((xp::&interface-implementation-missing-method-violation)
	   (%print-message #f (condition-message E))
	   #t)
	  (else E)))
    => #t)

  ;;An interface-type fails to implement another interface-type: mismatching method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method-prototype doit
			(lambda (<string>) => (<number>))))

		    (define-interface-type <ITwo>
		      (implements <IOne>)
		      (method-prototype doit
			(lambda (<string>) => (<symbol>))))

		    (type-annotation-super-and-sub? <IOne> <ITwo>)))
	(catch E
	  ((xp::&interface-implementation-mismatching-method-violation)
	   (%print-message #f (condition-message E))
	   #t)
	  (else E)))
    => #t)

  (void))


(parametrise ((check-test-name	'record-implements))

  ;;A record-type implements an interface-type.
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype doit
	    (lambda (<string>) => (<number>))))

	(define-record-type <blue>
	  (implements <IOne>)
	  (method ({doit <number>} {S <string>})
	    1))

	(values (type-annotation-super-and-sub? <IOne> <blue>)
		(type-annotation-super-and-sub? <blue> <IOne>)
		(type-annotation-matching <IOne> <blue>)
		(type-annotation-matching <blue> <IOne>)))
    => #t #f 'exact-match 'no-match)

  ;;A record-type  implements an interface-type.   <B> implements <I>, the  parent of
  ;;<B> has the methods.
  ;;
  (check
      (internal-body
	(define-interface-type <I>
	  (method-prototype doit
	    (lambda (<string>) => (<number>))))

	(define-record-type <A>
	  (method ({doit <number>} {S <string>})
	    1))

	(define-record-type <B>
	  (parent <A>)
	  (implements <I>))

	(values (type-annotation-super-and-sub? <I> <A>)
		(type-annotation-super-and-sub? <I> <B>)
		(type-annotation-super-and-sub? <A> <I>)
		(type-annotation-super-and-sub? <B> <I>)
		(type-annotation-matching <I> <A>)
		(type-annotation-matching <I> <B>)
		(type-annotation-matching <A> <I>)
		(type-annotation-matching <B> <I>)))
    => #f #t #f #f 'no-match 'exact-match 'no-match 'no-match)

  ;;A record-type  implements an interface-type.   <A> implements <ISub>, it  has all
  ;;the methods in <superI> and <ISub>.
  ;;
  (check
      (internal-body
	(define-interface-type <ISuper>
	  (method-prototype super-doit
	    (lambda (<string>) => (<number>))))

	(define-interface-type <ISub>
	  (parent <ISuper>)
	  (method-prototype sub-doit
	    (lambda (<string>) => (<fixnum>))))

	(define-record-type <A>
	  (implements <ISub>)
	  (method ({super-doit <number>} {S <string>})
	    1)
	  (method ({sub-doit <fixnum>} {S <string>})
	    1)
	  #| end of DEFINE-INTERFACE-TYPE |# )

	(values (type-annotation-super-and-sub? <ISuper> <A>)
		(type-annotation-super-and-sub? <ISub>   <A>)
		(type-annotation-super-and-sub? <A> <ISuper>)
		(type-annotation-super-and-sub? <A> <ISub>)
		(type-annotation-matching <ISuper> <A>)
		(type-annotation-matching <ISub>   <A>)
		(type-annotation-matching <A> <ISuper>)
		(type-annotation-matching <A> <ISub>)))
    => #t #t #f #f
    'exact-match 'exact-match 'no-match 'no-match)

  ;;A record-type implements an interface-type.   <B> implements <ISub>.  <B> and its
  ;;parent <A> have has all the methods in <ISuper> and <ISub>.
  ;;
  (check
      (internal-body
	(define-interface-type <ISuper>
	  (method-prototype super-doit
	    (lambda (<string>) => (<number>))))

	(define-interface-type <ISub>
	  (parent <ISuper>)
	  (method-prototype sub-doit
	    (lambda (<string>) => (<fixnum>))))

	(define-record-type <A>
	  (method ({super-doit <number>} {S <string>})
	    1)
	  #| end of DEFINE-INTERFACE-TYPE |# )

	(define-record-type <B>
	  (parent <A>)
	  (implements <ISub>)
	  (method ({sub-doit <fixnum>} {S <string>})
	    1)
	  #| end of DEFINE-INTERFACE-TYPE |# )

	(values (type-annotation-super-and-sub? <ISuper> <A>)
		(type-annotation-super-and-sub? <ISuper> <B>)
		(type-annotation-super-and-sub? <ISub>   <A>)
		(type-annotation-super-and-sub? <ISub>   <B>)
		(type-annotation-super-and-sub? <A> <ISuper>)
		(type-annotation-super-and-sub? <A> <ISub>)
		(type-annotation-super-and-sub? <B> <ISuper>)
		(type-annotation-super-and-sub? <B> <ISub>)
		(type-annotation-matching <ISuper> <A>)
		(type-annotation-matching <ISuper> <B>)
		(type-annotation-matching <ISub>   <A>)
		(type-annotation-matching <ISub>   <B>)
		(type-annotation-matching <A> <ISuper>)
		(type-annotation-matching <A> <ISub>)
		(type-annotation-matching <B> <ISuper>)
		(type-annotation-matching <B> <ISub>)))
    => #f #t  #f #t  #f #f  #f #f
    'no-match 'exact-match 'no-match 'exact-match
    'no-match 'no-match 'no-match 'no-match)

  ;;A record-type  implements an  interface-type and the  interfaces that  the latter
  ;;implements.
  ;;
  (check
      (internal-body
	(define-interface-type <A>
	  (method-prototype red		(lambda () => (<fixnum>)))
	  #| end of interface |# )

	(define-interface-type <B>
	  (implements <A>)
	  (method-prototype red		(lambda () => (<fixnum>)))
	  (method-prototype blue	(lambda () => (<symbol>)))
	  #| end of interface |# )

	(define-record-type <C>
	  (implements <B>)
	  (method ({red  <fixnum>}) 1)
	  (method ({blue <symbol>}) 'ciao)
	  #| end of interface |# )

	(values (type-annotation-super-and-sub? <A> <B>)
		(type-annotation-super-and-sub? <A> <C>)
		(type-annotation-super-and-sub? <B> <A>)
		(type-annotation-super-and-sub? <B> <C>)
		(type-annotation-super-and-sub? <C> <A>)
		(type-annotation-super-and-sub? <C> <B>)
		))
    => #t #t #f #t #f #f)

  ;;A record-type implements an interface-type.  Implementation checking.
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (nongenerative dummy:<IOne>)
	  (method-prototype doit
	    (lambda (<string>) => (<number>))))

	(define-record-type <blue>
	  (implements <IOne>)
	  (method ({doit <number>} {S <string>})
	    1))

	(values (td::object-type-implements-interface? 'dummy:<IOne> (record-type-descriptor <blue>))
		(td::object-type-implements-interface? (car (type-unique-identifiers <IOne>))
						       (record-type-descriptor <blue>))
		(is-a? (new <blue>) <IOne>)
		(is-a? (cast-signature (<top>) (new <blue>)) <IOne>)))
    => #t #t #t #t)

  ;;A  record-type  implements  an  interface-type and  its  parent.   Implementation
  ;;checking.
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype one
	    (lambda (<string>) => (<number>))))

	(define-interface-type <ITwo>
	  (parent <IOne>)
	  (method-prototype two
	    (lambda (<string>) => (<number>))))

	(define-record-type <blue>
	  (implements <ITwo>)
	  (method ({one <number>} {S <string>})
	    1)
	  (method ({two <number>} {S <string>})
	    1))

	(values (td::object-type-implements-interface? (car (type-unique-identifiers <IOne>)) (record-type-descriptor <blue>))
		(is-a? (new <blue>) <IOne>)
		(is-a? (cast-signature (<top>) (new <blue>)) <IOne>)
		;;
		(td::object-type-implements-interface? (car (type-unique-identifiers <ITwo>)) (record-type-descriptor <blue>))
		(is-a? (new <blue>) <ITwo>)
		(is-a? (cast-signature (<top>) (new <blue>)) <ITwo>)
		))
    => #t #t #t #t #t #t)

;;; --------------------------------------------------------------------
;;; errors

  ;;A record-type fails to implement another interface-type: missing method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method-prototype doit
			(lambda (<string>) => (<number>))))

		    (define-record-type <blue>
		      (implements <IOne>))

		    (type-annotation-super-and-sub? <IOne> <blue>)))
	(catch E
	  ((xp::&interface-implementation-missing-method-violation)
	   (%print-message #f (condition-message E))
	   #t)
	  (else E)))
    => #t)

  ;;A record-type fails to implement another interface-type: mismatching method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method-prototype doit
			(lambda (<string>) => (<number>))))

		    (define-record-type <blue>
		      (implements <IOne>)
		      (method ({doit <symbol>} {S <string>})
			'ciao))

		    (type-annotation-super-and-sub? <IOne> <blue>)))
	(catch E
	  ((xp::&interface-implementation-mismatching-method-violation)
	   (%print-message #f (condition-message E))
	   #t)
	  (else E)))
    => #t)

  (void))


(parametrise ((check-test-name	'record-type-calls))

  ;;The record-type "<blue>" implements the interface-type "<IOne>".
  ;;
  ;;   <blue> +++> <IOne>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype ione-doit
	    (lambda () => (<number>))))

	(define-record-type <blue>
	  (implements <IOne>)
	  (fields val)
	  (method ({ione-doit <number>})
	    (+ 10 (.val this))))

	(define (fun {O <IOne>})
	  (.ione-doit O))

	(fun (new <blue> 1)))
    => 11)

;;; --------------------------------------------------------------------
;;; interface-types parents

  ;;The record-type "<blue>" implements the interface-types "<IOne>" and "<ITwo>".
  ;;
  ;;               <IOne>
  ;;                  ^
  ;;                  |
  ;;   <blue> +++> <ITwo>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype ione-doit
	    (lambda () => (<number>))))

	(define-interface-type <ITwo>
	  (parent <IOne>)
	  (method-prototype itwo-doit
	    (lambda () => (<symbol>))))

	(define-record-type <blue>
	  (implements <ITwo>)
	  (fields val)
	  (method ({ione-doit <number>})
	    (+ 10 (.val this)))
	  (method ({itwo-doit <symbol>})
	    'ciao))

	(define (fun-1 {O <IOne>})
	  (.ione-doit O))

	(define (fun-2 {O <ITwo>})
	  (vector (.ione-doit O)
		  (.itwo-doit O)))

	(define O
	  (new <blue> 1))

	(values (fun-1 O) (fun-2 O)))
    => 11 '#(11 ciao))

  ;;The record-type  "<blue>" implements  the interface-types "<IOne>"  and "<ITwo>".
  ;;"<blue>" implements the "composite" method from "<IOne>" and "<ITwo>".
  ;;
  ;;               <IOne>
  ;;                  ^
  ;;                  |
  ;;   <blue> +++> <ITwo>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype doit
	    (lambda (<fixnum>) => (<string>))))

	(define-interface-type <ITwo>
	  (parent <IOne>)
	  (method-prototype doit
	    (lambda (<flonum>) => (<string>))))

	(define-record-type <blue>
	  (implements <ITwo>)
	  (method ({doit <string>} {O <fixnum>})
	    (fixnum->string O))
	  (method ({doit <string>} {O <flonum>})
	    (flonum->string O)))

	(define (fun-1 {O <IOne>})
	  (.doit O 123))

	(define (fun-2 {O <ITwo>})
	  (vector (.doit O 123)
		  (.doit O 4.5)))

	(define O
	  (new <blue>))

	(values (fun-1 O) (fun-2 O)))
    => "123" '#("123" "4.5"))

  ;;Both  the record-types  "<blue>" and  "<dark-blue>" implement  the interface-type
  ;;"<IOne>".  For  instances of  "<dark-blue>": the implementation  in "<dark-blue>"
  ;;shadows the one in "<blue>".
  ;;
  ;;   <blue> +++> <IOne>
  ;;      ^          ^
  ;;      |          +
  ;;   <dark-blue> +++
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype ione-doit
	    (lambda () => (<number>))))

	(define-record-type <blue>
	  (implements <IOne>)
	  (method ({ione-doit <number>})
	    11))

	(define-record-type <dark-blue>
	  (parent <blue>)
	  (implements <IOne>)
	  (method ({ione-doit <number>})
	    22))

	(define (fun {O <IOne>})
	  (.ione-doit O))

	(define O
	  (new <blue>))

	(define P
	  (new <dark-blue>))

	(values (fun O) (fun P)))
    => 11 22)

;;; --------------------------------------------------------------------
;;; record-type parents

  ;;The record-type "<dark-blue>" inherits  the implementation of the interface-types
  ;;"<IOne>" and "<ITwo>".
  ;;
  ;;               <IOne>
  ;;                  ^
  ;;                  |
  ;;   <blue> +++> <ITwo>
  ;;     ^
  ;;     |
  ;;   <dark-blue>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype ione-doit
	    (lambda () => (<number>))))

	(define-interface-type <ITwo>
	  (parent <IOne>)
	  (method-prototype itwo-doit
	    (lambda () => (<symbol>))))

	(define-record-type <blue>
	  (implements <ITwo>)
	  (fields val)
	  (method ({ione-doit <number>})
	    (+ 10 (.val this)))
	  (method ({itwo-doit <symbol>})
	    'ciao))

	(define-record-type <dark-blue>
	  (parent <blue>))

	(define (fun-1 {O <IOne>})
	  (.ione-doit O))

	(define (fun-2 {O <ITwo>})
	  (vector (.ione-doit O)
		  (.itwo-doit O)))

	(define O
	  (new <dark-blue> 1))

	(values (fun-1 O) (fun-2 O)))
    => 11 '#(11 ciao))

  ;;The record-type "<dark-blue>" implements the interface-type "<ITwo>" and inherits
  ;;from "<blue>" the implementation of the interface-type "<IOne>".
  ;;
  ;;   <blue> +++> <IOne>
  ;;     ^
  ;;     |
  ;;   <dark-blue> +++> <ITwo>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype ione-doit
	    (lambda () => (<number>))))

	(define-interface-type <ITwo>
	  (method-prototype itwo-doit
	    (lambda () => (<symbol>))))

	(define-record-type <blue>
	  (implements <IOne>)
	  (fields val)
	  (method ({ione-doit <number>})
	    (+ 10 (.val this))))

	(define-record-type <dark-blue>
	  (parent <blue>)
	  (implements <ITwo>)
	  (method ({itwo-doit <symbol>})
	    'ciao))

	(define (fun-1 {O <IOne>})
	  (.ione-doit O))

	(define (fun-2 {O <ITwo>})
	  (.itwo-doit O))

	(define O
	  (new <dark-blue> 1))

	(values (fun-1 O) (fun-2 O)))
    => 11 'ciao)

;;; --------------------------------------------------------------------
;;; interface-types implementations

  ;;The record-type "<blue>" implements the interface-types "<IOne>" and "<ITwo>".
  ;;
  ;;   <blue> +++> <ITwo> +++> <IOne>
  ;;
  (check
      (internal-body
	(define-interface-type <IOne>
	  (method-prototype ione-doit
	    (lambda () => (<number>))))

	(define-interface-type <ITwo>
	  (implements <IOne>)
	  (method-prototype ione-doit
	    (lambda () => (<number>)))
	  (method-prototype itwo-doit
	    (lambda () => (<symbol>))))

	(define-record-type <blue>
	  (implements <ITwo>)
	  (fields val)
	  (method ({ione-doit <number>})
	    (+ 10 (.val this)))
	  (method ({itwo-doit <symbol>})
	    'ciao))

	(define (fun-1 {O <IOne>})
	  (.ione-doit O))

	(define (fun-2 {O <ITwo>})
	  (vector (.ione-doit O)
		  (.itwo-doit O)))

	(define O
	  (new <blue> 1))

	(values (fun-1 O) (fun-2 O)))
    => 11 '#(11 ciao))

  ;;The  record-type "<blue>"  implements  the interface-types  "<ITwo>", its  parent
  ;;"<IOne>", and automatically the interface "<IThree>" implemented by "<IOne>".
  ;;
  ;;               <IOne> +++> <IThree>
  ;;                  ^
  ;;                  |
  ;;   <blue> +++> <ITwo>
  ;;
  (check
      (internal-body
	(define-interface-type <IThree>
	  (method-prototype ithree-doit
	    (lambda () => (<string>))))

	(define-interface-type <IOne>
	  (implements <IThree>)
	  (method-prototype ione-doit
	    (lambda () => (<number>)))
	  (method-prototype ithree-doit
	    (lambda () => (<string>))))

	(define-interface-type <ITwo>
	  (parent <IOne>)
	  (method-prototype itwo-doit
	    (lambda () => (<symbol>))))

	(define-record-type <blue>
	  (implements <ITwo>)
	  (fields val)
	  (method ({ione-doit <number>})
	    (+ 10 (.val this)))
	  (method ({itwo-doit <symbol>})
	    'ciao)
	  (method ({ithree-doit <string>})
	    "hello"))

	(define (fun-1 {O <IOne>})
	  (vector (.ione-doit O)
		  (.ithree-doit O)))

	(define (fun-2 {O <ITwo>})
	  (vector (.ione-doit O)
		  (.itwo-doit O)
		  (.ithree-doit O)))

	(define (fun-3 {O <IThree>})
	  (.ithree-doit O))

	(define O
	  (new <blue> 1))

	(values (fun-1 O) (fun-2 O) (fun-3 O)))
    => '#(11 "hello") '#(11 ciao "hello") "hello")

  (void))


(parametrise ((check-test-name	'record-type-errors))

  ;;Attempt to instantiate interface.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <Arith>
		      (method-prototype add
			(lambda () => (<number>))))

		    (new <Arith>)))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '<Arith>)

  ;;Run-time type validation failure caused by APPLY.
  ;;
  (check
      (internal-body
	(define-interface-type <Arith>
	  (method-prototype add
	    (lambda () => (<number>))))

	(define-record-type <duo>
	  (implements <Arith>)
	  (fields one two)
	  (method ({add <number>})
	    (+ (.one this) (.two this))))

	(define (fun {O <Arith>})
	  (.add O))

	(apply fun (list (new <duo>  1 2))))
    => 3)

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
		    (define-interface-type <Arith>
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
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '<fixnum>)

  (void))


(parametrise ((check-test-name	'default-methods))

  ;;The record-type  "<duo>" implements the  interface-type "<Stringer>" which  has a
  ;;default method TO-STRING.
  ;;
  (check
      (internal-body
	(define-interface-type <Stringer>
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

	(fun (new <duo> 1 2)))
    => "#[duo 1 2]")

  ;;The record-type  "<duo>" implements the  interface-type "<Stringer>" which  has a
  ;;default method TO-STRING.  "<duo>" implements the method by itself.
  ;;
  (check
      (internal-body
	(define-interface-type <Stringer>
	  (method ({to-string <string>})
	    (with-output-to-string
	      (lambda ()
		(display this)))))

	(define-record-type <duo>
	  (implements <Stringer>)
	  (fields one two)
	  (method ({to-string <string>})
	    (with-output-to-string
	      (lambda ()
		(display "#[duo ")
		(display (.one this))
		(display #\space)
		(display (.two this))
		(display #\])))))

	(define (fun {O <Stringer>})
	  (.to-string O))

	(fun (new <duo>  1 2)))
    => "#[duo 1 2]")

  ;;The record-type  "<duo>" implements the  interface-type "<Stringer>" which  has a
  ;;default   method  TO-STRING.    The  default   method  implementation   calls  an
  ;;interface-type's method implemented by the record-type.
  ;;
  (check
      (internal-body
	(define-interface-type <Stringer>
	  (method-prototype value
	    (lambda () => (<top>)))
	  (method (doit)
	    (.value this)))

	(define-record-type <duo>
	  (implements <Stringer>)
	  (fields one two)
	  (method ({value <top>})
	    (.one this)))

	(define (fun {O <Stringer>})
	  (.doit O))

	(fun (new <duo> 1 2)))
    => 1)

;;; --------------------------------------------------------------------
;;; errors

  ;;The  record-type "<blue>"  implements  the interface-type  "<IOne>"  which has  a
  ;;default method doit-ione.   The default method implementation attempts  to call a
  ;;method from the record-type.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method (doit-ione)
			(.val this)))

		    (define-record-type <duo>
		      (implements <IOne>)
		      (fields val))

		    (define (fun {O <IOne>})
		      (.doit-ione O))

		    (fun (new <blue> 1))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-form E)))
	  (else E)))
    => '(method-call val this))

  ;;The  interface  type  "<ITwo>"  inherits  from "<IOne>"  a  method  with  default
  ;;implementation and tries to extend it with a method prototype.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method ({doit <number>})
			1))

		    (define-interface-type <ITwo>
		      (parent <IOne>)
		      (method-prototype doit
			(lambda (<string>) => (<number>))))

		    (void)))
	(catch E
	  ((&syntax)
	   #;(print-condition E)
	   (%print-message #f (condition-message E))
	   (syntax-violation-subform E))
	  (else E)))
    => 'doit)

  ;;The  interface  type  "<ITwo>"  inherits  from "<IOne>"  a  method  with  default
  ;;implementation and tries to extend it with a method prototype.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method-prototype doit
			(lambda (<string>) => (<number>))))

		    (define-interface-type <ITwo>
		      (parent <IOne>)
		      (method ({doit <number>})
			1))

		    (void)))
	(catch E
	  ((&syntax)
	   #;(print-condition E)
	   (%print-message #f (condition-message E))
	   (syntax-violation-subform E))
	  (else E)))
    => 'doit)

  ;;The  interface type  "<ITwo>"  inherits  from "<IOne>"  a  method having  default
  ;;implementation and tries to  extend it with and tries to extend  it with a method
  ;;having default implementation.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-interface-type <IOne>
		      (method ({doit <number>})
			1))

		    (define-interface-type <ITwo>
		      (parent <IOne>)
		      (method ({doit <number>} {S <string>})
			2))

		    (void)))
	(catch E
	  ((&syntax)
	   #;(print-condition E)
	   (%print-message #f (condition-message E))
	   (syntax-violation-subform E))
	  (else E)))
    => 'doit)

  (void))


(parametrise ((check-test-name	'multiple-implementations))

  ;;Two record-types in a hierarchy both implement the same interface.
  ;;
  (check
      (internal-body
	(define-interface-type <Arith>
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
	(define-interface-type <Arith>
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
	(define-interface-type <Arith>
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
	(define-interface-type <Stuff>
	  (method-prototype red
	    (lambda () => (<top>))))

	(define itd
	  (type-descriptor <Stuff>))

	(values (td::interface-type-descr? itd)
		(td::interface-type-descr.type-name itd)))
    => #t '<Stuff>)

  (check
      (internal-body
	(define-interface-type <Stuff>
	  (method-prototype red
	    (lambda () => (<top>))))

	(define itd
	  (type-descriptor <Stuff>))

	(procedure? (td::interface-type-descr.method-retriever itd)))
    => #t)

  (void))


(parametrise ((check-test-name	'instantiable-bodies))

  ;;Generic interfaces through instantiable bodies.
  ;;
  (check
      (internal-body
	(define-instantiable-body define-iface-arith
	  (define-interface-type <Iface>
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
	  (define-interface-type <Iface>
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

    (define-interface-type <Sequence>
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
