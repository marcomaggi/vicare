;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <procedure> type
;;;Date: Mon Apr  4, 2016
;;;
;;;Abstract
;;;
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
(program (test-types-procedure-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <procedure> objects\n")


(parametrise ((check-test-name	'predicate))

;;; type predicate

  (check-for-true	(is-a? display <procedure>))
  (check-for-true	(is-a? (lambda () #t) <procedure>))
  (check-for-true	(let ((O display))
			  (is-a? O <procedure>)))
  (check-for-true	(let ((O (lambda () #t)))
			  (is-a? O <procedure>)))

  (check-for-true	(let (({O <procedure>} display))
			  (is-a? O <procedure>)))

  (check-for-false	(is-a? 123 <procedure>))

  (void))


(parametrise ((check-test-name	'closure-type-spec))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({a <fixnum>}) a)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ <fixnum>} {a <fixnum>}) a)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ <fixnum> <string>} {a <fixnum>} {b <string>})
  							 (values a b))))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> <string>) => (<fixnum> <string>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
  							 (list a b))))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> <fixnum>) => ((list-of <fixnum>)))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ . (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
  							 (values a b))))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> <fixnum>) => (list-of <fixnum>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({a <fixnum>} . {rest (list-of <fixnum>)})
  							 (values a rest))))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> . (list-of <fixnum>)) => (<fixnum> (list-of <fixnum>)))))

;;; --------------------------------------------------------------------

  (check
      (expander::type-signature.syntax-object (type-of (case-lambda
							 (({a <fixnum>})
							  a)
							 (({a <fixnum>} {b <string>})
							  (list a b)))))
    (=> expander::syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> ((list <fixnum> <string>))))))

  (check
      (expander::type-signature.syntax-object (type-of (case-lambda
							 (({_ <fixnum> <false>} {a <fixnum>})
							  (values a #f))
							 (({_ <fixnum> <string>} {a <fixnum>} {b <string>})
							  (values a b)))))
    (=> expander::syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum> <false>))
	 ((<fixnum> <string>)	=> (<fixnum> <string>)))))

  (check
      (expander::type-signature.syntax-object (type-of (case-lambda
							 (({_ <fixnum>} {a <fixnum>})
							  a)
							 (({_ (list <fixnum> <string>)} {a <fixnum>} {b <string>})
							  (list a b)))))
    (=> expander::syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> ((list <fixnum> <string>))))))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define (fun {a <fixnum>}) a)
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (internal-body
	(define ({fun <fixnum>} {a <fixnum>}) a)
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (internal-body
	(define ({fun <fixnum> <string>} {a <fixnum>} {b <string>})
	  (values a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> <string>) => (<fixnum> <string>))))

  (check
      (internal-body
	(define ({fun (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
	  (list a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> <fixnum>) => ((list-of <fixnum>)))))

  (check
      (internal-body
	(define ({fun . (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
	  (values a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> <fixnum>) => (list-of <fixnum>))))

  (check
      (internal-body
	(define (fun {a <fixnum>} . {rest (list-of <fixnum>)})
	  (values a rest))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum> . (list-of <fixnum>)) => (<fixnum> (list-of <fixnum>)))))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(case-define fun
	  (({a <fixnum>})
	   a)
	  (({a <fixnum>} {b <string>})
	   (list a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> ((list <fixnum> <string>))))))

  (check
      (internal-body
	(case-define fun
	  (({_ <fixnum>} {a <fixnum>})
	   a))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((lambda (<fixnum>)		=> (<fixnum>))))

  (check
      (internal-body
	(case-define fun
	  (({_ <fixnum> <false>} {a <fixnum>})
	   (values a #f))
	  (({_ <fixnum> <string>} {a <fixnum>} {b <string>})
	   (values a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum> <false>))
	 ((<fixnum> <string>)	=> (<fixnum> <string>)))))

  (check
      (internal-body
	(case-define fun
	  (({_ <fixnum>} {a <fixnum>})
	   a)
	  (({_ (list <fixnum> <string>)} {a <fixnum>} {b <string>})
	   (list a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> expander::syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> ((list <fixnum> <string>))))))

  (void))


(parametrise ((check-test-name	'super-and-sub))

  (define-syntax-rule (doit-true ?super ?sub)
    (check-for-true	(type-annotation-super-and-sub? ?super ?sub)))

  (define-syntax-rule (doit-false ?super ?sub)
    (check-for-false	(type-annotation-super-and-sub? ?super ?sub)))

;;; --------------------------------------------------------------------

  (doit-true	<procedure>	<procedure>)
  (doit-true	<procedure>	(lambda <list> => (<fixnum>)))

  (doit-false	(lambda <list> => (<fixnum>))		<procedure>)
  (doit-false	(lambda <list> => (<fixnum>))		<fixnum>)

;;; --------------------------------------------------------------------

  (doit-true	(lambda <list> => (<fixnum>))	(lambda <list> => (<fixnum>)))
  (doit-true	(lambda <list> => (<number>))	(lambda <list> => (<fixnum>)))
  (doit-false	(lambda <list> => (<fixnum>))	(lambda <list> => (<number>)))

  (doit-true	(lambda (<fixnum>) => <list>)	(lambda (<fixnum>) => <list>))
  (doit-false	(lambda (<number>) => <list>)	(lambda (<fixnum>) => <list>))
  (doit-true	(lambda (<fixnum>) => <list>)	(lambda (<number>) => <list>))

  (doit-true	(lambda (<fixnum>) => (<fixnum>))	(lambda (<fixnum>) => (<fixnum>)))
  (doit-false	(lambda (<number>) => (<number>))	(lambda (<fixnum>) => (<fixnum>)))
  (doit-false	(lambda (<fixnum>) => (<fixnum>))	(lambda (<number>) => (<number>)))
  (doit-true	(lambda (<fixnum>) => (<number>))	(lambda (<number>) => (<number>)))
  (doit-false	(lambda (<number>) => (<fixnum>))	(lambda (<number>) => (<number>)))

  (doit-false	(lambda (<fixnum>) => (<string>))	(lambda (<fixnum>) => (<fixnum>)))
  (doit-false	(lambda (<string>) => (<fixnum>))	(lambda (<fixnum>) => (<fixnum>)))
  (doit-false	(lambda (<top>)    => (<top>))	(lambda (<fixnum>) => (<pair>)))

;;; --------------------------------------------------------------------

  (doit-true	<procedure>
		(case-lambda
		  ((<fixnum>) => (<fixnum>))))

  (doit-false	(case-lambda
		  ((<fixnum>) => (<fixnum>)))
		<procedure>)

  (doit-true	(lambda (<fixnum>) => (<string>))
		(case-lambda
		  ((<fixnum>) => (<string>))
		  ((<flonum>) => (<string>))))

  (doit-true	(lambda (<flonum>) => (<string>))
		(case-lambda
		  ((<fixnum>) => (<string>))
		  ((<flonum>) => (<string>))))

;;;

  (doit-false (case-lambda
		((<fixnum>) => (<string>))
		((<flonum>) => (<string>)))
	      (lambda (<fixnum>) => (<string>)))

  (doit-false (case-lambda
		((<fixnum>) => (<string>))
		((<flonum>) => (<string>)))
	      (lambda (<flonum>) => (<string>)))

  (doit-false (case-lambda
		((<fixnum>) => (<string>))
		((<flonum>) => (<string>)))
	      (lambda (<string>) => (<fixnum>)))

  (doit-true (case-lambda
	       ((<fixnum>)	=> (<boolean>))
	       ((<vector>)	=> (<boolean>)))
	     (case-lambda
	       ((<bignum>)	=> (<boolean>))
	       ((<integer>)	=> (<boolean>))
	       ((<vector>)	=> (<boolean>))))

  (doit-false (case-lambda
		((<top>)		=> (<pair>))
		((<top>)		=> (<real>))
		((<top>)		=> (<vector>)))
	      (case-lambda
		((<top>)		=> (<fixnum>))
		((<top>)		=> (<flonum>))
		((<top>)		=> ((vector-of <true>)))))

  (doit-false (case-lambda
		((<top>)		=> (<pair>))
		((<top>)		=> (<real>))
		((<top>)		=> (<vector>)))
	      (case-lambda
		((<top>)		=> (<fixnum>))
		((<top>)		=> (<flonum>))
		((<top>)		=> (<transcoder>)) ;;this does not match
		((<top>)		=> ((vector-of <true>)))))

;;; --------------------------------------------------------------------
;;; special procedures types: <thunk>

  (doit-true	<thunk>		(lambda () => <list>))
  (doit-true	<thunk>		(case-lambda
				  (() => <list>)))
  (doit-true	<thunk>		(case-lambda
				  (()		=> (<string>))
				  ((<fixnum>)	=> (<string>))))
  (doit-true	<thunk>		(case-lambda
				  ((<fixnum>)	=> (<string>))
				  (()		=> (<string>))))

;;; --------------------------------------------------------------------
;;; special procedures types: <type-predicate>

  ;; <type-predicate> == (lambda (<top>) => (<boolean>))

  (doit-true	<type-predicate>	(lambda (<top>)	=> (<boolean>)))
  (doit-false	<type-predicate>	(lambda (<top>)	=> (<string>)))
  (doit-false	<type-predicate>	(lambda (<top>)	=> (<top>)))
  (doit-true	<type-predicate>	(lambda (<top>)	=> (<true>)))
  (doit-true	<type-predicate>	(lambda (<top>)	=> (<false>)))
  (doit-false	<type-predicate>	(lambda (<string>)	=> (<boolean>)))

  (doit-true	(lambda (<top>)	=> (<boolean>))		<type-predicate>)
  (doit-false	(lambda (<top>)	=> (<string>))		<type-predicate>)
  (doit-true	(lambda (<top>)	=> (<top>))		<type-predicate>)
  (doit-false	(lambda (<top>)	=> (<true>))		<type-predicate>)
  (doit-false	(lambda (<top>)	=> (<false>))		<type-predicate>)
  (doit-true	(lambda (<string>)	=> (<boolean>))		<type-predicate>)

  (doit-true	<type-predicate>	(case-lambda
					  ((<top>)	=> (<boolean>))
					  ((<string>)	=> (<string>))))
  (doit-true	<type-predicate>	(case-lambda
					  ((<string> <number>)	=> (<string>))
					  ((<top>)		=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; special procedures types: type-predicate

  ;;Type type annotation:
  ;;
  ;;   (type-predicate <fixnum>)
  ;;
  ;;is expanded to:
  ;;
  ;;   (lambda (<fixnum>) => (<boolean>))

  (doit-true	(lambda (<bottom>)	=> (<boolean>))		(lambda (<fixnum>)	=> (<true>)))
  (doit-false	(lambda (<fixnum>)	=> (<true>))		(lambda (<bottom>)	=> (<boolean>)))

  (doit-true	(lambda (<bottom>)	=> (<boolean>))		(lambda (<top>)	=> (<boolean>)))
  (doit-true	(lambda (<fixnum>)	=> (<boolean>))		(lambda (<top>)	=> (<boolean>)))

  (doit-false	(lambda (<top>)	=> (<boolean>))		(lambda (<bottom>)	=> (<boolean>)))
  (doit-false	(lambda (<top>)	=> (<boolean>))		(lambda (<fixnum>)	=> (<boolean>)))

  (doit-false	(case-lambda
		  ((<fixnum>)	=> (<true>))
		  ((<bottom>)	=> (<boolean>)))
		(lambda (<top>)	=> (<boolean>)))

  (doit-true	(case-lambda
		  ((<fixnum>)	=> (<true>))
		  ((<bottom>)	=> (<boolean>)))
		(lambda (<fixnum>)	=> (<true>)))

  (doit-true	(case-lambda
		  ((<fixnum>)	=> (<true>))
		  ((<bottom>)	=> (<boolean>)))
		(case-lambda
		  ((<fixnum>)	=> (<true>))
		  ((<bottom>)	=> (<boolean>))))

  (doit-true	(lambda (<bottom>) => (<boolean>))		(type-predicate <fixnum>))
  (doit-false	(type-predicate <fixnum>)		(lambda (<bottom>) => (<boolean>)))
  (doit-false	(lambda (<top>) => (<boolean>))		(type-predicate <fixnum>))
  (doit-true	(type-predicate <fixnum>)		(lambda (<top>) => (<boolean>)))
  (doit-true	(type-predicate <fixnum>)		(type-predicate <fixnum>))
  (doit-true	(type-predicate <fixnum>)		(type-predicate <number>))
  (doit-false	(type-predicate <number>)		(type-predicate <fixnum>))

;;; --------------------------------------------------------------------
;;; special procedures types: <type-destructor>

  (doit-true	<type-destructor>		(lambda (<top>) => (<boolean>)))
  (doit-true	<type-destructor>		(lambda (<top>) => <list>))
  (doit-true	<type-destructor>		(case-lambda
						  ((<top> <top>)	=> (<top>))
						  ((<top>)		=> <list>)))

;;; --------------------------------------------------------------------
;;; special procedures types: <type-printer>

  (doit-true	<type-printer>		(lambda (<top> <textual-output-port> <procedure>) => <list>))
  (doit-true	<type-printer>		(lambda (<fixnum> <textual-output-port> <procedure>) => (<void>)))

;;; --------------------------------------------------------------------
;;; special procedures types: <equality-predicate>

  (doit-true	<equality-predicate>	(lambda (<top>    <top>)	=> (<boolean>)))
  (doit-true	<equality-predicate>	(lambda (<string> <string>)	=> (<boolean>)))
  (doit-false	<equality-predicate>	(lambda (<top>    <top>)	=> (<top>)))
  (doit-false	<equality-predicate>	(lambda (<top> <top> <top>)	=> (<boolean>)))

;;; --------------------------------------------------------------------
;;; special procedures types: equality-predicate

  (doit-true	(equality-predicate <fixnum>)	(lambda (<fixnum>    <fixnum>)	=> (<boolean>)))
  (doit-true	(equality-predicate <string>)	(lambda (<string>	<string>)	=> (<boolean>)))
  (doit-false	(equality-predicate <fixnum>)	(lambda (<fixnum>    <fixnum>)	=> (<top>)))
  (doit-false	(equality-predicate <fixnum>)	(lambda (<fixnum> <fixnum> <fixnum>)	=> (<boolean>)))

;;; --------------------------------------------------------------------
;;; special procedures types: <comparison-procedure>

  (doit-true	<comparison-procedure>	(lambda (<top>    <top>)	=> (<fixnum>)))
  (doit-true	<comparison-procedure>	(lambda (<string> <string>)	=> (<fixnum>)))
  (doit-false	<comparison-procedure>	(lambda (<top>    <top>)	=> (<top>)))
  (doit-false	<comparison-procedure>	(lambda (<top> <top> <top>)	=> (<fixnum>)))

;;; --------------------------------------------------------------------
;;; special procedures types: comparison-procedure

  (doit-true	(comparison-procedure <fixnum>)	(lambda (<fixnum> <fixnum>)	=> (<fixnum>)))
  (doit-true	(comparison-procedure <string>)	(lambda (<string> <string>)	=> (<fixnum>)))
  (doit-false	(comparison-procedure <fixnum>)	(lambda (<fixnum> <fixnum>)	=> (<top>)))
  (doit-false	(comparison-procedure <fixnum>)	(lambda (<fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))

;;; --------------------------------------------------------------------
;;; special procedures types: <hash-function>

  (doit-true	<hash-function>		(lambda (<top>)		=> (<non-negative-fixnum>)))
  (doit-true	<hash-function>		(lambda (<string>)		=> (<non-negative-fixnum>)))
  (doit-false	<hash-function>		(lambda (<top>)		=> (<fixnum>)))
  (doit-false	<hash-function>		(lambda (<top> <top>)	=> (<non-negative-fixnum>)))

;;; --------------------------------------------------------------------
;;; special procedures types: hash-function

  (doit-true	(hash-function <fixnum>)	(lambda (<fixnum>)		=> (<non-negative-fixnum>)))
  (doit-true	(hash-function <string>)	(lambda (<string>)		=> (<non-negative-fixnum>)))
  (doit-false	(hash-function <fixnum>)	(lambda (<top>)		=> (<fixnum>)))
  (doit-false	(hash-function <fixnum>)	(lambda (<top> <top>)	=> (<non-negative-fixnum>)))

;;; --------------------------------------------------------------------
;;; special procedures types: <type-method-retriever>

  (doit-true	<type-method-retriever>		(lambda (<symbol>)	=> (<procedure>)))
  (doit-true	<type-method-retriever>		(lambda (<symbol>)	=> ((or <false> <procedure>))))
  (doit-true	<type-method-retriever>		(lambda (<top>)	=> (<procedure>)))
  (doit-true	<type-method-retriever>		(lambda (<top>)	=> ((or <false> <procedure>))))

  (doit-false	<type-method-retriever>		(lambda (<string>)	=> ((or <false> <procedure>))))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
