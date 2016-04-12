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
    (=> syntax=?)
    #'((lambda (<fixnum>) => <list>)))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ <fixnum>} {a <fixnum>}) a)))
    (=> syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ <fixnum> <string>} {a <fixnum>} {b <string>})
  							 (values a b))))
    (=> syntax=?)
    #'((lambda (<fixnum> <string>) => (<fixnum> <string>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
  							 (list a b))))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => ((list-of <fixnum>)))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ . (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
  							 (values a b))))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => (list-of <fixnum>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({a <fixnum>} . {rest (list-of <fixnum>)})
  							 (values a rest))))
    (=> syntax=?)
    #'((lambda (<fixnum> . (list-of <fixnum>)) => <list>)))

;;; --------------------------------------------------------------------

  (check
      (expander::type-signature.syntax-object (type-of (case-lambda
							 (({a <fixnum>})
							  a)
							 (({a <fixnum>} {b <string>})
							  (list a b)))))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> <list>)
	 ((<fixnum> <string>)	=> <list>))))

  (check
      (expander::type-signature.syntax-object (type-of (case-lambda
							 (({_ <fixnum>} {a <fixnum>})
							  a)
							 (({_ <fixnum> <string>} {a <fixnum>} {b <string>})
							  (values a b)))))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> (<fixnum> <string>)))))

  (check
      (expander::type-signature.syntax-object (type-of (case-lambda
							 (({_ <fixnum>} {a <fixnum>})
							  a)
							 (({_ (list <fixnum> <string>)} {a <fixnum>} {b <string>})
							  (list a b)))))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> ((list <fixnum> <string>))))))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define (fun {a <fixnum>}) a)
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum>) => <list>)))

  (check
      (internal-body
	(define ({fun <fixnum>} {a <fixnum>}) a)
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (internal-body
	(define ({fun <fixnum> <string>} {a <fixnum>} {b <string>})
	  (values a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> <string>) => (<fixnum> <string>))))

  (check
      (internal-body
	(define ({fun (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
	  (list a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => ((list-of <fixnum>)))))

  (check
      (internal-body
	(define ({fun . (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
	  (values a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => (list-of <fixnum>))))

  (check
      (internal-body
	(define (fun {a <fixnum>} . {rest (list-of <fixnum>)})
	  (values a rest))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> . (list-of <fixnum>)) => <list>)))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(case-define fun
	  (({a <fixnum>})
	   a)
	  (({a <fixnum>} {b <string>})
	   (list a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> <list>)
	 ((<fixnum> <string>)	=> <list>))))

  (check
      (internal-body
	(case-define fun
	  (({_ <fixnum>} {a <fixnum>})
	   a))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum>)		=> (<fixnum>))))

  (check
      (internal-body
	(case-define fun
	  (({_ <fixnum>} {a <fixnum>})
	   a)
	  (({_ <fixnum> <string>} {a <fixnum>} {b <string>})
	   (values a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> (<fixnum> <string>)))))

  (check
      (internal-body
	(case-define fun
	  (({_ <fixnum>} {a <fixnum>})
	   a)
	  (({_ (list <fixnum> <string>)} {a <fixnum>} {b <string>})
	   (list a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> ((list <fixnum> <string>))))))

  (void))


(parametrise ((check-test-name	'super-and-sub))

  (check-for-true	(type-annotation-super-and-sub? <procedure>
							<procedure>))

  (check-for-true	(type-annotation-super-and-sub? <procedure>
							(lambda <list> => (<fixnum>))))

  (check-for-false	(type-annotation-super-and-sub? (lambda <list> => (<fixnum>))
							<procedure>))

  (check-for-false	(type-annotation-super-and-sub? (lambda <list> => (<fixnum>))
							<fixnum>))

;;; --------------------------------------------------------------------

  (check-for-true	(type-annotation-super-and-sub? (lambda <list> => (<fixnum>))
							(lambda <list> => (<fixnum>))))

  (check-for-true	(type-annotation-super-and-sub? (lambda <list> => (<number>))
							(lambda <list> => (<fixnum>))))

  (check-for-false	(type-annotation-super-and-sub? (lambda <list> => (<fixnum>))
							(lambda <list> => (<number>))))

;;;

  (check-for-true	(type-annotation-super-and-sub? (lambda (<fixnum>) => <list>)
							(lambda (<fixnum>) => <list>)))

  (check-for-true	(type-annotation-super-and-sub? (lambda (<number>) => <list>)
							(lambda (<fixnum>) => <list>)))

  (check-for-false	(type-annotation-super-and-sub? (lambda (<fixnum>) => <list>)
							(lambda (<number>) => <list>)))

;;;

  (check-for-true	(type-annotation-super-and-sub? (lambda (<fixnum>) => (<fixnum>))
							(lambda (<fixnum>) => (<fixnum>))))

  (check-for-true	(type-annotation-super-and-sub? (lambda (<number>) => (<number>))
							(lambda (<fixnum>) => (<fixnum>))))

  (check-for-false	(type-annotation-super-and-sub? (lambda (<fixnum>) => (<fixnum>))
							(lambda (<number>) => (<number>))))

  (check-for-false	(type-annotation-super-and-sub? (lambda (<fixnum>) => (<number>))
							(lambda (<number>) => (<number>))))

  (check-for-false	(type-annotation-super-and-sub? (lambda (<number>) => (<fixnum>))
							(lambda (<number>) => (<number>))))

;;;

  (check-for-false	(type-annotation-super-and-sub? (lambda (<fixnum>) => (<string>))
							(lambda (<fixnum>) => (<fixnum>))))

  (check-for-false	(type-annotation-super-and-sub? (lambda (<string>) => (<fixnum>))
							(lambda (<fixnum>) => (<fixnum>))))

  (check-for-true	(type-annotation-super-and-sub? (lambda (<top>) => (<top>))
							(lambda (<fixnum>) => (<pair>))))

;;; --------------------------------------------------------------------

  (check-for-true	(type-annotation-super-and-sub? <procedure>
							(case-lambda
							  ((<fixnum>) => (<fixnum>)))))

  (check-for-false	(type-annotation-super-and-sub? (case-lambda
							  ((<fixnum>) => (<fixnum>)))
							<procedure>))

;;;

  (check-for-true	(type-annotation-super-and-sub? (case-lambda
							  ((<fixnum>) => (<string>))
							  ((<flonum>) => (<string>)))
							(lambda (<fixnum>) => (<string>))))

  (check-for-true	(type-annotation-super-and-sub? (case-lambda
							  ((<fixnum>) => (<string>))
							  ((<flonum>) => (<string>)))
							(lambda (<flonum>) => (<string>))))

  (check-for-false	(type-annotation-super-and-sub? (case-lambda
							  ((<fixnum>) => (<string>))
							  ((<flonum>) => (<string>)))
							(lambda (<string>) => (<fixnum>))))

  (check-for-true	(type-annotation-super-and-sub? (case-lambda
							  ((<real>)	=> (<boolean>))
							  ((<vector>)	=> (<boolean>)))
							(case-lambda
							  ((<fixnum>)	=> (<boolean>))
							  ((<flonum>)	=> (<boolean>))
							  ((<vector>)	=> (<boolean>)))))

  (check-for-true	(type-annotation-super-and-sub? (case-lambda
							  ((<top>)		=> (<pair>))
							  ((<top>)		=> (<real>))
							  ((<top>)		=> (<vector>)))
							(case-lambda
							  ((<top>)		=> (<fixnum>))
							  ((<top>)		=> (<flonum>))
							  ((<top>)		=> ((vector-of <true>))))))

  (check-for-false	(type-annotation-super-and-sub? (case-lambda
							  ((<top>)		=> (<pair>))
							  ((<top>)		=> (<real>))
							  ((<top>)		=> (<vector>)))
							(case-lambda
							  ((<top>)		=> (<fixnum>))
							  ((<top>)		=> (<flonum>))
							  ((<top>)		=> (<transcoder>)) ;;this does not match
							  ((<top>)		=> ((vector-of <true>))))))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
