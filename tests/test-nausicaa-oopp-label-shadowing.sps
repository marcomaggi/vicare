;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for label identifier shadowing
;;;Date: Fri Jul  1, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (except (nausicaa)
		define-condition-type
		&warning)
  (prefix (only (rnrs)
		define-condition-type
		&warning)
	  rnrs.)
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing label shadowing\n")


(parametrise ((check-test-name	'base))

  (define-syntax define-condition-type
    (lambda (stx)
      (syntax-case stx ()
	((_ ?type ?supertype ?constructor ?predicate (?field ?accessor) ...)
	 #'(begin
	     (with-label-shadowing (?supertype)
	       (rnrs.define-condition-type the-type
		 ?supertype ?constructor ?predicate
		 (?field ?accessor) ...))
	     (define-label ?type
	       (shadows the-type)
	       (predicate ?predicate)
	       (virtual-fields (immutable ?field ?accessor) ...)))))))

  (define-label &warning
    (predicate warning?)
    (shadows rnrs.&warning))

;;; --------------------------------------------------------------------

  (let ()
    (define-condition-type &alpha
      &warning make-alpha alpha?
      (a alpha-a)
      (b alpha-b))

    (check
	(let ((E (make-alpha 1 2)))
	  (list (alpha? E) (is-a? E &alpha) (warning? E)))
      => '(#t #t #t))

    (check
	(let ((E (make-alpha 1 2)))
	  (list (alpha-a E) (alpha-b E)))
      => '(1 2))

    (check
	(let (((E &alpha) (make-alpha 1 2)))
	  (list (E a) (E b)))
      => '(1 2))

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-condition-type &alpha
      &warning make-alpha alpha?
      (a alpha-a)
      (b alpha-b))

    (define-condition-type &beta
      &alpha make-beta beta?
      (a beta-a)
      (b beta-b))

    (check
	(let ((E (make-beta 1 2 3 4)))
	  (list (beta?  E) (is-a? E &beta)
		(alpha? E) (is-a? E &alpha)
		(warning? E)))
      => '(#t #t #t #t #t))

    (check
	(let ((E (make-beta 1 2 3 4)))
	  (list (alpha-a E) (alpha-b E)
		(beta-a E)  (beta-b E)))
      => '(1 2 3 4))

    (check
	(let (((E &beta) (make-beta 1 2 3 4)))
	  (list (E a) (E b)))
      => '(3 4))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'rnrs.define-condition-type 'scheme-indent-function 1)
;; coding: utf-8-unix
;; End:
