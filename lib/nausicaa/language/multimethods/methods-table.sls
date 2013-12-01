;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: functions for handling methods tables
;;;Date: Wed Mar 23, 2011
;;;
;;;Abstract
;;;--------
;;;
;;;	The ancestor of this library is ScmObj by Dorai Sitaram.
;;;
;;;Method alists
;;;-------------
;;;
;;;	The collection  of methods  in a generic  function is  an alist;
;;;	each entry has the format:
;;;
;;;		(signature . closure)
;;;
;;;	the key is the method's signature.
;;;
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 1996 Dorai Sitaram
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
(library (nausicaa language multimethods methods-table)
  (export
    define-methods-table
    compute-applicable-methods
    applicable-method-signature?
    more-specific-signature?
    merge-methods-alists

    ;;Commented  out because  not  used by  outer code;  But  it can  be
    ;;exported if there is the need.  (Marco Maggi; Mon Nov 25, 2013)
    ;;
    #;add-method-to-methods-alist)
  (import (vicare)
    (only (nausicaa language symbols-tree)
	  tree-cons
	  treeq)
    (vicare unsafe operations))


;;;; helpers

(define-syntax-rule (list-copy ?ell)
  (let loop ((ell ?ell))
    (if (pair? ell)
	(cons ($car ell) (loop ($cdr ell)))
      ell)))


(define-syntax define-methods-table
  ;;Used in  the body of DEFINE-GENERIC  to define a  methods table with
  ;;its cache.
  ;;
  (syntax-rules ()
    ;;We could automatically generate  most of the identifier arguments,
    ;;we do not do it for hygiene sake.
    ;;
    ((_ ?generic-function ?number-of-arguments
	?table-name-func ?table-add ?cache-name ?cache-store ?cache-ref ?init)
     (begin
       (define ?table-name-func
	 ;;We  need to  access the  table  through a  function; this  is
	 ;;because the  table must be  mutated by code  across different
	 ;;libraries  when  generic functions  are  merged.   It is  not
	 ;;possible for code of a library to mutate a binding in another
	 ;;library.
	 ;;
	 (let ((table ?init))
	   (case-lambda
	    (()
	     table)
	    ((new-table)
	     (set! table new-table)))))
       (define ?cache-name '()) ;symbols tree
       (define (?cache-store signature methods)
	 (set! ?cache-name (tree-cons (map car signature) methods ?cache-name)))
       (define (?cache-ref signature)
	 (treeq (map car signature) ?cache-name #f))
       (define (?table-add signature closure)
	 (let ((len (length signature)))
	   (unless (= ?number-of-arguments len)
	     (assertion-violation (quote ?generic-function)
	       (string-append
		"attempt add method to generic function with wrong number of arguments, \
		 generic function has " (number->string ?number-of-arguments)
		" method has " (number->string len))
	       signature)))
	 (set! ?cache-name '())
	 (add-method-to-methods-alist ?table-name-func signature closure))
     ))))


(define (add-method-to-methods-alist methods-alist-func signature closure)
  ;;Add a  method's entry to the  alist of methods;  return the modified
  ;;method alist.
  ;;
  ;;A new  method entry  is added  only if no  method with  the selected
  ;;signature already  exists.  If a  method with the  signature already
  ;;exists, its closure is overwritten with the new one.
  ;;
  (methods-alist-func
   (cond ((find (lambda (method-entry)
		  (for-all eq? signature (car method-entry)))
	    (methods-alist-func))
	  => (lambda (method-entry)
	       (set-cdr! method-entry closure)
	       (methods-alist-func)))
	 (else
	  (cons (cons signature closure) (methods-alist-func))))))

(define (compute-applicable-methods call-signature methods-alist-func)
  ;;Filter out from the alist returned by METHODS-ALIST-FUNC the methods
  ;;not  applicable to  a tuple  of arguments  with types  in the  tuple
  ;;CALL-SIGNATURE.  Then  sort the list  of applicable methods  so that
  ;;the more  specific are the  first ones.   Return the sorted  list of
  ;;applicable method entries.
  ;;
  (list-sort
   (lambda (method-entry1 method-entry2)
     (more-specific-signature? (car method-entry1) (car method-entry2) call-signature))
   (filter
       (lambda (method-entry)
	 (applicable-method-signature? call-signature (car method-entry)))
     (methods-alist-func))))


(define (applicable-method-signature? call-signature method-signature)
  ;;Return true  if a method with  METHOD-SIGNATURE can be  applied to a
  ;;tuple of arguments having CALL-SIGNATURE.
  ;;
  (and (= (length call-signature) (length method-signature))
       (for-all (lambda (maybe-parent maybe-child)
		  (memq ($car maybe-parent) maybe-child))
		method-signature call-signature)))

(define (more-specific-signature? signature1 signature2 call-signature)
  ;;Return true if METHOD1 is more specific than METHOD2 with respect to
  ;;CALL-SIGNATURE.   This  function   must  be  applied  to  applicable
  ;;methods.  The longest signature is more specific, by definition.
  ;;
  (let next-argument-type ((signature1     signature1)
			   (signature2     signature2)
			   (call-signature call-signature))
    (if (null? signature1)

	;;If we are here: the  two signatures have EQ?  car values; this
	;;is  an error  because ADD-METHOD-TO-METHODS-ALIST  should have
	;;detected this and replaced one method's closure with the other
	;;in the alist of methods.
	(assertion-violation 'more-specific-signature?
	  "two methods with same signature in generic function"
	  signature1)

      (let ((uid-hierarchy-1 ($car signature1))
	    (uid-hierarchy-2 ($car signature2)))
	(cond ((eq? ($car uid-hierarchy-1) ($car uid-hierarchy-2))
	       (next-argument-type ($cdr signature1) ($cdr signature2) ($cdr call-signature)))
	      ((memq ($car uid-hierarchy-2) uid-hierarchy-1)
	       #t)
	      (else
	       #f))))))


(define (merge-methods-alists . list-of-methods-alist-funcs)
  ;;Given a list  of methods alist functions: duplicate  the first list,
  ;;then merge the remaining ones into the copy and return the result.
  ;;
  ;;Merging is performed visiting the  list from left to right; when two
  ;;methods have  the same  signature, the one  from the  leftmost alist
  ;;takes precedence.
  ;;
  (define-inline (main)
    (if (null? list-of-methods-alist-funcs)
	'()
      (fold-left (lambda (result-alist next-alist-func)
		   (merge-two-methods-alists result-alist (next-alist-func)))
		 (list-copy (($car list-of-methods-alist-funcs)))
		 ($cdr list-of-methods-alist-funcs))))

  (define-inline (merge-two-methods-alists result-alist other-alist)
    ;;Merge OTHER-ALIST into RESULT-ALIST and return a new alist.
    ;;
    (fold-left (lambda (result-alist next-method-entry)
		 ;;If a method with  the same signature does not already
		 ;;exist   in   RESULT-ALIST:    prepend   a   copy   of
		 ;;NEXT-METHOD-ENTRY to RESULT-ALIST  and return the new
		 ;;alist.
		 ;;
		 ;;NEXT-METHOD-ENTRY is  duplicated because the original
		 ;;can be modified by ADD-METHOD.
		 ;;
		 (if (find (lambda (method-entry)
			     (for-all eq? ($car method-entry) ($car next-method-entry)))
		       result-alist)
		     result-alist
		   (cons (cons ($car next-method-entry) ($cdr next-method-entry))
			 result-alist)))
	       result-alist
	       other-alist))

  (main))


;;;; done

)

;;; end of file
