;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: search tree using symbols as keys
;;;Date: Mon Dec 27, 2010
;;;
;;;Abstract
;;;
;;;	This  library handles  search trees  in which  keys are  list of
;;;	Scheme  symbols and  values can  be anything;  the tree  has the
;;;	structure of  nested alists.   Inserting the following  keys and
;;;	values (in this order):
;;;
;;;         (a b1 c1 d1)	1
;;;         (a b1 c1 d2)	2
;;;         (a b1 c2 d1)	1
;;;         (a b1 c1 d3)	3
;;;         (a b2 c1)		4
;;;         (a b1 c2 d2)	2
;;;         (a b2 c2)		5
;;;         (a b1 c2 d3)	3
;;;         (a b2 c3)		6
;;;         (a b2)		7
;;;         (a b1 c2)		8
;;;
;;;	yields the following tree:
;;;
;;;	    ((a . ((b2 . ((#f . 7)
;;;                       (c3 . ((#f . 6)))
;;;                       (c2 . ((#f . 5)))
;;;                       (c1 . ((#f . 4)))))
;;;                (b1 . ((c2 . ((#f . 8)
;;;                              (d3 . ((#f . 3)))
;;;                              (d2 . ((#f . 2)))
;;;                              (d1 . ((#f . 1)))))
;;;                       (c1 . ((d3 . ((#f . 3)))
;;;                              (d2 . ((#f . 2)))
;;;                              (d1 . ((#f . 1))))))))))
;;;
;;;     notice  that keys  are stored  as sequences  ending with  a pair
;;;     having #f  as key, and such  pairs are kept at  the beginning of
;;;     the alist.
;;;
;;;	Storing a key/value pair whose key already exists causes the old
;;;	value to be overwritten.
;;;
;;;Copyright (c) 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language symbols-tree)
  (export
    tree-cons treeq make-tree-iterator tree-merge)
  (import (rnrs)
    (rnrs mutable-pairs))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ...) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ...)
	  (begin ?form0 ?form ...)))))))

(define-syntax while
  (syntax-rules ()
    ((_ ?test ?body0 ?body ...)
     (let loop ()
       (when ?test ?body0 ?body ... (loop))))))

(define-inline (push! stack val)
  (set! stack (cons val stack)))

(define-inline (pop! stack)
  (set! stack (cdr stack)))

(define-inline (pop-caar! stack)
  (set! stack (cons (cdar stack) (cdr stack))))


;;;; building

(define tree-cons
  (case-lambda
   ((key value tree)
    (tree-cons key value tree #f))
   ((key value tree overwrite?)
    ;;Add KEY/VALUE pair to TREE.  KEY  must be a list of symbols, VALUE
    ;;can be anything.
    ;;
    (define (main key value tree)
      (assert (or (null? tree) (pair? tree)))
      (%cons key value tree))

    (define (%cons key value tree)
      (cond ((null? key)
	     (cond ((null? tree)
		    `((#f . ,value)))
		   ((caar tree) ;the first entry has non-#f key
		    `((#f . ,value) . ,tree))
		   (else ;the first entry has #f key, overwrite its value
		    (when overwrite?
		      (set-cdr! (car tree) value))
		    tree)))
	    ((null? tree)
	     `(,(%key->entry key value)))
	    (else
	     (let ((entry (assq (car key) tree)))
	       (cond (entry ;key found
		      (set-cdr! entry (%cons (cdr key) value (cdr entry)))
		      tree)
		     ((caar tree) ;key not found, the first entry has non-#f key
		      `(,(%key->entry key value) . ,tree))
		     (else ;key not found, the first entry has #f key
		      `(,(car tree)
			,(%key->entry key value)
			. ,(cdr tree))))))))

    (define (%key->entry key value)
      ;;Given a non-null KEY and a VALUE, build and return a search tree
      ;;entry holding values from KEY.  Example, for the key and value:
      ;;
      ;;	(A B C D)  VALUE
      ;;
      ;;we build the entry:
      ;;
      ;;	(A . ((B . ((C . ((D . ((#f . VALUE)))))))))
      ;;
      ;;while for the key and value:
      ;;
      ;;	(A)	VALUE
      ;;
      ;;we build the entry:
      ;;
      ;;	(A . ((#f . VALUE)))
      ;;
      (let ((A (car key))
	    (D (cdr key)))
	(cons A (if (null? D)
		    `((#f . ,value))
		  (let recur ((a (car D))
			      (d (cdr D)))
		    `((,a . ,(if (null? d)
				 `((#f . ,value))
			       (recur (car d) (cdr d))))))))))

    (main key value tree))))


;;;; searching

(define (treeq key tree default)
  (assert (or (null? tree) (pair? tree)))
  (assert (or (null? key)  (pair? key)))
  (let search ((key key) (tree tree))
    (if (null? key)
	(cond ((null? tree)
	       default)
	      ((caar tree) ;the first entry has non-#f key
	       default)
	      (else ;the first entry has #f key, found the value
	       (cdar tree)))
      (let ((entry (assq (car key) tree)))
	(if entry
	    (search (cdr key) (cdr entry))
	  default)))))


;;;; iteration documentation

#| In the next code page  we implement the iteration of the the elements
of a  tree; here we explain how  it goes.  Iterating over  a tree means;
composing a sequence of keys and values; for the following simple tree:

    ((#f . 1)
     (a . ((#f . 2)))
     (b . ((c1 . ((#f . 3)))
           (c2 . ((#f . 4))))))

iterating means generating the following sequence of key/value couples:

    ()		1
    (a)		2
    (b c1)	3
    (b c3)	4

for  implementation reasons the  iterator returns  the keys  as reversed
lists:

    ()		1
    (a)		2
    (c1 b)	3
    (c3 b)	4

but this is not a big problem.

An iteration has a state composed  of a stack of subtrees and a reversed
key.  For the tree:

    ((#f . 1)
     (a . ((#f . 2)))
     (b . ((c1 . ((#f . 3)))
           (c2 . ((#f . 4))))))

it goes as follows:

* Initialisation:

    rkey  = []
    stack = [((#f . 1) (a . ...) (b . ...))]

* Depth first search until #f key is found:

    rkey  = []
    stack = [((#f . 1) (a . ...) (b . ...))]

  store the key and value, pop the caar from the stack:

    results = () 1
    rkey    = []
    stack   = [((a . ((#f . 2))) (b . ...))]

  return the results.

* Depth first search until #f key is found:

    rkey  = [a]
    stack = [((#f . 2))
             ((a . ((#f . 2))) (b . ...))]

  store the key and value, pop the caar from the stack:

    results = (a) 2
    rkey  = [a]
    stack = [()
             ((a . ((#f . 2))) (b . ...))]

  back track, pop the caar from the stack:

    results = (a) 2
    rkey  = []
    stack = [((b . ((c1 . ((#f . 3))) (c2 . ...))))]

  return the results.

* Depth first search until #f key is found:

    rkey  = [c1 b]
    stack = [((#f . 3))
             ((c1 . ((#f . 3))) (c2 . ...))
             ((b . ((c1 . ((#f . 3))) (c2 . ...))))]

  store the key and value, pop the caar from the stack:

    results = (c1 b) 3
    rkey    = [c1 b]
    stack   = [()
               ((c1 . ((#f . 3))) (c2 . ...))
               ((b . ((c1 . ((#f . 3))) (c2 . ...))))]

  back track, pop the caar from the stack:

    results = (c1 b) 3
    rkey    = [b]
    stack   = [((c2 . ((#f . 4))))
               ((b . ((c1 . ((#f . 3))) (c2 . ...))))]

  return the results.

* Depth first search until #f key is found:

    rkey    = [c2 b]
    stack   = [((#f . 4))
               ((c2 . ((#f . 4))))
               ((b . ((c1 . ...) (c2 . ((#f . 4))))))]

  store the key and value, pop the caar from the stack:

    results = (c2 b) 4
    rkey    = [c2 b]
    stack   = [()
               ((c2 . ((#f . 4))))
               ((b . ((c1 . ...) (c2 . ((#f . 4))))))]

  back track, pop the caar from the stack:

    results = (c2 b) 4
    rkey    = [b]
    stack   = [()
               ((b . ((c1 . ...) (c2 . ((#f . 4))))))]

  back track, pop the caar from the stack:

    results = (c2 b) 4
    rkey    = []
    stack   = [()]

  back track:

    results = (c2 b) 4
    rkey    = []
    stack   = []

  return the results.

|#


;;;; iteration

(define (make-tree-iterator tree)
  (let ((stack	`(((#f . #f) ;fake entry to make the iterator function simpler
		   . ,tree)))
	(rkey	'()))

    (define (depth-first-search)
      ;;To be  called when the STACK  has at least one  element and that
      ;;element is non null.
      ;;
      (unless (null? stack)
	(let ((tree (car stack)))
	  (when (caar tree) ;the first entry has non-#f key
	    (push! rkey  (caar tree))
	    (push! stack (cdar tree))
	    (depth-first-search)))))

    (define-inline (step)
      ;;Assume the top of the stack  is an alist whose first element has
      ;;already been  consumed; pop  the alist and  push the cdr  of the
      ;;alist.  Example when the top of the stack has a cdr, before:
      ;;
      ;;    stack = [((#f . 1) (b . ...)) ...]
      ;;
      ;;after:
      ;;
      ;;    stack = [((b . ...)) ...]
      ;;
      ;;example when the top of the stack has no cdr, before:
      ;;
      ;;    stack = [((#f . 1)) ...]
      ;;
      ;;after:
      ;;
      ;;    stack = [() ...]
      ;;
      (unless (or (null? stack)
		  (null? (car stack)))
	(pop-caar! stack)))

    (define (back-track)
      ;;To be called after having consumed  an element on the top of the
      ;;stack.  To be called when the STACK has at least one element and
      ;;that element is non null.
      ;;
      ;;Example, before:
      ;;
      ;;    rkey  = (a)
      ;;    stack = [() ((a . ((#f . 1))) (b . ((#f . 2))))]
      ;;
      ;;after:
      ;;
      ;;    rkey  = ()
      ;;    stack = [((b . ((#f . 2))))]
      ;;
      (while (and (not (null? stack))
		  (null? (car stack)))
	(unless (null? stack)
	  (pop! stack)
	  (unless (null? rkey)
	    (pop! rkey))
	  (step))))

    (lambda ()
      (step)
      (back-track)
      (depth-first-search)
      (if (null? stack)
          (values #f #f #f)
	(values #t rkey (cdaar stack))))
    ))


;;;; merge

(define tree-merge
  ;;FIXME  This implementation should  be better;  as it  is now  it was
  ;;quick to write.
  ;;
  (case-lambda
   ((dst src)
    (tree-merge dst src #f))
   ((dst src overwrite?)
    (let ((I (make-tree-iterator src)))
      (let loop ()
	(let-values (((more? rkey val) (I)))
	  (if more?
	      (begin
		(set! dst (tree-cons (reverse rkey) val dst overwrite?))
		(loop))
	    dst)))))))


;;;; done

)

;;; end of file
