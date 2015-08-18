;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for binary search trees
;;;Date: Fri Aug 14, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare containers binary-search-trees)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: binary search trees\n")


;;;; fixnum binary search trees

(define-record-type <fixnum-node>
  (parent <unbalanced-binary-node>)

  (fields (immutable sort-key))

  (protocol
   (lambda (make-unbalanced-binary-node)
     (case-lambda*
       (()
	((make-unbalanced-binary-node #f #f) (void)))

       (({key fixnum?})
	((make-unbalanced-binary-node #f #f) key))

       (({key fixnum?} {left false-or-unbalanced-binary-node?} {right false-or-unbalanced-binary-node?})
	((make-unbalanced-binary-node left right) key)))))

  #| end of DEFINE-RECORD-TYPE |# )

(define (key< new old)
  (fx<? (<fixnum-node>-sort-key new)
	(<fixnum-node>-sort-key old)))

(define (make-comparison-proc target-key)
  (lambda (node)
    (let ((key (<fixnum-node>-sort-key node)))
      (cond ((fx=? target-key key)	 0)
	    ((fx<? target-key key)	-1)
	    (else			+1)))))

;;; --------------------------------------------------------------------

(define (tree . key*)
  (fold-left (lambda (root key)
	       (unbalanced-tree-insert! root key< (make-<fixnum-node> key)))
    #f key*))

(define (make-tree)
  ;; 5-------10----12
  ;; |        |     |
  ;; 1--3--4  7--9 11
  ;;    |     |  |
  ;;    2     6  8
  (tree 5 1 3 2 4 10 7 12 6 9 8 11))

;;; --------------------------------------------------------------------

(define (validate-node root node-key dad-key left-key right-key)
  (let ((node (binary-tree-find root (make-comparison-proc node-key))))
    (assert node)
    (assert (= node-key (<fixnum-node>-sort-key node)))
    (let* ((dad    (binary-node-parent node))
	   (left   (binary-node-left   node))
	   (right  (binary-node-right  node)))
      ;;The parent is the expected one.
      (if dad-key
	  (assert (= dad-key (<fixnum-node>-sort-key dad)))
	(assert (not dad)))
      ;;The parent has NODE as child.
      (when dad-key
	(assert (binary-node-parent-and-child? dad node)))
      ;;The left child is the expected one.
      (if left-key
	  (assert (= left-key (<fixnum-node>-sort-key left)))
	(assert (not left)))
      ;;The left child has NODE as parent.
      (when left-key
	(assert (binary-node-parent-and-left-child? node left)))
      ;;The right child is the expected one.
      (if right-key
	  (assert (= right-key (<fixnum-node>-sort-key right)))
	(assert (not right)))
      ;;The right child has NODE as parent.
      (when right-key
	(assert (binary-node-parent-and-right-child? node right)))
      #t)))


;;;; helpers

(define (make-list-20)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   19)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

(define (make-vector-20)
  ;;Build and return a new vector object holding fixnums from 0 to 99 included.
  ;;
  (do ((vec (make-vector 20))
       (i 0 (fxadd1 i)))
      ((fx=? i 20)
       vec)
    (vector-set! vec i i)))

;;; --------------------------------------------------------------------

(define (make-list-100)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   99)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

(define (make-vector-100)
  ;;Build and return a new vector object holding fixnums from 0 to 99 included.
  ;;
  (do ((vec (make-vector 100))
       (i 0 (fxadd1 i)))
      ((fx=? i 100)
       vec)
    (vector-set! vec i i)))

;;; --------------------------------------------------------------------

;;A list of 5 objects, enstackd in a stack, is fully stored in a single buffer.
;;
(define-constant LIST-5			'(0 1 2 3 4))
(define-constant LIST-5-REVERSED	(reverse LIST-5))
(define-constant LIST-5-NEGATED		(map - LIST-5))

;;A list of 20 objects, enstackd in a stack, is stored in two buffers.
;;
(define-constant LIST-20		(make-list-20))
(define-constant LIST-20-REVERSED	(reverse LIST-20))
(define-constant LIST-20-NEGATED	(map - LIST-20))

;;A list of 100 objects, enstackd in a stack, is stored in multiple buffers.
;;
(define-constant LIST-100		(make-list-100))
(define-constant LIST-100-REVERSED	(reverse LIST-100))
(define-constant LIST-100-NEGATED	(map - LIST-100))

;;A vector of 5 objects, enstackd in a stack, is fully stored in a single buffer.
;;
(define-constant VECTOR-5		(list->vector LIST-5))
(define-constant VECTOR-5-REVERSED	(list->vector LIST-5-REVERSED))

;;A vector of 20 objects, enstackd in a stack, is stored in two buffers.
;;
(define-constant VECTOR-20		(list->vector LIST-20))
(define-constant VECTOR-20-REVERSED	(list->vector LIST-20-REVERSED))

;;A vector of 100 objects, enstackd in a stack, is stored in multiple buffers.
;;
(define-constant VECTOR-100		(list->vector LIST-100))
(define-constant VECTOR-100-REVERSED	(list->vector LIST-100-REVERSED))


(parametrise ((check-test-name	'binary-nodes))

  (check
      (binary-node? (make-<fixnum-node> 123 #f #f))
    => #t)

  (check
      (let* ((lx (make-<fixnum-node> 0))
	     (rx (make-<fixnum-node> 2))
	     (rt (make-<fixnum-node> 1 lx rx)))
	(values (<fixnum-node>-sort-key (binary-node-left  rt))
		(<fixnum-node>-sort-key rt)
		(<fixnum-node>-sort-key (binary-node-right rt))))
    => 0 1 2)

  #t)


(parametrise ((check-test-name	'unbalanced-nodes))

  (check
      (unbalanced-binary-node? (make-<fixnum-node> 123))
    => #t)

  (check
      (let* ((lx (make-<fixnum-node> 0))
	     (rx (make-<fixnum-node> 2))
	     (rt (make-<fixnum-node> 1 lx rx)))
	(values (<fixnum-node>-sort-key (binary-node-left  rt))
		(<fixnum-node>-sort-key rt)
		(<fixnum-node>-sort-key (binary-node-right rt))))
    => 0 1 2)

;;; --------------------------------------------------------------------

  (check
      (let* ((node (make-<fixnum-node> 0))
	     (root (unbalanced-tree-insert! #f key< node)))
	(eq? root node))
    => #t)

  (check
      (internal-body
	(define (insert root fx)
	  (unbalanced-tree-insert! root key< (make-<fixnum-node> fx)))
	(let* ((root #f)
	       (root (insert root 1))
	       (root (insert root 0))
	       (root (insert root 2)))
	  (values (and root (<fixnum-node>-sort-key root))
		  (let ((lx (binary-node-left root)))
		    (and lx (<fixnum-node>-sort-key lx)))
		  (let ((rx (binary-node-right root)))
		    (and rx (<fixnum-node>-sort-key rx))))))
    => 1 0 2)

  (check
      (internal-body
	(define (insert root fx)
	  (unbalanced-tree-insert! root key< (make-<fixnum-node> fx)))
	(let* ((root #f)
	       (root (insert root 0))
	       (root (insert root 1))
	       (root (insert root 2)))
	  (values (<fixnum-node>-sort-key root)
		  (<fixnum-node>-sort-key (binary-node-right root))
		  (<fixnum-node>-sort-key (binary-node-right (binary-node-right root))))))
    => 0 1 2)

  (check
      (internal-body
	(define (insert root fx)
	  (unbalanced-tree-insert! root key< (make-<fixnum-node> fx)))
	(let* ((root #f)
	       (root (insert root 2))
	       (root (insert root 1))
	       (root (insert root 0)))
	  (values (<fixnum-node>-sort-key root)
		  (<fixnum-node>-sort-key (binary-node-left root))
		  (<fixnum-node>-sort-key (binary-node-left (binary-node-left root))))))
    => 2 1 0)

  (check
      (let ((root (tree 1 0 2)))
	(values (<fixnum-node>-sort-key root)
		(<fixnum-node>-sort-key (binary-node-left  root))
		(<fixnum-node>-sort-key (binary-node-right root))))
    => 1 0 2)

  #t)


(parametrise ((check-test-name	'unbalanced-nodes-searching))

;;; minimum search

  (check
      (let ((root (tree)))
	(binary-tree-minimum root (lambda () 99)))
    => 99)

  (check
      (let ((root (tree)))
	(binary-tree-minimum root))
    => #f)

  (check
      (let ((root (tree 0)))
	(<fixnum-node>-sort-key (binary-tree-minimum root)))
    => 0)

  (check
      (let ((root (tree 1 0 2)))
	(<fixnum-node>-sort-key (binary-tree-minimum root)))
    => 0)

  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (check
      (let ((root (tree 5 3 8 1 4 6 9 0 2 7 10)))
	(<fixnum-node>-sort-key (binary-tree-minimum root)))
    => 0)

;;; --------------------------------------------------------------------
;;; maximum search

  (check
      (let ((root (tree)))
	(binary-tree-maximum root (lambda () 99)))
    => 99)

  (check
      (let ((root (tree)))
	(binary-tree-maximum root))
    => #f)

  (check
      (let ((root (tree 0)))
	(<fixnum-node>-sort-key (binary-tree-maximum root)))
    => 0)


  (check
      (let ((root (tree 1 0 2)))
	(<fixnum-node>-sort-key (binary-tree-maximum root)))
    => 2)

  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (check
      (let ((root (tree 5 3 8 1 4 6 9 0 2 7 10)))
	(<fixnum-node>-sort-key (binary-tree-maximum root)))
    => 10)

;;; --------------------------------------------------------------------
;;; finding

  (check
      (let ((root (tree)))
	(define (search target-key)
	  (binary-tree-find root (make-comparison-proc target-key)))
	(search 99))
    => #f)

  (check
      (let ((root (tree 5 3 8 1 4 6 9 0 2 7 10)))
	(define (search target-key)
	  (binary-tree-find root (make-comparison-proc target-key)))
	(search 99))
    => #f)

  (check
      (let ((root (tree 5 3 8 1 4 6 9 0 2 7 10)))
	(define (search target-key)
	  (binary-tree-find root (make-comparison-proc target-key) (lambda () 'not-found)))
	(search 99))
    => 'not-found)

  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (check
      (let ((root (tree 5 3 8 1 4 6 9 0 2 7 10)))
	(define (search target-key)
	  (binary-tree-find root (make-comparison-proc target-key)))
	(map <fixnum-node>-sort-key
	  (fold-right
	      (lambda (target-key knil)
		(cons (search target-key) knil))
	    '() '(0 1 2 3 4 5 6 7 8 9 10))))
    => '(0 1 2 3 4 5 6 7 8 9 10))

;;; --------------------------------------------------------------------
;;; root search

  (check
      (let ((root (tree)))
	(binary-tree-root root))
    => #f)

  (check
      (let ((root (tree 0)))
	(<fixnum-node>-sort-key (binary-tree-root root)))
    => 0)

  (check
      (let* ((root (tree 1 0 2))
	     (node (binary-tree-find root (make-comparison-proc 0))))
	(<fixnum-node>-sort-key (binary-tree-root node)))
    => 1)

  (check
      (let* ((root (tree 1 0 2))
	     (node (binary-tree-find root (make-comparison-proc 2))))
	(<fixnum-node>-sort-key (binary-tree-root node)))
    => 1)

  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (for-all (lambda (target)
	     (check
		 (let* ((root (tree 5 3 8 1 4 6 9 0 2 7 10))
			(node (binary-tree-find root (make-comparison-proc target))))
		   (<fixnum-node>-sort-key (binary-tree-root node)))
	       => 5))
    '(0 1 2 3 4 5 6 7 8 9 10))

  #t)


(parametrise ((check-test-name	'unbalanced-nodes-validation))

  (check-for-true
   (let ((root #f))
     (binary-tree-valid? root key<)))

  (check-for-true
   (let ((root (tree 5)))
     (binary-tree-valid? root key<)))

  (check-for-true
   (let ((root (tree 5 4)))
     (binary-tree-valid? root key<)))

  (check-for-true
   (let ((root (tree 5 6)))
     (binary-tree-valid? root key<)))

  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (check-for-true
   (let ((root (tree 5 3 8 1 4 6 9 0 2 7 10)))
     (binary-tree-valid? root key<)))

  ;; 3--5--6
  ;; |  |
  ;; 2  99
  ;;
  (check-for-false
   (let ((root (make-<fixnum-node> 3
				   (make-<fixnum-node> 5
						       (make-<fixnum-node> 99)
						       (make-<fixnum-node> 6))
				   (make-<fixnum-node> 2))))
     (binary-tree-valid? root key<)))

  ;; 3-----4
  ;; |
  ;; 2--0
  ;; |
  ;; 1
  ;;
  (check-for-false
   (let ((root (make-<fixnum-node> 3
				   (make-<fixnum-node> 4)
				   (make-<fixnum-node> 2
						       (make-<fixnum-node> 1)
						       (make-<fixnum-node> 0)))))
     (binary-tree-valid? root key<)))

  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (check-for-true
   (let ((root (tree 5 3 8 1 4 6 9 0 2 7 10)))
     (validate-node root  0  1 #f #f)
     (validate-node root  1  3  0  2)
     (validate-node root  2  1 #f #f)
     (validate-node root  3  5  1  4)
     (validate-node root  4  3 #f #f)
     (validate-node root  5 #f  3  8)
     (validate-node root  6  8 #f  7)
     (validate-node root  7  6 #f #f)
     (validate-node root  8  5  6  9)
     (validate-node root  9  8 #f 10)
     (validate-node root 10  9 #f #f)
     #t))

  #t)


(parametrise ((check-test-name	'unbalanced-nodes-removal))

  (define (%fold-it root)
    ;;Fold the tree with a in-order iteration, forwards and backwards.
    ;;
    ;;By reversing we return  a list in which: the first item is  the sort key of the
    ;;first node visited in the iteration.
    (values (reverse
	     (binary-tree-fold-in-order-forwards
		 (lambda (knil node)
		   (cons (<fixnum-node>-sort-key node) knil))
	       '() root))
	    (binary-tree-fold-in-order-backwards
		(lambda (node knil)
		  (cons (<fixnum-node>-sort-key node) knil))
	      '() root)))

;;; --------------------------------------------------------------------
;;; checking with validation predicate

  (check-for-true
   (let* ((root (tree 0))
	  (root (unbalanced-tree-remove! root root)))
     (binary-tree-valid? root key<)))

  (check-for-true
   (let* ((root (tree 0 1 2))
	  (node (binary-tree-find root (make-comparison-proc 1)))
	  (root (unbalanced-tree-remove! node root)))
     (binary-tree-valid? root key<)))

  (check-for-true
   (let* ((root (tree 0 1 2))
	  (node (binary-tree-find root (make-comparison-proc 2)))
	  (root (unbalanced-tree-remove! node root)))
     (binary-tree-valid? root key<)))

  (let-syntax
      ((check-with-validation (syntax-rules ()
				((_ ?key)
				 ;; 5------8--9--10
				 ;; |      |
				 ;; 3--4   6--7
				 ;; |
				 ;; 1--2
				 ;; |
				 ;; 0
				 ;;
				 (check-for-true
				  (let* ((root (tree 5 3 8 1 4 6 9 0 2 7 10))
					 (node (binary-tree-find root (make-comparison-proc ?key)))
					 (root (unbalanced-tree-remove! node root)))
				    (binary-tree-valid? root key<))))
				)))

    (check-with-validation 0)
    (check-with-validation 1)
    (check-with-validation 2)
    (check-with-validation 3)
    (check-with-validation 4)
    (check-with-validation 5)
    (check-with-validation 6)
    (check-with-validation 7)
    (check-with-validation 8)
    (check-with-validation 9)
    (check-with-validation 10)

    #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; checking by folding

  (let-syntax
      ((check-with-folding (syntax-rules ()
			     ((_ ?key ?expected)
			      ;; 5------8--9--10
			      ;; |      |
			      ;; 3--4   6--7
			      ;; |
			      ;; 1--2
			      ;; |
			      ;; 0
			      ;;
			      (check
				  (let* ((key  ?key)
					 (root (tree 5 3 8 1 4 6 9 0 2 7 10))
					 (node (binary-tree-find root (make-comparison-proc key)))
					 (root (unbalanced-tree-remove! node root)))
				    (%fold-it root))
				=> (quote ?expected) (quote ?expected)))
			     )))

    (check-with-folding 0		(  1 2 3 4 5 6 7 8 9 10))
    (check-with-folding 1		(0   2 3 4 5 6 7 8 9 10))
    (check-with-folding 2		(0 1   3 4 5 6 7 8 9 10))
    (check-with-folding 3		(0 1 2   4 5 6 7 8 9 10))
    (check-with-folding 4		(0 1 2 3   5 6 7 8 9 10))
    (check-with-folding 5		(0 1 2 3 4   6 7 8 9 10))
    (check-with-folding 6		(0 1 2 3 4 5   7 8 9 10))
    (check-with-folding 7		(0 1 2 3 4 5 6   8 9 10))
    (check-with-folding 8		(0 1 2 3 4 5 6 7   9 10))
    (check-with-folding 9		(0 1 2 3 4 5 6 7 8   10))
    (check-with-folding 10		(0 1 2 3 4 5 6 7 8 9   ))

    #| end of LET-SYNTAX |# )

  ;;Before:
  ;;
  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  ;;After:
  ;;
  ;; 4------8--9--10
  ;; |      |
  ;; 3      6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (check
      (let* ((key  5)
	     (root (tree 5 3 8 1 4 6 9 0 2 7 10))
	     (node (binary-tree-find root (make-comparison-proc key)))
	     (root (unbalanced-tree-remove! node root)))
	(validate-node root  0  1 #f #f)
	(validate-node root  1  3  0  2)
	(validate-node root  2  1 #f #f)
	(validate-node root  3  4  1 #f)
	(validate-node root  4 #f  3  8)
	(validate-node root  6  8 #f  7)
	(validate-node root  7  6 #f #f)
	(validate-node root  8  4  6  9)
	(validate-node root  9  8 #f 10)
	(validate-node root 10  9 #f #f)
	(%fold-it root))
    => '(0 1 2 3 4 6 7 8 9 10) '(0 1 2 3 4 6 7 8 9 10))

  #t)


(parametrise ((check-test-name	'iteration-tree-test))

;;; check the tree used for iterator tests

;;; --------------------------------------------------------------------
;;; check tree

  (check	;node 1
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 1))))
	(values (= 5  (<fixnum-node>-sort-key (binary-node-parent node)))
		(not  (binary-node-left  node))
		(= 3  (<fixnum-node>-sort-key (binary-node-right node)))))
    => #t #t #t)

  (check	;node 2
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 2))))
	(values (= 3  (<fixnum-node>-sort-key (binary-node-parent node)))
		(not  (binary-node-left  node))
		(not  (binary-node-right node))))
    => #t #t #t)

  (check	;node 3
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 3))))
	(values (= 1  (<fixnum-node>-sort-key (binary-node-parent node)))
		(= 2  (<fixnum-node>-sort-key (binary-node-left   node)))
		(= 4  (<fixnum-node>-sort-key (binary-node-right  node)))))
    => #t #t #t)

  ;; 5-------10----12
  ;; |        |     |
  ;; 1--3--4  7--9 11
  ;;    |     |  |
  ;;    2     6  8
  (check	;node 4
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 4))))
	(values (= 3  (<fixnum-node>-sort-key (binary-node-parent node)))
		(not  (binary-node-left  node))
		(not  (binary-node-right node))))
    => #t #t #t)

  (check	;node 5
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 5))))
	(values (not  (binary-node-parent node))
		(= 1  (<fixnum-node>-sort-key (binary-node-left  node)))
		(= 10 (<fixnum-node>-sort-key (binary-node-right node)))))
    => #t #t #t)

  (check	;node 6
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 6))))
	(values (= 7  (<fixnum-node>-sort-key (binary-node-parent node)))
		(not  (binary-node-left  node))
		(not  (binary-node-right node))))
    => #t #t #t)

  (check	;node 7
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 7))))
	(values (= 10 (<fixnum-node>-sort-key (binary-node-parent node)))
		(= 6  (<fixnum-node>-sort-key (binary-node-left   node)))
		(= 9  (<fixnum-node>-sort-key (binary-node-right  node)))))
    => #t #t #t)

  ;; 5-------10----12
  ;; |        |     |
  ;; 1--3--4  7--9 11
  ;;    |     |  |
  ;;    2     6  8
  (check	;node 8
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 8))))
	(values (= 9  (<fixnum-node>-sort-key (binary-node-parent node)))
		(not  (binary-node-left  node))
		(not  (binary-node-right node))))
    => #t #t #t)

  (check	;node 9
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 9))))
	(values (= 7  (<fixnum-node>-sort-key (binary-node-parent node)))
		(= 8  (<fixnum-node>-sort-key (binary-node-left   node)))
		(not (binary-node-right  node))))
    => #t #t #t)

  (check	;node 10
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 10))))
	(values (= 5  (<fixnum-node>-sort-key (binary-node-parent node)))
		(= 7  (<fixnum-node>-sort-key (binary-node-left   node)))
		(= 12 (<fixnum-node>-sort-key (binary-node-right  node)))))
    => #t #t #t)

  (check	;node 11
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 11))))
	(values (= 12 (<fixnum-node>-sort-key (binary-node-parent node)))
		(not  (binary-node-left  node))
		(not  (binary-node-right node))))
    => #t #t #t)

  (check	;node 12
      (let* ((root (make-tree))
	     (node (binary-tree-find root (make-comparison-proc 12))))
	(values (= 10 (<fixnum-node>-sort-key (binary-node-parent node)))
		(= 11 (<fixnum-node>-sort-key (binary-node-left   node)))
		(not (binary-node-right  node))))
    => #t #t #t)

  #t)


(parametrise ((check-test-name	'iteration-in-order))

;;; check the tree used for iterator tests

  (define-syntax doit-forwards
    (syntax-rules ()
      ((_ ?tree ?expected)
       (check
	   ;;By reversing we return  a list in which: the first item  is the sort key
	   ;;of the first node visited in the iteration.
	   (reverse
	    (binary-tree-fold-in-order-forwards
		(lambda (knil node)
		  (cons (<fixnum-node>-sort-key node) knil))
	      '() ?tree))
	 => (quote ?expected)))
      ))

  (define-syntax doit-backwards
    (syntax-rules ()
      ((_ ?tree ?expected)
       (check
	   ;;By reversing we return  a list in which: the first item  is the sort key
	   ;;of the first node visited in the iteration.
	   (reverse
	    (binary-tree-fold-in-order-backwards
		(lambda (node knil)
		  (cons (<fixnum-node>-sort-key node) knil))
	      '() ?tree))
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; forwards iteration

  (doit-forwards #f			())	;empty tree
  (doit-forwards (tree 1)		(1))	;one node tree
  (doit-forwards (tree 0 1 2)		(0 1 2))
  (doit-forwards (tree 2 1 0)		(0 1 2))
  (doit-forwards (tree 1 0 2)		(0 1 2))
  (doit-forwards (make-tree)		(1 2 3 4 5 6 7 8 9 10 11 12))

;;; --------------------------------------------------------------------
;;; backwards iteration

  (doit-backwards #f			())	;empty tree
  (doit-backwards (tree 1)		(1))	;one node tree
  (doit-backwards (tree 0 1 2)		(2 1 0))
  (doit-backwards (tree 2 1 0)		(2 1 0))
  (doit-backwards (tree 1 0 2)		(2 1 0))
  (doit-backwards (make-tree)		(12 11 10 9 8 7 6 5 4 3 2 1))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'binary-tree-fold-in-order-forwards	'scheme-indent-function 1)
;; eval: (put 'binary-tree-fold-in-order-backwards	'scheme-indent-function 1)
;; eval: (put 'binary-tree-fold-pre-order-forwards	'scheme-indent-function 1)
;; eval: (put 'binary-tree-fold-pre-order-backwards	'scheme-indent-function 1)
;; eval: (put 'binary-tree-fold-post-order-forwards	'scheme-indent-function 1)
;; eval: (put 'binary-tree-fold-post-order-backwards	'scheme-indent-function 1)
;; eval: (put 'binary-tree-fold-level-order-forwards	'scheme-indent-function 1)
;; eval: (put 'binary-tree-fold-level-order-backwards	'scheme-indent-function 1)
;; End:
