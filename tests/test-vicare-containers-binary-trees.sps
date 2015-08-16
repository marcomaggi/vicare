;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for binary tree skeleton
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
  (vicare containers binary-trees)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: binary tree skeleton\n")


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
      (binary-node? (make-binary-node 123 #f #f))
    => #t)

  (check
      (let* ((lx (make-binary-node 0))
	     (rx (make-binary-node 2))
	     (rt (make-binary-node 1 lx rx)))
	(values (binary-node-sort-key (binary-node-left  rt))
		(binary-node-sort-key rt)
		(binary-node-sort-key (binary-node-right rt))))
    => 0 1 2)

  #t)


(parametrise ((check-test-name	'unbalanced-nodes))

  (define (key< new old)
    (< (binary-node-sort-key new)
       (binary-node-sort-key old)))

  (define (tree . key*)
    (fold-left (lambda (root key)
		 (unbalanced-tree-insert! root key< (make-unbalanced-binary-node key)))
      #f key*))

  (define (make-comparison-proc target-key)
    (lambda (node)
      (let ((key (binary-node-sort-key node)))
	(cond ((= target-key key)	 0)
	      ((< target-key key)	-1)
	      (else			+1)))))

;;; --------------------------------------------------------------------

  (check
      (unbalanced-binary-node? (make-unbalanced-binary-node 123))
    => #t)

  (check
      (let* ((lx (make-unbalanced-binary-node 0))
	     (rx (make-unbalanced-binary-node 2))
	     (rt (make-unbalanced-binary-node 1 lx rx)))
	(values (binary-node-sort-key (binary-node-left  rt))
		(binary-node-sort-key rt)
		(binary-node-sort-key (binary-node-right rt))))
    => 0 1 2)

;;; --------------------------------------------------------------------

  (check
      (let* ((node (make-unbalanced-binary-node 0))
	     (root (unbalanced-tree-insert! #f key< node)))
	(eq? root node))
    => #t)

  (check
      (internal-body
	(define (insert root fx)
	  (unbalanced-tree-insert! root key< (make-unbalanced-binary-node fx)))
	(let* ((root #f)
	       (root (insert root 1))
	       (root (insert root 0))
	       (root (insert root 2)))
	  (values (and root (binary-node-sort-key root))
		  (let ((lx (binary-node-left root)))
		    (and lx (binary-node-sort-key lx)))
		  (let ((rx (binary-node-right root)))
		    (and rx (binary-node-sort-key rx))))))
    => 1 0 2)

  (check
      (internal-body
	(define (insert root fx)
	  (unbalanced-tree-insert! root key< (make-unbalanced-binary-node fx)))
	(let* ((root #f)
	       (root (insert root 0))
	       (root (insert root 1))
	       (root (insert root 2)))
	  (values (binary-node-sort-key root)
		  (binary-node-sort-key (binary-node-right root))
		  (binary-node-sort-key (binary-node-right (binary-node-right root))))))
    => 0 1 2)

  (check
      (internal-body
	(define (insert root fx)
	  (unbalanced-tree-insert! root key< (make-unbalanced-binary-node fx)))
	(let* ((root #f)
	       (root (insert root 2))
	       (root (insert root 1))
	       (root (insert root 0)))
	  (values (binary-node-sort-key root)
		  (binary-node-sort-key (binary-node-left root))
		  (binary-node-sort-key (binary-node-left (binary-node-left root))))))
    => 2 1 0)

  (check
      (let ((root (tree 1 0 2)))
	(values (binary-node-sort-key root)
		(binary-node-sort-key (binary-node-left  root))
		(binary-node-sort-key (binary-node-right root))))
    => 1 0 2)

  #t)


(parametrise ((check-test-name	'unbalanced-nodes-searching))

  (define (key< new old)
    (< (binary-node-sort-key new)
       (binary-node-sort-key old)))

  (define (tree . key*)
    (fold-left (lambda (root key)
		 (unbalanced-tree-insert! root key< (make-unbalanced-binary-node key)))
      #f key*))

  (define (make-comparison-proc target-key)
    (lambda (node)
      (let ((key (binary-node-sort-key node)))
	(cond ((= target-key key)	 0)
	      ((< target-key key)	-1)
	      (else			+1)))))

;;; --------------------------------------------------------------------
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
	(binary-node-sort-key (binary-tree-minimum root)))
    => 0)

  (check
      (let ((root (tree 1 0 2)))
	(binary-node-sort-key (binary-tree-minimum root)))
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
	(binary-node-sort-key (binary-tree-minimum root)))
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
	(binary-node-sort-key (binary-tree-maximum root)))
    => 0)


  (check
      (let ((root (tree 1 0 2)))
	(binary-node-sort-key (binary-tree-maximum root)))
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
	(binary-node-sort-key (binary-tree-maximum root)))
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
	(map binary-node-sort-key
	  (fold-right
	      (lambda (target-key knil)
		(cons (search target-key) knil))
	    '() '(0 1 2 3 4 5 6 7 8 9 10))))
    => '(0 1 2 3 4 5 6 7 8 9 10))

  #t)


(parametrise ((check-test-name	'unbalanced-nodes-validation))

  (define (key< new old)
    (< (binary-node-sort-key new)
       (binary-node-sort-key old)))

  (define (tree . key*)
    (fold-left (lambda (root key)
		 (unbalanced-tree-insert! root key< (make-unbalanced-binary-node key)))
      #f key*))

  (define (make-comparison-proc target-key)
    (lambda (node)
      (let ((key (binary-node-sort-key node)))
	(cond ((= target-key key)	 0)
	      ((< target-key key)	-1)
	      (else			+1)))))

;;; --------------------------------------------------------------------

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
   (let ((root (make-binary-node 3
				 (make-binary-node 5
						   (make-binary-node 99)
						   (make-binary-node 6))
				 (make-binary-node 2))))
     (binary-tree-valid? root key<)))

  ;; 3-----4
  ;; |
  ;; 2--0
  ;; |
  ;; 1
  ;;
  (check-for-false
   (let ((root (make-binary-node 3
				 (make-binary-node 4)
				 (make-binary-node 2
						   (make-binary-node 1)
						   (make-binary-node 0)))))
     (binary-tree-valid? root key<)))

  #t)


(parametrise ((check-test-name	'unbalanced-nodes-removal))

  (define (key< new old)
    (< (binary-node-sort-key new)
       (binary-node-sort-key old)))

  (define (tree . key*)
    (fold-left (lambda (root key)
		 (unbalanced-tree-insert! root key< (make-unbalanced-binary-node key)))
      #f key*))

  (define (make-comparison-proc target-key)
    (lambda (node)
      (let ((key (binary-node-sort-key node)))
	(cond ((= target-key key)	 0)
	      ((< target-key key)	-1)
	      (else			+1)))))

;;; --------------------------------------------------------------------

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
	  (node (binary-tree-find root (make-comparison-proc 0))))
     (unbalanced-tree-remove! root)
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
   (let* ((root (tree 5 3 8 1 4 6 9 0 2 7 10))
	  (node (binary-tree-find root (make-comparison-proc 10))))
     (unbalanced-tree-remove! root)
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
   (let* ((root (tree 5 3 8 1 4 6 9 0 2 7 10))
	  (node (binary-tree-find root (make-comparison-proc 1))))
     (unbalanced-tree-remove! root)
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
   (let* ((root (tree 5 3 8 1 4 6 9 0 2 7 10))
	  (node (binary-tree-find root (make-comparison-proc 5))))
     (unbalanced-tree-remove! root)
     (binary-tree-valid? root key<)))

  ;; 5------8--9--10
  ;; |      |
  ;; 3--4   6--7
  ;; |
  ;; 1--2
  ;; |
  ;; 0
  ;;
  (for-all (lambda (target)
	     (check-for-true
	      (let* ((root (tree 5 3 8 1 4 6 9 0 2 7 10))
		     (node (binary-tree-find root (make-comparison-proc 0))))
		(unbalanced-tree-remove! root)
		(binary-tree-valid? root key<))))
    '(0 1 2 3 4 5 6 7 8 9 10))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'binary-tree-fold-in-order	'scheme-indent-function 1)
;; End:
