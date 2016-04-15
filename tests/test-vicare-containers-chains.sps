;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for chain containers
;;;Date: Fri Aug  7, 2015
;;;
;;;Abstract
;;;
;;;	Chain containers a doubly-linked lists.
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare containers chains)
  (vicare containers chains sort)
  (vicare containers iteration-thunks)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: chain containers\n")


;;;; helpers

(define (make-list-20)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   19)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

;;; --------------------------------------------------------------------

(define (make-list-100)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   99)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

;;; --------------------------------------------------------------------

;;A list of 5 objects, enqueued in a deque, is fully stored in a single buffer.
;;
(define-constant LIST-5			'(0 1 2 3 4))
(define-constant LIST-5-REVERSED	(reverse LIST-5))
(define-constant LIST-5-NEGATED		(map - LIST-5))

;;A list of 20 objects, enqueued in a deque, is stored in two buffers.
;;
(define-constant LIST-20		(make-list-20))
(define-constant LIST-20-REVERSED	(reverse LIST-20))
(define-constant LIST-20-NEGATED	(map - LIST-20))

;;A list of 100 objects, enqueued in a deque, is stored in multiple buffers.
;;
(define-constant LIST-100		(make-list-100))
(define-constant LIST-100-REVERSED	(reverse LIST-100))
(define-constant LIST-100-NEGATED	(map - LIST-100))


(parametrise ((check-test-name	'making))

  (check
      (chain-link? (make-chain-link 1))
    => #t)

  #t)


(parametrise ((check-test-name	'pred))

  (check-for-false (chain-link? 123))
  (check-for-true  (chain-link? (make-chain-link 1)))
  (check-for-true  (chain-link? (chain 1 2 3)))

;;; --------------------------------------------------------------------

  (check-for-false (chain? 123))
  (check-for-true  (chain? '()))
  (check-for-true  (chain? (make-chain-link 1)))
  (check-for-true  (chain? (chain 1 2 3)))

  #t)


(parametrise ((check-test-name	'object))

  (define who 'test)

;;; object

  (check
      (let ((link (make-chain-link 123)))
	(chain-link-ref link))
    => 123)

  (check
      (let ((link (make-chain-link 123)))
	(chain-link-set! link 456)
	(chain-link-ref  link))
    => 456)

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (chain-link-hash (chain 1 2 3))))

  (check
      (let ((A (chain 1 2 3))
	    (B (chain 1 2 3))
	    (T (make-hashtable chain-link-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (chain 1 2 3)))
	(chain-link-property-list S))
    => '())

  (check
      (let ((S (chain 1 2 3)))
	(chain-link-putprop S 'ciao 'salut)
	(chain-link-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (chain 1 2 3)))
	(chain-link-getprop S 'ciao))
    => #f)

  (check
      (let ((S (chain 1 2 3)))
	(chain-link-putprop S 'ciao 'salut)
	(chain-link-remprop S 'ciao)
	(chain-link-getprop S 'ciao))
    => #f)

  (check
      (let ((S (chain 1 2 3)))
	(chain-link-putprop S 'ciao 'salut)
	(chain-link-putprop S 'hello 'ohayo)
	(list (chain-link-getprop S 'ciao)
	      (chain-link-getprop S 'hello)))
    => '(salut ohayo))

  #f)


(parametrise ((check-test-name 'inspect))

;;; length

  (check
      (chain-length (make-chain-link 1))
    => 1)

  (check
      (chain-length (chain))
    => 0)

  (check
      (chain-length (chain 1))
    => 1)

  (check
      (chain-length (chain 1 2 3))
    => 3)

;;; --------------------------------------------------------------------
;;; forwards length

  (check
      (chain-forwards-length (chain))
    => 0)

  (check
      (chain-forwards-length (chain 1))
    => 1)

  (check
      (chain-forwards-length (chain 1 2 3))
    => 3)

  (check
      (let ((C1 (make-chain-link 1))
	    (C2 (make-chain-link 2))
	    (C3 (make-chain-link 3)))
	(chain-link-next-set! C1 C2)
	(chain-link-next-set! C2 C3)
	(values (chain-forwards-length C1)
		(chain-forwards-length C2)
		(chain-forwards-length C3)))
    => 3 2 1)

;;; --------------------------------------------------------------------
;;; backwards length

  (check
      (chain-backwards-length (chain))
    => 0)

  (check
      (chain-backwards-length (chain-rear (chain 1)))
    => 1)

  (check
      (chain-backwards-length (chain-rear (chain 1 2 3)))
    => 3)

  (check
      (let ((C1 (make-chain-link 1))
	    (C2 (make-chain-link 2))
	    (C3 (make-chain-link 3)))
	(chain-link-next-set! C1 C2)
	(chain-link-next-set! C2 C3)
	(values (chain-backwards-length C1)
		(chain-backwards-length C2)
		(chain-backwards-length C3)))
    => 1 2 3)

  #t)


(parametrise ((check-test-name	'access))

;;; --------------------------------------------------------------------
;;; chain-front

  (check
      (let* ((C (chain 1 2 3 4))
	     (K (chain-front C)))
	(chain-link-ref K))
    => 1)

  (check
      (let* ((C (chain-link-next (chain 1 2 3 4)))
	     (K (chain-front C)))
	(chain-link-ref K))
    => 1)

  (check
      (let* ((C (chain-link-next (chain-link-next (chain 1 2 3 4))))
	     (K (chain-front C)))
	(chain-link-ref K))
    => 1)

  (check
      (let* ((C (chain-rear (chain 1 2 3 4)))
	     (K (chain-front C)))
	(chain-link-ref K))
    => 1)

;;; --------------------------------------------------------------------
;;; chain-rear

  (check
      (let* ((C (chain 1 2 3 4))
	     (K (chain-rear C)))
	(chain-link-ref K))
    => 4)

  (check
      (let* ((C (chain-link-next (chain 1 2 3 4)))
	     (K (chain-rear C)))
	(chain-link-ref K))
    => 4)

  (check
      (let* ((C (chain-link-next (chain-link-next (chain 1 2 3 4))))
	     (K (chain-rear C)))
	(chain-link-ref K))
    => 4)

  (check
      (let* ((C (chain-rear (chain 1 2 3 4)))
	     (K (chain-rear C)))
	(chain-link-ref K))
    => 4)

;;; --------------------------------------------------------------------
;;; chain-push-front!

  (check
      (let* ((C '())
	     (F (chain-push-front! C (make-chain-link 0))))
	(chain->list F))
    => '(0))

  (check
      (let* ((C (chain 1 2 3 4))
	     (L (make-chain-link 0))
	     (F (chain-push-front! C L)))
	(chain->list F))
    => '(0 1 2 3 4))

  (check
      (let* ((C (chain-rear (chain 1 2 3 4)))
	     (L (make-chain-link 0))
	     (F (chain-push-front! C L)))
	(chain->list F))
    => '(0 1 2 3 4))

;;; --------------------------------------------------------------------
;;; chain-push-rear!

  (check
      (let* ((C '())
	     (F (chain-push-rear! C (make-chain-link 0))))
	(chain->list F))
    => '(0))

  (check
      (let* ((C (chain 1 2 3 4))
	     (L (make-chain-link 5))
	     (F (chain-push-rear! C L)))
	(chain->list F))
    => '(1 2 3 4 5))

  (check
      (let* ((C (chain-link-next (chain 1 2 3 4)))
	     (L (make-chain-link 5))
	     (F (chain-push-rear! C L)))
	(chain->list F))
    => '(1 2 3 4 5))

;;; --------------------------------------------------------------------
;;; chain-pop-front!

  (check
      (try
	  (chain-pop-front! (chain))
	(catch E
	  ((&assertion)
	   #t)
	  (else E)))
    => #t)

  (check
      (let ((C (chain 0)))
	(receive (F C)
	    (chain-pop-front! C)
	  (values (chain-link-ref F) (chain->list C))))
    => 0 '())

  (check
      (let ((C (chain 0 1 2 3 4)))
	(receive (F C)
	    (chain-pop-front! C)
	  (values (chain-link-ref F) (chain->list C))))
    => 0 '(1 2 3 4))

;;; --------------------------------------------------------------------
;;; chain-pop-rear!

  (check
      (try
	  (chain-pop-rear! (chain))
	(catch E
	  ((&assertion)
	   #t)
	  (else E)))
    => #t)

  (check
      (let ((C (chain 0)))
	(receive (R C)
	    (chain-pop-rear! C)
	  (values (chain-link-ref R) (chain->list C))))
    => 0 '())

  (check
      (let ((C (chain 0)))
	(receive (R C)
	    (chain-pop-rear! C)
	  (values (chain-link-ref R) C)))
    => 0 '())

  (check
      (let ((C (chain 0 1 2 3 4)))
	(receive (R C)
	    (chain-pop-rear! C)
	  (values (chain-link-ref R) (chain->list C))))
    => 4 '(0 1 2 3))

;;; --------------------------------------------------------------------
;;; chain-index-forwards

  (check
      (try
	  (chain-index-forwards (chain) 0)
	(catch E
	  ((&assertion)
	   #t)
	  (else E)))
    => #t)

  (check
      (try
	  (chain-index-forwards (chain 10 11 12 13 14) 5)
	(catch E
	  ((&assertion)
	   #t)
	  (else E)))
    => #t)

  (check (chain-index-forwards (chain 10 11 12 13 14) 0) => 10)
  (check (chain-index-forwards (chain 10 11 12 13 14) 1) => 11)
  (check (chain-index-forwards (chain 10 11 12 13 14) 2) => 12)
  (check (chain-index-forwards (chain 10 11 12 13 14) 3) => 13)
  (check (chain-index-forwards (chain 10 11 12 13 14) 4) => 14)

  (check
      (let ((C (chain-link-next (chain-link-next (chain 10 11 12 13 14)))))
	(chain-index-forwards C 0))
    => 12)

  (check
      (let ((C (chain-link-next (chain-link-next (chain 10 11 12 13 14)))))
	(chain-index-forwards C 1))
    => 13)

  (check
      (let ((C (chain-link-next (chain-link-next (chain 10 11 12 13 14)))))
	(chain-index-forwards C 2))
    => 14)

;;; --------------------------------------------------------------------
;;; chain-index-backwards

  (check
      (try
	  (chain-index-backwards (chain) 0)
	(catch E
	  ((&assertion)
	   #t)
	  (else E)))
    => #t)

  (check
      (try
	  (chain-index-backwards (chain-rear (chain 10 11 12 13 14)) 5)
	(catch E
	  ((&assertion)
	   #t)
	  (else E)))
    => #t)

  (check (chain-index-backwards (chain-rear (chain 10 11 12 13 14)) 0) => 14)
  (check (chain-index-backwards (chain-rear (chain 10 11 12 13 14)) 1) => 13)
  (check (chain-index-backwards (chain-rear (chain 10 11 12 13 14)) 2) => 12)
  (check (chain-index-backwards (chain-rear (chain 10 11 12 13 14)) 3) => 11)
  (check (chain-index-backwards (chain-rear (chain 10 11 12 13 14)) 4) => 10)

  (check
      (let ((C (chain-link-next (chain-link-next (chain 10 11 12 13 14)))))
	(chain-index-backwards C 0))
    => 12)

  (check
      (let ((C (chain-link-next (chain-link-next (chain 10 11 12 13 14)))))
	(chain-index-backwards C 1))
    => 11)

  (check
      (let ((C (chain-link-next (chain-link-next (chain 10 11 12 13 14)))))
	(chain-index-backwards C 2))
    => 10)

  #t)


(parametrise ((check-test-name	'access/remove))

  (check
      (let ((A (make-chain-link 0)))
	(chain-link-remove! A)
	(chain->list A))
    => '(0))

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! C)
	(values (chain->list A)
		(chain->list C)
		(chain->list E)))
    => '(0 1 3 4) '(2) '(0 1 3 4))

;;; --------------------------------------------------------------------
;;; removing from front

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! A)
	(values (chain->list A)
		(chain->list B)))
    => '(0) '(1 2 3 4))

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! A)
	(chain-link-remove! B)
	(values (chain->list A)
		(chain->list B)
		(chain->list C)))
    => '(0) '(1) '(2 3 4))

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! A)
	(chain-link-remove! B)
	(chain-link-remove! C)
	(values (chain->list A)
		(chain->list B)
		(chain->list C)
		(chain->list D)))
    => '(0) '(1) '(2) '(3 4))

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! A)
	(chain-link-remove! B)
	(chain-link-remove! C)
	(chain-link-remove! D)
	(values (chain->list A)
		(chain->list B)
		(chain->list C)
		(chain->list D)
		(chain->list E)))
    => '(0) '(1) '(2) '(3) '(4))

;;; --------------------------------------------------------------------
;;; removing from rear

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! E)
	(values (chain->list A)
		(chain->list E)))
    => '(0 1 2 3) '(4))

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! E)
	(chain-link-remove! D)
	(values (chain->list A)
		(chain->list D)
		(chain->list E)))
    => '(0 1 2) '(3) '(4))

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! E)
	(chain-link-remove! D)
	(chain-link-remove! C)
	(values (chain->list A)
		(chain->list C)
		(chain->list D)
		(chain->list E)))
    => '(0 1) '(2) '(3) '(4))

  (check
      (let* ((A (chain 0 1 2 3 4))
	     (B (chain-link-next A))
	     (C (chain-link-next B))
	     (D (chain-link-next C))
	     (E (chain-link-next D)))
	(chain-link-remove! B)
	(chain-link-remove! C)
	(chain-link-remove! D)
	(chain-link-remove! E)
	(values (chain->list A)
		(chain->list B)
		(chain->list C)
		(chain->list D)
		(chain->list E)))
    => '(0) '(1) '(2) '(3) '(4))

  #t)


(parametrise ((check-test-name	'copy))

  (check
      (chain-copy-forwards (chain))
    => '())

  (check
      (chain->list (chain-copy-forwards (chain 0)))
    => '(0))

  (check
      (chain->list (chain-copy-forwards (chain 0 1 2 3 4)))
    => '(0 1 2 3 4))

  #t)


(parametrise ((check-test-name	'reverse))

  (check
      (chain-reverse-forwards (chain))
    => '())

  (check
      (chain->list (chain-reverse-forwards (chain 0)))
    => '(0))

  (check
      (chain->list (chain-reverse-forwards (chain 0 1 2 3 4)))
    => '(4 3 2 1 0))

  #t)


(parametrise ((check-test-name	'append))

  (check
      (chain-append-forwards)
    => '())

;;; --------------------------------------------------------------------

  (check
      (chain-append-forwards (chain))
    => '())

  (check
      (chain->list (chain-append-forwards (chain 0)))
    => '(0))

  (check
      (chain->list (chain-append-forwards (chain 0 1 2 3 4)))
    => '(0 1 2 3 4))

;;; --------------------------------------------------------------------

  (check
      (chain-append-forwards (chain) (chain))
    => '())

  (check
      (chain->list (chain-append-forwards (chain 0) (chain 1)))
    => '(0 1))

  (check
      (chain->list (chain-append-forwards (chain 0 1 2 3 4) (chain 5 6 7 8 9)))
    => '(0 1 2 3 4 5 6 7 8 9))

  (check
      (chain->list (chain-append-forwards (chain 0 1 2) (chain)))
    => '(0 1 2))

  (check
      (chain->list (chain-append-forwards (chain) (chain 0 1 2)))
    => '(0 1 2))

;;; --------------------------------------------------------------------

  (check
      (chain-append-forwards (chain) (chain) (chain))
    => '())

  (check
      (chain->list (chain-append-forwards (chain 0) (chain 1) (chain 2)))
    => '(0 1 2))

  (check
      (chain->list (chain-append-forwards (chain 0 1 2) (chain 3 4 5) (chain 6 7 8 9)))
    => '(0 1 2 3 4 5 6 7 8 9))

;;;

  (check
      (chain->list (chain-append-forwards (chain) (chain 0 1 2) (chain 3 4 5)))
    => '(0 1 2 3 4 5))

  (check
      (chain->list (chain-append-forwards (chain 0 1 2) (chain) (chain 3 4 5)))
    => '(0 1 2 3 4 5))

  (check
      (chain->list (chain-append-forwards (chain 0 1 2) (chain 3 4 5) (chain)))
    => '(0 1 2 3 4 5))

;;;

  (check
      (chain->list (chain-append-forwards (chain) (chain) (chain 0 1 2)))
    => '(0 1 2))

  (check
      (chain->list (chain-append-forwards (chain) (chain 0 1 2) (chain)))
    => '(0 1 2))

  (check
      (chain->list (chain-append-forwards (chain 0 1 2) (chain) (chain)))
    => '(0 1 2))

  #t)


(parametrise ((check-test-name	'folding))

  (check
      (let ((C (chain)))
	(chain-fold-left-forwards
	    (lambda (knil obj)
	      (cons obj knil))
	  '() C))
    => '())

  (check
      (let ((C (chain 0 1 2 3 4)))
	(chain-fold-left-forwards (lambda (knil obj)
				    (cons obj knil))
	  '() C))
    => '(4 3 2 1 0))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-left-forwards
	  (lambda (knil obj)
	    (cons obj knil))
	'()
	(chain 0 1 2 3 4))
    => '(4 3 2 1 0))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-left-forwards
	  (lambda (knil obj)
	    (cons obj knil))
	'()
	(chain-link-next
	 (chain-link-next
	  (chain 0 1 2 3 4))))
    => '(4 3 2))

;;; --------------------------------------------------------------------

  (check
      (let ((C (chain)))
	(chain-fold-right-forwards (lambda (obj knil)
				     (cons obj knil))
	  '() C))
    => '())

  (check
      (let ((C (chain 0 1 2 3 4)))
	(chain-fold-right-forwards (lambda (obj knil)
				     (cons obj knil))
	  '() C))
    => '(0 1 2 3 4))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-right-forwards
	  (lambda (obj knil)
	    (cons obj knil))
	'()
	(chain 0 1 2 3 4))
    => '(0 1 2 3 4))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-right-forwards
	  (lambda (obj knil)
	    (cons obj knil))
	'()
	(chain-link-next
	 (chain-link-next
	  (chain 0 1 2 3 4))))
    => '(2 3 4))

;;; --------------------------------------------------------------------

  (check
      (let ((C (chain)))
  	(chain-fold-left-backwards (lambda (knil obj)
  				     (cons obj knil))
	  '() C))
    => '())

  (check
      (let ((C (chain-rear (chain 0 1 2 3 4))))
  	(chain-fold-left-backwards (lambda (knil obj)
  				     (cons obj knil))
	  '() C))
    => '(0 1 2 3 4))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-left-backwards
	  (lambda (knil obj)
	    (cons obj knil))
	'()
	(chain-rear (chain 0 1 2 3 4)))
    => '(0 1 2 3 4))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-left-backwards
	  (lambda (knil obj)
	    (cons obj knil))
	'()
	(chain-link-prev
	 (chain-link-prev
	  (chain-rear (chain 0 1 2 3 4)))))
    => '(0 1 2))

;;; --------------------------------------------------------------------

  (check
      (let ((C (chain)))
  	(chain-fold-right-backwards (lambda (obj knil)
				      (cons obj knil))
	  '() C))
    => '())

  (check
      (let ((C (chain-rear (chain 0 1 2 3 4))))
  	(chain-fold-right-backwards (lambda (obj knil)
				      (cons obj knil))
	  '() C))
    => '(4 3 2 1 0))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-right-backwards (lambda (obj knil)
				    (cons obj knil))
	'()
	(chain-rear (chain 0 1 2 3 4)))
    => '(4 3 2 1 0))

  ;;Documentation example.
  ;;
  (check
      (chain-fold-right-backwards (lambda (obj knil)
				    (cons obj knil))
	'()
	(chain-link-prev
	 (chain-link-prev
	  (chain-rear (chain 0 1 2 3 4)))))
    => '(2 1 0))

  #t)


(parametrise ((check-test-name	'map))

;;; mapping one argument

  (check
      (chain-map-forwards
	  add1
	(chain))
    => '())

  (check
      (chain->list
       (chain-map-forwards
	   add1
	 (chain 10)))
    => '(11))

  (check
      (chain->list
       (chain-map-forwards
	   add1
	 (chain 10 20 30)))
    => '(11 21 31))

;;; mapping two arguments

  (check
      (chain-map-forwards
	  +
	(chain)
	(chain))
    => '())

  (check
      (chain->list
       (chain-map-forwards
	   +
	 (chain 1)
	 (chain 10)))
    => '(11))

  (check
      (chain->list
       (chain-map-forwards
	   +
	 (chain 1  2  3)
	 (chain 10 20 30)))
    => '(11 22 33))

;;; mapping three arguments

  (check
      (chain-map-forwards
	  +
	(chain)
	(chain)
	(chain))
    => '())

  (check
      (chain->list
       (chain-map-forwards
	   +
	 (chain 1)
	 (chain 10)
	 (chain 100)))
    => '(111))

  (check
      (chain->list
       (chain-map-forwards
	   +
	 (chain 1 2 3)
	 (chain 10 20 30)
	 (chain 100 200 300)))
    => '(111 222 333))

  ;;Different list length.
  ;;
  (check
      (chain->list
       (chain-map-forwards
	   +
	 (chain 1 2 3)
	 (chain 10 20 30 40)
	 (chain 100 200 300)))
    => '(111 222 333))

  #t)


(parametrise ((check-test-name	'for-each))

  (define (fun . obj*)
    ;;Remember that ADD-RESULT returns its argument.
    (add-result (list->vector obj*)))

;;; --------------------------------------------------------------------
;;; for-each'ing one argument

  (check
      (with-result
	(void-object?(chain-for-each-forwards
			 add-result
		       (chain))))
    => '(#t ()))


  (check
      (with-result
	(chain-for-each-forwards
	    add-result
	  (chain 10)))
    => '(10 (10)))


  (check
      (with-result
	(chain-for-each-forwards
	    add-result
	  (chain 10 20 30)))
    => '(30 (10 20 30)))

;;; for-each'ing two arguments

  (check
      (with-result
	(void-object? (chain-for-each-forwards
			  fun
			(chain)
			(chain))))
    => '(#t ()))

  (check
      (with-result
	(chain-for-each-forwards
	    fun
	  (chain 1)
	  (chain 10)))
    => '(#(1 10) (#(1 10))))

  (check
      (with-result
	(chain-for-each-forwards
	    fun
	  (chain 1  2  3)
	  (chain 10 20 30)))
    => '(#(3 30) (#(1 10) #(2 20) #(3 30))))

;;; for-each'ing three arguments

  (check
      (with-result
	(void-object? (chain-for-each-forwards
			  fun
			(chain)
			(chain)
			(chain))))
    => '(#t ()))

  (check
      (with-result
	(chain-for-each-forwards
	    fun
	  (chain 1)
	  (chain 10)
	  (chain 100)))
    => '(#(1 10 100) (#(1 10 100))))

  (check
      (with-result
	(chain-for-each-forwards
	    fun
	  (chain 1 2 3)
	  (chain 10 20 30)
	  (chain 100 200 300)))
    => '(#(3 30 300) (#(1 10 100) #(2 20 200) #(3 30 300))))

  ;;Different list length.
  ;;
  (check
      (with-result
	(chain-for-each-forwards
	    fun
	  (chain 1 2 3)
	  (chain 10 20 30 40)
	  (chain 100 200 300)))
    => '(#(3 30 300) (#(1 10 100) #(2 20 200) #(3 30 300))))

  #t)


(parametrise ((check-test-name	'for-all))

  (define (fun . obj*)
    (add-result (list->vector obj*))
    ;;Remember that FOR-ALL returns the result of the last application.
    (for-all values obj*))

;;; --------------------------------------------------------------------
;;; for-all'ing one argument

  (check
      (with-result
	(chain-for-all-forwards
	    add-result
	  (chain)))
    => '(#t ()))


  (check
      (with-result
	(chain-for-all-forwards
	    add-result
	  (chain 10)))
    => '(10 (10)))


  (check
      (with-result
	(chain-for-all-forwards
	    add-result
	  (chain 10 20 30)))
    => '(30 (10 20 30)))

  (check
      (with-result
	(chain-for-all-forwards
	    add-result
	  (chain 10 #f 30)))
    => '(#f (10 #f)))

;;; for-all'ing two arguments

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain)
	  (chain)))
    => '(#t ()))

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain 1)
	  (chain 10)))
    => '(10 (#(1 10))))

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain 1  2  3)
	  (chain 10 20 30)))
    => '(30 (#(1 10) #(2 20) #(3 30))))

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain 1  2  3)
	  (chain 10 #f 30)))
    => '(#f (#(1 10) #(2 #f))))

;;; for-all'ing three arguments

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain)
	  (chain)
	  (chain)))
    => '(#t ()))

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain 1)
	  (chain 10)
	  (chain 100)))
    => '(100 (#(1 10 100))))

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain 1 2 3)
	  (chain 10 20 30)
	  (chain 100 200 300)))
    => '(300 (#(1 10 100) #(2 20 200) #(3 30 300))))

  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain 1 2 3)
	  (chain 10 #f 30)
	  (chain 100 200 300)))
    => '(#f (#(1 10 100) #(2 #f 200))))

  ;;Different list length.
  ;;
  (check
      (with-result
	(chain-for-all-forwards
	    fun
	  (chain 1 2 3)
	  (chain 10 20 30 40)
	  (chain 100 200 300)))
    => '(300 (#(1 10 100) #(2 20 200) #(3 30 300))))

  #t)


(parametrise ((check-test-name	'exists))

  (define (fun . obj*)
    (add-result (list->vector obj*))
    (find (lambda (obj)
	    (<= 30 obj))
      obj*))

;;; --------------------------------------------------------------------
;;; exists'ing one argument

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain)))
    => '(#f ()))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

;;; exists'ing two arguments

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain)
	  (chain)))
    => '(#f ()))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1)
	  (chain 10)))
    => '(#f (#(1 10))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1)
	  (chain 30)))
    => '(30 (#(1 30))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1  2  3)
	  (chain 10 20 30)))
    => '(30 (#(1 10) #(2 20) #(3 30))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1  2  3)
	  (chain 10 20 25)))
    => '(#f (#(1 10) #(2 20) #(3 25))))

;;; exists'ing three arguments

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain)
	  (chain)
	  (chain)))
    => '(#f ()))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1)
	  (chain 10)
	  (chain 20)))
    => '(#f (#(1 10 20))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1)
	  (chain 10)
	  (chain 100)))
    => '(100 (#(1 10 100))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1 2 3)
	  (chain 1.1 2.1 30)
	  (chain 1.2 2.2 300)))
    => '(30 (#(1 1.1 1.2) #(2 2.1 2.2) #(3 30 300))))

  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1 2 3)
	  (chain 1.1 2.1 3.1)
	  (chain 1.2 2.2 3.2)))
    => '(#f (#(1 1.1 1.2) #(2 2.1 2.2) #(3 3.1 3.2))))

  ;;Different list length.
  ;;
  (check
      (with-result
	(chain-exists-forwards
	    fun
	  (chain 1 2 3)
	  (chain 1.1 2.1 3.1 4.1)
	  (chain 1.2 2.2 3.2)))
    => '(#f (#(1 1.1 1.2) #(2 2.1 2.2) #(3 3.1 3.2))))

  #t)


(parametrise ((check-test-name	'find))

  (check (chain-find-forwards even? (chain))				=> #f)
  (check (chain-find-forwards even? (chain) 'not-found)			=> 'not-found)

  (check (chain-find-forwards even? (chain 1 3 5 7))			=> #f)
  (check (chain-find-forwards even? (chain 1 3 5 7) 'not-found)		=> 'not-found)

  (check (chain-find-forwards even? (chain 1 3 5 6 7))			=> 6)
  (check (chain-find-forwards even? (chain 1 3 5 6 7) 'not-found)	=> 6)

  #t)


(parametrise ((check-test-name	'filter))

  (check
      (chain->list (chain-filter-forwards even? (chain)))
    => '())

  (check
      (chain->list (chain-filter-forwards even? (chain 1 3 5 7)))
    => '())

  (check
      (chain->list (chain-filter-forwards even? (chain 1 3 5 6 7)))
    => '(6))

  (check
      (chain->list (chain-filter-forwards even? (chain 1 2 3 4 5 6 7)))
    => '(2 4 6))

  (check
      (chain->list (chain-filter-forwards even? (chain 1 2 3 4 5 6 7 8)))
    => '(2 4 6 8))

  (check
      (chain->list (chain-filter-forwards even? (chain 2 4 6 8)))
    => '(2 4 6 8))

  #t)


(parametrise ((check-test-name	'partition))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?body ?expected-in ?expected-ou)
       (check
	   (receive (in ou)
	       ?body
	     (values (chain->list in) (chain->list ou)))
	 => ?expected-in ?expected-ou))
      ))

;;; --------------------------------------------------------------------

  (doit (chain-partition-forwards even? (chain))			'() '())

  (doit (chain-partition-forwards even? (chain 1 3 5 7))		'() '(1 3 5 7))
  (doit (chain-partition-forwards even? (chain 2 4 6 8))		'(2 4 6 8) '())

  (doit (chain-partition-forwards even? (chain 1 3 5 6 7))		'(6) '(1 3 5 7))

  (doit (chain-partition-forwards even? (chain 1 2 3 4 5 6 7))		'(2 4 6) '(1 3 5 7))

  (doit (chain-partition-forwards even? (chain 1 2 3 4 5 6 7 8))	'(2 4 6 8) '(1 3 5 7))

  #t)


(parametrise ((check-test-name 'conversion))

  (check
      (chain->list (chain))
    => '())

  (check
      (chain->list (chain 1))
    => '(1))

  (check
      (chain->list (chain 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (chain->vector (chain))
    => '#())

  (check
      (chain->vector (chain 1))
    => '#(1))

  (check
      (chain->vector (chain 1 2 3))
    => '#(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (chain->list (list->chain '()))
    => '())

  (check
      (chain->list (list->chain '(1)))
    => '(1))

  (check
      (chain->list (list->chain '(1 2 3)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (chain->vector (vector->chain '#()))
    => '#())

  (check
      (chain->vector (vector->chain '#(1)))
    => '#(1))

  (check
      (chain->vector (vector->chain '#(1 2 3)))
    => '#(1 2 3))

  #t)


(parametrise ((check-test-name	'sorting))

  (define (permutations ls)
    (define (rem* ls)
      (if (null? ls)
	  '()
	(begin
	  (cons (cdr ls)
		(map (lambda (a)
		       (cons (car ls) a))
		  (rem* (cdr ls)))))))
    (if (null? ls)
	'(())
      (begin
	(apply append (map (lambda (x a*)
			     (map (lambda (a) (cons x a)) a*))
			ls
			(map permutations (rem* ls)))))))

;;; --------------------------------------------------------------------

  (check
      (let* ((C1 (chain 0 1 2 3 4 5))
	     (C2 (chain-sort-forwards < C1)))
	(chain->list C2))
    => '(0 1 2 3 4 5))

  (check
      (let* ((C1 (chain 5 4 3 2 1 0))
	     (C2 (chain-sort-forwards < C1)))
	(chain->list C2))
    => '(0 1 2 3 4 5))

  (check
      (let* ((C1 (chain 0 4 3 1 2 5))
	     (C2 (chain-sort-forwards < C1)))
	(chain->list C2))
    => '(0 1 2 3 4 5))

;;; --------------------------------------------------------------------

  (check
      (for-all (lambda (ell)
		 #;(debug-print ell)
		 (equal? LIST-5 (chain->list (chain-sort-forwards < (list->chain ell)))))
	(permutations LIST-5))
    => #t)

  #t)


(parametrise ((check-test-name	'iteration-thunks))

  (define (xcons a b)
    (cons b a))

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-chain-forwards-iteration-thunk (chain)))
    => '())

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-chain-forwards-iteration-thunk (chain 0 1 2 3 4 5)))
    => '(5 4 3 2 1 0))

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-chain-backwards-iteration-thunk (chain)))
    => '())

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-chain-backwards-iteration-thunk
	   (chain-rear (chain 0 1 2 3 4 5))))
    => '(0 1 2 3 4 5))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'chain-fold-left-forwards		'scheme-indent-function 1)
;; eval: (put 'chain-fold-right-forwards	'scheme-indent-function 1)
;; eval: (put 'chain-fold-left-backwards	'scheme-indent-function 1)
;; eval: (put 'chain-fold-right-backwards	'scheme-indent-function 1)
;; eval: (put 'chain-map-forwards		'scheme-indent-function 1)
;; eval: (put 'chain-map-backwards		'scheme-indent-function 1)
;; eval: (put 'chain-for-each-forwards		'scheme-indent-function 1)
;; eval: (put 'chain-for-each-backwards		'scheme-indent-function 1)
;; eval: (put 'chain-for-all-forwards		'scheme-indent-function 1)
;; eval: (put 'chain-for-all-backwards		'scheme-indent-function 1)
;; eval: (put 'chain-exists-forwards		'scheme-indent-function 1)
;; eval: (put 'chain-exists-backwards		'scheme-indent-function 1)
;; eval: (put 'chain-find-forwards		'scheme-indent-function 1)
;; eval: (put 'chain-find-backwards		'scheme-indent-function 1)
;; End:

