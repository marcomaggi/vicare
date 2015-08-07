;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for chain containers
;;;Date: Fri Aug  7, 2015
;;;
;;;Abstract
;;;
;;;	Chain containers a doubly-linked lists.
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
  (vicare containers chains)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: chain containers\n")


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


(parametrise ((check-test-name 'operations))

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


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'chain-fold-left-forwards		'scheme-indent-function 1)
;; eval: (put 'chain-fold-right-forwards	'scheme-indent-function 1)
;; eval: (put 'chain-fold-left-backwards	'scheme-indent-function 1)
;; eval: (put 'chain-fold-right-backwards	'scheme-indent-function 1)
;; End:

