;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: iterators for sequences of values
;;;Date: Thu Jul 21, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers iteration-thunks)
  (export
    make-list-iteration-thunk
    make-spine-iteration-thunk
    make-string-iteration-thunk
    make-vector-iteration-thunk
    make-bytevector-u8-iteration-thunk
    make-bytevector-s8-iteration-thunk

    iteration-thunk-fold		$iteration-thunk-fold
    iteration-thunk-map			$iteration-thunk-map
    iteration-thunk-for-each		$iteration-thunk-for-each
    iteration-thunk-for-all		$iteration-thunk-for-all
    iteration-thunk-exists		$iteration-thunk-exists
    iteration-thunk-find		$iteration-thunk-find
    iteration-thunk-filter		$iteration-thunk-filter
    iteration-thunk-partition		$iteration-thunk-partition)
  (import (vicare)
    (vicare system $fx)
    (vicare system $strings)
    (vicare system $vectors)
    (vicare system $bytevectors))


;;;; common iterators

(define (make-list-iteration-thunk ell)
  (lambda ()
    (cond ((pair? ell)
	   (receive-and-return (item)
	       (car ell)
	     (set! ell (cdr ell))))
	  ((null? ell)
	   (void))
	  (else
	   (assertion-violation 'list-iteration-thunk
	     "expected pair as list object" ell)))))

(define (make-spine-iteration-thunk ell)
  (lambda ()
    (cond ((pair? ell)
	   (receive-and-return (item)
	       ell
	     (set! ell (cdr ell))))
	  ((null? ell)
	   (void))
	  (else
	   (assertion-violation 'spine-iteration-thunk
	     "expected pair as list object" ell)))))

(define* (make-string-iteration-thunk {str string?})
  (let ((str.idx 0)
	(str.len ($string-length str)))
    (lambda ()
      (if ($fx< str.idx str.len)
	  (receive-and-return (ch)
	      ($string-ref str str.idx)
	    (++ str.idx))
	(void)))))

(define* (make-vector-iteration-thunk {vec vector?})
  (let ((vec.idx 0)
	(vec.len ($vector-length vec)))
    (lambda ()
      (if ($fx< vec.idx vec.len)
	  (receive-and-return (ch)
	      ($vector-ref vec vec.idx)
	    (++ vec.idx))
	(void)))))

(define* (make-bytevector-u8-iteration-thunk {bv bytevector?})
  (let ((bv.idx 0)
	(bv.len ($bytevector-length bv)))
    (lambda ()
      (if ($fx< bv.idx bv.len)
	  (receive-and-return (ch)
	      ($bytevector-u8-ref bv bv.idx)
	    (++ bv.idx))
	(void)))))

(define* (make-bytevector-s8-iteration-thunk {bv bytevector?})
  (let ((bv.idx 0)
	(bv.len ($bytevector-length bv)))
    (lambda ()
      (if ($fx< bv.idx bv.len)
	  (receive-and-return (ch)
	      ($bytevector-s8-ref bv bv.idx)
	    (++ bv.idx))
	(void)))))


;;;; folding

(case-define* iteration-thunk-fold

  (({kons procedure?} knil {fun procedure?})
   ($iteration-thunk-fold kons knil fun))

  (({kons procedure?} knil {iter1 procedure?} {iter2 procedure?})
   ($iteration-thunk-fold kons knil iter1 iter2))

  (({kons procedure?} knil {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iteration-thunk-fold-multi kons knil iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iteration-thunk-fold

  ((kons knil fun)
   (let ((item (fun)))
     (if (void-object? item)
	 knil
       ($iteration-thunk-fold kons (kons knil item) fun))))

  ((kons knil iter1 iter2)
   (let ((item1 (iter1))
	 (item2 (iter2)))
     (if (or (void-object? item1)
	     (void-object? item2))
	 knil
       ($iteration-thunk-fold kons (kons knil item1 item2) iter1 iter2))))

  ((kons knil iter1 iter2 . iter*)
   ($iteration-thunk-fold-multi kons knil iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iteration-thunk-fold-multi kons knil iter1 iter2 iter*)
  (let ((item1 (iter1))
	(item2 (iter2))
	(item* (map (lambda (it)
		      (apply it '()))
		 iter*)))
    (if (or (void-object? item1)
	    (void-object? item2)
	    (exists void-object? item*))
	knil
      ($iteration-thunk-fold-multi kons (apply kons knil item1 item2 item*) iter1 iter2 iter*))))


;;;; operations: map

(case-define* iteration-thunk-map
  (({acceptor procedure?} {fun procedure?} {iter procedure?})
   ($iteration-thunk-map acceptor fun iter))

  (({acceptor procedure?} {fun procedure?} {iter1 procedure?} {iter2 procedure?})
   ($iteration-thunk-map acceptor fun iter1 iter2))

  (({acceptor procedure?} {fun procedure?} {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iteration-thunk-map-multi acceptor fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iteration-thunk-map

  ((acceptor fun iter)
   ($iteration-thunk-fold
       (lambda (knil item)
	 (acceptor (fun item)))
     (void) iter))

  ((acceptor fun iter1 iter2)
   ($iteration-thunk-fold
       (lambda (knil item1 item2)
	 (acceptor (fun item1 item2)))
     (void) iter1 iter2))

  ((acceptor fun iter1 iter2 . iter*)
   ($iteration-thunk-map-multi acceptor fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iteration-thunk-map-multi acceptor fun iter1 iter2 iter*)
  ($iteration-thunk-fold-multi
      (lambda (knil item1 item2 . item*)
	(acceptor (apply fun item1 item2 item*)))
    (void) iter1 iter2 iter*))


;;;; operations: for-each

(case-define* iteration-thunk-for-each
  (({fun procedure?} {iter procedure?})
   ($iteration-thunk-for-each fun iter))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?})
   ($iteration-thunk-for-each fun iter1 iter2))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iteration-thunk-for-each-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iteration-thunk-for-each

  ((fun iter)
   ($iteration-thunk-fold
       (lambda (knil item)
	 (fun item)
	 knil)
     (void) iter))

  ((fun iter1 iter2)
   ($iteration-thunk-fold
       (lambda (knil item1 item2)
	 (fun item1 item2)
	 knil)
     (void) iter1 iter2))

  ((fun iter1 iter2 . iter*)
   ($iteration-thunk-for-each-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iteration-thunk-for-each-multi fun iter1 iter2 iter*)
  ($iteration-thunk-fold-multi
      (lambda (knil item1 item2 . item*)
	(apply fun item1 item2 item*)
	knil)
    (void) iter1 iter2 iter*))


;;;; operations: for-all

(case-define* iteration-thunk-for-all
  (({fun procedure?} {iter procedure?})
   ($iteration-thunk-for-all fun iter))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?})
   ($iteration-thunk-for-all fun iter1 iter2))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iteration-thunk-for-all-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iteration-thunk-for-all

  ((fun iter)
   (returnable
     ($iteration-thunk-fold
	 (lambda (knil item)
	   (or (fun item)
	       (return #f)))
       #t iter)))

  ((fun iter1 iter2)
   (returnable
     ($iteration-thunk-fold
	 (lambda (knil item1 item2)
	   (or (fun item1 item2)
	       (return #f)))
       #t iter1 iter2)))

  ((fun iter1 iter2 . iter*)
   ($iteration-thunk-for-all-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iteration-thunk-for-all-multi fun iter1 iter2 iter*)
  (returnable
    ($iteration-thunk-fold-multi
	(lambda (knil item1 item2 . item*)
	  (or (apply fun item1 item2 item*)
	      (return #f)))
      #t iter1 iter2 iter*)))


;;;; operations: exists

(case-define* iteration-thunk-exists
  (({fun procedure?} {iter procedure?})
   ($iteration-thunk-exists fun iter))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?})
   ($iteration-thunk-exists fun iter1 iter2))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iteration-thunk-exists-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iteration-thunk-exists

  ((fun iter)
   (call/cc
       (lambda (escape)
	 ($iteration-thunk-fold
	     (lambda (knil item)
	       (cond ((fun item)
		      => escape)
		     (else knil)))
	   #f iter))))

  ((fun iter1 iter2)
   (call/cc
       (lambda (escape)
	 ($iteration-thunk-fold
	     (lambda (knil item1 item2)
	       (cond ((fun item1 item2)
		      => escape)
		     (else knil)))
	   #f iter1 iter2))))

  ((fun iter1 iter2 . iter*)
   ($iteration-thunk-exists-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iteration-thunk-exists-multi fun iter1 iter2 iter*)
  (call/cc
      (lambda (escape)
	($iteration-thunk-fold-multi
	    (lambda (knil item1 item2 . item*)
	      (cond ((apply fun item1 item2 item*)
		     => escape)
		    (else knil)))
	  #f iter1 iter2 iter*))))


;;;; operations: find

(case-define* iteration-thunk-find
  (({fun procedure?} {iter procedure?})
   ($iteration-thunk-find fun iter #f))
  (({fun procedure?} {iter procedure?} not-found-handler)
   ($iteration-thunk-find fun iter not-found-handler)))

(define ($iteration-thunk-find fun iter not-found-handler)
  (returnable
    ($iteration-thunk-fold
	(lambda (knil obj)
	  (if (fun obj)
	      (return obj)
	    knil))
      (void) iter)
    ;;If we are here no object was found.
    (if (procedure? not-found-handler)
	(not-found-handler)
      not-found-handler)))


;;;; operations: filter and partition

(define* (iteration-thunk-filter {acceptor procedure?} {pred procedure?} {iter procedure?})
  ($iteration-thunk-filter acceptor pred iter))

(define ($iteration-thunk-filter acceptor pred iter)
  ($iteration-thunk-fold
      (lambda (knil obj)
	(when (pred obj)
	  (acceptor obj))
	knil)
    (void) iter))

;;; --------------------------------------------------------------------

(define* (iteration-thunk-partition {matching-acceptor procedure?} {not-matching-acceptor procedure?} {pred procedure?} {iter procedure?})
  ($iteration-thunk-partition matching-acceptor not-matching-acceptor pred iter))

(define ($iteration-thunk-partition matching-acceptor not-matching-acceptor pred iter)
  ($iteration-thunk-fold
      (lambda (knil obj)
	(if (pred obj)
	    (matching-acceptor obj)
	  (not-matching-acceptor obj))
	knil)
    (void) iter))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'iteration-thunk-fold		'scheme-indent-function 1)
;; eval: (put '$iteration-thunk-fold		'scheme-indent-function 1)
;; eval: (put '$iteration-thunk-fold-multi	'scheme-indent-function 1)
;; eval: (put 'iteration-thunk-map		'scheme-indent-function 1)
;; eval: (put 'iteration-thunk-for-each		'scheme-indent-function 1)
;; eval: (put 'iteration-thunk-for-all		'scheme-indent-function 1)
;; eval: (put 'iteration-thunk-exists		'scheme-indent-function 1)
;; eval: (put 'iteration-thunk-find		'scheme-indent-function 1)
;; eval: (put 'iteration-thunk-filter		'scheme-indent-function 2)
;; eval: (put 'iteration-thunk-partition	'scheme-indent-function 3)
;; End:
