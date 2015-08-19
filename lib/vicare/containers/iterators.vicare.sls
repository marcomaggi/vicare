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
(library (vicare containers iterators)
  (export
    make-spine-iterator
    make-string-iterator
    make-vector-iterator
    make-bytevector-u8-iterator
    make-bytevector-s8-iterator

    iterator-fold			$iterator-fold
    iterator-for-each			$iterator-for-each
    iterator-for-all			$iterator-for-all
    iterator-exists			$iterator-exists
    iterator-find			$iterator-find
    iterator-filter			$iterator-filter
    iterator-partition			$iterator-partition)
  (import (vicare)
    (vicare system $fx)
    (vicare system $strings)
    (vicare system $vectors)
    (vicare system $bytevectors))


;;;; common iterators

(define (make-spine-iterator ell)
  (lambda ()
    (cond ((pair? ell)
	   (receive-and-return (item)
	       (car ell)
	     (set! ell (cdr ell))))
	  ((null? ell)
	   (void))
	  (else
	   (assertion-violation 'spine-iterator
	     "expected pair as spine object" ell)))))

(define* (make-string-iterator {str string?})
  (let ((str.idx 0)
	(str.len ($string-length str)))
    (lambda ()
      (if ($fx< str.idx str.len)
	  (receive-and-return (ch)
	      ($string-ref str str.idx)
	    (++ str.idx))
	(void)))))

(define* (make-vector-iterator {vec vector?})
  (let ((vec.idx 0)
	(vec.len ($vector-length vec)))
    (lambda ()
      (if ($fx< vec.idx vec.len)
	  (receive-and-return (ch)
	      ($vector-ref vec vec.idx)
	    (++ vec.idx))
	(void)))))

(define* (make-bytevector-u8-iterator {bv bytevector?})
  (let ((bv.idx 0)
	(bv.len ($bytevector-length bv)))
    (lambda ()
      (if ($fx< bv.idx bv.len)
	  (receive-and-return (ch)
	      ($bytevector-u8-ref bv bv.idx)
	    (++ bv.idx))
	(void)))))

(define* (make-bytevector-s8-iterator {bv bytevector?})
  (let ((bv.idx 0)
	(bv.len ($bytevector-length bv)))
    (lambda ()
      (if ($fx< bv.idx bv.len)
	  (receive-and-return (ch)
	      ($bytevector-s8-ref bv bv.idx)
	    (++ bv.idx))
	(void)))))


;;;; folding

(case-define* iterator-fold

  (({kons procedure?} knil {fun procedure?})
   ($iterator-fold kons knil fun))

  (({kons procedure?} knil {fun1 procedure?} {fun2 procedure?})
   ($iterator-fold kons knil fun1 fun2))

  (({kons procedure?} knil {fun1 procedure?} {fun2 procedure?} . {fun* procedure?})
   ($iterator-fold-multi kons knil fun1 fun2 fun*))

  #| end of CASE-DEFINE |# )

(case-define $iterator-fold

  ((kons knil fun)
   (let ((item (fun)))
     (if (void-object? item)
	 knil
       ($iterator-fold kons (kons knil item) fun))))

  ((kons knil fun1 fun2)
   (let ((item1 (fun1))
	 (item2 (fun2)))
     (if (or (void-object? item1)
	     (void-object? item2))
	 knil
       ($iterator-fold kons (kons knil item1 item2) fun1 fun2))))

  ((kons knil fun1 fun2 . fun*)
   ($iterator-fold-multi kons knil fun1 fun2 fun*))

  #| end of CASE-DEFINE |# )

(define ($iterator-fold-multi kons knil fun1 fun2 fun*)
  (let ((item1 (fun1))
	(item2 (fun2))
	(item* (map apply fun*)))
    (if (or (void-object? item1)
	    (void-object? item2)
	    (exists void-object? item*))
	knil
      ($iterator-fold-multi kons (apply kons knil item1 item2 item*) fun1 fun2 fun*))))


;;;; operations: for-each

(case-define* iterator-for-each
  (({fun procedure?} {iter procedure?})
   ($iterator-for-each fun iter))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?})
   ($iterator-for-each fun iter1 iter2))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iterator-for-each-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iterator-for-each

  ((fun iter)
   ($iterator-fold (lambda (knil item)
		     (fun item)
		     knil)
     (void) iter))

  ((fun iter1 iter2)
   ($iterator-fold (lambda (knil item1 item2)
		     (fun item1 item2)
		     knil)
     iter1 iter2))

  ((fun iter1 iter2 . iter*)
   ($iterator-for-each-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iterator-for-each-multi fun iter1 iter2 iter*)
  ($iterator-fold-multi (lambda (knil item1 item2 . item*)
			  (apply fun item1 item2 item*)
			  knil)
    (void) iter1 iter2 iter*))


;;;; operations: for-all

(case-define* iterator-for-all
  (({fun procedure?} {iter procedure?})
   ($iterator-for-all fun iter))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?})
   ($iterator-for-all fun iter1 iter2))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iterator-for-all-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iterator-for-all

  ((fun iter)
   (returnable
     ($iterator-fold (lambda (knil item)
		       (or (fun item)
			   (return #f)))
       #t iter)))

  ((fun iter1 iter2)
   (returnable
     ($iterator-fold (lambda (knil item1 item2)
		       (or (fun item1 item2)
			   (return #f)))
       #t iter1 iter2)))

  ((fun iter1 iter2 . iter*)
   ($iterator-for-all-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iterator-for-all-multi fun iter1 iter2 iter*)
  (returnable
    ($iterator-fold-multi (lambda (knil item1 item2 . item*)
			    (or (apply fun item1 item2 item*)
				(return #f)))
      #t iter1 iter2 iter*)))


;;;; operations: exists

(case-define* iterator-exists
  (({fun procedure?} {iter procedure?})
   ($iterator-exists fun iter))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?})
   ($iterator-exists fun iter1 iter2))

  (({fun procedure?} {iter1 procedure?} {iter2 procedure?} . {iter* procedure?})
   ($iterator-exists-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(case-define $iterator-exists

  ((fun iter)
   (call/cc
       (lambda (escape)
	 ($iterator-fold (lambda (knil item)
			   (cond ((fun item)
				  => escape)
				 (else knil)))
	   #f iter))))

  ((fun iter1 iter2)
   (call/cc
       (lambda (escape)
	 ($iterator-fold (lambda (knil item1 item2)
			   (cond ((fun item1 item2)
				  => escape)
				 (else knil)))
	   #f iter1 iter2))))

  ((fun iter1 iter2 . iter*)
   ($iterator-exists-multi fun iter1 iter2 iter*))

  #| end of CASE-DEFINE |# )

(define ($iterator-exists-multi fun iter1 iter2 iter*)
  (call/cc
      (lambda (escape)
	($iterator-fold-multi (lambda (knil item1 item2 . item*)
				(cond ((apply fun item1 item2 item*)
				       => escape)
				      (else knil)))
	  #f iter1 iter2 iter*))))


;;;; operations: find

(case-define* iterator-find
  (({fun procedure?} {iterator procedure?})
   ($iterator-find fun iterator #f))
  (({fun procedure?} {iterator procedure?} not-found-rv)
   ($iterator-find fun iterator not-found-rv)))

(define ($iterator-find fun iterator not-found-rv)
  (returnable
    ($iterator-fold (lambda (knil obj)
		      (if (fun obj)
			  (return obj)
			knil))
      not-found-rv iterator)))


;;;; operations: filter and partition

(define* (iterator-filter {acceptor procedure?} {pred procedure?} {iter procedure?})
  ($iterator-filter acceptor pred iter))

(define ($iterator-filter acceptor pred iter)
  ($iterator-fold (lambda (knil obj)
		    (when (pred obj)
		      (acceptor obj))
		    knil)
    (void) iter))

;;; --------------------------------------------------------------------

(define* (iterator-partition {matching-acceptor procedure?} {not-matching-acceptor procedure?} {pred procedure?} {iter procedure?})
  ($iterator-partition matching-acceptor not-matching-acceptor pred iter))

(define ($iterator-partition matching-acceptor not-matching-acceptor pred iter)
  ($iterator-fold (lambda (knil obj)
		    (if (pred obj)
			(matching-acceptor obj)
		      (not-matching-acceptor obj))
		    knil)
    (void) iter))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'iterator-fold		'scheme-indent-function 1)
;; eval: (put '$iterator-fold		'scheme-indent-function 1)
;; eval: (put '$iterator-fold-multi	'scheme-indent-function 1)
;; End:
