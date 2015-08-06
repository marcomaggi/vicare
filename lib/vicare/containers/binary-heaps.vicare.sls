;;;
;;;Part of: Vicare Scheme
;;;Contents: binary heap containers
;;;Date: Thu Aug  6, 2015
;;;
;;;Abstract
;;;
;;;	This code is derived from:
;;;
;;;	   <http://en.literateprograms.org/Binary_heap_%28Scheme%29>
;;;
;;;	by S. Carton.  The site claims that  the code is available under the Creative
;;;	Commons CC0 1.0 Universal Public Domain Dedication.
;;;
;;;Copyright (c) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers binary-heaps)
  (export
    make-binary-heap		binary-heap?

    binary-heap-hash		$binary-heap-hash
    binary-heap-putprop		$binary-heap-putprop
    binary-heap-getprop		$binary-heap-getprop
    binary-heap-remprop		$binary-heap-remprop
    binary-heap-property-list	$binary-heap-property-list

    binary-heap-empty?		$binary-heap-empty?
    binary-heap-not-empty?	$binary-heap-not-empty?
    binary-heap-size		$binary-heap-size

    binary-heap-top		$binary-heap-top
    binary-heap-push!		$binary-heap-push!
    binary-heap-pop!		$binary-heap-pop!
    binary-heap-fill!		$binary-heap-fill!
    binary-heap-purge!		$binary-heap-purge!

    binary-heap-copy		$binary-heap-copy
    binary-heap-merge		$binary-heap-merge
    binary-heap-blend!		$binary-heap-blend!

    binary-heap-sort-to-list!	$binary-heap-sort-to-list!)
  (import (vicare)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $vectors))


;;;; helpers

(define-syntax (array-index< stx)
  ;;Compare elements by their index.
  ;;
  (syntax-case stx ()
    ((_ ?arry ?item< ?idx1 ?idx2)
     (identifier? #'?arry)
     #'(?item< ($vector-ref ?arry ?idx1)
	       ($vector-ref ?arry ?idx2)))
    ))

(define-syntax (array-swap! stx)
  (define (idx? obj)
    (or (identifier? obj)
	(fixnum? (syntax->datum obj))))
  (syntax-case stx ()
    ((_ ?arry ?idx1 ?idx2)
     (and (identifier? #'?arry)
	  (idx? #'?idx1)
	  (idx? #'?idx2))
     #'(let ((val1 ($vector-ref ?arry ?idx1)))
	 ($vector-set! ?arry ?idx1 ($vector-ref ?arry ?idx2))
	 ($vector-set! ?arry ?idx2 val1)))
    ))


;;;; data structure

(define-record-type binary-heap
  (nongenerative vicare:containers:binary-heap)
  (fields (mutable	uid)
	  (immutable	item<)
	  (mutable	size)
	  (mutable	array))
  (protocol
   (lambda (make-record)
     (case-lambda*
       (({item< procedure?})
	(make-record #f item< 0 '#()))
       (({item< procedure?} {initial-size non-negative-fixnum?})
	(make-record #f item< 0 (make-vector initial-size)))))))


;;;; UID stuff

(define* (binary-heap-hash {H binary-heap?})
  ($binary-heap-hash H))

(define ($binary-heap-hash H)
  (unless ($binary-heap-uid H)
    ($binary-heap-uid-set! H (gensym)))
  (symbol-hash ($binary-heap-uid H)))

;;; --------------------------------------------------------------------

(define* (binary-heap-putprop {H binary-heap?} {key symbol?} value)
  ($binary-heap-putprop H key value))

(define ($binary-heap-putprop H key value)
  (unless ($binary-heap-uid H)
    ($binary-heap-uid-set! H (gensym)))
  (putprop ($binary-heap-uid H) key value))

;;; --------------------------------------------------------------------

(define* (binary-heap-getprop {H binary-heap?} {key symbol?})
  ($binary-heap-getprop H key))

(define ($binary-heap-getprop H key)
  (unless ($binary-heap-uid H)
    ($binary-heap-uid-set! H (gensym)))
  (getprop ($binary-heap-uid H) key))

;;; --------------------------------------------------------------------

(define* (binary-heap-remprop {H binary-heap?} {key symbol?})
  ($binary-heap-remprop H key))

(define ($binary-heap-remprop H key)
  (unless ($binary-heap-uid H)
    ($binary-heap-uid-set! H (gensym)))
  (remprop ($binary-heap-uid H) key))

;;; --------------------------------------------------------------------

(define* (binary-heap-property-list {H binary-heap?})
  ($binary-heap-property-list H))

(define ($binary-heap-property-list H)
  (unless ($binary-heap-uid H)
    ($binary-heap-uid-set! H (gensym)))
  (property-list ($binary-heap-uid H)))


;;;; inspection

(define* (binary-heap-empty? {H binary-heap?})
  ($binary-heap-empty? H))

(define ($binary-heap-empty? H)
  ($fxzero? ($binary-heap-size H)))

;;; --------------------------------------------------------------------

(define* (binary-heap-not-empty? {H binary-heap?})
  ($binary-heap-not-empty? H))

(define ($binary-heap-not-empty? H)
  ($fxpositive? ($binary-heap-size H)))


;;;; accessors and mutators

(module (binary-heap-push!
	 $binary-heap-push!)

  (define* (binary-heap-push! {H binary-heap?} elem)
    ($binary-heap-push! H elem))

  (define* ($binary-heap-push! H elem)
    (let* ((size      ($binary-heap-size H))
	   (size^     (fxadd1 size)))
      (let* ((arry      ($binary-heap-array H))
	     (arry.len  ($vector-length arry)))
	(when (> size^ arry.len)
	  (let ((new-size (if (fxzero? arry.len)
			      16
			    (fx* 2 arry.len))))
	    ($binary-heap-array-set! H (vector-resize arry new-size)))))
      ($vector-set! ($binary-heap-array H) size elem)
      ($binary-heap-size-set! H size^)
      (%binary-heap-move-up H size)))

  (define (%binary-heap-move-up H position)
    (let ((parent-idx  (quotient (- position 1) 2))
	  (arry        (binary-heap-array H))
	  (item<       (binary-heap-item< H)))
      (cond ((or (= position 0) (array-index< arry item< parent-idx position))
	     'end-move-up)
	    ((array-index< arry item< position parent-idx)
	     (begin
	       (array-swap! arry position parent-idx)
	       (%binary-heap-move-up H parent-idx)))
	    (else 'end-move-up))))

  #| end of module |# )

(module (binary-heap-pop!
	 $binary-heap-pop!)

  (define* (binary-heap-pop! {H binary-heap?})
    ($binary-heap-pop! H))

  (define* ($binary-heap-pop! H)
    (if ($binary-heap-empty? H)
	(assertion-violation __who__ "binary heap is empty" H)
      (receive-and-return (old-root)
	  (binary-heap-top  H)
	(let ((size^ (fxsub1 ($binary-heap-size H))))
	  (let ((arry ($binary-heap-array H)))
	    (array-swap! arry 0 size^))
	  ($binary-heap-size-set! H size^)
	  (%binary-heap-move-down H 0)))))

  (define (%binary-heap-move-down H position)
    (let* ((lchild  (fxadd1 (fx* 2 position)))
	   (rchild  (fx+ 2  (fx* 2 position)))
	   (size    (binary-heap-size  H))
	   (arry    (binary-heap-array H))
	   (item<   (binary-heap-item< H))
	   (swap!   (lambda (pos child)
		      (begin
			(array-swap! arry pos child)
			(%binary-heap-move-down H child)))))
      (cond ((> lchild (fxsub1 size))
	     'end-move-down)
	    ((> rchild (fxsub1 size))
	     (if (array-index< arry item< lchild position)
		 (swap! position lchild)
	       'end-move-down))
	    (else
	     (let ((smallest-child (if (array-index< arry item< lchild rchild)
				       lchild
				     rchild)))
	       (if (array-index< arry item< smallest-child position)
		   (swap! position smallest-child)
		 'end-move-down))))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define* (binary-heap-top {H binary-heap?})
  ($binary-heap-top H))

(define* ($binary-heap-top H)
  (if ($binary-heap-empty? H)
      (assertion-violation __who__ "binary heap is empty" H)
    ($vector-ref ($binary-heap-array H) 0)))

(define* (binary-heap-fill! {H binary-heap?} item*)
  ($binary-heap-fill! H item*))

(define ($binary-heap-fill! H item*)
  (if (pair? item*)
      (begin
	(binary-heap-push! H ($car item*))
	(binary-heap-fill! H ($cdr item*)))
    H))

;;; --------------------------------------------------------------------

(define* (binary-heap-purge! {H binary-heap?})
  ($binary-heap-purge! H))

(define ($binary-heap-purge! H)
  ($binary-heap-size-set!  H 0)
  ($binary-heap-array-set! H (make-vector 16)))


;;;; sorting

(define* (binary-heap-sort-to-list! {H binary-heap?})
  ($binary-heap-sort-to-list! H))

(define ($binary-heap-sort-to-list! H)
  (if ($fxzero? ($binary-heap-size H))
      '()
    (cons ($binary-heap-pop! H)
	  ($binary-heap-sort-to-list! H))))


;;;; other operations

(define* (binary-heap-copy {H binary-heap?})
  ($binary-heap-copy H))

(define* ($binary-heap-copy src)
  (receive-and-return (dst)
      (make-binary-heap ($binary-heap-item< src))
    ($binary-heap-array-set! dst (vector-copy ($binary-heap-array src)))
    ($binary-heap-size-set!  dst ($binary-heap-size src))))

(define* (binary-heap-merge {H1 binary-heap?} {H2 binary-heap?})
  ($binary-heap-merge H1 H2))

(define* ($binary-heap-merge H1 H2)
  ;;Merge two heaps without destroying them.
  ;;
  ($binary-heap-blend! ($binary-heap-copy H1)
		       ($binary-heap-copy H2)))

(define* (binary-heap-blend! {H1 binary-heap?} {H2 binary-heap?})
  ($binary-heap-blend! H1 H2))

(define* ($binary-heap-blend! H1 H2)
  ;;Merge two heaps destroying them.
  ;;
  ($binary-heap-array-set! H1 (vector-resize ($binary-heap-array H1)
					     (fx+ ($binary-heap-size H1)
						  ($binary-heap-size H2))))
  (while ($binary-heap-not-empty? H2)
    ($binary-heap-push! H1 ($binary-heap-pop! H2)))
  H1)


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
