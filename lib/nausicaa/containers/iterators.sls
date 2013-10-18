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
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa containers iterators)
  (export
    <iterator>
    <list-iterator>
    <string-iterator>
    <vector-iterator>
    <bytevector-u8-iterator>

    &stop-iteration

    ;; auxiliary syntaxes
    subject:
    start:		past:
    stride:)
  (import (nausicaa)
    (prefix (vicare language-extensions makers) mk.)
    (vicare arguments validation)
    (vicare unsafe operations))


(define-auxiliary-syntaxes
  subject:
  start:
  past:
  stride:)


(define-constant THE-SENTINEL
  (make-sentinel))

(define-class <iterator>
  (nongenerative nausicaa:containers:iterators:<iterator>)
  (abstract)
  (fields (mutable current))
  (super-protocol
   (lambda (make-top)
     (lambda ()
       ((make-top) THE-SENTINEL))))
  (methods (more?	iterator-more?)
	   (next	iterator-next)))

(define-generic iterator-more? (iterator))
(define-generic iterator-next  (iterator))

(define-condition-type &stop-iteration
  (parent &serious)
  (fields ((iterator <iterator>))))


(module (<list-iterator>)

  (define-class <list-iterator>
    (nongenerative nausicaa:containers:iterators:<list-iterator>)
    (parent <iterator>)

    (fields (mutable   (spine	<spine>))
	    (immutable (stride	<positive-fixnum>)))

    (protocol (lambda (make-iterator)
		(lambda ((subject <spine>) (stride <positive-fixnum>))
		  ((make-iterator) subject stride))))

    (maker    (lambda (stx)
		(syntax-case stx ()
		  ((_ (?clause ...))
		   #'(%make-list-iterator ?clause ...))
		  )))

    #| end of class |# )

  (mk.define-maker %make-list-iterator
      make-<list-iterator>
    ((subject:	(void)	(mk.mandatory))
     (stride:	+1)))

  (define-method (iterator-more? (I <list-iterator>))
    (let loop ((i           1)
	       ((L <spine>) (I $spine)))
      (and (not (L null?))
	   (or (= i (I $stride))
	       (loop (+ 1 i) (L $cdr))))))

  (define-method (iterator-next (I <list-iterator>))
    (let loop ((i           1)
	       ((L <spine>) (I $spine)))
      (cond ((L null?)
	     (set! (I $spine) L)
	     (raise (&stop-iteration (I))))
	    ((eq? THE-SENTINEL (I $current)) ;only the first time
	     (receive-and-return (retval)
		 (L $car)
	       (set! (I $current) retval)
	       (set! (I $spine)   (L $cdr))))
	    ((= i (I $stride))
	     (receive-and-return (retval)
		 (L $car)
	       (set! (I $current) retval)
	       (set! (I $spine)   (L $cdr))))
	    (else
	     (loop (+ 1 i) (L $cdr))))))

  #| end of module |# )


(module (<sequence> <sequence-iterator>)

  (define-auxiliary-syntaxes
    <sequence>)

  (define-label <past-index>
    (predicate (lambda (obj)
		 (or ((<nonnegative-fixnum>) obj)
		     (not obj)))))

  (define-mixin <sequence-iterator>
    (parent <iterator>)
    (fields (mutable   (subject	<sequence>))
	    (mutable   (index	<nonnegative-fixnum>))
	    (immutable (past	<nonnegative-fixnum>))
	    (immutable (stride	<positive-fixnum>)))

    (protocol
     (lambda (make-iterator)
       (lambda ((sequence <sequence>) (start <nonnegative-fixnum>) (past <past-index>) (stride <positive-fixnum>))
	 (define who 'make-<sequence-iterator>)
	 (let ((past (or past (sequence length))))
	   (with-arguments-validation (who)
	       ((start-and-past-for-sequence	sequence start past))
	     ((make-iterator sequence) start past stride))))))

    #| end of mixin |# )

  #| end of module |# )


(module (<string-iterator>)

  (define-class <string-iterator>
    (nongenerative nausicaa:containers:iterators:<string-iterator>)
    (mixins (<sequence-iterator>
	     (<sequence>			<string>)
	     (start-and-past-for-sequence	start-and-past-for-string)))

    (maker (lambda (stx)
	     (syntax-case stx ()
	       ((_ (?clause ...))
		#'(%make-string-iterator ?clause ...))
	       )))

    #| end of class |# )

  (mk.define-maker %make-string-iterator
      (make-<string-iterator>)
    ((subject:	(void)	(mk.mandatory))
     (start:	0)
     (past:	#f)
     (stride:	+1)))

  (define-method (iterator-more? (I <string-iterator>))
    (if ($fxpositive? (I $stride))
	($fx< (I $index) (I $past))
      ($fx> (I $index) (I $past))))

  (define-method (iterator-next (I <string-iterator>))
    (if (I more?)
	(receive-and-return (retval)
	    (I subject [(I $index)])
	  (set!  (I $current) retval)
	  (I $index incr! (I stride)))
      (raise (&stop-iteration (I)))))

  #| end of module |# )


(module (<vector-iterator>)

  (define-class <vector-iterator>
    (nongenerative nausicaa:containers:iterators:<vector-iterator>)
    (mixins (<sequence-iterator>
	     (<sequence>			<vector>)
	     (start-and-past-for-sequence	start-and-past-for-vector)))

    (maker (lambda (stx)
	     (syntax-case stx ()
	       ((_ (?clause ...))
		#'(%make-vector-iterator ?clause ...))
	       )))

    #| end of class |# )

  (mk.define-maker %make-vector-iterator
      (make-<vector-iterator>)
    ((subject:	(void)	(mk.mandatory))
     (start:	0)
     (past:	#f)
     (stride:	+1)))

  (define-method (iterator-more? (I <vector-iterator>))
    (if ($fxpositive? (I $stride))
	($fx< (I $index) (I $past))
      ($fx> (I $index) (I $past))))

  (define-method (iterator-next (I <vector-iterator>))
    (if (I more?)
	(receive-and-return (retval)
	    (I subject [(I $index)])
	  (set!  (I $current) retval)
	  (I $index incr! (I stride)))
      (raise (&stop-iteration (I)))))

  #| end of module |# )


(module (<bytevector-u8-iterator>)

  (define-class <bytevector-u8-iterator>
    (nongenerative nausicaa:containers:iterators:<bytevector-iterator>)
    (mixins (<sequence-iterator>
	     (<sequence>			<bytevector-u8>)
	     (start-and-past-for-sequence	start-and-past-for-bytevector)))

    (maker (lambda (stx)
	     (syntax-case stx ()
	       ((_ (?clause ...))
		#'(%make-bytevector-iterator ?clause ...))
	       )))

    #| end of class |# )

  (mk.define-maker %make-bytevector-iterator
      (make-<bytevector-iterator>)
    ((subject:	(void)	(mk.mandatory))
     (start:	0)
     (past:	#f)
     (stride:	+1)))

  (define-method (iterator-more? (I <bytevector-u8-iterator>))
    (if ($fxpositive? (I $stride))
	($fx< (I $index) (I $past))
      ($fx> (I $index) (I $past))))

  (define-method (iterator-next (I <bytevector-u8-iterator>))
    (if (I more?)
	(receive-and-return (retval)
	    (I $subject [(I $index)])
	  (set! (I $current) retval)
	  (I $index incr! (I stride)))
      (raise (&stop-iteration (I)))))

  #| end of module |# )


;;;; done

)

;;; end of file
