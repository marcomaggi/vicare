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
    <sequence-iterator>
    <string-iterator>
    <vector-iterator>
    <bytevector-u8-iterator>

    &stop-iteration

    ;; multimethods
    iterator-more?
    iterator-next

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
  (fields (immutable subject)
		;Contains  the  object  over   which  the  iteration  is
		;performed.  Notice that it is untagged.
	  (mutable   current))
		;Initialised to THE-SENTINEL,  contains the current item
		;referenced by the iterator.
  (super-protocol
   (lambda (make-top)
     (lambda (subject)
       ((make-top) subject THE-SENTINEL))))
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
		  ((make-iterator subject) subject stride))))

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


(module (<sequence-iterator>)

  (define-class <sequence-iterator>
    (nongenerative nausicaa:containers:iterators:<sequence-iterator>)
    (abstract)
    (parent <iterator>)

    (fields (immutable (getter	<procedure>))
	    (mutable   (index	<nonnegative-fixnum>))
	    (immutable (past	<nonnegative-fixnum>))
	    (immutable (stride	<fixnum>)))

    (super-protocol
     (lambda (make-iterator)
       (lambda (subject (sequence.getter <procedure>) (sequence.length <nonnegative-fixnum>)
	   (start <nonnegative-fixnum>) (past <nonnegative-fixnum>) (stride <fixnum>))
	 (define who 'make-<sequence-iterator>)
	 (with-arguments-validation (who)
	     ((length-start-past-stride		sequence.length start past stride))
	   ((make-iterator subject) sequence.getter start past stride)))))

    #| end of class |# )

  (define-argument-validation (length-start-past-stride who len start past stride)
    (cond (($fxpositive? stride)
	   (and ($fx>= past start)
		($fx<= past len)))
	  (($fxnegative? stride)
	   (and ($fx<  start len)
		($fx>= start past)))
	  (else #f))
    (procedure-argument-violation who
      (string-append "invalid start="	(number->string start)
		     " past="		(number->string past)
		     " stride="		(number->string stride)
		     " indexes for sequence of length " (number->string len))))

  (define-method (iterator-more? (I <sequence-iterator>))
    (if ($fxpositive? (I $stride))
	($fx< (I $index) (I $past))
      ($fx> (I $index) (I $past))))

  (define-method (iterator-next (I <sequence-iterator>))
    (if (I more?)
	(receive-and-return (retval)
	    ((I $getter) (I $index))
	  (set! (I $current) retval)
	  (set! (I $index) (+ (I $index) (I $stride)))
          #;(I index incr! (I $stride)))
      (raise (&stop-iteration (I)))))

  #| end of module |# )


(module (<string-iterator>)

  (define-class <string-iterator>
    (nongenerative nausicaa:containers:iterators:<string-iterator>)
    (parent <sequence-iterator>)

    ;;This tagged  virtual field references the  untagged concrete field
    ;;in "<iterator>";  it only purpose  is to provide tagged  access to
    ;;the subject.
    (virtual-fields (immutable (subject <string>)
			       (lambda ((I <iterator>)) (I $subject))))

    (protocol (lambda (make-subject-iterator)
		(lambda ((subject <string>) start past (stride <fixnum>))
		  (let ((start (or start
				   (if ($fxnegative? stride)
				       (subject $length)
				     0)))
			(past  (or past
				   (if ($fxnegative? stride)
				       0
				     (subject $length)))))
		    ((make-subject-iterator subject (lambda (index) ($string-ref subject index))
					    (subject $length) start past stride))))))

    (maker (lambda (stx)
	     (syntax-case stx ()
	       ((_ (?clause ...))
		#'(%make-string-iterator ?clause ...))
	       )))

    #| end of class |# )

  (mk.define-maker %make-string-iterator
      (make-<string-iterator>)
    ((subject:	(void)	(mk.mandatory))
     (start:	#f)
     (past:	#f)
     (stride:	+1)))

  #| end of module |# )


(module (<vector-iterator>)

  (define-class <vector-iterator>
    (nongenerative nausicaa:containers:iterators:<vector-iterator>)
    (parent <sequence-iterator>)

    ;;This tagged  virtual field references the  untagged concrete field
    ;;in "<iterator>";  it only purpose  is to provide tagged  access to
    ;;the subject.
    (virtual-fields (immutable (subject <vector>)
			       (lambda ((I <iterator>)) (I $subject))))

    (protocol (lambda (make-subject-iterator)
		(lambda ((subject <vector>) start past (stride <fixnum>))
		  (let ((start (or start
				   (if ($fxnegative? stride)
				       (subject $length)
				     0)))
			(past  (or past
				   (if ($fxnegative? stride)
				       0
				     (subject $length)))))
		    ((make-subject-iterator subject (lambda (index) ($vector-ref subject index))
					    (subject $length) start past stride))))))

    (maker (lambda (stx)
	     (syntax-case stx ()
	       ((_ (?clause ...))
		#'(%make-vector-iterator ?clause ...))
	       )))

    #| end of class |# )

  (mk.define-maker %make-vector-iterator
      (make-<vector-iterator>)
    ((subject:	(void)	(mk.mandatory))
     (start:	#f)
     (past:	#f)
     (stride:	+1)))

  #| end of module |# )


(module (<bytevector-u8-iterator>)

  (define-class <bytevector-u8-iterator>
    (nongenerative nausicaa:containers:iterators:<bytevector-u8-iterator>)
    (parent <sequence-iterator>)

    ;;This tagged  virtual field references the  untagged concrete field
    ;;in "<iterator>";  it only purpose  is to provide tagged  access to
    ;;the subject.
    (virtual-fields (immutable (subject <bytevector-u8>)
			       (lambda ((I <iterator>)) (I $subject))))

    (protocol (lambda (make-subject-iterator)
		(lambda ((subject <bytevector-u8>) start past (stride <fixnum>))
		  (let ((start (or start
				   (if ($fxnegative? stride)
				       (subject $length)
				     0)))
			(past  (or past
				   (if ($fxnegative? stride)
				       0
				     (subject $length)))))
		    ((make-subject-iterator subject (lambda (index) ($bytevector-u8-ref subject index))
					    (subject $length) start past stride))))))

    (maker (lambda (stx)
	     (syntax-case stx ()
	       ((_ (?clause ...))
		#'(%make-bytevector-u8-iterator ?clause ...))
	       )))

    #| end of class |# )

  (mk.define-maker %make-bytevector-u8-iterator
      (make-<bytevector-u8-iterator>)
    ((subject:	(void)	(mk.mandatory))
     (start:	#f)
     (past:	#f)
     (stride:	+1)))

  #| end of module |# )


;;;; done

)

;;; end of file
