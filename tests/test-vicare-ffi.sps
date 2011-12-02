;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for FFI functions
;;;Date: Sun Nov 27, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (vicare)
		(parameterize parametrise))
  (checks)
  (prefix (vicare words)
	  words.)
  (prefix (vicare ffi)
	  ffi.)
  (prefix (vicare platform-constants)
	  plat.))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare FFI\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))


(parametrise ((check-test-name	'pointers))

  (check
      (ffi.pointer? (ffi.integer->pointer 123))
    => #t)

  (check
      (ffi.pointer? '#(123))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer->integer (ffi.integer->pointer 0))
    => 0)

  (check
      (ffi.pointer->integer (ffi.integer->pointer 123))
    => 123)

  (check
      (ffi.pointer->integer (ffi.integer->pointer (words.greatest-machine-word)))
    => (words.greatest-machine-word))

  (check	;error, integer too big
      (catch #f
	(ffi.integer->pointer (+ 10 (words.greatest-machine-word))))
    => (list (+ 10 (words.greatest-machine-word))))

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer? (ffi.null-pointer))
    => #t)

  (check
      (ffi.pointer->integer (ffi.null-pointer))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer-diff (ffi.integer->pointer 123)
			(ffi.integer->pointer 100))
    => 23)

  (check
      (ffi.pointer-diff (ffi.integer->pointer 10)
			(ffi.integer->pointer 10))
    => 0)

  (check
      (let ((one (ffi.integer->pointer (words.greatest-machine-word)))
	    (two (ffi.integer->pointer 0)))
	(ffi.pointer-diff one two))
    => (words.greatest-machine-word))

  (check
      (let ((one (ffi.integer->pointer (words.greatest-machine-word)))
	    (two (ffi.integer->pointer 0)))
	(ffi.pointer-diff two one))
    => (- (words.greatest-machine-word)))

  (check
      (let* ((one (ffi.integer->pointer 123))
	     (two (ffi.integer->pointer 456))
	     (D   (ffi.pointer-diff one two)))
;;;(check-pretty-print (list one two D))
	(ffi.pointer=? one (ffi.pointer-add two D)))
    => #t)

  (check
      (let* ((one (ffi.integer->pointer 456))
	     (two (ffi.integer->pointer 123))
	     (D   (ffi.pointer-diff one two)))
;;;(check-pretty-print (list one two D))
	(ffi.pointer=? one (ffi.pointer-add two D)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer-add (ffi.integer->pointer 123) 1000)
    => (ffi.integer->pointer 1123))

  (check
      (ffi.pointer-add (ffi.integer->pointer 123) 0)
    => (ffi.integer->pointer 123))

  (check
      (ffi.pointer-add (ffi.integer->pointer 123) -100)
    => (ffi.integer->pointer 23))

  (check
      (let ((P (ffi.integer->pointer (words.greatest-machine-word)))
	    (D 0))
	(ffi.pointer=? P (ffi.pointer-add P D)))
    => #t)

  (check
      (let ((P (ffi.null-pointer))
	    (D 0))
	(ffi.pointer=? P (ffi.pointer-add P D)))
    => #t)

  (check
      (let ((P (ffi.null-pointer))
	    (D (words.greatest-machine-word)))
	(ffi.pointer=? (ffi.integer->pointer (words.greatest-machine-word))
		       (ffi.pointer-add P D)))
    => #t)

  (check
      (let ((P (ffi.integer->pointer (words.greatest-machine-word)))
	    (D 1))
	(equal? (list P D)
		(catch #f
		  (ffi.pointer-add P D))))
    => #t)

  (check
      (let ((P (ffi.null-pointer))
	    (D -1))
	(equal? (list P D)
		(catch #f
		  (ffi.pointer-add P D))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((one (ffi.integer->pointer 123))
	    (two (ffi.integer->pointer 456)))
	(eq? one two))
    => #f)

  (check
      (let ((one (ffi.integer->pointer 123))
	    (two (ffi.integer->pointer 456)))
	(eqv? one two))
    => #f)

  (check
      (let ((one (ffi.integer->pointer 123))
	    (two (ffi.integer->pointer 456)))
	(equal? one two))
    => #f)

  (check
      (let ((one (ffi.integer->pointer 123)))
	(eq? one one))
    => #t)

  (check
      (let ((one (ffi.integer->pointer 123)))
	(eqv? one one))
    => #t)

  (check
      (let ((one (ffi.integer->pointer 123)))
	(equal? one one))
    => #t)

  (check
      (let ((one (ffi.integer->pointer 123))
	    (two (ffi.integer->pointer 123)))
	(eq? one two))
    => #f)

  (check
      (let ((one (ffi.integer->pointer 123))
	    (two (ffi.integer->pointer 123)))
	(eqv? one two))
    => #t)

  (check
      (let ((one (ffi.integer->pointer 123))
	    (two (ffi.integer->pointer 123)))
	(equal? one two))
    => #t)

  #t)


(parametrise ((check-test-name	'allocation))

  (check
      (let ((P (ffi.malloc 10)))
	(ffi.free P)
	(ffi.pointer->integer P))
    => 0)

  (check
      (let ((P (ffi.guarded-malloc 10)))
	(ffi.free P)
	(ffi.pointer->integer P))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((P (ffi.malloc 10))
	     (Q (ffi.realloc P 20)))
	(ffi.free Q)
	(list (ffi.pointer->integer P)
	      (ffi.pointer->integer Q)))
    => '(0 0))

  (check
      (let* ((P (ffi.guarded-malloc 10))
	     (Q (ffi.guarded-realloc P 20)))
	(ffi.free Q)
	(list (ffi.pointer->integer P)
	      (ffi.pointer->integer Q)))
    => '(0 0))

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.calloc 10 20)))
	(ffi.free P)
	(ffi.pointer->integer P))
    => 0)

  (check
      (let ((P (ffi.guarded-calloc 10 20)))
	(ffi.free P)
	(ffi.pointer->integer P))
    => 0)

  #t)


(parametrise ((check-test-name	'memory))

  (check
      (let* ((count	123)
	     (P		(ffi.guarded-malloc count))
	     (Q		(ffi.guarded-malloc count))
	     (bv	(make-bytevector count)))
	(ffi.memset P -9 count)
	(ffi.memcpy Q P count)
	(ffi.memory-copy bv 0 Q 0 count)
	bv)
    => (make-bytevector 123 -9))

  (check
      (let* ((count	123)
	     (P		(ffi.guarded-malloc count))
	     (Q		(ffi.guarded-malloc count))
	     (bv	(make-bytevector count)))
	(ffi.memset P -9 count)
	(ffi.memmove Q P count)
	(ffi.memory-copy bv 0 Q 0 count)
	bv)
    => (make-bytevector 123 -9))

;;; --------------------------------------------------------------------
;;; memory-copy

  (check	;bytevector to bytevector
      (let ((src '#vu8(1 2 3 4))
	    (dst (make-bytevector 2)))
	(ffi.memory-copy dst 0 src 2 2)
	dst)
    => '#vu8(3 4))

  (check	;bytevector to memory
      (let ((src (ffi.guarded-malloc 4))
	    (dst (make-bytevector 2)))
	(ffi.pointer-set-c-uint8! src 0 1)
	(ffi.pointer-set-c-uint8! src 1 2)
	(ffi.pointer-set-c-uint8! src 2 3)
	(ffi.pointer-set-c-uint8! src 3 4)
	(ffi.memory-copy dst 0 src 2 2)
	dst)
    => '#vu8(3 4))

  (check	;memory to memory and memory to bytevector
      (let ((src (ffi.guarded-malloc 4))
	    (dst (ffi.guarded-malloc 2))
	    (bv  (make-bytevector 2)))
	(ffi.pointer-set-c-uint8! src 0 1)
	(ffi.pointer-set-c-uint8! src 1 2)
	(ffi.pointer-set-c-uint8! src 2 3)
	(ffi.pointer-set-c-uint8! src 3 4)
	(ffi.memory-copy dst 0 src 2 2)
	(ffi.memory-copy bv  0 dst 0 2)
	bv)
    => '#vu8(3 4))

  #t)


(parametrise ((check-test-name	'access))

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-uint8! P 2 123)
	(ffi.pointer-ref-c-uint8  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-uint8! P 2 (words.greatest-u8))
	(ffi.pointer-ref-c-uint8  P 2))
    => (words.greatest-u8))

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-uint8! P 2 (words.least-u8))
	(ffi.pointer-ref-c-uint8  P 2))
    => (words.least-u8))

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-sint8! P 2 -123)
	(ffi.pointer-ref-c-sint8  P 2))
    => -123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-sint8! P 2 (words.greatest-s8))
	(ffi.pointer-ref-c-sint8  P 2))
    => (words.greatest-s8))

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-sint8! P 2 (words.least-s8))
	(ffi.pointer-ref-c-sint8  P 2))
    => (words.least-s8))

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-uint16! P 2 123)
	(ffi.pointer-ref-c-uint16  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-sint16! P 2 -123)
	(ffi.pointer-ref-c-sint16  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-uint32! P 2 123)
	(ffi.pointer-ref-c-uint32  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-sint32! P 2 -123)
	(ffi.pointer-ref-c-sint32  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-uint64! P 2 123)
	(ffi.pointer-ref-c-uint64  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-sint64! P 2 -123)
	(ffi.pointer-ref-c-sint64  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-float! P 2 1.23)
	(fl>? 0.0001 (fl- 1.23 (ffi.pointer-ref-c-float  P 2))))
    => #t)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-double! P 2 -1.23)
	(ffi.pointer-ref-c-double  P 2))
    => -1.23)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-pointer! P 2 (ffi.integer->pointer 123))
	(ffi.pointer-ref-c-pointer  P 2))
    => (ffi.integer->pointer 123))

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-signed-char! P 2 123)
	(ffi.pointer-ref-c-signed-char  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-unsigned-char! P 2 123)
	(ffi.pointer-ref-c-unsigned-char  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-signed-short! P 2 123)
	(ffi.pointer-ref-c-signed-short  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-unsigned-short! P 2 123)
	(ffi.pointer-ref-c-unsigned-short  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-signed-int! P 2 123)
	(ffi.pointer-ref-c-signed-int  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-unsigned-int! P 2 123)
	(ffi.pointer-ref-c-unsigned-int  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-signed-long! P 2 123)
	(ffi.pointer-ref-c-signed-long  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-unsigned-long! P 2 123)
	(ffi.pointer-ref-c-unsigned-long  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-signed-long-long! P 2 123)
	(ffi.pointer-ref-c-signed-long-long  P 2))
    => 123)

  (check
      (let ((P (ffi.guarded-malloc 32)))
	(ffi.pointer-set-c-unsigned-long-long! P 2 123)
	(ffi.pointer-ref-c-unsigned-long-long  P 2))
    => 123)

  #t)


(parametrise ((check-test-name	'case-errno))

  (check
      (ffi.case-errno plat.EPERM
	((EPERM)	1)
	((ENOMEM)	2)
	((EAGAIN)	3))
    => 1)

  (check
      (ffi.case-errno plat.EPERM
	((EPERM)	1)
	((ENOMEM)	2)
	((EAGAIN)	3)
	(else		#f))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (ffi.case-errno plat.EPERM
	((ENOMEM EPERM)	1)
	((EAGAIN)	3))
    => 1)

  (check
      (ffi.case-errno plat.EPERM
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (ffi.case-errno plat.EAGAIN
	((ENOMEM EPERM)	1)
	((EAGAIN)	3))
    => 3)

  (check
      (ffi.case-errno plat.EAGAIN
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(ffi.case-errno plat.EFAULT
	  ((ENOMEM EPERM)	1)
	  ((EAGAIN)		3)))
    => (list plat.EFAULT))

  (check
      (ffi.case-errno plat.EFAULT
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

;;;Syntax error "unknown symbolic error code"
;;;
  #;(ffi.case-errno plat.EFAULT
  ((ENOMEM EPERM)	1)
  ((ciao)		2))

  #t)


(parametrise ((check-test-name	'calls))

  (define callout-maker
    (ffi.make-c-callout 'unsigned-int '(unsigned-int)))

  (define callback-maker
    (ffi.make-c-callback 'unsigned-int '(unsigned-int)))

  (define identity
    (callout-maker (callback-maker values)))

  (check (identity 123) => 123)

  #t)


(parametrise ((check-test-name	'libc))

  (define libc
    (ffi.dlopen))

  (when libc

    (let* ((maker	(ffi.make-c-callout 'double '(double)))
	   (sinh	(maker (ffi.dlsym libc "sinh"))))
      (check
	  (flonum? (sinh 1.2))
	=> #t)

      #f))

  #t)


(parametrise ((check-test-name	'zlib))

  (define zlib
    (ffi.dlopen "libz.so"))

  (when zlib
    (check
	(let* ((maker		(ffi.make-c-callout 'signed-int '(pointer pointer pointer unsigned-long)))
	       (compress*	(maker (ffi.dlsym zlib "compress")))
	       (uncompress*	(maker (ffi.dlsym zlib "uncompress"))))
	  (define (compress src.bv)
	    (let-values (((src.ptr src.len) (ffi.bytevector->guarded-memory src.bv)))
	      (let* ((&dst.len	(ffi.guarded-malloc 8))
		     (dst.ptr	(ffi.guarded-malloc src.len)))
		(compress* dst.ptr &dst.len src.ptr src.len)
		(let ((dst.len (ffi.pointer-ref-c-unsigned-long &dst.len 0)))
		  (ffi.memory->bytevector dst.ptr dst.len)))))
	  (define (uncompress src.bv out-len)
	    (let-values (((src.ptr src.len) (ffi.bytevector->guarded-memory src.bv)))
	      (let* ((&dst.len	(ffi.guarded-malloc 8))
		     (dst.ptr	(ffi.guarded-malloc out-len)))
		(uncompress* dst.ptr &dst.len src.ptr src.len)
		(let ((dst.len (ffi.pointer-ref-c-unsigned-long &dst.len 0)))
		  (ffi.memory->bytevector dst.ptr dst.len)))))
	  (let* ((src.len 4096)
		 (src.bv  (make-bytevector src.len 99)))
	    (bytevector=? src.bv (uncompress (compress src.bv) src.len))))
      => #t))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'ffi.case-errno	'scheme-indent-function 1)
;; eval: (put 'catch		'scheme-indent-function 1)
;; End:
