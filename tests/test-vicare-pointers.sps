;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for pointer-related functions
;;;Date: Tue Feb  7, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare errno)
  (prefix (vicare words)
	  words.)
  (prefix (vicare platform-constants)
	  plat.)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare pointer functions\n")


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
      (pointer? (integer->pointer 123))
    => #t)

  (check
      (pointer? '#(123))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer->integer (integer->pointer 0))
    => 0)

  (check
      (pointer->integer (integer->pointer 123))
    => 123)

  (check
      (pointer->integer (integer->pointer (words.greatest-machine-word)))
    => (words.greatest-machine-word))

  (check	;error, integer too big
      (catch #f
	(integer->pointer (+ 10 (words.greatest-machine-word))))
    => (list (+ 10 (words.greatest-machine-word))))

;;; --------------------------------------------------------------------

  (check
      (pointer? (null-pointer))
    => #t)

  (check
      (pointer->integer (null-pointer))
    => 0)

  (check
      (pointer-null? (null-pointer))
    => #t)

  (check
      (pointer-null? (integer->pointer 123))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer-diff (integer->pointer 123)
			(integer->pointer 100))
    => 23)

  (check
      (pointer-diff (integer->pointer 10)
			(integer->pointer 10))
    => 0)

  (check
      (let ((one (integer->pointer (words.greatest-machine-word)))
	    (two (integer->pointer 0)))
	(pointer-diff one two))
    => (words.greatest-machine-word))

  (check
      (let ((one (integer->pointer (words.greatest-machine-word)))
	    (two (integer->pointer 0)))
	(pointer-diff two one))
    => (- (words.greatest-machine-word)))

  (check
      (let* ((one (integer->pointer 123))
	     (two (integer->pointer 456))
	     (D   (pointer-diff one two)))
;;;(check-pretty-print (list one two D))
	(pointer=? one (pointer-add two D)))
    => #t)

  (check
      (let* ((one (integer->pointer 456))
	     (two (integer->pointer 123))
	     (D   (pointer-diff one two)))
;;;(check-pretty-print (list one two D))
	(pointer=? one (pointer-add two D)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (pointer-add (integer->pointer 123) 1000)
    => (integer->pointer 1123))

  (check
      (pointer-add (integer->pointer 123) 0)
    => (integer->pointer 123))

  (check
      (pointer-add (integer->pointer 123) -100)
    => (integer->pointer 23))

  (check
      (let ((P (integer->pointer (words.greatest-machine-word)))
	    (D 0))
	(pointer=? P (pointer-add P D)))
    => #t)

  (check
      (let ((P (null-pointer))
	    (D 0))
	(pointer=? P (pointer-add P D)))
    => #t)

  (check
      (let ((P (null-pointer))
	    (D (words.greatest-machine-word)))
	(pointer=? (integer->pointer (words.greatest-machine-word))
		       (pointer-add P D)))
    => #t)

  (check
      (let ((P (integer->pointer (words.greatest-machine-word)))
	    (D 1))
	(equal? (list P D)
		(catch #f
		  (pointer-add P D))))
    => #t)

  (check
      (let ((P (null-pointer))
	    (D -1))
	(equal? (list P D)
		(catch #f
		  (pointer-add P D))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((one (integer->pointer 123))
	    (two (integer->pointer 456)))
	(eq? one two))
    => #f)

  (check
      (let ((one (integer->pointer 123))
	    (two (integer->pointer 456)))
	(eqv? one two))
    => #f)

  (check
      (let ((one (integer->pointer 123))
	    (two (integer->pointer 456)))
	(equal? one two))
    => #f)

  (check
      (let ((one (integer->pointer 123)))
	(eq? one one))
    => #t)

  (check
      (let ((one (integer->pointer 123)))
	(eqv? one one))
    => #t)

  (check
      (let ((one (integer->pointer 123)))
	(equal? one one))
    => #t)

  (check
      (let ((one (integer->pointer 123))
	    (two (integer->pointer 123)))
	(eq? one two))
    => #f)

  (check
      (let ((one (integer->pointer 123))
	    (two (integer->pointer 123)))
	(eqv? one two))
    => #t)

  (check
      (let ((one (integer->pointer 123))
	    (two (integer->pointer 123)))
	(equal? one two))
    => #t)

  #t)


(parametrise ((check-test-name	'allocation))

  (check
      (let ((P (malloc 10)))
	(free P)
	(pointer->integer P))
    => 0)

  (check
      (let ((P (guarded-malloc 10)))
	(free P)
	(pointer->integer P))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((P (malloc 10))
	     (Q (realloc P 20)))
	(free Q)
	(list (pointer->integer P)
	      (pointer->integer Q)))
    => '(0 0))

  (check
      (let* ((P (guarded-malloc 10))
	     (Q (guarded-realloc P 20)))
	(free Q)
	(list (pointer->integer P)
	      (pointer->integer Q)))
    => '(0 0))

;;; --------------------------------------------------------------------

  (check
      (let ((P (calloc 10 20)))
	(free P)
	(pointer->integer P))
    => 0)

  (check
      (let ((P (guarded-calloc 10 20)))
	(free P)
	(pointer->integer P))
    => 0)

  #t)


(parametrise ((check-test-name	'memory))

  (check
      (let* ((count	123)
	     (P		(guarded-malloc count))
	     (Q		(guarded-malloc count))
	     (bv	(make-bytevector count)))
	(memset P -9 count)
	(memcpy Q P count)
	(memory-copy bv 0 Q 0 count)
	bv)
    => (make-bytevector 123 -9))

  (check
      (let* ((count	123)
	     (P		(guarded-malloc count))
	     (Q		(guarded-malloc count))
	     (bv	(make-bytevector count)))
	(memset P -9 count)
	(memmove Q P count)
	(memory-copy bv 0 Q 0 count)
	bv)
    => (make-bytevector 123 -9))

;;; --------------------------------------------------------------------
;;; memcmp

  (check
      (let*-values (((count)	4)
		    ((P P.len)	(bytevector->guarded-memory '#vu8(1 2 3 4)))
		    ((Q Q.len)	(bytevector->guarded-memory '#vu8(1 2 3 4))))
	(memcmp P Q count))
    => 0)

  (check
      (let*-values (((count)	4)
		    ((P P.len)	(bytevector->guarded-memory '#vu8(1 2 3 4)))
		    ((Q Q.len)	(bytevector->guarded-memory '#vu8(1 2 8 4))))
	(negative? (memcmp P Q count)))
    => #t)

  (check
      (let*-values (((count)	4)
		    ((P P.len)	(bytevector->guarded-memory '#vu8(1 2 8 4)))
		    ((Q Q.len)	(bytevector->guarded-memory '#vu8(1 2 3 4))))
	(positive? (memcmp P Q count)))
    => #t)

;;; --------------------------------------------------------------------
;;; memory-copy

  (check	;bytevector to bytevector
      (let ((src '#vu8(1 2 3 4))
	    (dst (make-bytevector 2)))
	(memory-copy dst 0 src 2 2)
	dst)
    => '#vu8(3 4))

  (check	;bytevector to memory
      (let ((src (guarded-malloc 4))
	    (dst (make-bytevector 2)))
	(pointer-set-c-uint8! src 0 1)
	(pointer-set-c-uint8! src 1 2)
	(pointer-set-c-uint8! src 2 3)
	(pointer-set-c-uint8! src 3 4)
	(memory-copy dst 0 src 2 2)
	dst)
    => '#vu8(3 4))

  (check	;memory to memory and memory to bytevector
      (let ((src (guarded-malloc 4))
	    (dst (guarded-malloc 2))
	    (bv  (make-bytevector 2)))
	(pointer-set-c-uint8! src 0 1)
	(pointer-set-c-uint8! src 1 2)
	(pointer-set-c-uint8! src 2 3)
	(pointer-set-c-uint8! src 3 4)
	(memory-copy dst 0 src 2 2)
	(memory-copy bv  0 dst 0 2)
	bv)
    => '#vu8(3 4))

  #t)


(parametrise ((check-test-name	'cstrings))

  (check
      (let* ((cstr (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	     (bv   (cstring->bytevector cstr)))
	bv)
    => '#vu8(65 66 67 68))

  (check
      (let* ((cstr (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	     (bv   (cstring->bytevector cstr 2)))
	bv)
    => '#vu8(65 66))

;;; --------------------------------------------------------------------

  (check
      (let* ((cstr (string->guarded-cstring "ABCD"))
	     (str  (cstring->string cstr)))
	str)
    => "ABCD")

  (check
      (let* ((cstr (string->guarded-cstring "ABCD"))
	     (str  (cstring->string cstr 2)))
	str)
    => "AB")

;;; --------------------------------------------------------------------

  (check
      (let ((cstr (bytevector->guarded-cstring '#vu8(65 66 67 68))))
	(strlen cstr))
    => 4)

  (check
      (let* ((cstr (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	     (cstr (strdup cstr)))
	(strlen cstr))
    => 4)

  (check
      (let* ((cstr (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	     (cstr (strndup cstr 4)))
	(strlen cstr))
    => 4)

  (check
      (let* ((cstr (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	     (cstr (strndup cstr 3)))
	(strlen cstr))
    => 3)

;;; --------------------------------------------------------------------
;;; strcmp

  (check
      (let ((cstr1 (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	    (cstr2 (bytevector->guarded-cstring '#vu8(65 66 67 68))))
	(strcmp cstr1 cstr2))
    => 0)

  (check
      (let ((cstr1 (bytevector->guarded-cstring '#vu8(65 66 69 68)))
	    (cstr2 (bytevector->guarded-cstring '#vu8(65 66 67 68))))
	(positive?(strcmp cstr1 cstr2)))
    => #t)

  (check
      (let ((cstr1 (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	    (cstr2 (bytevector->guarded-cstring '#vu8(65 66 69 68))))
	(negative? (strcmp cstr1 cstr2)))
    => #t)

;;; --------------------------------------------------------------------
;;; strncmp

  (check
      (let ((cstr1 (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	    (cstr2 (bytevector->guarded-cstring '#vu8(65 66 67 68))))
	(strncmp cstr1 cstr2 4))
    => 0)

  (check
      (let ((cstr1 (bytevector->guarded-cstring '#vu8(65 66 69 68)))
	    (cstr2 (bytevector->guarded-cstring '#vu8(65 66 67 68))))
	(positive? (strncmp cstr1 cstr2 3)))
    => #t)

  (check
      (let ((cstr1 (bytevector->guarded-cstring '#vu8(65 66 67 68)))
	    (cstr2 (bytevector->guarded-cstring '#vu8(65 66 69 68))))
	(negative? (strncmp cstr1 cstr2 3)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((argv (bytevectors->guarded-argv '(#vu8(65 66 67 68)
						    #vu8(75 77 77 78)
						    #vu8(85 86 87 88))))
	     (strs (argv->bytevectors argv)))
	strs)
    => '(#vu8(65 66 67 68)
	     #vu8(75 77 77 78)
	     #vu8(85 86 87 88)))

  (check
      (let* ((argv (strings->guarded-argv '("ciao" "hello" "salut")))
	     (strs (argv->strings argv)))
	strs)
    => '("ciao" "hello" "salut"))

  (check
      (let ((argv (bytevectors->guarded-argv '(#vu8(65 66 67 68)
						   #vu8(75 77 77 78)
						   #vu8(85 86 87 88)))))
	(argv-length argv))
    => 3)

;;; --------------------------------------------------------------------

  (check 'this	;cstring16->bytevector
      (let* ((cstr (bytevector->guarded-cstring (bytevector-append (string->utf16n "ciao")
								   '#vu8(0 0))))
	     (bv   (cstring16->bytevector cstr)))
	bv)
    => (string->utf16n "ciao"))

  (check	;cstring16n->string
      (let* ((cstr (bytevector->guarded-cstring (bytevector-append (string->utf16n "ciao")
								   '#vu8(0 0))))
	     (S    (cstring16n->string cstr)))
	S)
    => "ciao")

  (check	;cstring16le->string
      (let* ((cstr (bytevector->guarded-cstring (bytevector-append (string->utf16le "ciao")
								   '#vu8(0 0))))
	     (S    (cstring16le->string cstr)))
	S)
    => "ciao")

  (check	;cstring16be->string
      (let* ((cstr (bytevector->guarded-cstring (bytevector-append (string->utf16be "ciao")
								   '#vu8(0 0))))
	     (S    (cstring16be->string cstr)))
	S)
    => "ciao")

  #t)


(parametrise ((check-test-name	'access))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint8! P 2 123)
	(pointer-ref-c-uint8  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint8! P 2 (words.greatest-u8))
	(pointer-ref-c-uint8  P 2))
    => (words.greatest-u8))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint8! P 2 (words.least-u8))
	(pointer-ref-c-uint8  P 2))
    => (words.least-u8))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint8! P 2 -123)
	(pointer-ref-c-sint8  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint8! P 2 (words.greatest-s8))
	(pointer-ref-c-sint8  P 2))
    => (words.greatest-s8))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint8! P 2 (words.least-s8))
	(pointer-ref-c-sint8  P 2))
    => (words.least-s8))

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint16! P 2 123)
	(pointer-ref-c-uint16  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint16! P 2 -123)
	(pointer-ref-c-sint16  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint32! P 2 123)
	(pointer-ref-c-uint32  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint32! P 2 -123)
	(pointer-ref-c-sint32  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint64! P 2 123)
	(pointer-ref-c-uint64  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint64! P 2 -123)
	(pointer-ref-c-sint64  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-float! P 2 1.23)
	(fl>? 0.0001 (fl- 1.23 (pointer-ref-c-float  P 2))))
    => #t)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-double! P 2 -1.23)
	(pointer-ref-c-double  P 2))
    => -1.23)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-pointer! P 2 (integer->pointer 123))
	(pointer-ref-c-pointer  P 2))
    => (integer->pointer 123))

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-char! P 2 123)
	(pointer-ref-c-signed-char  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-char! P 2 123)
	(pointer-ref-c-unsigned-char  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-short! P 2 123)
	(pointer-ref-c-signed-short  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-short! P 2 123)
	(pointer-ref-c-unsigned-short  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-int! P 2 123)
	(pointer-ref-c-signed-int  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-int! P 2 123)
	(pointer-ref-c-unsigned-int  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long! P 2 123)
	(pointer-ref-c-signed-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long! P 2 123)
	(pointer-ref-c-unsigned-long  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long-long! P 2 123)
	(pointer-ref-c-signed-long-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long-long! P 2 123)
	(pointer-ref-c-unsigned-long-long  P 2))
    => 123)

  #t)


(parametrise ((check-test-name	'case-errno))

  (check
      (case-errno plat.EPERM
	((EPERM)	1)
	((ENOMEM)	2)
	((EAGAIN)	3))
    => 1)

  (check
      (case-errno plat.EPERM
	((EPERM)	1)
	((ENOMEM)	2)
	((EAGAIN)	3)
	(else		#f))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (case-errno plat.EPERM
	((ENOMEM EPERM)	1)
	((EAGAIN)	3))
    => 1)

  (check
      (case-errno plat.EPERM
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (case-errno plat.EAGAIN
	((ENOMEM EPERM)	1)
	((EAGAIN)	3))
    => 3)

  (check
      (case-errno plat.EAGAIN
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(case-errno plat.EFAULT
	  ((ENOMEM EPERM)	1)
	  ((EAGAIN)		3)))
    => (list plat.EFAULT))

  (check
      (case-errno plat.EFAULT
	((ENOMEM EPERM)	1)
	((EAGAIN)	3)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

;;;Syntax error "unknown symbolic error code"
;;;
  #;(case-errno plat.EFAULT
  ((ENOMEM EPERM)	1)
  ((ciao)		2))

  #t)


(parametrise ((check-test-name	'local-storage))

  (check
      (let ((a 1) (b 2))
	(with-local-storage '#()
	  (lambda ()
	    (+ a b 4))))
    => 7)

  (check
      (let ((a 1) (b 2))
	(with-local-storage '#(4)
	  (lambda (&int32)
	    (pointer-set-c-sint32! &int32 0 4)
	    (+ a b (pointer-ref-c-sint32 &int32 0)))))
    => 7)

  (check
      (let ((a 1) (b 2))
	(with-local-storage '#(4 8)
	  (lambda (&int32 &int64)
	    (pointer-set-c-sint32! &int32 0 4)
	    (pointer-set-c-sint64! &int64 0 8)
	    (+ a b
	       (pointer-ref-c-sint32 &int32 0)
	       (pointer-ref-c-sint64 &int64 0)))))
    => 15)

  (check	;exception going through
      (catch #f
	(with-local-storage '#(4)
	  (lambda (&int32)
	    (assertion-violation #f "the error" 1 2 3))))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'objects))

  (check
      (pointer? (scheme-object->pointer '(1 . 2)))
    => #t)

  (check
      (let ((O '(1 . 2)))
	(register-to-avoid-collecting O)
	(eq? O (unwind-protect
		   (pointer->scheme-object (scheme-object->pointer O))
		 (forget-to-avoid-collecting O))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'case-errno		'scheme-indent-function 1)
;; eval: (put 'catch			'scheme-indent-function 1)
;; eval: (put 'with-local-storage	'scheme-indent-function 1)
;; End:
