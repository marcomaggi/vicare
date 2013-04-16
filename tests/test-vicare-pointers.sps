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
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (vicare platform words)
	  words.)
  (prefix (vicare platform constants)
	  plat.)
  (prefix (vicare unsafe-operations) $)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare pointer functions\n")
#;(struct-guardian-logger	#t)


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

(define-syntax catch-assertion
  (syntax-rules ()
    ((_ ?print-message . ?body)
     (guard (E
	     ((assertion-violation? E)
	      (when ?print-message
		(check-pretty-print (condition-message E)))
	      (condition-irritants E))
	     (else E))
       (let () . ?body)))))


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
      (let ((P (integer->pointer 123)))
	(pointer=? P (pointer-clone P)))
    => #t)

  (check
      (pointer-null? (pointer-clone (null-pointer)))
    => #t)

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
      (pointer-and-offset? (integer->pointer 123) 0)
    => #t)

  (check
      (pointer-and-offset? (integer->pointer 0) 0)
    => #t)

  (check
      (pointer-and-offset? (integer->pointer 1024) -1024)
    => #t)

  (check
      (pointer-and-offset? (integer->pointer 0) -1)
    => #f)

  (check
      (pointer-and-offset? (integer->pointer 1024) -1025)
    => #f)

  (check
      (pointer-and-offset? (integer->pointer (words.greatest-c-pointer)) 0)
    => #t)

  (check
      (pointer-and-offset? (integer->pointer (words.greatest-c-pointer)) -1)
    => #t)

  (check
      (pointer-and-offset? (integer->pointer (words.greatest-c-pointer)) +1)
    => #f)

  (check
      (pointer-and-offset? (integer->pointer 0) (words.greatest-c-pointer))
    => #t)

  (check
      (pointer-and-offset? (integer->pointer 1) (words.greatest-c-pointer))
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


(parametrise ((check-test-name		'memory-blocks))

  (check
      (let ((B (make-memory-block (integer->pointer 123) 4096)))
	(memory-block? B))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((B (make-memory-block (integer->pointer 123) 4096)))
	(list (pointer->integer (memory-block-pointer B))
	      (memory-block-size    B)))
    => '(123 4096))

;;; --------------------------------------------------------------------

  (check
      (let ((B (make-memory-block (integer->pointer 123) 4096)))
	(memory-block?/non-null B))
    => #t)

  (check
      (let ((B (make-memory-block (integer->pointer 123) 4096)))
	(memory-block?/not-null B))
    => #t)

  (check
      (let ((B (make-memory-block (integer->pointer 0) 0)))
	(memory-block?/non-null B))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (begin
	(make-memory-block/guarded (malloc 16) 16)
	(collect))
    => (void))

;;; --------------------------------------------------------------------

  (check
      (let ((B (make-memory-block (integer->pointer 123) 4096)))
	(memory-block-reset B)
	(list (pointer->integer (memory-block-pointer B))
	      (memory-block-size B)))
    => '(0 0))

;;; --------------------------------------------------------------------

  (check	;free
      (let ((B (make-memory-block (malloc 16) 16)))
	(free B)
	(list (pointer->integer (memory-block-pointer B))
	      (memory-block-size B)))
    => '(0 0))

  (check	;realloc
      (let ((B (make-memory-block (malloc 16) 16)))
;;;(check-pretty-print B)
	(realloc B 32)
;;;(check-pretty-print B)
	(begin0
	    (memory-block-size B)
	  (free B)))
    => 32)

;;; --------------------------------------------------------------------
;;; unsafe accessors

  (check
      (let ((B (make-memory-block (integer->pointer 123) 4096)))
	(list (pointer->integer ($memory-block-pointer B))
	      ($memory-block-size B)))
    => '(123 4096))

  (collect))


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

  (check 	;cstring16->bytevector
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


(parametrise ((check-test-name	'pointer-access))

;;; uint8

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
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint8! P 2 (words.greatest-u8*))))
    => `(,(words.greatest-u8*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint8! P 2 (words.least-u8*))))
    => `(,(words.least-u8*)))

;;; --------------------------------------------------------------------
;;; sint8

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

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint8! P 2 (words.greatest-s8*))))
    => `(,(words.greatest-s8*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint8! P 2 (words.least-s8*))))
    => `(,(words.least-s8*)))

;;; --------------------------------------------------------------------
;;; uint16

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint16! P 2 123)
	(pointer-ref-c-uint16  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint16! P 2 (words.least-u16))
	(pointer-ref-c-uint16  P 2))
    => (words.least-u16))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint16! P 2 (words.greatest-u16))
	(pointer-ref-c-uint16  P 2))
    => (words.greatest-u16))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint16! P 2 (words.greatest-u16*))))
    => `(,(words.greatest-u16*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint16! P 2 (words.least-u16*))))
    => `(,(words.least-u16*)))

;;; --------------------------------------------------------------------
;;; sint16

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint16! P 2 -123)
	(pointer-ref-c-sint16  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint16! P 2 (words.least-s16))
	(pointer-ref-c-sint16  P 2))
    => (words.least-s16))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint16! P 2 (words.greatest-s16))
	(pointer-ref-c-sint16  P 2))
    => (words.greatest-s16))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint16! P 2 (words.greatest-s16*))))
    => `(,(words.greatest-s16*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint16! P 2 (words.least-s16*))))
    => `(,(words.least-s16*)))

;;; --------------------------------------------------------------------
;;; uint32

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint32! P 2 123)
	(pointer-ref-c-uint32  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint32! P 2 (words.greatest-u32))
	(pointer-ref-c-uint32  P 2))
    => (words.greatest-u32))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint32! P 2 (words.least-u32))
	(pointer-ref-c-uint32  P 2))
    => (words.least-u32))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint32! P 2 (words.greatest-u32*))))
    => `(,(words.greatest-u32*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint32! P 2 (words.least-u32*))))
    => `(,(words.least-u32*)))

;;; --------------------------------------------------------------------
;;; sint32

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint32! P 2 -123)
	(pointer-ref-c-sint32  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint32! P 2 (words.greatest-s32))
	(pointer-ref-c-sint32  P 2))
    => (words.greatest-s32))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint32! P 2 (words.least-s32))
	(pointer-ref-c-sint32  P 2))
    => (words.least-s32))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint32! P 2 (words.greatest-s32*))))
    => `(,(words.greatest-s32*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint32! P 2 (words.least-s32*))))
    => `(,(words.least-s32*)))

;;; --------------------------------------------------------------------
;;; uint64

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint64! P 2 123)
	(pointer-ref-c-uint64  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint64! P 2 (words.greatest-u64))
	(pointer-ref-c-uint64  P 2))
    => (words.greatest-u64))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-uint64! P 2 (words.least-u64))
	(pointer-ref-c-uint64  P 2))
    => (words.least-u64))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint64! P 2 (words.greatest-u64*))))
    => `(,(words.greatest-u64*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-uint64! P 2 (words.least-u64*))))
    => `(,(words.least-u64*)))

;;; --------------------------------------------------------------------
;;; sint64

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint64! P 2 -123)
	(pointer-ref-c-sint64  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint64! P 2 (words.greatest-s64))
	(pointer-ref-c-sint64  P 2))
    => (words.greatest-s64))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-sint64! P 2 (words.least-s64))
	(pointer-ref-c-sint64  P 2))
    => (words.least-s64))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint64! P 2 (words.greatest-s64*))))
    => `(,(words.greatest-s64*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-sint64! P 2 (words.least-s64*))))
    => `(,(words.least-s64*)))

;;; --------------------------------------------------------------------
;;; float

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-float! P 2 1.23)
	(fl>? 0.0001 (fl- 1.23 (pointer-ref-c-float  P 2))))
    => #t)

;;; --------------------------------------------------------------------
;;; double

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-double! P 2 -1.23)
	(pointer-ref-c-double  P 2))
    => -1.23)

;;; --------------------------------------------------------------------
;;; pointer

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-pointer! P 2 (integer->pointer 123))
	(pointer-ref-c-pointer  P 2))
    => (integer->pointer 123))

;;; --------------------------------------------------------------------
;;; c-signed-char

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-char! P 2 123)
	(pointer-ref-c-signed-char  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-char! P 2 (words.greatest-c-signed-char))
	(pointer-ref-c-signed-char  P 2))
    => (words.greatest-c-signed-char))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-char! P 2 (words.least-c-signed-char))
	(pointer-ref-c-signed-char  P 2))
    => (words.least-c-signed-char))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-char! P 2 (words.greatest-c-signed-char*))))
    => `(,(words.greatest-c-signed-char*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-char! P 2 (words.least-c-signed-char*))))
    => `(,(words.least-c-signed-char*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-char

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-char! P 2 123)
	(pointer-ref-c-unsigned-char  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-char! P 2 (words.greatest-c-unsigned-char))
	(pointer-ref-c-unsigned-char  P 2))
    => (words.greatest-c-unsigned-char))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-char! P 2 (words.least-c-unsigned-char))
	(pointer-ref-c-unsigned-char  P 2))
    => (words.least-c-unsigned-char))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-char! P 2 (words.greatest-c-unsigned-char*))))
    => `(,(words.greatest-c-unsigned-char*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-char! P 2 (words.least-c-unsigned-char*))))
    => `(,(words.least-c-unsigned-char*)))

;;; --------------------------------------------------------------------
;;; c-signed-short

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-short! P 2 123)
	(pointer-ref-c-signed-short  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-short! P 2 (words.greatest-c-signed-short))
	(pointer-ref-c-signed-short  P 2))
    => (words.greatest-c-signed-short))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-short! P 2 (words.least-c-signed-short))
	(pointer-ref-c-signed-short  P 2))
    => (words.least-c-signed-short))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-short! P 2 (words.greatest-c-signed-short*))))
    => `(,(words.greatest-c-signed-short*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-short! P 2 (words.least-c-signed-short*))))
    => `(,(words.least-c-signed-short*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-short

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-short! P 2 123)
	(pointer-ref-c-unsigned-short  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-short! P 2 (words.greatest-c-unsigned-short))
	(pointer-ref-c-unsigned-short  P 2))
    => (words.greatest-c-unsigned-short))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-short! P 2 (words.least-c-unsigned-short))
	(pointer-ref-c-unsigned-short  P 2))
    => (words.least-c-unsigned-short))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-short! P 2 (words.greatest-c-unsigned-short*))))
    => `(,(words.greatest-c-unsigned-short*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-short! P 2 (words.least-c-unsigned-short*))))
    => `(,(words.least-c-unsigned-short*)))

;;; --------------------------------------------------------------------
;;; c-signed-int

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-int! P 2 123)
	(pointer-ref-c-signed-int  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-int! P 2 (words.greatest-c-signed-int))
	(pointer-ref-c-signed-int  P 2))
    => (words.greatest-c-signed-int))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-int! P 2 (words.least-c-signed-int))
	(pointer-ref-c-signed-int  P 2))
    => (words.least-c-signed-int))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-int! P 2 (words.greatest-c-signed-int*))))
    => `(,(words.greatest-c-signed-int*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-int! P 2 (words.least-c-signed-int*))))
    => `(,(words.least-c-signed-int*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-int

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-int! P 2 123)
	(pointer-ref-c-unsigned-int  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-int! P 2 (words.greatest-c-unsigned-int))
	(pointer-ref-c-unsigned-int  P 2))
    => (words.greatest-c-unsigned-int))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-int! P 2 (words.least-c-unsigned-int))
	(pointer-ref-c-unsigned-int  P 2))
    => (words.least-c-unsigned-int))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-int! P 2 (words.greatest-c-unsigned-int*))))
    => `(,(words.greatest-c-unsigned-int*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-int! P 2 (words.least-c-unsigned-int*))))
    => `(,(words.least-c-unsigned-int*)))

;;; --------------------------------------------------------------------
;;; c-signed-long

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long! P 2 123)
	(pointer-ref-c-signed-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long! P 2 (words.greatest-c-signed-long))
	(pointer-ref-c-signed-long  P 2))
    => (words.greatest-c-signed-long))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long! P 2 (words.least-c-signed-long))
	(pointer-ref-c-signed-long  P 2))
    => (words.least-c-signed-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-long! P 2 (words.greatest-c-signed-long*))))
    => `(,(words.greatest-c-signed-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-long! P 2 (words.least-c-signed-long*))))
    => `(,(words.least-c-signed-long*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-long

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long! P 2 123)
	(pointer-ref-c-unsigned-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long! P 2 (words.greatest-c-unsigned-long))
	(pointer-ref-c-unsigned-long  P 2))
    => (words.greatest-c-unsigned-long))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long! P 2 (words.least-c-unsigned-long))
	(pointer-ref-c-unsigned-long  P 2))
    => (words.least-c-unsigned-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-long! P 2 (words.greatest-c-unsigned-long*))))
    => `(,(words.greatest-c-unsigned-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-long! P 2 (words.least-c-unsigned-long*))))
    => `(,(words.least-c-unsigned-long*)))

;;; --------------------------------------------------------------------
;;; c-signed-long-long

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long-long! P 2 123)
	(pointer-ref-c-signed-long-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long-long! P 2 (words.greatest-c-signed-long-long))
	(pointer-ref-c-signed-long-long  P 2))
    => (words.greatest-c-signed-long-long))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-signed-long-long! P 2 (words.least-c-signed-long-long))
	(pointer-ref-c-signed-long-long  P 2))
    => (words.least-c-signed-long-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-long-long! P 2 (words.greatest-c-signed-long-long*))))
    => `(,(words.greatest-c-signed-long-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-signed-long-long! P 2 (words.least-c-signed-long-long*))))
    => `(,(words.least-c-signed-long-long*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-long-long

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long-long! P 2 123)
	(pointer-ref-c-unsigned-long-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long-long! P 2 (words.greatest-c-unsigned-long-long))
	(pointer-ref-c-unsigned-long-long  P 2))
    => (words.greatest-c-unsigned-long-long))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-unsigned-long-long! P 2 (words.least-c-unsigned-long-long))
	(pointer-ref-c-unsigned-long-long  P 2))
    => (words.least-c-unsigned-long-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-long-long! P 2 (words.greatest-c-unsigned-long-long*))))
    => `(,(words.greatest-c-unsigned-long-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-unsigned-long-long! P 2 (words.least-c-unsigned-long-long*))))
    => `(,(words.least-c-unsigned-long-long*)))

;;; --------------------------------------------------------------------
;;; c-size_t

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-size_t! P 2 123)
	(pointer-ref-c-size_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-size_t! P 2 (words.greatest-c-size_t))
	(pointer-ref-c-size_t  P 2))
    => (words.greatest-c-size_t))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-size_t! P 2 (words.least-c-size_t))
	(pointer-ref-c-size_t  P 2))
    => (words.least-c-size_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-size_t! P 2 (words.greatest-c-size_t*))))
    => `(,(words.greatest-c-size_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-size_t! P 2 (words.least-c-size_t*))))
    => `(,(words.least-c-size_t*)))

;;; --------------------------------------------------------------------
;;; c-ssize_t

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-ssize_t! P 2 123)
	(pointer-ref-c-ssize_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-ssize_t! P 2 (words.greatest-c-ssize_t))
	(pointer-ref-c-ssize_t  P 2))
    => (words.greatest-c-ssize_t))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-ssize_t! P 2 (words.least-c-ssize_t))
	(pointer-ref-c-ssize_t  P 2))
    => (words.least-c-ssize_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-ssize_t! P 2 (words.greatest-c-ssize_t*))))
    => `(,(words.greatest-c-ssize_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-ssize_t! P 2 (words.least-c-ssize_t*))))
    => `(,(words.least-c-ssize_t*)))

;;; --------------------------------------------------------------------
;;; c-off_t

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-off_t! P 2 123)
	(pointer-ref-c-off_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-off_t! P 2 (words.greatest-c-off_t))
	(pointer-ref-c-off_t  P 2))
    => (words.greatest-c-off_t))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-off_t! P 2 (words.least-c-off_t))
	(pointer-ref-c-off_t  P 2))
    => (words.least-c-off_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-off_t! P 2 (words.greatest-c-off_t*))))
    => `(,(words.greatest-c-off_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-off_t! P 2 (words.least-c-off_t*))))
    => `(,(words.least-c-off_t*)))

;;; --------------------------------------------------------------------
;;; c-ptrdiff_t

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-ptrdiff_t! P 2 123)
	(pointer-ref-c-ptrdiff_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-ptrdiff_t! P 2 (words.greatest-c-ptrdiff_t))
	(pointer-ref-c-ptrdiff_t  P 2))
    => (words.greatest-c-ptrdiff_t))

  (check
      (let ((P (guarded-malloc 32)))
	(pointer-set-c-ptrdiff_t! P 2 (words.least-c-ptrdiff_t))
	(pointer-ref-c-ptrdiff_t  P 2))
    => (words.least-c-ptrdiff_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-ptrdiff_t! P 2 (words.greatest-c-ptrdiff_t*))))
    => `(,(words.greatest-c-ptrdiff_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (pointer-set-c-ptrdiff_t! P 2 (words.least-c-ptrdiff_t*))))
    => `(,(words.least-c-ptrdiff_t*)))

  (collect))


(parametrise ((check-test-name	'array-access))

;;; uint8

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint8! P 2 123)
	(array-ref-c-uint8  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint8! P 2 (words.greatest-u8))
	(array-ref-c-uint8  P 2))
    => (words.greatest-u8))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint8! P 2 (words.least-u8))
	(array-ref-c-uint8  P 2))
    => (words.least-u8))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint8! P 2 (words.greatest-u8*))))
    => `(,(words.greatest-u8*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint8! P 2 (words.least-u8*))))
    => `(,(words.least-u8*)))

;;; --------------------------------------------------------------------
;;; sint8

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint8! P 2 -123)
	(array-ref-c-sint8  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint8! P 2 (words.greatest-s8))
	(array-ref-c-sint8  P 2))
    => (words.greatest-s8))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint8! P 2 (words.least-s8))
	(array-ref-c-sint8  P 2))
    => (words.least-s8))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint8! P 2 (words.greatest-s8*))))
    => `(,(words.greatest-s8*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint8! P 2 (words.least-s8*))))
    => `(,(words.least-s8*)))

;;; --------------------------------------------------------------------
;;; uint16

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint16! P 2 123)
	(array-ref-c-uint16  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint16! P 2 (words.least-u16))
	(array-ref-c-uint16  P 2))
    => (words.least-u16))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint16! P 2 (words.greatest-u16))
	(array-ref-c-uint16  P 2))
    => (words.greatest-u16))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint16! P 2 (words.greatest-u16*))))
    => `(,(words.greatest-u16*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint16! P 2 (words.least-u16*))))
    => `(,(words.least-u16*)))

;;; --------------------------------------------------------------------
;;; sint16

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint16! P 2 -123)
	(array-ref-c-sint16  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint16! P 2 (words.least-s16))
	(array-ref-c-sint16  P 2))
    => (words.least-s16))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint16! P 2 (words.greatest-s16))
	(array-ref-c-sint16  P 2))
    => (words.greatest-s16))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint16! P 2 (words.greatest-s16*))))
    => `(,(words.greatest-s16*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint16! P 2 (words.least-s16*))))
    => `(,(words.least-s16*)))

;;; --------------------------------------------------------------------
;;; uint32

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint32! P 2 123)
	(array-ref-c-uint32  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint32! P 2 (words.greatest-u32))
	(array-ref-c-uint32  P 2))
    => (words.greatest-u32))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint32! P 2 (words.least-u32))
	(array-ref-c-uint32  P 2))
    => (words.least-u32))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint32! P 2 (words.greatest-u32*))))
    => `(,(words.greatest-u32*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint32! P 2 (words.least-u32*))))
    => `(,(words.least-u32*)))

;;; --------------------------------------------------------------------
;;; sint32

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint32! P 2 -123)
	(array-ref-c-sint32  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint32! P 2 (words.greatest-s32))
	(array-ref-c-sint32  P 2))
    => (words.greatest-s32))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint32! P 2 (words.least-s32))
	(array-ref-c-sint32  P 2))
    => (words.least-s32))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint32! P 2 (words.greatest-s32*))))
    => `(,(words.greatest-s32*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint32! P 2 (words.least-s32*))))
    => `(,(words.least-s32*)))

;;; --------------------------------------------------------------------
;;; uint64

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint64! P 2 123)
	(array-ref-c-uint64  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint64! P 2 (words.greatest-u64))
	(array-ref-c-uint64  P 2))
    => (words.greatest-u64))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-uint64! P 2 (words.least-u64))
	(array-ref-c-uint64  P 2))
    => (words.least-u64))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint64! P 2 (words.greatest-u64*))))
    => `(,(words.greatest-u64*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-uint64! P 2 (words.least-u64*))))
    => `(,(words.least-u64*)))

;;; --------------------------------------------------------------------
;;; sint64

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint64! P 2 -123)
	(array-ref-c-sint64  P 2))
    => -123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint64! P 2 (words.greatest-s64))
	(array-ref-c-sint64  P 2))
    => (words.greatest-s64))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-sint64! P 2 (words.least-s64))
	(array-ref-c-sint64  P 2))
    => (words.least-s64))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint64! P 2 (words.greatest-s64*))))
    => `(,(words.greatest-s64*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-sint64! P 2 (words.least-s64*))))
    => `(,(words.least-s64*)))

;;; --------------------------------------------------------------------
;;; float

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-float! P 2 1.23)
	(fl>? 0.0001 (fl- 1.23 (array-ref-c-float  P 2))))
    => #t)

;;; --------------------------------------------------------------------
;;; double

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-double! P 2 -1.23)
	(array-ref-c-double  P 2))
    => -1.23)

;;; --------------------------------------------------------------------
;;; pointer

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-pointer! P 2 (integer->pointer 123))
	(array-ref-c-pointer  P 2))
    => (integer->pointer 123))

;;; --------------------------------------------------------------------
;;; c-signed-char

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-char! P 2 123)
	(array-ref-c-signed-char  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-char! P 2 (words.greatest-c-signed-char))
	(array-ref-c-signed-char  P 2))
    => (words.greatest-c-signed-char))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-char! P 2 (words.least-c-signed-char))
	(array-ref-c-signed-char  P 2))
    => (words.least-c-signed-char))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-char! P 2 (words.greatest-c-signed-char*))))
    => `(,(words.greatest-c-signed-char*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-char! P 2 (words.least-c-signed-char*))))
    => `(,(words.least-c-signed-char*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-char

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-char! P 2 123)
	(array-ref-c-unsigned-char  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-char! P 2 (words.greatest-c-unsigned-char))
	(array-ref-c-unsigned-char  P 2))
    => (words.greatest-c-unsigned-char))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-char! P 2 (words.least-c-unsigned-char))
	(array-ref-c-unsigned-char  P 2))
    => (words.least-c-unsigned-char))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-char! P 2 (words.greatest-c-unsigned-char*))))
    => `(,(words.greatest-c-unsigned-char*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-char! P 2 (words.least-c-unsigned-char*))))
    => `(,(words.least-c-unsigned-char*)))

;;; --------------------------------------------------------------------
;;; c-signed-short

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-short! P 2 123)
	(array-ref-c-signed-short  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-short! P 2 (words.greatest-c-signed-short))
	(array-ref-c-signed-short  P 2))
    => (words.greatest-c-signed-short))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-short! P 2 (words.least-c-signed-short))
	(array-ref-c-signed-short  P 2))
    => (words.least-c-signed-short))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-short! P 2 (words.greatest-c-signed-short*))))
    => `(,(words.greatest-c-signed-short*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-short! P 2 (words.least-c-signed-short*))))
    => `(,(words.least-c-signed-short*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-short

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-short! P 2 123)
	(array-ref-c-unsigned-short  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-short! P 2 (words.greatest-c-unsigned-short))
	(array-ref-c-unsigned-short  P 2))
    => (words.greatest-c-unsigned-short))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-short! P 2 (words.least-c-unsigned-short))
	(array-ref-c-unsigned-short  P 2))
    => (words.least-c-unsigned-short))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-short! P 2 (words.greatest-c-unsigned-short*))))
    => `(,(words.greatest-c-unsigned-short*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-short! P 2 (words.least-c-unsigned-short*))))
    => `(,(words.least-c-unsigned-short*)))

;;; --------------------------------------------------------------------
;;; c-signed-int

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-int! P 2 123)
	(array-ref-c-signed-int  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-int! P 2 (words.greatest-c-signed-int))
	(array-ref-c-signed-int  P 2))
    => (words.greatest-c-signed-int))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-int! P 2 (words.least-c-signed-int))
	(array-ref-c-signed-int  P 2))
    => (words.least-c-signed-int))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-int! P 2 (words.greatest-c-signed-int*))))
    => `(,(words.greatest-c-signed-int*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-int! P 2 (words.least-c-signed-int*))))
    => `(,(words.least-c-signed-int*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-int

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-int! P 2 123)
	(array-ref-c-unsigned-int  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-int! P 2 (words.greatest-c-unsigned-int))
	(array-ref-c-unsigned-int  P 2))
    => (words.greatest-c-unsigned-int))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-int! P 2 (words.least-c-unsigned-int))
	(array-ref-c-unsigned-int  P 2))
    => (words.least-c-unsigned-int))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-int! P 2 (words.greatest-c-unsigned-int*))))
    => `(,(words.greatest-c-unsigned-int*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-int! P 2 (words.least-c-unsigned-int*))))
    => `(,(words.least-c-unsigned-int*)))

;;; --------------------------------------------------------------------
;;; c-signed-long

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-long! P 2 123)
	(array-ref-c-signed-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-long! P 2 (words.greatest-c-signed-long))
	(array-ref-c-signed-long  P 2))
    => (words.greatest-c-signed-long))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-long! P 2 (words.least-c-signed-long))
	(array-ref-c-signed-long  P 2))
    => (words.least-c-signed-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-long! P 2 (words.greatest-c-signed-long*))))
    => `(,(words.greatest-c-signed-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-long! P 2 (words.least-c-signed-long*))))
    => `(,(words.least-c-signed-long*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-long

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-long! P 2 123)
	(array-ref-c-unsigned-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-long! P 2 (words.greatest-c-unsigned-long))
	(array-ref-c-unsigned-long  P 2))
    => (words.greatest-c-unsigned-long))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-long! P 2 (words.least-c-unsigned-long))
	(array-ref-c-unsigned-long  P 2))
    => (words.least-c-unsigned-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-long! P 2 (words.greatest-c-unsigned-long*))))
    => `(,(words.greatest-c-unsigned-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-long! P 2 (words.least-c-unsigned-long*))))
    => `(,(words.least-c-unsigned-long*)))

;;; --------------------------------------------------------------------
;;; c-signed-long-long

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-long-long! P 2 123)
	(array-ref-c-signed-long-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-long-long! P 2 (words.greatest-c-signed-long-long))
	(array-ref-c-signed-long-long  P 2))
    => (words.greatest-c-signed-long-long))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-signed-long-long! P 2 (words.least-c-signed-long-long))
	(array-ref-c-signed-long-long  P 2))
    => (words.least-c-signed-long-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-long-long! P 2 (words.greatest-c-signed-long-long*))))
    => `(,(words.greatest-c-signed-long-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-signed-long-long! P 2 (words.least-c-signed-long-long*))))
    => `(,(words.least-c-signed-long-long*)))

;;; --------------------------------------------------------------------
;;; c-unsigned-long-long

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-long-long! P 2 123)
	(array-ref-c-unsigned-long-long  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-long-long! P 2 (words.greatest-c-unsigned-long-long))
	(array-ref-c-unsigned-long-long  P 2))
    => (words.greatest-c-unsigned-long-long))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-unsigned-long-long! P 2 (words.least-c-unsigned-long-long))
	(array-ref-c-unsigned-long-long  P 2))
    => (words.least-c-unsigned-long-long))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-long-long! P 2 (words.greatest-c-unsigned-long-long*))))
    => `(,(words.greatest-c-unsigned-long-long*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-unsigned-long-long! P 2 (words.least-c-unsigned-long-long*))))
    => `(,(words.least-c-unsigned-long-long*)))

;;; --------------------------------------------------------------------
;;; c-size_t

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-size_t! P 2 123)
	(array-ref-c-size_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-size_t! P 2 (words.greatest-c-size_t))
	(array-ref-c-size_t  P 2))
    => (words.greatest-c-size_t))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-size_t! P 2 (words.least-c-size_t))
	(array-ref-c-size_t  P 2))
    => (words.least-c-size_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-size_t! P 2 (words.greatest-c-size_t*))))
    => `(,(words.greatest-c-size_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-size_t! P 2 (words.least-c-size_t*))))
    => `(,(words.least-c-size_t*)))

;;; --------------------------------------------------------------------
;;; c-ssize_t

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-ssize_t! P 2 123)
	(array-ref-c-ssize_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-ssize_t! P 2 (words.greatest-c-ssize_t))
	(array-ref-c-ssize_t  P 2))
    => (words.greatest-c-ssize_t))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-ssize_t! P 2 (words.least-c-ssize_t))
	(array-ref-c-ssize_t  P 2))
    => (words.least-c-ssize_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-ssize_t! P 2 (words.greatest-c-ssize_t*))))
    => `(,(words.greatest-c-ssize_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-ssize_t! P 2 (words.least-c-ssize_t*))))
    => `(,(words.least-c-ssize_t*)))

;;; --------------------------------------------------------------------
;;; c-off_t

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-off_t! P 2 123)
	(array-ref-c-off_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-off_t! P 2 (words.greatest-c-off_t))
	(array-ref-c-off_t  P 2))
    => (words.greatest-c-off_t))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-off_t! P 2 (words.least-c-off_t))
	(array-ref-c-off_t  P 2))
    => (words.least-c-off_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-off_t! P 2 (words.greatest-c-off_t*))))
    => `(,(words.greatest-c-off_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-off_t! P 2 (words.least-c-off_t*))))
    => `(,(words.least-c-off_t*)))

;;; --------------------------------------------------------------------
;;; c-ptrdiff_t

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-ptrdiff_t! P 2 123)
	(array-ref-c-ptrdiff_t  P 2))
    => 123)

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-ptrdiff_t! P 2 (words.greatest-c-ptrdiff_t))
	(array-ref-c-ptrdiff_t  P 2))
    => (words.greatest-c-ptrdiff_t))

  (check
      (let ((P (guarded-malloc 32)))
	(array-set-c-ptrdiff_t! P 2 (words.least-c-ptrdiff_t))
	(array-ref-c-ptrdiff_t  P 2))
    => (words.least-c-ptrdiff_t))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-ptrdiff_t! P 2 (words.greatest-c-ptrdiff_t*))))
    => `(,(words.greatest-c-ptrdiff_t*)))

  (check
      (catch-assertion #f
	(let ((P (guarded-malloc 32)))
	  (array-set-c-ptrdiff_t! P 2 (words.least-c-ptrdiff_t*))))
    => `(,(words.least-c-ptrdiff_t*)))

  (collect))


(parametrise ((check-test-name	'mblock-access))

  (define (block-malloc number-of-bytes)
    (make-memory-block/guarded (malloc* number-of-bytes) number-of-bytes))

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-uint8! P 2 123)
	(pointer-ref-c-uint8  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-uint8! P 2 (words.greatest-u8))
	(pointer-ref-c-uint8  P 2))
    => (words.greatest-u8))

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-uint8! P 2 (words.least-u8))
	(pointer-ref-c-uint8  P 2))
    => (words.least-u8))

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-sint8! P 2 -123)
	(pointer-ref-c-sint8  P 2))
    => -123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-sint8! P 2 (words.greatest-s8))
	(pointer-ref-c-sint8  P 2))
    => (words.greatest-s8))

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-sint8! P 2 (words.least-s8))
	(pointer-ref-c-sint8  P 2))
    => (words.least-s8))

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-uint16! P 2 123)
	(pointer-ref-c-uint16  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-sint16! P 2 -123)
	(pointer-ref-c-sint16  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-uint32! P 2 123)
	(pointer-ref-c-uint32  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-sint32! P 2 -123)
	(pointer-ref-c-sint32  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-uint64! P 2 123)
	(pointer-ref-c-uint64  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-sint64! P 2 -123)
	(pointer-ref-c-sint64  P 2))
    => -123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-float! P 2 1.23)
	(fl>? 0.0001 (fl- 1.23 (pointer-ref-c-float  P 2))))
    => #t)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-double! P 2 -1.23)
	(pointer-ref-c-double  P 2))
    => -1.23)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-pointer! P 2 (integer->pointer 123))
	(pointer-ref-c-pointer  P 2))
    => (integer->pointer 123))

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-signed-char! P 2 123)
	(pointer-ref-c-signed-char  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-unsigned-char! P 2 123)
	(pointer-ref-c-unsigned-char  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-signed-short! P 2 123)
	(pointer-ref-c-signed-short  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-unsigned-short! P 2 123)
	(pointer-ref-c-unsigned-short  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-signed-int! P 2 123)
	(pointer-ref-c-signed-int  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-unsigned-int! P 2 123)
	(pointer-ref-c-unsigned-int  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-signed-long! P 2 123)
	(pointer-ref-c-signed-long  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-unsigned-long! P 2 123)
	(pointer-ref-c-unsigned-long  P 2))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-signed-long-long! P 2 123)
	(pointer-ref-c-signed-long-long  P 2))
    => 123)

  (check
      (let ((P (block-malloc 32)))
	(pointer-set-c-unsigned-long-long! P 2 123)
	(pointer-ref-c-unsigned-long-long  P 2))
    => 123)

  (collect))


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


;;;; done

(collect)
(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'case-errno		'scheme-indent-function 1)
;; eval: (put 'catch			'scheme-indent-function 1)
;; eval: (put 'with-local-storage	'scheme-indent-function 1)
;; eval: (put 'catch-assertion		'scheme-indent-function 1)
;; End:
