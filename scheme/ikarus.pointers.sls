;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus.pointers)
  (export
    ;; pointer objects
    null-pointer			pointer-null?
    pointer->integer			integer->pointer
    pointer-diff			pointer-add
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

    ;; shared libraries inteface
    dlopen				dlclose
    dlsym				dlerror

    ;; calling functions and callbacks
    make-c-callout			make-c-callback

    ;; raw memory allocation
    malloc				free
    memcpy

    ;; errno interface
    errno

    ;; memory accessors and mutators
    pointer-c-ref-uint8			pointer-c-ref-sint8
    pointer-c-ref-uint16		pointer-c-ref-sint16
    pointer-c-ref-uint32		pointer-c-ref-sint32
    pointer-c-ref-uint64		pointer-c-ref-sint64

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long

    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    pointer-c-set-uint8!		pointer-c-set-sint8!
    pointer-c-set-uint16!		pointer-c-set-sint16!
    pointer-c-set-uint32!		pointer-c-set-sint32!
    pointer-c-set-uint64!		pointer-c-set-sint64!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!

    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-pointer!)
  (import (ikarus)
    (only (ikarus system $pointers)
	  #;pointer? $pointer=)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare words)
	    words.))


;;;; arguments validation

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (flonum who obj)
  (flonum? obj)
  (assertion-violation who "expected flonum as argument" obj))

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (non-negative-exact-integer who obj)
  (or (and (fixnum? obj) (unsafe.fx<= 0 obj))
      (and (bignum? obj) (<= 0 obj)))
  (assertion-violation who "expected non-negative exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (errno who obj)
  (or (boolean? obj) (and (fixnum? obj) (<= obj 0)))
  (assertion-violation who "expected boolean or negative fixnum as errno argument" obj))

(define-argument-validation (machine-word who obj)
  (words.machine-word? obj)
  (assertion-violation who
    "expected non-negative exact integer in the range of a machine word as argument" obj))

(define-argument-validation (ptrdiff who obj)
  (words.ptrdiff? obj)
  (assertion-violation who
    "expected exact integer representing pointer difference as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (uint8 who obj)
  (words.word-u8? obj)
  (assertion-violation who
    "expected exact integer representing an 8-bit signed integer as argument" obj))

(define-argument-validation (sint8 who obj)
  (words.word-s8? obj)
  (assertion-violation who
    "expected exact integer representing an 8-bit unsigned integer as argument" obj))

(define-argument-validation (uint16 who obj)
  (words.word-u16? obj)
  (assertion-violation who
    "expected exact integer representing an 16-bit signed integer as argument" obj))

(define-argument-validation (sint16 who obj)
  (words.word-s16? obj)
  (assertion-violation who
    "expected exact integer representing an 16-bit unsigned integer as argument" obj))

(define-argument-validation (uint32 who obj)
  (words.word-u32? obj)
  (assertion-violation who
    "expected exact integer representing an 32-bit signed integer as argument" obj))

(define-argument-validation (sint32 who obj)
  (words.word-s32? obj)
  (assertion-violation who
    "expected exact integer representing an 32-bit unsigned integer as argument" obj))

(define-argument-validation (uint64 who obj)
  (words.word-u64? obj)
  (assertion-violation who
    "expected exact integer representing an 64-bit signed integer as argument" obj))

(define-argument-validation (sint64 who obj)
  (words.word-s64? obj)
  (assertion-violation who
    "expected exact integer representing an 64-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-char who obj)
  (words.signed-char? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"signed char\" as argument" obj))

(define-argument-validation (unsigned-char who obj)
  (words.unsigned-char? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"unsigned char\" as argument" obj))

(define-argument-validation (signed-short who obj)
  (words.signed-short? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"signed short\" as argument" obj))

(define-argument-validation (unsigned-short who obj)
  (words.unsigned-short? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"unsigned short\" as argument" obj))

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"signed int\" as argument" obj))

(define-argument-validation (unsigned-int who obj)
  (words.unsigned-int? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"unsigned int\" as argument" obj))

(define-argument-validation (signed-long who obj)
  (words.signed-long? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"signed long\" as argument" obj))

(define-argument-validation (unsigned-long who obj)
  (words.unsigned-long? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"unsigned long\" as argument" obj))

(define-argument-validation (signed-long-long who obj)
  (words.signed-long-long? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"signed long long\" as argument" obj))

(define-argument-validation (unsigned-long-long who obj)
  (words.unsigned-long-long? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"unsigned long long\" as argument" obj))


;;; shared libraries interface

(define (dlerror)
  (let ((p (capi.ffi-dlerror)))
    (and p (latin1->string p))))

(define dlopen
  (case-lambda
   (()
    (capi.ffi-dlopen #f #f #f))
   ((libname)
    (dlopen libname #f #f))
   ((libname lazy? global?)
    (define who 'dlopen)
    (with-arguments-validation (who)
	((string  libname))
      (capi.ffi-dlopen (string->latin1 libname) lazy? global?)))))

(define (dlclose ptr)
  (define who 'dlclose)
  (with-arguments-validation (who)
      ((pointer  ptr))
    (capi.ffi-dlclose ptr)))

(define (dlsym handle name)
  (define who 'dlsym)
  (with-arguments-validation (who)
      ((pointer  handle)
       (string   name))
    (capi.ffi-dlsym handle (string->latin1 name))))


;;; pointer manipulation procedures

(define NULL-POINTER
  (capi.ffi-fixnum->pointer 0))

(define (pointer? obj)
  (capi.ffi-pointer? obj))

(define (null-pointer)
  NULL-POINTER)

(define (pointer-null? obj)
  (and (pointer? obj) (capi.ffi-pointer-null? obj)))

;;; --------------------------------------------------------------------

(define (integer->pointer x)
  (define who 'integer->pointer)
  (with-arguments-validation (who)
      ((machine-word  x))
    (if (fixnum? x)
	(capi.ffi-fixnum->pointer x)
      (capi.ffi-bignum->pointer x))))

(define (pointer->integer x)
  (define who 'pointer->integer)
  (with-arguments-validation (who)
      ((pointer	x))
    (capi.ffi-pointer->integer x)))

;;; --------------------------------------------------------------------

(define (pointer-add ptr delta)
  (define who 'pointer-add)
  (with-arguments-validation (who)
      ((pointer  ptr)
       (ptrdiff  delta))
    (let ((rv (capi.ffi-pointer-add ptr delta)))
      (or rv
	  (assertion-violation who
	    "requested pointer arithmetic operation would cause machine word overflow" ptr delta)))))

(define (pointer-diff ptr1 ptr2)
  (define who 'pointer-diff)
  (with-arguments-validation (who)
      ((pointer  ptr1)
       (pointer  ptr2))
    (capi.ffi-pointer-diff ptr1 ptr2)))

(define (pointer+ ptr off)
  (integer->pointer (+ (pointer->integer ptr) off)))

;;; --------------------------------------------------------------------

(let-syntax ((define-pointer-comparison
	       (syntax-rules ()
		 ((_ ?who ?pred)
		  (define (?who ptr1 ptr2)
		    (define who '?who)
		    (with-arguments-validation (who)
			((pointer ptr1)
			 (pointer ptr2))
		      (?pred ptr1 ptr2)))))))

(define-pointer-comparison pointer=?		$pointer=)
;;;  (define-pointer-comparison pointer=?		capi.ffi-pointer-eq)
  (define-pointer-comparison pointer<>?		capi.ffi-pointer-neq)
  (define-pointer-comparison pointer<?		capi.ffi-pointer-lt)
  (define-pointer-comparison pointer>?		capi.ffi-pointer-gt)
  (define-pointer-comparison pointer<=?		capi.ffi-pointer-le)
  (define-pointer-comparison pointer>=?		capi.ffi-pointer-ge))

;;; --------------------------------------------------------------------

(let-syntax ((define-accessor (syntax-rules ()
					((_ ?who ?accessor)
					 (define (?who pointer offset)
					   (define who '?who)
					   (with-arguments-validation (who)
					       ((pointer  pointer)
						(ptrdiff  offset))
					     (?accessor pointer offset)))))))
  (define-accessor pointer-c-ref-uint8		capi.ffi-pointer-c-ref-uint8)
  (define-accessor pointer-c-ref-sint8		capi.ffi-pointer-c-ref-sint8)
  (define-accessor pointer-c-ref-uint16		capi.ffi-pointer-c-ref-uint16)
  (define-accessor pointer-c-ref-sint16		capi.ffi-pointer-c-ref-sint16)
  (define-accessor pointer-c-ref-uint32		capi.ffi-pointer-c-ref-uint32)
  (define-accessor pointer-c-ref-sint32		capi.ffi-pointer-c-ref-sint32)
  (define-accessor pointer-c-ref-uint64		capi.ffi-pointer-c-ref-uint64)
  (define-accessor pointer-c-ref-sint64		capi.ffi-pointer-c-ref-sint64)

  (define-accessor pointer-ref-c-float		capi.ffi-pointer-ref-c-float)
  (define-accessor pointer-ref-c-double		capi.ffi-pointer-ref-c-double)
  (define-accessor pointer-ref-c-pointer	capi.ffi-pointer-ref-c-pointer)

  (define-accessor pointer-ref-c-signed-char	capi.ffi-pointer-ref-c-signed-char)
  (define-accessor pointer-ref-c-signed-short	capi.ffi-pointer-ref-c-signed-short)
  (define-accessor pointer-ref-c-signed-int	capi.ffi-pointer-ref-c-signed-int)
  (define-accessor pointer-ref-c-signed-long	capi.ffi-pointer-ref-c-signed-long)
  (define-accessor pointer-ref-c-signed-long-long capi.ffi-pointer-ref-c-signed-long-long)
  (define-accessor pointer-ref-c-unsigned-char	capi.ffi-pointer-ref-c-unsigned-char)
  (define-accessor pointer-ref-c-unsigned-short	capi.ffi-pointer-ref-c-unsigned-short)
  (define-accessor pointer-ref-c-unsigned-int	capi.ffi-pointer-ref-c-unsigned-int)
  (define-accessor pointer-ref-c-unsigned-long	capi.ffi-pointer-ref-c-unsigned-long)
  (define-accessor pointer-ref-c-unsigned-long-long capi.ffi-pointer-ref-c-unsigned-long-long))

;;; --------------------------------------------------------------------

(let-syntax ((define-mutator (syntax-rules ()
				       ((_ ?who ?mutator ?word-type)
					(define (?who pointer offset value)
					  (define who '?who)
					  (with-arguments-validation (who)
					      ((pointer     pointer)
					       (ptrdiff     offset)
					       (?word-type  value))
					    (?mutator pointer offset value)))))))
  (define-mutator pointer-c-set-uint8!		capi.ffi-pointer-c-set-uint8!	uint8)
  (define-mutator pointer-c-set-sint8!		capi.ffi-pointer-c-set-sint8!	sint8)
  (define-mutator pointer-c-set-uint16!		capi.ffi-pointer-c-set-uint16!	uint16)
  (define-mutator pointer-c-set-sint16!		capi.ffi-pointer-c-set-sint16!	sint16)
  (define-mutator pointer-c-set-uint32!		capi.ffi-pointer-c-set-uint32!	uint32)
  (define-mutator pointer-c-set-sint32!		capi.ffi-pointer-c-set-sint32!	sint32)
  (define-mutator pointer-c-set-uint64!		capi.ffi-pointer-c-set-uint64!	uint64)
  (define-mutator pointer-c-set-sint64!		capi.ffi-pointer-c-set-sint64!	sint64)

  (define-mutator pointer-set-c-float!		capi.ffi-pointer-set-c-float!	flonum)
  (define-mutator pointer-set-c-double!		capi.ffi-pointer-set-c-double!	flonum)
  (define-mutator pointer-set-c-pointer!	capi.ffi-pointer-set-c-pointer!	pointer)

  (define-mutator pointer-set-c-signed-char!	capi.ffi-pointer-set-c-signed-char!	signed-char)
  (define-mutator pointer-set-c-signed-short!	capi.ffi-pointer-set-c-signed-short!	signed-short)
  (define-mutator pointer-set-c-signed-int!	capi.ffi-pointer-set-c-signed-int!	signed-int)
  (define-mutator pointer-set-c-signed-long!	capi.ffi-pointer-set-c-signed-long!	signed-long)
  (define-mutator pointer-set-c-signed-long-long!
    capi.ffi-pointer-set-c-signed-long-long! signed-long-long)

  (define-mutator pointer-set-c-unsigned-char!	capi.ffi-pointer-set-c-unsigned-char!	unsigned-char)
  (define-mutator pointer-set-c-unsigned-short!	capi.ffi-pointer-set-c-unsigned-short!	unsigned-short)
  (define-mutator pointer-set-c-unsigned-int!	capi.ffi-pointer-set-c-unsigned-int!	unsigned-int)
  (define-mutator pointer-set-c-unsigned-long!	capi.ffi-pointer-set-c-unsigned-long!	unsigned-long)
  (define-mutator pointer-set-c-unsigned-long-long!
    capi.ffi-pointer-set-c-unsigned-long-long! unsigned-long-long))


;;; explicit memory management

(define (malloc len)
  (if (and (fixnum? len) (fx>? len 0))
      (foreign-call "ikrt_malloc" len)
    (die 'malloc "not a positive fixnum" len)))

(define (free x)
  (if (pointer? x)
      (foreign-call "ikrt_free" x)
    (die 'free "not a pointer" x)))

(define (memcpy dst dst-offset src src-offset count)
  (define who 'memcpy)
  (unless (and (fixnum? dst-offset) (fx>=? dst-offset 0))
    (die who "not a positive fixnum" dst-offset))
  (unless (and (fixnum? src-offset) (fx>=? src-offset 0))
    (die who "not a positive fixnum" src-offset))
  (unless (and (fixnum? count) (fx>=? count 0))
    (die who "not a postive fixnum" count))
  (cond ((and (pointer? dst) (bytevector? src))
	 (unless (fx<=? (fx+ src-offset count) (bytevector-length src))
	   (die who "source bytevector length exceeded"
		(bytevector-length src) src-offset count))
	 (foreign-call "ikrt_memcpy_from_bv"
		       (pointer+ dst dst-offset) src src-offset count))
	((and (bytevector? dst) (pointer? src))
	 (unless (fx<=? (fx+ dst-offset count) (bytevector-length dst))
	   (die who "destination bytevector length exceeded"
		(bytevector-length dst) dst-offset count))
	 (foreign-call "ikrt_memcpy_to_bv"
		       dst dst-offset (pointer+ src src-offset) count))
	(else
	 (die who "destination and source not a bytevector/pointer pair"
	      dst dst))))


;;; libffi interface

(define (int? x)
  (or (fixnum? x) (bignum? x)))

(define (checker who)
  (define (checker t)
    (cond
     ((vector? t)
      (let ((t* (vector-map checker t)))
	(lambda (v)
	  (and (vector? v)
	       (let ((n (vector-length v)))
		 (and (= n (vector-length t))
		      (let f ((i 0))
			(or (= i n)
			    (and ((vector-ref t* i) (vector-ref v i))
				 (f (+ i 1)))))))))))
     (else
      (case t
	((unsigned-char)      int?)
	((signed-char)        int?)
	((unsigned-short)     int?)
	((signed-short)       int?)
	((unsigned-int)       int?)
	((signed-int)         int?)
	((unsigned-long)      int?)
	((signed-long)        int?)
	((unsigned-long-long) int?)
	((signed-long-long)   int?)
	((float)              flonum?)
	((double)             flonum?)
	((pointer)            pointer?)
	(else (die who "invalid type" t))))))
  checker)

(define (ffi-prep-cif who rtype argtypes)
  (define (convert x)
    (cond
     ((vector? x) (vector-map convert x))
     (else
      (case x
	((void)                1)
	((unsigned-char)       2)
	((signed-char)         3)
	((unsigned-short)      4)
	((signed-short)        5)
	((unsigned-int)        6)
	((signed-int)          7)
	((unsigned-long)       8)
	((signed-long)         9)
	((unsigned-long-long) 10)
	((signed-long-long)   11)
	((float)              12)
	((double)             13)
	((pointer)            14)
	(else (die who "invalid type" x))))))
  (unless (list? argtypes)
    (die who "arg types is not a list" argtypes))
  (let ((argtypes-n (vector-map convert (list->vector argtypes)))
	(rtype-n (convert rtype)))
    (values (or (foreign-call "ikrt_ffi_prep_cif" rtype-n argtypes-n)
		(if (ffi-enabled?)
		    (die who "failed to initialize" rtype argtypes)
		  (die who "FFI support is not enabled.  \
                                You need to recompile ikarus with \
                                --enable-libffi option set in order \
                                to make use of the (ikarus foreign) \
                                library.")))
	    argtypes-n
	    rtype-n)))

(define (make-c-callout rtype argtypes)
  (define who 'make-c-callout)
  (let ((argtypes (if (equal? '(void) argtypes)
		      '()
		    argtypes)))
    (let-values (((cif argtypes-n rtype-n) (ffi-prep-cif who rtype argtypes)))
      (let* ((argtypes-vec (list->vector argtypes))
	     (checkers (vector-map (checker who) argtypes-vec)))
	(lambda (cfun)
	  (define data (vector cif cfun argtypes-n rtype-n))
	  (unless (pointer? cfun)
	    (die who "not a pointer" cfun))
	  (lambda args
	    (let ((argsvec (list->vector args)))
	      (unless (= (vector-length argsvec)
			 (vector-length argtypes-vec))
		(error 'callout-procedure "arg length mismatch"
		       (vector->list argtypes-vec)
		       args))
	      (vector-for-each
		  (lambda (p? t x)
		    (unless (p? x)
		      (die 'callout-procedure
			   (format "argument does not match type ~a" t)
			   x)))
		checkers argtypes-vec argsvec)
	      (foreign-call "ikrt_ffi_call" data argsvec))))))))

(define (make-c-callback rtype argtypes)
  (define who 'make-c-callback)
  (let ((argtypes (if (equal? '(void) argtypes)
		      '()
		    argtypes)))
    (let-values (((cif argtypes-n rtype-n) (ffi-prep-cif who rtype argtypes)))
      (lambda (proc)
	(unless (procedure? proc)
	  (die who "not a procedure"))
	(let ((proc
	       (cond
		((eq? rtype 'void) proc)
		(else
		 (let ((p? ((checker who) rtype)))
		   (lambda args
		     (let ((v (apply proc args)))
		       (unless (p? v)
			 (die 'callback
			      (format "returned value does not match type ~a"
				rtype)
			      v))
		       v)))))))
	  (let ((data (vector cif proc argtypes-n rtype-n)))
	    (or (foreign-call "ikrt_prepare_callback" data)
		(die who "cannot prepare foreign callback"))))))))

(define (ffi-enabled?)
  (foreign-call "ikrt_has_ffi"))


;;;; errno interface

(define errno
  (case-lambda
   (()
    (foreign-call "ikrt_last_errno"))
   ((errno)
    (with-arguments-validation (errno)
	((errno  errno))
      (foreign-call "ikrt_set_errno" errno)))))


;;;; done

)

;;; end of file
