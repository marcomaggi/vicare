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
    pointer?
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
    pointer-ref-c-signed-char
    pointer-ref-c-signed-short
    pointer-ref-c-signed-int
    pointer-ref-c-signed-long
    pointer-ref-c-signed-long-long
    pointer-ref-c-unsigned-char
    pointer-ref-c-unsigned-short
    pointer-ref-c-unsigned-int
    pointer-ref-c-unsigned-long
    pointer-ref-c-unsigned-long-long
    pointer-ref-c-float
    pointer-ref-c-double
    pointer-ref-c-pointer
    pointer-set-c-char!
    pointer-set-c-short!
    pointer-set-c-int!
    pointer-set-c-long!
    pointer-set-c-long-long!
    pointer-set-c-pointer!
    pointer-set-c-float!
    pointer-set-c-double!)
  (import (ikarus) #;(except (ikarus)
		  pointer?
		  integer->pointer pointer->integer
		  dlopen dlerror dlclose dlsym malloc free memcpy)
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

;;; --------------------------------------------------------------------

(define-argument-validation (non-negative-exact-integer who obj)
  (or (and (fixnum? obj) (unsafe.fx<= 0 obj))
      (and (bignum? obj) (<= 0 obj)))
  (assertion-violation who "expected non-negative exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (errno who obj)
  (or (boolean? obj) (and (fixnum? obj) (<= obj 0)))
  (assertion-violation who "expected boolean or negative fixnum as errno argument" obj))

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (machine-word who obj)
  (words.machine-word? obj)
  (assertion-violation who
    "expected non-negative exact integer in the range of a machine word as argument" obj))

(define-argument-validation (ptrdiff who obj)
  (words.ptrdiff? obj)
  (assertion-violation who
    "expected exact integer representing pointer difference as argument" obj))


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

  (define-pointer-comparison pointer=?		capi.ffi-pointer-eq)
  (define-pointer-comparison pointer<>?		capi.ffi-pointer-neq)
  (define-pointer-comparison pointer<?		capi.ffi-pointer-lt)
  (define-pointer-comparison pointer>?		capi.ffi-pointer-gt)
  (define-pointer-comparison pointer<=?		capi.ffi-pointer-le)
  (define-pointer-comparison pointer>=?		capi.ffi-pointer-ge))


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


;;; getters and setters

(define-syntax define-getter
  (syntax-rules ()
    ((_ name foreign-name)
     (define name
       (lambda (p i)
	 (if (pointer? p)
	     (if (fixnum? i)
		 (foreign-call foreign-name p i)
	       (die 'name "index is not a fixnum" i))
	   (die 'name "not a pointer" p)))))))

(define-syntax define-setter
  (syntax-rules ()
    ((_ name pred? foreign-name)
     (define name
       (lambda (p i v)
	 (if (pointer? p)
	     (if (fixnum? i)
		 (if (pred? v)
		     (foreign-call foreign-name p i v)
		   (die 'name
			(format "value must satisfy the predicate ~a" 'pred?)
			v))
	       (die 'name "index is not a fixnum" i))
	   (die 'name "not a pointer" p)))))))

(define (int? x) (or (fixnum? x) (bignum? x)))

(define-getter pointer-ref-c-signed-char        "ikrt_ref_char")
(define-getter pointer-ref-c-signed-short       "ikrt_ref_short")
(define-getter pointer-ref-c-signed-int         "ikrt_ref_int")
(define-getter pointer-ref-c-signed-long        "ikrt_ref_long")
(define-getter pointer-ref-c-signed-long-long   "ikrt_ref_longlong")
(define-getter pointer-ref-c-unsigned-char      "ikrt_ref_uchar")
(define-getter pointer-ref-c-unsigned-short     "ikrt_ref_ushort")
(define-getter pointer-ref-c-unsigned-int       "ikrt_ref_uint")
(define-getter pointer-ref-c-unsigned-long      "ikrt_ref_ulong")
(define-getter pointer-ref-c-unsigned-long-long "ikrt_ref_ulonglong")
(define-getter pointer-ref-c-float              "ikrt_ref_float")
(define-getter pointer-ref-c-double             "ikrt_ref_double")
(define-getter pointer-ref-c-pointer            "ikrt_ref_pointer")

(define-setter pointer-set-c-char!      int?     "ikrt_set_char")
(define-setter pointer-set-c-short!     int?     "ikrt_set_short")
(define-setter pointer-set-c-int!       int?     "ikrt_set_int")
(define-setter pointer-set-c-long!      int?     "ikrt_set_long")
(define-setter pointer-set-c-long-long! int?     "ikrt_set_longlong")
(define-setter pointer-set-c-float!     flonum?  "ikrt_set_float")
(define-setter pointer-set-c-double!    flonum?  "ikrt_set_double")
(define-setter pointer-set-c-pointer!   pointer? "ikrt_set_pointer")


;;; libffi interface

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
