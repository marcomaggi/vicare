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
  (export pointer? integer->pointer pointer->integer
          dlopen dlerror dlclose dlsym malloc free memcpy
          errno
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
          pointer-set-c-double!
          make-c-callout make-c-callback)
  (import
    (except (ikarus)
      pointer?
      integer->pointer pointer->integer
      dlopen dlerror dlclose dlsym malloc free memcpy))

  ;;; pointer manipulation procedures

  (define (pointer? x)
    (foreign-call "ikrt_is_pointer" x))

  (define (integer->pointer x)
    (cond
      [(fixnum? x)
       (foreign-call "ikrt_fx_to_pointer" x)]
      [(bignum? x)
       (foreign-call "ikrt_bn_to_pointer" x)]
      [else
       (die 'integer->pointer "not an integer" x)]))

  (define (pointer->integer x)
    (cond
      [(pointer? x)
       (foreign-call "ikrt_pointer_to_int" x)]
      [else
       (die 'pointer->integer "not a pointer" x)]))

  ;;; dynamic loading procedures

  (define dlerror
    (lambda ()
      (let ([p (foreign-call "ikrt_dlerror")])
        (and p (utf8->string p)))))

  (define dlopen
    (let ()
      (define (open x lazy? global?)
        (foreign-call "ikrt_dlopen" x lazy? global?))
      (case-lambda
        [()
         (open #f #f #f)]
        [(x)
         (dlopen x #f #f)]
        [(x lazy? global?)
         (cond
           [(string? x) (open (string->utf8 x) lazy? global?)]
           [else (die 'dlopen "library name must be a string" x)])])))

  (define dlclose
    (lambda (x)
      (if (pointer? x)
          (foreign-call "ikrt_dlclose" x)
          (die 'dlclose "not a pointer" x))))

  (define dlsym
    (lambda (handle name)
      (define who 'dlsym)
      (if (pointer? handle)
          (if (string? name)
              (foreign-call "ikrt_dlsym" handle (string->utf8 name))
              (die who "invalid symbol name" name))
          (die who "handle is not a pointer" handle))))

  ;;; explicit memory management

  (define (malloc len)
    (if (and (fixnum? len) (fx>? len 0))
        (foreign-call "ikrt_malloc" len)
        (die 'malloc "not a positive fixnum" len)))

  (define (free x)
    (if (pointer? x)
        (foreign-call "ikrt_free" x)
        (die 'free "not a pointer" x)))

  (define (pointer+ ptr off)
    (integer->pointer (+ (pointer->integer ptr) off)))


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
      [(_ name foreign-name)
       (define name
         (lambda (p i)
           (if (pointer? p)
               (if (fixnum? i)
                   (foreign-call foreign-name p i)
                   (die 'name "index is not a fixnum" i))
               (die 'name "not a pointer" p))))]))

  (define-syntax define-setter
    (syntax-rules ()
      [(_ name pred? foreign-name)
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
               (die 'name "not a pointer" p))))]))

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
        [(vector? t)
         (let ([t* (vector-map checker t)])
           (lambda (v)
             (and (vector? v)
                  (let ([n (vector-length v)])
                    (and (= n (vector-length t))
                         (let f ([i 0])
                           (or (= i n)
                               (and ((vector-ref t* i) (vector-ref v i))
                                    (f (+ i 1))))))))))]
        [else
         (case t
           [(unsigned-char)      int?]
           [(signed-char)        int?]
           [(unsigned-short)     int?]
           [(signed-short)       int?]
           [(unsigned-int)       int?]
           [(signed-int)         int?]
           [(unsigned-long)      int?]
           [(signed-long)        int?]
           [(unsigned-long-long) int?]
           [(signed-long-long)   int?]
           [(float)              flonum?]
           [(double)             flonum?]
           [(pointer)            pointer?]
           [else (die who "invalid type" t)])]))
    checker)



  (define (ffi-prep-cif who rtype argtypes)
    (define (convert x)
      (cond
        [(vector? x) (vector-map convert x)]
        [else
         (case x
           [(void)                1]
           [(unsigned-char)       2]
           [(signed-char)         3]
           [(unsigned-short)      4]
           [(signed-short)        5]
           [(unsigned-int)        6]
           [(signed-int)          7]
           [(unsigned-long)       8]
           [(signed-long)         9]
           [(unsigned-long-long) 10]
           [(signed-long-long)   11]
           [(float)              12]
           [(double)             13]
           [(pointer)            14]
           [else (die who "invalid type" x)])]))
    (unless (list? argtypes)
      (die who "arg types is not a list" argtypes))
    (let ([argtypes-n (vector-map convert (list->vector argtypes))]
          [rtype-n (convert rtype)])
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
    (let-values ([(cif argtypes-n rtype-n)
                  (ffi-prep-cif who rtype argtypes)])
      (let* ([argtypes-vec (list->vector argtypes)]
             [checkers (vector-map (checker who) argtypes-vec)])
        (lambda (cfun)
          (define data (vector cif cfun argtypes-n rtype-n))
          (unless (pointer? cfun)
            (die who "not a pointer" cfun))
          (lambda args
            (let ([argsvec (list->vector args)])
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
              (foreign-call "ikrt_ffi_call" data argsvec)))))))

  (define (make-c-callback rtype argtypes)
    (define who 'make-c-callback)
    (let-values ([(cif argtypes-n rtype-n)
                  (ffi-prep-cif who rtype argtypes)])
      (lambda (proc)
        (unless (procedure? proc)
          (die who "not a procedure"))
        (let ([proc
               (cond
                 [(eq? rtype 'void) proc]
                 [else
                  (let ([p? ((checker who) rtype)])
                    (lambda args
                      (let ([v (apply proc args)])
                        (unless (p? v)
                          (die 'callback
                             (format "returned value does not match type ~a"
                                     rtype)
                             v))
                        v)))])])
          (let ([data (vector cif proc argtypes-n rtype-n)])
            (or (foreign-call "ikrt_prepare_callback" data)
                (die who "cannot prepare foreign callback")))))))

  (define (ffi-enabled?)
    (foreign-call "ikrt_has_ffi"))

  (define (errno)
    (foreign-call "ikrt_last_errno"))

  )



