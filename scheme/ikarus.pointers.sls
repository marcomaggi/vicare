;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011-2013, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.pointers)
  (export

    ;; pointer objects
    pointer?				list-of-pointers?
    false-or-pointer?
    null-pointer			set-pointer-null!
    pointer-null?			pointer-non-null?
    pointer->integer			integer->pointer
    pointer-clone			pointer-and-offset?
    pointer-diff			pointer-add
    pointer=?				pointer!=?
    pointer<?				pointer>?
    pointer<=?				pointer>=?
    pointer-min				pointer-max

    ;; memory blocks
    memory-block?			memory-block?/non-null
    make-memory-block/guarded		memory-block-size
    memory-block-reset			null-memory-block
    (rename (%make-memory-block		make-memory-block)
	    (%memory-block-pointer	memory-block-pointer)
	    (memory-block?/non-null	memory-block?/not-null))
    memory-block=?			memory-block!=?

    ;; shared libraries inteface
    dlopen				dlclose
    dlsym				dlerror

    ;; calling functions and callbacks
    make-c-callout-maker		make-c-callout-maker/with-errno
    make-c-callback-maker		free-c-callback

    ;; raw memory allocation
    malloc				guarded-malloc
    realloc				guarded-realloc
    calloc				guarded-calloc
    free				memcmp
    memcpy				memmove
    memset				memory-copy
    bytevector->memory			bytevector->guarded-memory
    memory->bytevector

    with-local-storage

    ;; C strings
    strlen
    strcmp				strncmp
    strdup				strndup
    guarded-strdup			guarded-strndup
    cstring->bytevector			cstring->string
    cstring16->bytevector		cstring16n->string
    cstring16le->string			cstring16be->string
    bytevector->cstring			bytevector->guarded-cstring
    string->cstring			string->guarded-cstring

    ;; C array of C strings
    argv-length
    argv->bytevectors			argv->strings
    bytevectors->argv			bytevectors->guarded-argv
    strings->argv			strings->guarded-argv

    ;; errno interface
    errno

    ;; memory accessors and mutators
    pointer-ref-c-uint8			pointer-ref-c-sint8
    pointer-ref-c-uint16		pointer-ref-c-sint16
    pointer-ref-c-uint32		pointer-ref-c-sint32
    pointer-ref-c-uint64		pointer-ref-c-sint64

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long

    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-double-complex	pointer-ref-c-pointer

    pointer-ref-c-size_t		pointer-ref-c-ssize_t
    pointer-ref-c-off_t			pointer-ref-c-ptrdiff_t

    pointer-set-c-uint8!		pointer-set-c-sint8!
    pointer-set-c-uint16!		pointer-set-c-sint16!
    pointer-set-c-uint32!		pointer-set-c-sint32!
    pointer-set-c-uint64!		pointer-set-c-sint64!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!

    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-double-complex!	pointer-set-c-pointer!

    pointer-set-c-size_t!		pointer-set-c-ssize_t!
    pointer-set-c-off_t!		pointer-set-c-ptrdiff_t!

    ;; memory array accessors and mutators
    array-ref-c-uint8			array-ref-c-sint8
    array-ref-c-uint16			array-ref-c-sint16
    array-ref-c-uint32			array-ref-c-sint32
    array-ref-c-uint64			array-ref-c-sint64

    array-ref-c-signed-char		array-ref-c-unsigned-char
    array-ref-c-signed-short		array-ref-c-unsigned-short
    array-ref-c-signed-int		array-ref-c-unsigned-int
    array-ref-c-signed-long		array-ref-c-unsigned-long
    array-ref-c-signed-long-long	array-ref-c-unsigned-long-long

    array-ref-c-float			array-ref-c-double
    array-ref-c-double-complex		array-ref-c-pointer

    array-ref-c-size_t			array-ref-c-ssize_t
    array-ref-c-off_t			array-ref-c-ptrdiff_t

    array-set-c-uint8!			array-set-c-sint8!
    array-set-c-uint16!			array-set-c-sint16!
    array-set-c-uint32!			array-set-c-sint32!
    array-set-c-uint64!			array-set-c-sint64!

    array-set-c-signed-char!		array-set-c-unsigned-char!
    array-set-c-signed-short!		array-set-c-unsigned-short!
    array-set-c-signed-int!		array-set-c-unsigned-int!
    array-set-c-signed-long!		array-set-c-unsigned-long!
    array-set-c-signed-long-long!	array-set-c-unsigned-long-long!

    array-set-c-float!			array-set-c-double!
    array-set-c-double-complex!		array-set-c-pointer!

    array-set-c-size_t!			array-set-c-ssize_t!
    array-set-c-off_t!			array-set-c-ptrdiff_t!

    ;; bindings or (vicare system $pointers)
    $pointer!=
    $pointer<				$pointer>
    $pointer<=				$pointer>=
    $pointer-min			$pointer-max
    #| end of EXPORT |# )
  (import (except (vicare)
		  ;; pointer objects
		  pointer?				list-of-pointers?
		  false-or-pointer?
		  null-pointer				set-pointer-null!
		  pointer-null?				pointer-non-null?
		  pointer->integer			integer->pointer
		  pointer-clone				pointer-and-offset?
		  pointer-diff				pointer-add
		  pointer=?				pointer!=?
		  pointer<?				pointer>?
		  pointer<=?				pointer>=?
		  pointer-min				pointer-max

		  ;; memory blocks
		  make-memory-block			make-memory-block/guarded
		  memory-block?
		  memory-block?/non-null		memory-block?/not-null
		  memory-block-pointer			memory-block-size
		  memory-block-reset			null-memory-block
		  memory-block=?			memory-block!=?

		  ;; shared libraries inteface
		  dlopen				dlclose
		  dlsym					dlerror

		  ;; calling functions and callbacks
		  make-c-callout-maker			make-c-callout-maker/with-errno
		  make-c-callback-maker			free-c-callback

		  ;; raw memory allocation
		  malloc				guarded-malloc
		  realloc				guarded-realloc
		  calloc				guarded-calloc
		  free					memcmp
		  memcpy				memmove
		  memset				memory-copy
		  bytevector->memory			bytevector->memory*
		  bytevector->guarded-memory		bytevector->guarded-memory*
		  memory->bytevector

		  with-local-storage

		  &out-of-memory-error
		  &out-of-memory-error-rtd		&out-of-memory-error-rcd
		  make-out-of-memory-error		out-of-memory-error?

		  ;; C strings
		  strlen
		  strcmp				strncmp
		  strdup				strndup
		  guarded-strdup			guarded-strndup
		  cstring->bytevector			cstring->string
		  cstring16->bytevector			cstring16n->string
		  cstring16le->string			cstring16be->string
		  bytevector->cstring			bytevector->guarded-cstring
		  string->cstring			string->guarded-cstring

		  ;; C array of C strings
		  argv-length
		  argv->bytevectors			argv->strings
		  bytevectors->argv			bytevectors->guarded-argv
		  strings->argv				strings->guarded-argv

		  ;; errno interface
		  errno

		  ;; memory accessors and mutators
		  pointer-ref-c-uint8			pointer-ref-c-sint8
		  pointer-ref-c-uint16			pointer-ref-c-sint16
		  pointer-ref-c-uint32			pointer-ref-c-sint32
		  pointer-ref-c-uint64			pointer-ref-c-sint64

		  pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
		  pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
		  pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
		  pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
		  pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long

		  pointer-ref-c-float			pointer-ref-c-double
		  pointer-ref-c-double-complex		pointer-ref-c-pointer

		  pointer-ref-c-size_t			pointer-ref-c-ssize_t
		  pointer-ref-c-off_t			pointer-ref-c-ptrdiff_t

		  pointer-set-c-uint8!			pointer-set-c-sint8!
		  pointer-set-c-uint16!			pointer-set-c-sint16!
		  pointer-set-c-uint32!			pointer-set-c-sint32!
		  pointer-set-c-uint64!			pointer-set-c-sint64!

		  pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
		  pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
		  pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
		  pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
		  pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!

		  pointer-set-c-float!			pointer-set-c-double!
		  pointer-set-c-double-complex!		pointer-set-c-pointer!

		  pointer-set-c-size_t!			pointer-set-c-ssize_t!
		  pointer-set-c-off_t!			pointer-set-c-ptrdiff_t!

		  ;; memory array accessors and mutators
		  array-ref-c-uint8			array-ref-c-sint8
		  array-ref-c-uint16			array-ref-c-sint16
		  array-ref-c-uint32			array-ref-c-sint32
		  array-ref-c-uint64			array-ref-c-sint64

		  array-ref-c-signed-char		array-ref-c-unsigned-char
		  array-ref-c-signed-short		array-ref-c-unsigned-short
		  array-ref-c-signed-int		array-ref-c-unsigned-int
		  array-ref-c-signed-long		array-ref-c-unsigned-long
		  array-ref-c-signed-long-long		array-ref-c-unsigned-long-long

		  array-ref-c-float			array-ref-c-double
		  array-ref-c-double-complex		array-ref-c-pointer

		  array-ref-c-size_t			array-ref-c-ssize_t
		  array-ref-c-off_t			array-ref-c-ptrdiff_t

		  array-set-c-uint8!			array-set-c-sint8!
		  array-set-c-uint16!			array-set-c-sint16!
		  array-set-c-uint32!			array-set-c-sint32!
		  array-set-c-uint64!			array-set-c-sint64!

		  array-set-c-signed-char!		array-set-c-unsigned-char!
		  array-set-c-signed-short!		array-set-c-unsigned-short!
		  array-set-c-signed-int!		array-set-c-unsigned-int!
		  array-set-c-signed-long!		array-set-c-unsigned-long!
		  array-set-c-signed-long-long!		array-set-c-unsigned-long-long!

		  array-set-c-float!			array-set-c-double!
		  array-set-c-double-complex!		array-set-c-pointer!

		  array-set-c-size_t!			array-set-c-ssize_t!
		  array-set-c-off_t!			array-set-c-ptrdiff_t!)
    (vicare system structs)
    (only (vicare system $pointers)
	  $pointer=)
    (vicare system $bytevectors)
    (only (ikarus conditions)
	  %raise-out-of-memory)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $vectors)
    (only (ikarus hash-tables)
	  $exact-integer-hash)
    (prefix (vicare unsafe capi)
	    capi.)
    (prefix (vicare platform words)
	    words.)
    (only (vicare language-extensions syntaxes)
	  with-pathnames
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-inequality-predicate))

  (module (arguments-validation)
    (include "ikarus.config.scm"))


;;;; validation predicates and assertions

(define (errno-value? obj)
  (or (boolean? obj)
      (non-negative-fixnum? obj)))

(define (pathname? obj)
  (or (bytevector? obj)
      (string?     obj)))

(define (pointer/memory-block? obj)
  (or (pointer? obj)
      (memory-block? obj)))

(define-syntax-rule (pointer-offset? ?obj)
  (non-negative-fixnum? ?obj))

(define-syntax-rule (number-of-bytes? ?N)
  (words.size_t? ?N))

(define-syntax-rule (number-of-elements? ?N)
  (words.size_t? ?N))

(define (byte/octet? obj)
  (or (words.word-u8? obj)
      (words.word-s8? obj)))

(define (vector-of-lengths? obj)
  (and (vector? obj)
       (vector-for-all non-negative-fixnum? obj)))

;;; --------------------------------------------------------------------

(define-syntax (assert-pointer-and-offset stx)
  (syntax-case stx ()
    ((_ ?ptr ?delta)
     (and (identifier? #'?ptr)
	  (identifier? #'?delta))
     #'(unless (%pointer-and-offset? ?ptr ?delta)
	 (procedure-arguments-consistency-violation __who__
	   "offset would cause pointer overflow or underflow" ?ptr ?delta)))
    ))

(define-syntax (assert-memory-and-ptrdiff stx)
  (syntax-case stx ()
    ((_ ?memory ?offset ?data-size)
     (and (identifier? #'?memory)
	  (identifier? #'?offset))
     #'(unless (or (pointer? ?memory)
		   (<= (+ ?offset ?data-size) (memory-block-size ?memory)))
	 (procedure-arguments-consistency-violation __who__
	   "offset from pointer out of range for data size"
	   ?memory ?offset ?data-size)))
    ))

(define-syntax (assert-memory-and-index stx)
  (syntax-case stx ()
    ((_ ?memory ?index ?data-size)
     (and (identifier? #'?memory)
	  (identifier? #'?index))
     #'(unless (or (pointer? ?memory)
		   (<= (* ?index ?data-size) (memory-block-size ?memory)))
	 (procedure-arguments-consistency-violation __who__
	   "offset from pointer out of range for data size"
	   ?memory ?index ?data-size)))
    ))


;;;; errno interface

(case-define* errno
  (()
   (foreign-call "ikrt_last_errno"))
  (({errno errno-value?})
   (foreign-call "ikrt_set_errno" errno)
   (values)))


;;;; memory blocks

(define-struct memory-block
  (pointer
		;Pointer object referencing a block of raw memory.
   size
		;Exact integer representing the number  of bytes in the memory block.
		;It must be in the range of a C language type "size_t".
   owner?
		;Boolean, if true the block of  memory must be released whenever this
		;structure instance is garbage collected.
   ))

(define (%struct-memory-block-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[memory-block")
  (%display " pointer=")	(%display ($memory-block-pointer S))
  (%display " size=")		(%display ($memory-block-size    S))
  (%display " owner?=")		(%display ($memory-block-owner?  S))
  (%display "]"))

(define (%memory-block-destructor S)
  (when ($memory-block-owner? S)
    ;;Remember that FREE will mutate to NULL the pointer.
    (capi.ffi-free ($memory-block-pointer S))
    ($set-memory-block-pointer! S (void))
    ($set-memory-block-size!    S (void))))

;;; --------------------------------------------------------------------

(define* (%make-memory-block {pointer pointer?} {size number-of-bytes?})
  ;;Wrapper for the constructor MAKE-MEMORY-BLOCK which validates the arguments.
  ;;
  (make-memory-block pointer size #f))

(define (null-memory-block)
  (make-memory-block (null-pointer) 0 #f))

(define* (make-memory-block/guarded {pointer pointer?} {size number-of-bytes?})
  (make-memory-block pointer size #t))

(define (memory-block?/non-null obj)
  (and (memory-block? obj)
       (not (pointer-null? ($memory-block-pointer obj)))))

(define (%memory-block-pointer obj)
  (pointer-clone (memory-block-pointer obj)))

(define* (memory-block-reset {B memory-block?})
  (%memory-block-destructor B)
  ($set-memory-block-pointer! B (null-pointer))
  ($set-memory-block-size!    B 0)
  ($set-memory-block-owner?!  B #f))

;;; --------------------------------------------------------------------

(define-equality/sorting-predicate memory-block=?	$memory-block=	memory-block?)
(define-inequality-predicate       memory-block!=?	$memory-block!=	memory-block?)

(define ($memory-block= A B)
  (and ($pointer= ($memory-block-pointer  A)
		  ($memory-block-pointer  B))
       (= ($memory-block-size A)
	  ($memory-block-size B))))

(define ($memory-block!= A B)
  (not ($memory-block= A B)))


;;; shared libraries interface

(define (dlerror)
  (let ((p (capi.ffi-dlerror)))
    (and p (ascii->string p))))

(case-define* dlopen
  (()
   (capi.ffi-dlopen #f #f #f))
  ((libname)
   (dlopen libname #f #f))
  (({libname pathname?} lazy? global?)
   (with-pathnames ((libname.bv libname))
     (capi.ffi-dlopen libname.bv lazy? global?))))

(define* (dlclose {ptr pointer?})
  (capi.ffi-dlclose ptr))

(define* (dlsym {handle pointer?} {name string?})
  (capi.ffi-dlsym handle (string->ascii name)))


;;; pointer manipulation procedures

(define (false-or-pointer? obj)
  (or (not obj)
      (pointer? obj)))

(define-list-of-type-predicate list-of-pointers? pointer?)

(define (pointer? obj)
  ;;This is also a primitive operation.
  (import (prefix (only (vicare) pointer?) sys:))
  (sys:pointer? obj))

(define (null-pointer)
  (capi.ffi-fixnum->pointer 0))

(define (pointer-null? obj)
  (and (pointer? obj) (capi.ffi-pointer-null? obj)))

(define* (set-pointer-null! {ptr pointer?})
  (capi.ffi-set-pointer-null! ptr)
  (values))

(define (pointer-non-null? obj)
  (and (pointer? obj)
       (not (capi.ffi-pointer-null? obj))))

;;; --------------------------------------------------------------------

(define* (integer->pointer {x words.machine-word?})
  (if (fixnum? x)
      (capi.ffi-fixnum->pointer x)
    (capi.ffi-bignum->pointer x)))

(define* (pointer->integer {x pointer?})
  (capi.ffi-pointer->integer x))

;;; --------------------------------------------------------------------

(define* (pointer-clone {ptr pointer?})
  (capi.ffi-pointer-clone ptr))

;;; --------------------------------------------------------------------

(define* (pointer-and-offset? {ptr pointer?} {offset exact-integer?})
  (%pointer-and-offset? ptr offset))

(define (%pointer-and-offset? pointer offset)
  (cond ((zero? offset)
	 #t)
	((negative? offset)
	 ;;            pointer
	 ;;               v
	 ;; |-------------+----------| range of pointers
	 ;; 0                       max
	 ;;
	 ;; |-------------| range of (- offset)
	 ;;
	 (<= (- offset) (capi.ffi-pointer->integer pointer)))
	(else
	 ;;            pointer
	 ;;               v
	 ;; |-------------+----------| range of pointers
	 ;; 0                       max
	 ;;
	 ;;               |----------| range of (- offset)
	 ;;
	 (<= offset (- (words.greatest-c-pointer)
		       (capi.ffi-pointer->integer pointer))))))

(define* (pointer-add {ptr pointer?} {delta words.ptrdiff_t?})
  (assert-pointer-and-offset ptr delta)
  (let ((rv (capi.ffi-pointer-add ptr delta)))
    (or rv
	(procedure-arguments-consistency-violation __who__
	  "requested pointer arithmetic operation would cause machine word overflow or underflow"
	  ptr delta))))

(define* (pointer-diff {ptr1 pointer?} {ptr2 pointer?})
  ;;Implemented  at the  Scheme level  because  converting pointers  to Scheme  exact
  ;;integer objects is the simplest and safest way to correctly handle the full range
  ;;of possible pointer values.
  (- (capi.ffi-pointer->integer ptr1)
     (capi.ffi-pointer->integer ptr2)))

(define (pointer+ ptr off)
  (integer->pointer (+ (pointer->integer ptr) off)))


;;;; comparison

(define-equality/sorting-predicate pointer=?	$pointer=	pointer?)
(define-equality/sorting-predicate pointer<?	$pointer<	pointer?)
(define-equality/sorting-predicate pointer<=?	$pointer<=	pointer?)
(define-equality/sorting-predicate pointer>?	$pointer>	pointer?)
(define-equality/sorting-predicate pointer>=?	$pointer>=	pointer?)
(define-inequality-predicate       pointer!=?	$pointer!=	pointer?)

(define ($pointer!= ptr1 ptr2)
  (capi.ffi-pointer-neq ptr1 ptr2))

(define ($pointer< ptr1 ptr2)
  (capi.ffi-pointer-lt ptr1 ptr2))

(define ($pointer> ptr1 ptr2)
  (capi.ffi-pointer-gt ptr1 ptr2))

(define ($pointer<= ptr1 ptr2)
  (capi.ffi-pointer-le ptr1 ptr2))

(define ($pointer>= ptr1 ptr2)
  (capi.ffi-pointer-ge ptr1 ptr2))


;;;; min max

(define-min/max-comparison pointer-max $pointer-max pointer?)
(define-min/max-comparison pointer-min $pointer-min pointer?)

(define ($pointer-min str1 str2)
  (if ($pointer< str1 str2) str1 str2))

(define ($pointer-max str1 str2)
  (if ($pointer< str1 str2) str2 str1))


;;;; pointer accessors

(let-syntax
    ((def (syntax-rules ()
			((_ ?who ?accessor ?data-size)
			 (define* (?who {memory pointer/memory-block?} {offset words.ptrdiff_t?})
			   (assert-memory-and-ptrdiff memory offset ?data-size)
			   (?accessor memory offset)))
			)))
  (def pointer-ref-c-uint8		capi.ffi-pointer-ref-c-uint8	      1)
  (def pointer-ref-c-sint8		capi.ffi-pointer-ref-c-sint8	      1)
  (def pointer-ref-c-uint16		capi.ffi-pointer-ref-c-uint16	      2)
  (def pointer-ref-c-sint16		capi.ffi-pointer-ref-c-sint16	      2)
  (def pointer-ref-c-uint32		capi.ffi-pointer-ref-c-uint32	      4)
  (def pointer-ref-c-sint32		capi.ffi-pointer-ref-c-sint32	      4)
  (def pointer-ref-c-uint64		capi.ffi-pointer-ref-c-uint64	      8)
  (def pointer-ref-c-sint64		capi.ffi-pointer-ref-c-sint64	      8)

  (def pointer-ref-c-float		capi.ffi-pointer-ref-c-float		      words.SIZEOF_FLOAT)
  (def pointer-ref-c-double		capi.ffi-pointer-ref-c-double		      words.SIZEOF_DOUBLE)
  (def pointer-ref-c-double-complex	capi.ffi-pointer-ref-c-double-complex	      words.SIZEOF_DOUBLE_COMPLEX)
  (def pointer-ref-c-pointer		capi.ffi-pointer-ref-c-pointer		      words.SIZEOF_POINTER)

  (def pointer-ref-c-signed-char	capi.ffi-pointer-ref-c-signed-char	      words.SIZEOF_CHAR)
  (def pointer-ref-c-signed-short	capi.ffi-pointer-ref-c-signed-short	      words.SIZEOF_SHORT)
  (def pointer-ref-c-signed-int		capi.ffi-pointer-ref-c-signed-int	      words.SIZEOF_INT)
  (def pointer-ref-c-signed-long	capi.ffi-pointer-ref-c-signed-long	      words.SIZEOF_LONG)
  (def pointer-ref-c-signed-long-long	capi.ffi-pointer-ref-c-signed-long-long	      words.SIZEOF_LONG_LONG)

  (def pointer-ref-c-unsigned-char	capi.ffi-pointer-ref-c-unsigned-char	      words.SIZEOF_CHAR)
  (def pointer-ref-c-unsigned-short	capi.ffi-pointer-ref-c-unsigned-short	      words.SIZEOF_SHORT)
  (def pointer-ref-c-unsigned-int	capi.ffi-pointer-ref-c-unsigned-int	      words.SIZEOF_INT)
  (def pointer-ref-c-unsigned-long	capi.ffi-pointer-ref-c-unsigned-long	      words.SIZEOF_LONG)
  (def pointer-ref-c-unsigned-long-long	capi.ffi-pointer-ref-c-unsigned-long-long     words.SIZEOF_LONG_LONG)

  (def pointer-ref-c-size_t		capi.ffi-pointer-ref-c-size_t		      words.SIZEOF_SIZE_T)
  (def pointer-ref-c-ssize_t		capi.ffi-pointer-ref-c-ssize_t		      words.SIZEOF_SSIZE_T)
  (def pointer-ref-c-off_t		capi.ffi-pointer-ref-c-off_t		      words.SIZEOF_OFF_T)
  (def pointer-ref-c-ptrdiff_t		capi.ffi-pointer-ref-c-ptrdiff_t	      words.SIZEOF_PTRDIFF_T)

  #| end of LET-SYNTAX |# )


;;;; pointer mutators

(let-syntax
    ((def (syntax-rules ()
	    ((_ ?who ?mutator ?type-pred ?data-size)
	     (define* (?who {memory pointer/memory-block?} {offset words.ptrdiff_t?} {value ?type-pred})
	       (assert-memory-and-ptrdiff memory offset ?data-size)
	       (?mutator memory offset value)
	       (values)))
	    )))
  (def pointer-set-c-uint8!		capi.ffi-pointer-set-c-uint8!		words.word-u8?	      1)
  (def pointer-set-c-sint8!		capi.ffi-pointer-set-c-sint8!		words.word-s8?	      1)
  (def pointer-set-c-uint16!		capi.ffi-pointer-set-c-uint16!		words.word-u16?	      2)
  (def pointer-set-c-sint16!		capi.ffi-pointer-set-c-sint16!		words.word-s16?	      2)
  (def pointer-set-c-uint32!		capi.ffi-pointer-set-c-uint32!		words.word-u32?	      4)
  (def pointer-set-c-sint32!		capi.ffi-pointer-set-c-sint32!		words.word-s32?	      4)
  (def pointer-set-c-uint64!		capi.ffi-pointer-set-c-uint64!		words.word-u64?	      8)
  (def pointer-set-c-sint64!		capi.ffi-pointer-set-c-sint64!		words.word-s64?	      8)

  (def pointer-set-c-float!		capi.ffi-pointer-set-c-float!		flonum?		      words.SIZEOF_FLOAT)
  (def pointer-set-c-double!		capi.ffi-pointer-set-c-double!		flonum?		      words.SIZEOF_DOUBLE)
  (def pointer-set-c-double-complex!	capi.ffi-pointer-set-c-double-complex!	cflonum?	      words.SIZEOF_DOUBLE_COMPLEX)
  (def pointer-set-c-pointer!		capi.ffi-pointer-set-c-pointer!		pointer?	      words.SIZEOF_POINTER)

  (def pointer-set-c-signed-char!	capi.ffi-pointer-set-c-signed-char!	words.signed-char?    words.SIZEOF_CHAR)
  (def pointer-set-c-signed-short!	capi.ffi-pointer-set-c-signed-short!	words.signed-short?   words.SIZEOF_SHORT)
  (def pointer-set-c-signed-int!	capi.ffi-pointer-set-c-signed-int!	words.signed-int?     words.SIZEOF_INT)
  (def pointer-set-c-signed-long!	capi.ffi-pointer-set-c-signed-long!	words.signed-long?    words.SIZEOF_LONG)
  (def pointer-set-c-signed-long-long!	capi.ffi-pointer-set-c-signed-long-long! words.signed-long-long? words.SIZEOF_LONG)

  (def pointer-set-c-unsigned-char!	capi.ffi-pointer-set-c-unsigned-char!	words.unsigned-char?  words.SIZEOF_CHAR)
  (def pointer-set-c-unsigned-short!	capi.ffi-pointer-set-c-unsigned-short!	words.unsigned-short? words.SIZEOF_SHORT)
  (def pointer-set-c-unsigned-int!	capi.ffi-pointer-set-c-unsigned-int!	words.unsigned-int?   words.SIZEOF_INT)
  (def pointer-set-c-unsigned-long!	capi.ffi-pointer-set-c-unsigned-long!	words.unsigned-long?  words.SIZEOF_LONG)
  (def pointer-set-c-unsigned-long-long! capi.ffi-pointer-set-c-unsigned-long-long! words.unsigned-long-long? words.SIZEOF_LONG_LONG)

  (def pointer-set-c-size_t!		capi.ffi-pointer-set-c-size_t!		words.size_t?	      words.SIZEOF_SIZE_T)
  (def pointer-set-c-ssize_t!		capi.ffi-pointer-set-c-ssize_t!		words.ssize_t?	      words.SIZEOF_SSIZE_T)
  (def pointer-set-c-off_t!		capi.ffi-pointer-set-c-off_t!		words.off_t?	      words.SIZEOF_OFF_T)
  (def pointer-set-c-ptrdiff_t!		capi.ffi-pointer-set-c-ptrdiff_t!	words.ptrdiff_t?      words.SIZEOF_PTRDIFF_T)

  #| end of LET-SYNTAX |# )


;;;; array accessors

(let-syntax
    ((def (syntax-rules ()
	    ((_ ?who ?accessor ?data-size)
	     (define* (?who {memory pointer/memory-block?} {index words.ptrdiff_t?})
	       (assert-memory-and-ptrdiff memory index ?data-size)
	       (assert-memory-and-index   memory index ?data-size)
	       (?accessor memory index)))
	    )))
  (def array-ref-c-uint8		capi.ffi-array-ref-c-uint8		1)
  (def array-ref-c-sint8		capi.ffi-array-ref-c-sint8		1)
  (def array-ref-c-uint16		capi.ffi-array-ref-c-uint16		2)
  (def array-ref-c-sint16		capi.ffi-array-ref-c-sint16		2)
  (def array-ref-c-uint32		capi.ffi-array-ref-c-uint32		4)
  (def array-ref-c-sint32		capi.ffi-array-ref-c-sint32		4)
  (def array-ref-c-uint64		capi.ffi-array-ref-c-uint64		8)
  (def array-ref-c-sint64		capi.ffi-array-ref-c-sint64		8)

  (def array-ref-c-float		capi.ffi-array-ref-c-float		words.SIZEOF_FLOAT)
  (def array-ref-c-double		capi.ffi-array-ref-c-double		words.SIZEOF_DOUBLE)
  (def array-ref-c-double-complex	capi.ffi-array-ref-c-double-complex	words.SIZEOF_DOUBLE_COMPLEX)
  (def array-ref-c-pointer		capi.ffi-array-ref-c-pointer		words.SIZEOF_POINTER)

  (def array-ref-c-signed-char		capi.ffi-array-ref-c-signed-char	words.SIZEOF_CHAR)
  (def array-ref-c-signed-short		capi.ffi-array-ref-c-signed-short	words.SIZEOF_SHORT)
  (def array-ref-c-signed-int		capi.ffi-array-ref-c-signed-int		words.SIZEOF_INT)
  (def array-ref-c-signed-long		capi.ffi-array-ref-c-signed-long	words.SIZEOF_LONG)
  (def array-ref-c-signed-long-long	capi.ffi-array-ref-c-signed-long-long	words.SIZEOF_LONG_LONG)

  (def array-ref-c-unsigned-char	capi.ffi-array-ref-c-unsigned-char	words.SIZEOF_CHAR)
  (def array-ref-c-unsigned-short	capi.ffi-array-ref-c-unsigned-short	words.SIZEOF_SHORT)
  (def array-ref-c-unsigned-int		capi.ffi-array-ref-c-unsigned-int	words.SIZEOF_INT)
  (def array-ref-c-unsigned-long	capi.ffi-array-ref-c-unsigned-long	words.SIZEOF_LONG)
  (def array-ref-c-unsigned-long-long	capi.ffi-array-ref-c-unsigned-long-long	words.SIZEOF_LONG_LONG)

  (def array-ref-c-size_t		capi.ffi-array-ref-c-size_t		words.SIZEOF_SIZE_T)
  (def array-ref-c-ssize_t		capi.ffi-array-ref-c-ssize_t		words.SIZEOF_SSIZE_T)
  (def array-ref-c-off_t		capi.ffi-array-ref-c-off_t		words.SIZEOF_OFF_T)
  (def array-ref-c-ptrdiff_t		capi.ffi-array-ref-c-ptrdiff_t		words.SIZEOF_PTRDIFF_T)

  #| end of LET-SYNTAX |# )


;;;; array mutators

(let-syntax
    ((def (syntax-rules ()
	    ((_ ?who ?mutator ?type-pred ?data-size)
	     (define* (?who {memory pointer/memory-block?} {index words.ptrdiff_t?} {value ?type-pred})
	       (assert-memory-and-ptrdiff memory index ?data-size)
	       (assert-memory-and-index   memory index ?data-size)
	       (?mutator memory index value)
	       (values)))
	    )))
  (def array-set-c-uint8!		capi.ffi-array-set-c-uint8!		words.word-u8?	      1)
  (def array-set-c-sint8!		capi.ffi-array-set-c-sint8!		words.word-s8?	      1)
  (def array-set-c-uint16!		capi.ffi-array-set-c-uint16!		words.word-u16?	      2)
  (def array-set-c-sint16!		capi.ffi-array-set-c-sint16!		words.word-s16?	      2)
  (def array-set-c-uint32!		capi.ffi-array-set-c-uint32!		words.word-u32?	      4)
  (def array-set-c-sint32!		capi.ffi-array-set-c-sint32!		words.word-s32?	      4)
  (def array-set-c-uint64!		capi.ffi-array-set-c-uint64!		words.word-u64?	      8)
  (def array-set-c-sint64!		capi.ffi-array-set-c-sint64!		words.word-s64?	      8)

  (def array-set-c-float!		capi.ffi-array-set-c-float!		flonum?		      words.SIZEOF_FLOAT)
  (def array-set-c-double!		capi.ffi-array-set-c-double!		flonum?		      words.SIZEOF_DOUBLE)
  (def array-set-c-double-complex!	capi.ffi-array-set-c-double-complex!	cflonum?	      words.SIZEOF_DOUBLE_COMPLEX)
  (def array-set-c-pointer!		capi.ffi-array-set-c-pointer!		pointer?	      words.SIZEOF_POINTER)

  (def array-set-c-signed-char!		capi.ffi-array-set-c-signed-char!	words.signed-char?    words.SIZEOF_CHAR)
  (def array-set-c-signed-short!	capi.ffi-array-set-c-signed-short!	words.signed-short?   words.SIZEOF_SHORT)
  (def array-set-c-signed-int!		capi.ffi-array-set-c-signed-int!	words.signed-int?     words.SIZEOF_INT)
  (def array-set-c-signed-long!		capi.ffi-array-set-c-signed-long!	words.signed-long?    words.SIZEOF_LONG)
  (def array-set-c-signed-long-long!	capi.ffi-array-set-c-signed-long-long!	words.signed-long-long? words.SIZEOF_LONG)

  (def array-set-c-unsigned-char!	capi.ffi-array-set-c-unsigned-char!	words.unsigned-char?  words.SIZEOF_CHAR)
  (def array-set-c-unsigned-short!	capi.ffi-array-set-c-unsigned-short!	words.unsigned-short? words.SIZEOF_SHORT)
  (def array-set-c-unsigned-int!	capi.ffi-array-set-c-unsigned-int!	words.unsigned-int?   words.SIZEOF_INT)
  (def array-set-c-unsigned-long!	capi.ffi-array-set-c-unsigned-long!	words.unsigned-long?  words.SIZEOF_LONG)
  (def array-set-c-unsigned-long-long!	capi.ffi-array-set-c-unsigned-long-long! words.unsigned-long-long? words.SIZEOF_LONG_LONG)

  (def array-set-c-size_t!		capi.ffi-array-set-c-size_t!		words.size_t?	      words.SIZEOF_SIZE_T)
  (def array-set-c-ssize_t!		capi.ffi-array-set-c-ssize_t!		words.ssize_t?	      words.SIZEOF_SSIZE_T)
  (def array-set-c-off_t!		capi.ffi-array-set-c-off_t!		words.off_t?	      words.SIZEOF_OFF_T)
  (def array-set-c-ptrdiff_t!		capi.ffi-array-set-c-ptrdiff_t!		words.ptrdiff_t?      words.SIZEOF_PTRDIFF_T)

  #| end of LET-SYNTAX |# )


;;; raw memory management

(define* (malloc {number-of-bytes number-of-bytes?})
  ;;CAPI.FFI-MALLOC returns either a pointer or false.
  (or (capi.ffi-malloc number-of-bytes)
      (%raise-out-of-memory __who__)))

(define* (realloc {memory pointer/memory-block?} {number-of-bytes number-of-bytes?})
  ;;Take care at the C level not to  realloc NULL pointers and of mutating POINTER to
  ;;NULL.  If MEMORY is a MEMORY-BLOCK: update both the pointer and size fields.
  ;;
  ;;CAPI.FFI-REALLOC returns either a pointer or false.
  (or (capi.ffi-realloc memory number-of-bytes)
      (%raise-out-of-memory __who__)))

(define* (calloc {number-of-elements number-of-elements?} {element-size number-of-bytes?})
  ;;CAPI.FFI-CALLOC returns either a pointer or false.
  (or (capi.ffi-calloc number-of-elements element-size)
      (%raise-out-of-memory __who__)))

(define* (free {obj pointer/memory-block?})
  ;;Take care  at the C level  not to "free()" null  pointers and of mutating  PTR to
  ;;NULL.  Also if OBJ is a MEMORY-BLOCK: set the size to zero.
  (capi.ffi-free obj)
  (values))

;;; --------------------------------------------------------------------

(define* (memory-copy dst {dst.start pointer-offset?} src {src.start pointer-offset?} {count number-of-bytes?})
  (cond ((pointer? dst)
	 (cond ((pointer? src)
		(capi.ffi-memcpy (pointer-add dst dst.start)
				 (pointer-add src src.start)
				 count)
		(values))
	       ((bytevector? src)
		(if (bytevector-start-index-and-count-for-word8? src src.start count)
		    (begin
		      (foreign-call "ikrt_memcpy_from_bv" (pointer-add dst dst.start) src src.start count)
		      (values))
		  (procedure-arguments-consistency-violation __who__
		    "start index and bytes count out of range for source bytevector" src src.start count)))
	       (else
		(procedure-argument-violation __who__ "expected pointer or bytevector as source argument" src))))
	((bytevector? dst)
	 (unless (bytevector-start-index-and-count-for-word8? dst dst.start count)
	   (procedure-arguments-consistency-violation __who__
	     "start index and bytes count out of range for destination bytevector" dst dst.start count))
	 (cond ((pointer? src)
		(foreign-call "ikrt_memcpy_to_bv" dst dst.start (pointer-add src src.start) count)
		(values))
	       ((bytevector? src)
		(unless (bytevector-start-index-and-count-for-word8? src src.start count)
		  (procedure-arguments-consistency-violation __who__
		    "start index and bytes count out of range for source bytevector" src src.start count))
		($bytevector-copy!/count src src.start dst dst.start count)
		;;FIXME This VALUES is to be removed at the next boot image rotation.
		;;(Marco Maggi; Sun Nov 27, 2016)
		(values))
	       (else
		(procedure-argument-violation __who__ "expected pointer or bytevector as source argument" src))))
	(else
	 (procedure-argument-violation __who__ "expected pointer or bytevector as destination argument" dst))))

;;; --------------------------------------------------------------------

(define* (memcpy {dst pointer?} {src pointer?} {count number-of-bytes?})
  (capi.ffi-memcpy dst src count)
  (values))

(define* (memmove {dst pointer?} {src pointer?} {count number-of-bytes?})
  (capi.ffi-memmove dst src count)
  (values))

(define* (memset {ptr pointer?} {byte byte/octet?} {count number-of-bytes?})
  (capi.ffi-memset ptr byte count)
  (values))

(define* (memcmp {ptr1 pointer?} {ptr2 pointer?} {count number-of-bytes?})
  (capi.ffi-memcmp ptr1 ptr2 count))

;;; --------------------------------------------------------------------

(define* (memory->bytevector {pointer pointer?} {length non-negative-fixnum?})
  (capi.ffi-memory->bytevector pointer length))

(define* (bytevector->memory {bv bytevector?})
  (cond ((capi.ffi-bytevector->memory bv)
	 => (lambda (rv)
	      (values rv ($bytevector-length bv))))
	(else
	 (%raise-out-of-memory __who__))))

;;; --------------------------------------------------------------------

(define %memory-guardian)

(define (%free-allocated-memory)
  (do ((pointer (%memory-guardian) (%memory-guardian)))
      ((not pointer))
    ;;We know that POINTER is a pointer object.
    (unless (capi.ffi-pointer-null? pointer)
      (capi.ffi-free pointer)
      (capi.ffi-set-pointer-null! pointer))))

;;; --------------------------------------------------------------------

(define* (guarded-malloc {number-of-bytes number-of-bytes?})
  ;;CAPI.FFI-MALLOC returns either a pointer or false.
  (cond ((capi.ffi-malloc number-of-bytes)
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

(define* (guarded-realloc {memory pointer/memory-block?} {number-of-bytes number-of-bytes?})
  ;;CAPI.FFI-REALLOC returns either a pointer or false.
  (cond ((capi.ffi-realloc memory number-of-bytes)
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

(define* (guarded-calloc {number-of-elements number-of-elements?} {element-size number-of-bytes?})
  ;;CAPI.FFI-calloc returns either a pointer or false.
  (cond ((capi.ffi-calloc number-of-elements element-size)
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

(define* (bytevector->guarded-memory {bv bytevector?})
  (cond ((capi.ffi-bytevector->memory bv)
	 => (lambda (ptr)
	      (values (%memory-guardian ptr) ($bytevector-length bv))))
	(else
	 (%raise-out-of-memory __who__))))


;;;; C strings

(define* (strlen {pointer pointer?})
  (capi.ffi-strlen pointer))

(define* (strcmp {pointer1 pointer?} {pointer2 pointer?})
  (capi.ffi-strcmp pointer1 pointer2))

(define* (strncmp {pointer1 pointer?} {pointer2 pointer?} {count number-of-bytes?})
  (capi.ffi-strncmp pointer1 pointer2 count))

(define* (strdup {pointer pointer?})
  (or (capi.ffi-strdup pointer)
      (%raise-out-of-memory __who__)))

(define* (strndup {pointer pointer?} {count number-of-bytes?})
  (or (capi.ffi-strndup pointer count)
      (%raise-out-of-memory __who__)))

;;; --------------------------------------------------------------------

(define* (bytevector->cstring {bv bytevector?})
  (or (capi.ffi-bytevector->cstring bv)
      (%raise-out-of-memory __who__)))

(case-define* cstring->bytevector
  (({pointer pointer?})
   (capi.ffi-cstring->bytevector pointer (capi.ffi-strlen pointer)))
  (({pointer pointer?} {count non-negative-fixnum?})
   (capi.ffi-cstring->bytevector pointer count)))

(define* (cstring16->bytevector {pointer pointer?})
  (capi.ffi-cstring16->bytevector pointer))

;;; --------------------------------------------------------------------

(case-define* cstring->string
  (({pointer pointer?})
   (ascii->string (capi.ffi-cstring->bytevector pointer (capi.ffi-strlen pointer))))
  (({pointer pointer?} {count non-negative-fixnum?})
   (ascii->string (capi.ffi-cstring->bytevector pointer count))))

(define* (cstring16n->string {pointer pointer?})
  (utf16n->string (capi.ffi-cstring16->bytevector pointer)))

(define* (cstring16le->string {pointer pointer?})
  (utf16le->string (capi.ffi-cstring16->bytevector pointer)))

(define* (cstring16be->string {pointer pointer?})
  (utf16be->string (capi.ffi-cstring16->bytevector pointer)))

(define* (string->cstring {str string?})
  (or (capi.ffi-bytevector->cstring (string->ascii str))
      (%raise-out-of-memory __who__)))

;;; --------------------------------------------------------------------

(define* (bytevectors->argv {bv* list-of-bytevectors?})
  (or (capi.ffi-bytevectors->argv bv*)
      (%raise-out-of-memory __who__)))

(define* (argv->bytevectors {pointer pointer?})
  (capi.ffi-argv->bytevectors pointer))

(define* (strings->argv {str* list-of-strings?})
  (or (capi.ffi-bytevectors->argv (map string->ascii str*))
      (%raise-out-of-memory __who__)))

(define* (argv->strings {pointer pointer?})
  (map ascii->string (capi.ffi-argv->bytevectors pointer)))

(define* (argv-length {pointer pointer?})
  (capi.ffi-argv-length pointer))

;;; --------------------------------------------------------------------

(define* (guarded-strdup {pointer pointer?})
  (cond ((capi.ffi-strdup pointer)
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

(define* (guarded-strndup {pointer pointer?} {count number-of-bytes?})
  (cond ((capi.ffi-strndup pointer count)
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

(define* (bytevector->guarded-cstring {bv bytevector?})
  (cond ((capi.ffi-bytevector->cstring bv)
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

(define* (string->guarded-cstring {str string?})
  (cond ((capi.ffi-bytevector->cstring (string->latin1 str))
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

;;; --------------------------------------------------------------------

(define* (bytevectors->guarded-argv {bv* list-of-bytevectors?})
  (cond ((capi.ffi-bytevectors->argv bv*)
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))

(define* (strings->guarded-argv {str* list-of-strings?})
  (cond ((capi.ffi-bytevectors->argv (map string->ascii str*))
	 => %memory-guardian)
	(else
	 (%raise-out-of-memory __who__))))


;;;; local storage

(define* (with-local-storage {lengths vector-of-lengths?} {proc procedure?})
  (capi.ffi-with-local-storage lengths proc))


;;;; Libffi: C API

(define-syntax-rule (capi.ffi-enabled?)
  (foreign-call "ikrt_has_ffi"))

(define-syntax-rule (capi.ffi-prep-cif type-ids)
  (foreign-call "ikrt_ffi_prep_cif" type-ids))

(define-syntax-rule (capi.ffi-callout user-data args)
  (foreign-call "ikrt_ffi_call" user-data args))

(define-syntax-rule (capi.ffi-prepare-callback cif.proc)
  (foreign-call "ikrt_ffi_prepare_callback" cif.proc))

(define-syntax-rule (capi.ffi-free-c-callback c-callback-pointer)
  (foreign-call "ikrt_ffi_release_callback" c-callback-pointer))


;;;; Libffi: native type identifiers

;;The fixnums  identifying the  types must  be kept  in sync  with the  definition of
;;"type_id_t" in the file "ikarus-ffi.c".
;;
(define-inline-constant TYPE_ID_VOID            0)
(define-inline-constant TYPE_ID_UINT8           1)
(define-inline-constant TYPE_ID_SINT8           2)
(define-inline-constant TYPE_ID_UINT16          3)
(define-inline-constant TYPE_ID_SINT16          4)
(define-inline-constant TYPE_ID_UINT32          5)
(define-inline-constant TYPE_ID_SINT32          6)
(define-inline-constant TYPE_ID_UINT64          7)
(define-inline-constant TYPE_ID_SINT64          8)
(define-inline-constant TYPE_ID_FLOAT           9)
(define-inline-constant TYPE_ID_DOUBLE         10)
(define-inline-constant TYPE_ID_POINTER        11)
(define-inline-constant TYPE_ID_UCHAR          12)
(define-inline-constant TYPE_ID_SCHAR          13)
(define-inline-constant TYPE_ID_USHORT         14)
(define-inline-constant TYPE_ID_SSHORT         15)
(define-inline-constant TYPE_ID_UINT           16)
(define-inline-constant TYPE_ID_SINT           17)
(define-inline-constant TYPE_ID_ULONG          18)
(define-inline-constant TYPE_ID_SLONG          19)
(define-inline-constant TYPE_ID_MAX            20)

(define (%type-symbol->type-id type)
  (case type
    ((uint8_t)			TYPE_ID_UINT8)
    ((int8_t)			TYPE_ID_SINT8)
    ((uint16_t)			TYPE_ID_UINT16)
    ((int16_t)			TYPE_ID_SINT16)
    ((uint32_t)			TYPE_ID_UINT32)
    ((int32_t)			TYPE_ID_SINT32)
    ((uint64_t)			TYPE_ID_UINT64)
    ((int64_t)			TYPE_ID_SINT64)

    ((float)			TYPE_ID_FLOAT)
    ((double)			TYPE_ID_DOUBLE)
    ((pointer)			TYPE_ID_POINTER)
    ((callback)			TYPE_ID_POINTER)

    ((void)			TYPE_ID_VOID)
    ((unsigned-char)		TYPE_ID_UCHAR)
    ((signed-char)		TYPE_ID_SCHAR)
    ((unsigned-short)		TYPE_ID_SSHORT)
    ((signed-short)		TYPE_ID_USHORT)
    ((unsigned-int)		TYPE_ID_UINT)
    ((signed-int)		TYPE_ID_SINT)
    ((unsigned-long)		TYPE_ID_ULONG)
    ((signed-long)		TYPE_ID_SLONG)
    ((unsigned-long-long)	TYPE_ID_UINT64)
    ((signed-long-long)		TYPE_ID_SINT64)

    ((size_t)			(cond ((= words.SIZEOF_SIZE_T words.SIZEOF_INT)
				       TYPE_ID_UINT)
				      ((= words.SIZEOF_SIZE_T words.SIZEOF_LONG)
				       TYPE_ID_ULONG)
				      (else
				       TYPE_ID_UINT64)))

    ((ssize_t)			(cond ((= words.SIZEOF_SSIZE_T words.SIZEOF_INT)
				       TYPE_ID_SINT)
				      ((= words.SIZEOF_SSIZE_T words.SIZEOF_LONG)
				       TYPE_ID_SLONG)
				      (else
				       TYPE_ID_SINT64)))

    ((off_t)			(cond ((= words.SIZEOF_OFF_T words.SIZEOF_INT)
				       TYPE_ID_SINT)
				      ((= words.SIZEOF_OFF_T words.SIZEOF_LONG)
				       TYPE_ID_SLONG)
				      (else
				       TYPE_ID_SINT64)))

    ((ptrdiff_t)		(cond ((= words.SIZEOF_PTRDIFF_T words.SIZEOF_INT)
				       TYPE_ID_SINT)
				      ((= words.SIZEOF_PTRDIFF_T words.SIZEOF_LONG)
				       TYPE_ID_SLONG)
				      (else
				       TYPE_ID_SINT64)))

    (else
     (procedure-argument-violation #f "invalid FFI type specifier" type))))

(let-syntax ((define-predicate (syntax-rules ()
				 ((_ ?who ?pred)
				  (define (?who obj) (?pred obj))))))
  (define-predicate %unsigned-char?		words.unsigned-char?)
  (define-predicate %unsigned-short?		words.unsigned-short?)
  (define-predicate %unsigned-int?		words.unsigned-int?)
  (define-predicate %unsigned-long?		words.unsigned-long?)
  (define-predicate %unsigned-long-long?	words.unsigned-long-long?)

  (define-predicate %signed-char?		words.signed-char?)
  (define-predicate %signed-short?		words.signed-short?)
  (define-predicate %signed-int?		words.signed-int?)
  (define-predicate %signed-long?		words.signed-long?)
  (define-predicate %signed-long-long?		words.signed-long-long?)

  (define-predicate %sint8?			words.word-s8?)
  (define-predicate %uint8?			words.word-u8?)
  (define-predicate %sint16?			words.word-s16?)
  (define-predicate %uint16?			words.word-u16?)
  (define-predicate %sint32?			words.word-s32?)
  (define-predicate %uint32?			words.word-u32?)
  (define-predicate %sint64?			words.word-s64?)
  (define-predicate %uint64?			words.word-u64?)

  (define-predicate %size_t?			words.size_t?)
  (define-predicate %ssize_t?			words.ssize_t?)
  (define-predicate %off_t?			words.off_t?)
  (define-predicate %ptrdiff_t?			words.ptrdiff_t?))

(define (pointer/bytevector? obj)
  (or (pointer? obj) (bytevector? obj)))

(define (%select-type-predicate type)
  (case type
    ((unsigned-char)		%unsigned-char?)
    ((unsigned-short)		%unsigned-short?)
    ((unsigned-int)		%unsigned-int?)
    ((unsigned-long)		%unsigned-long?)
    ((unsigned-long-long)	%unsigned-long-long?)

    ((signed-char)		%signed-char?)
    ((signed-short)		%signed-short?)
    ((signed-int)		%signed-int?)
    ((signed-long)		%signed-long?)
    ((signed-long-long)		%signed-long-long?)

    ((float)			flonum?)
    ((double)			flonum?)
    ((pointer)			pointer/bytevector?)
    ((callback)			pointer?)

    ((int8_t)			%sint8?)
    ((uint8_t)			%uint8?)
    ((int16_t)			%sint16?)
    ((uint16_t)			%uint16?)
    ((int32_t)			%sint32?)
    ((uint32_t)			%uint32?)
    ((int64_t)			%sint64?)
    ((uint64_t)			%uint64?)

    ((size_t)			%size_t?)
    ((ssize_t)			%ssize_t?)
    ((off_t)			%off_t?)
    ((ptrdiff_t)		%ptrdiff_t?)

    (else
     (procedure-argument-violation #f "unknown FFI type specifier" type))))


;;; Libffi: call interfaces

(define (ffi-enabled?)
  (capi.ffi-enabled?))

;;Descriptor  for callout  and callback  generators associated  to the  same function
;;signature.  Once allocated, instances of this  type are never released; rather they
;;are cached in CIF-TABLE.
;;
(define-struct cif
  (cif
		;Pointer to a malloc-ed C  language data structure of type "ffi_cif".
		;Once allocated these structures are never released.
   callout-maker
		;False or closure.  The closure  generates callout functions of given
		;signature.
   callout-maker/with-errno
   callback-maker
		;False or Closure.  The closure generates callback functions of given
		;signature
   arg-checkers
		;Vector of predicates used to validate arguments.
   retval-checker
		;Predicate used to validate return value.
   arg-types
		;Vector of symbols representing arg types.
   retval-type
		;Symbol representing return value type.
   ))

;;Maximum for the hash  value of signature vectors.  It is used  to avoid overflow of
;;fixnums, allowing unsafe fx operations to be used.
;;
(define H_MAX
  (- (greatest-fixnum) TYPE_ID_MAX))

(define* (%signature-hash signature)
  ;;Given a vector of fixnums representing native  types for the return value and the
  ;;arguments of a callout or callback, return a fixnum hash value.
  ;;
  (let loop ((signature signature)
	     (len       ($vector-length signature))
	     (H		0)
	     (i         0))
    (cond (($fx= i len)
	   H)
	  (($fx< H_MAX H)
	   (procedure-argument-violation __who__ "FFI signature too big" signature))
	  (else
	   (loop signature len
		 ($fx+ H ($vector-ref signature i))
		 ($fxadd1 i))))))

(define (%$signature=? vec1 vec2)
  (let ((len1 ($vector-length vec1)))
    (and ($fx= len1 ($vector-length vec2))
	 (let loop ((i 0))
	   (or ($fx= i len1)
	       (and ($fx= ($vector-ref vec1 i)
				($vector-ref vec2 i))
		    (loop ($fxadd1 i))))))))

;;Table of structures of type CIF, used to avoid generating duplicates.
;;
(define CIF-TABLE #f)

(define* (%ffi-prep-cif who retval-type {arg-types list?})
  ;;Return an instance of CIF structure  representing the call interface for callouts
  ;;and  callbacks of  the given  signature.  If  a CIF  structure for  such function
  ;;signature  already exists:  retrieve it  from the  hash table;  else build  a new
  ;;structure.
  ;;
  ;;RETVAL-TYPE  must  be  a  symbol  representing the  type  of  the  return  value.
  ;;ARG-TYPES must be a list of symbols representing the types of the arguments.
  ;;
  (let* ((arg-types	(if (equal? '(void) arg-types) '() arg-types))
	 (signature	(vector-map %type-symbol->type-id
			  (list->vector (cons retval-type arg-types)))))
    (unless CIF-TABLE
      (set! CIF-TABLE (make-hashtable %signature-hash %$signature=?)))
    (or (hashtable-ref CIF-TABLE signature #f)
	(let* ((cif			(capi.ffi-prep-cif signature))
	       (arg-types		(if (null? arg-types)
					    '#()
					  (list->vector arg-types)))
	       (arg-checkers		(if (null? arg-types)
					    #f
					  (vector-map %select-type-predicate arg-types)))
	       (retval-checker	(if (eq? 'void retval-type)
				    #f
				  (%select-type-predicate retval-type))))
	  (and cif
	       (let ((S (make-cif cif #f #f #f arg-checkers retval-checker arg-types retval-type)))
		 (hashtable-set! CIF-TABLE signature S)
		 S)))
	(if (ffi-enabled?)
	    (assertion-violation __who__ "failed to initialize C interface" retval-type arg-types)
	  (assertion-violation __who__ "FFI support is not enabled")))))


;;;; Libffi: callouts

(define* (make-c-callout-maker {retval-type symbol?} {arg-types list-of-symbols?})
  ;;Given the symbol RETVAL-TYPE representing the type of the return value and a list
  ;;of symbols ARG-TYPES representing the types of the arguments: return a closure to
  ;;be used to generate Scheme callout functions from pointers to C functions.
  ;;
  (let ((S (%ffi-prep-cif __who__ retval-type arg-types)))
    (or (cif-callout-maker S)
	(receive-and-return (maker)
	    (lambda (c-function-pointer)
	      (%callout-maker S c-function-pointer))
	  (set-cif-callout-maker! S maker)))))

(define* (%callout-maker S {c-function-pointer pointer?})
  ;;Worker  function for  Scheme callout  maker functions.   Return a  closure to  be
  ;;called to call a foreign function.
  ;;
  ;;S must be  an instance of the  CIF data structure.  C-FUNCTION-POINTER  must be a
  ;;pointer object referencing the foreign function.
  ;;
  (let ((user-data (cons (cif-cif S) c-function-pointer)))
    (lambda args	;this is the callout function
      (%generic-callout-wrapper user-data S args))))

(define* (%generic-callout-wrapper user-data S args)
  ;;Worker function for the wrapper of  the actual foreign function: call the foreign
  ;;function and  return its return value.   This function exists mostly  to validate
  ;;the input arguments.
  ;;
  ;;USER-DATA must be a pair whose car is a pointer object referencing a Libffi's CIF
  ;;data structure and whose cdr is a  pointer object representing the address of the
  ;;foreign function to call.
  ;;
  ;;S must be an instance of the CIF data structure.
  ;;
  ;;ARGS is the list of arguments in the call.
  ;;
  (let ((args (list->vector args)))
    ;;Validations.
    (let ((types     (cif-arg-types    S))
	  (checkers  (cif-arg-checkers S)))
      (unless ($fx= ($vector-length args)
		    ($vector-length types))
	(assertion-violation __who__ "wrong number of arguments" types args))
      (when checkers
	(vector-for-each (lambda (arg-pred type arg)
			   (unless (arg-pred arg)
			     (procedure-arguments-consistency-violation __who__
			       "argument does not match specified type" type arg)))
	  checkers types args)))
    (capi.ffi-callout user-data args)))

;;; --------------------------------------------------------------------

(define* (make-c-callout-maker/with-errno {retval-type symbol?} {arg-types list-of-symbols?})
  ;;Given the symbol RETVAL-TYPE representing the type of the return value and a list
  ;;of symbols ARG-TYPES representing the types of the arguments: return a closure to
  ;;be used to generate Scheme callout functions from pointers to C functions.
  ;;
  (let ((S (%ffi-prep-cif __who__ retval-type arg-types)))
    (or (cif-callout-maker/with-errno S)
	(receive-and-return (maker)
	    (lambda (c-function-pointer)
	      (%callout-maker/with-errno S c-function-pointer))
	  (set-cif-callout-maker/with-errno! S maker)))))

(define* (%callout-maker/with-errno S {c-function-pointer pointer?})
  ;;Worker  function for  Scheme callout  maker functions.   Return a  closure to  be
  ;;called to call a foreign function.
  ;;
  ;;S must be  an instance of the  CIF data structure.  C-FUNCTION-POINTER  must be a
  ;;pointer object referencing the foreign function.
  ;;
  (let ((user-data (cons (cif-cif S) c-function-pointer)))
    (lambda args	;this is the callout function
      (let ((rv (%generic-callout-wrapper user-data S args)))
	(values rv (errno))))))


;;;; Libffi: callbacks

(define* (make-c-callback-maker {retval-type symbol?} {arg-types list-of-symbols?})
  ;;Given the symbol RETVAL-TYPE representing the type of the return value and a list
  ;;of symbols ARG-TYPES representing the types of the arguments: return a closure to
  ;;be used to generate Scheme callback pointers from Scheme functions.
  ;;
  (let ((S (%ffi-prep-cif __who__ retval-type arg-types)))
    (or (cif-callback-maker S)
	(receive-and-return (maker)
	    (lambda (proc)
	      (%callback-maker S proc))
	  (set-cif-callback-maker! S maker)))))

(define* (%callback-maker S {proc procedure?})
  ;;Worker  function  for Scheme  callback  maker  functions.   Return a  pointer  to
  ;;callable machine code.
  ;;
  ;;S  must be  an instance  of the  CIF  data structure.   PROC must  be the  Scheme
  ;;function to wrap.
  ;;
  (let* ((retval-pred	(cif-retval-checker S))
	 (retval-type (cif-retval-type    S))
	 (proc	(if (or (eq? retval-type 'void)
			(not arguments-validation))
		    (begin
		      proc) ;no return value to be validated
		  ;;This is a wrapper for a Scheme function that
		  ;;needs validation of the return value.
		  (lambda args
		    (let ((v (apply proc args)))
		      (if (retval-pred v)
			  v
			(assertion-violation 'callback
			  "returned value does not match specified type" retval-type v)))))))
    (or (capi.ffi-prepare-callback (cons (cif-cif S) proc))
	(assertion-violation __who__ "internal error building FFI callback"))))

(define* (free-c-callback {c-callback-pointer pointer?})
  (unless (capi.ffi-free-c-callback c-callback-pointer)
    (assertion-violation __who__
      "attempt to release unkwnown callback pointer" c-callback-pointer)))


;;;; done

(set! %memory-guardian (make-guardian))
(set-struct-type-printer!	(type-descriptor memory-block)	%struct-memory-block-printer)
(set-struct-type-destructor!	(type-descriptor memory-block)	%memory-block-destructor)
(post-gc-hooks (cons %free-allocated-memory (post-gc-hooks)))


;; (define end-of-file-dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.pointers end")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'with-pathnames 'scheme-indent-function 1)
;; eval: (put 'arguments-validation-forms 'scheme-indent-function 0)
;; End:
