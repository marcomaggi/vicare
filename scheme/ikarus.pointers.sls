;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
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
    pointer-clone			pointer-and-offset?
    pointer-diff			pointer-add
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?
    set-pointer-null!

    ;; memory blocks
    memory-block?			memory-block?/non-null
    make-memory-block/guarded		memory-block-size
    memory-block-reset			null-memory-block
    (rename (%make-memory-block		make-memory-block)
	    (%memory-block-pointer	memory-block-pointer)
	    (memory-block?/non-null	memory-block?/not-null))

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
    malloc*				guarded-malloc*
    realloc*				guarded-realloc*
    calloc*				guarded-calloc*
    free				memcmp
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
    strdup				strdup*
    strndup				strndup*
    guarded-strdup			guarded-strdup*
    guarded-strndup			guarded-strndup*
    cstring->bytevector			cstring->string
    cstring16->bytevector		cstring16n->string
    cstring16le->string			cstring16be->string
    bytevector->cstring			bytevector->cstring*
    bytevector->guarded-cstring		bytevector->guarded-cstring*
    string->cstring			string->cstring*
    string->guarded-cstring		string->guarded-cstring*

    ;; C array of C strings
    argv-length
    argv->bytevectors			argv->strings
    bytevectors->argv			bytevectors->argv*
    bytevectors->guarded-argv		bytevectors->guarded-argv*
    strings->argv			strings->argv*
    strings->guarded-argv		strings->guarded-argv*

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
    pointer-ref-c-pointer

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
    pointer-set-c-pointer!

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
    array-ref-c-pointer

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
    array-set-c-pointer!

    array-set-c-size_t!			array-set-c-ssize_t!
    array-set-c-off_t!			array-set-c-ptrdiff_t!)
  (import (except (ikarus)
		  ;; pointer objects
		  pointer?
		  null-pointer				pointer-null?
		  pointer->integer			integer->pointer
		  pointer-clone				pointer-and-offset?
		  pointer-diff				pointer-add
		  pointer=?				pointer<>?
		  pointer<?				pointer>?
		  pointer<=?				pointer>=?
		  set-pointer-null!

		  ;; memory blocks
		  make-memory-block			make-memory-block/guarded
		  memory-block?
		  memory-block?/non-null		memory-block?/not-null
		  memory-block-pointer			memory-block-size
		  memory-block-reset			null-memory-block

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
		  malloc*				guarded-malloc*
		  realloc*				guarded-realloc*
		  calloc*				guarded-calloc*
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
		  strdup				strdup*
		  strndup				strndup*
		  guarded-strdup			guarded-strdup*
		  guarded-strndup			guarded-strndup*
		  cstring->bytevector			cstring->string
		  cstring16->bytevector			cstring16n->string
		  cstring16le->string			cstring16be->string
		  bytevector->cstring			bytevector->cstring*
		  bytevector->guarded-cstring		bytevector->guarded-cstring*
		  string->cstring			string->cstring*
		  string->guarded-cstring		string->guarded-cstring*

		  ;; C array of C strings
		  argv-length
		  argv->bytevectors			argv->strings
		  bytevectors->argv			bytevectors->argv*
		  bytevectors->guarded-argv		bytevectors->guarded-argv*
		  strings->argv				strings->argv*
		  strings->guarded-argv			strings->guarded-argv*

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
		  pointer-ref-c-pointer

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
		  pointer-set-c-pointer!

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
		  array-ref-c-pointer

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
		  array-set-c-pointer!

		  array-set-c-size_t!			array-set-c-ssize_t!
		  array-set-c-off_t!			array-set-c-ptrdiff_t!)
    (only (ikarus system $pointers)
	  $pointer=)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare words)
	    words.))

  (module (arguments-validation)
    (import (vicare include))
    (include/verbose "ikarus.config.ss"))


;;;; arguments validation

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (symbol who obj)
  (symbol? obj)
  (assertion-violation who "expected symbol as argument" obj))

(define-argument-validation (list who obj)
  (list? obj)
  (assertion-violation who "expected list as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (flonum who obj)
  (flonum? obj)
  (assertion-violation who "expected flonum as argument" obj))

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (exact-integer who obj)
  (or (fixnum? obj) (bignum? obj))
  (assertion-violation who "expected exact integer as argument" obj))

(define-argument-validation (non-negative-exact-integer who obj)
  (or (and (fixnum? obj) (unsafe.fx<= 0 obj))
      (and (bignum? obj) (<= 0 obj)))
  (assertion-violation who "expected non-negative exact integer as argument" obj))

(define-argument-validation (null/list-of-symbols who obj)
  (or (null? obj) (and (list? obj) (for-all symbol? obj)))
  (assertion-violation who "expected list of symbols as argument" obj))

(define-argument-validation (list-of-bytevectors who obj)
  (and (list? obj) (for-all bytevector? obj))
  (assertion-violation who "expected list of bytevectors as argument" obj))

(define-argument-validation (list-of-strings who obj)
  (and (list? obj) (for-all string? obj))
  (assertion-violation who "expected list of strings as argument" obj))

(define-argument-validation (vector-of-lengths who obj)
  (and (vector? obj)
       (vector-for-all (lambda (obj)
			 (and (fixnum? obj) (unsafe.fx<= 0 obj)))
	 obj))
  (assertion-violation who "expected list of non-negative fixnums as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (memory-block who obj)
  (memory-block? obj)
  (assertion-violation who "expected instance of memory-block as argument" obj))

(define-argument-validation (memory-block/non-null who obj)
  (memory-block?/non-null obj)
  (assertion-violation who
    "expected instance of memory-block referencing non-null as argument" obj))

(define-argument-validation (pointer/memory-block who obj)
  (or (pointer? obj) (memory-block? obj))
  (assertion-violation who
    "expected pointer or instance of memory-block as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pathname who obj)
  (or (bytevector? obj) (string? obj))
  (assertion-violation who "expected string or bytevector as pathname argument" obj))

(define-argument-validation (errno who obj)
  (or (boolean? obj) (and (fixnum? obj) (unsafe.fx<= obj 0)))
  (assertion-violation who "expected boolean or negative fixnum as errno argument" obj))

(define-argument-validation (machine-word who obj)
  (words.machine-word? obj)
  (assertion-violation who
    "expected non-negative exact integer in the range of a machine word as argument" obj))

(define-argument-validation (ptrdiff who obj)
  (words.ptrdiff_t? obj)
  (assertion-violation who
    "expected exact integer representing pointer difference as argument" obj))

(define-argument-validation (memory/ptrdiff who memory offset data-size)
  (or (pointer? memory)
      (<= (+ offset data-size) (memory-block-size memory)))
  (assertion-violation who
    "offset from pointer out of range for data size"
    memory offset data-size))

(define-argument-validation (memory/index who memory index data-size)
  (or (pointer? memory)
      (<= (* index data-size) (memory-block-size memory)))
  (assertion-violation who
    "offset from pointer out of range for data size"
    memory index data-size))

(define-argument-validation (pointer-and-offset who pointer offset)
  (%pointer-and-offset? pointer offset)
  (assertion-violation who
    "offset would cause pointer overflow or underflow"
    pointer offset))

(define-argument-validation (size_t-number-of-bytes who obj)
  (words.size_t? obj)
  (assertion-violation who "expected size_t as number of bytes argument" obj))

(define-argument-validation (fixnum-number-of-bytes who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who
    "expected non-negative fixnum as number of bytes argument" obj))

(define-argument-validation (number-of-elements who obj)
  (words.size_t? obj)
  (assertion-violation who "expected size_t as number of elements argument" obj))

(define-argument-validation (byte who obj)
  (or (words.word-u8? obj)
      (words.word-s8? obj))
  (assertion-violation who
    "expected exact integer representing an 8-bit signed or unsigned integer as argument" obj))

(define-argument-validation (pointer-offset who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as pointer offset argument" obj))

(define-argument-validation (start-index-for-bytevector who idx bv)
  ;;To be used after  START-INDEX validation.  Valid scenarios for start
  ;;indexes:
  ;;
  ;;  |...|word
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                      ^start
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                  ^start
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                      ^start = bv.len
  ;;
  ;;  | empty bytevector
  ;;  ^start = bv.len = 0
  ;;
  ;;the following is an invalid scenario:
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                    ^start = bv.len
  ;;
  (let ((bv.len (unsafe.bytevector-length bv)))
    (or (unsafe.fx=  idx bv.len)
	(unsafe.fx<= idx bv.len)))
  (assertion-violation who
    (string-append "start index argument "		(number->string idx)
		   " too big for bytevector length "	(number->string (unsafe.bytevector-length bv)))
    idx))

(define-argument-validation (count-for-bytevector who count bv bv.start)
  (let ((end (unsafe.fx+ bv.start count)))
    (unsafe.fx<= end (unsafe.bytevector-length bv)))
  (assertion-violation who
    (string-append "word count "			(number->string count)
		   " too big for bytevector length "	(number->string (unsafe.bytevector-length bv))
		   " start index "			(number->string bv.start))
    count))

;;; --------------------------------------------------------------------

(define-argument-validation (uint8 who obj)
  (words.word-u8? obj)
  (assertion-violation who
    "expected exact integer representing an 8-bit unsigned integer as argument" obj))

(define-argument-validation (sint8 who obj)
  (words.word-s8? obj)
  (assertion-violation who
    "expected exact integer representing an 8-bit signed integer as argument" obj))

(define-argument-validation (uint16 who obj)
  (words.word-u16? obj)
  (assertion-violation who
    "expected exact integer representing an 16-bit unsigned integer as argument" obj))

(define-argument-validation (sint16 who obj)
  (words.word-s16? obj)
  (assertion-violation who
    "expected exact integer representing an 16-bit signed integer as argument" obj))

(define-argument-validation (uint32 who obj)
  (words.word-u32? obj)
  (assertion-violation who
    "expected exact integer representing an 32-bit unsigned integer as argument" obj))

(define-argument-validation (sint32 who obj)
  (words.word-s32? obj)
  (assertion-violation who
    "expected exact integer representing an 32-bit signed integer as argument" obj))

(define-argument-validation (uint64 who obj)
  (words.word-u64? obj)
  (assertion-violation who
    "expected exact integer representing an 64-bit unsigned integer as argument" obj))

(define-argument-validation (sint64 who obj)
  (words.word-s64? obj)
  (assertion-violation who
    "expected exact integer representing an 64-bit signed integer as argument" obj))

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

(define-argument-validation (size_t who obj)
  (words.size_t? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"size_t\" as argument" obj))

(define-argument-validation (ssize_t who obj)
  (words.ssize_t? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"ssize_t\" as argument" obj))

(define-argument-validation (off_t who obj)
  (words.off_t? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"off_t\" as argument" obj))

(define-argument-validation (ptrdiff_t who obj)
  (words.ptrdiff_t? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"ptrdiff_t\" as argument" obj))


;;;; errno interface

(define errno
  (case-lambda
   (()
    (foreign-call "ikrt_last_errno"))
   ((errno)
    (define who 'errno)
    (with-arguments-validation (who)
	((errno  errno))
      (foreign-call "ikrt_set_errno" errno)))))


;;;; memory blocks

(define-struct memory-block
  (pointer
		;Pointer object referencing a block of raw memory.
   size
		;Exact integer  representing the number of  bytes in the
		;memory block.  It must be in  the range of a C language
		;type "size_t".
   owner?
		;Boolean, if true  the block of memory  must be released
		;whenever this structure instance is garbage collected.
   ))

(define (%struct-memory-block-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[memory-block")
  (%display " pointer=")	(%display ($memory-block-pointer S))
  (%display " size=")		(%display ($memory-block-size    S))
  (%display " owner?=")		(%display ($memory-block-owner?  S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define (%unsafe.memory-block-destructor S)
  (when ($memory-block-owner? S)
    ;;Remember that FREE will mutate to NULL the pointer.
    (capi.ffi-free ($memory-block-pointer S))
    ($set-memory-block-pointer! S (void))
    ($set-memory-block-size!    S (void))))

;;; --------------------------------------------------------------------

(define (%make-memory-block pointer size)
  ;;Wrapper for  the constructor  MAKE-MEMORY-BLOCK which  validates the
  ;;arguments.
  ;;
  (define who 'make-memory-block)
  (with-arguments-validation (who)
      ((pointer	pointer)
       (size_t	size))
    (make-memory-block pointer size #f)))

(define (null-memory-block)
  (make-memory-block (null-pointer) 0 #f))

(define (make-memory-block/guarded pointer size)
  (define who 'make-memory-block/guarded)
  (with-arguments-validation (who)
      ((pointer	pointer)
       (size_t	size))
    (make-memory-block pointer size #t)))

(define (memory-block?/non-null obj)
  (and (memory-block? obj)
       (not (pointer-null? ($memory-block-pointer obj)))))

(define (%memory-block-pointer obj)
  (pointer-clone (memory-block-pointer obj)))

(define (memory-block-reset B)
  (define who 'memory-block-reset)
  (with-arguments-validation (who)
      ((memory-block	B))
    (%unsafe.memory-block-destructor B)
    ($set-memory-block-pointer! B (null-pointer))
    ($set-memory-block-size!    B 0)
    ($set-memory-block-owner?!  B #f)))


;;; shared libraries interface

(define (dlerror)
  (let ((p (capi.ffi-dlerror)))
    (and p (ascii->string p))))

(define dlopen
  (case-lambda
   (()
    (capi.ffi-dlopen #f #f #f))
   ((libname)
    (dlopen libname #f #f))
   ((libname lazy? global?)
    (define who 'dlopen)
    (with-arguments-validation (who)
	((pathname  libname))
      (with-pathnames ((libname.bv libname))
	(capi.ffi-dlopen libname.bv lazy? global?))))))

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
    (capi.ffi-dlsym handle (string->ascii name))))


;;; pointer manipulation procedures

(define (pointer? obj)
  ;;FIXME Why  in hell do I have  to keep this function  rather than use
  ;;the  $FIXNUM?   primitive   operation  exported  by  (ikarus  system
  ;;$pointers)? (Marco Maggi; Nov 30, 2011)
  ;;
  (capi.ffi-pointer? obj))

(define (null-pointer)
  (capi.ffi-fixnum->pointer 0))

(define (pointer-null? obj)
  (and (pointer? obj) (capi.ffi-pointer-null? obj)))

(define (set-pointer-null! ptr)
  (define who 'set-pointer-null!)
  (with-arguments-validation (who)
      ((pointer ptr))
    (capi.ffi-set-pointer-null! ptr)))

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

(define (pointer-clone obj)
  (define who 'pointer-clone)
  (with-arguments-validation (who)
      ((pointer	obj))
    (capi.ffi-pointer-clone obj)))

;;; --------------------------------------------------------------------

(define (pointer-and-offset? pointer offset)
  (define who 'pointer-and-offset?)
  (with-arguments-validation (who)
      ((pointer		pointer)
       (exact-integer	offset))
    (%pointer-and-offset? pointer offset)))

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

(define (pointer-add ptr delta)
  (define who 'pointer-add)
  (with-arguments-validation (who)
      ((pointer			ptr)
       (exact-integer		delta)
       (pointer-and-offset	ptr delta))
    (let ((rv (capi.ffi-pointer-add ptr delta)))
      (or rv
	  (assertion-violation who
	    "requested pointer arithmetic operation would cause \
             machine word overflow or underflow"
	    ptr delta)))))

(define (pointer-diff ptr1 ptr2)
  (define who 'pointer-diff)
  (with-arguments-validation (who)
      ((pointer  ptr1)
       (pointer  ptr2))
    ;;Implemented  at the  Scheme level  because converting  pointers to
    ;;Scheme exact  integer objects  is the simplest  and safest  way to
    ;;correctly handle the full range of possible pointer values.
    (- (capi.ffi-pointer->integer ptr1)
       (capi.ffi-pointer->integer ptr2))))

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
  (define-pointer-comparison pointer<>?		capi.ffi-pointer-neq)
  (define-pointer-comparison pointer<?		capi.ffi-pointer-lt)
  (define-pointer-comparison pointer>?		capi.ffi-pointer-gt)
  (define-pointer-comparison pointer<=?		capi.ffi-pointer-le)
  (define-pointer-comparison pointer>=?		capi.ffi-pointer-ge))


;;;; pointer accessors

(let-syntax
    ((define-accessor (syntax-rules ()
			((_ ?who ?accessor ?data-size)
			 (define (?who memory offset)
			   (define who '?who)
			   (with-arguments-validation (who)
			       ((pointer/memory-block	memory)
				(ptrdiff		offset)
				(memory/ptrdiff		memory offset ?data-size))
			     (?accessor memory offset)))))))
  (define-accessor pointer-ref-c-uint8
    capi.ffi-pointer-ref-c-uint8 1)
  (define-accessor pointer-ref-c-sint8
    capi.ffi-pointer-ref-c-sint8 1)
  (define-accessor pointer-ref-c-uint16
    capi.ffi-pointer-ref-c-uint16 2)
  (define-accessor pointer-ref-c-sint16
    capi.ffi-pointer-ref-c-sint16 2)
  (define-accessor pointer-ref-c-uint32
    capi.ffi-pointer-ref-c-uint32 4)
  (define-accessor pointer-ref-c-sint32
    capi.ffi-pointer-ref-c-sint32 4)
  (define-accessor pointer-ref-c-uint64
    capi.ffi-pointer-ref-c-uint64 8)
  (define-accessor pointer-ref-c-sint64
    capi.ffi-pointer-ref-c-sint64 8)

  (define-accessor pointer-ref-c-float
    capi.ffi-pointer-ref-c-float
    words.SIZEOF_FLOAT)
  (define-accessor pointer-ref-c-double
    capi.ffi-pointer-ref-c-double
    words.SIZEOF_DOUBLE)
  (define-accessor pointer-ref-c-pointer
    capi.ffi-pointer-ref-c-pointer
    words.SIZEOF_POINTER)

  (define-accessor pointer-ref-c-signed-char
    capi.ffi-pointer-ref-c-signed-char
    words.SIZEOF_CHAR)
  (define-accessor pointer-ref-c-signed-short
    capi.ffi-pointer-ref-c-signed-short
    words.SIZEOF_SHORT)
  (define-accessor pointer-ref-c-signed-int
    capi.ffi-pointer-ref-c-signed-int
    words.SIZEOF_INT)
  (define-accessor pointer-ref-c-signed-long
    capi.ffi-pointer-ref-c-signed-long
    words.SIZEOF_LONG)
  (define-accessor pointer-ref-c-signed-long-long
    capi.ffi-pointer-ref-c-signed-long-long
    words.SIZEOF_LONG_LONG)

  (define-accessor pointer-ref-c-unsigned-char
    capi.ffi-pointer-ref-c-unsigned-char
    words.SIZEOF_CHAR)
  (define-accessor pointer-ref-c-unsigned-short
    capi.ffi-pointer-ref-c-unsigned-short
    words.SIZEOF_SHORT)
  (define-accessor pointer-ref-c-unsigned-int
    capi.ffi-pointer-ref-c-unsigned-int
    words.SIZEOF_INT)
  (define-accessor pointer-ref-c-unsigned-long
    capi.ffi-pointer-ref-c-unsigned-long
    words.SIZEOF_LONG)
  (define-accessor pointer-ref-c-unsigned-long-long
    capi.ffi-pointer-ref-c-unsigned-long-long
    words.SIZEOF_LONG_LONG)

  (define-accessor pointer-ref-c-size_t
    capi.ffi-pointer-ref-c-size_t
    words.SIZEOF_SIZE_T)
  (define-accessor pointer-ref-c-ssize_t
    capi.ffi-pointer-ref-c-ssize_t
    words.SIZEOF_SSIZE_T)
  (define-accessor pointer-ref-c-off_t
    capi.ffi-pointer-ref-c-off_t
    words.SIZEOF_OFF_T)
  (define-accessor pointer-ref-c-ptrdiff_t
    capi.ffi-pointer-ref-c-ptrdiff_t
    words.SIZEOF_PTRDIFF_T)
  )


;;;; pointer mutators

(let-syntax
    ((define-mutator (syntax-rules ()
		       ((_ ?who ?mutator ?word-type ?data-size)
			(define (?who memory offset value)
			  (define who '?who)
			  (with-arguments-validation (who)
			      ((pointer/memory-block	memory)
			       (ptrdiff			offset)
			       (memory/ptrdiff		memory offset ?data-size)
			       (?word-type		value))
			    (?mutator memory offset value)))))))
  (define-mutator pointer-set-c-uint8!
    capi.ffi-pointer-set-c-uint8!
    uint8 1)
  (define-mutator pointer-set-c-sint8!
    capi.ffi-pointer-set-c-sint8!
    sint8 1)
  (define-mutator pointer-set-c-uint16!
    capi.ffi-pointer-set-c-uint16!
    uint16 2)
  (define-mutator pointer-set-c-sint16!
    capi.ffi-pointer-set-c-sint16!
    sint16 2)
  (define-mutator pointer-set-c-uint32!
    capi.ffi-pointer-set-c-uint32!
    uint32 4)
  (define-mutator pointer-set-c-sint32!
    capi.ffi-pointer-set-c-sint32!
    sint32 4)
  (define-mutator pointer-set-c-uint64!
    capi.ffi-pointer-set-c-uint64!
    uint64 8)
  (define-mutator pointer-set-c-sint64!
    capi.ffi-pointer-set-c-sint64!
    sint64 8)

  (define-mutator pointer-set-c-float!
    capi.ffi-pointer-set-c-float!
    flonum
    words.SIZEOF_FLOAT)
  (define-mutator pointer-set-c-double!
    capi.ffi-pointer-set-c-double!
    flonum
    words.SIZEOF_DOUBLE)
  (define-mutator pointer-set-c-pointer!
    capi.ffi-pointer-set-c-pointer!
    pointer
    words.SIZEOF_POINTER)

  (define-mutator pointer-set-c-signed-char!
    capi.ffi-pointer-set-c-signed-char!
    signed-char
    words.SIZEOF_CHAR)
  (define-mutator pointer-set-c-signed-short!
    capi.ffi-pointer-set-c-signed-short!
    signed-short
    words.SIZEOF_SHORT)
  (define-mutator pointer-set-c-signed-int!
    capi.ffi-pointer-set-c-signed-int!
    signed-int
    words.SIZEOF_INT)
  (define-mutator pointer-set-c-signed-long!
    capi.ffi-pointer-set-c-signed-long!
    signed-long
    words.SIZEOF_LONG)
  (define-mutator pointer-set-c-signed-long-long!
    capi.ffi-pointer-set-c-signed-long-long!
    signed-long-long
    words.SIZEOF_LONG)

  (define-mutator pointer-set-c-unsigned-char!
    capi.ffi-pointer-set-c-unsigned-char!
    unsigned-char
    words.SIZEOF_CHAR)
  (define-mutator pointer-set-c-unsigned-short!
    capi.ffi-pointer-set-c-unsigned-short!
    unsigned-short
    words.SIZEOF_SHORT)
  (define-mutator pointer-set-c-unsigned-int!
    capi.ffi-pointer-set-c-unsigned-int!
    unsigned-int
    words.SIZEOF_INT)
  (define-mutator pointer-set-c-unsigned-long!
    capi.ffi-pointer-set-c-unsigned-long!
    unsigned-long
    words.SIZEOF_LONG)
  (define-mutator pointer-set-c-unsigned-long-long!
    capi.ffi-pointer-set-c-unsigned-long-long!
    unsigned-long-long
    words.SIZEOF_LONG_LONG)

  (define-mutator pointer-set-c-size_t!
    capi.ffi-pointer-set-c-size_t!
    size_t
    words.SIZEOF_SIZE_T)
  (define-mutator pointer-set-c-ssize_t!
    capi.ffi-pointer-set-c-ssize_t!
    ssize_t
    words.SIZEOF_SSIZE_T)
  (define-mutator pointer-set-c-off_t!
    capi.ffi-pointer-set-c-off_t!
    off_t
    words.SIZEOF_OFF_T)
  (define-mutator pointer-set-c-ptrdiff_t!
    capi.ffi-pointer-set-c-ptrdiff_t!
    ptrdiff_t
    words.SIZEOF_PTRDIFF_T)

  )


;;;; array accessors

(let-syntax
    ((define-accessor (syntax-rules ()
			((_ ?who ?accessor ?data-size)
			 (define (?who memory offset)
			   (define who '?who)
			   (with-arguments-validation (who)
			       ((pointer/memory-block	memory)
				(ptrdiff		offset)
				(memory/index		memory offset ?data-size))
			     (?accessor memory offset)))))))
  (define-accessor array-ref-c-uint8
    capi.ffi-array-ref-c-uint8 1)
  (define-accessor array-ref-c-sint8
    capi.ffi-array-ref-c-sint8 1)
  (define-accessor array-ref-c-uint16
    capi.ffi-array-ref-c-uint16 2)
  (define-accessor array-ref-c-sint16
    capi.ffi-array-ref-c-sint16 2)
  (define-accessor array-ref-c-uint32
    capi.ffi-array-ref-c-uint32 4)
  (define-accessor array-ref-c-sint32
    capi.ffi-array-ref-c-sint32 4)
  (define-accessor array-ref-c-uint64
    capi.ffi-array-ref-c-uint64 8)
  (define-accessor array-ref-c-sint64
    capi.ffi-array-ref-c-sint64 8)

  (define-accessor array-ref-c-float
    capi.ffi-array-ref-c-float
    words.SIZEOF_FLOAT)
  (define-accessor array-ref-c-double
    capi.ffi-array-ref-c-double
    words.SIZEOF_DOUBLE)
  (define-accessor array-ref-c-pointer
    capi.ffi-array-ref-c-pointer
    words.SIZEOF_POINTER)

  (define-accessor array-ref-c-signed-char
    capi.ffi-array-ref-c-signed-char
    words.SIZEOF_CHAR)
  (define-accessor array-ref-c-signed-short
    capi.ffi-array-ref-c-signed-short
    words.SIZEOF_SHORT)
  (define-accessor array-ref-c-signed-int
    capi.ffi-array-ref-c-signed-int
    words.SIZEOF_INT)
  (define-accessor array-ref-c-signed-long
    capi.ffi-array-ref-c-signed-long
    words.SIZEOF_LONG)
  (define-accessor array-ref-c-signed-long-long
    capi.ffi-array-ref-c-signed-long-long
    words.SIZEOF_LONG_LONG)

  (define-accessor array-ref-c-unsigned-char
    capi.ffi-array-ref-c-unsigned-char
    words.SIZEOF_CHAR)
  (define-accessor array-ref-c-unsigned-short
    capi.ffi-array-ref-c-unsigned-short
    words.SIZEOF_SHORT)
  (define-accessor array-ref-c-unsigned-int
    capi.ffi-array-ref-c-unsigned-int
    words.SIZEOF_INT)
  (define-accessor array-ref-c-unsigned-long
    capi.ffi-array-ref-c-unsigned-long
    words.SIZEOF_LONG)
  (define-accessor array-ref-c-unsigned-long-long
    capi.ffi-array-ref-c-unsigned-long-long
    words.SIZEOF_LONG_LONG)

  (define-accessor array-ref-c-size_t
    capi.ffi-array-ref-c-size_t
    words.SIZEOF_SIZE_T)
  (define-accessor array-ref-c-ssize_t
    capi.ffi-array-ref-c-ssize_t
    words.SIZEOF_SSIZE_T)
  (define-accessor array-ref-c-off_t
    capi.ffi-array-ref-c-off_t
    words.SIZEOF_OFF_T)
  (define-accessor array-ref-c-ptrdiff_t
    capi.ffi-array-ref-c-ptrdiff_t
    words.SIZEOF_PTRDIFF_T)

  )


;;;; array mutators

(let-syntax
    ((define-mutator (syntax-rules ()
		       ((_ ?who ?mutator ?word-type ?data-size)
			(define (?who memory offset value)
			  (define who '?who)
			  (with-arguments-validation (who)
			      ((pointer/memory-block	memory)
			       (ptrdiff			offset)
			       (memory/index		memory offset ?data-size)
			       (?word-type		value))
			    (?mutator memory offset value)))))))
  (define-mutator array-set-c-uint8!
    capi.ffi-array-set-c-uint8!
    uint8 1)
  (define-mutator array-set-c-sint8!
    capi.ffi-array-set-c-sint8!
    sint8 1)
  (define-mutator array-set-c-uint16!
    capi.ffi-array-set-c-uint16!
    uint16 2)
  (define-mutator array-set-c-sint16!
    capi.ffi-array-set-c-sint16!
    sint16 2)
  (define-mutator array-set-c-uint32!
    capi.ffi-array-set-c-uint32!
    uint32 4)
  (define-mutator array-set-c-sint32!
    capi.ffi-array-set-c-sint32!
    sint32 4)
  (define-mutator array-set-c-uint64!
    capi.ffi-array-set-c-uint64!
    uint64 8)
  (define-mutator array-set-c-sint64!
    capi.ffi-array-set-c-sint64!
    sint64 8)

  (define-mutator array-set-c-float!
    capi.ffi-array-set-c-float!
    flonum
    words.SIZEOF_FLOAT)
  (define-mutator array-set-c-double!
    capi.ffi-array-set-c-double!
    flonum
    words.SIZEOF_DOUBLE)
  (define-mutator array-set-c-pointer!
    capi.ffi-array-set-c-pointer!
    pointer
    words.SIZEOF_POINTER)

  (define-mutator array-set-c-signed-char!
    capi.ffi-array-set-c-signed-char!
    signed-char
    words.SIZEOF_CHAR)
  (define-mutator array-set-c-signed-short!
    capi.ffi-array-set-c-signed-short!
    signed-short
    words.SIZEOF_SHORT)
  (define-mutator array-set-c-signed-int!
    capi.ffi-array-set-c-signed-int!
    signed-int
    words.SIZEOF_INT)
  (define-mutator array-set-c-signed-long!
    capi.ffi-array-set-c-signed-long!
    signed-long
    words.SIZEOF_LONG)
  (define-mutator array-set-c-signed-long-long!
    capi.ffi-array-set-c-signed-long-long!
    signed-long-long
    words.SIZEOF_LONG)

  (define-mutator array-set-c-unsigned-char!
    capi.ffi-array-set-c-unsigned-char!
    unsigned-char
    words.SIZEOF_CHAR)
  (define-mutator array-set-c-unsigned-short!
    capi.ffi-array-set-c-unsigned-short!
    unsigned-short
    words.SIZEOF_SHORT)
  (define-mutator array-set-c-unsigned-int!
    capi.ffi-array-set-c-unsigned-int!
    unsigned-int
    words.SIZEOF_INT)
  (define-mutator array-set-c-unsigned-long!
    capi.ffi-array-set-c-unsigned-long!
    unsigned-long
    words.SIZEOF_LONG)
  (define-mutator array-set-c-unsigned-long-long!
    capi.ffi-array-set-c-unsigned-long-long!
    unsigned-long-long
    words.SIZEOF_LONG_LONG)

  (define-mutator array-set-c-size_t!
    capi.ffi-array-set-c-size_t!
    size_t
    words.SIZEOF_SIZE_T)
  (define-mutator array-set-c-ssize_t!
    capi.ffi-array-set-c-ssize_t!
    ssize_t
    words.SIZEOF_SSIZE_T)
  (define-mutator array-set-c-off_t!
    capi.ffi-array-set-c-off_t!
    off_t
    words.SIZEOF_OFF_T)
  (define-mutator array-set-c-ptrdiff_t!
    capi.ffi-array-set-c-ptrdiff_t!
    ptrdiff_t
    words.SIZEOF_PTRDIFF_T)

  )


;;; raw memory management

(define-condition-type &out-of-memory-error &error
  make-out-of-memory-error out-of-memory-error?)

(define &out-of-memory-error-rtd
  (record-type-descriptor &out-of-memory-error))

(define &out-of-memory-error-rcd
  (record-constructor-descriptor &out-of-memory-error))

(define (%raise-out-of-memory who)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition "failed raw memory allocation")
	      (make-out-of-memory-error))))

;;; --------------------------------------------------------------------

(define (malloc number-of-bytes)
  (define who 'malloc)
  (with-arguments-validation (who)
      ((size_t-number-of-bytes	 number-of-bytes))
    (capi.ffi-malloc number-of-bytes)))

(define (malloc* number-of-bytes)
  (or (malloc number-of-bytes)
      (%raise-out-of-memory 'malloc*)))

(define (realloc memory number-of-bytes)
  (define who 'realloc)
  (with-arguments-validation (who)
      ((pointer/memory-block	memory)
       (size_t-number-of-bytes	number-of-bytes))
    ;;Take  care at  the C  level not  to realloc  NULL pointers  and of
    ;;mutating POINTER  to NULL.   If MEMORY  is a  MEMORY-BLOCK: update
    ;;both the pointer and size fields.
    (capi.ffi-realloc memory number-of-bytes)))

(define (realloc* pointer number-of-bytes)
  (or (realloc pointer number-of-bytes)
      (%raise-out-of-memory 'realloc*)))

(define (calloc number-of-elements element-size)
  (define who 'calloc)
  (with-arguments-validation (who)
      ((number-of-elements	number-of-elements)
       (size_t-number-of-bytes	element-size))
    (capi.ffi-calloc number-of-elements element-size)))

(define (calloc* number-of-elements element-size)
  (or (calloc number-of-elements element-size)
      (%raise-out-of-memory 'calloc*)))

(define (free obj)
  (define who 'free)
  (with-arguments-validation (who)
      ((pointer/memory-block	obj))
    ;;Take care  at the  C level  not to "free()"  null pointers  and of
    ;;mutating PTR to NULL.  Also if OBJ is a MEMORY-BLOCK: set the size
    ;;to zero.
    (capi.ffi-free obj)))

;;; --------------------------------------------------------------------

(define (memory-copy dst dst.start src src.start count)
  (define who 'memory-copy)
  (with-arguments-validation (who)
      ((pointer-offset	dst.start)
       (pointer-offset	src.start))
    (cond ((pointer? dst)
	   (cond ((pointer? src)
		  (capi.ffi-memcpy (pointer-add dst dst.start)
				   (pointer-add src src.start)
				   count))
		 ((bytevector? src)
		  (with-arguments-validation (who)
		      ((start-index-for-bytevector	src.start src)
		       (count-for-bytevector		count src src.start))
		    (foreign-call "ikrt_memcpy_from_bv" (pointer-add dst dst.start) src src.start count)))
		 (else
		  (assertion-violation who "expected pointer or bytevector as source argument" src))))
	  ((bytevector? dst)
	   (with-arguments-validation (who)
	       ((start-index-for-bytevector	dst.start dst)
		(count-for-bytevector		count dst dst.start))
	     (cond ((pointer? src)
		    (foreign-call "ikrt_memcpy_to_bv" dst dst.start (pointer-add src src.start) count))
		   ((bytevector? src)
		    (with-arguments-validation (who)
			((start-index-for-bytevector	src.start src)
			 (count-for-bytevector		count src src.start))
		      (unsafe.bytevector-copy!/count src src.start dst dst.start count)))
		   (else
		    (assertion-violation who "expected pointer or bytevector as source argument" src)))))
	  (else
	   (assertion-violation who "expected pointer or bytevector as destination argument" dst)))))

;;; --------------------------------------------------------------------

(define (memcpy dst src count)
  (define who 'memcpy)
  (with-arguments-validation (who)
      ((pointer			dst)
       (pointer			src)
       (size_t-number-of-bytes	count))
    (capi.ffi-memcpy dst src count)))

(define (memmove dst src count)
  (define who 'memmove)
  (with-arguments-validation (who)
      ((pointer			dst)
       (pointer			src)
       (size_t-number-of-bytes	count))
    (capi.ffi-memmove dst src count)))

(define (memset ptr byte count)
  (define who 'memset)
  (with-arguments-validation (who)
      ((pointer			ptr)
       (byte			byte)
       (size_t-number-of-bytes	count))
    (capi.ffi-memset ptr byte count)))

(define (memcmp ptr1 ptr2 count)
  (define who 'memcp)
  (with-arguments-validation (who)
      ((pointer			ptr1)
       (pointer			ptr2)
       (size_t-number-of-bytes	count))
    (capi.ffi-memcmp ptr1 ptr2 count)))

;;; --------------------------------------------------------------------

(define (memory->bytevector pointer length)
  (define who 'memory->bytevector)
  (with-arguments-validation (who)
      ((pointer			pointer)
       (fixnum-number-of-bytes	length))
    (capi.ffi-memory->bytevector pointer length)))

(define (bytevector->memory bv)
  (define who 'bytevector->memory)
  (with-arguments-validation (who)
      ((bytevector	bv))
    (let ((rv (capi.ffi-bytevector->memory bv)))
      (if rv
	  (values rv (unsafe.bytevector-length bv))
	(values #f #f)))))

(define (bytevector->memory* bv)
  (define who 'bytevector->memory*)
  (with-arguments-validation (who)
      ((bytevector	bv))
    (let ((rv (capi.ffi-bytevector->memory bv)))
      (if rv
	  (values rv (unsafe.bytevector-length bv))
	(%raise-out-of-memory who)))))

;;; --------------------------------------------------------------------

(define %memory-guardian
  (make-guardian))

(define (%free-allocated-memory)
  (do ((pointer (%memory-guardian) (%memory-guardian)))
      ((not pointer))
    ;;We know that POINTER is a pointer object.
    (unless (capi.ffi-pointer-null? pointer)
      (capi.ffi-free pointer)
      (capi.ffi-set-pointer-null! pointer))))

(define (guarded-malloc number-of-bytes)
  (let ((rv (malloc number-of-bytes)))
    (and rv (%memory-guardian rv))))

(define (guarded-malloc* number-of-bytes)
  (or (guarded-malloc number-of-bytes)
      (%raise-out-of-memory 'guarded-malloc*)))

(define (guarded-realloc pointer number-of-bytes)
  (let ((rv (realloc pointer number-of-bytes)))
    (and rv (if (pointer? rv)
		(%memory-guardian rv)
	      rv))))

(define (guarded-realloc* pointer number-of-bytes)
  (or (guarded-realloc pointer number-of-bytes)
      (%raise-out-of-memory 'guarded-realloc*)))

(define (guarded-calloc number-of-elements element-size)
  (let ((rv (calloc number-of-elements element-size)))
    (and rv (%memory-guardian rv))))

(define (guarded-calloc* number-of-elements element-size)
  (or (guarded-calloc number-of-elements element-size)
      (%raise-out-of-memory 'guarded-calloc*)))

(define (bytevector->guarded-memory bv)
  (let-values (((ptr len) (bytevector->memory bv)))
    (if ptr
	(values (%memory-guardian ptr) len)
      (values #f #f))))

(define (bytevector->guarded-memory* bv)
  (define who 'bytevector->guarded-memory*)
  (let-values (((ptr len) (bytevector->memory bv)))
    (if ptr
	(values (%memory-guardian ptr) len)
      (%raise-out-of-memory who))))


;;;; C strings

(define (strlen pointer)
  (define who 'strlen)
  (with-arguments-validation (who)
      ((pointer pointer))
    (capi.ffi-strlen pointer)))

(define (strcmp pointer1 pointer2)
  (define who 'strcmp)
  (with-arguments-validation (who)
      ((pointer pointer1)
       (pointer pointer2))
    (capi.ffi-strcmp pointer1 pointer2)))

(define (strncmp pointer1 pointer2 count)
  (define who 'strncmp)
  (with-arguments-validation (who)
      ((pointer			pointer1)
       (pointer			pointer2)
       (size_t-number-of-bytes	count))
    (capi.ffi-strncmp pointer1 pointer2 count)))

(define (strdup pointer)
  (define who 'strdup)
  (with-arguments-validation (who)
      ((pointer pointer))
    (capi.ffi-strdup pointer)))

(define (strdup* pointer)
  (or (strdup pointer)
      (%raise-out-of-memory 'strdup*)))

(define (strndup pointer count)
  (define who 'strndup)
  (with-arguments-validation (who)
      ((pointer			pointer)
       (size_t-number-of-bytes	count))
    (capi.ffi-strndup pointer count)))

(define (strndup* pointer count)
  (or (strndup pointer count)
      (%raise-out-of-memory 'strndup)))

;;; --------------------------------------------------------------------

(define (bytevector->cstring bv)
  (define who 'bytevector->cstring)
  (with-arguments-validation (who)
      ((bytevector bv))
    (capi.ffi-bytevector->cstring bv)))

(define (bytevector->cstring* bv)
  (or (bytevector->cstring bv)
      (%raise-out-of-memory 'bytevector->cstring*)))

(define cstring->bytevector
  (case-lambda
   ((pointer)
    (define who 'cstring->bytevector)
    (with-arguments-validation (who)
	((pointer pointer))
      (capi.ffi-cstring->bytevector pointer (capi.ffi-strlen pointer))))
   ((pointer count)
    (define who 'cstring->bytevector)
    (with-arguments-validation (who)
	((pointer			pointer)
	 (fixnum-number-of-bytes	count))
      (capi.ffi-cstring->bytevector pointer count)))))

(define (cstring16->bytevector pointer)
  (define who 'cstring16->bytevector)
  (with-arguments-validation (who)
      ((pointer pointer))
    (capi.ffi-cstring16->bytevector pointer)))

;;; --------------------------------------------------------------------

(define cstring->string
  (case-lambda
   ((pointer)
    (define who 'cstring->string)
    (with-arguments-validation (who)
	((pointer pointer))
      (ascii->string (capi.ffi-cstring->bytevector pointer (capi.ffi-strlen pointer)))))
   ((pointer count)
    (define who 'cstring->string)
    (with-arguments-validation (who)
	((pointer			pointer)
	 (fixnum-number-of-bytes	count))
      (ascii->string (capi.ffi-cstring->bytevector pointer count))))))

(define (cstring16n->string pointer)
  (define who 'cstring16n->string)
  (with-arguments-validation (who)
      ((pointer pointer))
    (utf16n->string (capi.ffi-cstring16->bytevector pointer))))

(define (cstring16le->string pointer)
  (define who 'cstring16le->string)
  (with-arguments-validation (who)
      ((pointer pointer))
    (utf16le->string (capi.ffi-cstring16->bytevector pointer))))

(define (cstring16be->string pointer)
  (define who 'cstring16be->string)
  (with-arguments-validation (who)
      ((pointer pointer))
    (utf16be->string (capi.ffi-cstring16->bytevector pointer))))

(define (string->cstring str)
  (define who 'string->cstring)
  (with-arguments-validation (who)
      ((string	str))
    (bytevector->cstring (string->ascii str))))

(define (string->cstring* str)
  (or (string->cstring str)
      (%raise-out-of-memory 'string->cstring*)))

;;; --------------------------------------------------------------------

(define (bytevectors->argv bvs)
  (define who 'bytevectors->argv)
  (with-arguments-validation (who)
      ((list-of-bytevectors bvs))
    (capi.ffi-bytevectors->argv bvs)))

(define (bytevectors->argv* bvs)
  (or (bytevectors->argv bvs)
      (%raise-out-of-memory 'bytevectors->argv*)))

(define (argv->bytevectors pointer)
  (define who 'argv->bytevectors)
  (with-arguments-validation (who)
      ((pointer pointer))
    (capi.ffi-argv->bytevectors pointer)))

(define (strings->argv strs)
  (define who 'strings->argv)
  (with-arguments-validation (who)
      ((list-of-strings strs))
    (capi.ffi-bytevectors->argv (map string->ascii strs))))

(define (strings->argv* strs)
  (or (strings->argv strs)
      (%raise-out-of-memory 'strings->argv)))

(define (argv->strings pointer)
  (define who 'argv->strings)
  (with-arguments-validation (who)
      ((pointer pointer))
    (map ascii->string (capi.ffi-argv->bytevectors pointer))))

(define (argv-length pointer)
  (define who 'argv-length)
  (with-arguments-validation (who)
      ((pointer pointer))
    (capi.ffi-argv-length pointer)))

;;; --------------------------------------------------------------------

(define (guarded-strdup pointer)
  (let ((rv (strdup pointer)))
    (and rv (%memory-guardian rv))))

(define (guarded-strdup* pointer)
  (or (guarded-strdup pointer)
      (%raise-out-of-memory 'guarded-strdup*)))

(define (guarded-strndup pointer count)
  (let ((rv (strndup pointer count)))
    (and rv (%memory-guardian rv))))

(define (guarded-strndup* pointer count)
  (or (guarded-strndup pointer count)
      (%raise-out-of-memory 'guarded-strndup*)))

(define (bytevector->guarded-cstring bv)
  (let ((rv (bytevector->cstring bv)))
    (and rv (%memory-guardian rv))))

(define (bytevector->guarded-cstring* bv)
  (or (bytevector->guarded-cstring bv)
      (%raise-out-of-memory 'bytevector->guarded-cstring*)))

(define (string->guarded-cstring str)
  (let ((rv (bytevector->cstring (string->latin1 str))))
    (and rv (%memory-guardian rv))))

(define (string->guarded-cstring* str)
  (or (string->guarded-cstring str)
      (%raise-out-of-memory 'string->guarded-cstring*)))

(define (bytevectors->guarded-argv bvs)
  (let ((rv (bytevectors->argv bvs)))
    (and rv (%memory-guardian rv))))

(define (bytevectors->guarded-argv* bvs)
  (or (bytevectors->guarded-argv bvs)
      (%raise-out-of-memory 'bytevectors->guarded-argv*)))

(define (strings->guarded-argv bvs)
  (let ((rv (strings->argv bvs)))
    (and rv (%memory-guardian rv))))

(define (strings->guarded-argv* bvs)
  (or (strings->guarded-argv bvs)
      (%raise-out-of-memory 'strings->guarded-argv)))


;;;; local storage

(define (with-local-storage lengths proc)
  (define who 'with-local-storage)
  (with-arguments-validation (who)
      ((vector-of-lengths	lengths)
       (procedure		proc))
    (capi.ffi-with-local-storage lengths proc)))


;;;; Libffi: C API

(define-inline (capi.ffi-enabled?)
  (foreign-call "ikrt_has_ffi"))

(define-inline (capi.ffi-prep-cif type-ids)
  (foreign-call "ikrt_ffi_prep_cif" type-ids))

(define-inline (capi.ffi-callout user-data args)
  (foreign-call "ikrt_ffi_call" user-data args))

(define-inline (capi.ffi-prepare-callback cif.proc)
  (foreign-call "ikrt_ffi_prepare_callback" cif.proc))

(define-inline (capi.ffi-free-c-callback c-callback-pointer)
  (foreign-call "ikrt_ffi_release_callback" c-callback-pointer))


;;;; Libffi: native type identifiers

;;The  fixnums identifying  the  types must  be  kept in  sync with  the
;;definition of "type_id_t" in the file "ikarus-ffi.c".
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
     (assertion-violation #f "invalid FFI type specifier" type))))

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
     (assertion-violation #f "unknown FFI type specifier" type))))


;;; Libffi: call interfaces

(define (ffi-enabled?)
  (capi.ffi-enabled?))

;;Descriptor for callout and  callback generators associated to the same
;;function signature.  Once allocated,  instances of this type are never
;;released; rather they are cached in CIF-TABLE.
;;
(define-struct cif
  (cif			;Pointer   to  a   malloc-ed  C   language  data
			;structure  of type  "ffi_cif".   Once allocated
			;these structures are never released.
   callout-maker	;False   or  closure.   The   closure  generates
			;callout functions of given signature.
   callout-maker/with-errno
   callback-maker	;False   or  Closure.   The   closure  generates
			;callback functions of given signature
   arg-checkers		;vector of predicates used to validate arguments
   retval-checker	;predicate used to validate return value
   arg-types		;vector of symbols representing arg types
   retval-type		;symbol representing return value type
   ))

;;Maximum for the hash value of  signature vectors.  It is used to avoid
;;overflow of fixnums, allowing unsafe fx operations to be used.
;;
(define H_MAX
  (- (greatest-fixnum) TYPE_ID_MAX))

(define (%signature-hash signature)
  ;;Given a vector  of fixnums representing native types  for the return
  ;;value and  the arguments of a  callout or callback,  return a fixnum
  ;;hash value.
  ;;
  (let loop ((signature signature)
	     (len       (unsafe.vector-length signature))
	     (H		0)
	     (i         0))
    (cond ((unsafe.fx= i len)
	   H)
	  ((unsafe.fx< H_MAX H)
	   (assertion-violation '%signature-hash "FFI signature too big" signature))
	  (else
	   (loop signature len
		 (unsafe.fx+ H (unsafe.vector-ref signature i))
		 (unsafe.fxadd1 i))))))

(define (%unsafe.signature=? vec1 vec2)
  (let ((len1 (unsafe.vector-length vec1)))
    (and (unsafe.fx= len1 (unsafe.vector-length vec2))
	 (let loop ((i 0))
	   (or (unsafe.fx= i len1)
	       (and (unsafe.fx= (unsafe.vector-ref vec1 i)
				(unsafe.vector-ref vec2 i))
		    (loop (unsafe.fxadd1 i))))))))

;;Table of structures of type CIF, used to avoid generating duplicates.
;;
(define CIF-TABLE #f)

(define (%ffi-prep-cif who retval-type arg-types)
  ;;Return an instance of  CIF structure representing the call interface
  ;;for  callouts  and callbacks  of  the  given  signature.  If  a  CIF
  ;;structure for  such function  signature already exists:  retrieve it
  ;;from the hash table; else build a new structure.
  ;;
  ;;RETVAL-TYPE must  be a  symbol representing the  type of  the return
  ;;value.  ARG-TYPES must  be a list of symbols  representing the types
  ;;of the arguments.
  ;;
  (define who '%ffi-prep-cif)
  (with-arguments-validation (who)
      ((list	arg-types))
    (let* ((arg-types	(if (equal? '(void) arg-types) '() arg-types))
	   (signature	(vector-map %type-symbol->type-id
			  (list->vector (cons retval-type arg-types)))))
      (unless CIF-TABLE
	(set! CIF-TABLE (make-hashtable %signature-hash %unsafe.signature=?)))
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
	      (assertion-violation who "failed to initialize C interface" retval-type arg-types)
	    (assertion-violation who "FFI support is not enabled"))))))


;;;; Libffi: callouts

(define (make-c-callout-maker retval-type arg-types)
  ;;Given  the symbol RETVAL-TYPE  representing the  type of  the return
  ;;value and a list of  symbols ARG-TYPES representing the types of the
  ;;arguments: return  a closure to  be used to generate  Scheme callout
  ;;functions from pointers to C functions.
  ;;
  (define who 'make-c-callout-maker)
  (with-arguments-validation (who)
      ((symbol			retval-type)
       (null/list-of-symbols	arg-types))
    (let ((S (%ffi-prep-cif who retval-type arg-types)))
      (or (cif-callout-maker S)
	  (let ((maker (lambda (c-function-pointer)
			 (%callout-maker S c-function-pointer))))
	    (set-cif-callout-maker! S maker)
	    maker)))))

(define (%callout-maker S c-function-pointer)
  ;;Worker  function  for  Scheme  callout maker  functions.   Return  a
  ;;closure to be called to call a foreign function.
  ;;
  ;;S must be an instance of the CIF data structure.  C-FUNCTION-POINTER
  ;;must be a pointer object referencing the foreign function.
  ;;
  (define who '%callout-maker)
  (with-arguments-validation (who)
      ((pointer  c-function-pointer))
    (let ((user-data (cons (cif-cif S) c-function-pointer)))
      (lambda args	;this is the callout function
	(%generic-callout-wrapper user-data S args)))))

(define (%generic-callout-wrapper user-data S args)
  ;;Worker function for the wrapper of the actual foreign function: call
  ;;the foreign  function and return  its return value.   This functions
  ;;exists mostly to validate the input arguments.
  ;;
  ;;USER-DATA must be a pair whose car is a pointer object referencing a
  ;;Libffi's  CIF data  structure  and  whose cdr  is  a pointer  object
  ;;representing the address of  the foreign function to call.
  ;;
  ;;S must be an instance of the CIF data structure.
  ;;
  ;;ARGS is the list of arguments in the call.
  ;;
  (define who '%generic-callout-wrapper)
  (let ((args (list->vector args)))
    (arguments-validation-forms
      (let ((types     (cif-arg-types    S))
	    (checkers  (cif-arg-checkers S)))
	(unless (unsafe.fx= (unsafe.vector-length args)
			    (unsafe.vector-length types))
	  (assertion-violation who "wrong number of arguments" types args))
	(when checkers
	  (vector-for-each (lambda (arg-pred type arg)
			     (unless (arg-pred arg)
			       (assertion-violation who
				 "argument does not match specified type" type arg)))
	    checkers types args))))
    (capi.ffi-callout user-data args)))

;;; --------------------------------------------------------------------

(define (make-c-callout-maker/with-errno retval-type arg-types)
  ;;Given  the symbol RETVAL-TYPE  representing the  type of  the return
  ;;value and a list of  symbols ARG-TYPES representing the types of the
  ;;arguments: return  a closure to  be used to generate  Scheme callout
  ;;functions from pointers to C functions.
  ;;
  (define who 'make-c-callout-maker/with-errno)
  (with-arguments-validation (who)
      ((symbol			retval-type)
       (null/list-of-symbols	arg-types))
    (let ((S (%ffi-prep-cif who retval-type arg-types)))
      (or (cif-callout-maker/with-errno S)
	  (let ((maker (lambda (c-function-pointer)
			 (%callout-maker/with-errno S c-function-pointer))))
	    (set-cif-callout-maker/with-errno! S maker)
	    maker)))))

(define (%callout-maker/with-errno S c-function-pointer)
  ;;Worker  function  for  Scheme  callout maker  functions.   Return  a
  ;;closure to be called to call a foreign function.
  ;;
  ;;S must be an instance of the CIF data structure.  C-FUNCTION-POINTER
  ;;must be a pointer object referencing the foreign function.
  ;;
  (define who '%callout-maker/with-errno)
  (with-arguments-validation (who)
      ((pointer  c-function-pointer))
    (let ((user-data (cons (cif-cif S) c-function-pointer)))
      (lambda args	;this is the callout function
	(let ((rv (%generic-callout-wrapper user-data S args)))
	  (values rv (foreign-call "ikrt_last_errno")))))))


;;;; Libffi: callbacks

(define (make-c-callback-maker retval-type arg-types)
  ;;Given  the symbol RETVAL-TYPE  representing the  type of  the return
  ;;value and a list of  symbols ARG-TYPES representing the types of the
  ;;arguments: return a  closure to be used to  generate Scheme callback
  ;;pointers from Scheme functions.
  ;;
  (define who 'make-c-callback-maker)
  (with-arguments-validation (who)
      ((symbol			retval-type)
       (null/list-of-symbols	arg-types))
    (let ((S (%ffi-prep-cif who retval-type arg-types)))
      (or (cif-callback-maker S)
	  (let ((maker (lambda (proc)
			 (%callback-maker S proc))))
	    (set-cif-callback-maker! S maker)
	    maker)))))

(define (%callback-maker S proc)
  ;;Worker  function  for Scheme  callback  maker  functions.  Return  a
  ;;pointer to callable machine code.
  ;;
  ;;S must be  an instance of the CIF data structure.   PROC must be the
  ;;Scheme function to wrap.
  ;;
  (define who 'callback-generator)
  (with-arguments-validation (who)
      ((procedure  proc))
    (let* ((retval-pred	(cif-retval-checker S))
	   (retval-type (cif-retval-type    S))
	   (proc	(if (or (eq? retval-type 'void)
				(not arguments-validation))
			    proc ;no return value to be validated
			  ;;This is a wrapper for a Scheme function that
			  ;;needs validation of the return value.
			  (lambda args
			    (let ((v (apply proc args)))
			      (if (retval-pred v)
				  v
				(assertion-violation 'callback
				  "returned value does not match specified type" retval-type v)))))))
      (or (capi.ffi-prepare-callback (cons (cif-cif S) proc))
	  (assertion-violation who "internal error building FFI callback")))))

(define (free-c-callback c-callback-pointer)
  (define who 'free-c-callback)
  (with-arguments-validation (who)
      ((pointer	c-callback-pointer))
    (or (capi.ffi-free-c-callback c-callback-pointer)
	(assertion-violation who
	  "attempt to release unkwnown callback pointer" c-callback-pointer))))


;;;; done

(set-rtd-printer!	(type-descriptor memory-block)	%struct-memory-block-printer)
(set-rtd-destructor!	(type-descriptor memory-block)	%unsafe.memory-block-destructor)

(post-gc-hooks (cons %free-allocated-memory (post-gc-hooks)))

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-pathnames 'scheme-indent-function 1)
;; eval: (put 'arguments-validation-forms 'scheme-indent-function 0)
;; End:
