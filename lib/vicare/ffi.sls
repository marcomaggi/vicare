;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2008, 2009  Abdulaziz Ghuloum
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


(library (vicare ffi)
  (export

    ;; shared libraries inteface
    dlopen				dlclose
    dlsym				dlerror

    ;; calling functions and callbacks
    make-c-callout-maker		make-c-callout-maker/with-errno
    make-c-callback-maker		free-c-callback

    ;; system agnostic shared libraries interface
    open-shared-object			close-shared-object
    lookup-shared-object

    &shared-object-error
    make-shared-object-error
    shared-object-error?

    &shared-object-opening-error
    make-shared-object-opening-error
    shared-object-opening-error?
    condition-shared-object-opening-name

    &shared-object-closing-error
    make-shared-object-closing-error
    shared-object-closing-error?
    condition-shared-object-closing-so-handle

    &shared-object-lookup-error
    make-shared-object-lookup-error
    shared-object-lookup-error?
    condition-shared-object-lookup-so-handle
    condition-shared-object-lookup-foreign-symbol

    &out-of-memory-error
    make-out-of-memory-error		out-of-memory-error?

;;; --------------------------------------------------------------------
;;; reexports from (vicare)

    ;; pointer values
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
    make-memory-block			make-memory-block/guarded
    memory-block?
    memory-block?/non-null		memory-block?/not-null
    memory-block-pointer		memory-block-size
    memory-block-reset

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

    ;; C arrays of C strings
    argv-length
    argv->bytevectors			argv->strings
    bytevectors->argv			bytevectors->argv*
    bytevectors->guarded-argv		bytevectors->guarded-argv*
    strings->argv			strings->argv*
    strings->guarded-argv		strings->guarded-argv*

    ;; C strings
    strlen
    strcmp				strncmp
    strdup				strdup*
    strndup				strndup*
    guarded-strdup			guarded-strdup*
    guarded-strndup			guarded-strndup*
    cstring->bytevector			cstring->string
    bytevector->cstring			bytevector->cstring*
    bytevector->guarded-cstring		bytevector->guarded-cstring*
    string->cstring			string->cstring*
    string->guarded-cstring		string->guarded-cstring*

    ;; errno interface
    errno
    case-errno				errno-code
    &errno				make-errno-condition
    errno-condition?			condition-errno
    strerror

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
  (import (vicare)
    (ikarus system $foreign)
    (vicare errno))


;;;; condition objects

(define-condition-type &shared-object-error &error
  make-shared-object-error shared-object-error?)

(define-condition-type &shared-object-opening-error &shared-object-error
  make-shared-object-opening-error shared-object-opening-error?
  (name condition-shared-object-opening-name))

(define-condition-type &shared-object-closing-error &shared-object-error
  make-shared-object-closing-error shared-object-closing-error?
  (so-handle		condition-shared-object-closing-so-handle))

(define-condition-type &shared-object-lookup-error &shared-object-error
  make-shared-object-lookup-error shared-object-lookup-error?
  (so-handle		condition-shared-object-lookup-so-handle)
  (foreign-symbol	condition-shared-object-lookup-foreign-symbol))


;;;; system agnostic API to load libraries

(define open-shared-object
  ;;Open the shared library  identified by the string SHARED-OBJECT-NAME
  ;;and return a library descriptor.
  ;;
  ;;DLOPEN returns a  pointer to the external library  descriptor, or #f
  ;;if an error occurred.
  ;;
  (case-lambda
   (()
    ;;Self-opening the process should never fail.
    (dlopen))
   ((shared-object-name)
    (define who 'open-shared-object)
    (or (dlopen shared-object-name #f #f)
	(raise
	 (condition (make-shared-object-opening-error shared-object-name)
		    (make-who-condition who)
		    (make-message-condition (dlerror))))))))

(define (close-shared-object so-handle)
  ;;Close the shared object referenced by SO-HANDLE.
  ;;
  ;;DLCLOSE returns  a pointer  to the external  entity, or #f  when the
  ;;symbol is not found.
  ;;
  (define who 'lookup-shared-object)
  (or (dlclose so-handle)
      (raise
       (condition (make-shared-object-closing-error so-handle)
		  (make-who-condition who)
		  (make-message-condition (dlerror))))))

(define (lookup-shared-object so-handle foreign-symbol)
  ;;Look up the foreign symbol  selected by the string FOREIGN-SYMBOL in
  ;;the shared library identified by SO-HANDLE.
  ;;
  ;;DLSYM  returns a  pointer to  the external  entity, or  #f  when the
  ;;symbol is not found.
  ;;
  (define who 'lookup-shared-object)
  (or (dlsym so-handle foreign-symbol)
      (raise
       (condition (make-shared-object-lookup-error so-handle foreign-symbol)
		  (make-who-condition who)
		  (make-message-condition (dlerror))))))


;;;; done

)

;;; end of file
