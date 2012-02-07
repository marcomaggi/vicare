;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;; --------------------------------------------------------------------

    ;; pointer values
    pointer?
    null-pointer			pointer-null?
    pointer->integer			integer->pointer
    pointer-diff			pointer-add
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

    ;; raw memory allocation
    malloc				guarded-malloc
    realloc				guarded-realloc
    calloc				guarded-calloc
    free				memcmp
    memcpy				memmove
    memset				memory-copy
    bytevector->memory			memory->bytevector
    bytevector->guarded-memory

    with-local-storage

    ;; C arrays of C strings
    bytevectors->argv			argv->bytevectors
    strings->argv			argv->strings
    bytevectors->guarded-argv
    strings->guarded-argv
    argv-length

    ;; C strings
    strlen
    strcmp				strncmp
    strdup				strndup
    guarded-strdup			guarded-strndup
    cstring->bytevector
    bytevector->cstring			bytevector->guarded-cstring
    cstring->string
    string->cstring			string->guarded-cstring

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
    pointer-set-c-pointer!)
  (import (vicare)
    (ikarus system $foreign)
    (vicare errno)))

;;; end of file
