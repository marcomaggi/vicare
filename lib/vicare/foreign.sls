;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2008, 2009  Abdulaziz Ghuloum
;;; Modified by Marco Maggi
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (vicare foreign)
  (export
    ;; pointer values
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
    &errno				make-errno-condition
    errno-condition?			condition-errno

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
  (import (vicare)
    (ikarus system $foreign))


;;;; done

)

;;; end of file
