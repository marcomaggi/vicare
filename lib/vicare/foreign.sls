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
    pointer-set-c-char!
    pointer-set-c-short!
    pointer-set-c-int!
    pointer-set-c-long!
    pointer-set-c-long-long!
    pointer-set-c-pointer!
    pointer-set-c-float!
    pointer-set-c-double!
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
    pointer-ref-c-pointer
    pointer-ref-c-float
    pointer-ref-c-double
    )
  (import (vicare)
    (ikarus system $foreign))


;;;; peekers

;; (define-syntax define-signed-peeker
;;   (syntax-rules ()
;;     ((_ ?name ?sizeof-data)
;;      (define ?name (case ?sizeof-data
;; 		     ((1) pointer-ref-c-signed-char)
;; 		     ((2) pointer-ref-c-signed-short)
;; 		     ((4) pointer-ref-c-signed-int)
;; 		     ((8) pointer-ref-c-signed-long-long))))))

;; (define-syntax define-unsigned-peeker
;;   (syntax-rules ()
;;     ((_ ?name ?sizeof-data)
;;      (define ?name (case ?sizeof-data
;; 		     ((1) pointer-ref-c-unsigned-char)
;; 		     ((2) pointer-ref-c-unsigned-short)
;; 		     ((4) pointer-ref-c-unsigned-int)
;; 		     ((8) pointer-ref-c-unsigned-long-long))))))

;; (define-signed-peeker pointer-ref-c-int8	1)
;; (define-signed-peeker pointer-ref-c-int16	2)
;; (define-signed-peeker pointer-ref-c-int32	4)
;; (define-signed-peeker pointer-ref-c-int64	8)

;; (define-unsigned-peeker pointer-ref-c-uint8	1)
;; (define-unsigned-peeker pointer-ref-c-uint16	2)
;; (define-unsigned-peeker pointer-ref-c-uint32	4)
;; (define-unsigned-peeker pointer-ref-c-uint64	8)

;; (define pointer-ref-c-void*	pointer-ref-c-pointer)


;;;; pokers

;; (define const:2^15 (expt 2 15))
;; (define const:2^16 (expt 2 16))
;; (define const:2^31 (expt 2 31))
;; (define const:2^32 (expt 2 32))
;; (define const:2^63 (expt 2 63))
;; (define const:2^64 (expt 2 64))

;; (define-syntax define-setter
;;   (syntax-rules ()
;;     ((_ ?name ?min ?max ?setter)
;;      (define (?name pointer offset value)
;;        (if (and (<= ?min value) (<= value ?max))
;; 	   (?setter pointer offset value)
;; 	 (assertion-violation (quote ?name)
;; 	   "value out of bounds for pointer setter type" value))))))

;; (define-syntax define-signed-poker
;;   (syntax-rules ()
;;     ((_ ?name ?sizeof-data)
;;      (define ?name (case ?sizeof-data
;; 		     ((1) pointer-set-c-signed-char!)
;; 		     ((2) pointer-set-c-signed-short!)
;; 		     ((4) pointer-set-c-signed-int!)
;; 		     ((8) pointer-set-c-signed-long-long!))))))

;; (define-syntax define-unsigned-poker
;;   (syntax-rules ()
;;     ((_ ?name ?sizeof-data)
;;      (define ?name (case ?sizeof-data
;; 		     ((1) pointer-set-c-unsigned-char!)
;; 		     ((2) pointer-set-c-unsigned-short!)
;; 		     ((4) pointer-set-c-unsigned-int!)
;; 		     ((8) pointer-set-c-unsigned-long-long!))))))

;; (define-setter pointer-set-c-signed-char!
;;   -128 127 pointer-set-c-char!)
;; (define-setter pointer-set-c-signed-short!
;;   (- const:2^15) (- const:2^15 1) pointer-set-c-short!)
;; (define-setter pointer-set-c-signed-int!
;;   (- const:2^31) (- const:2^31 1) pointer-set-c-int!)
;; (define-setter pointer-set-c-signed-long-long!
;;   (- const:2^63) (- const:2^63 1) pointer-set-c-long-long!)

;; (define pointer-set-c-signed-long!
;;   (if (c-inspect on-32-bits-system) pointer-set-c-signed-int! pointer-set-c-signed-long-long!))

;; (define-setter pointer-set-c-unsigned-char!
;;   0 255 pointer-set-c-char!)
;; (define-setter pointer-set-c-unsigned-short!
;;   0 (- const:2^16 1) pointer-set-c-short!)
;; (define-setter pointer-set-c-unsigned-int!
;;   0 (- const:2^32 1) pointer-set-c-int!)
;; (define-setter pointer-set-c-unsigned-long-long!
;;   0 (- const:2^64 1) pointer-set-c-long-long!)

;; (define pointer-set-c-unsigned-long!
;;   (if (c-inspect on-32-bits-system) pointer-set-c-unsigned-int! pointer-set-c-unsigned-long-long!))

;; (define-signed-poker pointer-set-c-int8!	1)
;; (define-signed-poker pointer-set-c-int16!	2)
;; (define-signed-poker pointer-set-c-int32!	4)
;; (define-signed-poker pointer-set-c-int64!	8)

;; (define-unsigned-poker pointer-set-c-uint8!	1)
;; (define-unsigned-poker pointer-set-c-uint16!	2)
;; (define-unsigned-poker pointer-set-c-uint32!	4)
;; (define-unsigned-poker pointer-set-c-uint64!	8)

;; (define pointer-set-c-void*!		pointer-set-c-pointer!)


;;;; done

)

;;; end of file
