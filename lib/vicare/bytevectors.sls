;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: additional functions and syntaxes for bytevectors
;;;Date: Sat Jul 27, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare bytevectors)
  (export
    bytevector-u16-litend-set!
    bytevector-u16-bigend-set!
    bytevector-u16-litend-ref
    bytevector-u16-bigend-ref
    bytevector-s16-litend-set!
    bytevector-s16-bigend-set!
    bytevector-s16-litend-ref
    bytevector-s16-bigend-ref
    bytevector-u32-litend-set!
    bytevector-u32-bigend-set!
    bytevector-u32-litend-ref
    bytevector-u32-bigend-ref
    bytevector-s32-litend-set!
    bytevector-s32-bigend-set!
    bytevector-s32-litend-ref
    bytevector-s32-bigend-ref
    bytevector-u64-litend-set!
    bytevector-u64-bigend-set!
    bytevector-u64-litend-ref
    bytevector-u64-bigend-ref
    bytevector-s64-litend-set!
    bytevector-s64-bigend-set!
    bytevector-s64-litend-ref
    bytevector-s64-bigend-ref
    ;; bytevector-uint-litend-set!
    ;; bytevector-uint-bigend-set!
    ;; bytevector-uint-litend-ref
    ;; bytevector-uint-bigend-ref
    ;; bytevector-sint-litend-set!
    ;; bytevector-sint-bigend-set!
    ;; bytevector-sint-litend-ref
    ;; bytevector-sint-bigend-ref
    bytevector-ieee-single-litend-set!
    bytevector-ieee-single-bigend-set!
    bytevector-ieee-single-litend-ref
    bytevector-ieee-single-bigend-ref
    bytevector-ieee-double-litend-set!
    bytevector-ieee-double-bigend-set!
    bytevector-ieee-double-litend-ref
    bytevector-ieee-double-bigend-ref
;;;
    bytevector-u16-litend-scaled-set!
    bytevector-u16-bigend-scaled-set!
    bytevector-u16-native-scaled-set!
    bytevector-u16-litend-scaled-ref
    bytevector-u16-bigend-scaled-ref
    bytevector-u16-native-scaled-ref

    bytevector-s16-litend-scaled-set!
    bytevector-s16-bigend-scaled-set!
    bytevector-s16-native-scaled-set!
    bytevector-s16-litend-scaled-ref
    bytevector-s16-bigend-scaled-ref
    bytevector-s16-native-scaled-ref

    bytevector-u32-litend-scaled-set!
    bytevector-u32-bigend-scaled-set!
    bytevector-u32-native-scaled-set!
    bytevector-u32-litend-scaled-ref
    bytevector-u32-bigend-scaled-ref
    bytevector-u32-native-scaled-ref

    bytevector-s32-litend-scaled-set!
    bytevector-s32-bigend-scaled-set!
    bytevector-s32-native-scaled-set!
    bytevector-s32-litend-scaled-ref
    bytevector-s32-bigend-scaled-ref
    bytevector-s32-native-scaled-ref

    bytevector-u64-litend-scaled-set!
    bytevector-u64-bigend-scaled-set!
    bytevector-u64-native-scaled-set!
    bytevector-u64-litend-scaled-ref
    bytevector-u64-bigend-scaled-ref
    bytevector-u64-native-scaled-ref

    bytevector-s64-litend-scaled-set!
    bytevector-s64-bigend-scaled-set!
    bytevector-s64-native-scaled-set!
    bytevector-s64-litend-scaled-ref
    bytevector-s64-bigend-scaled-ref
    bytevector-s64-native-scaled-ref

    bytevector-ieee-single-litend-scaled-set!
    bytevector-ieee-single-bigend-scaled-set!
    bytevector-ieee-single-native-scaled-set!
    bytevector-ieee-single-litend-scaled-ref
    bytevector-ieee-single-bigend-scaled-ref
    bytevector-ieee-single-native-scaled-ref

    bytevector-ieee-double-litend-scaled-set!
    bytevector-ieee-double-bigend-scaled-set!
    bytevector-ieee-double-native-scaled-set!
    bytevector-ieee-double-litend-scaled-ref
    bytevector-ieee-double-bigend-scaled-ref
    bytevector-ieee-double-native-scaled-ref
    )
  (import (vicare))


;;;; single identifier getters and setters
;;
;;The  following   syntaxes  are   wrappers  for   BYTEVECTOR-*-REF  and
;;BYTEVECTOR-*-SET! specifying the endiannes  in their name, without the
;;need of an additional argument (useful in macros).
;;

(define-syntax-rule (bytevector-u16-litend-set! ?bv ?idx ?val)
  (bytevector-u16-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-u16-bigend-set! ?bv ?idx ?val)
  (bytevector-u16-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-u16-litend-ref ?bv ?idx)
  (bytevector-u16-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-u16-bigend-ref ?bv ?idx)
  (bytevector-u16-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-s16-litend-set! ?bv ?idx ?val)
  (bytevector-s16-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-s16-bigend-set! ?bv ?idx ?val)
  (bytevector-s16-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-s16-litend-ref ?bv ?idx)
  (bytevector-s16-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-s16-bigend-ref ?bv ?idx)
  (bytevector-s16-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-u32-litend-set! ?bv ?idx ?val)
  (bytevector-u32-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-u32-bigend-set! ?bv ?idx ?val)
  (bytevector-u32-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-u32-litend-ref ?bv ?idx)
  (bytevector-u32-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-u32-bigend-ref ?bv ?idx)
  (bytevector-u32-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-s32-litend-set! ?bv ?idx ?val)
  (bytevector-s32-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-s32-bigend-set! ?bv ?idx ?val)
  (bytevector-s32-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-s32-litend-ref ?bv ?idx)
  (bytevector-s32-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-s32-bigend-ref ?bv ?idx)
  (bytevector-s32-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-u64-litend-set! ?bv ?idx ?val)
  (bytevector-u64-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-u64-bigend-set! ?bv ?idx ?val)
  (bytevector-u64-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-u64-litend-ref ?bv ?idx)
  (bytevector-u64-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-u64-bigend-ref ?bv ?idx)
  (bytevector-u64-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-s64-litend-set! ?bv ?idx ?val)
  (bytevector-s64-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-s64-bigend-set! ?bv ?idx ?val)
  (bytevector-s64-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-s64-litend-ref ?bv ?idx)
  (bytevector-s64-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-s64-bigend-ref ?bv ?idx)
  (bytevector-s64-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

;; (define-syntax-rule (bytevector-uint-litend-set! ?bv ?idx ?val)
;;   (bytevector-uint-set! ?bv ?idx ?val (endianness little)))

;; (define-syntax-rule (bytevector-uint-bigend-set! ?bv ?idx ?val)
;;   (bytevector-uint-set! ?bv ?idx ?val (endianness big)))

;; (define-syntax-rule (bytevector-uint-litend-ref ?bv ?idx)
;;   (bytevector-uint-ref ?bv ?idx (endianness little)))

;; (define-syntax-rule (bytevector-uint-bigend-ref ?bv ?idx)
;;   (bytevector-uint-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

;; (define-syntax-rule (bytevector-sint-litend-set! ?bv ?idx ?val)
;;   (bytevector-sint-set! ?bv ?idx ?val (endianness little)))

;; (define-syntax-rule (bytevector-sint-bigend-set! ?bv ?idx ?val)
;;   (bytevector-sint-set! ?bv ?idx ?val (endianness big)))

;; (define-syntax-rule (bytevector-sint-litend-ref ?bv ?idx)
;;   (bytevector-sint-ref ?bv ?idx (endianness little)))

;; (define-syntax-rule (bytevector-sint-bigend-ref ?bv ?idx)
;;   (bytevector-sint-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-ieee-single-litend-set! ?bv ?idx ?val)
  (bytevector-ieee-single-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-ieee-single-bigend-set! ?bv ?idx ?val)
  (bytevector-ieee-single-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-ieee-single-litend-ref ?bv ?idx)
  (bytevector-ieee-single-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-ieee-single-bigend-ref ?bv ?idx)
  (bytevector-ieee-single-ref ?bv ?idx (endianness big)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-ieee-double-litend-set! ?bv ?idx ?val)
  (bytevector-ieee-double-set! ?bv ?idx ?val (endianness little)))

(define-syntax-rule (bytevector-ieee-double-bigend-set! ?bv ?idx ?val)
  (bytevector-ieee-double-set! ?bv ?idx ?val (endianness big)))

(define-syntax-rule (bytevector-ieee-double-litend-ref ?bv ?idx)
  (bytevector-ieee-double-ref ?bv ?idx (endianness little)))

(define-syntax-rule (bytevector-ieee-double-bigend-ref ?bv ?idx)
  (bytevector-ieee-double-ref ?bv ?idx (endianness big)))


;;;; scaled single identifier getters and setters
;;
;;The  following   syntaxes  are   wrappers  for   BYTEVECTOR-*-REF  and
;;BYTEVECTOR-*-SET! specifying the endiannes  in their name, without the
;;need of  an additional  argument (useful in  macros), and  scaling the
;;index with the size of the word.
;;

(define-syntax-rule (bytevector-u16-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-u16-set! ?bv (* ?idx 2) ?val (endianness little)))

(define-syntax-rule (bytevector-u16-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-u16-set! ?bv (* ?idx 2) ?val (endianness big)))

(define-syntax-rule (bytevector-u16-native-scaled-set! ?bv ?idx ?val)
  (bytevector-u16-set! ?bv (* ?idx 2) ?val (endianness native)))

(define-syntax-rule (bytevector-u16-litend-scaled-ref ?bv ?idx)
  (bytevector-u16-ref ?bv (* ?idx 2) (endianness little)))

(define-syntax-rule (bytevector-u16-bigend-scaled-ref ?bv ?idx)
  (bytevector-u16-ref ?bv (* ?idx 2) (endianness big)))

(define-syntax-rule (bytevector-u16-native-scaled-ref ?bv ?idx)
  (bytevector-u16-ref ?bv (* ?idx 2) (endianness native)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-s16-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-s16-set! ?bv (* ?idx 2) ?val (endianness little)))

(define-syntax-rule (bytevector-s16-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-s16-set! ?bv (* ?idx 2) ?val (endianness big)))

(define-syntax-rule (bytevector-s16-native-scaled-set! ?bv ?idx ?val)
  (bytevector-s16-set! ?bv (* ?idx 2) ?val (endianness native)))

(define-syntax-rule (bytevector-s16-litend-scaled-ref ?bv ?idx)
  (bytevector-s16-ref ?bv (* ?idx 2) (endianness little)))

(define-syntax-rule (bytevector-s16-bigend-scaled-ref ?bv ?idx)
  (bytevector-s16-ref ?bv (* ?idx 2) (endianness big)))

(define-syntax-rule (bytevector-s16-native-scaled-ref ?bv ?idx)
  (bytevector-s16-ref ?bv (* ?idx 2) (endianness native)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-u32-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-u32-set! ?bv (* ?idx 4) ?val (endianness little)))

(define-syntax-rule (bytevector-u32-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-u32-set! ?bv (* ?idx 4) ?val (endianness big)))

(define-syntax-rule (bytevector-u32-native-scaled-set! ?bv ?idx ?val)
  (bytevector-u32-set! ?bv (* ?idx 4) ?val (endianness native)))

(define-syntax-rule (bytevector-u32-litend-scaled-ref ?bv ?idx)
  (bytevector-u32-ref ?bv (* ?idx 4) (endianness little)))

(define-syntax-rule (bytevector-u32-bigend-scaled-ref ?bv ?idx)
  (bytevector-u32-ref ?bv (* ?idx 4) (endianness big)))

(define-syntax-rule (bytevector-u32-native-scaled-ref ?bv ?idx)
  (bytevector-u32-ref ?bv (* ?idx 4) (endianness native)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-s32-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-s32-set! ?bv (* ?idx 4) ?val (endianness little)))

(define-syntax-rule (bytevector-s32-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-s32-set! ?bv (* ?idx 4) ?val (endianness big)))

(define-syntax-rule (bytevector-s32-native-scaled-set! ?bv ?idx ?val)
  (bytevector-s32-set! ?bv (* ?idx 4) ?val (endianness native)))

(define-syntax-rule (bytevector-s32-litend-scaled-ref ?bv ?idx)
  (bytevector-s32-ref ?bv (* ?idx 4) (endianness little)))

(define-syntax-rule (bytevector-s32-bigend-scaled-ref ?bv ?idx)
  (bytevector-s32-ref ?bv (* ?idx 4) (endianness big)))

(define-syntax-rule (bytevector-s32-native-scaled-ref ?bv ?idx)
  (bytevector-s32-ref ?bv (* ?idx 4) (endianness native)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-u64-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-u64-set! ?bv (* ?idx 8) ?val (endianness little)))

(define-syntax-rule (bytevector-u64-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-u64-set! ?bv (* ?idx 8) ?val (endianness big)))

(define-syntax-rule (bytevector-u64-native-scaled-set! ?bv ?idx ?val)
  (bytevector-u64-set! ?bv (* ?idx 8) ?val (endianness native)))

(define-syntax-rule (bytevector-u64-litend-scaled-ref ?bv ?idx)
  (bytevector-u64-ref ?bv (* ?idx 8) (endianness little)))

(define-syntax-rule (bytevector-u64-bigend-scaled-ref ?bv ?idx)
  (bytevector-u64-ref ?bv (* ?idx 8) (endianness big)))

(define-syntax-rule (bytevector-u64-native-scaled-ref ?bv ?idx)
  (bytevector-u64-ref ?bv (* ?idx 8) (endianness native)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-s64-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-s64-set! ?bv (* ?idx 8) ?val (endianness little)))

(define-syntax-rule (bytevector-s64-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-s64-set! ?bv (* ?idx 8) ?val (endianness big)))

(define-syntax-rule (bytevector-s64-native-scaled-set! ?bv ?idx ?val)
  (bytevector-s64-set! ?bv (* ?idx 8) ?val (endianness native)))

(define-syntax-rule (bytevector-s64-litend-scaled-ref ?bv ?idx)
  (bytevector-s64-ref ?bv (* ?idx 8) (endianness little)))

(define-syntax-rule (bytevector-s64-bigend-scaled-ref ?bv ?idx)
  (bytevector-s64-ref ?bv (* ?idx 8) (endianness big)))

(define-syntax-rule (bytevector-s64-native-scaled-ref ?bv ?idx)
  (bytevector-s64-ref ?bv (* ?idx 8) (endianness native)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-ieee-single-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-ieee-single-set! ?bv (* ?idx 4) ?val (endianness little)))

(define-syntax-rule (bytevector-ieee-single-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-ieee-single-set! ?bv (* ?idx 4) ?val (endianness big)))

(define-syntax-rule (bytevector-ieee-single-native-scaled-set! ?bv ?idx ?val)
  (bytevector-ieee-single-native-set! ?bv (* ?idx 4) ?val))

(define-syntax-rule (bytevector-ieee-single-litend-scaled-ref ?bv ?idx)
  (bytevector-ieee-single-ref ?bv (* ?idx 4) (endianness little)))

(define-syntax-rule (bytevector-ieee-single-bigend-scaled-ref ?bv ?idx)
  (bytevector-ieee-single-ref ?bv (* ?idx 4) (endianness big)))

(define-syntax-rule (bytevector-ieee-single-native-scaled-ref ?bv ?idx)
  (bytevector-ieee-single-native-ref ?bv (* ?idx 4)))

;;; --------------------------------------------------------------------

(define-syntax-rule (bytevector-ieee-double-litend-scaled-set! ?bv ?idx ?val)
  (bytevector-ieee-double-set! ?bv (* ?idx 8) ?val (endianness little)))

(define-syntax-rule (bytevector-ieee-double-bigend-scaled-set! ?bv ?idx ?val)
  (bytevector-ieee-double-set! ?bv (* ?idx 8) ?val (endianness big)))

(define-syntax-rule (bytevector-ieee-double-native-scaled-set! ?bv ?idx ?val)
  (bytevector-ieee-double-native-set! ?bv (* ?idx 8) ?val))

(define-syntax-rule (bytevector-ieee-double-litend-scaled-ref ?bv ?idx)
  (bytevector-ieee-double-ref ?bv (* ?idx 8) (endianness little)))

(define-syntax-rule (bytevector-ieee-double-bigend-scaled-ref ?bv ?idx)
  (bytevector-ieee-double-ref ?bv (* ?idx 8) (endianness big)))

(define-syntax-rule (bytevector-ieee-double-native-scaled-ref ?bv ?idx)
  (bytevector-ieee-double-native-ref ?bv (* ?idx 8)))


;;;; done

)

;;; end of file
