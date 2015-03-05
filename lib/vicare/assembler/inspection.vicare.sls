;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: assembler inspection
;;;Date: Mon Oct  8, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare assembler inspection)
  (export)
  (import (vicare))


;;;; assember functions

(define (ret who accumulator)
  (void))

(define (cltd who accumulator)
  (void))

(define (movl who accumulator src dst)
  (void))

(define (mov32 who accumulator src dst)
  (void))

(define (movb who accumulator src dst)
  (void))

(define (addl who accumulator src dst)
  (void))

(define (subl who accumulator src dst)
  (void))

(define (sall who accumulator src dst)
  (void))

(define (shrl who accumulator src dst)
  (void))

(define (sarl who accumulator src dst)
  (void))

(define (andl who accumulator src dst)
  (void))

(define (orl who accumulator src dst)
  (void))

(define (xorl who accumulator src dst)
  (void))

(define (leal who accumulator src dst)
  (void))

(define (cmpl who accumulator src dst)
  (void))

(define (imull who accumulator src dst)
  (void))

(define (idivl who accumulator dst)
  (void))

(define (pushl who accumulator dst)
  (void))

(define (popl who accumulator dst)
  (void))

(define (notl who accumulator dst)
  (void))

(define (bswap who accumulator dst)
  (void))

(define (negl who accumulator dst)
  (void))

(define (jmp who accumulator dst)
  (void))

(define (call who accumulator dst)
  (void))

(define (movsd who accumulator src dst)
  (void))

(define (cvtsi2sd who accumulator src dst)
  (void))

(define (cvtsd2ss who accumulator src dst)
  (void))

(define (cvtss2sd who accumulator src dst)
  (void))

(define (movss who accumulator src dst)
  (void))

(define (addsd who accumulator src dst)
  (void))

(define (subsd who accumulator src dst)
  (void))

(define (mulsd who accumulator src dst)
  (void))

(define (divsd who accumulator src dst)
  (void))

(define (ucomisd who accumulator src dst)
  (void))

(define (ja who accumulator dst)
  (void))

(define (jae who accumulator dst)
  (void))

(define (jb who accumulator dst)
  (void))

(define (jbe who accumulator dst)
  (void))

(define (jg who accumulator dst)
  (void))

(define (jge who accumulator dst)
  (void))

(define (jl who accumulator dst)
  (void))

(define (jle who accumulator dst)
  (void))

(define (je who accumulator dst)
  (void))

(define (jna who accumulator dst)
  (void))

(define (jnae who accumulator dst)
  (void))

(define (jnb who accumulator dst)
  (void))

(define (jnbe who accumulator dst)
  (void))

(define (jng who accumulator dst)
  (void))

(define (jnge who accumulator dst)
  (void))

(define (jnl who accumulator dst)
  (void))

(define (jnle who accumulator dst)
  (void))

(define (jne who accumulator dst)
  (void))

(define (jo who accumulator dst)
  (void))

(define (jp who accumulator dst)
  (void))

(define (jnp who accumulator dst)
  (void))

;;; --------------------------------------------------------------------

(define (byte who accumulator x)
  (void))

(define (byte-vector who accumulator x)
  (void))

(define (int who accumulator a)
  (void))

(define (label who accumulator L)
  (void))

(define (label-address who accumulator L)
  (void))

(define (current-frame-offset who accumulator)
  (void))

(define (nop who accumulator ac)
  (void))


;;;; done

)

;;; end of file
