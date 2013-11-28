;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: infix parser table
;;;Date: Thu Nov 28, 2013
;;;
;;;Abstract
;;;
;;;	The parser  table and the  general concept  of the package  is a
;;;	rework of Guile-Arith  by Ian Grant.  The parser  driver is from
;;;	the Lalr-scm package  by Dominique Boucher; the  parser table is
;;;	also generated using Lalr-scm.
;;;
;;;Copyright (c) 2010, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;Copyright (C) 2000 The Free Software Foundation
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
(library (vicare language-extensions infix parser-table)
  (export make-infix-sexp-parser)
  (import (vicare)
    (vicare language-extensions infix lr-driver))


;;; This function is taken from (nausicaa language infix sexp-parser).
(define (make-infix-sexp-parser)
  (lr-driver
   '#(((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . -32))
      ((*default* . -29) (LPAREN . 18))
      ((*default* . *error*) (*eoi* . 41)
       (QUESTION-ID . 40) (AND . 39) (IOR . 38)
       (XOR . 37) (LT . 36) (GT . 35) (LE . 34)
       (GE . 33) (EQ . 32) (ADD . 31) (SUB . 30)
       (MUL . 29) (DIV . 28) (MOD . 27) (EXPT . 26)
       (INCR . 25) (DECR . 24) (BIT-AND . 23)
       (BIT-IOR . 22) (BIT-XOR . 21) (BIT-SHL . 20)
       (BIT-SHR . 19))
      ((*default* . -26) (BIT-SHR . 19) (BIT-SHL . 20))
      ((*default* . -16) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25))
      ((*default* . -15) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25))
      ((*default* . -14) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29))
      ((*default* . -13) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25))
      ((*default* . -22) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
       (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
      ((*default* . *error*) (QUESTION-ID . 40)
       (RPAREN . 42) (AND . 39) (IOR . 38) (XOR . 37)
       (LT . 36) (GT . 35) (LE . 34) (GE . 33) (EQ . 32)
       (ADD . 31) (SUB . 30) (MUL . 29) (DIV . 28)
       (MOD . 27) (EXPT . 26) (INCR . 25) (DECR . 24)
       (BIT-AND . 23) (BIT-IOR . 22) (BIT-XOR . 21)
       (BIT-SHL . 20) (BIT-SHR . 19))
      ((*default* . -34) (BIT-NOT . 1) (DECR . 2)
       (INCR . 3) (SUB . 4) (ADD . 5) (NOT . 6)
       (LPAREN . 7) (NUM . 8) (ID . 9))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . -18)) ((*default* . -17))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . -1) (*eoi* . accept))
      ((*default* . -33))
      ((*default* . *error*) (RPAREN . 65))
      ((*default* . -37) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-NOT . 1) (BIT-XOR . 21) (BIT-IOR . 22)
       (BIT-AND . 23) (DECR . 66) (INCR . 67)
       (EXPT . 26) (MOD . 27) (DIV . 28) (MUL . 29)
       (SUB . 68) (ADD . 69) (EQ . 32) (GE . 33)
       (LE . 34) (GT . 35) (LT . 36) (NOT . 6)
       (XOR . 37) (IOR . 38) (AND . 39) (LPAREN . 7)
       (NUM . 8) (QUESTION-ID . 40) (ID . 9))
      ((*default* . -28)) ((*default* . -27))
      ((*default* . -25) (BIT-SHR . 19) (BIT-SHL . 20))
      ((*default* . -24) (BIT-SHR . 19) (BIT-SHL . 20))
      ((*default* . -23) (BIT-SHR . 19) (BIT-SHL . 20))
      ((*default* . -7) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26))
      ((*default* . -6) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26))
      ((*default* . -4) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27))
      ((*default* . -5) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27))
      ((*default* . -3) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29))
      ((*default* . -2) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29))
      ((*default* . -12) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25))
      ((*default* . -11) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
      ((*default* . -10) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
      ((*default* . -9) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
      ((*default* . -8) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
      ((*default* . -21) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
       (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
      ((*default* . -20) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
       (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
      ((*default* . -19) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
       (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
      ((*default* . *error*) (QUESTION-ID . 40)
       (COLON-ID . 72) (AND . 39) (IOR . 38) (XOR . 37)
       (LT . 36) (GT . 35) (LE . 34) (GE . 33) (EQ . 32)
       (ADD . 31) (SUB . 30) (MUL . 29) (DIV . 28)
       (MOD . 27) (EXPT . 26) (INCR . 25) (DECR . 24)
       (BIT-AND . 23) (BIT-IOR . 22) (BIT-XOR . 21)
       (BIT-SHL . 20) (BIT-SHR . 19))
      ((*default* . -30))
      ((*default* . -18) (BIT-NOT . 1) (DECR . 2)
       (INCR . 3))
      ((*default* . -17) (BIT-NOT . 1) (DECR . 2)
       (INCR . 3))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . -35))
      ((*default* . -37) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-NOT . 1) (BIT-XOR . 21) (BIT-IOR . 22)
       (BIT-AND . 23) (DECR . 66) (INCR . 67)
       (EXPT . 26) (MOD . 27) (DIV . 28) (MUL . 29)
       (SUB . 68) (ADD . 69) (EQ . 32) (GE . 33)
       (LE . 34) (GT . 35) (LT . 36) (NOT . 6)
       (XOR . 37) (IOR . 38) (AND . 39) (LPAREN . 7)
       (NUM . 8) (QUESTION-ID . 40) (ID . 9))
      ((*default* . *error*) (ID . 9) (NUM . 8)
       (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
       (INCR . 3) (DECR . 2) (BIT-NOT . 1))
      ((*default* . -3) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29))
      ((*default* . -2) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29)) ((*default* . -36))
      ((*default* . -31) (BIT-SHR . 19) (BIT-SHL . 20)
       (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
       (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
       (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
       (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36)
       (XOR . 37) (IOR . 38) (AND . 39)
       (QUESTION-ID . 40)))
   (vector '((1 . 10)) '((1 . 11)) '((1 . 12))
	   '((1 . 13)) '((1 . 14)) '((1 . 15)) '((1 . 16))
	   '((1 . 17)) '() '() '() '() '() '() '() '() '() '()
	   '((2 . 43) (1 . 44)) '((1 . 45)) '((1 . 46))
	   '((1 . 47)) '((1 . 48)) '((1 . 49)) '() '()
	   '((1 . 50)) '((1 . 51)) '((1 . 52)) '((1 . 53))
	   '((1 . 54)) '((1 . 55)) '((1 . 56)) '((1 . 57))
	   '((1 . 58)) '((1 . 59)) '((1 . 60)) '((1 . 61))
	   '((1 . 62)) '((1 . 63)) '((1 . 64)) '() '() '()
	   '((3 . 70) (1 . 71)) '() '() '() '() '() '() '() '()
	   '() '() '() '() '() '() '() '() '() '() '() '() '()
	   '((1 . 12)) '((1 . 13)) '((1 . 73)) '((1 . 74)) '()
	   '((3 . 75) (1 . 71)) '((1 . 76)) '() '() '() '())
   (vector '()
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     $1)
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 $2 yy-stack-states
				     yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 (list $1 $2)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 (list (car $1) $2)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 (list (car $1) $2)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 (list (cdr $2) $1)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 (list (cdr $2) $1)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 (list $1 $2)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 1 (list $1 $2)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
				     yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $4 $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 4 1 (cons $1 $3)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 5 1 (list $2 $1 $3 $5)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
				     yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $3 $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 3 1 $2 yy-stack-states
				     yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states . yy-stack-values)
	     (yy-reduce-pop-and-push 0 2 '() yy-stack-states
				     yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 2 (cons $1 $2)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states $2 $1 . yy-stack-values)
	     (yy-reduce-pop-and-push 2 3 (cons $1 $2)
				     yy-stack-states yy-stack-values))
	   (lambda
	       (yy-reduce-pop-and-push yypushback yycustom
				       yy-stack-states . yy-stack-values)
	     (yy-reduce-pop-and-push 0 3 '() yy-stack-states
				     yy-stack-values)))))


;;;; done

)

;;; end of file
