;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: compile script
;;;Date: Sat Dec 10, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

#!r6rs
(import
  (only (vicare installation-configuration))
  (only (vicare errno))
  (only (vicare platform-constants))
  (only (vicare platform constants))
  (only (vicare platform features))
  (only (vicare platform utilities))
  (only (vicare numerics constants))
  (only (vicare unsafe-operations))
  (only (vicare arguments validation))
  (only (vicare syntactic-extensions))
  (only (vicare arguments general-c-buffers))

  (only (vicare unsafe-capi))
  (only (vicare include))
  (only (vicare infix))
  (only (vicare unsafe-unicode))
  (only (vicare words))
  (only (vicare flonum-parser))
  (only (vicare flonum-formatter))
  (only (vicare weak-hashtables))
  (only (vicare keywords))
  (only (vicare checks))

  (only (vicare assembler inspection))
  (only (vicare debugging compiler))

;;; --------------------------------------------------------------------

  (only (srfi :0))
  (only (srfi :1))
  (only (srfi :2))
  (only (srfi :6))
  (only (srfi :8))
  (only (srfi :9))
  (only (srfi :11))
  (only (srfi :13))
  (only (srfi :14))
  (only (srfi :16))
  (only (srfi :19))
  (only (srfi :19 time extensions))
  (only (srfi :23))
  (only (srfi :25))
;;;(only (srfi :25 multi-dimensional-arrays arlib))
  (only (srfi :26))
  (only (srfi :27))
  (only (srfi :28))
  (only (srfi :31))
  (only (srfi :37))
  (only (srfi :38))
  (only (srfi :39))
  (only (srfi :41))
  (only (srfi :42))
  (only (srfi :43))
  (only (srfi :45))
  (only (srfi :48))
  (only (srfi :61))
  (only (srfi :64))
  (only (srfi :67))
  (only (srfi :69))
  (only (srfi :78))
  (only (srfi :98))
  (only (srfi :99))

  ;;We  really   need  all   of  these  for   this  SRFI,   because  the
  ;;implementation is in (srfi :101).
  (only (srfi :101))
  (only (srfi :101 random-access-lists))
  (only (srfi :101 random-access-lists procedures))
  (only (srfi :101 random-access-lists syntax))
  (only (srfi :101 random-access-lists equal))
  )

;;; end of file
