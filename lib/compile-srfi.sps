;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: compile script
;;;Date: Tue Mar 19, 2013
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

#!r6rs
(import
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

  ;;This SRFI depends upon (vicare posix).  So its compilation is in the
  ;;file "compile-srfi-posix.sps".
;;;(only (srfi :106))

  (only (srfi :111))
  )

;;; end of file
