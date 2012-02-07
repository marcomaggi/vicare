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
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (only (vicare errno))
  (only (vicare platform-constants))
  (only (vicare include))
  (only (vicare syntactic-extensions))
  (only (vicare flonum-parser))
  (only (vicare flonum-formatter))
  (only (vicare unsafe-operations))
  (only (vicare unsafe-unicode))
  (only (vicare unsafe-capi))
  (only (vicare words))
  (only (vicare installation-configuration))
  (only (vicare gcc))
  (only (vicare weak-hashtables))
  )

;;; end of file
