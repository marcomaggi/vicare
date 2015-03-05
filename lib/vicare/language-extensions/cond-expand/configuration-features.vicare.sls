;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: configuration time features
;;;Date: Sat Aug 24, 2013
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


#!r6rs
(library (vicare language-extensions cond-expand configuration-features)
  (export configuration-time-features)
  (import (vicare))
  (define-syntax-rule (feature ?inspector ?symbol)
    (if (?inspector)
	'(?symbol)
      '()))
  (define (configuration-time-features)
    (append (feature vicare-built-with-srfi-enabled  srfi-enabled)
	    (feature vicare-built-with-ffi-enabled   ffi-enabled)
	    (feature vicare-built-with-iconv-enabled iconv-enabled)
	    (feature vicare-built-with-posix-enabled posix-enabled)
	    (feature vicare-built-with-glibc-enabled glibc-enabled)
	    (feature vicare-built-with-linux-enabled linux-enabled)
	    )))

;;; end of file
