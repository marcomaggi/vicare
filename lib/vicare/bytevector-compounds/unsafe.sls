;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: unsafe bindings for bytevector-compound objects
;;;Date: Tue Apr 16, 2013
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
(library (vicare bytevector-compounds unsafe)
  (export

    ;; inspection
    $bytevector-compound-empty?		$bytevector-compound-filled?
    $bytevector-compound-length		$bytevector-compound-total-length
    $bytevector-compound-data

    ;; queue operations
    $bytevector-compound-enqueue!	$bytevector-compound-dequeue!

    ;; accessors and mutators
    $bytevector-compound-u8-set!	$bytevector-compound-u8-ref
    $bytevector-compound-s8-set!	$bytevector-compound-s8-ref
    )
  (import (vicare bytevector-compounds core)))

;;; end of file
