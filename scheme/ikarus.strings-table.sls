;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: table of interned strings
;;;Date: Thu Sep 12, 2013
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
(library (ikarus.strings-table)
  (export
    $initialize-interned-strings-table!
    intern-string
    $interned-strings)
  (import (except (ikarus)
		  intern-string)
    (vicare arguments validation)
    (vicare unsafe operations))


(define STRING-TABLE #f)

(define ($initialize-interned-strings-table!)
  (set! STRING-TABLE (make-hashtable $string-hash $string=)))

(define (intern-string str)
  (define who 'intern-string)
  (with-arguments-validation (who)
      ((string	str))
    (if ($fxzero? ($string-length str))
	str
      (or (hashtable-ref STRING-TABLE str #f)
	  (begin
	    (hashtable-set! STRING-TABLE str str)
	    str)))))

(define ($interned-strings)
  (hashtable-keys STRING-TABLE))


;;;; done

)

;;; end of file
