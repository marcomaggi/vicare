;;;
;;;Part of: Vicare Scheme
;;;Contents: object property library
;;;Date: Fri Nov 14, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers object-properties)
  (export
    object-property-initial-capacity
    object-property-default-value
    make-object-property)
  (import (vicare)
    (vicare containers weak-hashtables)
    (only (vicare language-extensions sentinels)
	  sentinel))
  (define object-property-initial-capacity
    (make-parameter 16
      (lambda (initial-capacity)
	(assert (integer? initial-capacity))
	initial-capacity)))
  (define object-property-default-value
    (make-parameter sentinel))
  (define (make-object-property hash-func equiv-func)
    (let ((table    (make-weak-hashtable hash-func equiv-func (object-property-initial-capacity)))
	  (default  (object-property-default-value)))
      (case-lambda
       ((object)
	(weak-hashtable-ref table object default))
       ((object value)
	(weak-hashtable-set! table object value))))))

;;; end of file
