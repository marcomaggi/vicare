;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: support for foreign library autoloading
;;;Date: Mon Jan  9, 2012
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare.foreign-libraries)
  (export
    register-filename-foreign-library
    retrieve-filename-foreign-libraries
    autoload-filename-foreign-library)
  (import (ikarus)
    (prefix (ikarus system $foreign) ffi.))


;;;; foreign libraries table

(define FOREIGN-LIBRARIES-TABLE
  (make-hashtable string-hash string=?))

(define (register-filename-foreign-library filename foreign-library-id)
  (let ((ls (hashtable-ref FOREIGN-LIBRARIES-TABLE filename #f)))
    (hashtable-set! FOREIGN-LIBRARIES-TABLE filename
		    (if ls
			(cons foreign-library-id ls)
		      (list foreign-library-id)))))

(define (retrieve-filename-foreign-libraries filename)
  (hashtable-ref FOREIGN-LIBRARIES-TABLE filename #f))


;;;; foreign libraries loading

(define (autoload-filename-foreign-library libid)
  (define who 'autoload-filename-foreign-library)
  (let* ((libname	(string-append "lib" libid ".so"))
	 (rv		(ffi.dlopen libname #t #t)))
    ;;The handle is lost: the library cannot be closed.
    (unless rv
      (error who (ffi.dlerror)))))


;;;; code



;;;; done

)

;;; end of file
