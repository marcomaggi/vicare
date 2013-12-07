;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: support for foreign library autoloading
;;;Date: Mon Jan  9, 2012
;;;
;;;Abstract
;;;
;;;	Support for foreign library  autoloading.  The functions in this
;;;	library  are  used  to:  register associations  between  library
;;;	source  files and  FASL files;  dynamically load  foreign shared
;;;	library in the appropriate way.
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;Table  associating  library source  file  names  to  lists of  strings
;;representing foreign shared library identifiers.
;;
(define FOREIGN-LIBRARIES-TABLE
  (make-hashtable string-hash string=?))

(define (register-filename-foreign-library filename foreign-library-id)
  ;;Register the string FOREIGN-LIBRARY-ID as foreign library identifier
  ;;associated  to  the  library  source  file name  whose  pathname  is
  ;;FILENAME.
  ;;
  (let ((ls (hashtable-ref FOREIGN-LIBRARIES-TABLE filename #f)))
    (hashtable-set! FOREIGN-LIBRARIES-TABLE filename
		    (if ls
			(cons foreign-library-id ls)
		      (list foreign-library-id)))))

(define (retrieve-filename-foreign-libraries filename)
  ;;Return a  list of  strings representing foreign  library identifiers
  ;;associated  to  the  library  source  file name  whose  pathname  is
  ;;FILENAME.  If  the file has no associated  foreign libraries: return
  ;;false.
  ;;
  (hashtable-ref FOREIGN-LIBRARIES-TABLE filename #f))


;;;; foreign libraries loading

(define (autoload-filename-foreign-library libid)
  ;;Load  the foreign  shared  library whose  identifier  is the  string
  ;;LIBID.   Make  all the  exported  symbols  immediately  part of  the
  ;;process image, so that the macro FOREIGN-CALL can reference them.
  ;;
  (define who 'autoload-filename-foreign-library)
  (define-inline (%make-libname-unix   id)	(string-append "lib" id ".so"))
  (define-inline (%make-libname-bsd    id)	(string-append "lib" id ".so"))
  (define-inline (%make-libname-cygwin id)	(string-append id ".dll"))
  (define-inline (%make-libname-darwin id)	(string-append "lib" id ".dylib"))
  (module (target-os-uid)
    (include "ikarus.config.ss"))
  (let* ((libname	(case target-os-uid
			  ((linux)	(%make-libname-unix   libid))
			  ((bsd)	(%make-libname-bsd    libid))
			  ((cygwin)	(%make-libname-cygwin libid))
			  ((darwin)	(%make-libname-darwin libid))
			  (else
			   (error who
			     "internal error: invalid target OS UID"
			     target-os-uid))))
	 (rv		(ffi.dlopen libname #t #t)))
    ;;FIXME The handle is lost: the library cannot be closed.
    (unless rv
      (error who (ffi.dlerror)))))


;;;; done

)

;;; end of file
