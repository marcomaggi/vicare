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
;;;Copyright (C) 2012, 2013, 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (vicare.foreign-libraries)
  (export
    dynamically-load-shared-object-from-identifier)
  (import (vicare)
    (prefix (vicare system $foreign) ffi.)
    (prefix (only (ikarus.options)
		  verbose?)
	    option.))


;;;; foreign libraries loading

(define* (dynamically-load-shared-object-from-identifier libid)
  ;;Load the foreign  shared library whose identifier is the  string LIBID.  Make all
  ;;the exported  symbols immediately part  of the process  image, so that  the macro
  ;;FOREIGN-CALL can reference them.
  ;;
  (define-inline (%make-libname-unix   id)	(string-append "lib" id ".so"))
  (define-inline (%make-libname-bsd    id)	(string-append "lib" id ".so"))
  (define-inline (%make-libname-cygwin id)	(string-append id ".dll"))
  (define-inline (%make-libname-darwin id)	(string-append "lib" id ".dylib"))
  (module (target-os-uid)
    (include "ikarus.config.scm"))
  (let* ((libname	(case target-os-uid
			  ((linux)	(%make-libname-unix   libid))
			  ((bsd)	(%make-libname-bsd    libid))
			  ((cygwin)	(%make-libname-cygwin libid))
			  ((darwin)	(%make-libname-darwin libid))
			  (else
			   (error __who__
			     "internal error: invalid target OS UID"
			     target-os-uid))))
	 (rv		(begin
			  (when (option.verbose?)
			    (fprintf (current-error-port)
				     "vicare: loading shared object \"~a\"\n"
				     libname))
			  (ffi.dlopen libname #t #t))))
    ;;FIXME The handle is lost: the library cannot be closed.
    (unless rv
      (error __who__ (ffi.dlerror)))))


;;;; done

;;(foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.foreign-libraries end"))

#| end of file |# )

;;; end of file
