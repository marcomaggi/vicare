;;;
;;;Part of: Vicare
;;;Contents: GCC wrapper library
;;;Date: Mon Dec  5, 2011
;;;
;;;Abstract
;;;
;;;	This is a toy library making use of GCC to create, at runtime, C
;;;	language shared libraries to be  loaded by the FFI.  Each shared
;;;	library exports a  single C function accessed as  callout by the
;;;	Scheme program.
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare gcc)
  (export
    initialise			define-c-function

    COMPILE-FLAGS		LINK-FLAGS
    CFLAGS			LDFLAGS)
  (import (vicare)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare posix) px.)
    (prefix (vicare glibc) glibc.)
    (prefix (vicare platform-constants) plat.)
    (vicare syntactic-extensions))


;;;; arguments validation

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (list-of-strings who obj)
  (and (list? obj) (for-all string? obj))
  (assertion-violation who "expected list of strings as argument" obj))


;;;; initialisation

(define TMPDIR #f)	;it must have execute permissions
(define GCC    #f)

(define (initialise gcc-file temporary-directory)
  ;;Initialise the library.
  ;;
  ;;GCC-FILE must be the filename of the GCC executable.
  ;;
  ;;TEMPORARY-DIRECTORY  must  be the  string  pathname  of an  existing
  ;;directory on a partition with  executable permissions; it is used to
  ;;create temporary files, including the shared libraries.
  ;;
  (define who 'initialise)
  (if (and temporary-directory
	   (px.file-is-directory? temporary-directory #f)
	   (px.access temporary-directory (fxior plat.R_OK plat.W_OK)))
      (let ((T (string->latin1 (string-append temporary-directory "/vicare-gcc-XXXXXX"))))
	(glibc.mkdtemp T)
	(set! TMPDIR (latin1->string T)))
    (error who
      "unable to retrieve pathname of readable and writable directory for temporary files"))
  (if (and gcc-file
	   (file-exists? gcc-file)
	   (px.access gcc-file plat.X_OK))
      (set! GCC gcc-file)
    (error who
      "unable to retrieve pathname of executable GCC")))


;;;; compiler and linker flags

(define COMPILE-FLAGS
  (make-parameter '("-c")
    (lambda (obj)
      (with-arguments-validation (COMPILE-FLAGS)
	  ((list-of-strings obj))
	obj))))

(define LINK-FLAGS
  (make-parameter '("-pipe" "-shared" "-fPIC")
    (lambda (obj)
      (with-arguments-validation (LINK-FLAGS)
	  ((list-of-strings obj))
	obj))))

(define CFLAGS
  (make-parameter '("-O2")
    (lambda (obj)
      (with-arguments-validation (CFLAGS)
	  ((list-of-strings obj))
	obj))))

(define LDFLAGS
  (make-parameter '()
    (lambda (obj)
      (with-arguments-validation (LDFLAGS)
	  ((list-of-strings obj))
	obj))))


;;;; compilation and linking commands

#;(define ENV
  (map (lambda (pair)
	 (string-append (car pair) "=" (cdr pair)))
    (px.environ)))

(define (%compile-object-file source-filename object-filename)
  #;(px.execve GCC (cons GCC (append (CFLAGS) (COMPILE-FLAGS)
				   `("-o" ,object-filename ,source-filename)))
	     ENV)
  (px.execv GCC (cons GCC (append (CFLAGS) (COMPILE-FLAGS)
				  `("-o" ,object-filename ,source-filename)))))

(define (%compile-library-file library-filename object-filename)
  #;(px.execve GCC (cons GCC (append (LDFLAGS) (LINK-FLAGS)
				   `("-o" ,library-filename ,object-filename)))
	     ENV)
  (px.execv GCC (cons GCC (append (LDFLAGS) (LINK-FLAGS)
				  `("-o" ,library-filename ,object-filename)))))


;;;; defining libraries

(define-syntax define-c-function
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?retval-type ?who (?arg-types ...) ?code)
       (with-syntax ((IDENTIFIER (symbol->string (syntax->datum #'?who))))
	 #'(define ?who
	     (%compile-and-load IDENTIFIER
				'?retval-type '(?arg-types ...)
				?code)))))))

(define (%compile-and-load identifier retval-type arg-types code)
  (define who '%compile-and-load)
  (with-arguments-validation (who)
      ((string	identifier)
       (string	code))
    (let ((source-filename  (string-append TMPDIR "/"    identifier ".c"))
	  (object-filename  (string-append TMPDIR "/"    identifier ".o"))
	  (library-filename (string-append TMPDIR "/lib" identifier ".so")))
      (when (file-exists? source-filename)  (delete-file source-filename))
      (when (file-exists? object-filename)  (delete-file object-filename))
      (when (file-exists? library-filename) (delete-file library-filename))
      (with-output-to-file source-filename
	(lambda ()
	  (display code (current-output-port))))
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (unless (and (px.WIFEXITED status)
				(fx= 0 (px.WEXITSTATUS status)))
		     (error who "error compiling object file"))))
	       (lambda ()
		 (%compile-object-file source-filename object-filename)))
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (unless (and (px.WIFEXITED status)
				(fx= 0 (px.WEXITSTATUS status)))
		     (error who "error linking object file"))))
	       (lambda ()
		 (%compile-library-file library-filename object-filename)))
      (%load-library-and-make-callout library-filename identifier retval-type arg-types))))

(define (%load-library-and-make-callout library-filename identifier retval-type arg-types)
  (define who '%compile-and-load)
  (let ((lib (ffi.dlopen library-filename)))
    (if lib
	(let ((maker (ffi.make-c-callout-maker retval-type arg-types)))
	  (maker (ffi.dlsym lib identifier)))
      (error who (ffi.dlerror)))))


;;;; done

)

;;; end of file
;; Local Variables:
;; coding: utf-8
;; End:
