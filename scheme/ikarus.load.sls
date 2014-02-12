;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (ikarus load)
  (export
    load		load-r6rs-script
    load-and-serialize-source-library
    library-path	library-extensions
    fasl-directory	fasl-path
    fasl-search-path)
  (import (except (ikarus)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  load			load-r6rs-script
		  library-path		library-extensions
		  fasl-directory	fasl-path
		  fasl-search-path	get-annotated-datum)
    (prefix (only (ikarus.posix)
		  getenv
		  mkdir/parents
		  split-pathname-root-and-tail
		  real-pathname
		  file-modification-time)
	    posix.)
    (only (ikarus.compiler)
	  compile-core-expr)
    (only (vicare.foreign-libraries)
	  retrieve-filename-foreign-libraries)
    (only (psyntax library-manager)
	  serialize-collected-libraries
	  serialize-loaded-library
	  current-library-source-file-locator
	  current-library-source-loader
	  current-library-source-loader-by-filename
	  current-library-serialized-loader
	  current-include-file-locator
	  current-include-file-loader)
    (only (psyntax expander)
	  expand-r6rs-top-level-make-evaluator)
    (only (ikarus.reader)
	  get-annotated-datum
	  read-script-source-file
	  read-library-source-file)
    (prefix (only (vicare options)
		  print-loaded-libraries)
	    config.)
    (vicare arguments validation))

  (include "ikarus.wordsize.scm")


;;;; arguments validation

(define-argument-validation (search-path who obj)
  (for-all string? obj)
  (procedure-argument-violation who
    "expected list of strings representing directory pathnames as search path" obj))


;;;; handling of FASL repository file names

(module (fasl-search-path
	 fasl-directory
	 fasl-path
	 %make-fasl-pathname)

  (define (%file-exists? filename)
    ;;FILE-EXISTS? will raise an error if P is empty, so we wrap it.
    ;;
    (and filename
	 (string? filename)
	 (not (fxzero? (string-length filename)))
	 (file-exists? filename)))

  ;;The file extension of serialised FASL files.
  (define-constant FASL-EXTENSION
    (boot.case-word-size
     ((32)	".vicare-32bit-fasl")
     ((64)	".vicare-64bit-fasl")))

  (define-constant DEFAULT-FASL-DIRECTORY
    (let ((P (posix.getenv "VICARE_FASL_DIRECTORY")))
      (if (%file-exists? P)
	  (posix.real-pathname P)
	(let ((P (posix.getenv "HOME")))
	  (if (%file-exists? P)
	      (string-append (posix.real-pathname P) "/.vicare/precompiled")
	    "")))))

  ;;The search path to look for FASL  files.  Notice that we do not test
  ;;the existence of the directories.
  ;;
  (define fasl-search-path
    (make-parameter (list DEFAULT-FASL-DIRECTORY)
      (lambda (P)
	(define who 'fasl-path)
	(with-arguments-validation (who)
	    ((search-path	P))
	  P))))

  ;;The directory under which serialised FASL files must be saved.
  ;;
  (define fasl-directory
    (make-parameter DEFAULT-FASL-DIRECTORY
      (lambda (P)
	(define who 'fasl-directory)
	(if (string? P)
	    (if (file-exists? P)
		(posix.real-pathname P)
	      (error who "attempt to set non-existent directory pathname" P))
	  (error who "expected string as directory pathname" P)))))

  (define (fasl-path filename)
    ;;Given a source file name return the associated full FASL file name
    ;;using  the  current  value  of FASL-DIRECTORY.   Return  false  if
    ;;FASL-DIRECTORY is unset (which should never happen).
    ;;
    (let ((d (fasl-directory)))
      (and (not (string=? d ""))
	   (%make-fasl-pathname d filename))))

  (define (%make-fasl-pathname prefix-pathname source-file-pathname)
    ;;We assume  that PREFIX-PATHNAME,  if it  exists, has  already been
    ;;normalised; if PREFIX-PATHNAME does not  exist: it will be created
    ;;when the FASL file is created.
    ;;
    (string-append prefix-pathname
		   (posix.real-pathname source-file-pathname)
		   FASL-EXTENSION))

  #| end of module |# )


;;;; loading source programs

(define* (load-r6rs-script (filename string?) serialize? run?)
  ;;Load source code from FILENAME,  which must be a string representing
  ;;a filename, expecting an R6RS program or an R6RS library and compile
  ;;it.
  ;;
  ;;If  SERIALIZE? is  true: the  libraries  needed by  the program  are
  ;;compiled, serialized and saved in FASL files.
  ;;
  ;;If RUN? is true: the loaded R6RS program is compiled and evaluated.
  ;;
  (let* ((prog  (read-script-source-file filename))
	 (thunk (expand-r6rs-top-level-make-evaluator prog)))
    (when serialize?
      (serialize-collected-libraries (lambda (lib-filename contents)
				       (store-serialized-library lib-filename contents))
				     (lambda (core-expr)
				       (compile-core-expr core-expr))))
    (when run?
      (thunk))))


;;;; loading source libraries then serializing them

(define* (load-and-serialize-source-library (source-filename string?))
  ;;Load a source library filename, expand it, compile it, serialize it.
  ;;Return unspecified values.
  ;;
  (define verbose? #t)
  (when verbose?
    (fprintf (current-error-port)
	     "loading source library: ~a\n"
	     source-filename))
  (let ((lib ((current-library-source-loader-by-filename) source-filename)))
    (when verbose?
      (fprintf (current-error-port)
	       "serializing library: ~a\n"
	       (fasl-path source-filename)))
    (serialize-loaded-library lib
			      (lambda (source-filename contents)
				(store-serialized-library source-filename contents))
			      (lambda (core-expr)
				(compile-core-expr core-expr)))))


;;;; locating source library files
;;
;;The library source file locator is  a function that converts a library
;;name specification into the corresponding file pathname.
;;
(module (locate-library-source-file
	 library-path
	 library-extensions)
  (define-constant __who__
    'locate-library-source-file)

  (define library-path
    ;;Hold a list of strings  representing directory pathnames being the
    ;;search path.
    ;;
    (make-parameter
	'(".")
      (lambda (obj)
	(define-constant __who__ 'library-path)
	(with-arguments-validation (__who__)
	    ((list-of-strings	obj))
	  obj))))

  (define library-extensions
    ;;Hold a list of strings  representing file name extensions, leading
    ;;dot included.
    ;;
    (make-parameter
	'(".vicare.sls" ".sls")
      (lambda (obj)
	(define-constant __who__ 'library-extensions)
	(with-arguments-validation (__who__)
	    ((list-of-strings	obj))
	  obj))))

  (define (locate-library-source-file libname pending-libraries)
    ;;Default   value    for   the   CURRENT-LIBRARY-SOURCE-FILE-LOCATOR
    ;;parameter.  Given  a library  name, as defined  by R6RS:  scan the
    ;;library search  path for the  corresponding source file;  return a
    ;;string representing the source file pathname having:
    ;;
    ;;* The  directory part equal to  one of the directory  pathnames in
    ;;  the parameter  LIBRARY-PATH.
    ;;
    ;;*  File extension  part  equal to  one of  the  extensions in  the
    ;;  parameter LIBRARY-EXTENSIONS.
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    ;;PENDING-LIBRARIES must be  a possibly empty list  of library names
    ;;whose installation is currently  pending; when non-empty, the list
    ;;represents   the  libraries   that  are   requesting  LIBNAME   as
    ;;dependency.
    ;;
    (let loop ((rootname-str     (%library-identifiers->file-name libname))
	       (directories      (library-path))
	       (file-extensions  (library-extensions))
	       (failed-list      '()))
      (cond ((null? directories)
	     ;;No suitable library was found.
	     (%file-locator-resolution-error libname (reverse failed-list)
					     pending-libraries))

	    ((null? file-extensions)
	     ;;No more extensions: try the  next directory in the search
	     ;;path.
	     (loop rootname-str (cdr directories) (library-extensions) failed-list))
	    (else
	     ;;Check the  file existence  in the current  directory with
	     ;;the current  file extension;  if not  found try  the next
	     ;;file extension.
	     (let ((pathname (string-append (car directories) rootname-str (car file-extensions))))
	       (if (file-exists? pathname)
		   pathname
		 (loop rootname-str directories (cdr file-extensions) (cons pathname failed-list))))))))

  (module (%library-identifiers->file-name)

    (define (%library-identifiers->file-name library-name.ids)
      ;;Convert the  non-empty list of  identifiers from a  library name
      ;;into  a  string  representing the  corresponding  relative  file
      ;;pathname,  without   extension  but  including  a   leading  #\/
      ;;character.  Examples:
      ;;
      ;;   (%library-identifiers->file-name '(alpha beta gamma))
      ;;   => "/alpha/beta/gamma"
      ;;
      ;;   (%library-identifiers->file-name '(alpha beta main))
      ;;   => "/alpha/beta/main_"
      ;;
      ;;notice  how  the  component  "main",  when  appearing  last,  is
      ;;"quoted" by appending an underscore.
      ;;
      (assert (not (null? library-name.ids)))
      (receive (port extract)
	  (open-string-output-port)
	(let next-component ((component		(car library-name.ids))
			     (ls		(cdr library-name.ids))
			     (first-component?	#t))
	  (write-char #\/ port)
	  (let ((component-name (symbol->string component)))
	    (for-each (lambda (n)
			(let ((c (integer->char n)))
			  (if (or (char<=? #\a c #\z)
				  (char<=? #\A c #\Z)
				  (char<=? #\0 c #\9)
				  (memv c '(#\. #\- #\+ #\_)))
			      (write-char c port)
			    (let-values (((D M) (div-and-mod n 16)))
			      (write-char #\% port)
			      (display-hex D port)
			      (display-hex M port)))))
	      (bytevector->u8-list (string->utf8 component-name)))
	    (if (null? ls)
		(when (and (not first-component?)
			   (main*? component-name))
		  (write-char #\_ port))
	      (next-component (car ls) (cdr ls) #f))))
	(extract)))

    (define (display-hex n port)
      (if (<= 0 n 9)
	  (display n port)
	(write-char (integer->char (+ (char->integer #\a) (- n 10))) port)))

    (define (main*? component-name)
      (and (>= (string-length component-name) 4)
	   (string=? (substring component-name 0 4) "main")
	   (for-all (lambda (ch)
		      (char=? ch #\_))
	     (string->list (substring component-name 4 (string-length component-name))))))

    #| end of module: %LIBRARY-IDENTIFIERS->FILE-NAME |# )

  (define (%file-locator-resolution-error libname failed-list pending-libraries)
    (raise
     (apply condition (make-error)
	    (make-who-condition 'expander)
	    (make-message-condition "cannot locate library in library-path")
	    (make-library-resolution-condition libname failed-list)
	    (map make-imported-from-condition pending-libraries))))

  (define-condition-type &library-resolution
      &condition
    make-library-resolution-condition
    library-resolution-condition?
    (library condition-library)
    (files condition-files))

  (define-condition-type &imported-from
      &condition
    make-imported-from-condition
    imported-from-condition?
    (importing-library importing-library))

  #| end of module: LOCATE-LIBRARY-SOURCE-FILE |# )


;;;; loading and storing precompiled library files

(module (load-serialized-library
	 store-serialized-library)

  (define-struct serialized-library
    (contents
		;A list of values  representing a LIBRARY record holding
		;precompiled  code.   For  details  on  the  format  see
		;SERIALIZE-LIBRARY in "psyntax.library-manager.sls".
     ))

  (define (load-serialized-library filename success-kont)
    ;;Given a source  file name load the associated FASL  file and apply
    ;;SUCCESS-KONT to  the library contents,  return the result  of such
    ;;application.  If a  FASL file is not available  or invalid: return
    ;;false.
    ;;
    ;;Print  to the  current error  port appropriate  warning about  the
    ;;availability of the FASL file.
    ;;
    ;;This   function   is  the   default   value   for  the   parameter
    ;;CURRENT-LIBRARY-SERIALIZED-LOADER.
    ;;
    (define (%print-loaded-library name)
      (when (config.print-loaded-libraries)
	(display (string-append "Vicare loading: " name "\n")
		 (console-error-port))))
    (let ((ikfasl (let next-prefix ((search-path (fasl-search-path)))
		    (if (null? search-path)
			#f
		      (let ((ikfasl (%make-fasl-pathname (car search-path) filename)))
			(if (file-exists? ikfasl)
			    ikfasl
			  (next-prefix (cdr search-path))))))))
      (cond ((or (not ikfasl)
		 (not (file-exists? ikfasl)))
	     (%print-loaded-library filename)
	     #f)
	    ((< (posix.file-modification-time ikfasl)
		(posix.file-modification-time filename))
	     (%print-loaded-library filename)
	     (fprintf (console-error-port)
		      "WARNING: not using fasl file ~s because it is older \
                       than the source file ~s\n" ikfasl filename)
	     #f)
	    (else
	     (%print-loaded-library ikfasl)
	     (let ((x (let ((port (open-file-input-port ikfasl)))
			(unwind-protect
			    (fasl-read port)
			  (close-input-port port)))))
	       (if (serialized-library? x)
		   (apply success-kont filename (serialized-library-contents x))
		 (begin
		   (fprintf (console-error-port)
			    "WARNING: not using fasl file ~s because it was \
                             compiled with a different instance of Vicare.\n" ikfasl)
		   #f)))))))

  (define (store-serialized-library filename contents)
    ;;Given the source  file name of a library file  and the contents of
    ;;an already compiled library write a FASL file in the repository.
    ;;
    ;;CONTENTS must  be a list  of values representing a  LIBRARY record
    ;;holding precompiled  code.  See the function  SERIALIZE-LIBRARY in
    ;;"psyntax.library-manager.sls" for details on the format.
    ;;
    (cond ((fasl-path filename)
	   => (lambda (ikfasl)
		(define-syntax-rule (%display ?thing)
		  (display ?thing stderr))
		(%display "serialising ")
		(%display ikfasl)
		(%display " ... ")
		(receive (dir name)
		    (posix.split-pathname-root-and-tail ikfasl)
		  (posix.mkdir/parents dir #o755))
		(let ((port (open-file-output-port ikfasl (file-options no-fail))))
		  (unwind-protect
		      (fasl-write (make-serialized-library contents) port
				  (retrieve-filename-foreign-libraries filename))
		    (close-output-port port)))
		(%display "done\n")))))

  #| end of module |# )


;;;; general loading of programs and libraries

(module (load)

  (case-define* load
    ;;Load  source   code  from  FILENAME,   which  must  be   a  string
    ;;representing  a  filename, expecting:  an  R6RS  program, an  R6RS
    ;;library or just  a list of forms.  Then transform  the contents of
    ;;the file in  a list of symbolic expressions; for  each form in the
    ;;source apply EVAL-PROC to the corresponding symbolic expression.
    ;;
    ;;When  EVAL-PROC is  not  given:  the forms  are  evaluated in  the
    ;;current INTERACTION-ENVIRONMENT.
    ;;
    ((filename)
     (load filename load-handler))
    (((filename string?) (eval-proc procedure?))
     (let next-form ((ls (read-script-source-file filename)))
       (unless (null? ls)
	 (eval-proc (car ls))
	 (next-form (cdr ls))))))

  (define (load-handler x)
    (eval x (interaction-environment)))

  #| end of module: LOAD |# )


;;;; locating and loading include files

(define (locate-include-file filename synner)
  ;;Convert the string FILENAME into  the string pathname of an existing
  ;;file; return the pathname.
  ;;
  (unless (and (string? filename)
	       (not (fxzero? (string-length filename))))
    (synner "file name must be a nonempty string" filename))
  (if (char=? (string-ref filename 0) #\/)
      ;;It is an absolute pathname.
      (posix.real-pathname filename)
    ;;It is a relative pathname.  Search the file in the library path.
    (let loop ((ls (library-path)))
      (if (null? ls)
	  (synner "file does not exist in library path" filename)
	(let ((ptn (string-append (car ls) "/" filename)))
	  (if (file-exists? ptn)
	      (posix.real-pathname ptn)
	    (loop (cdr ls))))))))

(define (read-include-file pathname synner)
  ;;Open the file PATHNAME, read all  the datums and return them.  If an
  ;;error occurs call SYNNER.
  ;;
  ;;We expect the  returned contents to be a list  of annotated symbolic
  ;;expressions.
  ;;
  (guard (E (else
	     (synner (condition-message E) pathname)))
    (let ((port (open-input-file pathname)))
      (unwind-protect
	  (let recur ()
	    (let ((datum (get-annotated-datum port)))
	      (if (eof-object? datum)
		  '()
		(cons datum (recur)))))
	(close-input-port port)))))


;;;; done

(current-library-source-file-locator	locate-library-source-file)
(current-library-source-loader		read-library-source-file)
(current-library-serialized-loader	load-serialized-library)
(current-include-file-locator		locate-include-file)
(current-include-file-loader		read-include-file)

)

;;; end of file
