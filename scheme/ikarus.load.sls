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
    load-and-serialize-source-library
    load			load-r6rs-script
    library-path		library-extensions
    fasl-directory		fasl-path
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
	  current-source-library-file-locator
	  current-source-library-loader
	  current-binary-library-file-locator
	  current-binary-library-loader
	  current-library-locator
	  library-locator-options-no-raise-when-open-fails?
	  failed-library-location-collector
	  current-library-expander
	  serialize-collected-libraries
	  serialize-library
	  current-source-library-loader-by-filename
	  current-include-file-locator
	  current-include-file-loader
	  source-code-location)
    (only (psyntax expander)
	  expand-r6rs-top-level-make-evaluator)
    (only (ikarus.reader)
	  get-annotated-datum
	  read-script-source-file
	  read-library-source-port
	  read-library-source-file)
    (only (ikarus fasl read)
	  fasl-read-header
	  fasl-read-object)
    (only (ikarus.fasl.write)
	  fasl-write-header
	  fasl-write-object)
    (prefix (only (vicare options)
		  print-loaded-libraries
		  verbose?)
	    options.)
    (vicare arguments validation)
    (vicare unsafe operations))

  (include "ikarus.wordsize.scm")


;;;; arguments validation

(define-argument-validation (search-path who obj)
  (for-all string? obj)
  (procedure-argument-violation who
    "expected list of strings representing directory pathnames as search path" obj))

(define (%false-or-non-empty-string? S)
  (or (not S)
      (and (string? S)
	   (not (fxzero? (string-length S))))))


;;;; helpers

(define (%print-verbose-message message . format-args)
  (when (options.verbose?)
    (apply fprintf (current-error-port) message format-args)))


;;;; locating serialized libraries stored in FASL files

(module (locate-binary-library-file
	 fasl-search-path
	 fasl-directory
	 fasl-path)

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

  (define fasl-directory
    ;;The directory under which serialised FASL files must be saved.  It
    ;;can  be an  empty  string  or a  string  representing an  existing
    ;;directory.  This value is prepended to source file names to obtain
    ;;the pathname of a FASL file in a FASL repository.
    ;;
    ;;When set to  the empty string: the FASL file  is stored right next
    ;;to its source file, in the same directory.
    ;;
    (make-parameter DEFAULT-FASL-DIRECTORY
      (lambda (P)
	(define-constant __who__ 'fasl-directory)
	(cond ((not (string? P))
	       (error __who__
		 "expected string as destination FASL directory pathname" P))
	      ((string-empty? P)
	       P)
	      ((file-exists? P)
	       (posix.real-pathname P))
	      (else
	       (error __who__ "attempt to set non-existent directory pathname" P))))))

  (define (fasl-path source-pathname)
    ;;Given an existent relative or absolute source file pathname return
    ;;the associated full FASL file  pathname using the current value of
    ;;FASL-DIRECTORY as prefix.  Return false if FASL-DIRECTORY is unset
    ;;(which should never happen).
    ;;
    (let ((d (fasl-directory)))
      (and (not (string=? d ""))
	   (%make-fasl-pathname d source-pathname))))

  (define (%make-fasl-pathname prefix-pathname source-pathname)
    ;;We assume  that PREFIX-PATHNAME,  if it  exists, has  already been
    ;;normalised; if PREFIX-PATHNAME does not  exist: it will be created
    ;;when the FASL file is created.
    ;;
    (string-append prefix-pathname
		   (posix.real-pathname source-pathname)
		   FASL-EXTENSION))

  (define* (locate-binary-library-file (libref library-reference?))
    ;;Default   value    for   the   CURRENT-BINARY-LIBRARY-FILE-LOCATOR
    ;;parameter.  Given a R6RS library  reference: scan the FASL library
    ;;search path for the corresponding FASL file.
    ;;
    ;;Return 2 values.  When successful:  a string representing the fasl
    ;;file pathname;  a thunk to be  called to continue the  search from
    ;;the next  directory in the  search path.  Otherwise  return: false
    ;;and false.
    ;;
    (let loop ((tailname.str     (let ()
				   (import LIBRARY-REFERENCE-TO-FILENAME-STEM)
				   (library-reference->filename-stem libref)))
	       (directories      (fasl-search-path)))
      (if (null? directories)
	  ;;No suitable library was found.
	  (values #f #f)
	;;Check the  file existence  in the  current directory  with the
	;;current  file  extension;  if  not found  try  the  next  file
	;;extension.
	(let* ((pathname (string-append ($car directories)
					tailname.str
					FASL-EXTENSION))
	       (continue (let ((dirs ($cdr directories)))
			   (lambda ()
			     (loop tailname.str dirs)))))
	  (if (file-exists? pathname)
	      (values pathname continue)
	    (begin
	      ((failed-library-location-collector) pathname)
	      (continue)))))))

  #| end of module |# )


;;;; locating source libraries stored in files

(module (locate-source-library-file
	 library-path
	 library-extensions)
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

  (define* (locate-source-library-file (libref library-reference?))
    ;;Default   value    for   the   CURRENT-SOURCE-LIBRARY-FILE-LOCATOR
    ;;parameter.   Given  a  R6RS  library reference:  scan  the  source
    ;;library search  path for the  corresponding file.
    ;;
    ;;Return 2 values.  When successful:  a string representing the fasl
    ;;file pathname;  a thunk to be  called to continue the  search from
    ;;the next  directory in the  search path.  Otherwise  return: false
    ;;and false.
    ;;
    (let loop ((tailname.str  (let ()
				(import LIBRARY-REFERENCE-TO-FILENAME-STEM)
				(library-reference->filename-stem libref)))
	       (directories   (library-path))
	       (extensions    (library-extensions)))
      (cond ((null? directories)
	     (values #f #f))

	    ((null? extensions)
	     ;;No more extensions: try the  next directory in the search
	     ;;path again with the full list of extensions.
	     (loop tailname.str ($cdr directories) (library-extensions)))

	    (else
	     ;;Build the file  pathname with the next  directory and the
	     ;;next  extension, then  check  the its  existence; if  not
	     ;;found try the next file extension.
	     (let* ((pathname (string-append ($car directories)
					     tailname.str
					     ($car extensions)))
		    (continue (let ((exts  ($cdr extensions)))
				(lambda ()
				  (loop tailname.str directories exts)))))
	       (if (file-exists? pathname)
		   (values pathname continue)
		 (begin
		   ((failed-library-location-collector) pathname)
		   (continue))))))))

  #| end of module: LOCATE-SOURCE-LIBRARY-FILE |# )


;;;; converting library references to file names

(module LIBRARY-REFERENCE-TO-FILENAME-STEM
  (library-reference->filename-stem)

  (define* (library-reference->filename-stem (libref library-reference?))
    ;;Convert  the non-empty  list of  identifiers from  a R6RS  library
    ;;reference into  a string  representing the  corresponding relative
    ;;file  pathname,  without extension  but  including  a leading  #\/
    ;;character.  Examples:
    ;;
    ;;   (library-reference->filename-stem '(alpha beta gamma))
    ;;   => "/alpha/beta/gamma"
    ;;
    ;;   (library-reference->filename-stem '(alpha beta main))
    ;;   => "/alpha/beta/main_"
    ;;
    ;;notice how the component "main",  when appearing last, is "quoted"
    ;;by appending an underscore.
    ;;
    ;;The  returned  value  can  be  used as  source  library  name,  by
    ;;appending an extension  like ".sls", and as FASL  library name, by
    ;;appending an extension like ".vicare-64-bit-fasl".
    ;;
    (define libref.ids
      (library-reference->identifiers libref))
    (receive (port extract)
	(open-string-output-port)
      (let next-component ((component		($car libref.ids))
			   (ls			($cdr libref.ids))
			   (first-component?	#t))
	(write-char #\/ port)
	(let ((component-name (symbol->string component)))
	  (for-each (lambda (N)
		      (let ((ch ($fixnum->char N)))
			(if (or ($char<= #\a ch #\z)
				($char<= #\A ch #\Z)
				($char<= #\0 ch #\9)
				($char=  ch #\.)
				($char=  ch #\-)
				($char=  ch #\+)
				($char=  ch #\_))
			    (write-char ch port)
			  (receive (D M)
			      (div-and-mod N 16)
			    (write-char #\% port)
			    (display-hex D port)
			    (display-hex M port)))))
	    (bytevector->u8-list (string->utf8 component-name)))
	  (if (null? ls)
	      (when (and (not first-component?)
			 (main*? component-name))
		(write-char #\_ port))
	    (next-component ($car ls) ($cdr ls) #f))))
      (extract)))

  (define (display-hex N port)
    (if ($fx<= 0 N 9)
	(display N port)
      (write-char ($fixnum->char ($fx+ ($char->fixnum #\a) ($fx- N 10))) port)))

  (define (main*? component-name)
    (and ($fx>= ($string-length component-name) 4)
	 ($string= ($substring component-name 0 4) "main")
	 (for-all (lambda (ch)
		    ($char= ch #\_))
	   (string->list ($substring component-name 4 ($string-length component-name))))))

  #| end of module: LIBRARY-REFERENCE->FILENAME-STEM |# )


;;;; locating source and binary libraries: run-time locator

(module (run-time-library-locator)

  (define* (run-time-library-locator (libref library-reference?) options)
    ;;Possible  value for  the  parameter CURRENT-LIBRARY-LOCATOR;  this
    ;;function is meant to be used  to search for libraries when running
    ;;an application.
    ;;
    ;;Given a  R6RS library reference:  scan the search path  for binary
    ;;libraries in search of a matching FASL library file; if a compiled
    ;;library is  not found: scan  the search path for  source libraries
    ;;libraries in search of a matching source library file.
    ;;
    ;;OPTIONS must be an enum set returned by LIBRARY-LOCATION-OPTIONS.
    ;;
    ;;When successful return 2 values:
    ;;
    ;;1. An input port  from which the library can be  read; if the port
    ;;   is binary: a compiled library can  be read from it; if the port
    ;;    is textual  a  source library  can  be read  from  it.  It  is
    ;;   responsibility of the caller to close the returned port when no
    ;;   more needed.
    ;;
    ;;2. A thunk to be called to continue the search.  This thunk allows
    ;;    the caller  to  reject a  library  if it  does  not meet  some
    ;;   additional constraint; for example:  if its version number does
    ;;   not conform to LIBREF.
    ;;
    ;;When no matching library is found: return false and false.
    ;;
    (let ((binary-locator (current-binary-library-file-locator))
	  (source-locator (current-source-library-file-locator)))
      (define (%source-search-start)
	(%source-search-step options
			     (lambda ()
			       (source-locator libref))
			     (lambda ()
			       (values #f #f))))
      (define (%binary-search-start)
	(%binary-search-step options
			     (lambda ()
			       (binary-locator libref))
			     %source-search-start))
      (%binary-search-start)))

;;; --------------------------------------------------------------------

  (define (%binary-search-step options next-binary-file-match search-fail-kont)
    (receive (binary-pathname further-binary-file-match)
	(next-binary-file-match)
      (if binary-pathname
	  (%handle-binary-file-match options binary-pathname
				     (lambda ()
				       (%binary-search-step options further-binary-file-match search-fail-kont)))
	(search-fail-kont))))

  (define (%source-search-step options next-source-file-match search-fail-kont)
    (receive (source-pathname further-source-file-match)
	(next-source-file-match)
      (if source-pathname
	  (%handle-source-file-match options source-pathname
				     (lambda ()
				       (%source-search-step options further-source-file-match search-fail-kont)))
	(search-fail-kont))))

;;; --------------------------------------------------------------------

  (define (%handle-binary-file-match options binary-pathname next-binary-search-step)
    (define (%continue)
      ;;If we are here it means the previous pathname was rejected.
      ((failed-library-location-collector) binary-pathname)
      (next-binary-search-step))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%open-binary binary-pathname)))
	    %continue))

  (define (%handle-source-file-match options source-pathname next-source-search-step)
    (define (%continue)
      ;;If we are here it means the previous pathname was rejected.
      ((failed-library-location-collector) source-pathname)
      (next-source-search-step))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%open-source source-pathname)))
	    %continue))

;;; --------------------------------------------------------------------

  (define-syntax-rule (%open-binary ?pathname)
    (receive-and-return (port)
	(open-file-input-port ?pathname
	  (file-options)
	  (buffer-mode block))
      (fasl-read-header port)))

  (define-syntax-rule (%open-source ?pathname)
    (open-file-input-port ?pathname
      (file-options)
      (buffer-mode block)
      (native-transcoder)))

  #| end of module: RUN-TIME-LIBRARY-LOCATOR |# )


;;;; locating source and binary libraries: compile-time locator

(module (compile-time-library-locator)

  (define* (compile-time-library-locator (libref library-reference?) options)
    ;;Possible  value for  the  parameter CURRENT-LIBRARY-LOCATOR;  this
    ;;function  is meant  to  be  used to  search  for  libraries to  be
    ;;compiled for installation, for example by a package.
    ;;
    ;;Given a  R6RS library reference:
    ;;
    ;;1. Scan the search path for source libraries in search of a source
    ;;   library file.
    ;;
    ;;2. If a  matching source is found: look in  the FASL-DIRECTORY for
    ;;   an already compiled FASL file.
    ;;
    ;;3. If the FASL  file exists and it is newer  than the source file:
    ;;   accept it.
    ;;
    ;;4. If the FASL file does not  exist or it is older than the source
    ;;   file: accept the source file.
    ;;
    ;;OPTIONS must be an enum set returned by LIBRARY-LOCATION-OPTIONS.
    ;;
    ;;When  successful (a  source or  fasl  file is  accepted) return  2
    ;;values:
    ;;
    ;;1. An input port  from which the library can be  read; if the port
    ;;   is binary: a compiled library can  be read from it; if the port
    ;;    is textual  a  source library  can  be read  from  it.  It  is
    ;;   responsibility of the caller to close the returned port when no
    ;;   more needed.
    ;;
    ;;2. A thunk to be called to continue the search.  This thunk allows
    ;;    the caller  to  reject a  library  if it  does  not meet  some
    ;;   additional constraint; for example:  if its version number does
    ;;   not conform to LIBREF.
    ;;
    ;;When no matching library is found: return false and false.
    ;;
    (let ((source-locator (current-source-library-file-locator)))
      (%source-search-step options
			   (lambda ()
			     (source-locator libref))
			   (lambda ()
			     (values #f #f)))))

;;; --------------------------------------------------------------------

  (define (%source-search-step options next-source-file-match search-fail-kont)
    (receive (source-pathname further-source-file-match)
	(next-source-file-match)
      (if source-pathname
	  (let ((fasl-pathname (fasl-path source-pathname)))
	    (if (or (not (file-exists? fasl-pathname))
		    (receive-and-return (rv)
			(< (posix.file-modification-time fasl-pathname)
			   (posix.file-modification-time source-pathname))
		      (when rv
			(%print-verbose-message "WARNING: not using fasl file ~s \
                                                 because it is older \
                                                 than the source file ~s\n"
						fasl-pathname source-pathname))))
		(begin
		  ((failed-library-location-collector) fasl-pathname)
		  (%handle-source-file-match options source-pathname further-source-file-match search-fail-kont))
	      (%handle-binary-file-match options fasl-pathname source-pathname
					 further-source-file-match search-fail-kont)))
	(search-fail-kont))))

;;; --------------------------------------------------------------------

  (define (%handle-source-file-match options source-pathname next-source-file-match search-fail-kont)
    (define (%continue)
      ;;If we are here it means the previous pathname was rejected.
      ((failed-library-location-collector) source-pathname)
      (%source-search-step options next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%open-source source-pathname)))
	    %continue))

  (define (%handle-binary-file-match options fasl-pathname source-pathname
				     next-source-file-match search-fail-kont)
    (define (%continue)
      ((failed-library-location-collector) fasl-pathname)
      (%handle-source-file-match options source-pathname next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%open-binary fasl-pathname)))
	    %continue))

;;; --------------------------------------------------------------------

  (define-syntax-rule (%open-binary ?pathname)
    (receive-and-return (port)
	(open-file-input-port ?pathname
	  (file-options)
	  (buffer-mode block))
      (fasl-read-header port)))

  (define-syntax-rule (%open-source ?pathname)
    (open-file-input-port ?pathname
      (file-options)
      (buffer-mode block)
      (native-transcoder)))

  #| end of module: COMPILE-TIME-LIBRARY-LOCATOR |# )


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
  (parametrise ((current-library-locator (if serialize?
					     compile-time-library-locator
					   run-time-library-locator)))
    (let* ((prog  (read-script-source-file filename))
	   (thunk (parametrise ((source-code-location filename))
		    (expand-r6rs-top-level-make-evaluator prog))))
      (when serialize?
	(serialize-collected-libraries (lambda (source-pathname libname contents)
					 (store-serialized-library (fasl-path source-pathname)
								   source-pathname libname contents))
				       (lambda (core-expr)
					 (compile-core-expr core-expr))))
      (when run?
	(thunk)))))

(case-define* load
  ;;Load source code from FILENAME,  which must be a string representing
  ;;a filename,  expecting: an R6RS program,  an R6RS library or  just a
  ;;list of forms.  Then transform the contents of the file in a list of
  ;;symbolic expressions; for each form in the source apply EVAL-PROC to
  ;;the corresponding symbolic expression.
  ;;
  ;;When EVAL-PROC is not given: the  forms are evaluated in the current
  ;;INTERACTION-ENVIRONMENT.
  ;;
  ((filename)
   (load filename (lambda (sexp)
		    (eval sexp (interaction-environment)))))
  (((filename string?) (eval-proc procedure?))
   (let next-form ((ls (read-script-source-file filename)))
     (unless (null? ls)
       (eval-proc (car ls))
       (next-form (cdr ls))))))


;;;; loading libraries from source

(define* (default-source-library-loader (libref library-reference?) (port input-port?))
  ;;Default value fo the parameter CURRENT-SOURCE-LIBRARY-LOADER.  Given
  ;;a textual  input PORT: read  from it a LIBRARY  symbolic expression;
  ;;verify that  its version  reference conforms  to LIBREF;  expand it;
  ;;compile it; install it.
  ;;
  ;;If successful return true; otherwise return false.
  ;;
  ;;We assume that  applying the function PORT-ID to PORT  will return a
  ;;string  representing  a  file  name   associated  to  the  port  (or
  ;;equivalent).
  ;;
  (let ((source-filename  (port-id port))
	(library-sexp     (read-library-source-port port))
	(reject-key       (gensym)))
    ;;If the version  of the loaded library does not  conform to LIBREF:
    ;;we make the expander raise  an exception with REJECT-KEY as raised
    ;;object; we catch it here and return false.
    (guard (E ((eq? E reject-key)
	       #f))
      ((current-library-expander) library-sexp source-filename
       (lambda (libname)
	 ;;We expect LIBNAME to be  the R6RS library name extracted from
	 ;;LIBRARY-SEXP.
	 (unless (conforming-library-name-and-library-reference? libname libref)
	   (raise reject-key)))))))


;;;; loading libraries from serialised locations

(define* (default-binary-library-loader (libref library-reference?) (port input-port?)
	   (success-kont procedure?))
  ;;Default value fo the parameter CURRENT-BINARY-LIBRARY-LOADER.  Given
  ;;a binary input PORT: read from  it a serialized library; verify that
  ;;its version reference conforms to LIBREF; install it.
  ;;
  ;;If successful return true; otherwise return false.
  ;;
  ;;We assume that  applying the function PORT-ID to PORT  will return a
  ;;string  representing  a  file  name   associated  to  the  port  (or
  ;;equivalent).
  ;;
  (define fasl-pathname (port-id port))
  (define reject-key    (gensym))
  ;;If the version of the loaded  library does not conform to LIBREF: we
  ;;make the loader raise an exception with REJECT-KEY as raised object;
  ;;we catch it here and return false.
  (guard (E ((eq? E reject-key)
	     #f))
    (let ((rv (load-serialized-library port success-kont
		(lambda (libname)
		  ;;We  expect  LIBNAME  to  be  the  R6RS  library  name
		  ;;extracted the FASL file.
		  (unless (conforming-library-name-and-library-reference? libname libref)
		    (raise reject-key))))))
      (or rv
	  (begin
	    (%print-verbose-message "WARNING: not using fasl file ~s because invalid or \
                                  compiled with a different instance of Vicare\n"
				    fasl-pathname)
	    #f)))))


;;;; loading source libraries then serializing them

(define* (load-and-serialize-source-library (source-pathname string?)
					    (fasl-pathname %false-or-non-empty-string?))
  ;;Load a source library filename, expand it, compile it, serialize it.
  ;;Return unspecified values.
  ;;
  (%print-verbose-message "loading source library: ~a\n" source-pathname)
  (let ((lib           ((current-source-library-loader-by-filename) source-pathname
			;;This  is  a  function  to apply  to  the  R6RS
			;;library name  of the loaded library  to verify
			;;if its version conforms  to the requested one.
			;;There  is no  requested  version  here, so  we
			;;always return doing nothing.
			(lambda (libname) (void))))
	(fasl-pathname (or fasl-pathname (fasl-path source-pathname))))
    (%print-verbose-message "serializing library: ~a\n" fasl-pathname)
    (serialize-library lib
		       (lambda (source-pathname libname contents)
			 (store-serialized-library fasl-pathname source-pathname
						   libname contents))
		       (lambda (core-expr)
			 (compile-core-expr core-expr)))))


;;;; loading and storing precompiled library files

(module (load-serialized-library
	 store-serialized-library)

  (define-struct serialized-library
    (contents
		;A list of values  representing a LIBRARY record holding
		;precompiled  code.   For  details  on  the  format  see
		;SERIALIZE-LIBRARY in "psyntax.library-manager.sls".
     ))

  (define (load-serialized-library port success-kont verify-libname)
    ;;Given a string representing the  existent pathname of a FASL file:
    ;;load it looking for a precompiled library, then apply SUCCESS-KONT
    ;;to the library contents.
    ;;
    ;;If successful: return the  result of the SUCCESS-KONT application.
    ;;If the  file has invalid  contents: return false.  If  opening the
    ;;file fails: return false.
    ;;
    ;;VERIFY-LIBNAME must be a procedure accepting a single argument; it
    ;;must verify that  the argument is a R6RS library  name and that it
    ;;conforms to  some additional constraint (especially  the version);
    ;;it must  raise an  exception if something  is wrong;  otherwise it
    ;;should just return unspecified values.
    ;;
    (let ((libname (fasl-read-object port)))
      (verify-libname libname)
      (let ((x (fasl-read-object port)))
	(and (serialized-library? x)
	     (apply success-kont (serialized-library-contents x))))))

  (define (store-serialized-library fasl-pathname source-pathname libname contents)
    ;;Given the  FASL pathname of  a compiled library to  be serialized:
    ;;store the CONTENTS into it, creating  a new file or overwriting an
    ;;existing one.
    ;;
    ;;FASL-PATHNAME must  be a string  representing the pathname  of the
    ;;binary library FASL file.
    ;;
    ;;SOURCE-PATHNAME must be a string  representing the pathname of the
    ;;source library file (or equivalent).
    ;;
    ;;LIBNAME must be a R6RS library name.
    ;;
    ;;CONTENTS must  be a list  of values representing a  LIBRARY record
    ;;holding precompiled  code.  See the function  SERIALIZE-LIBRARY in
    ;;"psyntax.library-manager.sls" for details on the format.
    ;;
    (%print-verbose-message "serialising ~a ..." fasl-pathname)
    (receive (dir name)
	(posix.split-pathname-root-and-tail fasl-pathname)
      (unless (string-empty? dir)
	(posix.mkdir/parents dir #o755)))
    (let ((port (open-file-output-port fasl-pathname (file-options no-fail))))
      (unwind-protect
	  (begin
	    (fasl-write-header port)
	    (fasl-write-object libname port)
	    (fasl-write-object (make-serialized-library contents) port
			       (retrieve-filename-foreign-libraries source-pathname)))
	(close-output-port port)))
    (%print-verbose-message "done\n"))

  #| end of module |# )


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

(current-library-locator		run-time-library-locator)

(current-source-library-file-locator	locate-source-library-file)
(current-source-library-loader		default-source-library-loader)

(current-binary-library-file-locator	locate-binary-library-file)
(current-binary-library-loader		default-binary-library-loader)

(current-include-file-locator		locate-include-file)
(current-include-file-loader		read-include-file)

)

;;; end of file
;; Local Variables:
;; eval: (put 'load-serialized-library 'scheme-indent-function 2)
;; End:
