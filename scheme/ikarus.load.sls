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


#!vicare
(library (ikarus load)
  (export
    load-and-serialize-source-library
    serialize-library-record
    run-time-library-locator
    compile-time-library-locator
    source-library-locator
    default-source-library-file-locator
    default-binary-library-file-locator
    load			load-r6rs-script
    library-path		library-extensions
    fasl-directory		fasl-path
    fasl-search-path		fasl-stem+extension)
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  load			load-r6rs-script
		  library-path		library-extensions
		  fasl-directory	fasl-path
		  fasl-search-path	get-annotated-datum)
    (prefix (only (ikarus.posix)
		  file-string-pathname?
		  directory-exists?
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
	  library?
	  library-name
	  library-source-file-name
	  find-library-by-name
	  current-library-collection
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
	  current-library-record-serializer
	  current-include-file-locator
	  current-include-file-loader
	  source-code-location
	  install-binary-library-and-its-dependencies)
    (only (psyntax expander)
	  expand-r6rs-top-level-make-evaluator)
    (only (ikarus.reader)
	  get-annotated-datum
	  read-script-source-file
	  read-library-source-port
	  read-library-source-file)
    (only (ikarus library-utils)
	  library-reference?
	  library-name?
	  library-reference->identifiers
	  conforming-library-name-and-library-reference?)
    (only (ikarus fasl read)
	  fasl-read-header
	  fasl-read-object)
    (only (ikarus.fasl.write)
	  fasl-write-header
	  fasl-write-object)
    (prefix (only (vicare options)
		  verbose-about-libraries?
		  print-loaded-libraries
		  verbose?)
	    options.)
    (vicare arguments validation)
    (vicare unsafe operations))

  (include "ikarus.wordsize.scm" #t)


;;;; arguments validation

(define (false-or-file-string-pathname? S)
  (or (not S)
      (and (posix.file-string-pathname? S))))

(define (search-path? obj)
  (and (list? obj)
       (for-all posix.file-string-pathname? obj)))


;;;; helpers

(define (%print-verbose-message message . format-args)
  (when (options.verbose?)
    (apply fprintf (current-error-port) message format-args)))

(module (%log-library-debug-message)

  (define-syntax %log-library-debug-message
    (if (options.verbose-about-libraries?)
	(syntax-rules ()
	  ((_ ?template ?arg ...)
	   (%logger ?template ?arg ...)))
      (lambda (stx) #'(void))))

  (define (%logger template . args)
    (apply fprintf (current-error-port)
	   (string-append "vicare ***: " template "\n")
	   args))

  #| end of module |# )


;;;; converting library references to file names

(module LIBRARY-REFERENCE-TO-FILENAME-STEM
  (library-reference->filename-stem)

  (define* (library-reference->filename-stem {libref library-reference?})
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


;;;; converting source pathnames to binary pathnames

(module SOURCE-PATHNAME->BINARY-PATHNAME
  (source-pathname->binary-pathname)

  (define* (source-pathname->binary-pathname {source-pathname posix.file-string-pathname?})
    (define (%error ptn)
      (assertion-violation __who__
	"unable to build valid FASL file pathname from source pathname"
	source-pathname ptn))
    (let ((ptn (cond ((%string-suffix? source-pathname ".vicare.sls")
		      (%desuffix       source-pathname ".vicare.sls"))
		     ((%string-suffix? source-pathname ".sls")
		      (%desuffix       source-pathname ".sls"))
		     ((%string-suffix? source-pathname ".vicare.ss")
		      (%desuffix       source-pathname ".vicare.ss"))
		     ((%string-suffix? source-pathname ".ss")
		      (%desuffix       source-pathname ".ss"))
		     ((%string-suffix? source-pathname ".vicare.scm")
		      (%desuffix       source-pathname ".vicare.scm"))
		     ((%string-suffix? source-pathname ".scm")
		      (%desuffix       source-pathname ".scm"))
		     (else
		      source-pathname))))
      (if (posix.file-string-pathname? ptn)
	  (let ((binary-pathname (string-append (fasl-directory) "/" ptn ".fasl")))
	    (if (posix.file-string-pathname? binary-pathname)
		binary-pathname
	      (%error binary-pathname)))
	(%error ptn))))

  (define (%string-suffix? str suffix)
    (let ((str.len    (string-length str))
	  (suffix.len (string-length suffix)))
      (and (fx< suffix.len str.len)
	   (string=? suffix (substring str (fx- str.len suffix.len) str.len)))))

  (define (%desuffix str suffix)
    (substring str 0 (fx- (string-length str) (string-length suffix))))

  #| end of module |# )


;;;; locating serialized libraries stored in FASL files

(module (default-binary-library-file-locator
	 fasl-search-path
	 fasl-directory
	 fasl-path
	 fasl-stem+extension)
  (import LIBRARY-REFERENCE-TO-FILENAME-STEM)

  (define (%existent-directory-pathname? dir-pathname)
    (and dir-pathname
	 (not (string-empty? dir-pathname))
	 (posix.directory-exists? dir-pathname)))

  (define-constant FASL-EXTENSION
    ;;The file extension of serialised FASL files.
    ;;
    ;;NOTE   In    previous   versions   there   were    2   extensions:
    ;;".vicare-32bit-fasl" for 32-bit platforms and ".vicare-64bit-fasl"
    ;;for 64-bit  platforms.  But  since version 0.4  there is  a single
    ;;extension.  (Marco Maggi; Thu Feb 20, 2014)
    ;;
    ".fasl")

  (define-constant DEFAULT-FASL-DIRECTORY
    ;;Default  value  for  the   FASL-DIRECTORY  parameter;  it  is  the
    ;;directory under which new FASL  files holding binary libraries are
    ;;created when using the "compile dependencies" execution mode.
    ;;
    ;;It is initialised with with  the value of the environment variable
    ;;VICARE_FASL_DIRECTORY, if  set and  holding an  existent pathname;
    ;;otherwise it is initialised with the pathname:
    ;;
    ;;   ~/.vicare/precompiled
    ;;
    ;;if  it is  possible  to determine  the value  of  the user's  home
    ;;directory;      otherwise      it      is      initialised      to:
    ;;
    ;;   /tmp/vicare/precompiled
    ;;
    (let ((P (posix.getenv "VICARE_FASL_DIRECTORY")))
      (if (%existent-directory-pathname? P)
	  (posix.real-pathname P)
	(let ((P (posix.getenv "HOME")))
	  (if (%existent-directory-pathname? P)
	      (string-append (posix.real-pathname P) "/.vicare/precompiled")
	    "/tmp/vicare/precompiled")))))

  (define fasl-directory
    ;;The directory under which serialised FASL files must be saved.  It
    ;;must  be  a  string  representing  the  pathname  of  an  existing
    ;;directory; such  pathanme is normalised  upon storing it  into the
    ;;parameter.  This value is prepended to source file names to obtain
    ;;the pathname of a FASL file in a FASL repository.
    ;;
    ;;When this parameter is set to  the dot string: the FASL files will
    ;;be stored  in the  current working  directory at  the time  of the
    ;;parameter mutation.
    ;;
    (make-parameter DEFAULT-FASL-DIRECTORY
      (lambda (P)
	(define-constant __who__ 'fasl-directory)
	(cond ((not (posix.file-string-pathname? P))
	       (error __who__
		 "expected string as destination FASL directory pathname" P))
	      ((posix.directory-exists? P)
	       (posix.real-pathname P))
	      (else
	       (error __who__ "attempt to set non-existent directory pathname" P))))))

  (define fasl-search-path
    ;;The search path  to in which to look for  FASL files.  Notice that
    ;;we  do not  test for  directories existence:  a directory  may not
    ;;exist at the time this search  path is initialised, but be created
    ;;later.
    ;;
    (make-parameter
	(let ()
	  (module (target-os-uid
		   scheme-lib-dir
		   vicare-lib-dir)
	    (include "ikarus.config.ss"))
	  (case target-os-uid
	    ((linux bsd darwin cygwin)
	     (list scheme-lib-dir vicare-lib-dir))
	    (else
	     (error 'fasl-search-path
	       "internal error: invalid target OS UID" target-os-uid))))
      (lambda* ({P search-path?})
	P)))

  (define* (fasl-path {libref library-reference?})
    ;;Given  a  R6RS  library  reference:  build  and  return  a  string
    ;;representing  the  pathname   of  the  FASL  file   in  which  the
    ;;corresponding binary library can be  stored.  The directory of the
    ;;pathname is the current value of the parameter FASL-DIRECTORY.
    ;;
    (string-append (fasl-directory)
		   (library-reference->filename-stem libref)
		   FASL-EXTENSION))

  (define* (fasl-stem+extension {libref library-reference?})
    ;;Given  a  R6RS  library  reference:  build  and  return  a  string
    ;;representing  the pathname  stem of  the  FASL file  in which  the
    ;;corresponding  binary library  can  be stored.   The  "stem" is  a
    ;;string  to append  to  a  directory pathname  to  obtain the  full
    ;;pathname; for example, the stem of the library reference:
    ;;
    ;;   (alpha beta gamma (1 2 3))
    ;;
    ;;on a 64-bit platform is currently:
    ;;
    ;;   "/alpha/beta/gamma.vicare-64bit-fasl"
    ;;
    (string-append (library-reference->filename-stem libref)
		   FASL-EXTENSION))

  (define* (default-binary-library-file-locator {libref library-reference?})
    ;;Default   value    for   the   CURRENT-BINARY-LIBRARY-FILE-LOCATOR
    ;;parameter.  Given a R6RS library  reference: scan the FASL library
    ;;search path for the corresponding FASL file.
    ;;
    ;;Return 2 values.  When successful:  a string representing the fasl
    ;;file pathname;  a thunk to be  called to continue the  search from
    ;;the next  directory in the  search path.  Otherwise  return: false
    ;;and false.
    ;;
    (%log-library-debug-message "~a: locating binary library file for: ~a" __who__ libref)
    (let loop ((stem.str     (library-reference->filename-stem libref))
	       (directories  (fasl-search-path)))
      (if (null? directories)
	  ;;No suitable library file was found.
	  (begin
	    (%log-library-debug-message "~a: exhausted search path, no binary library file found for: ~a" __who__ libref)
	    (values #f #f))
	;;Check the  file existence  in the  current directory  with the
	;;current  file  extension;  if  not found  try  the  next  file
	;;extension.
	(let* ((binary-pathname (string-append ($car directories)
					       stem.str
					       FASL-EXTENSION))
	       (continue        (let ((dirs ($cdr directories)))
				  (lambda ()
				    (loop stem.str dirs)))))
	  (%log-library-debug-message "~a: trying: ~a" __who__ binary-pathname)
	  (if (file-exists? binary-pathname)
	      (begin
		(%log-library-debug-message "~a: found: ~a" __who__ binary-pathname)
		(values binary-pathname continue))
	    (begin
	      ((failed-library-location-collector) binary-pathname)
	      (%log-library-debug-message "~a: unexistent: ~a" __who__ binary-pathname)
	      (continue)))))))

  #| end of module |# )


;;;; locating source libraries stored in files

(module (default-source-library-file-locator
	 library-path
	 library-extensions)

  (define library-path
    ;;Hold a list of strings  representing directory pathnames being the
    ;;search path.
    ;;
    (make-parameter
	'()
      (lambda* ({P search-path?})
	P)))

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

  (define* (default-source-library-file-locator {libref library-reference?})
    ;;Default   value    for   the   CURRENT-SOURCE-LIBRARY-FILE-LOCATOR
    ;;parameter.   Given  a  R6RS  library reference:  scan  the  source
    ;;library search  path for the  corresponding file.
    ;;
    ;;Return 2 values.  When successful:  a string representing the fasl
    ;;file pathname;  a thunk to be  called to continue the  search from
    ;;the next  directory in the  search path.  Otherwise  return: false
    ;;and false.
    ;;
    (%log-library-debug-message "~a: locating source library file for: ~a" __who__ libref)
    (let loop ((tailname.str  (let ()
				(import LIBRARY-REFERENCE-TO-FILENAME-STEM)
				(library-reference->filename-stem libref)))
	       (directories   (library-path))
	       (extensions    (library-extensions)))
      (cond ((null? directories)
	     (%log-library-debug-message "~a: exhausted search path, no source library file found for: ~a" __who__ libref)
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
		   (begin
		     (%log-library-debug-message "~a: found: ~a" __who__ pathname)
		     (values pathname continue))
		 (begin
		   ((failed-library-location-collector) pathname)
		   (continue))))))))

  #| end of module: DEFAULT-SOURCE-LIBRARY-FILE-LOCATOR |# )


;;;; locating source and binary libraries: utilities

(module LIBRARY-LOCATOR-UTILS
  (%source-search-start
   %binary-search-start
   %open-source-library
   %open-binary-library)

  (module (%source-search-start)
    ;;This function can  be used to visit the directories  in the source
    ;;libraries  search path  and find  a  source file  matching a  R6RS
    ;;library reference.
    ;;
    ;;LIBREF  must be  a R6RS  library reference.   FAIL-KONT must  be a
    ;;thunk to be called when the  search fails.  OPTIONS must be a list
    ;;of symbols; at present the supported options are:
    ;;
    ;;   move-on-when-open-fails
    ;;
    ;;The return value is a thunk to call to start the search.  When the
    ;;returned thunk  finds a matching  library source file,  it returns
    ;;two values:
    ;;
    ;;1. A textual input port from which the source library can be read;
    ;;   it is  responsibility of the caller to close  the returned port
    ;;   when no more needed.
    ;;
    ;;2. A thunk to be called to continue the search.  This thunk allows
    ;;    the caller  to  reject a  library  if it  does  not meet  some
    ;;   additional constraint; for example:  if its version number does
    ;;   not conform to LIBREF.
    ;;
    ;;When  no  matching library  is  found:  the returned  thunk  calls
    ;;FAIL-KONT  and  returns  its   return  values;  the  default  fail
    ;;continuation returns false and false.
    ;;
    (case-define* %source-search-start
      (({libref library-reference?} options)
       (%source-search-start libref options (lambda ()
					      (values #f #f))))
      (({libref library-reference?} options {fail-kont procedure?})
       (let ((source-locator (current-source-library-file-locator)))
	 (lambda ()
	   (%source-search-step options
				(lambda ()
				  (source-locator libref))
				fail-kont)))))

    (define (%source-search-step options next-source-file-match search-fail-kont)
      (receive (source-pathname further-source-file-match)
	  (next-source-file-match)
	(if source-pathname
	    (%handle-source-file-match options source-pathname
				       (lambda ()
					 (%source-search-step options further-source-file-match search-fail-kont)))
	  (begin
	    ((failed-library-location-collector) source-pathname)
	    (search-fail-kont)))))

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
		  (%open-source-library source-pathname)))
	      %continue))

    #| end of module: %SOURCE-SEARCH-START |# )

;;; --------------------------------------------------------------------

  (module (%binary-search-start)
    ;;This function can  be used to visit the directories  in the binary
    ;;libraries search path and find a FASL file matching a R6RS library
    ;;reference.
    ;;
    ;;LIBREF  must be  a R6RS  library reference.   FAIL-KONT must  be a
    ;;thunk to be called when the  search fails.  OPTIONS must be a list
    ;;of symbols; at present the supported options are:
    ;;
    ;;   move-on-when-open-fails
    ;;
    ;;The return value is a thunk to call to start the search.  When the
    ;;returned thunk finds a matching  library FASL file, it returns two
    ;;values:
    ;;
    ;;1. A binary input port from  which the binary library can be read;
    ;;   it is  responsibility of the caller to close  the returned port
    ;;   when no more needed.
    ;;
    ;;2. A thunk to be called to continue the search.  This thunk allows
    ;;    the caller  to  reject a  library  if it  does  not meet  some
    ;;   additional constraint; for example:  if its version number does
    ;;   not conform to LIBREF.
    ;;
    ;;When  no  matching library  is  found:  the returned  thunk  calls
    ;;FAIL-KONT  and  returns  its   return  values;  the  default  fail
    ;;continuation returns false and false.
    ;;
    (case-define* %binary-search-start
      (({libref library-reference?} options)
       (%binary-search-start libref options (lambda ()
					      (values #f #f))))
      (({libref library-reference?} options {fail-kont procedure?})
       (let ((binary-locator (current-binary-library-file-locator)))
	 (lambda ()
	   (%binary-search-step options
				(lambda ()
				  (binary-locator libref))
				fail-kont)))))

    (define (%binary-search-step options next-binary-file-match search-fail-kont)
      (receive (binary-pathname further-binary-file-match)
	  (next-binary-file-match)
	(if binary-pathname
	    (%handle-binary-file-match options binary-pathname
				       (lambda ()
					 (%binary-search-step options further-binary-file-match search-fail-kont)))
	  (begin
	    ((failed-library-location-collector) binary-pathname)
	    (search-fail-kont)))))

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
		  (%open-binary-library binary-pathname)))
	      %continue))

    #| end of module: %BINARY-SEARCH-START |# )

;;; --------------------------------------------------------------------

  (define-syntax-rule (%open-binary-library ?pathname)
    (receive-and-return (port)
	(open-file-input-port ?pathname
	  (file-options)
	  (buffer-mode block))
      (fasl-read-header port)))

  (define-syntax-rule (%open-source-library ?pathname)
    (open-file-input-port ?pathname
      (file-options)
      (buffer-mode block)
      (native-transcoder)))

  #| end of module |# )


;;;; locating source and binary libraries: run-time locator

(define* (run-time-library-locator {libref library-reference?} options)
  ;;Possible  value  for  the  parameter  CURRENT-LIBRARY-LOCATOR;  this
  ;;function is meant to be used to search for libraries when running an
  ;;application.
  ;;
  ;;Given a R6RS library reference and  a list of search options: return
  ;;a thunk to be used to start the search for a matching library.
  ;;
  ;;The returned  thunk scans  the search path  for binary  libraries in
  ;;search of a  matching FASL library file; if a  binary library is not
  ;;found: it scans the search path  for source libraries in search of a
  ;;matching source library file.
  ;;
  ;;OPTIONS must be a list of  symbols; at present the supported options
  ;;are:
  ;;
  ;;   move-on-when-open-fails
  ;;
  ;;When successful the returned thunk return 2 values:
  ;;
  ;;1. An input port from which the  library can be read; if the port is
  ;;   binary: a  compiled library can be  read from it; if  the port is
  ;;    textual  a   source  library  can  be  read  from   it.   It  is
  ;;   responsibility of  the caller to close the returned  port when no
  ;;   more needed.
  ;;
  ;;2. A thunk  to be called to continue the  search.  This thunk allows
  ;;    the  caller  to reject  a  library  if  it  does not  meet  some
  ;;   additional  constraint; for example:  if its version  number does
  ;;   not conform to LIBREF.
  ;;
  ;;When no matching library is  found: the returned thunk returns false
  ;;and false.
  ;;
  (import LIBRARY-LOCATOR-UTILS)
  (%log-library-debug-message "~a: locating library for: ~a" __who__ libref)
  (%binary-search-start libref options (%source-search-start libref options)))


;;;; locating source and binary libraries: compile-time locator

(module (compile-time-library-locator)
  (import LIBRARY-LOCATOR-UTILS)
  (define-constant __who__ 'compile-time-library-locator)

  (define* (compile-time-library-locator {libref library-reference?} options)
    ;;Possible  value for  the  parameter CURRENT-LIBRARY-LOCATOR;  this
    ;;function  is meant  to  be  used to  search  for  libraries to  be
    ;;compiled for installation, for example by a package.
    ;;
    ;;Given a R6RS library reference: return a thunk to be used to start
    ;;the search for a matching library.
    ;;
    ;;The returned thunk does the following:
    ;;
    ;;1.  Scan the next directory  from the source libraries search path
    ;;   for a source library whose name matches LIBREF.
    ;;
    ;;2. If a  matching source is found: look in  the FASL-DIRECTORY for
    ;;   an already compiled FASL file.
    ;;
    ;;3. If the FASL file does not  exist or it is older than the source
    ;;   file:  accept the source  file and  prepare as next  search the
    ;;    search for  the source  file in  the next  directory from  the
    ;;   search path.
    ;;
    ;;4. If the FASL  file exists and it is newer  than the source file:
    ;;   accept  it and  prepare as  next search  the acceptance  of the
    ;;   source file.
    ;;
    ;;5.  If no  source file  exists: scan  the FASL  search path  for a
    ;;   binary library.
    ;;
    ;;Remember  that the  FASL  file  can be  rejected  if  it has  been
    ;;compiled by another boot image or it has the wrong library UID.
    ;;
    ;;OPTIONS  must be  a  list  of symbols;  at  present the  supported
    ;;options are:
    ;;
    ;;   move-on-when-open-fails
    ;;
    ;;When successful (a source or binary file is accepted) the returned
    ;;thunk returns 2 values:
    ;;
    ;;1. An input port  from which the library can be  read; if the port
    ;;   is binary: a compiled library can  be read from it; if the port
    ;;    is textual  a  source library  can  be read  from  it.  It  is
    ;;   responsibility of the caller to close the returned port when no
    ;;   more needed.
    ;;
    ;;   -  The boolean  true.  It  means the  library has  already been
    ;;     loaded and installed in the collection.
    ;;
    ;;2. A thunk to be called to continue the search.  This thunk allows
    ;;    the caller  to  reject a  library  if it  does  not meet  some
    ;;   additional constraint; for example:  if its version number does
    ;;   not conform to LIBREF.
    ;;
    ;;When  no matching  library is  found: the  returned thunk  returns
    ;;false and false.
    ;;
    (import LIBRARY-LOCATOR-UTILS)
    (%log-library-debug-message "~a: start search for library: ~a" __who__ libref)
    (let ((source-locator (current-source-library-file-locator))
	  (fail-kont      (%binary-search-start libref options)))
      (lambda ()
	(%binary/source-search-step options libref
				    (lambda ()
				      (source-locator libref))
				    fail-kont))))

;;; --------------------------------------------------------------------

  (define (%binary/source-search-step options libref next-source-file-match search-fail-kont)
    (receive (source-pathname further-source-file-match)
	(next-source-file-match)
      (if source-pathname
	  (begin
	    (%log-library-debug-message "~a: found source: ~a" __who__ source-pathname)
	    (let ((binary-pathname (fasl-path libref)))
	      (%log-library-debug-message "~a: checking binary: ~a" __who__ binary-pathname)
	      (if (and (file-exists? binary-pathname)
		       (receive-and-return (rv)
			   (< (posix.file-modification-time source-pathname)
			      (posix.file-modification-time binary-pathname))
			 (unless rv
			   (%print-verbose-message "WARNING: not using fasl file ~s \
                                                    because it is older \
                                                    than the source file ~s\n"
						   binary-pathname source-pathname))))
		  ;;We  try the  binary  library first,  and the  source
		  ;;library next.
		  (%handle-local-binary-file-match options libref binary-pathname source-pathname
						   further-source-file-match search-fail-kont)
		;;No  suitable binary  library, try  the source  library
		;;directly.
		(begin
		  ((failed-library-location-collector) binary-pathname)
		  (%handle-source-file-match options libref source-pathname further-source-file-match search-fail-kont)))))
	(begin
	  (%log-library-debug-message "~a: no source file for: ~a" __who__ libref)
	  (search-fail-kont)))))

;;; --------------------------------------------------------------------

  (define (%handle-local-binary-file-match options libref binary-pathname source-pathname
					   next-source-file-match search-fail-kont)
    (define (%continue)
      ((failed-library-location-collector) binary-pathname)
      (%log-library-debug-message "~a: rejected: ~a" __who__ binary-pathname)
      (%handle-source-file-match options libref source-pathname next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%log-library-debug-message "~a: opening: ~a" __who__ binary-pathname)
		(%open-binary-library binary-pathname)))
	    %continue))

  (define (%handle-source-file-match options libref source-pathname next-source-file-match search-fail-kont)
    (define (%continue)
      ;;If we are here it means the previous pathname was rejected.
      ((failed-library-location-collector) source-pathname)
      (%log-library-debug-message "~a: rejected: ~a" __who__ source-pathname)
      (%binary/source-search-step options libref next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%log-library-debug-message "~a: opening: ~a" __who__ source-pathname)
		(%open-source-library source-pathname)))
	    %continue))

  #| end of module: COMPILE-TIME-LIBRARY-LOCATOR |# )


;;;; locating source and binary libraries: source-onlye locator

(define* (source-library-locator {libref library-reference?} options)
  ;;Possible  value  for  the  parameter  CURRENT-LIBRARY-LOCATOR;  this
  ;;function is  meant to be used  to search for source  libraries first
  ;;and the for binary libraries.
  ;;
  ;;Given a R6RS library reference and  a list of search options: return
  ;;a thunk to be used to start the search for a matching library.
  ;;
  ;;The returned  thunk scans  the search path  for source  libraries in
  ;;search of a matching source library file; if a source library is not
  ;;found: it scans the search path  for binary libraries in search of a
  ;;matching FASL library file.
  ;;
  ;;OPTIONS must be a list of  symbols; at present the supported options
  ;;are:
  ;;
  ;;   move-on-when-open-fails
  ;;
  ;;When successful the returned thunk returns 2 values:
  ;;
  ;;1. An input port from which the  library can be read; if the port is
  ;;   binary: a  compiled library can be  read from it; if  the port is
  ;;    textual  a   source  library  can  be  read  from   it.   It  is
  ;;   responsibility of  the caller to close the returned  port when no
  ;;   more needed.
  ;;
  ;;2. A thunk  to be called to continue the  search.  This thunk allows
  ;;    the  caller  to reject  a  library  if  it  does not  meet  some
  ;;   additional  constraint; for example:  if its version  number does
  ;;   not conform to LIBREF.
  ;;
  ;;When no matching library is  found: the returned thunk returns false
  ;;and false.
  ;;
  (import LIBRARY-LOCATOR-UTILS)
  (%log-library-debug-message "~a: locating library for: ~a" __who__ libref)
  (%source-search-start libref options (%binary-search-start libref options)))


;;;; loading source programs

(define* (load-r6rs-script {file-pathname posix.file-string-pathname?} serialize? run?)
  ;;Load  source  code  from  FILE-PATHNAME,  which  must  be  a  string
  ;;representing a file  pathname, expecting an R6RS program  or an R6RS
  ;;library and compile it.
  ;;
  ;;If  SERIALIZE? is  true: the  libraries  needed by  the program  are
  ;;compiled, serialized and saved in FASL files.
  ;;
  ;;If RUN? is true: the loaded R6RS program is compiled and evaluated.
  ;;
  (%log-library-debug-message "~a: loading R6RS script: ~a" __who__ file-pathname)
  (let* ((prog  (read-script-source-file file-pathname))
	 (thunk (parametrise ((source-code-location file-pathname))
		  (expand-r6rs-top-level-make-evaluator prog))))
    (when serialize?
      (serialize-collected-libraries (lambda (source-pathname libname contents)
				       (store-serialized-library (fasl-path libname)
								 source-pathname libname contents))
				     (lambda (core-expr)
				       (compile-core-expr core-expr))))
    (when run?
      (thunk))))

(case-define* load
  ;;Load  source  code  from  FILE-PATHNAME,  which  must  be  a  string
  ;;representing a  file pathname, expecting:  an R6RS program,  an R6RS
  ;;library or just a list of forms.  Then transform the contents of the
  ;;file in a list of symbolic  expressions; for each form in the source
  ;;apply EVAL-PROC to the corresponding symbolic expression.
  ;;
  ;;When EVAL-PROC is not given: the  forms are evaluated in the current
  ;;INTERACTION-ENVIRONMENT.
  ;;
  ((file-pathname)
   (load file-pathname (lambda (sexp)
			 (eval sexp (interaction-environment)))))
  (({file-pathname posix.file-string-pathname?} {eval-proc procedure?})
   (%log-library-debug-message "~a: loading script: ~a" __who__ file-pathname)
   (let next-form ((ls (read-script-source-file file-pathname)))
     (unless (null? ls)
       (eval-proc (car ls))
       (next-form (cdr ls))))))

(define* (load-and-serialize-source-library {source-pathname posix.file-string-pathname?}
					    {binary-pathname false-or-file-string-pathname?})
  ;;Load a source library filename, expand it, compile it, serialize it.
  ;;Return unspecified values.
  ;;
  (define lib
    (let* ((port          (let ()
			    (import LIBRARY-LOCATOR-UTILS)
			    (%log-library-debug-message "~a: loading library: ~a" __who__ source-pathname)
			    (%print-verbose-message "loading library: ~a\n" source-pathname)
			    (%open-source-library source-pathname)))
	   (library-sexp  (read-library-source-port port))
	   (reject-key    (gensym)))
      (receive (uid libname
		    import-libdesc* visit-libdesc* invoke-libdesc*
		    invoke-code visit-code
		    export-subst export-env
		    guard-code guard-libdesc*
		    option*)
	  ((current-library-expander) library-sexp source-pathname (lambda (libname) (void)))
	(find-library-by-name libname))))
  (when lib
    (serialize-library-record lib binary-pathname)))


;;;; loading libraries from source

(define* (default-source-library-loader {libref library-reference?} {port textual-input-port?})
  ;;Default value fo the parameter CURRENT-SOURCE-LIBRARY-LOADER.  Given
  ;;a textual  input PORT: read  from it a LIBRARY  symbolic expression;
  ;;verify  that its  version  reference conforms  to  LIBREF; load  and
  ;;install all its dependency libraries; expand it; compile it; install
  ;;it.
  ;;
  ;;If successful  return a sexp  representing the R6RS library  name of
  ;;the loaded library; otherwise return false.
  ;;
  ;;We assume that  applying the function PORT-ID to PORT  will return a
  ;;string  representing  a  file  name   associated  to  the  port  (or
  ;;equivalent).
  ;;
  (%log-library-debug-message "~a: loading library from port: ~a" __who__ port)
  (let ((source-pathname  (port-id port))
	(library-sexp     (read-library-source-port port))
	(reject-key       (gensym)))
    ;;If the version  of the loaded library does not  conform to LIBREF:
    ;;we make the expander raise  an exception with REJECT-KEY as raised
    ;;object; we catch it here and return false.
    (guard (E ((eq? E reject-key)
	       (%log-library-debug-message "~a: rejected library from port: ~a" __who__ port)
	       #f))
      (receive (uid libname
		    import-libdesc* visit-libdesc* invoke-libdesc*
		    invoke-code visit-code
		    export-subst export-env
		    guard-code guard-libdesc*
		    option*)
	  ((current-library-expander) library-sexp source-pathname
	   (lambda (libname)
	     ;;We expect LIBNAME to be  the R6RS library name extracted from
	     ;;LIBRARY-SEXP.
	     (unless (conforming-library-name-and-library-reference? libname libref)
	       (raise reject-key))))
	libname))))


;;;; loading libraries from serialised locations

(define* (default-binary-library-loader {libref library-reference?} {port binary-input-port?})
  ;;Default value fo the parameter CURRENT-BINARY-LIBRARY-LOADER.  Given
  ;;a binary input PORT: read from  it a serialized library; verify that
  ;;its version  reference conforms to  LIBREF; install it (and  all its
  ;;dependency libraries).
  ;;
  ;;If successful  return a sexp  representing the R6RS library  name of
  ;;the installed library; otherwise return false.
  ;;
  ;;We assume that  applying the function PORT-ID to PORT  will return a
  ;;string  representing  a  file  name   associated  to  the  port  (or
  ;;equivalent).
  ;;
  ;;
  (define fasl-pathname (port-id port))
  (define reject-key    (gensym))
  ;;If the version of the loaded  library does not conform to LIBREF: we
  ;;make the loader raise an exception with REJECT-KEY as raised object;
  ;;we catch it here and return false.
  (%log-library-debug-message "~a: loading library from port: ~a" __who__ port)
  (guard (E ((eq? E reject-key)
	     (%log-library-debug-message "~a: rejected library from port: ~a" __who__ port)
	     #f))
    (let ((rv (load-serialized-library port install-binary-library-and-its-dependencies
		(lambda (libname)
		  ;;We  expect  LIBNAME  to  be the  R6RS  library  name
		  ;;extracted from the serialized library.
		  (unless (conforming-library-name-and-library-reference? libname libref)
		    (raise reject-key))))))
      (when rv
	(assert (library-name? rv)))
      (or rv
	  (begin
	    (%print-verbose-message "WARNING: not using fasl file ~s because invalid or \
                                  compiled with a different instance of Vicare\n"
				    fasl-pathname)
	    #f)))))


;;;; loading and storing precompiled library files

(case-define* serialize-library-record
  ;;This   function   is   the   default   value   for   the   parameter
  ;;CURRENT-LIBRARY-RECORD-SERIALIZER.
  ;;
  ;;Given an already installed LIBRARY  record and an optional FASL file
  ;;pathname: serialize  the compiled  library in  a FASL  file.  Return
  ;;unspecified  values.  Only  libraries loaded  form source  files are
  ;;serialized:  if a  library  was  loaded from  a  FASL file:  nothing
  ;;happens.   If a  FASL file  already exists  for the  library: it  is
  ;;silently overwritten.
  ;;
  ;;When  the binary  pathname is  not given  or it  is false:  a binary
  ;;pathname is built from the source pathname FASL-DIRECTORY as prefix.
  ;;
  (({lib library?})
   (serialize-library-record lib #f))
  (({lib library?} {binary-pathname false-or-file-string-pathname?})
   (cond ((library-source-file-name lib)
	  => (lambda (source-pathname)
	       (define binary-pathname^
		 (or binary-pathname (let ()
				       (import SOURCE-PATHNAME->BINARY-PATHNAME)
				       (source-pathname->binary-pathname source-pathname))))
	       (%print-verbose-message "serializing library: ~a\n" binary-pathname^)
	       (%log-library-debug-message "~a: serializing library: ~a" __who__ source-pathname)
	       (serialize-library lib
				  (lambda (source-pathname libname contents)
				    (store-serialized-library binary-pathname^ source-pathname
							      libname contents))
				  (lambda (core-expr)
				    (compile-core-expr core-expr))))))))

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
    ;;existing one.  Return unspecified values.
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
    (%print-verbose-message "serializing ~a ..." fasl-pathname)
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
    (%print-verbose-message " done\n"))

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

(current-source-library-file-locator	default-source-library-file-locator)
(current-source-library-loader		default-source-library-loader)

(current-binary-library-file-locator	default-binary-library-file-locator)
(current-binary-library-loader		default-binary-library-loader)

(current-library-record-serializer	serialize-library-record)

(current-include-file-locator		locate-include-file)
(current-include-file-loader		read-include-file)

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.load"))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'load-serialized-library 'scheme-indent-function 2)
;; End:
