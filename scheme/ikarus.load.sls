;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (c) 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
    compile-source-program
    run-compiled-program
    compile-source-library
    load
    load-r6rs-script

    ;; stuff for (vicare libraries)
    default-library-loader
    current-source-library-loader
    current-binary-library-loader

    current-library-locator
    run-time-library-locator
    compile-time-library-locator
    source-library-locator

    current-library-source-search-path-scanner
    current-library-binary-search-path-scanner

    default-library-source-search-path-scanner
    default-library-binary-search-path-scanner

    default-include-loader
    default-include-file-locator
    default-include-file-loader

    current-include-file-locator
    current-include-file-loader

    current-library-serialiser
    current-library-store-directory-serialiser)
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  load					load-r6rs-script
		  library-source-search-path		library-extensions
		  library-binary-search-path		get-annotated-datum)
    (prefix (ikarus.posix)
	    posix.)
    (only (ikarus.compiler)
	  compile-core-expr)
    (only (vicare.foreign-libraries)
	  retrieve-filename-foreign-libraries)
    (prefix (psyntax.library-manager) libman.)
    (only (psyntax.expander)
	  expand-r6rs-top-level-make-evaluator
	  expand-r6rs-top-level-make-compiler)
    (only (ikarus.reader)
	  get-annotated-datum
	  read-script-source-file
	  read-library-source-port)
    (ikarus library-utils)
    (only (ikarus fasl read)
	  fasl-read-header
	  fasl-read-object)
    (only (ikarus.fasl.write)
	  fasl-write-header
	  fasl-write-object)
    (prefix (only (ikarus.options)
		  print-loaded-libraries?
		  print-debug-messages?
		  verbose?)
	    option.))

  (include "ikarus.wordsize.scm" #t)


;;;; helpers and arguments validation

(define (%false-or-file-string-pathname? S)
  (or (not S)
      (and (posix.file-string-pathname? S))))

;;; --------------------------------------------------------------------

(module (REJECT-KEY)

  (define-syntax (expand-time-gensym stx)
    (syntax-case stx ()
      ((_ ?template)
       (let* ((tmp (syntax->datum #'?template))
	      (fxs (vector->list (foreign-call "ikrt_current_time_fixnums_2")))
	      (str (apply string-append tmp (map (lambda (N)
						   (string-append "." (number->string N)))
					      fxs)))
	      (sym (gensym str)))
	 (with-syntax
	     ((SYM (datum->syntax #'here sym)))
	   (fprintf (current-error-port) "expand-time gensym ~a\n" sym)
	   #'(quote SYM))))))

  (define-constant REJECT-KEY
    (expand-time-gensym "library-reject-key-gensym"))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define (%print-library-verbose-message template . args)
  (when (option.print-loaded-libraries?)
    ;;We do not want an exception from the I/O layer to ruin things.
    (guard (E (else (void)))
      (let ((P (current-error-port)))
	(apply fprintf P (string-append "vicare: " template "\n") args)
	(flush-output-port P)))))

(define (%print-library-debug-message template . args)
  (when (and (option.print-loaded-libraries?)
	     (option.print-debug-messages?))
    ;;We do not want an exception from the I/O layer to ruin things.
    (guard (E (else (void)))
      (let ((P (current-error-port)))
	(apply fprintf P (string-append "vicare: " template "\n") args)
	(flush-output-port P)))))

;;; --------------------------------------------------------------------

(define failed-library-location-collector
  ;;Hold a function that is used to register queried locations that failed to provide
  ;;a requested library.   Such locations must be represented by  a printable object,
  ;;for example a string.
  ;;
  ;;The  collector function  must  accept 1  or  0 arguments:  when  called with  one
  ;;argument  it must  register  it as  location descriptor;  when  called with  zero
  ;;arguments  it must  return  the registered  collection or  locations  as list  of
  ;;objects.  The collector function can return unspecified values.
  ;;
  ;;For example, the collector function can be set to:
  ;;
  ;;   (let ((ell '()))
  ;;     (case-lambda
  ;;      (()
  ;;       ell)
  ;;      ((location)
  ;;       (set! ell (cons location ell)))))
  ;;
  ;;and used  to collect  library file  names that where  tried but  do not  exist or
  ;;failed to be opened.
  ;;
  (make-parameter
      (case-lambda
       (()
	'())
       ((origin)
	(void)))
    (lambda* ({obj procedure?})
      obj)))


;;;; built-in library locator options

(define-enumeration library-locator-option
  (move-on-when-open-fails
		;If  attempting to  open a  file fails:  do not  raise an  exception,
		;rather move on with the search.
   )
  library-locator-options)

(define (library-locator-options-no-raise-when-open-fails? options)
  (enum-set-member? 'move-on-when-open-fails options))

(define-constant current-library-locator-options
  (make-parameter
      (library-locator-options move-on-when-open-fails)))


;;;; loading libraries from input ports

(module (default-library-loader)
  (define-syntax __module_who__
    (identifier-syntax 'default-library-loader))

  (define* (default-library-loader libref)
    ;;Default value for  the parameter CURRENT-LIBRARY-LOADER.  Given  a R6RS library
    ;;reference: attempt to locate the library in an external repository and load it;
    ;;all the dependency libraries are interned.  Return unspecified values.
    ;;
    ;;This function  makes use of:  the library  locator referenced by  the parameter
    ;;CURRENT-LIBRARY-LOCATOR; the source library  loader referenced by the parameter
    ;;CURRENT-SOURCE-LIBRARY-LOADER;  the binary  library  loader  referenced by  the
    ;;parameter CURRENT-BINARY-LIBRARY-LOADER.
    ;;
    ;;LIBREF must be a library reference as defined by R6RS:
    ;;
    ;;   (?identifier0 ?identifier ...)
    ;;   (?identifier0 ?identifier ... ?version-reference)
    ;;
    (%print-library-debug-message "~a: searching: ~a" __module_who__ libref)
    (parametrise
	;;For error reporting  purposes: collect all the library  locations that were
	;;found by the library locator, but were rejected for some reason.
	((failed-library-location-collector (let ((ell '()))
					      (case-lambda
					       (()
						ell)
					       ((location)
						(set! ell (cons location ell)))))))
      (let loop ((next-locator-search ((current-library-locator) libref)))
	(receive (rv further-locator-search)
	    (next-locator-search)
	  (%print-library-debug-message "~a: reading from: ~a" __module_who__ rv)
	  (assert (or (input-port? rv) (boolean? rv)))
	  (cond ((binary-port? rv)
		 (%load-binary-library libref rv (lambda ()
						   (loop further-locator-search))))
		((textual-port? rv)
		 (%load-source-library libref rv (lambda ()
						   (loop further-locator-search))))

		((and (boolean? rv) rv)
		 ;;When the library locator returns  #t: it declares that the library
		 ;;has  been loaded  and  it  is already  in  the interned  libraries
		 ;;collection.  So let's try to retrieve it.
		 (let ((lib (libman.find-library-in-collection-by-reference libref)))
		   (if lib
		       #t
		     (loop further-locator-search))))

		((not rv)
		 ;;No suitable library was found.
		 (%file-locator-resolution-error libref
						 (reverse ((failed-library-location-collector)))
						 (cdr (libman.external-pending-libraries))))

		(else
		 (assertion-violation __module_who__
		   "invalid return values from library locator"
		   rv further-locator-search)))))))

  (define (%load-binary-library libref port loop)
    ;;A binary location was found.  We can read the binary library from PORT.
    ;;
    (%print-loading-library port)
    (cond ((unwind-protect
	       ((current-binary-library-loader) libref port)
	     (close-input-port port))
	   ;;Success.  The  library and all  its dependencies have  been successfully
	   ;;loaded and interned.
	   => (lambda (libname)
		(%print-loaded-library port)
		#t))
	  (else
	   ;;Failure.  The  library read from  port has been  rejected; try to  go on
	   ;;with the search.
	   (%print-rejected-library port)
	   (close-input-port port)
	   (loop))))

  (define (%load-source-library libref port loop)
    ;;A source location  was found.  We can  read the source library  from PORT; we
    ;;assume that applying the function PORT-ID to PORT will return the source file
    ;;name.
    ;;
    (%print-loading-library port)
    (cond ((unwind-protect
	       ((current-source-library-loader) libref port)
	     (close-input-port port))
	   ;;Success.  The  library and all  its dependencies have  been successfully
	   ;;loaded and interned.
	   => (lambda (libname)
		(%print-loaded-library port)
		#t))
	  ;;Failure.  The library read from port has been rejected; try to go on with
	  ;;the search.
	  (else
	   (%print-rejected-library port)
	   (close-input-port port)
	   (loop))))

  ;;Keep the library names aligned!!!
  (define (%print-loading-library  port) (%print-library-debug-message "loading:  ~a" (port-id port)))
  (define (%print-loaded-library   port) (%print-library-debug-message "loaded:   ~a" (port-id port)))
  (define (%print-rejected-library port) (%print-library-debug-message "rejected: ~a" (port-id port)))

  (define (%file-locator-resolution-error libref failed-list pending-libraries)
    (raise
     (apply condition (make-error)
  	    (make-who-condition 'expander)
  	    (make-message-condition "cannot locate library")
  	    (make-library-resolution-condition libref failed-list)
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

  #| end of module: DEFAULT-LIBRARY-LOADER |# )

(module ()
  (libman.current-library-loader default-library-loader))

;;; --------------------------------------------------------------------

(define* (default-source-library-loader {libref library-reference?} {port textual-input-port?})
  ;;Default value  for the parameter CURRENT-SOURCE-LIBRARY-LOADER.   Given a textual
  ;;input PORT:
  ;;
  ;;1. Read from it a LIBRARY symbolic expression.
  ;;
  ;;2. Verify that its version reference conforms to LIBREF.
  ;;
  ;;3. Using the expander procedure  referenced by CURRENT-LIBRARY-EXPANDER: load and
  ;;intern all  its dependency  libraries; expand  it; compile it;  intern it  in the
  ;;internal collection of libraries.
  ;;
  ;;If successful  return a  sexp representing  the R6RS library  name of  the loaded
  ;;library; otherwise return false.
  ;;
  ;;We  assume that  applying  the function  PORT-ID  to PORT  will  return a  string
  ;;representing a file name associated to the port (or equivalent).
  ;;
  (%print-library-debug-message "~a: reading library from port: ~a" __who__ port)
  (let ((source-pathname  (port-id port))
	(libsexp          (read-library-source-port port)))
    ;;If the library name extracted from LIBSEXP  does not conform to LIBREF: we make
    ;;the expander raise  an exception with REJECT-KEY as raised  object; we catch it
    ;;here and return false.
    (define (%verify-libname libname)
      (unless (conforming-library-name-and-library-reference? libname libref)
	(raise REJECT-KEY)))
    (guard (E ((eq? E REJECT-KEY)
	       (%print-library-debug-message "~a: rejected library from port: ~a" __who__ port)
	       #f))
      (receive (uid libname . unused)
	  ;;This  call to  the library  expander loads  and interns  all the  library
	  ;;dependencies using FIND-LIBRARY-BY-REFERENCE.
	  ((libman.current-library-expander) libsexp source-pathname %verify-libname)
	(%print-library-verbose-message "loaded library \"~a\" from: ~a" libname (port-id port))
	libname))))

(define current-source-library-loader
  ;;Reference a function used to load a source library from a textual input port.
  ;;
  ;;The referenced  function must accept two  arguments: a R6RS library  reference, a
  ;;textual input port from which the source library can be read.  It must: read from
  ;;the port a LIBRARY symbolic expression;  verify that its library name conforms to
  ;;the library reference;  load and intern all its dependency  libraries; expand it;
  ;;compile it; intern it.
  ;;
  ;;If successful  the function  must return a  symbolic expression  representing the
  ;;R6RS library name of the loaded library; otherwise return #f.
  ;;
  (make-parameter
      default-source-library-loader
    (lambda* ({obj procedure?})
      obj)))

;;; --------------------------------------------------------------------

(define* (default-binary-library-loader {libref library-reference?} {port binary-input-port?})
  ;;Default value  for the  parameter CURRENT-BINARY-LIBRARY-LOADER.  Given  a binary
  ;;input PORT: read from it a  serialised library; verify that its version reference
  ;;conforms to LIBREF; intern it (and all its dependency libraries).
  ;;
  ;;If successful  return a sexp representing  the R6RS library name  of the interned
  ;;library; otherwise return false.
  ;;
  ;;We  assume that  applying  the function  PORT-ID  to PORT  will  return a  string
  ;;representing a file name associated to the port (or equivalent).
  ;;
  (%print-library-debug-message "~a: loading library from port: ~a" __who__ port)
  (let ((binary-pathname (port-id port)))
    ;;If the version  of the loaded library  does not conform to LIBREF:  we make the
    ;;loader raise  an exception with REJECT-KEY  as raised object; we  catch it here
    ;;and return false.
    (define (%verify-libname libname)
      (unless (conforming-library-name-and-library-reference? libname libref)
	(raise REJECT-KEY)))
    (guard (E ((eq? E REJECT-KEY)
	       (%print-library-debug-message "~a: rejected library from port: ~a" __who__ port)
	       #f))
      (cond ((read-library-binary-port port libman.intern-binary-library-and-its-dependencies %verify-libname)
	     => (lambda (libname)
		  (%print-library-verbose-message "loaded library \"~a\" from: ~a" libname (port-id port))
		  libname))
	    (else
	     (%print-library-verbose-message
	      "warning: not using FASL file ~s because invalid or compiled with a different instance of Vicare"
	      binary-pathname)
	     #f)))))

(define current-binary-library-loader
  ;;Reference a function used to load a binary library.
  ;;
  ;;The referenced  function must accept two  arguments: a R6RS library  reference, a
  ;;binary input port from  which the serialised library can be  read.  It must: read
  ;;from the port a serialised library; verify  that its library name conforms to the
  ;;library reference; intern it along with all its dependency libraries.
  ;;
  ;;If successful  the function  must return a  symbolic expression  representing the
  ;;R6RS library name of the interned library; otherwise return #f.
  ;;
  (make-parameter
      default-binary-library-loader
    (lambda* ({obj procedure?})
      obj)))


;;;; locating library files in search paths

(define* (default-library-source-search-path-scanner {libref library-reference?})
  ;;Default  value for  the parameter  CURRENT-LIBRARY-SOURCE-SEARCH-PATH-SCANNER.  Given  a
  ;;R6RS library reference: scan "(library-source-search-path)" for the corresponding
  ;;source file.
  ;;
  ;;Return  2  values.   When  successful:  a string  representing  the  source  file
  ;;pathname; a thunk to be called to  continue the search from the next directory in
  ;;the search path.  Otherwise return: false and false.
  ;;
  (%print-library-debug-message "~a: locating source library file for: ~a" __who__ libref)
  (let loop ((stem        (library-reference->filename-stem libref))
	     (directories (library-source-search-path))
	     (extensions  (library-extensions)))
    (cond ((null? directories)
	   ;;No more directories in the search path.
	   (%print-library-debug-message "~a: exhausted search path, no source library file found for: ~a" __who__ libref)
	   (values #f #f))

	  ((null? extensions)
	   ;;No more extensions: try the next directory in the search path again with
	   ;;the full list of extensions.
	   (loop stem (cdr directories) (library-extensions)))

	  (else
	   ;;Build the file pathname with the  next directory and the next extension,
	   ;;then check its existence; if not found try the next file extension.
	   (let* ((source-pathname (string-append (car directories) stem (car extensions)))
		  (continue-thunk  (lambda ()
				     (loop stem directories (cdr extensions)))))
	     (if (file-exists? source-pathname)
		 (let ((source-pathname (posix.real-pathname source-pathname)))
		   (%print-library-debug-message "~a: found: ~a" __who__ source-pathname)
		   (values source-pathname continue-thunk))
	       (begin
		 ((failed-library-location-collector) source-pathname)
		 (continue-thunk))))))))

(define current-library-source-search-path-scanner
  ;;Hold a  function used to convert  a R6RS library reference  into the
  ;;corresponding source file pathname and  search for it in the library
  ;;search path.
  ;;
  ;;The referenced function must accept, as single value, a R6RS library
  ;;reference and it must return  two values.  When successful: a string
  ;;representing  the source  file pathname;  a  thunk to  be called  to
  ;;continue  the search  from the  next directory  in the  search path.
  ;;Otherwise return: false and false.
  ;;
  (make-parameter
      default-library-source-search-path-scanner
    (lambda* ({obj procedure?})
      obj)))

;;; --------------------------------------------------------------------

(define* (default-library-binary-search-path-scanner {libref library-reference?})
  ;;Default  value for  the parameter  CURRENT-LIBRARY-BINARY-SEARCH-PATH-SCANNER.  Given  a
  ;;R6RS library reference: scan "(library-binary-search-path)" for the corresponding
  ;;FASL file.
  ;;
  ;;Return 2 values.  When successful: a  string representing the fasl file pathname;
  ;;a thunk to be called to continue the search from the next directory in the search
  ;;path.  Otherwise return: false and false.
  ;;
  (%print-library-debug-message "~a: locating binary library file for: ~a" __who__ libref)
  (let loop ((stem        (library-reference->filename-stem libref))
	     (directories (library-binary-search-path)))
    (if (pair? directories)
	;;Check file existence in the first directory from the list and prepare thunk
	;;continue the search in the other directories.
	(let* ((binary-pathname (directory+library-stem->library-binary-pathname (car directories) stem))
	       (continue-thunk  (lambda ()
				  (loop stem (cdr directories)))))
	  (%print-library-debug-message "~a: trying: ~a" __who__ binary-pathname)
	  (if (file-exists? binary-pathname)
	      (let ((binary-pathname (posix.real-pathname binary-pathname)))
		(%print-library-debug-message "~a: found: ~a" __who__ binary-pathname)
		(values binary-pathname continue-thunk))
	    (begin
	      ((failed-library-location-collector) binary-pathname)
	      (%print-library-debug-message "~a: unexistent: ~a" __who__ binary-pathname)
	      (continue-thunk))))
      ;;No suitable library file was found.
      (begin
	(%print-library-debug-message "~a: exhausted search path, no binary library file found for: ~a" __who__ libref)
	(values #f #f)))))

(define current-library-binary-search-path-scanner
  ;;Hold a function  used to convert a R6RS library  reference into the corresponding
  ;;compiled library  file pathname  in the  search path  specified by  the parameter
  ;;LIBRARY-BINARY-SEARCH-PATH.
  ;;
  ;;The referenced  function must accept, as  single value, a R6RS  library reference
  ;;and it must  return two values.  When successful: a  string representing the FASL
  ;;file  pathname; a  thunk  to be  called  to  continue the  search  from the  next
  ;;directory in the search path.  Otherwise return: false and false.
  ;;
  (make-parameter
      default-library-binary-search-path-scanner
    (lambda* ({obj procedure?})
      obj)))


;;;; locating source and binary libraries: utilities

(module LIBRARY-LOCATOR-UTILS
  (%source-search-start
   %binary-search-start
   %open-source-library
   %open-binary-library)

  (module (%source-search-start)
    ;;This function can be used to search for a source library file using the current
    ;;source library locator  referenced by CURRENT-LIBRARY-SOURCE-SEARCH-PATH-SCANNER, with
    ;;the purpose of finding a library that matches a given R6RS library reference.
    ;;
    ;;The ragument  LIBREF must be a  R6RS library reference.  The  optional argument
    ;;FAIL-KONT must  be a thunk  to be called when  the search fails.   The argument
    ;;OPTIONS must be a list of symbols; at present the supported options are:
    ;;
    ;;   move-on-when-open-fails
    ;;
    ;;The return  value is a thunk  to call to  start the search.  When  the returned
    ;;thunk finds a matching library source file, it returns two values:
    ;;
    ;;1.  A textual  input port  from which  the source  library can  be read;  it is
    ;;   responsibility of the caller to close the returned port when no more needed.
    ;;
    ;;2. A thunk to  be called to continue the search.  This  thunk allows the caller
    ;;   to  reject a library  if it does not  meet some additional  constraints; for
    ;;   example: if its version number does not conform to LIBREF.
    ;;
    ;;When  no matching  library is  found: the  returned thunk  calls FAIL-KONT  and
    ;;returns  its return  values; the  default fail  continuation returns  false and
    ;;false.
    ;;
    ;;Pseudo-code usage example:
    ;;
    ;;   (let loop ((next-locator-search (%source-search-start
    ;;                                    '(a b (1 2))
    ;;                                    '(move-on-when-open-fails)
    ;;                                    (lambda ()
    ;;                                      (error #f "no match")))))
    ;;     (receive (port further-locator-match)
    ;;         (next-locator-search)
    ;;       (if (validate-library-from-port port)
    ;;           (success)
    ;;         (loop further-locator-match))))
    ;;
    (case-define* %source-search-start
      (({libref library-reference?} options)
       (%source-search-start libref options (lambda ()
					      (values #f #f))))
      (({libref library-reference?} options {fail-kont procedure?})
       (let ((source-locator (current-library-source-search-path-scanner)))
	 (lambda ()
	   (%source-search-step options
				(lambda ()
				  (source-locator libref))
				fail-kont)))))

    (define (%source-search-step options next-source-file-match search-fail-kont)
      ;;If NEXT-SOURCE-FILE-MATCH  is successful we  expect: SOURCE-PATHNAME to  be a
      ;;string representing the fasl file pathname; FURTHER-SOURCE-FILE-MATCH to be a
      ;;thunk to  be called  to continue  the search.  Otherwise  they are  false and
      ;;false.
      (receive (source-pathname further-source-file-match)
	  (next-source-file-match)
	(if source-pathname
	    ;;The  source locator  found a  match: we  open the  file and  return the
	    ;;results.
	    (%handle-source-file-match options source-pathname
				       (lambda ()
					 (%source-search-step options further-source-file-match search-fail-kont)))
	  ;;Either the  source locator  found no  match or we  have rejected  all the
	  ;;matching libraries.  Tail-call the fail continuation.
	  (search-fail-kont))))

    (define (%handle-source-file-match options source-pathname next-source-search-step)
      ;;The source locator found a match: we  attempt to open the file and return the
      ;;textual input port and a search-continuation thunk.
      ;;
      (define (%continue)
	;;If we  are here it means  the previous pathname was  rejected.  We register
	;;the  pathname for  later  use  in error  messages,  then  we tail-call  the
	;;search-continuation thunk.
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
    ;;This function can be used to search for a library binary file using the current
    ;;binary library locator  referenced by CURRENT-LIBRARY-BINARY-SEARCH-PATH-SCANNER, with
    ;;the purpose of finding a library that matches a given R6RS library reference.
    ;;
    ;;The argument  LIBREF must be a  R6RS library reference.  The  optional argument
    ;;FAIL-KONT must  be a thunk  to be called when  the search fails.   The argument
    ;;OPTIONS must be a list of symbols; at present the supported options are:
    ;;
    ;;   move-on-when-open-fails
    ;;
    ;;The return  value is a thunk  to call to  start the search.  When  the returned
    ;;thunk finds a matching library FASL file, it returns two values:
    ;;
    ;;1.  A binary  input port  from which  the  binary library  can be  read; it  is
    ;;   responsibility of the caller to close the returned port when no more needed.
    ;;
    ;;2. A thunk to  be called to continue the search.  This  thunk allows the caller
    ;;   to  reject a library  if it does not  meet some additional  constraints; for
    ;;   example: if its version number does not conform to LIBREF.
    ;;
    ;;When  no matching  library is  found: the  returned thunk  calls FAIL-KONT  and
    ;;returns  its return  values; the  default fail  continuation returns  false and
    ;;false.
    ;;
    ;;Pseudo-code usage example:
    ;;
    ;;   (let loop ((next-locator-search (%binary-search-start
    ;;                                    '(a b (1 2))
    ;;                                    '(move-on-when-open-fails)
    ;;                                    (lambda ()
    ;;                                      (error #f "no match")))))
    ;;     (receive (port further-locator-match)
    ;;         (next-locator-search)
    ;;       (if (validate-library-from-port port)
    ;;           (success)
    ;;         (loop further-locator-match))))
    ;;
    (case-define* %binary-search-start
      (({libref library-reference?} options)
       (%binary-search-start libref options (lambda ()
					      (values #f #f))))
      (({libref library-reference?} options {fail-kont procedure?})
       (let ((binary-locator (current-library-binary-search-path-scanner)))
	 (lambda ()
	   (%binary-search-step options
				(lambda ()
				  (binary-locator libref))
				fail-kont)))))

    (define (%binary-search-step options next-binary-file-match search-fail-kont)
      ;;If NEXT-BINARY-FILE-MATCH  is successful we  expect: BINARY-PATHNAME to  be a
      ;;string representing the FASL file pathname; FURTHER-BINARY-FILE-MATCH to be a
      ;;thunk to  be called  to continue  the search.  Otherwise  they are  false and
      ;;false.
      (receive (binary-pathname further-binary-file-match)
	  (next-binary-file-match)
	(if binary-pathname
	    ;;The  binary locator  found a  match: we  open the  file and  return the
	    ;;results.
	    (%handle-binary-file-match options binary-pathname
				       (lambda ()
					 (%binary-search-step options further-binary-file-match search-fail-kont)))
	  ;;Either the  binary locator  found no  match or we  have rejected  all the
	  ;;matching libraries.  Tail-call the fail continuation.
	  (search-fail-kont))))

    (define (%handle-binary-file-match options binary-pathname next-binary-search-step)
      ;;The binary locator found a match: we  attempt to open the file and return the
      ;;binary input port and a search-continuation thunk.
      ;;
      (define (%continue)
	;;If we  are here it means  the previous pathname was  rejected.  We register
	;;the  pathname for  later  use  in error  messages,  then  we tail-call  the
	;;search-continuation thunk.
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
;;
;;The reference scenario for the run-time library locator is this:
;;
;;1. We  install the package Vicare  Scheme, compiling bundled libraries  and putting
;;   them in some system directory; the libraries might be installed with pathnames:
;;
;;      /usr/local/lib/vicare-scheme/vicare/posix.fasl
;;
;;2. We install additional packages, compiling distributed libraries and putting them
;;   in some system directory; the libraries might be installed with pathnames:
;;
;;      /usr/local/lib/vicare-scheme/vicare/something.fasl
;;
;;3. We configure  the library binary search  path to make sure that  it includes the
;;   system directory:
;;
;;      (library-binary-search-path)
;;      => (... "/usr/local/lib/vicare-scheme" ...)
;;
;;4. We configure the  parameter CURRENT-LIBRARY-BINARY-SEARCH-PATH-SCANNER to reference the
;;   default compiled library locator DEFAULT-LIBRARY-BINARY-SEARCH-PATH-SCANNER, which will
;;   scan the search path in "(library-binary-search-path)".
;;
;;5. We compose a Scheme program "demo.sps" which imports the libraries:
;;
;;      (import (vicare)
;;        (prefix (vicare posix) posix.)
;;        (vicare something))
;;      ---
;;
;;   and we execute it selecting the run-time library locator:
;;
;;      $ vicare --library-locator run-time --r6rs-script demo.sps
;;
;;   the command line option "--library-locator" will put RUN-TIME-LIBRARY-LOCATOR in
;;   the parameter CURRENT-LIBRARY-LOCATOR.
;;
(define* (run-time-library-locator {libref library-reference?})
  ;;Possible value for the parameter  CURRENT-LIBRARY-LOCATOR; this function is meant
  ;;to be used to search for libraries when running an application.
  ;;
  ;;Given a R6RS library reference and a list of search options: return a thunk to be
  ;;used to start the search for a matching library.
  ;;
  ;;The returned thunk  scans the search path  for compiled libraries in  search of a
  ;;matching binary file; if  a matching compiled library is not  found: it scans the
  ;;search path for source libraries in search of a matching source file.
  ;;
  ;;When successful the returned thunk returns 2 values:
  ;;
  ;;1. An  input port from which  the library can be  read; if the port  is binary: a
  ;;   compiled library can be read from it;  if the port is textual a source library
  ;;   can be read from it.  It is responsibility of the caller to close the returned
  ;;   port when no more needed.
  ;;
  ;;2. A thunk to be called to continue  the search.  This thunk allows the caller to
  ;;   reject a library if it does not meet some additional constraints; for example:
  ;;   if its version number does not conform to LIBREF.
  ;;
  ;;When no matching library is found: the returned thunk returns false and false.
  ;;
  (import LIBRARY-LOCATOR-UTILS)
  (%print-library-debug-message "~a: locating library for: ~a" __who__ libref)
  (let ((options (current-library-locator-options)))
    (%binary-search-start libref options (%source-search-start libref options))))


;;;; locating source and binary libraries: compile-time locator
;;
;;The reference scenario for the run-time library locator is this:
;;
;;* We  have installed  the package  Vicare Scheme,  compiling bundled  libraries and
;;  putting them  in some  system directory;  the libraries  might be  installed with
;;  pathnames:
;;
;;      /usr/local/lib/vicare-scheme/vicare/posix.fasl
;;
;;*  We have  unpacked the  distribution tarball  of a  package providing  additional
;;  Vicare libraries.  We have the source libraries under:
;;
;;      $(srcdir)/lib/vicare/this.sls
;;      $(srcdir)/lib/vicare/that.sls
;;
;;  we want to compile them under the build directory:
;;
;;      $(builddir)/lib/vicare/this.fasl
;;      $(builddir)/lib/vicare/that.fasl
;;
;;  and then install them in a system directory:
;;
;;      /usr/local/lib/vicare-scheme/vicare/this.fasl
;;      /usr/local/lib/vicare-scheme/vicare/that.fasl
;;
;;  In the package's  building infrastructure (for example a Makefile  managed by the
;;  GNU Autotools) we need to write  appropriate invocations of "vicare" to build the
;;  libraries  locally  and  pick  the  appropriate  source  libraries  and  compiled
;;  libraries.
;;
;;* It  may be that  the local  libraries need to  load libraries installed  from the
;;  Vicare Scheme distribution, and also have local dependencies:
;;
;;      (library (vicare this)
;;        (export)
;;        (import (vicare)
;;          (vicare that))
;;        ---)
;;
;;      (library (vicare that)
;;        (export)
;;        (import (vicare)
;;          (prefix (vicare posix) posix.))
;;        ---)
;;
;;* It  may be that an  older version of the  package is already installed,  so there
;;  already exist installed binary libraries:
;;
;;      /usr/local/lib/vicare-scheme/vicare/this.fasl
;;      /usr/local/lib/vicare-scheme/vicare/that.fasl
;;
;;  we  want  the libraries  under  "$(builddir)/lib"  to  take precedence  over  the
;;  libraries  under  "/usr/local/lib/vicare-scheme".  It  may  be  that there  exist
;;  installed source libraries:
;;
;;      /usr/local/lib/vicare-scheme/vicare/this.sls
;;      /usr/local/lib/vicare-scheme/vicare/that.sls
;;
;;  we want the libraries under "$(srcdir)/lib" to take precedence over the libraries
;;  under "/usr/local/lib/vicare-scheme".
;;
;;At the Scheme level we want the following:
;;
;;*  Configure the  library  source search  path  to include  only  the local  source
;;  directory:
;;
;;     (library-source-search-path)
;;     => ("$(srcdir)/lib")
;;
;;* Configure the library binary search path to include the system directory:
;;
;;     (library-binary-search-path)
;;     => (... "/usr/local/lib/vicare-scheme" ...)
;;
;;* Configure the store directory to reference the local build directory:
;;
;;     (compiled-libraries-store-directory)
;;     => "$(builddir)/lib"
;;
;;* Configure the library binary file locator parameter:
;;
;;     (current-library-binary-search-path-scanner default-library-binary-search-path-scanner)
;;
;;  which will scan the search path in "(library-binary-search-path)".
;;
;;* Configure the library source file locator parameter:
;;
;;     (current-library-source-search-path-scanner default-library-source-search-path-scanner)
;;
;;  which will scan the search path in "(library-source-search-path)".
;;
;;* Configure the library locator parameter:
;;
;;     (current-library-locator compile-time-library-locator)
;;
;;  which implements the appropriate policy.
;;
;;To achieve the desired result, we have two options:
;;
;;1. For every library  to be compiled locally, we write in  the Makefile an explicit
;;   dependency rule:
;;
;;      lib/vicare/that.fasl: lib/vicare/that.sls
;;              VICARE_SOURCE_PATH=; export VICARE_SOURCE_PATH=; \
;;              vicare --library-locator compile-time      \
;;                 -F /usr/local/lib/vicare-scheme         \
;;                 -L $(srcdir)/lib                        \
;;                 --store-directory $(builddir)/lib       \
;;                 -o $@ -c $<
;;
;;      lib/vicare/this.fasl: lib/vicare/this.sls lib/vicare/that.fasl
;;              VICARE_SOURCE_PATH=; export VICARE_SOURCE_PATH; \
;;              vicare --library-locator compile-time      \
;;                 -F /usr/local/lib/vicare-scheme         \
;;                 -L $(srcdir)/lib                        \
;;                 --store-directory $(builddir)/lib       \
;;                 -o $@ -c $<
;;
;;2. Write a script "compile-all.sps" that  imports at least the local libraries that
;;   are leaves in the local package dependency tree:
;;
;;      (import (only (vicare that))
;;              (only (vicare this)))
;;
;;   write a single Makefile rule that compiles  in the store all the dependencies of
;;   the script:
;;
;;      .PHONY: vfasl
;;
;;      vfasl:
;;              VICARE_SOURCE_PATH=; export VICARE_SOURCE_PATH; \
;;              vicare --library-locator compile-time      \
;;                 -F /usr/local/lib/vicare-scheme         \
;;                 -L $(srcdir)/lib                        \
;;                 --store-directory $(builddir)/lib       \
;;                 --compile-dependencies compile-all.sps
;;
(module (compile-time-library-locator)
  (module (%open-source-library
	   %open-binary-library
	   %binary-search-start)
    (import LIBRARY-LOCATOR-UTILS))
  (define-constant __module_who__ 'compile-time-library-locator)

  (define* (compile-time-library-locator {libref library-reference?})
    ;;Possible  value for  the  parameter CURRENT-LIBRARY-LOCATOR;  this function  is
    ;;meant to be used  to search for libraries to be  compiled for installation, for
    ;;example by a package.
    ;;
    ;;Given a R6RS library  reference: return a thunk to be used  to start the search
    ;;for a matching library.  The search for source libraries is performed using the
    ;;library source file locator in CURRENT-LIBRARY-SOURCE-SEARCH-PATH-SCANNER.  The
    ;;search  for compiled  libraries  is  performed using  the  library binary  file
    ;;locator in CURRENT-LIBRARY-BINARY-SEARCH-PATH-SCANNER.
    ;;
    ;;The returned thunk does the following:
    ;;
    ;;1. Ask the library source file locator for the next matching source file.
    ;;
    ;;2. If a matching source is found:  look for an already compiled library file in
    ;;   the COMPILED-LIBRARIES-STORE-DIRECTORY:
    ;;
    ;;   2.1. If no compiled file exists or it exists but it is older than the source
    ;;        file: accept the source file as matching.
    ;;
    ;;   2.2. If a compiled file exists and  it is newer than the source file: accept
    ;;        the compiled file as matching.
    ;;
    ;;   2.3. Return to the caller the matching file.
    ;;
    ;;   2.4. If the caller rejects the  binary file: return to the caller the source
    ;;        file.
    ;;
    ;;   2.5. If the caller rejects the source file: loop to 1
    ;;
    ;;3. If no source file exists: loop to 1.
    ;;
    ;;Remember  that the  binary file  can be  rejected if  it has  been compiled  by
    ;;another boot image or it has the wrong library UID.
    ;;
    ;;When successful (a source or binary file matching LIBREF is found) the returned
    ;;thunk returns 2 values:
    ;;
    ;;1. An input port.   If the port is binary: a compiled library  can be read from
    ;;   it.  If  the port is textual: a  source library can be read from  it.  It is
    ;;   responsibility of the caller to close the returned port when no more needed.
    ;;
    ;;2. A thunk to  be called to continue the search.  This  thunk allows the caller
    ;;   to  reject a  library if it  does not meet  some additional  constraint; for
    ;;   example: if its version number does not conform to LIBREF.
    ;;
    ;;When no matching library is found: the returned thunk returns false and false.
    ;;
    (%print-library-debug-message "~a: start search for library: ~a" __who__ libref)
    (let* ((options        (current-library-locator-options))
	   (source-locator (current-library-source-search-path-scanner))
	   (fail-kont      (%binary-search-start libref options)))
      (lambda ()
	(%locator-search-step options libref
			      (lambda ()
				(source-locator libref))
			      fail-kont))))

;;; --------------------------------------------------------------------

  (define (%locator-search-step options libref next-source-file-match search-fail-kont)
    (receive (source-pathname further-source-file-match)
	(next-source-file-match)
      (if source-pathname
	  ;;A matching source library was found in a local directory.
	  (begin
	    (%print-library-debug-message "~a: found source: ~a" __module_who__ source-pathname)
	    (let ((binary-pathname (library-reference->library-binary-pathname-in-store-directory libref)))
	      (%print-library-debug-message "~a: checking binary: ~a" __module_who__ binary-pathname)
	      (if (and (file-exists? binary-pathname)
		       (receive-and-return (rv)
			   (< (posix.file-modification-time source-pathname)
			      (posix.file-modification-time binary-pathname))
			 (unless rv
			   (%print-library-verbose-message
			    "warning: not using fasl file ~s because it is older than the source file ~s"
			    binary-pathname source-pathname))))
		  ;;The library binary file exists in  the store and it is newer than
		  ;;the source  library.  We  try the binary  library first,  and the
		  ;;source library next.
		  (%handle-local-binary-file-match options libref binary-pathname source-pathname
						   further-source-file-match search-fail-kont)
		;;No suitable  binary library in  the store.  Try the  source library
		;;directly.
		(begin
		  ((failed-library-location-collector) binary-pathname)
		  (%handle-source-file-match options libref source-pathname further-source-file-match search-fail-kont)))))
	;;No matching source  library was found in local  directories.  Tail-call the
	;;fail continuation to search for a compiled library installed under a system
	;;directory.
	(begin
	  (%print-library-debug-message "~a: no source file for: ~a" __module_who__ libref)
	  (search-fail-kont)))))

;;; --------------------------------------------------------------------

  (define (%handle-local-binary-file-match options libref binary-pathname source-pathname
					   next-source-file-match search-fail-kont)
    ;;This  function handles  the case:  a library  source file  matching LIBREF  was
    ;;found; a library binary  file matching LIBREF was also found  in the store, and
    ;;it is newer than the source library.
    ;;
    ;;First we try to open the binary library and return values to the caller; if the
    ;;binary library is cannot be opened or  it is rejected, we default to the source
    ;;library.
    ;;
    (define (%continue)
      ;;If we are here either opening the binary library failed or the binary library
      ;;was rejected.  We go for the source library.
      ;;
      ((failed-library-location-collector) binary-pathname)
      (%print-library-debug-message "~a: rejected: ~a" __module_who__ binary-pathname)
      (%handle-source-file-match options libref source-pathname next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%print-library-debug-message "~a: opening: ~a" __module_who__ binary-pathname)
		(%open-binary-library binary-pathname)))
	    %continue))

  (define (%handle-source-file-match options libref source-pathname next-source-file-match search-fail-kont)
    ;;This  function handles  the case:  a library  source file  matching LIBREF  was
    ;;found; either  a library  binary file  matching LIBREF was  *not* found  in the
    ;;store, or it was found but rejected.
    ;;
    (define (%continue)
      ;;If we are here the source library  pathname has been rejected.  We move on to
      ;;the next search result.
      ;;
      ((failed-library-location-collector) source-pathname)
      (%print-library-debug-message "~a: rejected: ~a" __module_who__ source-pathname)
      (%locator-search-step options libref next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(%print-library-debug-message "~a: opening: ~a" __module_who__ source-pathname)
		(%open-source-library source-pathname)))
	    %continue))

  #| end of module: COMPILE-TIME-LIBRARY-LOCATOR |# )


;;;; locating source and binary libraries: source locator
;;
;;The reference scenario for the source library locator is this:
;;
;;* We  have installed  the package  Vicare Scheme,  compiling bundled  libraries and
;;  putting them  in some  system directory;  the libraries  might be  installed with
;;  pathnames:
;;
;;      /usr/local/lib/vicare-scheme/vicare/posix.fasl
;;
;;*  We have  unpacked the  distribution tarball  of a  package providing  additional
;;  Vicare libraries.  We have the source libraries under:
;;
;;      $(srcdir)/lib/vicare/this.sls
;;      $(srcdir)/lib/vicare/that.sls
;;
;;  we want to compile them under the build directory:
;;
;;      $(builddir)/lib/vicare/this.fasl
;;      $(builddir)/lib/vicare/that.fasl
;;
;;  and then install them in a system directory:
;;
;;      /usr/local/lib/vicare-scheme/vicare/this.fasl
;;      /usr/local/lib/vicare-scheme/vicare/that.fasl
;;
;;  In the package's  building infrastructure (for example a Makefile  managed by the
;;  GNU Autotools) we need to write  appropriate invocations of "vicare" to build the
;;  libraries  locally  and  pick  the  appropriate  source  libraries  and  compiled
;;  libraries.
;;
;;  We want to automatically generate an include Makefile holding the compilation and
;;  installation recipes correctly describing the dependencies among libraries.
;;
;;* It  may be that  the local  libraries need to  load libraries installed  from the
;;  Vicare Scheme distribution, and also have local dependencies:
;;
;;      (library (vicare this)
;;        (export)
;;        (import (vicare)
;;          (vicare that))
;;        ---)
;;
;;      (library (vicare that)
;;        (export)
;;        (import (vicare)
;;          (prefix (vicare posix) posix.))
;;        ---)
;;
;;* It  may be that an  older version of the  package is already installed,  so there
;;  already exist installed binary libraries:
;;
;;      /usr/local/lib/vicare-scheme/vicare/this.fasl
;;      /usr/local/lib/vicare-scheme/vicare/that.fasl
;;
;;  we want the binary libraries  under "/usr/local/lib/vicare-scheme" to be ignored.
;;  It may be that there exist installed source libraries:
;;
;;      /usr/local/lib/vicare-scheme/vicare/this.sls
;;      /usr/local/lib/vicare-scheme/vicare/that.sls
;;
;;  we want  the source libraries under  "$(srcdir)/lib" to take precedence  over the
;;  libraries under "/usr/local/lib/vicare-scheme".
;;
;;At the Scheme level we want the following:
;;
;;*  Configure the  library  source search  path  to include  only  the local  source
;;  directory:
;;
;;     (library-source-search-path)
;;     => ("$(srcdir)/lib")
;;
;;* Configure the library binary search path to include the system directory:
;;
;;     (library-binary-search-path)
;;     => (... "/usr/local/lib/vicare-scheme" ...)
;;
;;* Configure the library binary file locator parameter:
;;
;;     (current-library-binary-search-path-scanner default-library-binary-search-path-scanner)
;;
;;  which will scan the search path in "(library-binary-search-path)".
;;
;;* Configure the library source file locator parameter:
;;
;;     (current-library-source-search-path-scanner default-library-source-search-path-scanner)
;;
;;  which will scan the search path in "(library-source-search-path)".
;;
;;* Configure the library locator parameter:
;;
;;     (current-library-locator source-library-locator)
;;
;;  which implements the appropriate policy.
;;
;;To achieve the desired result, in the Makefile we write rules as follows:
;;
;;   .PHONY: dependencies
;;
;;   dependencies:
;;          vicare --library-locator source					\
;;             -F /usr/local/lib/vicare-scheme					\
;;             -L $(srcdir)/lib							\
;;             --r6rs-script $(srcdir)/scripts/build-makefile-rules.sps --	\
;;             $(slsdir)/libraries.scm >$(slsdir)/dependencies.make
;;
(define* (source-library-locator {libref library-reference?})
  ;;Possible value for the parameter  CURRENT-LIBRARY-LOCATOR; this function is meant
  ;;to be  used to search  for source libraries only  first and for  binary libraries
  ;;later.
  ;;
  ;;Given a R6RS library reference and a list of search options: return a thunk to be
  ;;used to start the search for a matching library.
  ;;
  ;;The  returned  thunk  uses  the  current source  library  locator  referenced  by
  ;;CURRENT-LIBRARY-SOURCE-SEARCH-PATH-SCANNER, with the purpose of finding a library
  ;;file that matches a given R6RS library reference.  If no source library is found:
  ;;the       current      binary       library      locator       referenced      by
  ;;CURRENT-LIBRARY-BINARY-SEARCH-PATH-SCANNER is used.
  ;;
  ;;When successful the returned thunk returns 2 values:
  ;;
  ;;1. An input  port.  If it is a  textual input port: a source library  can be read
  ;;   from it.  If it is a binary input  port: a binary library can be read from it.
  ;;   It  is responsibility of the  caller to close  the returned port when  no more
  ;;   needed.
  ;;
  ;;2. A thunk to be called to continue  the search.  This thunk allows the caller to
  ;;   reject a library if it does  not meet some additional constraint; for example:
  ;;   if its version number does not conform to LIBREF.
  ;;
  ;;When no matching library is found: the returned thunk returns false and false.
  ;;
  (import LIBRARY-LOCATOR-UTILS)
  (%print-library-debug-message "~a: locating library for: ~a" __who__ libref)
  (let ((options (current-library-locator-options)))
    (%source-search-start libref options (%binary-search-start libref options))))


;;;; parameters

(define current-library-locator
  ;;Hold a function  used to locate a  library from its R6RS  library reference.  The
  ;;selected locator function must accept as single argument a R6RS library reference
  ;;and it must return a thunk as single value.
  ;;
  ;;When invoked, the returned thunk must return two values:
  ;;
  ;;* When a matching source library is found:
  ;;
  ;;  1. A textual input port from which the library can be read.
  ;;
  ;;  2. A  thunk to be called to  continue the search in case the  source library is
  ;;     rejected.
  ;;
  ;;* When a matching binary library is found:
  ;;
  ;;  1. A binary input port from which the library can be read.
  ;;
  ;;  2. A  thunk to be called to  continue the search in case the  binary library is
  ;;     rejected.
  ;;
  ;;* When a matching library is found and interned by the thunk itself:
  ;;
  ;;  1. The boolean true.
  ;;
  ;;  2. A thunk to be called to continue the search in case the library is rejected.
  ;;
  ;;* When no matching library is found:
  ;;
  ;;  1. The boolean false.
  ;;
  ;;  2. The boolean false.
  ;;
  ;;When an input port is returned as first value: it is responsibility of the caller
  ;;to  close  the returned  port  when  no more  needed;  the  thunk discharges  any
  ;;responsibility.
  ;;
  ;;When a thunk is returned as second value:  it must have the same API of the thunk
  ;;returned by the  locator function; the possibility to continue  the search allows
  ;;the caller to reject a library if it does not meet some additional constraint.
  ;;
  ;;The parameter is meant to be used as in the following pseudo-code:
  ;;
  ;;   (let loop ((next-locator-search ((current-library-locator) libref)))
  ;;     (receive (rv further-locator-search)
  ;;         (next-locator-search)
  ;;       (cond ((binary-port?  rv)
  ;;              (read-validate-intern-binary-library rv
  ;;                  (lambda ()
  ;;                    (loop further-locator-search))))
  ;;             ((textual-port? rv)
  ;;              (read-validate-intern-source-library rv
  ;;                  (lambda ()
  ;;                    (loop further-locator-search))))
  ;;             ((and (boolean? rv) rv)
  ;;              (library-already-interned))
  ;;             ((not rv)
  ;;              (no-matching-library-was-found))
  ;;             (else
  ;;              (assertion-violation __who__
  ;;                "invalid return values from library locator" rv)))))
  ;;
  (make-parameter
      run-time-library-locator
    (lambda* ({obj procedure?})
      obj)))


;;;; serialising compiled library files

(define* (default-library-serialiser {lib libman.library?} {binary-pathname posix.file-string-pathname?})
  ;;Default  value for  the  parameter CURRENT-LIBRARY-SERIALISER.   Given a  LIBRARY
  ;;object: serialise  the compiled library  in a  given FASL file  pathname.  Return
  ;;unspecified values.
  ;;
  ;;Only libraries loaded  form source files are serialised, if  a library was loaded
  ;;from  a FASL  file: nothing  happens.   If a  FASL  file already  exists for  the
  ;;library: it is silently overwritten.
  ;;
  (when (libman.library-loaded-from-source-file? lib)
    (let ((source-pathname (libman.library-source-file-name lib)))
      (%print-library-verbose-message "serialising library: ~a" binary-pathname)
      (serialise-library lib
			 (lambda (source-pathname libname contents)
			   (store-full-serialised-library-to-file binary-pathname source-pathname libname contents))
			 (lambda (core-expr)
			   (compile-core-expr core-expr))))))

(define current-library-serialiser
  ;;References a  function used to  serialise a compiled  library into a  gieven FASL
  ;;file pathname.
  ;;
  ;;The referenced  function must accept 2  arguments: a LIBRARY object  and a string
  ;;file pathname representing  the FASL pathname; it can  return unspecified values;
  ;;if an error occurs it must raise an exception.
  ;;
  ;;Only libraries loaded form source files  must be serialised: if the given library
  ;;was loaded from a binary file: nothing must happen.
  ;;
  (make-parameter
      default-library-serialiser
    (lambda* ({obj procedure?})
      obj)))

;;; --------------------------------------------------------------------

(define* (default-library-store-directory-serialiser {lib libman.library?})
  ;;Default  value  for   the  parameter  CURRENT-LIBRARY-STORE-DIRECTORY-SERIALISER.
  ;;Given aLIBRARY  object: serialise the compiled  library in a FASL  file under the
  ;;currently selected store directory.  Return unspecified values.  Serialisation is
  ;;performed  with the  library  serialiser procedure  referenced  by the  parameter
  ;;CURRENT-LIBRARY-SERIALISER.
  ;;
  (when (libman.library-loaded-from-source-file? lib)
    (let ((binary-pathname (library-name->library-binary-pathname-in-store-directory (libman.library-name lib))))
      ((current-library-serialiser) lib binary-pathname))))

(define current-library-store-directory-serialiser
  ;;References a  function used to  serialise a compiled library  into a file  in the
  ;;currently selected store directory.
  ;;
  ;;The referenced  function must accept 2  arguments: a LIBRARY object  and a string
  ;;file pathname representing  the FASL pathname; it can  return unspecified values;
  ;;if an error occurs it must raise an exception.
  ;;
  ;;Only libraries loaded form source files  are serialised: if the given library was
  ;;loaded from a FASL file: nothing must happen.
  ;;
  (make-parameter
      default-library-store-directory-serialiser
    (lambda* ({obj procedure?})
      obj)))

;;; --------------------------------------------------------------------

(define (serialise-collected-libraries serialise compile)
  ;;Traverse the  current collection of libraries  and serialise the contents  of all
  ;;the LIBRARY objects  that were loaded from source; to  "serialise" means to write
  ;;the compiled contents in a FASL file.  Return unspecified values.
  ;;
  ;;SERIALISE must be a closure to be invoked as follows:
  ;;
  ;;   (serialise ?source-pathname ?libname ?contents)
  ;;
  ;;where: ?SOURCE-PATHNAME  must be  a string representing  the source  library file
  ;;pathname; ?LIBNAME  must be a  symbolic expression representing a  R6RS compliant
  ;;library name;  ?CONTENTS must  be a  value representing  the compiled  library as
  ;;defined by the procedure SERIALISE-LIBRARY.
  ;;
  ;;COMPILE must be a closure to be invoked as follows:
  ;;
  ;;   (compile ?core-language-expression)
  ;;
  ;;where ?CORE-LANGUAGE-EXPRESSION  must be  a symbolic expression  representing the
  ;;invoke, visit or guard code from the  library; the closure must compile the given
  ;;core language  expression and return  a closure  object wrapping the  code object
  ;;representing the init expression.
  ;;
  (for-each (lambda (lib)
	      (serialise-library lib serialise compile))
    ((libman.current-library-collection))))

(define* (serialise-library {lib libman.library?} {serialise procedure?} {compile procedure?})
  ;;Compile and serialise the given LIBRARY object.  Return unspecified values.
  ;;
  ;;SERIALISE must be a closure to be invoked as follows:
  ;;
  ;;   (serialise ?source-pathname ?libname ?contents)
  ;;
  ;;where: ?SOURCE-PATHNAME  must be  a string representing  the source  library file
  ;;pathname; ?LIBNAME  must be a  symbolic expression representing a  R6RS compliant
  ;;library name;  ?CONTENTS must  be a  value representing  the compiled  library as
  ;;defined by this procedure.
  ;;
  ;;COMPILE must be a closure to be invoked as follows:
  ;;
  ;;   (compile ?core-language-expression)
  ;;
  ;;where ?CORE-LANGUAGE-EXPRESSION  must be  a symbolic expression  representing the
  ;;invoke, visit or guard code from the  library; the closure must compile the given
  ;;core language  expression and return  a closure  object wrapping the  code object
  ;;representing the init expression.
  ;;
  (let ((libname         (libman.library-name lib))
	(source-pathname (libman.library-source-file-name lib)))
    (serialise source-pathname libname
	       (list (libman.library-uid lib)
		     libname
		     (map libman.library-descriptor (libman.library-imp-lib* lib))
		     (map libman.library-descriptor (libman.library-vis-lib* lib))
		     (map libman.library-descriptor (libman.library-inv-lib* lib))
		     (libman.library-export-subst lib)
		     (libman.library-export-env lib)
		     (compile (libman.library-visit-code lib))
		     (compile (libman.library-invoke-code lib))
		     (compile (libman.library-guard-code lib))
		     (map libman.library-descriptor (libman.library-guard-lib* lib))
		     (libman.library-visible? lib)
		     (libman.library-option* lib)
		     source-pathname))))

;;; --------------------------------------------------------------------
;;; reading and writing binary libraries in FASL files

(module (read-library-binary-port
	 store-full-serialised-library-to-port
	 store-full-serialised-library-to-file)

  (define-struct serialised-library
    (contents
		;A list of  values representing a LIBRARY  object holding precompiled
		;code.   For   details  on   the  format  see   SERIALISE-LIBRARY  in
		;"psyntax.library-manager.sls".
     ))

  (define* (read-library-binary-port {port binary-input-port?}
				     {success-kont procedure?}
				     {verify-libname procedure?})
    ;;Given  a string  representing the  existent pathname  of a  FASL file:  load it
    ;;looking for a full binary library serialisation, then apply SUCCESS-KONT to the
    ;;library contents.
    ;;
    ;;If successful: return the result of  the SUCCESS-KONT application.  If the file
    ;;has invalid contents: return false.
    ;;
    ;;VERIFY-LIBNAME must be a procedure accepting  a single argument; it must verify
    ;;that  the  argument is  a  R6RS  library name  and  that  it conforms  to  some
    ;;additional constraint (especially  the version); it must raise  an exception if
    ;;something is wrong; otherwise it should just return unspecified values.
    ;;
    (let ((libname (fasl-read-object port)))
      (verify-libname libname)
      (let ((x (fasl-read-object port)))
	(and (serialised-library? x)
	     (apply success-kont (serialised-library-contents x))))))

  (define* (store-full-serialised-library-to-port {port binary-output-port?}
						  {source-pathname posix.file-string-pathname?}
						  {libname library-name?}
						  contents)
    ;;Given a  binary output port:  store the CONTENTS  of a serialised  full library
    ;;into it.  Return unspecified values.
    ;;
    ;;SOURCE-PATHNAME  must be  a  string  representing the  pathname  of the  source
    ;;library file (or equivalent).
    ;;
    ;;LIBNAME must be a R6RS library name.
    ;;
    ;;CONTENTS  must be  a  list  of values  representing  a  LIBRARY object  holding
    ;;precompiled     code.      See     the    function     SERIALISE-LIBRARY     in
    ;;"psyntax.library-manager.sls" for details on the format.
    ;;
    (fasl-write-header port)
    (fasl-write-object libname port)
    (fasl-write-object (make-serialised-library contents) port
		       (retrieve-filename-foreign-libraries source-pathname)))

  (define* (store-full-serialised-library-to-file {binary-pathname posix.file-string-pathname?}
						  {source-pathname posix.file-string-pathname?}
						  {libname library-name?}
						  contents)
    ;;Given  the FASL  pathname of  a compiled  library to  be serialised:  store the
    ;;CONTENTS into it,  creating a new file or overwriting  an existing one.  Return
    ;;unspecified values.
    ;;
    ;;BINARY-PATHNAME  must be  a  string  representing the  pathname  of the  binary
    ;;library FASL file.
    ;;
    ;;SOURCE-PATHNAME  must be  a  string  representing the  pathname  of the  source
    ;;library file (or equivalent).
    ;;
    ;;LIBNAME must be a R6RS library name.
    ;;
    ;;CONTENTS  must be  a  list  of values  representing  a  LIBRARY object  holding
    ;;precompiled     code.      See     the    function     SERIALISE-LIBRARY     in
    ;;"psyntax.library-manager.sls" for details on the format.
    ;;
    (%print-library-verbose-message "serialising ~a ..." binary-pathname)
    (receive (dir name)
	(posix.split-pathname-root-and-tail binary-pathname)
      (unless (string-empty? dir)
	(posix.mkdir/parents dir #o755)))
    (let ((port (open-file-output-port binary-pathname (file-options no-fail))))
      (unwind-protect
	  (store-full-serialised-library-to-port port source-pathname libname contents)
	(close-output-port port)))
    (%print-library-verbose-message "library serialisation done"))

  #| end of module |# )


;;;; loading source programs

(define* (load-r6rs-script {file-pathname posix.file-string-pathname?} serialise? run?)
  ;;Load source code  from FILE-PATHNAME, which must be a  string representing a file
  ;;pathname, expecting an R6RS program or an R6RS library and compile it.
  ;;
  ;;If  SERIALISE?  is true:  the  libraries  needed  by  the program  are  compiled,
  ;;serialised and saved in FASL files.
  ;;
  ;;If RUN? is true: the loaded R6RS program is compiled and evaluated.
  ;;
  (%print-library-verbose-message "~a: loading R6RS script: ~a" __who__ file-pathname)
  (let* ((prog  (read-script-source-file file-pathname))
	 (thunk (parametrise ((libman.source-code-location file-pathname))
		  (expand-r6rs-top-level-make-evaluator prog))))
    (when serialise?
      (serialise-collected-libraries
       (lambda (source-pathname libname contents)
	 (store-full-serialised-library-to-file
	  (cond ((compiled-libraries-store-directory)
		 (library-name->library-binary-pathname-in-store-directory libname))
		(else
		 (error __who__
		   "cannot determine a destination directory for compiled library files")))
	  source-pathname libname contents))
       (lambda (core-expr)
	 (compile-core-expr core-expr))))
    (when run?
      (thunk))))

(case-define* load
  ;;Load source code  from FILE-PATHNAME, which must be a  string representing a file
  ;;pathname, expecting: an  R6RS program, an R6RS  library or just a  list of forms.
  ;;Then transform the  contents of the file  in a list of  symbolic expressions; for
  ;;each form in the source apply EVAL-PROC to the corresponding symbolic expression.
  ;;
  ;;When  EVAL-PROC   is  not  given:  the   forms  are  evaluated  in   the  current
  ;;INTERACTION-ENVIRONMENT.
  ;;
  ((file-pathname)
   (load file-pathname (lambda (sexp)
			 (eval sexp (interaction-environment)))))
  (({file-pathname posix.file-string-pathname?} {eval-proc procedure?})
   (%print-library-verbose-message "~a: loading script: ~a" __who__ file-pathname)
   (let next-form ((ls (read-script-source-file file-pathname)))
     (unless (null? ls)
       (eval-proc (car ls))
       (next-form (cdr ls))))))


;;;; compiling libraries

(module (compile-source-library)

  (define* (compile-source-library {source-pathname posix.file-string-pathname?}
				   {binary-pathname %false-or-file-string-pathname?})
    ;;Load the  first LIBRARY form from  the given file pathname;  expand it, compile
    ;;it, serialise it.  Return unspecified values.
    ;;
    ;;The source library  is expanded with the procedure currently  referenced by the
    ;;parameter  CURRENT-LIBRARY-EXPANDER.   If  the BINARY-PATHNAME  is  given:  the
    ;;binary library  is serialised  with the procedure  currently referenced  by the
    ;;parameter  CURRENT-LIBRARY-SERIALISER;  otherwise  it is  serialised  with  the
    ;;procedure       currently        referenced       by        the       parameter
    ;;CURRENT-LIBRARY-STORE-DIRECTORY-SERIALISER.
    ;;
    (cond ((let ((libsexp (%read-first-library-form-from-source-library-file __who__ source-pathname)))
	     ;;We receive all these values, but we are interested only in the LIBNAME.
	     (receive (uid libname
			   import-libdesc* visit-libdesc* invoke-libdesc*
			   invoke-code visit-code
			   export-subst export-env
			   guard-code guard-libdesc*
			   option*)
		 ((libman.current-library-expander) libsexp source-pathname (lambda (libname) (void)))
	       (libman.find-library-by-name libname)))
	   => (lambda (lib)
		(if binary-pathname
		    ((current-library-serialiser) lib binary-pathname)
		  ((current-library-store-directory-serialiser) lib))))
	  (else
	   (error __who__
	     "unable to retrieve LIBRARY object after expanding and compiling source library"
	     source-pathname))))

  (define (%read-first-library-form-from-source-library-file who source-pathname)
    (module (%open-source-library)
      (import LIBRARY-LOCATOR-UTILS))
    (%print-library-verbose-message "~a: loading library: ~a" who source-pathname)
    (let ((port (%open-source-library source-pathname)))
      (unwind-protect
	  (read-library-source-port port)
	(close-input-port port))))

  #| end of module |# )


;;;; compiling source programs to binary programs

(module (compile-source-program
	 run-compiled-program)

  ;;NOTE To use  a struct type here looks  cool.  But, if in future we  want to allow
  ;;compiled programs to be independent from  specific boot image builds, we may need
  ;;to switch to a basic Scheme object, like a vector:
  ;;
  ;;   #(vicare-compiled-program ?lib-descr* ?closure)
  ;;
  ;;so the API would be:
  ;;
  ;;   (define (make-serialised-program lib-descr* closure)
  ;;     (vector 'vicare-compiled-program lib-descr* closure))
  ;;
  ;;   (define (serialised-program-lib-descr* prog)
  ;;     (vector-ref prog 1))
  ;;
  ;;   (define (serialised-program-closure prog)
  ;;     (vector-ref prog 2))
  ;;
  ;;(Marco Maggi; Mon Dec 15, 2014)
  ;;
  (define-struct serialised-program
    (lib-descr*
		;A   list  of   library  descriptors   representing  the
		;libraries needed to run the program.
     closure
		;A  closure  object  representing the  program.   To  be
		;evaluated after having invoked the required libraries.
     ))

  (define* (compile-source-program {source-filename posix.file-string-pathname?}
				   {binary-filename %false-or-file-string-pathname?})
    ;;Read the  file referenced by  the pathname  SOURCE-FILENAME; expand it  as R6RS
    ;;program with Vicare extensions; compile  it; store the compiled result.  Return
    ;;unspecified results.
    ;;
    ;;If BINARY-FILENAME is a valid file  pathname: the compiled program is stored in
    ;;the selected file, overwriting old file contents.  If BINARY-FILENAME is false:
    ;;a  pathname  is built  from  SOURCE-FILENAME  using  a default  procedure.   If
    ;;BINARY-FILENAME is invalid: an exception is raised.
    ;;
    (receive (lib-descr* thunk)
	((expand-r6rs-top-level-make-compiler (read-script-source-file source-filename)))
      (store-serialised-program source-filename lib-descr* thunk binary-filename)))

  (define* (run-compiled-program {binary-filename posix.file-string-pathname?})
    (receive (lib-descr* closure)
	(load-serialised-program binary-filename)
      (for-each (lambda (descr)
		  (cond ((libman.find-library-by-descriptor descr)
			 => (lambda (lib)
			      (libman.invoke-library lib)))
			(else
			 (error __who__
			   "unable to load library required by program" descr))))
	lib-descr*)
      #;(debug-print 'running-thunk closure)
      (closure)))

;;; --------------------------------------------------------------------

  (define* (load-serialised-program {binary-filename posix.file-string-pathname?})
    ;;Given the file name  of a serialised program: load it  and verify its contents.
    ;;Return  2  values: a  list  of  library  descriptors representing  the  program
    ;;dependencies; a closure object (thunk) to be called to run the program.
    ;;
    (cond ((not (posix.file-string-pathname? binary-filename))
	   (%error-invalid-pathname __who__
	     "invalid string as selected serialised program file" binary-filename))
	  ((not (file-exists? binary-filename))
	   (%error-invalid-pathname __who__
	     "selected serialised program file does not exist" binary-filename))
	  (else
	   (let ((x (let ((port (open-file-input-port binary-filename)))
		      (unwind-protect
			  (fasl-read port)
			(close-input-port port)))))
	     (if (serialised-program? x)
		 (values (serialised-program-lib-descr* x)
			 (serialised-program-closure    x))
	       (error __who__
		 "invalid contents in selected serialised program file"
		 binary-filename))))))

  (define* (store-serialised-program source-filename lib-descr* closure binary-filename)
    ;;Given  the source  name of  an  R6RS script,  the list  of library  descriptors
    ;;required for its execution, a closure object  to be called to run it: write the
    ;;serialised program FASL file.
    ;;
    (let ((binary-filename (%make-binary-filename __who__ binary-filename source-filename)))
      (%print-library-verbose-message "serialising ~a ... " binary-filename)
      (receive (dir name)
	  (posix.split-pathname-root-and-tail binary-filename)
	(unless (string-empty? dir)
	  (posix.mkdir/parents dir #o755)))
      (let ((port (open-file-output-port binary-filename
					 ;;FIXME To  be uncommented at the  next boot
					 ;;image rotation.  (Marco Maggi; Mon Dec 15,
					 ;;2014)
					 #;(file-options no-fail executable)
					 (let ()
					   (import (ikarus enumerations))
					   (make-file-options '(no-fail executable))))))
	(unwind-protect
	    (fasl-write (make-serialised-program lib-descr* closure) port
			(retrieve-filename-foreign-libraries source-filename))
	  (close-output-port port)))
      (%print-library-verbose-message "done")))

;;; --------------------------------------------------------------------

  (define (%make-binary-filename who binary-filename source-filename)
    (cond ((posix.file-string-pathname? binary-filename)
	   binary-filename)
	  ((not binary-filename)
	   (program-source-pathname->program-binary-pathname source-filename))
	  (else
	   (%error-invalid-pathname who
	     "invalid FASL filename for serialised program"
	     binary-filename))))

  (define (%error-invalid-pathname who message pathname)
    (raise
     (condition (make-who-condition who)
		(make-message-condition message)
		(make-i/o-filename-error pathname)
		(make-irritants-condition (list pathname)))))

  #| end of module |# )


;;;; locating and loading include files

(define* (default-include-loader include-pathname verbose? synner)
  ;;Default value for  the parameter CURRENT-INCLUDE-LOADER.  Search  an include file
  ;;with  name  INCLUDE-PATHNAME.  When  successful  return  2 values:  the  absolute
  ;;pathname from which  the file was loaded, a symbolic  expresison representing the
  ;;file contents.  When  an error occurs: call the procedure  SYNNER, which is meant
  ;;to raise an exception.
  ;;
  ;;If  VERBOSE?  is  true:  display  verbose messages  on  the  current  error  port
  ;;describing the including process.
  ;;
  ;;The include  file is  searched using  the procedure  referenced by  the parameter
  ;;CURRENT-INCLUDE-FILE-LOCATOR.  The file is  loaded using the procedure referenced
  ;;by the parameter CURRENT-INCLUDE-FILE-LOADER.
  ;;
  (unless (posix.file-string-pathname? include-pathname)
    (synner "file name must be a non-empty string representing a valid pathname" include-pathname))
  (when verbose?
    (fprintf (current-error-port) "Vicare: searching include file: ~a\n" include-pathname))
  (let ((include-pathname ((current-include-file-locator) include-pathname synner)))
    (when verbose?
      (fprintf (current-error-port) "Vicare: including file: ~a\n" include-pathname))
    (values include-pathname ((current-include-file-loader) include-pathname synner))))

(module ()
  (libman.current-include-loader default-include-loader))

;;; --------------------------------------------------------------------

(define* (default-include-file-locator include-pathname {synner procedure?})
  ;;Default  value for  the parameter  CURRENT-INCLUDE-FILE-LOCATOR.  Given  a string
  ;;INCLUDE-PATHNAME  which must  represent an  absolute or  relative file  pathname,
  ;;convert it into the absolute pathname of an existing file, as string.  Return the
  ;;absolute string pathname.
  ;;
  ;;If INCLUDE-PATHNAME  is a relative pathname:  the file is searched  in the search
  ;;path represented by LIBRARY-SOURCE-SEARCH-PATH,  by appending INCLUDE-PATHNAME to
  ;;the directories in the search path.
  ;;
  ;;SYNNER must be a procedure used to raise an exception when an error occurs.
  ;;
  (unless (posix.file-string-pathname? include-pathname)
    (synner "file name must be a non-empty string representing a valid pathname" include-pathname))
  (if (posix.file-string-absolute-pathname? include-pathname)
      ;;It is an absolute pathname.
      (posix.real-pathname include-pathname)
    ;;It is a relative pathname.  Search the file in the library source search path.
    (let loop ((ls (library-source-search-path)))
      (if (pair? ls)
	  (let ((ptn (string-append (car ls) "/" include-pathname)))
	    (if (file-exists? ptn)
		(posix.real-pathname ptn)
	      (loop (cdr ls))))
	(synner "file does not exist in library source path" include-pathname)))))

(define current-include-file-locator
  ;;Hold a function used to convert an  include file name into the corresponding file
  ;;pathname.  The referenced function must accept 3 arguments: a string representing
  ;;the include  file name; a  boolean, true if the  process of loading  must display
  ;;verbose messages  on the  current error  port; a synner  function used  to report
  ;;errors.
  ;;
  ;;The synner function is called as:
  ;;
  ;;   (synner ?error-message ?irritants)
  ;;
  (make-parameter
      default-include-file-locator
    (lambda* ({obj procedure?})
      obj)))

;;; --------------------------------------------------------------------

(define* (default-include-file-loader {include-pathname posix.file-string-pathname?} {synner procedure?})
  ;;Default  value  for the  parameter  CURRENT-INCLUDE-FILE-LOADER.   Open the  file
  ;;INCLUDE-PATHNAME, read all  the datums and return them.  If  an error occurs call
  ;;SYNNER.  The returned  contents must be a list of  annotated symbolic expressions
  ;;as returned by the reader.
  ;;
  (guard (E (else
	     (synner (condition-message E) include-pathname)))
    (let ((port (open-input-file include-pathname)))
      (unwind-protect
	  (let recur ()
	    (let ((datum (get-annotated-datum port)))
	      (if (eof-object? datum)
		  '()
		(cons datum (recur)))))
	(close-input-port port)))))

(define current-include-file-loader
  ;;Hold  a function  used to  load an  include file.   The referenced  function must
  ;;accept 2  arguments: a string  representing an  existent file pathname;  a synner
  ;;function used to report errors.
  ;;
  ;;The synner function is called as:
  ;;
  ;;   (synner ?error-message ?irritants)
  ;;
  (make-parameter
      default-include-file-loader
    (lambda* ({obj procedure?})
      obj)))


;;;; done

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.load"))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'read-library-binary-port		'scheme-indent-function 2)
;; eval: (put '%error-invalid-pathname		'scheme-indent-function 1)
;; End:
