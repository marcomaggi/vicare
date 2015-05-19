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
    load

    ;; stuff for (vicare libraries)
    default-library-loader
    current-source-library-loader
    current-binary-library-loader

    current-library-locator
    run-time-library-locator
    compile-time-library-locator
    source-library-locator

    default-include-loader
    default-include-file-locator
    default-include-file-loader

    current-include-file-locator
    current-include-file-loader

    ;; internal stuff
    load-r6rs-script
    compile-source-program
    run-compiled-program
    compile-source-library
    current-library-serialiser
    current-library-serialiser-in-build-directory)
  (import (except (vicare)
		  load
		  current-include-loader
		  default-include-loader
		  default-include-file-locator
		  default-include-file-loader
		  current-include-file-locator
		  current-include-file-loader)
    (prefix (ikarus.posix)
	    posix.)
    (prefix (only (ikarus.compiler)
		  compile-core-expr-to-thunk)
	    compiler.)
    (prefix (psyntax.library-manager) libman.)
    (only (psyntax.expander)
	  expand-top-level-make-compiler)
    (only (psyntax.compat)
	  print-expander-warning-message)
    (prefix (only (ikarus.reader)
		  read-script-from-file
		  read-library-from-port)
	    reader.)
    (psyntax.library-utils)
    (only (ikarus fasl read)
	  fasl-read-header
	  fasl-read-object)
    (only (ikarus.fasl.write)
	  fasl-write-header
	  fasl-write-object))


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
    (print-library-debug-message "~a: searching: ~a" __module_who__ libref)
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
	  (print-library-debug-message "~a: reading from: ~a" __module_who__ rv)
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
  (define (%print-loading-library  port) (print-library-debug-message "loading:  ~a" (port-id port)))
  (define (%print-loaded-library   port) (print-library-debug-message "loaded:   ~a" (port-id port)))
  (define (%print-rejected-library port) (print-library-debug-message "rejected: ~a" (port-id port)))

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


;;;; interning libraries: source library loader

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
  (print-library-debug-message "~a: reading library from port: ~a" __who__ port)
  (let ((source-pathname  (port-id port))
	(libsexp          (reader.read-library-from-port port)))
    ;;If the library name extracted from LIBSEXP  does not conform to LIBREF: we make
    ;;the expander raise  an exception with REJECT-KEY as raised  object; we catch it
    ;;here and return false.
    (define (%verify-libname libname)
      (unless (conforming-library-name-and-library-reference? libname libref)
	(raise REJECT-KEY)))
    (guard (E ((eq? E REJECT-KEY)
	       (print-library-debug-message "~a: rejected library from port: ~a" __who__ port)
	       #f))
      ;;This  call  to  the  library  expander loads  and  interns  all  the  library
      ;;dependencies using FIND-LIBRARY-BY-REFERENCE.
      (let ((lib ((libman.current-library-expander) libsexp source-pathname %verify-libname)))
	(receive-and-return (name)
	    (libman.library-name lib)
	  (print-library-verbose-message "loaded library \"~a\" from: ~a" name (port-id port)))))))

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


;;;; interning libraries: binary library loader

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
  (print-library-debug-message "~a: loading library from port: ~a" __who__ port)
  (let ((binary-pathname (port-id port)))
    ;;If the version  of the loaded library  does not conform to LIBREF:  we make the
    ;;loader raise  an exception with REJECT-KEY  as raised object; we  catch it here
    ;;and return false.
    (define (%verify-libname libname)
      (unless (conforming-library-name-and-library-reference? libname libref)
	(raise REJECT-KEY)))
    (guard (E ((eq? E REJECT-KEY)
	       (print-library-debug-message "~a: rejected library from port: ~a" __who__ port)
	       #f))
      (cond ((read-serialised-library-from-binary-port port %verify-libname)
	     => (lambda (serialised-lib)
		  (print-library-verbose-message "loaded library \"~a\" from: ~a"
						 (serialised-library-name serialised-lib)
						 (port-id port))
		  (let ((interned-lib (intern-binary-library-and-its-dependencies serialised-lib)))
		    (libman.library-name interned-lib))))
	    (else
	     (print-library-verbose-message
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


;;;; locating source and binary libraries: utilities

(module LIBRARY-LOCATOR-UTILS
  (%source-search-start
   %binary-search-start
   %open-source-library
   %open-binary-library)

  (module (%source-search-start)
    ;;This function can be used to search for a source library file using the current
    ;;source            library            locator           referenced            by
    ;;CURRENT-LIBRARY-SOURCE-SEARCH-PATH-SCANNER,  with  the  purpose  of  finding  a
    ;;library that matches a given R6RS library reference.
    ;;
    ;;The argument  LIBREF must be a  R6RS library reference.  The  optional argument
    ;;FAIL-KONT must  be a thunk  to be called when  the search fails.   The argument
    ;;OPTIONS  must be  an enum-set  of type  LIBRARY-LOCATOR-OPTION; at  present the
    ;;supported options are:
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
    ;;                                    (library-locator-options move-on-when-open-fails)
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
    ;;binary            library            locator           referenced            by
    ;;CURRENT-LIBRARY-BINARY-SEARCH-PATH-SCANNER,  with  the  purpose  of  finding  a
    ;;library that matches a given R6RS library reference.
    ;;
    ;;The argument  LIBREF must be a  R6RS library reference.  The  optional argument
    ;;FAIL-KONT must  be a thunk  to be called when  the search fails.   The argument
    ;;OPTIONS  must be  an enum-set  of type  LIBRARY-LOCATOR-OPTION; at  present the
    ;;supported options are:
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
    ;;                                    (library-locator-options move-on-when-open-fails)
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

(define* (run-time-library-locator {libref library-reference?})
  ;;Possible value for the parameter  CURRENT-LIBRARY-LOCATOR; this function is meant
  ;;to be used to search for libraries when running an application.
  ;;
  ;;Given a R6RS library reference: return a thunk to be used to start the search for
  ;;a  matching library.   The  returned thunk  scans the  search  path for  compiled
  ;;libraries in search of a matching binary  file; if a matching compiled library is
  ;;not found: it scans the search path  for source libraries in search of a matching
  ;;source file.
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
  (print-library-debug-message "~a: locating library for: ~a" __who__ libref)
  (let ((options (current-library-locator-options)))
    (%binary-search-start libref options (%source-search-start libref options))))


;;;; locating source and binary libraries: compile-time locator

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
    ;;library source file scanner in CURRENT-LIBRARY-SOURCE-SEARCH-PATH-SCANNER.  The
    ;;search  for compiled  libraries  is  performed using  the  library binary  file
    ;;scanner in CURRENT-LIBRARY-BINARY-SEARCH-PATH-SCANNER.
    ;;
    ;;The returned thunk does the following:
    ;;
    ;;1. Ask the library source file scanner for the next matching source file.
    ;;
    ;;2. If a matching source is found:  look for an already compiled library file in
    ;;   the COMPILED-LIBRARIES-BUILD-DIRECTORY:
    ;;
    ;;   2.1.  If  no compiled file exists or  it if exists but it is  older than the
    ;;        source file: accept the source file as matching.
    ;;
    ;;   2.2. If a compiled file exists and  it is newer than the source file: accept
    ;;        the compiled file as matching.
    ;;
    ;;   2.3. Return to the caller the matching file pathname.
    ;;
    ;;   2.4. If  the caller rejects the  binary file pathname: return  to the caller
    ;;        the source file pathname.
    ;;
    ;;   2.5. If the caller rejects the source file: loop to 1.
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
    (print-library-debug-message "~a: start search for library: ~a" __who__ libref)
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
	    (print-library-debug-message "~a: found source: ~a" __module_who__ source-pathname)
	    (let ((binary-pathname (library-reference->library-binary-pathname-in-build-directory libref)))
	      (print-library-debug-message "~a: checking binary: ~a" __module_who__ binary-pathname)
	      (if (and (file-exists? binary-pathname)
		       (receive-and-return (rv)
			   (< (posix.file-modification-time source-pathname)
			      (posix.file-modification-time binary-pathname))
			 (unless rv
			   (print-library-verbose-message
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
	  (print-library-debug-message "~a: no source file for: ~a" __module_who__ libref)
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
      (print-library-debug-message "~a: rejected: ~a" __module_who__ binary-pathname)
      (%handle-source-file-match options libref source-pathname next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(print-library-debug-message "~a: opening: ~a" __module_who__ binary-pathname)
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
      (print-library-debug-message "~a: rejected: ~a" __module_who__ source-pathname)
      (%locator-search-step options libref next-source-file-match search-fail-kont))
    (values (with-exception-handler
		(lambda (E)
		  (if (i/o-error? E)
		      (if (library-locator-options-no-raise-when-open-fails? options)
			  (%continue)
			(raise E))
		    (raise E)))
	      (lambda ()
		(print-library-debug-message "~a: opening: ~a" __module_who__ source-pathname)
		(%open-source-library source-pathname)))
	    %continue))

  #| end of module: COMPILE-TIME-LIBRARY-LOCATOR |# )


;;;; locating source and binary libraries: source locator

(define* (source-library-locator {libref library-reference?})
  ;;Possible value for the parameter  CURRENT-LIBRARY-LOCATOR; this function is meant
  ;;to be used to search for source libraries first and for binary libraries later.
  ;;
  ;;Given a R6RS library reference: return a thunk to be used to start the search for
  ;;a matching library.   The returned thunk uses the current  source library scanner
  ;;referenced  by CURRENT-LIBRARY-SOURCE-SEARCH-PATH-SCANNER,  with  the purpose  of
  ;;finding a library file that matches a given R6RS library reference.  If no source
  ;;library   is  found:   the   current  binary   library   scanner  referenced   by
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
  (print-library-debug-message "~a: locating library for: ~a" __who__ libref)
  (let ((options (current-library-locator-options)))
    (%source-search-start libref options (%binary-search-start libref options))))


;;;; library locator selection

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
    (print-library-verbose-message "serialising library: ~a" binary-pathname)
    (store-full-serialised-library-to-file binary-pathname lib)))

(define current-library-serialiser
  ;;References a  function used to  serialise a compiled  library into a  gieven FASL
  ;;file pathname.
  ;;
  ;;The referenced  function must accept 2  arguments: a LIBRARY object  and a string
  ;;file pathname representing the binary library  pathname (the pathname of the FASL
  ;;file); it  can return  unspecified values; if  an error occurs  it must  raise an
  ;;exception.
  ;;
  ;;Only libraries loaded form source files  must be serialised: if the given library
  ;;was loaded from a binary file: nothing must happen.
  ;;
  (make-parameter
      default-library-serialiser
    (lambda* ({obj procedure?})
      obj)))

;;; --------------------------------------------------------------------

(define* (default-library-build-directory-serialiser {lib libman.library?})
  ;;Default  value for  the parameter  CURRENT-LIBRARY-SERIALISER-IN-BUILD-DIRECTORY.
  ;;Given a LIBRARY object:  serialise the compiled library in a  FASL file under the
  ;;currently selected build directory.  Return unspecified values.  Serialisation is
  ;;performed  with the  library  serialiser procedure  referenced  by the  parameter
  ;;CURRENT-LIBRARY-SERIALISER.
  ;;
  (when (libman.library-loaded-from-source-file? lib)
    (let ((binary-pathname (library-name->library-binary-pathname-in-build-directory (libman.library-name lib))))
      ((current-library-serialiser) lib binary-pathname))))

(define current-library-serialiser-in-build-directory
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
      default-library-build-directory-serialiser
    (lambda* ({obj procedure?})
      obj)))

;;; --------------------------------------------------------------------
;;; reading and writing binary libraries in FASL files

(define* (read-serialised-library-from-binary-port {port binary-input-port?} {verify-libname procedure?})
  ;;Given a string representing the existent pathname of a FASL file: load it looking
  ;;for a  full binary library  serialisation, build a SERIALISED-LIBRARY  object and
  ;;return it.  If the file has invalid contents: return false.
  ;;
  ;;VERIFY-LIBNAME must  be a procedure accepting  a single argument; it  must verify
  ;;that the argument is a R6RS library  name and that it conforms to some additional
  ;;constraint (especially the  version); it must raise an exception  if something is
  ;;wrong; otherwise it should just return unspecified values.
  ;;
  (let ((libname (fasl-read-object port)))
    (verify-libname libname)
    (let ((x (fasl-read-object port)))
      (and (serialised-library? x)
	   x))))

(define* (store-full-serialised-library-to-file {binary-pathname posix.file-string-pathname?} {lib libman.library?})
  ;;Given  the FASL  pathname  of a  compiled  library to  be  serialised: store  the
  ;;CONTENTS into  it, creating a  new file or  overwriting an existing  one.  Return
  ;;unspecified values.
  ;;
  ;;BINARY-PATHNAME must be a string representing  the pathname of the binary library
  ;;FASL file.
  ;;
  ;;LIB must be a LIBRARY object representing the library to be serialised.
  ;;
  (receive (dir name)
      (posix.split-pathname-root-and-tail binary-pathname)
    (unless (string-empty? dir)
      ;;Create the destination directory if it does not already exists.
      (posix.mkdir/parents dir #o755)))
  ;;Serialise the library
  (let ((port (open-file-output-port binary-pathname (file-options no-fail))))
    (unwind-protect
	(store-full-serialised-library-to-port port lib)
      (close-output-port port)))
  (print-library-verbose-message "library serialisation done"))

(define* (store-full-serialised-library-to-port {port binary-output-port?} {lib libman.library?})
  ;;Given a binary output port: store the  contents of a serialised full library into
  ;;it.  Return unspecified values.
  ;;
  ;;LIB must be a LIBRARY object representing the library to be serialised.
  ;;
  (fasl-write-header port)
  ;;Write  the name  first, so  that we  can read  it back  and validate  the library
  ;;without reading the whole file.
  (fasl-write-object (libman.library-name lib)                        port)
  (fasl-write-object (%library-object->serialised-library-object lib) port (libman.library-foreign-library* lib)))

;;; --------------------------------------------------------------------

(define-struct serialised-library
  (uid
		;A gensym  uniquely identifying this serialised  library.  This field
		;is equal to the one of "library" objects.
   name
		;A library name as  defined by R6RS.  This field is  equal to the one
		;of "library" objects.
   import-libdesc*
		;A list of library descriptors as selected by the IMPORT syntax.
   visit-libdesc*
		;A list of  library descriptors representing the  libraries needed by
		;the visit code.
   invoke-libdesc*
		;A list of  library descriptors representing the  libraries needed by
		;the invoke code.
   export-subst
		;An alist public-name/label representing  the syntactic bindings from
		;the GLOBAL-ENV  that this library  exports.  This field is  equal to
		;the one of "library" objects.
   global-env
		;An  alist  label/descriptor  representing  the  top-level  synatctic
		;bindings defined by this library.  This field is equal to the one of
		;"library" objects.
   visit-proc
		;A thunk to call to evaluate the visit code.
   invoke-proc
		;A thunk to call to evaluate the invoke code.
   guard-proc
		;A thunk  to call  to evaluate  the guard code.   The thunk  runs the
		;STALE-WHEN composite test expression.
   guard-libdesc*
		;A list of  library descriptors representing the  libraries needed by
		;the guard code.
   visible?
		;A boolean determining if the  library is visible.  This attribute is
		;used  by  INTERNED-LIBRARIES  to   select  libraries  to  report  as
		;interned.  This field is equal to the one of "library" objects.
   source-file-name
		;False or a  string representing the pathname of the  file from which
		;the source code of the library was read.
   option*
		;A sexp holding  library options.  This field is equal  to the one of
		;"library" objects.
   foreign-library*
		;A list of strings representing  identifiers of shared libraries that
		;must be  loaded before  this library is  invoked.  For  example: for
		;"libvicare-curl.so", the  string identifier is  "vicare-curl".  This
		;field is equal to the one of "library" objects.
   ))

(define (%library-object->serialised-library-object lib)
  (make-serialised-library
   (libman.library-uid lib)
   (libman.library-name lib)
   (map libman.library-descriptor (libman.library-imp-lib* lib)) ;import-libdesc*
   (map libman.library-descriptor (libman.library-vis-lib* lib)) ;visit-libdesc*
   (map libman.library-descriptor (libman.library-inv-lib* lib)) ;invoke-libdesc*
   (libman.library-export-subst lib)
   (libman.library-global-env   lib)
   (compiler.compile-core-expr-to-thunk (libman.library-visit-code  lib)) ;visit-proc
   (compiler.compile-core-expr-to-thunk (libman.library-invoke-code lib)) ;invoke-proc
   (compiler.compile-core-expr-to-thunk (libman.library-guard-code  lib)) ;guard-proc
   (map libman.library-descriptor (libman.library-guard-lib* lib)) ;guard-libdesc*
   (libman.library-visible? lib)
   (libman.library-source-file-name lib)
   (libman.library-option* lib)
   (libman.library-foreign-library* lib)))

(define (intern-binary-library-and-its-dependencies slib)
  ;;Intern  the  "serialised-library" object  SLIB,  which  must represent  a  binary
  ;;library read  from a FASL file;  also intern all its  dependency libraries.  When
  ;;successful: return a "library" object representing the interned library.
  ;;
  ;;Dependency libraries are interned with FIND-LIBRARY-BY-NAME, which does the right
  ;;thing if the libraries are already interned.
  ;;
  (define (%library-descriptor->library-object libdesc)
    (libman.find-library-in-collection-by-name (libman.library-descriptor-name libdesc)))
  (define (%library-version-mismatch-warning name depname filename)
    (print-expander-warning-message "library ~s has an inconsistent dependency \
                                     on library ~s; file ~s will be recompiled from source."
				    name depname filename))
  (define (%library-stale-warning name filename)
    (print-expander-warning-message "library ~s is stale; file ~s will be recompiled from source."
				    name filename))
  (define guard-libdesc*
    (serialised-library-guard-libdesc* slib))
  (let loop ((libdesc* (append (serialised-library-import-libdesc* slib)
			       (serialised-library-visit-libdesc*  slib)
			       (serialised-library-invoke-libdesc* slib)
			       guard-libdesc*)))
    (cond ((pair? libdesc*)
	   ;;For every  library descriptor  in the list  of dependencies:  search the
	   ;;library, load it if needed and intern it.
	   (let* ((deplib-descr    (car libdesc*))
		  (deplib-libname  (libman.library-descriptor-name deplib-descr))
		  (deplib-lib      (libman.find-library-by-name    deplib-libname)))
	     (if (and (libman.library? deplib-lib)
		      (eq? (libman.library-descriptor-uid deplib-descr)
			   (libman.library-uid            deplib-lib)))
		 ;;Dependency  library successfully  interned.  Go  on with  the next
		 ;;one.
		 (loop (cdr libdesc*))
	       ;;Failed to  intern dependency  library: print a  message to  warn the
	       ;;user, then return false.
	       (begin
		 (%library-version-mismatch-warning (serialised-library-name slib)
						    deplib-libname
						    (serialised-library-source-file-name slib))
		 #f))))
	  (else
	   ;;Invoke  all  the  guard  libraries  so we  can  evaluate  the  composite
	   ;;STALE-WHEN test expression.
	   (for-each (lambda (guard-libdesc)
		       (libman.invoke-library (libman.find-library-by-name (libman.library-descriptor-name guard-libdesc))))
	     guard-libdesc*)
	   ;;Evaluate   the   composite   STALE-WHEN  test   expression   and   react
	   ;;appropriately.
	   (if ((serialised-library-guard-proc slib))
	       ;;The compiled library is stale: print a message to warn the user then
	       ;;return false.
	       (begin
		 (%library-stale-warning (serialised-library-name slib) (serialised-library-source-file-name slib))
		 #f)
	     ;;The compiled library is fine: intern it and return it.
	     (libman.intern-library
	      (libman.make-library
	       (serialised-library-uid slib)
	       (serialised-library-name slib)
	       (map %library-descriptor->library-object (serialised-library-import-libdesc* slib))
	       (map %library-descriptor->library-object (serialised-library-visit-libdesc*  slib))
	       (map %library-descriptor->library-object (serialised-library-invoke-libdesc* slib))
	       (serialised-library-export-subst slib)
	       (serialised-library-global-env   slib)
	       (serialised-library-visit-proc   slib)
	       (serialised-library-invoke-proc  slib)
	       #f		  ;visit-code
	       #f		  ;invoke-code
	       (quote (quote #f)) ;guard-code
	       '()		  ;guard-lib*
	       (serialised-library-visible? slib)
	       #f ;source-file-name
	       (serialised-library-option* slib)
	       (serialised-library-foreign-library* slib))))))))


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
  (print-library-verbose-message "~a: loading R6RS script: ~a" __who__ file-pathname)
  (let ((compiler-thunk (parametrise ((libman.source-code-location file-pathname))
			  ;;Expand  the  top  level program;  intern  the  depencency
			  ;;libraries.
			  (expand-top-level-make-compiler (reader.read-script-from-file file-pathname)))))
    ;;Traverse the current collection of libraries  and serialise the contents of all
    ;;the LIBRARY objects that were loaded from source; to "serialise" means to write
    ;;the compiled contents in a FASL file.  Return unspecified values.
    (when serialise?
      (for-each (lambda (lib)
		  (define binary-pathname
		    (cond ((compiled-libraries-build-directory)
			   (library-name->library-binary-pathname-in-build-directory (libman.library-name lib)))
			  (else
			   (error __who__
			     "cannot determine a destination directory for compiled library files"))))
		  (store-full-serialised-library-to-file binary-pathname lib))
	((libman.current-library-collection))))
    (when run?
      (print-library-verbose-message "~a: running R6RS script: ~a" __who__ file-pathname)
      (receive (lib-descr* run-thunk option* foreign-library*)
	  ;;Invoke the dependency libraries and compile the top level program.
	  (compiler-thunk)
	;;Run the top level program.
	(run-thunk)))))

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
   (print-library-verbose-message "~a: loading script: ~a" __who__ file-pathname)
   (let next-form ((ls (reader.read-script-from-file file-pathname)))
     (unless (null? ls)
       (eval-proc (car ls))
       (next-form (cdr ls))))))


;;;; compiling libraries

(module (compile-source-library)

  (define* (compile-source-library {source-pathname posix.file-string-pathname?}
				   {binary-pathname %false-or-file-string-pathname?})
    ;;Load the  first LIBRARY form from  the given file pathname;  expand it, compile
    ;;it, serialise  it.  When  successful: return  a "library"  object; if  an error
    ;;occurs: raise an exception.
    ;;
    ;;The source library  is expanded with the procedure currently  referenced by the
    ;;parameter  CURRENT-LIBRARY-EXPANDER.   If  the BINARY-PATHNAME  is  given:  the
    ;;binary library  is serialised  with the procedure  currently referenced  by the
    ;;parameter  CURRENT-LIBRARY-SERIALISER;  otherwise  it is  serialised  with  the
    ;;procedure       currently        referenced       by        the       parameter
    ;;CURRENT-LIBRARY-SERIALISER-IN-BUILD-DIRECTORY.
    ;;
    (let ((libsexp (%read-first-library-form-from-source-library-file __who__ source-pathname)))
      (receive-and-return (lib)
	  ((libman.current-library-expander) libsexp source-pathname (lambda (libname) (void)))
	(if binary-pathname
	    ((current-library-serialiser) lib binary-pathname)
	  ((current-library-serialiser-in-build-directory) lib)))))

  (define (%read-first-library-form-from-source-library-file who source-pathname)
    (module (%open-source-library)
      (import LIBRARY-LOCATOR-UTILS))
    (print-library-verbose-message "~a: loading library: ~a" who source-pathname)
    (let ((port (%open-source-library source-pathname)))
      (unwind-protect
	  (reader.read-library-from-port port)
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
     thunk
		;A  closure  object  representing the  program.   To  be
		;evaluated after having invoked the required libraries.
     option*
		;A sexp holding library options.
     foreign-library*
		;A list of strings representing  identifiers of shared libraries that
		;must be loaded before this program is run.
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
    (receive (lib-descr* run-thunk option* foreign-library*)
	;;Expand the program; invoke the dependency libraries; compile the program.
	((expand-top-level-make-compiler (reader.read-script-from-file source-filename)))
      ;;The RUN-THUNK is a closure object: if we call it, we run the program.
      (store-serialised-program binary-filename source-filename
				lib-descr* run-thunk option* foreign-library*)))

  (define* (run-compiled-program {binary-filename posix.file-string-pathname?})
    (receive (prog)
	(load-serialised-program binary-filename)
      (for-each (lambda (descr)
		  (cond ((libman.find-library-by-descriptor descr)
			 => (lambda (lib)
			      (libman.invoke-library lib)))
			(else
			 (error __who__
			   "unable to load library required by program" descr))))
	(serialised-program-lib-descr* prog))
      ;;Notice that the host's shared objects associated to this program have already
      ;;been loaded by the FASL reader.  We need to do nothing here.
      ((serialised-program-thunk prog))))

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
	   (let ((obj (let ((port (open-file-input-port binary-filename)))
			(unwind-protect
			    (fasl-read port)
			  (close-input-port port)))))
	     (if (serialised-program? obj)
		 obj
	       (error __who__
		 "invalid contents in selected serialised program file"
		 binary-filename))))))

  (define* (store-serialised-program binary-filename source-filename
				     lib-descr* run-thunk option* foreign-library*)
    ;;Given  the source  name of  an  R6RS script,  the list  of library  descriptors
    ;;required for its execution, a closure object  to be called to run it: write the
    ;;serialised program FASL file.
    ;;
    (let ((binary-filename (%make-binary-filename __who__ binary-filename source-filename)))
      (print-library-verbose-message "serialising program ~a ... " binary-filename)
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
	    (fasl-write (make-serialised-program lib-descr* run-thunk option* foreign-library*)
			port foreign-library*)
	  (close-output-port port)))
      (print-library-verbose-message "done")))

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
    (fprintf (current-error-port) "vicare: searching include file: ~a\n" include-pathname))
  (let ((include-pathname ((current-include-file-locator) include-pathname synner)))
    (when verbose?
      (fprintf (current-error-port) "vicare: including file: ~a\n" include-pathname))
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
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.load after"))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put '%error-invalid-pathname		'scheme-indent-function 1)
;; End:
