;;;Copyright (c) 2013, 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


(library (psyntax library-manager)
  (export
    ;; library inspection
    library?
    library-uid			library-name
    library-imp-lib*		library-vis-lib*
    library-inv-lib*		library-export-subst
    library-export-env		library-visit-state
    library-invoke-state	library-visit-code
    library-invoke-code		library-guard-code
    library-guard-lib*		library-visible?
    library-source-file-name	library-option*
    library-descriptor
    library-name-identifiers
    imported-label->syntactic-binding

    ;; library installation
    install-library		uninstall-library
    installed-libraries

    ;; library operations
    visit-library		invoke-library
    serialize-collected-libraries
    serialize-library

    ;; installed library collection
    find-library-by-name	library-exists?
    current-library-collection
    install-binary-library-and-its-dependencies

    ;; finding and loading libraries
    current-library-locator
    current-source-library-file-locator
    current-source-library-loader
    current-binary-library-file-locator
    current-binary-library-loader
    failed-library-location-collector

    current-source-library-loader-by-filename

    ;; library locator options
    library-locator-options-no-raise-when-open-fails?

    ;; expander
    current-library-expander

    ;; finding and loading include files
    default-include-loader
    current-include-loader
    current-include-file-locator
    current-include-file-loader

    ;;other parameters
    source-code-location)
  (import (rnrs)
    (psyntax compat))


;;;; helpers

(define (string-pathname? obj)
  (and (string? obj)
       (not (string-empty? obj))))

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


;;;; type definitions: library record

(define-record library
  (uid
		;A gensym uniquely identifying this library.
		;
		;This gensym is registered: in the LIBRARY record in the
		;collection  of installed  libraries; in  the FASL  file
		;containing  this  library  in compiled  and  serialized
		;form;  in  the  FASL   files  containing  the  compiled
		;libraries that import this one.
		;
		;Whenever a  compiled library imports this  one, the UID
		;stored in the FASL files  is compared to this field: if
		;they  are  EQ?   the  compiled versions  are  in  sync,
		;otherwise the importing library must be recompiled.
   name
		;A library name as defined by R6RS:
		;
		;   (?identifier0 ?identifier ... . ?version)
		;
		;where the ?IDENTIFIERs are symbols and ?VERSION is null
		;or  a list  of  non-negative  fixnums representing  the
		;version numbers.
   imp-lib*
		;The  list of  LIBRARY  records selected  by the  IMPORT
		;syntax.
   vis-lib*
		;The list of LIBRARY  records selecting libraries needed
		;by the visit code.
   inv-lib*
		;The list of LIBRARY  records selecting libraries needed
		;by the invoke code.
   export-subst
		;A  subst  selecting  the  exported  bindings  from  the
		;EXPORT-ENV.
   export-env
		;The  EXPORT-ENV  representing  the  top-level  bindings
		;defined by the library body.
   visit-state
		;When set  to a procedure:  it is  the thunk to  call to
		;compile  and  evaluate the  visit  code.   When set  to
		;something else: this library has been already visited.
   invoke-state
		;When set  to a procedure:  it is  the thunk to  call to
		;compile  and evaluate  the  invoke code.   When set  to
		;something else: this library has been already invoked.
   visit-code
		;When this  structure is created from  source code: this
		;field   is   a   core  language   symbolic   expression
		;representing the visit code.
		;
		;When this  structure is created from  precompiled FASL:
		;this  field is  a thunk  to be  evaluated to  visit the
		;library.
   invoke-code
		;When this  structure is created from  source code: this
		;field   is   a   core  language   symbolic   expression
		;representing the invoke code.
		;
		;When this  structure is created from  precompiled FASL:
		;this field  is a  thunk to be  evaluated to  invoke the
		;library.
   guard-code
		;When this  structure is created from  source code: this
		;field   is   a   core  language   symbolic   expression
		;representing the guard code.
		;
		;When this  structure is created from  precompiled FASL:
		;this  field is  a  thunk  to be  evaluated  to run  the
		;STALE-WHEN composite test expression.
   guard-lib*
		;The list of LIBRARY  records selecting libraries needed
		;by the STALE-WHEN composite test expression.
   visible?
		;A boolean determining if  the library is visible.  This
		;attribute  is  used  by INSTALLED-LIBRARIES  to  select
		;libraries to report as installed.
		;
		;A library should be marked as visible if it is meant to
		;be  imported by  client  code in  "normal" use;  unsafe
		;libraries  like (ikarus  system  ---)  should *not*  be
		;visible.
   source-file-name
		;False or a string representing the pathname of the file
		;from which the source code of the library was read.
   option*
		;A sexp holding library options.
   )
  (lambda (S port sub-printer)
    (define-syntax-rule (%display thing)
      (display thing port))
    (define-syntax-rule (%write thing)
      (write thing port))
    (%display "#<library ")
    (%display ($library-name S))
    (%display " filename=")	(%write ($library-source-file-name S))
    (%display ">")))

(define* (library-descriptor (lib library?))
  ;;Given a library  record return a pair having the  library UID as car
  ;;and the library name as cdr.
  ;;
  (cons ($library-uid   lib)
	($library-name lib)))

(define (library-descriptor? obj)
  (and (pair? obj)
       (symbol?       ($car obj))
       (library-name? ($cdr obj))))

(define-syntax-rule (library-descriptor-uid ?lib)
  (car ?lib))

(define-syntax-rule (library-descriptor-name ?lib)
  (cdr ?lib))

(define* (library-name-identifiers (lib library?))
  (library-name->identifiers ($library-name lib)))


;;;; errors and condition object types

;;To be used to signal an exeption when: a library loaded in response to
;;a library reference request has a library name that does not conform.
;;
(define-condition-type &non-conforming-library
    &error
  make-non-conforming-library-condition
  non-conforming-library-condition?
  (name		non-conforming-library-name)
  (reference	non-conforming-library-reference)
  (file		non-conforming-library-file))

(define (raise-non-conforming-library who library-name library-reference filename)
  (raise
   (condition
    (make-non-conforming-library-condition library-name library-reference filename)
    (make-who-condition who)
    (make-message-condition "loaded library name does not conform to requested library reference"))))


;;;; collection of already installed libraries
;;
;;When a library  is installed: it is added to  this collection.  When a
;;library is uninstalled: it is removed from this collection.
;;
;;When reasoning about loading libraries, remember that:
;;
;;* A R6RS library name is a perfectly valid R6RS library reference.
;;
;;* In a running process there can be only one library loaded with a
;;  specific  list of library  name identifiers; it is  forbidden to
;;  load two libraries having the same library name identifiers.
;;
(define current-library-collection
  ;;Hold a collection of installed LIBRARY structs.  A "collection" is a
  ;;lambda closed upon a list.  Interface:
  ;;
  ;;* When called with no arguments: return the list.
  ;;
  ;;* When called with one argument: add  the argument to the list if it
  ;;  is not already there according to EQ?.
  ;;
  ;;* When called with two arguments:
  ;;
  ;;  - If  the second argument is true: remove  the first argument from
  ;;    the list, if present according to EQ?.
  ;;
  ;;  - If the  second argument is false: add the  first argument to the
  ;;    list, if not already there according to EQ?.
  ;;
  (make-parameter (let ((set '()))
		    (case-lambda*
		      (()
		       set)
		      (((lib library?))
		       (unless (memq lib set)
			 (%log-library-debug-message "installed library: ~a" (library-name lib))
			 (set! set (cons lib set))))
		      (((lib library?) del?)
		       (if del?
			   (set! set (remq lib set))
			 (unless (memq lib set)
			   (%log-library-debug-message "installed library: ~a" (library-name lib))
			   (set! set (cons lib set)))))))
    (lambda* ((obj procedure?))
      obj)))

(define (library-exists? libref)
  ;;Given  a R6RS  library  reference search  the corresponding  LIBRARY
  ;;record  in  the  collection   of  already  installed  libraries:  if
  ;;successful return true, otherwise return false.
  ;;
  (and (find-library-in-collection-by-reference libref)
       #t))

(define (find-library-in-collection-by-predicate pred)
  ;;Visit  the current  installed  libraries collection  and return  the
  ;;first for  which PRED returns true.   If PRED returns false  for all
  ;;the entries in the collection: return false.
  ;;
  (let next-library-struct ((ls ((current-library-collection))))
    (cond ((null? ls)
	   #f)
	  ((pred ($car ls))
	   ($car ls))
	  (else
	   (next-library-struct ($cdr ls))))))

(define* (find-library-in-collection-by-descriptor libdesc)
  ;;Given   a  library   descriptor,  as   generated  by   the  function
  ;;LIBRARY-DESCRIPTOR: return the corresponding LIBRARY record from the
  ;;collection of installed libraries or raise an assertion.
  ;;
  (let ((uid (library-descriptor-uid libdesc)))
    (or (find-library-in-collection-by-predicate
	 (lambda (lib)
	   (eq? uid ($library-uid lib))))
	(assertion-violation __who__
	  "cannot find installed library with required descriptor" libdesc))))

(module (find-library-by-name
	 find-library-in-collection-by-reference
	 %external-pending-libraries)

  (define (find-library-by-name libref)
    ;;Given  a R6RS  library reference:  try to  search and  install the
    ;;corresponding  library,  if  it  is not  already  installed;  when
    ;;successful  return  the  corresponding LIBRARY  record,  otherwise
    ;;raise an exception.
    ;;
    ;;Search  for the  library in  the  internal collection  or, if  not
    ;;found, in the external source  (for example the file system) using
    ;;the current CURRENT-LIBRARY-LOADER.
    ;;
    (or (find-library-in-collection-by-reference libref)
	(%find-and-install-external-library libref)))

  (define (find-library-in-collection-by-reference libref)
    (find-library-in-collection-by-predicate (lambda (lib)
					       (%conforming-identifiers? libref lib))))

  (define (%conforming-identifiers? libref lib)
    (equal? (library-reference->identifiers libref)
	    (library-name-identifiers lib)))

  (define* (%find-and-install-external-library libref)
    ;;Given a  R6RS library reference  try to load a  conforming library
    ;;using the current library loader.
    ;;
    (with-pending-library-request (__who__ libref)
      ;;Load the library, either source or precompiled, and install it.
      ((current-library-loader) libref)
      ;;Check if we actually succeeded.
      (or (find-library-in-collection-by-reference libref)
	  (assertion-violation __who__
	    "cannot find library conforming to requested library reference"
	    libref))))

  (module (with-pending-library-request %external-pending-libraries)

    (define %external-pending-libraries
      ;;Hold  a   list  of   items  representing  the   libraries  whose
      ;;installation is  currently pending.   Each item  is the  list of
      ;;R6RS  library   name  identifiers.   Used  to   detect  circular
      ;;dependencies between libraries.
      ;;
      (make-parameter '()))

    (define (%pending-library-request? libref)
      (member (library-reference->identifiers libref)
	      (%external-pending-libraries)))

    (define (%assert-not-pending-library-request who libref)
      (when (%pending-library-request? libref)
	(assertion-violation who
	  "circular attempt to import library was detected" libref)))

    (define-syntax (with-pending-library-request stx)
      (syntax-case stx ()
	((_ (?who ?libref) ?body0 ?body ...)
	 (identifier? #'?who)
	 #'(let ((libref ?libref))
	     (%assert-not-pending-library-request ?who libref)
	     (parametrise ((%external-pending-libraries (cons libref (%external-pending-libraries))))
	       ?body0 ?body ...)))
	))

    #| end of module |# )

  #| end of module |# )


;;;; parameters

(define current-library-expander
  ;;The current  library expander is  used to expand LIBRARY  forms from
  ;;source files.
  ;;
  (make-parameter
      (lambda (library-sexp)
        (assertion-violation 'current-library-expander "not initialized"))
    (lambda* ((obj procedure?))
      obj)))

(define source-code-location
  ;;This parameter is  used to expand the  identifier syntax "__file__".
  ;;It is  meant to be  set to a string  representing the source  of the
  ;;code being expanded; for example the source file name.
  ;;
  (make-parameter "<unknown-source-location>"
    (lambda* ((obj string?))
      obj)))


;;;; loading source and binary libraries

;;NOTE Here we would like to use a proper enumeration definition and the
;;predicate function below  to test for library locator  options; but we
;;cannot.  First because using  DEFINE-ENUMERATION causes the boot image
;;to crash  at initialisation time (for  no fucking reason I  can figure
;;out);    second    because    we     cannot    export    the    syntax
;;LIBRARY-LOCATOR-OPTIONS from a library component of the boot image, we
;;could do  it by defining  the syntax  in an external  library.  (Marco
;;Maggi; Tue Feb 18, 2014)
;;
;; (define-enumeration library-locator-option
;;   (move-on-when-open-fails
;; 		;If attempting  to open  a file fails:  do not  raise an
;; 		;exception, rather move on with the search.
;;    )
;;   library-locator-options)
;;
;; (define (library-locator-options-no-raise-when-open-fails? options)
;;   (enum-set-member? 'move-on-when-open-fails options))
;;
(define-syntax (library-locator-options stx)
  (syntax-case stx ()
    ((_ ?sym ...)
     (and (all-identifiers? #'(?sym ...))
	  (let ((syms (syntax->datum #'(?sym ...))))
	    (for-all (lambda (sym)
		       (memq sym '(move-on-when-open-fails)))
	      syms)))
     #'(list (quote ?sym) ...))))

(define (library-locator-options-no-raise-when-open-fails? options)
  (memq 'move-on-when-open-fails options))

;;; --------------------------------------------------------------------

(define current-library-locator
  ;;Hold  a function  used to  locate a  library from  its R6RS  library
  ;;reference; this parameter  is initialised to false here  and must be
  ;;by "ikarus.main.sls" with one of the functions:
  ;;
  ;;   run-time-library-locator
  ;;   compile-time-library-locator
  ;;
  ;;or a custom function selected by the user.
  ;;
  ;;The selected locator function must  accept as  arguments:
  ;;
  ;;1. A R6RS library reference.
  ;;
  ;;2. A list of symbols representing options.
  ;;
  ;;and  it must  return a  thunk as  single value.   When invoked,  the
  ;;returned thunk must return two values:
  ;;
  ;;1. An input port from which the  library can be read; if the port is
  ;;   binary: a  compiled library can be  read from it; if  the port is
  ;;    textual  a   source  library  can  be  read  from   it.   It  is
  ;;   responsibility of  the caller to close the returned  port when no
  ;;   more needed.
  ;;
  ;;2. A  thunk to be  called to continue the  search; it must  have the
  ;;   same  API of the  thunk returned  by the locator  function.  This
  ;;   thunk allows the  caller to reject a library if  it does not meet
  ;;   some  additional constraint; for  example: if its  version number
  ;;   does not conform to LIBREF.
  ;;
  ;;When no matching library is found: return false and false.
  ;;
  (make-parameter
      #f
    (lambda* ((obj procedure?))
      obj)))

(define failed-library-location-collector
  ;;Hold  a function  that is  used to  register queried  locations that
  ;;failed  to provide  a  requested library.   Such  locations must  be
  ;;represented by a printable object, for example a string.
  ;;
  ;;The collector  function must  accept 1 or  0 arguments:  when called
  ;;with one argument  it must register it as  location descriptor; when
  ;;called with zero arguments it  must return the registered collection
  ;;or locations as list of  objects.  The collector function can return
  ;;unspecified values.
  ;;
  ;;For example, set the collector function can be set to:
  ;;
  ;;   (let ((ell '()))
  ;;     (case-lambda
  ;;      (()
  ;;       ell)
  ;;      ((location)
  ;;       (set! ell (cons location ell)))))
  ;;
  ;;and used to  collect library file names that where  tried but do not
  ;;exist or failed to be opened.
  ;;
  (make-parameter
      (case-lambda
       (()
	'())
       ((origin)
	(void)))
    (lambda* ((obj procedure?))
      obj)))

;;; --------------------------------------------------------------------

(define current-source-library-file-locator
  ;;Hold a  function used to convert  a R6RS library reference  into the
  ;;corresponding source file pathname.
  ;;
  ;;The referenced function must accept, as single value, a R6RS library
  ;;reference and it must return  two values.  When successful: a string
  ;;representing  the source  file pathname;  a  thunk to  be called  to
  ;;continue  the search  from the  next directory  in the  search path.
  ;;Otherwise return: false and false.
  ;;
  (make-parameter
      (lambda (libref pending-libraries)
	(error 'current-source-library-file-locator
	  "source library locator not set"))
    (lambda* ((obj procedure?))
      obj)))

(define current-source-library-loader
  ;;Hold a function used to laod a library from a source file.
  ;;
  ;;The  referenced function  must:  accept a  string  file pathname  as
  ;;single  argument,  open the  pathname  for  input using  the  native
  ;;transcoder, read the first datum, close the port, return the datum.
  ;;
  (make-parameter
      (lambda (filename)
	(error 'current-source-library-loader
	  "source library loader not set"))
    (lambda* ((obj procedure?))
      obj)))

;;; --------------------------------------------------------------------

(define current-binary-library-file-locator
  ;;Hold a  function used to convert  a R6RS library reference  into the
  ;;corresponding FASL file pathname.
  ;;
  ;;The referenced function must accept, as single value, a R6RS library
  ;;reference and it must return  two values.  When successful: a string
  ;;representing  the  FASL file  pathname;  a  thunk  to be  called  to
  ;;continue  the search  from the  next directory  in the  search path.
  ;;Otherwise return: false and false.
  ;;
  (make-parameter
      (lambda (libref pending-libraries)
	(error 'current-binary-library-file-locator
	  "serialized library locator not set"))
    (lambda* ((obj procedure?))
      obj)))

(define current-binary-library-loader
  ;;Hold a function used to load a precompiled library.
  ;;
  ;;The  referenced   function  must   accept  2  arguments:   a  string
  ;;representing  the  pathname of  the  file  from which  a  serialized
  ;;library can be  read; a continuation function to  be called whenever
  ;;loading the precompiled file succeeds.
  ;;
  ;;The success continuation must return true if loading the precompiled
  ;;library succeeds, otherwise it must  return false.  For more details
  ;;on       the      success       continuation      function       see
  ;;%INSTALL-PRECOMPILED-LIBRARY-AND-ITS-DEPENCENCIES.
  ;;
  (make-parameter
      (lambda (pathname success-kont)
	#f)
    (lambda* ((obj procedure?))
      obj)))


;;;; loading libraries from files: requesting by library name
;;
;;The  library  loader  is  a  function that  loads  a  library,  either
;;precompiled or from source, and installs it.
;;
(module (current-library-loader)

  (module (default-library-loader)
    (define-constant __who__ 'default-library-loader)

    (define* (default-library-loader libref)
      ;;Default value for the parameter CURRENT-LIBRARY-LOADER.  Given a
      ;;R6RS library  reference: attempt  to locate  the library  in the
      ;;current repository and load it;  try first to load a precompiled
      ;;library, if any,  then try to load a source  library; the loaded
      ;;library is  installed along  with all its  dependency libraries.
      ;;Return unspecified values.
      ;;
      ;;LIBREF must be a library reference as defined by R6RS:
      ;;
      ;;   (?identifier0 ?identifier ... . ?version-reference)
      ;;
      (%log-library-debug-message "~a: searching: ~a" __who__ libref)
      (parametrise
	  ((failed-library-location-collector (let ((ell '()))
						(case-lambda
						 (()
						  ell)
						 ((location)
						  (set! ell (cons location ell)))))))
	(let loop ((next-locator-search ((current-library-locator)
					 libref
					 (library-locator-options move-on-when-open-fails))))
	  (receive (port further-locator-search)
	      (next-locator-search)
	    (%log-library-debug-message "~a: reading from: ~a" __who__ port)
	    (cond ((binary-port? port)
		   ;;A binary location was found.   We can read the binary
		   ;;library from PORT.
		   (%print-loading-library port)
		   (let ((rv (unwind-protect
				 ((current-binary-library-loader) libref port)
			       (close-input-port port))))
		     (if rv
			 ;;Success.  The library  and all its dependencies
			 ;;have been successfully loaded and installed.
			 (begin
			   (%print-loaded-library port)
			   #t)
		       ;;Failure.   The library  read from  port has  been
		       ;;rejected; try to go on with the search.
		       (begin
			 (%print-rejected-library port)
			 (loop further-locator-search)))))

		  ((textual-port? port)
		   ;;A source location was found.   We can read the source
		   ;;library  from  PORT;  we  assume  that  applying  the
		   ;;function PORT-ID to PROT  will return the source file
		   ;;name.
		   (%print-loading-library port)
		   (let ((rv (unwind-protect
				 ((current-source-library-loader) libref port)
			       (close-input-port port))))
		     (if rv
			 ;;Success.  The library  and all its dependencies
			 ;;have been successfully loaded and installed.
			 (begin
			   (%print-loaded-library port)
			   #t)
		       ;;Failure.   The library  read from  port has  been
		       ;;rejected; try to go on with the search.
		       (begin
			 (%print-rejected-library port)
			 (loop further-locator-search)))))

		  ((not port)
		   ;;No suitable library was found.
		   (%file-locator-resolution-error libref
						   (reverse ((failed-library-location-collector)))
						   (cdr (%external-pending-libraries))))

		  (else
		   (assertion-violation __who__
		     "internal error: invalid return values from library locator"
		     port further-locator-search)))))))

    ;;Keep the library names aligned!!!                                     VV
    (define (%print-loading-library port)   (%log-loaded-library "loading:  ~a ..." (port-id port)))
    (define (%print-loaded-library port)    (%log-loaded-library "loaded:   ~a" (port-id port)))
    (define (%print-rejected-library port)  (%log-loaded-library "rejected: ~a" (port-id port)))

    (define (%log-loaded-library template . args)
      (when (options.print-loaded-libraries)
	(guard (E (else
		   ;;We do not  want an exception from the  I/O layer to
		   ;;ruin things.
		   (void)))
	  (apply fprintf (current-error-port)
		 (string-append "vicare: " template "\n")
		 args))))

    #| end of module: DEFAULT-LIBRARY-LOADER |# )

;;; --------------------------------------------------------------------

  (define (%file-locator-resolution-error libref failed-list pending-libraries)
    (raise
     (apply condition (make-error)
  	    (make-who-condition 'expander)
  	    (make-message-condition "cannot locate library in library-path")
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

;;; --------------------------------------------------------------------

  (define current-library-loader
    ;;Hold a function used to load a library, either precompiled or from
    ;;source, given a library name.
    ;;
    (make-parameter
	default-library-loader
      (lambda* ((obj procedure?))
	obj)))

  #| end of module: CURRENT-LIBRARY-LOADER |# )


;;;; loading libraries from files: requesting by library file pathname

(module (current-source-library-loader-by-filename)

  (define* (default-library-source-loader-by-filename (source-pathname string-pathname?) (libname-predicate procedure?))
    ;;Default          value          for         the          parameter
    ;;CURRENT-SOURCE-LIBRARY-LOADER-BY-FILENAME.   Given a  library file
    ;;pathname: load  the file, expand  the first LIBRARY  form, compile
    ;;the result, install the  library, return the corresponding LIBRARY
    ;;record.
    ;;
    ;;LIBNAME-PREDICATE must  be a  predicate function  to apply  to the
    ;;R6RS library name of the loaded library.
    ;;
    (%log-library-debug-message "~a: searching: ~a" __who__ source-pathname)
    (receive (uid libname
		  import-desc* visit-desc* invoke-desc*
		  invoke-code visit-code
		  export-subst export-env
		  guard-code guard-desc*
		  option*)
	(parametrise ((source-code-location source-pathname))
	  ((current-library-expander)
	   ((current-source-library-loader) source-pathname)
	   source-pathname libname-predicate))
      (find-library-by-name libname)))

  (define current-source-library-loader-by-filename
    ;;Hold  a function  used to  load a  source library  given the  file
    ;;pathname.
    ;;
    (make-parameter
	default-library-source-loader-by-filename
      (lambda* ((obj procedure?))
	obj)))

  #| end of module |# )


;;;; serializing precompiled libraries

(define (serialize-collected-libraries serialize compile)
  ;;Traverse  the  current collection  of  libraries  and serialize  the
  ;;contents  of all  the  LIBRARY  records having  a  source file  (the
  ;;records that do not have a source file represent the libraries built
  ;;in the boot image).
  ;;
  ;;"Serializing"  means to  write the  precompiled contents  in a  FASL
  ;;file.
  ;;
  ;;SERIALIZE must be a closure wrapping STORE-SERIALIZE-LIBRARY.
  ;;
  ;;COMPILE must be a closure wrapping the function COMPILE-CORE-EXPR.
  ;;
  (for-each (lambda (lib)
	      (serialize-library lib serialize compile))
    ((current-library-collection))))

(define* (serialize-library (lib library?) (serialize procedure?) (compile procedure?))
  ;;Compile and serialize the given library record.
  ;;
  ;;NOTE We  do *not* write the  source file pathname to  the FASL file.
  ;;When, later, the FASL file will  be loaded and the library installed
  ;;in the collection: the absence of  the source pathname will mark the
  ;;LIBRARY record as coming from a FASL file rather than a source file;
  ;;only  records coming  from a  source files  are serialized  when the
  ;;--compile-dependencies option  is used.   (Marco Maggi; Sat  Feb 22,
  ;;2014)
  ;;
  (define source-pathname ($library-source-file-name lib))
  ;;We serialize  only libraries having  a source file in  their LIBRARY
  ;;record.
  (when source-pathname
    (%log-library-debug-message "~a: serializing: ~a" __who__ ($library-name lib))
    (serialize source-pathname ($library-name lib)
	       (list ($library-uid lib)
		     ($library-name lib)
		     (map library-descriptor ($library-imp-lib* lib))
		     (map library-descriptor ($library-vis-lib* lib))
		     (map library-descriptor ($library-inv-lib* lib))
		     ($library-export-subst lib)
		     ($library-export-env lib)
		     (compile ($library-visit-code lib))
		     (compile ($library-invoke-code lib))
		     (compile ($library-guard-code lib))
		     (map library-descriptor ($library-guard-lib* lib))
		     ($library-visible? lib)
		     ($library-option* lib)
		     source-pathname))))

(define (install-binary-library-and-its-dependencies
	 uid libname
	 import-libdesc* visit-libdesc* invoke-libdesc*
	 export-subst export-env
	 visit-proc invoke-proc guard-proc
	 guard-libdesc* visible? library-option* source-filename)
  ;;Used  as  success  continuation  function by  the  function  in  the
  ;;parameter  CURRENT-BINARY-LIBRARY-LOADER.  All  the arguments  after
  ;;SOURCE-FILENAME are the CONTENTS of the serialized library.
  ;;
  ;;Make  sure all  dependencies  are met,  by  loading the  appropriate
  ;;libraries, then install the library represented by the arguments and
  ;;return a  sexp representing the  R6RS library name of  the installed
  ;;library; otherwise return #f.
  ;;
  (let loop ((libdesc* (append import-libdesc* visit-libdesc* invoke-libdesc* guard-libdesc*)))
    (if (null? libdesc*)
	(begin
	  ;;Invoke  all  the guard  libraries  so  we can  evaluate  the
	  ;;composite STALE-WHEN test expression.
	  (for-each (lambda (guard-libdesc)
		      (invoke-library
		       (find-library-by-name (library-descriptor-name guard-libdesc))))
	    guard-libdesc*)
	  ;;Evaluate the composite STALE-WHEN  test expression and react
	  ;;appropriately.
	  (if (guard-proc)
	      ;;The  precompiled library  is stale:  print a  message to
	      ;;warn the user then return false.
	      (begin
		(library-stale-warning libname source-filename)
		#f)
	    ;;The  precompiled library  is fine:  install it  and return
	    ;;true.
	    (let ((visit-code        #f)
		  (invoke-code       #f)
		  (guard-code        (quote (quote #f)))
		  (guard-libdesc*    '())
		  (source-file-name  #f))
	      (install-library uid libname
			       import-libdesc* visit-libdesc* invoke-libdesc*
			       export-subst export-env
			       visit-proc invoke-proc
			       visit-code invoke-code
			       guard-code guard-libdesc*
			       visible? source-file-name library-option*)
	      libname)))
      (begin
	;;For  every library  descriptor  in the  list of  dependencies:
	;;search the library, load it if needed and install it.
	;;
	(let* ((deplib-descr    (car libdesc*))
	       (deplib-libname  (library-descriptor-name deplib-descr))
	       (deplib-lib      (find-library-by-name deplib-libname)))
	  (if (and (library? deplib-lib)
		   (eq? (library-descriptor-uid deplib-descr)
			(library-uid            deplib-lib)))
	      (loop (cdr libdesc*))
	    (begin
	      ;;Print a message to warn the user.
	      (library-version-mismatch-warning libname deplib-libname source-filename)
	      #f)))))))


;;;; installing libraries

(case-define installed-libraries
  ;;Return a list  of LIBRARY structs being already  installed.  If ALL?
  ;;is true:  return all the  installed libraries, else return  only the
  ;;visible ones.
  ;;
  (()
   (installed-libraries #f))
  ((all?)
   (let next-library-struct ((ls ((current-library-collection))))
     (cond ((null? ls)
	    '())
	   ((or all? ($library-visible? ($car ls)))
	    (cons ($car ls) (next-library-struct ($cdr ls))))
	   (else
	    (next-library-struct ($cdr ls)))))))

(module (install-library)
  ;;INSTALL-LIBRARY  builds a  LIBRARY  record and  installs  it in  the
  ;;internal collection  of libraries;  return unspecified  values.  The
  ;;arguments are:
  ;;
  ;;UID -
  ;;   A gensym uniquely identifying this library.
  ;;
  ;;NAME -
  ;;   A R6RS library name.
  ;;
  ;;IMPORT-LIBDESC* -
  ;;   A list of library descriptors enumerating the libraries specified
  ;;   in the IMPORT clauses.
  ;;
  ;;VISIT-LIBDESC* -
  ;;   A list of library descriptors enumerating the libraries needed by
  ;;   the visit code.
  ;;
  ;;INVOKE-LIBDESC* -
  ;;   A list of library  descriptors enmerating the libraries needed by
  ;;   the invoke code.
  ;;
  ;;EXPORT-SUBST -
  ;;   A subst selecting the bindings to export from the EXPORT-ENV.
  ;;
  ;;EXPORT-ENV -
  ;;   The list of top-level bindings defined by the library body.  Some
  ;;   of them are to be exported, others are not.
  ;;
  ;;VISIT-PROC -
  ;;   A thunk to evaluate to visit the library.
  ;;
  ;;INVOKE-PROC -
  ;;   A thunk to evaluate to invoke the library.
  ;;
  ;;VISIT-CODE -
  ;;   When this  argument is created from source code:  this field is a
  ;;   core  language symbolic  expression representing the  visit code.
  ;;   When this  argument is created from precompiled  FASL: this field
  ;;   is a thunk to be evaluated to visit the library.
  ;;
  ;;INVOKE-CODE -
  ;;   When this  argument is created from source code:  this field is a
  ;;   core  language symbolic expression representing  the invoke code.
  ;;   When this  argument is created from precompiled  FASL: this field
  ;;   is a thunk to be evaluated to invoke the library.
  ;;
  ;;GUARD-CODE -
  ;;   When this  argument is created from source code:  this field is a
  ;;   core  language symbolic  expression representing the  guard code.
  ;;   When this  argument is created from precompiled  FASL: this field
  ;;   is a  thunk to be evaluated to run  the STALE-WHEN composite test
  ;;   expression.
  ;;
  ;;GUARD-LIBDESC* -
  ;;   A list of library  descriptors enmerating the libraries needed by
  ;;   the composite STALE-WHEN test expression.
  ;;
  ;;VISIBLE? -
  ;;   A  boolean, true  if this library  is to be  made visible  to the
  ;;   function INSTALLED-LIBRARIES.
  ;;
  ;;SOURCE-FILE-NAME -
  ;;   False or a string representing the source file name.
  ;;
  ;;LIBRARY-OPTION* -
  ;;   A list of sexps representing library options.
  ;;
  (define-constant __who__ 'install-library)
  (case-define* install-library
    (((uid symbol?) (libname library-name?)
      import-libdesc* visit-libdesc* invoke-libdesc*
      export-subst export-env
      visit-proc invoke-proc
      visit-code invoke-code
      guard-code guard-libdesc*
      visible? source-file-name library-option*)
     (let ((import-lib*	(map find-library-in-collection-by-descriptor import-libdesc*))
	   (visit-lib*	(map find-library-in-collection-by-descriptor visit-libdesc*))
	   (invoke-lib*	(map find-library-in-collection-by-descriptor invoke-libdesc*))
	   (guard-lib*	(map find-library-in-collection-by-descriptor guard-libdesc*)))
       (when (library-exists? libname)
	 (assertion-violation __who__ "library is already installed" libname))
       (let ((lib (make-library uid libname import-lib* visit-lib* invoke-lib*
				export-subst export-env visit-proc invoke-proc
				visit-code invoke-code guard-code guard-lib*
				visible? source-file-name library-option*)))
	 (%install-library-record lib)
	 (when (memq 'visit-upon-loading library-option*)
	   (visit-library lib))))
     (void)))

  (define (%install-library-record lib)
    (for-each
	(lambda (export-env-entry)
	  ;;See the comments in the expander  code for the format of the
	  ;;EXPORT-ENV.  Entries  in the  EXPORT-ENV are  different from
	  ;;entries  in  the LEXENV;  here  we  transform an  EXPORT-ENV
	  ;;binding into a LEXENV binding descriptor.
	  (let* ((label    (car export-env-entry))
		 (binding  (cdr export-env-entry))
		 (binding1 (case (car binding)
			     ((global)        (cons* 'global        lib (cdr binding)))
			     ((global-macro)  (cons* 'global-macro  lib (cdr binding)))
			     ((global-macro!) (cons* 'global-macro! lib (cdr binding)))
			     ((global-ctv)    (cons* 'global-ctv    lib (cdr binding)))
			     ((core-prim
			       library import export
			       define define-syntax define-alias
			       define-fluid-syntax define-fluid-override
			       let-syntax letrec-syntax begin-for-syntax
			       module begin set! stale-when
			       global mutable
			       core-macro macro macro!
			       $core-rtd $rtd $module $fluid $synonym)
			      binding)
			     (else
			      (assertion-violation __who__
				"invalid syntactic binding descriptor type in EXPORT-ENV entry"
				lib export-env-entry)))))
	    ;;When the library is serialized: the content of the label's
	    ;;"value" slot is not saved, so we have to set it here every
	    ;;time the library is loaded.
	    (set-label-binding! label binding1)))
      ;;This expression returns the EXPORT-ENV of the library LIB.
      ($library-export-env lib))
    ;;Register the record in the collection of installed libraries.
    ((current-library-collection) lib))

  #| end of module: INSTALL-LIBRARY |# )


;;;; uninstalling libraries
;;
;;Libraries  from   the  collection   of  installed  libraries   can  be
;;uninstalled,   either   to  free   system   resources   or  to   allow
;;reinstallation from new files.
;;
(case-define* uninstall-library
  ;;Uninstall a library.  Return unspecified values.
  ;;
  ;;THE IMPLEMENTATION OF THIS FUNCTION IS INCOMPLETE.
  ;;
  ((libname)
   (uninstall-library libname #t))
  (((libname library-name?) err?)
   ;;FIXME: check that no other import is in progress.  (Ghuloum)
   (cond ((find-library-in-collection-by-reference libname)
	  => (lambda (lib)
	       ;;Remove LIB from the current collection.
	       ((current-library-collection) lib #t)
	       ;;Remove label gensyms from the internal table.
	       (for-each (lambda (export-env-entry)
			   ;;We expect the entry to have the format:
			   ;;
			   ;;   (?label . (?type . ?loc))
			   ;;
			   (let ((label   (car export-env-entry))
				 (binding (cdr export-env-entry)))
			     (remove-location label)
			     (when (memq (car binding)
					 '(global global-macro global-macro! global-ctv))
			       (remove-location (cdr binding)))))
		 ($library-export-env lib))))
	 (else
	  (when err?
	    (assertion-violation __who__ "library not installed" libname))))))


;;;; utilities for the expansion process

(define (imported-label->syntactic-binding lab)
  ;;If  a label  is associated  to  a binding  from the  the boot  image
  ;;environment or to a binding from  a library's EXPORT-ENV: it has the
  ;;associated descriptor in its "value"  field; otherwise such field is
  ;;set to #f.
  ;;
  ;;So, if we  have a label, we  can check if it  references an imported
  ;;binding  simply  by  checking  its   "value"  field;  this  is  what
  ;;IMPORTED-LABEL->SYNTACTIC-BINDING does.
  ;;
  (label-binding lab))

(define* (invoke-library (lib library?))
  ;;Evaluate the invoke.
  ;;
  (let ((invoke (library-invoke-state lib)))
    (when (procedure? invoke)
      ($set-library-invoke-state! lib (lambda ()
					(assertion-violation 'invoke
					  "circularity detected" lib)))
      (for-each invoke-library ($library-inv-lib* lib))
      ($set-library-invoke-state! lib (lambda ()
					(assertion-violation 'invoke
					  "first invoke did not return" lib)))
      (invoke)
      ($set-library-invoke-state! lib #t))))

(define* (visit-library (lib library?))
  ;;Evaluate the visit code.
  ;;
  (let ((visit ($library-visit-state lib)))
    (when (procedure? visit)
      ($set-library-visit-state! lib (lambda ()
				       (assertion-violation 'visit
					 "circularity detected" lib)))
      (for-each invoke-library ($library-vis-lib* lib))
      ($set-library-visit-state! lib (lambda ()
				       (assertion-violation 'invoke
					 "first visit did not return" lib)))
      (visit)
      ($set-library-visit-state! lib #t))))


;;;; including files

(module (current-include-loader
	 default-include-loader)

  (define* (default-include-loader (filename string?) verbose? synner)
    ;;Default value for the parameter CURRENT-INCLUDE-LOADER.  Search an
    ;;include file with name FILENAME.  When successful return 2 values:
    ;;the  full pathname  from which  the  file was  loaded, a  symbolic
    ;;expresison representing the file  contents.  When an error occurs:
    ;;call the procedure SYNNER.
    ;;
    ;;If VERBOSE? is true: display verbose messages on the current error
    ;;port describing the including process.
    ;;
    (when verbose?
      (fprintf (current-error-port)
	       "Vicare: searching include file: ~a\n" filename))
    (let ((pathname ((current-include-file-locator) filename synner)))
      (when verbose?
	(fprintf (current-error-port)
		 "Vicare: including file: ~a\n" pathname))
      (values pathname ((current-include-file-loader) pathname synner))))

  (define current-include-loader
    ;;Hold a function used to load an include file.
    ;;
    (make-parameter
	default-include-loader
      (lambda* ((obj procedure?))
	obj)))

  #| end of module: CURRENT-INCLUDE-LOADER |# )

(define current-include-file-locator
  ;;Hold  a function  used  to convert  an include  file  name into  the
  ;;corresponding   file  pathname;   this   parameter  is   initialised
  ;;"ikarus.load.sls" with the function LOCATE-INCLUDE-FILE.
  ;;
  ;;The referenced function must accept  3 values: a string representing
  ;;the include  file name; a  boolean, true  if the process  of loading
  ;;must display  verbose messages on  the current error port;  a synner
  ;;function used to report errors.
  ;;
  (make-parameter
      (lambda (filename pending-libraries)
	(error 'current-include-file-locator
	  "include file locator not set" filename))
    (lambda* ((obj procedure?))
      obj)))

(define current-include-file-loader
  ;;Hold a  function used  to laod  an include  file; this  parameter is
  ;;initialised "ikarus.load.sls" with the function READ-INCLUDE-FILE.
  ;;
  ;;The referenced function must accept  3 values: a string representing
  ;;an existent file pathname; a boolean, true if the process of loading
  ;;must display  verbose messages on  the current error port;  a synner
  ;;function used to report errors.
  ;;
  (make-parameter
      (lambda (filename)
	(error 'current-include-file-loader
	  "include file loader not set" filename))
    (lambda* ((obj procedure?))
      obj)))


;;;; done

;; #!vicare
;; (define dummy
;;   (let ()
;;     (import (vicare))
;;     (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.library-manager"))))

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-pending-library-request 'scheme-indent-function 1)
;; End:
