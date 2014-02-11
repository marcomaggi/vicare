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
    library-name		library-version
    library-descriptor		library-subst
    imported-label->syntactic-binding

    ;; library installation
    install-library		uninstall-library
    installed-libraries

    ;; library operations
    visit-library		invoke-library
    serialize-collected-libraries

    ;; finding and loading libraries
    find-library-by-name	library-exists?

    current-library-expander
    current-library-collection
    current-library-source-file-locator
    current-library-source-loader
    current-library-serialized-loader

    ;; finding and loading include files
    default-include-loader
    current-include-loader
    current-include-file-locator
    current-include-file-loader

    ;; library names and version numbers
    library-name?
    library-version-numbers?		library-version-number?
    library-name-decompose
    library-name->identifiers		library-name->version
    library-name-identifiers=?		library-name=?
    library-name<?			library-name<=?
    library-version=?
    library-version<?			library-version<=?

    ;; library references and conformity
    library-reference?			library-version-reference?
    library-sub-version-reference?	library-sub-version?
    library-reference-decompose
    library-reference->identifiers
    library-reference->version-reference
    library-reference-identifiers=?
    conforming-sub-version-and-sub-version-reference?
    conforming-version-and-version-reference?
    conforming-library-name-and-library-reference?)
  (import (rnrs)
    (psyntax compat)
    (vicare arguments validation)
    (vicare language-extensions simple-match))


;;;; type definitions: library record

(define-record library
  (id
		;A gensym uniquely identifying this library.
   name
		;Non-empty list of  symbols representing the identifiers
		;from the library name.
   version
		;Null or a list of non-negative fixnums representing the
		;version number from the library name.
   imp*
		;The  list of  LIBRARY  records selected  by the  IMPORT
		;syntax.
   vis*
		;The list of LIBRARY  records selecting libraries needed
		;by the visit code.
   inv*
		;The list of LIBRARY  records selecting libraries needed
		;by the invoke code.
   subst
		;A  subst  selecting  the  exported  bindings  from  the
		;EXPORT-ENV.
   env
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
   guard-req*
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
    ;;Printer function.
    ;;
    (define-inline (%display thing)
      (display thing port))
    (define-inline (%write thing)
      (write thing port))
    (%display "#<library ")
    (%display (if (null? ($library-version S))
		  ($library-name S)
		(append ($library-name S)
			(list ($library-version S)))))
    (%display " filename=")	(%write ($library-source-file-name S))
    (%display ">")))

(define-argument-validation (library who obj)
  (library? obj)
  (assertion-violation who "expected instance of library struct as argument" obj))

(define* (library-descriptor lib)
  ;;Given a library record return a list holding: the unique library id,
  ;;the  list of  symbols from  the library  name, null  or the  list of
  ;;version numbers from the library name.
  ;;
  (with-arguments-validation (__who__)
      ((library		lib))
    (list ($library-id      lib)
	  ($library-name    lib)
	  ($library-version lib))))


;;;; expanding libraries
;;
;;The  current library  expander is  used to  expand LIBRARY  forms from
;;source files.
;;
(define current-library-expander
  (make-parameter
      (lambda (x)
        (assertion-violation 'current-library-expander "not initialized"))
    (lambda (obj)
      (define who 'current-library-expander)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))


;;;; collection of already installed libraries
;;
;;When a library  is installed: it is added to  this collection.  When a
;;library is uninstalled: it is removed from this collection.
;;
(module (current-library-collection)

  (define (make-collection)
    ;;Build  and return  a "collection":  a lambda  closed upon  a list.
    ;;Interface:
    ;;
    ;;* When called with no arguments: return the list.
    ;;
    ;;* When called  with one argument: add the argument  to the list if
    ;;  it is not already there according to EQ?.
    ;;
    ;;* When called with two arguments:
    ;;
    ;;  - If the second argument is true: remove the first argument from
    ;;    the list, if present according to EQ?.
    ;;
    ;;  - If the second argument is false: add the first argument to the
    ;;    list, if not already there according to EQ?.
    ;;
    (let ((set '()))
      (case-lambda
       (()
	set)
       ((x)
	(unless (memq x set)
	  (set! set (cons x set))))
       ((x del?)
	(if del?
	    (set! set (remq x set))
	  (unless (memq x set)
	    (set! set (cons x set))))))))

  (define current-library-collection
    ;;Hold a collection of installed LIBRARY structs.
    ;;
    (make-parameter (make-collection)
      (lambda (x)
	(define who 'current-library-collection)
	(with-arguments-validation (who)
	    ((procedure	x))
	  x))))

  #| end of module: CURRENT-LIBRARY-COLLECTION |# )

(define %external-pending-libraries
  ;;Hold  the list  of  library names  whose  installation is  currently
  ;;pending; used to detect circular dependencies between libraries.
  ;;
  ;;For this  parameter: a "library name"  is a list of  symbols without
  ;;the version specification.
  ;;
  (make-parameter '()))


;;;; loading source and serialized libraries from files

(define current-library-source-file-locator
  ;;Hold a  function used to  convert a library name  specification into
  ;;the  corresponding  file  pathname; this  parameter  is  initialised
  ;;"ikarus.load.sls" with the function LOCATE-LIBRARY-SOURCE-FILE.
  ;;
  ;;The selected  function must accept  2 values: a  string representing
  ;;the source  library file  name; the possibly  empty list  of library
  ;;names that  representing the  libraries that requested  this library
  ;;loading.  The second argument is the cdr of the current value in the
  ;;parameter %EXTERNAL-PENDING-LIBRARIES.
  ;;
  (make-parameter
      (lambda (filename pending-libraries)
	(error 'current-library-source-file-locator
	  "source library locator not set" filename))
    (lambda (obj)
      (define who 'current-library-source-file-locator)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))

(define current-library-source-loader
  ;;Hold a  function used to  laod a  source library; this  parameter is
  ;;initialised      "ikarus.load.sls"       with      the      function
  ;;READ-LIBRARY-SOURCE-FILE.
  ;;
  ;;The  referenced function  must:  accept a  string  file pathname  as
  ;;single  argument,  open the  pathname  for  input using  the  native
  ;;transcoder, read the first datum, close the port, return the datum.
  ;;
  (make-parameter
      (lambda (filename)
	(error 'current-library-source-loader
	  "source library loader not set" filename))
    (lambda (obj)
      (define who 'current-library-source-loader)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))

(define current-library-serialized-loader
  ;;Hold a function  used to load a precompiled  library; this parameter
  ;;is    initialised   in    "ikarus.load.sls"   with    the   function
  ;;LOAD-SERIALIZED-LIBRARY.
  ;;
  ;;The  referenced   function  must   accept  2  arguments:   a  string
  ;;representing the  pathname of the  file from which a  library source
  ;;can  be  read;  a  function   to  be  called  whenever  loading  the
  ;;precompiled file succeeds.
  ;;
  ;;For   details    on   the   success   continuation    function   see
  ;;%INSTALL-PRECOMPILED-LIBRARY-AND-ITS-DEPENCENCIES.
  ;;
  (make-parameter
      (lambda (file-name success-kont)
	#f)
    (lambda (obj)
      (define who 'current-library-serialized-loader)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))


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
      (lambda (obj)
	(define who 'current-include-loader)
	(with-arguments-validation (who)
	    ((procedure	obj))
	  obj))))

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
    (lambda (obj)
      (define who 'current-include-file-locator)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))

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
    (lambda (obj)
      (define who 'current-include-file-loader)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))


;;;; loading libraries from files
;;
;;The  library  loader  is  a  function that  loads  a  library,  either
;;precompiled or from source, and installs it.
;;
(module (current-library-loader)
  (define-constant __who__ 'default-library-loader)

  (define* (default-library-loader requested-libname)
    ;;Default value  for the parameter CURRENT-LIBRARY-LOADER.   Given a
    ;;library name  specification: search  the associated  file pathname
    ;;and attempt  to load  the file;  try first  to load  a precompiled
    ;;file,  if any,  then  try to  load the  source  file.  The  loaded
    ;;library is installed.
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    (let ((filename ((current-library-source-file-locator)
		     requested-libname
		     (cdr (%external-pending-libraries)))))
      (cond ((not filename)
	     (assertion-violation __who__ "cannot find library" requested-libname))
	    ;;Try to load  a FASL library file associated  to the source
	    ;;file pathname.
	    (((current-library-serialized-loader)
	      filename %install-precompiled-library-and-its-depencencies))
	    (else
	     ;;If we  are here: the precompiled  library loader returned
	     ;;false, which means no valid  FASL file was available.  So
	     ;;try to load the source file.
	     ((current-library-expander)
	      ;;Return  a symbolic  expression representing  the LIBRARY
	      ;;form, or raise an exception.
	      ((current-library-source-loader) filename)
	      filename
	      (lambda (library-name.ids library-name.version)
		(%verify-library requested-libname filename
				 library-name.ids library-name.version)))))))

  (define (%install-precompiled-library-and-its-depencencies
	   filename
	   uid libname.ids libname.version
	   import-descr* visit-descr* invoke-descr*
	   export-subst export-env
	   visit-proc invoke-proc guard-proc
	   guard-descr* visible? library-option*)
    ;;Used  as success  continuation  function by  the  function in  the
    ;;parameter  CURRENT-LIBRARY-SERIALIZED-LOADER.   All the  arguments
    ;;after FILENAME are the CONTENTS of the serialized library.
    ;;
    ;;Make sure all  dependencies are met, then install  the library and
    ;;return true; otherwise return #f.
    ;;
    (let loop ((descr* (append import-descr* visit-descr* invoke-descr* guard-descr*)))
      (cond ((null? descr*)
	     (for-each (lambda (guard-library-descriptor)
			 (let* ((guard-uid     (car  guard-library-descriptor))
				(guard-libname (cadr guard-library-descriptor))
				(guard-lib     (find-library-by-name guard-libname)))
			   (invoke-library guard-lib)))
	       guard-descr*)
	     (if (guard-proc)
		 ;;The precompiled library is stale.
		 (begin
		   (library-stale-warning libname.ids filename)
		   #f)
	       (let ((visit-code        #f)
		     (invoke-code       #f)
		     (guard-code        (quote (quote #f)))
		     (guard-descr*      '())
		     (source-file-name  #f))
		 (install-library uid
				  libname.ids libname.version
				  import-descr* visit-descr* invoke-descr*
				  export-subst export-env
				  visit-proc invoke-proc
				  visit-code invoke-code
				  guard-code guard-descr*
				  visible? source-file-name library-option*)
		 #t)))
	    (else
	     ;;We expect each library descriptor to have the format:
	     ;;
	     ;;   (?uid ?libname ?version)
	     ;;
	     ;;where ?UID is  a unique symbol associated  to the library
	     ;;and  ?LIBNAME is  the  list of  symbols representing  the
	     ;;library name.
	     ;;
	     (let* ((deplib-descr	(car  descr*))
		    (deplib-uid		(car  deplib-descr))
		    (deplib-libname	(cadr deplib-descr))
		    (deplib-lib		(find-library-by-name deplib-libname)))
	       (if (and (library? deplib-lib)
			(eq? deplib-uid (library-id deplib-lib)))
		   (loop (cdr descr*))
		 (begin
		   (library-version-mismatch-warning libname.ids deplib-libname filename)
		   #f)))))))

  (define (%verify-library requested-libname filename
			   found-library-name.ids found-library-name.version)
    ;;Verify  the  name  of  loaded  library against  the  name  of  the
    ;;requested library.
    ;;
    ;;FOUND-LIBRARY-NAME.IDS  is the  list of  symbols from  the library
    ;;name.   FOUND-LIBRARY-NAME.VERSION is  null or  the list  of exact
    ;;integers representing the library version.
    ;;
    (unless (equal? found-library-name.ids requested-libname)
      (assertion-violation __who__
	(let-values (((port extract) (open-string-output-port)))
	  (display "expected to find library " port)
	  (write requested-libname port)
	  (display " in file " port)
	  (display filename port)
	  (display ", found " port)
	  (write found-library-name.ids port)
	  (display " instead" port)
	  (extract)))))

  (define current-library-loader
    ;;Hold a function used to load a library, either precompiled or from
    ;;source.
    ;;
    (make-parameter
	default-library-loader
      (lambda (f)
	(define who 'current-library-loader)
	(with-arguments-validation (who)
	    ((procedure	f))
	  f))))

  #| end of module: CURRENT-LIBRARY-LOADER |# )


;;;; finding libraries, already loaded or not

(define (library-exists? libname)
  ;;Given a library name search  the corresponding LIBRARY record in the
  ;;collection of already installed libraries: return true or false.
  ;;
  ;;For this function, a "library name" is a list of symbols without the
  ;;version specification.
  ;;
  (and (%find-library-in-collection-by (lambda (x)
					 (equal? (library-name x) libname)))
       #t))

(module (find-library-by-name)

  (define (find-library-by-name libname)
    ;;Given a library name: try  to search and install the corresponding
    ;;library, if  it is not  already installed; when  successful return
    ;;the corresponding LIBRARY record.
    ;;
    ;;Search  for the  library in  the  internal collection  or, if  not
    ;;found, in the external source  (for example the file system) using
    ;;the current CURRENT-LIBRARY-LOADER.
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    (or (%find-library-in-collection-by (lambda (x)
					  (equal? (library-name x) libname)))
	(%find-and-install-external-library libname)))

  (define* (%find-and-install-external-library libname)
    ;;Given a  library name  try to  load it  using the  current library
    ;;loader.
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    (when (member libname (%external-pending-libraries))
      (assertion-violation __who__
	"circular attempt to import library was detected" libname))
    (parametrise ((%external-pending-libraries (cons libname (%external-pending-libraries))))
      ;;Load the  library, either from  source file or  serialized file,
      ;;and install it.
      ((current-library-loader) libname)
      (or (%find-library-in-collection-by (lambda (x)
					    (equal? (library-name x) libname)))
	  (assertion-violation __who__
	    "handling external library did not yield the correct library" libname))))

  #| end of module: FIND-LIBRARY-BY-NAME |# )

(define (%find-library-in-collection-by pred)
  ;;Visit  the current  installed  libraries collection  and return  the
  ;;first for  which PRED returns true.   If PRED returns false  for all
  ;;the entries in the collection: return false.
  ;;
  (let next-library-struct ((ls ((current-library-collection))))
    (cond ((null? ls)
	   #f)
	  ((pred (car ls))
	   (car ls))
	  (else
	   (next-library-struct (cdr ls))))))

(define (%find-library-in-collection-by-descr/die descr)
  ;;Given   a  library   descriptor,  as   generated  by   the  function
  ;;LIBRARY-DESCRIPTOR: return the corresponding LIBRARY record from the
  ;;collection of installed libraries or raise an assertion.
  ;;
  (let ((uid (car descr)))
    (or (%find-library-in-collection-by (lambda (x)
					  (eq? uid (library-id x))))
	(assertion-violation #f
	  "cannot find installed library with required descriptor" descr))))


;;;; serializing precompiled libraries

(define (serialize-collected-libraries serialize compile)
  ;;Traverse  the  current collection  of  libraries  and serialize  the
  ;;contents  of all  the  LIBRARY  records having  a  source file  (the
  ;;records that do not have a source file represent the libraries built
  ;;in the boot image).
  ;;
  ;;"Serializing" means to write in a FASL file.
  ;;
  ;;The   argument    SERIALIZE   should    be   a    closure   wrapping
  ;;DO-SERIALIZE-LIBRARY.
  ;;
  ;;The  argument COMPILE  should  be a  closure  wrapping the  function
  ;;COMPILE-CORE-EXPR.
  ;;
  (for-each (lambda (lib)
	      (serialize-library lib serialize compile))
    ((current-library-collection))))

(define* (serialize-library (lib library?) (serialize procedure?) (compile procedure?))
  (when ($library-source-file-name lib)
    (serialize ($library-source-file-name lib)
	       (list ($library-id lib)
		     ($library-name lib)
		     ($library-version lib)
		     (map library-descriptor ($library-imp* lib))
		     (map library-descriptor ($library-vis* lib))
		     (map library-descriptor ($library-inv* lib))
		     ($library-subst lib)
		     ($library-env lib)
		     (compile ($library-visit-code lib))
		     (compile ($library-invoke-code lib))
		     (compile ($library-guard-code lib))
		     (map library-descriptor ($library-guard-req* lib))
		     ($library-visible? lib)
		     ($library-option* lib)))))


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
	   ((or all? (library-visible? (car ls)))
	    (cons (car ls) (next-library-struct (cdr ls))))
	   (else
	    (next-library-struct (cdr ls)))))))

(module (install-library)
  ;;INSTALL-LIBRARY  builds a  LIBRARY  record and  installs  it in  the
  ;;internal collection of libraries; return unspecified values.  We can
  ;;see  EXPAND-LIBRARY  for  a   more  detailed  description,  but  the
  ;;arguments are:
  ;;
  ;;ID -
  ;;   A gensym uniquely identifying this library.
  ;;
  ;;NAME -
  ;;   A list of symbols representing the library name.
  ;;
  ;;VER -
  ;;   A list of exact integers representing the library version.
  ;;
  ;;IMPORT-DESCR* -
  ;;   A list of library descriptors enumerating the libraries specified
  ;;   in the IMPORT clauses.
  ;;
  ;;VISIT-DESCR* -
  ;;   A list of library descriptors enumerating the libraries needed by
  ;;   the visit code.
  ;;
  ;;INVOKE-DESCR* -
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
  ;;GUARD-DESCR* -
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
  (case-define install-library
    ;;FIXME  At  the next  boot  image  rotation the  optional  argument
    ;;LIBRARY-OPTION*  must become  a mandatory  argument.  For  this to
    ;;happen  the appropriate  argument must  be  added to  the uses  of
    ;;INSTALL-LIBRARY in the "makefile.sps".   (Marco Maggi; Mon Feb 10,
    ;;2014)
    ((id
      libname ver
      import-descr* visit-descr* invoke-descr*
      export-subst export-env
      visit-proc invoke-proc
      visit-code invoke-code
      guard-code guard-descr*
      visible? source-file-name)
     (install-library id libname ver
		      import-descr* visit-descr* invoke-descr*
		      export-subst export-env
		      visit-proc invoke-proc
		      visit-code invoke-code
		      guard-code guard-descr*
		      visible? source-file-name '()))
    ((id
      libname ver
      import-descr* visit-descr* invoke-descr*
      export-subst export-env
      visit-proc invoke-proc
      visit-code invoke-code
      guard-code guard-descr*
      visible? source-file-name library-option*)
     (let ((import-lib*	(map %find-library-in-collection-by-descr/die import-descr*))
	   (visit-lib*	(map %find-library-in-collection-by-descr/die visit-descr*))
	   (invoke-lib*	(map %find-library-in-collection-by-descr/die invoke-descr*))
	   (guard-lib*	(map %find-library-in-collection-by-descr/die guard-descr*)))
       (unless (and (symbol? id) (list? libname) (list? ver))
	 (assertion-violation __who__
	   "invalid spec with id/name/ver" id libname ver))
       (when (library-exists? libname)
	 (assertion-violation __who__
	   "library is already installed" libname))
       (let ((lib (make-library id libname ver import-lib* visit-lib* invoke-lib*
				export-subst export-env visit-proc invoke-proc
				visit-code invoke-code guard-code guard-lib*
				visible? source-file-name library-option*)))
	 (%install-library-record lib)
	 (when (memq 'visit-upon-loading library-option*)
	   (visit-library lib))))))

  (define (%install-library-record lib)
    (for-each
	(lambda (export-env-entry)
	  ;;See the comments in the expander  code for the format of the
	  ;;EXPORT-ENV.  Entries  in the  EXPORT-ENV are  different from
	  ;;entries in the LEXENV; here we transform an EXPORT-ENV entry
	  ;;into a LEXENV entry.
	  (let* ((label    (car export-env-entry))
		 (binding  (cdr export-env-entry))
		 (binding1 (case (car binding)
			     ((global)        (cons* 'global        lib (cdr binding)))
			     ((global-macro)  (cons* 'global-macro  lib (cdr binding)))
			     ((global-macro!) (cons* 'global-macro! lib (cdr binding)))
			     ((global-ctv)    (cons* 'global-ctv    lib (cdr binding)))
			     (( ;;
			       library import export
			       define define-syntax define-alias define-fluid-syntax
			       let-syntax letrec-syntax begin-for-syntax
			       module begin set! stale-when
			       global mutable
			       core-prim core-macro macro
			       $core-rtd $rtd $module $fluid $synonym)
			      binding)
			     (else
			      (assertion-violation __who__
				"invalid syntactic binding descriptor type in EXPORT-ENV entry"
				lib export-env-entry)))))
	    ;;When the library is serialized: the content of the "value"
	    ;;slot is  not saved, so we  have to set it  here every time
	    ;;the library is loaded.
	    (set-label-binding! label binding1)))
      ;;This expression returns the EXPORT-ENV of the library LIB.
      (library-env lib))
    ((current-library-collection) lib))

  #| end of module: INSTALL-LIBRARY |# )


;;;; uninstalling libraries
;;
;;Libraries  from   the  collection   of  installed  libraries   can  be
;;uninstalled,   either   to  free   system   resources   or  to   allow
;;reinstallation from new files.
;;
(case-define* uninstall-library
  ;;Uninstall a library.
  ;;
  ;;THE IMPLEMENTATION OF THIS FUNCTION IS INCOMPLETE.
  ;;
  ((name)
   (uninstall-library name #t))
  ((name err?)
   ;;FIXME: check that no other import is in progress.  (Ghuloum)
   ;;
   ;;FIXME: need to  unintern labels and locations  of library bindings.
   ;;(Ghuloum)
   (let ((lib (%find-library-in-collection-by (lambda (x)
						(equal? (library-name x) name)))))
     (when (and err? (not lib))
       (assertion-violation __who__ "library not installed" name))
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
       (library-env lib)))
   (values)))


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

(define (invoke-library lib)
  ;;Evaluate the invoke code for the LIBRARY struct LIB.
  ;;
  (let ((invoke (library-invoke-state lib)))
    (when (procedure? invoke)
      (set-library-invoke-state! lib
				 (lambda ()
				   (assertion-violation 'invoke
				     "circularity detected" lib)))
      (for-each invoke-library (library-inv* lib))
      (set-library-invoke-state! lib
				 (lambda ()
				   (assertion-violation 'invoke
				     "first invoke did not return" lib)))
      (invoke)
      (set-library-invoke-state! lib #t))))

(define (visit-library lib)
  ;;Evaluate the visit code for the LIBRARY struct LIB.
  ;;
  (let ((visit (library-visit-state lib)))
    (when (procedure? visit)
      (set-library-visit-state! lib
				(lambda ()
				  (assertion-violation 'visit
				    "circularity detected" lib)))
      (for-each invoke-library (library-vis* lib))
      (set-library-visit-state! lib
				(lambda ()
				  (assertion-violation 'invoke
				    "first visit did not return" lib)))
      (visit)
      (set-library-visit-state! lib #t))))


;;;; R6RS library name and version utilities

(define (library-version-numbers? obj)
  ;;Return #t if  OBJ is a list of library  version numbers according to
  ;;R6RS, this includes OBJ being null.
  ;;
  ;;NOTE According to R6RS: OBJ should  be an exact integer, which means
  ;;a finxum or bignum for Vicare.   We accept only fixnums because they
  ;;are faster  to handle and "big  enough".  (Marco Maggi; Tue  Apr 23,
  ;;2013)
  ;;
  (or (null? obj)
      (and (list? obj)
	   (for-all library-version-number? obj))))

(define (library-version-number? obj)
  ;;Return #t if OBJ is a version number according to R6RS.
  ;;
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

(define (library-name? sexp)
  ;;Return  #t if  SEXP is  a  symbolic expressions  compliant with  the
  ;;definition of <LIBRARY NAME> according to R6RS.
  ;;
  (receive (identifiers version)
      (library-name-decompose sexp)
    (if identifiers #t #f)))

;;; --------------------------------------------------------------------

(define (library-name-decompose obj)
  ;;Scan OBJ  validating it as  a <LIBRARY  NAME> as specified  by R6RS.
  ;;Return   two  values:   the   list  of   identifiers,  the   version
  ;;specification.  The  version can  be null.   If OBJ  is not  a valid
  ;;<LIBRARY NAME>:  return #f  and #f.  The  returned values  may share
  ;;structure with OBJ.
  ;;
  (if (or (null? obj) (not (list? obj)))
      (values #f #f)
    (let next-identifier ((next (car obj))
			  (tail (cdr obj))
			  (ids  '()))
      (cond ((symbol? next) ;identifier
	     (if (null? tail)
		 ;;There is  no version number, so we  return OBJ itself
		 ;;as list of identifiers.
		 (values obj '())
	       (next-identifier (car tail) (cdr tail) (cons next ids))))
	    ((and (list? next) (null? tail)) ;version spec
	     (if (library-version-numbers? next)
		 (values (reverse ids) next)
	       (values #f #f)))
	    (else
	     (values #f #f))))))

(define (library-name->identifiers sexp)
  ;;Given  a  symbolic  expressions  compliant with  the  definition  of
  ;;<LIBRARY NAME>  according to R6RS:  return the list  of identifiers.
  ;;If SEXP is not compliant return #f.
  ;;
  (receive (identifiers version)
      (library-name-decompose sexp)
    identifiers))

(define (library-name->version sexp)
  ;;Given  a  symbolic  expressions  compliant with  the  definition  of
  ;;<LIBRARY  NAME>  according  to  R6RS:  return the  list  of  version
  ;;numbers.  If SEXP is not compliant return #f.
  ;;
  (receive (identifiers version)
      (library-name-decompose sexp)
    version))

;;; --------------------------------------------------------------------

(define* (library-name-identifiers=? (sexp1 library-name?) (sexp2 library-name?))
  ;;Given  two symbolic  expressions  compliant with  the definition  of
  ;;<LIBRARY NAME>  according to R6RS: return  #t if they  have the same
  ;;list of identifiers.
  ;;
  (for-all eq?
	   (library-name->identifiers sexp1)
	   (library-name->identifiers sexp2)))

(module (library-name=?
	 library-name<?
	 library-name<=?)

  (define (library-name=? sexp1 sexp2)
    ;;Given two  symbolic expressions  compliant with the  definition of
    ;;<LIBRARY NAME> according to R6RS: return  #t if they have the same
    ;;list of identifiers and the same version numbers.
    ;;
    (%library-name-comparison library-version=? sexp1 sexp2))

  (define (library-name<? sexp1 sexp2)
    ;;Given two  symbolic expressions  compliant with the  definition of
    ;;<LIBRARY NAME> according to R6RS: return  #t if they have the same
    ;;list of  identifiers and  the version  of SEXP1  is less  than the
    ;;version of SEXP2.
    ;;
    (%library-name-comparison library-version<? sexp1 sexp2))

  (define (library-name<=? sexp1 sexp2)
    ;;Given two  symbolic expressions  compliant with the  definition of
    ;;<LIBRARY NAME> according to R6RS: return  #t if they have the same
    ;;list of identifiers and the version of SEXP1 is less than or equal
    ;;to the version of SEXP2.
    ;;
    (%library-name-comparison library-version<=? sexp1 sexp2))

  (define* (%library-name-comparison version-predicate (sexp1 library-name?) (sexp2 library-name?))
    (let-values
	(((ids1 vrs1) (library-name-decompose sexp1))
	 ((ids2 vrs2) (library-name-decompose sexp2)))
      (and (= (length ids1)
	      (length ids2))
	   (for-all eq? ids1 ids2)
	   (version-predicate vrs1 vrs2))))

  #|end of module |# )

;;; --------------------------------------------------------------------

(define* (library-version=? (vrs1 library-version-numbers?) (vrs2 library-version-numbers?))
  ;;Given two lists of version  numbers compliant with the definition of
  ;;<LIBRARY NAME>  according to R6RS: return  #t if they  have the same
  ;;numbers.
  ;;
  ;;If one of the lists is longer  and the elements up to the end of the
  ;;shortest are equal: the lists are "equal" if the tail of the longest
  ;;is made of zeros.
  ;;
  ;;Examples:
  ;;
  ;;	(1 2 3) == (1 2 3)
  ;;	(1 2 3) != (1 2 3 4)
  ;;	(1 2 3) == (1 2 3 0 0 0)
  ;;
  (let loop ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   (or (null? vrs2)
	       (for-all (lambda (fx)
			  ($fxzero? fx))
		 vrs2)))
	  ((null? vrs2)
	   (for-all (lambda (fx)
		      ($fxzero? fx))
	     vrs1)) ;it cannot be (null? vrs1) here
	  (else
	   (and ($fx= ($car vrs1) ($car vrs2))
		(loop ($cdr vrs1) ($cdr vrs2)))))))

(define* (library-version<? (vrs1 library-version-numbers?) (vrs2 library-version-numbers?))
  ;;Given two lists of version  numbers compliant with the definition of
  ;;<LIBRARY NAME>  according to R6RS:  return #t if the  version number
  ;;represented by VRS1  is less than the version  number represented by
  ;;VRS2.
  ;;
  ;;Comparison  of digits  stops at  the first  digit for  which <  or >
  ;;return true.
  ;;
  ;;If one of the lists is longer  and the elements up to the end of the
  ;;shortest are equal: the lists are "equal" if the tail of the longest
  ;;is made of zeros.
  ;;
  ;;Examples:
  ;;
  ;;	(1 2 3) <  (4 2 3)
  ;;	(1 2 3) <  (1 4 3)
  ;;	(1 2 3) <  (1 2 4)
  ;;	(1 2 3) <  (1 2 3 4)
  ;;	(1 2 3) !< (1 2 3 0 0 0)
  ;;
  (let loop ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   (cond ((null? vrs2)		#f)
		 ((find (lambda (fx)
			  ($fxpositive? fx))
		    vrs2)		#t)
		 (else			#f)))
	  ((null? vrs2)
	   #f)
	  (($fx< ($car vrs1) ($car vrs2))
	   #t)
	  (($fx> ($car vrs1) ($car vrs2))
	   #f)
	  (else ;;(= (car vrs1) (car vrs2))
	   (loop ($cdr vrs1) ($cdr vrs2))))))

(define* (library-version<=? (vrs1 library-version-numbers?) (vrs2 library-version-numbers?))
  ;;Given two lists of version  numbers compliant with the definition of
  ;;<LIBRARY NAME>  according to R6RS:  return #t if the  version number
  ;;represented  by VRS1 is  less than  or equal  to the  version number
  ;;represented by VRS2.
  ;;
  ;;Comparison of digits  stops at the first digit  for which <= returns
  ;;false.
  ;;
  ;;If one of the lists is longer  and the elements up to the end of the
  ;;shortest are equal: the lists are "equal" if the tail of the longest
  ;;is made of zeros.
  ;;
  ;;Examples:
  ;;
  ;;	(1 2 3) <= (1 2 3)
  ;;	(1 2 3) <= (4 2 3)
  ;;	(1 2 3) <= (1 4 3)
  ;;	(1 2 3) <= (1 2 4)
  ;;	(1 2 3) <= (1 2 3 4)
  ;;	(1 2 3 0) <= (1 2 3)
  ;;
  (let loop ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   #t)
	  ((null? vrs2)
	   (for-all (lambda (fx)
		      ($fxzero? fx))
	     vrs1))
	  (else
	   (and ($fx<= ($car vrs1) ($car vrs2))
		(loop  ($cdr vrs1) ($cdr vrs2)))))))


;;;; R6RS library references and conformity

(define-syntax %normalise-to-boolean
  (syntax-rules ()
    ((_ ?expr)
     (if ?expr #t #f))))

;;; --------------------------------------------------------------------
;;; predicates

(define (library-reference? sexp)
  ;;Return true  if SEXP is  a valid  library reference as  specified by
  ;;R6RS.
  ;;
  (receive (identifiers version)
      (library-reference-decompose sexp)
    (%normalise-to-boolean identifiers)))

(define (library-version-reference? obj)
  ;;Return true if OBJ is a valid library version reference as specified
  ;;by R6RS.
  ;;
  (match obj
    ;;We decide to  accept empty AND clauses, which  will always match a
    ;;version specification.
    (('and (let ?version-reference ...))
     (for-all library-version-reference? ?version-reference))

    ;;We decide  to accept  empty OR clauses,  which will never  match a
    ;;version specification.
    (('or  (let ?version-reference ...))
     (for-all library-version-reference? ?version-reference))

    (('not (let ?version-reference))
     (library-version-reference? ?version-reference))

    ;;Notice that null is a valid version reference as specified by R6RS
    ;;(see  the table  at  the  end of  the  documentation node  "scheme
    ;;library form version" in Nausicaa).  Null always matches.
    (((let ?sub-version-reference ...))
     (for-all library-sub-version-reference? ?sub-version-reference))))

(define (library-sub-version-reference? sub-version)
  ;;Return  true if  OBJ is  a  valid library  sub-version reference  as
  ;;specified by R6RS.
  ;;
  (match sub-version
    (('and (let ?sub-version ...))
     (for-all library-sub-version-reference? ?sub-version))
    (('or  (let ?sub-version ...))
     (for-all library-sub-version-reference? ?sub-version))
    (('not (let ?sub-version))
     (library-sub-version-reference? ?sub-version))
    (('<= (let ?sub-version))
     (library-sub-version-reference? ?sub-version))
    (('>= (let ?sub-version))
     (library-sub-version-reference? ?sub-version))
    ((apply library-sub-version?)
     #t)
    (_ #f)))

(define (library-sub-version? obj)
  ;;Return #t if OBJ is a sub-version number.
  ;;
  ;;NOTE According to R6RS: OBJ should be an exact non-negative integer,
  ;;which means a  non-negative finxum or bignum for  Vicare.  We accept
  ;;only fixnums  because they  are faster to  handle and  "big enough".
  ;;(Marco Maggi; Tue Apr 23, 2013)
  ;;
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

;;; --------------------------------------------------------------------
;;; decomposition

(define (library-reference-decompose obj)
  ;;Scan  OBJ validating  it as  a <library  reference> as  specified by
  ;;R6RS.   Return two  values:  the list  of  identifiers, the  version
  ;;reference.  The version can be null.  If OBJ is not a valid <library
  ;;reference>: return #f and #f.
  ;;
  (if (or (null? obj)
	  (not (list? obj)))
      (values #f #f)
    (let next-identifier ((next ($car obj))
			  (rest ($cdr obj))
			  (ids  '()))
      (cond ((symbol? next) ;identifier
	     (if (null? rest)
		 ;;No  version   reference,  so  OBJ  is   the  list  of
		 ;;identifiers.
		 (values obj '()) ; == (values (reverse (cons next ids)) '())
	       (next-identifier ($car rest) ($cdr rest) (cons next ids))))
	    ((and (list? next) (null? rest)) ;version spec
	     (if (library-version-reference? next)
		 (values (reverse ids) next)
	       (values #f #f)))
	    (else
	     (values #f #f))))))

;;; --------------------------------------------------------------------

(define (library-reference->identifiers sexp)
  (receive (identifiers version)
      (library-reference-decompose sexp)
    identifiers))

(define (library-reference->version-reference sexp)
  (receive (identifiers version)
      (library-reference-decompose sexp)
    version))

;;; --------------------------------------------------------------------

(define* (library-reference-identifiers=? (ref1 library-reference?) (ref2 library-reference?))
  (let ((ids1 (library-reference->identifiers ref1))
	(ids2 (library-reference->identifiers ref2)))
    (and (= (length ids1)
	    (length ids2))
	 (for-all eq? ids1 ids2))))

;;; --------------------------------------------------------------------

(define* (conforming-sub-version-and-sub-version-reference? (sub-version library-sub-version?)
							    (sub-version-reference library-sub-version-reference?))
  ;;SUB-VERSION must  be a fixnum  representing a single  version number
  ;;from a library name, as defined by R6RS.
  ;;
  ;;SUB-VERSION-REFERENCE  must be  a  single  sub-version reference  as
  ;;specified by R6RS:
  ;;
  ;;   ?sub-version-reference
  ;;     == ?sub-version ...
  ;;     == (>=  ?sub-version)
  ;;     == (<=  ?sub-version)
  ;;     == (and ?sub-version-reference ...)
  ;;     == (or  ?sub-version-reference ...)
  ;;     == (not ?sub-version-reference)
  ;;
  (define (%recurse sub-ver-ref)
    (conforming-sub-version-and-sub-version-reference? sub-version sub-ver-ref))
  (match sub-version-reference
    ((apply library-sub-version?)
     ($fx= sub-version sub-version-reference))

    (('>= (let ?sub-version-ref))
     ($fx>= sub-version ?sub-version-ref))

    (('<= (let ?sub-version-ref))
     ($fx<= sub-version ?sub-version-ref))

    (('and)
     #t)

    (('and (let ?sub-version-ref ...))
     (%normalise-to-boolean
      (for-all %recurse ?sub-version-ref)))

    (('or)
     #f)

    (('or (let ?sub-version-reference ...))
     (%normalise-to-boolean
      (find %recurse ?sub-version-reference)))

    (('not (let ?sub-version-ref))
     (not (%recurse ?sub-version-ref)))

    (else
     (assertion-violation __who__
       "invalid library sub-version reference" sub-version-reference))))

;;; --------------------------------------------------------------------

(define* (conforming-version-and-version-reference? (version library-version-numbers?)
						    (version-reference library-version-reference?))
  ;;VERSION must be a list of version numbers as specified by R6RS.
  ;;
  ;;VERSION-REFERENCE must be a version reference as specified by R6RS:
  ;;
  ;;  ?version-reference
  ;;     == (?sub-version-reference ...)
  ;;     == (and ?version-reference ...)
  ;;     == (or  ?version-reference ...)
  ;;     == (not ?version-reference)
  ;;
  ;;  ?sub-version-reference
  ;;     == ?sub-version
  ;;     == (>=  ?sub-version)
  ;;     == (<=  ?sub-version)
  ;;     == (and ?sub-version-reference ...)
  ;;     == (or  ?sub-version-reference ...)
  ;;     == (not ?sub-version-reference)
  ;;
  ;;  ?sub-version
  ;;     == #<non-negative fixnum>
  ;;
  (match version-reference
    (()
     #t)

    (('and (let ?version-reference ...))
     (%normalise-to-boolean
      (for-all (lambda (reference)
		 (conforming-version-and-version-reference? version reference))
	?version-reference)))

    (('or (let ?version-reference ...))
     (%normalise-to-boolean
      (find (lambda (reference)
	      (conforming-version-and-version-reference? version reference))
	?version-reference)))

    (('not (let ?version-reference))
     (not (conforming-version-and-version-reference? version ?version-reference)))

    (_
     (let next-sub-version ((version		version)
			    (version-reference	version-reference))
       (cond ((null? version-reference)
	      ;;According  to R6RS:  if  the  version reference  is
	      ;;shorter than the version, it is a match.
	      #t)
	     ((null? version)
	      (null? version-reference))
	     ((conforming-sub-version-and-sub-version-reference?
	       ($car version) ($car version-reference))
	      (next-sub-version ($cdr version) ($cdr version-reference)))
	     (else
	      #f))))))

;;; --------------------------------------------------------------------

(define* (conforming-library-name-and-library-reference? (name library-name?)
							 (reference library-reference?))
  (let-values
      (((libnam.ids libnam.version)  (library-name-decompose name))
       ((libref.ids libref.version)  (library-reference-decompose reference)))
    (and (for-all eq? libnam.ids libref.ids)
	 (conforming-version-and-version-reference? libnam.version libref.version))))


;;;; done

)

;;; end of file
