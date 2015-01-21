;;;Copyright (c) 2013, 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#!vicare
(library (psyntax.library-manager)
  (export
    ;; library inspection
    library?
    library-uid				library-name
    library-imp-lib*			library-vis-lib*
    library-inv-lib*			library-export-subst
    library-export-env			library-visit-state
    library-invoke-state		library-visit-code
    library-invoke-code			library-guard-code
    library-guard-lib*			library-visible?
    library-source-file-name		library-option*
    library-loaded-from-source-file?	library-loaded-from-binary-file?
    library-descriptor			library-descriptor?
    library-descriptor-uid		library-descriptor-name

    ;; interning libraries
    intern-library			unintern-library
    interned-libraries			intern-binary-library-and-its-dependencies

    ;; library operations
    visit-library			invoke-library

    ;; interned library collection
    current-library-collection		find-library-in-collection-by-predicate
    find-library-by-name		find-library-in-collection-by-name
    find-library-by-reference		find-library-in-collection-by-reference
    find-library-by-descriptor		find-library-in-collection-by-descriptor
    external-pending-libraries

    ;; parameters
    current-library-loader
    current-library-expander
    current-include-loader
    source-code-location

    ;; miscellaneous
    library-name-identifiers
    imported-label->syntactic-binding)
  (import (rnrs)
    (psyntax.compat))


;;;; type definitions: library object

(define-record library
  (uid
		;A  gensym  uniquely  identifying  this  interned  library;  it  also
		;identifies the corresponding serialised library.
		;
		;This gensym is  registered in: the LIBRARY object  in the collection
		;of interned  libraries; the binary  file containing this  library in
		;compiled  and  serialised  form;  the binary  files  containing  the
		;compiled libraries that import this one.
		;
		;Whenever a compiled library imports this  one, the UID stored in the
		;binary file is compared to this field: if they are EQ?  the compiled
		;versions  are  in sync,  otherwise  the  importing library  must  be
		;recompiled.
   name
		;A library name as defined by R6RS; it is the symbolic expression:
		;
		;   (?identifier0 ?identifier ...)
		;   (?identifier0 ?identifier ... ?version)
		;
		;where  the  ?IDENTIFIERs are  symbols  and  ?VERSION  is a  list  of
		;non-negative fixnums representing the version numbers.
   imp-lib*
		;The list of LIBRARY objects selected by the IMPORT syntax.
   vis-lib*
		;The list of LIBRARY objects  selecting libraries needed by the visit
		;code.
   inv-lib*
		;The list of LIBRARY objects selecting libraries needed by the invoke
		;code.
   export-subst
		;A subst  selecting the exported  bindings from the  EXPORT-ENV.  See
		;the expander's code for details.
   export-env
		;The EXPORT-ENV  representing the  top-level bindings defined  by the
		;library body.  See the expander's code for details.
   visit-state
		;When set  to a  procedure: it is  the thunk to  call to  compile and
		;evaluate the visit  code.  When set to something  else: this library
		;has been already visited.
   invoke-state
		;When set  to a  procedure: it is  the thunk to  call to  compile and
		;evaluate the invoke code.  When  set to something else: this library
		;has been already invoked.
   visit-code
		;When this object  is created from source code: this  field is a core
		;language symbolic expression representing the visit code.  When this
		;object  is created  from a  binary file:  this field  is a  thunk to
		;evaluate to visit the library.
   invoke-code
		;When this object  is created from source code: this  field is a core
		;language  symbolic expression  representing the  invoke code.   When
		;this object is created from a binary  file: this field is a thunk to
		;evaluate to invoke the library.
   guard-code
		;When this object  is created from source code: this  field is a core
		;language symbolic expression representing the guard code.  When this
		;object  is created  from a  binary file:  this field  is a  thunk to
		;evaluate to run the STALE-WHEN composite test expression.
   guard-lib*
		;The  list  of LIBRARY  objects  selecting  libraries needed  by  the
		;STALE-WHEN composite test expression.
   visible?
		;A boolean determining if the  library is visible.  This attribute is
		;used  by  INTERNED-LIBRARIES  to   select  libraries  to  report  as
		;interned.
		;
		;A library should be marked as visible  if it is meant to be imported
		;by client  code in "normal"  use; unsafe libraries in  the hierarchy
		;"(vicare system ---)" should *not* be visible.
   source-file-name
		;False or a  string representing the pathname of the  file from which
		;the source code of the library was read.
   option*
		;A sexp holding library options.
   )
  (lambda (S port sub-printer)
    (define-syntax-rule (%display thing)
      (display thing port))
    (define-syntax-rule (%write thing)
      (write thing port))
    (%display "#<library ")
    (%display (library-name S))
    (%display " filename=")	(%write (library-source-file-name S))
    (%display ">")))

;;; --------------------------------------------------------------------

(define* (library-name-identifiers {lib library?})
  (library-name->identifiers ($library-name lib)))

(define* (library-loaded-from-source-file? {lib library?})
  (and (library-source-file-name lib) #t))

(define* (library-loaded-from-binary-file? {lib library?})
  (not (library-loaded-from-source-file? lib)))

;;; --------------------------------------------------------------------

(define* (library-descriptor {lib library?})
  ;;Given  a library  object return  a pair  having the  library UID  as car  and the
  ;;library name as cdr.
  ;;
  (cons (library-uid  lib)
	(library-name lib)))

(define (library-descriptor? obj)
  (and (pair? obj)
       (symbol?       (car obj))
       (library-name? (cdr obj))))

(define* (library-descriptor-uid {lib library-descriptor?})
  (car lib))

(define* (library-descriptor-name {lib library-descriptor?})
  (cdr lib))


;;;; collection of already interned libraries

(module (current-library-collection)
  ;;A  "library  collection"  is  a  closure object  closed  upon  a  list.   Library
  ;;collections are handled as  stacks: new items are pushed to the  top of the list,
  ;;items  are searched  from top  to bottom  stopping at  the first  match.  Closure
  ;;object interface:
  ;;
  ;;*  When  called  with no  arguments:  return  the  full  list of  LIBRARY  struct
  ;;  instances.
  ;;
  ;;* When called with one LIBRARY struct  argument: push the argument on the list as
  ;;  first head item, if it is not already there according to EQ?.
  ;;
  ;;* When called with two arguments:
  ;;
  ;;  - If the  second argument is true: remove the first argument  from the list, if
  ;;    present according to EQ?.
  ;;
  ;;  - If the  second argument is false: add the first argument  to the list, if not
  ;;    already there according to EQ?.
  ;;
  ;;When a library is interned: it is  added to the current library collection.  When
  ;;a library is uninterned: it is removed from the current library collection.
  ;;
  ;;When reasoning about loading libraries, remember that:
  ;;
  ;;* A R6RS library name is a perfectly valid R6RS library reference.
  ;;
  ;;* A specific LIBRARY struct instance can be added to the collection only once.
  ;;
  ;;* Multiple LIBRARY struct instances having the  same library name might be added to
  ;;  the collection;  when searching a  LIBRARY in  the collection matching  a library
  ;;  name: the search is performed from top  to bottom stopping at the first match, so
  ;;  the last collected instance is  returned.  However adding such multiple instances
  ;;  should be avoided.
  ;;
  ;;The parameter CURRENT-LIBRARY-COLLECTION is initialised with the closure bound to
  ;;DEFAULT-LIBRARY-COLLECTION;  such  default  collection holds  all  the  libraries
  ;;interned in a common running "vicare" process.
  ;;
  ;;As special case, when building a new boot image: a separate collection defined in
  ;;the  boot-image  build-script  (BOOTSTRAP-COLLECTION)  is  used  to  collect  the
  ;;libraries that will  end in the boot  image.  So, while building  the boot image:
  ;;DEFAULT-LIBRARY-COLLECTION contains the libraries used  to expand and compile the
  ;;boot  image source  code; BOOTSTRAP-COLLECTION  contains the  libraries that  are
  ;;components of the boot image.
  ;;
  (define default-library-collection
    (let ((set '()))
      (case-lambda*
	(()
	 set)
	(({lib library?})
	 (unless (memq lib set)
	   (library-debug-message "interned library: ~a" (library-name lib))
	   (set! set (cons lib set))))
	(({lib library?} del?)
	 (if del?
	     (begin
	       (library-debug-message "uninterning library: ~a" (library-name lib))
	       (set! set (remq lib set)))
	   (unless (memq lib set)
	     (library-debug-message "interned library: ~a" (library-name lib))
	     (set! set (cons lib set))))))))

  (define current-library-collection
    (make-parameter default-library-collection
      (lambda* ({obj procedure?})
	obj)))

  #| end of module: CURRENT-LIBRARY-COLLECTION |# )

(define (%remove-library-from-current-collection lib)
  ((current-library-collection) lib #t))


;;;; finding interned libraries

(define* (find-library-in-collection-by-predicate {pred procedure?})
  ;;Visit the current  interned-libraries collection and return the  first for which
  ;;PRED returns true.  If PRED returns false  for all the entries in the collection:
  ;;return false.
  ;;
  (let next-library-struct ((lib* ((current-library-collection))))
    (and (pair? lib*)
	 (if (pred (car lib*))
	     (car lib*)
	   (next-library-struct (cdr lib*))))))

(define* (find-library-in-collection-by-descriptor {libdesc library-descriptor?})
  ;;Given  a library  descriptor, as  generated by  the function  LIBRARY-DESCRIPTOR:
  ;;visit  the interned-libraries  collection and  return the  first LIBRARY  object
  ;;having  the  same  library UID.   If  no  matching  library  is found:  raise  an
  ;;exception.
  ;;
  (let ((uid (library-descriptor-uid libdesc)))
    (or (find-library-in-collection-by-predicate (lambda (lib)
						   (eq? uid (library-uid lib))))
	(error __who__
	  "cannot find interned library with required descriptor" libdesc))))

(define* (find-library-in-collection-by-reference {libref library-reference?})
  ;;Given  a symbolic  expression representing  a R6RS  library reference:  visit the
  ;;interned-libraries  collection  and  return  the  first  LIBRARY  object  having
  ;;conforming library  name identifiers.   If no matching  library is  found: return
  ;;false.
  ;;
  (find-library-in-collection-by-predicate (lambda (lib)
					     (equal? (library-reference->identifiers libref)
						     (library-name-identifiers lib)))))

(define* (find-library-in-collection-by-name {libname library-name?})
  ;;Given a symbolic expression representing a  R6RS library name: visit the interned
  ;;libraries  collection  and return  the  first  LIBRARY object  having  conforming
  ;;library name identifiers.  If no matching library is found: return false.
  ;;
  (find-library-in-collection-by-predicate (lambda (lib)
					     (equal? (library-name->identifiers libname)
						     (library-name-identifiers lib)))))


;;;; finding libraries, either interned or from external repositories

(module (find-library-by-name
	 find-library-by-reference
	 find-library-by-descriptor
	 external-pending-libraries)

  (define* (find-library-by-name {libname library-name?})
    ;;Given a R6RS library name: try  to search and intern the corresponding library,
    ;;if it is not already interned; when successful return the corresponding LIBRARY
    ;;object, otherwise raise an exception.
    ;;
    ;;First search for the library in the  internal collection then, if not found, in
    ;;an  external  libraries  repository  using  the  procedure  referenced  by  the
    ;;parameter CURRENT-LIBRARY-LOADER.
    ;;
    (or (find-library-in-collection-by-name libname)
	(%find-and-intern-external-library libname)))

  (define* (find-library-by-reference {libref library-reference?})
    ;;Given a  R6RS library  reference: try  to search  and intern  the corresponding
    ;;library,  if   it  is  not   already  interned;  when  successful   return  the
    ;;corresponding LIBRARY object, otherwise raise an exception.
    ;;
    ;;First search for the library in the  internal collection then, if not found, in
    ;;an  external  libraries  repository  using  the  procedure  referenced  by  the
    ;;parameter CURRENT-LIBRARY-LOADER.
    ;;
    (or (find-library-in-collection-by-reference libref)
	(%find-and-intern-external-library libref)))

  (define* (find-library-by-descriptor libdescr)
    ;;Given a  library descriptor, as  generated by the  function LIBRARY-DESCRIPTOR:
    ;;try  to search  and intern  the  corresponding library,  if it  is not  already
    ;;interned; when  successful return  the corresponding LIBRARY  object, otherwise
    ;;raise an exception.
    ;;
    ;;First search for the library in the  internal collection then, if not found, in
    ;;an  external  libraries  repository  using  the  procedure  referenced  by  the
    ;;parameter CURRENT-LIBRARY-LOADER.
    ;;
    (let ((lib (find-library-by-name (library-descriptor-name libdescr))))
      (if (eq? (library-descriptor-uid libdescr)
	       (library-uid            lib))
	  lib
	(error __who__ "unable to load required library" libdescr))))

  (module (%find-and-intern-external-library external-pending-libraries)

    (define* (%find-and-intern-external-library libref)
      ;;Given a  R6RS library reference  try to load  a conforming library  using the
      ;;current library loader.
      ;;
      (with-pending-library-request (__who__ libref)
	;;Load the library, either source or binary, and intern it.
	((current-library-loader) libref)
	;;Check if we actually succeeded.
	(or (find-library-in-collection-by-reference libref)
	    (error __who__
	      "cannot find library conforming to requested library reference"
	      libref))))

    (define external-pending-libraries
      ;;Hold a list of items representing  the libraries whose interning is currently
      ;;pending.  Each  item is the list  of R6RS library name  identifiers.  Used to
      ;;detect circular dependencies between libraries.
      ;;
      (make-parameter '()))

    (define (%pending-library-request? libref)
      (member (library-reference->identifiers libref)
	      (external-pending-libraries)))

    (define (%assert-not-pending-library-request who libref)
      (when (%pending-library-request? libref)
	(error who "circular attempt to import library was detected" libref)))

    (define-syntax (with-pending-library-request stx)
      (syntax-case stx ()
	((_ (?who ?libref) ?body0 ?body ...)
	 (identifier? #'?who)
	 #'(let ((libref ?libref))
	     (%assert-not-pending-library-request ?who libref)
	     (parametrise ((external-pending-libraries (cons libref (external-pending-libraries))))
	       ?body0 ?body ...)))
	))

    #| end of module |# )

  #| end of module |# )


;;;; parameters

(define current-library-expander
  ;;The current library  expander is used to expand LIBRARY  forms from source files.
  ;;The interface is as follows:
  ;;
  ;;   (receive (uid libname
  ;;             import-libdesc* visit-libdesc* invoke-libdesc*
  ;;             invoke-code visit-code
  ;;             export-subst export-env
  ;;             guard-code guard-libdesc*
  ;;             library-option*)
  ;;       ((current-library-expander) ?libsexp ?source-pathname ?verify-libname)
  ;;     ---)
  ;;
  ;;The argument ?LIBSEXP must be the symbolic expression:
  ;;
  ;;   (library . _)
  ;;
  ;;or an ANNOTATION  struct representing such expression.   This symbolic expression
  ;;is usually produced by the reader.
  ;;
  ;;The optional ?SOURCE-PATHNAME must be #f or a string representing the source file
  ;;from which the library was loaded; it is used for information purposes.
  ;;
  ;;The  optional argument  ?VERIFY-LIBNAME  must  be a  procedure  accepting a  R6RS
  ;;library name as argument; it is meant to perform some validation upon the library
  ;;name components (especially  the version) and raise an exception  if something is
  ;;wrong; otherwise it should just return.
  ;;
  (make-parameter
      (lambda (library-sexp)
        (assertion-violation 'current-library-expander "not initialised"))
    (lambda* ({obj procedure?})
      obj)))

(define current-library-loader
  ;;Reference a  function used to  load a library, either  source or binary,  given a
  ;;R6RS library reference; the referenced function must load the library and all its
  ;;dependencies in the internal collection.  The parameter is used as follows:
  ;;
  ;;   ((current-libary-loader) ?libref)
  ;;
  ;;The referenced function is meant to be  called after we have checked the internal
  ;;collection  of interned  libraries  and we  found that  no  library matching  the
  ;;library reference is loaded.
  ;;
  ;;The referenced function is allowed to return unspecified values.
  ;;
  (make-parameter
      (lambda (library-sexp)
        (assertion-violation 'current-library-loader "not initialised"))
    (lambda* ({obj procedure?})
      obj)))

(define current-include-loader
  ;;Hold a function used to load an  include file.  The referenced function is called
  ;;as follows:
  ;;
  ;;   (include-loader ?include-pathname ?verbose ?synner)
  ;;
  ;;where: ?INCLUDE-PATHNAME  must be a  string representing an absolute  or relative
  ;;pathname; ?VERBOSE can be any value; ?SYNNER must be a procedure used to raise an
  ;;exception when an error occurs:
  ;;
  ;;   (?synner ?message-string ?irritants)
  ;;
  ;;When  successful, the  referenced function  must  return 2  values: the  absolute
  ;;pathname from which  the file was loaded, a symbolic  expression representing the
  ;;file contents  (usually such  expression is  generated by  the reader).   When an
  ;;error occurs: call the procedure SYNNER, which is meant to raise an exception.
  ;;
  ;;If ?VERBOSE is  non-false: the referenced function must  display verbose messages
  ;;on the current error port describing the including process.
  ;;
  (make-parameter
      (lambda (filename verbose? synner)
	(assertion-violation 'current-include-loader "parameter not set"))
    (lambda* ({obj procedure?})
      obj)))

(define source-code-location
  ;;This parameter is  used to expand the identifier syntax  "__file__".  It is meant
  ;;to be  set to a string  representing the source  of the code being  expanded; for
  ;;example the source file name.
  ;;
  (make-parameter
      "<unknown-source-location>"
    (lambda* ({obj string?})
      obj)))


;;;; interning libraries

(case-define interned-libraries
  ;;Return the list of LIBRARY objects  currently interned.  If ALL?  is true: return
  ;;all the interned libraries, else return only the visible ones.
  ;;
  (()
   (interned-libraries #f))
  ((all?)
   ;;We want to return a newly allocated list.
   (fold-right (lambda (lib knil)
		 (if (or all? (library-visible? lib))
		     (cons lib knil)
		   knil))
     '()
     ((current-library-collection)))))

(module (intern-library)
  ;;Build a  LIBRARY object and  intern it in  the internal collection  of libraries;
  ;;return unspecified values.  The arguments are:
  ;;
  ;;UID -
  ;;   A gensym uniquely identifying this library.
  ;;
  ;;NAME -
  ;;   A R6RS library name.
  ;;
  ;;IMPORT-LIBDESC* -
  ;;    A list  of library  descriptors enumerating  the libraries  specified in  the
  ;;   IMPORT clauses.
  ;;
  ;;VISIT-LIBDESC* -
  ;;   A  list of library descriptors  enumerating the libraries needed  by the visit
  ;;   code.
  ;;
  ;;INVOKE-LIBDESC* -
  ;;   A  list of library descriptors  enmerating the libraries needed  by the invoke
  ;;   code.
  ;;
  ;;EXPORT-SUBST -
  ;;   A subst selecting the bindings to export from the EXPORT-ENV.
  ;;
  ;;EXPORT-ENV -
  ;;   The list of top-level bindings defined  by the library body.  Some of them are
  ;;   to be exported, others are not.
  ;;
  ;;VISIT-PROC -
  ;;   A thunk to evaluate to visit the library.
  ;;
  ;;INVOKE-PROC -
  ;;   A thunk to evaluate to invoke the library.
  ;;
  ;;VISIT-CODE -
  ;;   When this argument is created from  source code: this field is a core language
  ;;    symbolic expression  representing  the  visit code.   When  this argument  is
  ;;   created  from a compiled  library: this  field is a  thunk to be  evaluated to
  ;;   visit the library.
  ;;
  ;;INVOKE-CODE -
  ;;   When this argument is created from  source code: this field is a core language
  ;;    symbolic expression  representing the  invoke  code.  When  this argument  is
  ;;   created from a binary library: this field is a thunk to be evaluated to invoke
  ;;   the library.
  ;;
  ;;GUARD-CODE -
  ;;   When this argument is created from  source code: this field is a core language
  ;;    symbolic expression  representing  the  guard code.   When  this argument  is
  ;;   created from  a binary library: this field  is a thunk to be  evaluated to run
  ;;   the STALE-WHEN composite test expression.
  ;;
  ;;GUARD-LIBDESC* -
  ;;   A list of library descriptors enmerating the libraries needed by the composite
  ;;   STALE-WHEN test expression.
  ;;
  ;;VISIBLE? -
  ;;    A boolean,  true  if this  library  is to  be made  visible  to the  function
  ;;   INTERNED-LIBRARIES.
  ;;
  ;;SOURCE-FILE-NAME -
  ;;   False or a string representing the source file name.
  ;;
  ;;LIBRARY-OPTION* -
  ;;   A list of sexps representing library options.
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'intern-library))

  (define* (intern-library {uid symbol?} {libname library-name?}
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
      (when (find-library-in-collection-by-name libname)
	(assertion-violation __module_who__ "library is already interned" libname))
      (let ((lib (make-library uid libname import-lib* visit-lib* invoke-lib*
			       export-subst export-env visit-proc invoke-proc
			       visit-code invoke-code guard-code guard-lib*
			       visible? source-file-name library-option*)))
	(%intern-library-object lib)
	(when (memq 'visit-upon-loading library-option*)
	  (visit-library lib)))))

  (define (%intern-library-object lib)
    (for-each
	(lambda (export-env-entry)
	  ;;See the comments  in the expander code for the  format of the EXPORT-ENV.
	  ;;Entries in the EXPORT-ENV are different  from entries in the LEXENV; here
	  ;;we transform an EXPORT-ENV binding into a LEXENV binding descriptor.
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
			      (assertion-violation __module_who__
				"invalid syntactic binding descriptor type in EXPORT-ENV entry"
				lib export-env-entry)))))
	    ;;When the library is serialised: the content of the label's "value" slot
	    ;;is not  saved, so  we have  to set it  here every  time the  library is
	    ;;loaded.
	    (set-label-binding! label binding1)))
      ;;This expression returns the EXPORT-ENV of the library LIB.
      (library-export-env lib))
    ;;Register the object in the collection of interned libraries.
    ((current-library-collection) lib))

  #| end of module: INTERN-LIBRARY |# )

(define (intern-binary-library-and-its-dependencies
	 uid libname
	 import-libdesc* visit-libdesc* invoke-libdesc*
	 export-subst export-env
	 visit-proc invoke-proc guard-proc
	 guard-libdesc* visible? library-option* source-filename)
  ;;Whenever we load a serialied compiled library from a binary file we read the file
  ;;decoding  whatever  format  we have  used;  the  result  is  a tuple  of  objects
  ;;representing the compiled library.
  ;;
  ;;This function interns the binary library  in the internal collection after having
  ;;loaded  and interned  all its  dependency  libraries.  When  successful return  a
  ;;symbolic expression representing the R6RS library name; otherwise return #f.
  ;;
  ;;Dependency libraries are interned with FIND-LIBRARY-BY-NAME, which does the right
  ;;thing if the libraries are already interned.
  ;;
  (let loop ((libdesc* (append import-libdesc* visit-libdesc* invoke-libdesc* guard-libdesc*)))
    (cond ((pair? libdesc*)
	   ;;For every  library descriptor  in the list  of dependencies:  search the
	   ;;library, load it if needed and intern it.
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
		 #f))))
	  (else
	   ;;Invoke  all  the  guard  libraries  so we  can  evaluate  the  composite
	   ;;STALE-WHEN test expression.
	   (for-each (lambda (guard-libdesc)
		       (invoke-library (find-library-by-name (library-descriptor-name guard-libdesc))))
	     guard-libdesc*)
	   ;;Evaluate   the   composite   STALE-WHEN  test   expression   and   react
	   ;;appropriately.
	   (if (guard-proc)
	       ;;The compiled library is stale: print a message to warn the user then
	       ;;return false.
	       (begin
		 (library-stale-warning libname source-filename)
		 #f)
	     ;;The compiled library is fine: intern it and return true.
	     (let ((visit-code        #f)
		   (invoke-code       #f)
		   (guard-code        (quote (quote #f)))
		   (guard-libdesc*    '())
		   (source-file-name  #f))
	       (intern-library uid libname
				import-libdesc* visit-libdesc* invoke-libdesc*
				export-subst export-env
				visit-proc invoke-proc
				visit-code invoke-code
				guard-code guard-libdesc*
				visible? source-file-name library-option*)
	       libname))))))


;;;; uninterning libraries

(case-define* unintern-library
  ;;Libraries from the collection of interned  libraries can be uninterned, either to
  ;;free  system resources  or  to  allow reinterning  from  new  files.  Unintern  a
  ;;library.  Return unspecified values.
  ;;
  ;;FIXME The implementation  of this function is incomplete.  (Marco  Maggi; Wed Dec
  ;;24, 2014)
  ;;
  ((libname)
   (unintern-library libname #t))
  (({libname library-name?} err?)
   ;;FIXME: check that no other import is in progress.  (Ghuloum)
   (cond ((find-library-in-collection-by-reference libname)
	  => (lambda (lib)
	       (%remove-library-from-current-collection lib)
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
	    (assertion-violation __who__ "library not uninterned" libname))))))


;;;; utilities for the expansion process

(define (imported-label->syntactic-binding lab)
  ;;If a label gensym is associated to  a binding from the the boot image environment
  ;;or to a binding from a library's  EXPORT-ENV: it has the associated descriptor in
  ;;its "value" field; otherwise such field is set to #f.
  ;;
  ;;So, if we have a label, we can  check if it references an imported binding simply
  ;;by  checking its  "value" field;  this is  what IMPORTED-LABEL->SYNTACTIC-BINDING
  ;;does.   If LAB  is a  label genysm  referencing an  imported binding:  return the
  ;;associated syntactic binding descriptor; otherwise return false.
  ;;
  (label-binding lab))

(define* (invoke-library {lib library?})
  ;;Evaluate the invoke code.
  ;;
  (let ((invoke (library-invoke-state lib)))
    (when (procedure? invoke)
      (set-library-invoke-state! lib (lambda ()
				       (assertion-violation __who__ "circularity detected" lib)))
      (for-each invoke-library (library-inv-lib* lib))
      (set-library-invoke-state! lib (lambda ()
				       (assertion-violation __who__ "first invoke did not return" lib)))
      (library-debug-message "invoking: ~a" (library-name lib))
      (invoke)
      (set-library-invoke-state! lib #t))))

(define* (visit-library {lib library?})
  ;;Evaluate the visit code.
  ;;
  (let ((visit (library-visit-state lib)))
    (when (procedure? visit)
      (set-library-visit-state! lib (lambda ()
				      (assertion-violation __who__ "circularity detected" lib)))
      (for-each invoke-library (library-vis-lib* lib))
      (set-library-visit-state! lib (lambda ()
				      (assertion-violation __who__ "first visit did not return" lib)))
      (library-debug-message "visiting: ~a" (library-name lib))
      (visit)
      (set-library-visit-state! lib #t))))


;;;; done

;; #!vicare
;; (define dummy
;;   (let ()
;;     (import (vicare))
;;     (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.library-manager"))))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'with-pending-library-request 'scheme-indent-function 1)
;; End:
