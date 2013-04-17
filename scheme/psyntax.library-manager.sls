;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
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
    library-spec		library-name
    library-version		library-subst
    imported-label->binding

    ;; library installation
    install-library		uninstall-library
    installed-libraries

    ;; library operations
    visit-library		invoke-library
    serialize-all

    ;; finding libraries
    find-library-by-name	library-exists?
    library-path		library-extensions

    current-library-expander
    current-library-collection
    current-precompiled-library-loader)
  (import (rnrs)
    (psyntax compat)
    (vicare syntactic-extensions)
    (vicare arguments validation))


;;;; arguments validation

(define-argument-validation (list-of-strings who obj)
  (and (list? obj) (for-all string? obj))
  (assertion-violation who "expected list of strings as argument" obj))


;;;; public configuration parameters

(define library-path
  ;;Hold a  list of strings  representing directory pathnames  being the
  ;;search path.
  ;;
  (make-parameter
      '(".")
    (lambda (obj)
      (define who 'library-path)
      (with-arguments-validation (who)
	  ((list-of-strings	obj))
	obj))))

(define library-extensions
  ;;Hold a  list of strings  representing file name  extensions, leading
  ;;dot included.
  ;;
  (make-parameter
      '(".vicare.sls" ".sls")
    (lambda (obj)
      (define who 'library-extensions)
      (with-arguments-validation (who)
	  ((list-of-strings	obj))
	obj))))


;;;; type definitions

(define-record library
  (id
		;A gensym uniquely identifying this library.
   name
		;Non-empty list of  symbols representing the identifiers
		;from the library name.
   version
		;Null  or  a list  of  exact  integers representing  the
		;version number from the library name.
   imp*
   vis*
   inv*
   subst
   env
   visit-state
   invoke-state
   visit-code
   invoke-code
   guard-code
   guard-req*
   visible?
		;A boolean determining if the library is visible.
   source-file-name
		;False or a string representing the pathname of the file
		;from which the source code of the library was read.
   )
  (lambda (S port sub-printer)
    ;;Printer function.
    ;;
    (define-inline (%display thing)
      (display thing port))
    (define-inline (%write thing)
      (write thing port))
    (%display "#<library")
    (%display (if (null? ($library-version S))
		  ($library-name S)
		(append ($library-name S)
			(list ($library-version S)))))
    (%display ">")))

(define-argument-validation (library who obj)
  (library? obj)
  (assertion-violation who "expected instance of library struct as argument" obj))


;;;; collection of already loaded libraries

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
    ;;Hold a collection of LIBRARY structs.
    ;;
    (make-parameter (make-collection)
      (lambda (x)
	(define who 'current-library-collection)
	(with-arguments-validation (who)
	    ((procedure	x))
	  x))))

  #| end of module: CURRENT-LIBRARY-COLLECTION |# )


;;;; finding source libraries on the file system
;;
;;The library  file locator is a  function that converts a  library name
;;specification into the corresponding file pathname.
;;
(module (library-file-locator)

  (define (%default-library-file-locator libname)
    ;;Default  value for  the LIBRARY-FILE-LOCATOR  parameter.  Given  a
    ;;library name, scan  the library search path  for the corresponding
    ;;file; return  a string representing  the file pathname  having the
    ;;directory  part  equal  to  one  of  the  directory  pathnames  in
    ;;LIBRARY-PATH.
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    ;;If a  matching file is not  found call a function  from the compat
    ;;library: FILE-LOCATOR-RESOLUTION-ERROR.
    ;;
    (let loop ((rootname-str     (%library-identifiers->file-name libname))
	       (directories      (library-path))
	       (file-extensions  (library-extensions))
	       (failed-list      '()))
      (cond ((null? directories)
	     ;;No suitable library was found.
	     (file-locator-resolution-error libname (reverse failed-list)
					    (let ((ls (%external-pending-libraries)))
					      (if (null? ls)
						  (error 'library-manager "BUG")
						(cdr ls)))))
	    ((null? file-extensions)
	     ;;No more extensions: try the  next directory in the search
	     ;;path.
	     (loop rootname-str (cdr directories) (library-extensions) failed-list))
	    (else
	     ;;Check the  file existence  in the current  directory with
	     ;;the current  file extension;  if not  found try  the next
	     ;;file extension.
	     (let ((name (string-append (car directories) rootname-str (car file-extensions))))
	       (if (file-exists? name)
		   name
		 (loop rootname-str directories
		       (cdr file-extensions) (cons name failed-list))))))))

  (define (%library-identifiers->file-name library-name.ids)
    ;;Convert the non-empty list of identifiers from a library name into
    ;;a string  representing the  corresponding relative  file pathname,
    ;;without  extension   but  including   a  leading   #\/  character.
    ;;Examples:
    ;;
    ;;	(%library-identifiers->file-name '(alpha beta gamma))
    ;;	=> "/alpha/beta/gamma"
    ;;
    ;;	(%library-identifiers->file-name '(alpha beta main))
    ;;	=> "/alpha/beta/main_"
    ;;
    ;;notice how the component "main",  when appearing last, is "quoted"
    ;;by appending an underscore.
    ;;
    (assert (not (null? library-name.ids)))
    (let-values (((port extract) (open-string-output-port)))
      (define (display-hex n)
	(if (<= 0 n 9)
	    (display n port)
	  (write-char (integer->char (+ (char->integer #\a) (- n 10))) port)))
      (define (main*? component-name)
	(and (>= (string-length component-name) 4)
	     (string=? (substring component-name 0 4) "main")
	     (for-all (lambda (ch)
			(char=? ch #\_))
	       (string->list (substring component-name 4 (string-length component-name))))))
      (let next-component ((component		(car library-name.ids))
			   (ls			(cdr library-name.ids))
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
			    (display-hex D)
			    (display-hex M)))))
	    (bytevector->u8-list (string->utf8 component-name)))
	  (if (null? ls)
	      (when (and (not first-component?)
			 (main*? component-name))
		(write-char #\_ port))
	    (next-component (car ls) (cdr ls) #f))))
      (extract)))

  (define library-file-locator
    ;;Hold a  function used to  convert a library name  specification into
    ;;the corresponding file pathname.
    ;;
    (make-parameter
	%default-library-file-locator
      (lambda (obj)
	(define who 'library-file-locator)
	(with-arguments-validation (who)
	    ((procedure	obj))
	  obj))))

  #| end of module: LIBRARY-FILE-LOCATOR |# )


;;;; loading precompiled libraries from files

(define current-precompiled-library-loader
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
  ;;%INSTALL-LIBRARY-AND-DEPS.
  ;;
  (make-parameter
      (lambda (file-name success-kont)
	#f)
    (lambda (obj)
      (define who 'current-precompiled-library-loader)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))

(define (%install-library-and-deps filename id name ver imp* vis* inv*
				   exp-subst exp-env
				   visit-proc invoke-proc guard-proc
				   guard-req* visible?)
  ;;Used  as  success  continuation  function by  the  function  in  the
  ;;parameter   CURRENT-PRECOMPILED-LIBRARY-LOADER.    Make   sure   all
  ;;dependencies  are met,  then install  the library  and return  true;
  ;;otherwise return #f.
  ;;
  (let f ((deps (append imp* vis* inv* guard-req*)))
    (cond ((null? deps)
	   ;; CHECK
	   (for-each (lambda (x)
		       (let* ((label (car x))
			      (dname (cadr x))
			      (lib   (find-library-by-name dname)))
			 (invoke-library lib)))
	     guard-req*)
	   (cond ((guard-proc) ;;; stale
		  (library-stale-warning name filename)
		  #f)
		 (else
		  (install-library id name ver imp* vis* inv*
				   exp-subst exp-env visit-proc invoke-proc
				   #f #f ''#f '() visible? #f)
		  #t)))
	  (else
	   (let* ((d		(car deps))
		  (label	(car d))
		  (dname	(cadr d))
		  (l		(find-library-by-name dname)))
	     (if (and (library? l) (eq? label (library-id l)))
		 (f (cdr deps))
	       (begin
		 (library-version-mismatch-warning name dname filename)
		 #f)))))))


;;;; loading libraries from files
;;
;;The  library  loader  is  a  function that  loads  a  library,  either
;;precompiled or from source.
;;
(module (library-loader)

  (define (%default-library-loader requested-libname)
    ;;Default value  for the parameter LIBRARY-LOADER.   Given a library
    ;;name  specification:  search  the  associated  file  pathname  and
    ;;attempt to load  the file.  Try first to load  a precompiled file,
    ;;if any, then try to load the source file.
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    (define who '%default-library-loader)
    (let ((filename ((library-file-locator) requested-libname)))
      (cond ((not filename)
	     (assertion-violation who "cannot find library" requested-libname))
	    ;;If the  precompiled library  loader returns false:  try to
	    ;;load the source file.
	    (((current-precompiled-library-loader)
	      filename %install-library-and-deps))
	    (else
	     ((current-library-expander)
	      ;;Return  a symbolic  expression representing  the LIBRARY
	      ;;form, or raise an exception.
	      (read-library-source-file filename)
	      filename
	      (lambda (library-name.ids library-name.version)
		(%verify-library requested-libname filename
				 library-name.ids library-name.version)))))))

  (define (%verify-library requested-libname filename
			   found-library-name.ids found-library-name.version)
    ;;Verify  the  name  of  loaded  library against  the  name  of  the
    ;;requested library.
    ;;
    ;;FOUND-LIBRARY-NAME.IDS  is the  list of  symbols from  the library
    ;;name.   FOUND-LIBRARY-NAME.VERSION is  null or  the list  of exact
    ;;integers representing the library version.
    ;;
    (define who '%default-library-loader)
    (unless (equal? found-library-name.ids requested-libname)
      (assertion-violation who
	(let-values (((port extract) (open-string-output-port)))
	  (display "expected to find library " port)
	  (write requested-libname port)
	  (display " in file " port)
	  (display filename port)
	  (display ", found " port)
	  (write found-library-name.ids port)
	  (display " instead" port)
	  (extract)))))

  (define library-loader
    ;;Hold a function used to load a library, either precompiled or from
    ;;source.
    ;;
    (make-parameter
	%default-library-loader
      (lambda (f)
	(define who 'library-loader)
	(with-arguments-validation (who)
	    ((procedure	f))
	  f))))

  #| end of module: LIBRARY-LOADER |# )


;;;; finding libraries, already loaded or not

(define (library-exists? libname)
  ;;Given a library name search  the corresponding LIBRARY record in the
  ;;collection of already loaded libraries: return true or false.
  ;;
  ;;For this function, a "library name" is a list of symbols without the
  ;;version specification.
  ;;
  (and (%find-library-in-collection-by (lambda (x)
					 (equal? (library-name x) libname)))
       #t))

(module (find-library-by-name)

  (define (find-library-by-name libname)
    ;;Given  a library  name  return the  corresponding LIBRARY  record.
    ;;Search  for the  library in  the  internal collection  or, if  not
    ;;found, in the external source (for example the file system).
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    (or (%find-library-in-collection-by (lambda (x)
					  (equal? (library-name x) libname)))
	(%find-external-library libname)))

  (define (%find-external-library libname)
    ;;Given a  library name  try to  load it  using the  current library
    ;;loader.
    ;;
    ;;For this function,  a "library name" is a list  of symbols without
    ;;the version specification.
    ;;
    (define who '%find-external-library)
    (when (member libname (%external-pending-libraries))
      (assertion-violation who "circular attempt to import library was detected" libname))
    (parametrise ((%external-pending-libraries (cons libname (%external-pending-libraries))))
      ((library-loader) libname)
      (or (%find-library-in-collection-by (lambda (x)
					    (equal? (library-name x) libname)))
	  (assertion-violation who
	    "handling external library did not yield the correct library" libname))))

  #| end of module: FIND-LIBRARY-BY-NAME |# )

(define (%find-library-in-collection-by pred)
  ;;Visit the current library collection  and return the first for which
  ;;PRED returns true.  If PRED returns false for all the entries in the
  ;;collection: return false.
  ;;
  (let next-library-struct ((ls ((current-library-collection))))
    (cond ((null? ls)
	   #f)
	  ((pred (car ls))
	   (car ls))
	  (else
	   (next-library-struct (cdr ls))))))

(define (%find-library-in-collection-by-spec/die spec)
  ;;Given a  library specification, being a  list whose car  is a unique
  ;;symbol associated  to the library, return  the corresponding LIBRARY
  ;;record or raise an assertion.
  ;;
  (let ((id (car spec)))
    (or (%find-library-in-collection-by (lambda (x)
					 (eq? id (library-id x))))
	(assertion-violation #f "cannot find library with required spec" spec))))

(define %external-pending-libraries
  ;;Used to detect circular dependencies between libraries.
  ;;
  (make-parameter '()))


(define current-library-expander
  (make-parameter
      (lambda (x)
        (assertion-violation 'library-expander "not initialized"))
    (lambda (obj)
      (define who 'current-library-expander)
      (with-arguments-validation (who)
	  ((procedure	obj))
	obj))))

(module (serialize-all)

  (define (serialize-all serialize compile)
    ;;Traverse  the current  collection of  libraries and  serialize the
    ;;contents  of all  the LIBRARY  records having  a source  file (the
    ;;records that  do not  have a source  file represent  the libraries
    ;;built in the boot image).
    ;;
    ;;"Serializing" means to write in a FASL file.
    ;;
    ;;The   argument   SERIALIZE   should    be   a   closure   wrapping
    ;;DO-SERIALIZE-LIBRARY.
    ;;
    ;;The argument  COMPILE should  be a  closure wrapping  the function
    ;;COMPILE-CORE-EXPR.
    ;;
    (for-each (lambda (lib)
		(%serialize-library lib serialize compile))
      ((current-library-collection))))

  (define (%serialize-library lib serialize compile)
    ;;Serialize the contents of a LIBRARY record.
    ;;
    (when ($library-source-file-name lib)
      (serialize ($library-source-file-name lib)
		 (list ($library-id lib)
		       ($library-name lib)
		       ($library-version lib)
		       (map library-desc ($library-imp* lib))
		       (map library-desc ($library-vis* lib))
		       (map library-desc ($library-inv* lib))
		       ($library-subst lib)
		       ($library-env lib)
		       (compile ($library-visit-code lib))
		       (compile ($library-invoke-code lib))
		       (compile ($library-guard-code lib))
		       (map library-desc ($library-guard-req* lib))
		       ($library-visible? lib)))))

  (define (library-desc lib)
    (list ($library-id   lib)
	  ($library-name lib)))

  #| end of module: SERIALIZE-ALL |# )


(define uninstall-library
  ;;Uninstall a library.
  ;;
  ;;THE IMPLEMENTATION OF THIS FUNCTION IS INCOMPLETE.
  ;;
  (case-lambda
   ((name)
    (uninstall-library name #t))
   ((name err?)
    (define who 'uninstall-library)
       ;;; FIXME: check that no other import is in progress
       ;;; FIXME: need to unintern labels and locations of
       ;;;        library bindings
    (let ((lib (%find-library-in-collection-by (lambda (x)
						 (equal? (library-name x) name)))))
      (when (and err? (not lib))
	(assertion-violation who "library not installed" name))
      ;;Remove LIB from the current collection.
      ((current-library-collection) lib #t)
      ;;Remove label gensyms from the internal table.
      (for-each (lambda (x)
		  (let ((label   (car x))
			(binding (cdr x)))
		    (remove-location label)
		    (when (memq (car binding) '(global global-macro global-macro! global-ctv))
		      (remove-location (cdr binding)))))
	(library-env lib)))
    (values))))


;;;; installing libraries

(define installed-libraries
  ;;Return a list  of LIBRARY structs being already  installed.  If ALL?
  ;;is true:  return all the  installed libraries, else return  only the
  ;;visible ones.
  ;;
  (case-lambda
   ((all?)
    (let next-library-struct ((ls ((current-library-collection))))
      (cond ((null? ls)
	     '())
	    ((or all? (library-visible? (car ls)))
	     (cons (car ls) (next-library-struct (cdr ls))))
	    (else
	     (next-library-struct (cdr ls))))))
   (()
    (installed-libraries #f))))

(module (install-library)

  (define (install-library id libname ver imp* vis* inv* exp-subst exp-env
			   visit-proc invoke-proc visit-code invoke-code
			   guard-code guard-req*
			   visible? source-file-name)
    (let ((imp-lib*	(map %find-library-in-collection-by-spec/die imp*))
	  (vis-lib*	(map %find-library-in-collection-by-spec/die vis*))
	  (inv-lib*	(map %find-library-in-collection-by-spec/die inv*))
	  (guard-lib*	(map %find-library-in-collection-by-spec/die guard-req*)))
      (unless (and (symbol? id) (list? libname) (list? ver))
	(assertion-violation 'install-library
	  "invalid spec with id/name/ver" id libname ver))
      (when (library-exists? libname)
	(assertion-violation 'install-library
	  "library is already installed" libname))
      (let ((lib (make-library id libname ver imp-lib* vis-lib* inv-lib*
			       exp-subst exp-env visit-proc invoke-proc
			       visit-code invoke-code guard-code guard-lib*
			       visible? source-file-name)))
	(%install-library-record lib))))

  (define (%install-library-record lib)
    (for-each
	(lambda (x)
	  (let* ((label    (car x))
		 (binding  (cdr x))
		 (binding1 (case (car binding)
			     ((global)        (cons* 'global        lib (cdr binding)))
			     ((global-macro)  (cons* 'global-macro  lib (cdr binding)))
			     ((global-macro!) (cons* 'global-macro! lib (cdr binding)))
			     ((global-ctv)    (cons* 'global-ctv    lib (cdr binding)))
			     (else            binding))))
	    (set-label-binding! label binding1)))
      (library-env lib))
    ((current-library-collection) lib))

  #| end of module: INSTALL-LIBRARY |# )


(define (imported-label->binding lab)
  (label-binding lab))

(define (invoke-library lib)
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

(define (library-spec lib)
  ;;Given a library record return a list holding: the unique library id,
  ;;the  list of  symbols from  the library  name, null  or the  list of
  ;;version numbers from the library name.
  ;;
  (define who 'library-spec)
  (with-arguments-validation (who)
      ((library		lib))
    (list ($library-id      lib)
	  ($library-name    lib)
	  ($library-version lib))))


;;;; done

)

;;; end of file
