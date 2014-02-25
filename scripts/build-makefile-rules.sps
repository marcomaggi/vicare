;; build-makefile-rules.sps --
;;
;;FIXME  We are  not tracking  the  include files  among the  libraries'
;;precedences,  but we  should.  At  present the  include files  are not
;;collected by the  expander, but maybe they should.   (Marco Maggi; Tue
;;Feb 25, 2014)
;;
#!r6rs
(import (vicare)
  (prefix (vicare libraries) libs.)
  (prefix (vicare language-extensions posix) px.))


;;;; string pathname helpers

(module (strip-source-file-prefix
	 strip-lib-prefix)

  (define* (strip-source-file-prefix (pathname string?))
    ;;Given  a source  file  pathname:  if it  starts  with "../lib"  or
    ;;"./lib"  or a  directory in  the  source search  path: strip  such
    ;;prefix and return the result; otherwise return PATHNAME itself.
    ;;
    (define pathname^
      (cond ((%string-prefix? "../lib" pathname)
	     (%deprefix "../lib" pathname))
	    ((%string-prefix? "./lib" pathname)
	     (%deprefix "../lib" pathname))
	    (else
	     (let loop ((dirs (libs.library-path)))
	       (cond ((null? dirs)
		      pathname)
		     ((%string-prefix? (car dirs) pathname)
		      (%deprefix (car dirs) pathname))
		     (else
		      (loop (cdr dirs))))))))
    ;;Strip all the leading slashes.
    (let loop ((ptn pathname^))
      (if (char=? #\/ (string-ref ptn 0))
	  (loop (substring ptn 1 (string-length ptn)))
	ptn)))

  (define* (strip-lib-prefix (pathname string?))
    (cond ((%string-prefix? "lib/" pathname)
	   (%deprefix "lib/" pathname))
	  ((string=? "lib" pathname)
	   "")
	  (else
	   pathname)))

  (define (%string-prefix? prefix str)
    (let ((prefix.len (string-length prefix)))
      (and (fx<= prefix.len (string-length str))
	   (string=? prefix (substring str 0 prefix.len)))))

  (define (%deprefix prefix str)
    (substring str (string-length prefix) (string-length str)))

  #| end of module |# )


;;;; the meat

(module (%process-library-reference)

  (define-constant ALREADY-PROCESSED-TABLE
    ;;After processing a library reference: we store its source pathname
    ;;here.   This  way we  can  safely  recursively visit  the  library
    ;;dependencies and  output stuff  for them.  This  allows us  not to
    ;;list  all  the  libraries  in   the  library  specs,  because  the
    ;;precedences are included automatically.
    (make-hashtable string-hash string=?))

  (define* (%process-library-reference (libref libs.library-reference?))
    (define lib
      (receive-and-return (lib)
	  (libs.find-library-by-name libref)
	(unless lib
	  (error __who__ "cannot find library record for library reference" libref))))
    (define dependencies-list
      ;;False  or  a list  of  library  names:  the  first name  is  the
      ;;target's, the  rest is  the list of  libnames of  libraries upon
      ;;which the target depends.
      (%library-build-dependency-list lib))
    (when dependencies-list
      (let ((source-pathname (string-append "lib/" (strip-source-file-prefix (libs.library-source-file-name lib))))
	    ;;Remember that the stem+extension starts with a slash!!!
	    (binary-pathname (string-append "lib" (libs.fasl-stem+extension libref))))
	(unless (hashtable-ref ALREADY-PROCESSED-TABLE source-pathname #f)
	  (fprintf stderr "processing: ~a\n" source-pathname)
	  (%build-recipe dependencies-list binary-pathname source-pathname)
	  (%build-installation-stuff binary-pathname source-pathname)
	  (hashtable-set! ALREADY-PROCESSED-TABLE source-pathname #t)
	  (for-each-in-order
	      %process-library-reference
	    (cdr dependencies-list))))))

  (define* (%library-build-dependency-list (lib libs.library?))
    ;;Return  a list  of  R6RS  library names;  the  first  name is  the
    ;;target's, the rest is the list  of library names of libraries upon
    ;;which the  target depends.   Only libraries  having a  source file
    ;;field are included.
    (and (libs.library-source-file-name lib)
	 (cons (libs.library-name lib)
	       (fold-left (lambda (knil dep-lib)
			    (if (libs.library-source-file-name dep-lib)
				(cons (libs.library-name dep-lib)
				      knil)
			      knil))
		 '()
		 (libs.library-imp-lib* lib)))))

  (define (%build-recipe dependencies-list binary-pathname source-pathname)
    (print-recipe "~a: \\\n\t\t~a" binary-pathname source-pathname)
    (unless (null? (cdr dependencies-list))
      (for-each (lambda (libname)
		  (print-recipe " \\\n\t\tlib~a" (libs.fasl-stem+extension libname)))
	(cdr dependencies-list)))
    (print-recipe " \\\n\t\t$(VICARE_NEW_BOOT)\n")
    (print-recipe "\t$(VICARE_COMPILE_RUN) --output $@ --compile-library $<\n\n"))

  (define (%build-installation-stuff binary-pathname source-pathname)
    (let ((stem (%string-replace-nasty-chars '(#\/ #\- #\% #\.) #\_ binary-pathname)))
      (when (conditionals)
	(for-each-in-order
	    (lambda (conditional)
	      (print-recipe "if ~a\n" conditional))
	  (conditionals)))
      (receive (dir file)
	  (px.split-pathname-root-and-tail binary-pathname)
	(print-recipe "~adir = $(bundledlibsdir)/~a\n" stem
		      (strip-lib-prefix dir)))
      (print-recipe "nodist_~a_DATA = ~a\n" stem binary-pathname)
      (unless (or (hashtable-ref FROM-TEMPLATES-SOURCE-FILES-TABLE source-pathname #f)
		  (hashtable-ref BUILT-SOURCE-FILES-TABLE          source-pathname #f))
	(print-recipe "EXTRA_DIST += ~a\n" source-pathname))
      (print-recipe "CLEANFILES += ~a\n" binary-pathname)
      (when (conditionals)
	(for-each-in-order
	    (lambda (conditional)
	      (print-recipe "endif\n"))
	  (conditionals)))
      (print-recipe "\n")))

  (define (%string-replace-nasty-chars chs ch.repl str1)
    (receive-and-return (str2)
	(make-string (string-length str1))
      (do ((i 0 (fxadd1 i)))
	  ((fx= i (string-length str1)))
	(let ((ch (string-ref str1 i)))
	  (if (memv ch chs)
	      (string-set! str2 i ch.repl)
	    (string-set! str2 i ch))))))

  #| end of module |# )


;;;; libraries specifications

(define-constant FROM-TEMPLATES-SOURCE-FILES-TABLE
  ;;Here we store the pathnames of source library files that are created
  ;;by  running "configure"  and processing  ".sls.in" templates.   Such
  ;;source fiels must *not* be included in the EXTRA_DIST list of files.
  ;;
  (receive-and-return (T)
      (make-hashtable string-hash string=?)
    (for-each
	(lambda (source-pathname)
	  (hashtable-set! T source-pathname #t))
      FROM-TEMPLATES-SOURCE-FILES)))

(define-constant BUILT-SOURCE-FILES-TABLE
  ;;Here we store the pathnames of source library files that are created
  ;;by  some  makefile rule;  such  files  are  listed in  the  makefile
  ;;BUILT_SOURCES variable.  Such source fiels must *not* be included in
  ;;the EXTRA_DIST list of files.
  ;;
  (receive-and-return (T)
      (make-hashtable string-hash string=?)
    (for-each
	(lambda (source-pathname)
	  (hashtable-set! T source-pathname #t))
      BUILT-SOURCE-FILES)))


;;;; go!!!

(define conditionals
  (make-parameter #f))

(define build-recipes-port
  (make-parameter #f))

(define (print-recipe template . args)
  (apply fprintf (build-recipes-port) template args))

(define (main)
  ;;We want to load source files only.
  (libs.current-library-locator libs.source-library-locator)
  (libs.fasl-search-path '())

  (fprintf stdout "## dependencies.make --\n#\n# Automatically built.\n\n")

  (fprintf stdout "EXTRA_DIST += ")
  (vector-for-each
      (lambda (source-pathname)
	(fprintf stdout " \\\n\t~a.in" source-pathname))
    (hashtable-keys FROM-TEMPLATES-SOURCE-FILES-TABLE))
  (fprintf stdout "\n\n")

  (receive (port get-build-recipes)
      (open-string-output-port)
    (for-each-in-order
	(lambda (spec-entry)
	  (parametrise ((conditionals       (car spec-entry))
			(build-recipes-port port))
	    (let ((library-names (cdr spec-entry)))
	      (for-each-in-order %process-library-reference library-names))))
      LIBRARIES-SPECS)
    (display (get-build-recipes) stdout)
    (flush-output-port stdout))

  (fprintf stdout "\n### end of file\n# Local Variables:\n# mode: makefile-automake\n# End:\n")
  (flush-output-port stdout))

(module (FROM-TEMPLATES-SOURCE-FILES
	 BUILT-SOURCE-FILES
	 LIBRARIES-SPECS)
  (include "libraries.scm"))

(main)

;;; end of file
