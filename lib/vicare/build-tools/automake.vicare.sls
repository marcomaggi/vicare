;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: utilities for GNU Automake automated generation
;;;Date: Tue Feb 25, 2014
;;;
;;;Abstract
;;;
;;;	This is a very  rough library used to generate GNU  Automake rules for Vicare
;;;	Scheme librariers:  source to  FASL compilation, FASL  installation, optional
;;;	source  installation,   source  distribution.   We  should   see  the  script
;;;	"build-makefile-rules.sps" for how this library is used.
;;;
;;;       Scheme libraries  processed by  this library must  reside in  the directory
;;;     "$(top_srcdir)/lib"  of  the  distribution  package;  this  limitation  makes
;;;     pathname processing so much simpler...
;;;
;;;Copyright (C) 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare build-tools automake)
  (export
    build-library-dependencies
    include-library-build-hierarchies
    include-library-dependencies-hierarchies
    include-install-rules)
  (import (vicare)
    (prefix (vicare libraries) libs.)
    (prefix (vicare posix) px.))


;;;; configuration parameters

(define include-install-rules
  ;;If set to true: generate installation rules for all the libraries; otherwise skip
  ;;the install rules.
  ;;
  (make-parameter #t
    (lambda (obj)
      (and obj #t))))


(define include-library-build-hierarchies
  ;;List of library  hierarchy prefixes used to select which  libraries to include in
  ;;the generated  makefile compile rules.   If left to  null: all the  libraries are
  ;;included.
  ;;
  ;;To include only libraries under:
  ;;
  ;;   (vicare ---)
  ;;   (srfi ---)
  ;;   (nausicaa ---)
  ;;
  ;;we do:
  ;;
  ;;   (include-library-hierarchies '((vicare) (srfi) (nausicaa)))
  ;;
  (make-parameter '()
    (lambda (obj)
      (assert (and (list? obj)
		   (for-all (lambda (entry)
			      (for-all symbol? entry))
		     obj)))
      obj)))

(define include-library-dependencies-hierarchies
  ;;List of library  hierarchy prefixes used to select which  libraries to include in
  ;;the  dependencies lists  of  build makefile  rules.   If left  to  null: all  the
  ;;libraries are included.
  ;;
  ;;To include only libraries under:
  ;;
  ;;   (vicare ---)
  ;;   (srfi ---)
  ;;   (nausicaa ---)
  ;;
  ;;we do:
  ;;
  ;;   (include-library-hierarchies '((vicare) (srfi) (nausicaa)))
  ;;
  (make-parameter '()
    (lambda (obj)
      (assert (and (list? obj)
		   (for-all (lambda (entry)
			      (for-all symbol? entry))
		     obj)))
      obj)))




;;;; string pathname helpers

(module (strip-source-file-prefix
	 strip-lib-prefix)

  (define pwd
    (px.getcwd/string))

  (define pwd/lib
    (px.realpath/string (string-append pwd "/lib")))

  (define pwd/../lib
    (px.realpath/string (string-append pwd "/../lib")))

  (define* (strip-source-file-prefix {pathname string?})
    ;;Given  a source  file pathname:  if it  starts with  "../lib" or  "./lib" or  a
    ;;directory in the  source search path: strip such prefix  and return the result;
    ;;otherwise return PATHNAME itself.
    ;;
    (define pathname^
      (cond ((%string-prefix? "../lib" pathname)
	     (%deprefix "../lib" pathname))

	    ((%string-prefix? "./lib" pathname)
	     (%deprefix "../lib" pathname))

	    ((%string-prefix? "lib" pathname)
	     (%deprefix "lib" pathname))

	    ((%string-prefix? pwd/lib pathname)
	     (%deprefix pwd/lib pathname))

	    ((%string-prefix? pwd/../lib pathname)
	     (%deprefix pwd/../lib pathname))

	    ;;This must be the last among the ones starting with pwd.
	    ((%string-prefix? pwd pathname)
	     (%deprefix pwd pathname))

	    (else
	     (let loop ((dirs (libs.library-source-search-path)))
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

  (define* (strip-lib-prefix {pathname string?})
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


;;;; library name helpers

(module (%library-name-under-hierarchies?)

  (define (%library-name-under-hierarchies? libname hierarchy-prefix*)
    (or (null? hierarchy-prefix*)
	(exists (lambda (hierarchy-prefix)
		  (%library-name-under-hierarchy? libname hierarchy-prefix))
	  hierarchy-prefix*)))

  (define (%library-name-under-hierarchy? libname hierarchy-prefix)
    ;;Return true if  the R6RS library name LIBNAME is  under the hierarchy specified
    ;;by HIERARCHY-PREFIX, which must be a list of symbols; otherwise return false.
    ;;
    ;;   (%library-name-under-hierarchy? '(nausicaa) '(nausicaa))
    ;;   => #t
    ;;
    ;;   (%library-name-under-hierarchy? '(vicare platform errno.sls)
    ;;                                   '(vicare platform))
    ;;   => #t
    ;;
    ;;   (%library-name-under-hierarchy? '(libtest lists) '(vicare))
    ;;   => #f
    ;;
    (if (pair? hierarchy-prefix)
	(and (eq? (car libname) (car hierarchy-prefix))
	     (%library-name-under-hierarchy? (cdr libname) (cdr hierarchy-prefix)))
      #t))

  #| end of module |# )


;;;; the meat

(module (%process-library-reference)

  (define-constant ALREADY-PROCESSED-TABLE
    ;;After processing a library reference: we  store its source pathname here.  This
    ;;way we can safely, recursively visit  the library dependencies and output stuff
    ;;for them.  This allows  us not to list all the libraries  in the library specs,
    ;;because the precedences are included automatically.
    (make-hashtable string-hash string=?))

  (define* (%process-library-reference {libref libs.library-reference?})
    (define lib
      ;;This is a LIBRARY struct.
      ;;
      (receive-and-return (lib)
	  (libs.find-library-by-name libref)
	(unless lib
	  (error __who__ "cannot find library record for library reference" libref))))
    (define target/dependencies-list
      ;;False or a list of library names: the first name is the target's, the rest is
      ;;the list of libnames of libraries upon which the target depends.
      ;;
      (%library-build-dependency-list lib))
    (when target/dependencies-list
      (let ((source-pathname (string-append "lib/" (strip-source-file-prefix (libs.library-source-file-name lib))))
	    ;;Remember that the stem+extension starts with a slash!!!
	    (binary-pathname (libs.directory+library-stem->library-binary-pathname "lib" (libs.library-reference->filename-stem libref))))
	(unless (hashtable-ref ALREADY-PROCESSED-TABLE source-pathname #f)
	  (fprintf stderr "processing: ~a\n" source-pathname)
	  (%build-compilation-recipe target/dependencies-list binary-pathname source-pathname)
	  (%build-installation-stuff binary-pathname source-pathname)
	  (hashtable-set! ALREADY-PROCESSED-TABLE source-pathname #t)
	  (for-each-in-order
	      %process-library-reference
	    (cdr target/dependencies-list))))))

  (define* (%library-build-dependency-list {lib libs.library?})
    ;;Return a list of  R6RS library names; the first name is  the target's, the rest
    ;;is the list of library names of  libraries upon which the target depends.  Only
    ;;libraries having  a source file  field are  included; only libraries  under the
    ;;hierarchies selected by INCLUDE-LIBRARY-HIERARCHIES are included.
    ;;
    (and (libs.library-source-file-name lib)
	 (let ((libname (libs.library-name lib)))
	   (and (%library-name-under-hierarchies? libname (include-library-build-hierarchies))
		(cons libname
		      (fold-left
			  (lambda (knil dep-lib)
			    (if (libs.library-source-file-name dep-lib)
				(let ((libname (libs.library-name dep-lib)))
				  (if (%library-name-under-hierarchies? libname (include-library-dependencies-hierarchies))
				      (cons libname knil)
				    knil))
			      knil))
			'()
			(libs.library-imp-lib* lib)))))))

  (define (%build-compilation-recipe target/dependencies-list binary-pathname source-pathname)
    (print-makefile-content "~a: \\\n\t\t~a" binary-pathname source-pathname)
    (unless (null? (cdr target/dependencies-list))
      (for-each (lambda (libname)
		  (print-makefile-content " \\\n\t\t~a"
					  (libs.directory+library-stem->library-binary-pathname
					   "lib" (libs.library-name->filename-stem libname))))
	(cdr target/dependencies-list)))
    (print-makefile-content " \\\n\t\t$(FASL_PREREQUISITES)\n")
    (print-makefile-content "\t$(VICARE_COMPILE_RUN) --output $@ --compile-library $<\n\n"))

  (define (%build-installation-stuff binary-pathname source-pathname)
    (let ((fasl-stem (%string-replace-nasty-chars '(#\/ #\- #\% #\.) #\_ binary-pathname))
	  (sls-stem  (%string-replace-nasty-chars '(#\/ #\- #\% #\.) #\_ source-pathname))
	  (noinst    (if (include-install-rules) "" "noinst_")))

      ;;Generate the opening Automake conditional directives.
      (when (conditionals)
	(for-each-in-order
	    (lambda (conditional)
	      (print-makefile-content "if ~a\n" conditional))
	  (conditionals)))

      ;;Generate the installation-directory variable-definitions.
      ;;
      ;;NOTE For whatever reason these directories definitions are needed by Automake
      ;;even when  the targets  are *excluded* from  installation with  the "noinst_"
      ;;prefix.
      (receive (dir file)
	  (px.split-pathname-root-and-tail binary-pathname)
	(let ((instdir-suffix (strip-lib-prefix dir)))
	  (print-makefile-content "~adir = $(bundledlibsdir)/~a\n"  fasl-stem instdir-suffix)
	  (print-makefile-content "~adir  = $(bundledlibsdir)/~a\n" sls-stem  instdir-suffix)))

      ;;Generate the variable definition that defines the FASL file as build target.
      (print-makefile-content "nodist_~a~a_DATA = ~a\n" noinst fasl-stem binary-pathname)

      ;;Generate  the variable  definition  that  causes the  source  file  to be  an
      ;;installation target.
      (print-makefile-content "if WANT_INSTALL_SOURCES\ndist_~a~a_DATA = ~a\nendif\n"
			      noinst sls-stem source-pathname)

      ;;Generate the variable  assignment that causes the source file  to be included
      ;;in the distribution as  extra file.  If the soruce is  a template source file
      ;;or an automatically generated source file: it is excluded.
      (unless (or (hashtable-ref (from-templates-source-files-table) source-pathname #f)
		  (hashtable-ref (built-source-files-table)          source-pathname #f))
	(print-makefile-content "EXTRA_DIST += ~a\n" source-pathname))

      ;;Generate the variable  assignment that causes the FASL file  to be removes by
      ;;the "clean" rule.
      (print-makefile-content "CLEANFILES += ~a\n" binary-pathname)

      ;;Generate the closing Automake conditional directives.
      (when (conditionals)
	(for-each-in-order
	    (lambda (conditional)
	      (print-makefile-content "endif\n"))
	  (conditionals)))

      ;;Done.
      (print-makefile-content "\n")))

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

(define-constant from-templates-source-files-table
  ;;Here we store the  pathnames of source library files that  are created by running
  ;;"configure" and processing ".sls.in" templates.   The templates ".sls.in" must be
  ;;included  in the  EXTRA_DIST; the  processed source  files ".sls"  must *not*  be
  ;;included in the EXTRA_DIST.
  ;;
  (make-parameter #f))

(define-constant built-source-files-table
  ;;Here we  store the  pathnames of source  library files that  are created  by some
  ;;makefile  rule; such  files are  listed in  the makefile  BUILT_SOURCES variable.
  ;;Such source files must *not* be included in the EXTRA_DIST list of files.
  ;;
  (make-parameter #f))


;;;; go!!!

(define conditionals
  (make-parameter #f))

(define build-recipes-port
  (make-parameter #f))

(define (print-makefile-content template . args)
  (apply fprintf (build-recipes-port) template args))

(define (build-library-dependencies from-templates-source-files
				    built-source-files
				    libraries-specs)
  ;;Process source libraries specifications and return a string representing stuff to
  ;;be included in a "Makefile.am".
  ;;
  ;;FROM-TEMPLATES-SOURCE-FILES must be a list of strings representing library source
  ;;file pathnames that  are created by running "configure"  and processing ".sls.in"
  ;;templates.   The templates  ".sls.in" must  be  included in  the EXTRA_DIST;  the
  ;;processed source files ".sls" must *not* be included in the EXTRA_DIST.
  ;;
  ;;BUILT-SOURCE-FILES must  be a  list of strings  representing library  source file
  ;;pathnames that are  created by some makefile  rule; such files are  listed in the
  ;;makefile BUILT_SOURCES variable.  Such source files must *not* be included in the
  ;;EXTRA_DIST list of files.
  ;;
  ;;LIBRARIES-SPECS must be a list of  lists selecting the Scheme libraries to visit.
  ;;Each sublist must be of the form:
  ;;
  ;;   ((?want-symbol ...) ?libref ...)
  ;;
  ;;where:  ?WANT-SYMBOL is  an Automake  conditional symbol  and ?LIBREF  is a  R6RS
  ;;library reference.
  ;;

  ;;We want to load source files only.
  (parametrise
      ((libs.current-library-locator libs.source-library-locator))

    (from-templates-source-files-table
     (receive-and-return (T)
	 (make-hashtable string-hash string=?)
       (for-each
	   (lambda (source-pathname)
	     (hashtable-set! T source-pathname #t))
	 from-templates-source-files)))

    (built-source-files-table
     (receive-and-return (T)
	 (make-hashtable string-hash string=?)
       (for-each
	   (lambda (source-pathname)
	     (hashtable-set! T source-pathname #t))
	 built-source-files)))

    (receive (port get-automake-stuff)
	(open-string-output-port)
      (fprintf port "## dependencies.make --\n#\n# Automatically built.\n\n")

      ;;Add to the EXTRA_DIST the Autoconf template library files, if any.
      (let ((source-pathnames (hashtable-keys (from-templates-source-files-table))))
	(unless (zero? (vector-length source-pathnames))
	  (fprintf port "EXTRA_DIST += ")
	  (vector-for-each
	      (lambda (source-pathname)
		(fprintf port " \\\n\t~a.in" source-pathname))
	    source-pathnames)
	  (fprintf port "\n\n")))

      ;;Process all the library references.
      (for-each-in-order
	  (lambda (spec-entry)
	    (parametrise ((conditionals       (car spec-entry))
			  (build-recipes-port port))
	      (let ((library-names (cdr spec-entry)))
		(for-each-in-order %process-library-reference library-names))))
	libraries-specs)

      (fprintf port "\n### end of file\n# Local Variables:\n# mode: makefile-automake\n# End:\n")
      (get-automake-stuff))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; End:
