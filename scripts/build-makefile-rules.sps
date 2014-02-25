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

;;We want to load source files only.
(libs.current-library-locator libs.source-library-locator)
(libs.fasl-search-path '())


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
      (unless (or (hashtable-ref FROM-TEMPLATES-SOURCE-FILES source-pathname #f)
		  (hashtable-ref BUILT-SOURCE-FILES          source-pathname #f))
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

(define-constant FROM-TEMPLATES-SOURCE-FILES
  ;;Here we store the pathnames of source library files that are created
  ;;by  running "configure"  and processing  ".sls.in" templates.   Such
  ;;source fiels must *not* be included in the EXTRA_DIST list of files.
  ;;
  (receive-and-return (T)
      (make-hashtable string-hash string=?)
    (for-each
	(lambda (source-pathname)
	  (hashtable-set! T source-pathname #t))
      '("lib/vicare/platform/configuration.sls"
	"lib/vicare/platform/constants.sls"
	"lib/vicare/platform/errno.sls"
	"lib/vicare/platform/words.sls"
	"lib/nausicaa/uri/pathnames.sls"))))

(define-constant BUILT-SOURCE-FILES
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
      '("lib/vicare/platform/features.sls"))))

(define-constant lib-spec
  '((#f
     (vicare platform configuration)
     (vicare platform words)
     (vicare platform errno)
     (vicare platform constants)
     (vicare platform features)
     (vicare platform utilities)

     (vicare unsafe capi)
     (vicare unsafe operations)
     (vicare unsafe unicode)

     (vicare language-extensions cond-expand)
     (vicare language-extensions cond-expand OS-id-features)
     (vicare language-extensions cond-expand feature-cond)
     (vicare language-extensions cond-expand helpers)
     (vicare language-extensions cond-expand platform-features)
     (vicare language-extensions cond-expand configuration-features)
     (vicare language-extensions cond-expand registry)

     (vicare arguments validation)
     (vicare arguments general-c-buffers)

     (vicare language-extensions syntaxes)
     (vicare language-extensions amb)
     (vicare language-extensions simple-match)
     (vicare language-extensions coroutines)
     (vicare language-extensions increments)
     (vicare language-extensions infix)
     (vicare language-extensions keywords)
     (vicare language-extensions sentinels)
     (vicare language-extensions namespaces)
     (vicare language-extensions custom-ports)
     (vicare language-extensions variables)
     (vicare language-extensions streams)
     (vicare language-extensions loops)
     (vicare language-extensions ascii-chars)
     (vicare language-extensions comparisons)
     (vicare language-extensions hooks)
     (vicare language-extensions callables)
     (vicare language-extensions define-record-extended)
     (vicare language-extensions c-enumerations)
     (vicare language-extensions identifier-substitutions)
     (vicare language-extensions makers)
     (vicare language-extensions try)

     (vicare checks)

     (vicare crypto randomisations low)
     (vicare crypto randomisations)
     (vicare crypto randomisations blum-blum-shub)
     (vicare crypto randomisations borosh)
     (vicare crypto randomisations cmrg)
     (vicare crypto randomisations distributions)
     (vicare crypto randomisations lists)
     (vicare crypto randomisations marsaglia)
     (vicare crypto randomisations mersenne)
     (vicare crypto randomisations strings)
     (vicare crypto randomisations vectors)

     (vicare numerics constants)
     (vicare numerics flonum-parser)
     (vicare numerics flonum-formatter)

     (vicare containers bytevectors)
     (vicare containers auxiliary-syntaxes)
     (vicare containers weak-hashtables)
     (vicare containers object-properties)
     (vicare containers knuth-morris-pratt)
     (vicare containers bytevector-compounds core)
     (vicare containers bytevector-compounds)
     (vicare containers bytevector-compounds unsafe)
     (vicare containers char-sets)
     (vicare containers char-sets blocks)
     (vicare containers char-sets categories)
     (vicare containers lists stx)
     (vicare containers lists low)
     (vicare containers lists)
     (vicare containers vectors low)
     (vicare containers vectors)
     (vicare containers strings low)
     (vicare containers strings)
     (vicare containers strings rabin-karp)
     (vicare containers levenshtein)
     (vicare containers one-dimension-co)
     (vicare containers one-dimension-cc)
     (vicare containers bytevectors u8)
     (vicare containers bytevectors s8)
     (vicare containers arrays)
     (vicare containers stacks)
     (vicare containers queues)

     (vicare parser-tools silex lexer)
     (vicare parser-tools silex)
     (vicare parser-tools silex utilities)
     (vicare parser-tools unix-pathnames)

     (vicare net channels)

     #| end no conditional |# )

    ((WANT_LIBFFI)
     (vicare ffi)
     (vicare ffi foreign-pointer-wrapper))

    ((WANT_LIBICONV)
     (vicare iconv))

    ((WANT_POSIX)
     (vicare posix)
     (vicare posix pid-files)
     (vicare posix lock-pid-files)
     (vicare posix log-files)
     (vicare posix daemonisations)
     (vicare posix simple-event-loop)
     (vicare posix tcp-server-sockets))

    ((WANT_GLIBC)
     (vicare glibc))

    ((WANT_LIBFFI WANT_POSIX WANT_GLIBC)
     (vicare gcc))

    ((WANT_POSIX WANT_LINUX)
     (vicare linux))

    ((WANT_READLINE)
     (vicare readline))

    (#f
     (vicare assembler inspection)
     (vicare debugging compiler)
     (vicare parser-logic)

     (vicare irregex)
     (vicare pregexp)
     (vicare getopts)
     (vicare formations))

    ((WANT_CRE2)
     (vicare cre2))

    ((WANT_SRFI)
     (srfi :0)
     (srfi :1)
     (srfi :2)
     (srfi :6)
     (srfi :8)
     (srfi :9)
     (srfi :11)
     (srfi :13)
     (srfi :14)
     (srfi :16)
     (srfi :19)
     (srfi :23)
     (srfi :25)
;;;(srfi :25 multi-dimensional-arrays arlib)
     (srfi :26)
     (srfi :27)
     (srfi :28)
     (srfi :31)
     (srfi :37)
     (srfi :38)
     (srfi :39)
     (srfi :41)
     (srfi :42)
     (srfi :43)
     (srfi :45)
     (srfi :48)
     (srfi :61)
     (srfi :64)
     (srfi :67)
     (srfi :69)
     (srfi :78)
     (srfi :98)
     (srfi :99)
     ;;We  really   need  all   of  these  for   this  SRFI,   because  the
     ;;implementation is in (srfi :101).
     (srfi :101)
     (srfi :101 random-access-lists)
     (srfi :101 random-access-lists procedures)
     (srfi :101 random-access-lists syntax)
     (srfi :101 random-access-lists equal)
     (srfi :111)
     (srfi :112))

    ((WANT_SRFI WANT_POSIX)
     (srfi :106))

    ((WANT_NAUSICAA)
     (nausicaa language auxiliary-syntaxes)
     (nausicaa language oopp)
     (nausicaa language multimethods)
     (nausicaa language builtins)
     (nausicaa language conditions)
     (nausicaa language simple-match)
     (nausicaa language infix)
     (nausicaa)

     (nausicaa containers lists)
     (nausicaa containers vectors)
     (nausicaa containers strings)
     (nausicaa containers arrays)
     (nausicaa containers stacks)
     (nausicaa containers queues)
     (nausicaa containers bitvectors)
     (nausicaa containers iterators)

     (nausicaa parser-tools source-locations)
     (nausicaa parser-tools lexical-tokens)
     (nausicaa parser-tools silex default-error-handler)
     (nausicaa parser-tools lalr lr-driver)
     (nausicaa parser-tools lalr glr-driver)
     (nausicaa parser-tools lalr)

     (nausicaa parser-tools ip-addresses ipv4-address-lexer)
     (nausicaa parser-tools ip-addresses ipv4-address-parser)
     (nausicaa parser-tools ip-addresses ipv6-address-lexer)
     (nausicaa parser-tools ip-addresses ipv6-address-parser)
     (nausicaa parser-tools ipv4-addresses)
     (nausicaa parser-tools ipv6-addresses)
     (nausicaa parser-tools uri)

     (nausicaa uri ip)
     (nausicaa uri)
     (nausicaa parser-tools uri utilities)
     (nausicaa uri pathnames abstract)
     (nausicaa uri pathnames unix)
     (nausicaa uri pathnames)

     (nausicaa mehve)

     #| end of nausicaa conditional |# )

    #| end of spec |# ))


;;;; go!!!

(define conditionals
  (make-parameter #f))

(define build-recipes-port
  (make-parameter #f))

(define (print-recipe template . args)
  (apply fprintf (build-recipes-port) template args))

(fprintf stdout "## dependencies.make --\n#\n# Automatically built.\n\n")

(fprintf stdout "EXTRA_DIST += ")
(vector-for-each
    (lambda (source-pathname)
      (fprintf stdout " \\\n\t~a.in" source-pathname))
  (hashtable-keys FROM-TEMPLATES-SOURCE-FILES))
(fprintf stdout "\n\n")

(receive (port get-build-recipes)
    (open-string-output-port)
  (for-each-in-order
      (lambda (spec-entry)
	(parametrise ((conditionals       (car spec-entry))
		      (build-recipes-port port))
	  (let ((library-names (cdr spec-entry)))
	    (for-each-in-order %process-library-reference library-names))))
    lib-spec)
  (display (get-build-recipes) stdout)
  (flush-output-port stdout))

(fprintf stdout "\n### end of file\n# Local Variables:\n# mode: makefile-automake\n# End:\n")
(flush-output-port stdout)

;;; end of file
