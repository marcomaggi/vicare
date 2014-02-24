;;; build-makefile-rules.sps --

#!r6rs
(import (vicare)
  (prefix (vicare libraries) libs.)
  (prefix (vicare language-extensions posix) px.))

;;We want to load source files only.
(libs.current-library-locator libs.source-library-locator)
(libs.fasl-search-path '())

;;We perform  this import through eval,  so that it happens  at run-time
;;after we have set the source library locator.
(eval '(import
	   (only (vicare platform configuration))
	 (only (vicare platform words))
	 (only (vicare platform errno))
	 (only (vicare platform constants))
	 (only (vicare platform features))
	 (only (vicare platform utilities))

	 (only (vicare unsafe capi))
	 (only (vicare unsafe operations))
	 (only (vicare unsafe unicode))

	 (only (vicare language-extensions cond-expand))
	 (only (vicare language-extensions cond-expand OS-id-features))
	 (only (vicare language-extensions cond-expand feature-cond))
	 (only (vicare language-extensions cond-expand helpers))
	 (only (vicare language-extensions cond-expand platform-features))
	 (only (vicare language-extensions cond-expand configuration-features))
	 (only (vicare language-extensions cond-expand registry))

	 (only (vicare arguments validation))
	 (only (vicare arguments general-c-buffers))

	 (only (vicare language-extensions syntaxes))
	 (only (vicare language-extensions amb))
	 (only (vicare language-extensions simple-match))
	 (only (vicare language-extensions coroutines))
	 (only (vicare language-extensions increments))
	 (only (vicare language-extensions infix))
	 (only (vicare language-extensions keywords))
	 (only (vicare language-extensions sentinels))
	 (only (vicare language-extensions namespaces))
	 (only (vicare language-extensions custom-ports))
	 (only (vicare language-extensions variables))
	 (only (vicare language-extensions streams))
	 (only (vicare language-extensions loops))
	 (only (vicare language-extensions ascii-chars))
	 (only (vicare language-extensions comparisons))
	 (only (vicare language-extensions hooks))
	 (only (vicare language-extensions callables))
	 (only (vicare language-extensions define-record-extended))
	 (only (vicare language-extensions c-enumerations))
	 (only (vicare language-extensions identifier-substitutions))
	 (only (vicare language-extensions makers))
	 (only (vicare language-extensions try))

	 (only (vicare checks))

	 (only (vicare crypto randomisations low))
	 (only (vicare crypto randomisations))
	 (only (vicare crypto randomisations blum-blum-shub))
	 (only (vicare crypto randomisations borosh))
	 (only (vicare crypto randomisations cmrg))
	 (only (vicare crypto randomisations distributions))
	 (only (vicare crypto randomisations lists))
	 (only (vicare crypto randomisations marsaglia))
	 (only (vicare crypto randomisations mersenne))
	 (only (vicare crypto randomisations strings))
	 (only (vicare crypto randomisations vectors))

	 (only (vicare numerics constants))
	 (only (vicare numerics flonum-parser))
	 (only (vicare numerics flonum-formatter))

	 (only (vicare containers bytevectors))
	 (only (vicare containers auxiliary-syntaxes))
	 (only (vicare containers weak-hashtables))
	 (only (vicare containers object-properties))
	 (only (vicare containers knuth-morris-pratt))
	 (only (vicare containers bytevector-compounds core))
	 (only (vicare containers bytevector-compounds))
	 (only (vicare containers bytevector-compounds unsafe))
	 (only (vicare containers char-sets))
	 (only (vicare containers char-sets blocks))
	 (only (vicare containers char-sets categories))
	 (only (vicare containers lists stx))
	 (only (vicare containers lists low))
	 (only (vicare containers lists))
	 (only (vicare containers vectors low))
	 (only (vicare containers vectors))
	 (only (vicare containers strings low))
	 (only (vicare containers strings))
	 (only (vicare containers strings rabin-karp))
	 (only (vicare containers levenshtein))
	 (only (vicare containers one-dimension-co))
	 (only (vicare containers one-dimension-cc))
	 (only (vicare containers bytevectors u8))
	 (only (vicare containers bytevectors s8))
	 (only (vicare containers arrays))
	 (only (vicare containers stacks))
	 (only (vicare containers queues))

	 (only (vicare parser-tools silex lexer))
	 (only (vicare parser-tools silex))
	 (only (vicare parser-tools silex utilities))
	 (only (vicare parser-tools unix-pathnames))

	 (only (vicare net channels))

	 (only (vicare ffi))
	 (only (vicare ffi foreign-pointer-wrapper))

	 (only (vicare iconv))

	 (only (vicare posix))
	 (only (vicare posix pid-files))
	 (only (vicare posix lock-pid-files))
	 (only (vicare posix log-files))
	 (only (vicare posix daemonisations))
	 (only (vicare posix simple-event-loop))
	 (only (vicare posix tcp-server-sockets))

	 (only (vicare glibc))
	 (only (vicare gcc))
	 (only (vicare linux))
	 (only (vicare readline))

	 (only (vicare assembler inspection))
	 (only (vicare debugging compiler))
	 (only (vicare parser-logic))

	 (only (vicare irregex))
	 (only (vicare pregexp))
	 (only (vicare getopts))
	 (only (vicare formations))

	 (only (vicare cre2))

	 (only (srfi :0))
	 (only (srfi :1))
	 (only (srfi :2))
	 (only (srfi :6))
	 (only (srfi :8))
	 (only (srfi :9))
	 (only (srfi :11))
	 (only (srfi :13))
	 (only (srfi :14))
	 (only (srfi :16))
	 (only (srfi :19))
	 (only (srfi :23))
	 (only (srfi :25))
;;;(only (srfi :25 multi-dimensional-arrays arlib))
	 (only (srfi :26))
	 (only (srfi :27))
	 (only (srfi :28))
	 (only (srfi :31))
	 (only (srfi :37))
	 (only (srfi :38))
	 (only (srfi :39))
	 (only (srfi :41))
	 (only (srfi :42))
	 (only (srfi :43))
	 (only (srfi :45))
	 (only (srfi :48))
	 (only (srfi :61))
	 (only (srfi :64))
	 (only (srfi :67))
	 (only (srfi :69))
	 (only (srfi :78))
	 (only (srfi :98))
	 (only (srfi :99))
	 ;;We  really   need  all   of  these  for   this  SRFI,   because  the
	 ;;implementation is in (srfi :101).
	 (only (srfi :101))
	 (only (srfi :101 random-access-lists))
	 (only (srfi :101 random-access-lists procedures))
	 (only (srfi :101 random-access-lists syntax))
	 (only (srfi :101 random-access-lists equal))
	 ;;This SRFI depends upon (vicare posix).
	 (only (srfi :106))
	 (only (srfi :111))
	 (only (srfi :112))

	 (only (nausicaa language auxiliary-syntaxes))
	 (only (nausicaa language oopp))
	 (only (nausicaa language multimethods))
	 (only (nausicaa language builtins))
	 (only (nausicaa language conditions))
	 (only (nausicaa language simple-match))
	 (only (nausicaa language infix))
	 (only (nausicaa))

	 (only (nausicaa containers lists))
	 (only (nausicaa containers vectors))
	 (only (nausicaa containers strings))
	 (only (nausicaa containers arrays))
	 (only (nausicaa containers stacks))
	 (only (nausicaa containers queues))
	 (only (nausicaa containers bitvectors))
	 (only (nausicaa containers iterators))

	 (only (nausicaa parser-tools source-locations))
	 (only (nausicaa parser-tools lexical-tokens))
	 (only (nausicaa parser-tools silex default-error-handler))
	 (only (nausicaa parser-tools lalr lr-driver))
	 (only (nausicaa parser-tools lalr glr-driver))
	 (only (nausicaa parser-tools lalr))

	 (only (nausicaa parser-tools ip-addresses ipv4-address-lexer))
	 (only (nausicaa parser-tools ip-addresses ipv4-address-parser))
	 (only (nausicaa parser-tools ip-addresses ipv6-address-lexer))
	 (only (nausicaa parser-tools ip-addresses ipv6-address-parser))
	 (only (nausicaa parser-tools ipv4-addresses))
	 (only (nausicaa parser-tools ipv6-addresses))
	 (only (nausicaa parser-tools uri))

	 (only (nausicaa uri ip))
	 (only (nausicaa uri))
	 (only (nausicaa parser-tools uri utilities))
	 (only (nausicaa uri pathnames abstract))
	 (only (nausicaa uri pathnames unix))
	 (only (nausicaa uri pathnames))

	 (only (nausicaa mehve))

	 #| end of import |# )
      (interaction-environment))


;;;; string pathname helpers

(module (strip-source-file-prefix
	 %string-prefix?)

  (define* (strip-source-file-prefix (pathname string?))
    ;;Given  a source  file  pathname:  if it  starts  with "../lib"  or
    ;;"./lib"  or a  directory in  the  source search  path: strip  such
    ;;prefix and return the result; otherwise return PATHNAME itself.
    ;;
    (cond ((%string-prefix? "../lib/" pathname)
	   (%deprefix "../lib/" pathname))
	  ((%string-prefix? "./lib/" pathname)
	   (%deprefix "../lib/" pathname))
	  (else
	   (let loop ((dirs (libs.library-path)))
	     (cond ((null? dirs)
		    pathname)
		   ((%string-prefix? (car dirs) pathname)
		    (%deprefix (car dirs) pathname))
		   (else
		    (loop (cdr dirs))))))))

  (define (%string-prefix? prefix str)
    (let ((prefix.len (string-length prefix)))
      (and (fx<= prefix.len (string-length str))
	   (string=? prefix (substring str 0 prefix.len)))))

  (define (%deprefix prefix str)
    (substring str (string-length prefix) (string-length str)))

  #| end of module |# )


;;;; installation groups helpers

(module (install-group-register
	 install-group-makefile-declarations)

  (define-constant TABLE
    (make-hashtable string-hash string=?))

  (define (install-group-register fasl-pathname)
    ;;FASL-PATHNAME is the FASL file name, starting with a slash.
    ;;
    (receive (dir file)
	(px.split-pathname-root-and-tail fasl-pathname)
      (let ((record (hashtable-ref TABLE dir #f)))
	(if record
	    (set-cdr! (last-pair record) (list fasl-pathname))
	  (hashtable-set! TABLE dir (list fasl-pathname))))))

  (define (install-group-makefile-declarations)
    (let ((keys (vector-sort string<? (hashtable-keys TABLE))))
      (receive (port getter)
	  (open-string-output-port)
	(vector-for-each
	    (lambda (dir)
	      ;;DIR is a  fasl pathname directory part,  starting with a
	      ;;slash.
	      (let* ((val  (hashtable-ref TABLE dir #f))
		     (stem (string-replace-nasty-chars '(#\/ #\- #\%) #\_ dir)))
		(receive (opening closing)
		    (%build-conditional-enclosure stem)
		  (opening port)
		  (fprintf port "fasl_~adir = $(bundledlibsdir)~a\nnodist_fasl_~a_DATA ="
			   stem dir stem)
		  (for-each (lambda (fasl-pathname)
			      (fprintf port " \\\n\tlib~a" fasl-pathname))
		    val)
		  (fprintf port "\n")
		  (closing port)
		  (fprintf port "\n"))))
	  keys)
	(getter))))

  (define (%build-conditional-enclosure stem)
;;;(fprintf stderr "conditional enclosure for: ~a\n" stem)
    (cond ((%string-prefix? "_srfi__3a106" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_SRFI\nif WANT_POSIX\n"))
		   (lambda (port)
		     (fprintf port "endif\nendif\n"))))
	  ((%string-prefix? "_srfi" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_SRFI\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((%string-prefix? "_vicare_ffi" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_LIBFFI\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((%string-prefix? "_vicare_posix" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_POSIX\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((%string-prefix? "_vicare_glibc" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_GLIBC\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((%string-prefix? "_vicare_iconv" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_LIBICONV\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((%string-prefix? "_vicare_gcc" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_LIBFFI\nif WANT_POSIX\nif WANT_GLIBC\n"))
		   (lambda (port)
		     (fprintf port "endif\nendif\nendif\n"))))
	  ((%string-prefix? "_vicare_linux" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_LINUX\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((%string-prefix? "_vicare_readline" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_READLINE\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((%string-prefix? "_vicare_cre2" stem)
	   (values (lambda (port)
		     (fprintf port "if WANT_CRE2\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  ((or (%string-prefix? "_nausicaa" stem)
	       ;;This is a  special case: only (nausicaa) is  in the top
	       ;;directory.
	       (string-empty? stem))
	   (values (lambda (port)
		     (fprintf port "if WANT_NAUSICAA\n"))
		   (lambda (port)
		     (fprintf port "endif\n"))))
	  (else
	   (values values values))))

  (define (string-replace-nasty-chars chs ch.repl str1)
    (receive-and-return (str2)
	(make-string (string-length str1))
      (do ((i 0 (fxadd1 i)))
	  ((fx= i (string-length str1)))
	(let ((ch (string-ref str1 i)))
	  (if (memv ch chs)
	      (string-set! str2 i ch.repl)
	    (string-set! str2 i ch))))))

  #| end of module |# )


;;;; the meat

(define* (library-build-dependency-list (lib libs.library?))
  ;;Return a list of R6RS library names; the first name is the target's,
  ;;the rest  is the list of  library names of libraries  upon which the
  ;;target  depends.  Only  libraries  having a  source  file field  are
  ;;included.
  ;;
  (and (libs.library-source-file-name lib)
       (cons (libs.library-name lib)
	     (fold-left (lambda (knil dep-lib)
			  (if (libs.library-source-file-name dep-lib)
			      (cons (libs.library-name dep-lib)
				    knil)
			    knil))
	       '()
	       (libs.library-imp-lib* lib)))))

(define (process-rule lib rule port)
;;;(fprintf stderr "processing: ~a\n" lib)
  (let* ((libname       (car rule))
	 (fasl-pathname (libs.fasl-stem+extension libname)))
    ;;Remember that FASL-PATHNAME starts with a slash!!!
    (install-group-register fasl-pathname)
    (fprintf port "lib~a: \\\n\t\tlib~a \\\n"
	     fasl-pathname
	     (strip-source-file-prefix (libs.library-source-file-name lib)))
    (when (not (null? (cdr rule)))
      (for-each (lambda (libname)
		  (fprintf port "\t\tlib~a \\\n"
			   (libs.fasl-stem+extension libname)))
	(cdr rule)))
    (fprintf port
	     (string-append "\t\t$(VICARE_NEW_BOOT)\n"
			    "\t$(VICARE_COMPILE_RUN) --output $@ --compile-library $<\n\n"))))


;;;; go!!!

(fprintf stdout "## dependencies.make --\n#\n# Automatically built.\n\n")

(receive (port getter)
    (open-string-output-port)
  (for-each (lambda (lib)
	      (let ((rule (library-build-dependency-list lib)))
		(when rule
		  (process-rule lib rule port))))
    (libs.installed-libraries))
  (display (getter) stdout)
  (flush-output-port stdout))

(fprintf stdout (install-group-makefile-declarations))
(fprintf stdout "\n### end of file\n# Local Variables:\n# mode: makefile-automake\n# End:\n")
(flush-output-port stdout)

;;; end of file
