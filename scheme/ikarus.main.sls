;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;This is  here to test that  we can import things  from other libraries
;;within the compiler itself.
(library (ikarus startup)
  (export
    vicare-version
    bootfile
    host-info)
  (import (except (vicare)
		  host-info))
  (include "ikarus.config.ss" #t))


(library (ikarus main)
  (export

    ;; automatic structs finalisation
    $struct-guardian
    struct-guardian-logger		struct-guardian-log

    ;; automatic R6RS records finalisation
    $record-guardian
    record-guardian-logger		record-guardian-log)
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  load-r6rs-script
		  load
		  $struct-guardian
		  struct-guardian-logger
		  struct-guardian-log
		  $record-guardian
		  record-guardian-logger
		  record-guardian-log)
    (prefix (ikarus startup) config.)
    (prefix (ikarus.options) option.)
    (prefix (only (ikarus.compiler)
		  optimize-level
		  $generate-debug-calls
		  $assembler-output
		  $optimizer-output
		  $open-mvcalls
		  $source-optimizer-passes-count)
	    compiler.)
    (prefix (only (ikarus.debugger)
		  guarded-start)
	    debugger.)
    (prefix (only (psyntax.expander)
		  initialise-type-spec-for-built-in-object-types
		  initialise-core-prims-tagging)
	    psyntax.)
    (prefix (only (psyntax.library-manager)
		  current-library-expander
		  source-code-location)
	    psyntax.)
    (only (ikarus.reader)
	  read-source-file
	  read-script-source-file)
    (only (ikarus.symbol-table)
	  $initialize-symbol-table!)
    (only (ikarus.strings-table)
	  $initialize-interned-strings-table!)
    (prefix (ikarus load) load.)
    (prefix (only (ikarus library-utils)
		  init-search-paths-and-directories)
	    libutils.)
    (prefix (only (ikarus.posix)
		  getenv
		  real-pathname
		  split-search-path-string)
	    posix.)
    (only (vicare system $structs)
	  $struct-ref
	  $struct-rtd)
    (only (ikarus cafe)
	  cafe-input-port)
    (prefix (only (ikarus.readline)
		  readline-enabled?
		  make-readline-input-port)
	    readline.))

  (include "ikarus.wordsize.scm" #t)


;;;; helpers

(define (%error-and-exit msg)
  (let ((port (current-error-port)))
    (display "*** vicare error: " port)
    (display msg port)
    (newline port)
    (exit 1)))

(define (%error-invalid-rc)
  (%error-and-exit "option --no-rcfile is invalid when used along with --rcfile"))

(define-auxiliary-syntaxes
  serialise?
  run?)

(define-syntax load-r6rs-script
  (syntax-rules (serialise? run?)
    ((_ ?filename (serialise? ?ser) (run? ?run))
     (load.load-r6rs-script ?filename ?ser ?run))
    ((_ ?filename (serialise? ?ser) (run? ?run))
     (load.load-r6rs-script ?filename ?ser ?run))
    ))

(define (%string->sexp expr-string)
  (let loop ((port     (open-string-input-port expr-string))
	     (the-expr '()))
    (let ((form (read port)))
      (if (eof-object? form)
	  (cons 'begin (reverse the-expr))
	(loop port (cons form the-expr))))))


;;;; data types

(define-struct run-time-config
  (exec-mode
		;A  symbol representing  the requested  execution mode:  R6RS-SCRIPT,
		;BINARY-PROGRAM,        COMPILE-DEPENDENCIES,        COMPILE-LIBRARY,
		;COMPILE-PROGRAM, COMPILE-SOMETHING, REPL.
   script
		;A  string  representing a  file  name:  the  main script.   When  in
		;R6RS-SCRIPT,   BINARY-PROGRAM,   COMPILE-PROGRAM,   COMPILE-LIBRARY,
		;COMPILE-SOMETHING or COMPILE-DEPENDENCIES mode: it must hold an R6RS
		;program.  When in script mode: it must hold a script.

   rcfiles
		;#f, #t, or a proper list of strings representing file names.
		;
		;When #f: avoid executing any run-command files.
		;
		;When #t: only if the EXEC-MODE is REPL, load and execute the default
		;run-command file as an R6RS program.
		;
		;When a  list of strings:  load the listed  files to be  evaluated as
		;R6RS programs before instantiating libraries.

   load-libraries
		;Null or a  list of strings representing file names:  libraries to be
		;instantiated,  adding the  result  to  the interaction  environment,
		;after the RC files and before the load scripts.

   program-options
		;Null or  a list of strings  representing command line options  to be
		;handed to the executed program through the COMMAND-LINE function.

   no-greetings
		;If  true: avoid  printing the  greetings message  when starting  the
		;REPL.

   library-source-search-path
		;Null or a  list of strings representing  directory names: additional
		;locations in which to search for libraries.

   library-binary-search-path
		;Null or a  list of strings representing  directory names: additional
		;locations in which to search for FASL files.

   store-directory
		;False of a  string representing the initial value  for the parameter
		;COMPILED-LIBRARIES-STORE-DIRECTORY.

   more-file-extensions
		;Turn on  search for more  library file extension  than ".vicare.sls"
		;and ".sls".

   raw-repl
		;If true do  not create a readline console input  port, even when the
		;readline interface is available.
   output-file
		;False or a  non-empty string representing the pathname  of an output
		;file.  It has multiple purposes: output file for compiled libraries;
		;output file for compiled programs.
   ))

(define-inline (run-time-config-load-libraries-register! cfg pathname)
  (set-run-time-config-load-libraries! cfg (cons pathname (run-time-config-load-libraries cfg))))

(define-inline (run-time-config-library-source-search-path-register! cfg pathname)
  (set-run-time-config-library-source-search-path! cfg (cons pathname (run-time-config-library-source-search-path cfg))))

(define-inline (run-time-config-library-binary-search-path-register! cfg pathname)
  (set-run-time-config-library-binary-search-path! cfg (cons pathname (run-time-config-library-binary-search-path cfg))))

(define (run-time-config-rcfiles-register! cfg new-rcfile)
  (let ((rcfiles (run-time-config-rcfiles cfg)))
    (if (boolean? new-rcfile)
	(if (boolean? rcfiles)
	    (set-run-time-config-rcfiles! cfg new-rcfile)
	  (%error-invalid-rc))
      (set-run-time-config-rcfiles! cfg (cons new-rcfile
					      (case rcfiles
						((#t) '())
						((#f) (%error-invalid-rc))
						(else rcfiles)))))))


(define-syntax with-run-time-config
  ;;Dot notation for instances of RUN-TIME-CONFIG structures.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?cfg) . ?body)
       (identifier? #'?cfg)
       (let* ((cfg-id	#'?cfg)
	      (cfg-str	(symbol->string (syntax->datum cfg-id))))
	 (define (%dot-id field-str)
	   (datum->syntax cfg-id (string->symbol (string-append cfg-str field-str))))
	 (with-syntax
	     ((CFG.EXEC-MODE		(%dot-id ".exec-mode"))
	      (CFG.SCRIPT		(%dot-id ".script"))
	      (CFG.RCFILES		(%dot-id ".rcfiles"))
	      (CFG.LOAD-LIBRARIES	(%dot-id ".load-libraries"))
	      (CFG.PROGRAM-OPTIONS	(%dot-id ".program-options"))
	      (CFG.NO-GREETINGS		(%dot-id ".no-greetings"))
	      (CFG.LIBRARY-SOURCE-SEARCH-PATH	(%dot-id ".library-source-search-path"))
	      (CFG.LIBRARY-BINARY-SEARCH-PATH	(%dot-id ".library-binary-search-path"))
	      (CFG.STORE-DIRECTORY	(%dot-id ".store-directory"))
	      (CFG.MORE-FILE-EXTENSIONS	(%dot-id ".more-file-extensions"))
	      (CFG.RAW-REPL		(%dot-id ".raw-repl"))
	      (CFG.OUTPUT-FILE		(%dot-id ".output-file")))
	   #'(let-syntax
		 ((CFG.EXEC-MODE
		   (identifier-syntax
		    (_
		     (run-time-config-exec-mode ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-exec-mode! ?cfg ?val))))

		  (CFG.SCRIPT
		   (identifier-syntax
		    (_
		     (run-time-config-script ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-script! ?cfg ?val))))

		  (CFG.RCFILES
		   (identifier-syntax
		    (_
		     (run-time-config-rcfiles ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-rcfiles! ?cfg ?val))))

		  (CFG.LOAD-LIBRARIES
		   (identifier-syntax
		    (_
		     (run-time-config-load-libraries ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-load-libraries! ?cfg ?val))))

		  (CFG.PROGRAM-OPTIONS
		   (identifier-syntax
		    (_
		     (run-time-config-program-options ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-program-options! ?cfg ?val))))

		  (CFG.NO-GREETINGS
		   (identifier-syntax
		    (_
		     (run-time-config-no-greetings ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-no-greetings! ?cfg ?val))))

		  (CFG.LIBRARY-SOURCE-SEARCH-PATH
		   (identifier-syntax
		    (_
		     (run-time-config-library-source-search-path ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-library-source-search-path! ?cfg ?val))))

		  (CFG.LIBRARY-BINARY-SEARCH-PATH
		   (identifier-syntax
		    (_
		     (run-time-config-library-binary-search-path ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-library-binary-search-path! ?cfg ?val))))

		  (CFG.STORE-DIRECTORY
		   (identifier-syntax
		    (_
		     (run-time-config-store-directory ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-store-directory! ?cfg ?val))))

		  (CFG.MORE-FILE-EXTENSIONS
		   (identifier-syntax
		    (_
		     (run-time-config-more-file-extensions ?cfg))
		    ((set! _ ?val)
		     (set-run-time-config-more-file-extensions! ?cfg ?val))))

		  (CFG.RAW-REPL
		   (identifier-syntax
		    (run-time-config-raw-repl ?cfg)))

		  (CFG.OUTPUT-FILE
		   (identifier-syntax
		    (run-time-config-output-file ?cfg))))
	       . ?body)))))))


(define (parse-command-line-arguments)
  ;;From the command line we want to extract the following informations:
  ;;
  ;;* The execution mode: interactive REPL, R6RS program, eval script, compilation of
  ;;a program, compilation of a library, compilation of a script's dependencies.  The
  ;;default is: interactive REPL.
  ;;
  ;;* The main program script, if any.
  ;;
  ;;* A list of run-command files to be executed, if any.
  ;;
  ;;* A list of auxiliary libraries to be instantiated, if any.
  ;;
  ;;* A list of options to be handed to the main script as arguments.
  ;;
  ;;The options for Vicare itself (debugging, logging, etc) are parsed and a thunk is
  ;;assembled to initialise the associated global state.
  ;;
  ;;Return  two values:  a  RUN-TIME-CONFIG structure,  a thunk  to  be evaluated  to
  ;;configure the global state for the selected execution mode.
  ;;
  (define cfg
    (make-run-time-config #f		;exec-mode
			  #f		;script
			  #t		;rcfiles
			  '()		;load-libraries
			  '()		;program-options
			  #f		;no-greetings
			  '()		;library-source-search-path
			  '()		;library-binary-search-path
			  #f		;store-directory
			  #f		;more-file-extensions
			  #f		;raw-repl
			  #f		;output-file
			  ))

  (let next-option ((args	(command-line-arguments))
		    (k		void))
    (define-syntax %option=
      (syntax-rules ()
	((_ ?opt)
	 (string=? (car args) ?opt))
	((_ ?opt . ?opts)
	 (or (%option= ?opt)
	     (%option= . ?opts)))))

    (define (%return cfg k)
      (let ((mode	(run-time-config-exec-mode cfg))
	    (rcfiles	(run-time-config-rcfiles   cfg)))
	(set-run-time-config-exec-mode! cfg (or mode 'repl))
	(when (list? rcfiles)
	  (set-run-time-config-rcfiles!    cfg (reverse rcfiles))))
      (set-run-time-config-load-libraries! cfg (reverse (run-time-config-load-libraries cfg)))
      (values cfg k))

    (cond ((null? args)
	   (%return cfg k))

	  ((%option= "-h" "--help")
	   (print-help-screen)
	   (exit 0))

	  ((%option= "-V" "--version")
	   (print-version-screen)
	   (exit 0))

	  ((%option= "--version-only")
	   (print-version-only)
	   (exit 0))

	  ((%option= "--license")
	   (print-license-screen)
	   (exit 0))

;;; --------------------------------------------------------------------
;;; execution modes

	  ((%option= "--r6rs-script")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --r6rs-script requires a script name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit "option --r6rs-script given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'r6rs-script)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

	  ((%option= "--binary-program")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --binary-program requires a program name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit "option --binary-program given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'binary-program)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

	  ((%option= "--compile-library")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --compile-library requires a library name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit
		   "option --compile-library given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'compile-library)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

	  ((%option= "--compile-dependencies")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --compile-dependencies requires a script name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit
		   "option --compile-dependencies given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'compile-dependencies)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

	  ((%option= "--compile-program")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --compile-program requires a script name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit
		   "option --compile-program given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'compile-program)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

	  ((%option= "-c" "--compile")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option -c or --compile requires a file name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit
		   "option -c or --compile given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'compile-something)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

;;; --------------------------------------------------------------------
;;; Vicare options without argument

	  ((%option= "-d" "-g" "--debug")
	   (option.debug-mode-enabled? #t)
	   (next-option (cdr args) (lambda () (k) (compiler.$generate-debug-calls #t))))

	  ((%option= "-nd" "--no-debug")
	   (option.debug-mode-enabled? #f)
	   (next-option (cdr args) (lambda () (k) (compiler.$generate-debug-calls #f))))

	  ((%option= "--gc-integrity-checks")
	   (next-option (cdr args) (lambda () (k) (foreign-call "ikrt_enable_gc_integrity_checks"))))

	  ((%option= "--no-gc-integrity-checks")
	   (next-option (cdr args) (lambda () (k) (foreign-call "ikrt_disable_gc_integrity_checks"))))

	  ((%option= "--no-greetings")
	   (set-run-time-config-no-greetings! cfg #t)
	   (next-option (cdr args) k))

	  ((%option= "--print-assembly")
	   (next-option (cdr args) (lambda () (k) (compiler.$assembler-output #t))))

	  ((%option= "--print-optimizer" "--print-optimiser")
	   (next-option (cdr args) (lambda () (k) (compiler.$optimizer-output #t))))

	  ((%option= "--no-rcfile")
	   (run-time-config-rcfiles-register! cfg #f)
	   (next-option (cdr args) k))

	  ((%option= "--more-file-extensions")
	   (set-run-time-config-more-file-extensions! cfg #t)
	   (next-option (cdr args) k))

	  ((%option= "--raw-repl")
	   (set-run-time-config-raw-repl! cfg #t)
	   (next-option (cdr args) k))

	  ((%option= "--print-loaded-libraries")
	   (option.print-loaded-libraries? #t)
	   (next-option (cdr args) k))

	  ((%option= "--no-print-loaded-libraries")
	   (option.print-loaded-libraries? #f)
	   (next-option (cdr args) k))

	  ((%option= "--debug-messages")
	   (option.print-debug-messages? #t)
	   (next-option (cdr args) k))

	  ((%option= "--no-debug-messages")
	   (option.print-debug-messages? #f)
	   (next-option (cdr args) k))

	  ((%option= "-v" "--verbose")
	   (option.verbose? #t)
	   (next-option (cdr args) k))

	  ((%option= "--silent")
	   (option.verbose? #f)
	   (next-option (cdr args) k))

	  ((%option= "--report-errors-at-runtime")
	   (option.report-errors-at-runtime #t)
	   (next-option (cdr args) k))

	  ((%option= "--no-report-errors-at-runtime")
	   (option.report-errors-at-runtime #f)
	   (next-option (cdr args) k))

	  ((%option= "--strict-r6rs")
	   (option.strict-r6rs #t)
	   (next-option (cdr args) k))

	  ((%option= "--no-strict-r6rs")
	   (option.strict-r6rs #f)
	   (next-option (cdr args) k))

	  ((%option= "--descriptive-labels")
	   (option.descriptive-labels #t)
	   (next-option (cdr args) k))

	  ((%option= "--no-descriptive-labels")
	   (option.descriptive-labels #f)
	   (next-option (cdr args) k))

;;; --------------------------------------------------------------------
;;; Vicare options with argument

	  ((%option= "-o" "--output")
	   (if (null? (cdr args))
	       (%error-and-exit "--output requires a file name argument")
	     (begin
	       (set-run-time-config-output-file! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "--rcfile")
	   (if (null? (cdr args))
	       (%error-and-exit "--rcfile requires a file name argument")
	     (begin
	       (run-time-config-rcfiles-register! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "-l" "--load-library")
	   (if (null? (cdr args))
	       (%error-and-exit "-l or --eval-script requires a file name argument")
	     (begin
	       (run-time-config-load-libraries-register! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "--source-path")
	   (if (null? (cdr args))
	       (%error-and-exit "--source-path requires a directory name")
	     (begin
	       (run-time-config-library-source-search-path-register! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "-L" "--library-path")
	   (if (null? (cdr args))
	       (%error-and-exit "--library-path requires a directory name")
	     (begin
	       (run-time-config-library-binary-search-path-register! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "--store-directory")
	   (if (null? (cdr args))
	       (%error-and-exit "--store-directory requires a directory name")
	     (begin
	       (set-run-time-config-store-directory! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "--prompt")
	   (if (null? (cdr args))
	       (%error-and-exit "--prompt requires a string argument")
	     (let ((prompt (cadr args)))
	       (next-option (cddr args) (lambda () (k) (waiter-prompt-string prompt))))))

	  ((%option= "--library-locator")
	   (if (null? (cdr args))
	       (%error-and-exit "--library-locator requires a locator name")
	     (let ((name (cadr args)))
	       (load.current-library-locator
		(cond ((string=? name "run-time")
		       load.run-time-library-locator)
		      ((string=? name "compile-time")
		       load.compile-time-library-locator)
		      ((string=? name "source")
		       load.source-library-locator)
		      (else
		       (%error-and-exit "invalid library location selection"))))
	       (next-option (cddr args) k))))

;;; --------------------------------------------------------------------
;;; compiler options with argument

	  ;; ((%option= "--compiler-letrec-pass")
	  ;;  (if (null? (cdr args))
	  ;;      (%error-and-exit "--compiler-letrec-pass requires a mode argument")
	  ;;    (begin
	  ;;      (guard (E (else
	  ;; 		  (%error-and-exit "invalid argument to --compiler-letrec-pass")))
	  ;; 	 ($current-letrec-pass (string->symbol (cadr args))))
	  ;;      (next-option (cddr args) k))))

	  ((%option= "--optimizer-passes-count")
	   (if (null? (cdr args))
	       (%error-and-exit "--optimizer-passes-count requires a number argument")
	     (begin
	       (guard (E (else
			  (%error-and-exit "invalid argument to --optimizer-passes-count")))
		 (compiler.$source-optimizer-passes-count (string->number (cadr args))))
	       (next-option (cddr args) k))))

;;; --------------------------------------------------------------------
;;; compiler options without argument

	  ((%option= "-O3")
	   (next-option (cdr args) (lambda () (k) (compiler.optimize-level 3))))

	  ((%option= "-O2")
	   (next-option (cdr args) (lambda () (k) (compiler.optimize-level 2))))

	  ((%option= "-O1")
	   (next-option (cdr args) (lambda () (k) (compiler.optimize-level 1))))

	  ((%option= "-O0")
	   (next-option (cdr args) (lambda () (k) (compiler.optimize-level 0))))

	  ((%option= "--enable-open-mvcalls")
	   (next-option (cdr args) (lambda () (k) (compiler.$open-mvcalls #t))))

	  ((%option= "--disable-open-mvcalls")
	   (next-option (cdr args) (lambda () (k) (compiler.$open-mvcalls #f))))

;;; --------------------------------------------------------------------
;;; program options

	  ((%option= "--")
	   (set-run-time-config-program-options! cfg (cdr args))
	   (%return cfg k))

	  (else
	   (let ((X (car args)))
	     (cond ((char=? #\- (string-ref X 0))
		    (%error-and-exit (string-append "unknown option \"" X "\"")))
		   ((run-time-config-exec-mode cfg)
		    (%error-and-exit "program file name given after mode option"))
		   (else
		    (set-run-time-config-exec-mode! cfg 'r6rs-script)
		    (set-run-time-config-script!    cfg X)
		    (next-option (cdr args) k))))))))


;;;; greetings screen

(define (%print-greetings cfg)
  (with-run-time-config (cfg)
    (unless cfg.no-greetings
      (print-greetings-screen))))

(define (print-greetings-screen)
  ;;Print text to give informations at the start of the REPL.
  ;;
  (define port (current-output-port))
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%newline)
    (newline port))
  (%display "Vicare Scheme version ")
  (%display config.vicare-version)
  (boot.case-word-size
   ((32)
    (%display ", 32-bit"))
   ((64)
    (%display ", 64-bit")))
  (%newline)
  ;;Print the git branch and HEAD commit checksum.
  (let ((rev (foreign-call "ikrt_get_last_revision")))
    (unless (zero? (bytevector-length rev))
      (%display "Revision ")
      (%display (ascii->string rev))
      (%newline)))
  (%display "Build ")
  ;;This  LET-SYNTAX looks  weird, but  it  is to  take the  DATE-STRING
  ;;result at expansion-time rather than run-time.
  (let-syntax ((ds (lambda (stx) (date-string))))
    (%display ds))
  (%newline)
  (%display "
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors
Copyright (c) 2011-2015 Marco Maggi\n\n"))

(define (print-version-screen)
  ;;Print the version screen.
  ;;
  (print-greetings-screen)
  (display "\
This is free software; see the  source or use the '--license' option for
copying conditions.  There is NO warranty; not  even for MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.\n\n" (current-output-port))
  (flush-output-port (current-output-port)))

(define (print-license-screen)
  (define port (current-output-port))
  (print-greetings-screen)
  (display "\
This file  is free software you  can redistribute it  and/or modify it
under the terms of the GNU  General Public License as published by the
Free Software  Foundation; either version  3, or (at your  option) any
later version.

This  file is  distributed in  the hope  that it  will be  useful, but
WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
General Public License for more details.

You  should have received  a copy  of the  GNU General  Public License
along with this file; see the file COPYING.  If not, write to the Free
Software Foundation,  Inc., 59  Temple Place -  Suite 330,  Boston, MA
02111-1307, USA.\n\n" port)
  (flush-output-port port))

(define (print-version-only)
  (define port (current-output-port))
  (display config.vicare-version port)
  (newline port)
  (flush-output-port port))


(define (print-help-screen)
  (display (string-append
	    "Vicare Scheme, a compiler for R6RS Scheme.
Usage:

vicare [OPTIONS] [FILENAME]                     [-- [PROGRAM OPTS]]
vicare [OPTIONS] --r6rs-script PROGRAM          [-- [PROGRAM OPTS]]
vicare [OPTIONS] --binary-program PROGRAM       [-- [PROGRAM OPTS]]
vicare [OPTIONS] --compile-library LIBFILE
vicare [OPTIONS] --compile-dependencies PROGRAM
vicare [OPTIONS] --compile-program PROGRAM
vicare [OPTIONS] --compile FILE

the  OPTIONS are  interpreted by  vicare, PROGRAM  OPTS can  be obtained
using the COMMAND-LINE procedure in the (rnrs programs) library.

Options controlling execution modes:

   --r6rs-script PROGRAM
        Start Vicare  in R6RS-script mode.  The PROGRAM  file is handled
       	as an R6RS program.

   --binary-program PROGRAM
        Start  Vicare in  compiled-program  mode.  The  PROGRAM file  is
       	handled as a precompiled R6RS program: loaded and executed.

   --compile-library LIBFILE
        Load the  R6RS library source  LIBFILE, compile it and  save the
        result in the FASL repository.

   --compile-dependencies PROGRAM
        Load  the R6RS program  PROGRAM, compile all the  libraries upon
	which it depends  and save them in the FASL repository.  PROGRAM
	itself is not evaluated.

   --compile-program PROGRAM
        Load  the R6RS program  PROGRAM, compile it and store it as FASL
        file.  PROGRAMitself is not evaluated.

   --compile FILE
        Load the  selected file, recognise  it as program or  library by
	the file  extension (.sps or .sls),  compile it and store  it as
	FASL file.

When none  of these options is given,  but a FILENAME is  present on the
command line: act as if the  --r6rs-script option had been used with the
given file name.   If no FILENAME is present on  the command line: enter
the REPL.

Other options:

   -b BOOTFILE
   --boot BOOTFILE
        Select the boot image.  The default is " config.bootfile "

   --no-rcfile
        Disable loading of run-command files.

   -o OFILE
   --output OFILE
        Select the pathname of the output file.

   --rcfile RCFILE
        Load and evaluate  RCFILE as an R6RS program  at startup, before
	loading libraries and running the main script.  This  option can
        be used multiple times.

   -l LIBFILE
   --load-library LIBFILE
        Load LIBFILE expecting it to contain one or more R6RS libraries;
        after executing the RC files, load the libraries in the internal
        collection but do not add  them to any environment.  This option
        can be used multiple times.

   --no-greetings
        Suppress greetings when entering the REPL.

   -S DIRECTORY
   --source-path DIRECTORY
        Add DIRECTORY  to the library  search path.  This option  can be
        used multiple times.

   -L DIRECTORY
   --library-path DIRECTORY
        Add DIRECTORY to the FASL search path.  This option can  be used
        multiple times.

   --store-directory DIRECTORY
        Select  DIRECTORY as  pathname  under  which compiled  libraries
        files are temporarily stored  before being installed.  When used
        multiple times: the last one wins.

   --more-file-extensions
        Rather   than    searching   only   libraries   with   extension
        \".vicare.sls\"  and \".sls\",  search also  for \".vicare.ss\",
        \".ss\", \".vicare.scm\", \".scm\" and the \"main\" file.

   --prompt STRING
        Use STRING as prompt for the REPL.  Defaults to \"vicare\".

   --repl-on-sigint
        When  this option  is  used an  interprocess  signal handler  is
	registered at program startup to enter a debugging REPL whenever
	a SIGINT signal is received.

   --raw-repl
	Do not create a readline console input port even if the readline
	interface is available.

   -d
   -g
   --debug
        Turn  on debugging  mode.  Unhandled  exceptions in  the program
	will result  in starting the debugger, which  allows stack trace
	inspection.

   -nd
   --no-debug
        Turn off debugging mode.

   --gc-integrity-checks
        Enable garbage collection integrity checks.  This slows down the
        garbage collection.

   --no-gc-integrity-checks
        Disable   garbage   collection integrity   checks.  This  is the
        default.

   --print-loaded-libraries
        Whenever a library file is loaded print a message on the console
        error port.  This is for debugging purposes.

   --no-print-loaded-libraries
        Disables the effect of --print-loaded-libraries.

   --debug-messages
        Be more verbose aboud undertaken actions.  This is for debugging
        purposes.

   --no-debug-messages
        Disables the effect of --debug-messages.

   --report-errors-at-runtime
        When possible  and meaningful:  report errors at  runtime rather
        than  at  compile  time.    Runtime  errors  reporting  is  R6RS
        compliant.  The default is to raise errors at compile time.

   --no-report-errors-at-runtime
        Disables the effect of --report-errors-at-runtime.

   --strict-r6rs
        Strictly follow R6RS specifications: disable Vicare extensions.

   --no-strict-r6rs
        Do  not  strictly  follow  R6RS  specifications:  enable  Vicare
        extensions.  Disables the effect  of --strict-r6rs.  This is the
        default.

   --descriptive-labels
        For debugging purposes: generate  descriptive labels in expanded
        and assembly code.

   --no-descriptive-labels
        For debugging  purposes: do  not generate descriptive  labels in
        expanded  code  and  assembly code.  Disables   the   effect  of
        --descriptive-labels.  This is the default.

   --library-locator NAME
        Select a  library  locator.  NAME can  be one  among:  run-time,
        compile-time, source.

   -O0
        Turn off the source optimizer.

   -O1
   -O2
   -O3
        Turn on various levels of compiler optimisations.

   --optimizer-passes-count COUNT
        Specify how  many passes to  perform with the  source optimizer.
        Must be a positive fixnum.  Defaults to 1.

   --enable-open-mvcalls
   --disable-open-mvcalls
        Enable  or  disable  inlining   of  calls  to  CALL-WITH-VALUES.
        Defaults to disable.

   --print-assembly
        Print  to  the  current  error port  the  assembly  instructions
	generated when compiling code.

   --print-optimizer
   --print-optimiser
        Print  to the  current error  port a  symbolic  expression which
        results from running the optimiser.

   -v
   --verbose
        Enable verbose messages.

   --silent
        Disable verbose messages.
   -V
   --version
       Print version message on stderr then exit.

   --version-only
        Print only  the version number  on stdout followed by  a newline
        then exit.

   --license
       Print license message on stderr then exit.

   -h
   --help
       Print this help message on stderr then exit.

Consult Vicare Scheme User's Guide for more details.\n\n")
	   (current-output-port))
  (flush-output-port (current-output-port)))


;;;; code evaluation driver

(define-syntax doit
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (start (lambda () ?body0 ?body ...)))))

(define (start proc)
  (if (compiler.$generate-debug-calls)
      (debugger.guarded-start proc)
    (proc)))


;;;; before-the-main-action code evaluation procedures

(define (load-rc-files-as-r6rs-scripts cfg)
  ;;Load the RC files as R6RS scripts and discard the resulting environment.
  ;;
  ;;When  CFG.RCFILES  is  #t: if  the  execution  mode  is  REPL, load  the  default
  ;;run-command file "~/.vicarerc".
  ;;
  ;;When CFG.RCFILES is #f: do nothing.
  ;;
  ;;When CFG.RCFILES  is not  a boolean: it  must be a  list of  strings representing
  ;;run-command file pathnames.  The files are loaded an evaluated as R6RS scripts.
  ;;
  (for-each
      (lambda (filename)
	(with-exception-handler
	    (lambda (E)
	      (raise-continuable
	       (condition (make-who-condition 'vicare)
			  (make-message-condition
			   (string-append "failed loading run-commmand file: " filename))
			  E)))
	  (lambda ()
	    (load-r6rs-script filename (serialise? #f) (run? #t)))))
    (with-run-time-config (cfg)
      (case cfg.rcfiles
	((#t)
	 (cond ((and (eq? 'repl cfg.exec-mode)
		     (posix.getenv "HOME"))
		=> (lambda (home)
		     (if (string-empty? home)
			 '()
		       (let ((f (string-append home "/.vicarerc")))
			 (if (file-exists? f)
			     (list f)
			   '())))))
	       (else '())))
	((#f)
	 '())
	(else
	 cfg.rcfiles)))))

(define (load-libraries cfg)
  ;;Load the  library files selected  on the command line.   Notice that
  ;;there is only one internal collection of loaded libraries.
  ;;
  (with-run-time-config (cfg)
    (doit (for-each (lambda (source-filename)
		      (for-each (lambda (library-form)
				  (parametrise ((psyntax.source-code-location source-filename))
				    ((psyntax.current-library-expander) library-form)))
			(read-source-file source-filename)))
	    cfg.load-libraries))))


;;;; main action procedures

(define (load-r6rs-program cfg)
  ;;Execute  the  selected main  script  as  R6RS  program.  Return  the
  ;;resulting environment.
  ;;
  (with-run-time-config (cfg)
    (doit (load-r6rs-script cfg.script (serialise? #f) (run? #t)))))

(define (run-compiled-program cfg)
  (with-run-time-config (cfg)
    (doit (load.run-compiled-program cfg.script))))

;;; --------------------------------------------------------------------

(define (compile-dependencies cfg)
  (with-run-time-config (cfg)
    (doit (load-r6rs-script cfg.script (serialise? #t) (run? #f)))))

(define (compile-program cfg)
  (with-run-time-config (cfg)
    (doit (load.compile-source-program cfg.script cfg.output-file))))

(define (compile-library cfg)
  (with-run-time-config (cfg)
    (doit (load.compile-source-library cfg.script cfg.output-file))))

(module (compile-something)
  ;;Compile either  a program  or a library  depending on the  file extension  of the
  ;;selected main file.
  ;;
  (define* (compile-something cfg)
    (with-run-time-config (cfg)
      (case-file-type-from-extension cfg.script
	((library)
	 (compile-library cfg))
	((program)
	 (compile-program cfg))
	(else
	 (raise
	  (condition (make-who-condition __who__)
		     (make-message-condition "cannot determine type of source file to compile (library or program)")
		     (make-i/o-filename-error cfg.script)
		     (make-irritants-condition (list cfg.script))))))))

  (module (case-file-type-from-extension program library)

    (define-auxiliary-syntaxes program library)

    (define-syntax case-file-type-from-extension
      (syntax-rules (program library else)
	((_ ?pathname
	    ((library) . ?library-body)
	    ((program) . ?program-body)
	    (else      . ?else-body))
	 (let ((pathname ?pathname))
	   (cond ((%string-suffix? pathname ".sls")	. ?library-body)
		 ((%string-suffix? pathname ".sps")	. ?program-body)
		 (else					. ?else-body))))
	))

    (define (%string-suffix? str suffix)
      (let ((str.len    (string-length str))
	    (suffix.len (string-length suffix)))
	(and (fx< suffix.len str.len)
	     (string=? suffix (substring str (fx- str.len suffix.len) str.len)))))

    #| end of module: CASE-FILE-TYPE-FROM-EXTENSION |# )

  #| end of module: COMPILE-SOMETHING |# )


;;;; some basic initialisation

(module ()
  ;;See "ikarus.symbol-table.sls"  for an explanation of  this.  Nothing
  ;;must be executed before the initialisation of the symbol table.
  ($initialize-symbol-table!)
  ;;See "ikarus.strings.table.sls".
  ($initialize-interned-strings-table!)

  (psyntax.initialise-type-spec-for-built-in-object-types)
  (psyntax.initialise-core-prims-tagging)

  #| end of module |# )


;;;; automatic struct finalisation

(module ($struct-guardian struct-guardian-logger struct-guardian-log)
  (define %struct-guardian
    (make-guardian))

  (define ($struct-guardian S)
    (let ((logger (struct-guardian-logger)))
      (cond ((procedure? logger)
	     (guard (E (else (void)))
	       (logger S #f 'registration))
	     (%struct-guardian S))
	    (logger
	     (guard (E (else (void)))
	       (struct-guardian-log S #f 'registration))
	     (%struct-guardian S))
	    (else
	     (%struct-guardian S)))))

  (define struct-guardian-logger
    (make-parameter #f
      (lambda (obj)
	(cond ((or (boolean? obj)
		   (procedure? obj))
	       obj)
	      (obj	#t)
	      (else	#f)))))

  (define (struct-guardian-log S E action)
    (case action
      ((registration)
       (fprintf (current-error-port)
		"*** Vicare debug: struct guardian: registered struct:\n\
                 ***\t~s\n" S))
      ((before-destruction)
       (fprintf (current-error-port)
		"*** Vicare debug: struct guardian: before destruction:\n\
                 ***\t~s\n" S))
      ((after-destruction)
       (fprintf (current-error-port)
		"*** Vicare debug: struct guardian: after destruction:\n\
                 ***\t~s\n" S))
      ((exception)
       (fprintf (current-error-port)
		"*** Vicare debug: struct guardian: exception:\n\
                 ***\t~s\n\
                 ***\t~s\n" S E))
      (else
       (assertion-violation 'struct-guardian-log
	 "invalid action in struct destruction process" S action))))

  (define FIELD-INDEX-OF-DESTRUCTOR-IN-STD 5)

  (define (%struct-guardian-destructor)
    (guard (E (else (void)))
      (define-syntax-rule (%execute ?S ?body0 . ?body)
	(do ((?S (%struct-guardian) (%struct-guardian)))
	    ((not ?S))
	  ?body0 . ?body))
      (define-inline (%extract-destructor S)
	($struct-ref ($struct-rtd S) FIELD-INDEX-OF-DESTRUCTOR-IN-STD))
      (define-inline (%call-logger ?logger ?struct ?exception ?action)
	(guard (E (else (void)))
	  (?logger ?struct ?exception ?action)))
      (let ((logger (struct-guardian-logger)))
	(cond ((procedure? logger)
	       (%execute S
		 (guard (E (else
			    (%call-logger logger S E 'exception)))
		   (%call-logger logger S #f 'before-destruction)
		   ((%extract-destructor S) S)
		   (%call-logger logger S #f 'after-destruction)
		   (struct-reset S))))
	      (logger
	       (%execute S
		 (guard (E (else
			    (%call-logger struct-guardian-log S E 'exception)))
		   (%call-logger struct-guardian-log S #f 'before-destruction)
		   ((%extract-destructor S) S)
		   (%call-logger struct-guardian-log S #f 'after-destruction)
		   (struct-reset S))))
	      (else
	       (%execute S
		 (guard (E (else (void)))
		   ((%extract-destructor S) S)
		   (struct-reset S))))))))

  (post-gc-hooks (cons %struct-guardian-destructor (post-gc-hooks)))

  #| end of module |# )


;;;; automatic R6RS records finalisation

(module ($record-guardian record-guardian-logger record-guardian-log)

  ;;Whenever a  record instance, with destructor  function registered in
  ;;its descriptor, is created: it is  registered in this guardian to be
  ;;destroyed appropriately.
  (define %record-guardian
    (make-guardian))

  (define ($record-guardian S)
    ;;Wrapper for  %RECORD-GUARDIAN that  handles the invocation  of the
    ;;logger function at record instantiation time.
    ;;
    (let ((logger (record-guardian-logger)))
      (cond ((procedure? logger)
	     (guard (E (else (void)))
	       (logger S #f 'registration))
	     (%record-guardian S))
	    (logger
	     (guard (E (else (void)))
	       (record-guardian-log S #f 'registration))
	     (%record-guardian S))
	    (else
	     (%record-guardian S)))))

  ;;Parameter to select the current logger function.
  (define record-guardian-logger
    (make-parameter #f
      (lambda (obj)
	(cond ((or (boolean? obj)
		   (procedure? obj))
	       obj)
	      (obj	#t)
	      (else	#f)))))

  (define (record-guardian-log S E action)
    ;;Default logger function.   S must be the record  instance.  E must
    ;;be #f  or a condition object  raised because an error  occurred in
    ;;the destructor.  ACTION  must be a symbol selecting  the action to
    ;;perform.
    ;;
    (case action
      ((registration)
       (fprintf (current-error-port)
		"*** Vicare debug: record guardian: registered record:\n\
                 ***\t~s\n" S))
      ((before-destruction)
       (fprintf (current-error-port)
		"*** Vicare debug: record guardian: before destruction:\n\
                 ***\t~s\n" S))
      ((after-destruction)
       (fprintf (current-error-port)
		"*** Vicare debug: record guardian: after destruction:\n\
                 ***\t~s\n" S))
      ((exception)
       (fprintf (current-error-port)
		"*** Vicare debug: record guardian: exception:\n\
                 ***\t~s\n\
                 ***\t~s\n" S E))
      (else
       (assertion-violation 'record-guardian-log
	 "invalid action in record destruction process" S action))))

  (define FIELD-INDEX-OF-DESTRUCTOR-IN-RTD 12)

  (define (%record-guardian-destructor)
    ;;The finalisation  function called  to handle the  record instances
    ;;collected by the guardian.
    ;;
    (guard (E (else (void)))
      (define-syntax-rule (%execute ?S ?body0 . ?body)
	(do ((?S (%record-guardian) (%record-guardian)))
	    ((not ?S))
	  ?body0 . ?body))
      (define (%extract-destructor S)
	($struct-ref ($struct-rtd S) FIELD-INDEX-OF-DESTRUCTOR-IN-RTD))
      (define (%call-logger logger record exception action)
	(guard (E (else (void)))
	  (logger record exception action)))
      (let ((logger (record-guardian-logger)))
	(cond ((procedure? logger)
	       (%execute S
		 (guard (E (else
			    (%call-logger logger S E 'exception)))
		   (%call-logger logger S #f 'before-destruction)
		   ((%extract-destructor S) S)
		   (%call-logger logger S #f 'after-destruction)
		   (record-reset S))))
	      (logger
	       (%execute S
		 (guard (E (else
			    (%call-logger record-guardian-log S E 'exception)))
		   (%call-logger record-guardian-log S #f 'before-destruction)
		   ((%extract-destructor S) S)
		   (%call-logger record-guardian-log S #f 'after-destruction)
		   (record-reset S))))
	      (else
	       (%execute S
		 (guard (E (else (void)))
		   ((%extract-destructor S) S)
		   (record-reset S))))))))

  (post-gc-hooks (cons %record-guardian-destructor (post-gc-hooks)))

  #| end of module |# )


;;;; main expressions

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.main here"))

(receive (cfg execution-state-initialisation-according-to-command-line-options)
    (parse-command-line-arguments)

  (with-run-time-config (cfg)
    (execution-state-initialisation-according-to-command-line-options)

    ;;If  a library  locator has  already been  selected (perhaps  by a  command line
    ;;option): accept it.  Otherwise explicitly select one.
    (load.current-library-locator
     (cond ((load.current-library-locator))
	   ((memq cfg.exec-mode '(compile-library compile-program compile compile-dependencies))
	    load.compile-time-library-locator)
	   (else
	    load.run-time-library-locator)))

    ;;Initialise search paths and library directories.
    ;;
    ;;We  must initialise  first  the  library locator,  then  the  search paths  and
    ;;directories.
    ;;
    (libutils.init-search-paths-and-directories (reverse cfg.library-source-search-path)
						(reverse cfg.library-binary-search-path)
						cfg.store-directory
						cfg.more-file-extensions)

    ;;Initialise the command line arguments.
    (cond ((eq? 'repl cfg.exec-mode)
	   (command-line-arguments (cons "*interactive*" cfg.program-options)))
	  (cfg.script
	   (command-line-arguments (cons cfg.script      cfg.program-options))))

    (when (and (readline.readline-enabled?) (not cfg.raw-repl))
      (cafe-input-port (readline.make-readline-input-port)))

    ;;Evaluate code before the main action.
    (load-rc-files-as-r6rs-scripts cfg)
    (load-libraries cfg)

    ;;Perform the main action.
    (case cfg.exec-mode
      ((r6rs-script)
       (load-r6rs-program cfg))

      ((binary-program)
       (run-compiled-program cfg))

      ((compile-dependencies)
       (compile-dependencies cfg))

      ((compile-program)
       (compile-program cfg))

      ((compile-library)
       (compile-library cfg))

      ((compile-something)
       (compile-something cfg))

      ((repl)
       (%print-greetings cfg)
       (new-cafe (lambda (x)
		   (doit (eval x (interaction-environment))))))

      (else
       (assertion-violation 'vicare
	 "Vicare internal error: invalid execution mode" cfg.exec-mode))))

  (exit 0))


;;;; done

#| end of library (ikarus main) |# )

;;; end of file
;;Local Variables:
;;eval: (put 'with-run-time-config		'scheme-indent-function 1)
;;eval: (put '%execute				'scheme-indent-function 1)
;;eval: (put 'case-file-type-from-extension	'scheme-indent-function 1)
;;End:
