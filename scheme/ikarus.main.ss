;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;This is  here to test that  we can import things  from other libraries
;;within the compiler itself.
(library (ikarus startup)
  (export
    vicare-lib-dir
    vicare-version
    vicare-revision
    bootfile
    host-info)
  (import (except (ikarus)
		  host-info)
    (vicare include))
  (include "ikarus.config.ss"))


(library (ikarus main)
  (export)
  (import (except (ikarus)
		  load-r6rs-script
		  load
		  host-info)
    (prefix (ikarus startup)
	    config.)
    (only (ikarus.compiler)
	  generate-debug-calls)
    (only (ikarus.debugger)
	  guarded-start)
    (only (psyntax library-manager)
	  current-library-expander)
    (only (ikarus.reader)
	  read-source-file)
    (only (ikarus.symbol-table)
	  initialize-symbol-table!)
    (prefix (only (ikarus load)
		  load
		  load-r6rs-script)
	    loading.)
    (only (vicare syntactic-extensions)
	  define-inline))


;;;; helpers

(define (%error-and-exit msg)
  (let ((port (current-error-port)))
    (display "*** vicare error: " port)
    (display msg port)
    (newline port)
    (exit 1)))

(define (%error-invalid-rc)
  (%error-and-exit "option --no-rcfile is invalid when used along with --rcfile"))

(define-syntax serialize?	(syntax-rules ()))
(define-syntax run?		(syntax-rules ()))

(define-syntax load-r6rs-script
  (syntax-rules (serialize? run?)
    ((_ ?filename (serialize? ?ser) (run? ?run))
     (loading.load-r6rs-script ?filename ?ser ?run))))


;;;; data types

(define-struct run-time-config
  (exec-mode
		;A  symbol representing  the  requested execution  mode:
		;R6RS-SCRIPT, SCRIPT, COMPILE, REPL.
   script
		;A  string representing  a file  name: the  main script.
		;When in  R6RS-SCRIPT or COMPILE  mode: it must  hold an
		;R6RS  program.  When  in script  mode: it  must  hold a
		;script.

   rcfiles	;#f,  #t, null or  a list  of strings  representing file
		;names.  When #f: avoid executing any run-command files;
		;when #t: load and  execute the default run-command file
		;as an  R6RS program;  when null or  a list  of strings:
		;load the listed files  to be evaluated as R6RS programs
		;before instantiating libraries.

   load-libraries
		;Null  or a  list  of strings  representing file  names:
		;libraries to be instantiated,  adding the result to the
		;interaction environment, after  the RC files and before
		;the load scripts.

   eval-files
		;Null  or a  list  of strings  representing file  names:
		;source code to  be loaded and handed to  EVAL under the
		;interaction  environment;  before  the main  script  is
		;evaluated, but after the libraries have been loaded.

   program-options
		;Null  or a  list of  strings representing  command line
		;options to  be handed  to the executed  program through
		;the COMMAND-LINE function.

   no-greetings
		;If  true:  avoid printing  the  greetings message  when
		;starting the REPL.

   search-path
		;Null or a list of strings representing directory names:
		;additional locations in which to search for libraries.
   ))

(define-inline (run-time-config-load-libraries-register! cfg pathname)
  (set-run-time-config-load-libraries! cfg (cons pathname (run-time-config-load-libraries cfg))))

(define-inline (run-time-config-eval-files-register! cfg pathname)
  (set-run-time-config-eval-files! cfg (cons pathname (run-time-config-eval-files cfg))))

(define-inline (run-time-config-search-path-register! cfg pathname)
  (set-run-time-config-search-path! cfg (cons pathname (run-time-config-search-path cfg))))

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
	      (CFG.EVAL-FILES		(%dot-id ".eval-files"))
	      (CFG.PROGRAM-OPTIONS	(%dot-id ".program-options"))
	      (CFG.NO-GREETINGS		(%dot-id ".no-greetings"))
	      (CFG.SEARCH-PATH		(%dot-id ".search-path")))
	   #'(let-syntax
		 ((CFG.EXEC-MODE	(identifier-syntax
					 (_
					  (run-time-config-exec-mode ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-exec-mode! ?cfg ?val))))
		  (CFG.SCRIPT		(identifier-syntax
					 (_
					  (run-time-config-script ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-script! ?cfg ?val))))
		  (CFG.RCFILES	(identifier-syntax
					 (_
					  (run-time-config-rcfiles ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-rcfiles! ?cfg ?val))))
		  (CFG.LOAD-LIBRARIES	(identifier-syntax
					 (_
					  (run-time-config-load-libraries ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-load-libraries! ?cfg ?val))))
		  (CFG.EVAL-FILES	(identifier-syntax
					 (_
					  (run-time-config-eval-files ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-eval-files! ?cfg ?val))))
		  (CFG.PROGRAM-OPTIONS	(identifier-syntax
					 (_
					  (run-time-config-program-options ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-program-options! ?cfg ?val))))
		  (CFG.NO-GREETINGS	(identifier-syntax
					 (_
					  (run-time-config-no-greetings ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-no-greetings! ?cfg ?val))))
		  (CFG.SEARCH-PATH	(identifier-syntax
					 (_
					  (run-time-config-search-path ?cfg))
					 ((set! _ ?val)
					  (set-run-time-config-search-path! ?cfg ?val)))))
	       . ?body)))))))


(define (parse-command-line-arguments)
  ;;From the command line we want to extract the following informations:
  ;;
  ;;* The execution mode: interactive REPL, R6RS program then REPL, R6RS
  ;;program, eval script, compilation  of dependencies.  The default is:
  ;;interactive REPL.
  ;;
  ;;* The main program script, if any.
  ;;
  ;;* A list of run-command files to be executed, if any.
  ;;
  ;;* A list of auxiliary scripts to be evaluated, if any.
  ;;
  ;;* A list of auxiliary libraries to be instantiated, if any.
  ;;
  ;;* A list of options to be handed to the main script as arguments.
  ;;
  ;;The options  for Vicare itself (debugging, logging,  etc) are parsed
  ;;and a thunk is assembled to initialise the associated global state.
  ;;
  ;;Return  two  values: a  RUN-TIME-CONFIG  structure,  a  thunk to  be
  ;;evaluated to  configure the global state for  the selected execution
  ;;mode.
  ;;
  (define cfg
    (make-run-time-config #f		;exec-mode
			  #f		;script
			  #f		;rcfiles
			  '()		;load-libraries
			  '()		;eval-files
			  '()		;program-options
			  #f		;no-greetings
			  '()))		;search-path

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
      (set-run-time-config-eval-files!     cfg (reverse (run-time-config-eval-files   cfg)))
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

	  ((%option= "--r6rs-repl")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --r6rs-repl requires a script name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit "option --r6rs-repl given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'r6rs-repl)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

	  ((%option= "--script")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --script requires a script name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit "option --script given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'script)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

	  ((%option= "--compile-dependencies")
	   (cond ((null? (cdr args))
		  (%error-and-exit "option --compile-dependencies requires a script name"))
		 ((run-time-config-exec-mode cfg)
		  (%error-and-exit "option --compile-dependencies given after other mode option"))
		 (else
		  (set-run-time-config-exec-mode! cfg 'compile)
		  (set-run-time-config-script!    cfg (cadr args))
		  (next-option (cddr args) k))))

;;; --------------------------------------------------------------------
;;; Vicare options

	  ((%option= "-d" "--debug")
	   (next-option (cdr args) (lambda () (k) (generate-debug-calls #t))))

	  ((%option= "-nd" "--no-debug")
	   (next-option (cdr args) (lambda () (k) (generate-debug-calls #f))))

	  ((%option= "--no-greetings")
	   (set-run-time-config-no-greetings! #t)
	   (next-option (cdr args) k))

	  ((%option= "--print-assembly")
	   (next-option (cdr args) (lambda () (k) (assembler-output #t))))

	  ((%option= "--print-optimizer" "--print-optimiser")
	   (next-option (cdr args) (lambda () (k) (optimizer-output #t))))

	  ((%option= "-O2")
	   (next-option (cdr args) (lambda () (k) (optimize-level 2))))

	  ((%option= "-O1")
	   (next-option (cdr args) (lambda () (k) (optimize-level 1))))

	  ((%option= "-O0")
	   (next-option (cdr args) (lambda () (k) (optimize-level 0))))

	  ((%option= "--no-rcfile")
	   (run-time-config-rcfiles-register! cfg #f)
	   (next-option (cdr args) k))

	  ((%option= "--rcfile")
	   (if (null? (cdr args))
	       (%error-and-exit "--rcfile requires a script name")
	     (begin
	       (run-time-config-rcfiles-register! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "-f" "--eval-file")
	   (if (null? (cdr args))
	       (%error-and-exit "-e or --eval-script requires a script name")
	     (begin
	       (run-time-config-eval-files-register! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "-l" "--load-library")
	   (if (null? (cdr args))
	       (%error-and-exit "-l or --eval-script requires a script name")
	     (begin
	       (run-time-config-load-libraries-register! cfg (cadr args))
	       (next-option (cddr args) k))))

	  ((%option= "-L" "--search-path")
	   (if (null? (cdr args))
	       (%error-and-exit "-L or --search-path requires a directory name")
	     (begin
	       (run-time-config-search-path-register! cfg (cadr args))
	       (next-option (cddr args) k))))

;;; --------------------------------------------------------------------
;;; program options

	  ((%option= "--")
	   (set-run-time-config-program-options! cfg (cdr args))
	   (%return cfg k))

	  (else	;error
	   (let ((X (car args)))
	     (cond ((char=? #\- (string-ref X 0))
		    (%error-and-exit (string-append "unknown option \"" X "\"")))
		   ((run-time-config-exec-mode cfg)
		    (%error-and-exit "program file name given after mode option"))
		   (else
		    (set-run-time-config-exec-mode! cfg 'r6rs-script)
		    (set-run-time-config-script!    cfg (car args))
		    (next-option (cddr args) k))))))))


(define (print-greetings-screen)
  ;;Print text to give informations at the start of the REPL.
  ;;
  (define port (current-error-port))
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%newline)
    (newline port))
  (%display "Vicare Scheme version ")
  (%display config.vicare-version)
  (unless (= 30 (fixnum-width))
    (%display ", 64-bit"))
  (%newline)
  (unless (zero? (string-length config.vicare-revision))
    (%display "Revision ")
    (%display config.vicare-revision)
    (%newline))
  (%display "Build ")
  ;;This  LET-SYNTAX looks  weird, but  it  is to  take the  DATE-STRING
  ;;result at expansion-time rather than run-time.
  (let-syntax ((ds (lambda (stx) (date-string))))
    (%display ds))
  (%newline)
  (%display "\
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors
Copyright (c) 2011 Marco Maggi
"))

(define (print-version-screen)
  ;;Print the version screen.
  ;;
  (print-greetings-screen)
  (display "\
This is free software; see the  source or use the '--license' option for
copying conditions.  There is NO warranty; not  even for MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.
" (current-error-port)))

(define (print-license-screen)
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
02111-1307, USA.
" (current-error-port)))

(define (print-version-only)
  (define port (current-error-port))
  (display config.vicare-version port)
  (newline port))


(define (print-help-screen)
  (display (string-append
	    "Vicare Scheme, a compiler for R6RS Scheme.
Usage:

vicare [OPTIONS] [FILENAME]                     [-- [PROGRAM OPTS]]
vicare [OPTIONS] --r6rs-script PROGRAM          [-- [PROGRAM OPTS]]
vicare [OPTIONS] --r6rs-repl PROGRAM            [-- [PROGRAM OPTS]]
vicare [OPTIONS] --script CODE                  [-- [PROGRAM OPTS]]
vicare [OPTIONS] --compile-dependencies PROGRAM [-- [PROGRAM OPTS]]

the  OPTIONS are  interpreted by  vicare, PROGRAM  OPTS can  be obtained
using the COMMAND-LINE procedure in the (rnrs programs) library.

Options controlling execution modes:

   --r6rs-script PROGRAM
        Start Vicare  in R6RS-script mode.  The PROGRAM  file is handled
       	as an R6RS program.

   --r6rs-repl PROGRAM
        Start Vicare  in R6RS-script mode.  Act as if  the --r6rs-script
	option had been  used but, after the script execution, enter the
	REPL rather than exiting.

   --script CODEFILE
        Start Vicare in  evaluation mode.  The CODEFILE is  handled as a
       	sequence of R6RS expressions: such expressions are used as first
       	argument for EVAL under the interaction environment.

   --compile-dependencies PROGRAM
        Load  the R6RS program  PROGRAM, compile all the  libraries upon
	which it depends  and save them in the FASL repository.  PROGRAM
	itself is not evaluated.

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

   --rcfile RCFILE
        Load and evaluate  RCFILE as an R6RS program  at startup, before
	loading libraries, evaluating codes and running the main script.
	This option can be used multiple times.

   -l LIBFILE
   --load-library LIBFILE
        Load LIBFILE expecting it to contain one or more R6RS libraries;
        after  executing  the  RC  files,  expand  and  instantiate  the
        libraries  adding  them to  the  interaction environment.   This
        option can be used multiple times.

   -f CODEFILE
   --eval-file CODEFILE
        Load CODEFILE  expecting it  to contain valid  R6RS expressions;
	after instantiating  the libraries hand  the code to  EVAL under
	the interaction environment.  This option can  be used  multiple
        times.

   --no-greetings
        Suppress greetings when entering the REPL.

   -L DIRECTORY
   --search-path DIRECTORY
        Add DIRECTORY  to the library  search path.  This option  can be
        used multiple times.

   -d
   --debug
        Turn  on debugging  mode.  Unhandled  exceptions in  the program
	will result  in starting the debugger, which  allows stack trace
	inspection.

   -nd
   --no-debug
        Turn off debugging mode.

   -O0
   -O1
   -O2
        Turn  on  various levels  of  compiler optimisations  (currently
        unsupported).

   --print-assembly
        Print  to  the  current  error port  the  assembly  instructions
	generated when compiling code.

   --print-optimizer
   --print-optimiser
        Print  to the  current error  port a  symbolic  expression which
        results from running the optimiser.

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

If neither the --no-rcfile nor the  --rcfile options are used: a list of
run-command files is read from the environment variable VICARE_RC_FILES,
which  must  contain  a  colon  separated list  of  pathnames.   If  the
enviroment variable is empty or unset, by default the file \".vicarerc\"
is  used  searching  for  it  in  the directory  selected  by  the  HOME
environment variable.

Consult Vicare Scheme User's Guide for more details.\n\n")
	   (current-error-port)))


(define (init-library-path cfg)
  (define (prefix ext ls)
    (append (map (lambda (x)
		   (string-append ext x))
	      ls)
	    ls))
  (with-run-time-config (cfg)
    (library-path (append cfg.search-path
			  (cond ((getenv "VICARE_LIBRARY_PATH")
				 => split-path)
				(else '(".")))
			  (list config.vicare-lib-dir)))
    (library-extensions (prefix "/main" (prefix ".vicare" (library-extensions))))))

(define (split-path s)
  (define (nodata i s ls)
    (cond ((= i (string-length s))
	   ls)
	  ((char=? #\: (string-ref s i))
	   (nodata (+ i 1) s ls))
	  (else
	   (data (+ i 1) s ls (list (string-ref s i))))))
  (define (data i s ls ac)
    (cond ((= i (string-length s))
	   (cons (list->string (reverse ac)) ls))
	  ((char=? (string-ref s i) #\:)
	   (nodata (+ i 1) s
		   (cons (list->string (reverse ac)) ls)))
	  (else
	   (data (+ i 1) s ls (cons (string-ref s i) ac)))))
  (reverse (nodata 0 s '())))


(define (load-rc-files-as-r6rs-scripts cfg)
  ;;Load  the  RC  files  as  R6RS scripts  and  discard  the  resulting
  ;;environment.
  ;;
  (for-each
      (lambda (filename)
	(with-exception-handler
	    (lambda (E)
	      (raise-continuable
	       (condition (make-who-condition 'vicare)
			  (make-message-condition
			   (string-append "loading rc file " filename " failed"))
			  E)))
	  (lambda ()
	    (load-r6rs-script filename (serialize? #f) (run? #t)))))
    (with-run-time-config (cfg)
      (case cfg.rcfiles
	((#t)
	 (cond ((getenv "VICARE_RC_FILES")
		=> split-path)
	       ((getenv "HOME")
		=> (lambda (home)
		     (let ((f (string-append home "/.vicarerc")))
		       (if (file-exists? f)
			   (list f)
			 '()))))
	       (else '())))
	((#f)
	 '())
	(else
	 cfg.rcfiles)))))


(define-syntax doit
  (syntax-rules ()
    ((_ e e* ...)
     (start (lambda () e e* ...)))))

(define (start proc)
  (if (generate-debug-calls)
      (guarded-start proc)
    (proc)))

(define (load-libraries cfg)
  ;;Load the  library files selected  on the command line.   Notice that
  ;;there is only one internal collection of loaded libraries.
  ;;
  (with-run-time-config (cfg)
    (doit (for-each (lambda (filename)
		      (for-each (lambda (library-form)
				  ((current-library-expander) library-form))
			(read-source-file filename)))
	    cfg.load-libraries))))

(define (load-eval-files cfg)
  (with-run-time-config (cfg)
    (doit (for-each loading.load cfg.eval-files))))

(define (load-r6rs-program cfg)
  ;;Execute  the  selected main  script  as  R6RS  program.  Return  the
  ;;resulting environment.
  ;;
  (with-run-time-config (cfg)
    (doit (load-r6rs-script cfg.script (serialize? #f) (run? #t)))))

(define (compile-dependencies cfg)
  (with-run-time-config (cfg)
    (doit (load-r6rs-script cfg.script (serialize? #t) (run? #f)))))

(define (load-evaluated-script cfg)
  (with-run-time-config (cfg)
    (doit (loading.load cfg.script))))


;;;; main expressions

(initialize-symbol-table!)
(let-values (((cfg execution-state-initialisation-according-to-command-line-options)
	      (parse-command-line-arguments)))
  (with-run-time-config (cfg)
    (define-inline (%print-greetings)
      (unless cfg.no-greetings
	(print-greetings-screen)))

    (init-library-path cfg)
    (load-rc-files-as-r6rs-scripts cfg)

    (execution-state-initialisation-according-to-command-line-options)
    ;;Added to fix Vicare issue #3.  The optimisation code is unfinished
    ;;anyway according to comments in the relevant files.  (Marco Maggi,
    ;;Mon Jun 7, 2010)
    (when (< 0 (optimize-level))
      (display "*** vicare warning: optimization level artificially set to 0\n"
	       (current-error-port)))
    (optimize-level 0)

    (command-line-arguments
     (cons (if (eq? 'interactive-repl cfg.exec-mode)
	       "*interactive*"
	     cfg.script)
	   cfg.program-options))
    (load-libraries  cfg)
    (load-eval-files cfg)

    (case cfg.exec-mode
      ((r6rs-script)
       (load-r6rs-program cfg))

      ((r6rs-repl)
       (let ((env (load-r6rs-program cfg)))
	 (interaction-environment env)
	 (%print-greetings)
	 (new-cafe (lambda (x)
		     (doit (eval x env))))))

      ((compile)
       (compile-dependencies cfg))

      ((script)
       (load-evaluated-script cfg))

      ;;When no options selected: just enter a clean REPL.
      ((interactive-repl)
       (%print-greetings)
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
;;eval: (put 'with-run-time-config 'scheme-indent-function 1)
;;End:
