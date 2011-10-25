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
    print-greeting		host-info
    init-library-path		split-path)
  (import (except (ikarus)
		  host-info)
    (vicare include))
  (include "ikarus.config.ss")


(define (host-info)
  target)

(define (init-library-path)
  (define (prefix ext ls)
    (append (map (lambda (x) (string-append ext x)) ls) ls))
  (library-path (append (cond ((getenv "VICARE_LIBRARY_PATH")
			       => split-path)
			      (else '(".")))
			(list vicare-lib-dir)))
  (library-extensions (prefix "/main" (prefix ".vicare" (library-extensions)))))

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

(define (print-greeting)
  (display "Vicare Scheme version ")
  (display vicare-version)
  (unless (zero? (string-length vicare-revision))
    (display "+"))
  (unless (= 30 (fixnum-width))
    (display ", 64-bit"))
  (newline)
  (unless (zero? (string-length vicare-revision))
    (display "Revision ")
    (display vicare-revision)
    (newline)
    (display "Build ")
    (let-syntax ((ds (lambda (x) (date-string))))
      (display ds))
    (newline))
  (display "Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors\n\
Copyright (c) 2011 Marco Maggi\n\n"))



#| end of library (ikarus startup) |# )


;;; Finally, we're ready to evaluate the files and enter the cafe.
(library (ikarus main)
  (export)
  (import (except (ikarus) load-r6rs-script)
    (except (ikarus startup) host-info)
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
    (only (ikarus load)
	  load-r6rs-script))


(define rcfiles #t) ;; #f for no rcfile, list for specific list

(define (parse-command-line-arguments)
  (let next-option ((args (command-line-arguments))
		    (k    void))
    (define (invalid-rc-error)
      (assertion-violation 'vicare "--no-rcfile is invalid with --rcfile"))
    (cond ((null? args)
	   (values '() #f #f '() k))

	  ((member (car args) '("-d" "--debug"))
	   (next-option (cdr args) (lambda () (k) (generate-debug-calls #t))))

	  ((member (car args) '("-nd" "--no-debug"))
	   (next-option (cdr args) (lambda () (k) (generate-debug-calls #f))))

	  ((string=? (car args) "--print-assembler")
	   (next-option (cdr args) (lambda () (k) (assembler-output #t))))

	  ((string=? (car args) "-O2")
	   (next-option (cdr args) (lambda () (k) (optimize-level 2))))

	  ((string=? (car args) "-O1")
	   (next-option (cdr args) (lambda () (k) (optimize-level 1))))

	  ((string=? (car args) "-O0")
	   (next-option (cdr args) (lambda () (k) (optimize-level 0))))

	  ((string=? (car args) "--no-rcfile")
	   (unless (boolean? rcfiles) (invalid-rc-error))
	   (set! rcfiles #f)
	   (next-option (cdr args) k))

	  ((string=? (car args) "--rcfile")
	   (let ((d (cdr args)))
	     (when (null? d) (die 'vicare "--rcfile requires a script name"))
	     (set! rcfiles (cons (car d)
				 (case rcfiles
				   ((#t) '())
				   ((#f) (invalid-rc-error))
				   (else rcfiles))))
	     (next-option (cdr d) k)))

	  ((string=? (car args) "--")
	   (values '() #f #f (cdr args) k))

	  ((string=? (car args) "--script")
	   (let ((d (cdr args)))
	     (if (null? d)
		 (die 'vicare "--script requires a script name")
	       (values '() (car d) 'script (cdr d) k))))

	  ((string=? (car args) "--r6rs-script")
	   (let ((d (cdr args)))
	     (if (null? d)
		 (die 'vicare "--r6rs-script requires a script name")
	       (values '() (car d) 'r6rs-script (cdr d) k))))

	  ((string=? (car args) "--r6rs-repl")
	   (let ((d (cdr args)))
	     (if (null? d)
		 (die 'vicare "--r6rs-repl requires a script name")
	       (values '() (car d) 'r6rs-repl (cdr d) k))))

	  ((string=? (car args) "--compile-dependencies")
	   (let ((d (cdr args)))
	     (if (null? d)
		 (die 'vicare "--compile-dependencies requires a script name")
	       (values '() (car d) 'compile (cdr d) k))))

	  (else
	   (let-values (((f* script script-type a* k) (next-option (cdr args) k)))
	     (values (cons (car args) f*) script script-type a* k))))))


(initialize-symbol-table!)
(init-library-path)
(let-values (((files script script-type args init-command-line-args)
	      (parse-command-line-arguments)))

  (define (start proc)
    (if (generate-debug-calls)
	(guarded-start proc)
      (proc)))

  (define-syntax doit
    (syntax-rules ()
      ((_ e e* ...)
       (start (lambda () e e* ...)))))

  (define (default-rc-files)
    (cond ((getenv "VICARE_RC_FILES")
	   => split-path)
	  ((getenv "HOME")
	   => (lambda (home)
		(let ((f (string-append home "/.vicarerc")))
		  (if (file-exists? f)
		      (list f)
		    '()))))
	  (else '())))

  (define (%load-files-and-execute-script)
    ;;Load  the files  selected on  the command  line, then  execute the
    ;;selected script.  Return the resulting environment.
    ;;
    (doit
     (command-line-arguments (cons script args))
     (for-each (lambda (filename)
		 (for-each (lambda (src)
			     ((current-library-expander) src))
		   (read-source-file filename)))
       files)
     (load-r6rs-script script #f #t)))

  ;;Load the RC files as R6RS scripts.
  (for-each
      (lambda (filename)
	(with-exception-handler
	    (lambda (con)
	      (raise-continuable
	       (condition (make-who-condition 'vicare)
			  (make-message-condition (string-append "loading rc file "
								 filename " failed"))
			  con)))
	  (lambda ()
	    (load-r6rs-script filename #f #t))))
    (case rcfiles
      ((#t) (default-rc-files))
      ((#f) '())
      (else (reverse rcfiles))))

  ;;Update the global  run state with actions selected  from the command
  ;;line: debugging  mode on/off, compiler  optimisation level, logging,
  ;;etc.
  (init-command-line-args)

  ;;Added to fix  Vicare issue #3.  The optimisation  code is unfinished
  ;;anyway according  to comments in the relevant  files.  (Marco Maggi,
  ;;Mon Jun 7, 2010)
  (when (< 0 (optimize-level))
    (display "*** vicare warning: optimization level artificially set to 0\n"
	     (current-error-port)))
  (optimize-level 0)

  (case script-type
    ;;Load files and execute script.
    ((r6rs-script)
     (%load-files-and-execute-script))

    ;;Load  files,  execute  script,  finally  enter  a  REPL  with  the
    ;;resulting environment.
    ((r6rs-repl)
     (print-greeting)
     (let ((env (%load-files-and-execute-script)))
       (interaction-environment env)
       (new-cafe (lambda (x)
		   (doit (eval x env))))))

    ;;Run the script compiling dependencies.
    ((compile)
     (unless (null? files)
       (assertion-violation 'vicare
	 "load files not allowed when using option --compile-dependencies" files))
     (doit
      (command-line-arguments (cons script args))
      (load-r6rs-script script #t #f)))

    ;;Load files and script.
    ((script)
     (command-line-arguments (cons script args))
     (doit
      (for-each load files)
      (load script)))

    ;;When no options selecte: just enter a clean REPL.
    (else
     (print-greeting)
     (command-line-arguments (cons "*interactive*" args))
     (doit
      (for-each load files))
     (new-cafe (lambda (x)
		 (doit
		  (eval x (interaction-environment)))))))

  (exit 0))


;;;; done

#| end of library (ikarus main) |# )

;;; end of file
