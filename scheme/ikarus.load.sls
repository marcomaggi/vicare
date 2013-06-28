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

(library (ikarus load)
  (export
    load		load-r6rs-script
    fasl-directory	fasl-path
    fasl-search-path)
  (import (except (ikarus)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  load			load-r6rs-script
		  fasl-directory	fasl-path
		  fasl-search-path)
    (prefix (only (ikarus.posix)
		  getenv
		  mkdir/parents
		  split-file-name
		  real-pathname
		  file-modification-time)
	    posix.)
    (only (ikarus.compiler)
	  compile-core-expr)
    (only (vicare.foreign-libraries)
	  retrieve-filename-foreign-libraries)
    (only (psyntax library-manager)
	  serialize-all
	  current-precompiled-library-loader)
    (only (psyntax expander)
	  compile-r6rs-top-level)
    (only (ikarus.reader)
	  read-script-source-file)
    (prefix (only (vicare options)
		  print-loaded-libraries)
	    config.)
    (vicare arguments validation))

  (include "ikarus.wordsize.scm")


;;;; arguments validation

(define-argument-validation (search-path who obj)
  (for-all string? obj)
  (assertion-violation who
    "expected list of strings representing directory pathnames as search path" obj))


;;;; handling of FASL repository file names

;;The file extension of serialised FASL files.
(define FASL-EXTENSION
  (case-word-size
   ((32)	".vicare-32bit-fasl")
   ((64)	".vicare-64bit-fasl")))

(define DEFAULT-FASL-DIRECTORY
  (let ((P (posix.getenv "VICARE_FASL_DIRECTORY")))
    (if (and P (file-exists? P))
	(posix.real-pathname P)
      (let ((P (posix.getenv "HOME")))
	(if (and P (file-exists? P))
	    (string-append (posix.real-pathname P) "/.vicare/precompiled")
	  ""))))
  ;;The following  code was the  original in Ikarus.  (Marco  Maggi; Sat
  ;;Mar 10, 2012)
  #;(cond ((posix.getenv "VICARE_FASL_DIRECTORY"))
	((posix.getenv "HOME")
	 => (lambda (s)
	      (string-append s "/.vicare/precompiled")))
	(else "")))

;;The search  path to look for FASL  files.  Notice that we  do not test
;;the existence of the directories.
;;
(define fasl-search-path
  (make-parameter (list DEFAULT-FASL-DIRECTORY)
    (lambda (P)
      (define who 'fasl-path)
      (with-arguments-validation (who)
	  ((search-path	P))
	P))))

;;The directory under which serialised FASL files must be saved.
;;
(define fasl-directory
  (make-parameter DEFAULT-FASL-DIRECTORY
    (lambda (P)
      (define who 'fasl-directory)
      (if (string? P)
	  (if (file-exists? P)
	      (posix.real-pathname P)
	    (error who "attempt to set non-existent directory pathname" P))
	(error who "expected string as directory pathname" P)))))

(define (fasl-path filename)
  ;;Given a source  file name return the associated  full FASL file name
  ;;using  the  current  value   of  FASL-DIRECTORY.   Return  false  if
  ;;FASL-DIRECTORY is unset (which should never happen).
  ;;
  (let ((d (fasl-directory)))
    (and (not (string=? d ""))
	 (make-fasl-pathname d filename))))

(define (make-fasl-pathname prefix-pathname source-file-pathname)
  ;;We  assume that  PREFIX-PATHNAME,  if it  exists,  has already  been
  ;;normalised; if  PREFIX-PATHNAME does not  exist: it will  be created
  ;;when the FASL file is created.
  ;;
  (string-append prefix-pathname
		 (posix.real-pathname source-file-pathname)
		 FASL-EXTENSION))


;;;; loading and serialising libraries

(define-struct serialized-library
  (contents
		;A list of values  representing a LIBRARY record holding
		;precompiled code.  See  the function %SERIALIZE-LIBRARY
		;in  "psyntax.library-manager.sls"  for details  on  the
		;format.
   ))

(define (load-serialized-library filename success-kont)
  ;;Given a  source file  name load the  associated FASL file  and apply
  ;;SUCCESS-KONT  to the  library contents,  return the  result  of such
  ;;application.  If  a FASL  file is not  available or  invalid: return
  ;;false.
  ;;
  ;;Print  to  the current  error  port  appropriate  warning about  the
  ;;availability of the FASL file.
  ;;
  (define (%print-loaded-library name)
    (when (config.print-loaded-libraries)
      (display (string-append "vicare loading: " name "\n")
	       (console-error-port))))
  (let ((ikfasl (let next-prefix ((search-path (fasl-search-path)))
		  (if (null? search-path)
		      #f
		    (let ((ikfasl (make-fasl-pathname (car search-path) filename)))
		      (if (file-exists? ikfasl)
			  ikfasl
			(next-prefix (cdr search-path))))))
		#;(fasl-path filename)))
    (cond ((or (not ikfasl)
	       (not (file-exists? ikfasl)))
	   (%print-loaded-library filename)
	   #f)
	  ((< (posix.file-modification-time ikfasl)
	      (posix.file-modification-time filename))
	   (%print-loaded-library filename)
	   (fprintf (console-error-port)
		    "WARNING: not using fasl file ~s because it is older \
                     than the source file ~s\n" ikfasl filename)
	   #f)
	  (else
	   (%print-loaded-library ikfasl)
	   (let ((x (let* ((port (open-file-input-port ikfasl))
			   (x    (fasl-read port)))
		      (close-input-port port)
		      x)))
	     (if (serialized-library? x)
		 (apply success-kont filename (serialized-library-contents x))
	       (begin
		 (fprintf (console-error-port)
			  "WARNING: not using fasl file ~s because it was \
                           compiled with a different instance of Vicare.\n" ikfasl)
		 #f)))))))

(define (do-serialize-library filename contents)
  ;;Given the source file name of  a library file and the contents of an
  ;;already compiled library write a FASL file in the repository.
  ;;
  ;;CONTENTS  must be  a list  of values  representing a  LIBRARY record
  ;;holding precompiled  code.  See  the function  %SERIALIZE-LIBRARY in
  ;;"psyntax.library-manager.sls" for details on the format.
  ;;
  (let ((ikfasl (fasl-path filename)))
    (when ikfasl
      (let ((stderr (current-error-port)))
	(define-inline (%display thing)
	  (display thing stderr))
	(%display "serialising ")
	(%display ikfasl)
	(%display " ... ")
	(let-values (((dir name) (posix.split-file-name ikfasl)))
	  (posix.mkdir/parents dir #o755))
	(let ((port (open-file-output-port ikfasl (file-options no-fail))))
	  (unwind-protect
	      (fasl-write (make-serialized-library contents) port
			  (retrieve-filename-foreign-libraries filename))
	    (close-output-port port)))
	(%display "done\n")))))


(define (load-handler x)
  (eval x (interaction-environment)))

;;Commented out because unused (Marco Maggi; Oct 25, 2011).
;;
;; (define (read-and-eval port eval-proc)
;;   (let ((x (read port)))
;;     (unless (eof-object? x)
;;       (eval-proc x)
;;       (read-and-eval port eval-proc))))

(define load
  ;;Load source code from FILENAME,  which must be a string representing
  ;;a filename,  expecting an  R6RS program, an  R6RS library or  just a
  ;;list of  forms and transform the contents  of the file in  a list of
  ;;symbolic expressions.   For each form in the  source apply EVAL-PROC
  ;;to the corresponding symbolic expression.
  ;;
  ;;When  EVAL-PROC  is  not  given:  the forms  are  evaluated  in  the
  ;;current INTERACTION-ENVIRONMENT.
  ;;
  (case-lambda
   ((filename)
    (load filename load-handler))
   ((filename eval-proc)
    (define who 'load)
    (unless (string? filename)
      (assertion-violation who "expected string as filename argument" filename))
    (unless (procedure? eval-proc)
      (assertion-violation who
	"expected procedure as symbolic expression evaluator argument" eval-proc))
    (let next-form ((ls (read-script-source-file filename)))
      (unless (null? ls)
	(eval-proc (car ls))
	(next-form (cdr ls)))))))

(define (load-r6rs-script filename serialize? run?)
  ;;Load source code from FILENAME,  which must be a string representing
  ;;a filename, expecting an R6RS program or an R6RS library and compile
  ;;it.
  ;;
  ;;If SERIALIZE? is true: the compiled result is serialised in the FASL
  ;;repository.
  ;;
  ;;If RUN? is true: the compiled result is evaluated.
  ;;
  (define who 'load-r6rs-script)
  (unless (string? filename)
    (assertion-violation who "expected string as file name argument" filename))
  (let* ((prog  (read-script-source-file filename))
	 (thunk (compile-r6rs-top-level prog)))
    (when serialize?
      (serialize-all (lambda (file-name contents)
		       (do-serialize-library file-name contents))
		     (lambda (core-expr)
		       (compile-core-expr core-expr))))
    (when run?
      (thunk))))

;;Someday I will write this function.  (Marco Maggi; Mon Jun 4, 2012)
;;
;; (define (compile-r6rs-library filename)
;;   ((current-library-expander)
;;    (read-library-source-file filename)
;;    filename
;;    (lambda (library-ids library-version) (void)))
;;   (serialize-all
;;    (lambda (file-name contents)
;;      (do-serialize-library file-name contents))
;;    (lambda (core-expr)
;;      (compile-core-expr core-expr))))


;;;; done

(current-precompiled-library-loader load-serialized-library)

)

;;; end of file
