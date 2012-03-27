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
    fasl-directory	fasl-path)
  (import (except (ikarus)
		  load			load-r6rs-script
		  fasl-directory	fasl-path)
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
    (only (vicare syntactic-extensions)
	  unwind-protect
	  define-inline))


;;;; handling of FASL repository file names

;;The file extension of serialised FASL files.
(define FASL-EXTENSION
  (cond ((<= (fixnum-width) 32)	".vicare-32bit-fasl")
	(else			".vicare-64bit-fasl")))

;;The directory under which serialised FASL files must be saved.
;;
(define fasl-directory
  (make-parameter
      (let ((P (posix.getenv "VICARE_FASL_DIRECTORY")))
	(if (and P (file-exists? P))
	    (posix.real-pathname P)
	  (let ((P (posix.getenv "HOME")))
	    (if (and P (file-exists? P))
		(string-append (posix.real-pathname P) "/.vicare/precompiled")
	      ""))))
;;;The following code was the original in Ikarus.  (Marco Maggi; Sat Mar
;;;10, 2012)
;;;
;;;      (cond ((posix.getenv "VICARE_FASL_DIRECTORY"))
;;;	    ((posix.getenv "HOME")
;;;	     => (lambda (s)
;;;		  (string-append s "/.vicare/precompiled")))
;;;	    (else ""))
    (lambda (P)
      (define who 'fasl-directory)
      (if (string? P)
	  (if (file-exists? P)
	      P
	    (error who "attempt to set non-existent directory pathname" P))
	(error who "expected string as directory pathname" P)))))

(define (fasl-path filename)
  ;;Given a source  file name return the associated  full FASL file name
  ;;using  the  current  value   of  FASL-DIRECTORY.   Return  false  if
  ;;FASL-DIRECTORY is unset (which should never happen).
  ;;
  (let ((d (fasl-directory)))
    (and (not (string=? d ""))
	 (string-append d (posix.real-pathname filename) FASL-EXTENSION))))


;;;; loading and serialising libraries

(define-struct serialized-library
  (contents))

(define (load-serialized-library filename success-kont)
  ;;Given a  source file  name load the  associated FASL file  and apply
  ;;SUCCESS-KONT  to the  library contents,  return the  result  of such
  ;;application.  If  a FASL  file is not  available or  invalid: return
  ;;false.
  ;;
  ;;Print  to  the current  error  port  appropriate  warning about  the
  ;;availability of the FASL file.
  ;;
  (let ((ikfasl (fasl-path filename)))
    (cond ((or (not ikfasl)
	       (not (file-exists? ikfasl)))
	   #f)
	  ((< (posix.file-modification-time ikfasl)
	      (posix.file-modification-time filename))
	   (fprintf (current-error-port)
		    "WARNING: not using fasl file ~s because it is older \
                     than the source file ~s\n" ikfasl filename)
	   #f)
	  (else
	   (let ((x (let* ((port (open-file-input-port ikfasl))
			   (x    (fasl-read port)))
		      (close-input-port port)
		      x)))
	     (if (serialized-library? x)
		 (apply success-kont filename (serialized-library-contents x))
	       (begin
		 (fprintf (current-error-port)
			  "WARNING: not using fasl file ~s because it was \
                           compiled with a different instance of Vicare.\n" ikfasl)
		 #f)))))))

(define (do-serialize-library filename contents)
  ;;Given the source file name of  a library file and the contents of an
  ;;already compiled library write a FASL file in the repository.
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
      (serialize-all
       (lambda (file-name contents)
	 (do-serialize-library file-name contents))
       (lambda (core-expr)
	 (compile-core-expr core-expr))))
    (when run?
      (thunk))))

;; (define (compile-r6rs-library filename)
;;   ((current-library-expander)
;;    (read-library-source-file filename)
;;    filename
;;    (lambda (name) (void)))
;;   (serialize-all
;;    (lambda (file-name contents)
;;      (do-serialize-library file-name contents))
;;    (lambda (core-expr)
;;      (compile-core-expr core-expr))))


;;;; done

(current-precompiled-library-loader load-serialized-library)

)

;;; end of file
