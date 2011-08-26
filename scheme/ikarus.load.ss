;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi
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


(library (ikarus load)
  (export load load-r6rs-script fasl-directory)
  (import
    (except (ikarus) fasl-directory load load-r6rs-script)
    (only (ikarus.compiler) compile-core-expr)
    (only (psyntax library-manager)
      serialize-all current-precompiled-library-loader)
    (only (psyntax expander) compile-r6rs-top-level)
    (only (ikarus.reader.annotated) read-script-source-file))

  (define-struct serialized-library (contents))

  (define fasl-extension
    (cond
      [(<= (fixnum-width) 32) ".vicare-32bit-fasl"]
      [else                   ".vicare-64bit-fasl"]))

  (define fasl-directory
    (make-parameter
      (cond
        [(getenv "VICARE_FASL_DIRECTORY")]
        [(getenv "HOME") =>
         (lambda (s)
           (string-append s "/.vicare/precompiled"))]
        [else ""])
      (lambda (s)
        (if (string? s)
            s
            (die 'fasl-directory "not a string" s)))))

  (define (fasl-path filename)
    (let ([d (fasl-directory)])
      (and (not (string=? d ""))
        (string-append d (file-real-path filename) fasl-extension))))

  (define (load-serialized-library filename sk)
    (let ([ikfasl (fasl-path filename)])
      (cond
        [(or (not ikfasl) (not (file-exists? ikfasl))) #f]
        [(< (file-mtime ikfasl) (file-mtime filename))
         (fprintf (current-error-port)
            "WARNING: not using fasl file ~s because it is older \
             than the source file ~s\n"
           ikfasl
           filename)
         #f]
        [else
         (let ([x
                (let ([p (open-file-input-port ikfasl)])
                  (let ([x (fasl-read p)])
                    (close-input-port p)
                    x))])
           (if (serialized-library? x)
               (apply sk (serialized-library-contents x))
               (begin
                 (fprintf (current-error-port)
                    "WARNING: not using fasl file ~s because it was \
                     compiled with a different instance of vicare.\n"
                    ikfasl)
                 #f)))])))

  (define (do-serialize-library filename contents)
    (let ([ikfasl (fasl-path filename)])
      (cond
        [(not ikfasl) (void)]
        [else
         (fprintf (current-error-port) "Serializing ~s ...\n" ikfasl)
         (let-values ([(dir name) (split-file-name ikfasl)])
           (make-directory* dir))
         (let ([p (open-file-output-port ikfasl (file-options no-fail))])
           (fasl-write (make-serialized-library contents) p)
           (close-output-port p))])))

  (define load-handler
    (lambda (x)
      (eval x (interaction-environment))))

  (define read-and-eval
    (lambda (p eval-proc)
      (let ([x (read p)])
        (unless (eof-object? x)
          (eval-proc x)
          (read-and-eval p eval-proc)))))

  (define load
    ;;Load  the  source  code  from  file  X, which  must  be  a  string
    ;;representing a  file pathname, and  transform the contents  of the
    ;;file in a  list of symbolic expressions.  For  each library in the
    ;;source apply EVAL-PROC to the corresponding symbolic expression.
    ;;
    (case-lambda
      [(x) (load x load-handler)]
      [(x eval-proc)
       (unless (string? x)
         (die 'load "not a string" x))
       (unless (procedure? eval-proc)
         (die 'load "not a procedure" eval-proc))
       (let ([ls (read-script-source-file x)])
         (let f ()
           (unless (null? ls)
             (let ([a (car ls)])
               (set! ls (cdr ls))
               (eval-proc a))
             (f))))]))

  (define load-r6rs-script
    (lambda (filename serialize? run?)
      (unless (string? filename)
        (die 'load-r6rs-script "file name is not a string" filename))
      (let ([prog (read-script-source-file filename)])
        (let([thunk (compile-r6rs-top-level prog)])
          (when serialize?
            (serialize-all
              (lambda (file-name contents)
                (do-serialize-library file-name contents))
              (lambda (core-expr)
                (compile-core-expr core-expr))))
          (when run? (thunk))))))

  (current-precompiled-library-loader load-serialized-library)

  )
