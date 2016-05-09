;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!r6rs
(library (ikarus.compiler.config)
  (export
    compiler-initialisation/storage-location-gensyms-associations-func
    generate-descriptive-labels?
    generate-debug-calls
    strip-source-info
    optimizer-output
    perform-core-type-inference?
    perform-unsafe-primrefs-introduction?
    assembler-output
    enabled-function-application-integration?
    check-compiler-pass-preconditions
    ;;
    (rename (strict-r6rs options::strict-r6rs))
    options::print-verbose-messages?
    options::print-debug-messages?)
  (import (rnrs)
    (ikarus.compiler.compat)
    (prefix (only (ikarus.options)
		  print-verbose-messages?
		  print-debug-messages?)
	    options::))


;;;; helpers

(define-syntax define-parameter-boolean-option
  (syntax-rules ()
    ((_ ?who)
     (define-parameter-boolean-option ?who #f))
    ((_ ?who ?default)
     (define ?who
       (make-parameter ?default
	 (lambda (value)
	   (and value #t)))))
    ))


;;;; configuration parameters

(define-parameter-boolean-option strict-r6rs)
(define-parameter-boolean-option generate-descriptive-labels?)

(define compiler-initialisation/storage-location-gensyms-associations-func
  (make-parameter #f))

;;Set to true when the option "--debug" is used on the command line of the executable
;;"vicare"; else set to #f.
;;
(define-parameter-boolean-option generate-debug-calls)

;;When  true: while  processing  the core  language  form ANNOTATED-CASE-LAMBDA,  the
;;annotation about the  source code location of the expression  is removed; otherwise
;;it is left in to be used by  the debugger.  We should strip source annotations when
;;building the boot image.
;;
(define-parameter-boolean-option strip-source-info)

(define-parameter-boolean-option optimizer-output)
(define-parameter-boolean-option assembler-output)

;;When true: the pass CORE-TYPE-INFERENCE is performed, else it is skipped.
;;
(define-parameter-boolean-option perform-core-type-inference? #t)

;;When true: the pass INTRODUCE-UNSAFE-PRIMREFS is performed, else it is skipped.  It
;;makes sense to perform such compiler pass  only if we have first performed the core
;;type inference.
;;
(define-parameter-boolean-option perform-unsafe-primrefs-introduction? #t)

;;When true: the source optimiser will attempt integration of function applications.
;;
(define-parameter-boolean-option enabled-function-application-integration? #t)

;;When  true: perform  additional  compiler code-validation  passes  to validate  the
;;recordised code between true compiler passes.
;;
(define-parameter-boolean-option check-compiler-pass-preconditions)


;;;; done

#| end of library |# )

;;; end of file
