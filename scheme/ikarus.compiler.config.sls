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
    option.strict-r6rs
    option.verbose?
    option.print-debug-messages?)
  (import (rnrs)
    (ikarus.compiler.compat)
    (prefix (only (ikarus.options)
		  strict-r6rs
		  verbose?
		  print-debug-messages?)
	    option.))


;;;; configuration parameters

(define compiler-initialisation/storage-location-gensyms-associations-func
  (make-parameter #f))

(define generate-descriptive-labels?
  (make-parameter #f))

(define generate-debug-calls
  ;;Set  to true  when  the option  "--debug"  is used  on the  command  line of  the
  ;;executable "vicare"; else set to #f.
  ;;
  (make-parameter #f))

(define strip-source-info
  ;;When true:  while processing  the core  language form  ANNOTATED-CASE-LAMBDA, the
  ;;annotation about the source code location of the expression is removed; otherwise
  ;;it is left in to be used by the debugger.
  ;;
  ;;We should strip source annotations when building the boot image.
  ;;
  (make-parameter #f))

(define optimizer-output
  (make-parameter #f))

(define perform-core-type-inference?
  ;;When true: the pass CORE-TYPE-INFERENCE is performed, else it is skipped.
  ;;
  (make-parameter #t))

(define perform-unsafe-primrefs-introduction?
  ;;When true: the pass INTRODUCE-UNSAFE-PRIMREFS  is performed, else it is skipped.
  ;;It makes sense to perform such compiler  pass only if we have first performed the
  ;;core type inference.
  ;;
  (make-parameter #t))

(define assembler-output
  (make-parameter #f))

(define enabled-function-application-integration?
  ;;When  true:   the  source   optimiser  will   attempt  integration   of  function
  ;;applications.
  ;;
  (make-parameter #t
    (lambda (obj)
      (and obj #t))))

(define check-compiler-pass-preconditions
  ;;When true:  perform additional  compiler code-validation  passes to  validate the
  ;;recordised code between true compiler passes.
  ;;
  (make-parameter #f
    (lambda (obj)
      (and obj #t))))


;;;; done

#| end of library |# )

;;; end of file
