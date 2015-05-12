;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(library (psyntax.library-collectors)
  (export
    make-collector
    imp-collector		inv-collector
    vis-collector		stale-when-collector)
  (import (rnrs)
    (psyntax.compat)
    (only (psyntax.lexical-environment)
	  assertion-violation/internal-error)
    (only (psyntax.library-manager)
	  library?))


;;;; library records collectors

(define (make-collector)
  (let ((ls '()))
    (case-lambda*
     (()
      ls)
     (({x library?})
      ;;Prepend X to the list LS if it is not already contained according to EQ?.
      (set! ls (if (memq x ls)
		   ls
		 (cons x ls)))))))

(define-syntax %mk-collector-parameter
  (syntax-rules ()
    ((_ ?who)
     (fluid-let-syntax
	 ((__who__ (identifier-syntax (quote ?who))))
       (make-parameter
	   (lambda args
	     (assertion-violation/internal-error __who__ "parameter not initialized"))
	 (lambda (x)
	   (if (procedure? x)
	       x
	     (assertion-violation __who__ "expected procedure as argument" x))))))
    ))

(define imp-collector
  ;;Imported libraries  collector.  Holds  a collector function  (see MAKE-COLLECTOR)
  ;;filled with  the LIBRARY records representing  the libraries from an  R6RS import
  ;;specification:  every time  the expander  parses an  IMPORT syntax,  the selected
  ;;libraries are represented  by a LIBRARY record  and such record is  added to this
  ;;collection.
  ;;
  (%mk-collector-parameter imp-collector))

(define inv-collector
  ;;Invoked  libraries collector.   Holds a  collector function  (see MAKE-COLLECTOR)
  ;;filled  with the  LIBRARY  records representing  the  libraries defining  initial
  ;;entries in the  lexical environment that are  used in the code  we are expanding.
  ;;
  ;;The  libraries collected  here need  to  be invoked  before the  current code  is
  ;;expanded.
  ;;
  ;;The libraries  collected here are not  only the ones directly  imported, but also
  ;;the ones imported by the imported libraries.  For example, the library:
  ;;
  ;;   (library (subdemo)
  ;;     (export sub-var)
  ;;     (import (vicare))
  ;;     (define sub-var 456))
  ;;
  ;;is imported by the library:
  ;;
  ;;   (library (demo)
  ;;     (export var sub-var)
  ;;     (import (vicare) (subdemo))
  ;;     (define var 123))
  ;;
  ;;which is imported by the program:
  ;;
  ;;   (import (vicare) (demo))
  ;;   (define (doit)
  ;;     (list var sub-var))
  ;;   (doit)
  ;;
  ;;when the body of  the function DOIT is expanded: the  identifiers VAR and SUB-VAR
  ;;are  captured by  bindings  in  the lexical  environment  having descriptor  with
  ;;format:
  ;;
  ;;   (global . (?library . ?loc))
  ;;
  ;;where  ?LIBRARY is  the  record of  type LIBRARY  representing  the library  that
  ;;defines the variable and ?LOC is a loc gensym holding the variable's value in its
  ;;"value" slot.  Such LIBRARY records are added to the INV-COLLECTOR.
  ;;
  ;;For  the  identifier VAR:  ?LIBRARY  represents  the  library "(demo)";  for  the
  ;;identifier  SUB-VAR: ?LIBRARY  represents the  library "(subdemo)".   Notice that
  ;;while  "(demo)"  is present  in  the  IMPORT specification,  and  so  it is  also
  ;;registered in the IMP-COLLECTOR, "(subdemo)" is not and it is only present in the
  ;;INV-COLLECTOR.
  ;;
  (%mk-collector-parameter inv-collector))

(define vis-collector
  ;;Visit libraries collector.  Holds a collector function (see MAKE-COLLECTOR) which
  ;;is  meant to  be  filled with  the  LIBRARY records.   This  collector holds  the
  ;;libraries collected by the INV-COLLECTOR when  expanding the right-hand side of a
  ;;syntax definition.
  ;;
  ;;For example, the library:
  ;;
  ;;  (library (demo)
  ;;    (export var)
  ;;    (import (vicare))
  ;;    (define var 123))
  ;;
  ;;is loaded by the program:
  ;;
  ;;  (import (vicare)
  ;;    (for (demo) expand))
  ;;  (define-syntax doit (lambda (stx) var))
  ;;  (doit)
  ;;
  ;;the right-hand side of the syntax definition is the expression:
  ;;
  ;;  (lambda (stx) var)
  ;;
  ;;when such expression is expanded: the identifier VAR is found to be captured by a
  ;;binding in the lexical environment having descriptor with format:
  ;;
  ;;   (global . (?library . ?loc))
  ;;
  ;;where ?LIBRARY  is the record of  type LIBRARY representing the  library "(demo)"
  ;;and ?LOC is a loc gensym holding 123 in its "value" slot.  The record ?LIBRARY is
  ;;added first to INV-COLLECTOR and, after finishing the expansion of the right-hand
  ;;side,  it  is moved  to  the  VIS-COLLECTOR.  See  %EXPAND-MACRO-TRANSFORMER  for
  ;;details.
  ;;
  (%mk-collector-parameter vis-collector))


(define stale-when-collector
  ;;Collects test expressions from STALE-WHEN syntaxes and LIBRARY records needed for
  ;;such  expressions.   This  parameter  holds a  special  collector  function  (see
  ;;%MAKE-STALE-COLLECTOR) which handles 2  collections: one for expanded expressions
  ;;representing STALE-WHEN  test expressions, one  for LIBRARY records  defining the
  ;;imported variables needed by the test expressions.
  ;;
  ;;The library:
  ;;
  ;;   (library (subsubdemo)
  ;;     (export sub-sub-var)
  ;;     (import (vicare))
  ;;     (define sub-sub-var 456))
  ;;
  ;;is imported by the library:
  ;;
  ;;   (library (subdemo)
  ;;     (export sub-var sub-sub-var)
  ;;     (import (vicare) (subsubdemo))
  ;;     (define sub-var 456))
  ;;
  ;;which is imported by the library:
  ;;
  ;;   (library (demo)
  ;;     (export var)
  ;;     (import (vicare) (subdemo))
  ;;     (define var
  ;;       (stale-when (< sub-var sub-sub-var)
  ;;         123)))
  ;;
  ;;which is imported by the program:
  ;;
  ;;   (import (vicare) (demo))
  ;;   (debug-print var)
  ;;
  ;;when the  test expression of the  STALE-WHEN syntax is expanded,  the identifiers
  ;;SUB-VAR and SUB-SUB-VAR are captured by  bindings in the lexical environment with
  ;;the format:
  ;;
  ;;   (global . (?library . ?loc))
  ;;
  ;;where  ?LIBRARY is  the  record of  type LIBRARY  representing  the library  that
  ;;defines the variable and ?LOC is a loc gensym holding the variable's value in its
  ;;"value" slot.  Such  LIBRARY records are added first to  INV-COLLECTOR and, after
  ;;finishing  the  expansion  of  the  STALE-WHEN   test,  they  are  moved  to  the
  ;;STALE-WHEN-COLLECTOR.  See HANDLE-STALE-WHEN for details.
  ;;
  ;;For the identifier SUB-VAR: ?LIBRARY  represents the library "(subdemo)"; for the
  ;;identifier SUB-SUB-VAR:  ?LIBRARY represents the library  "(subsubdemo)".  Notice
  ;;that while "(subdemo)" is present in the  IMPORT specification, and so it is also
  ;;registered in the IMP-COLLECTOR, "(subsubdemo)" is  not and it is only present in
  ;;the STALE-WHEN-COLLECTOR.
  ;;
  ;;The collector function  referenced by this parameter returns 2  values, which are
  ;;usually named GUARD-CODE and GUARD-LIB*:
  ;;
  ;;GUARD-CODE is a single core language expression representing a composition of all
  ;;the STALE-WHEN test  expressions present in the  body of a library.   If at least
  ;;one of  the test expressions  evaluates to  true: the whole  composite expression
  ;;evaluates to true.
  ;;
  ;;GUARD-LIB* is  a list  of LIBRARY  records representing  the libraries  needed to
  ;;evaluate the composite test expression.
  ;;
  (make-parameter #f))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:

