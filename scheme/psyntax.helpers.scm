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

(import (prefix (rnrs syntax-case) sys::))

(define-syntax commented-out
  ;;Comment out a sequence of forms.  It allows us to comment out forms and still use
  ;;the editor's autoindentation features in the commented out section.
  ;;
  (syntax-rules ()
    ((_ . ?form)
     (module ()))
    ))

(define-syntax /comment
  (syntax-rules ()))

(define-syntax comment
  (syntax-rules (/comment)
    ((comment ?form ... /comment)
     (module ()))
    ))

;;This syntax can be used as standalone identifier  and it expands to #f.  It is used
;;as "annotated expression"  argument in calls to the BUILD-  functions when there is
;;no annotated expression to be given.
;;
(define-syntax no-source
  (lambda (x) #f))

(define-syntax-rule (reverse-and-append ?item**)
  (reverse-and-append-with-tail ?item** '()))

(define-syntax-rule (reverse-and-append-with-tail ?item** ?tail-item*)
  ($fold-left/stx (lambda (knil item)
		    (append item knil))
		  ?tail-item*
		  ?item**))

(define-syntax ($fold-left/stx stx)
  ;;Like FOLD-LEFT, but expand the loop inline.   The "function" to be folded must be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  ;;
  ;;This  implementation: is  non-tail recursive,  assumes proper  list arguments  of
  ;;equal length.
  ;;
  (sys::syntax-case stx ()
    ((_ ?combine ?knil ?ell0 ?ell ...)
     (sys::with-syntax (((ELL ...) (sys::generate-temporaries #'(?ell ...))))
       (sys::syntax
	(let recur ((knil ?knil)
		    (ell0 ?ell0)
		    (ELL  ?ell)
		    ...)
	  (if (pair? ell0)
	      (recur (?combine knil (car ell0) (car ELL) ... )
		     (cdr ell0) (cdr ELL) ...)
	    knil)))))
    ))

(define-syntax ($map/stx stx)
  ;;Like  MAP, but  expand the  loop inline.   The "function"  to be  mapped must  be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  (sys::syntax-case stx ()
    ((_ ?proc ?ell0 ?ell ...)
     ;;This implementation  is: tail recursive,  loops in order, assumes  proper list
     ;;arguments of equal length.
     (sys::with-syntax
	 (((ELL0 ELL ...) (sys::generate-temporaries #'(?ell0 ?ell ...))))
       (sys::syntax
	(letrec ((loop (lambda (result.head result.last-pair ELL0 ELL ...)
			 (if (pair? ELL0)
			     (let* ((result.last-pair^ (let ((new-last-pair (cons (?proc (car ELL0)
											 (car ELL)
											 ...)
										  '())))
							 (if result.last-pair
							     (begin
							       (set-cdr! result.last-pair new-last-pair)
							       new-last-pair)
							   new-last-pair)))
				    (result.head^       (or result.head result.last-pair^)))
			       (loop result.head^ result.last-pair^ (cdr ELL0) (cdr ELL) ...))
			   (or result.head '())))))
	  (loop #f #f ?ell0 ?ell ...))
	)))
    ))

(define* (map-for-two-retvals proc . ell*)
  (cond ((for-all pair? ell*)
	 (let-values
	     (((rv1  rv2)  (apply proc (map car ell*)))
	      ((rv1* rv2*) (apply map-for-two-retvals proc (map cdr ell*))))
	   (values (cons rv1 rv1*) (cons rv2 rv2*))))
	((for-all null? ell*)
	 (values '() '()))
	(else
	 (assertion-violation 'map-for-two-retvals "length mismatch" ell*))))

;;; --------------------------------------------------------------------

(define-syntax (with-who stx)
  (sys::syntax-case stx ()
    ((_ ?who ?body0 ?body ...)
     (sys::identifier? #'?who)
     (sys::syntax (fluid-let-syntax
		     ((__who__ (identifier-syntax (quote ?who))))
		   ?body0 ?body ...)))
    ))

(define-syntax define-module-who
  (lambda (stx)
    (sys::syntax-case stx ()
      ((_ ?module-who)
       (sys::with-syntax
	   ((MODULE-WHO (sys::datum->syntax (sys::syntax ?module-who) '__module_who__)))
	 (sys::syntax
	  (define-syntax MODULE-WHO
	    (identifier-syntax (quote ?module-who))))))
      )))

;;; --------------------------------------------------------------------

(define (false-or-procedure? obj)
  (or (not obj)
      (procedure? obj)))

(define (false-or-symbol? obj)
  (or (not     obj)
      (symbol? obj)))

(define (non-empty-list-of-symbols? obj)
  (and (not (null? obj))
       (list? obj)
       (for-all symbol? obj)))

(define-syntax-rule (trace-define (?name . ?formals) . ?body)
  (define (?name . ?formals)
    (debug-print (quote ?name) 'arguments . ?formals)
    (call-with-values
	(lambda () . ?body)
      (lambda retvals
	(debug-print (quote ?name) 'retvals retvals)
	(apply values retvals)))))

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'sys::syntax-case		'scheme-indent-function 2)
;; eval: (put 'sys::with-syntax		'scheme-indent-function 1)
;; End:
