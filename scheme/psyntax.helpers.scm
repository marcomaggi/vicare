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

(define-syntax commented-out
  ;;Comment out a sequence of forms.  It allows us to comment out forms and still use
  ;;the editor's autoindentation features in the commented out section.
  ;;
  (syntax-rules ()
    ((_ . ?form)
     (module ()))
    ))

;;This syntax can be used as standalone identifier  and it expands to #f.  It is used
;;as "annotated expression"  argument in calls to the BUILD-  functions when there is
;;no annotated expression to be given.
;;
(define-syntax no-source
  (lambda (x) #f))

(define-syntax-rule (reverse-and-append ?item**)
  (apply append (reverse ?item**)))

(define-syntax with-who
  (syntax-rules ()
    ((_ ?who ?body0 ?body ...)
     (fluid-let-syntax ((__who__ (identifier-syntax (quote ?who)))) ?body0 ?body ...))
    ))

;;; --------------------------------------------------------------------

(define (false-or-procedure? obj)
  (or (not obj)
      (procedure? obj)))

(define (non-empty-list-of-symbols? obj)
  (and (not (null? obj))
       (list? obj)
       (for-all symbol? obj)))

(define (improper-list->list-and-rest ell)
  (let loop ((ell   ell)
	     (item* '()))
    (syntax-match ell ()
      ((?car . ?cdr)
       (loop ?cdr (cons ?car item*)))
      (()
       (values (reverse item*) '()))
      (_
       (values (reverse item*) ell)))
    ))

(define* (proper-list->head-and-last ell)
  (let loop ((ell   ell)
	     (item* '()))
    (syntax-match ell ()
      (()
       (assertion-violation __who__ "expected non-empty list" ell))
      ((?last)
       (values (reverse item*) ?last))
      ((?car . ?cdr)
       (loop ?cdr (cons ?car item*))))))

(define* (proper-list->last-item ell)
  (syntax-match ell ()
    (()
     (assertion-violation __who__ "expected non-empty list" ell))
    ((?last)
     ?last)
    ((?car . ?cdr)
     (proper-list->last-item ?cdr))
    ))

(define-syntax-rule (trace-define (?name . ?formals) . ?body)
  (define (?name . ?formals)
    (debug-print (quote ?name) 'arguments . ?formals)
    (call-with-values
	(lambda () . ?body)
      (lambda retvals
	(debug-print (quote ?name) 'retvals retvals)
	(apply values retvals)))))

(module ($map-in-order
	 $map-in-order1)

  (case-define $map-in-order
    ((func ell)
     ($map-in-order1 func ell))
    ((func . ells)
     (if (null? ells)
	 '()
       (let recur ((ells ells))
	 (if (pair? ($car ells))
	     (let* ((cars ($map-in-order1 $car ells))
		    (cdrs ($map-in-order1 $cdr ells))
		    (head (apply func cars)))
	       (cons head (recur cdrs)))
	   '())))))

  (define-syntax-rule ($map-in-order1 ?func ?ell)
    (let recur ((ell ?ell))
      (if (pair? ell)
	  (let ((head (?func ($car ell))))
	    (cons head (recur ($cdr ell))))
	ell)))

  #| end of module |# )



;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
