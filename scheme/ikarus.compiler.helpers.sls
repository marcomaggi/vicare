;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


#!r6rs
(library (ikarus.compiler.helpers)
  (export
    if-building-rotation-boot-image?
    cond-expand
    %list-of-one-item?
    fxincr!
    expand-time-gensym
    $map/stx
    $for-each/stx
    $fold-right/stx)
  (import (vicare))


;;;; helper syntaxes

(define-syntax (if-building-rotation-boot-image? stx)
  (define rotating?
    (equal? "yes" (getenv "BUILDING_ROTATION_BOOT_IMAGE")))
  (define (log description.stx)
    (fprintf (current-error-port)
	     "ikarus.compiler: conditional for ~a boot image: ~a\n"
	     (if rotating? "rotation" "normal")
	     (syntax->datum description.stx)))
  (syntax-case stx ()
    ((_ ?description ?true-body)
     (begin
       (log #'?description)
       (if rotating? #'?true-body #'(module ()))))
    ((_ ?description ?true-body ?false-body)
     (begin
       (log #'?description)
       (if rotating? #'?true-body #'?false-body)))
    ))

(define-syntax (cond-expand stx)
  ;;A  simple  implementation of  COND-EXPAND  in  which  the tests  are  expressions
  ;;evaluated at expand time.
  ;;
  (syntax-case stx (else)
    ((?ctx (?test0 . ?clause0*) (?test . ?clause*) ... (else . ?else*))
     (with-syntax
	 ((OUTPUT (datum->syntax #'?ctx 'output)))
       #'(let-syntax ((OUTPUT (lambda (stx)
				(syntax-case stx ()
				  ((??ctx)
				   (datum->syntax #'??ctx
						  (cond (?test0 '(begin . ?clause0*))
							(?test  '(begin . ?clause*))
							...
							(else   '(begin . ?else*)))))
				  ))))
	   (OUTPUT))))
    ((?ctx (?test0 . ?clause0*) (?test . ?clause*) ...)
     (with-syntax
	 ((OUTPUT (datum->syntax #'?ctx 'output)))
       #'(let-syntax ((OUTPUT (lambda (stx)
				(syntax-case stx ()
				  ((??ctx)
				   (datum->syntax #'??ctx
						  (cond (?test0 '(begin . ?clause0*))
							(?test  '(begin . ?clause*))
							...
							(else   '(void)))))))))
	   (OUTPUT))))
    ))

;;; --------------------------------------------------------------------

(define-syntax-rule (%list-of-one-item? ?ell)
  (let ((ell ?ell))
    (and (pair? ell)
	 (null? (cdr ell)))))

(define-syntax-rule (fxincr! ?var)
  (set! ?var (fxadd1 ?var)))

(define-syntax (expand-time-gensym stx)
  ;;Generate a gensym at expand time and expand to the quoted symbol.
  ;;
  (syntax-case stx ()
    ((_ ?template)
     (let* ((tmp (syntax->datum #'?template))
	    (fxs (vector->list (foreign-call "ikrt_current_time_fixnums_2")))
	    (str (apply string-append tmp (map (lambda (N)
						 (string-append "." (number->string N)))
					    fxs)))
	    (sym (gensym str)))
       (with-syntax
	   ((SYM (datum->syntax #'here sym)))
	 (fprintf (current-error-port) "expand-time gensym ~a\n" sym)
	 #'(quote SYM))))))

;;; --------------------------------------------------------------------

(define-syntax ($map/stx stx)
  ;;Like  MAP, but  expand the  loop inline.   The "function"  to be  mapped must  be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  (syntax-case stx ()
    ((_ ?proc ?ell0 ?ell ...)
     ;;This implementation  is: tail recursive,  loops in order, assumes  proper list
     ;;arguments of equal length.
     (with-syntax
	 (((ELL0 ELL ...) (generate-temporaries #'(?ell0 ?ell ...))))
       #'(letrec ((loop (lambda (result.head result.last-pair ELL0 ELL ...)
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
	   (loop #f #f ?ell0 ?ell ...)))
     ;;This alternative  implementation: is non-tail recursive,  loops in unspecified
     ;;order, assumes proper list arguments of equal length.
     ;;
     ;; (with-syntax (((T ...) (generate-temporaries #'(?ell ...))))
     ;;   #'(let recur ((t ?ell0) (T ?ell) ...)
     ;; 	   (if (null? t)
     ;; 	       '()
     ;; 	     (cons (?proc (car t) (car T) ...)
     ;; 		   (recur (cdr t) (cdr T) ...))))))
     )))

(define-syntax ($for-each/stx stx)
  ;;Like FOR-HEACH, but expand the loop inline.   The "function" to be mapped must be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  ;;
  ;;This implementation:  is tail recursive,  assumes proper list arguments  of equal
  ;;length.
  ;;
  (syntax-case stx ()
    ((_ ?func ?ell0 ?ell ...)
     (with-syntax (((T ...) (generate-temporaries #'(?ell ...))))
       #'(let loop ((t ?ell0) (T ?ell) ...)
	   (when (pair? t)
	     (?func (car t) (car T) ...)
	     (loop  (cdr t) (cdr T) ...)))))
    ))

(define-syntax ($fold-right/stx stx)
  ;;Like FOLD-RIGHT, but expand the loop inline.  The "function" to be folded must be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  ;;
  ;;This  implementation: is  non-tail recursive,  assumes proper  list arguments  of
  ;;equal length.
  ;;
  (syntax-case stx ()
    ((_ ?combine ?knil ?ell0 ?ell ...)
     (with-syntax (((ELL ...) (generate-temporaries #'(?ell ...))))
       #'(let recur ((knil ?knil)
		     (ell0 ?ell0)
		     (ELL  ?ell)
		     ...)
	   (if (pair? ell0)
	       ;;This is FOLD-RIGHT so: first we recur, then we combine.
	       (?combine (car ell0) (car ELL) ... (recur knil (cdr ell0) (cdr ELL) ...))
	     knil))))
    ))


;;;; done

#| end of library |# )

;;; end of file
