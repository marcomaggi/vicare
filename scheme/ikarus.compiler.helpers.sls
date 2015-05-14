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
    sl-apply-label-func
    if-building-rotation-boot-image?		cond-expand
    expand-time-gensym
    %list-of-one-item?				fxincr!
    $map/stx					$for-each/stx
    $fold-right/stx
    struct-case					define-structure
    print-compiler-warning-message
    remq1
    union					difference)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config))


;;;; stuff

(define sl-apply-label-func
  (make-parameter (lambda ()
		    (assertion-violation 'sl-apply-label-func
		      "parameter not initialised"))))


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

;;; --------------------------------------------------------------------

(define-syntax struct-case
  ;;Specialised  CASE syntax  for data  structures.  Notice  that we  could use  this
  ;;syntax for  any set of struct  types, not only  the struct types defined  in this
  ;;library.
  ;;
  ;;Given:
  ;;
  ;;  (define-struct alpha (a b c))
  ;;  (define-struct beta  (d e f))
  ;;
  ;;we want to expand:
  ;;
  ;;  (struct-case ?expr
  ;;    ((alpha a b)
  ;;     (do-this))
  ;;    ((beta d e f)
  ;;     (do-that))
  ;;    (else
  ;;     (do-other)))
  ;;
  ;;into:
  ;;
  ;;  (let ((v ?expr))
  ;;    (if ($struct/rtd? v (type-descriptor alpha))
  ;;        (let ((a ($struct-ref v 0))
  ;;              (b ($struct-ref v 1)))
  ;;          (do-this))
  ;;      (if ($struct/rtd? v (type-descriptor beta))
  ;;          (let ((d ($struct-ref v 0))
  ;;                (e ($struct-ref v 1))
  ;;                (f ($struct-ref v 2)))
  ;;            (do-that))
  ;;        (begin
  ;;          (do-other)))))
  ;;
  ;;notice that: in the clauses the pattern "(alpha  a b)" must list the fields A and
  ;;B in the same order in which they appear in the struct type definition.
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?expr ?clause ...)
	 (with-syntax ((BODY (%generate-body #'(?clause ...))))
	   #'(let ((v ?expr))
	       (import (only (vicare system $structs)
			     $struct-ref $struct/rtd?))
	       BODY)))))

    (define (%generate-body clauses-stx)
      (syntax-case clauses-stx (else)
        (()
	 (with-syntax ((INPUT-FORM stx))
	   #'(error 'compiler "unknown struct type" v 'INPUT-FORM)))

        (((else ?body0 ?body ...))
	 #'(let () ?body0 ?body ...))

        ((((?struct-name ?field-name ...) ?body0 ?body ...) . ?other-clauses)
	 (identifier? #'?struct-name)
         (with-syntax
	     ((RTD		#'(type-descriptor ?struct-name))
	      ((FIELD-NAM ...)  (%filter-field-names #'(?field-name ...)))
	      ((FIELD-IDX ...)	(%enumerate #'(?field-name ...) 0))
	      (ALTERN		(%generate-body #'?other-clauses)))
	   #'(if ($struct/rtd? v RTD)
		 (let ((FIELD-NAM ($struct-ref v FIELD-IDX))
		       ...)
		   ?body0 ?body ...)
	       ALTERN)))))

    (define (%filter-field-names field*.stx)
      ;;FIELD*.STX  must be  a  syntax object  holding a  list  of identifiers  being
      ;;underscores or struct  field names.  Filter out the underscores  and return a
      ;;list of identifiers representing the true field names.
      ;;
      (syntax-case field*.stx ()
        (() '())
        ((?field-name . ?other-names)
	 (eq? '_ (syntax->datum #'?field-name))
	 (%filter-field-names #'?other-names))
        ((?field-name . ?other-names)
	 (cons #'?field-name (%filter-field-names #'?other-names)))
	))

    (define (%enumerate field*.stx next-field-idx)
      ;;FIELD*.STX  must be  a  syntax object  holding a  list  of identifiers  being
      ;;underscores  or  struct  field  names.    NEXT-FIELD-IDX  must  be  a  fixnum
      ;;representing the index of the first field in FIELD*.STX.
      ;;
      ;;Return  a  list  of  fixnums  representing  the  indexes  of  the  fields  in
      ;;FIELD*.STX, discarding the fixnums matching underscores.
      ;;
      (syntax-case field*.stx ()
        (() '())
        ((?field-name . ?other-names)
	 (eq? '_ (syntax->datum #'?field-name))
	 (%enumerate #'?other-names (fxadd1 next-field-idx)))
        ((?field-name . ?other-names)
	 (cons next-field-idx (%enumerate #'?other-names (fxadd1 next-field-idx))))
	))

    (main stx)))

;;; --------------------------------------------------------------------

(define-syntax define-structure
  ;;A syntax to define struct types for compatibility with the notation used in Oscar
  ;;Waddell's thesis; it allows  the definition of struct types in  which some of the
  ;;fields are initialised  by the maker with default values,  while other fields are
  ;;initialised with arguments handed to the maker.
  ;;
  ;;Synopsis:
  ;;
  ;;  (define-structure ?name
  ;;    (?field-without ...)
  ;;    ((?field-with ?default)
  ;;	 ...))
  ;;
  ;;where: ?NAME is the struct type name, ?FIELD-WITHOUT are identifier names for the
  ;;fields  without default,  ?FIELD-WITH are  identifier names  for the  fields with
  ;;default, ?DEFAULT are the default values.
  ;;
  ;;The maker accepts a number of arguments equal to the number of ?FIELD-WITHOUT, in
  ;;the same order in which they appear in the struct definition.
  ;;
  ;;(It is a bit ugly...  Marco Maggi; Oct 10, 2012)
  ;;
  (lambda (stx)
    (define (%format-id ctxt template-str . args)
      (datum->syntax ctxt (string->symbol
			   (apply format template-str (map syntax->datum args)))))
    (syntax-case stx ()
      ((_ ?name (?field-without-default ...) ((?field-with-default ?default) ...))
       (identifier? #'?name)
       (let ((name.id #'?name))
	 (with-syntax
	     ((PRED			(%format-id name.id "~s?" name.id))
	      (MAKER			(%format-id name.id "make-~s" name.id))
	      ((GETTER ...)		(map (lambda (x)
					       (%format-id name.id "~s-~s" name.id x))
					  #'(?field-without-default ... ?field-with-default ...)))
	      ((UNSAFE-GETTER ...)	(map (lambda (x)
					       (%format-id name.id "$~s-~s" name.id x))
					  #'(?field-without-default ... ?field-with-default ...)))
	      ((SETTER ...)		(map (lambda (x)
					       (%format-id name.id "set-~s-~s!" name.id x))
					  #'(?field-without-default ... ?field-with-default ...)))
	      ((UNSAFE-SETTER ...)	(map (lambda (x)
					       (%format-id name.id "$set-~s-~s!" name.id x))
					  #'(?field-without-default ... ?field-with-default ...))))
	   #'(module (?name PRED
			    GETTER ... UNSAFE-GETTER ...
			    SETTER ... UNSAFE-SETTER ...
			    MAKER)
	       (module private
		 (?name PRED
			GETTER ... UNSAFE-GETTER ...
			SETTER ... UNSAFE-SETTER ...
			MAKER)
		 (define-struct ?name
		   (?field-without-default ... ?field-with-default ...)))
	       (module (MAKER)
		 (define (MAKER ?field-without-default ...)
		   (import private)
		   (MAKER ?field-without-default ... ?default ...)))
	       (module (?name PRED
			      GETTER ... UNSAFE-GETTER ...
			      SETTER ... UNSAFE-SETTER ...)
		 (import private))))))

      ((_ ?name (?field ...))
       (identifier? #'?name)
       #'(define-struct ?name (?field ...)))
      )))


;;;; helper functions

(define (print-compiler-warning-message template . args)
  (when (option.verbose?)
    (let ((P (current-error-port)))
      (display "vicare: compiler warning: " P)
      (apply fprintf P template args)
      (newline P))))

;;; --------------------------------------------------------------------

(define (remq1 x ls)
  ;;Scan the list  LS and remove only the  first instance of object X,  using EQ?  as
  ;;comparison function; return the resulting list which may share its tail with LS.
  ;;
  (if (pair? ls)
      (if (eq? x (car ls))
	  (cdr ls)
	(let ((t (remq1 x (cdr ls))))
	  (cond ((eq? t (cdr ls))
		 ls)
		(else
		 (cons (car ls) t)))))
    '()))

(define (union s1 s2)
  ;;Return a  list which  is the  union between  the lists  S1 and  S2, with  all the
  ;;duplicates removed.
  ;;
  (define (add* s1 s2)
    (if (pair? s1)
	(add (car s1)
	     (add* (cdr s1) s2))
      s2))
  (define (add x s)
    (if (memq x s)
	s
      (cons x s)))
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else
	 (add* s1 s2))))

(define (difference s1 s2)
  ;;Return a list holding  all the elements from the list S1 not  present in the list
  ;;S2.
  ;;
  (define (rem* s2 s1)
    (if (pair? s2)
	(remq1 (car s2)
	       (rem* (cdr s2) s1))
      s1))
  (cond ((null? s1) '())
	((null? s2) s1)
	(else
	 (rem* s2 s1))))


;;;; done

#| end of library |# )

;;; end of file
