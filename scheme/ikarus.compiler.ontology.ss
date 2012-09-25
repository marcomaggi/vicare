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

(define-syntax define-ontology
  ;;Define the operators:  T, T:description, T?, T=?, T:and,  T:or to be
  ;;applied to Scheme values.
  ;;
  ;;
  (lambda (x)
    (define (main x)
      (syntax-case x ()
	((_ T T:description T? T:=? T:and T:or
	    (?name0 ?cls0)
	    (?name  ?cls)
	    ...)
	 (with-syntax ((((NAME PREDNAME VAL) ...)
			(%generate-base-cases #'T #'?name0 #'((?name0 ?cls0) (?name ?cls) ...))))
	   #'(begin
	       (define-struct T
		 (n))
	       #;(define-record-type (T make-T T?)
		 (sealed #t)
		 (fields (immutable n T-n)))

	       (define (T:and x0 x1)
		 (make-T (bitwise-and (T-n x0) (T-n x1))))

	       (define (T:or x0 x1)
		 (make-T (bitwise-ior (T-n x0) (T-n x1))))

	       (define (test x v)
		 (cond ((= 0 ($fxlogand x v))	'no)
		       ((= v ($fxlogor  x v))	'yes)
		       (else			'maybe)))
;;;The following was the original version (Marco Maggi; Oct 27, 2011).
;;;
;;;	       (define (test x v)
;;;		 (let ((bits (bitwise-and x v)))
;;;		   (cond
;;;		    ((= 0 (bitwise-and x v))	'no)
;;;		    ((= v (bitwise-ior x v))	'yes)
;;;		    (else			'maybe))))

	       (define NAME (make-T VAL))
	       ...

	       (define (PREDNAME x)
		 (test (T-n x) VAL))
	       ...

	       (define (T:description x)
		 (let* ((ls '())
			(ls (case (PREDNAME x)
			      ((yes) (cons 'NAME ls))
			      (else  ls)))
			...)
		   ls))

	       (define (T:=? x y)
		 (= (T-n x) (T-n y)))

	       )))))

;;; --------------------------------------------------------------------

    (define (%generate-base-cases T main ls)
      (define (%value-name x)
	;;Return an identifier with name "T:x" in the lexical context of
	;;T.
        (datum->syntax T (string->symbol (string-append (symbol->string (syntax->datum T)) ":"
							(symbol->string x)))))
      (define (%predicate-name x)
	;;Return an  identifier with name "T:x?" in  the lexical context
	;;of T.
        (datum->syntax T (string->symbol (string-append (symbol->string (syntax->datum T)) ":"
							(symbol->string x) "?"))))
      (define (%property-names ls)
	;;Given a list of S-expressions each having one of the formats:
	;;
	;;   (INAME (inclusive IPROP ...))
	;;   (ENAME (exclusive EPROP ...))
	;;
	;;return the list: (IPROP ...)
	;;
	(if (null? ls)
	    '()
	  (let ((first (car ls))
		(rest  (%property-names (cdr ls))))
	    (let ((name (car  first))
		  (info (cadr first)))
	      (case (car info)
		((exclusive)
		 rest)
		((inclusive)
		 (append (cdr info) rest))
		(else
		 (assert #f)))))))
      (let* ((main-datum (syntax->datum main))
	     (ls-datum   (syntax->datum ls))
	     (pnames     (%property-names ls-datum))
	     (alist      (%make-ontology main-datum ls-datum))
             ;;Filter out from ALIST all then "inclusive" entries.
	     (alist1     (remp (lambda (entry)
				 (memq (car entry) pnames))
			   alist)))
	(map (lambda (entry)
	       (list (%value-name     (car entry))
		     (%predicate-name (car entry))
		     (cdr entry)))
	  alist1)))

;;; --------------------------------------------------------------------

    (define (%make-ontology main-id ls)
      ;;LS is a list of S-expressions each having one of the formats:
      ;;
      ;;   (INAME (inclusive IPROP ...))
      ;;   (ENAME (exclusive EPROP ...))
      ;;
      (define (set-cons x ls)
        (cond
	 ((memq x ls) ls)
	 (else (cons x ls))))
      (define (union ls1 ls2)
        (cond
	 ((null? ls1) ls2)
	 (else (union (cdr ls1) (set-cons (car ls1) ls2)))))
      (define (difference ls1 ls2)
        (cond
	 ((null? ls1) '())
	 ((memq (car ls1) ls2) (difference (cdr ls1) ls2))
	 (else (cons (car ls1) (difference (cdr ls1) ls2)))))
      (define (collect-names ls)
        (syntax-case ls ()
          (() '())
          (((name (of name* ...)) . rest)
           (union (cons #'name #'(name* ...)) (collect-names #'rest)))))
      (define (expand x all)
        (define (lookup x ls)
          (cond
	   ((null? ls) (values 'tag '()))
	   (else
	    (let ((a (car ls)))
	      (cond
	       ((eq? x (car a))
		(values (cadr a) (cdr ls)))
	       (else
		(let-values (((xp ls) (lookup x (cdr ls))))
		  (values xp (cons a ls)))))))))
        (let f ((x x) (ls ls))
          (let-values (((xp ls) (lookup x ls)))
            (cond
	     ((pair? xp)
	      (cons (car xp) (map (lambda (x) (f x ls)) (cdr xp))))
	     ((eq? xp 'tag) x)
	     (else (error 'expand-lookup "invalid" xp))))))
      (define (rename alist x)
        (cond
	 ((symbol? x) (cdr (assq x alist)))
	 (else (cons (car x) (map (lambda (x) (rename alist x)) (cdr x))))))
      (define (enumerate ls)
        (let f ((i 1) (ls ls))
          (cond
	   ((null? ls) '())
	   (else (cons i (f (* i 2) (cdr ls)))))))
      (define (unique-elements x)
        (define (exclude m ls)
          (cond
	   ((null? ls) '())
	   ((zero? (bitwise-and m (car ls)))
	    (cons (car ls) (exclude m (cdr ls))))
	   (else (exclude m (cdr ls)))))
        (define (exclusive* m* x**)
          (cond
	   ((null? (cdr m*)) (values (car m*) (car x**)))
	   (else
	    (let-values (((m1 x1*) (values (car m*) (car x**)))
			 ((m2 x2*) (exclusive* (cdr m*) (cdr x**))))
	      (let ((x1* (exclude m2 x1*))
		    (x2* (exclude m1 x2*)))
		(values (bitwise-ior m1 m2) (append x1* x2*)))))))
        (define (inclusive* m* x**)
          (cond
	   ((null? (cdr m*)) (values (car m*) (car x**)))
	   (else
	    (let-values (((m1 x1*) (values (car m*) (car x**)))
			 ((m2 x2*) (inclusive* (cdr m*) (cdr x**))))
	      (values (bitwise-ior m1 m2)
		      (remp not
			(apply append
			       (map (lambda (x)
				      (map (lambda (y)
					     (if (= (bitwise-and m1 m2 x)
						    (bitwise-and m1 m2 y))
						 (bitwise-ior x y)
					       #f))
					x2*))
				 x1*))))))))
        (define (f* ls)
          (cond
	   ((null? ls) (values '() '()))
	   (else
	    (let-values (((m x*) (f (car ls)))
			 ((m* x**) (f* (cdr ls))))
	      (values (cons m m*) (cons x* x**))))))
        (define (f x)
          (cond
	   ((integer? x) (values x (list x)))
	   (else
	    (let ((tag (car x)) (ls (cdr x)))
	      (let-values (((m* x**) (f* ls)))
		(case tag
		  ((exclusive) (exclusive* m* x**))
		  ((inclusive) (inclusive* m* x**))
		  (else (error 'f "invalid"))))))))
        (let-values (((m ls) (f x)))
          ls))
      (define (expand-names alist)
        (lambda (n)
          (let f ((alist alist))
            (cond
	     ((null? alist) '())
	     ((zero? (bitwise-and n (cdar alist)))
	      (f (cdr alist)))
	     (else
	      (cons (caar alist) (f (cdr alist))))))))
      (define (extend-alist* ls alist)
        (define (extend-alist x alist)
          (define (lookup x)
            (cond
	     ((assq x alist) => cdr)
	     (else (error 'lookup "cannot find" x alist))))
          (let ((name (car x)) (info (cadr x)))
            (let ((tag (car info)) (x* (map lookup (cdr info))))
              (case tag
                ((exclusive)
                 (cons (cons name (apply bitwise-ior x*)) alist))
                ((inclusive)
                 (assert (= (apply bitwise-ior x*) (apply bitwise-and x*)))
                 (cons (cons name (apply bitwise-ior x*)) alist))
                (else (assert #f))))))
        (cond
	 ((null? ls) alist)
	 (else
	  (extend-alist (car ls)
			(extend-alist* (cdr ls) alist)))))
      (let* ((names (difference (collect-names ls) (map car ls)))
             (names-alist (map cons names (enumerate names))))
        (let* ((expanded (expand main-id ls))
               (renamed (rename names-alist expanded)))
          (let* ((unique* (list-sort < (unique-elements renamed)))
                 (canonicals (map (expand-names names-alist) unique*)))
            (let* ((canonical-alist (map cons canonicals (enumerate canonicals)))
                   (seed-alist
                    (map
			(lambda (x)
			  (let ((ls (filter (lambda (y) (memq x (car y))) canonical-alist)))
			    (cons x (apply bitwise-ior (map cdr ls)))))
                      names)))
              (extend-alist* ls seed-alist))))))

    (main x)))

;;See below for the expansion of this syntax.
(define-ontology T T:description T? T=? T:and T:or
  (object		(inclusive obj-tag obj-immediacy obj-truth))
  (obj-immediacy	(exclusive nonimmediate immediate))
  (immediate		(exclusive fixnum boolean null char void))
  (obj-truth		(exclusive false non-false))
  (obj-tag		(exclusive procedure string vector pair null
				   boolean char number void bytevector
				   symbol other-object))
  (boolean		(exclusive true false))
  (number		(inclusive number-tag number-size number-exactness))
  (number-size		(exclusive negative zero positive))
  (number-tag		(exclusive fixnum flonum other-number))
  (number-exactness	(exclusive exact inexact))
  (exact		(exclusive fixnum other-exact))
  (inexact		(exclusive flonum other-inexact))
  )

#!eof


;;;The expansion of:

(define-ontology T T:description T? T=? T:and T:or
  (object		(inclusive obj-tag obj-immediacy obj-truth))
  (obj-immediacy	(exclusive nonimmediate immediate))
  (immediate		(exclusive fixnum boolean null char void))
  (obj-truth		(exclusive false non-false))
  (obj-tag		(exclusive procedure string vector pair null
				   boolean char number void bytevector
				   symbol other-object))
  (boolean		(exclusive true false))
  (number		(inclusive number-tag number-size number-exactness))
  (number-size		(exclusive negative zero positive))
  (number-tag		(exclusive fixnum flonum other-number))
  (number-exactness	(exclusive exact inexact))
  (exact		(exclusive fixnum other-exact))
  (inexact		(exclusive flonum other-inexact))
  )

;;;is the following:

(begin
  (define-record-type (T make-T T?)
    (sealed #t)
    (fields (immutable n T-n)))

  (define (T:and x0 x1)
    (make-T (bitwise-and (T-n x0) (T-n x1))))

  (define (T:or x0 x1)
    (make-T (bitwise-ior (T-n x0) (T-n x1))))

  (define (test x v)
    (cond ((= 0 ($fxlogand x v))	'no)
	  ((= v ($fxlogor  x v))	'yes)
	  (else				'maybe)))

;;; 25 bits                                                            9876543210
;;;                                                          9876543210
;;;                                                      4321
  (define T:object		(make-T 16777215))	;111111111111111111111111
  (define T:immediate		(make-T 232504))	;      111000110000111000
  (define T:boolean		(make-T 3072))		;            110000000000
  (define T:number		(make-T 16773120))	;111111111111000000000000
  (define T:exact		(make-T 258048))	;      111111000000000000
  (define T:inexact		(make-T 16515072))	;111111000000000000000000
  (define T:nonimmediate	(make-T 16544711))	;111111000111001111000111
  (define T:non-false		(make-T 16776191))	;111111111111101111111111
  (define T:other-object	(make-T 1))		;                       1
  (define T:symbol		(make-T 2))		;                      10
  (define T:bytevector		(make-T 4))		;                     100
  (define T:void		(make-T 8))		;                    1000
  (define T:char		(make-T 16))		;                   10000
  (define T:null		(make-T 32))		;                  100000
  (define T:pair		(make-T 64))		;                 1000000
  (define T:vector		(make-T 128))		;                10000000
  (define T:string		(make-T 256))		;               100000000
  (define T:procedure		(make-T 512))		;              1000000000
  (define T:false		(make-T 1024))		;             10000000000
  (define T:true		(make-T 2048))		;            100000000000
  (define T:positive		(make-T 2396160))	;  1001001001000000000000
  (define T:zero		(make-T 4792320))	; 10010010010000000000000
  (define T:negative		(make-T 9584640))	;100100100100000000000000
  (define T:other-number	(make-T 1863680))	;   111000111000000000000
  (define T:other-exact		(make-T 28672))		;         111000000000000
  (define T:fixnum		(make-T 229376))	;      111000000000000000
  (define T:other-inexact	(make-T 1835008))	;   111000000000000000000
  (define T:flonum		(make-T 14680064))	;111000000000000000000000

  (define (T:object? x)		(test (T-n x) 16777215))
  (define (T:immediate? x)	(test (T-n x) 232504))
  (define (T:boolean? x)	(test (T-n x) 3072))
  (define (T:number? x)		(test (T-n x) 16773120))
  (define (T:exact? x)		(test (T-n x) 258048))
  (define (T:inexact? x)	(test (T-n x) 16515072))
  (define (T:nonimmediate? x)	(test (T-n x) 16544711))
  (define (T:non-false? x)	(test (T-n x) 16776191))
  (define (T:other-object? x)	(test (T-n x) 1))
  (define (T:symbol? x)		(test (T-n x) 2))
  (define (T:bytevector? x)	(test (T-n x) 4))
  (define (T:void? x)		(test (T-n x) 8))
  (define (T:char? x)		(test (T-n x) 16))
  (define (T:null? x)		(test (T-n x) 32))
  (define (T:pair? x)		(test (T-n x) 64))
  (define (T:vector? x)		(test (T-n x) 128))
  (define (T:string? x)		(test (T-n x) 256))
  (define (T:procedure? x)	(test (T-n x) 512))
  (define (T:false? x)		(test (T-n x) 1024))
  (define (T:true? x)		(test (T-n x) 2048))
  (define (T:positive? x)	(test (T-n x) 2396160))
  (define (T:zero? x)		(test (T-n x) 4792320))
  (define (T:negative? x)	(test (T-n x) 9584640))
  (define (T:other-number? x)	(test (T-n x) 1863680))
  (define (T:other-exact? x)	(test (T-n x) 28672))
  (define (T:fixnum? x)		(test (T-n x) 229376))
  (define (T:other-inexact? x)	(test (T-n x) 1835008))
  (define (T:flonum? x)		(test (T-n x) 14680064))

  (define (T:description x)
    (let* ((ls '())
           (ls (case (T:object? x)
		 ((yes) (cons 'T:object ls))
		 (else ls)))
           (ls (case (T:immediate? x)
		 ((yes) (cons 'T:immediate ls))
		 (else ls)))
           (ls (case (T:boolean? x)
		 ((yes) (cons 'T:boolean ls))
		 (else ls)))
           (ls (case (T:number? x)
		 ((yes) (cons 'T:number ls))
		 (else ls)))
           (ls (case (T:exact? x)
		 ((yes) (cons 'T:exact ls))
		 (else ls)))
           (ls (case (T:inexact? x)
		 ((yes) (cons 'T:inexact ls))
		 (else ls)))
           (ls (case (T:nonimmediate? x)
		 ((yes) (cons 'T:nonimmediate ls))
		 (else ls)))
           (ls (case (T:non-false? x)
		 ((yes) (cons 'T:non-false ls))
		 (else ls)))
           (ls (case (T:other-object? x)
		 ((yes) (cons 'T:other-object ls))
		 (else ls)))
           (ls (case (T:symbol? x)
		 ((yes) (cons 'T:symbol ls))
		 (else ls)))
           (ls (case (T:bytevector? x)
		 ((yes) (cons 'T:bytevector ls))
		 (else ls)))
           (ls (case (T:void? x)
		 ((yes) (cons 'T:void ls))
		 (else ls)))
           (ls (case (T:char? x)
		 ((yes) (cons 'T:char ls))
		 (else ls)))
           (ls (case (T:null? x)
		 ((yes) (cons 'T:null ls))
		 (else ls)))
           (ls (case (T:pair? x)
		 ((yes) (cons 'T:pair ls))
		 (else ls)))
           (ls (case (T:vector? x)
		 ((yes) (cons 'T:vector ls))
		 (else ls)))
           (ls (case (T:string? x)
		 ((yes) (cons 'T:string ls))
		 (else ls)))
           (ls (case (T:procedure? x)
		 ((yes) (cons 'T:procedure ls))
		 (else ls)))
           (ls (case (T:false? x)
		 ((yes) (cons 'T:false ls))
		 (else ls)))
           (ls (case (T:true? x)
		 ((yes) (cons 'T:true ls))
		 (else ls)))
           (ls (case (T:positive? x)
		 ((yes) (cons 'T:positive ls))
		 (else ls)))
           (ls (case (T:zero? x)
		 ((yes) (cons 'T:zero ls))
		 (else ls)))
           (ls (case (T:negative? x)
		 ((yes) (cons 'T:negative ls))
		 (else ls)))
           (ls (case (T:other-number? x)
		 ((yes) (cons 'T:other-number ls))
		 (else ls)))
           (ls (case (T:other-exact? x)
		 ((yes) (cons 'T:other-exact ls))
		 (else ls)))
           (ls (case (T:fixnum? x)
		 ((yes) (cons 'T:fixnum ls))
		 (else ls)))
           (ls (case (T:other-inexact? x)
		 ((yes) (cons 'T:other-inexact ls))
		 (else ls)))
           (ls (case (T:flonum? x)
		 ((yes) (cons 'T:flonum ls))
		 (else ls))))
      ls))

  (define (T=? x y)
    (= (T-n x) (T-n y))))


(define (do-test expr result expected)
  (if (equal? result expected)
      (printf "OK: ~s -> ~s\n" expr expected)
      (error 'test "failed/got/expected" expr result expected)))

(define-syntax test
  (syntax-rules ()
    ((_ expr expected) (do-test 'expr expr 'expected))))

(test (T:object? T:object) yes)
(test (T:object? T:true)   yes)
(test (T:true? T:object)   maybe)
(test (T:true? T:true)     yes)
(test (T:true? T:false)    no)
(test (T:true? T:null)     no)
(test (T:non-false? T:true) yes)
(test (T:non-false? T:null) yes)
(test (T:non-false? T:false) no)
(test (T:non-false? T:boolean) maybe)
(test (T:non-false? T:object) maybe)
(test (T:boolean? T:true) yes)
(test (T:boolean? T:false) yes)
(test (T:boolean? (T:or T:true T:false)) yes)
(test (T:boolean? (T:and T:true T:false)) no)
(test (T:object? (T:and T:true T:false)) no)

;;; end of file
