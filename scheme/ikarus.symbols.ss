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


(library (ikarus.symbols)
  (export
    ;; R6RS functions
    symbol->string

    ;; generating symbols
    gensym gensym? gensym->unique-string gensym-prefix
    gensym-count print-gensym

    ;; internal functions
    $unintern-gensym

    ;; object properties
    getprop putprop remprop property-list

    ;; ???
    top-level-value top-level-bound? set-top-level-value!
    symbol-value symbol-bound? set-symbol-value!
    reset-symbol-proc! system-value system-value-gensym)
  (import (except (ikarus)
		  ;; R6RS functions
		  symbol->string

		  ;; generating symbols
		  gensym gensym? gensym->unique-string gensym-prefix
		  gensym-count print-gensym

		  ;; object properties
		  getprop putprop remprop property-list

		  ;; ???
		  top-level-value top-level-bound? set-top-level-value!
		  symbol-value symbol-bound? set-symbol-value!
		  reset-symbol-proc! system-value system-value-gensym

		  ;; internal functions
		  $unintern-gensym)
    (except (ikarus system $symbols)
	    $unintern-gensym)
    (ikarus system $pairs)
    (ikarus system $fx))


;;;; syntax helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

;;; --------------------------------------------------------------------

(define-inline (%assert-argument-is-symbol who obj . ?body)
  (unless (symbol? who)
    (assertion-violation who "expected symbol as argument" obj))
  (begin (values) . ?body))

(define-inline (%assert-argument-is-gensym who obj . ?body)
  (unless (symbol? who)
    (assertion-violation who "expected generated symbol as argument" obj))
  (begin (values) . ?body))

(define-inline (%unsafe.assert-argument-is-bound-symbol who obj . ?body)
  (when ($unbound-object? obj)
    (assertion-violation who "expected bound symbol as argument" obj))
  (begin (values) . ?body))

(define-inline (%unsafe.assert-argument-is-procedure who obj . ?body)
  (unless (procedure? obj)
    (assertion-violation who "expected procedure as argument" obj))
  (begin (values) . ?body))


(define gensym
  (case-lambda
   (()
    ($make-symbol #f))
   ((s)
    (define who 'gensym)
    (cond ((string? s)
	   ($make-symbol s))
	  ((symbol? s)
	   ($make-symbol ($symbol-string s)))
	  (else
	   (assertion-violation who
	     "expected string or symbol as argument" s))))))

(define (gensym? x)
  (and (symbol? x)
       (let ((s ($symbol-unique-string x)))
	 (and s #t))))

(define ($unintern-gensym x)
  (define who 'unintern-gensym)
  (%assert-argument-is-symbol who x
    (foreign-call "ikrt_unintern_gensym" x)
    (void)))

(define (gensym->unique-string x)
  (define who 'gensym->unique-string)
  (%assert-argument-is-symbol who x
    (let ((us ($symbol-unique-string x)))
      (cond ((string? us)
	     us)
	    ((not us)
	     (assertion-violation who "expected generated symbol as argument" x))
	    (else
	     (let f ((x x))
	       (let ((id (uuid)))
		 ($set-symbol-unique-string! x id)
		 (if (foreign-call "ikrt_intern_gensym" x)
		     id
		   (f x)))))))))

(define gensym-prefix
  (make-parameter
      "g"
    (lambda (x)
      (if (string? x)
	  x
	(assertion-violation 'gensym-prefix "not a string" x)))))

(define gensym-count
  (make-parameter
      0
    (lambda (x)
      (if (and (fixnum? x) ($fx>= x 0))
	  x
	(assertion-violation 'gensym-count "not a valid count" x)))))

(define print-gensym
  (make-parameter
      #t
    (lambda (x)
      (if (or (boolean? x) (eq? x 'pretty))
	  x
	(assertion-violation 'print-gensym "not in #t|#f|pretty" x)))))


(define (top-level-value x)
  (define who 'top-level-value)
  (%assert-argument-is-symbol who x
    (let ((v ($symbol-value x)))
      (when ($unbound-object? v)
	(raise
	 (condition (make-undefined-violation)
		    (make-who-condition 'eval)
		    (make-message-condition "unbound variable")
		    (make-irritants-condition (list (string->symbol (symbol->string x)))))))
      v)))

(define (top-level-bound? x)
  (define who 'top-level-bound?)
  (%assert-argument-is-symbol who x
    (not ($unbound-object? ($symbol-value x)))))

(define (set-top-level-value! x v)
  (define who 'set-top-level-value!)
  (%assert-argument-is-symbol who x
    ($set-symbol-value! x v)))

(define (symbol-value x)
  (define who 'symbol-value)
  (%assert-argument-is-symbol who x
    (let ((v ($symbol-value x)))
      (%unsafe.assert-argument-is-bound-symbol who v
	v))))

(define (symbol-bound? x)
  (define who 'symbol-bound?)
  (%assert-argument-is-symbol who x
    (not ($unbound-object? ($symbol-value x)))))

(define (set-symbol-value! x v)
  (define who 'set-symbol-value!)
  (%assert-argument-is-symbol who x
    ($set-symbol-value! x v)
    ;;If V  is not a  procedure: raise an  exception if the  client code
    ;;attemtps to apply it.
    ($set-symbol-proc!  x (if (procedure? v)
			      v
			    (lambda args
			       (assertion-violation 'apply
				 "not a procedure" ($symbol-value x)))))))

(define (reset-symbol-proc! x)
  (define who 'reset-symbol-proc!)
  (%assert-argument-is-symbol who x
    (let ((v ($symbol-value x)))
      ($set-symbol-proc! x (if (procedure? v)
			       v
			     (lambda args
			       (assertion-violation 'apply
				 "not a procedure" (top-level-value x))))))))

#;(define string->symbol
    (lambda (x)
      (unless (string? x)
	(die 'string->symbol "not a string" x))
      (foreign-call "ikrt_string_to_symbol" x)))


(define (symbol->string x)
  ;;Defined by  R6RS.  Return the name  of the symbol X  as an immutable
  ;;string.
  ;;
  (define who 'symbol->string)
  (%assert-argument-is-symbol who x
    (let ((str ($symbol-string x)))
      (or str
	  (let ((ct (gensym-count)))
;;;FIXME What if gensym-count is a bignum?
	    (let ((str (string-append (gensym-prefix) (fixnum->string ct))))
	      ($set-symbol-string! x str)
	      (gensym-count ($fxadd1 ct))
	      str))))))


;;;; property lists

(define (putprop x k v)
  ;;Add a new property K with value V to the property list of the symbol
  ;;X.  K must be a symbol, V can be any value.
  ;;
  (define who 'putprop)
  (%assert-argument-is-symbol who x
    (%assert-argument-is-symbol who k
      (let ((p ($symbol-plist x)))
	(cond ((assq k p)
	       => (lambda (x)
		    (set-cdr! x v)))
	      (else
	       ($set-symbol-plist! x (cons (cons k v) p))))))))

(define (getprop x k)
  ;;Return  the value  of the  property K  in the  property list  of the
  ;;symbol X; if K is not set return false.  K must be a symbol.
  ;;
  (define who 'getprop)
  (%assert-argument-is-symbol who x
    (%assert-argument-is-symbol who k
      (let ((p ($symbol-plist x)))
	(cond ((assq k p)
	       => cdr)
	      (else #f))))))

(define (remprop x k)
  ;;Remove property K from the list associated to the symbol X.
  ;;
  (define who 'remprop)
  (%assert-argument-is-symbol who x
    (%assert-argument-is-symbol who k
      (let ((plist ($symbol-plist x)))
	(unless (null? plist)
	  (let ((a ($car plist)))
	    (if (eq? ($car a) k)
		($set-symbol-plist! x ($cdr plist))
	      (let loop ((q     plist)
			 (plist ($cdr plist)))
		(unless (null? plist)
		  (let ((a ($car plist)))
		    (if (eq? ($car a) k)
			($set-cdr! q ($cdr plist))
		      (loop plist ($cdr plist))))))))))
      )))

(define (property-list x)
  ;;Return a new association list  representing the property list of the
  ;;symbol X.
  ;;
  ;;NOTE We duplicated the structure of the internl association list, so
  ;;that modifying the returned value does not affect the internal state
  ;;of the property list.
  ;;
  (define who 'property-list)
  (%assert-argument-is-symbol who x
    (let loop ((ls    ($symbol-plist x))
	       (accum '()))
      (if (null? ls)
	  accum
	(let ((a ($car ls)))
	  (loop ($cdr ls)
		(cons (cons ($car a) ($cdr a))
		      accum)))))))


(define system-value-gensym (gensym))

(define (system-value x)
  (define who 'system-value)
  (%assert-argument-is-symbol who x
    (cond ((getprop x system-value-gensym)
	   => (lambda (g)
		(let ((v ($symbol-value g)))
		  (if ($unbound-object? v)
		      (assertion-violation 'system-value "not a system symbol" x)
		    v))))
	  (else
	   (assertion-violation 'system-value "not a system symbol" x)))))


;;;; done

)

;;; end of file
;;Local Variables:
;;coding: utf-8-unix
;;eval: (put '%assert-argument-is-symbol 'scheme-indent-function 'my-scheme-indent-to-body)
;;eval: (put '%assert-argument-is-gensym 'scheme-indent-function 'my-scheme-indent-to-body)
;;eval: (put '%assert-argument-is-bound-symbol 'scheme-indent-function 'my-scheme-indent-to-body)
;;eval: (put '%assert-argument-is-procedure 'scheme-indent-function 'my-scheme-indent-to-body)
;;End:
