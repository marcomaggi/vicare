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


#!vicare
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

    ;; conversion functions
    string-or-symbol->string
    string-or-symbol->symbol

    ;; unsafe operations
    $symbol->string
    $getprop $putprop $remprop $property-list


    ;; internals handling of symbols and special symbols
    unbound-object	unbound-object?
    top-level-value	top-level-bound?	set-top-level-value!
    symbol-value	symbol-bound?		set-symbol-value!
    reset-symbol-proc!

    system-value	system-value-gensym
    system-label	system-label-gensym
    system-id		system-id-gensym)
  (import (except (vicare)
		  ;; R6RS functions
		  symbol->string

		  ;; generating symbols
		  gensym gensym? gensym->unique-string gensym-prefix
		  gensym-count print-gensym

		  ;; object properties
		  getprop putprop remprop property-list

		  ;; conversion functions
		  string-or-symbol->string
		  string-or-symbol->symbol

		  ;; internals handling of symbols and special symbols
		  unbound-object	unbound-object?
		  symbol-value		symbol-bound?		set-symbol-value!
		  reset-symbol-proc!

		  top-level-value	top-level-bound?	set-top-level-value!
		  system-value		system-value-gensym
		  system-label		system-label-gensym
		  system-id		system-id-gensym

		  ;; internal functions
		  $unintern-gensym)
    ;;NOTE This is a delicate library defining some low level feature like the system
    ;;gensyms.   Let's try  to import  only  the system  libraries, without  creating
    ;;external dependencies.  (Marco Maggi; Mon Apr 14, 2014)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $strings)
    (only (vicare system $numerics)
	  $add1-integer)
    (except (vicare system $symbols)
	    $symbol->string	$unintern-gensym
	    $getprop		$putprop		$remprop
	    $property-list
	    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri
	    ;;May 23, 2014)
	    system-value-gensym)
    (for (prefix (vicare) sys.)
      expand))


;;;; helpers

(define (string-or-symbol? obj)
  (or (string? obj)
      (symbol? obj)))


(case-define* gensym
  (()
   ($make-symbol #f))
  ((s)
   (cond ((string? s)
	  ($make-symbol s))
	 ((symbol? s)
	  ($make-symbol ($symbol-string s)))
	 (else
	  (procedure-argument-violation __who__
	    "expected string or symbol as argument" s)))))

(define (gensym? x)
  (and (symbol? x)
       (let ((s ($symbol-unique-string x)))
	 (and s #t))))

(define* ($unintern-gensym {x symbol?})
  (foreign-call "ikrt_unintern_gensym" x)
  (void))

(define* (gensym->unique-string {x symbol?})
  (let ((us ($symbol-unique-string x)))
    (cond ((string? us)
	   us)
	  ((not us)
	   (procedure-argument-violation __who__
	     "expected generated symbol as argument" x))
	  (else
	   (let loop ((x x))
	     (let ((id (uuid)))
	       ($set-symbol-unique-string! x id)
	       (if (foreign-call "ikrt_intern_gensym" x)
		   id
		 (loop x))))))))

(define gensym-prefix
  (make-parameter
      "g"
    (lambda (x)
      (if (string? x)
	  x
	(procedure-argument-violation 'gensym-prefix "not a string" x)))))

(define gensym-count
  (make-parameter
      0
    (lambda (x)
      (if (and (fixnum? x) ($fx>= x 0))
	  x
	(procedure-argument-violation 'gensym-count "not a valid count" x)))))

(define print-gensym
  (make-parameter
      #t
    (lambda (x)
      (if (or (boolean? x) (eq? x 'pretty))
	  x
	(procedure-argument-violation 'print-gensym "not in #t|#f|pretty" x)))))


(define (unbound-object? x)
  ($unbound-object? x))

(define (unbound-object)
  (foreign-call "ikrt_unbound_object"))

(define* (top-level-value {x symbol?})
  (receive-and-return (v)
      ($symbol-value x)
    (when ($unbound-object? v)
      (raise
       (condition (make-undefined-violation)
		  (make-who-condition 'eval)
		  (make-message-condition "unbound variable")
		  (make-irritants-condition (list (string->symbol (symbol->string x)))))))))

(define* (top-level-bound? {x symbol?})
  (not ($unbound-object? ($symbol-value x))))

(define* (set-top-level-value! {x symbol?} v)
  ($set-symbol-value! x v))

(define* (symbol-value {x symbol?})
  (receive-and-return (obj)
      ($symbol-value x)
    (when ($unbound-object? obj)
      (procedure-argument-violation __who__
	"expected bound symbol as argument" x obj))))

(define* (symbol-bound? {x symbol?})
  (not ($unbound-object? ($symbol-value x))))

(define* (set-symbol-value! {x symbol?} v)
  ($set-symbol-value! x v)
  ;;If  V is  not a  procedure: raise  an exception  if the  client code
  ;;attemtps to apply it.
  ($set-symbol-proc!  x (if (procedure? v)
			    v
			  (lambda args
			    (procedure-argument-violation 'apply
			      "not a procedure"
			      `(top-level-value-of-symbol ,x)
			      ($symbol-value x) args)))))

(define* (reset-symbol-proc! {x symbol?})
  ;;X is meant to be a location gensym.  If the value currently in the field "value"
  ;;of X is a closure object: store such value also in the field "proc" of X.
  ;;
  ;;NOTE Whenever binary code performs a call to a global closure object, it does the
  ;;following:
  ;;
  ;;* From the relocation vector of the current code object: retrieve the loc gensym
  ;;  of the procedure to call.
  ;;
  ;;* From the loc gensym: extract the value of the "proc" slot, which is meant to be
  ;;  a closure object.
  ;;
  ;;* Actually call the closure object.
  ;;
  (let ((v ($symbol-value x)))
    ($set-symbol-proc! x (if (procedure? v)
			     v
			   (lambda args
			     (procedure-argument-violation 'apply
			       "not a procedure"
			       `(top-level-value-of-symbol ,x)
			       (top-level-value x) args))))))


(define* (symbol->string {x symbol?})
  ;;Defined by  R6RS.  Return the name  of the symbol X  as an immutable
  ;;string.
  ;;
  ($symbol->string x))

(define ($symbol->string x)
  ;;Return the string name of the symbol X.
  ;;
  (let ((str ($symbol-string x)))
    (or str
	(let ((ct (gensym-count)))
	  (receive-and-return (str)
	      (string-append (gensym-prefix) (number->string ct))
	    ($set-symbol-string! x str)
	    (gensym-count ($add1-integer ct)))))))

(define* (string-or-symbol->string {obj string-or-symbol?})
  ;;Defined by Vicare.  If OBJ is a string return a copy of it; if it is
  ;;a symbol return a new string object equal to its string name.
  ;;
  (let ((str (if (string? obj)
		 obj
	       ($symbol->string obj))))
    (substring str 0 ($string-length str))))

(define* (string-or-symbol->symbol {obj string-or-symbol?})
  ;;Defined by Vicare.  If OBJ is a  symbol return it; if it is a string
  ;;return a symbol having it as string name.
  ;;
  (if (symbol? obj)
      obj
    (string->symbol obj)))


;;;; property lists

(define* (putprop {x symbol?} {k symbol?} v)
  ;;Add a new property K with value V to the property list of the symbol
  ;;X.  K must be a symbol, V can be any value.
  ;;
  ($putprop x k v))

(define ($putprop x k v)
  (let ((p ($symbol-plist x)))
    (cond ((assq k p)
	   => (lambda (x)
		($set-cdr! x v)))
	  (else
	   ($set-symbol-plist! x (cons (cons k v) p))))))

(define* (getprop {x symbol?} {k symbol?})
  ;;Return  the value  of the  property K  in the  property list  of the
  ;;symbol X; if K is not set return false.  K must be a symbol.
  ;;
  ($getprop x k))

(define ($getprop x k)
  (let ((p ($symbol-plist x)))
    (cond ((assq k p)
	   => cdr)
	  (else #f))))

(define* (remprop {x symbol?} {k symbol?})
  ;;Remove property K from the list associated to the symbol X.
  ;;
  ($remprop x k))

(define ($remprop x k)
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
		  (loop plist ($cdr plist)))))))))))

(define* (property-list {x symbol?})
  ;;Return a new association list  representing the property list of the
  ;;symbol X.
  ;;
  ;;NOTE We duplicated the structure of the internl association list, so
  ;;that modifying the returned value does not affect the internal state
  ;;of the property list.
  ;;
  ($property-list x))

(define ($property-list x)
  (let loop ((ls    ($symbol-plist x))
	     (accum '()))
    (if (null? ls)
	accum
      (let ((a ($car ls)))
	(loop ($cdr ls)
	      (cons (cons ($car a) ($cdr a))
		    accum))))))


(define-syntax (expand-time-gensym stx)
  (syntax-case stx ()
    ((_ ?template)
     (let* ((tmp (syntax->datum #'?template))
	    (fxs (vector->list (foreign-call "ikrt_current_time_fixnums_2")))
	    (str (apply string-append tmp (map (lambda (N)
						 (string-append "." (number->string N)))
					    fxs)))
	    (sym (sys.gensym str)))
       (with-syntax
	   ((SYM (datum->syntax #'here sym)))
	 (fprintf (current-error-port) "expand-time gensym ~a\n" sym)
	 #'(quote SYM))))))

(define system-value-gensym
  ;;Notice that this gensym is generated a-new every time the boot image
  ;;is initialised.   We must avoid  the source optimizer  to precompute
  ;;and hard-code a value.
  (expand-time-gensym "system-value-gensym"))

(define system-label-gensym
  ;;Notice that this gensym is generated a-new every time the boot image
  ;;is initialised.   We must avoid  the source optimizer  to precompute
  ;;and hard-code a value.
  (expand-time-gensym "system-label-gensym"))

(define system-id-gensym
  ;;Notice that this gensym is generated a-new every time the boot image
  ;;is initialised.   We must avoid  the source optimizer  to precompute
  ;;and hard-code a value.
  (expand-time-gensym "system-id-gensym"))

(define* (system-value {x symbol?})
  ;;When  the boot  image is  loaded, it  initialises itself;  for every
  ;;primitive function (CONS, CAR, ...)  one of the operations is to put
  ;;the actual  function (a closure  object) in  the "value" field  of a
  ;;gensym, and then put such gensym  in the property list of the symbol
  ;;being the name of the primitive,  using an internal gensym (bound to
  ;;SYSTEM-VALUE-GENSYM) as key.
  ;;
  ;;For example, this is more or less what happens to CONS:
  ;;
  ;;   (define G-cons (gensym "cons"))
  ;;   ($set-symbol-value 'G-cons #<procedure cons>)
  ;;   (putprop 'cons system-value-gensym 'G-cons)
  ;;
  ;;so later we can do:
  ;;
  ;;   ($symbol-value (getprop 'G-cons system-value-gensym))
  ;;   => #<procedure cons>
  ;;
  ;;or use the equivalent public API:
  ;;
  ;;   (system-value 'cons)    => #<procedure cons>
  ;;
  (cond ((getprop x system-value-gensym)
	 => (lambda (g)
	      (receive-and-return (v)
		  ($symbol-value g)
		(when ($unbound-object? v)
		  (procedure-argument-violation __who__ "not a system symbol" x)))))
	(else
	 (procedure-argument-violation __who__ "not a system symbol" x))))

(define* (system-label {x symbol?})
  ;;If  X is  the  symbol  name of  a  primitive  procedure: return  its
  ;;syntactic binding label gensym, otherwise return false.
  ;;
  (getprop x system-label-gensym))

(define* (system-id {x symbol?})
  ;;If  X is  the  symbol  name of  a  primitive  procedure: return  its
  ;;syntactic binding identifier, otherwise return false.
  ;;
  (getprop x system-id-gensym))


;;;; done

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.symbols"))

#| end of library |# )

;;; end of file
;;Local Variables:
;;coding: utf-8-unix
;;End:
