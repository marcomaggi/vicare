;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
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

(library (ikarus.symbol-table)
  (export
    string->symbol
    $initialize-symbol-table!
    $symbol-table-size
    $log-symbol-table-status)
  (import (except (ikarus)
		  string->symbol)
    (except (ikarus system $symbols)
	    $symbol-table-size
	    $log-symbol-table-status)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))


;;;; data types

;;A  SYMBOL-TABLE structure  is in  practice a  hash table  holding weak
;;references to  interned symbols.  When the number  of interned symbols
;;equals the number of buckets (whatever the distribution), the table is
;;enlarged  doubling  the  number   of  buckets.   The  table  is  never
;;restricted by reducing the number of buckets.
;;
;;Constructor: make-symbol-table LEN MASK VECTOR GUARDIAN
;;
;;Predicate: symbol-table? OBJ
;;
;;Field name: size
;;Accessor: symbol-table-size TABLE
;;Mutator: set-symbol-table-size! TABLE
;;  The number of symbols interned  in the table.  The maximum number of
;;  interned symbols is the greatest fixnum.
;;
;;Field name: mask
;;Accessor: symbol-table-mask TABLE
;;Mutator: set-symbol-table-mask! TABLE
;;  A bitmask used to convert a  symbol's hash number into an index into
;;  the vector in the VEC field as follows:
;;
;;    (define index (fxlogand mask (symbol-hash symbol)))
;;
;;Field name: vec
;;Accessor: symbol-table-vec TABLE
;;Mutator: set-symbol-table-vec! TABLE
;;  Actual collection of interned symbols, this is the vector of buckets
;;  of the  hash table.  Each  element in the  vector is a list  of weak
;;  pairs holding  the interned symbols.  The maximum  number of buckets
;;  is half the greatest fixnum.
;;
;;Field name: guardian
;;Accessor: symbol-table-guardian TABLE
;;Mutator: set-symbol-table-guardian! TABLE
;;  A guardian  in which interned  symbols are registered.   Whenever an
;;  interned symbol is garbage  collected, the guardian extracts it from
;;  the table.
;;
;;  FIXME The guardian is scanned  for symbols to be uninterned whenever
;;  a new symbol is interned.
;;
(define-struct symbol-table
  (size mask vec guardian))

(define MAX-NUMBER-OF-BUCKETS
  (fxdiv (greatest-fixnum) 2))


;;This is  the actual table used  at run-time.  Notice that  the mask is
;;always one less than the vector size.
;;
;;By  experiments it has  been determined  that, after  initialising the
;;table with the symbols from the boot process, the number of buckets is
;;4096.  (Marco Maggi; Oct 31, 2011)
;;
(define THE-SYMBOL-TABLE
  (make-symbol-table 0 4095 (make-vector 4096 '()) (make-guardian))
  #;(make-symbol-table 0 #b11 (make-vector 4 '()) (make-guardian)))

(define ($symbol-table-size)
  (symbol-table-size THE-SYMBOL-TABLE))

(define ($log-symbol-table-status)
  ;;Write to the current error  port a description of the current symbol
  ;;table status.
  ;;
  (define port
    (current-error-port))
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%newline)
    (newline port))
  (%display "Vicare internal symbol table status:\n")
  (%display "\tnumber of interned symbols: ")
  (%display ($symbol-table-size))
  (%newline)
  (%display "\tnumber of hash table buckets: ")
  (%display (vector-length (symbol-table-vec THE-SYMBOL-TABLE)))
  (%newline)
  (%newline)
  (flush-output-port port))

(define ($initialize-symbol-table!)
  ;;Retrieve  the vector  used by  the  symbol table  in the  "vicare"
  ;;executable (constructed  while booting), retrieve  all the entries
  ;;from it and store them in THE-SYMBOL-TABLE.
  ;;
  (define (intern-car x)
    (when (pair? x)
      (let ((sym (unsafe.car x)))
	(intern-symbol! sym
			(unsafe.fxand (symbol-hash sym) (symbol-table-mask THE-SYMBOL-TABLE))
			THE-SYMBOL-TABLE))
      (intern-car (unsafe.cdr x))))
  (vector-for-each intern-car (foreign-call "ikrt_get_symbol_table")))


(define (string->symbol x)
  ;;Defined by R6RS.  Return a symbol whose name is X.
  ;;
  ;;Lookup the  symbol in  the symbol table:  if it is  already there,
  ;;return it; else create a new entry.
  ;;
  (define who 'string->symbol)
  (with-arguments-validation (who)
      ((string  x))
    (lookup x (string-hash x) THE-SYMBOL-TABLE)))

(define (lookup str ih table)
  ;;Subroutine of  STRING->SYMBOL.  Search for  the string STR  in TABLE
  ;;using IH  as its hash  number.  STR must  be the name of  an already
  ;;interned  symbol or  a  symbol to  be  interned.  TABLE  must be  an
  ;;instance  of  SYMBOL-TABLE structure.   Return  a  reference to  the
  ;;interned symbol.
  ;;
  (define (chain-lookup str idx table ls)
    (if (null? ls)
	(let ((sym (intern str idx table)))
;;; doesn't need eq? check there
	  (bleed-guardian sym table))
      (let ((a (unsafe.car ls)))
	;;FIXME  Can we use  $SYMBOL-STRING rather  than SYMBOL->STRING?
	;;(Marco Maggi; Oct 31, 2011)
	(if (string=? str (symbol->string a))
	    (bleed-guardian a table)
	  (chain-lookup str idx table (unsafe.cdr ls))))))
  (let ((idx (unsafe.fxand ih (symbol-table-mask table))))
    (chain-lookup str idx table (vector-ref (symbol-table-vec table) idx))))


(define (intern str idx table)
  ;;Given a string STR being the  name of a symbol to be interned, store
  ;;it in the  symbol TABLE and return the  associated symbol.  IDX must
  ;;be the index in the TABLE's vector.
  ;;
  (let ((sym ($make-symbol str)))
    ($set-symbol-unique-string! sym #f)
    (intern-symbol! sym idx table)
    sym))

(define (intern-symbol! sym idx table)
  ;;Given a symbol SYM being to  be interned, store it in the TABLE; IDX
  ;;must be the index in the TABLE's vector.  Return unspecified values.
  ;;
  ;;The symbol is registered in the  TABLE's guardian, so that it can be
  ;;removed if garbage collected.
  ;;
  (let ((number-of-interned-symbols (symbol-table-size table)))
    (if (unsafe.fx= number-of-interned-symbols (greatest-fixnum))
	(assertion-violation 'intern-symbol!
	  "reached maximum number of interned symbols" sym)
      (let ((vec (symbol-table-vec table)))
	(vector-set! vec idx (weak-cons sym (vector-ref vec idx)))
	((symbol-table-guardian table) sym)
	(let ((n (unsafe.fxadd1 number-of-interned-symbols)))
	  (set-symbol-table-size! table n)
	  (when (unsafe.fx= n (symbol-table-mask table))
	    (extend-table table)))))))

(define (bleed-guardian sym table)
  ;;Subroutine of LOOKUP.  Scan the  symbols queried in the guardian for
  ;;removal from TABLE.  Notice that this can happen:
  ;;
  ;;1. The symbol CIAO is interned.
  ;,
  ;;2.  CIAO  is garbage  collected.   It is  still  in  TABLE but  also
  ;;registered in the guardian for removal.
  ;;
  ;;3. CIAO is created again before the guardian uninterns it.
  ;;
  ;;If  the symbol  SYM is  interned in  TABLE but  already  queried for
  ;;removal in the guardian: do not unintern it and register it again in
  ;;the guardian.
  ;;
  ;;If  a symbol queried  in the  guardian for  removal is  not unbound:
  ;;unintern it.
  ;;
  (define-inline (dead? sym)
    (and ($unbound-object? ($symbol-value sym))
	 (null? ($symbol-plist sym))))
  (let ((g (symbol-table-guardian table)))
    (cond ((g) => (lambda (a)
		    (let loop ((a a))
		      (cond ((eq? a sym) (g a))
			    ((begin
			       (if (dead? a)
				   (unintern a table)
				 (g a))
			       (g))
			     => loop)))))))
  sym)

(define (unintern sym table)
  ;;Remove the interned symbol SYM from TABLE.
  ;;
  (set-symbol-table-size! table (unsafe.fxsub1 (symbol-table-size table)))
  (let ((idx (unsafe.fxand (symbol-hash sym) (symbol-table-mask table)))
	(vec (symbol-table-vec table)))
    (let ((ls (vector-ref vec idx)))
      (if (eq? (unsafe.car ls) sym)
	  (vector-set! vec idx (cdr ls))
	(let loop ((prev ls)
		   (ls   (unsafe.cdr ls)))
	  (if (eq? (unsafe.car ls) sym)
	      (unsafe.set-cdr! prev (unsafe.cdr ls))
	    (loop ls (unsafe.cdr ls))))))))


(define (extend-table table)
  ;;Double the size of the vector in TABLE, which must be an instance of
  ;;SYMBOL-TABLE structure.
  ;;
  (let* ((vec1	(symbol-table-vec table))
	 (len1	(unsafe.vector-length vec1)))
    ;;Do not allow the vector length to exceed the maximum fixnum.
    (when (unsafe.fx< len1 MAX-NUMBER-OF-BUCKETS)
      ;;If  we start  with an  even and  power of  2 vector  length, the
      ;;length is always even and power of 2...
      (let* ((len2	(unsafe.fx+ len1 len1))
	     ;;... and the mask is always composed by all the significant
	     ;;bits set to 1.
	     (mask	(unsafe.fxsub1 len2))
	     (vec2	(make-vector len2 '())))
	(define (insert p)
	  (unless (null? p)
	    (let ((a    (unsafe.car p))
		  (rest (unsafe.cdr p)))
	      ;;Recycle this pair by setting its cdr to the value in the
	      ;;vector.
	      (let ((idx (unsafe.fxand (symbol-hash a) mask)))
		(unsafe.set-cdr! p (unsafe.vector-ref vec2 idx))
		(unsafe.vector-set! vec2 idx p))
	      (insert rest))))
	;;Insert in the new vector all the entries in the old vector.
	(vector-for-each insert vec1)
	;;Update the TABLE structure.
	(set-symbol-table-vec!  table vec2)
	(set-symbol-table-mask! table mask)))))


;;;; done

)

;;; end of file
