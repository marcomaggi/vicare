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


;;;; Introduction
;;
;;For  an explanation  of  primitive operation  definition and  internal
;;representation: see the file "pass-specify-rep.ss".
;;


;;;; syntax helpers

(define-syntax /section
  (syntax-rules ()))

(define-syntax section
  (syntax-rules (/section)
    ((section e* ... /section) (begin e* ...))))


;;;; helpers

(section

 (define (prm op . args)
   ;;Perform  the primitive operation  OP applying  it to  the arguments
   ;;ARGS.
   ;;
   (make-primcall op args))

 (define (nop)
   (make-primcall 'nop '()))

 (define (tag-test x mask tag)
   ;;Primary tag test.  X must be a word referencing a Scheme value.
   ;;
   ;;Test if X is a word of  type TAG.  Use MASK to extract bits from X,
   ;;then verify if the bits are equal to TAG.
   ;;
   (if mask
       (prm '= (prm 'logand x (K mask)) (K tag))
     (prm '= x (K tag))))

 (define (sec-tag-test x primary-mask primary-tag secondary-mask secondary-tag)
   ;;Primary and secondary  tag test; to be used when  a Scheme value is
   ;;implemented by specialising another value (example: port values are
   ;;specialised vector values).  X must  be a word referencing a Scheme
   ;;value.
   ;;
   ;;Test  if X  is  a word  of  type PRIMARY-TAG:  use PRIMARY-MASK  to
   ;;extract  bits  from  X,  then  verify  if the  bits  are  equal  to
   ;;PRIMARY-TAG.
   ;;
   ;;If the  primary test is successful:  extract the first  word W from
   ;;the  memory block referenced  by X,  use SECONDARY-MASK  to extract
   ;;bits from W, then verify if the bits are equal to SECONDARY-TAG.
   ;;
   (make-conditional
       (tag-test x primary-mask primary-tag)
     (tag-test (prm 'mref x (K (- primary-tag))) secondary-mask secondary-tag)
     (K #f)))

 (define (dirty-vector-set address)
   (define shift-bits 2)
   (prm 'mset32
	(prm 'mref pcr (K pcb-dirty-vector))
	(prm 'sll (prm 'srl address (K pageshift)) (K shift-bits))
	(K dirty-word)))

 (define (smart-dirty-vector-set addr what)
   (struct-case what
     ((constant t)
      (if (or (fx? t) (immediate? t))
	  (prm 'nop)
	(dirty-vector-set addr)))
     ((known x t)
      (cond ((eq? (T:immediate? t) 'yes)
	     (record-optimization 'smart-dirty-vec t)
	     (nop))
	    (else
	     (smart-dirty-vector-set addr x))))
     (else
      (dirty-vector-set addr))))

 (module (mem-assign)

   (define (mem-assign v base offset)
     ;;Store V at OFFSET from BASE.
     ;;
     ;;V must be a struct instance representing recordized code.
     ;;
     ;;X must be a struct instance representing a (possibly tagged) base
     ;;address.
     ;;
     ;;OFFSET must be an exact integer representing an offset in bytes.
     ;;
     ;;Generate low level recordized code  needed to store the result of
     ;;evaluating V at OFFSET from the base heap address BASE.
     ;;
     (struct-case v
       ((constant value)
	(if (or (fx? value)
		(immediate? value))
	    (prm 'mset base (K offset) (T v))
	  (%slow-mem-assign v base offset)))
       ((known expr type)
	(cond ((eq? (T:immediate? type) 'yes)
	       (record-optimization 'mem-assign v)
	       (prm 'mset base (K offset) (T expr)))
	      (else
	       (%slow-mem-assign expr base offset))))
       (else
	(%slow-mem-assign v base offset))))

   (define (%slow-mem-assign v base offset)
     (with-tmp ((t (prm 'int+ base (K offset))))
       (make-seq (prm 'mset t (K 0) (T v))
		 (dirty-vector-set t))))

   #| end of module: mem-assign |# )

 (define (align-code UNknown-amount known-amount)
   ;;Given  a compile-time  known  amount of  bytes  and a  compile-time
   ;;UNknown amount of bytes, which must be summed to obtain the size of
   ;;a memory block to be allocated: compute and return the actual total
   ;;amount of  bytes to be  allocated on  the heap to  keep subsequence
   ;;memory blocks allocated.
   ;;
   ;;See details about memory allocation in the documentation.  See also
   ;;the ALIGN function defined in the library (ikarus.compiler).
   ;;
   (define-inline (%shift-left ?expr)	(prm 'sll ?expr (K align-shift)))
   (define-inline (%shift-right ?expr)	(prm 'sra ?expr (K align-shift)))
   (%shift-left
    (%shift-right
     (prm 'int+ UNknown-amount (K (+ known-amount (sub1 object-alignment)))))))

 (define (assert-fixnum x)
   (struct-case x
     ((constant i)
      (if (fx? i)
	  (nop)
	(interrupt)))
     ((known expr type)
      (case-symbols (T:fixnum? type)
	((yes) (nop))
	((no)  (interrupt))
	(else  (assert-fixnum expr))))
     (else
      (interrupt-unless (cogen-pred-fixnum? x)))))

 (define (assert-string x)
   (struct-case x
     ((constant s)
      (if (string? s)
	  (nop)
	(interrupt)))
     ((known expr type)
      (case-symbols (T:string? type)
	((yes)
	 (record-optimization 'assert-string x)
	 (nop))
	((no)
	 (interrupt))
	(else
	 (assert-string expr))))
     (else
      (interrupt-unless (cogen-pred-string? x)))))

 /section)


(section

 (define-primop base-rtd safe
   ;;The base RTD  of all the struct  types is stored in  the C language
   ;;structure PCB.
   ;;
   ((V) (prm 'mref pcr (K pcb-base-rtd)))
   ((P) (K #t))
   ((E) (prm 'nop)))

 (define-primop void safe
   ;;This  is the  definition  of  the Scheme  function  VOID.  It  just
   ;;returns the void object, which is true.
   ;;
   ((V) (K void-object))
   ((P) (K #t))
   ((E) (prm 'nop)))

 (define-primop nop unsafe
   ;;This is the definition of the primitive operation NOP.
   ;;
   ((E) (prm 'nop)))

 (define-primop neq? unsafe
   ;;This is the implementation of the Scheme function NEQ?.
   ;;
   ((P x y) (prm '!= (T x) (T y)))
   ((E x y) (nop)))

 (define-primop eq? safe
   ;;This is the implementation of the Scheme function EQ?.
   ;;
   ((P x y) (prm '= (T x) (T y)))
   ((E x y) (nop)))

 (define (equable? x)
   (or (fx? x)
       (not (number? x))))

 (define (equable-constant? x)
   (struct-case x
     ((constant xv)
      (equable? xv))
     ((known expr)
      (equable-constant? expr))
     (else
      #f)))

 (define-primop eqv? safe
   ;;This is the implementation of the primitive operation EQV?.
   ;;
   ;;Notice  that at  the Scheme  level the  EQV? predicate  is the  one
   ;;exported by (ikarus predicates).
   ;;
   ((P x y)
    (if (or (equable-constant? x)
	    (equable-constant? y))
	(prm '= (T x) (T y))
      (interrupt)))
   ((E x y) (nop)))

 (define-primop null? safe
   ;;This is the implementation of the Scheme function NULL?.
   ;;
   ((P x) (prm '= (T x) (K nil)))
   ((E x) (nop)))

 (define-primop not safe
   ;;This is the implementation of the Scheme function NOT.
   ;;
   ((P x) (prm '= (T x) (K bool-f)))
   ((E x) (nop)))

 (define-primop eof-object safe
   ;;This is the implementation of the Scheme function EOF-OBJECT.
   ;;
   ((V) (K eof))
   ((P) (K #t))
   ((E) (nop)))

 (define-primop eof-object? safe
   ;;This is the implementation of the Scheme function EOF-OBJECT?.
   ;;
   ((P x) (prm '= (T x) (K eof)))
   ((E x) (nop)))

 (define-primop $unbound-object? unsafe
   ;;This   is   the   implementation   of   the   primitive   operation
   ;;$UNBOUND-OBJECT?.
   ;;
   ((P x) (prm '= (T x) (K unbound)))
   ((E x) (nop)))

 (define-primop immediate? safe
   ;;This is the implementation of the Scheme function IMMEDIATE?.
   ;;
   ((P x)
    (make-conditional
	(tag-test (T x) fx-mask fx-tag)
      (K #t)
      (tag-test (T x) 7 7)))
   ((E x) (nop)))

 (define-primop boolean? safe
   ;;This is the implementation of the Scheme function BOOLEAN?.
   ;;
   ((P x)
    (tag-test (T x) bool-mask bool-tag))
   ((E x) (nop)))

 (define-primop bwp-object? safe
   ;;This is the implementation of the Scheme function BWP-OBJECT?.
   ;;
   ((P x) (prm '= (T x) (K bwp-object)))
   ((E x) (nop)))

 (define-primop $forward-ptr? unsafe
   ;;Primitive operation.  When a Scheme  object's memory block is moved
   ;;by the garbage collector: the first word of the old memory block is
   ;;overwritten with a  special value, the "forward  pointer".  See the
   ;;garbage collector for details.
   ;;
   ;;This predicate evaluates to true if X is such a machine word.
   ;;
   ;;This  definition  must  be  kept   in  sync  with  the  C  language
   ;;preprocessor constant IK_FORWARD_PTR.
   ;;
   ;;FIXME  At present  this operation  is  not exported  by any  Scheme
   ;;library, should it be?  (Marco Maggi; Oct 17, 2012)
   ;;
   ((P x)
    (prm '= (T x) (K -1)))
   ((E x)
    (nop)))

 (define-primop pointer-value unsafe
   ;;FIXME What is this for?  (Marco Maggi; Oct 17, 2012)
   ;;
   ((V x) (prm 'logand
	       (prm 'srl (T x) (K 1))
	       (K (* -1 fx-scale))))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-primop $arg-list unsafe
   ;;Return  the  value  of  the  field "arg_list"  in  the  C  language
   ;;structure PCB.
   ;;
   ((V) (prm 'mref pcr (K pcb-arg-list)))
   ((P) (K #t))
   ((E) (nop)))

 (define-primop $collect-key unsafe
   ;;Return  the value  of the  field  "collect_key" in  the C  language
   ;;structure PCB.
   ;;
   ((V)   (prm 'mref pcr (K pcb-collect-key)))
   ((E x) (prm 'mset pcr (K pcb-collect-key) (T x))))

 (define-primop $memq safe
   ((P x ls)
    (struct-case ls
      ((constant ls)
       (if (list? ls)
	   ;;We assume that a list hard-coded in the source is not "very
	   ;;long",   so  we   unroll  the   search  into   sequence  of
	   ;;conditionals.
	   (with-tmp ((x (T x)))
	     (let loop ((ls ls))
	       (cond ((null? ls)
		      (K #f))
		     ((null? ($cdr ls))
		      (prm '= x (T (K ($car ls)))))
		     (else
		      (make-conditional
			  (prm '= x (T (K ($car ls))))
			(K #t) ;return a boolean
			(loop ($cdr ls)))))))
	 (interrupt)))
      ((known expr)
       (cogen-pred-$memq x expr))
      (else
       (interrupt))))
   ((V x ls)
    (struct-case ls
      ((constant ls)
       (if (list? ls)
	   ;;We assume that a list hard-coded in the source is not "very
	   ;;long",   so  we   unroll  the   search  into   sequence  of
	   ;;conditionals.
	   (with-tmp ((x (T x)))
	     (let loop ((ls ls))
	       (cond ((null? ls)
		      (K bool-f))
		     (else
		      (make-conditional
			  (prm '= x (T (K ($car ls))))
			(T (K ls)) ;return the value
			(loop ($cdr ls)))))))
	 (interrupt)))
      ((known expr)
       (cogen-value-$memq x expr))
      (else
       (interrupt))))
   ((E x ls)
    (nop)))

 (define-primop memq safe
   ((P x ls)
    (cogen-pred-$memq x ls))
   ((V x ls)
    (cogen-value-$memq x ls))
   ((E x ls)
    (struct-case ls
      ((constant ls)
       (if (list? ls)
	   (nop)
	 (interrupt)))
      ((known expr)
       (cogen-effect-memq x expr))
      (else
       (interrupt)))))

 (define-primop memv safe
   ((V x ls)
    (struct-case ls
      ((constant lsv)
       (if (and (list? lsv)
		(andmap equable? lsv))
	   (cogen-value-$memq x ls)
	 (interrupt)))
      ((known expr t)
       (cogen-value-memv x expr))
      (else
       (interrupt))))
   ((P x ls)
    (struct-case ls
      ((constant lsv)
       (if (and (list? lsv)
		(andmap equable? lsv))
	   (cogen-pred-$memq x ls)
	 (interrupt)))
      ((known expr t)
       (cogen-pred-memv x expr))
      (else
       (interrupt))))
   ((E x ls)
    (struct-case ls
      ((constant val)
       (if (list? val)
	   (nop)
	 (interrupt)))
      ((known expr t)
       (cogen-effect-memv x expr))
      (else
       (interrupt)))))

 /section)


;;;; pairs
;;
;;A  pair is  a fixed-length  block of  memory composed  of  two machine
;;words; the least significant bits of  a reference to a pair are a pair
;;tag.
;;
;;  |------------------------|-------------| reference to pair
;;        heap offset           pair tag
;;
(section

 (define-primop pair? safe
   ((P x)
    (tag-test (T x) pair-mask pair-tag))
   ((E x) (nop)))

 (define-primop cons safe
   ((V a d)
    (with-tmp ((t (prm 'alloc (K pair-size) (K pair-tag))))
      (prm 'mset t (K (- disp-car pair-tag)) (T a))
      (prm 'mset t (K (- disp-cdr pair-tag)) (T d))
      t))
   ((P a d) (K #t))
   ((E a d) (prm 'nop)))

 (define-primop $car unsafe
   ((V x) (prm 'mref  (T x) (K (- disp-car pair-tag))))
   ((E x) (nop)))

 (define-primop $cdr unsafe
   ((V x) (prm 'mref  (T x) (K (- disp-cdr pair-tag))))
   ((E x) (nop)))

 (define-primop $set-car! unsafe
   ((E x v)
    (with-tmp ((x (T x)))
      (prm 'mset x (K (- disp-car pair-tag)) (T v))
      (smart-dirty-vector-set x v))))

 (define-primop $set-cdr! unsafe
   ((E x v)
    (with-tmp ((x (T x)))
      (prm 'mset x (K (- disp-cdr pair-tag)) (T v))
      (smart-dirty-vector-set x v))))

 (define (assert-pair x)
   (struct-case x
     ((known x t)
      (case (T:pair? t)
	((yes) (record-optimization 'assert-pair x) (nop))
	((no)  (interrupt))
	(else  (assert-pair x))))
     (else
      (interrupt-unless (tag-test x pair-mask pair-tag)))))

 (define-primop car safe
   ((V x)
    (with-tmp ((x (T x)))
      (assert-pair x)
      (prm 'mref x (K (- disp-car pair-tag)))))
   ((E x) (assert-pair (T x))))

 (define-primop cdr safe
   ((V x)
    (with-tmp ((x (T x)))
      (assert-pair x)
      (prm 'mref x (K (- disp-cdr pair-tag)))))
   ((E x) (assert-pair (T x))))

 (define-primop set-car! safe
   ((E x v)
    (with-tmp ((x (T x)))
      (assert-pair x)
      (prm 'mset x (K (- disp-car pair-tag)) (T v))
      (smart-dirty-vector-set x v))))

 (define-primop set-cdr! safe
   ((E x v)
    (with-tmp ((x (T x)))
      (assert-pair x)
      (prm 'mset x (K (- disp-cdr pair-tag)) (T v))
      (smart-dirty-vector-set x v))))


 (define (expand-cxr val ls)
   (cond
    ((null? ls) (T val))
    (else
     (with-tmp ((x (expand-cxr val (cdr ls))))
       (assert-pair x)
       (prm 'mref x
	    (case (car ls)
	      ((a)  (K (- disp-car pair-tag)))
	      (else (K (- disp-cdr pair-tag)))))))))

 (define-primop caar   safe ((V x) (expand-cxr x '(a a))))
 (define-primop cadr   safe ((V x) (expand-cxr x '(a d))))
 (define-primop cdar   safe ((V x) (expand-cxr x '(d a))))
 (define-primop cddr   safe ((V x) (expand-cxr x '(d d))))
 (define-primop caaar  safe ((V x) (expand-cxr x '(a a a))))
 (define-primop caadr  safe ((V x) (expand-cxr x '(a a d))))
 (define-primop cadar  safe ((V x) (expand-cxr x '(a d a))))
 (define-primop caddr  safe ((V x) (expand-cxr x '(a d d))))
 (define-primop cdaar  safe ((V x) (expand-cxr x '(d a a))))
 (define-primop cdadr  safe ((V x) (expand-cxr x '(d a d))))
 (define-primop cddar  safe ((V x) (expand-cxr x '(d d a))))
 (define-primop cdddr  safe ((V x) (expand-cxr x '(d d d))))
;;;(define-primop caaaar safe ((V x) (expand-cxr x '(a a a a))))
;;;(define-primop caaadr safe ((V x) (expand-cxr x '(a a a d))))
;;;(define-primop caadar safe ((V x) (expand-cxr x '(a a d a))))
;;;(define-primop caaddr safe ((V x) (expand-cxr x '(a a d d))))
;;;(define-primop cadaar safe ((V x) (expand-cxr x '(a d a a))))
;;;(define-primop cadadr safe ((V x) (expand-cxr x '(a d a d))))
;;;(define-primop caddar safe ((V x) (expand-cxr x '(a d d a))))
 (define-primop cadddr safe ((V x) (expand-cxr x '(a d d d))))
;;;(define-primop cdaaar safe ((V x) (expand-cxr x '(d a a a))))
;;;(define-primop cdaadr safe ((V x) (expand-cxr x '(d a a d))))
;;;(define-primop cdadar safe ((V x) (expand-cxr x '(d a d a))))
;;;(define-primop cdaddr safe ((V x) (expand-cxr x '(d a d d))))
;;;(define-primop cddaar safe ((V x) (expand-cxr x '(d d a a))))
;;;(define-primop cddadr safe ((V x) (expand-cxr x '(d d a d))))
;;;(define-primop cdddar safe ((V x) (expand-cxr x '(d d d a))))
;;;(define-primop cddddr safe ((V x) (expand-cxr x '(d d d d))))


 (define-primop list safe
   ((V) (K nil))
   ((V . arg*)
    (let ((n (length arg*)) (t* (map T arg*)))
      (with-tmp ((v (prm 'alloc (K (align (* n pair-size))) (K pair-tag))))
	(prm 'mset v (K (- disp-car pair-tag)) (car t*))
	(prm 'mset v
	     (K (- (+ disp-cdr (* (sub1 n) pair-size)) pair-tag))
	     (K nil))
	(let f ((t* (cdr t*)) (i pair-size))
	  (cond
	   ((null? t*) v)
	   (else
	    (with-tmp ((tmp (prm 'int+ v (K i))))
	      (prm 'mset tmp (K (- disp-car pair-tag)) (car t*))
	      (prm 'mset tmp (K (+ disp-cdr (- pair-size) (- pair-tag))) tmp)
	      (f (cdr t*) (+ i pair-size)))))))))
   ((P . arg*) (K #t))
   ((E . arg*) (nop)))

 (define-primop cons* safe
   ((V) (interrupt))
   ((V x) (T x))
   ((V a . a*)
    (let ((t* (map T a*)) (n (length a*)))
      (with-tmp ((v (prm 'alloc (K (* n pair-size)) (K pair-tag))))
	(prm 'mset v (K (- disp-car pair-tag)) (T a))
	(let f ((t* t*) (i pair-size))
	  (cond
	   ((null? (cdr t*))
	    (seq* (prm 'mset v (K (- i disp-cdr pair-tag)) (car t*)) v))
	   (else
	    (with-tmp ((tmp (prm 'int+ v (K i))))
	      (prm 'mset tmp (K (- disp-car pair-tag)) (car t*))
	      (prm 'mset tmp (K (- (- disp-cdr pair-tag) pair-size)) tmp)
	      (f (cdr t*) (+ i pair-size)))))))))
   ((P) (interrupt))
   ((P x) (P x))
   ((P a . a*) (K #t))
   ((E) (interrupt))
   ((E . a*) (nop)))

 /section)


(section  ;;; vectors
 (section ;;; helpers
  (define (vector-range-check x idx)
    (define (check-non-vector x idx)
      (define (check-fx idx)
	(seq*
	 (interrupt-unless (tag-test (T x) vector-mask vector-tag))
	 (with-tmp ((len (cogen-value-$vector-length x)))
	   (interrupt-unless (prm 'u< (T idx) len))
	   (interrupt-unless-fixnum len))))
      (define (check-? idx)
	(seq*
	 (interrupt-unless (tag-test (T x) vector-mask vector-tag))
	 (with-tmp ((len (cogen-value-$vector-length x)))
	   (interrupt-unless (prm 'u< (T idx) len))
	   (with-tmp ((t (prm 'logor len (T idx))))
	     (interrupt-unless-fixnum t)))))
      (struct-case idx
	((constant i)
	 (if (and (fx? i) (>= i 0))
	     (check-fx idx)
	   (check-? idx)))
	((known idx idx-t)
	 (case (T:fixnum? idx-t)
	   ((yes) (check-fx idx))
	   ((maybe) (vector-range-check x idx))
	   (else
	    (printf "vector check with mismatch index tag ~s" idx-t)
	    (vector-range-check x idx))))
	(else (check-? idx))))
    (define (check-vector x idx)
      (define (check-fx idx)
	(with-tmp ((len (cogen-value-$vector-length x)))
	  (interrupt-unless (prm 'u< (T idx) len))))
      (define (check-? idx)
	(seq*
	 (interrupt-unless-fixnum (T idx))
	 (with-tmp ((len (cogen-value-$vector-length x)))
	   (interrupt-unless (prm 'u< (T idx) len)))))
      (struct-case idx
	((constant i)
	 (if (and (fx? i) (>= i 0))
	     (check-fx idx)
	   (interrupt)))
	((known idx idx-t)
	 (case (T:fixnum? idx-t)
	   ((yes) (check-fx idx))
	   ((no)  (interrupt))
	   (else  (check-vector x idx))))
	(else (check-? idx))))
    (struct-case x
      ((known x t)
       (case (T:vector? t)
	 ((yes) (record-optimization 'check-vector x) (check-vector x idx))
	 ((no) (interrupt))
	 (else (check-non-vector x idx))))
      (else (check-non-vector x idx))))
  /section)

 (define-primop vector? safe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag fx-mask fx-tag))
   ((E x) (nop)))

(define-primop $make-vector unsafe
  ;;Notice that  the code  below does not  initialise the  vector's data
  ;;area leaving the items set to  whatever is there on the Scheme heap;
  ;;this can be bad for garbage  collection if the newly built vector is
  ;;moved before the items are  set to some correct Scheme object.  This
  ;;is why  the unsafe  operations library defines  a $MAKE-CLEAN-VECTOR
  ;;macro which builds a new vector  and clears the data area filling it
  ;;with zero fixnums (which is  fast from C language using "memset()").
  ;;(Marco Maggi; Jan 18, 2012)
  ;;
  ((V len)
   (struct-case len
     ((constant i)
      (if (and (fx? i) #f)
	  (interrupt)
	(with-tmp ((v (prm 'alloc
			   (K (align (+ (* i wordsize) disp-vector-data)))
			   (K vector-tag))))
	  (prm 'mset v
	       (K off-vector-length)
	       (K (* i fx-scale)))
	  v)))
     ((known expr t)
      (cogen-value-$make-vector expr))
     (else
      (with-tmp ((alen (align-code (T len) disp-vector-data)))
	(with-tmp ((v (prm 'alloc alen (K vector-tag))))
	  (prm 'mset v (K off-vector-length) (T len))
	  v)))))
  ((P len) (K #t))
  ((E len) (nop)))

 (define-primop make-vector safe
   ((V len)
    (with-tmp ((x (make-forcall "ikrt_make_vector1" (list (T len)))))
      (interrupt-when (prm '= x (K 0)))
      x)))

 (define-primop $vector-ref unsafe
   ((V x i)	;if it appears in "for expression value" position
    (or (struct-case i
	  ((constant i)
	   (and (fx? i)
		(fx>= i 0)
		(prm 'mref (T x)
		     (K (+ (* i wordsize) off-vector-data)))))
	  ((known i t)
	   (cogen-value-$vector-ref x i))
	  (else #f))
	;;Notice  that I  is not  multiplied  by the  WORDSIZE; this  is
	;;because I is  a fixnum and a fixnum representing  the index of
	;;the I-th slot in a vector,  taken as a "long", also represents
	;;the offset in bytes of the word in the I-th slot.
	(prm 'mref (T x)
	     (prm 'int+ (T i) (K off-vector-data)))
	))
   ((E x i)	;if it appears in "for side-effect" position
    (nop)))

 (define-primop $vector-length unsafe
   ((V x) (prm 'mref (T x) (K off-vector-length)))
   ((E x) (prm 'nop))
   ((P x) (K #t)))

 (define-primop vector-length safe
   ((V x)
    (struct-case x
      ((known x t)
       (case (T:vector? t)
	 ((yes) (record-optimization 'vector-length x) (cogen-value-$vector-length x))
	 ((no)  (interrupt))
	 (else  (cogen-value-vector-length x))))
      (else
       (seq*
	(interrupt-unless (tag-test (T x) vector-mask vector-tag))
	(with-tmp ((t (cogen-value-$vector-length x)))
	  (interrupt-unless-fixnum t)
	  t)))))
   ((E x)
    (struct-case x
      ((known x t)
       (case (T:vector? t)
	 ((yes) (record-optimization 'vector-length x) (nop))
	 ((no)  (interrupt))
	 (else  (cogen-effect-vector-length x))))
      (else
       (seq*
	(interrupt-unless (tag-test (T x) vector-mask vector-tag))
	(with-tmp ((t (cogen-value-$vector-length x)))
	  (interrupt-unless-fixnum t))))))
   ((P x)
    (seq* (cogen-effect-vector-length x) (K #t))))

 (define-primop vector-ref safe
   ((V x i)
    (seq* (vector-range-check x i)
	  (cogen-value-$vector-ref x i)))
   ((E x i)
    (vector-range-check x i)))

 (define-primop $vector-set! unsafe
   ((E x i v)
    ;; X -> reference to vector
    ;; I -> index as fixnum
    ;; V -> whatever Scheme value
    (struct-case i
      ((constant i)
       (if (not (fx? i))
	   (interrupt)
	 (mem-assign v (T x) (fx+ (fx* i wordsize) off-vector-data))))
      ((known i)
       (cogen-effect-$vector-set! x i v))
      (else
       ;;Notice  that I  is  not  multiplied by  the  WORDSIZE; this  is
       ;;because I  is a fixnum and  a fixnum representing the  index of
       ;;the I-th slot  in a vector, taken as a  "long", also represents
       ;;the offset in bytes of the word in the I-th slot.
       (mem-assign v
		   (prm 'int+ (T x) (T i))
		   off-vector-data)
       ))))

 (define-primop vector-set! safe
   ((E x i v)
    (seq*
     (vector-range-check x i)
     (cogen-effect-$vector-set! x i v))))

 (define-primop vector safe
   ((V . arg*)
    (with-tmp ((v (prm 'alloc
		       (K (align (+ disp-vector-data
				    (* (length arg*) wordsize))))
		       (K vector-tag))))
      (seq*
       (prm 'mset v (K off-vector-length)
	    (K (* (length arg*) wordsize)))
       (let f ((t* (map T arg*))
	       (i off-vector-data))
	 (cond
	  ((null? t*) v)
	  (else
	   (make-seq
	    (prm 'mset v (K i) (car t*))
	    (f (cdr t*) (+ i wordsize)))))))))
   ((E . arg*) (prm 'nop))
   ((P . arg*) (K #t)))

 /section)


(section ;;; closures

 (define-primop procedure? safe
   ;;Evaluate to true if X is a closure object.
   ;;
   ((P x)
    (tag-test (T x) closure-mask closure-tag)))

 (define-primop $cpref unsafe
   ;;Whenever  the body  of a  closure  references a  free variable  the
   ;;closure is closed upon...
   ;;
   ((V x i)
    (struct-case i
      ((constant i)
       (unless (fx? i)
	 (interrupt))
       (prm 'mref (T x) (K (+ off-closure-data (* i wordsize)))))
      ((known expr t)
       (cogen-value-$cpref x expr))
      (else
       (interrupt)))))

 /section)


;;;; symbols
;;
;;A symbol  is a fixed length  memory block referenced  by machine words
;;tagged as vectors.  The first machine word of a symbol block is tagged
;;has  symbol  in  its  least  significant  bits and  it  has  the  most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to symbol
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| symbol first word
;;     all set to zero         symbol tag
;;
;;A symbol memory  block is 6 words wide and  contains references to the
;;following fields: string, ustring, value, proc, plist.
;;
(section

 (define-primop symbol? safe
   ((P x)
    (sec-tag-test (T x) vector-mask symbol-primary-tag #f symbol-tag))
   ((E x)
    (nop)))

 (define-primop $make-symbol unsafe
   ((V str)
    ;;X is a machine word referencing a symbol's memory block.
    (with-tmp ((x (prm 'alloc
		       (K (align symbol-record-size))
		       (K symbol-primary-tag))))
      (prm 'mset x (K (fx- symbol-primary-tag))  (K symbol-tag))
      (prm 'mset x (K off-symbol-record-string)  (T str))
      (prm 'mset x (K off-symbol-record-ustring) (K 0))
      (prm 'mset x (K off-symbol-record-value)   (K unbound))
      (prm 'mset x (K off-symbol-record-proc)    (K unbound))
      (prm 'mset x (K off-symbol-record-plist)   (K nil))
      x))
   ((P str)
    (K #t))
   ((E str)
    (nop)))

 (define-primop $symbol-string unsafe
   ((V x)
    (prm 'mref (T x) (K off-symbol-record-string)))
   ((E x)
    (nop)))

 (define-primop $set-symbol-string! unsafe
   ((E x v)
    (mem-assign v (T x) off-symbol-record-string)))

 (define-primop $symbol-unique-string unsafe
   ((V x)
    (prm 'mref (T x) (K off-symbol-record-ustring)))
   ((E x)
    (nop)))

 (define-primop $set-symbol-unique-string! unsafe
   ((E x v)
    (mem-assign v (T x) off-symbol-record-ustring)))

 (define-primop $symbol-plist unsafe
   ((V x)
    (prm 'mref (T x) (K off-symbol-record-plist)))
   ((E x)
    (nop)))

 (define-primop $set-symbol-plist! unsafe
   ((E x v)
    (mem-assign v (T x) off-symbol-record-plist)))

 (define-primop $symbol-value unsafe
   ((V x)
    (prm 'mref (T x) (K off-symbol-record-value)))
   ((E x)
    (nop)))

 (define-primop $symbol-proc unsafe
   ((V x)
    (prm 'mref (T x) (K off-symbol-record-proc)))
   ((E x)
    (nop)))

 (define-primop $set-symbol-value! unsafe
   ((E x v)
    (with-tmp ((x^ (T x)))
      (prm 'mset x^ (K off-symbol-record-value) (T v))
      (dirty-vector-set x^))))

 (define-primop $set-symbol-proc! unsafe
   ((E x v)
    (with-tmp ((x^ (T x)))
      (prm 'mset x^ (K off-symbol-record-proc) (T v))
      (dirty-vector-set x^))))

 (define-primop $set-symbol-value/proc! unsafe
   ((E x v)
    (with-tmp ((x^ (T x))
	       (v^ (T v)))
      (prm 'mset x^ (K off-symbol-record-value) v^)
      (prm 'mset x^ (K off-symbol-record-proc)  v^)
      (dirty-vector-set x^))))

 (define-primop top-level-value safe
   ((V x)
    (struct-case x
      ((constant s)
       (if (symbol? s)
	   (with-tmp ((v (cogen-value-$symbol-value x)))
	     (interrupt-when (cogen-pred-$unbound-object? v))
	     v)
	 (interrupt)))
      ((known expr)
       (cogen-value-top-level-value expr))
      (else
       (with-tmp ((x^ (T x)))
	 (interrupt-unless (cogen-pred-symbol? x^))
	 (with-tmp ((v (cogen-value-$symbol-value x^)))
	   (interrupt-when (cogen-pred-$unbound-object? v))
	   v)))))
   ((E x)
    ;;The difference between the V execution context and the E execution
    ;;context  is that:  here  we  do *not*  return  the  object in  the
    ;;symbol's field "value".
    ;;
    (struct-case x
      ((constant s)
       (if (symbol? s)
	   (with-tmp ((v (cogen-value-$symbol-value x)))
	     (interrupt-when (cogen-pred-$unbound-object? v)))
	 (interrupt)))
      ((known expr)
       (cogen-effect-top-level-value expr))
      (else
       (with-tmp ((x^ (T x)))
	 (interrupt-unless (cogen-pred-symbol? x^))
	 (with-tmp ((v (cogen-value-$symbol-value x^)))
	   (interrupt-when (cogen-pred-$unbound-object? v))))))))


 (define-primop $init-symbol-function! unsafe
   ((E x v)
    (with-tmp ((x^ (T x))
	       (v^ (T v)))
      (prm 'mset x^ (K off-symbol-record-proc) v^)
      (dirty-vector-set x^))))

 /section)


;;;; fixnums
;;
;;A fixnum  is a machine  word whose least  significant bits are  set to
;;zero.  R6RS states  that a fixnum must have at least  24 bits in which
;;to store  the number; on a  32-bit platform, 29 bits  are available to
;;store the number:
;;
;; (greatest-fixnum)       => +536870911
;; (expt 2 29)             => +536870912
;; (- (expt 2 29) 1)       => +536870911
;;
;; (least-fixnum)          => -536870912
;; (- (expt 2 29))         => -536870912
;;
(section

 (define-primop fixnum? safe
   ((P x) (tag-test (T x) fx-mask fx-tag))
   ((E x) (nop)))


 (define-primop fixnum-width safe
   ((V) (K (fxsll (- (* wordsize 8) fx-shift) fx-shift)))
   ((E) (nop))
   ((P) (K #t)))

 (define-primop least-fixnum safe
   ((V) (K (sll (- (expt 2 (- (- (* wordsize 8) fx-shift) 1)))
		fx-shift)))
   ((E) (nop))
   ((P) (K #t)))

 (define-primop greatest-fixnum safe
   ((V) (K (sll (- (expt 2 (- (- (* wordsize 8) fx-shift) 1)) 1)
		fx-shift)))
   ((E) (nop))
   ((P) (K #t)))




 (define-primop $fxzero? unsafe
   ((P x) (prm '= (T x) (K 0)))
   ((E x) (nop)))

 (define-primop $fx= unsafe
   ((P x y) (prm '= (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $fx< unsafe
   ((P x y) (prm '< (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $fx<= unsafe
   ((P x y) (prm '<= (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $fx> unsafe
   ((P x y) (prm '> (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $fx>= unsafe
   ((P x y) (prm '>= (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $fxadd1 unsafe
   ((V x) (cogen-value-$fx+ x (K 1)))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-primop $fxsub1 unsafe
   ((V x) (cogen-value-$fx+ x (K -1)))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-primop $fx+ unsafe
   ((V x y) (prm 'int+ (T x) (T y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-primop $fx* unsafe
   ((V a b)
    (struct-case a
      ((constant a)
       (unless (fx? a) (interrupt))
       (prm 'int* (T b) (K a)))
      ((known a t) (cogen-value-$fx* a b))
      (else
       (struct-case b
	 ((constant b)
	  (unless (fx? b) (interrupt))
	  (prm 'int* (T a) (K b)))
	 ((known b t) (cogen-value-$fx* a b))
	 (else
	  (prm 'int* (T a) (prm 'sra (T b) (K fx-shift))))))))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-primop $fxlognot unsafe
   ((V x) (cogen-value-$fxlogxor x (K -1)))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-primop $fxlogand unsafe
   ((V x y) (prm 'logand (T x) (T y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-primop $fxlogor unsafe
   ((V x y) (prm 'logor (T x) (T y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-primop $fxlogxor unsafe
   ((V x y) (prm 'logxor (T x) (T y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-primop $fx- unsafe
   ((V x y) (prm 'int- (T x) (T y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-primop $fxsll unsafe
   ((V x i)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (prm 'sll (T x) (K i)))
      ((known i t) (cogen-value-$fxsll x i))
      (else
       (prm 'sll (T x) (prm 'sra (T i) (K fx-shift))))))
   ((P x i) (K #t))
   ((E x i) (nop)))

 (define-primop $fxsra unsafe
   ((V x i)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (prm 'logand
	    (prm 'sra (T x)
		 (K (if (< i (* wordsize 8))
			i
		      (- (* wordsize 8) 1))))
	    (K (* -1 fx-scale))))
      ((known i t) (cogen-value-$fxsra x i))
      (else
       (with-tmp ((i (prm 'sra (T i) (K fx-shift))))
	 (with-tmp ((i (make-conditional
			   (prm '< i (K (* 8 wordsize)))
			 i
			 (K (- (* 8 wordsize) 1)))))
	   (prm 'logand
		(prm 'sra (T x) i)
		(K (* -1 fx-scale))))))))
   ((P x i) (K #t))
   ((E x i) (nop)))

 (define-primop $fxquotient unsafe
   ((V a b)
    (with-tmp ((b (T b))) ;;; FIXME: why is quotient called remainder?
      (prm 'sll (prm 'int-quotient (T a) b) (K fx-shift))))
   ((P a b) (K #t))
   ((E a b) (nop)))

;;;FIXME Is this actually finished used somewhere?
 (define-primop $int-quotient unsafe
   ((V a b)
    (prm 'sll (prm 'int-quotient (T a) (T b)) (K fx-shift))))

;;;FIXME Is this actually finished and used somewhere?
 (define-primop $int-remainder unsafe
   ((V a b) (prm 'int-remainder (T a))))

 ;;This  implementation is  wrong  as documented  in  issue 9:  incorrect
 ;;results for negative numbers.  It  is replaced with another version in
 ;;"ikarus.fixnums.ss".
 ;;
 ;; (define-primop $fxmodulo unsafe
 ;;   ((V a b)
 ;;    (with-tmp ((b (T b)))
 ;;      (with-tmp ((c (prm 'logand b
 ;;                       (prm 'sra (prm 'logxor b (T a))
 ;;                          (K (sub1 (* 8 wordsize)))))))
 ;;        (prm 'int+ c (prm 'int-remainder (T a) b)))))
 ;;   ((P a b) (K #t))
 ;;   ((E a b) (nop)))

;;;FIXME Is this actually used somewhere?
 (define-primop $fxinthash unsafe
   ((V key)
    (with-tmp ((k (T key)))
      (with-tmp ((k (prm 'int+ k (prm 'logxor (prm 'sll k (K 15)) (K -1)))))
	(with-tmp ((k (prm 'logxor k (prm 'sra k (K 10)))))
	  (with-tmp ((k (prm 'int+ k (prm 'sll k (K 3)))))
	    (with-tmp ((k (prm 'logxor k (prm 'sra k (K 6)))))
	      (with-tmp ((k (prm 'int+ k (prm 'logxor (prm 'sll k (K 11)) (K -1)))))
		(with-tmp ((k (prm 'logxor k (prm 'sra k (K 16)))))
		  (prm 'sll k (K fx-shift)))))))))))
;;;(define inthash
;;;    (lambda (key)
;;;      ;static int inthash(int key) { /* from Bob Jenkin's */
;;;      ;  /* http://burtleburtle.net/bob/hash/doobs.html */
;;;      ;  key += ~(key << 15);
;;;      ;  key ^=  (key >> 10);
;;;      ;  key +=  (key << 3);
;;;      ;  key ^=  (key >> 6);
;;;      ;  key += ~(key << 11);
;;;      ;  key ^=  (key >> 16);
;;;      ;  return key;
;;;      ;}
;;;      (let* ((key ($fx+ key ($fxlognot ($fxsll key 15))))
;;;             (key ($fxlogxor key ($fxsra key 10)))
;;;             (key ($fx+ key ($fxsll key 3)))
;;;             (key ($fxlogxor key ($fxsra key 6)))
;;;             (key ($fx+ key ($fxlognot ($fxsll key 11))))
;;;             (key ($fxlogxor key ($fxsra key 16))))
;;;        key)))

 /section)


;;;; bignums
;;
;;A bignum is a variable length memory block referenced by machine words
;;tagged as vectors.  The first machine word of a bignum block is tagged
;;has bignum in its least significant  bits; then comes a sign bit, zero
;;if positive; the remaining  most significant bits represent the number
;;of words in the memory block after the first one.
;;
;;  |------------------------|-------------| reference to bignum
;;        heap offset          vector tag
;;
;;                       sign bit
;;  |----------------------|-|-------------| bignum first word
;;     number of words         bignum tag
;;
(section

 (define-primop bignum? safe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag bignum-mask bignum-tag))
   ((E x) (nop)))

 (define-primop $bignum-positive? unsafe
   ((P x)
    (prm '= (prm 'logand
		 (prm 'mref (T x) (K (- vector-tag)))
		 (K bignum-sign-mask))
	 (K 0)))
   ((E x) (nop)))

 (define-primop $bignum-byte-ref unsafe
   ((V s i)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (prm 'sll
	    (prm 'logand
		 (prm 'mref (T s)
		      (K (+ i (- disp-bignum-data vector-tag))))
		 (K 255))
	    (K fx-shift)))
      ((known i t) (cogen-value-$bignum-byte-ref s i))
      (else
       (prm 'sll
	    (prm 'srl ;;; FIXME: bref
		 (prm 'mref (T s)
		      (prm 'int+
			   (prm 'sra (T i) (K fx-shift))
;;; ENDIANNESS DEPENDENCY
			   (K (- disp-bignum-data
				 (- wordsize 1)
				 vector-tag))))
		 (K (* (- wordsize 1) 8)))
	    (K fx-shift)))))
   ((P s i) (K #t))
   ((E s i) (nop)))

 (define-primop $bignum-size unsafe
   ((V x)
    (prm 'sll
	 (prm 'sra
	      (prm 'mref (T x) (K (- vector-tag)))
	      (K bignum-length-shift))
	 (K (* 2 fx-shift)))))

 /section)


;;;; flonums
;;
;;A flonum  is a fixed length  memory block referenced  by machine words
;;tagged as vectors.  The first machine word of a flonum block is tagged
;;has  flonum  in  its  least  significant  bits and  it  has  the  most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to flonum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| flonum first word
;;     all set to zero         flonum tag
;;
;;A  flonum memory  block is  16 bytes  wide on  both 32-bit  and 64-bit
;;platforms; to allow for the same binary layout on both platforms, on a
;;32-bit platform the actual number is stored in the last two words:
;;
;;    1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word     unused           data words
;;
;;on a 64-bit platform the actual number is stored in the second word:
;;
;;            1st word                 2nd word
;;  |-------------------------|-------------------------|
;;           tagged word               data word
;;
(section

 (define ($flop-aux op fl0 fl1)
   ;;Flonum operation between two operands.
   ;;
   (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
     ;;tag the first word of the result as flonum
     (prm 'mset x (K (- vector-tag)) (K flonum-tag))
     ;;load the first operand in a register for flonums
     (prm 'fl:load  (T fl0) (K (- disp-flonum-data vector-tag)))
     ;;perform the operation between the register and FL1
     (prm op        (T fl1) (K (- disp-flonum-data vector-tag)))
     ;;store the result from the register into memory referenced by X
     (prm 'fl:store x       (K (- disp-flonum-data vector-tag)))
     x))

 (define ($flop-aux* op fl fl*)
   ;;Flonum operation  between three or  more operands (but also  upon a
   ;;single operand).
   ;;
   (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
     ;;tag the first word of the result as flonum
     (prm 'mset x (K (- vector-tag)) (K flonum-tag))
     ;;load the first operand in a register for flonums
     (prm 'fl:load (T fl) (K (- disp-flonum-data vector-tag)))
     (let f ((fl* fl*))
       (cond ((null? fl*)
	      (prm 'nop))
	     (else
	      (make-seq
	       ;;perform the operation between the register and the next
	       ;;operand
	       (prm op (T (car fl*)) (K (- disp-flonum-data vector-tag)))
	       (f (cdr fl*))))))
     ;;store the result from the register into memory referenced by X
     (prm 'fl:store x (K (- disp-flonum-data vector-tag)))
     x))

 (define ($flcmp-aux op fl0 fl1)
   ;;Flonum comparison operation.
   ;;
   (make-seq
    ;; load the first operand in a register for flonums
    (prm 'fl:load (T fl0) (K (- disp-flonum-data vector-tag)))
    ;; perform the operation between the register and FL1
    (prm op       (T fl1) (K (- disp-flonum-data vector-tag)))))

 (define-primop flonum? safe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag #f flonum-tag))
   ((E x) (nop)))

 (define-primop $flonum-u8-ref unsafe
   ((V s i)
    (struct-case i
      ((constant i)
       ;;the data area is 8 bytes wide
       (unless (and (fx? i) (fx<= 0 i) (fx<= i 7))
	 (interrupt))
       (prm 'sll
	    (prm 'logand
		 (prm 'bref (T s)
		      (K (+ (- 7 i) (- disp-flonum-data vector-tag))))
		 (K 255))
	    (K fx-shift)))
      ((known expr t)
       (cogen-value-$flonum-u8-ref s expr))
      (else (interrupt))))
   ((P s i) (K #t))
   ((E s i) (nop)))

 (define-primop $make-flonum unsafe
   ((V)
    (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
      (prm 'mset x (K (- vector-tag)) (K flonum-tag))
      x))
   ((P str) (K #t))
   ((E str) (nop)))

 (define-primop $flonum-set! unsafe
   ((E x i v)
    (struct-case i
      ((constant i)
       ;;the data area is 8 bytes wide
       (unless (and (fx? i) (fx<= 0 i) (fx<= i 7))
	 (interrupt))
       ;;store the byte
       (prm 'bset
	    (T x)
	    (K (+ (- 7 i) (- disp-flonum-data vector-tag)))
	    (prm 'sra (T v) (K fx-shift))))
      ((known expr t)
       (cogen-effect-$flonum-set! x expr v))
      (else (interrupt)))))

 (define-primop $fixnum->flonum unsafe
   ((V fx)
    (case wordsize
      ((4)
       (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
	 ;;tag the first word of the result as flonum
	 (prm 'mset x (K (- vector-tag)) (K flonum-tag))
	 ;;perform the operation storing the result in a flonum register
	 (prm 'fl:from-int
	      (K 0) ; dummy
	      (prm 'sra (T fx) (K fx-shift)))
	 ;;store the result from the register into memory referenced by X
	 (prm 'fl:store x (K (- disp-flonum-data vector-tag)))
	 x))
      (else
       (with-tmp ((f (cogen-value-$make-flonum)))
	 (make-forcall "ikrt_fixnum_to_flonum" (list (T fx) f)))))))


 (define (check-flonums ls code)
   (if (null? ls)
       code
     (struct-case (car ls)
       ((constant v)
	(if (flonum? v)
	    (check-flonums (cdr ls) code)
	  (interrupt)))
       ((known x t)
	(case (T:flonum? t)
	  ((yes)
	   (record-optimization 'check-flonum x)
	   (check-flonums (cdr ls) code))
	  ((no)
	   (interrupt))
	  (else
	   (check-flonums (cons x (cdr ls)) code))))
       (else
	(check-flonums (cdr ls)
		       (with-tmp ((x (T (car ls))))
			 (interrupt-unless
			  (tag-test x vector-mask vector-tag))
			 (interrupt-unless
			  (prm '= (prm 'mref x (K (- vector-tag)))
			       (K flonum-tag)))
			 code))))))

;;;  (define (primary-tag-tests ls)
;;;    (cond
;;;      ((null? ls) (prm 'nop))
;;;      (else
;;;       (seq*
;;;         (interrupt-unless
;;;           (tag-test (car ls) vector-mask vector-tag))
;;;         (primary-tag-tests (cdr ls))))))
;;;  (define (secondary-tag-tests ls)
;;;    (define (or* a*)
;;;      (cond
;;;        ((null? (cdr a*)) (car a*))
;;;        (else (prm 'logor (car a*) (or* (cdr a*))))))
;;;    (interrupt-unless
;;;      (prm '= (or* (map (lambda (x)
;;;                          (prm 'mref x (K (- vector-tag))))
;;;                        ls))
;;;           (K flonum-tag))))
;;;  (let ((check
;;;         (let f ((ls ls) (ac '()))
;;;           (cond
;;;             ((null? ls) ac)
;;;             (else
;;;              (struct-case (car ls)
;;;                ((constant v)
;;;                 (if (flonum? v)
;;;                     (f (cdr ls) ac)
;;;                     #f))
;;;                (else (f (cdr ls) (cons (T (car ls)) ac)))))))))
;;;    (cond
;;;      ((not check) (interrupt))
;;;      ((null? check) code)
;;;      (else
;;;       (seq*
;;;         (primary-tag-tests check)
;;;         (secondary-tag-tests check)
;;;         code)))))

 (define-primop $fl+ unsafe
   ((V x y) ($flop-aux 'fl:add! x y)))
 (define-primop $fl- unsafe
   ((V x y) ($flop-aux 'fl:sub! x y)))
 (define-primop $fl* unsafe
   ((V x y) ($flop-aux 'fl:mul! x y)))
 (define-primop $fl/ unsafe
   ((V x y) ($flop-aux 'fl:div! x y)))

 (define-primop fl+ safe
   ((V) (K (make-object 0.0)))
   ((V x) (check-flonums (list x) (T x)))
   ((V x . x*) (check-flonums (cons x x*) ($flop-aux* 'fl:add! x x*)))
   ((P . x*) (check-flonums x* (K #t)))
   ((E . x*) (check-flonums x* (nop))))
 (define-primop fl* safe
   ((V) (K (make-object 1.0)))
   ((V x) (check-flonums (list x) (T x)))
   ((V x . x*) (check-flonums (cons x x*) ($flop-aux* 'fl:mul! x x*)))
   ((P . x*) (check-flonums x* (K #t)))
   ((E . x*) (check-flonums x* (nop))))

;;;FIXME The following implementation of FL- was used by the compiler to
;;;override   the  implemtation   in  "ikarus.numerics.ss"   for  speed.
;;;Unfortunately it does not handle correctly the case:
;;;
;;;  (FL- 0.0) => -0.0
;;;
;;;returning "+0.0" because:
;;;
;;;  (FL- 0.0 0.0) => 0.0
;;;
;;;for this  reason I  commented it out.   It there  a way to  include a
;;;conditional to fix this implementation? (Marco Maggi; Aug 27, 2011)
;;;
;;; (define-primop fl- safe
;;;   ((V x) (check-flonums (list x) ($flop-aux 'fl:sub! (K 0.0) x)))
;;;   ((V x . x*) (check-flonums (cons x x*) ($flop-aux* 'fl:sub! x x*)))
;;;   ((P x . x*) (check-flonums (cons x x*) (K #t)))
;;;   ((E x . x*) (check-flonums (cons x x*) (nop))))

 (define-primop fl/ safe
   ((V x) (check-flonums (list x) ($flop-aux 'fl:div! (K 1.0) x)))
   ((V x . x*) (check-flonums (cons x x*) ($flop-aux* 'fl:div! x x*)))
   ((P x . x*) (check-flonums (cons x x*) (K #t)))
   ((E x . x*) (check-flonums (cons x x*) (nop))))

 (define-primop $fl= unsafe
   ((P x y) ($flcmp-aux 'fl:= x y)))
 (define-primop $fl< unsafe
   ((P x y) ($flcmp-aux 'fl:< x y)))
 (define-primop $fl<= unsafe
   ((P x y) ($flcmp-aux 'fl:<= x y)))
 (define-primop $fl> unsafe
   ((P x y) ($flcmp-aux 'fl:> x y)))
 (define-primop $fl>= unsafe
   ((P x y) ($flcmp-aux 'fl:>= x y)))

 (define-primop fl=? safe
   ((P x y) (check-flonums (list x y) ($flcmp-aux 'fl:= x y)))
   ((E x y) (check-flonums (list x y) (nop))))
 (define-primop fl<? safe
   ((P x y) (check-flonums (list x y) ($flcmp-aux 'fl:< x y)))
   ((E x y) (check-flonums (list x y) (nop))))
 (define-primop fl<=? safe
   ((P x y) (check-flonums (list x y) ($flcmp-aux 'fl:<= x y)))
   ((E x y) (check-flonums (list x y) (nop))))
 (define-primop fl>? safe
   ((P x y) (check-flonums (list x y) ($flcmp-aux 'fl:> x y)))
   ((E x y) (check-flonums (list x y) (nop))))
 (define-primop fl>=? safe
   ((P x y) (check-flonums (list x y) ($flcmp-aux 'fl:>= x y)))
   ((E x y) (check-flonums (list x y) (nop))))

 (define-primop $flonum-sbe unsafe
   ((V x)
    (prm 'sll
	 ;;extract the 12 most significant bits
	 (prm 'srl
	      ;;retrieve the second data word
	      (prm 'mref32 (T x)
		   (K (- (+ disp-flonum-data 4) vector-tag)))
	      (K 20))
	 (K fx-shift))))

 /section)


;;;; ratnums
;;
;;A ratnum  is a fixed length  memory block referenced  by machine words
;;tagged as vectors.  The first machine word of a ratnum block is tagged
;;has  ratnum  in  its  least  significant  bits and  it  has  the  most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to ratnum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| ratnum first word
;;     all set to zero         ratnum tag
;;
;;A ratnum memory block is 4 words wide; a reference to the numerator is
;;stored in the second word and a reference to the denominator is stored
;;in the third word
;;
;;     1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word   numerator   denominator     unused
;;
(section

 (define-primop ratnum? safe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag #f ratnum-tag))
   ((E x) (nop)))

 (define-primop $make-ratnum unsafe
   ((V num den)
    (with-tmp ((x (prm 'alloc (K (align ratnum-size)) (K vector-tag))))
      (prm 'mset x (K (- vector-tag)) (K ratnum-tag))
      (prm 'mset x (K (- disp-ratnum-num vector-tag)) (T num))
      (prm 'mset x (K (- disp-ratnum-den vector-tag)) (T den))
      x))
   ((P str) (K #t))
   ((E str) (nop)))


 (define-primop $ratnum-n unsafe
   ((V x) (prm 'mref (T x) (K (- disp-ratnum-num vector-tag)))))

 (define-primop $ratnum-d unsafe
   ((V x) (prm 'mref (T x) (K (- disp-ratnum-den vector-tag)))))

 /section)


;;;; compnum
;;
;;A compnum is  a fixed length memory block  referenced by machine words
;;tagged  as vectors.   The first  machine word  of a  compnum  block is
;;tagged has compnum  in its least significant bits and  it has the most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to compnum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| compnum first word
;;     all set to zero         compnum tag
;;
;;A compnum memory  block is 4 words wide; a reference  to the real part
;;is stored in the second word  and a reference to the imaginary part is
;;stored in the third word
;;
;;     1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word   real part    imag part     unused
;;
(section

 (define-primop compnum? safe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag #f compnum-tag))
   ((E x) (nop)))

 (define-primop $make-compnum unsafe
   ((V real imag)
    (with-tmp ((x (prm 'alloc (K (align compnum-size)) (K vector-tag))))
      (prm 'mset x (K (- vector-tag)) (K compnum-tag))
      (prm 'mset x (K (- disp-compnum-real vector-tag)) (T real))
      (prm 'mset x (K (- disp-compnum-imag vector-tag)) (T imag))
      x))
   ((P str) (K #t))
   ((E str) (nop)))


 (define-primop $compnum-real unsafe
   ((V x) (prm 'mref (T x) (K (- disp-compnum-real vector-tag)))))

 (define-primop $compnum-imag unsafe
   ((V x) (prm 'mref (T x) (K (- disp-compnum-imag vector-tag)))))

 /section)


;;;; cflonum
;;
;;A cflonum is  a fixed length memory block  referenced by machine words
;;tagged  as vectors.   The first  machine word  of a  cflonum  block is
;;tagged has cflonum  in its least significant bits and  it has the most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to cflonum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| cflonum first word
;;     all set to zero         cflonum tag
;;
;;A cflonum memory  block is 4 words wide; a reference  to the real part
;;is stored in the second word  and a reference to the imaginary part is
;;stored in the third word
;;
;;     1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word   real part    imag part     unused
;;
(section

 (define-primop cflonum? safe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag #f cflonum-tag))
   ((E x) (nop)))

 (define-primop $make-cflonum unsafe
   ((V real imag)
    (with-tmp ((x (prm 'alloc (K (align cflonum-size)) (K vector-tag))))
      (prm 'mset x (K (- vector-tag)) (K cflonum-tag))
      (prm 'mset x (K (- disp-cflonum-real vector-tag)) (T real))
      (prm 'mset x (K (- disp-cflonum-imag vector-tag)) (T imag))
      x))
   ((P str) (K #t))
   ((E str) (nop)))

 (define-primop $cflonum-real unsafe
   ((V x) (prm 'mref (T x) (K (- disp-cflonum-real vector-tag)))))

 (define-primop $cflonum-imag unsafe
   ((V x) (prm 'mref (T x) (K (- disp-cflonum-imag vector-tag)))))

 /section)


(section ;;; generic arithmetic

 (define (assert-fixnums a a*)
   (define (or* a a*)
     (cond
      ((null? a*) a)
      (else (or* (prm 'logor a (T (car a*))) (cdr a*)))))
   (define (known-fixnum? x)
     (struct-case x
       ((constant i) (fx? i))
       ((known x t)
	(case (T:fixnum? t)
	  ((yes) (record-optimization 'assert-fixnum x) #t)
	  (else  #f)))
       (else #f)))
   (define (known-non-fixnum? x)
     (struct-case x
       ((constant i) (not (fx? i)))
       ((known x t) (eq? (T:fixnum? t) 'no))
       (else #f)))
   (let-values (((fx* others) (partition known-fixnum? (cons a a*))))
     (let-values (((nfx* others) (partition known-non-fixnum?  others)))
       (cond
        ((not (null? nfx*)) (interrupt))
        ((null? others)     (nop))
        (else
         (interrupt-unless
	  (tag-test (or* (T (car others)) (cdr others)) fx-mask fx-tag)))))))

 (define (fixnum-fold-p op a a*)
   (seq*
    (assert-fixnums a a*)
    (let f ((a a) (a* a*))
      (cond
       ((null? a*) (K #t))
       (else
	(let ((b (car a*)))
	  (make-conditional
	      (prm op (T a) (T b))
	    (f b (cdr a*))
	    (K #f))))))))

 (define-primop = safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop < safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '< a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop <= safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '<= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop > safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '> a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop >= safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '>= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx= safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx< safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '< a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx<= safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '<= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx> safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '> a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx>= safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '>= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx=? safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx<? safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '< a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx<=? safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '<= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx>? safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '> a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx>=? safe
   ((P) (interrupt))
   ((P a . a*) (fixnum-fold-p '>= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop - safe
   ((V a)
    (interrupt)
    (seq*
     (assert-fixnums a '())
     (prm 'int-/overflow (K 0) (T a))))
   ((V a . a*)
    (interrupt)
    (seq*
     (assert-fixnums a a*)
     (let f ((a (T a)) (a* a*))
       (cond
	((null? a*) a)
	(else
	 (f (prm 'int-/overflow a (T (car a*))) (cdr a*)))))))
   ((P a . a*) (seq* (assert-fixnums a a*) (K #t)))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop + safe
   ((V) (K 0))
   ((V a . a*)
    (interrupt)
    (seq*
     (assert-fixnums a a*)
     (let f ((a (T a)) (a* a*))
       (cond
	((null? a*) a)
	(else
	 (f (prm 'int+/overflow a (T (car a*))) (cdr a*)))))))
   ((P) (K #t))
   ((P a . a*) (seq* (assert-fixnums a a*) (K #t)))
   ((E) (nop))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop add1 safe
   ((V x) (cogen-value-+ x (K 1))))
 (define-primop sub1 safe
   ((V x) (cogen-value-+ x (K -1))))

 (define-primop fxadd1 safe
   ((V x) (cogen-value-+ x (K 1))))
 (define-primop fxsub1 safe
   ((V x) (cogen-value-+ x (K -1))))


 (define (cogen-binary-* a b)
   (define (cogen-*-non-constants a b)
     (interrupt)
     (with-tmp ((a (T a)) (b (T b)))
       (assert-fixnum a)
       (assert-fixnum b)
       (prm 'int*/overflow a
	    (prm 'sra b (K fx-shift)))))
   (define (cogen-*-constant a b)
     (struct-case a
       ((constant ak)
	(if (fx? ak)
	    (begin
	      (interrupt)
	      (with-tmp ((b (T b)))
		(assert-fixnum b)
		(prm 'int*/overflow a b)))
	  (interrupt)))
       ((known x t) (cogen-*-constant x b))
       (else #f)))
   (or (cogen-*-constant a b)
       (cogen-*-constant b a)
       (cogen-*-non-constants a b)))


 (define-primop * safe
   ((V) (K (fxsll 1 fx-shift)))
   ((V a b) (cogen-binary-* a b))
   ((P) (K #t))
   ((P a . a*) (seq* (assert-fixnums a a*) (K #t)))
   ((E) (nop))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop bitwise-and safe
   ((V) (K (fxsll -1 fx-shift)))
   ((V a . a*)
    (interrupt)
    (seq*
     (assert-fixnums a a*)
     (let f ((a (T a)) (a* a*))
       (cond
	((null? a*) a)
	(else
	 (f (prm 'logand a (T (car a*))) (cdr a*)))))))
   ((P) (K #t))
   ((P a . a*) (seq* (assert-fixnums a a*) (K #t)))
   ((E) (nop))
   ((E a . a*) (assert-fixnums a a*)))

 (define-primop fx+ safe
   ((V x y) (cogen-value-+ x y)))

 (define-primop fx- safe
   ((V x)   (cogen-value-- (K 0) x))
   ((V x y) (cogen-value-- x y)))

 (define-primop fx* safe
   ((V a b) (cogen-binary-* a b)))

 (define-primop zero? safe
   ((P x)
    (seq*
     (assert-fixnum x)
     (cogen-pred-$fxzero? x)))
   ((E x) (assert-fixnum x)))


 (define-primop fxarithmetic-shift-left safe
   ((V x n)
    (struct-case n
      ((constant i)
       (cond
	((and (fx? i)
	      (>= i 0)
	      (< i (- (* wordsize 8) fx-shift)))
	 (with-tmp ((x (T x)))
	   (assert-fixnum x)
	   (cond
	    ((< i 6)
	     (let f ((i i))
	       (cond
		((zero? i) x)
		(else
		 (interrupt)
		 (prm 'sll/overflow (f (- i 1)) (K 1))))))
	    (else
	     (with-tmp ((x2 (prm 'sll x (K i))))
	       (interrupt-unless (prm '= (prm 'sra x2 (K i)) x))
	       x2)))))
	(else
	 (interrupt))))
      (else
       (with-tmp ((x (T x)) (n (T n)))
	 (assert-fixnums x (list n))
	 (with-tmp ((n (prm 'sra n (K fx-shift))))
	   (interrupt-when
	    (prm '< n (K 0)))
	   (interrupt-when
	    (prm '>= n (K (- (* wordsize 8) fx-shift))))
	   (with-tmp ((x2 (prm 'sll x n)))
	     (interrupt-unless (prm '= (prm 'sra x2 n) x))
	     x2)))))))



 (define-primop fxarithmetic-shift-right safe
   ((V x n)
    (struct-case n
;;; FIXME: check for known types
      ((constant i)
       (cond
	((and (fx? i)
	      (>= i 0)
	      (< i (- (* wordsize 8) fx-shift)))
	 (prm 'sll
	      (prm 'sra (T x) (K (+ i fx-shift)))
	      (K fx-shift)))
	(else
	 (interrupt))))
      (else
       (with-tmp ((x (T x)) (n (T n)))
	 (assert-fixnums x (list n))
	 (with-tmp ((n (prm 'sra n (K fx-shift))))
	   (interrupt-when
	    (prm '< n (K 0)))
	   (interrupt-when
	    (prm '>= n (K (- (* wordsize 8) fx-shift))))
	   (prm 'sll
		(prm 'sra (prm 'sra x n) (K fx-shift))
		(K fx-shift))))))))


 (define (log2 n)
   (let f ((n n) (i 0))
     (cond
      ((zero? (fxand n 1))
       (f (fxsra n 1) (+ i 1)))
      ((= n 1) i)
      (else #f))))


 (define-primop div safe
   ((V x n)
    (struct-case n
      ((constant i)
       (cond
	((and (fx? i) (> i 0) (log2 i)) =>
	 (lambda (bits)
	   (seq*
	    (interrupt-unless (cogen-pred-fixnum? x))
	    (prm 'sll
		 (prm 'sra (T x) (K (+ bits fx-shift)))
		 (K fx-shift)))))
	(else
	 (interrupt))))
      ((known expr t)
       (cogen-value-div x expr))
      (else (interrupt)))))

 (define-primop quotient safe
   ((V x n)
    (struct-case n
      ((constant i)
       (if (eqv? i 2)
	   (seq*
	    (interrupt-unless (cogen-pred-fixnum? x))
	    (make-conditional
		(prm '< (T x) (K 0))
	      (prm 'logand
		   (prm 'int+
			(prm 'sra (T x) (K 1))
			(K (fxsll 1 (sub1 fx-shift))))
		   (K (fxsll -1 fx-shift)))
	      (prm 'logand
		   (prm 'sra (T x) (K 1))
		   (K (fxsll -1 fx-shift)))))
	 (interrupt)))
      ((known expr t) (cogen-value-quotient x expr))
      (else (interrupt)))))

 /section)


;;;; structs
;;
;;A data  structure is a variable  length block of  memory referenced by
;;machine  words  tagged as  vectors;  the  first  machine word  of  the
;;structure is  a reference  to the type  descriptor, which is  itself a
;;data structure.  A block of memory is a data structure if and only if:
;;a reference to it is tagged as  vector and its first word is tagged as
;;vector.
;;
;; |----------------|----------| reference to structure
;;   heap offset     vector tag
;;
;; |----------------|----------| first word of structure
;;   heap offset     vector tag    = reference to rtd
;;                                 = reference to structure
;;
;;The type  descriptor of  the type descriptors  is the return  value of
;;BASE-RTD.
;;
(section

 (define-primop $struct? unsafe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag vector-mask vector-tag))
   ((E x) (nop)))

 (define-primop $struct/rtd? unsafe
   ((P x rtd)
    (make-conditional
	(tag-test (T x) vector-mask vector-tag)
      (prm '= (prm 'mref (T x) (K (- vector-tag))) (T rtd))
      (K #f)))
   ((E x rtd) (nop)))

 (define-primop $make-struct unsafe
   ((V rtd len)
    (struct-case len
      ((constant i)
       (unless (fx? i) (interrupt))
       (with-tmp ((t (prm 'alloc
			  (K (align (+ (* i wordsize) disp-struct-data)))
			  (K vector-tag))))
	 (prm 'mset t (K (- disp-struct-rtd vector-tag)) (T rtd))
	 t))
      ((known expr t)
       (cogen-value-$make-struct rtd expr))
      (else
       (with-tmp ((ln (align-code len disp-struct-data)))
	 (with-tmp ((t (prm 'alloc ln (K vector-tag))))
	   (prm 'mset t (K (- disp-struct-rtd vector-tag)) (T rtd))
	   t)))))
   ((P rtd len) (K #t))
   ((E rtd len) (nop)))

 (define-primop $struct-rtd unsafe
   ((V x)
    (prm 'mref (T x) (K (- disp-struct-rtd vector-tag))))
   ((E x) (nop))
   ((P x) #t))

 (define-primop $struct-ref unsafe
   ((V x i) (cogen-value-$vector-ref x i))
   ((E x i) (nop)))

 (define-primop $struct-set! unsafe
   ((V x i v)
    (seq* (cogen-effect-$vector-set! x i v)
	  (K void-object)))
   ((E x i v) (cogen-effect-$vector-set! x i v))
   ((P x i v)
    (seq* (cogen-effect-$vector-set! x i v)
	  (K #t))))

 (define-primop $struct unsafe
   ((V rtd . v*)
    (with-tmp ((t (prm 'alloc
		       (K (align
			   (+ disp-struct-data
			      (* (length v*) wordsize))))
		       (K vector-tag))))
      (prm 'mset t (K (- disp-struct-rtd vector-tag)) (T rtd))
      (let f ((v* v*)
	      (i (- disp-struct-data vector-tag)))
	(cond
	 ((null? v*) t)
	 (else
	  (make-seq
	   (prm 'mset t (K i) (T (car v*)))
	   (f (cdr v*) (+ i wordsize))))))))
   ((P rtd . v*) (K #t))
   ((E rtd . v*) (nop)))

 /section)


;;;; characters
;;
;;A character is a machine word  whose least significant bits are set to
;;the  character tag.   When stored  in a  string: the  machine  word is
;;trimmed to its least significant 32 bits.
;;
;;The most  significant bits, interpreted  as integer, represent  a code
;;point  in  the range  [0,  #x10FFFF] but  not  in  the range  [#xD800,
;;#xDFFF].
;;
(section

 (define-primop char? safe
   ((P x) (tag-test (T x) char-mask char-tag))
   ((E x) (nop)))

 (define-primop $char= unsafe
   ((P x y) (prm '= (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $char< unsafe
   ((P x y) (prm '< (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $char<= unsafe
   ((P x y) (prm '<= (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $char> unsafe
   ((P x y) (prm '> (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $char>= unsafe
   ((P x y) (prm '>= (T x) (T y)))
   ((E x y) (nop)))

 (define-primop $fixnum->char unsafe
   ((V x)
    (prm 'logor
	 (prm 'sll (T x) (K (- char-shift fx-shift)))
	 (K char-tag)))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-primop $char->fixnum unsafe
   ((V x) (prm 'sra (T x) (K (- char-shift fx-shift))))
   ((P x) (K #t))
   ((E x) (nop)))


 (define (assert-chars a a*)
   (define (or* a a*)
     (cond
      ((null? a*) a)
      (else (or* (prm 'logor a (T (car a*))) (cdr a*)))))
   (define (known-char? x)
     (struct-case x
       ((constant i) (char? i))
       ((known x t) (eq? (T:char? t) 'yes))
       (else #f)))
   (define (known-non-char? x)
     (struct-case x
       ((constant i) (not (char? i)))
       ((known x t) (eq? (T:char? t) 'no))
       (else #f)))
   (let-values (((fx* others) (partition known-char? (cons a a*))))
     (let-values (((nfx* others) (partition known-non-char?  others)))
       (cond
        ((not (null? nfx*)) (interrupt))
        ((null? others)     (nop))
        (else
         (interrupt-unless
	  (tag-test (or* (T (car others)) (cdr others)) char-mask char-tag)))))))

 (define (char-fold-p op a a*)
   (seq*
    (assert-chars a a*)
    (let f ((a a) (a* a*))
      (cond
       ((null? a*) (K #t))
       (else
	(let ((b (car a*)))
	  (make-conditional
	      (prm op (T a) (T b))
	    (f b (cdr a*))
	    (K #f))))))))


 (define-primop char=? safe
   ((P) (interrupt))
   ((P a . a*) (char-fold-p '= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-chars a a*)))

 (define-primop char<? safe
   ((P) (interrupt))
   ((P a . a*) (char-fold-p '< a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-chars a a*)))

 (define-primop char<=? safe
   ((P) (interrupt))
   ((P a . a*) (char-fold-p '<= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-chars a a*)))

 (define-primop char>? safe
   ((P) (interrupt))
   ((P a . a*) (char-fold-p '> a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-chars a a*)))

 (define-primop char>=? safe
   ((P) (interrupt))
   ((P a . a*) (char-fold-p '>= a a*))
   ((E) (interrupt))
   ((E a . a*) (assert-chars a a*)))

 /section)


;;;; bytevectors
;;
;;Bytevectors are blocks of memory referenced by machine words tagged as
;;bytevectors.   The  first  word  in  the  memory  block  is  a  fixnum
;;representing the  number of  bytes in the  data area; a  bytevector is
;;capable of  holding at  most a  number of values  equal to  the return
;;value of GREATEST-FIXNUM.
;;
;;When allocating  a bytevector capable  of holding N bytes,  the actual
;;size of  the allocated data area  is N+1; the additional  last byte is
;;not part of the data area and is perpetually set to zero.
;;
;;To  allow  for  the same  binary  layout  on  both 32-bit  and  64-bit
;;platforms,  the data area  starts 8  bytes after  the beginning;  on a
;;32-bit platform the layout is:
;;
;;    1st word   2nd word                       last byte
;;  |----------|----------|-------------------|-----------|
;;     length     unused        data area      set to zero
;;
;;on a 64-bit platform the layout is:
;;
;;         1st word                             last byte
;;  |---------------------|-------------------|-----------|
;;         length               data area      set to zero
;;
(section

 (define-primop bytevector? safe
   ((P x) (tag-test (T x) bytevector-mask bytevector-tag))
   ((E x) (nop)))

 (define-primop $make-bytevector unsafe
   ((V n)
    (struct-case n ;number of bytes
      ((constant n)
       (unless (fx? n) (interrupt))
       (with-tmp ((s (prm 'alloc
			  (K (align (+ n 1 disp-bytevector-data)))
			  (K bytevector-tag))))
	 ;; store the length in the first word
	 (prm 'mset s
	      (K (- disp-bytevector-length bytevector-tag))
	      (K (* n fx-scale)))
	 (prm 'bset s
	      (K (+ n (- disp-bytevector-data bytevector-tag)))
	      (K 0))
	 s))
      ((known expr t)
       (cogen-value-$make-bytevector expr))
      (else
       (with-tmp ((s (prm 'alloc
			  (align-code
			   (prm 'sra (T n) (K fx-shift))
			   (+ 1 disp-bytevector-data))
			  (K bytevector-tag))))
	 (prm 'mset s
	      (K (- disp-bytevector-length bytevector-tag))
	      (T n))
	 (prm 'bset s
	      (prm 'int+
		   (prm 'sra (T n) (K fx-shift))
		   (K (- disp-bytevector-data bytevector-tag)))
	      (K 0))
	 s))))
   ((P n) (K #t))
   ((E n) (nop)))

 (define-primop $bytevector-length unsafe
   ((V x) (prm 'mref (T x) (K (- disp-bytevector-length bytevector-tag))))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-primop $bytevector-u8-ref unsafe
   ((V s i)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (prm 'sll
	    (prm 'logand
		 (prm 'bref (T s)
		      (K (+ i (- disp-bytevector-data bytevector-tag))))
		 (K 255))
	    (K fx-shift)))
      (else
       (prm 'sll
	    (prm 'logand
		 (prm 'bref (T s)
		      (prm 'int+
			   (prm 'sra (T i) (K fx-shift))
			   (K (- disp-bytevector-data bytevector-tag))))
		 (K 255))
	    (K fx-shift)))))
   ((P s i) (K #t))
   ((E s i) (nop)))

 (define-primop $bytevector-s8-ref unsafe
   ((V s i)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (prm 'sra
	    (prm 'sll
		 (prm 'logand
		      (prm 'bref (T s)
			   (K (+ i (- disp-bytevector-data bytevector-tag))))
		      (K 255))
		 (K (- (* wordsize 8) 8)))
	    (K (- (* wordsize 8) (+ 8 fx-shift)))))
      (else
       (prm 'sra
	    (prm 'sll
		 (prm 'bref (T s)
		      (prm 'int+
			   (prm 'sra (T i) (K fx-shift))
			   (K (- disp-bytevector-data bytevector-tag))))
		 (K (- (* wordsize 8) 8)))
	    (K (- (* wordsize 8) (+ 8 fx-shift)))))))
   ((P s i) (K #t))
   ((E s i) (nop)))


 (define-primop $bytevector-set! unsafe
   ((E x i c)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (struct-case c
	 ((constant c)
	  (unless (fx? c) (interrupt))
	  (prm 'bset (T x)
	       (K (+ i (- disp-bytevector-data bytevector-tag)))
	       (K (cond
		   ((<= -128 c 127) c)
		   ((<= 128 c 255) (- c 256))
		   (else (interrupt))))))
	 (else
	  (prm 'bset (T x)
	       (K (+ i (- disp-bytevector-data bytevector-tag)))
	       (prm 'sra (T c) (K fx-shift))))))
      (else
       (struct-case c
	 ((constant c)
	  (unless (fx? c) (interrupt))
	  (prm 'bset (T x)
	       (prm 'int+
		    (prm 'sra (T i) (K fx-shift))
		    (K (- disp-bytevector-data bytevector-tag)))
	       (K (cond
		   ((<= -128 c 127) c)
		   ((<= 128 c 255) (- c 256))
		   (else (interrupt))))))
	 (else
	  (prm 'bset (T x)
	       (prm 'int+
		    (prm 'sra (T i) (K fx-shift))
		    (K (- disp-bytevector-data bytevector-tag)))
	       (prm 'sra (T c) (K fx-shift)))))))))

 (define-primop $bytevector-ieee-double-native-ref unsafe
   ((V bv i)
    (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
      (prm 'mset x (K (- vector-tag)) (K flonum-tag))
      (prm 'fl:load
	   (prm 'int+ (T bv) (prm 'sra (T i) (K fx-shift)))
	   (K (- disp-bytevector-data bytevector-tag)))
      (prm 'fl:store x (K (- disp-flonum-data vector-tag)))
      x)))


;;;The following uses unsupported SSE3 instructions.
;;;
;;;(define-primop $bytevector-ieee-double-nonnative-ref unsafe
;;;  ((V bv i)
;;;   (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
;;;     (prm 'mset x (K (- vector-tag)) (K flonum-tag))
;;;     (prm 'fl:load
;;;       (prm 'int+ (T bv) (prm 'sra (T i) (K fx-shift)))
;;;       (K (- disp-bytevector-data bytevector-tag)))
;;;     (prm 'fl:shuffle
;;;       (K (make-object '#vu8(7 6 2 3 4 5 1 0)))
;;;       (K (- disp-bytevector-data bytevector-tag)))
;;;     (prm 'fl:store x (K (- disp-flonum-data vector-tag)))
;;;     x)))

 (define-primop $bytevector-ieee-double-nonnative-ref unsafe
   ((V bv i)
    (case wordsize
      ((4)
       (let ((bvoff (- disp-bytevector-data bytevector-tag))
	     (floff (- disp-flonum-data vector-tag)))
	 (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
	   (prm 'mset x (K (- vector-tag)) (K flonum-tag))
	   (with-tmp ((t (prm 'int+ (T bv)
			      (prm 'sra (T i) (K fx-shift)))))
	     (with-tmp ((x0 (prm 'mref t (K bvoff))))
	       (prm 'bswap! x0 x0)
	       (prm 'mset x (K (+ floff wordsize)) x0))
	     (with-tmp ((x0 (prm 'mref t (K (+ bvoff wordsize)))))
	       (prm 'bswap! x0 x0)
	       (prm 'mset x (K floff) x0)))
	   x)))
      (else
       (let ((bvoff (- disp-bytevector-data bytevector-tag))
	     (floff (- disp-flonum-data vector-tag)))
	 (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
	   (prm 'mset x (K (- vector-tag)) (K flonum-tag))
	   (with-tmp ((t (prm 'int+ (T bv)
			      (prm 'sra (T i) (K fx-shift)))))
	     (with-tmp ((x0 (prm 'mref t (K bvoff))))
	       (prm 'bswap! x0 x0)
	       (prm 'mset x (K floff) x0)))
	   x))))))


 (define-primop $bytevector-ieee-double-native-set! unsafe
   ((E bv i x)
    (seq*
     (prm 'fl:load (T x) (K (- disp-flonum-data vector-tag)))
     (prm 'fl:store
	  (prm 'int+ (T bv) (prm 'sra (T i) (K fx-shift)))
	  (K (- disp-bytevector-data bytevector-tag))))))


 (define-primop $bytevector-ieee-single-native-ref unsafe
   ((V bv i)
    (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
      (prm 'mset x (K (- vector-tag)) (K flonum-tag))
      (prm 'fl:load-single
	   (prm 'int+ (T bv) (prm 'sra (T i) (K fx-shift)))
	   (K (- disp-bytevector-data bytevector-tag)))
      (prm 'fl:single->double)
      (prm 'fl:store x (K (- disp-flonum-data vector-tag)))
      x)))

 (define-primop $bytevector-ieee-single-native-set! unsafe
   ((E bv i x)
    (seq*
     (prm 'fl:load (T x) (K (- disp-flonum-data vector-tag)))
     (prm 'fl:double->single)
     (prm 'fl:store-single
	  (prm 'int+ (T bv) (prm 'sra (T i) (K fx-shift)))
	  (K (- disp-bytevector-data bytevector-tag))))))

 (define-primop $bytevector-ieee-single-nonnative-ref unsafe
   ((V bv i)
    (let ((bvoff (- disp-bytevector-data bytevector-tag))
	  (floff (- disp-flonum-data vector-tag)))
      (with-tmp ((x (prm 'alloc (K (align flonum-size)) (K vector-tag))))
	(prm 'mset x (K (- vector-tag)) (K flonum-tag))
	(with-tmp ((t (prm 'int+ (T bv) (prm 'sra (T i) (K fx-shift)))))
	  (with-tmp ((x0 (prm 'mref t (K bvoff))))
	    (prm 'bswap! x0 x0)
	    (prm 'mset x (K floff) x0)))
	(prm 'fl:load-single x (K (+ floff (- wordsize 4))))
	(prm 'fl:single->double)
	(prm 'fl:store x (K floff))
	x))))


;;;The following uses unsupported SSE3 instructions.
;;;
;;;(define-primop $bytevector-ieee-double-nonnative-set! unsafe
;;;  ((E bv i x)
;;;   (seq*
;;;     (prm 'fl:load (T x) (K (- disp-flonum-data vector-tag)))
;;;     (prm 'fl:shuffle
;;;       (K (make-object '#vu8(7 6 2 3 4 5 1 0)))
;;;       (K (- disp-bytevector-data bytevector-tag)))
;;;     (prm 'fl:store
;;;       (prm 'int+ (T bv) (prm 'sra (T i) (K fx-shift)))
;;;       (K (- disp-bytevector-data bytevector-tag))))))

 (define-primop $bytevector-ieee-double-nonnative-set! unsafe
   ((E bv i x)
    (case wordsize
      ((4)
       (let ((bvoff (- disp-bytevector-data bytevector-tag))
	     (floff (- disp-flonum-data vector-tag)))
	 (with-tmp ((t (prm 'int+ (T bv)
			    (prm 'sra (T i) (K fx-shift)))))
	   (with-tmp ((x0 (prm 'mref (T x) (K floff))))
	     (prm 'bswap! x0 x0)
	     (prm 'mset t (K (+ bvoff wordsize)) x0))
	   (with-tmp ((x0 (prm 'mref (T x) (K (+ floff wordsize)))))
	     (prm 'bswap! x0 x0)
	     (prm 'mset t (K bvoff) x0)))))
      (else
       (let ((bvoff (- disp-bytevector-data bytevector-tag))
	     (floff (- disp-flonum-data vector-tag)))
	 (with-tmp ((t (prm 'int+ (T bv)
			    (prm 'sra (T i) (K fx-shift)))))
	   (with-tmp ((x0 (prm 'mref (T x) (K floff))))
	     (prm 'bswap! x0 x0)
	     (prm 'mset t (K bvoff) x0))))))))

 (define-primop $bytevector-ieee-single-nonnative-set! unsafe
   ((E bv i x)
    (let ((bvoff (- disp-bytevector-data bytevector-tag))
	  (floff (- disp-flonum-data vector-tag)))
      (seq*
       (prm 'fl:load (T x) (K floff))
       (prm 'fl:double->single)
       (with-tmp ((t (prm 'int+ (T bv)
			  (prm 'sra (T i) (K fx-shift)))))
	 (prm 'fl:store-single t (K bvoff))
	 (case wordsize
	   ((4)
	    (with-tmp ((x0 (prm 'mref t (K bvoff))))
	      (prm 'bswap! x0 x0)
	      (prm 'mset t (K bvoff) x0)))
	   (else
	    (with-tmp ((x0 (prm 'mref32 t (K bvoff))))
	      (prm 'bswap! x0 x0)
	      (prm 'mset32 t (K bvoff) (prm 'sra x0 (K 32)))))))))))

 /section)


;;;; strings
;;
;;Strings are  blocks of  memory referenced by  machine words  tagged as
;;strings; the first  word in the memory block  is a fixnum representing
;;the number  of characters  in the  data area; a  string is  capable of
;;holding at  most a number of  characters equal to the  return value of
;;GREATEST-FIXNUM.
;;
;;  |------------------------|-------------| reference to string
;;        heap offset          string tag
;;
(section

 (define-primop string? safe
   ((P x) (tag-test (T x) string-mask string-tag))
   ((E x) (nop)))

 (define-primop $make-string unsafe
   ((V n)
    (struct-case n
      ((constant n)
       (unless (fx? n) (interrupt))
       (with-tmp ((s (prm 'alloc
			  ;;Characters  are  32-bit  unsigned  integers,
			  ;;*not* machine words.
			  (K (align (+ (* n 4) disp-string-data)))
			  (K string-tag))))
	 (prm 'mset s
	      (K off-string-length)
	      (K (* n fx-scale)))
	 s))
      ((known expr)
       (cogen-value-$make-string expr))
      (else
       (with-tmp ((s (prm 'alloc
			  (align-code (T n) disp-string-data)
			  (K string-tag))))
	 (prm 'mset s
	      (K off-string-length)
	      (T n))
	 s))))
   ((P n) (K #t))
   ((E n) (nop)))

 (define-primop $string-length unsafe
   ((V x) (prm 'mref (T x) (K off-string-length)))
   ((P x) (K #t))
   ((E x) (nop)))


 (define-primop $string-ref unsafe
   ((V s i)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (prm 'mref32 (T s)
	    (K (+ (* i char-size)
		  off-string-data))))
      (else
       (prm 'mref32 (T s)
	    (prm 'int+
		 (cond
		  ((= wordsize char-size) (T i))
		  ((= wordsize 8) (prm 'sra (T i) (K 1)))
		  (else (error '$string-ref "invalid operand")))
		 (K off-string-data))))))
   ((P s i) (K #t))
   ((E s i) (nop)))

;;;(Marco Maggi; Oct  31, 2011) Commented out because it  appears not to
;;;cause  correct code  generation  when  the index  argument  is not  a
;;;fixnum; specifically:
;;;
;;;   (guard (E ((assertion-violation? E)
;;;              #t)
;;;             (else E))
;;;     (string-ref "ciao" #\Z))
;;;
;;;causes a weird error while compiling:
;;;
;;;Exception trapped by debugger.
;;; Condition components:
;;;   1. &assertion
;;;   2. &who: *
;;;   3. &message: "not a number"
;;;   4. &irritants: (#\d)
;;;[t] Trace. [r] Reraise exception. [c] Continue. [q] Quit. [?] Help.
;;;vicare>
;;;
;;;(define-primop string-ref safe
;;;  ((V s i)
;;;   (seq*
;;;    (assert-fixnum i)
;;;    (assert-string s)
;;;    (interrupt-unless (prm 'u< (T i) (cogen-value-$string-length s)))
;;;    (cogen-value-$string-ref s i)))
;;;  ((P s i)
;;;   (seq*
;;;    (assert-fixnum i)
;;;    (assert-string s)
;;;    (interrupt-unless (prm 'u< (T i) (cogen-value-$string-length s)))
;;;    (K #t)))
;;;  ((E s i)
;;;   (seq*
;;;    (assert-fixnum i)
;;;    (assert-string s)
;;;    (interrupt-unless (prm 'u< (T i) (cogen-value-$string-length s))))))

 (define-primop $string-set! unsafe
   ((E x i c)
    (struct-case i
      ((constant i)
       (unless (fx? i) (interrupt))
       (prm 'mset32 (T x)
	    (K (+ (* i char-size) off-string-data))
	    (T c)))
      (else
       (prm 'mset32 (T x)
	    (prm 'int+
		 (case-word-size
		  ((32)
		   ;;It is  a fixnum representing a  character index and
		   ;;its raw value is also the offset in bytes.
		   (T i))
		  ((64)
		   ;;It is a fixnum  representing a character index and,
		   ;;after shifting one  bit, its raw value  is also the
		   ;;offset in bytes.
		   (prm 'sra (T i) (K 1))))
		 ;; (cond ((= wordsize char-size)
		 ;; 	(T i))
		 ;;       ((= wordsize 8)
		 ;; 	(prm 'sra (T i) (K 1)))
		 ;;       (else
		 ;; 	(error '$string-set! "invalid operand")))
		 (K off-string-data))
	    (T c))))))

 /section)


;;;; ports
;;
;;A port value  is a block of memory allocated as  a vector; a reference
;;to a  port value is a  reference to a  vector.  The first word  of the
;;vector is  the bitwise OR between a  port tag and a  bitvector of port
;;attributes.
;;
;;  |----------------|----------| port value
;;    heap offset     vector tag
;;
;;  |----------------|----------| 1st word of port's memory block
;;   port attributes   port tag
;;
;;At present the port tag is 6 bits wide, so we have:
;;
;;- on 32 bits platforms: 32 - 6 = 24
;;- on 64 bits platforms: 64 - 6 = 58
;;
;;bits available for attributes.
;;
(section
 (define-primop port? safe
   ((P x) (sec-tag-test (T x) vector-mask vector-tag port-mask port-tag))
   ((E x) (nop)))

 ;;FIXME  The  following two  are  commented  out  because they  require
 ;;defining  INPUT-PORT-TAG and  OUTPUT-PORT-TAG as  bitwise  OR between
 ;;PORT-TAG and the  bits used in the attributes  bitvector to tag ports
 ;;as input and output (see  the file "ikarus.io.ss"); this is possible,
 ;;but requires both the tags allocation  in all the source code and the
 ;;bits allocation in the attributes bitvector to be "definitive" (guess
 ;;by Marco Maggi; Sep 23, 2011).
 ;;
 ;;(define-primop input-port? safe
 ;;  ((P x) (sec-tag-test (T x) vector-mask vector-tag #f input-port-tag))
 ;;  ((E x) (nop)))
 ;;
 ;;(define-primop output-port? safe
 ;;  ((P x) (sec-tag-test (T x) vector-mask vector-tag #f output-port-tag))
 ;;  ((E x) (nop)))

 (define port-attrs-shift 6)

 (define-primop $make-port unsafe
   ((V attrs idx sz buf tr id read write getp setp cl cookie)
    (with-tmp ((p (prm 'alloc (K (align port-size)) (K vector-tag))))
      (prm 'mset p (K (- vector-tag))
	   (prm 'logor (prm 'sll (T attrs) (K port-attrs-shift)) (K port-tag)))
      (prm 'mset p (K (- disp-port-index	vector-tag)) (T idx))
      (prm 'mset p (K (- disp-port-size		vector-tag)) (T sz))
      (prm 'mset p (K (- disp-port-buffer	vector-tag)) (T buf))
      (prm 'mset p (K (- disp-port-transcoder	vector-tag)) (T tr))
      (prm 'mset p (K (- disp-port-id		vector-tag)) (T id))
      (prm 'mset p (K (- disp-port-read!	vector-tag)) (T read))
      (prm 'mset p (K (- disp-port-write!	vector-tag)) (T write))
      (prm 'mset p (K (- disp-port-get-position	vector-tag)) (T getp))
      (prm 'mset p (K (- disp-port-set-position! vector-tag)) (T setp))
      (prm 'mset p (K (- disp-port-close	vector-tag)) (T cl))
      (prm 'mset p (K (- disp-port-cookie	vector-tag)) (T cookie))
      (prm 'mset p (K (- disp-port-unused1	vector-tag)) (K 0))
      (prm 'mset p (K (- disp-port-unused2	vector-tag)) (K 0))
      p)))

 (define-primop $port-index unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-index	vector-tag)))))
 (define-primop $port-size unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-size		vector-tag)))))
 (define-primop $port-buffer unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-buffer	vector-tag)))))
 (define-primop $port-transcoder unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-transcoder	vector-tag)))))
 (define-primop $port-id unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-id		vector-tag)))))
 (define-primop $port-read! unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-read!	vector-tag)))))
 (define-primop $port-write! unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-write!	vector-tag)))))
 (define-primop $port-get-position unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-get-position	vector-tag)))))
 (define-primop $port-set-position! unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-set-position! vector-tag)))))
 (define-primop $port-close unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-close	vector-tag)))))
 (define-primop $port-cookie unsafe
   ((V x) (prm 'mref (T x) (K (- disp-port-cookie	vector-tag)))))
 (define-primop $port-attrs unsafe
   ;;Given  a  port value  X:  return  a  fixnum representing  the  port
   ;;attributes.   To  be  used  when  the  argument  has  already  been
   ;;validated as port value.
   ((V x)
    (prm 'sra
	 (prm 'mref (T x) (K (- disp-port-attrs vector-tag)))
	 (K port-attrs-shift))))

 (define-primop $port-tag unsafe
   ;;Extract  from  a port  reference  a  fixnum  representing the  port
   ;;attributes.  If  the argument  is not a  port reference  the return
   ;;value is zero.
   ((V x)
    (make-conditional
	(tag-test (T x) vector-mask vector-tag)
      (with-tmp ((tag (prm 'mref (T x) (K (- disp-port-attrs vector-tag)))))
	(make-conditional
	    (tag-test tag port-mask port-tag)
	  (prm 'sra tag (K port-attrs-shift))
	  (K 0)))
      (K 0))))

 (define-primop $set-port-index! unsafe
   ((E x i) (prm 'mset (T x) (K (- disp-port-index vector-tag)) (T i))))
 (define-primop $set-port-size! unsafe
   ((E x i) (prm 'mset (T x) (K (- disp-port-size vector-tag)) (T i))))
 (define-primop $set-port-attrs! unsafe
   ((E x i)
    (prm 'mset (T x)
	 (K (- disp-port-attrs vector-tag))
	 (prm 'logor (prm 'sll (T i) (K port-attrs-shift)) (K port-tag)))))

 /section)


;;;; pointers
;;
;;A pointer is  a fixed length memory block,  two words wide, referenced
;;by  machine words  tagged as  vectors.  The  first machine  word  of a
;;pointer block is tagged has  pointer in its least significant bits and
;;it has the most significant bits set to zero.  The second machine word
;;of a pointer block holds the actual pointer value.
;;
;;  |------------------------|-------------| reference to pointer
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| pointer first word
;;     all set to zero         pointer tag
;;
;;  |--------------------------------------| pointer second word
;;              pointer value
;;
(section

 (define-primop $pointer? safe
   ((P x)
    (sec-tag-test (T x) vector-mask vector-tag #f pointer-tag))
   ((E x) (nop)))

 (define-primop $pointer= unsafe
   ((V x y)
    ;;This is  a predicate but a  forcall is currently  not supported by
    ;;the P function (Marco Maggi; Nov 30, 2011).
    (with-tmp ((arg1 (T x)))
      (with-tmp ((arg2 (T y)))
	(make-forcall "ikrt_pointer_eq" (list arg1 arg2)))))
   ((E x y)
    (nop)))

 /section)


(section ;;; interrupts-and-engines

 (define-primop $interrupted? unsafe
   ;;Evaluate  to true  if the  field  "interrupted" of  the C  language
   ;;struct PCB is not zero.
   ;;
   ((P)
    (prm '!= (prm 'mref pcr (K pcb-interrupted)) (K 0))))

 (define-primop $unset-interrupted! unsafe
   ;;Set to zero the field "interrupted" of the C language struct PCB.
   ;;
   ((E)
    (prm 'mset pcr (K pcb-interrupted) (K 0))))

 (define-primop $do-event safe
   ;;Set to 1 the field "engine_counter" of the C language struct PCB.
   ;;
   ((E)
    (begin
      (interrupt)
      (prm 'incr/zero? pcr (K pcb-engine-counter)
	   (K (fxsll 1 fx-shift))))))

 (define-primop $swap-engine-counter! unsafe
   ;;Set to X  the field "engine_counter" of the C  language struct PCB;
   ;;return the previous field value.
   ;;
   ((V x)
    ;;FIXME: should be atomic swap  instead of load and set!  (Abdulaziz
    ;;Ghuloum)
    ;;
    (with-tmp ((x0 (T x)))
      (with-tmp ((t (prm 'mref pcr (K pcb-engine-counter))))
	(prm 'mset pcr (K pcb-engine-counter) x0)
	t))))

 (define-primop $stack-overflow-check unsafe
   ;;If the  field "frame_redline"  of the C  language structure  PCB is
   ;;less than stack base pointer: ???
   ;;
   ((E)
    (make-shortcut
     (make-conditional
	 ;; ESP = stack base pointer
	 ;; PCR = pointer to the PCB structure
     	 (prm 'u< esp (prm 'mref pcr (K pcb-frame-redline)))
       (prm 'interrupt)
       (prm 'nop))
     (make-forcall "ik_stack_overflow" '()))))

 /section)


(section ;;; control operations

 (define-primop $fp-at-base unsafe
   ((P)
    (prm '= (prm 'int+
		 (prm 'mref pcr (K pcb-frame-base))
		 (K (- wordsize)))
	 fpr)))

 (define-primop $current-frame unsafe
   ((V) (prm 'mref pcr (K pcb-next-continuation))))


 (define-primop $seal-frame-and-call unsafe
   ((V x) ;;; PCB NEXT CONT;;; PCB BASE
    (with-tmp ((k (prm 'alloc (K continuation-size) (K vector-tag))))
      (with-tmp ((base (prm 'int+
			    (prm 'mref pcr (K pcb-frame-base))
			    (K (- wordsize)))))
	(with-tmp ((underflow-handler (prm 'mref base (K 0))))
	  (prm 'mset k (K (- vector-tag)) (K continuation-tag))
	  (prm 'mset k (K (- disp-continuation-top vector-tag)) fpr)
	  (prm 'mset k (K (- disp-continuation-next vector-tag))
	       (prm 'mref pcr (K pcb-next-continuation)))
	  (prm 'mset k (K (- disp-continuation-size vector-tag)) (prm 'int- base fpr))
	  (prm 'mset pcr (K pcb-next-continuation) k)
	  (prm 'mset pcr (K pcb-frame-base) fpr)
	  (prm '$call-with-underflow-handler underflow-handler (T x) k)))))
   ((E . args) (interrupt))
   ((P . args) (interrupt)))

 (define-primop $frame->continuation unsafe
   ((V x)
    (with-tmp ((t (prm 'alloc
		       (K (align (+ disp-closure-data wordsize)))
		       (K closure-tag))))
      (prm 'mset t (K off-closure-code)
	   (K (make-code-loc (sl-continuation-code-label))))
      (prm 'mset t (K off-closure-data)
	   (T x))
      t))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-primop $make-call-with-values-procedure unsafe
   ((V) (K (make-closure
            (make-code-loc (sl-cwv-label))
            '() #f)))
   ((P) (interrupt))
   ((E) (interrupt)))

 (define-primop $make-values-procedure unsafe
   ((V)
    (K (make-closure (make-code-loc (sl-values-label))
		     '() #f)))
   ((P)
    (interrupt))
   ((E)
    (interrupt)))

 (define-primop $make-annotated-procedure unsafe
   ((V annotation proc)
    (with-tmp ((t (prm 'alloc
		       (K (align (+ disp-closure-data (* 2 wordsize))))
		       (K closure-tag))))
      (prm 'mset t (K off-closure-code)
	   (K (make-code-loc (sl-annotated-procedure-label))))
      (prm 'mset t (K off-closure-data)
	   (T annotation))
      (prm 'mset t (K (+ off-closure-data wordsize))
	   (T proc))
      t))
   ((P) (interrupt))
   ((E) (interrupt)))

 (define-primop $annotated-procedure-annotation unsafe
   ((V proc)
    (prm 'mref (T proc)
	 (K off-closure-data))))


 /section)


(section ;;; hash table tcbuckets

 (define-primop $make-tcbucket unsafe
   ((V tconc key val next)
    (with-tmp ((x (prm 'alloc (K (align tcbucket-size)) (K vector-tag))))
      (prm 'mset x (K (- disp-tcbucket-tconc vector-tag)) (T tconc))
      (prm 'mset x (K (- disp-tcbucket-key vector-tag)) (T key))
      (prm 'mset x (K (- disp-tcbucket-val vector-tag)) (T val))
      (prm 'mset x (K (- disp-tcbucket-next vector-tag)) (T next))
      x)))

 (define-primop $tcbucket-key unsafe
   ((V x) (prm 'mref (T x) (K (- disp-tcbucket-key vector-tag)))))
 (define-primop $tcbucket-val unsafe
   ((V x) (prm 'mref (T x) (K (- disp-tcbucket-val vector-tag)))))
 (define-primop $tcbucket-next unsafe
   ((V x) (prm 'mref (T x) (K (- disp-tcbucket-next vector-tag)))))

 (define-primop $set-tcbucket-key! unsafe
   ((E x v) (mem-assign v (T x) (- disp-tcbucket-key vector-tag))))
 (define-primop $set-tcbucket-val! unsafe
   ((E x v) (mem-assign v (T x) (- disp-tcbucket-val vector-tag))))
 (define-primop $set-tcbucket-next! unsafe
   ((E x v) (mem-assign v (T x) (- disp-tcbucket-next vector-tag))))
 (define-primop $set-tcbucket-tconc! unsafe
   ((E x v) (mem-assign v (T x) (- disp-tcbucket-tconc vector-tag))))


 /section)


(section ;;; code objects

 ;;An object is a code object if the reference to it is tagged as vector
 ;;and if the first word is tagged as code.
 (define-primop code? unsafe
   ((P x)
    (sec-tag-test (T x) vector-mask vector-tag #f code-tag)))

 (define-primop $closure-code unsafe
   ;;First  extract  from the  closure  object  the raw  memory  pointer
   ;;referencing the first  byte of binary code in a  code object's data
   ;;area:
   ;;
   ;;  memory pointer = (prm 'mref (T closure) (K off-closure-code))
   ;;
   ;;we know that such memory  pointer is DISP-CODE-DATA bytes after the
   ;;pointer  to the  first  byte of  the code  object,  so we  subtract
   ;;DISP-CODE-DATA  and add  the vector  tag:  the result  is a  tagged
   ;;reference to the code object.
   ;;
   ;;    |----------| closure object
   ;;      p_memory
   ;;
   ;;      meta data        binary code
   ;;    |-----------|-----------------------| code object
   ;;    ^           ^
   ;; s_code      p_memory
   ;;
   ;;    |...........| disp-code-data
   ;;
   ((V x)
    (prm 'int+
	 (prm 'mref (T x) (K off-closure-code))
	 (K (- vector-tag disp-code-data)))))

 (define-primop $code-freevars unsafe
   ((V x)
    (prm 'mref (T x) (K (- disp-code-freevars vector-tag)))))

 (define-primop $code-reloc-vector unsafe
   ((V x)
    (prm 'mref (T x) (K (- disp-code-relocsize vector-tag)))))

 (define-primop $code-size unsafe
   ((V x)
    (prm 'mref (T x) (K (- disp-code-instrsize vector-tag)))))

 (define-primop $code-annotation unsafe
   ((V x)
    (prm 'mref (T x) (K (- disp-code-annotation vector-tag)))))

 (define-primop $code->closure unsafe
   ((V x)
    (with-tmp
	;;Allocate a closure's memory block  and tag the reference to it
	;;as closure.
	((v (prm 'alloc
		       (K (align (+ 0 disp-closure-data)))
		       (K closure-tag))))
      ;;Store in the  closure's memory block a raw pointer  to the first
      ;;byte of the code object's data area.
      (prm 'mset v
	   (K off-closure-code)
	   (prm 'int+ (T x)
		(K (- disp-code-data vector-tag))))
      v)))

 (define-primop $code-ref unsafe
   ((V x i)
    (prm 'sll
	 (prm 'logand
	      (prm 'bref (T x)
		   (prm 'int+
			(prm 'sra (T i) (K fx-shift))
			(K (- disp-code-data vector-tag))))
	      (K 255))
	 (K fx-shift))))

 (define-primop $code-set! unsafe
   ((E x i v)
    (prm 'bset (T x)
	 (prm 'int+
	      (prm 'sra (T i) (K fx-shift))
	      (K (- disp-code-data vector-tag)))
	 (prm 'sra (T v) (K fx-shift)))))

 (define-primop $set-code-annotation! unsafe
   ((E x v)
    (mem-assign v (T x) (- disp-code-annotation vector-tag))))

 /section)


;;;; port transcoders
;;
;;A transcoder  is a  word tagged to  make it  of a disjoint  type.  The
;;transcoder data (codec,  EOL style, error handling) is  encoded in the
;;most significant bits of this word.
;;
;;  |---------------------------|------------| transcoder
;;         payload bits          transcoder-tag
;;
(section

 (define-primop transcoder? unsafe
   ((P x) (tag-test (T x) transcoder-mask transcoder-tag)))

 (define-primop $data->transcoder unsafe
   ;;Given a fixnum X: encode it as payload bits of a transcoder word.
   ;;
   ((V x) (prm 'logor
	       (prm 'sll (T x) (K (- transcoder-payload-shift fx-shift)))
	       (K transcoder-tag))))

 (define-primop $transcoder->data unsafe
   ;;Given a transcoder word X: extract the payload bits and return them
   ;;as fixnum.
   ;;
   ((V x) (prm 'sra (T x) (K (- transcoder-payload-shift fx-shift)))))

 /section)

;;; end of file
;;;Local Variables:
;;;eval: (put 'make-conditional	'scheme-indent-function 1)
;;;eval: (put 'with-tmp		'scheme-indent-function 1)
;;;eval: (put 'struct-case	'scheme-indent-function 1)
;;;End:
