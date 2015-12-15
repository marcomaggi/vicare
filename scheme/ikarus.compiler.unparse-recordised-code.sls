;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.unparse-recordised-code)
  (export
    unparse-recordized-code
    unparse-recordized-code/sexp
    unparse-recordized-code/pretty
    unparse-recordised-code
    unparse-recordised-code/sexp
    unparse-recordised-code/pretty)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.scheme-objects-ontology))


;;;; helpers

(case-define %map-in-order
  ;;We need this  to make sure that  the names generated in  the symbolic expressions
  ;;are predictable.
  ;;
  ((func ell)
   (if (pair? ell)
       (let ((A (func (car ell))))
	 (cons A (%map-in-order func (cdr ell))))
     '()))

  ((func ell1 ell2)
   (if (pair? ell1)
       (let ((A (func (car ell1) (car ell2))))
	 (cons A (%map-in-order func (cdr ell1) (cdr ell2))))
     '())))


(define (unparse-recordised-code x)
  ;;Unparse  the  struct instance  X  (representing  recordized  code in  the  core
  ;;language  already processed  by the  compiler) into  a human  readable symbolic
  ;;expression to be used when raising errors.
  ;;
  ;;Being that this function is used only  when signaling errors: it makes no sense
  ;;to use unsafe operations: let's keep it safe!!!
  ;;
  (import SCHEME-OBJECTS-ONTOLOGY)
  (define E unparse-recordised-code)
  (define E-nfv (make-E-nfv E))
  (struct-case x
    ((constant)
     (E-constant 'constant x E))

    ((known expr type)
     `(known ,(E expr) ,(core-type-tag-description type)))

    ((code-loc x)
     `(code-loc ,x))

    ((var x)
     (string->symbol (format ":~a" x)))

    ((prelex name)
     (string->symbol (format ":~a" name)))

    ((primref x)
     x)

    ((conditional test conseq altern)
     (E-conditional 'conditional test conseq altern E))

    ((primopcall op arg*)
     `(,op . ,(%map-in-order E arg*)))

    ((asmcall op arg*)
     `(asmcall ,op . ,(%map-in-order E arg*)))

    ((bind lhs* rhs* body)
     `(let ,(%map-in-order (lambda (lhs rhs)
			     (list (E lhs) (E rhs)))
			   lhs* rhs*)
	,(E body)))

    ((recbind lhs* rhs* body)
     `(letrec ,(%map-in-order (lambda (lhs rhs)
				(list (E lhs) (E rhs)))
			      lhs* rhs*)
	,(E body)))

    ((rec*bind lhs* rhs* body)
     `(letrec* ,(%map-in-order (lambda (lhs rhs)
				 (list (E lhs) (E rhs)))
			       lhs* rhs*)
	,(E body)))

    ((fix lhs* rhs* body)
     `(fix ,(%map-in-order (lambda (lhs rhs)
			     (list (E lhs) (E rhs)))
			   lhs* rhs*)
	,(E body)))

    ((seq e0 e1)
     (letrec ((recur (lambda (x ac)
		       (struct-case x
			 ((seq e0 e1)
			  (recur e0 (recur e1 ac)))
			 (else
			  (cons (E x) ac))))))
       (cons 'seq (recur e0 (recur e1 '())))))

    ((clambda-case info body)
     `(,(if (case-info-proper info)
	    (%map-in-order E (case-info-args info))
	  ;;The loop  below is like MAP  but for improper  lists: it maps E  over the
	  ;;improper list X.
	  (let ((X (case-info-args info)))
	    (let recur ((A (car X))
			(D (cdr X)))
	      (if (pair? D)
		  (cons (E A) (recur (car D) (cdr D)))
		(E A)))))
       ,(E body)))

    ((clambda label cls* cp freevar*)
     ;;FIXME Should we print more fields?  (Marco Maggi; Oct 11, 2012)
     `(clambda (label: ,(%pretty-symbol label))
	       (cp:    ,(E cp))
	       (free:  ,(and freevar* (%map-in-order E freevar*)))
	       ,@(%map-in-order E cls*)))

    ((closure-maker code freevar*)
     `(closure (freevars: ,(%map-in-order E freevar*))
	       ,(E code)))

    ((codes list body)
     `(codes ,(%map-in-order E list)
	     ,(E body)))

    ((funcall rator rand*)
     `(funcall ,(E rator) . ,(%map-in-order E rand*)))

    ((jmpcall label rator rand*)
     `(jmpcall ,(%pretty-symbol label) ,(E rator) . ,(%map-in-order E rand*)))

    ((forcall rator rand*)
     `(foreign-call ,rator . ,(%map-in-order E rand*)))

    ((assign lhs rhs)
     `(set! ,(E lhs) ,(E rhs)))

    ((foreign-label x)
     `(foreign-label ,x))

    ((fvar idx)
     (E-fvar idx))

    ((nfv)
     (E-nfv x))

    ((locals vars body)
     (E-locals vars body E))

    ((asm-instr op d s)
     `(asm ,op ,(E d) ,(E s)))

    ((disp s0 s1)
     `(disp ,(E s0) ,(E s1)))

    ((non-tail-call-frame rand* live body)
     (E-non-tail-call-frame rand* live body E))

    ((shortcut body handler)
     `(shortcut
	  ,(E body)
	,(E handler)))

    ((non-tail-call)
     (E-non-tail-call x E))

    (else x)))


(define (unparse-recordised-code/sexp input-expr)
  ;;Unparse the  struct instance  INPUT-EXPR (representing  recordized code  in the
  ;;core language already processed by the compiler) into a human readable symbolic
  ;;expression to  be used when printing  to some port for  miscellaneous debugging
  ;;purposes.
  ;;
  ;;This  module attempts  to  unparse  recordized code  and  construct a  symbolic
  ;;expression that still represents the struct types in the recordized code.
  ;;
  ;;This function recognises only structures of the following type:
  ;;
  ;;   assign		bind		clambda
  ;;   conditional	constant	fix
  ;;   forcall		foreign-label	funcall
  ;;   known		prelex		primopcall
  ;;   primref		rec*bind	recbind
  ;;   seq		var		asmcall
  ;;
  ;;other values are not processed and are returned as they are.
  ;;
  ;;*NOTE* Being that this function is used  only when debugging: it makes no sense
  ;;to use unsafe operations: LET'S KEEP IT SAFE!!!
  ;;
  (import SCHEME-OBJECTS-ONTOLOGY)
  (define (E x)
    (struct-case x
      ((constant)
       (E-constant 'constant x E))

      ((prelex)
       (E-var x))

      ((var)
       (E-var x))

      ((assign lhs rhs)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (E lhs))
	      (Y (E rhs)))
	 (if (symbol? (prelex-source-assigned? lhs))
	     `(assign-init ,X ,Y)
	   `(assign ,X ,Y))))

      ((primref x)
       `(primref ,x))

      ((known expr type)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (E expr))
	      (Y (core-type-tag-description type)))
	 `(known ,X ,Y)))

      ((clambda)
       (E-clambda x E E-var))

      ((closure-maker code freevar*)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (E code))
	      (Y (let ((freevar* (%map-in-order E freevar*)))
		   (if (null? freevar*)
		       'no-freevars
		     `(freevars: . ,freevar*)))))
	 `(closure-maker ,X ,Y)))

      ((primopcall op arg*)
       (cons* 'primopcall op (%%map-in-order E arg*)))

      ((asmcall op arg*)
       (cons* 'asmcall    op (%%map-in-order E arg*)))

      ((funcall rator rand*)
       (let ((rator (E rator)))
	 (cons* 'funcall rator (%%map-in-order E rand*))))

      ((forcall rator rand*)
       `(foreign-call ,rator . ,(%%map-in-order E rand*)))

      ((jmpcall label op rand*)
       ;;Let's impose order in the generation of X  and Y and Z so that the temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (%pretty-symbol label))
	      (Y (E op))
	      (Z (%map-in-order E rand*)))
	 `(jmpcall ,X ,Y . ,Z)))

      ((seq e0 e1)
       (E-seq 'seq e0 e1 E))

      ((conditional test conseq altern)
       (E-conditional 'conditional test conseq altern E))

      ((bind lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (list 'bind (%map-in-order list lhs* rhs*) body)))

      ((fix lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (list 'fix (%map-in-order list lhs* rhs*) body)))

      ((recbind lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (list 'recbind (%map-in-order list lhs* rhs*) body)))

      ((rec*bind lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (list 'rec*bind (%map-in-order list lhs* rhs*) body)))

      ((codes clambda* body)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (%map-in-order (lambda (clam)
				  (let ((sexp (E clam)))
				    (cons* (car sexp)
					   ;;Print the pretty gensym name.
					   `(label: ,(%pretty-symbol (clambda-label clam)))
					   (cdr sexp))))
				clambda*))
	      (Y (E body)))
	 `(codes ,X ,Y)))

      ((code-loc label)
       ;;Print the pretty gensym name.
       `(code-loc ,(%pretty-symbol label)))

      ((shortcut body handler)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (E body))
	      (Y (E handler)))
	 `(shortcut ,X ,Y)))

      ((locals vars body)
       (E-locals vars body E))

      ((object obj)
       `(object ,(cond ((symbol? obj)
			(%pretty-symbol obj))
		       (else
			(E obj)))))

      ;; ------------------------------

      ((foreign-label x)
       `(foreign-label ,x))

      ((fvar idx)
       (E-fvar idx))

      ((nfv)
       (E-nfv x))

      ((asm-instr op d s)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (E d))
	      (Y (E s)))
	 `(asm-instr ,op ,X ,Y)))

      ((disp s0 s1)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (E s0))
	      (Y (E s1)))
	 `(disp ,X ,Y)))

      ((non-tail-call-frame rand* live body)
       (E-non-tail-call-frame rand* live body E))

      ((shortcut body handler)
       ;;Let's  impose order  in the  generation of  X and  Y so  that the  temporary
       ;;variables numbering is deterministic and can  be used in the compiler's test
       ;;files.
       (let* ((X (E body))
	      (Y (E handler)))
	 `(shortcut ,X ,Y)))

      ((non-tail-call target value args mask size)
       (E-non-tail-call x E))

      (else x)))

  (define E-var (make-E-var E))
  (define E-nfv (make-E-nfv E))

  (E input-expr))


(define (unparse-recordised-code/pretty input-expr)
  ;;Unparse the  struct instance  INPUT-EXPR (representing  recordized code  in the
  ;;core language already processed by the compiler) into a human readable symbolic
  ;;expression to  be used when printing  to some port for  miscellaneous debugging
  ;;purposes.
  ;;
  ;;This module attempts  to unparse recordized code and  reconstruct a Scheme-like
  ;;symbolic expression; the returned sexp does *not* exactly represent the input.
  ;;
  ;;This function recognises only structures of the following type:
  ;;
  ;;   assign		bind		clambda
  ;;   conditional	constant	fix
  ;;   forcall	foreign-label	funcall
  ;;   known		prelex		primopcall
  ;;   primref	rec*bind	recbind
  ;;   seq		var		asmcall
  ;;
  ;;other values are not processed and are returned as they are.
  ;;
  ;;*NOTE* Being that this function is used  only when debugging: it makes no sense
  ;;to use unsafe operations: LET'S KEEP IT SAFE!!!
  ;;
  (import SCHEME-OBJECTS-ONTOLOGY)
  (define (E x)
    (struct-case x
      ((constant)
       (E-constant 'quote x E))

      ((prelex)
       (E-var x))

      ((var)
       (E-var x))

      ((assign lhs rhs)
       `(set! ,(E lhs) ,(E rhs)))

      ((primref x)
       x)

      ((known expr type)
       `(known ,(E expr) ,(core-type-tag-description type)))

      ((clambda)
       (E-clambda x E E-var))

      ((closure-maker code freevar*)
       `(closure-maker ,(E code)
		       ,(let ((freevar* (%map-in-order E freevar*)))
			  (if (null? freevar*)
			      'no-freevars
			    `(freevars: . ,freevar*)))))

      ((primopcall op arg*)
       (cons op (%%map-in-order E arg*)))

      ((asmcall op arg*)
       (cons* 'asmcall op (%%map-in-order E arg*)))

      ((funcall rator rand*)
       (let ((rator (E rator)))
	 (cons rator (%%map-in-order E rand*))))

      ((forcall rator rand*)
       `(foreign-call ,rator . ,(%%map-in-order E rand*)))

      ((jmpcall label op rand*)
       `(jmpcall ,(%pretty-symbol label) ,(E op) . ,(%map-in-order E rand*)))

      ((foreign-label x)
       `(foreign-label ,x))

      ((seq e0 e1)
       (E-seq 'begin e0 e1 E))

      ((conditional test conseq altern)
       (E-conditional 'if test conseq altern E))

      ((bind lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (E-let (%map-in-order list lhs* rhs*) body)))

      ((fix lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (list 'fix (%map-in-order list lhs* rhs*) body)))

      ((recbind lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (list 'letrec (%map-in-order list lhs* rhs*) body)))

      ((rec*bind lhs* rhs* body)
       (let* ((lhs* (%%map-in-order E-var lhs*))
	      (rhs* (%%map-in-order E     rhs*))
	      (body (E body)))
	 (list 'letrec* (%map-in-order list lhs* rhs*) body)))

      ((codes clambda* body)
       `(codes ,(%map-in-order (lambda (clam)
				 (let ((sexp (E clam)))
				   (cons* (car sexp)
					  ;;Print the pretty gensym name.
					  `(label: (%pretty-symbol (clambda-label clam)))
					  (cdr sexp))))
			       clambda*)
	       ,(E body)))

      ((code-loc label)
       ;;Print the pretty gensym name.
       `(code-loc ,(%pretty-symbol label)))

      ((shortcut body handler)
       `(shortcut ,(E body) ,(E handler)))

      ((locals vars body)
       (E-locals vars body E))

      ((object obj)
       `(object ,(cond ((symbol? obj)
			(%pretty-symbol obj))
		       (else
			(E obj)))))

      ;; ------------------------------

      ((foreign-label x)
       `(foreign-label ,x))

      ((fvar idx)
       (E-fvar idx))

      ((nfv)
       (E-nfv x))

      ((asm-instr op d s)
       `(asm-instr ,op ,(E d) ,(E s)))

      ((disp s0 s1)
       `(disp ,(E s0) ,(E s1)))

      ((non-tail-call-frame rand* live body)
       (E-non-tail-call-frame rand* live body E))

      ((shortcut body handler)
       `(shortcut ,(E body) ,(E handler)))

      ((non-tail-call target value args mask size)
       (E-non-tail-call x E))

      (else x)))

  (define E-var (make-E-var E))
  (define E-nfv (make-E-nfv E))

  (E input-expr))


;;;; helpers

(define (E-let b* body)
  ;;B*  must be  a list  of already  unparsed LET-like  bindings; BODY  must be  an
  ;;already unparsed symbolic expression representing a body.
  ;;
  ;;If B* represents  a single binding: compress  nested LET and LET*  forms into a
  ;;single LET* form.   If B* represents multiple bindings: just  return a LET-like
  ;;form.
  ;;
  ;;Example:
  ;;
  ;;   (let ((a 1))
  ;;     (let ((b 2))
  ;;       (let ((a 3))
  ;;         (list a b))))
  ;;
  ;;is compressed into:
  ;;
  ;;   (let* ((a 1)
  ;;          (b 2)
  ;;          (a 3))
  ;;     (list a b))
  ;;
  ;;while:
  ;;
  ;;   (let ((a 1)
  ;;         (b 2))
  ;;     (let ((c 3))
  ;;       (list a b c)))
  ;;
  ;;is returned as is.
  ;;
  (cond ((and (%list-of-one-item? b*)
	      (pair? body)
	      (or (eq? (car body) 'let*)
		  (and (eq? (car body) 'let)
		       (%list-of-one-item? (cadr body)))))
	 (list 'let* (append b* (cadr body)) (caddr body)))
	(else
	 (list 'let b* body))))

;;; --------------------------------------------------------------------

(module (E-clambda)

  (define (E-clambda x E E-var)
    (struct-case x
      ((clambda label.unused cls*)
       (let ((cls* (%%map-in-order (lambda (clause)
				     (E-clambda-clause clause E E-var))
				   cls*)))
	 (if (%list-of-one-item? cls*)
	     (cons 'lambda (car cls*))
	   (cons 'case-lambda cls*))))))

  (define (E-clambda-clause x E E-var)
    (struct-case x
      ((clambda-case info body)
       (let ((args (E-args (case-info-proper info) (case-info-args info) E E-var)))
	 (list args (E body))))))

  (define (E-args proper? x E E-var)
    (if proper?
	(%%map-in-order E-var x)
      ;;The loop below is  like MAP but for improper lists: it  maps E-var over the
      ;;improper list X.
      (let recur ((A (car x))
		  (D (cdr x)))
	(if (pair? D)
	    (let ((A (E-var A)))
	      (cons A (recur (car D) (cdr D))))
	  (E-var A)))))

  #| end of module: E-clambda |# )

;;; --------------------------------------------------------------------

(define (E-constant sym x E)
  (list sym
	(struct-case x
	  ((constant x.const)
	   (cond ((symbol? x.const)
		  ;;Extract the pretty name; this is useful when X.CONST is a loc gensym.
		  (%pretty-symbol x.const))
		 ((object? x.const)
		  (E x.const))
		 ((closure-maker? x.const)
		  (E x.const))
		 ((code-loc? x.const)
		  (E x.const))
		 ((foreign-label? x.const)
		  (struct-case x.const
		    ((foreign-label name)
		     `(foreign-label ,name))))
		 (else
		  x.const))))))

;;; --------------------------------------------------------------------

(module (make-E-var)
  ;;Given a struct instance X of type  PRELEX or VAR, identifying the location of a
  ;;binding: return a  symbol representing a unique name for  the binding.  The map
  ;;between structures and symbols is cached in a hash table.
  ;;
  ;;This function acts in such a way that the input:
  ;;
  ;;   (let ((a 1))
  ;;     (let ((a a))
  ;;       a))
  ;;
  ;;is transformed into:
  ;;
  ;;   (let ((a_0 1))
  ;;     (let ((a_1 a_0))
  ;;       a_1))
  ;;
  (define (make-E-var E)
    (define H
      ;;Map PRELEX and VAR structures to already built binding name symbols.
      (make-eq-hashtable))
    (define T
      ;;Map binding  pretty string names  to number of  times this string  name has
      ;;already been used.
      (make-hashtable string-hash string=?))
    (lambda (x)
      (or (hashtable-ref H x #f)
	  (struct-case x
	    ((prelex x.name)
	     (%build-name x x.name T H))
	    ((var x.name)
	     (let ((rep (%build-name x x.name T H)))
	       (cond ((var-loc x)
		      => (lambda (loc)
			   (cons rep (E loc))))
		     (else rep))))
	    ((fvar)
	     (E x))
	    (else x)))))

  (define (%build-name x x.name T H)
    (let* ((name (symbol->string x.name))
	   (N    (hashtable-ref T name 0)))
      (hashtable-set! T name (+ N 1))
      (receive-and-return (sym)
	  (string->symbol (string-append name "_" (number->string N)))
	(hashtable-set! H x sym))))

  #| end of module: make-E-var |# )

;;; --------------------------------------------------------------------

(define (make-E-nfv E)
  ;;Given a struct instance  X of type NFV, identifying the  location of a non-tail
  ;;call stack operand: return a symbol representing a unique name for the operand.
  ;;The map between structures and symbols is cached in a hash table.
  ;;
  (define H
    ;;Map NFV structures to already built operand name symbols.
    (make-eq-hashtable))
  (define T
    ;;Map  operand pretty  string names  to number  of times  this string  name has
    ;;already been used.
    (make-hashtable string-hash string=?))
  (lambda (x)
    (or (hashtable-ref H x #f)
	(struct-case x
	  ((nfv idx loc)
	   (let ((sym (let* ((name (format "nfv.~a" idx))
			     (N    (hashtable-ref T name 0)))
			(hashtable-set! T name (+ N 1))
			(receive-and-return (sym)
			    (string->symbol (string-append name "_" (number->string N)))
			  (hashtable-set! H x sym)))))
	     (if loc
		 (cons sym (E loc))
	       sym)))))))

;;; --------------------------------------------------------------------

(define (E-seq sym e0 e1 E)
  (cons sym
	;;Here we flatten nested SEQ instances into a unique output SEQ form.
	(let recur ((expr  e0)
		    (expr* (list e1)))
	  (struct-case expr
	    ((seq expr.e0 expr.e1)
	     (recur expr.e0 (cons expr.e1 expr*)))
	    (else
	     (let ((expr^ (E expr)))
	       (if (pair? expr*)
		   (cons expr^ (recur (car expr*) (cdr expr*)))
		 (list expr^))))))))


(define (E-conditional symbol test conseq altern E)
  ;;Let's impose  order in  the generation of  X and  Y and Z  so that  the temporary
  ;;variables  numbering is  deterministic and  can be  used in  the compiler's  test
  ;;files.
  (let* ((X (E test))
	 (Y (E conseq))
	 (Z (E altern)))
    `(,symbol ,X ,Y ,Z)))

(define (E-non-tail-call x E)
  (struct-case x
    ((non-tail-call target retval-var all-rand* mask size)
     ;;Let's impose order  in the generation of X  and Y and Z so  that the temporary
     ;;variables numbering  is deterministic and can  be used in the  compiler's test
     ;;files.
     (let* ((X (and target
		    (cond ((symbol? target)
			   (%pretty-symbol target))
			  (else target))))
	    (Y (and retval-var (E retval-var)))
	    (Z (if (and all-rand* (pair? all-rand*))
		   `(all-rand*: . ,(%map-in-order (lambda (arg)
						    (cond ((symbol? arg)
							   arg)
							  ((nfv? arg)
							   (E arg))
							  ((fvar? arg)
							   (E arg))
							  (else arg)))
						  all-rand*))
		 '(all-rand*: #f))))
       `(non-tail-call
	  (target: ,X)
	  (retval-var: ,Y)
	  ,Z
	  (mask:   ,mask)
	  (size:   ,size))))))

(define (E-non-tail-call-frame rand* live body E)
  ;;Let's impose order in  the generation of X and Y so  that the temporary variables
  ;;numbering is deterministic and can be used in the compiler's test files.
  (let* ((X (if (pair? rand*)
		`(rand*: . ,(%map-in-order E rand*))
	      '(rand*: #f)))
	 (Y (if live
		`(live: . ,(%map-in-order E live))
	      '(live: #f)))
	 (Z (E body)))
    `(non-tail-call-frame ,X ,Y ,Z)))

(define (E-fvar idx)
  (string->symbol (format "fvar.~a" idx)))

(define (E-locals vars body E)
  ;;Let's impose order in  the generation of X and Y so  that the temporary variables
  ;;numbering is deterministic and can be used in the compiler's test files.
  (let* ((X (cond ((null? vars)
		   '(local-vars: . #f))
		  ((list? vars)
		   `(local-vars: . ,(let ((A (car vars)))
				      (if (vector? A)
					  (cons (vector-map E A)
						(%map-in-order E (cdr vars)))
					(%map-in-order E vars)))))
		  (else
		   ;;This includes the case of VARS being #f.
		   `(local-vars: ,vars))))
	 (Y (E body)))
    `(locals ,X ,Y)))

;;; --------------------------------------------------------------------

(define (%pretty-symbol sym)
  (string->symbol (symbol->string sym)))

(define (%%map-in-order f ls)
  ;;This version of  MAP imposes an order to  the application of F to  the items in
  ;;LS.
  ;;
  (if (pair? ls)
      (let ((a (f (car ls))))
	(cons a (%%map-in-order f (cdr ls))))
    '()))

;;; --------------------------------------------------------------------

(define unparse-recordized-code        unparse-recordised-code)
(define unparse-recordized-code/sexp   unparse-recordised-code/sexp)
(define unparse-recordized-code/pretty unparse-recordised-code/pretty)


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; eval: (put 'shortcut			'scheme-indent-function 1)
;; End:
