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


(module (debug-scc optimize-letrec current-letrec-pass)



(define (unique-prelex x)
  (let ([x (make-prelex (prelex-name x) (prelex-operand x))])
    (set-prelex-source-referenced?! x #t)
    x))

(define (build-assign* lhs* rhs* body)
  (define (mark-assigned! lhs)
      ;;; FIXME: this is very fragile
    (unless (prelex-source-assigned? lhs)
      (set-prelex-source-assigned?! lhs
				    (or (prelex-global-location lhs) #t))))
  (for-each mark-assigned! lhs*)
  (let f ([lhs* lhs*] [rhs* rhs*])
    (cond
     [(null? lhs*) body]
     [else
      (make-seq
       (make-assign (car lhs*) (car rhs*))
       (f (cdr lhs*) (cdr rhs*)))])))

(define (optimize-letrec/basic x)
  (define who 'optimize-letrec/basic)
  (define (do-rec*bind lhs* rhs* body)
    (make-bind lhs* (map (lambda (x) (make-constant #f)) lhs*)
	       (build-assign* lhs* rhs* body)))
  (define (do-recbind lhs* rhs* body)
    (let ([t* (map unique-prelex lhs*)])
      (make-bind lhs* (map (lambda (x) (make-constant #f)) lhs*)
		 (make-bind t* rhs*
			    (build-assign* lhs* t* body)))))
  (define (L x)
    (struct-case x
      [(clambda g cls* cp free name)
       (make-clambda g
		     (map (lambda (x)
			    (struct-case x
			      [(clambda-case info body)
			       (make-clambda-case info (E body))]))
		       cls*)
		     cp free name)]))
  (define (E x)
    (struct-case x
      [(constant) x]
      [(prelex)
       (assert (prelex-source-referenced? x))
       x]
      [(assign lhs rhs)
       (assert (prelex-source-assigned? lhs))
       (make-assign lhs (E rhs))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (if (null? lhs*)
	   (E body)
	 (make-bind lhs* (map E rhs*) (E body)))]
      [(recbind lhs* rhs* body)
       (if (null? lhs*)
	   (E body)
	 (do-recbind lhs* (map E rhs*) (E body)))]
      [(rec*bind lhs* rhs* body)
       (if (null? lhs*)
	   (E body)
	 (do-rec*bind lhs* (map E rhs*) (E body)))]
      [(conditional e0 e1 e2)
       (make-conditional (E e0) (E e1) (E e2))]
      [(seq e0 e1) (make-seq (E e0) (E e1))]
      [(clambda g cls* cp free name)
       (L x)]
      [(funcall rator rand*)
       (make-funcall (E rator) (map E rand*))]
      [(mvcall p c)
       (make-mvcall (E p) (E c))]
      [(forcall rator rand*)
       (make-forcall rator (map E rand*))]
      [else (error who "invalid expression" (unparse-recordized-code x))]))
  (E x))


(define (optimize-letrec/waddell x)
  (define simple-primitives '())
  (define who 'optimize-letrec/waddell)
  (define (extend-hash lhs* h ref)
    (for-each (lambda (lhs) (hashtable-set! h lhs #t)) lhs*)
    (lambda (x)
      (unless (hashtable-ref h x #f)
	(hashtable-set! h x #t)
	(ref x))))
  (define (E* x* ref comp)
    (cond
     [(null? x*) '()]
     [else
      (cons (E (car x*) ref comp)
	    (E* (cdr x*) ref comp))]))
  (define (do-rhs* i lhs* rhs* ref comp vref vcomp)
    (cond
     [(null? rhs*) '()]
     [else
      (let ([h (make-eq-hashtable)]
	    [rest (do-rhs* (fxadd1 i) lhs* (cdr rhs*) ref comp vref vcomp)])
	(let ([ref
	       (lambda (x)
		 (unless (hashtable-ref h x #f)
		   (hashtable-set! h x #t)
		   (ref x)
		   (when (memq x lhs*)
		     (vector-set! vref i #t))))]
	      [comp
	       (lambda ()
		 (vector-set! vcomp i #t)
		 (comp))])
	  (cons (E (car rhs*) ref comp) rest)))]))
  (define (partition-rhs* i lhs* rhs* vref vcomp)
    (cond
     [(null? lhs*) (values '() '() '() '() '() '())]
     [else
      (let-values
	  ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
	    (partition-rhs* (fxadd1 i) (cdr lhs*) (cdr rhs*) vref vcomp)]
	   [(lhs rhs) (values (car lhs*) (car rhs*))])
	(cond
	 [(prelex-source-assigned? lhs)
	  (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
	 [(clambda? rhs)
	  (values slhs* srhs* (cons lhs llhs*) (cons rhs lrhs*) clhs* crhs*)]
	 [(or (vector-ref vref i) (vector-ref vcomp i))
	  (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
	 [else
	  (values (cons lhs slhs*) (cons rhs srhs*) llhs* lrhs* clhs* crhs*)]
	 ))]))
  (define (do-recbind lhs* rhs* body ref comp letrec?)
    (let ([h (make-eq-hashtable)]
	  [vref (make-vector (length lhs*) #f)]
	  [vcomp (make-vector (length lhs*) #f)])
      (let* ([ref (extend-hash lhs* h ref)]
	     [body (E body ref comp)])
	(let ([rhs* (do-rhs* 0 lhs* rhs* ref comp vref vcomp)])
	  (let-values ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
			(partition-rhs* 0 lhs* rhs* vref vcomp)])
              ;;; (let ([made-complex
              ;;;        (filter (lambda (x) (not (var-assigned x)))
              ;;;                clhs*)])
              ;;;   (unless (null? made-complex)
              ;;;     (set! complex-count
              ;;;       (+ complex-count (length made-complex)))
              ;;;     (printf "COMPLEX (~s) = ~s\n"
              ;;;             complex-count
              ;;;             (map unparse-recordized-code made-complex))))
	    (let ([void* (map (lambda (x) (make-constant (void))) clhs*)])
	      (make-bind slhs* srhs*
			 (make-bind clhs* void*
				    (make-fix llhs* lrhs*
					      (if letrec?
						  (let ([t* (map unique-prelex clhs*)])
						    (make-bind t* crhs*
							       (build-assign* clhs* t* body)))
						(build-assign* clhs* crhs* body)))))))))))
  (define (E x ref comp)
    (struct-case x
      [(constant) x]
      [(prelex) (ref x) x]
      [(assign lhs rhs)
       (ref lhs)
       (comp)
       (make-assign lhs (E rhs ref comp))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let ([rhs* (E* rhs* ref comp)])
	 (let ([h (make-eq-hashtable)])
	   (let ([body (E body (extend-hash lhs* h ref) comp)])
	     (make-bind lhs* rhs* body))))]
      [(recbind lhs* rhs* body)
       (if (null? lhs*)
	   (E body ref comp)
	 (do-recbind lhs* rhs* body ref comp #t))]
      [(rec*bind lhs* rhs* body)
       (if (null? lhs*)
	   (E body ref comp)
	 (do-recbind lhs* rhs* body ref comp #f))]
      [(conditional e0 e1 e2)
       (make-conditional (E e0 ref comp) (E e1 ref comp) (E e2 ref comp))]
      [(seq e0 e1) (make-seq (E e0 ref comp) (E e1 ref comp))]
      [(clambda g cls* cp free name)
       (make-clambda g
		     (map (lambda (x)
			    (struct-case x
			      [(clambda-case info body)
			       (let ([h (make-eq-hashtable)])
				 (let ([body (E body (extend-hash (case-info-args info) h ref) void)])
				   (make-clambda-case info body)))]))
		       cls*)
		     cp free name)]
      [(funcall rator rand*)
       (let ([rator (E rator ref comp)] [rand* (E* rand* ref comp)])
	 (struct-case rator
	   [(primref op)
	    (unless (memq op simple-primitives)
	      (comp))]
	   [else
	    (comp)])
	 (make-funcall rator rand*))]
      [(mvcall p c)
       (let ([p (E p ref comp)] [c (E c ref comp)])
	 (comp)
	 (make-mvcall p c))]
      [(forcall rator rand*)
       (make-forcall rator (E* rand* ref comp))]
      [else (error who "invalid expression" (unparse-recordized-code x))]))
  (E x (lambda (x) (error who "free var found" x))
     void))

(define (optimize-letrec/scc x)
  (define who 'optimize-letrec/scc)
  (module (get-sccs-in-order)
    (define-struct node (data link* lowlink root done collection))
    (define (create-graph v* e** data*)
      (define h (make-eq-hashtable))
      (let ([v*
	     (let f ([v* v*] [data* data*])
	       (cond
		[(null? v*) '()]
		[else
		 (let ([node (make-node (car data*) '() #f #f #f #f)])
		   (hashtable-set! h (car v*) node)
		   (cons node (f (cdr v*) (cdr data*))))]))])
	(for-each
            (lambda (v e*)
              (set-node-link*! v
			       (map (lambda (f)
				      (or (hashtable-ref h f #f)
					  (error who "invalid node" f)))
				 e*)))
	  v* e**)
	v*))
    (define (compute-sccs v*) ; Tarjan's algorithm
      (define scc* '())
      (define (compute-sccs v)
	(define index 0)
	(define stack '())
	(define (tarjan v)
	  (let ([v-index index])
	    (set-node-root! v v-index)
	    (set! stack (cons v stack))
	    (set! index (fx+ index 1))
	    (for-each
                (lambda (v^)
                  (unless (node-done v^)
                    (unless (node-root v^) (tarjan v^))
                    (set-node-root! v (fxmin (node-root v) (node-root v^)))))
	      (node-link* v))
	    (when (fx= (node-root v) v-index)
	      (set! scc*
		    (cons
		     (let f ([ls stack])
		       (let ([v^ (car ls)])
			 (set-node-done! v^ #t)
			 (cons v^ (if (eq? v^ v)
				      (begin (set! stack (cdr ls)) '())
				    (f (cdr ls))))))
		     scc*)))))
	(tarjan v))
      (for-each (lambda (v) (unless (node-done v) (compute-sccs v))) v*)
      (reverse scc*))
    (define (get-sccs-in-order n* e** data*)
      (let ([G (create-graph n* e** data*)])
	(let ([sccs (compute-sccs G)])
	  (map (lambda (scc) (map node-data scc)) sccs)))))
  (define (gen-letrecs scc* ordered? body)
    (define (mkfix b* body)
      (if (null? b*)
	  body
	(make-fix (map binding-lhs b*)
		  (map binding-rhs b*)
		  body)))
    (define (gen-letrec scc fix* body)
      (define (mklet lhs* rhs* body)
	(if (null? lhs*)
	    body
	  (make-bind lhs* rhs* body)))
      (define (lambda-binding? x)
	(and (not (prelex-source-assigned? (binding-lhs x)))
	     (clambda? (binding-rhs x))))
      (define (mkset!s b* body)
	(cond
	 [(null? b*) body]
	 [else
	  (let* ([b (car b*)]
		 [lhs (binding-lhs b)])
	    (unless (prelex-source-assigned? lhs)
	      (when (debug-scc)
		(printf "MADE COMPLEX ~s\n" (unparse-recordized-code lhs)))
	      (set-prelex-source-assigned?! lhs
					    (or (prelex-global-location lhs) #t)))
	    (make-seq
	     (make-assign lhs (binding-rhs b))
	     (mkset!s (cdr b*) body)))]))
      (cond
       [(null? (cdr scc))
	(let ([b (car scc)])
	  (cond
	   [(lambda-binding? b)
	    (values (cons b fix*) body)]
	   [(not (memq b (binding-free* b)))
	    (values '()
		    (mklet (list (binding-lhs b))
			   (list (binding-rhs b))
			   (mkfix fix* body)))]
	   [else
	    (values '()
		    (mklet (list (binding-lhs b))
			   (list (make-funcall (make-primref 'void) '()))
			   (mkset!s scc
				    (mkfix fix* body))))]))]
       [else
	(let-values ([(lambda* complex*)
		      (partition lambda-binding? scc)])
	  (cond
	   [(null? complex*)
	    (values (append lambda* fix*) body)]
	   [else
	    (let ([complex*
		   (if ordered? (sort-bindings complex*) complex*)])
	      (values '()
		      (mklet (map binding-lhs complex*)
			     (map (lambda (x)
				    (make-funcall (make-primref 'void) '()))
			       complex*)
			     (mkfix (append lambda* fix*)
				    (mkset!s complex* body)))))]))]))
    (let-values ([(fix* body)
		  (let f ([scc* scc*])
		    (cond
		     [(null? scc*) (values '() body)]
		     [else
		      (let-values ([(fix* body) (f (cdr scc*))])
			(gen-letrec (car scc*) fix* body))]))])
      (mkfix fix* body)))
  (define (do-recbind lhs* rhs* body bc ordered?)
    (define (make-bindings lhs* rhs* bc i)
      (cond
       [(null? lhs*) '()]
       [else
	(let ([b (make-binding i (car lhs*) (car rhs*) #f bc '())])
	  (set-prelex-operand! (car lhs*) b)
	  (cons b (make-bindings (cdr lhs*) (cdr rhs*) bc (+ i 1))))]))
    (define (complex? x)
      (or (binding-complex x)
	  (prelex-source-assigned? (binding-lhs x))))
    (define (insert-order-edges b*)
      (define (mark pb b*)
	(unless (null? b*)
	  (let ([b (car b*)])
	    (if (complex? b)
		(let ([free* (binding-free* b)])
		  (unless (memq pb free*)
		    (set-binding-free*! b (cons pb free*)))
		  (mark b (cdr b*)))
	      (mark pb (cdr b*))))))
      (unless (null? b*)
	(let ([b (car b*)])
	  (if (complex? b)
	      (mark b (cdr b*))
	    (insert-order-edges (cdr b*))))))
    (let ([b* (make-bindings lhs* rhs* bc 0)])
      (for-each (lambda (b) (set-binding-rhs! b (E (binding-rhs b) b))) b*)
      (for-each (lambda (x) (set-prelex-operand! x #f)) lhs*)
      (let ([body (E body bc)])
	(when ordered? (insert-order-edges b*))
	(let ([scc* (get-sccs-in-order b* (map binding-free* b*) b*)])
	  (when (debug-scc)
	    (printf "SCCS:\n")
	    (for-each
                (lambda (scc)
                  (printf "  ~s\n"
			  (map unparse-recordized-code (map binding-lhs scc))))
	      scc*))
	  (gen-letrecs scc* ordered? body)))))
  (define (sort-bindings ls)
    (list-sort
     (lambda (x y) (< (binding-serial x) (binding-serial y)))
     ls))
  (define-struct binding (serial lhs rhs complex prev free*))
  (define (mark-complex bc)
    (unless (binding-complex bc)
      (set-binding-complex! bc #t)
      (mark-complex (binding-prev bc))))
  (define (mark-free var bc)
    (let ([rb (prelex-operand var)])
      (when rb
	(let ([lb
	       (let ([pr (binding-prev rb)])
		 (let f ([bc bc])
		   (let ([bcp (binding-prev bc)])
		     (cond
		      [(eq? bcp pr) bc]
		      [else (f bcp)]))))])
	  (let ([free* (binding-free* lb)])
	    (unless (memq rb free*)
                ;(printf "MARK FREE ~s in ~s\n"
                ;        (unparse-recordized-code (binding-lhs rb))
                ;        (unparse-recordized-code (binding-lhs lb)))
	      (set-binding-free*! lb (cons rb free*))))))))
  (define (E* x* bc)
    (map (lambda (x) (E x bc)) x*))
  (define (L x bc)
    (struct-case x
      [(clambda g cls* cp free name)
       (let ([bc (make-binding #f #f #f #t bc '())])
	 (make-clambda g
		       (map (lambda (x)
			      (struct-case x
				[(clambda-case info body)
				 (make-clambda-case info (E body bc))]))
			 cls*)
		       cp free name))]))
  (define (E x bc)
    (struct-case x
      [(constant) x]
      [(prelex)
       (assert (prelex-source-referenced? x))
       (mark-free x bc)
       (when (prelex-source-assigned? x)
	 (mark-complex bc))
       x]
      [(assign lhs rhs)
       (assert (prelex-source-assigned? lhs))
		;(set-prelex-source-assigned?! lhs #t)
       (mark-free lhs bc)
       (mark-complex bc)
       (make-assign lhs (E rhs bc))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (if (null? lhs*)
	   (E body bc)
	 (make-bind lhs* (E* rhs* bc) (E body bc)))]
      [(recbind lhs* rhs* body)
       (if (null? lhs*)
	   (E body bc)
	 (do-recbind lhs* rhs* body bc #f))]
      [(rec*bind lhs* rhs* body)
       (if (null? lhs*)
	   (E body bc)
	 (do-recbind lhs* rhs* body bc #t))]
      [(conditional e0 e1 e2)
       (make-conditional (E e0 bc) (E e1 bc) (E e2 bc))]
      [(seq e0 e1) (make-seq (E e0 bc) (E e1 bc))]
      [(clambda g cls* cp free name)
       (L x bc)]
      [(funcall rator rand*)
       (mark-complex bc)
       (make-funcall (E rator bc) (E* rand* bc))]
      [(mvcall p c)
       (mark-complex bc)
       (make-mvcall (E p bc) (E c bc))]
      [(forcall rator rand*)
       (mark-complex bc)
       (make-forcall rator (E* rand* bc))]
      [else (error who "invalid expression" (unparse-recordized-code x))]))
		;(printf "===========================================\n")
  (let ([x (E x (make-binding #f #f #f #t #t '()))])
		;(pretty-print (unparse-recordized-code x))
    x))


(define debug-scc (make-parameter #f))

(define current-letrec-pass
  (make-parameter 'scc
    (lambda (x)
      (if (memq x '(scc waddell basic))
	  x
	(die 'current-letrec-pass "invalid" x)))))


(define (optimize-letrec x)
  (case (current-letrec-pass)
    [(scc)     (optimize-letrec/scc x)]
    [(waddell) (optimize-letrec/waddell x)]
    [(basic)   (optimize-letrec/basic x)]
    [else (die 'optimize-letrec "invalid" (current-letrec-pass))]))


;;;; done

#| end of module |# )

;;; end of file
