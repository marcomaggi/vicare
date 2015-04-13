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


(library (psyntax.syntax-match)
  (export
    syntax-match
    convert-pattern
    ellipsis? underscore?
    %verify-literals
    ellipsis-map
    syntax-dispatch)
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer)
    (prefix (rnrs syntax-case) sys.)
    (psyntax.lexical-environment)
    (psyntax.compat))


(define-syntax syntax-match
  ;;The SYNTAX-MATCH macro is almost like SYNTAX-CASE macro.  Except that:
  ;;
  ;;* The  syntax objects matched  are OUR stx objects,  not the host  systems syntax
  ;;  objects (whatever they may be we don't care).
  ;;
  ;;* The  literals are matched against  those in the system  library (psyntax system
  ;;  $all).  See the function SCHEME-STX for how those identifier are created.
  ;;
  ;;* The variables  in the patterns are  bound to ordinary variables  not to special
  ;;  pattern variables.
  ;;
  ;;The actual matching between the input expression and the patterns is performed by
  ;;the function  SYNTAX-DISPATCH; the  patterns in SYNTAX-MATCH  are converted  to a
  ;;sexps and handed to SYNTAX-DISPATCH along with the input expression.
  ;;
  (let ()
    (define (transformer stx)
      (syntax-case stx ()

	;;No more clauses.  This clause matches STX when:
	;;
	;;* The SYNTAX-MATCH use input form does not match any of the patterns, so we
	;;  want to raise a syntax violation in the expanded code.  Example:
	;;
	;;     (syntax-match #'(1 2) ()
	;;      ((?a ?b ?c)
	;;       (do-something-with ?a ?b ?c)))
	;;
	;;  the input form #'(1 2) does not match the pattern "(?a ?b ?c)".
	;;
	;;* The SYNTAX-MATCH use has the format:
	;;
	;;     (syntax-match ?input-form ?literals)
	;;
	;;  that is: no clauses were specified.  In this case we still want to expand
	;;  into a syntax violation raising form.
	;;
	;;Notice, again, that we do not want to raise a syntax error here, but in the
	;;expanded code.
	;;
	((_ ?input-form (?literals ...))
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (syntax
	  (syntax-violation 'syntax-match
	    "invalid syntax, no clause matches the input form" ?input-form)))

	;;The next clause has a fender.
	;;
	((_ ?input-form (?literals ...) (?pattern ?fender ?body) ?clause* ...)
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (receive (pattern ptnvars/levels)
	     (%convert-single-pattern (syntax ?pattern) (syntax (?literals ...)))
	   (with-syntax
	       ((PATTERN                   (sys.datum->syntax (syntax here) pattern))
		(((PTNVARS . LEVELS) ...)  ptnvars/levels))
	     (syntax
	      (let ((T ?input-form))
		;;If the input expression matches the symbolic expression PATTERN...
		(let ((ls/false (syntax-dispatch T 'PATTERN)))
		  (if (and ls/false
			   ;;...and the pattern variables satisfy the fender...
			   (apply (lambda (PTNVARS ...) ?fender) ls/false))
		      ;;...evaluate the body with the pattern variables assigned.
		      (apply (lambda (PTNVARS ...) ?body) ls/false)
		    ;;...else try to match the next clause.
		    (syntax-match T (?literals ...) ?clause* ...))))))))

	;;The next clause has NO fender.
	;;
	((_ ?input-form (?literals ...) (?pattern ?body) clause* ...)
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (receive (pattern ptnvars/levels)
	     (%convert-single-pattern (syntax ?pattern) (syntax (?literals ...)))
	   (with-syntax
	       ((PATTERN                   (sys.datum->syntax (syntax here) pattern))
		(((PTNVARS . LEVELS) ...)  ptnvars/levels))
	     (syntax
	      (let ((T ?input-form))
		;;If the input expression matches the symbolic expression PATTERN...
		(let ((ls/false (syntax-dispatch T 'PATTERN)))
		  (if ls/false
		      ;;...evaluate the body with the pattern variables assigned.
		      (apply (lambda (PTNVARS ...) ?body) ls/false)
		    ;;...else try to match the next clause.
		    (syntax-match T (?literals ...) clause* ...))))))))

	;;This is a true error in he use of SYNTAX-MATCH.
	;;
	(?stuff
	 (sys.syntax-violation 'syntax-match "invalid syntax in macro use" stx))
	))

    (module (%convert-single-pattern)

      (case-define %convert-single-pattern
	;;Recursive function.   Transform the PATTERN-STX into  a symbolic expression
	;;to  be handed  to SYNTAX-DISPATCH.   PATTERN-STX  must be  a syntax  object
	;;holding the  SYNTAX-MATCH pattern  to convert.  LITERALS  must be  a syntax
	;;object holding a list of identifiers being the literals in the PATTERN-STX.
	;;
	;;Return 2 values:
	;;
	;;1. The pattern as sexp.
	;;
	;;2. An ordered list of pairs, each representing a pattern variable that must
	;;   be bound whenever the body  associated to the pattern is evaluated.  The
	;;   car of each pair is the symbol being the pattern variable name.  The cdr
	;;   of each pair  is an exact integer representing the  nesting level of the
	;;   pattern variable.
	;;
	((pattern-stx literals)
	 (%convert-single-pattern pattern-stx literals 0 '()))

	((pattern-stx literals nesting-level pattern-vars)
	 (syntax-case pattern-stx ()

	   ;;A literal identifier is encoded as:
	   ;;
	   ;;   #(scheme-id ?identifier)
	   ;;
	   ;;the wildcard underscore identifier is encoded as:
	   ;;
	   ;;   _
	   ;;
	   ;;any other identifier will bind a variable and it is encoded as:
	   ;;
	   ;;   any
	   ;;
	   (?identifier
	    (sys.identifier? (syntax ?identifier))
	    (cond ((%bound-identifier-member? pattern-stx literals)
		   (values `#(scheme-id ,(sys.syntax->datum pattern-stx)) pattern-vars))
		  ((sys.free-identifier=? pattern-stx (syntax _))
		   (values '_ pattern-vars))
		  (else
		   (values 'any (cons (cons pattern-stx nesting-level)
				      pattern-vars)))))

	   ;;A tail pattern  with ellipsis which does not bind  a variable is encoded
	   ;;as:
	   ;;
	   ;;   #(each ?pattern)
	   ;;
	   ;;a tail pattern with ellipsis which does bind a variable is encoded as:
	   ;;
	   ;;   each-any
	   ;;
	   ((?pattern ?dots)
	    (%ellipsis? (syntax ?dots))
	    (receive (pattern^ pattern-vars^)
		(%convert-single-pattern (syntax ?pattern) literals
					 (+ nesting-level 1) pattern-vars)
	      (values (if (eq? pattern^ 'any)
			  'each-any
			`#(each ,pattern^))
		      pattern-vars^)))

	   ;;A non-tail pattern with ellipsis is encoded as:
	   ;;
	   ;;  #(each+ ?pattern-ellipsis (?pattern-following ...) . ?tail-pattern)
	   ;;
	   ((?pattern-x ?dots ?pattern-y ... . ?pattern-z)
	    (%ellipsis? (syntax ?dots))
	    (let*-values
		(((pattern-z pattern-vars)
		  (%convert-single-pattern (syntax ?pattern-z) literals
					   nesting-level pattern-vars))

		 ((pattern-y* pattern-vars)
		  (%convert-multi-pattern  (syntax (?pattern-y ...)) literals
					   nesting-level pattern-vars))

		 ((pattern-x pattern-vars)
		  (%convert-single-pattern (syntax ?pattern-x) literals
					   (+ nesting-level 1) pattern-vars)))
	      (values `#(each+ ,pattern-x ,(reverse pattern-y*) ,pattern-z)
		      pattern-vars)))

	   ;;A pair is encoded as pair.
	   ;;
	   ((?car . ?cdr)
	    (let*-values
		(((pattern-cdr pattern-vars)
		  (%convert-single-pattern (syntax ?cdr) literals
					   nesting-level pattern-vars))

		 ((pattern-car pattern-vars)
		  (%convert-single-pattern (syntax ?car) literals
					   nesting-level pattern-vars)))
	      (values (cons pattern-car pattern-cdr) pattern-vars)))

	   ;;Null is encoded as null.
	   ;;
	   (()
	    (values '() pattern-vars))

	   ;;A vector is encoded as:
	   ;;
	   ;;   #(vector ?datum)
	   ;;
	   (#(?item ...)
	    (receive (pattern-item* pattern-vars)
		(%convert-single-pattern (syntax (?item ...)) literals
					 nesting-level pattern-vars)
	      (values `#(vector ,pattern-item*) pattern-vars)))

	   ;;A datum is encoded as:
	   ;;
	   ;;   #(atom ?datum)
	   ;;
	   (?datum
	    (values `#(atom ,(sys.syntax->datum (syntax ?datum))) pattern-vars))
	   )))

      (define (%convert-multi-pattern pattern* literals nesting-level pattern-vars)
	;;Recursive function.
	;;
	(if (null? pattern*)
	    (values '() pattern-vars)
	  (let*-values
	      (((y pattern-vars^)
		(%convert-multi-pattern  (cdr pattern*) literals nesting-level pattern-vars))
	       ((x pattern-vars^^)
		(%convert-single-pattern (car pattern*) literals nesting-level pattern-vars^)))
	    (values (cons x y) pattern-vars^^))))

      (define (%bound-identifier-member? id list-of-ids)
	;;Return  #t if  the  identifier  ID is  BOUND-IDENTIFIER=?   to  one of  the
	;;identifiers in LIST-OF-IDS.
	;;
	(and (pair? list-of-ids)
	     (or (sys.bound-identifier=? id (car list-of-ids))
		 (%bound-identifier-member? id (cdr list-of-ids)))))

      (define (%ellipsis? x)
	(and (sys.identifier? x)
	     (sys.free-identifier=? x (syntax (... ...)))))

      #| end of module: %CONVERT-SINGLE-PATTERN |# )

    transformer))


;;;; pattern matching helpers

(define (convert-pattern pattern-stx literals)
  ;;This function is used both by  the transformer of the non-core macro
  ;;WITH-SYNTAX and  by the transformer  of the core  macro SYNTAX-CASE.
  ;;Transform the syntax object  PATTERN-STX, representing a SYNTAX-CASE
  ;;pattern, into a pattern in the format recognised by SYNTAX-DISPATCH.
  ;;
  ;;LITERALS is null or a  list of identifiers representing the literals
  ;;from a SYNTAX-CASE use.  Notice that the ellipsis and the underscore
  ;;identifiers cannot be literals.
  ;;
  ;;Return  2   values:  the  pattern  for   SYNTAX-DISPATCH,  an  alist
  ;;representing the pattern variables:
  ;;
  ;;* The  keys of the alist  are identifiers representing the  names of
  ;;  the pattern variables.
  ;;
  ;;*  The  values   of  the  alist  are   non-negative  exact  integers
  ;;   representing  the ellipsis  nesting  level  of the  corresponding
  ;;  pattern variable.  See SYNTAX-TRANSFORMER for details.
  ;;
  ;;The returned  pattern for  SYNTAX-DISPATCH is a  sexp
  ;;with the following format:
  ;;
  ;; P in pattern:                    |  matches:
  ;;----------------------------------+---------------------------
  ;;  ()                              |  empty list
  ;;  _                               |  anything (no binding created)
  ;;  any                             |  anything
  ;;  (p1 . p2)                       |  pair
  ;;  #(free-id <key>)                |  <key> with free-identifier=?
  ;;  each-any                        |  any proper list
  ;;  #(each p)                       |  (p*)
  ;;  #(each+ p1 (p2_1 ... p2_n) p3)  |   (p1* (p2_n ... p2_1) . p3)
  ;;  #(vector p)                     |  #(x ...) if p matches (x ...)
  ;;  #(atom <object>)                |  <object> with "equal?"
  ;;
  (define (%convert* pattern* ellipsis-nesting-level pvars.levels)
    (if (null? pattern*)
	(values '() pvars.levels)
      (receive (y pvars.levels)
	  (%convert* (cdr pattern*) ellipsis-nesting-level pvars.levels)
	(receive (x pvars.levels)
	    (%convert (car pattern*) ellipsis-nesting-level pvars.levels)
	  (values (cons x y) pvars.levels)))))

  (define (%convert p ellipsis-nesting-level pvars.levels)
    (syntax-match p ()
      (?id
       (identifier? ?id)
       (cond ((bound-id-member? ?id literals)
	      (values `#(free-id ,?id) pvars.levels))
	     ((free-id=? ?id (scheme-stx '_))
	      (values '_ pvars.levels))
	     (else
	      ;;It is a pattern variable.
	      (values 'any (cons (cons ?id ellipsis-nesting-level)
				 pvars.levels)))))

      ((p dots)
       (ellipsis? dots)
       (receive (p pvars.levels)
	   (%convert p (+ ellipsis-nesting-level 1) pvars.levels)
	 (values (if (eq? p 'any)
		     'each-any
		   `#(each ,p))
		 pvars.levels)))

      ((x dots ys ... . z)
       (ellipsis? dots)
       (receive (z pvars.levels)
	   (%convert z ellipsis-nesting-level pvars.levels)
	 (receive (ys pvars.levels)
	     (%convert* ys ellipsis-nesting-level pvars.levels)
	   (receive (x pvars.levels)
	       (%convert x (+ ellipsis-nesting-level 1) pvars.levels)
	     (values `#(each+ ,x ,(reverse ys) ,z)
		     pvars.levels)))))

      ((x . y)
       (receive (y pvars.levels)
	   (%convert y ellipsis-nesting-level pvars.levels)
	 (receive (x pvars.levels)
	     (%convert x ellipsis-nesting-level pvars.levels)
	   (values (cons x y) pvars.levels))))

      (()
       (values '() pvars.levels))

      (#(?item* ...)
       (not (<stx>? ?item*))
       (receive (item* pvars.levels)
	   (%convert ?item* ellipsis-nesting-level pvars.levels)
	 (values `#(vector ,item*) pvars.levels)))

      (?datum
       (values `#(atom ,(syntax->datum ?datum))
	       pvars.levels))))

  (%convert pattern-stx 0 '()))


(module (ellipsis? underscore?)

  (define (ellipsis? x)
    (%free-identifier-and-symbol? x '...))

  (define (underscore? x)
    (%free-identifier-and-symbol? x '_))

  (define (%free-identifier-and-symbol? x sym)
    (and (identifier? x)
	 (free-id=? x (scheme-stx sym))))

  #| end of module |# )

(define (%verify-literals literals use-stx)
  ;;Verify that  identifiers selected as literals  are: identifiers, not
  ;;ellipsisi, not usderscore.  If successful: return true, else raise a
  ;;syntax violation
  ;;
  ;;LITERALS is  a list  of literals  from SYNTAX-CASE  or SYNTAX-RULES.
  ;;USE-STX  is a  syntax  object  representing the  full  macro use  of
  ;;SYNTAX-CASE or SYNTAX-RULES:  it is used here  for descriptive error
  ;;reporting.
  ;;
  (for-each (lambda (x)
	      (when (or (not (identifier? x))
			(ellipsis? x)
			(underscore? x))
		(syntax-violation #f "invalid literal" use-stx x)))
    literals)
  #t)


(define (ellipsis-map proc ls . ls*)
  ;;This function  is used at  expand time  to generate the  portions of
  ;;macro  output  form  generated  by  templates  with  ellipsis.   See
  ;;SYNTAX-TRANSFORMER for details.
  ;;
  ;;For a syntax template:
  ;;
  ;;   (syntax ((?a ?b ...) ...))
  ;;
  ;;this function is called in the core language as:
  ;;
  ;;   ((primitive ellipsis-map) (primitive cons) ?a ?b)
  ;;
  (define-constant __who__ '...)
  (unless (list? ls)
    (assertion-violation __who__ "not a list" ls))
  ;;LS* must be a list of  sublists, each sublist having the same length
  ;;of LS.
  (unless (null? ls*)
    (let ((n (length ls)))
      (for-each
          (lambda (x)
            (unless (list? x)
              (assertion-violation __who__ "not a list" x))
            (unless (= (length x) n)
              (assertion-violation __who__ "length mismatch" ls x)))
	ls*)))
  (apply map proc ls ls*))


;;;; pattern matching

(module (syntax-dispatch)
  ;;Perform  the actual  matching between  an input  symbolic expression
  ;;being a  (wrapped, unwrapped  or partially unwrapped)  syntax object
  ;;and a  pattern symbolic expression.   If the expression  matches the
  ;;pattern return null or  a list of syntax objects to  be bound to the
  ;;pattern variables; else return false.
  ;;
  ;;The order of  syntax objects in the returned list  is established by
  ;;the pattern and it is the  same order in which the pattern variables
  ;;appear in the alist returned by CONVERT-PATTERN.
  ;;
  ;;The pattern  for SYNTAX-DISPATCH is  a symbolic expression  with the
  ;;following format:
  ;;
  ;; P in pattern:                    |  matches:
  ;;----------------------------------+---------------------------
  ;;  ()                              |  empty list
  ;;  _                               |  anything (no binding created)
  ;;  any                             |  anything
  ;;  (p1 . p2)                       |  pair
  ;;  #(free-id <key>)                |  <key> with free-identifier=?
  ;;  each-any                        |  any proper list
  ;;  #(each p)                       |  (p*)
  ;;  #(each+ p1 (p2_1 ... p2_n) p3)  |   (p1* (p2_n ... p2_1) . p3)
  ;;  #(vector p)                     |  #(x ...) if p matches (x ...)
  ;;  #(atom <object>)                |  <object> with "equal?"
  ;;
  (define (syntax-dispatch expr pattern)
    (%match expr pattern
	    '() #;mark*
	    '() #;rib*
	    '() #;annotated-expr*
	    '() #;pvar*
	    ))

  (define (%match expr pattern mark* rib* annotated-expr* pvar*)
    (cond ((not pvar*)
	   ;;No match.
	   #f)
	  ((eq? pattern '_)
	   ;;Match anything, bind nothing.
	   pvar*)
	  ((eq? pattern 'any)
	   ;;Match anything, bind a pattern variable.
	   (cons (%make-syntax-object expr mark* rib* annotated-expr*)
		 pvar*))
	  ((<stx>? expr)
	   ;;Visit the syntax object.
	   (and (not (top-marked? mark*))
		(receive (mark*^ rib*^ annotated-expr*^)
		    (join-wraps mark* rib* annotated-expr* expr)
		  (%match (<stx>-expr expr) pattern mark*^ rib*^ annotated-expr*^ pvar*))))
	  ((annotation? expr)
	   ;;Visit the ANNOTATION struct.
	   (%match (annotation-expression expr) pattern mark* rib* annotated-expr* pvar*))
	  (else
	   (%match* expr pattern mark* rib* annotated-expr* pvar*))))

  (define (%match* expr pattern mark* rib* annotated-expr* pvar*)
    (cond
     ;;End of list pattern: match the end of a list expression.
     ;;
     ((null? pattern)
      (and (null? expr)
	   pvar*))

     ;;Match a pair expression.
     ;;
     ((pair? pattern)
      (and (pair? expr)
	   (%match (car expr) (car pattern) mark* rib* annotated-expr*
		   (%match (cdr expr) (cdr pattern) mark* rib* annotated-expr* pvar*))))

     ;;Match any  proper list  expression and  bind a  pattern variable.
     ;;This happens when the original pattern symbolic expression is:
     ;;
     ;;   (?var ...)
     ;;
     ;;everything  in the  proper  list  must be  bound  to the  pattern
     ;;variable ?VAR.
     ;;
     ((eq? pattern 'each-any)
      (let ((l (%match-each-any expr mark* rib* annotated-expr*)))
	(and l (cons l pvar*))))

     (else
      ;;Here we expect the PATTERN to be a vector of the format:
      ;;
      ;;   #(?symbol ?stuff ...)
      ;;
      ;;where ?SYMBOL is a symbol.
      ;;
      (case (vector-ref pattern 0)

	;;The pattern is:
	;;
	;;   #(each ?sub-pattern)
	;;
	;;the expression  matches if it  is a  list in which  every item
	;;matches ?SUB-PATTERN.
	;;
	((each)
	 (if (null? expr)
	     (%match-empty (vector-ref pattern 1) pvar*)
	   (let ((pvar** (%match-each expr (vector-ref pattern 1)
				      mark* rib* annotated-expr*)))
	     (and pvar**
		  (%combine pvar** pvar*)))))

	;;The pattern is:
	;;
	;;   #(free-id ?literal)
	;;
	;;the  expression  matches  if  it is  an  identifier  equal  to
	;;?LITERAL according to FREE-IDENTIFIER=?.
	;;
	((free-id)
	 (and (symbol? expr)
	      (top-marked? mark*)
	      (free-id=? (%make-syntax-object expr mark* rib* annotated-expr*)
			 (vector-ref pattern 1))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(scheme-id ?symbol)
	;;
	;;the  expression matches  if it  is an  identifier equal  to an
	;;identifier    having   ?SYMBOL    as    name   according    to
	;;FREE-IDENTIFIER=?.
	;;
	((scheme-id)
	 (and (symbol? expr)
	      (top-marked? mark*)
	      (free-id=? (%make-syntax-object expr mark* rib* annotated-expr*)
			 (scheme-stx (vector-ref pattern 1)))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(each+ p1 (p2_1 ... p2_n) p3)
	;;
	;;which originally was:
	;;
	;;   (p1 ?ellipsis p2_1 ... p2_n . p3)
	;;
	;;the expression matches if ...
	;;
	((each+)
	 (receive (xr* y-pat pvar*)
	     (%match-each+ expr
			   (vector-ref pattern 1)
			   (vector-ref pattern 2)
			   (vector-ref pattern 3)
			   mark* rib* annotated-expr* pvar*)
	   (and pvar*
		(null? y-pat)
		(if (null? xr*)
		    (%match-empty (vector-ref pattern 1) pvar*)
		  (%combine xr* pvar*)))))

	;;The pattern is:
	;;
	;;  #(atom ?object)
	;;
	;;the  expression matches  if it  is  a single  object equal  to
	;;?OBJECT according to EQUAL?.
	;;
	((atom)
	 (and (equal? (vector-ref pattern 1)
		      (strip expr mark*))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(vector ?sub-pattern)
	;;
	;;the expression matches if it is a vector whose items match the
	;;?SUB-PATTERN.
	;;
	((vector)
	 (and (vector? expr)
	      (%match (vector->list expr) (vector-ref pattern 1)
		      mark* rib* annotated-expr* pvar*)))

	(else
	 (assertion-violation 'syntax-dispatch "invalid pattern" pattern))))))

  (define (%match-each expr pattern mark* rib* annotated-expr*)
    ;;Recursive function.   The expression  matches if it  is a  list in
    ;;which  every item  matches  PATTERN.   Return null  or  a list  of
    ;;sublists, each sublist being a list of pattern variable values.
    ;;
    (cond ((pair? expr)
	   (let ((first (%match (car expr) pattern mark* rib* annotated-expr* '())))
	     (and first
		  (let ((rest (%match-each (cdr expr) pattern mark* rib* annotated-expr*)))
		    (and rest (cons first rest))))))
	  ((null? expr)
	   '())
	  ((<stx>? expr)
	   (and (not (top-marked? mark*))
		(receive (mark*^ rib*^ annotated-expr*^)
		    (join-wraps mark* rib* annotated-expr* expr)
		  (%match-each (<stx>-expr expr) pattern mark*^ rib*^ annotated-expr*^))))
	  ((annotation? expr)
	   (%match-each (annotation-expression expr) pattern mark* rib* annotated-expr*))
	  (else #f)))

  (define (%match-each+ e x-pat y-pat z-pat mark* rib* annotated-expr* pvar*)
    (let loop ((e e) (mark* mark*) (rib* rib*) (annotated-expr* annotated-expr*))
      (cond ((pair? e)
	     (receive (xr* y-pat pvar*)
		 (loop (cdr e) mark* rib* annotated-expr*)
	       (if pvar*
		   (if (null? y-pat)
		       (let ((xr (%match (car e) x-pat mark* rib* annotated-expr* '())))
			 (if xr
			     (values (cons xr xr*) y-pat pvar*)
			   (values #f #f #f)))
		     (values '()
			     (cdr y-pat)
			     (%match (car e) (car y-pat) mark* rib* annotated-expr* pvar*)))
		 (values #f #f #f))))
	    ((<stx>? e)
	     (if (top-marked? mark*)
		 (values '() y-pat (%match e z-pat mark* rib* annotated-expr* pvar*))
	       (receive (mark* rib* annotated-expr*)
		   (join-wraps mark* rib* annotated-expr* e)
		 (loop (<stx>-expr e) mark* rib* annotated-expr*))))
	    ((annotation? e)
	     (loop (annotation-expression e) mark* rib* annotated-expr*))
	    (else
	     (values '() y-pat (%match e z-pat mark* rib* annotated-expr* pvar*))))))

  (define (%match-each-any e mark* rib* annotated-expr*)
    (cond ((pair? e)
	   (let ((l (%match-each-any (cdr e) mark* rib* annotated-expr*)))
	     (and l (cons (%make-syntax-object (car e) mark* rib* annotated-expr*) l))))
	  ((null? e)
	   '())
	  ((<stx>? e)
	   (and (not (top-marked? mark*))
		(receive (mark* rib* annotated-expr*)
		    (join-wraps mark* rib* annotated-expr* e)
		  (%match-each-any (<stx>-expr e) mark* rib* annotated-expr*))))
	  ((annotation? e)
	   (%match-each-any (annotation-expression e) mark* rib* annotated-expr*))
	  (else #f)))

  (define (%match-empty p pvar*)
    (cond ((null? p)
	   pvar*)
	  ((eq? p '_)
	   pvar*)
	  ((eq? p 'any)
	   (cons '() pvar*))
	  ((pair? p)
	   (%match-empty (car p) (%match-empty (cdr p) pvar*)))
	  ((eq? p 'each-any)
	   (cons '() pvar*))
	  (else
	   (case (vector-ref p 0)
	     ((each)
	      (%match-empty (vector-ref p 1) pvar*))
	     ((each+)
	      (%match-empty (vector-ref p 1)
			    (%match-empty (reverse (vector-ref p 2))
					  (%match-empty (vector-ref p 3) pvar*))))
	     ((free-id atom)
	      pvar*)
	     ((scheme-id atom)
	      pvar*)
	     ((vector)
	      (%match-empty (vector-ref p 1) pvar*))
	     (else
	      (assertion-violation 'syntax-dispatch "invalid pattern" p))))))

  (define (%make-syntax-object stx mark* rib* annotated-expr*)
    (if (and (null? mark*)
	     (null? rib*)
	     (null? annotated-expr*))
	stx
      (mkstx stx mark* rib* annotated-expr*)))

  (define (%combine pvar** pvar*)
    (if (null? (car pvar**))
	pvar*
      (cons (map car pvar**)
	    (%combine (map cdr pvar**) pvar*))))

  #| end of module: SYNTAX-DISPATCH |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
