;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
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


(module ()
  ;;Transformer  function  used to  expand  Vicare's  DEFINE-STRUCT macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (define-module-who define-struct)


;;;; helpers

(define (enumerate i ls)
  (if (pair? ls)
      (cons i (enumerate (fxadd1 i) (cdr ls)))
    '()))

(define (%make-alist-from-ids field-name*.sym operator*.id)
  ;;We want  to return a  symbolic expression representing the  following expand-time
  ;;expression:
  ;;
  ;;   (list (cons (quote ?field-sym0) (syntax ?operator0))
  ;;         (cons (quote ?field-sym)  (syntax ?operator))
  ;;         ...)
  ;;
  ;;which evaluates  to an  aslist whose keys  are field names  and whose  values are
  ;;syntactic identifiers bound to accessors or mutators.
  ;;
  (cons 'list (map (lambda (key.sym operator.id)
		     (list 'cons `(quote ,key.sym) `(syntax ,operator.id)))
		field-name*.sym operator*.id)))

(define (%parse-fields input-form.stx field*.stx)
  (define (synner message subform)
    (syntax-violation __module_who__ message input-form.stx subform))
  (if (pair? field*.stx)
      (syntax-match (car field*.stx) (brace)
	((brace ?name ?type)
	 (begin
	   (unless (identifier? ?name)
	     (synner "expected identifier as field name" ?name))
	   (receive (name* type*)
	       (%parse-fields input-form.stx (cdr field*.stx))
	     (values (cons ?name name*)
		     (cons ?type type*)))))

	(?name
	 (identifier? ?name)
	 (receive (name* type*)
	     (%parse-fields input-form.stx (cdr field*.stx))
	   (values (cons ?name name*)
		   (cons (<top>-type-id) type*))))

	(_
	 (synner "invalid syntax in struct field definition" (car field*.stx))))
    (values '() '())))


(define-macro-transformer (define-struct input-form.stx)
  (syntax-match input-form.stx (nongenerative)
    ((_ (?name ?maker ?predicate) (?field* ...))
     (%build-output-form input-form.stx ?name ?maker ?predicate ?field* #f))

    ((_ ?name (?field* ...))
     (%build-output-form input-form.stx ?name #f     #f         ?field* #f))

    ((_ (?name ?maker ?predicate) (?field* ...) (nongenerative ?uid))
     (identifier? ?uid)
     (%build-output-form input-form.stx ?name ?maker ?predicate ?field* ?uid))

    ((_ ?name (?field* ...) (nongenerative ?uid))
     (identifier? ?uid)
     (%build-output-form input-form.stx ?name #f     #f         ?field* ?uid))

    (_
     (__synner__ "invalid syntax in macro use"))))


(define (%build-output-form input-form.stx type.id maker.id predicate.id field*.stx uid)
  (define-values (field*.id field-type*.ann)
    (%parse-fields input-form.stx field*.stx))
  (define (string->id str)
    (datum->syntax type.id (string->symbol str)))
  (define (private-string->id str)
    (datum->syntax type.id (gensym str)))
  (unless (all-identifiers? field*.id)
    (syntax-violation __module_who__
      "expected list of identifiers as fields speciication"
      input-form.stx field*.id))
  (let* ((type.sym       (identifier->symbol type.id))
	 (type.str       (symbol->string type.sym))
	 (field*.sym     (map syntax->datum  field*.id))
	 (field*.str     (map symbol->string field*.sym))
	 (uid            (if uid
			     (identifier->symbol uid)
			   (gensym type.str)))
	 (std            (datum->syntax type.id (make-struct-type type.str field*.sym uid)))
	 (constructor.id (or maker.id     (string->id (string-append "make-" type.str))))
	 (predicate.id   (or predicate.id (string->id (string-append type.str "?"))))
	 (field*.idx     (enumerate 0 field*.id)))

    (define the-constructor.id
      (string->id "the-constructor"))

    (define accessor*.id
      (map (lambda (x)
	     (string->id (string-append type.str "-" x)))
	field*.str))

    (define mutator*.id
      (map (lambda (x)
	     (string->id (string-append "set-" type.str "-" x "!")))
	field*.str))

    (define method*.id
      (map (lambda (x)
	     (private-string->id (string-append "on-" type.str "-" x)))
	field*.str))

    (define unsafe-accessor*.id
      (map (lambda (x)
	     (string->id (string-append "$" type.str "-" x)))
	field*.str))

    (define unsafe-mutator*.id
      (map (lambda (x)
	     (string->id (string-append "$set-" type.str "-" x "!")))
	field*.str))

    (define constructor-arg*.id
      (map make-syntactic-identifier-for-temporary-variable field*.str))

    (define constructor-arg*.spec
      (map (lambda (arg.id field-type.ann)
	     `(brace ,arg.id ,field-type.ann))
	constructor-arg*.id field-type*.ann))

;;; --------------------------------------------------------------------

    (define accessor-sexp*
      (map (lambda (accessor.id unsafe-accessor.id field-type.ann)
	     (let ((stru.id	(make-syntactic-identifier-for-temporary-variable "stru")))
	       `(define/checked ({,accessor.id ,field-type.ann} {,stru.id ,type.id})
		  (,unsafe-accessor.id ,stru.id))))
	accessor*.id unsafe-accessor*.id field-type*.ann))

    (define mutator-sexp*
      (map (lambda (mutator.id unsafe-mutator.id field-type.ann)
	     (let ((stru.id	(make-syntactic-identifier-for-temporary-variable "stru"))
		   (val.id	(make-syntactic-identifier-for-temporary-variable "val")))
	       `(define/checked ({,mutator.id} {,stru.id ,type.id} {,val.id ,field-type.ann})
		  (,unsafe-mutator.id ,stru.id ,val.id))))
	mutator*.id unsafe-mutator*.id field-type*.ann))

    (define method-sexp*
      (map (lambda (method.id unsafe-accessor.id unsafe-mutator.id field-type.ann)
    	     (let ((stru.id	(make-syntactic-identifier-for-temporary-variable "stru"))
    		   (val.id	(make-syntactic-identifier-for-temporary-variable "val")))
	       ;;NOTE Structs are  used in the boot image, for  example, to implement
	       ;;keyword objects.  We may be tempted to use DEFINE/OVERLOAD here, but
	       ;;it would introduce a dependency from the overloaded functions' code.
	       ;;If we use CASE-DEFINE/CHECKED there is no dependency.  (Marco Maggi;
	       ;;Sat Aug 27, 2016)
    	       `(case-define/checked ,method.id
		  (({_ ,field-type.ann} {,stru.id ,type.id})
		   (,unsafe-accessor.id ,stru.id))
		  (({_}                 {,stru.id ,type.id} {,val.id ,field-type.ann})
		   (,unsafe-mutator.id ,stru.id ,val.id)))))
    	method*.id unsafe-accessor*.id unsafe-mutator*.id field-type*.ann))

;;; --------------------------------------------------------------------

    (define unsafe-accessor-sexp*
      (map (lambda (unsafe-accessor.id field.idx field-type.ann)
	     (let ((stru.id	(make-syntactic-identifier-for-temporary-variable "stru")))
	       `(define-syntax ,unsafe-accessor.id
		  (identifier-syntax
		   (lambda/typed ({_ ,field-type.ann} {,stru.id <struct>})
		     ($struct-ref ,stru.id ,field.idx))))))
	unsafe-accessor*.id field*.idx field-type*.ann))

    (define unsafe-mutator-sexp*
      (map (lambda (unsafe-mutator.id field.idx field-type.ann)
	     (let ((stru.id	(make-syntactic-identifier-for-temporary-variable "stru"))
		   (val.id	(make-syntactic-identifier-for-temporary-variable "val")))
	       `(define-syntax ,unsafe-mutator.id
		  (identifier-syntax
		   (lambda/typed ({_} {,stru.id <struct>} {,val.id ,field-type.ann})
		     ($struct-set! ,stru.id ,field.idx ,val.id))))))
	unsafe-mutator*.id field*.idx field-type*.ann))

    (define methods-table.sexp
      (%make-alist-from-ids field*.sym method*.id))

;;; --------------------------------------------------------------------

    (bless
     ;;It would seem a good idea to wrap this form into a MODULE that exports:
     ;;
     ;;   ,type.id
     ;;   ,constructor.id ,predicate.id
     ;;   ,@accessor*.id ,@unsafe-accessor*.id
     ;;   ,@mutator*.id  ,@unsafe-mutator*.id
     ;;
     ;;but doing  so introduces a  lexical contour that makes  it impossible to  do a
     ;;forward definition of the struct-type.   Forward definitions are useful, so we
     ;;use a simple BEGIN and use gensysm to make some syntactic bindings "private".
     `(begin
	(define/checked ({,predicate.id <boolean>} obj)
	  ($struct/rtd? obj ',std))
	(define-type ,type.id
	  (constructor
	      (make-struct-type-spec (syntax ,type.id) ',std
				     (syntax ,constructor.id) (syntax ,predicate.id)
				     ,methods-table.sexp)))
	(define/checked ({,constructor.id ,type.id} . ,constructor-arg*.spec)
	  (receive-and-return (S)
	      ($struct ',std . ,constructor-arg*.id)
	    (when ($std-destructor ',std)
	      ($struct-guardian S))))
	,@unsafe-accessor-sexp*
	,@unsafe-mutator-sexp*
	,@accessor-sexp*
	,@mutator-sexp*
	,@method-sexp*
	#| end of begin |# ))
    ))


;;;; done

#| end of module: DEFINE-STRUCT-MACRO |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
