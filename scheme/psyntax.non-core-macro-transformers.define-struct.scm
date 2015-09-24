;;;Copyright (c) 2010-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module (define-struct-macro)
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


(define (define-struct-macro input-form.stx)
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
    ))


(define (%build-output-form input-form.stx type.id maker.id predicate.id field*.id uid)
  (unless (all-identifiers? field*.id)
    (syntax-violation __module_who__
      "expected list of identifiers as fields speciication"
      input-form.stx field*.id))
  (let* ((string->id     (lambda (str)
			   (datum->syntax type.id (string->symbol str))))
	 (type.sym       (identifier->symbol type.id))
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

    (define unsafe-accessor*.id
      (map (lambda (x)
	     (string->id (string-append "$" type.str "-" x)))
	field*.str))

    (define unsafe-mutator*.id
      (map (lambda (x)
	     (string->id (string-append "$set-" type.str "-" x "!")))
	field*.str))

    (define accessor-sexp*
      (map (lambda (accessor.id unsafe-accessor.id)
	     (let ((stru.sym (gensym "stru")))
	       `(internal-define (unsafe) (,accessor.id ,stru.sym)
		  (,unsafe-accessor.id ,stru.sym))))
	accessor*.id unsafe-accessor*.id))

    (define mutator-sexp*
      ;;Safe record fields mutators.
      ;;
      (map (lambda (mutator.id unsafe-mutator.id)
	     (let ((stru.sym (gensym "stru"))
		   (val.sym  (gensym "val")))
	       `(internal-define (unsafe) (,mutator.id ,stru.sym ,val.sym)
		  (,unsafe-mutator.id ,stru.sym ,val.sym))))
	mutator*.id unsafe-mutator*.id))

    (define unsafe-accessor-sexp*
      (map (lambda (unsafe-accessor.id field.idx)
	     (let ((stru.sym (gensym "stru")))
	       `(define-syntax-rule (,unsafe-accessor.id ,stru.sym)
		  ($struct-ref ,stru.sym ,field.idx))))
	unsafe-accessor*.id field*.idx))

    (define unsafe-mutator-sexp*
      (map (lambda (unsafe-mutator.id field.idx)
	     (let ((stru.sym (gensym "stru"))
		   (val.sym  (gensym "val")))
	       `(define-syntax-rule (,unsafe-mutator.id ,stru.sym ,val.sym)
		  ($struct-set! ,stru.sym ,field.idx ,val.sym))))
	unsafe-mutator*.id field*.idx))

    (bless
     `(module (,type.id
	       ,constructor.id ,predicate.id
	       ,@accessor*.id ,@unsafe-accessor*.id
	       ,@mutator*.id  ,@unsafe-mutator*.id)
	(define ((brace ,predicate.id ,(boolean-tag-id)) obj)
	  ($struct/rtd? obj ',std))
	;;By putting  this form  here we  are sure  that PREDICATE.ID  is already
	;;bound when the "tag-type-spec" is built.
	(define-syntax ,type.id
	  (make-syntactic-binding-descriptor/struct-type-name ',std))
	(define (,constructor.id . ,field*.id)
	  (receive-and-return (S)
	      ($struct ',std ,@field*.id)
	    (when ($std-destructor ',std)
	      ($struct-guardian S))))
	,@unsafe-accessor-sexp*
	,@unsafe-mutator-sexp*
	,@accessor-sexp*
	,@mutator-sexp*))))


;;;; done

#| end of module: DEFINE-STRUCT-MACRO |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; fill-column: 85
;; End:
