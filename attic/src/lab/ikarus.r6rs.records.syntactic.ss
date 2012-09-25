


;;; (define-record-type <namespec> <clause> ...)
;;; <namespec> ::= (<record-name> <constructor-name> <predicate-name>)
;;;              | <record-name>
;;; <clause> ::= (fields <fieldspec> ...)
;;;            | (protocol <expr>)
;;;            | (parent <parent-name>)
;;;            | (sealed <bool>)  ; defaults to #f
;;;            | (opaque <bool>)  ; defaults to #f
;;;            | (nongenerative <uid>) ; use uid
;;;            | (nongenerative) ; compile-time generative
;;;
;;; <fieldspec> ::= (immutable <fieldname> <accessorname>)
;;;               | (mutable <fieldname> <accessorname> <mutattorname>)
;;;               | (immutable <fieldname>)
;;;               | (mutable <fieldname>)
;;;               | <fieldname>  ; defaults to immutable
;;;
;;; (record-type-descriptor <record-name>) => rtd
;;; (record-constructor-descriptor <record-name>) => rcd


(library (ikarus r6rs records syntactic)
  (export ---)
  (import ---)
  (define-syntax define-record-type
    (lambda (x)
      (define (id ctxt . str*)
        (datum->syntax ctxt 
          (string->symbol 
            (apply string-append 
              (map (lambda (x) 
                     (cond
                       [(symbol? x) (symbol->string x)]
                       [(string? x) x]
                       [else (error 'define-record-type "BUG")]))
                   str*)))))
      (define (get-record-name spec)
        (syntax-case spec ()
          [(foo make-foo foo?) #'foo]
          [foo #'foo]))
      (define (get-record-constructor-name spec ctxt)
        (syntax-case spec ()
          [(foo make-foo foo?) #'make-foo]
          [foo (id ctxt "make-" (syntax->datum #'foo))]))
      (define (get-record-predicate-name spec ctxt)
        (syntax-case spec ()
          [(foo make-foo foo?) #'foo?]
          [foo (id ctxt (syntax->datum #'foo) "?")]))
      (define (get-clause id ls)
        (syntax-case ls ()
          [() #f]
          [((x . rest) . ls)
           (if (free-identifier=? id #'x) 
               #'(x . rest)
               (get-clause id #'ls))]))
      (define (foo-rtd-code ctxt name clause*) 
        (define (convert-field-spec* ls)
          (list #'quote
            (list->vector
              (map (lambda (x) 
                     (syntax-case x (mutable immutable)
                       [(mutable name . rest) #'(mutable name)]
                       [(immutable name . rest) #'(immutable name)]
                       [name #'(immutable name)]))
                   ls))))
        (with-syntax ([name name]
                      [parent-rtd-code 
                       (syntax-case (get-clause #'parent clause*) ()
                         [(_ name) #'(record-type-descriptor name)]
                         [_ #'#f])]
                      [uid-code 
                       (syntax-case (get-clause #'nongenerative clause*) ()
                         [(_) (datum->syntax ctxt (gensym))]
                         [(_ uid) #''uid]
                         [_ #'#f])]
                      [sealed?
                       (syntax-case (get-clause #'sealed? clause*) ()
                         [(_ #t) #'#t]
                         [_      #'#f])]
                      [opaque?
                       (syntax-case (get-clause #'opaque? clause*) ()
                         [(_ #t) #'#t]
                         [_      #'#f])]
                      [fields 
                       (syntax-case (get-clause #'fields clause*) ()
                         [(_ field-spec* ...) 
                          (convert-field-spec* #'(field-spec* ...))]
                         [_ #''#()])])
          #'(make-record-type-descriptor 'name
               parent-rtd-code 
               uid-code sealed? opaque? fields)))
      (define (foo-rcd-code clause*) 
        (with-syntax ([parent-rcd-code 
                       (syntax-case (get-clause #'parent clause*) ()
                         [(_ name) #'(record-constructor-descriptor name)]
                         [_ #'#f])])
          #'(make-record-constructor-descriptor foo-rtd
               parent-rcd-code protocol)))
      (define (get-protocol-code clause*)
        (syntax-case (get-clause #'protocol clause*) ()
          [(_ expr) #'expr]
          [_        #'#f]))
      (define (do-define-record ctxt namespec clause*)
        (let ([foo (get-record-name namespec)])
          (with-syntax ([foo foo]
                        [make-foo (get-record-constructor-name namespec ctxt)] 
                        [foo? (get-record-predicate-name namespec ctxt)]
                        [foo-rtd-code (foo-rtd-code ctxt name clause*)]
                        [protocol-code (get-protocol-code clause*)])
            #'(begin
                (define foo-rtd foo-rtd-code)
                (define protocol protocol-code)
                (define foo-rcd foo-rcd-code)
                (define-syntax foo (list '$rtd #'foo-rtd #'foo-rcd))
                (define foo? (record-predicate foo-rtd))
                (define make-foo (record-constructor foo-rcd))
                (define foo-x* (record-accessor foo-rtd idx*))
                ...
                (define set-foo-x!* (record-mutator foo-rtd mutable-idx*))
                ...))))
      (syntax-case x ()
        [(ctxt namespec clause* ...)
         (do-define-record #'ctxt #'namespec #'(clause* ...))])))
        

)





  (define-record foo (bar baz)) 
==
  (define-record-type foo 
    (fields bar baz)
    (nongenerative))

  (define-record-type foo (fields x y) (nongenerative))
==
  (begin
    (define-syntax foo `($rtd <foo-rtd> <foo-rcd>))
       ;;; <foo-rcd> = #[rcd <foo-rtd> #f #f]
       ;;; <foo-rtd> = #[rtd foo <gensym> #(x y) ---]
    (define (make-foo x y) 
      ($record '<foo-rtd> x y))
    (define (foo? x)
      ($record/rtd? x '<foo-rtd>))
    (define (foo-x x)
      (if ($record/rtd? x '<foo-rtd>) ($record-ref x 0) (error ---)))
    (define (foo-y x)
      (if ($record/rtd? x '<foo-rtd>) ($record-ref x 1) (error ---))))

  (record-type-descriptor foo) 
== 
  '<foo-rtd>

  (record-constructor-descriptor foo) 
== 
  '<foo-rcd>

  (record-constructor '<foo-rcd>)
=>
  (default-rtd-constructor '<foo-rtd>)
=>
  (lambda (x y) 
    ($record '<foo-rtd> x y))



  (define-record-type foo (fields x y) (generative))
==
  (begin
    (define foo-rtd (make-rtd --- ---))
    (define foo-rcd (make-rcd foo-rtd #f #f))
    (define-syntax foo `($rtd #'foo-rtd #'foo-rcd))
    (define (make-foo x y) 
      ($record foo-rtd x y))
    (define (foo? x)
      ($record/rtd? x foo-rtd))
    (define (foo-x x)
      (if ($record/rtd? x foo-rtd) ($record-ref x 0) (error ---)))
    (define (foo-y x)
      (if ($record/rtd? x foo-rtd) ($record-ref x 1) (error ---))))


  (define-record-type foo 
    (fields x y) 
    (parent pfoo) ;;; pfoo = `($rtd <pfoo-rtd> <pfoo-rcd>)
    (nongenerative))
==
  (begin
    (define-syntax foo `($rtd <foo-rtd> <foo-rcd>))
       ;;; <pfoo-rcd> = #[rcd <foo-rtd> #f #f]
       ;;; <foo-rcd> = #[rcd <foo-rtd> #f #f]
       ;;; <foo-rtd> = #[rtd foo <gensym> #(x y) ---]
    (define (make-foo x y) 
      ($record '<foo-rtd> x y))
    (define (foo? x)
      ($record/rtd? x '<foo-rtd>))
    (define (foo-x x)
      (if ($record/rtd? x '<foo-rtd>) ($record-ref x 2) (error ---)))
    (define (foo-y x)
      (if ($record/rtd? x '<foo-rtd>) ($record-ref x 3) (error ---))))




(define-record-type bar 
   (fields c)
   (parent foo)
   (protocol 
     (lambda (p) 
       (lambda (a b c) 
         ((p a b) c))))
   (sealed #f)
   (opaque #t)
==
(begin
  (define protocol-0
    (lambda (p)
      (lambda (a b c) 
        ((p a b) c))))
  (define bars-rtd '<some-rtd>)
  (define-syntax bar `($rtd <some rtd> <some rcd>))




