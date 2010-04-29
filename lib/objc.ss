;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2008,2009  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



(library (objc)
  (export
    define-framework
    define-class
    define-object
    string->char*
    get-selector
    get-class-list
    get-class
    class-methods
    class-name
    method-name
    create-class
    class-instance-size
    class-parent
    class-ivars
    ivar-name
    ivar-type
    ivar-offset
    load-shared-object
    class-ivar
    class-add-instance-method
    class-add-class-method
    $)
  (import
    (ikarus)
    (ikarus system $foreign)
    (except (ypsilon-compat) format))

(define ptrsize 4)

(define objc
  (load-shared-object "libobjc.A.dylib"))
(define Cocoa
  (load-shared-object "/System/Library/Frameworks/Cocoa.framework/Cocoa"))

(define-syntax define-function
  (syntax-rules ()
    ((_ ret name args)
     (define name
       (c-function objc "Objective C Binding" ret __stdcall name args)))))

(define-function int objc_getClassList (void* int))
(define-function void objc_addClass (void*))
(define-function void* objc_getClass (char*))
(define-function void* sel_registerName (char*))
(define-function void* sel_getUid (char*))
(define-function void* class_getInstanceMethod (void* void*))
(define-function void* class_getClassMethod (void* void*))
(define-function void* class_nextMethodList (void* void*))
(define-function void* class_getInstanceVariable (void* void*))
(define-function void class_addMethods (void* void*))

(define-record-type class (fields ptr))
(define-record-type object (fields ptr))
(define-record-type lazy-object (fields ptr))
(define-record-type selector (fields ptr))
(define-record-type method (fields ptr))
(define-record-type ivar (fields ptr))

(define (pointer-ref addr offset)
  (assert (pointer? addr))
  (pointer-ref-c-pointer addr offset))

(define (offset? x) (or (fixnum? x) (bignum? x)))

(define (pointer-set addr offset val)
  (define who 'pointer-set)
  (check who pointer? addr)
  (check who pointer? val)
  (check who offset? offset)
  (pointer-set-c-pointer! addr offset val))

(define (char*len x)
  (let f ([i 0])
    (cond
      [(zero? (pointer-ref-c-unsigned-char x i)) i]
      [else (f (+ i 1))])))

(define (char*->bv x)
  (let ([n (char*len x)])
    (let ([bv (make-bytevector n)])
      (let f ([i 0])
        (cond
          [(= i n) bv]
          [else
           (bytevector-u8-set! bv i (pointer-ref-c-unsigned-char x i))
           (f (+ i 1))])))))

(define (bv->char* x)
  (let ([n (bytevector-length x)])
    (let ([p (malloc (+ n 1))])
      (pointer-set-c-char! p n 0)
      (let f ([i 0])
        (cond
          [(= i n) p]
          [else
           (pointer-set-c-char! p i (bytevector-s8-ref x i))
           (f (+ i 1))])))))

(define (bv->u8* x)
  (let ([n (bytevector-length x)])
    (if (= n 0)
        (integer->pointer 0)
        (let ([p (malloc n)])
          (let f ([i 0])
            (cond
              [(= i n) p]
              [else
               (pointer-set-c-char! p i (bytevector-s8-ref x i))
               (f (+ i 1))]))))))

(define (char*->string x)
  (utf8->string (char*->bv x)))

(define (string->char* x)
  (let ([bv (string->utf8 x)])
    (bv->char* bv)))

(define-syntax check
  (syntax-rules ()
    [(_ who pred expr)
     (let ([t expr])
       (unless (pred t)
         (die who (format "not a ~a" 'pred) t)))]))

(define (class-name x)
  (check 'class-name class? x)
  (char*->string (pointer-ref (class-ptr x) (* ptrsize 2))))

(define (class-parent x)
  (check 'class-parent class? x)
  (let ([super (pointer-ref (class-ptr x) (* ptrsize 1))])
    (if (nil? super)
        #f
        (make-class super))))

(define (class-metaclass x)
  (check 'class-metaclass class? x)
  (let ([super (pointer-ref (class-ptr x) (* ptrsize 0))])
    (if (nil? super)
        #f
        (make-class super))))

(define (get-root-class x)
  (let ([super (class-parent x)])
    (if super
        (get-root-class super)
        x)))

; FIXME: no hardocding
(define CLS_CLASS #x01)
(define CLS_META  #x02)

(define objc-class-isa-offset           (* 0 ptrsize))
(define objc-class-superclass-offset    (* 1 ptrsize))
(define objc-class-name-offset          (* 2 ptrsize))
(define objc-class-version-offset       (* 3 ptrsize))
(define objc-class-info-offset          (* 4 ptrsize))
(define objc-class-instance-size-offset (* 5 ptrsize))
(define objc-class-ivars-offset         (* 6 ptrsize))
(define objc-class-methodlists-offset   (* 7 ptrsize))
(define objc-class-cache-offset         (* 8 ptrsize))
(define objc-class-protocols-offset     (* 9 ptrsize))
(define objc-class-struct-size          (* 10 ptrsize))

(define objc-methodlist-obsolete-offset (* 0 ptrsize))
(define objc-methodlist-count-offset    (* 1 ptrsize))
(define objc-methodlist-methods-offset  (* 2 ptrsize))

(define objc-method-sel-offset          (* 0 ptrsize))
(define objc-method-types-offset        (* 1 ptrsize))
(define objc-method-imp-offset          (* 2 ptrsize))
(define objc-method-size                (* 3 ptrsize))

(define objc-ivarlist-count-offset      (* 0 ptrsize))
(define objc-ivarlist-ivars-offset      (* 1 ptrsize))
(define objc-ivar-name-offset           (* 0 ptrsize))
(define objc-ivar-type-offset           (* 1 ptrsize))
(define objc-ivar-offset-offset         (* 2 ptrsize))
(define objc-ivar-size                  (* 3 ptrsize))

(define (class-instance-size x)
  (check 'class-instance-size class? x)
  (pointer-ref-c-signed-long (class-ptr x) objc-class-instance-size-offset))

(define (ivar-name x)
  (check 'ivar-name ivar? x)
  (char*->string (pointer-ref (ivar-ptr x) 0)))

(define (ivar-type x)
  (check 'ivar-type ivar? x)
  (char*->string (pointer-ref (ivar-ptr x) ptrsize)))

(define (ivar-offset x)
  (check 'ivar-offset ivar? x)
  (pointer-ref-c-signed-int (ivar-ptr x) (* 2 ptrsize)))

(define (class-ivars x)
  (check 'class-ivars class? x)
  (let ([p (pointer-ref (class-ptr x) objc-class-ivars-offset)])
    (if (nil? p)
        '()
        (let ([n (pointer-ref-c-signed-long p 0)])
          (let f ([i 0] [off objc-ivarlist-ivars-offset])
            (if (= i n)
                '()
                (let ([iv (integer->pointer (+ off (pointer->integer p)))])
                  (cons (make-ivar iv)
                    (f (+ i 1) (+ off objc-ivar-size))))))))))

(define (create-class name super-class ivars intern?)
  (define who 'create-class)
  (check who string? name)
  (check who list? ivars)
  (check who class? super-class)
  (when (get-class name)
    (error who "class already exists" name))
  (let-values ([(ivars-ptr instance-size)
                (make-ivar-ptr ivars super-class)])
    (let* ([root-class (get-root-class super-class)]
           [class (malloc objc-class-struct-size)]
           [meta  (malloc objc-class-struct-size)])
      ;;; init meta class
      (pointer-set-c-long! meta objc-class-info-offset CLS_META)
      (pointer-set meta objc-class-name-offset (string->char* name))
      (pointer-set meta objc-class-methodlists-offset
        (malloc objc-methodlist-methods-offset))
      (pointer-set meta objc-class-superclass-offset
        (pointer-ref (class-ptr super-class) objc-class-isa-offset))
      (pointer-set meta objc-class-isa-offset
        (pointer-ref (class-ptr root-class) objc-class-isa-offset))
      ;;; init class
      (pointer-set-c-long! class objc-class-info-offset CLS_CLASS)
      (pointer-set class objc-class-name-offset (string->char* name))
      (pointer-set class objc-class-methodlists-offset
        (malloc objc-methodlist-methods-offset))
      (pointer-set class objc-class-superclass-offset
        (class-ptr super-class))
      (pointer-set class objc-class-ivars-offset ivars-ptr)
      (pointer-set-c-long! class objc-class-instance-size-offset instance-size)
      ;;; wire up
      (pointer-set class objc-class-isa-offset meta)
      (when intern? (objc_addClass class))
      (make-class class))))

(define (class-add-method who class sel rtype argtypes proc)
  (check who class? class)
  (check who symbol? sel)
  (check who procedure? proc)
  (let ([type (make-objc-type (cons rtype argtypes))])
    (let ([callback
           (make-c-callback
             (objc-type->ikarus-type rtype)
             (map objc-type->ikarus-type argtypes))])
      (let ([imp (callback
                   (lambda args
                     (convert-outgoing rtype
                       (apply proc (map convert-incoming argtypes args)))))])
        (let ([p (malloc (+ objc-methodlist-methods-offset
                            objc-method-size))])
          (pointer-set-c-int! p objc-methodlist-count-offset 1)
          (pointer-set p
            (+ objc-methodlist-methods-offset objc-method-sel-offset)
            (selector-ptr
              (or (get-selector (symbol->string sel))
                  (begin
                    (free p)
                    (error who "invalid selector")))))
          (pointer-set p
            (+ objc-methodlist-methods-offset objc-method-types-offset)
            (string->char* type))
          (pointer-set p
            (+ objc-methodlist-methods-offset objc-method-imp-offset)
            imp)
          (class_addMethods (class-ptr class) p))))))

(define (class-add-instance-method class sel rtype argtypes proc)
  (define who 'class-add-instance-method)
  (class-add-method who class sel rtype argtypes proc))

(define (class-add-class-method class sel rtype argtypes proc)
  (define who 'class-add-instance-method)
  (check who class? class)
  (class-add-method who (class-metaclass class) sel rtype argtypes proc))

(define (method-types x)
  (check 'method-types method? x)
  (char*->string (pointer-ref (method-ptr x) (* ptrsize 1))))

(define (method-pointer x)
  (check 'method-pointer method? x)
  (pointer-ref (method-ptr x) (* ptrsize 2)))

(define (method-selector x)
  (check 'method-selector method? x)
  (make-selector (pointer-ref (method-ptr x) (* ptrsize 0))))

(define (method-name x)
  (check 'method-name method? x)
  (selector-name (method-selector x)))

(define CLS_METHOD_ARRAY #x100)

(define (class-is? x what)
  (define alist
    '([method-array      #x100]
      [no-method-array  #x4000]))
  (check 'class-info class? x)
  (let ([mask
          (cond
            [(assq what alist) => cadr]
            [else (error 'class-is? "invalid what" what)])])
    (= mask (bitwise-and mask (pointer-ref-c-signed-long (class-ptr x) (* ptrsize 4))))))

(define (class-methods x)
  (define (methods x)
    (let ([n (pointer-ref-c-signed-int x ptrsize)]
          [array (integer->pointer (+ (pointer->integer x) (* 2 ptrsize)))])
      (let f ([i 0])
        (if (= i n)
            '()
            (let ([m (make-method
                       (integer->pointer
                         (+ (pointer->integer array)
                            (* 3 ptrsize i))))])
              (cons m (f (+ i 1))))))))
  (check 'class-methods class? x)
  (when (class-is? x 'method-array)
    (error 'class-methods "BUG: not yet for method arrays"))
  (let ([iterator (malloc ptrsize)])
    (pointer-set-c-long! iterator 0 0)
    (let f ()
      (let ([methodlist (class_nextMethodList (class-ptr x) iterator)])
        (cond
          [(nil? methodlist)
           (free iterator)
           '()]
          [else
           (let ([ls (methods methodlist)])
             (append ls (f)))])))))

(define (get-class-list)
  (let ([n (objc_getClassList (integer->pointer 0) 0)])
    (if (= n 0)
        '()
        (let ([buffer (malloc (* ptrsize n))])
          (let ([n (objc_getClassList buffer n)])
            (let f ([i 0] [ac '()])
              (if (= i n)
                  (begin (free buffer) ac)
                  (f (+ i 1)
                     (cons
                       (make-class
                         (integer->pointer
                           (pointer-ref-c-signed-long buffer (* ptrsize i))))
                       ac)))))))))

(define (nil? x)
  (zero? (pointer->integer x)))

(define (get-class name)
  (check 'lookup-class string? name)
  (let ([v (objc_getClass name)])
    (cond
      [(nil? v) #f]
      [else (make-class v)])))

(define (get-selector name)
  (check 'lookup-selector string? name)
  (let ([v (sel_registerName name)])
    (cond
      [(nil? v) #f]
      [else (make-selector v)])))

(define (selector-name x)
  (check 'selector-name selector? x)
  (char*->string (selector-ptr x)))

(define (get-class-method class selector)
  (check 'get-class-method class? class)
  (check 'get-class-method selector? selector)
  (let ([v (class_getClassMethod
             (class-ptr class)
             (selector-ptr selector))])
    (cond
      [(nil? v) #f]
      [else (make-method v)])))

(define (get-instance-method x selector)
  (check 'get-instance-method object? x)
  (check 'get-instance-method selector? selector)
  (let ([class (pointer-ref (object-ptr x) 0)])
    (let ([v (class_getInstanceMethod
               class
               (selector-ptr selector))])
    (cond
      [(nil? v) #f]
      [else (make-method v)]))))

(define-syntax define-class
  (syntax-rules ()
    [(_ name)
     (define name
       (or (get-class (symbol->string 'name))
           (error 'define-class "undefined class" 'name)))]))

(define-syntax define-framework
  (lambda (x)
    (syntax-case x ()
      [(_ name) (identifier? #'name)
       (let ([str (symbol->string (syntax->datum #'name))])
         (with-syntax ([framework-name
                        (string-append str ".framework/" str)])
           #'(define name
               (load-shared-object framework-name))))])))

(define (load-object lib name)
  (let ([ptr
         (or (dlsym (library-pointer lib) (symbol->string name))
             (error 'load-object "cannot find symbol" name))])
    (make-lazy-object ptr)))

(define-syntax define-object
  (lambda (x)
    (syntax-case x ()
      [(_ name lib)
       #'(define name (load-object lib 'name))])))

(define (symbol->selector x)
  (or (get-selector (symbol->string x))
      (error 'symbol->selector "undefined selector" x)))

(define ctype-info
  ; [name     size]
  '([pointer     4]
    [sint        4]))

(define objc-type-info
  '([object   "@" pointer]
    [selector ":" pointer]
    [class    "#" pointer]
    [void     "v" #f]
    [int      "i" sint]))

(define (ivar-info x)
  (cond
    [(assq x objc-type-info) =>
     (lambda (p)
       (let ([name (car p)] [typestr (cadr p)] [ctype (caddr p)])
         (cond
           [(assq ctype ctype-info) =>
            (lambda (p)
              (let ([name (car p)] [size (cadr p)])
                (values typestr size)))]
           [else (error 'ivar-info "invalid ctype" ctype)])))]
    [else (error 'ivar-info "invalid type" x)]))

(define (class-ivar class ivar-name)
  (define who 'class-ivar)
  (check who class? class)
  (check who string? ivar-name)
  (let ([char* (string->char* ivar-name)])
    (let ([v (class_getInstanceVariable (class-ptr class) char*)])
      (free char*)
      (cond
        [(nil? v) #f]
        [else (make-ivar v)]))))

(define (make-ivar-ptr ivars super-class)
  (define (make-ivar-ptr ivars super-class)
    ;;; ivars = ([name . type] ...)
    (define who 'make-ivar-ptr)
    (define count (length ivars))
    (define p
      (malloc (+ objc-ivarlist-ivars-offset (* count objc-ivar-size))))
    (pointer-set-c-int! p objc-ivarlist-count-offset count)
    (let f ([ivars ivars]
            [poff objc-ivarlist-ivars-offset]
            [ivaroff (class-instance-size super-class)])
      (cond
        [(null? ivars) (values p ivaroff)]
        [else
         (let ([ivar (car ivars)])
           (let ([name (car ivar)])
             (let-values ([(ivar-type ivar-size) (ivar-info (cdr ivar))])
               (pointer-set p (+ poff objc-ivar-name-offset)
                 (string->char* (symbol->string name)))
               (pointer-set p (+ poff objc-ivar-type-offset)
                 (string->char* ivar-type))
               (pointer-set-c-int! p (+ poff objc-ivar-offset-offset) ivaroff)
               (f (cdr ivars)
                  (+ poff objc-ivar-size)
                  (+ ivaroff ivar-size)))))])))
  (if (null? ivars)
      (values (integer->pointer 0) (class-instance-size super-class))
      (make-ivar-ptr ivars super-class)))

(define (make-objc-type signature)
  (define (type-string x)
    (cond
      [(assq x objc-type-info) => cadr]
      [else (error 'make-objc-type "invalid type" x)]))
  (apply string-append (map type-string signature)))

(define (make-signature method-name str)
  (define who 'make-signature)
  (let ([n (string-length str)])
    (define (scan i c)
      (cond
        [(= i n) (error who "cannot find " c)]
        [(char=? c (string-ref str i)) (+ i 1)]
        [else (scan (+ i 1) c)]))
    (define (parse i)
      (cond
        [(= i n) (error who "unterminated string")]
        [else
         (let ([c (string-ref str i)])
           (case c
             [(#\@) (values 'object (+ i 1))]
             [(#\:) (values 'selector (+ i 1))]
             [(#\#) (values 'class (+ i 1))]
             [(#\v) (values 'void (+ i 1))]
             [(#\f) (values 'float (+ i 1))]
             [(#\i) (values 'int (+ i 1))]
             [(#\I) (values 'uint (+ i 1))]
             [(#\S) (values 'ushort (+ i 1))]
             [(#\c) (values 'char (+ i 1))]
             [(#\{) ;;; struct
              (let ([i (scan (+ i 1) #\=)])
                (let-values ([(i ls)
                              (let f ([i i])
                                (let-values ([(x i) (parse i)])
                                  (cond
                                    [(>= i n) (error who "runaway")]
                                    [(char=? (string-ref str i) #\})
                                     (values (+ i 1) (list x))]
                                    [else
                                     (let-values ([(i ls) (f i)])
                                       (values i (cons x ls)))])))])
                  (values (list->vector ls) i)))]
             [(#\*) (values 'char* (+ i 1))]
             [(#\^)
              (let-values ([(t i) (parse (+ i 1))])
                (values (cons 'pointer t) i))]
             [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
               #\r)
              (values 'skip (+ i 1))]
             [else (error who "invalid char" c str)]))]))
    (define (cons/skip x y)
      (if (eq? x 'skip) y (cons x y)))
    (let f ([i 0])
      (cond
        [(= i n) '()]
        [else
         (let-values ([(x i) (parse i)])
           (cons/skip x (f i)))]))))

(define (objc-type->ikarus-type x)
  (cond
    [(vector? x)
     (vector-map objc-type->ikarus-type x)]
    [(pair? x) 'pointer]
    [else
     (case x
       [(selector) 'pointer]
       [(object)   'pointer]
       [(class)    'pointer]
       [(void)     'void]
       [(float)    'float]
       [(uint)     'unsigned-int]
       [(int)      'signed-int]
       [(char)     'signed-char]
       [(char*)    'pointer]
       [else (error 'objc-type->ikarus-type "invalid type" x)])]))

(define (convert-incoming t x)
  (case t
    [(object)
     (if (nil? x) #f (make-object x))]
    [(class)
     (if (nil? x) #f (make-class x))]
    [(selector)
     (if (nil? x) #f (make-selector x))]
    [(char int)   x]
    [(void)   (void)]
    [else (error 'convert-incoming "invalid type" t)]))

(define (convert-outgoing t x)
  (cond
    [(vector? t)
     (cond
       [(vector? x)
        (unless (= (vector-length x) (vector-length t))
          (error 'convert-outgoing "length mismatch" x t))
        (vector-map convert-outgoing t x)]
       [else (error 'convert-output "not a vector" x)])]
    [(and (pair? t) (eq? (car t) 'pointer))
     (case (cdr t)
       [(ushort)
        (cond
          [(string? x)
           (bv->u8* (string->utf16 x 'little))]
          [else (error 'convert-output "cannot convert to ushort*" x)])]
       [else (error 'convert-output "dunno how to convert" t)])]
    [else
     (case t
       [(selector)
        (cond
          [(selector? x) (selector-ptr x)]
          [(not x)       (integer->pointer 0)]
          [else (error 'convert-output "not a selector" x)])]
       [(object)
        (cond
          [(object? x) (object-ptr x)]
          [(lazy-object? x)
           (pointer-ref (lazy-object-ptr x) 0)]
          [(class? x) (class-ptr x)]
          [(not x)    (integer->pointer 0)]
          [else (error 'convert-output "cannot convert to object" x)])]
       [(class)
        (cond
          [(class? x) (class-ptr x)]
          [else (error 'convert-output "cannot convert to class" x)])]
       [(float)
        (cond
          [(number? x) (inexact x)]
          [else (error 'convert-output "cannot convert to float" x)])]
       [(uint int char)
        (cond
          [(or (fixnum? x) (bignum? x)) x]
          [(boolean? x) (if x 1 0)]
          [else (error 'convert-output "cannot convert to int" x)])]
       [(char*)
        (cond
          [(string? x) (string->char* x)]
          [else (error 'convert-output "cannot convert to char*" x)])]
       [(void) (void)]
       [else (error 'convert-outgoing "invalid type" t)])]))

(define (call-with-sig sig mptr args)
  (let ([rtype (car sig)] [argtypes (cdr sig)])
    (unless (= (length args) (length argtypes))
      (error 'call-with-sig "incorrect number of args" args argtypes))
    (let ([ffi (make-c-callout
                 (objc-type->ikarus-type rtype)
                 (map objc-type->ikarus-type argtypes))])
      (let ([proc (ffi mptr)])
        (convert-incoming rtype
          (apply proc (map convert-outgoing argtypes args)))))))

(define (send-message x method-name . args)
  (let ([selector (symbol->selector method-name)])
    (let ([method
           (cond
             [(class? x) (get-class-method x selector)]
             [(object? x) (get-instance-method x selector)]
             [(lazy-object? x)
              (get-instance-method
                (make-object (pointer-ref (lazy-object-ptr x) 0))
                selector)]
             [else (error 'send-message "not an object" x)])])
      (unless method
        (error 'send-message "undefined method" method-name))
      (let ([sig (make-signature method-name (method-types method))]
            [mptr (method-pointer method)])
        (call-with-sig sig mptr (cons* x selector args))))))

(define-syntax $
  (lambda (x)
    (define (process-rest ls)
      (syntax-case ls ()
        [() (values "" '())]
        [(kwd val . rest) (identifier? #'kwd)
         (let-values ([(sel args) (process-rest #'rest)])
           (values
             (string-append
               (symbol->string (syntax->datum #'kwd))
               sel)
             (cons #'val args)))]))
    (define (process-args ls)
      (let-values ([(sel args) (process-rest ls)])
        (cons (datum->syntax #'here (string->symbol sel)) args)))
    (syntax-case x ()
      [(_ receiver kwd)
       (identifier? #'kwd)
       #'(send-message receiver 'kwd)]
      [(_ receiver kwd/arg* ...)
       (identifier? #'kwd)
       (with-syntax ([(sel-name arg* ...)
                      (process-args #'(kwd/arg* ...))])
         #'(send-message receiver 'sel-name arg* ...))])))

) ; library
