;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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


(library (ikarus pretty-print)
  (export pretty-print pretty-width)
  (import
    (rnrs hashtables)
    (only (ikarus writer) traverse traversal-helpers)
    (only (ikarus.pretty-formats) get-fmt)
    (except (ikarus) pretty-print pretty-width))
  (define (map1ltr f ls)
    ;;; ltr so that gensym counts get assigned properly
    (cond
      [(null? ls) '()]
      [else
       (let ([a (f (car ls))])
         (cons a (map1ltr f (cdr ls))))]))

  (define pretty-width
    (make-parameter 60
      (lambda (x)
        (unless (and (exact? x) (integer? x) (> x 0))
          (die 'pretty-width "invalid argument" x))
        x)))

  (define (pretty-indent) 1)
  (define-struct cbox (length boxes))
  (define-struct pbox (length ls last))
  (define-struct mbox (length str val))
  (define-struct vbox (length prefix ls))
  (define-struct fbox (length box* sep*))
  (define (box-length x)
    (cond
      [(string? x) (string-length x)]
      [(cbox? x)   (cbox-length x)]
      [(pbox? x)   (pbox-length x)]
      [(mbox? x)   (mbox-length x)]
      [(vbox? x)   (vbox-length x)]
      [(fbox? x)   (fbox-length x)]
      [else (die 'boxify "invalid box" x)]))
  (define (boxify x h)
    (define shared-idx 0)
    (define (conc . a*)
      (let ([n
             (let f ([a* a*] [len 0])
               (cond
                 [(null? a*) len]
                 [else
                  (f (cdr a*) (fx+ len (box-length (car a*))))]))])
        (make-cbox n a*)))
    (define (boxify-list ls)
      (define (sum-box* ls)
        (cond
          [(null? (cdr ls))
           (box-length (car ls))]
          [else
           (fx+ (box-length (car ls))
                (fxadd1 (sum-box* (cdr ls))))]))
      (define (gensep*-default ls)
        (cond
          [(null? (cdr ls)) '()]
          [else
           (cons (pretty-indent) (gensep*-default (cdr ls)))]))
      (define (tab-value x)
        (cond
          [(eq? x 'tab) (pretty-indent)]
          [(fixnum? x) x]
          [else #f]))
      (define (select-alt alt-fmt* ls)
        (define (good-match? fmt ls)
          (cond
            [(not (pair? fmt)) #t]
            [(eq? (car fmt) 'read-macro)
             (and (unshared-list? ls) (fx= (length ls) 2))]
            [else
             (let ([a (car fmt)] [fmt (cdr fmt)])
               (cond
                 [(or (eq? a 'tab) (fixnum? a))
                  (good-match? fmt ls)]
                 [(and (pair? fmt) (eq? (car fmt) '...))
                  (and (unshared-list? ls)
                       (andmap (lambda (x) (good-match? a x)) ls))]
                 [(and (pair? ls) (not (graphed? ls)))
                  (and (good-match? a (car ls))
                       (good-match? fmt (cdr ls)))]
                 [else #f]))]))
         (ormap (lambda (fmt) (and (good-match? fmt ls) fmt))
                alt-fmt*))
      (define (applicable-formats a alt-fmt*)
        (cond
          [(and (symbol? a) (get-fmt a)) =>
           (lambda (fmt)
             (cond
               [(and (pair? fmt) (eq? (car fmt) 'alt))
                (append alt-fmt* (cdr fmt))]
               [else
                (append alt-fmt* (list fmt))]))]
          [(null? alt-fmt*) #f]
          [else       alt-fmt*]))
      (define (return sep* box*)
        (let ([n (sum-box* box*)])
          (conc "(" (make-fbox n box* sep*) ")")))
      (define (boxify-list ls alt-fmt*)
        (let ([a (car ls)])
          (cond
            [(applicable-formats a alt-fmt*) =>
             (lambda (fmt*)
               (let ([fmt (select-alt fmt* ls)])
                 (module (fmt-dots? skip-fmt fmt-tab sub-fmt)
                   (define (parse-fmt x)
                     (define (parse-dots tab fmt x)
                       (cond
                         [(and (pair? x) (eq? (car x) '...))
                          (values tab fmt #t (cdr x))]
                         [else
                          (values tab fmt #f x)]))
                     (define (parse-tab tab x)
                       (cond
                         [(pair? x)
                          (parse-dots tab (car x) (cdr x))]
                         [else (values tab #f #f #f)]))
                     (cond
                       [(pair? x)
                        (let ([a0 (car x)])
                          (cond
                            [(eq? a0 'tab)
                             (parse-tab (pretty-indent) (cdr x))]
                            [(fixnum? a0)
                             (parse-tab a0 (cdr x))]
                            [else (parse-tab #f x)]))]
                       [else (values (pretty-indent) #f #f #f)]))
                   (define (fmt-dots? x)
                     (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                        dots))
                   (define (fmt-tab x)
                     (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                        tab))
                   (define (sub-fmt x)
                     (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                        subfmt))
                   (define (skip-fmt x)
                     (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                        fmt)))
                 (define (boxify/fmt fmt x)
                   (cond
                     [(and (pair? fmt) (unshared-list? x))
                      (boxify-list x
                        (if (eq? (car fmt) 'alt)
                            (cdr fmt)
                            (list fmt)))]
                     [else (boxify x)]))
                 (define (read-macro? x)
                   (and (pair? x) (eq? (car x) 'read-macro)))
                 (cond
                   [(read-macro? fmt)
                    (conc (cdr fmt) (boxify (cadr ls)))]
                   [(fmt-dots? fmt)
                    (return (fmt-tab fmt)
                            (map1ltr (lambda (x) (boxify/fmt (sub-fmt fmt) x))
                                 ls))]
                   [else
                    (let ([a (boxify/fmt (sub-fmt fmt) a)])
                      (let-values ([(sep* ls)
                                    (let f ([fmt (skip-fmt fmt)] [ls (cdr ls)])
                                      (cond
                                        [(null? ls)
                                         (values '() '())]
                                        [(fmt-dots? fmt)
                                         (values (fmt-tab fmt)
                                                 (map1ltr
                                                   (lambda (x)
                                                    (boxify/fmt (sub-fmt fmt) x))
                                                   ls))]
                                        [else
                                         (let ([a
                                                (boxify/fmt (sub-fmt fmt)
                                                  (car ls))])
                                           (let-values ([(f^ l^)
                                                         (f (skip-fmt fmt)
                                                            (cdr ls))])
                                             (values (cons (fmt-tab fmt) f^)
                                                     (cons a l^))))]))])
                        (return sep* (cons a ls))))])))]
              [else
               (return (gensep*-default ls) (map1ltr boxify ls))])))
      (boxify-list ls '()))
    (define (boxify-pair x)
      (define (boxify-cdrs x)
        (cond
          [(and (pair? x) (not (graphed? x)))
           (let ([a (boxify (car x))])
             (let-values ([(ls last) (boxify-cdrs (cdr x))])
               (values (cons a ls) last)))]
          [else
           (values '() (boxify x))]))
      (let ([a (boxify (car x))])
        (let-values ([(ls last) (boxify-cdrs (cdr x))])
          (let ([ls (cons a ls)])
            (let ([n
                   (let f ([ls ls] [n 4])
                     (cond
                       [(null? ls) n]
                       [else
                        (f (cdr ls)
                           (fx+ (fxadd1 n) (box-length (car ls))))]))])
              (make-pbox (fx+ n (box-length last)) ls last))))))
    (define (boxify-vector x)
      (let ([ls (map1ltr boxify (vector->list x))])
        (let ([n
               (let f ([ls ls] [n 0])
                 (cond
                   [(null? ls) n]
                   [else
                    (f (cdr ls) (fx+ n (box-length (car ls))))]))])
          (make-vbox (fx+ (fx+ n 2) (vector-length x)) "#" ls))))
    (define (boxify-bytevector x)
      (define prefix "#vu8")
      (let ([ls (map (lambda (x) (number->string x))
                       (bytevector->u8-list x))])
        (let ([len (fold-left (lambda (ac s) (+ 1 ac (string-length s)))
                              (+ 1 (string-length prefix))
                              ls)])
          (make-vbox len prefix ls))))
    (define (graphed? x)
      (import traversal-helpers)
      (let ([b (hashtable-ref h x #f)])
        (let ([b (if (fixnum? b) b (car b))])
          (cond
            [(cyclic-set? b) #t]
            [(shared-set? b) (print-graph)]
            [else            #f]))))
    (define (unshared-list? x)
      ;;; all cdrs of non-empty list are not-shared?
      (and (pair? x)
           (let f ([x (cdr x)])
             (or (null? x)
                 (and (pair? x)
                      (not (graphed? x))
                      (f (cdr x)))))))
    (define (boxify-struct x)
      (define (boxify-vanilla-struct x)
        (cond
          [(let ([rtd (struct-type-descriptor x)])
             (and (record-type-descriptor? rtd)
                  (record-type-opaque? rtd)))
           "#<unknown>"]
	  [(keyword? x)
	   (string-append "#:" (symbol->string (keyword->symbol x)))]
          [else
           (let* ([name (boxify (struct-name x))]
                  [ls
                   (let ([n (struct-length x)])
                     (let f ([i 0])
                       (cond
                         [(fx= i n) '()]
                         [else
                          (let ([a (boxify (struct-ref x i))])
                            (cons a (f (+ i 1))))])))]
                  [ls (cons name ls)]
                  [len (fold-left (lambda (ac s) (+ 1 ac (box-length s)))
                                 -1 ls)])
              (conc "#[" (make-fbox len ls #f) "]"))]))
      (define (boxify-custom-struct out)
        (import traversal-helpers)
        (let ([ls
               (let f ([cache (cdr out)])
                 (cond
                   [(not cache) (list (car out))]
                   [else
                    (let ([obj (boxify (cache-object cache))])
                      (let ([ls (f (cache-next cache))])
                        (cons* (cache-string cache) obj ls)))]))])
          (let ([len (fold-left (lambda (ac s) (+ 1 ac (box-length s)))
                                 -1 ls)])
            (make-fbox len ls #f))))
      (let ([b (hashtable-ref h x #f)])
        (cond
          [(pair? b) (boxify-custom-struct (cdr b))]
          [else (boxify-vanilla-struct x)])))
    (define (boxify-shared x k)
      (import traversal-helpers)
      (let ([b (hashtable-ref h x #f)])
        (let ([b (if (fixnum? b) b (car b))])
          (cond
            [(mark-set? b)
             (string-append "#"
                (number->string (fxsra b mark-shift))
                "#")]
            [(or (cyclic-set? b)
                 (and (shared-set? b) (print-graph)))
             (let ([n shared-idx])
               (set! shared-idx (+ shared-idx 1))
               (set-mark! x h n)
               (let ([str (string-append "#" (number->string n) "=")])
                 (let ([xbox (k x)])
                   (make-cbox (+ (string-length str) (box-length xbox))
                     (list str xbox)))))]
            [else (k x)]))))
    (define (boxify x)
      (cond
        [(null? x)          "()"]
        [(vector? x)        (boxify-shared x boxify-vector)]
        [(unshared-list? x) (boxify-shared x boxify-list)]
        [(pair? x)          (boxify-shared x boxify-pair)]
        [(bytevector? x)    (boxify-shared x boxify-bytevector)]
        [(struct? x)        (boxify-shared x boxify-struct)]
        ;[(setbox? x)
        ; (let ([i (format "#~a=" (setbox-idx x))]
        ;       [b (boxify (setbox-data x))])
        ;   (make-cbox (+ (string-length i) (box-length b))
        ;     (list i b)))]
        ;[(refbox? x) (format "#~a#" (refbox-idx x))]
        [else           (format "~s" x)]))
    (boxify x))
  (define string-esc-table
    '((7 . "a")
      (8 . "b")
      (9 . "t")
      (10 . "n")
      (11 . "v")
      (12 . "f")
      (13 . "r")
      (34 . "\"")
      (92 . "\\")))
  (define (hexify n)
    (cond
      [(fx< n 10) (integer->char (fx+ n (char->integer #\0)))]
      [else (integer->char (fx+ (fx- n 10) (char->integer #\A)))]))
  (define (output x p)
    (define (output-cbox x p col)
      (let g ([ls (cbox-boxes x)] [p p] [col col])
        (cond
          [(null? ls) col]
          [else
           (g (cdr ls) p
              (f (car ls) p col))])))
    (define (tab col p)
      (newline p)
      (let f ([col col] [p p])
        (unless (fxzero? col)
          (display #\space p)
          (f (fxsub1 col) p))))
    (define (output-pbox x p col)
      (define (pbox-one-line x p col)
        (display "(" p)
        (let g ([ls (pbox-ls x)]
                [p p]
                [col (fx+ col 1)]
                [last (pbox-last x)])
          (cond
            [(null? ls)
             (display ". " p)
             (let ([col (f last p (fx+ col 2))])
               (display ")" p)
               (fx+ col 1))]
            [else
             (let ([col (f (car ls) p col)])
               (display " " p)
               (g (cdr ls) p (fx+ col 1) last))])))
      (define (pbox-multi-fill x p col)
        (display "(" p)
        (let g ([ls (cdr (pbox-ls x))]
                [p p]
                [start-col (fx+ col 1)]
                [col (f (car (pbox-ls x)) p (fx+ col 1))]
                [last (pbox-last x)])
          (cond
            [(null? ls)
             (let ([n (box-length last)])
               (let ([col
                      (cond
                        [(fx<= (fx+ (fx+ col n) 4) (pretty-width))
                         (display " . " p)
                         (fx+ col 3)]
                        [else
                         (tab start-col p)
                         (display ". " p)
                         (fx+ start-col 2)])])
                  (let ([col (f last p col)])
                    (display ")" p)
                    (fx+ col 1))))]
            [(fx<= (fx+ (fx+ col 1) (box-length (car ls)))
                   (pretty-width))
             (display " " p)
             (g (cdr ls) p start-col
                (f (car ls) p (fx+ col 1))
                last)]
            [else
             (tab start-col p)
             (g (cdr ls) p start-col
                (f (car ls) p start-col)
                last)])))
      (cond
        [(fx<= (fx+ col (pbox-length x)) (pretty-width))
         (pbox-one-line x p col)]
        [else
         (pbox-multi-fill x p col)]))
    (define (output-mbox x p col)
      (display (mbox-str x) p)
      (f (mbox-val x) p (fx+ col (string-length (mbox-str x)))))
    (define (output-vbox x p col)
      (display (vbox-prefix x) p)
      (let ([ls (vbox-ls x)] [col (+ col (string-length (vbox-prefix x)))])
        (cond
          [(null? ls)
           (display "()" p)
           (fx+ col 2)]
          [else
           (display "(" p)
           (let g ([ls (cdr ls)] [p p]
                   [col (f (car ls) p (fx+ col 1))]
                   [start (fx+ col 1)])
             (cond
               [(null? ls)
                (display ")" p)
                (fx+ col 1)]
               [(fx<= (fx+ (fx+ col 1) (box-length (car ls))) (pretty-width))
                (display " " p)
                (g (cdr ls) p
                   (f (car ls) p (fx+ col 1))
                   start)]
               [else
                (tab start p)
                (g (cdr ls) p
                   (f (car ls) p start)
                   start)]))])))
    (define (output-fbox x p col)
      (define (output-rest-cont box* sep* p col left)
        (cond
          [(null? box*) col]
          [(pair? sep*)
           (let* ([box (car box*)]
                  [sep (car sep*)]
                  [w (box-length box)])
             (cond
               [(fx<= (fx+ (fxadd1 w) col) (pretty-width))
                (display " " p)
                (output-rest-cont (cdr box*) (cdr sep*) p
                  (f box p (fxadd1 col)) left)]
               [(not sep)
                (display " " p)
                (output-rest-multi (cdr box*) (cdr sep*) p
                   (f box p (fxadd1 col)) left)]
               [else
                (let ([col (fx+ left sep)])
                  (tab col p)
                  (cond
                    [(fx<= (fx+ w col) (pretty-width))
                     (output-rest-cont (cdr box*) (cdr sep*) p
                       (f box p col) left)]
                    [else
                     (output-rest-multi (cdr box*) (cdr sep*) p
                       (f box p col) left)]))]))]
          [else
           (output-last-cont box* sep* p col left)]))
      (define (output-last-cont box* sep p col left)
        (define (sum ls)
          (cond
            [(null? ls) 0]
            [else (fx+ (box-length (car ls))
                       (fxadd1 (sum (cdr ls))))]))
        (cond
          [(not sep)
           (output-rest-cont box* '(#f . #f) p col left)]
          [(fx<= (fx+ (sum box*) col) (pretty-width))
           (let g ([box* box*] [p p] [col col])
             (cond
               [(null? box*) col]
               [else
                (display " " p)
                (g (cdr box*) p (f (car box*) p (fxadd1 col)))]))]
          [else
           (let g ([box* box*] [p p] [left (fx+ left sep)] [col col])
             (cond
               [(null? box*) col]
               [else
                (tab left p)
                (g (cdr box*) p left
                   (f (car box*) p left))]))]))
      (define (output-last-multi box* sep p col left)
        (define (sum ls)
          (cond
            [(null? ls) 0]
            [else (fx+ (box-length (car ls))
                       (fxadd1 (sum (cdr ls))))]))
        (cond
          [(not sep)
           (output-rest-multi box* '(#f . #f) p col left)]
          [else
           (let g ([box* box*] [p p] [left (fx+ left sep)] [col col])
             (cond
               [(null? box*) col]
               [else
                (tab left p)
                (g (cdr box*) p left
                   (f (car box*) p left))]))]))
      (define (output-rest-multi box* sep* p col left)
        (cond
          [(null? box*) col]
          [(pair? sep*)
           (let* ([box (car box*)]
                  [sep (car sep*)]
                  [w (box-length box)])
             (cond
               [(not sep)
                (display " " p)
                (output-rest-multi (cdr box*) (cdr sep*) p
                   (f box p (fxadd1 col)) left)]
               [else
                (let ([col (fx+ left sep)])
                  (tab col p)
                  (cond
                    [(fx<= (fx+ w col) (pretty-width))
                     (output-rest-cont (cdr box*) (cdr sep*) p
                       (f box p col) left)]
                    [else
                     (output-rest-multi (cdr box*) (cdr sep*) p
                       (f box p col) left)]))]))]
          [else (output-last-multi box* sep* p col left)]))
      (define (output-box-init box box* sep* p left)
        (let ([w (box-length box)])
          (cond
            [(fx<= (fx+ w left) (pretty-width))
             (let ([col (f box p left)])
               (output-rest-cont box* sep* p col left))]
            [else
             (let ([col (f box p left)])
               (output-rest-multi box* sep* p col left))])))
      (let ([box* (fbox-box* x)]
            [sep* (fbox-sep* x)])
        (output-box-init (car box*) (cdr box*) sep* p col)))
    (define (f x p col)
      (cond
        [(string? x)
         (display x p)
         (fx+ col (string-length x))]
        [(cbox? x)   (output-cbox x p col)]
        [(pbox? x)   (output-pbox x p col)]
        [(mbox? x)   (output-mbox x p col)]
        [(vbox? x)   (output-vbox x p col)]
        [(fbox? x)   (output-fbox x p col)]
        [else (die 'pretty-print-output "invalid" x)]))
    (f x p 0)
    (newline p))
  ;;;

  (define (pretty x p)
    (let ([h (make-eq-hashtable)])
      (traverse x h)
      (output (boxify x h) p)))
  ;;;
  (define pretty-print
    (case-lambda
      [(x) (pretty x (current-output-port))]
      [(x p)
       (if (output-port? p)
           (pretty x p)
           (die 'pretty-print "not an output port" p))]))

)
