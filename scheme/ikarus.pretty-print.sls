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


#!r6rs
(library (ikarus pretty-print)
  (export
    pretty-print
    pretty-print*
    pretty-width
    debug-print)
  (import (except (ikarus)
		  pretty-print
		  pretty-print*
		  pretty-width
		  debug-print)
    (only (ikarus writer)
	  traverse
	  traversal-helpers)
    (only (ikarus.pretty-formats)
	  get-fmt)
    (only (ikarus records procedural)
	  print-r6rs-record-instance)
    (vicare language-extensions syntaxes)
    (vicare arguments validation))


(define (map1ltr f ls)
;;; ltr so that gensym counts get assigned properly
  (if (null? ls)
      '()
    (let ((a (f (car ls)))) ;do this first
      (cons a (map1ltr f (cdr ls))))))

(define pretty-width
  (make-parameter 60
    (lambda (x)
      (define who 'pretty-width)
      (with-arguments-validation (who)
	  ((positive-exact-integer	x))
	x))))

(define (pretty-indent)
  1)

(define-struct cbox (length boxes))
(define-struct pbox (length ls last))
(define-struct mbox (length str val))
(define-struct vbox (length prefix ls))
(define-struct fbox (length box* sep*))

(define (box-length x)
  (cond
   ((string? x) (string-length x))
   ((cbox? x)   (cbox-length x))
   ((pbox? x)   (pbox-length x))
   ((mbox? x)   (mbox-length x))
   ((vbox? x)   (vbox-length x))
   ((fbox? x)   (fbox-length x))
   (else
    (assertion-violation 'boxify "invalid box" x))))


(define (boxify x h)
  (define shared-idx 0)

  (define (conc . a*)
    (let ((n (let f ((a* a*) (len 0))
	       (if (null? a*)
		   len
		 (f (cdr a*) (fx+ len (box-length (car a*))))))))
      (make-cbox n a*)))

  (define (boxify-list ls)
    (define (sum-box* ls)
      (cond
       ((null? (cdr ls))
	(box-length (car ls)))
       (else
	(fx+ (box-length (car ls))
	     (fxadd1 (sum-box* (cdr ls)))))))
    (define (gensep*-default ls)
      (cond
       ((null? (cdr ls)) '())
       (else
	(cons (pretty-indent) (gensep*-default (cdr ls))))))
    (define (tab-value x)
      (cond
       ((eq? x 'tab) (pretty-indent))
       ((fixnum? x) x)
       (else #f)))
    (define (select-alt alt-fmt* ls)
      (define (good-match? fmt ls)
	(cond
	 ((not (pair? fmt)) #t)
	 ((eq? (car fmt) 'read-macro)
	  (and (unshared-list? ls) (fx= (length ls) 2)))
	 (else
	  (let ((a (car fmt)) (fmt (cdr fmt)))
	    (cond
	     ((or (eq? a 'tab) (fixnum? a))
	      (good-match? fmt ls))
	     ((and (pair? fmt) (eq? (car fmt) '...))
	      (and (unshared-list? ls)
		   (andmap (lambda (x) (good-match? a x)) ls)))
	     ((and (pair? ls) (not (graphed? ls)))
	      (and (good-match? a (car ls))
		   (good-match? fmt (cdr ls))))
	     (else #f))))))
      (ormap (lambda (fmt) (and (good-match? fmt ls) fmt))
	     alt-fmt*))
    (define (applicable-formats a alt-fmt*)
      (cond
       ((and (symbol? a) (get-fmt a)) =>
	(lambda (fmt)
	  (cond
	   ((and (pair? fmt) (eq? (car fmt) 'alt))
	    (append alt-fmt* (cdr fmt)))
	   (else
	    (append alt-fmt* (list fmt))))))
       ((null? alt-fmt*) #f)
       (else       alt-fmt*)))
    (define (return sep* box*)
      (let ((n (sum-box* box*)))
	(conc "(" (make-fbox n box* sep*) ")")))
    (define (boxify-list ls alt-fmt*)
      (let ((a (car ls)))
	(cond
	 ((applicable-formats a alt-fmt*) =>
	  (lambda (fmt*)
	    (let ((fmt (select-alt fmt* ls)))
	      (module (fmt-dots? skip-fmt fmt-tab sub-fmt)
		(define (parse-fmt x)
		  (define (parse-dots tab fmt x)
		    (cond
		     ((and (pair? x) (eq? (car x) '...))
		      (values tab fmt #t (cdr x)))
		     (else
		      (values tab fmt #f x))))
		  (define (parse-tab tab x)
		    (cond
		     ((pair? x)
		      (parse-dots tab (car x) (cdr x)))
		     (else (values tab #f #f #f))))
		  (cond
		   ((pair? x)
		    (let ((a0 (car x)))
		      (cond
		       ((eq? a0 'tab)
			(parse-tab (pretty-indent) (cdr x)))
		       ((fixnum? a0)
			(parse-tab a0 (cdr x)))
		       (else (parse-tab #f x)))))
		   (else (values (pretty-indent) #f #f #f))))
		(define (fmt-dots? x)
		  (let-values (((tab subfmt dots fmt) (parse-fmt x)))
		    dots))
		(define (fmt-tab x)
		  (let-values (((tab subfmt dots fmt) (parse-fmt x)))
		    tab))
		(define (sub-fmt x)
		  (let-values (((tab subfmt dots fmt) (parse-fmt x)))
		    subfmt))
		(define (skip-fmt x)
		  (let-values (((tab subfmt dots fmt) (parse-fmt x)))
		    fmt)))
	      (define (boxify/fmt fmt x)
		(cond
		 ((and (pair? fmt) (unshared-list? x))
		  (boxify-list x
			       (if (eq? (car fmt) 'alt)
				   (cdr fmt)
				 (list fmt))))
		 (else (boxify x))))
	      (define (read-macro? x)
		(and (pair? x) (eq? (car x) 'read-macro)))
	      (cond
	       ((read-macro? fmt)
		(conc (cdr fmt) (boxify (cadr ls))))
	       ((fmt-dots? fmt)
		(return (fmt-tab fmt)
			(map1ltr (lambda (x) (boxify/fmt (sub-fmt fmt) x))
                                 ls)))
	       (else
		(let ((a (boxify/fmt (sub-fmt fmt) a)))
		  (let-values (((sep* ls)
				(let f ((fmt (skip-fmt fmt)) (ls (cdr ls)))
				  (cond
				   ((null? ls)
				    (values '() '()))
				   ((fmt-dots? fmt)
				    (values (fmt-tab fmt)
					    (map1ltr
					     (lambda (x)
					       (boxify/fmt (sub-fmt fmt) x))
					     ls)))
				   (else
				    (let ((a
					   (boxify/fmt (sub-fmt fmt)
						       (car ls))))
				      (let-values (((f^ l^)
						    (f (skip-fmt fmt)
						       (cdr ls))))
					(values (cons (fmt-tab fmt) f^)
						(cons a l^)))))))))
		    (return sep* (cons a ls)))))))))
	 (else
	  (return (gensep*-default ls) (map1ltr boxify ls))))))
    (boxify-list ls '()))
  (define (boxify-pair x)
    (define (boxify-cdrs x)
      (cond
       ((and (pair? x) (not (graphed? x)))
	(let ((a (boxify (car x))))
	  (let-values (((ls last) (boxify-cdrs (cdr x))))
	    (values (cons a ls) last))))
       (else
	(values '() (boxify x)))))
    (let ((a (boxify (car x))))
      (let-values (((ls last) (boxify-cdrs (cdr x))))
	(let ((ls (cons a ls)))
	  (let ((n
		 (let f ((ls ls) (n 4))
		   (cond
		    ((null? ls) n)
		    (else
		     (f (cdr ls)
			(fx+ (fxadd1 n) (box-length (car ls)))))))))
	    (make-pbox (fx+ n (box-length last)) ls last))))))
  (define (boxify-vector x)
    (let ((ls (map1ltr boxify (vector->list x))))
      (let ((n
	     (let f ((ls ls) (n 0))
	       (cond
		((null? ls) n)
		(else
		 (f (cdr ls) (fx+ n (box-length (car ls)))))))))
	(make-vbox (fx+ (fx+ n 2) (vector-length x)) "#" ls))))
  (define (boxify-bytevector x)
    (define prefix "#vu8")
    (let ((ls (map (lambda (x) (number->string x))
		(bytevector->u8-list x))))
      (let ((len (fold-left (lambda (ac s) (+ 1 ac (string-length s)))
		   (+ 1 (string-length prefix))
		   ls)))
	(make-vbox len prefix ls))))
  (define (graphed? x)
    (import traversal-helpers)
    (let ((b (hashtable-ref h x #f)))
      (let ((b (if (fixnum? b) b (car b))))
	(cond
	 ((cyclic-set? b) #t)
	 ((shared-set? b) (print-graph))
	 (else            #f)))))
  (define (unshared-list? x)
      ;;; all cdrs of non-empty list are not-shared?
    (and (pair? x)
	 (let f ((x (cdr x)))
	   (or (null? x)
	       (and (pair? x)
		    (not (graphed? x))
		    (f (cdr x)))))))
  (define (boxify-struct x)
    (define (boxify-vanilla-struct x)
      (cond
       ((record-type-descriptor? (struct-type-descriptor x))
	(call-with-string-output-port
	    (lambda (port)
	      (print-r6rs-record-instance x port))))
       ;;We do *not* handle opaque records specially.
       #;((let ((rtd (struct-type-descriptor x)))
       (and (record-type-descriptor? rtd)
       (record-type-opaque? rtd)))
       "#<unknown>")
       ((keyword? x)
	(string-append "#:" (symbol->string (keyword->symbol x))))
       (else
	(let* ((name (boxify (struct-name x)))
	       (ls
		(let ((n (struct-length x)))
		  (let f ((i 0))
		    (cond
		     ((fx= i n) '())
		     (else
		      (let ((a (boxify (struct-ref x i))))
			(cons a (f (+ i 1)))))))))
	       (ls (cons name ls))
	       (len (fold-left (lambda (ac s) (+ 1 ac (box-length s)))
		      -1 ls)))
	  (conc "#[" (make-fbox len ls #f) "]")))))

    (define (boxify-custom-struct out)
      (import traversal-helpers)
      (let ((ls
	     (let f ((cache (cdr out)))
	       (cond
		((not cache) (list (car out)))
		(else
		 (let ((obj (boxify (cache-object cache))))
		   (let ((ls (f (cache-next cache))))
		     (cons* (cache-string cache) obj ls))))))))
	(let ((len (fold-left (lambda (ac s) (+ 1 ac (box-length s)))
		     -1 ls)))
	  (make-fbox len ls #f))))

    (let ((b (hashtable-ref h x #f)))
      (cond
       ((pair? b) (boxify-custom-struct (cdr b)))
       (else (boxify-vanilla-struct x)))))

  (define (boxify-shared x k)
    (import traversal-helpers)
    (let ((b (hashtable-ref h x #f)))
      (let ((b (if (fixnum? b) b (car b))))
	(cond
	 ((mark-set? b)
	  (string-append "#"
			 (number->string (fxsra b mark-shift))
			 "#"))
	 ((or (cyclic-set? b)
	      (and (shared-set? b) (print-graph)))
	  (let ((n shared-idx))
	    (set! shared-idx (+ shared-idx 1))
	    (set-mark! x h n)
	    (let ((str (string-append "#" (number->string n) "=")))
	      (let ((xbox (k x)))
		(make-cbox (+ (string-length str) (box-length xbox))
			   (list str xbox))))))
	 (else (k x))))))
  (define (boxify x)
    (cond
     ((null? x)          "()")
     ((vector? x)        (boxify-shared x boxify-vector))
     ((unshared-list? x) (boxify-shared x boxify-list))
     ((pair? x)          (boxify-shared x boxify-pair))
     ((bytevector? x)    (boxify-shared x boxify-bytevector))
     ((struct? x)        (boxify-shared x boxify-struct))
;;;((setbox? x)
;;; (let ((i (format "#~a=" (setbox-idx x)))
;;;       (b (boxify (setbox-data x))))
;;;   (make-cbox (+ (string-length i) (box-length b))
;;;     (list i b))))
;;;((refbox? x) (format "#~a#" (refbox-idx x)))
     (else           (format "~s" x))))
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

(define FIXNUM-ZERO
  (char->integer #\0))

(define FIXNUM-A
  (char->integer #\A))

(define (hexify n)
  (if (fx< n 10)
      (integer->char (fx+ n FIXNUM-ZERO))
    (integer->char (fx+ (fx- n 10) FIXNUM-A))))


(define (output x port start-column ending-newline?)

  (define (output-cbox x port col)
    (let loop ((ls	(cbox-boxes x))
	       (port	port)
	       (col	col))
      (if (null? ls)
	  col
	(loop (cdr ls) port
	      (main (car ls) port col)))))

  (define (tab col port)
    (newline port)
    (let loop ((col col) (port port))
      (unless (fxzero? col)
	(put-char port #\space)
	(loop (fxsub1 col) port))))

  (define (output-pbox x port col)

    (define (pbox-one-line x port col)
      (display "(" port)
      (let loop ((ls	(pbox-ls x))
		 (port	port)
		 (col	(fx+ col 1))
		 (last	(pbox-last x)))
	(cond ((null? ls)
	       (display ". " port)
	       (let ((col (main last port (fx+ col 2))))
		 (display ")" port)
		 (fx+ col 1)))
	      (else
	       (let ((col (main (car ls) port col)))
		 (display " " port)
		 (loop (cdr ls) port (fx+ col 1) last))))))

    (define (pbox-multi-fill x port col)
      (display "(" port)
      (let g ((ls		(cdr (pbox-ls x)))
	      (port		port)
	      (start-col	(fx+ col 1))
	      (col		(main (car (pbox-ls x)) port (fx+ col 1)))
	      (last		(pbox-last x)))
	(cond ((null? ls)
	       (let ((n (box-length last)))
		 (let ((col (cond ((fx<= (fx+ (fx+ col n) 4) (pretty-width))
				   (display " . " port)
				   (fx+ col 3))
				  (else
				   (tab start-col port)
				   (display ". " port)
				   (fx+ start-col 2)))))
		   (let ((col (main last port col)))
		     (display ")" port)
		     (fx+ col 1)))))
	      ((fx<= (fx+ (fx+ col 1) (box-length (car ls)))
		     (pretty-width))
	       (display " " port)
	       (g (cdr ls) port start-col
		  (main (car ls) port (fx+ col 1))
		  last))
	      (else
	       (tab start-col port)
	       (g (cdr ls) port start-col
		  (main (car ls) port start-col)
		  last)))))

    (if (fx<= (fx+ col (pbox-length x)) (pretty-width))
	(pbox-one-line x port col)
      (pbox-multi-fill x port col)))

  (define (output-mbox x port col)
    (display (mbox-str x) port)
    (main (mbox-val x) port (fx+ col (string-length (mbox-str x)))))

  (define (output-vbox x port col)
    (display (vbox-prefix x) port)
    (let ((ls	(vbox-ls x))
	  (col	(+ col (string-length (vbox-prefix x)))))
      (cond ((null? ls)
	     (display "()" port)
	     (fx+ col 2))
	    (else
	     (display "(" port)
	     (let g ((ls	(cdr ls)) (port port)
		     (col	(main (car ls) port (fx+ col 1)))
		     (start	(fx+ col 1)))
	       (cond ((null? ls)
		      (display ")" port)
		      (fx+ col 1))
		     ((fx<= (fx+ (fx+ col 1) (box-length (car ls))) (pretty-width))
		      (display " " port)
		      (g (cdr ls) port
			 (main (car ls) port (fx+ col 1))
			 start))
		     (else
		      (tab start port)
		      (g (cdr ls) port
			 (main (car ls) port start)
			 start))))))))

  (define (output-fbox x port col)
    (define (output-rest-cont box* sep* port col left)
      (cond ((null? box*)
	     col)
	    ((pair? sep*)
	     (let* ((box	(car box*))
		    (sep	(car sep*))
		    (w		(box-length box)))
	       (cond ((fx<= (fx+ (fxadd1 w) col) (pretty-width))
		      (display " " port)
		      (output-rest-cont (cdr box*) (cdr sep*) port
					(main box port (fxadd1 col)) left))
		     ((not sep)
		      (display " " port)
		      (output-rest-multi (cdr box*) (cdr sep*) port
					 (main box port (fxadd1 col)) left))
		     (else
		      (let ((col (fx+ left sep)))
			(tab col port)
			(cond
			 ((fx<= (fx+ w col) (pretty-width))
			  (output-rest-cont (cdr box*) (cdr sep*) port
					    (main box port col) left))
			 (else
			  (output-rest-multi (cdr box*) (cdr sep*) port
					     (main box port col) left))))))))
	    (else
	     (output-last-cont box* sep* port col left))))

    (define (output-last-cont box* sep port col left)
      (define (sum ls)
	(if (null? ls)
	    0
	  (fx+ (box-length (car ls))
	       (fxadd1 (sum (cdr ls))))))

      (cond ((not sep)
	     (output-rest-cont box* '(#f . #f) port col left))
	    ((fx<= (fx+ (sum box*) col) (pretty-width))
	     (let g ((box* box*)
		     (port port)
		     (col  col))
	       (if (null? box*)
		   col
		 (begin
		   (display " " port)
		   (g (cdr box*) port (main (car box*) port (fxadd1 col)))))))
	    (else
	     (let g ((box* box*)
		     (port port)
		     (left (fx+ left sep))
		     (col  col))
	       (if (null? box*)
		   col
		 (begin
		   (tab left port)
		   (g (cdr box*) port left
		      (main (car box*) port left))))
	       ))))

    (define (output-last-multi box* sep port col left)
      (define (sum ls)
	(if (null? ls)
	    0
	  (fx+ (box-length (car ls))
	       (fxadd1 (sum (cdr ls))))))
      (if (not sep)
	  (output-rest-multi box* '(#f . #f) port col left)
	(let g ((box* box*)
		(port port)
		(left (fx+ left sep))
		(col  col))
	  (if (null? box*)
	      col
	    (begin
	      (tab left port)
	      (g (cdr box*) port left
		 (main (car box*) port left))))
	  )))

    (define (output-rest-multi box* sep* port col left)
      (cond ((null? box*)
	     col)
	    ((pair? sep*)
	     (let* ((box	(car box*))
		    (sep	(car sep*))
		    (w		(box-length box)))
	       (cond ((not sep)
		      (display " " port)
		      (output-rest-multi (cdr box*) (cdr sep*) port
					 (main box port (fxadd1 col)) left))
		     (else
		      (let ((col (fx+ left sep)))
			(tab col port)
			(if (fx<= (fx+ w col)
				  (pretty-width))
			    (output-rest-cont (cdr box*) (cdr sep*) port
					      (main box port col) left)
			  (output-rest-multi (cdr box*) (cdr sep*) port
					     (main box port col) left)))
		      ))))
	    (else
	     (output-last-multi box* sep* port col left))))

    (define (output-box-init box box* sep* port left)
      (if (fx<= (fx+ (box-length box) left)
		(pretty-width))
	  (let ((col (main box port left)))
	    (output-rest-cont box* sep* port col left))
	(let ((col (main box port left)))
	  (output-rest-multi box* sep* port col left))))

    (let ((box* (fbox-box* x))
	  (sep* (fbox-sep* x)))
      (output-box-init (car box*) (cdr box*) sep* port col))) ;end of OUTPUT-FBOX

  (define (main x port col)
    (cond ((string? x)
	   (display x port)
	   (fx+ col (string-length x)))
	  ((cbox? x)   (output-cbox x port col))
	  ((pbox? x)   (output-pbox x port col))
	  ((mbox? x)   (output-mbox x port col))
	  ((vbox? x)   (output-vbox x port col))
	  ((fbox? x)   (output-fbox x port col))
	  (else
	   (assertion-violation 'pretty-print-output "invalid" x))))

  (main x port start-column)
  (when ending-newline?
    (newline port)))


(define (pretty x port start-column ending-newline?)
  (let ((h (make-eq-hashtable)))
    (traverse x h)
    (output (boxify x h) port start-column ending-newline?)))

(define pretty-print
  (case-lambda
   ((x)
    (pretty x (current-output-port) 0 #t))
   ((x port)
    (define who 'pretty-print)
    (with-arguments-validation (who)
	((output-port port))
      (pretty x port 0 #t)))))

(define (pretty-print* x port start-column ending-newline?)
  (define who 'pretty-print*)
  (with-arguments-validation (who)
      ((output-port		port)
       (non-negative-fixnum	start-column))
    (pretty x port start-column ending-newline?)))

(define (debug-print . args)
  ;;Print arguments for debugging purposes.
  ;;
  (pretty-print args (current-error-port))
  (newline (current-error-port))
  (newline (current-error-port)))


;;;; done

)

;;; end of file
