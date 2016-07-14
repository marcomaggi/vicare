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


#!vicare
(library (ikarus pretty-print)
  (export
    pretty-print		pretty-print*
    pretty-width

    debug-print-enabled?
    debug-print			debug-print*)
  (import (except (vicare)
		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Fri Apr 15, 2016)
		  sentinel?
		  ;;;

		  pretty-print			pretty-print*
		  pretty-width

		  debug-print-enabled?
		  debug-print			debug-print*)
    (only (vicare system $structs)
	  base-rtd
	  $struct-rtd)
    (prefix (only (ikarus writer)
		  the-printer-printing-style
		  case-printing-style
		  traverse
		  TRAVERSAL-HELPERS)
	    writer::)
    (prefix (only (ikarus records procedural)
		  record-ref)
	    records::)
    (only (ikarus.pretty-formats)
	  get-fmt)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Apr 15,
    ;;2016)
    (only (ikarus unique-objects)
	  sentinel?))


;;;; helpers

(define (map1-in-order fun ls)
  ;;Non-tail recursive  function.  Map the  function FUN over  the list LS  in order:
  ;;make sure that  FUN is first applied to the  car of LS, then to the  items in the
  ;;cdr.  This  way if FUN causes  the generation of gensyms:  the car of LS  has the
  ;;first gensyms, the cdr has the latter ones.
  ;;
  (if (pair? ls)
      (let* ((A (fun (car ls)))		       ;this first
	     (D (map1-in-order fun (cdr ls)))) ;this next
	(cons A D))
    '()))

(define pretty-width
  (make-parameter 60
    (lambda (obj)
      (if (positive-exact-integer? obj)
	  obj
	(procedure-argument-violation 'pretty-width
	  "expected positive fixnum or bignum as parameter value" obj)))))

(define (pretty-indent)
  1)


;;;; data types definition

;;CBOX structs represent a simple  concatenation of objects' strings representations.
;;The string  representations must  be written  one after  the other  without further
;;special processing and no separator between them.
;;
(define-struct cbox
  (length
		;The total length of the representations.
   boxes
		;A list of strings or box structs.
   ))

;;VBOX structs represent a vector's or bytevector's string representation.
;;
(define-struct vbox
  (length
		;The total  length of the  representation.  Including the  prefix and
		;the parentheses.
   prefix
		;A string representing the prefix.  Either "#" or "#vu8".
   ls
		;A list of strings and box structures representing the objects.
   ))

;;PBOX structs represent the string  representation of: pairs, improper lists, cyclic
;;lists, lists with shared objects.
;;
(define-struct pbox
  (length
		;The total length  of the objects, including the  parentheses and the
		;dot.
   ls
		;A list of strings or boxes representing the car and cdr.
   last
		;The string or box representing the last object in list.
   ))

;;FBOX structs  represent the string representation  of sequenced of objects  and the
;;separators between them.  It is used for lists, structs and records.
;;
(define-struct fbox
  (length
		;The total  length of  the object representations  in BOX*  and their
		;separators in SEP*.
   box*
		;A list of strings or box structs.
   sep*
		;False or a  list of fixnums representing blank  characters to insert
		;as separators between the representations of the objects in BOX*.
   ))

;;; --------------------------------------------------------------------

(define (box-length x)
  (cond ((string? x)	(string-length x))
	((cbox? x)	(cbox-length x))
	((pbox? x)	(pbox-length x))
	((vbox? x)	(vbox-length x))
	((fbox? x)	(fbox-length x))
	(else
	 (assertion-violation 'pretty-printer
	   "internal error: invalid box" x))))


;;;; public API

(module (pretty-print pretty-print*)

  (case-define* pretty-print
    ((x)
     (%pretty x (current-output-port) 0 #t))
    ((x {port textual-output-port?})
     (%pretty x port 0 #t)))

  (define* (pretty-print* x {port textual-output-port?} {start-column non-negative-fixnum?} ending-newline?)
    (%pretty x port start-column ending-newline?))

  (define (%pretty x port start-column ending-newline?)
    (parametrise ((writer::the-printer-printing-style 'pretty-print))
      (let ((marks-table (make-eq-hashtable)))
	(writer::traverse x marks-table)
	(output (boxify x marks-table) port start-column)))
    (when ending-newline?
      (newline port)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define (debug-print . args)
  ;;Print arguments for debugging purposes.
  ;;
  (pretty-print args (current-error-port))
  (newline (current-error-port))
  (when (pair? args)
    (car args)))

(define debug-print-enabled?
  (make-parameter #f
    (lambda (obj)
      (if obj #t #f))))

(define (debug-print* . args)
  (when (debug-print-enabled?)
    (apply debug-print args)))


(define (boxify x marks-table)
  ;;
  ;;This function has a lot of internal functions because only a few of them make use
  ;;of the argument MARKS-TABLE, so it is  more convenient to let that few close upon
  ;;the argument and the other avoid accepting a MARKS-TABLE argument.
  ;;
  ;;The entry  point is the  function "%boxify-object",  which can boxify  any Scheme
  ;;object.   All  the  internal  syntactic  bindings whose  name  is  prefixed  with
  ;;"%boxify-object-" do boxify a specific Scheme object type.
  ;;
  (define shared-idx 0)

  (define (%boxify-object x)
    (cond
     ((null? x)			"()")
     ((vector? x)		(boxify-shared x %boxify-object-vector))
     ((unshared-list? x)	(boxify-shared x %boxify-object-list))
     ((pair? x)			(boxify-shared x %boxify-object-pair))
     ((bytevector? x)		(boxify-shared x %boxify-object-bytevector))
     ;;Right now the would block object is a struct instance, so we have to check for
     ;;it before checking for structs.  (Marco Maggi; Mon May 13, 2013)
     ((would-block-object? x)	"#!would-block-object")
     ((bwp-object? x)		"#!bwp-object")
     ((unbound-object? x)	"#!unbound-object")
     ((sentinel? x)		"#!sentinel")
     ;;At present  keywords are  structs, so  we must process  them before  the other
     ;;structs.  We do not want them to be printed as shared objects.
     ((keyword? x)		(%boxify-object-keyword x))
     ((record-object? x)	(boxify-shared x %boxify-object-record))
     ((struct? x)		(boxify-shared x %boxify-object-struct))
     ((gensym? x)		(boxify-shared x %boxify-object-format))
     ((string? x)		(boxify-shared x %boxify-object-format))
     (else
      ;;This call to FORMAT might cause  calls to DISPLAY which recursively enter the
      ;;objects writer.
      (format "~s" x))))

;;; --------------------------------------------------------------------

  (define (%boxify-object-format obj)
    ;;We want the "~s" format here, so that gensyms are written out in full.
    ;;
    (format "~s" obj))

  (define (%boxify-object-keyword x)
    (keyword->string x))

;;; --------------------------------------------------------------------

  (module (%boxify-object-list)

    (define (%boxify-object-list ls)
      (%internal-boxify-list ls '()))

    (define (%internal-boxify-list ls alt-fmt*)
      (let ((a (car ls)))
	(cond ((%applicable-formats a alt-fmt*)
	       => (lambda (fmt*)
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
			  (%internal-boxify-list x (if (eq? (car fmt) 'alt)
						       (cdr fmt)
						     (list fmt))))
			 (else (%boxify-object x))))
		      (define (pretty-format-reader-macro? x)
			(and (pair? x) (eq? (car x) 'pretty-format-reader-macro)))
		      (cond
		       ((pretty-format-reader-macro? fmt)
			(%concatenate-into-cbox (cdr fmt) (%boxify-object (cadr ls))))
		       ((fmt-dots? fmt)
			(return (fmt-tab fmt)
				(map1-in-order (lambda (x) (boxify/fmt (sub-fmt fmt) x))
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
						    (map1-in-order
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
	       (return (%gensep*-default ls) (map1-in-order %boxify-object ls))))))

    (define (%applicable-formats first-item alt-fmt*)
      ;;FIRST-ITEM is the  first item in the  list to format.  ALT-FMT* is  a list of
      ;;pretty-format templates that are applicable to format FIRST-ITEM.
      ;;
      ;;* If  FIRST-ITEM is a  symbol recognised  by the pretty-format  facilities as
      ;;   having one  or more  templates: prepend  that template  (or templates)  to
      ;;  ALT-FMT* and return the result.
      ;;
      ;;* If FIRST-ITEM is not a symbol or does not have a pretty-format template: if
      ;;  ALT-FMT* is null, return false; otherwise return ALT-FMT* unchanged.
      ;;
      (cond ((and (symbol? first-item)
		  (get-fmt first-item))
	     => (lambda (fmt)
		  (cond ((and (pair? fmt)
			      (eq? (car fmt) 'alt))
			 (append alt-fmt* (cdr fmt)))
			(else
			 (append alt-fmt* (list fmt))))))
	    ((null? alt-fmt*)
	     #f)
	    (else
	     alt-fmt*)))

    (define (return sep* box*)
      (let ((n (sum-box* box*)))
	(%concatenate-into-cbox "(" (make-fbox n box* sep*) ")")))

    (define (sum-box* ls)
      ;;Non-tail recursive function.  Return a fixnum representing the sum of all the
      ;;lengths of  the boxes and  strings in the  list LS; for  every pair add  1 to
      ;;represent a single white space of separation between the list items.
      ;;
      (let ((D (cdr ls)))
	(if (pair? (cdr ls))
	    (fx+ (box-length (car ls))
		 (fxadd1 (sum-box* D)))
	  (box-length (car ls)))))

    (define (tab-value x)
      (cond ((eq? x 'tab)
	     (pretty-indent))
	    ((fixnum? x)
	     x)
	    (else #f)))

    (module (select-alt)

      (define (select-alt alt-fmt* ls)
	;;Expect ALT-FMT*  to be a  list symbolic expressions  representing alternate
	;;pretty-format templates.   Select the template  that matches LS  and return
	;;it.
	;;
	(ormap (lambda (fmt)
		 (and (good-match? fmt ls)
		      fmt))
	       alt-fmt*))

      (define (good-match? fmt ls)
	;;Return true  if the  symbolic expression  FMT representing  a pretty-format
	;;template matches the list LS.
	;;
	(cond ((not (pair? fmt))
	       #t)

	      ((eq? (car fmt) 'pretty-format-reader-macro)
	       (and (unshared-list? ls)
		    (fx= (length ls) 2)))

	      (else
	       (let ((fmt.a (car fmt))
		     (fmt.d (cdr fmt)))
		 (cond ((or (eq? fmt.a 'tab)
			    (fixnum? fmt.a))
			(good-match? fmt.d ls))

		       ((and (pair? fmt.d)
			     (eq? (car fmt.d) '...))
			;;Ellipsis patterns: the rest of LS must match FMT.A.
			(and (unshared-list? ls)
			     (andmap (lambda (x)
				       (good-match? fmt.a x))
				     ls)))

		       ((and (pair? ls)
			     (not (graphed? ls)))
			;;Pair pattern.
			(and (good-match? fmt.a (car ls))
			     (good-match? fmt.d (cdr ls))))

		       (else #f))))))

      #| end of module: SELECT-ALT |# )

    #| end of module: %BOXIFY-OBJECT-LIST |# )

;;; --------------------------------------------------------------------

  (module (%boxify-object-pair)

    (define (%boxify-object-pair x)
      (let ((A (%boxify-object (car x))))
	(receive (ls last-box)
	    (%boxify-cdrs (cdr x))
	  (let* ((ls   (cons A ls))
		 (len  (fold-left (lambda (n box)
				    (fx+ (fxadd1 n) (box-length box)))
			 4 ls))
		 (len  (fx+ len (box-length last-box))))
	    (make-pbox len ls last-box)))))

    (define (%boxify-cdrs x)
      (cond ((and (pair? x)
		  (not (graphed? x)))
	     (let ((A (%boxify-object (car x))))
	       (receive (ls last-box)
		   (%boxify-cdrs (cdr x))
		 (values (cons A ls) last-box))))
	    (else
	     (values '() (%boxify-object x)))))

    #| end of module: %BOXIFY-OBJECT-PAIR |# )

;;; --------------------------------------------------------------------

  (define (%boxify-object-vector x)
    (define-constant PREFIX "#")
    (let* ((ls   (map1-in-order %boxify-object (vector->list x)))
	   (len  (fold-left (lambda (accum box)
			      (fx+ accum (box-length box)))
		   0 ls))
	   ;;The total length includes the prefix and the parentheses.
	   (len  (fxadd1 (fx+ (fx+ len 2) (vector-length x)))))
      (make-vbox len PREFIX ls)))

;;; --------------------------------------------------------------------

  (define (%boxify-object-bytevector x)
    (define-constant PREFIX "#vu8")
    (define-constant RADIX
      (printer-integer-radix))
    (define-constant RADIX-PREFIX
      (case RADIX
	((10)	"")
	((2)	"#b")
	((8)	"#o")
	((16)	"#x")
	(else	"")))
    (define-constant RADIX-10?
      (fx=? 10 RADIX))
    (let* ((ls   (map (lambda (x)
			(if RADIX-10?
			    (fixnum->string x)
			  (string-append RADIX-PREFIX (fixnum->string x RADIX))))
		   (bytevector->u8-list x)))
	   ;;The total length includes the prefix and the parentheses.
	   (len  (fold-left (lambda (ac s)
			      (+ 1 ac (string-length s)))
		   (+ 1 (string-length PREFIX))
		   ls)))
      (make-vbox len PREFIX ls)))

;;; --------------------------------------------------------------------

  (module (%boxify-object-struct)

    (define (%boxify-object-struct x)
      (let ((b (hashtable-ref marks-table x #f)))
	(if (pair? b)
	    (%boxify-struct/custom-printer (cdr b))
	  (%boxify-struct/default-printer x))))

    (define (%boxify-struct/custom-printer cache-stack)
      ;;Boxify a struct object that makes use of a custom printer functions.
      ;;
      ;;We expect CACHE-STACK to  be a pair whose car is a suffix  string to write at
      ;;the end and whose  cdr is false or a chain of  CACHE structs representing the
      ;;strings to print in reverse order.
      ;;
      ;;To follow what happens, let's take this example code:
      ;;
      ;;   (define-struct duo
      ;;     (one two))
      ;;
      ;;   (set-rtd-printer! (struct-type-descriptor duo)
      ;;     (lambda (stru port sub-printer)
      ;;       (display "#{duo " port)
      ;;       (sub-printer (duo-one stru))
      ;;       (display " " port)
      ;;       (sub-printer (duo-two stru))
      ;;       (display "}" port))
      ;;
      ;;   (pretty-print (make-duo 1 2))
      ;;
      ;;we expect CACHE-STACK to be the pair:
      ;;
      ;;   ("}" . ?caches)
      ;;
      ;;and the ?CACHES linked list to be:
      ;;
      ;;   ---
      ;;    | string=" "
      ;;    | object=2
      ;;    | next --> ---
      ;;   ---          | string="#{duo "
      ;;                | object=1
      ;;                | next --> #f
      ;;               ---
      ;;
      (import writer::TRAVERSAL-HELPERS)
      ;;This RECUR  loop is conceptually  a fold-right for  the linked list  of CACHE
      ;;structs.  We traverse the  linked list of CACHE structs from  the tail to the
      ;;end, boxifying the  tail objects first; this way the  marks "#N=" are defined
      ;;in the correct order.  The value PAIR* is  a proper list of pairs; the car of
      ;;each pair is  a STRING field, the cdr  of each pair is the box  of the OBJECT
      ;;field.
      ;;
      ;;For the example, the resulting PAIR* is:
      ;;
      ;;   ((" " . 2) ("#{duo " . 1))
      ;;
      (define pair*
	(let recur ((cache (cdr cache-stack)))
	  (if cache
	      (let ((pair* (recur (cache-next cache))))
		(cons (cons (cache-string cache)
			    (%boxify-object (cache-object cache)))
		      pair*))
	    '())))
      ;;This folding  transforms the list of  pairs in printing-reverse order  into a
      ;;list of boxes in the printing-correct  order.  For the example, the resulting
      ;;BOX* is:
      ;;
      ;;   ("#{duo" 1 " " 2 "}")
      ;;
      (define box*
	(fold-left (lambda (box* pair)
		     (if (string-empty? (car pair))
			 (cons (cdr pair) ;the boxification of the OBJECT field
			       box*)
		       (cons* (car pair) ;the STRING field
			      (cdr pair) ;the boxification of the OBJECT field
			      box*)))
	  (list (car cache-stack))
	  pair*))
      (define len
	(fold-left (lambda (ac box)
		     (+ 1 ac (box-length box)))
	  -1 box*))
      (make-fbox len box* #f))

    (define (%boxify-struct/default-printer stru)
      ;;We want a prettyfication as follows:
      ;;
      ;;   (define-struct duo
      ;;     (one two))
      ;;
      ;;   (pretty-print (make-duo 1 2))
      ;;   -> (struct duo
      ;;        (one 1)
      ;;        (two 2))
      ;;
      (let* ((std          (struct-rtd stru))
	     (field-name*  (struct-type-field-names std))
	     (instance?    (or (not (eq? std (base-rtd)))
			       (record-type-descriptor? std)
			       (record-constructor-descriptor? std))))
	(receive (field-box* field-sep*)
	    (%boxify-struct-fields stru
				   (if instance? 0 1)
				   (if instance? field-name* (cdr field-name*)))
	  (let* ((struct-box	(%boxify-object (if instance? 'struct 'struct-type)))
		 ;;The return value of STRUCT-NAME is a string.
		 (type-name-box	(if instance?
				    (struct-name stru)
				  (struct-ref stru 0)))
		 (box*		(cons* struct-box type-name-box field-box*))
		 (sep*		(cons* 0 ;separator between struct-box and type-name-box
				       (pretty-indent) ;separator between type-name-box and field
				       field-sep*))
		 (len		(fold-left (lambda (len box)
					     (+ len (box-length box)))
				  0 box*)))
	    (make-cbox (+ 2 len) (list "(" (make-fbox len box* sep*) ")"))))))

    (define (%boxify-struct-fields stru field-idx field-name*)
      ;;Non-tail recursive function.   Boxify the fields and return 2  values: a list
      ;;of  boxes  representing  the  fields,  a list  of  fixnums  representing  the
      ;;separators.
      ;;
      (if (pair? field-name*)
	  ;;First we do the next field...
	  (let ((box (let* ((box1 (%boxify-object (car field-name*)))
			    (box2 (%boxify-object (struct-ref stru field-idx)))
			    (len  (+ (box-length box1) (box-length box2)))
			    (box  (make-fbox len (list box1 box2) #f)))
		       (make-cbox (+ 2 len) (list "(" box ")"))))
		(sep (pretty-indent)))
	    ;;...  then we recurse  to process the rest of the  fields.  This way the
	    ;;shared marks "#N" are introduced correctly.
	    (receive (field-box* field-sep*)
		(%boxify-struct-fields stru (fxadd1 field-idx) (cdr field-name*))
	      (values (cons box field-box*)
		      (cons sep field-sep*))))
	(values '() '())))

    #| end of module: %BOXIFY-OBJECT-STRUCT |# )

;;; --------------------------------------------------------------------

  (module (%boxify-object-record)

    (define (%boxify-object-record x)
      (let ((b (hashtable-ref marks-table x #f)))
	(if (pair? b)
	    (%boxify-record/custom-printer (cdr b))
	  (%boxify-record/default-printer x))))

    (define (%boxify-record/custom-printer cache-stack)
      ;;Boxify a record object that makes use of a custom printer function.
      ;;
      (import writer::TRAVERSAL-HELPERS)
      (define pair*
	(let recur ((cache (cdr cache-stack)))
	  (if cache
	      (let ((pair* (recur (cache-next cache))))
		(cons (cons (cache-string cache)
			    (%boxify-object (cache-object cache)))
		      pair*))
	    '())))
      (define box*
	(fold-left (lambda (box* pair)
		     (if (string-empty? (car pair))
			 (cons (cdr pair) ;the boxification of the OBJECT field
			       box*)
		       (cons* (car pair) ;the STRING field
			      (cdr pair) ;the boxification of the OBJECT field
			      box*)))
	  (list (car cache-stack))
	  pair*))
      (define len
	(fold-left (lambda (ac box)
		     (+ 1 ac (box-length box)))
	  -1 box*))
      (make-fbox len box* #f))

    (define (%boxify-record/default-printer reco)
      ;;We want a prettyfication as follows:
      ;;
      ;;   (define-record-type duo
      ;;     (fields one two))
      ;;
      ;;   (pretty-print (make-duo 1 2))
      ;;   -> (record duo
      ;;        (one 1)
      ;;        (two 2))
      ;;
      ;;Here we want to extract the RTD from both opaque and non-opaque records.
      (let* ((rtd		($struct-rtd reco))
	     (field-name*	(vector->list (record-type-all-field-names rtd))))
	(receive (field-box* field-sep*)
	    (%boxify-record-fields reco 0 field-name*)
	  (let* ((record-box	(%boxify-object 'record))
		 (type-name-box	(symbol->string (record-type-name rtd)))
		 (box*		(cons* record-box type-name-box field-box*))
		 (sep*		(cons* 0 ;separator between record-box and type-name-box
				       (pretty-indent) ;separator between type-name-box and field
				       field-sep*))
		 (len		(fold-left (lambda (len box)
					     (+ len (box-length box)))
				  0 box*)))
	    (make-cbox (+ 2 len) (list "(" (make-fbox len box* sep*) ")"))))))

    (define (%boxify-record-fields reco field-idx field-name*)
      ;;Non-tail recursive function.   Boxify the fields and return 2  values: a list
      ;;of  boxes  representing  the  fields,  a list  of  fixnums  representing  the
      ;;separators.
      ;;
      (if (pair? field-name*)
	  ;;First we do the next field...
	  (let ((box (let* ((box1 (%boxify-object (car field-name*)))
			    (box2 (%boxify-object (records::record-ref reco field-idx)))
			    (len  (+ (box-length box1) (box-length box2)))
			    (box  (make-fbox len (list box1 box2) #f)))
		       (make-cbox (+ 2 len) (list "(" box ")"))))
		(sep (pretty-indent)))
	    ;;...  then we recurse  to process the rest of the  fields.  This way the
	    ;;shared marks "#N" are introduced correctly.
	    (receive (field-box* field-sep*)
		(%boxify-record-fields reco (fxadd1 field-idx) (cdr field-name*))
	      (values (cons box field-box*)
		      (cons sep field-sep*))))
	(values '() '())))

    #| end of module: %BOXIFY-OBJECT-record |# )

;;; --------------------------------------------------------------------
;;; helpers

  (define (%concatenate-into-cbox . string/box*)
    ;;STRING/BOX* must  be a list of  string objects or  BOX structs (any of  the box
    ;;structs).  Build and return a new CBOX struct holding the given arguments.
    ;;
    (let ((len (let loop ((item*      string/box*)
			  (accum-len  0))
		 (if (pair? item*)
		     (loop (cdr item*)
			   (fx+ accum-len (box-length (car item*))))
		   accum-len))))
      (make-cbox len string/box*)))

  (define (%gensep*-default ls)
    ;;Non-tail recursive  function.  Generate a  list of separators  to be used  in a
    ;;FBOX.  Build and  return a new list of  fixnums, one item for each  item in LS.
    ;;Each fixnum is the value returned by "(pretty-indent)".
    ;;
    (let ((D (cdr ls)))
      (if (pair? D)
	  (cons (pretty-indent) (%gensep*-default D))
	'())))

  (define* (graphed? x)
    (import writer::TRAVERSAL-HELPERS)
    (let ((b (get-writer-marks-bitfield __who__ marks-table x)))
      (cond ((writer-marks-bitfield.cyclic-set? b)	#t)
	    ((writer-marks-bitfield.shared-set? b)	(print-graph))
	    (else					#f))))

  (define (unshared-list? x)
    ;;Return true if  X is a non-empty list  and all its cdrs are  not-shared and not
    ;;part of a cyclic  compound.  Notice that the collected items  can be shared, it
    ;;is the spine of the list (the pairs) that must not be shared.
    ;;
    (and (pair? x)
	 (let loop ((D (cdr x)))
	   (or (null? D)
	       (and (pair? D)
		    (not (graphed? D))
		    (loop (cdr D)))))))

  (define* (boxify-shared x boxify-kont)
    (import writer::TRAVERSAL-HELPERS)
    (let ((b (get-writer-marks-bitfield __who__ marks-table x)))
      (cond ((writer-marks-bitfield.mark-set? b)
	     (string-append "#" (fixnum->string (writer-marks-bitfield.decode-mark b)) "#"))

	    ((or (writer-marks-bitfield.cyclic-set? b)
		 (and (writer-marks-bitfield.shared-set? b)
		      (print-graph)))
	     (let ((n (begin0
			  ;;Remember that SHARED-IDX is  a local syntactic binding in
			  ;;the outer function BOXIFY.
			  shared-idx
			(set! shared-idx (add1 shared-idx)))))
	       (writer-marks-table.set-mark! marks-table x n)
	       (let* ((str  (string-append "#" (fixnum->string n) "="))
		      (xbox (boxify-kont x)))
		 (make-cbox (+ (string-length str)
			       (box-length xbox))
			    (list str xbox)))))

	    (else
	     (boxify-kont x)))))

;;; --------------------------------------------------------------------

  (%boxify-object x))


(define (output x port start-column)
  ;;Display the box X to the textual output port PORT, starting at START-COLUMN.
  ;;
  ;;The function %OUTPUT-BOX is the printer of any box.
  ;;
  (define (%output-box x port col)
    ;;Print the string representation of X  to the textual output port PORT, starting
    ;;with an  indentation at column  COL.  Return  a fixnum representing  the column
    ;;index after the representation has been written.
    ;;
    (cond ((string? x)
	   (display x port)
	   (fx+ col (string-length x)))
	  ((cbox? x)   (output-cbox x port col))
	  ((pbox? x)   (output-pbox x port col))
	  ((vbox? x)   (output-vbox x port col))
	  ((fbox? x)   (output-fbox x port col))
	  (else
	   (assertion-violation 'pretty-print-output
	     "internal error: invalid argument" x))))

;;; --------------------------------------------------------------------

  (define (output-cbox x port col)
    (fold-left (lambda (col box)
		 (%output-box box port col))
      col (cbox-boxes x)))

;;; --------------------------------------------------------------------

  (module (output-pbox)

    (define (output-pbox x port col)
      ;;Output the pair object X to PORT starting at column COL.
      ;;
      (if (fx<= (fx+ col (pbox-length x))
		(pretty-width))
	  ;;The whole representation fits into  the current line without crossing the
	  ;;maximum width.
	  (%pbox-one-line x port col)
	;;To avoid  crossing the  maximum width: the  whole representation  must span
	;;multiple lines.
	(%pbox-multi-fill x port col)))

    (define (%pbox-one-line x port col)
      (display "(" port)
      (let loop ((ls		(pbox-ls x))
		 (port		port)
		 (col		(fxadd1 col))
		 (last-box	(pbox-last x)))
	(cond ((null? ls)
	       (let* ((col (begin
			     (display ". " port)
			     (fx+ col 2)))
		      (col (%output-box last-box port col)))
		 (display ")" port)
		 (fxadd1 col)))
	      (else
	       (let ((col (%output-box (car ls) port col)))
		 (display " " port)
		 (loop (cdr ls) port (fxadd1 col) last-box))))))

    (define (%pbox-multi-fill x port col)
      (display "(" port)
      (let g ((ls		(cdr (pbox-ls x)))
	      (port		port)
	      (start-col	(fxadd1 col))
	      (col		(%output-box (car (pbox-ls x)) port (fxadd1 col)))
	      (last-box		(pbox-last x)))
	(cond ((null? ls)
	       (let ((n (box-length last-box)))
		 (let ((col (cond ((fx<= (fx+ (fx+ col n) 4) (pretty-width))
				   (display " . " port)
				   (fx+ col 3))
				  (else
				   (%open-new-line-write-indentation start-col port)
				   (display ". " port)
				   (fx+ start-col 2)))))
		   (let ((col (%output-box last-box port col)))
		     (display ")" port)
		     (fxadd1 col)))))
	      ((fx<= (fx+ (fxadd1 col) (box-length (car ls)))
		     (pretty-width))
	       (display " " port)
	       (g (cdr ls) port start-col
		  (%output-box (car ls) port (fxadd1 col))
		  last-box))
	      (else
	       (%open-new-line-write-indentation start-col port)
	       (g (cdr ls) port start-col
		  (%output-box (car ls) port start-col)
		  last-box)))))

    #| end of module: OUTPUT-PBOX |# )

;;; --------------------------------------------------------------------

  (define (output-vbox x port col)
    ;;Print a vector or bytevector.  Return  a fixnum representing the current column
    ;;after the representation has been written.
    ;;
    ;;The prefix is either "#" or "#vu8".
    (display (vbox-prefix x) port)
    (let ((ls	(vbox-ls x))
	  (col	(+ col (string-length (vbox-prefix x)))))
      (if (pair? ls)
	  ;;The bytevector is not empty.
	  (begin
	    (display "(" port)
	    (let loop ((ls	(cdr ls))
		       (port	port)
		       (col	(%output-box (car ls) port (fxadd1 col)))
		       ;;If we  wrap around and  start a new  line: we begin  the new
		       ;;line with an indentation that  puts the next char one column
		       ;;after the open paren.
		       ;;
		       ;;   #vu8(1 2 3
		       ;;        4 5 6
		       ;;        7 8 9)
		       ;;
		       ;;   #(1 2 3 4
		       ;;     5 6 7 8
		       ;;     9)
		       ;;
		       (start	(fxadd1 col)))
	      (cond ((null? ls)
		     ;;No more objects to write.
		     (display ")" port)
		     (fxadd1 col))

		    ((fx<= (fx+ (fxadd1 col)
				(box-length (car ls)))
			   (pretty-width))
		     ;;More objects  to write  and we  are not  yet past  the maximum
		     ;;column.
		     (display " " port)
		     (loop (cdr ls) port
			   (%output-box (car ls) port (fxadd1 col))
			   start))

		    (else
		     ;;More objects to write and we are past the maximum column.
		     (%open-new-line-write-indentation start port)
		     (loop (cdr ls) port
			   (%output-box (car ls) port start)
			   start)))))
	;;The bytevector is empty.
	(begin
	  (display "()" port)
	  (fx+ col 2)))))

;;; --------------------------------------------------------------------

  (module (output-fbox)
    ;;FIXME This code needs a full review.  (Marco Maggi; Thu Aug 27, 2015)

    (define (output-fbox x port col)
      (let* ((box*		(fbox-box* x))
	     (box		(car box*))
	     (box*		(cdr box*))
	     (sep*		(fbox-sep* x))
	     (left-margin-col	col))
	(if (fx<= (fx+ (box-length box) left-margin-col)
		  (pretty-width))
	    ;;The  string representation  of  BOX  fits into  a  single line  without
	    ;;crossing the maximum column.
	    (let ((col (%output-box box port left-margin-col)))
	      (output-rest-cont box* sep* port col left-margin-col))
	  ;;The  string representation  of  BOX  must span  multiple  lines to  avoid
	  ;;crossing the maximum column.
	  (let ((col (%output-box box port left-margin-col)))
	    (output-rest-multi box* sep* port col left-margin-col)))))

    (define (output-rest-cont box* sep* port col left-margin-col)
      (cond ((null? box*)
	     col)
	    ((pair? sep*)
	     (let* ((box	(car box*))
		    (sep	(car sep*))
		    (w		(box-length box)))
	       (cond ((fx<= (fx+ (fxadd1 w) col) (pretty-width))
		      (display " " port)
		      (output-rest-cont (cdr box*) (cdr sep*) port
					(%output-box box port (fxadd1 col)) left-margin-col))
		     ((not sep)
		      (display " " port)
		      (output-rest-multi (cdr box*) (cdr sep*) port
					 (%output-box box port (fxadd1 col)) left-margin-col))
		     (else
		      (let ((col (fx+ left-margin-col sep)))
			(%open-new-line-write-indentation col port)
			(cond ((fx<= (fx+ w col) (pretty-width))
			       (output-rest-cont (cdr box*) (cdr sep*) port
						 (%output-box box port col) left-margin-col))
			      (else
			       (output-rest-multi (cdr box*) (cdr sep*) port
						  (%output-box box port col) left-margin-col))))))))
	    (else
	     (output-last-cont box* sep* port col left-margin-col))))

    (define (output-last-cont box* sep port col left-margin-col)
      (define (sum ls)
	(if (null? ls)
	    0
	  (fx+ (box-length (car ls))
	       (fxadd1 (sum (cdr ls))))))

      (cond ((not sep)
	     (output-rest-cont box* '(#f . #f) port col left-margin-col))
	    ((fx<= (fx+ (sum box*) col) (pretty-width))
	     (let g ((box* box*)
		     (port port)
		     (col  col))
	       (if (null? box*)
		   col
		 (begin
		   (display " " port)
		   (g (cdr box*) port (%output-box (car box*) port (fxadd1 col)))))))
	    (else
	     (let g ((box* box*)
		     (port port)
		     (left-margin-col (fx+ left-margin-col sep))
		     (col  col))
	       (if (null? box*)
		   col
		 (begin
		   (%open-new-line-write-indentation left-margin-col port)
		   (g (cdr box*) port left-margin-col
		      (%output-box (car box*) port left-margin-col))))
	       ))))

    (define (output-last-multi box* sep port col left-margin-col)
      (if (not sep)
	  (output-rest-multi box* '(#f . #f) port col left-margin-col)
	(let g ((box* box*)
		(port port)
		(left-margin-col (fx+ left-margin-col sep))
		(col  col))
	  (if (null? box*)
	      col
	    (begin
	      (%open-new-line-write-indentation left-margin-col port)
	      (g (cdr box*) port left-margin-col
		 (%output-box (car box*) port left-margin-col))))
	  )))

    (define (output-rest-multi box* sep* port col left-margin-col)
      (cond ((null? box*)
	     col)
	    ((pair? sep*)
	     (let* ((box	(car box*))
		    (sep	(car sep*))
		    (w		(box-length box)))
	       (cond ((not sep)
		      (display " " port)
		      (output-rest-multi (cdr box*) (cdr sep*) port
					 (%output-box box port (fxadd1 col)) left-margin-col))
		     (else
		      (let ((col (fx+ left-margin-col sep)))
			(%open-new-line-write-indentation col port)
			(if (fx<= (fx+ w col)
				  (pretty-width))
			    (output-rest-cont (cdr box*) (cdr sep*) port
					      (%output-box box port col) left-margin-col)
			  (output-rest-multi (cdr box*) (cdr sep*) port
					     (%output-box box port col) left-margin-col)))
		      ))))
	    (else
	     (output-last-multi box* sep* port col left-margin-col))))

    #| end of module: OUTPUT-FBOX |# )

;;; --------------------------------------------------------------------
;;; helpers

  (define (%open-new-line-write-indentation col port)
    ;;Write the indentation at the beginning of a new line.  Write a newline to PORT,
    ;;then print COL space characters to PORT.  COL must be a non-negative fixnum.
    ;;
    (newline port)
    (let loop ((col  col)
	       (port port))
      (unless (fxzero? col)
	(put-char port #\space)
	(loop (fxsub1 col) port))))

;;; --------------------------------------------------------------------

  (%output-box x port start-column))


;;;; done

#| end of file |# )

;;; end of file
