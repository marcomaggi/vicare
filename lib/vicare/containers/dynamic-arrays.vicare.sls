;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: dynamic vectors
;;;Date: Wed Aug 12, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare containers dynamic-arrays)
  (options typed-language)
  (export
    <dynamic-array>			dynamic-array
    make-dynamic-array			dynamic-array?

    dynamic-array-hash			$dynamic-array-hash
    dynamic-array-putprop		$dynamic-array-putprop
    dynamic-array-getprop		$dynamic-array-getprop
    dynamic-array-remprop		$dynamic-array-remprop
    dynamic-array-property-list	$dynamic-array-property-list

    dynamic-array-empty?		$dynamic-array-empty?
    dynamic-array-not-empty?		$dynamic-array-not-empty?
    dynamic-array-length		$dynamic-array-length

    (rename
     (dynamic-array-length		dynamic-array-size)
     ($dynamic-array-length		$dynamic-array-size))

    dynamic-array-ref			$dynamic-array-ref
    dynamic-array-set!			$dynamic-array-set!
    dynamic-array-insert!		$dynamic-array-insert!
    dynamic-array-remove!		$dynamic-array-remove!

    dynamic-array-front			$dynamic-array-front
    dynamic-array-rear			$dynamic-array-rear
    dynamic-array-push-front!		$dynamic-array-push-front!
    dynamic-array-push-rear!		$dynamic-array-push-rear!
    dynamic-array-pop-front!		$dynamic-array-pop-front!
    dynamic-array-pop-rear!		$dynamic-array-pop-rear!
    dynamic-array-purge!		$dynamic-array-purge!

    dynamic-array-fold-left		$dynamic-array-fold-left
    dynamic-array-fold-right		$dynamic-array-fold-right

    dynamic-array-map-left		$dynamic-array-map-left
    dynamic-array-map-right		$dynamic-array-map-right
    dynamic-array-for-each-left		$dynamic-array-for-each-left
    dynamic-array-for-each-right	$dynamic-array-for-each-right

    (rename
     (dynamic-array-map-left		dynamic-array-map)
     ($dynamic-array-map-left		$dynamic-array-map)
     (dynamic-array-for-each-left	dynamic-array-for-each)
     ($dynamic-array-for-each-left	$dynamic-array-for-each))

    dynamic-array-find-left		$dynamic-array-find-left
    dynamic-array-find-right		$dynamic-array-find-right
    dynamic-array-for-all		$dynamic-array-for-all
    dynamic-array-exists-left		$dynamic-array-exists-left
    dynamic-array-exists-right		$dynamic-array-exists-right

    (rename
     (dynamic-array-find-left		dynamic-array-find)
     ($dynamic-array-find-left		$dynamic-array-find)
     (dynamic-array-exists-left		dynamic-array-exists)
     ($dynamic-array-exists-left	$dynamic-array-exists))

    dynamic-array-filter		$dynamic-array-filter
    dynamic-array-partition		$dynamic-array-partition

    dynamic-array-copy!			$dynamic-array-copy!
    dynamic-array-reverse!		$dynamic-array-reverse!

    dynamic-array->list			list->dynamic-array
    dynamic-array->vector		vector->dynamic-array

    make-dynamic-array-front-iteration-thunk
    make-dynamic-array-rear-iteration-thunk)
  (import (vicare)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $vectors))


;;;; helpers

(define-syntax-rule (declare-operation-unary ?safe ?unsafe)
  (define* (?safe {V dynamic-array?})
    (?unsafe V)))

(define* (%enlarged-vector-length old-len)
  ;;Compute  the size  of  the enlarged  Scheme  vector length.   OLD-LEN  must be  a
  ;;non-negative fixnum representing the number of slots in the old Scheme vector.
  ;;
  ;;If OLD-LEN is less than 1024 slots: duplicate the length.
  ;;
  ;;If  OLD-LEN is  greater than,  or equal  to, 1024  slots: set  the new  length to
  ;;OLD-LEN + 1024.  Accept this new length if it is less than the greatest fixnum.
  ;;
  ;;If OLD-LEN equals the greater fixnum: raise an exception.
  ;;
  ;;Otherwise return the greatest fixnum.
  ;;
  (cond (($fx< old-len 1024)
	 ($fx* 2 old-len))
	((let ((delta 1024))
	   (if ($fx< old-len ($fx- (greatest-fixnum) delta))
	       ($fx+ old-len delta)
	     #f)))
	(($fx= old-len (greatest-fixnum))
	 (assertion-violation __who__ "cannot enlarge vector more than the greatest fixnum"))
	(else
	 (greatest-fixnum))))

(define ($vector-reset-range! vec start past)
  (when ($fx< start past)
    ($vector-set! vec start '#!void)
    ($vector-reset-range! vec ($fxadd1 start) past)))


;;;; type definition

(define-constant DEFAULT-DYNAMIC-ARRAY-LENGTH	15)
(define-constant DEFAULT-DYNAMIC-LEFT-SPAN	(fxdiv DEFAULT-DYNAMIC-ARRAY-LENGTH 2))

(define-record-type (<dynamic-array> make-dynamic-array dynamic-array?)
  (nongenerative vicare:containers:<dynamic-array>)

  (fields (mutable data)
		;The payload vector.
	  (mutable fir-idx)
		;A non-negative fixnum representing the index of the first used slot.
		;If this field is equal to PAS-IDX: the vector is empty.
	  (mutable pas-idx)
		;A non-negative  fixnum representing one  plus the index of  the last
		;used slot.
	  (mutable uid))
		;False or a unique gensym associated to this instance.

  (protocol
   (lambda (make-record)
     (case-define* make-<dynamic-array>
       (()
	(make-<dynamic-array> DEFAULT-DYNAMIC-ARRAY-LENGTH DEFAULT-DYNAMIC-LEFT-SPAN))

       (({full-length non-negative-fixnum?})
	(let ((left-span (div full-length 2)))
	  (make-record (make-vector full-length (void))
		       left-span left-span #f)))

       (({full-length non-negative-fixnum?} {left-span non-negative-fixnum?})
	(if (fx<=? left-span full-length)
	    (make-record (make-vector full-length (void))
			 left-span left-span #f)
	  (assertion-violation 'make-<dynamic-array>
	    "left span must be less than or equal to the full length"
	    full-length left-span)))

       #| end of CASE-DEFINE* |# )

     make-<dynamic-array>))

  #| end of DEFINE-RECORD-TYPE |# )

(define (dynamic-array . obj*)
  (receive-and-return (V)
      (make-dynamic-array)
    (for-each-in-order (lambda (obj)
			 ($dynamic-array-push-rear! V obj))
      obj*)))


;;;; UID stuff

(declare-operation-unary dynamic-array-hash	$dynamic-array-hash)

(define ($dynamic-array-hash Q)
  (unless ($<dynamic-array>-uid Q)
    ($<dynamic-array>-uid-set! Q (gensym)))
  (symbol-hash ($<dynamic-array>-uid Q)))

;;; --------------------------------------------------------------------

(define* (dynamic-array-putprop {Q dynamic-array?} {key symbol?} value)
  ($dynamic-array-putprop Q key value))

(define ($dynamic-array-putprop Q key value)
  (unless ($<dynamic-array>-uid Q)
    ($<dynamic-array>-uid-set! Q (gensym)))
  (putprop ($<dynamic-array>-uid Q) key value))

;;; --------------------------------------------------------------------

(define* (dynamic-array-getprop {Q dynamic-array?} {key symbol?})
  ($dynamic-array-getprop Q key))

(define ($dynamic-array-getprop Q key)
  (unless ($<dynamic-array>-uid Q)
    ($<dynamic-array>-uid-set! Q (gensym)))
  (getprop ($<dynamic-array>-uid Q) key))

;;; --------------------------------------------------------------------

(define* (dynamic-array-remprop {Q dynamic-array?} {key symbol?})
  ($dynamic-array-remprop Q key))

(define ($dynamic-array-remprop Q key)
  (unless ($<dynamic-array>-uid Q)
    ($<dynamic-array>-uid-set! Q (gensym)))
  (remprop ($<dynamic-array>-uid Q) key))

;;; --------------------------------------------------------------------

(define* (dynamic-array-property-list {Q dynamic-array?})
  ($dynamic-array-property-list Q))

(define ($dynamic-array-property-list Q)
  (unless ($<dynamic-array>-uid Q)
    ($<dynamic-array>-uid-set! Q (gensym)))
  (property-list ($<dynamic-array>-uid Q)))


;;;; inspection

(declare-operation-unary dynamic-array-length	$dynamic-array-length)

(define ($dynamic-array-length arry)
  ($fx- ($<dynamic-array>-pas-idx  arry)
	($<dynamic-array>-fir-idx arry)))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array-empty?	$dynamic-array-empty?)

(define ($dynamic-array-empty? arry)
  ($fx= ($<dynamic-array>-fir-idx arry)
	($<dynamic-array>-pas-idx arry)))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array-not-empty?	$dynamic-array-not-empty?)

(define ($dynamic-array-not-empty? arry)
  ($fx< ($<dynamic-array>-fir-idx arry)
	($<dynamic-array>-pas-idx arry)))


;;;; accessors and mutators

(define* (dynamic-array-ref {arry dynamic-array?} {i non-negative-fixnum?})
  ($dynamic-array-ref arry i))

(define ($dynamic-array-ref arry i)
  (let* ((fir-idx ($<dynamic-array>-fir-idx arry))
	 (idx     (fx+ i fir-idx))
	 (pas-idx ($<dynamic-array>-pas-idx arry)))
    (if ($fx< idx pas-idx)
	($vector-ref ($<dynamic-array>-data arry) idx)
      (assertion-violation __who__ "index out of range" arry i))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-set! {arry dynamic-array?} {i non-negative-fixnum?} obj)
  ($dynamic-array-set! arry i obj))

(define ($dynamic-array-set! arry i obj)
  (let* ((fir-idx ($<dynamic-array>-fir-idx arry))
	 (idx     (fx+ i fir-idx))
	 (pas-idx ($<dynamic-array>-pas-idx arry)))
    (if ($fx< idx pas-idx)
	($vector-set! ($<dynamic-array>-data arry) idx obj)
      (assertion-violation __who__ "index out of range" arry i))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-insert! {arry dynamic-array?} {i fixnum?} obj)
  ($dynamic-array-insert! arry i obj))

(define ($dynamic-array-insert! arry i obj)
  (if ($fx= -1 i)
      ($dynamic-array-push-front! arry obj)
    (let* ((fir-idx ($<dynamic-array>-fir-idx arry))
	   (pas-idx ($<dynamic-array>-pas-idx arry))
	   (obj-idx ($fx+ fir-idx i)))
      (if ($fx= pas-idx obj-idx)
	  ($dynamic-array-push-rear! arry obj)
	(if (or ($fx<  obj-idx fir-idx)
		($fx>= obj-idx pas-idx))
	    (assertion-violation __who__ "index out of range" arry i)
	  (let* ((data           ($<dynamic-array>-data arry))
		 (data.len       ($vector-length data))
		 (room-at-left   fir-idx)
		 (room-at-right  ($fx- data.len pas-idx))
		 (room-at-left+  ($fxpositive? room-at-left))
		 (room-at-right+ ($fxpositive? room-at-right)))

	    (define (%move-leftist-and-set!)
	      ;;Move leftist used slots towards the left.  Before:
	      ;;
	      ;;             0   1   2   3   4
	      ;;   |---|---|+++|+++|+++|+++|+++|---|---|
	      ;;             ^       ^           ^
	      ;;         fir-idx  obj-idx     pas-idx
	      ;;
	      ;;after:
	      ;;
	      ;;         0   1   2   3   4   5
	      ;;   |---|+++|+++|+++|---|+++|+++|---|---|
	      ;;         ^           ^           ^
	      ;;      fir-idx     obj-idx     pas-idx
	      ;;
	      #;(debug-print __who__)
	      (let ((fir-idx.new ($fxsub1 fir-idx))
		    (move-count  ($fx- obj-idx fir-idx)))
		#;(debug-print fir-idx fir-idx.new move-count)
		($vector-self-copy-forwards! data fir-idx fir-idx.new move-count)
		($<dynamic-array>-fir-idx-set! arry fir-idx.new)
		($vector-set! data ($fxsub1 obj-idx) obj)))

	    (define (%move-rightist-and-set!)
	      ;;Move rightist used slots towards the right.  Before:
	      ;;
	      ;;             0   1   2   3   4
	      ;;   |---|---|+++|+++|+++|+++|+++|---|---|
	      ;;             ^       ^           ^
	      ;;         fir-idx  obj-idx     pas-idx
	      ;;
	      ;;after:
	      ;;
	      ;;             0   1   2   3   4   5
	      ;;   |---|---|+++|+++|---|+++|+++|+++|---|
	      ;;             ^       ^               ^
	      ;;         fir-idx  obj-idx         pas-idx
	      ;;
	      #;(debug-print __who__)
	      (let ((pas-idx.new ($fxadd1 pas-idx)))
		($vector-self-copy-backwards! data pas-idx pas-idx.new ($fx- pas-idx obj-idx))
		($<dynamic-array>-pas-idx-set! arry pas-idx.new))
	      ($vector-set! data obj-idx obj))

	    (cond ((and room-at-left+ room-at-right+)
		   (if ($fx> room-at-left room-at-right)
		       ;;More room at left.
		       (%move-leftist-and-set!)
		     ;;More room at right or equal room.
		     (%move-rightist-and-set!)))

		  (room-at-left+
		   (%move-leftist-and-set!))

		  (room-at-right+
		   (%move-rightist-and-set!))

		  (else
		   ;;No room.
		   (assert (fx=? (vector-length data)
				 (fx- pas-idx fir-idx)))
		   #;(debug-print __who__ 'full)
		   (let* ((data.new-len  (%enlarged-vector-length data.len))
			  (data.new      (make-vector data.new-len))
			  (fir-idx.new   ($fxdiv ($fx- data.new-len data.len) 2))
			  (pas-idx.new   ($fxadd1 ($fx+ fir-idx.new data.len)))
			  (obj-idx.new   ($fx+ fir-idx.new i)))
		     ($vector-copy-source-range! data fir-idx obj-idx data.new fir-idx.new)
		     ($vector-copy-source-range! data obj-idx pas-idx data.new ($fxadd1 obj-idx.new))
		     ($<dynamic-array>-data-set!    arry data.new)
		     ($<dynamic-array>-fir-idx-set! arry fir-idx.new)
		     ($<dynamic-array>-pas-idx-set! arry pas-idx.new)
		     ($vector-set! data.new obj-idx.new obj))))))))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-remove! {arry dynamic-array?} {i non-negative-fixnum?})
  ($dynamic-array-remove! arry i))

(define ($dynamic-array-remove! arry i)
  (let* ((fir-idx ($<dynamic-array>-fir-idx arry))
	 (pas-idx ($<dynamic-array>-pas-idx arry))
	 (obj-idx ($fx+ fir-idx i)))
    (if (or ($fx<  obj-idx fir-idx)
	    ($fx>= obj-idx pas-idx))
	(assertion-violation __who__ "index out of range" arry i)
      (let ((data ($<dynamic-array>-data arry)))
	(cond (($fx= obj-idx fir-idx)
	       ($vector-set! data fir-idx (void))
	       ($<dynamic-array>-fir-idx-set! arry ($fxadd1 fir-idx)))

	      (($fx= obj-idx ($fxsub1 pas-idx))
	       ($vector-set! data obj-idx (void))
	       ($<dynamic-array>-pas-idx-set! arry ($fxsub1 pas-idx)))

	      (else
	       (let* ((used-at-left   ($fx- obj-idx fir-idx))
		      (used-at-right  ($fx- pas-idx obj-idx)))
		 (if ($fx< used-at-left used-at-right)
		     ;;Less used slots at the left.
		     (begin
		       ($vector-self-copy-backwards! data obj-idx ($fxadd1 obj-idx) used-at-left)
		       ($vector-set! data fir-idx (void))
		       ($<dynamic-array>-fir-idx-set! arry ($fxadd1 fir-idx)))
		   ;;Less used slots at the right.
		   (let ((pas-idx.new ($fxsub1 pas-idx)))
		     ($vector-self-copy-forwards! data ($fxadd1 obj-idx) obj-idx ($fxsub1 used-at-right))
		     ($vector-set! data pas-idx.new (void))
		     ($<dynamic-array>-pas-idx-set! arry pas-idx.new))))))))))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array-front	$dynamic-array-front)

(define ($dynamic-array-front arry)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry)))
    (if ($fx< fir-idx pas-idx)
	($vector-ref ($<dynamic-array>-data arry) fir-idx)
      (assertion-violation __who__ "the container is empty" arry))))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array-rear	$dynamic-array-rear)

(define ($dynamic-array-rear arry)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry)))
    (if ($fx< fir-idx pas-idx)
	($vector-ref ($<dynamic-array>-data arry) ($fxsub1 pas-idx))
      (assertion-violation __who__ "the container is empty" arry))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-push-front! {arry dynamic-array?} obj)
  ($dynamic-array-push-front! arry obj))

(define ($dynamic-array-push-front! arry obj)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry))
	(data    ($<dynamic-array>-data    arry)))
    (cond (($fxpositive? fir-idx)
	   ;;There is room at the left of the data area.
	   (let ((fir-idx ($fxsub1 fir-idx)))
	     ($vector-set! data fir-idx obj)
	     ($<dynamic-array>-fir-idx-set! arry fir-idx)))
	  (($fx< pas-idx ($vector-length data))
	   ;;There is  room at the right  of the data  area.  Move used slots  to the
	   ;;right.
	   (assert (fxzero? fir-idx))
	   (let* ((data.len     ($vector-length data))
		  (size         ($fx- pas-idx fir-idx))
		  ;;Notice that when:
		  ;;
		  ;;   DATA.LEN = PAS-IDX + 1
		  ;;
		  ;;we have:
		  ;;
		  ;;   ($fxdiv ($fx- data.len pas-idx) 2) => 0
		  ;;
		  ;;for this reason we add 1.
		  (pas-idx.new  ($fx+ pas-idx ($fxadd1 ($fxdiv ($fx- data.len pas-idx) 2))))
		  (fir-idx.new  ($fxsub1 ($fx- pas-idx.new size))))

	     ($vector-self-copy-backwards! data pas-idx pas-idx.new size)
	     ($vector-reset-range! data fir-idx fir-idx.new)
	     ($<dynamic-array>-fir-idx-set! arry fir-idx.new)
	     ($<dynamic-array>-pas-idx-set! arry pas-idx.new)
	     ($vector-set! data fir-idx.new obj)))
	  (else
	   ;;There is no room.
	   (assert (fx=? (vector-length data)
			 (fx- pas-idx fir-idx)))
	   (let* ((data.old-len  ($vector-length data))
		  (data.new-len  (%enlarged-vector-length data.old-len))
		  (data.new      (make-vector data.new-len))
		  (fir-idx.new   ($fxdiv ($fx- data.new-len data.old-len) 2))
		  (fir-idx+1.new ($fxadd1 fir-idx.new))
		  (pas-idx.new   ($fx+ fir-idx+1.new data.old-len)))
	     ($vector-copy-source-range! data fir-idx pas-idx data.new fir-idx+1.new)
	     ($<dynamic-array>-data-set!    arry data.new)
	     ($<dynamic-array>-fir-idx-set! arry fir-idx.new)
	     ($<dynamic-array>-pas-idx-set! arry pas-idx.new)
	     ($vector-set! data.new fir-idx.new obj))))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-push-rear! {arry dynamic-array?} obj)
  ($dynamic-array-push-rear! arry obj))

(define ($dynamic-array-push-rear! arry obj)
  (let* ((fir-idx   ($<dynamic-array>-fir-idx arry))
	 (pas-idx   ($<dynamic-array>-pas-idx arry))
	 (data      ($<dynamic-array>-data    arry))
	 (data.len  ($vector-length data)))
    (cond (($fx< pas-idx data.len)
	   ;;There is room at the right of the data area.
	   ($vector-set! data pas-idx obj)
	   ($<dynamic-array>-pas-idx-set! arry ($fxadd1 pas-idx)))
	  (($fxpositive? fir-idx)
	   ;;There is  room at the  left of  the data area.   Move used slots  to the
	   ;;left.
	   (let* ((size         ($fx- pas-idx fir-idx))
		  (fir-idx.new  ($fxdiv fir-idx 2))
		  (las-idx.new  ($fx+ fir-idx.new size))
		  (pas-idx.new  ($fxadd1 las-idx.new)))
	     ($vector-self-copy-forwards! data fir-idx fir-idx.new size)
	     ($vector-reset-range! data pas-idx.new pas-idx)
	     ($<dynamic-array>-fir-idx-set! arry fir-idx.new)
	     ($<dynamic-array>-pas-idx-set! arry pas-idx.new)
	     ($vector-set! data las-idx.new obj)))
	  (else
	   ;;There is no room.
	   (assert (fx=? (vector-length data)
			 (fx- pas-idx fir-idx)))
	   (let* ((data.old-len  ($vector-length data))
		  (data.new-len  (%enlarged-vector-length data.old-len))
		  (data.new      (make-vector data.new-len))
		  (fir-idx.new   ($fxdiv ($fx- data.new-len data.old-len) 2))
		  (las-idx.new   ($fx+ fir-idx.new data.old-len))
		  (pas-idx.new   ($fxadd1 las-idx.new)))
	     ($vector-copy-source-range! data fir-idx pas-idx data.new fir-idx.new)
	     ($<dynamic-array>-data-set!    arry data.new)
	     ($<dynamic-array>-fir-idx-set! arry fir-idx.new)
	     ($<dynamic-array>-pas-idx-set! arry pas-idx.new)
	     ($vector-set! data.new las-idx.new obj))))))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array-pop-front!	$dynamic-array-pop-front!)

(define ($dynamic-array-pop-front! arry)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry)))
    (if ($fx< fir-idx pas-idx)
	(begin0
	    ($vector-ref ($<dynamic-array>-data arry) fir-idx)
	  ($<dynamic-array>-fir-idx-set! arry ($fxadd1 fir-idx)))
      (assertion-violation __who__ "the container is empty" arry))))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array-pop-rear!	$dynamic-array-pop-rear!)

(define ($dynamic-array-pop-rear! arry)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry)))
    (if ($fx< fir-idx pas-idx)
	(let ((las-idx ($fxsub1 pas-idx)))
	  (begin0
	      ($vector-ref ($<dynamic-array>-data arry) las-idx)
	    ($<dynamic-array>-pas-idx-set! arry las-idx)))
      (assertion-violation __who__ "the container is empty" arry))))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array-purge!		$dynamic-array-purge!)

(define ($dynamic-array-purge! arry)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry))
	(data    ($<dynamic-array>-data    arry)))
    ($vector-reset-range! data fir-idx pas-idx)
    (let* ((mid ($fxdiv ($vector-length data) 2))
	   (fir mid)
	   (pas mid))
      ($<dynamic-array>-fir-idx-set! arry fir)
      ($<dynamic-array>-pas-idx-set! arry pas))))


;;;; mapping

(define* (dynamic-array-map-left {dst dynamic-array?} {fun procedure?} {src dynamic-array?})
  ($dynamic-array-map-left dst fun src))

(define ($dynamic-array-map-left dst fun src)
  (do ((arry   ($<dynamic-array>-data  src))
       (i      ($<dynamic-array>-fir-idx src) ($fxadd1 i))
       (i.past ($<dynamic-array>-pas-idx src)))
      (($fx= i i.past)
       dst)
    ($dynamic-array-push-rear! dst (fun ($vector-ref arry i)))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-map-right  {dst dynamic-array?} {fun procedure?} {src dynamic-array?})
  ($dynamic-array-map-right dst fun src))

(define ($dynamic-array-map-right dst fun src)
  (do ((data   ($<dynamic-array>-data    src))
       (i.last ($<dynamic-array>-fir-idx src))
       (i      ($fxsub1 ($<dynamic-array>-pas-idx src)) ($fxsub1 i)))
      (($fx< i i.last)
       dst)
    ($dynamic-array-push-front! dst (fun ($vector-ref data i)))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-for-each-left {fun procedure?} {src dynamic-array?})
  ($dynamic-array-for-each-left fun src))

(define ($dynamic-array-for-each-left fun src)
  (do ((arry   ($<dynamic-array>-data    src))
       (i      ($<dynamic-array>-fir-idx src) ($fxadd1 i))
       (i.past ($<dynamic-array>-pas-idx src)))
      (($fx= i i.past))
    (fun ($vector-ref arry i))))

(define* (dynamic-array-for-each-right  {fun procedure?} {src dynamic-array?})
  ($dynamic-array-for-each-right fun src))

(define ($dynamic-array-for-each-right fun src)
  (do ((data   ($<dynamic-array>-data    src))
       (i.last ($<dynamic-array>-fir-idx src))
       (i      ($fxsub1 ($<dynamic-array>-pas-idx src)) ($fxsub1 i)))
      (($fx< i i.last))
    (fun ($vector-ref data i))))


;;;; folding

(define* (dynamic-array-fold-left {kons procedure?} knil {arry dynamic-array?})
  ($dynamic-array-fold-left kons knil arry))

(define ($dynamic-array-fold-left kons knil arry)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry)))
    (let loop ((data ($<dynamic-array>-data arry))
	       (i    fir-idx)
	       (knil knil))
      (if ($fx< i pas-idx)
	  (loop data ($fxadd1 i) (kons knil ($vector-ref data i)))
	knil))))

;;; --------------------------------------------------------------------

(define* (dynamic-array-fold-right {kons procedure?} knil {arry dynamic-array?})
  ($dynamic-array-fold-right kons knil arry))

(define ($dynamic-array-fold-right kons knil arry)
  (let ((fir-idx ($<dynamic-array>-fir-idx arry))
	(pas-idx ($<dynamic-array>-pas-idx arry)))
    (let loop ((data ($<dynamic-array>-data arry))
	       (i    ($fxsub1 pas-idx))
	       (knil knil))
      (if ($fx<= fir-idx i)
	  (loop data ($fxsub1 i) (kons ($vector-ref data i) knil))
	knil))))


;;;; searching

(case-define* dynamic-array-find-left
  (({fun procedure?} {arry dynamic-array?})
   ($dynamic-array-find-left fun arry #f))
  (({fun procedure?} {arry dynamic-array?} not-found-rv)
   ($dynamic-array-find-left fun arry not-found-rv)))

(define ($dynamic-array-find-left fun arry not-found-rv)
  (returnable
    ($dynamic-array-fold-left
	(lambda (knil obj)
	  (if (fun obj)
	      (return obj)
	    knil))
      not-found-rv arry)))

;;; --------------------------------------------------------------------

(case-define* dynamic-array-find-right
  (({fun procedure?} {arry dynamic-array?})
   ($dynamic-array-find-right fun arry #f))
  (({fun procedure?} {arry dynamic-array?} not-found-rv)
   ($dynamic-array-find-right fun arry not-found-rv)))

(define ($dynamic-array-find-right fun arry not-found-rv)
  (returnable
    ($dynamic-array-fold-right
	(lambda (obj knil)
	  (if (fun obj)
	      (return obj)
	    knil))
      not-found-rv arry)))

;;; --------------------------------------------------------------------

(define* (dynamic-array-for-all {fun procedure?} {arry dynamic-array?})
  ($dynamic-array-for-all fun arry))

(define ($dynamic-array-for-all fun arry)
  (returnable
    ($dynamic-array-fold-left
	(lambda (knil obj)
	  (or (fun obj)
	      (return #f)))
      #t arry)))

;;; --------------------------------------------------------------------

(define* (dynamic-array-exists-left {fun procedure?} {arry dynamic-array?})
  ($dynamic-array-exists-left fun arry))

(define ($dynamic-array-exists-left fun arry)
  (returnable
    ($dynamic-array-fold-left
	(lambda (knil obj)
	  (if (fun obj)
	      (return obj)
	    knil))
      #f arry)))

;;; --------------------------------------------------------------------

(define* (dynamic-array-exists-right {fun procedure?} {arry dynamic-array?})
  ($dynamic-array-exists-right fun arry))

(define ($dynamic-array-exists-right fun arry)
  (returnable
    ($dynamic-array-fold-right
	(lambda (obj knil)
	  (if (fun obj)
	      (return obj)
	    knil))
      #f arry)))


;;;; filtering

(define* (dynamic-array-filter {dst-arry dynamic-array?} {fun procedure?} {src-arry dynamic-array?})
  ($dynamic-array-filter dst-arry fun src-arry))

(define ($dynamic-array-filter dst-arry pred src-arry)
  ($dynamic-array-fold-left
      (lambda (dst-arry obj)
	(when (pred obj)
	  ($dynamic-array-push-rear! dst-arry obj))
	dst-arry)
    dst-arry src-arry))

;;; --------------------------------------------------------------------

(define* (dynamic-array-partition {matching-arry dynamic-array?} {not-matching-arry dynamic-array?} {fun procedure?} {src-arry dynamic-array?})
  ($dynamic-array-partition matching-arry not-matching-arry fun src-arry))

(define ($dynamic-array-partition matching-arry not-matching-arry pred src-arry)
  ($dynamic-array-fold-left
      (lambda (knil obj)
	($dynamic-array-push-rear! (if (pred obj)
					(car knil)
				      (cdr knil))
				    obj)
	knil)
    (cons matching-arry not-matching-arry) src-arry)
  (values matching-arry not-matching-arry))



;;;; conversion

(declare-operation-unary dynamic-array->list	$dynamic-array->list)

(define ($dynamic-array->list arry)
  ($dynamic-array-fold-right cons '() arry))

;;; --------------------------------------------------------------------

(declare-operation-unary dynamic-array->vector	$dynamic-array->vector)

(define ($dynamic-array->vector arry)
  (receive-and-return (vec)
      (make-vector ($dynamic-array-length arry))
    ($dynamic-array-fold-left
	(lambda (knil item)
	  (let ((vec ($car knil))
		(idx ($cdr knil)))
	    ($vector-set! vec idx item)
	    ($set-cdr! knil ($fxadd1 idx))
	    knil))
      (cons vec 0)
      arry)))

;;; --------------------------------------------------------------------

(define* (list->dynamic-array {ell list?})
  ($list->dynamic-array (make-dynamic-array) ell))

(define* ($list->dynamic-array dst-arry ell)
  (fold-left (lambda (arry item)
	       ($dynamic-array-push-rear! arry item)
	       arry)
    dst-arry ell))

;;; --------------------------------------------------------------------

(define* (vector->dynamic-array {arry vector?})
  ($vector->dynamic-array (make-dynamic-array) arry))

(define ($vector->dynamic-array dst-arry arry)
  (vector-fold-left (lambda (arry item)
		      ($dynamic-array-push-rear! arry item)
		      arry)
    dst-arry arry))


;;;; miscellaneous operations

(define* (dynamic-array-copy! {dst dynamic-array?} {src dynamic-array?})
  ($dynamic-array-copy! dst src))

(define ($dynamic-array-copy! dst-arry src-arry)
  ($dynamic-array-fold-left
      (lambda (knil obj)
	($dynamic-array-push-rear! dst-arry obj)
	knil)
    dst-arry src-arry))

;;; --------------------------------------------------------------------

(define* (dynamic-array-reverse! {dst dynamic-array?} {src dynamic-array?})
  ($dynamic-array-reverse! dst src))

(define ($dynamic-array-reverse! dst-arry src-arry)
  ($dynamic-array-fold-left
      (lambda (knil obj)
	($dynamic-array-push-front! dst-arry obj)
	knil)
    dst-arry src-arry))


;;;; iteration thunks

(define* (make-dynamic-array-front-iteration-thunk {arry dynamic-array?})
  (let ((arry.idx 0)
	(arry.len ($dynamic-array-length arry)))
    (lambda ()
      (if ($fx< arry.idx arry.len)
	  (receive-and-return (ch)
	      ($dynamic-array-ref arry arry.idx)
	    (set! arry.idx ($fxadd1 arry.idx)))
	(void)))))

(define* (make-dynamic-array-rear-iteration-thunk {arry dynamic-array?})
  (let ((arry.idx ($fxsub1 ($dynamic-array-length arry))))
    (lambda ()
      (if ($fxnonnegative? arry.idx)
	  (receive-and-return (ch)
	      ($dynamic-array-ref arry arry.idx)
	    (set! arry.idx ($fxsub1 arry.idx)))
	(void)))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'dynamic-array-fold-left		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-fold-right		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-map-left		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-map-right		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-for-each-left	'scheme-indent-function 1)
;; eval: (put 'dynamic-array-for-each-right	'scheme-indent-function 1)
;; eval: (put '$dynamic-array-fold-left		'scheme-indent-function 1)
;; eval: (put '$dynamic-array-fold-right	'scheme-indent-function 1)
;; eval: (put '$dynamic-array-map-left		'scheme-indent-function 1)
;; eval: (put '$dynamic-array-map-right		'scheme-indent-function 1)
;; eval: (put '$dynamic-array-for-each-left	'scheme-indent-function 1)
;; eval: (put '$dynamic-array-for-each-right	'scheme-indent-function 1)
;; eval: (put 'dynamic-array-for-all		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-find-left		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-find-right		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-exists		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-exists-left	'scheme-indent-function 1)
;; eval: (put 'dynamic-array-exists-right	'scheme-indent-function 1)
;; eval: (put '$dynamic-array-for-all		'scheme-indent-function 1)
;; eval: (put '$dynamic-array-find-left		'scheme-indent-function 1)
;; eval: (put '$dynamic-array-find-right	'scheme-indent-function 1)
;; eval: (put '$dynamic-array-exists		'scheme-indent-function 1)
;; eval: (put '$dynamic-array-exists-left	'scheme-indent-function 1)
;; eval: (put '$dynamic-array-exists-right	'scheme-indent-function 1)
;; End:
