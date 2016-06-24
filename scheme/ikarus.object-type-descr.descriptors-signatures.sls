;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: object-type descriptors signatures
;;;Date: Sun Jun  5, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module
    ( ;;
     <descriptors-signature>
     <descriptors-signature>-rtd		<descriptors-signature>-rcd
     make-descriptors-signature			descriptors-signature?
     descriptors-signature.object-type-descrs
     descriptors-signature=?
     descriptors-signature.super-and-sub?
     descriptors-signature.match-formals-against-operands

     <lambda-descriptors>
     <lambda-descriptors>-rtd			<lambda-descriptors>-rcd
     make-lambda-descriptors			lambda-descriptors?		not-empty-list-of-lambda-descriptors?
     lambda-descriptors.retvals			lambda-descriptors.argvals
     lambda-descriptors=?
     lambda-descriptors.super-and-sub?
     lambda-descriptors.match-formals-against-operands
     select-most-specific-lambda-descriptors

     <case-lambda-descriptors>
     <case-lambda-descriptors>-rtd		<case-lambda-descriptors>-rcd
     make-case-lambda-descriptors		case-lambda-descriptors?
     case-lambda-descriptors.clause-signature*
     case-lambda-descriptors=?
     case-lambda-descriptors.super-and-sub?
     case-lambda-descriptors.match-formals-against-operands

     #| end of exports |# )


;;;; object-type descriptor signatures

(define-record-type (<descriptors-signature> make-descriptors-signature descriptors-signature?)
  (nongenerative vicare:type-descriptors:<descriptors-signature>)
  (sealed #t)
  (fields
    (immutable object-type-descrs	descriptors-signature.object-type-descrs)
		;A proper  or improper  list of object-type  descriptors representing
		;the signatures of lambda formals.
    #| end of FIELDS |# ))

(define <descriptors-signature>-rtd
  (record-type-descriptor <descriptors-signature>))

(define <descriptors-signature>-rcd
  (record-constructor-descriptor <descriptors-signature>))

;;; --------------------------------------------------------------------

(define* (descriptors-signature=? {S1 descriptors-signature?} {S2 descriptors-signature?})
  (let loop ((descrs1	(descriptors-signature.object-type-descrs S1))
	     (descrs2	(descriptors-signature.object-type-descrs S2)))
    (cond ((pair? descrs1)
	   (and (pair? descrs2)
		(object-type-descr=? (car descrs1) (car descrs2))
		(loop (cdr descrs1) (cdr descrs2))))
	  ((pair? descrs2)
	   #f)
	  (else
	   (object-type-descr=? descrs1 descrs2)))))


;;;; descriptors signatures for lambda procedures

(define-record-type (<lambda-descriptors> make-lambda-descriptors lambda-descriptors?)
  (nongenerative vicare:type-descriptors:<lambda-descriptors>)
  (sealed #t)
  (fields
    (immutable retvals	lambda-descriptors.retvals)
		;An instance of  "<descriptors-signature>" representing the signature
		;of the return values.
    (immutable argvals	lambda-descriptors.argvals)
		;An instance of  "<descriptors-signature>" representing the signature
		;of the formal values.
    #| end of FIELDS |# ))

(define <lambda-descriptors>-rtd
  (record-type-descriptor <lambda-descriptors>))

(define <lambda-descriptors>-rcd
  (record-constructor-descriptor <lambda-descriptors>))

;;; --------------------------------------------------------------------

(define (not-empty-list-of-lambda-descriptors? obj)
  (and (pair? obj)
       (and (lambda-descriptors? (car obj))
	    (let loop ((obj (cdr obj)))
	      (if (pair? obj)
		  (and (lambda-descriptors? (car obj))
		       (loop (cdr obj)))
		(null? obj))))))

;;; --------------------------------------------------------------------

(define* (lambda-descriptors=? {D1 lambda-descriptors?} {D2 lambda-descriptors?})
  (and (descriptors-signature=? (lambda-descriptors.retvals D1)
				(lambda-descriptors.retvals D2))
       (descriptors-signature=? (lambda-descriptors.argvals D1)
				(lambda-descriptors.argvals D2))))

(define* (lambda-descriptors.super-and-sub? {super.des lambda-descriptors?} {sub.des lambda-descriptors?})
  ;;Compare two closure's clauses type signatures to determine if they are super-type
  ;;and sub-type.  Return a boolean, true if they are super and sub.
  ;;
  ;;This happens when a  formal argument (super.des) must be a closure  and the operand (D2)
  ;;is a closure.  Example:
  ;;
  ;;   (define (fun {D2 (lambda (<fixnum>) => (<number>))})
  ;;     ---)
  ;;
  ;;   (define ({D2 <fixnum>} {A <number>})
  ;;     ---)
  ;;
  ;;   (type-of D2)	=> (lambda (<number>) => (<fixnum>))
  ;;   (fun D2)
  ;;
  ;;We want:
  ;;
  ;;* The formal's argvals to be sub-types of the operands's argvals.
  ;;
  ;;* The formal's retvals to be super-types of the operand's retvals.
  ;;
  ;;Like this:
  ;;
  ;;	   (super-and-sub? (lambda (<fixnum>) => (<number>))
  ;;	                   (lambda (<number>) => (<fixnum>)))	=> #t
  ;;
  ;;	   (super-and-sub? (lambda (<number>) => (<string>))
  ;;	                   (lambda (<fixnum>) => (<string>)))	=> #f
  ;;
  ;;	   (super-and-sub? (lambda (<string>) => (<fixnum>))
  ;;	                   (lambda (<string>) => (<number>)))	=> #f
  ;;
  (and (descriptors-signature.super-and-sub? (lambda-descriptors.retvals super.des) (lambda-descriptors.retvals   sub.des))
       (descriptors-signature.super-and-sub? (lambda-descriptors.argvals   sub.des) (lambda-descriptors.argvals super.des))))

(define* (lambda-descriptors.match-formals-against-operands {formals.des lambda-descriptors?} {operands.des descriptors-signature?})
  ;;Compare  formals' and  operands' type  signatures to  determine if  the closure's
  ;;clause represented by "<lambda-descriptors>" matches the operands.  Return one of
  ;;the symbols: exact-match, possible-match, no-match.
  ;;
  ;;In a function application, we want the function's arguments to be super-types and
  ;;the operands to be sub-types.  We do not care about the return values' types.
  ;;
  ;;   (match-formals-against-operands?
  ;;      (lambda (<number> <struct>) => (<string>))
  ;;      (<fixnum> <record>))				=> exact-match
  ;;
  ;;   (match-formals-against-operands?
  ;;      (lambda (<fixnum> <struct>) => (<string>))
  ;;      (<number> <record>))				=> possible-match
  ;;
  ;;   (match-formals-against-operands?
  ;;      (lambda (<fixnum> <struct>) => (<string>))
  ;;      (<string> <record>))				=> no-match
  ;;
  (descriptors-signature.match-formals-against-operands (lambda-descriptors.argvals formals.des) operands.des))

;;; --------------------------------------------------------------------

(define (select-most-specific-lambda-descriptors closure-entry* rands.sig)
  ;;This   function  is   used  to   determine   the  specialised   function  in   an
  ;;overloaded-function which is most specific for a tuple of operands.
  ;;
  ;;CLOSURE-ENTRY* is an alist having instances of "<lambda-descriptors>" as keys and
  ;;procedures  as values.   RANDS.SIG  is an  instance of  "<descriptors-signature>"
  ;;representing the  types of  the operands.   The return value  is the  alist entry
  ;;which is the most specific matching for the operands.
  ;;
  ;;We  want the  less specific  arguments  to be  super-types of  the most  specific
  ;;arguments; we do not care about the return values.
  ;;
  ;;	   (less-and-most-specific?
  ;;		(lambda (<number>) => (<string>))
  ;;	        (lambda (<fixnum>) => (<string>)))	=> #t
  ;;
  (fold-left
      (lambda (selected-entry entry)
	;;ENTRY is  a pair having  an instance of  <closure-type-descr> as car  and a
	;;procedure as cdr.   SELECTED-ENTRY is false or a pair  with the same format
	;;of ENTRY.
	(if (eq? 'exact-match
		 (lambda-descriptors.match-formals-against-operands (car entry) rands.sig))
	    (if selected-entry
		(if (lambda-descriptors.super-and-sub? (car selected-entry) (car entry))
		    entry
		  selected-entry)
	      entry)
	  selected-entry))
    #f closure-entry*))


;;;; descriptors signatures for case-lambda procedures

(define-record-type (<case-lambda-descriptors> make-case-lambda-descriptors case-lambda-descriptors?)
  (nongenerative vicare:type-descriptors:<case-lambda-descriptors>)
  (sealed #t)
  (fields
    (immutable clause-signature*	case-lambda-descriptors.clause-signature*)
		;A   list  of   "<lambda-descriptors>"  instances   representing  the
		;signatures of the clauses.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (lambda* ({clause*.sig not-empty-list-of-lambda-descriptors?})
	(make-record clause*.sig))))
  #| end of DEFINE-RECORD-TYPE |# )

(define <case-lambda-descriptors>-rtd
  (record-type-descriptor <case-lambda-descriptors>))

(define <case-lambda-descriptors>-rcd
  (record-constructor-descriptor <case-lambda-descriptors>))

;;; --------------------------------------------------------------------

(define* (case-lambda-descriptors=? {D1 case-lambda-descriptors?} {D2 case-lambda-descriptors?})
  (let super-loop ((clause1*.des (case-lambda-descriptors.clause-signature* D1))
		   (clause2*.des (case-lambda-descriptors.clause-signature* D2)))
    (if (pair? clause1*.des)
	(let sub-loop ((clause2*.des	clause2*.des)
		       (leftover2*.des	'()))
	  (if (pair? clause2*.des)
	      (if (lambda-descriptors=? (car clause1*.des) (car clause2*.des))
		  ;;We discard this CLAUSE2.  Go to the outer loop with the leftovers
		  ;;as CLAUSE2*.
		  (super-loop (cdr clause1*.des) (append (cdr clause2*.des) leftover2*.des))
		;;We add this CLAUSE2 to the leftovers.
		(sub-loop (cdr clause2*.des) (cons (car clause2*.des) leftover2*.des)))
	    ;;There are more CLAUSE2, but no more CLAUSE1.
	    #f))
      ;;There are no more CLAUSE1: are there more CLAUSE2?.
      (null? clause2*.des))))

(define* (case-lambda-descriptors.super-and-sub? {super.des case-lambda-descriptors?} {sub.des case-lambda-descriptors?})
  ;;Compare two  closure's type signatures  to determine  if they are  super-type and
  ;;sub-type.  Return a boolean, true if they are super and sub.  For every clause in
  ;;the super there must be a matching clause in the sub.
  ;;
  (let ((super-clause-signature*	(case-lambda-descriptors.clause-signature* super.des))
	(sub-clause-signature*		(case-lambda-descriptors.clause-signature*   sub.des)))
    (for-all (lambda (super-clause-signature)
	       (exists (lambda (sub-clause-signature)
			 (lambda-descriptors.super-and-sub? super-clause-signature sub-clause-signature))
		 sub-clause-signature*))
      super-clause-signature*)))

;;; --------------------------------------------------------------------

(define* (case-lambda-descriptors.match-formals-against-operands {formals.des case-lambda-descriptors?} {operands.des descriptors-signature?})
  ;;Compare formals' and operands' type signatures to determine if a closure's clause
  ;;exists  that matches  the  operands.   Return one  of  the symbols:  exact-match,
  ;;possible-match, no-match.
  ;;
  (returnable
    (fold-left (lambda (state formals.clause-des)
		 (case (lambda-descriptors.match-formals-against-operands formals.clause-des operands.des)
		   ((exact-match)
		    (return 'exact-match))
		   ((possible-match)
		    'possible-match)
		   (else
		    state)))
      'no-match (case-lambda-descriptors.clause-signature* formals.des))))


;;;; syntax helpers: object-type descriptors matching

(define-auxiliary-syntaxes
  <list> <null> <nelist>
  <list>/<list-of-descr>
  <list-of-descr> <list-descr> <pair-descr>)

(define-syntax (case-descriptors-signature-structure input-form.stx)
  (define (main input-form.stx)
    (syntax-case input-form.stx ()
      ((_ ?object-type-descrs . ?clause*)
       (with-syntax
	   (((CLAUSE ...)	(%parse-clauses #'?clause*)))
	 #'(let ((object-type-descrs ?object-type-descrs))
	     (cond CLAUSE ...))))
      ))

  (define (%parse-clauses clause*.stx)
    (syntax-case clause*.stx ()
      (()
       '())
      ((?clause0 . ?other-clauses)
       (cons (%parse-single-clause #'?clause0)
	     (%parse-clauses       #'?other-clauses)))
      (_
       (syntax-violation __who__ "invalid syntax in input clauses" input-form.stx clause*.stx))))

  (define (%parse-single-clause clause.stx)
    (syntax-case clause.stx (else => null? <null>
				  pair? <pair-descr>
				  <list> <nelist> <list-of-descr> <list-descr>
				  <list>/<list-of-descr>)
      ((else . ?body)
       clause.stx)

      ((null? . ?body)
       #'((null? object-type-descrs) . ?body))

      ((<null> => ?expr)
       #'((<null>-ctd? object-type-descrs) (?expr object-type-descrs)))
      ((<null> . ?body)
       #'((<null>-ctd? object-type-descrs) . ?body))

      ((pair? => ?expr)
       #'((pair? object-type-descrs) (?expr object-type-descrs)))
      ((pair? . ?body)
       #'((pair? object-type-descrs) . ?body))

      ((<pair-descr> => ?expr)
       #'((pair-type-descr? object-type-descrs) (?expr object-type-descrs)))
      ((<pair-descr> . ?body)
       #'((pair-type-descr? object-type-descrs) . ?body))

      ((<list> => ?expr)
       #'((<list>-ctd? object-type-descrs) (?expr object-type-descrs)))
      ((<list> . ?body)
       #'((<list>-ctd? object-type-descrs) . ?body))

      ((<nelist> => ?expr)
       #'((<nelist>-ctd? object-type-descrs) (?expr object-type-descrs)))
      ((<nelist> . ?body)
       #'((<nelist>-ctd? object-type-descrs) . ?body))

      ((<list-of-descr> => ?expr)
       #'((list-of-type-descr? object-type-descrs) (?expr object-type-descrs)))
      ((<list-of-descr> . ?body)
       #'((list-of-type-descr? object-type-descrs) . ?body))

      ((<list-descr> => ?expr)
       #'((list-type-descr? object-type-descrs) (?expr object-type-descrs)))
      ((<list-descr> . ?body)
       #'((list-type-descr? object-type-descrs) . ?body))

      ((<list>/<list-of-descr> => ?expr)
       #'((or (<list>-ctd?     object-type-descrs)
	      (list-of-type-descr? object-type-descrs))
	  (?expr object-type-descrs)))
      ((<list>/<list-of-descr> . ?body)
       #'((or (<list>-ctd?     object-type-descrs)
	      (list-of-type-descr? object-type-descrs))
	  . ?body))

      (_
       (syntax-violation __who__ "invalid syntax in input clauses" input-form.stx clause.stx))))

  (main input-form.stx))

(define-syntax case-descriptors-signature-structure*
  ;;This is like CASE-DESCRIPTORS-SIGNATURE-STRUCTURE but it has all the clauses.
  ;;
  (syntax-rules (else null? pair?
		      <pair-descr> <null>
		      <list> <nelist> <list-of-descr> <list-descr>)
    ((_ ?specs
	(pair? . ?body-pair)
	(<pair-descr> . ?body-<pair-descr>)
	(null? . ?body-null)
	(<null> . ?body-<null>)
	(<list> . ?body-<list>)
	(<list-of-descr> . ?body-<list-of-descr>)
	(<nelist> . ?body-<nelist>)
	(<list-descr> . ?body-<list-descr>)
	(else . ?body-else))
     (case-descriptors-signature-structure ?specs
       (pair? . ?body-pair)
       (<pair-descr> . ?body-<pair-descr>)
       (null? . ?body-null)
       (<null> . ?body-<null>)
       (<list> . ?body-<list>)
       (<list-of-descr> . ?body-<list-of-descr>)
       (<nelist> . ?body-<nelist>)
       (<list-descr> . ?body-<list-descr>)
       (else . ?body-else)))
    ))


;;;; matching super-type and sub-type

(define* (descriptors-signature.super-and-sub? {formals.sig	descriptors-signature?}
					       {operands.sig	descriptors-signature?})
  ;;Return true if FORMALS.SIG and OPERANDS.SIG  have the same structure and the type
  ;;in  the homologous  positions  are matching  super-type  and sub-type;  otherwise
  ;;return false.
  ;;
  (define-syntax-rule (super-and-sub? ?A ?B)
    (object-type-descr.matching-super-and-sub? ?A ?B))
  (define caller-who __who__)
  (let  ((formals.specs		(descriptors-signature.object-type-descrs formals.sig))
	 (operands.specs	(descriptors-signature.object-type-descrs operands.sig)))
    (cond
     ((<bottom>-ctd? formals.specs)
      (<bottom>-ctd? operands.specs))
     ((<bottom>-ctd? operands.specs)
      #f)
     (else
      (let recur ((formals.specs	formals.specs)
		  (operands.specs	operands.specs))
	(case-descriptors-signature-structure* formals.specs
	  (pair?
	   (case-descriptors-signature-structure operands.specs
	     (pair?
	      ;;If the formal is actually a super-type of the operand: good.
	      (and (super-and-sub? (car formals.specs) (car operands.specs))
		   (recur (cdr formals.specs) (cdr operands.specs))))
	     (<pair-descr>
	      (and (super-and-sub? (car formals.specs) (pair-type-descr.car-des operands.specs))
		   (recur (cdr formals.specs) (pair-type-descr.cdr-des operands.specs))))
	     (<list-descr>
	      ;;Splice the operand OTSs.
	      (recur formals.specs (list-type-descr.item-des* operands.specs)))
	     (else
	      ;;It is possible that there are more formals than operands.  Examples:
	      ;;
	      ;;  formals.sig  == (<number>  <fixnum> <string)
	      ;;  operands.sig == (<complex> <fixnum> . ())
	      ;;
	      ;;  formals.sig  == (<number>  <fixnum> <string)
	      ;;  operands.sig == (<complex> <fixnum> . <list>)
	      ;;
	      ;;  formals.sig  == (<number>  <fixnum> <string)
	      ;;  operands.sig == (<complex> <fixnum> . <null>)
	      ;;
	      ;;we want these cases to fail matching, in this function.
	      #f)))

	  (<pair-descr>
	   (let ((formal-car.des (pair-type-descr.car-des formals.specs))
		 (formal-cdr.des (pair-type-descr.cdr-des formals.specs)))
	     (case-descriptors-signature-structure operands.specs
	       (pair?
		;;If the formal is actually a super-type of the operand good.
		(and (super-and-sub? formal-car.des (car operands.specs))
		     (recur formal-cdr.des (cdr operands.specs))))
	       (<pair-descr>
		(and (super-and-sub? formal-car.des (pair-type-descr.car-des operands.specs))
		     (recur formal-cdr.des (pair-type-descr.cdr-des operands.specs))))
	       (<list-descr>
		(let ((operand-item*.des (list-type-descr.item-des* operands.specs)))
		  (case-descriptors-signature-structure operand-item*.des
		    (pair?
		     (and (super-and-sub? formal-car.des (car operand-item*.des))
			  ;;Splice the operand OTSs.
			  (recur formal-cdr.des (cdr operand-item*.des))))
		    (else
		     #f))))
	       (else #f))))

	  (null?
	   ;;Return true if both the signatures are proper lists with the same number
	   ;;of items, and all the items are correct super and sub.
	   (case-descriptors-signature-structure operands.specs
	     (null?	#t)
	     (<null>	#t)
	     (else	#f)))

	  (<null>
	   (case-descriptors-signature-structure operands.specs
	     (null?	#t)
	     (<null>	#t)
	     (else	#f)))

	  (<list>
	   ;;The formals signature matches any number operands of any type.
	   #t)

	  (<list-of-descr>
	   ;;The formals  signature matches any  number of operands of  the specified
	   ;;type.  Examples:
	   ;;
	   ;;  formals.sig  == (... . (list-of <number>))
	   ;;  operands.sig == (... . <fixnum>)
	   ;;
	   ;;  formals.sig  == (... . (list-of <string>))
	   ;;  operands.sig == (... . (list-of <string>))
	   ;;
	   ;;  formals.sig  == (... . (list-of <string>))
	   ;;  operands.sig == (... . (list <string>))
	   ;;
	   (let ((formal-item.des (list-of-type-descr.item-des formals.specs)))
	     ;;If the formal's type is "<top>": any type of operands is matched.
	     (or (<top>-ctd? formal-item.des)
		 (let item-recur ((operands.specs operands.specs))
		   (case-descriptors-signature-structure* operands.specs
		     (pair?
		      ;;formals.sig  == (... . (list-of <fixnum>))
		      ;;operands.sig == (... <fixnum> <fixnum>)
		      (and (super-and-sub? formal-item.des (car operands.specs))
			   (item-recur (cdr operands.specs))))

		     (<pair-descr>
		      ;;formals.sig  == (... . (list-of <number>))
		      ;;operands.sig == (... . (pair <fixnum> . ?list-type))
		      (and (super-and-sub? formal-item.des (pair-type-descr.car-des operands.specs))
			   (item-recur (pair-type-descr.cdr-des operands.specs))))

		     (null?
		      ;;formals.sig  == (... . (list-of <string>))
		      ;;operands.sig == (... . ())
		      #t)

		     (<null>
		      ;;formals.sig  == (... . (list-of <string>))
		      ;;operands.sig == (... . <null>)
		      #t)

		     (<list>
		      ;;formals.sig  == (... . (list-of <fixnum>))
		      ;;operands.sig == (... . <list>)
		      #f)

		     (<list-of-descr>
		      ;;formals.sig  == (... . (list-of <number>))
		      ;;operands.sig == (... . (list-of <fixnum>))
		      (super-and-sub? formal-item.des (list-of-type-descr.item-des operands.specs)))

		     (<nelist>
		      ;;formals.sig  == (... . (list-of <fixnum>))
		      ;;operands.sig == (... . <nelist>)
		      #f)

		     (<list-descr>
		      ;;formals.sig  == (... . (list-of <number>))
		      ;;operands.sig == (... . (list <fixnum>))
		      (item-recur (list-type-descr.item-des* operands.specs)))

		     (else
		      (assertion-violation caller-who "invalid operands signature" operands.sig)))))))

	  (<nelist>
	   ;;The formals signature matches one or more operands of any type.
	   (case-descriptors-signature-structure operands.specs
	     (pair?		#t)
	     (<nelist>		#t)
	     (<list-descr>	#t)
	     (<pair-descr>	#t)
	     (else		#f)))

	  (<list-descr>
	   ;;Splice the formals OTSs.
	   (recur (list-type-descr.item-des* formals.specs) operands.specs))

	  (else
	   (assertion-violation caller-who "invalid formals signature" formals.sig))))))))


;;;; matching formals and operands

(module (descriptors-signature.match-formals-against-operands)

  (define-syntax-rule (matching-super-and-sub? ?A ?B)
    (object-type-descr.matching-super-and-sub? ?A ?B))

  (define-syntax-rule (compatible-super-and-sub? ?A ?B)
    (object-type-descr.compatible-super-and-sub? ?A ?B))

  (define* (descriptors-signature.match-formals-against-operands formals.sig operands.sig)
    ;;In the context of a closure object application to fixed operands:
    ;;
    ;;   (?operator ?operand ...)
    ;;
    ;;compare the type signature of the operator's arguments to the type signature of
    ;;the  given  operands.   Return  a symbol  among:  exact-match,  possible-match,
    ;;no-match.
    ;;
    ;;FORMALS.SIG  is  a  "<descriptors-signature>" instance  representing  the  type
    ;;signature of a closure object's clause arguments.
    ;;
    ;;OPERANDS.SIG  is a  "<descriptors-signature>"  instance  representing the  type
    ;;signature of the given operands.
    ;;
    (define the-who __who__)
    (define (%error-invalid-formals-signature)
      (assertion-violation the-who "invalid formals signature" formals.sig))
    (define (%error-invalid-operands-signature)
      (assertion-violation the-who "invalid operands signature" operands.sig))
    (let loop ((state		'exact-match)
	       (formals.descrs	(descriptors-signature.object-type-descrs formals.sig))
	       (operands.descrs	(descriptors-signature.object-type-descrs operands.sig)))
      ;;In  this loop  the  variable  STATE always  degrades:  from "exact-match"  to
      ;;"possible-match"  or "no-match";  from  "possible-match"  to "no-match".   It
      ;;never upgrades.
      (case-descriptors-signature-structure* formals.descrs
	;;The operator accepts one more mandatory operand.
	(pair?
	 (%match-formals-pair-against-operands loop state
					       (car formals.descrs) (cdr formals.descrs)
					       operands.descrs
					       %error-invalid-formals-signature %error-invalid-operands-signature))
	(<pair-descr>
	 (%match-formals-pair-against-operands loop state
					       (pair-type-descr.car-des formals.descrs) (pair-type-descr.cdr-des formals.descrs)
					       operands.descrs
					       %error-invalid-formals-signature %error-invalid-operands-signature))

	;;The operator accepts no more operands.
	(null?
	 (%match-formals-null-against-operands state operands.descrs %error-invalid-operands-signature))
	(<null>
	 (%match-formals-null-against-operands state operands.descrs %error-invalid-operands-signature))

	(<list>
	 ;;The operator accepts zero or more operands of any type.
	 ;;
	 ;;   formals.sig  == (... . <list>)
	 ;;   operands.sig == (... ?type ...)
	 ;;
	 ;;Good.  And we are done here, let's return the final state.
	 state)

	(<list-of-descr>
	 ;;The operator accepts zero or more operands of a known type.
	 ;;
	 ;;   formals.sig  == (... . (list-of ?type))
	 ;;   operands.sig == (... ?type ...)
	 ;;
	 (%match-formals-list-of-against-operands state (list-of-type-descr.item-des formals.descrs) operands.descrs
						  %error-invalid-operands-signature))

	(<nelist>
	 ;;The operator accepts one or more operands of any type.
	 (case-descriptors-signature-structure* operands.descrs
	   ;;There is at least one more operand.  Good.
	   (pair?		state)
	   (<pair-descr>		state)
	   ;;No more operands.  Bad.
	   (null?		'no-match)
	   (<null>		'no-match)
	   ;;There is an unspecified number of rest operands.
	   (<list>		'possible-match)
	   (<list-of-descr>	'possible-match)
	   ;;There are one or more operands.
	   (<nelist>		state)
	   (<list-descr>		state)
	   (else
	    (%error-invalid-formals-signature))))

	(<list-descr>
	 ;;The operator  accepts a known  number of  operands, of known  type.  Let's
	 ;;splice the specifications.
	 (loop state (list-type-descr.item-des* formals.descrs) operands.descrs))

	(else
	 (%error-invalid-formals-signature)))))

;;; --------------------------------------------------------------------

  (module (%match-formals-pair-against-operands)

    (define (%match-formals-pair-against-operands loop state
						  formals-car.des formals-cdr.descrs operands.descrs
						  %error-invalid-formals-signature %error-invalid-operands-signature)
      (case-descriptors-signature-structure* operands.descrs
	(pair?
	 (%match-formals-pair-against-operands-pair loop state
						    formals-car.des formals-cdr.descrs
						    (car operands.descrs) (cdr operands.descrs)))
	(<pair-descr>
	 (%match-formals-pair-against-operands-pair loop state
						    formals-car.des formals-cdr.descrs
						    (pair-type-descr.car-des operands.descrs)
						    (pair-type-descr.cdr-des operands.descrs)))
	;;At least one more formal and no more operands.  Bad.
	(null?			'no-match)
	(<null>			'no-match)
	;;There is an unspecified number of operands, of unspecified type.
	(<list>			'possible-match)
	;;There is an unspecified number of operands, of known type.
	(<list-of-descr>
	 (%match-formals-pair-against-operands-list-of formals-car.des formals-cdr.descrs
						       (list-of-type-descr.item-des operands.descrs)
						       %error-invalid-formals-signature))
	;;There is at least one more operand, of unknown type.
	(<nelist>		'possible-match)
	;;There  is a  known number  of operands,  of known  type.  Let's  splice the
	;;operands.
	(<list-descr>
	 (%match-formals-pair-against-operands loop state
					       formals-car.des formals-cdr.descrs
					       (list-type-descr.item-des* operands.descrs)
					       %error-invalid-formals-signature %error-invalid-operands-signature))
	(else
	 (%error-invalid-operands-signature))))

    (define (%match-formals-pair-against-operands-pair loop state
						       formals-car.des formals-cdr.descrs
						       operands-car.des operands-cdr.descrs)
      (cond ((matching-super-and-sub? formals-car.des operands-car.des)
	     (loop state formals-cdr.descrs operands-cdr.descrs))
	    ((compatible-super-and-sub? formals-car.des operands-car.des)
	     (loop 'possible-match formals-cdr.descrs operands-cdr.descrs))
	    (else
	     'no-match)))

    (module (%match-formals-pair-against-operands-list-of)

      (define (%match-formals-pair-against-operands-list-of formals-car.des formals-cdr.descrs operand.des
							    %error-invalid-formals-signature)
	;;This       is      a       mutually      recursive       function      with
	;;%MATCH-FORMALS-AGAINST-OPERANDS-LIST-OF.
	;;
	(cond ((matching-super-and-sub? formals-car.des operand.des)
	       (%match-formals-against-operands-list-of formals-cdr.descrs operand.des %error-invalid-formals-signature))
	      ((compatible-super-and-sub? formals-car.des operand.des)
	       (%match-formals-against-operands-list-of formals-cdr.descrs operand.des %error-invalid-formals-signature))
	      (else 'no-match)))

      (define (%match-formals-against-operands-list-of formals.descrs operand.des %error-invalid-formals-signature)
	;;This  is   a  recursive  function   a  mutually  recursive   function  with
	;;%MATCH-FORMALS-PAIR-AGAINST-OPERANDS-LIST-OF.   The  operator accepts  more
	;;arguments of  a specified type and  there is an unspecified  number of rest
	;;operands of known type.
	;;
	;;   formals.sig  == (... ?type ...)
	;;   operands.sig == (... . (list-of ?type))
	;;
	;;This  function returns  a  symbol among:  possible-match, no-match.   Exact
	;;match is already excluded.
	;;
	;;The   argument   FORMALS.DESCRS   is   a  proper   or   improper   list   of
	;;"<object-type-descr>" representing the requested type for all rest operands.
	;;The argument  OPERAND.DES is an "<object-type-descr>"  instance representing
	;;the type of all the given rest operands.
	;;
	(case-descriptors-signature-structure* formals.descrs
	  ;;At least one more formal.
	  (pair?
	   (%match-formals-pair-against-operands-list-of (car formals.descrs) (cdr formals.descrs) operand.des
							 %error-invalid-formals-signature))
	  (<pair-descr>
	   (%match-formals-pair-against-operands-list-of (pair-type-descr.car-des formals.descrs)
							 (pair-type-descr.cdr-des formals.descrs)
							 operand.des
							 %error-invalid-formals-signature))
	  ;;No more formals.
	  (null?			'possible-match)
	  (<null>			'possible-match)

	  ;;There is an unspecified number of formals, of unspecified type.
	  (<list>			'possible-match)

	  ;;There is an unspecified number of formals, with a known type.
	  (<list-of-descr>
	   (let ((formal.des (list-of-type-descr.item-des formals.descrs)))
	     (cond ((matching-super-and-sub? formal.des operand.des)
		    'possible-match)
		   ((compatible-super-and-sub? formal.des operand.des)
		    'possible-match)
		   (else 'no-match))))

	  ;;There is at least one more formals, of unspecified type.
	  (<nelist>		'possible-match)

	  ;;There  is a  known number  of  formals, of  known type.   Let's splice  the
	  ;;specifications.
	  (<list-descr>
	   (%match-formals-against-operands-list-of (list-type-descr.item-des* formals.descrs)
						    operand.des %error-invalid-formals-signature))

	  (else
	   (%error-invalid-formals-signature))))

      #| end of module: %MATCH-FORMALS-PAIR-AGAINST-OPERANDS-LIST-OF |# )

    #| end of module: %MATCH-FORMALS-PAIR-AGAINST-OPERANDS |# )

;;; --------------------------------------------------------------------

  (define (%match-formals-null-against-operands state operands.descrs %error-invalid-operands-signature)
    (case-descriptors-signature-structure* operands.descrs
      ;;No more arguments and leftover operands.  Bad.
      (pair?		'no-match)
      (<pair-descr>	'no-match)

      ;;No more arguments and  no more operands.  Good.  And we  are done here, let's
      ;;return the final state.
      (null?		state)
      (<null>		state)

      ;;There may be other operands.
      (<list>		'possible-match)
      (<list-of-descr>	'possible-match)

      ;;There is at least one other operand.  Bad.
      (<nelist>		'no-match)
      (<list-descr>	'no-match)

      (else
       (%error-invalid-operands-signature))))

;;; --------------------------------------------------------------------

  (module (%match-formals-list-of-against-operands)

    (define (%match-formals-list-of-against-operands state formal.des operands.descrs %error-invalid-operands-signature)
      ;;Recursive function.  We  use this function when the operator  accepts zero or
      ;;more operands of a specified type.
      ;;
      ;;   formals.sig  == (... . (list-of ?type))
      ;;   operands.sig == (... ?type ...)
      ;;
      ;;The argument  FORMAL.DES is an instance  of "<object-type-descr>" representing
      ;;the requested type for all the rest operands.  The argument OPERANDS.DESCRS is
      ;;a proper or improper list  of "<object-type-descr>" instances representing the
      ;;types of the given rest operands.
      ;;
      (case-descriptors-signature-structure* operands.descrs
	;;At least one more operand.  Let's match it against the argument.
	(pair?
	 (%match-formals-list-of-against-operands-pair state formal.des (car operands.descrs) (cdr operands.descrs)
						       %error-invalid-operands-signature))
	(<pair-descr>
	 (%match-formals-list-of-against-operands-pair state formal.des
						       (pair-type-descr.car-des operands.descrs)
						       (pair-type-descr.cdr-des operands.descrs)
						       %error-invalid-operands-signature))
	;;No more operands.  Good.
	(null?		state)
	(<null>		state)

	;;There is an unspecified number of rest operands, with unspecified type.
	(<list>		'possible-match)

	;;There is an unspecified number of rest operands, with a known type.
	(<list-of-descr>
	 (let ((operand.des (list-of-type-descr.item-des operands.descrs)))
	   (cond ((matching-super-and-sub? formal.des operand.des)
		  state)
		 ((compatible-super-and-sub? formal.des operand.des)
		  'possible-match)
		 (else 'no-match))))

	;;There is at least one more operand, with unspecified type.
	(<nelist>	'possible-match)

	;;There is  a known number  of operands, with  known type.  Let's  splice the
	;;specifications.
	(<list-descr>
	 (%match-formals-list-of-against-operands state formal.des (list-type-descr.item-des* operands.descrs)
						  %error-invalid-operands-signature))

	(else
	 (%error-invalid-operands-signature))))

    (define (%match-formals-list-of-against-operands-pair state formal.des operands-car.des operands-cdr.descrs
							  %error-invalid-operands-signature)
      (cond ((matching-super-and-sub? formal.des operands-car.des)
	     (%match-formals-list-of-against-operands state formal.des operands-cdr.descrs
						      %error-invalid-operands-signature))
	    ((compatible-super-and-sub? formal.des operands-car.des)
	     (%match-formals-list-of-against-operands 'possible-match formal.des operands-cdr.descrs
						      %error-invalid-operands-signature))
	    (else
	     ;;The formal is INcompatible with the operand.  Bad.
	     'no-match)))

    #| end of module: %MATCH-FORMALS-LIST-OF-AGAINST-OPERANDS |# )

  #| end of module: DESCRIPTORS-SIGNATURE.MATCH-FORMALS-AGAINST-OPERANDS |# )


;;;; done

#| end of module |# )

;;; end of file
