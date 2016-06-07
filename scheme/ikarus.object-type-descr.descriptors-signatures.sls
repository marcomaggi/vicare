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

     <lambda-descriptors>
     <lambda-descriptors>-rtd			<lambda-descriptors>-rcd
     make-lambda-descriptors			lambda-descriptors?
     lambda-descriptors.retvals			lambda-descriptors.argvals
     lambda-descriptors=?			lambda-descriptors.match-super-and-sub
     not-empty-list-of-lambda-descriptors?

     <case-lambda-descriptors>
     <case-lambda-descriptors>-rtd		<case-lambda-descriptors>-rcd
     make-case-lambda-descriptors		case-lambda-descriptors?
     case-lambda-descriptors.clause-signature*
     case-lambda-descriptors=?			case-lambda-descriptors.match-super-and-sub

     descriptors-signature.matching-super-and-sub?
     descriptors-signature.compatible-super-and-sub?
     descriptors-signature.match-formals-against-operands

     #| end of exports |# )


;;;; object-type descriptor signatures

(define-record-type (<descriptors-signature> make-descriptors-signature descriptors-signature?)
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

(define* (lambda-descriptors=? {D1 lambda-descriptors?} {D2 lambda-descriptors?})
  (and (descriptors-signature=? (lambda-descriptors.retvals D1)
				(lambda-descriptors.retvals D2))
       (descriptors-signature=? (lambda-descriptors.argvals D1)
				(lambda-descriptors.argvals D2))))

(define* (lambda-descriptors.match-super-and-sub {D1 lambda-descriptors?} {D2 lambda-descriptors?})
  (let ((retvals-state (descriptors-signature.match-formals-against-operands (lambda-descriptors.retvals D1)
									     (lambda-descriptors.retvals D2)))
	(argvals-state (descriptors-signature.match-formals-against-operands (lambda-descriptors.argvals D1)
									     (lambda-descriptors.argvals D2))))
    (case retvals-state
      ((exact-match)
       (case argvals-state
	 ((exact-match)
	  'exact-match)
	 ((possible-match)
	  'possible-match)
	 (else
	  'no-match)))
      ((possible-match)
       (case argvals-state
	 ((exact-match possible-match)
	  'possible-match)
	 (else
	  'no-match)))
      (else
       'no-match))))


;;;; descriptors signatures for case-lambda procedures

(define-record-type (<case-lambda-descriptors> make-case-lambda-descriptors case-lambda-descriptors?)
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
  (let ((clause-signature2* (case-lambda-descriptors.clause-signature* D2)))
    (for-all (lambda (clause-signature1)
	       (exists (lambda (clause-signature2)
			 (lambda-descriptors=? clause-signature1 clause-signature2))
		 clause-signature2*))
      (case-lambda-descriptors.clause-signature* D1))))

(define* (case-lambda-descriptors.match-super-and-sub {D1 case-lambda-descriptors?} {D2 case-lambda-descriptors?})
  (let ((clause-signature1* (case-lambda-descriptors.clause-signature* D1))
	(clause-signature2* (case-lambda-descriptors.clause-signature* D2)))
    (returnable
      (fold-left (lambda (state clause-signature1)
		   (fold-left
		       (lambda (state clause-signature2)
			 (case (lambda-descriptors.match-super-and-sub clause-signature1 clause-signature2)
			   ;;Found an exactly matching clause: return now.
			   ((exact-match)	(return 'exact-match))
			   ;;Found a possibly matching clause: set the state.
			   ((possible-match)	'possible-match)
			   ;;This clause does not match: keep the previous state.
			   (else		state)))
		     state clause-signature2*))
	'no-match clause-signature1*))))


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

(define* (descriptors-signature.matching-super-and-sub? formals.sig operands.sig)
  ;;Return true if FORMALS.SIG and OPERANDS.SIG  have the same structure and the type
  ;;in  the homologous  positions  are matching  super-type  and sub-type;  otherwise
  ;;return false.
  ;;
  (%descriptors-signature.criterion-super-and-sub? __who__ formals.sig operands.sig
						   object-type-descr.matching-super-and-sub?))

(define* (descriptors-signature.compatible-super-and-sub? formals.sig operands.sig)
  ;;Return true if FORMALS.SIG and OPERANDS.SIG  have the same structure and the type
  ;;in the  homologous positions  are compatible  super-type and  sub-type; otherwise
  ;;return false.
  ;;
  (%descriptors-signature.criterion-super-and-sub? __who__ formals.sig operands.sig
						   (lambda (formal.des operand.des)
						     (or (object-type-descr.matching-super-and-sub?   formal.des operand.des)
							 (object-type-descr.compatible-super-and-sub? formal.des operand.des)))))

(define (%descriptors-signature.criterion-super-and-sub? caller-who formals.sig operands.sig
							 super-and-sub?)
  (let  ((formals.specs		(descriptors-signature.object-type-descrs formals.sig))
	 (operands.specs	(descriptors-signature.object-type-descrs operands.sig)))
    (cond
     ((<no-return>-ctd? formals.specs)
      (<no-return>-ctd? operands.specs))
     ((<no-return>-ctd? operands.specs)
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
      (cond ((object-type-descr.matching-super-and-sub? formals-car.des operands-car.des)
	     (loop state formals-cdr.descrs operands-cdr.descrs))
	    ((object-type-descr.compatible-super-and-sub? formals-car.des operands-car.des)
	     (loop 'possible-match formals-cdr.descrs operands-cdr.descrs))
	    (else
	     'no-match)))

    (module (%match-formals-pair-against-operands-list-of)

      (define (%match-formals-pair-against-operands-list-of formals-car.des formals-cdr.descrs operand.des
							    %error-invalid-formals-signature)
	;;This       is      a       mutually      recursive       function      with
	;;%MATCH-FORMALS-AGAINST-OPERANDS-LIST-OF.
	;;
	(cond ((object-type-descr.matching-super-and-sub? formals-car.des operand.des)
	       (%match-formals-against-operands-list-of formals-cdr.descrs operand.des %error-invalid-formals-signature))
	      ((object-type-descr.compatible-super-and-sub? formals-car.des operand.des)
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
	     (cond ((object-type-descr.matching-super-and-sub? formal.des operand.des)
		    'possible-match)
		   ((object-type-descr.compatible-super-and-sub? formal.des operand.des)
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
	   (cond ((object-type-descr.matching-super-and-sub? formal.des operand.des)
		  state)
		 ((object-type-descr.compatible-super-and-sub? formal.des operand.des)
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
      (cond ((object-type-descr.matching-super-and-sub? formal.des operands-car.des)
	     (%match-formals-list-of-against-operands state formal.des operands-cdr.descrs
						      %error-invalid-operands-signature))
	    ((object-type-descr.compatible-super-and-sub? formal.des operands-car.des)
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
