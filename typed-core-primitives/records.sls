;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for records core primitives
;;Date: Sat Jan  2, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives records)
  (export typed-core-primitives.records)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.records)


;;;; R6RS record-type descriptors, safe primitives

(section

(declare-type-predicate record-type-descriptor?	<record-type-descriptor>)

(declare-type-predicate record-constructor-descriptor? <record-constructor-descriptor>)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-record-type-descriptor
    (safe)
  (signatures
   ((<symbol>				   ;name
     (or <false> <record-type-descriptor>) ;parent
     (or <false> <symbol>)		   ;uid
     <top>				   ;sealed?
     <top>				   ;opaque?
     <vector>)				   ;fields
    => (<record-type-descriptor>)))
  (attributes
   ((_ _ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-record-constructor-descriptor
    (safe)
  (signatures
   ((<record-type-descriptor>
     (or <false> <record-constructor-descriptor>)
     (or <false> <procedure>))
    => (<record-constructor-descriptor>)))
  (attributes
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; procedures generation

(declare-core-primitive record-constructor
    (safe)
  (signatures
   ((<record-constructor-descriptor>)	=> (<procedure>)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-predicate
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> (<procedure>)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-accessor
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive record-mutator
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-accessor
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-mutator
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-field-mutable?
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<boolean>)))
  (attributes
   ((_ _)		effect-free)))

;;;

(declare-core-primitive record-type-generative?
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> (<boolean>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive record-type-opaque?
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<boolean>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-sealed?
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<boolean>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-field-names
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> ((vector-of <symbol>))))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-name
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<symbol>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-parent
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> ((or <false> <record-type-descriptor>))))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-uid
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<symbol>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-uids-list
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> ((nelist-of <symbol>))))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-all-field-names
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> ((vector-of <symbol>)))))

(declare-core-primitive record-type-implemented-interfaces
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> ((or <false> (vector-of (pair <symbol> <type-method-retriever>)))))))

;;; --------------------------------------------------------------------
;;; record-type procedures

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who <procedure>))
		((_ ?who ?rv-type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<record-type-descriptor>)		=> ((or <false> ?rv-type))))
		   (attributes
		    ((_)				effect-free)))))))
  (declare record-type-destructor		<type-destructor>)
  (declare record-type-printer			<type-printer>)
  (declare record-type-equality-predicate	(equality-predicate <top>))
  (declare record-type-comparison-procedure	(comparison-procedure <top>))
  (declare record-type-hash-function		(hash-function <top>))
  (declare record-type-method-retriever		<type-method-retriever>)
  #| end of LET-SYNTAX |# )

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who <procedure>))
		((_ ?who ?arg-type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<record-type-descriptor> ?arg-type)	=> (<void>))
		    ((<record-type-descriptor> <false>)		=> (<void>)))
		   (attributes
		    ((_ _)		result-true)))))))
  (declare record-type-destructor-set!			<type-destructor>)
  (declare record-type-printer-set!			<type-printer>)
  (declare record-type-equality-predicate-set!		<equality-predicate>)
  (declare record-type-comparison-procedure-set!	<comparison-procedure>)
  (declare record-type-hash-function-set!		<hash-function>)
  (declare record-type-method-retriever-set!		<type-method-retriever>)
  #| end of LET-SYNTAX |# )

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ))
		((_ ?who ?arg-type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<record-type-descriptor> (or <false> ?arg-type))	=> (<procedure>))))))))
  (declare record-type-compose-equality-predicate	<equality-predicate>)
  (declare record-type-compose-comparison-procedure	<comparison-procedure>)
  (declare record-type-compose-hash-function		<hash-function>)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; misc

(declare-core-primitive rcd-rtd
    (safe)
  (signatures
   ((<record-constructor-descriptor>)	=> (<record-type-descriptor>))))

(declare-core-primitive rcd-parent-rcd
    (safe)
  (signatures
   ((<record-constructor-descriptor>)	=> ((or <false> <record-constructor-descriptor>)))))

(declare-core-primitive record-type-method-call-late-binding-private
    (safe)
  (signatures
   ((<symbol> <record> . <list>)	=> <list>)))

(declare-core-primitive internal-applicable-record-type-destructor
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> (<type-destructor>))))

/section)


;;;; R6RS records, safe primitives

(section

(declare-type-predicate record? <record>)

(declare-type-predicate record-object? <record>)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-and-rtd?
    (safe)
  (signatures
   ((<record> <record-type-descriptor>)		=> (<boolean>)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive record-rtd
    (safe)
  (signatures
   ((<record>)		=> (<record-type-descriptor>)))
  (attributes
   ((_)			effect-free result-true)))

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who <procedure>))
		((_ ?who ?rv-type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<record>)		=> ((or <false> ?rv-type))))
		   (attributes
		    ((_)		effect-free)))))))
  (declare record-destructor		<type-destructor>)
  (declare record-printer		<type-printer>)
  (declare record-equality-predicate	(equality-predicate <top>))
  (declare record-comparison-procedure	(comparison-procedure <top>))
  (declare record-hash-function		(hash-function <top>))
  #| end of LET-SYNTAX |# )

(declare-core-primitive record-ref
    (safe)
  (signatures
   ((<record> <non-negative-fixnum>)	=> (<top>)))
  (attributes
   ((_ _)				effect-free)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-core-primitive record=?
    (safe)
  (signatures
   ((list-of <record>)			=> (<boolean>))))

(declare-core-primitive record!=?
    (safe)
  (signatures
   ((list-of <record>)			=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive record-reset!
    (safe)
  (signatures
   ((<record>)		=> (<void>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive record-guardian-log
    (safe)
  (signatures
   ((<record> <top> <symbol>)	=> (<void>)))
  (attributes
   ((_ _ _)			result-true)))

(declare-parameter record-guardian-logger	(or <boolean> <procedure>))

(declare-core-primitive internal-applicable-record-destructor
    (safe)
  (signatures
   ((<record>)				=> (<procedure>))))

/section)


;;;; R6RS record-type descriptors, unsafe primitives

(section

(declare-core-primitive $make-record-type-descriptor
    (unsafe)
  (signatures
   ;;name
   ;;parent uid sealed? opaque?
   ;;fields normalised-fields
   ((<symbol>
     (or <false> <record-type-descriptor>) (or <false> <symbol>) <boolean> <boolean>
     (vector-of (pair <symbol> <symbol>)) (vector-of (pair <boolean> <symbol>)))
    => (<record-type-descriptor>))))

(declare-core-primitive $make-record-type-descriptor-ex
    (unsafe)
  (signatures
   ((<symbol>				   ;name
     (or <false> <record-type-descriptor>) ;parent
     <symbol>				   ;uid
     <boolean>				   ;generative?
     <boolean>				   ;sealed?
     <boolean>				   ;opaque?
     (vector-of (list <symbol> <symbol>))  ;fields
     (vector-of (pair <boolean> <symbol>)) ;normalised-fields
     (or <false> (lambda (<bottom>) => <list>))	   ;destructor
     (or <false> <type-printer>)	   ;printer
     (or <false> <equality-predicate>)	   ;equality-predicate
     (or <false> <comparison-procedure>)   ;comparison-procedure
     (or <false> <hash-function>)	   ;hash-function
     (or <false> <type-method-retriever>)  ;method-retriever-retriever-public
     (or <false> <type-method-retriever>)  ;method-retriever-retriever-private
     (or <false> (vector-of (pair <symbol> <type-method-retriever>)))) ;implemented-interfaces
    => (<record-type-descriptor>))))

;;; --------------------------------------------------------------------

(declare-core-primitive $record-constructor
    (unsafe)
  (signatures
   ((<record-constructor-descriptor>)		=> (<procedure>)))
  (attributes
   ((_)						effect-free result-true)))

(declare-core-primitive $record-type-destructor
    (unsafe)
  (signatures
   ((<record-type-descriptor>)			=> ((or <false> <type-destructor>)))))

(declare-core-primitive $record-accessor/index
    (unsafe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))))

;;; --------------------------------------------------------------------

(declare-core-primitive $rtd-subtype?
    (unsafe)
  (signatures
   ((<record-type-descriptor> <record-type-descriptor>)		=> (<boolean>))))

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?rv-type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<record-type-descriptor>)		=> ((or <false> ?rv-type)))))))))
  (declare $record-type-equality-predicate		(equality-predicate <top>))
  (declare $record-type-comparison-procedure		(comparison-procedure <top>))
  (declare $record-type-hash-function			(hash-function <top>))
  (declare $record-type-method-retriever		<type-method-retriever>)
  (declare $record-type-method-retriever-private	<type-method-retriever>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?arg-type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<record-type-descriptor> ?arg-type)	=> (<void>))
		    ((<record-type-descriptor> <false>)		=> (<void>))))))))
  (declare $record-type-equality-predicate-set!		<equality-predicate>)
  (declare $record-type-comparison-procedure-set!	<comparison-procedure>)
  (declare $record-type-hash-function-set!		<hash-function>)
  (declare $record-type-method-retriever-set!		<type-method-retriever>)
  #| end of LET-SYNTAX |# )

/section)


;;;; R6RS record-constructor descriptors, unsafe primitives

(section

(declare-core-primitive $make-record-constructor-descriptor
    (unsafe)
  (signatures
   ;;rtd parent-rcd protocol
   ((<record-type-descriptor> (or <false> <record-constructor-descriptor>) (or <false> <procedure>))
    => (<record-constructor-descriptor>))))

/section)


;;;; R6RS records, unsafe primitives

(section

(declare-core-primitive $record-and-rtd?
    (unsafe)
  (signatures
   ((<struct> <record-type-descriptor>)		=> (<boolean>)))
  (attributes
   ((_)						effect-free)))

(declare-core-primitive $record-ref
    (unsafe)
  (signatures
   ((<record> <non-negative-fixnum>)		=> (<top>))))

;;; --------------------------------------------------------------------

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who <procedure>))
		((_ ?who ?rv-type)
		 (declare-core-primitive ?who
		     (unsafe)
		   (signatures
		    ((<record>)		=> ((or <false> ?rv-type))))
		   (attributes
		    ((_)		effect-free)))))))
  (declare $record-destructor		<type-destructor>)
  (declare $record-printer		<type-printer>)
  (declare $record-equality-predicate	(equality-predicate <top>))
  (declare $record-comparison-procedure	(comparison-procedure <top>))
  (declare $record-hash-function	(hash-function <top>))
  (declare $record-method-retriever	<type-method-retriever>)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $record-guardian
    (unsafe)
  (signatures
   ((<record>)		=> (<void>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
