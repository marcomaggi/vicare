;;;
;;;Part of: Vicare Scheme
;;;Contents: binary tree skeleton
;;;Date: Fri Aug 14, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers binary-trees (1 0 0))
  (export

    ;; plain binary nodes
    <binary-node>			make-binary-node
    binary-node?			false-or-binary-node?

    binary-node-sort-key		$binary-node-sort-key
    binary-node-parent			$binary-node-parent
    binary-node-left			$binary-node-left
    binary-node-right			$binary-node-right

    binary-node-sort-key-set!		$binary-node-sort-key-set!
    binary-node-left-set!		$binary-node-left-set!
    binary-node-right-set!		$binary-node-right-set!

    binary-tree-minimum			$binary-tree-minimum
    binary-tree-maximum			$binary-tree-maximum
    binary-tree-find			$binary-tree-find

    ;; unbalanced binary nodes
    <unbalanced-binary-node>		make-unbalanced-binary-node
    unbalanced-binary-node?		false-or-unbalanced-binary-node?

    unbalanced-tree-insert!		$unbalanced-tree-insert!
    unbalanced-tree-remove!		$unbalanced-tree-remove!
    )
  (import (vicare))


;;;; plain binary nodes: data type

(define-record-type (<binary-node> make-binary-node binary-node?)
  (nongenerative vicare:containers:<binary-node>)

  (fields (mutable sort-key)
		;An arbitrary object used as sort key.
	  (mutable parent)
		;False or an instance of "<binary-node>" being the parent node.
	  (mutable left)
		;False or an instance of "<binary-node>" being the left subtree.
	  (mutable right)
		;False or an instance of "<binary-node>" being the right subtree.
	  #| end of FIELDS |# )

  (protocol
   (lambda (make-record)
     (case-define* make-<binary-node>
       (()
	(make-record (void) #f #f #f))

       ((key)
	(make-record key #f #f #f))

       ((key {left false-or-binary-node?} {right false-or-binary-node?})
	(receive-and-return (node)
	    (make-record key #f left right)
	  (when left
	    ($<binary-node>-parent-set! left node))
	  (when right
	    ($<binary-node>-parent-set! right node)))))

     make-<binary-node>))

  #| end of DEFINE-RECORD-TYPE |# )

(define (false-or-binary-node? obj)
  (or (not obj)
      (binary-node? obj)))

;;; --------------------------------------------------------------------

(define-alias  binary-node-parent		 <binary-node>-parent)
(define-alias $binary-node-parent		$<binary-node>-parent)

(define-alias  binary-node-left			 <binary-node>-left)
(define-alias $binary-node-left			$<binary-node>-left)

(define-alias  binary-node-right		 <binary-node>-right)
(define-alias $binary-node-right		$<binary-node>-right)

(define-alias  binary-node-sort-key		 <binary-node>-sort-key)
(define-alias $binary-node-sort-key		$<binary-node>-sort-key)

;;; --------------------------------------------------------------------

(define* (binary-node-left-set! {node binary-node?} {left false-or-binary-node?})
  ($binary-node-left-set! node left))

(define ($binary-node-left-set! node left)
  ($<binary-node>-left-set! node left)
  (when left
    ($<binary-node>-parent-set! left node)))

;;; --------------------------------------------------------------------

(define* (binary-node-right-set! {node binary-node?} {right false-or-binary-node?})
  ($binary-node-right-set! node right))

(define ($binary-node-right-set! node right)
  ($<binary-node>-right-set! node right)
  (when right
    ($<binary-node>-parent-set! right node)))

;;; --------------------------------------------------------------------

(define-alias  binary-node-sort-key-set!	 <binary-node>-sort-key-set!)
(define-alias $binary-node-sort-key-set!	$<binary-node>-sort-key-set!)


;;;; plain binary trees: searching operations

(case-define* binary-tree-minimum

  (({root false-or-binary-node?})
   ($binary-tree-minimum root #f))

  (({root false-or-binary-node?} {empty-tree-handler procedure?})
   ($binary-tree-minimum root empty-tree-handler))

  #| end of CASE-DEFINE* |# )

(define ($binary-tree-minimum root empty-tree-handler)
  (cond (root
	    (let loop ((node root))
	      (cond (($binary-node-left node)
		     => loop)
		    (else node))))
	((procedure? empty-tree-handler)
	 (empty-tree-handler))
	(else
	 empty-tree-handler)))

;;; --------------------------------------------------------------------

(case-define* binary-tree-maximum

  (({root false-or-binary-node?})
   ($binary-tree-maximum root #f))

  (({root false-or-binary-node?} {empty-tree-handler procedure?})
   ($binary-tree-maximum root empty-tree-handler))

  #| end of CASE-DEFINE* |# )

(define ($binary-tree-maximum root empty-tree-handler)
  (cond (root
	    (let loop ((node root))
	      (cond (($binary-node-right node)
		     => loop)
		    (else node))))
	((procedure? empty-tree-handler)
	 (empty-tree-handler))
	(else
	 empty-tree-handler)))

;;; --------------------------------------------------------------------

(case-define* binary-tree-find

  (({root false-or-binary-node?} {compare procedure?})
   ($binary-tree-find root compare #f))

  (({root false-or-binary-node?} {compare procedure?} {not-found-handler procedure?})
   ($binary-tree-find root compare not-found-handler))

  #| end of CASE-DEFINE* |# )

(define ($binary-tree-find root compare not-found-handler)
  (define-syntax-rule (recurse ?node)
    ($binary-tree-find ?node compare not-found-handler))
  (cond (root
	    (case (compare root)
	      ((0)	root)
	      ((-1)	(recurse ($binary-node-left  root)))
	      ((+1)	(recurse ($binary-node-right root)))
	      (else
	       (expression-return-value-violation __who__
		 "invalid return value from comparison procedure" root))))
	((procedure? not-found-handler)
	 (not-found-handler))
	(else
	 not-found-handler)))


;;;; unbalanced binary nodes: data type

(define-record-type (<unbalanced-binary-node> make-unbalanced-binary-node unbalanced-binary-node?)
  (nongenerative vicare:containers:<unbalanced-binary-node>)

  (parent <binary-node>)

  (protocol
   (lambda (make-binary-node)
     (case-define* make-<unbalanced-binary-node>
       (()
	((make-binary-node (void) #f #f #f)))

       ((key)
	((make-binary-node key #f #f)))

       ((key {left false-or-unbalanced-binary-node?} {right false-or-unbalanced-binary-node?})
	((make-binary-node key left right))))

     make-<unbalanced-binary-node>))

  #| end of DEFINE-RECORD-TYPE |# )

(define (false-or-unbalanced-binary-node? obj)
  (or (not obj)
      (unbalanced-binary-node? obj)))


;;;; unbalanced binary nodes: insertion and removal

(define* (unbalanced-tree-insert! {root false-or-unbalanced-binary-node?} {key< procedure?} {new unbalanced-binary-node?})
  ($unbalanced-tree-insert! root key< new))

(define ($unbalanced-tree-insert! root key< new)
  (if root
      (let loop ((old root))
	(if (key< new old)
	    (cond (($binary-node-left old)
		   => loop)
		  (else
		   ($binary-node-left-set! old new)
		   root))
	  (cond (($binary-node-right old)
		 => loop)
		(else
		 ($binary-node-right-set! old new)
		 root))))
    new))

;;; --------------------------------------------------------------------

(define* (unbalanced-tree-remove! {node		unbalanced-binary-node?}
				  {left-parent	unbalanced-binary-node?}
				  {right-parent	unbalanced-binary-node?})
  ($unbalanced-tree-remove! node left-parent right-parent))

(define ($unbalanced-tree-remove! node left-parent right-parent)
  (let ((left  ($binary-node-left  node))
	(right ($binary-node-right node)))
    (cond ((and left right)
	   (let ((minimum ($binary-tree-minimum node #f)))
	     ($binary-node-left-set! left-parent minimum)
	       ($binary-node-right-set! right-parent left)
	       (void)))

	  (left
	   (if left-parent
	       ($binary-node-left-set! left-parent left)
	     ($binary-node-right-set! right-parent left))
	   left)

	  (right
	   (if left-parent
	       ($binary-node-left-set! left-parent right)
	     ($binary-node-right-set! right-parent right))
	   right)

	  (else
	   ;;No children.
	   (if left-parent
	       ($binary-node-left-set! left-parent #f)
	     ($binary-node-right-set! right-parent #f))
	   ;;No node takes place of NODE in the tree.
	   #f))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
