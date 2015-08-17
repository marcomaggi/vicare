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
    <binary-node>				make-binary-node
    binary-node?				false-or-binary-node?

    binary-node-sort-key			$binary-node-sort-key
    binary-node-parent				$binary-node-parent
    binary-node-left				$binary-node-left
    binary-node-right				$binary-node-right

    binary-node-sort-key-set!			$binary-node-sort-key-set!
    binary-node-left-set!			$binary-node-left-set!
    binary-node-right-set!			$binary-node-right-set!

    binary-node-parent-and-left-child?		$binary-node-parent-and-left-child?
    binary-node-parent-and-right-child?		$binary-node-parent-and-right-child?
    binary-node-parent-and-child?		$binary-node-parent-and-child?

    binary-tree-minimum				$binary-tree-minimum
    binary-tree-maximum				$binary-tree-maximum
    binary-tree-find				$binary-tree-find
    binary-tree-valid?				$binary-tree-valid?

    ;; forwards iterators
    binary-tree-begin-in-order-forwards		$binary-tree-begin-in-order-forwards
    binary-tree-step-in-order-forwards		$binary-tree-step-in-order-forwards

    binary-tree-begin-pre-order-forwards	$binary-tree-begin-pre-order-forwards
    binary-tree-step-pre-order-forwards		$binary-tree-step-pre-order-forwards

    binary-tree-begin-post-order-forwards	$binary-tree-begin-post-order-forwards
    binary-tree-step-post-order-forwards	$binary-tree-step-post-order-forwards

    binary-tree-begin-level-order-forwards	$binary-tree-begin-level-order-forwards
    binary-tree-step-level-order-forwards	$binary-tree-step-level-order-forwards

    ;; backwards iterators
    binary-tree-begin-in-order-backwards	$binary-tree-begin-in-order-backwards
    binary-tree-step-in-order-backwards		$binary-tree-step-in-order-backwards

    binary-tree-begin-pre-order-backwards	$binary-tree-begin-pre-order-backwards
    binary-tree-step-pre-order-backwards	$binary-tree-step-pre-order-backwards

    binary-tree-begin-post-order-backwards	$binary-tree-begin-post-order-backwards
    binary-tree-step-post-order-backwards	$binary-tree-step-post-order-backwards

    binary-tree-begin-level-order-backwards	$binary-tree-begin-level-order-backwards
    binary-tree-step-level-order-backwards	$binary-tree-step-level-order-backwards

    ;; folding
    binary-tree-fold-in-order-forwards		$binary-tree-fold-in-order-forwards
    binary-tree-fold-pre-order-forwards		$binary-tree-fold-pre-order-forwards
    binary-tree-fold-post-order-forwards	$binary-tree-fold-post-order-forwards
    binary-tree-fold-level-order-forwards	$binary-tree-fold-level-order-forwards

    binary-tree-fold-in-order-backwards		$binary-tree-fold-in-order-backwards
    binary-tree-fold-pre-order-backwards	$binary-tree-fold-pre-order-backwards
    binary-tree-fold-post-order-backwards	$binary-tree-fold-post-order-backwards
    binary-tree-fold-level-order-backwards	$binary-tree-fold-level-order-backwards

    ;; unbalanced binary nodes
    <unbalanced-binary-node>			make-unbalanced-binary-node
    unbalanced-binary-node?			false-or-unbalanced-binary-node?

    unbalanced-tree-insert!			$unbalanced-tree-insert!
    unbalanced-tree-remove!			$unbalanced-tree-remove!
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


;;;; plain binary nodes: structure predicates

(define* (binary-node-parent-and-left-child? {node1 binary-node?} {node2 binary-node?})
  ;;Return true if NODE2 is the left child of NODE1.
  ;;
  ($binary-node-parent-and-left-child? node1 node2))

(define ($binary-node-parent-and-left-child? node1 node2)
  (eq? node2 ($binary-node-left node1)))

;;; --------------------------------------------------------------------

(define* (binary-node-parent-and-right-child? {node1 binary-node?} {node2 binary-node?})
  ;;Return true if NODE2 is the right child of NODE1.
  ;;
  ($binary-node-parent-and-right-child? node1 node2))

(define ($binary-node-parent-and-right-child? node1 node2)
  (eq? node2 ($binary-node-right node1)))

;;; --------------------------------------------------------------------

(define* (binary-node-parent-and-child? {node1 binary-node?} {node2 binary-node?})
  ;;Return true if NODE2 is the left or right child of NODE1.
  ;;
  ($binary-node-parent-and-child? node1 node2))

(define ($binary-node-parent-and-child? node1 node2)
  (or ($binary-node-parent-and-left-child?  node1 node2)
      ($binary-node-parent-and-right-child? node1 node2)))

;;; --------------------------------------------------------------------

(define* (binary-tree-valid? {root false-or-binary-node?} {key< procedure?})
  ($binary-tree-valid? root key<))

(module ($binary-tree-valid?)

  (define ($binary-tree-valid? root key<)
    (if root
	(let ((left  ($binary-node-left  root))
	      (right ($binary-node-right root)))
	  (and (if left
		   (%traverse key< left #f root)
		 #t)
	       (if right
		   (%traverse key< right root #f)
		 #t)))
      ;;An empty tree is valid.
      #t))

  (define (%traverse key< node range-min range-max)
    ;; (debug-print __who__
    ;; 		 'min (and range-min (binary-node-sort-key range-min))
    ;; 		 'key (binary-node-sort-key node)
    ;; 		 'max (and range-max (binary-node-sort-key range-max)))
    (and (if range-min
	     (key< range-min node)
	   #t)
	 (if range-max
	     (key< node range-max)
	   #t)
	 (let ((left  ($binary-node-left  node))
	       (right ($binary-node-right node)))
	   (and (if left
		    (%traverse key< left range-min node)
		  #t)
		(if right
		    (%traverse key< right node range-max)
		  #t)))))

  #| end of module |# )


;;;; plain binary nodes: structure operations

(define* (binary-node-replace-in-parent! {old-child binary-node?} {new-child false-or-binary-node?})
  ;;In the parent of OLD-CHILD: replace OLD-CHILD with NEW-CHILD.
  ;;
  ($binary-node-replace-in-parent! old-child new-child))

(define ($binary-node-replace-in-parent! old-child new-child)
  (cond (($binary-node-parent old-child)
	 => (lambda (dad)
	      (if ($binary-node-parent-and-left-child? dad old-child)
		  ($binary-node-left-set! dad new-child)
		($binary-node-right-set! dad new-child))))))


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


;;;; plain binary trees: in-order iterations

(define* (binary-tree-begin-in-order-forwards {root false-or-binary-node?})
  ($binary-tree-begin-in-order-forwards root))

(define ($binary-tree-begin-in-order-forwards root)
  (if root
      ($binary-tree-minimum root #f)
    root))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-in-order-forwards {node binary-node?})
  ($binary-tree-step-in-order-forwards node))

(define ($binary-tree-step-in-order-forwards node)
  (cond (($binary-node-right node)
	 => (lambda (right)
	      ($binary-tree-minimum right #f)))
	(else
	 (let loop ((node node)
		    (dad  ($binary-node-parent node)))
	   (if (and dad ($binary-node-parent-and-right-child? dad node))
	       (loop dad ($binary-node-parent dad))
	     dad)))))

;;; --------------------------------------------------------------------

(define* (binary-tree-begin-in-order-backwards {root false-or-binary-node?})
  ($binary-tree-begin-in-order-backwards root))

(define ($binary-tree-begin-in-order-backwards root)
  (if root
      ($binary-tree-maximum root #f)
    root))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-in-order-backwards {root false-or-binary-node?})
  ($binary-tree-step-in-order-backwards root))

(define ($binary-tree-step-in-order-backwards node)
  (cond (($binary-node-left node)
	 => (lambda (left)
	      ($binary-tree-maximum left #f)))
	(else
	 (let loop ((node node)
		    (dad  ($binary-node-parent node)))
	   (if (and dad ($binary-node-parent-and-left-child? dad node))
	       (loop dad ($binary-node-parent dad))
	     dad)))))


;;;; plain binary trees: pre-order iterations

(define* (binary-tree-begin-pre-order-forwards {root false-or-binary-node?})
  ($binary-tree-begin-pre-order-forwards root))

(define ($binary-tree-begin-pre-order-forwards root)
  (if root
      ($binary-tree-minimum root #f)
    root))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-pre-order-forwards {node binary-node?})
  ($binary-tree-step-pre-order-forwards node))

(define ($binary-tree-step-pre-order-forwards node)
  (void))

;;; --------------------------------------------------------------------

(define* (binary-tree-begin-pre-order-backwards {root false-or-binary-node?})
  ($binary-tree-begin-pre-order-backwards root))

(define ($binary-tree-begin-pre-order-backwards root)
  (void))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-pre-order-backwards {root false-or-binary-node?})
  ($binary-tree-step-pre-order-backwards root))

(define ($binary-tree-step-pre-order-backwards root)
  (void))


;;;; plain binary trees: post-order iterations

(define* (binary-tree-begin-post-order-forwards {root false-or-binary-node?})
  ($binary-tree-begin-post-order-forwards root))

(define ($binary-tree-begin-post-order-forwards root)
  (if root
      ($binary-tree-minimum root #f)
    root))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-post-order-forwards {node binary-node?})
  ($binary-tree-step-post-order-forwards node))

(define ($binary-tree-step-post-order-forwards node)
  (void))

;;; --------------------------------------------------------------------

(define* (binary-tree-begin-post-order-backwards {root false-or-binary-node?})
  ($binary-tree-begin-post-order-backwards root))

(define ($binary-tree-begin-post-order-backwards root)
  (void))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-post-order-backwards {root false-or-binary-node?})
  ($binary-tree-step-post-order-backwards root))

(define ($binary-tree-step-post-order-backwards root)
  (void))


;;;; plain binary trees: level-order iterations

(define* (binary-tree-begin-level-order-forwards {root false-or-binary-node?})
  ($binary-tree-begin-level-order-forwards root))

(define ($binary-tree-begin-level-order-forwards root)
  (if root
      ($binary-tree-minimum root #f)
    root))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-level-order-forwards {node binary-node?})
  ($binary-tree-step-level-order-forwards node))

(define ($binary-tree-step-level-order-forwards node)
  (void))

;;; --------------------------------------------------------------------

(define* (binary-tree-begin-level-order-backwards {root false-or-binary-node?})
  ($binary-tree-begin-level-order-backwards root))

(define ($binary-tree-begin-level-order-backwards root)
  (void))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-level-order-backwards {root false-or-binary-node?})
  ($binary-tree-step-level-order-backwards root))

(define ($binary-tree-step-level-order-backwards root)
  (void))


;;;; plain binary trees: folding forwards

(define-syntax define-folder-forwards
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who ?begin ?step)
     (begin
       (define* (?safe-who {kons procedure?} knil {root false-or-binary-node?})
	 (?unsafe-who kons knil root))

       (define (?unsafe-who kons knil root)
	 (cond ((?begin root)
		=> (lambda (node)
		     (let next ((knil (kons knil node))
				(node node))
		       (cond ((?step node)
			      => (lambda (node)
				   (next (kons knil node) node)))
			     (else knil)))))
	       (else knil)))
       #| end of BEGIN |# ))
    ))

;;; --------------------------------------------------------------------

(define-folder-forwards binary-tree-fold-in-order-forwards
  $binary-tree-fold-in-order-forwards
  $binary-tree-begin-in-order-forwards
  $binary-tree-step-in-order-forwards)

(define-folder-forwards binary-tree-fold-pre-order-forwards
  $binary-tree-fold-pre-order-forwards
  $binary-tree-begin-pre-order-forwards
  $binary-tree-step-pre-order-forwards)

(define-folder-forwards binary-tree-fold-post-order-forwards
  $binary-tree-fold-post-order-forwards
  $binary-tree-begin-post-order-forwards
  $binary-tree-step-post-order-forwards)

(define-folder-forwards binary-tree-fold-level-order-forwards
  $binary-tree-fold-level-order-forwards
  $binary-tree-begin-level-order-forwards
  $binary-tree-step-level-order-forwards)


;;;; plain binary trees: folding backwards

(define-syntax define-folder-backwards
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who ?begin ?step)
     (begin
       (define* (?safe-who {kons procedure?} knil {root false-or-binary-node?})
	 (?unsafe-who kons knil root))

       (define (?unsafe-who kons knil root)
	 (cond ((?begin root)
		=> (lambda (node)
		     (let next ((knil (kons node knil))
				(node node))
		       (cond ((?step node)
			      => (lambda (node)
				   (next (kons node knil) node)))
			     (else knil)))))
	       (else knil)))
       #| end of BEGIN |# ))
    ))

;;; --------------------------------------------------------------------

(define-folder-backwards binary-tree-fold-in-order-backwards
  $binary-tree-fold-in-order-backwards
  $binary-tree-begin-in-order-backwards
  $binary-tree-step-in-order-backwards)

(define-folder-backwards binary-tree-fold-pre-order-backwards
  $binary-tree-fold-pre-order-backwards
  $binary-tree-begin-pre-order-backwards
  $binary-tree-step-pre-order-backwards)

(define-folder-backwards binary-tree-fold-post-order-backwards
  $binary-tree-fold-post-order-backwards
  $binary-tree-begin-post-order-backwards
  $binary-tree-step-post-order-backwards)

(define-folder-backwards binary-tree-fold-level-order-backwards
  $binary-tree-fold-level-order-backwards
  $binary-tree-begin-level-order-backwards
  $binary-tree-step-level-order-backwards)


;;;; unbalanced binary nodes: data type

(define-record-type (<unbalanced-binary-node> make-unbalanced-binary-node unbalanced-binary-node?)
  (nongenerative vicare:containers:<unbalanced-binary-node>)

  (parent <binary-node>)

  (protocol
   (lambda (make-binary-node)
     (case-define* make-<unbalanced-binary-node>
       (()
	((make-binary-node (void) #f #f)))

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

(define* (unbalanced-tree-remove! {node unbalanced-binary-node?})
  ($unbalanced-tree-remove! node))

(define ($unbalanced-tree-remove! node)
  (let ((left  ($binary-node-left  node))
	(right ($binary-node-right node)))
    (cond ((and left right)
	   (let* ((min     ($binary-tree-minimum node #f))
		  (min.dad ($binary-node-parent  min)))
	     ;;Insert MIN in the tree in place of NODE.
	     ($binary-node-replace-in-parent! node min)
	     ($binary-node-left-set!  min left)
	     ($binary-node-right-set! min right)
	     ;;If MIN is the minimum, by definition  of BST: MIN is the left child of
	     ;;MIN.DAD.
	     ($binary-node-left-set! min.dad #f)
	     min))

	  (left
	   ;;Left child exists, right child does not.
	   ;;
	   ($binary-node-replace-in-parent! node left)
	   left)

	  (right
	   ;;Right child exists, left child does not.
	   ;;
	   ($binary-node-replace-in-parent! node right)
	   right)

	  (else
	   ;;No children.  Remove this node from its parent.
	   ($binary-node-replace-in-parent! node #f)
	   ;;Reset this node.
	   ($<binary-node>-parent-set! node #f)
	   ($<binary-node>-left-set!   node #f)
	   ($<binary-node>-right-set!  node #f)
	   ;;No node takes place of NODE in the tree.
	   #f))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
