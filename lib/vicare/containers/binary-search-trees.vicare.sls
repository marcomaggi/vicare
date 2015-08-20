;;;
;;;Part of: Vicare Scheme
;;;Contents: binary search trees
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
(library (vicare containers binary-search-trees (1 0 0))
  (options visit-upon-loading)
  (export

    ;; plain binary nodes
    <binary-node>				make-binary-node
    binary-node?				false-or-binary-node?

    binary-node-parent				$binary-node-parent
    binary-node-left				$binary-node-left
    binary-node-right				$binary-node-right

    binary-node-left-set!			$binary-node-left-set!
    binary-node-right-set!			$binary-node-right-set!

    binary-node-parent-and-left-child?		$binary-node-parent-and-left-child?
    binary-node-parent-and-right-child?		$binary-node-parent-and-right-child?
    binary-node-parent-and-child?		$binary-node-parent-and-child?
    binary-node-root?				$binary-node-root?
    binary-node-leaf?				$binary-node-leaf?
    binary-tree-valid?				$binary-tree-valid?

    binary-tree-depth				$binary-tree-depth

    binary-tree-root				$binary-tree-root
    binary-tree-minimum				$binary-tree-minimum
    binary-tree-maximum				$binary-tree-maximum
    binary-tree-find				$binary-tree-find
    binary-tree-deepest-left-leaf		$binary-tree-deepest-left-leaf
    binary-tree-deepest-right-leaf		$binary-tree-deepest-right-leaf

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

    ;; breadth-first iteration
    binary-tree-begin-breadth-first-forwards	$binary-tree-begin-breadth-first-forwards
    binary-tree-step-breadth-first-forwards	$binary-tree-step-breadth-first-forwards

    binary-tree-begin-breadth-first-backwards	$binary-tree-begin-breadth-first-backwards
    binary-tree-step-breadth-first-backwards	$binary-tree-step-breadth-first-backwards

    binary-tree-fold-breadth-first-forwards	$binary-tree-fold-breadth-first-forwards
    binary-tree-fold-breadth-first-backwards	$binary-tree-fold-breadth-first-backwards

    ;; iteration thunks
    make-binary-tree-forwards-in-order-iteration-thunk
    make-binary-tree-forwards-pre-order-iteration-thunk
    make-binary-tree-forwards-post-order-iteration-thunk
    make-binary-tree-forwards-level-order-iteration-thunk
    make-binary-tree-forwards-breadth-first-iteration-thunk

    make-binary-tree-backwards-in-order-iteration-thunk
    make-binary-tree-backwards-pre-order-iteration-thunk
    make-binary-tree-backwards-post-order-iteration-thunk
    make-binary-tree-backwards-level-order-iteration-thunk
    make-binary-tree-backwards-breadth-first-iteration-thunk

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

  (fields (mutable parent)
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
	(make-record #f #f #f))

       (({left false-or-binary-node?} {right false-or-binary-node?})
	(receive-and-return (node)
	    (make-record #f left right)
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

;;; --------------------------------------------------------------------

(define* (binary-node-root? {node binary-node?})
  ($binary-node-root? node))

(define ($binary-node-root? node)
  (not ($binary-node-parent node)))

;;; --------------------------------------------------------------------

(define* (binary-node-leaf? {node binary-node?})
  ($binary-node-leaf? node))

(define ($binary-node-leaf? node)
  (and (not ($binary-node-left  node))
       (not ($binary-node-right node))))


;;;; plain binary nodes: inspection

(define* (binary-tree-depth {node false-or-binary-node?})
  ($binary-tree-depth node))

(define ($binary-tree-depth node)
  (if node
      (let loop ((i    1)
		 (node node))
	(cond (($binary-node-parent node)
	       => (lambda (dad)
		    (loop (add1 i) dad)))
	      (else i)))
    0))


;;;; plain binary nodes: structure operations

(define* (binary-node-replace-in-parent! {old-child binary-node?} {new-child false-or-binary-node?})
  ;;In the parent of OLD-CHILD: replace  OLD-CHILD with NEW-CHILD.  Link NEW-CHILD to
  ;;its new parent.
  ;;
  ($binary-node-replace-in-parent! old-child new-child))

(define ($binary-node-replace-in-parent! old-child new-child)
  (cond (($binary-node-parent old-child)
	 => (lambda (dad)
	      (if ($binary-node-parent-and-left-child? dad old-child)
		  ($binary-node-left-set! dad new-child)
		($binary-node-right-set! dad new-child))
	      (when new-child
		($<binary-node>-parent-set! new-child dad))
	      ($<binary-node>-parent-set! old-child #f)))
	(else
	 ;;OLD-CHILD has no parent.
	 (when new-child
	   ($<binary-node>-parent-set! new-child #f)))))


;;;; plain binary trees: searching operations

(define* (binary-tree-root {node false-or-binary-node?})
  ($binary-tree-root node))

(define ($binary-tree-root node)
  (if node
      (cond (($binary-node-parent node)
	     => $binary-tree-root)
	    (else node))
    #f))

;;; --------------------------------------------------------------------

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

;;; --------------------------------------------------------------------

(define* (binary-tree-deepest-left-leaf {node false-or-binary-node?})
  ($binary-tree-deepest-left-leaf node))

(define ($binary-tree-deepest-left-leaf node)
  (and node
       (cond (($binary-node-left node)
	      => $binary-tree-deepest-left-leaf)
	     (($binary-node-right node)
	      => $binary-tree-deepest-left-leaf)
	     (else node))))

;;; --------------------------------------------------------------------

(define* (binary-tree-deepest-right-leaf {node false-or-binary-node?})
  ($binary-tree-deepest-right-leaf node))

(define ($binary-tree-deepest-right-leaf node)
  (and node
       (cond (($binary-node-right node)
	      => $binary-tree-deepest-right-leaf)
	     (($binary-node-left  node)
	      => $binary-tree-deepest-right-leaf)
	     (else node))))


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
  root)

;;; --------------------------------------------------------------------

(define* (binary-tree-step-pre-order-forwards {node binary-node?})
  ($binary-tree-step-pre-order-forwards node))

(define ($binary-tree-step-pre-order-forwards node)
  (or ($binary-node-left  node)
      ($binary-node-right node)
      (let loop ((node node)
		 (dad  ($binary-node-parent node)))
	(cond ((not dad)
	       #f)
	      ((and ($binary-node-parent-and-left-child? dad node)
		    ($binary-node-right dad)))
	      (else
	       (loop dad ($binary-node-parent dad)))))))

;;; --------------------------------------------------------------------

(define* (binary-tree-begin-pre-order-backwards {root false-or-binary-node?})
  ($binary-tree-begin-pre-order-backwards root))

(define ($binary-tree-begin-pre-order-backwards root)
  root)

;;; --------------------------------------------------------------------

(define* (binary-tree-step-pre-order-backwards {node false-or-binary-node?})
  ($binary-tree-step-pre-order-backwards node))

(define ($binary-tree-step-pre-order-backwards node)
  (or ($binary-node-right node)
      ($binary-node-left  node)
      (let loop ((node node)
		 (dad  ($binary-node-parent node)))
	(cond ((not dad)
	       #f)
	      ((and ($binary-node-parent-and-right-child? dad node)
		    ($binary-node-left dad)))
	      (else
	       (loop dad ($binary-node-parent dad)))))))


;;;; plain binary trees: post-order iterations

(define* (binary-tree-begin-post-order-forwards {root false-or-binary-node?})
  ($binary-tree-begin-post-order-forwards root))

(define ($binary-tree-begin-post-order-forwards root)
  ($binary-tree-deepest-left-leaf root))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-post-order-forwards {node binary-node?})
  ($binary-tree-step-post-order-forwards node))

(define ($binary-tree-step-post-order-forwards node)
  (let ((dad ($binary-node-parent node)))
    (cond ((not dad)
	   #f)
	  ((let ((dad.right ($binary-node-right dad)))
	     (and dad.right
		  (not (eq? dad.right node))
		  dad.right))
	   => $binary-tree-deepest-left-leaf)
	  (else dad))))

  ;; dad = N->dad;
  ;; if (! dad)
  ;;   return dad;
  ;; else if ((dad->bro) && (dad->bro != N))
  ;;   return ucl_btree_find_deepest_son(dad->bro);
  ;; else
  ;;   return dad;

;;; --------------------------------------------------------------------

(define* (binary-tree-begin-post-order-backwards {root false-or-binary-node?})
  ($binary-tree-begin-post-order-backwards root))

(define ($binary-tree-begin-post-order-backwards root)
  ($binary-tree-deepest-right-leaf root))

;;; --------------------------------------------------------------------

(define* (binary-tree-step-post-order-backwards {node false-or-binary-node?})
  ($binary-tree-step-post-order-backwards node))

(define ($binary-tree-step-post-order-backwards node)
  (let ((dad ($binary-node-parent node)))
    (cond ((not dad)
	   #f)
	  ((let ((dad.left ($binary-node-left dad)))
	     (and dad.left
		  (not (eq? dad.left node))
		  dad.left))
	   => $binary-tree-deepest-right-leaf)
	  (else dad))))


;;;; plain binary trees: level-order iterations

(define* (binary-tree-begin-level-order-forwards {root false-or-binary-node?})
  ($binary-tree-begin-level-order-forwards root))

(define ($binary-tree-begin-level-order-forwards root)
  root)

;;; --------------------------------------------------------------------

(define* (binary-tree-step-level-order-forwards {node binary-node?})
  ($binary-tree-step-level-order-forwards node))

(define ($binary-tree-step-level-order-forwards node)
  ;;NOTE This  is a Scheme translation  of very old code  I wrote in the  C language.
  ;;This is why the style is so unschemey.  (Marco Maggi; Wed Aug 19, 2015)
  ;;
  (returnable
    (if (and (not ($binary-node-parent node))
	     (not ($binary-node-left   node))
	     (not ($binary-node-right  node)))
	#f
      (let loop ((i    0)
		 (org  node) ;the node from which we started this step
		 (last #f))  ;the NODE in the previous loop iteration
	(define-syntax node->dad	(identifier-syntax ($binary-node-parent  node)))
	(define-syntax node->left	(identifier-syntax ($binary-node-left    node)))
	(define-syntax node->right	(identifier-syntax ($binary-node-right   node)))
	(if node->dad
	    (begin
	      (set! last node)
	      (set! node node->dad)
	      (++ i)
	      (when (and node->right (not (eq? node->right last)))
		(set! last node)
		(set! node node->right)
		(-- i)
		(when (zero? i)
		  (return node))
		(when (and (eq? node org) (not node->left) (not node->right))
		  (return #f))
		(while (or node->left node->right)
		  (cond (node->left
			 (set! last node)
			 (set! node node->left))
			(node->right
			 (set! last node)
			 (set! node node->right)))
		  (-- i)
		  (when (zero? i)
		    (return node))
		  (when (and (eq? node org)
			     (not node->left)
			     (not node->right))
		    (return #f))))
	      (loop i org last))
	  (begin
	    (++ i)
	    (while (or node->left node->right)
	      (cond (node->left
		     (set! last node)
		     (set! node node->left))
		    (node->right
		     (set! last node)
		     (set! node node->right)))
	      (-- i)
	      (when (zero? i)
		(return node))
	      (when (and (eq? node org) (not node->left) (not node->right))
		(return #f)))
	    (loop i org last)))))))

;;; --------------------------------------------------------------------

(define* (binary-tree-begin-level-order-backwards {root false-or-binary-node?})
  ($binary-tree-begin-level-order-backwards root))

(define ($binary-tree-begin-level-order-backwards root)
  root)

;;; --------------------------------------------------------------------

(define* (binary-tree-step-level-order-backwards {node false-or-binary-node?})
  ($binary-tree-step-level-order-backwards node))

(define ($binary-tree-step-level-order-backwards node)
  ;;NOTE This  is a Scheme translation  of very old code  I wrote in the  C language.
  ;;This is why the style is so unschemey.  (Marco Maggi; Wed Aug 19, 2015)
  ;;
  (returnable
    (if (and (not ($binary-node-parent node))
	     (not ($binary-node-left   node))
	     (not ($binary-node-right  node)))
	#f
      (let loop ((i    0)
		 (org  node) ;the node from which we started this step
		 (last #f))  ;the NODE in the previous loop iteration
	(define-syntax node->dad	(identifier-syntax ($binary-node-parent  node)))
	(define-syntax node->left	(identifier-syntax ($binary-node-left    node)))
	(define-syntax node->right	(identifier-syntax ($binary-node-right   node)))
	(if node->dad
	    (begin
	      (set! last node)
	      (set! node node->dad)
	      (++ i)
	      (when (and node->left (not (eq? node->left last)))
		(set! last node)
		(set! node node->left)
		(-- i)
		(when (zero? i)
		  (return node))
		(when (and (eq? node org) (not node->right) (not node->left))
		  (return #f))
		(while (or node->right node->left)
		  (cond (node->right
			 (set! last node)
			 (set! node node->right))
			(node->left
			 (set! last node)
			 (set! node node->left)))
		  (-- i)
		  (when (zero? i)
		    (return node))
		  (when (and (eq? node org)
			     (not node->right)
			     (not node->left))
		    (return #f))))
	      (loop i org last))
	  (begin
	    (++ i)
	    (while (or node->right node->left)
	      (cond (node->right
		     (set! last node)
		     (set! node node->right))
		    (node->left
		     (set! last node)
		     (set! node node->left)))
	      (-- i)
	      (when (zero? i)
		(return node))
	      (when (and (eq? node org) (not node->left) (not node->right))
		(return #f)))
	    (loop i org last)))))))


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


;;;; breadth-first search iterators

(module (binary-tree-begin-breadth-first-forwards
	 $binary-tree-begin-breadth-first-forwards
	 binary-tree-step-breadth-first-forwards
	 $binary-tree-step-breadth-first-forwards)
  (import (vicare containers queues))

  (define* (binary-tree-begin-breadth-first-forwards {root false-or-binary-node?})
    ($binary-tree-begin-breadth-first-forwards root))

  (define ($binary-tree-begin-breadth-first-forwards root)
    (if root
	(let ((Q (make-queue)))
	  (cond (($binary-node-left  root)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (cond (($binary-node-right root)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (values Q root))
      (values #f #f)))

;;; --------------------------------------------------------------------

  (define* (binary-tree-step-breadth-first-forwards {Q queue?})
    ($binary-tree-step-breadth-first-forwards Q))

  (define ($binary-tree-step-breadth-first-forwards Q)
    (if ($queue-not-empty? Q)
	(let ((node ($queue-pop! Q)))
	  (cond (($binary-node-left  node)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (cond (($binary-node-right node)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (values Q node))
      (values #f #f)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (binary-tree-begin-breadth-first-backwards
	 $binary-tree-begin-breadth-first-backwards
	 binary-tree-step-breadth-first-backwards
	 $binary-tree-step-breadth-first-backwards)
  (import (vicare containers queues))

  (define* (binary-tree-begin-breadth-first-backwards {root false-or-binary-node?})
    ($binary-tree-begin-breadth-first-backwards root))

  (define ($binary-tree-begin-breadth-first-backwards root)
    (if root
	(let ((Q     (make-queue))
	      (left  ($binary-node-left  root))
	      (right ($binary-node-right root)))
	  (cond (($binary-node-right root)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (cond (($binary-node-left  root)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (values Q root))
      (values #f #f)))

;;; --------------------------------------------------------------------

  (define* (binary-tree-step-breadth-first-backwards {Q queue?})
    ($binary-tree-step-breadth-first-backwards Q))

  (define ($binary-tree-step-breadth-first-backwards Q)
    (if ($queue-not-empty? Q)
	(let ((node ($queue-pop! Q)))
	  (cond (($binary-node-right node)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (cond (($binary-node-left  node)
		 => (lambda (node)
		      ($queue-push! Q node))))
	  (values Q node))
      (values #f #f)))

  #| end of module |# )


;;;; breadth-first search iterators

(define* (binary-tree-fold-breadth-first-forwards {kons procedure?} knil {root false-or-binary-node?})
  ($binary-tree-fold-breadth-first-forwards kons knil root))

(define ($binary-tree-fold-breadth-first-forwards kons knil root)
  (receive (Q node)
      ($binary-tree-begin-breadth-first-forwards root)
    (if node
	(let next ((knil (kons knil node))
		   (Q    Q))
	  (receive (Q node)
	      ($binary-tree-step-breadth-first-forwards Q)
	    (if node
		(next (kons knil node) Q)
	      knil)))
      knil)))

;;; --------------------------------------------------------------------

(define* (binary-tree-fold-breadth-first-backwards {kons procedure?} knil {root false-or-binary-node?})
  ($binary-tree-fold-breadth-first-backwards kons knil root))

(define ($binary-tree-fold-breadth-first-backwards kons knil root)
  (receive (Q node)
      ($binary-tree-begin-breadth-first-backwards root)
    (if node
	(let next ((knil (kons node knil))
		   (Q    Q))
	  (receive (Q node)
	      ($binary-tree-step-breadth-first-backwards Q)
	    (if node
		(next (kons node knil) Q)
	      knil)))
      knil)))


;;;; plain binary trees: iteration thunks

(define-syntax define-iteration-thunk
  (syntax-rules ()
    ((_ ?who ?begin ?step)
     (define* (?who {root false-or-binary-node?})
       (let ((started?     #f)
	     (current-node root))
	 (lambda ()
	   (cond ((not current-node)
		  (void))
		 (started?
		  (cond ((?step current-node)
			 => (lambda (next-node)
			      (set! current-node next-node)
			      next-node))
			(else
			 (set! current-node #f)
			 (void))))
		 ((?begin current-node)
		  => (lambda (next-node)
		       (set! started? #t)
		       (set! current-node next-node)
		       next-node))
		 (else
		  (set! current-node #f)
		  (void)))))))
    ))

;;; --------------------------------------------------------------------

(define-iteration-thunk make-binary-tree-forwards-in-order-iteration-thunk
  $binary-tree-begin-in-order-forwards
  $binary-tree-step-in-order-forwards)

(define-iteration-thunk make-binary-tree-forwards-pre-order-iteration-thunk
  $binary-tree-begin-pre-order-forwards
  $binary-tree-step-pre-order-forwards)

(define-iteration-thunk make-binary-tree-forwards-post-order-iteration-thunk
  $binary-tree-begin-post-order-forwards
  $binary-tree-step-post-order-forwards)

(define-iteration-thunk make-binary-tree-forwards-level-order-iteration-thunk
  $binary-tree-begin-level-order-forwards
  $binary-tree-step-level-order-forwards)

;;; --------------------------------------------------------------------

(define-iteration-thunk make-binary-tree-backwards-in-order-iteration-thunk
  $binary-tree-begin-in-order-backwards
  $binary-tree-step-in-order-backwards)

(define-iteration-thunk make-binary-tree-backwards-pre-order-iteration-thunk
  $binary-tree-begin-pre-order-backwards
  $binary-tree-step-pre-order-backwards)

(define-iteration-thunk make-binary-tree-backwards-post-order-iteration-thunk
  $binary-tree-begin-post-order-backwards
  $binary-tree-step-post-order-backwards)

(define-iteration-thunk make-binary-tree-backwards-level-order-iteration-thunk
  $binary-tree-begin-level-order-backwards
  $binary-tree-step-level-order-backwards)


;;;; plain binary trees: breadth-first iteration thunks

(define-syntax define-breadth-first-iteration-thunk
  (syntax-rules ()
    ((_ ?who ?begin ?step)
     (define* (?who {root false-or-binary-node?})
       (let ((queue #f))
	 (lambda ()
	   (cond ((not root)
		  (void))
		 (queue
		  (receive (Q next-node)
		      (?step queue)
		    (if next-node
			(begin
			  (set! queue Q)
			  next-node)
		      (void))))
		 (else
		  (receive (Q first-node)
		      (?begin root)
		    (if first-node
			(begin
			  (set! queue Q)
			  first-node)
		      (void)))))))))
    ))

(define-breadth-first-iteration-thunk make-binary-tree-forwards-breadth-first-iteration-thunk
  $binary-tree-begin-breadth-first-forwards
  $binary-tree-step-breadth-first-forwards)

(define-breadth-first-iteration-thunk make-binary-tree-backwards-breadth-first-iteration-thunk
  $binary-tree-begin-breadth-first-backwards
  $binary-tree-step-breadth-first-backwards)


;;;; unbalanced binary nodes: data type

(define-record-type (<unbalanced-binary-node> make-unbalanced-binary-node unbalanced-binary-node?)
  (nongenerative vicare:containers:<unbalanced-binary-node>)

  (parent <binary-node>)

  (protocol
   (lambda (make-binary-node)
     (case-define* make-<unbalanced-binary-node>
       (()
	((make-binary-node #f #f)))

       (({left false-or-unbalanced-binary-node?} {right false-or-unbalanced-binary-node?})
	((make-binary-node left right))))

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

(define* (unbalanced-tree-remove! {node unbalanced-binary-node?} {root unbalanced-binary-node?})
  ($unbalanced-tree-remove! node root))

(define ($unbalanced-tree-remove! node root)
  (let ((left  ($binary-node-left  node))
	(right ($binary-node-right node)))
    (cond ((and left right)
	   ;;Replace NODE with the maximum in the left subtree.
	   (let* ((lmax     ($binary-tree-maximum left #f))
		  (lmax.dad ($binary-node-parent  lmax)))
	     (if (eq? lmax left)
		 ;;The scenario before the removal is:
		 ;;
		 ;;    node--right
		 ;;     |
		 ;;    lmax--#f
		 ;;     |
		 ;;    subleft
		 ;;
		 ;;we know  that LMAX has no  right subtree.  The scenario  after the
		 ;;removal must be:
		 ;;
		 ;;    lmax--right
		 ;;     |
		 ;;    subleft
		 ;;
		 (begin
		   ($binary-node-replace-in-parent! node lmax)
		   ($binary-node-right-set! lmax right))
	       ;;Insert LMAX in the  tree in place of NODE.  If we  are here and LMAX
	       ;;is the left-maximum,  by definition of BST: LMAX is  the right child
	       ;;of LMAX.DAD; LMAX has no right child; LMAX might have a left child.
	       ;;
	       ;;The scenario before the removal is:
	       ;;
	       ;;    node--right
	       ;;     |
	       ;;    left--...
	       ;;           |
	       ;;          lmax.dad--lmax--#f
	       ;;                     |
	       ;;                  subleft
	       ;;
	       ;;the scenario after the removal must be:
	       ;;
	       ;;    lmax--right
	       ;;     |
	       ;;    left--...
	       ;;           |
	       ;;          lmax.dad--subleft
	       ;;
	       ;;Notice that in the special case before the removal:
	       ;;
	       ;;    node--right
	       ;;     |
	       ;;    lmax.dad--lmax--#f
	       ;;               |
	       ;;             subleft
	       ;;
	       ;;the scenario after the removal must be:
	       ;;
	       ;;    lmax--right
	       ;;     |
	       ;;    lmax.dad--subleft
	       ;;
	       (begin
		 ($binary-node-replace-in-parent! node lmax)
		 ($binary-node-right-set! lmax.dad ($binary-node-left lmax))
		 ($binary-node-left-set!  lmax left)
		 ($binary-node-right-set! lmax right)))
	     ;;Reset NODE links.
	     ($<binary-node>-parent-set! node #f)
	     ($<binary-node>-left-set!   node #f)
	     ($<binary-node>-right-set!  node #f)
	     (if (eq? node root)
		 lmax
	       root)))

	  (left
	   ;;Left child exists, right child does not.
	   ;;
	   ($binary-node-replace-in-parent! node left)
	   (if (eq? node root)
	       left
	     root))

	  (right
	   ;;Right child exists, left child does not.
	   ;;
	   ($binary-node-replace-in-parent! node right)
	   (if (eq? node root)
	       right
	     root))

	  (else
	   ;;No children.  Remove this node from its parent.
	   ($binary-node-replace-in-parent! node #f)
	   ;;Reset this node.
	   ($<binary-node>-parent-set! node #f)
	   ($<binary-node>-left-set!   node #f)
	   ($<binary-node>-right-set!  node #f)
	   (if (eq? node root)
	       #f
	     root)))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
