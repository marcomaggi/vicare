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
    <binary-node>			make-binary-node
    binary-node?			false-or-binary-node?

    binary-node-object			$binary-node-object
    binary-node-left			$binary-node-left
    binary-node-right			$binary-node-right
    )
  (import (vicare))


(define-record-type (<binary-node> make-binary-node binary-node?)
  (nongenerative vicare:containers:<binary-node>)

  (fields (mutable object)
		;The payload object.
	  (mutable left)
		;False or an instance of "<binary-node>" being the left subtree.
	  (mutable right)
		;False or an instance of "<binary-node>" being the right subtree.
	  #| end of FIELDS |# )

  (protocol
   (lambda (make-record)
     (case-define* make-<binary-tree>
       (()
	(make-<binary-tree> (void) #f #f))

       ((object)
	(make-<binary-tree> object #f #f))

       ((object {left false-or-binary-node?} {right false-or-binary-node?})
	(make-record object left right)))

     make-<binary-tree>))

  #| end of DEFINE-RECORD-TYPE |# )

(define (false-or-binary-node? obj)
  (or (not obj)
      (binary-node? obj)))

(define-alias  binary-node-left			 <binary-node>-left)
(define-alias $binary-node-left			$<binary-node>-left)

(define-alias  binary-node-right		 <binary-node>-right)
(define-alias $binary-node-right		$<binary-node>-right)

(define-alias  binary-node-object		 <binary-node>-object)
(define-alias $binary-node-object		$<binary-node>-object)


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
