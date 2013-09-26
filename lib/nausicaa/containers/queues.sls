;;;
;;;Part of: Vicare Scheme
;;;Contents: queue class definition
;;;Date: Wed Oct 14, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa containers queues)
  (export
    <queue>
    (rename (queue.vicare-arguments-validation
	     <queue>.vicare-arguments-validation)
	    (queue/false.vicare-arguments-validation
	     <queue>/false.vicare-arguments-validation))
    list->queue
    vector->queue)
  (import (nausicaa)
    (vicare containers queues))
  (define-label <queue>
    (protocol (lambda () make-queue))
    (predicate queue?)
    (virtual-fields
     (immutable (empty?		<boolean>)	queue-empty?)
     (immutable ($empty?	<boolean>)	$queue-empty?)
     (immutable (not-empty?	<boolean>)	queue-not-empty?)
     (immutable ($not-empty?	<boolean>)	$queue-not-empty?)
     (immutable (size		<integer>)	queue-size)
     (immutable ($size		<integer>)	$queue-size))
    (methods
     (hash		queue-hash)
     ($hash		$queue-hash)

     (putprop		queue-putprop)
     ($putprop		$queue-putprop)
     (getprop		queue-getprop)
     ($getprop		$queue-getprop)
     (remprop		queue-remprop)
     ($remprop		$queue-remprop)
     (property-list	queue-property-list)
     ($property-list	$queue-property-list)

     (front		queue-front)
     ($front		$queue-front)
     (rear		queue-rear)
     ($rear		$queue-rear)
     (push!		queue-push!)
     ($push!		$queue-push!)
     (pop!		queue-pop!)
     ($pop!		$queue-pop!)
     (purge!		queue-purge!)
     ($purge!		$queue-purge!)

     (list		queue->list)
     ($list		$queue->list)
     (vector		queue->vector)
     ($vector		$queue->vector))
    #| end of label |# )
  #| end of library |# )

;;; end of file
