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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus collect)
  (export
    do-overflow
    do-vararg-overflow		do-stack-overflow
    collect			collect-key
    post-gc-hooks

    register-to-avoid-collecting
    forget-to-avoid-collecting
    replace-to-avoid-collecting
    retrieve-to-avoid-collecting
    collection-avoidance-list
    purge-collection-avoidance-list)
  (import (except (vicare)
		  collect		collect-key
		  post-gc-hooks

		  ;;FIXME  This except  is  to  be removed  at  the  next boot  image
		  ;;rotation.  (Marco Maggi; Sun Mar 29, 2015)
		  non-null-pointer?

		  register-to-avoid-collecting
		  forget-to-avoid-collecting
		  replace-to-avoid-collecting
		  retrieve-to-avoid-collecting
		  collection-avoidance-list
		  purge-collection-avoidance-list)
    (vicare system $fx)
    (vicare system $arg-list))


;;;; helpers

(define (non-null-pointer? obj)
  (and (pointer? obj)
       (not (pointer-null? obj))))


(define post-gc-hooks
  (make-parameter '()
    (lambda (ls)
      ;;NULL? check so that we don't  reference LIST? and ANDMAP at this
      ;;stage of booting.
      (if (or (null? ls)
	      (and (list? ls)
		   (andmap procedure? ls)))
          ls
	(assertion-violation 'post-gc-hooks "not a list of procedures" ls)))))

(define (do-post-gc ls number-of-words)
  (let ((k0 (collect-key)))
    ;;Run the hook functions.
    (parameterize ((post-gc-hooks '()))
      ;;Run the post-gc hooks.
      (for-each (lambda (x) (x)) ls))
    (if (eq? k0 (collect-key))
        (let ((was-enough? (foreign-call "ikrt_collect_check_after_gc_hooks" number-of-words)))
          ;;Handlers ran  without GC but  there was not enough  space in
          ;;the nursery for the pending allocation.
          (unless was-enough?
	    (do-post-gc ls number-of-words)))
      ;;Handlers did cause a GC, so, do the handlers again.
      (do-post-gc ls number-of-words))))

(case-define do-overflow
  ;;This function is called whenever a Scheme function tries to allocate an object on
  ;;the heap and the heap has no enough  room for it.  A garbage collection is run to
  ;;reclaim some heap space  and we expect that, at return time,  the heap has enough
  ;;room to allocate NUMBER-OF-WORDS bytes.
  ;;
  ((number-of-words)
   (do-overflow number-of-words #f))
  ((number-of-words requested-generation)
   (assert (or (not requested-generation)
	       (and (fixnum? requested-generation)
		    (<= 0 requested-generation 4))))
   (foreign-call "ik_collect_from_scheme_with_hooks" number-of-words requested-generation)
   (let ((ls (post-gc-hooks)))
     (unless (null? ls)
       (do-post-gc ls number-of-words)))
   ;;NOTE Do *not* remove this.  The code calling this function to reclaim heap space
   ;;expects DO-OVERFLOW to return a single value;  if it returns 0, 2 or more values
   ;;very bad assembly-level errors will happen.  (Marco Maggi; Thu Apr 4, 2013)
   #t))

(define do-vararg-overflow do-overflow)

(case-define* collect
  ;;Force a  garbage collection and make  room on the  heap for at least  4096 bytes.
  ;;4096 is  an arbitrary value.  It  is arbitrarily decided that  this function must
  ;;return a single value and such value is void.
  ;;
  (()
   (do-overflow 4096 #f)
   (void))
  ((requested-generation)
   (do-overflow 4096 (case requested-generation
		       ((0 fastest)	0)
		       ((#f 1 2 3)	requested-generation)
		       ((4 fullest)	4)
		       (else
			(procedure-argument-violation __who__
			  "requested invalid garbage collection generation level"
			  requested-generation))))
   (void)))

(define (do-stack-overflow)
  (foreign-call "ik_stack_overflow"))

(define (dump-metatable)
  (foreign-call "ik_dump_metatable"))

(define (dump-dirty-vector)
  (foreign-call "ik_dump_dirty_vector"))

(define (collect-key)
  (or ($collect-key)
      (begin
        ($collect-key (gensym "collect-key"))
        (collect-key))))


(define (register-to-avoid-collecting obj)
  (foreign-call "ik_register_to_avoid_collecting" obj))

(define* (forget-to-avoid-collecting {ptr pointer?})
  (foreign-call "ik_forget_to_avoid_collecting" ptr))

(define* (replace-to-avoid-collecting {ptr non-null-pointer?} obj)
  (foreign-call "ik_replace_to_avoid_collecting" ptr obj))

(define* (retrieve-to-avoid-collecting {ptr pointer?})
  (foreign-call "ik_retrieve_to_avoid_collecting" ptr))

(define (collection-avoidance-list)
  (foreign-call "ik_collection_avoidance_list"))

(define (purge-collection-avoidance-list)
  (foreign-call "ik_purge_collection_avoidance_list"))


;;;; done

#| end of library |# )

;;; end of file
