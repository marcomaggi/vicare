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
    do-overflow			do-overflow-words
    do-vararg-overflow		do-stack-overflow
    collect			collect-key
    post-gc-hooks

    register-to-avoid-collecting
    forget-to-avoid-collecting
    replace-to-avoid-collecting
    retrieve-to-avoid-collecting
    collection-avoidance-list
    purge-collection-avoidance-list)
  (import (except (ikarus)
		  collect		collect-key
		  post-gc-hooks

		  register-to-avoid-collecting
		  forget-to-avoid-collecting
		  replace-to-avoid-collecting
		  retrieve-to-avoid-collecting
		  collection-avoidance-list
		  purge-collection-avoidance-list)
    (ikarus system $fx)
    (ikarus system $arg-list)
    (vicare syntactic-extensions)
    (vicare arguments validation)
    (ikarus.emergency))


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

(define (do-post-gc ls n)
  (let ((k0 (collect-key)))
    ;;Run the hook functions.
    (parameterize ((post-gc-hooks '()))
      ;;Run the post-gc hooks.
      (for-each (lambda (x) (x)) ls))
    (if (eq? k0 (collect-key))
        (let ((was-enough? (foreign-call "ik_collect_check" n)))
          ;;Handlers ran  without GC but  there was not enough  space in
          ;;the nursery for the pending allocation.
          (unless was-enough?
	    (do-post-gc ls n)))
      ;;Handlers did cause a GC, so, do the handlers again.
      (do-post-gc ls n))))

(define (do-overflow n)
  ;;This function is called whenever a Scheme function tries to allocate
  ;;an object  on the heap and  the heap has  no enough room for  it.  A
  ;;garbage collection is  run to reclaim some heap space  and we expect
  ;;that, at return time, the heap has enough room to allocate N bytes.
  ;;
  (foreign-call "ik_collect" n)
  (let ((ls (post-gc-hooks)))
    (unless (null? ls)
      (do-post-gc ls n)))
  ;;NOTE  Do *not*  remove this.   This  code calling  this function  to
  ;;reclaim heap space expects DO-OVERFLOW  to return a single value; if
  ;;it returns 0,  2 or more values very bad  assembly-level errors will
  ;;happen.  (Marco Maggi; Thu Apr 4, 2013)
  #t)

(define (do-overflow-words n)
  ;;Like DO-OVERFLOW but make room for N words (rather thatn N bytes).
  ;;
  (do-overflow ($fxsll n 2)))

(define do-vararg-overflow do-overflow)

(define (collect)
  ;;Force a  garbage collection and make  room on the heap  for at least
  ;;4096 bytes.  4096 is an  arbitrary value.  It is arbitrarily decided
  ;;that this  function must  return a  single value  and such  value is
  ;;void.
  ;;
  (do-overflow 4096)
  (void))

(define (do-stack-overflow)
  (foreign-call "ik_stack_overflow"))

(define (dump-metatable)
  (foreign-call "ik_dump_metatable"))

(define (dump-dirty-vector)
  (foreign-call "ik_dump_dirty_vector"))

(define (collect-key)
  (or ($collect-key)
      (begin
        ($collect-key (gensym))
        (collect-key))))


(define (register-to-avoid-collecting obj)
  (foreign-call "ik_register_to_avoid_collecting" obj))

(define (forget-to-avoid-collecting ptr)
  (define who 'forget-to-avoid-collecting)
  (with-arguments-validation (who)
      ((pointer	ptr))
    (foreign-call "ik_forget_to_avoid_collecting" ptr)))

(define (replace-to-avoid-collecting ptr obj)
  (define who 'replace-to-avoid-collecting)
  (with-arguments-validation (who)
      ((non-null-pointer	ptr))
    (foreign-call "ik_replace_to_avoid_collecting" ptr obj)))

(define (retrieve-to-avoid-collecting ptr)
  (define who 'retrieve-to-avoid-collecting)
  (with-arguments-validation (who)
      ((pointer	ptr))
    (foreign-call "ik_retrieve_to_avoid_collecting" ptr)))

(define (collection-avoidance-list)
  (foreign-call "ik_collection_avoidance_list"))

(define (purge-collection-avoidance-list)
  (foreign-call "ik_purge_collection_avoidance_list"))


;;;; done

)

;;; end of file
