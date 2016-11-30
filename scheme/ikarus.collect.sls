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
    post-gc-hooks		automatic-garbage-collection
    automatic-collect

    register-to-avoid-collecting
    forget-to-avoid-collecting
    replace-to-avoid-collecting
    retrieve-to-avoid-collecting
    collection-avoidance-list
    purge-collection-avoidance-list)
  (import (except (vicare)
		  collect		collect-key
		  post-gc-hooks		automatic-garbage-collection
		  automatic-collect

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


;;;; garbage collection API

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

(module (do-overflow collect automatic-collect collect-key)

  (define (do-overflow number-of-words)
    ;;This function is called whenever a  Scheme function tries to allocate an object
    ;;on the heap  and the heap's nursery  has not enough room for  it.  An automatic
    ;;garbage collection  is run to  reclaim some heap space  and we expect  that, at
    ;;return time, the heap has enough room to allocate NUMBER-OF-WORDS bytes.
    ;;
    (foreign-call "ik_automatic_collect_from_scheme_with_hooks" number-of-words #f)
    (%post-gc-operations number-of-words #t))

  (case-define* automatic-collect
    ;;Call for  a garbage  collection and  make room on  the heap  for at  least 4096
    ;;machine words, by behaving like DO-OVERFLOW.   4096 is an arbitrary value equal
    ;;to a  Vicare's page size.   It is arbitrarily  decided that this  function must
    ;;return a single value and such value is void.
    ;;
    (()
     (%do-collect __who__ #f #t))
    ((requested-generation)
     (%do-collect __who__ requested-generation #t)))

  (case-define* collect
    ;;Force a garbage collection and make room  on the heap for at least 4096 machine
    ;;words.   4096 is  an arbitrary  value equal  to a  Vicare's page  size.  It  is
    ;;arbitrarily decided  that this  function must  return a  single value  and such
    ;;value is void.
    ;;
    (()
     (%do-collect __who__ #f #f))
    ((requested-generation)
     (%do-collect __who__ requested-generation #f)))

  (define (%do-collect who requested-generation automatic?)
    (let ((number-of-words       4096)
	  (requested-generation  (case requested-generation
				   ((0 fastest)		0)
				   ((#f 1 2 3)		requested-generation)
				   ((4 fullest)		4)
				   (else
				    (procedure-argument-violation who
				      "requested invalid garbage collection generation level"
				      requested-generation)))))
      (if automatic?
	  (foreign-call "ik_automatic_collect_from_scheme_with_hooks" number-of-words requested-generation)
	(foreign-call "ik_explicit_collect_from_scheme_with_hooks" number-of-words requested-generation))
      (%post-gc-operations number-of-words automatic?))
    (values))

  (define (%post-gc-operations number-of-words automatic?)
    (let ((ls (post-gc-hooks)))
      (unless (null? ls)
	(%do-post-gc ls number-of-words automatic?)))
    ;;NOTE Do  *not* remove  this.  The  code calling this  function to  reclaim heap
    ;;space expects DO-OVERFLOW to return a single  value; if it returns 0, 2 or more
    ;;values very  bad assembly-level errors will  happen.  (Marco Maggi; Thu  Apr 4,
    ;;2013)
    (void))

  (define (%do-post-gc ls number-of-words automatic?)
    (let ((k0 (collect-key)))
      ;;Run the hook functions.
      (parameterize ((post-gc-hooks '()))
	;;Run the post-gc hooks.
	(for-each (lambda (x) (x)) ls))
      (if (eq? k0 (collect-key))
	  ;;If we are  here: the post-GC hooks were called  without causing a further
	  ;;GC.
	  (let ((done-without-further-gc? (if automatic?
					      (foreign-call "ikrt_automatic_collect_from_scheme_check_after_gc_hooks" number-of-words)
					    (foreign-call "ikrt_explicit_collect_from_scheme_check_after_gc_hooks" number-of-words))))
	    (unless done-without-further-gc?
	      ;;If we are here: after running  the post-GC hooks there was not enough
	      ;;room on the heap's nursery to serve the pending memory allocation, so
	      ;;another  GC was  run.   This  further GC  might  have enqueued  other
	      ;;post-GC hooks: do the handlers again.
	      (%do-post-gc ls number-of-words automatic?)))
	;;Running the  post-GC hooks did  cause a GC;  some more post-GC  hooks might
	;;have been accumulated: do the handlers again.
	(%do-post-gc ls number-of-words automatic?))))

  (define (collect-key)
    ;;The core primitive operation $COLLECT-KEY is  a getter and setter for the value
    ;;of the field "collect_key" in the C language structure PCB.
    ;;
    (or ($collect-key)
	(begin
	  ($collect-key (gensym "collect-key"))
	  (collect-key))))

  #| end of module |# )

;;This  function is  called  when we  apply  a  tuple of  operands  a closure  object
;;accepting a variable number of arguments.  The Assembly code may need to allocate a
;;Scheme list to hold  the args argument or the rest argument:  if the heap's nursery
;;runs out of room while this allocation goes on, this function is called.
;;
(define do-vararg-overflow
  do-overflow)

(case-define automatic-garbage-collection
  ;;We want this function to be compatible with the parameters API, so we also need a
  ;;2 arguments branch.
  ;;
  (()
   (foreign-call "ikrt_automatic_garbage_collection_status"))
  ((obj)
   (foreign-call "ikrt_enable_disable_automatic_garbage_collection" obj))
  ((obj unused)
   (foreign-call "ikrt_enable_disable_automatic_garbage_collection" obj)))

(define (do-stack-overflow)
  (foreign-call "ik_stack_overflow"))

(define (dump-metatable)
  (foreign-call "ik_dump_metatable"))

(define (dump-dirty-vector)
  (foreign-call "ik_dump_dirty_vector"))


;;;; Scheme objects garbage collection avoidance API

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
  (foreign-call "ik_purge_collection_avoidance_list")
  (values))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.collect end")))

#| end of library |# )

;;; end of file
