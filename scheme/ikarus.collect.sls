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
  (export do-overflow do-overflow-words do-vararg-overflow collect
          do-stack-overflow collect-key post-gc-hooks

	  register-to-avoid-collecting
	  forget-to-avoid-collecting
	  replace-to-avoid-collecting
	  retrieve-to-avoid-collecting
	  collection-avoidance-list
	  purge-collection-avoidance-list)
  (import (except (ikarus)
		  collect
		  collect-key
		  post-gc-hooks

		  register-to-avoid-collecting
		  forget-to-avoid-collecting
		  replace-to-avoid-collecting
		  retrieve-to-avoid-collecting
		  collection-avoidance-list
		  purge-collection-avoidance-list)
    (ikarus system $fx)
    (ikarus system $arg-list)
    (vicare syntactic-extensions))


;;;; arguments validation

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (non-null-pointer who obj)
  (and (pointer? obj)
       (not (pointer-null? obj)))
  (assertion-violation who "expected non NULL pointer as argument" obj))


(define post-gc-hooks
  (make-parameter '()
    (lambda (ls)
      ;;; null? check so that we don't reference list? and andmap
      ;;; at this stage of booting.
      (if (or (null? ls) (and (list? ls) (andmap procedure? ls)))
          ls
          (die 'post-gc-hooks "not a list of procedures" ls)))))

(define (do-post-gc ls n)
  (let ([k0 (collect-key)])
    (parameterize ([post-gc-hooks '()])
      ;;FIXME Temporary work  around for issue #35:  disable running the
      ;;post GC hooks.  To be restored after the bug is fixed.
      (void)
      ;;This runs the hook functions.
      #;(for-each (lambda (x) (x)) ls))
    (if (eq? k0 (collect-key))
        (let ([was-enough? (foreign-call "ik_collect_check" n)])
          ;;; handlers ran without GC but there is was not enough
          ;;; space in the nursery for the pending allocation,
          (unless was-enough? (do-post-gc ls n)))
        (let ()
          ;;; handlers did cause a GC, so, do the handlers again.
          (do-post-gc ls n)))))

(define do-overflow
  (lambda (n)
    (foreign-call "ik_collect" n)
    (let ([ls (post-gc-hooks)])
      (unless (null? ls) (do-post-gc ls n)))))

(define do-overflow-words
  (lambda (n)
    (let ([n ($fxsll n 2)])
      (foreign-call "ik_collect" n)
      (let ([ls (post-gc-hooks)])
        (unless (null? ls) (do-post-gc ls n))))))

(define do-vararg-overflow do-overflow)

(define collect
  (lambda ()
    (do-overflow 4096)))

(define do-stack-overflow
  (lambda ()
    (foreign-call "ik_stack_overflow")))

(define dump-metatable
  (lambda ()
    (foreign-call "ik_dump_metatable")))

(define dump-dirty-vector
  (lambda ()
    (foreign-call "ik_dump_dirty_vector")))

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
