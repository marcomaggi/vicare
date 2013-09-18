;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: OOPP interface to extended vector functions
;;;Date: Wed Sep 18, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa containers vectors)
  (export <xvector>)
  (import (except (nausicaa)
		  vector-append
		  vector-copy
		  vector-copy!)
    (vicare containers vectors))


(define-label <xvector>
  (parent <vector>)
  (predicate vector?)
  (protocol (lambda () vector))

  ;; constructors
  (method-syntax concatenate
    (syntax-rules ()
      ((_ o ?list-of-vectors)
       (vector-concatenate (cons o ?list-of-vectors)))))

  (method-syntax concatenate-reverse
    (syntax-rules ()
      ((_ o ?list-of-vectors)
       (vector-concatenate-reverse (cons o ?list-of-vectors)))
      ((_ o ?list-of-vectors ?final-vector)
       (vector-concatenate-reverse (cons o ?list-of-vectors) ?final-vector))
      ((_ o ?list-of-vectors ?final-vector ?nvalues)
       (vector-concatenate-reverse (cons o ?list-of-vectors) ?final-vector ?nvalues))
      ))

  (method-syntax append
    (syntax-rules ()
      ((_ o . ?args)
       (vector-append o . ?args))))

  ;; predicates
  (method-syntax null?
    (syntax-rules ()
      ((_ o . ?args)
       (vector-null? o . ?args))))

  (method-syntax every
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-every ?proc  o . ?args))))

  (method-syntax any
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-any ?proc o . ?args))))

  ;; comparison
  (method-syntax =
    (syntax-rules ()
      ((_ o ?item= . ?args)
       (vector= ?item= o . ?args))))

  (method-syntax <>
    (syntax-rules ()
      ((_ o ?item= . ?args)
       (vector<> ?item= o . ?args))))

  (method-syntax <
    (syntax-rules ()
      ((_ o ?item= ?item< . ?args)
       (vector< ?item= ?item< o . ?args))))

  (method-syntax <=
    (syntax-rules ()
      ((_ o ?item= ?item< . ?args)
       (vector<= ?item= ?item< o . ?args))))

  (method-syntax >
    (syntax-rules ()
      ((_ o ?item= ?item< . ?args)
       (vector> ?item= ?item< o . ?args))))

  (method-syntax >=
    (syntax-rules ()
      ((_ o ?item= ?item< . ?args)
       (vector>= ?item= ?item< o . ?args))))

  ;; mapping
  (method-syntax map/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map/with-index ?proc o . ?args))))

  (method-syntax map!
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map! ?proc o . ?args))))

  (method-syntax map!/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map!/with-index ?proc o . ?args))))

  (method-syntax map*
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map* ?proc o . ?args))))

  (method-syntax map*/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map*/with-index ?proc o . ?args))))

  (method-syntax map*!
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map*! ?proc o . ?args))))

  (method-syntax map*!/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map*!/with-index ?proc o . ?args))))

  (method-syntax for-each*
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-for-each* ?proc o . ?args))))

  (method-syntax for-each*/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-for-each*/with-index ?proc o . ?args))))

  (method-syntax subvector-map
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (subvector-map ?proc o . ?args))))

  (method-syntax subvector-map/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (subvector-map/with-index ?proc o . ?args))))

  (method-syntax subvector-map!
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (subvector-map! ?proc o . ?args))))

  (method-syntax subvector-map!/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (subvector-map!/with-index ?proc o . ?args))))

  (method-syntax subvector-for-each
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (subvector-for-each ?proc o . ?args))))

  (method-syntax subvector-for-each/with-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (subvector-for-each/with-index ?proc o . ?args))))

  (method-syntax subvector-for-each-index
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (subvector-for-each-index ?proc o . ?args))))


  (method-syntax map/stx
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map/stx ?proc o . ?args))))

  (method-syntax map*/stx
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map*/stx ?proc o . ?args))))

  (method-syntax map!/stx
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map!/stx ?proc o . ?args))))

  (method-syntax map*!/stx
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-map*!/stx ?proc o . ?args))))

  (method-syntax for-each/stx
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-for-each/stx ?proc o . ?args))))

  (method-syntax for-each*/stx
    (syntax-rules ()
      ((_ o ?proc . ?args)
       (vector-for-each*/stx ?proc o . ?args))))

  ;; folding
  (method-syntax fold-left
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-left ?proc ?knil o . ?args))))

  (method-syntax fold-right
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-right ?proc ?knil o . ?args))))

  (method-syntax fold-left*
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-left* ?proc ?knil o . ?args))))

  (method-syntax fold-right*
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-right* ?proc ?knil o . ?args))))

  (method-syntax fold-left/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-left/stx ?proc ?knil o . ?args))))

  (method-syntax fold-right/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-right/stx ?proc ?knil o . ?args))))

  (method-syntax fold-left*/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-left*/stx ?proc ?knil o . ?args))))

  (method-syntax fold-right*/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-right*/stx ?proc ?knil o . ?args))))

  (method-syntax fold-left/with-index
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-left/with-index ?proc ?knil o . ?args))))

  (method-syntax fold-right/with-index
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-right/with-index ?proc ?knil o . ?args))))

  (method-syntax fold-left*/with-index
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-left*/with-index ?proc ?knil o . ?args))))

  (method-syntax fold-right*/with-index
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-right*/with-index ?proc ?knil o . ?args))))

  (method-syntax subvector-fold-left
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (subvector-fold-left ?proc ?knil o . ?args))))

  (method-syntax subvector-fold-right
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (subvector-fold-right ?proc ?knil o . ?args))))

  (method-syntax and-fold-left
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-left ?proc ?knil o . ?args))))

  (method-syntax and-fold-right
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-right ?proc ?knil o . ?args))))

  (method-syntax and-fold-left*
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-left* ?proc ?knil o . ?args))))

  (method-syntax and-fold-right*
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-right* ?proc ?knil o . ?args))))

  (method-syntax and-fold-left/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-left/stx ?proc ?knil o . ?args))))

  (method-syntax and-fold-right/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-right/stx ?proc ?knil o . ?args))))

  (method-syntax and-fold-left*/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-left*/stx ?proc ?knil o . ?args))))

  (method-syntax and-fold-right*/stx
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-and-fold-right*/stx ?proc ?knil o . ?args))))

  (method-syntax fold-left/pred
    (syntax-rules ()
      ((_ o ?proc ?knil . ?args)
       (vector-fold-left/pred ?proc ?knil o . ?args))))

  ;; selecting
  (method-syntax subvector
    (syntax-rules ()
      ((_ o . ?args)
       (subvector o . ?args))))

  (method-syntax subvector*
    (syntax-rules ()
      ((_ o . ?args)
       (subvector* o . ?args))))

  (method-syntax copy
    (syntax-rules ()
      ((_ o . ?args)
       (vector-copy o . ?args))))

  (method-syntax reverse-copy
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse-copy o . ?args))))

  (method-syntax copy!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-copy! o . ?args))))

  (method-syntax reverse-copy!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse-copy! o . ?args))))

  (method-syntax take
    (syntax-rules ()
      ((_ o . ?args)
       (vector-take o . ?args))))

  (method-syntax take-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-take-right o . ?args))))

  (method-syntax drop
    (syntax-rules ()
      ((_ o . ?args)
       (vector-drop o . ?args))))

  (method-syntax drop-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-drop-right o . ?args))))


  ;; padding and trimming
  (method-syntax trim
    (syntax-rules ()
      ((_ o . ?args)
       (vector-trim o . ?args))))

  (method-syntax trim-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-trim-right o . ?args))))

  (method-syntax trim-both
    (syntax-rules ()
      ((_ o . ?args)
       (vector-trim-both o . ?args))))

  (method-syntax pad
    (syntax-rules ()
      ((_ o . ?args)
       (vector-pad o . ?args))))

  (method-syntax pad-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-pad-right o . ?args))))


  ;; prefix and suffix
  (method-syntax prefix-length
    (syntax-rules ()
      ((_ o . ?args)
       (vector-prefix-length o . ?args))))

  (method-syntax suffix-length
    (syntax-rules ()
      ((_ o . ?args)
       (vector-suffix-length o . ?args))))

  (method-syntax prefix?
    (syntax-rules ()
      ((_ o . ?args)
       (vector-prefix? o . ?args))))

  (method-syntax suffix?
    (syntax-rules ()
      ((_ o . ?args)
       (vector-suffix? o . ?args))))


  ;; searching
  (method-syntax index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-index o . ?args))))

  (method-syntax index-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-index-right o . ?args))))

  (method-syntax skip
    (syntax-rules ()
      ((_ o . ?args)
       (vector-skip o . ?args))))

  (method-syntax skip-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-skip-right o . ?args))))

  (method-syntax count
    (syntax-rules ()
      ((_ o . ?args)
       (vector-count o . ?args))))

  (method-syntax contains
    (syntax-rules ()
      ((_ o . ?args)
       (vector-contains o . ?args))))

  (method-syntax binary-search
    (syntax-rules ()
      ((_ o . ?args)
       (vector-binary-search o . ?args))))


  ;; filtering
  (method-syntax filter
    (syntax-rules ()
      ((_ o . ?args)
       (vector-filter o . ?args))))

  (method-syntax delete
    (syntax-rules ()
      ((_ o . ?args)
       (vector-delete o . ?args))))


  ;; lists
  (method-syntax list*
    (syntax-rules ()
      ((_ o . ?args)
       (vector->list* o . ?args))))

  (method-syntax reverse-vector->list
    (syntax-rules ()
      ((_ o . ?args)
       (reverse-vector->list o . ?args))))

  ;; replicating
  (method-syntax xsubvector
    (syntax-rules ()
      ((_ o . ?args)
       (xsubvector o . ?args))))

  (method-syntax xcopy!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-xcopy! o . ?args))))


  ;; mutating
  (method-syntax fill*!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fill*! o . ?args))))

  (method-syntax swap!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-swap! o . ?args))))


  ;; reverse and replace
  (method-syntax reverse
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse o . ?args))))

  (method-syntax reverse!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse! o . ?args))))

  (method-syntax replace
    (syntax-rules ()
      ((_ o . ?args)
       (vector-replace o . ?args))))

  )


;;;; done

)

;;; end of file
