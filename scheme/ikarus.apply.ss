;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus apply)
  (export apply)
  (import
    (except (ikarus) apply)
    (ikarus system $pairs)
    (ikarus system $stack))

  (define apply
    (let ()
      (define (err f ls)
        (if (procedure? f)
            (die 'apply "not a list" ls)
            (die 'apply "not a procedure" f)))
      (define (fixandgo f a0 a1 ls p d)
        (cond
          [(null? ($cdr d))
           (let ([last ($car d)])
             ($set-cdr! p last)
             (if (and (procedure? f) (list? last))
                 ($$apply f a0 a1 ls)
                 (err f last)))]
          [else (fixandgo f a0 a1 ls d ($cdr d))]))
      (define apply
        (case-lambda
          [(f ls)
           (if (and (procedure? f) (list? ls))
               ($$apply f ls)
               (err f ls))]
          [(f a0 ls)
           (if (and (procedure? f) (list? ls))
               ($$apply f a0 ls)
               (err f ls))]
          [(f a0 a1 ls)
           (if (and (procedure? f) (list? ls))
               ($$apply f a0 a1 ls)
               (err f ls))]
          [(f a0 a1 . ls)
           (fixandgo f a0 a1 ls ls ($cdr ls))]))
      apply)))
