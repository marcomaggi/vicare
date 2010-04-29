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


(library (ikarus promises)
  (export force make-promise)
  (import 
    (except (ikarus) force make-promise))

  (define (force x)
    (unless (procedure? x) 
      (die 'force "not a procedure" x))
    (x))

  (define (make-promise proc)
    (unless (procedure? proc)
      (die 'make-promise "not a procedure" proc))
    (let ([results #f])
      (lambda () 
        (if results
            (apply values results)
            (call-with-values proc
              (lambda x*
                (if results
                    (apply values results) 
                    (begin
                      (set! results x*) 
                      (apply values x*))))))))))

