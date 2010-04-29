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


(library (ikarus command-line)
  (export command-line-arguments command-line)
  (import 
    (ikarus system $arg-list)
    (except (ikarus) command-line command-line-arguments))

  (define (command-line) (command-line-arguments))
  (define command-line-arguments
    (make-parameter 
      (map (lambda (x) 
             (cond
               [(string? x)  x]
               [(bytevector? x) (utf8->string x)]
               [else (die 'command-line "invalid" x)]))
           ($arg-list))
      (lambda (x)
        (if (and (list? x) (andmap string? x))
            x
            (die 'command-list 
              "invalid command-line-arguments" x))))))

