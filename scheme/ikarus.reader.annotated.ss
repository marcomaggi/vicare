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


(library (ikarus.reader.annotated)
  (export read-source-file read-library-source-file read-script-source-file)
  (import
    (except (ikarus) read-annotated read-script-annotated)
    (only (ikarus.reader) read-annotated read-script-annotated)
    (only (ikarus.io) open-string-input-port/id))

  (define (annotated-port file-name)
    (open-string-input-port/id
      (with-input-from-file file-name 
        (lambda () 
          (let ([x (get-string-all (current-input-port))])
            (if (eof-object? x) "" x))))
      file-name))

  (define (read-library-source-file file-name)
    (read-annotated (annotated-port file-name)))

  (define (read-source-file file-name)
    (let ([p (annotated-port file-name)])
      (let f ()
        (let ([x (read-annotated p)])
          (if (eof-object? x)
              '()
              (cons x (f)))))))

  (define (read-script-source-file file-name)
    (let ([p (annotated-port file-name)])
      (let ([x (read-script-annotated p)])
        (if (eof-object? x)
            (begin (close-input-port p) '())
            (cons x 
              (let f ()
                (let ([x (read-annotated p)])
                  (cond
                    [(eof-object? x) 
                     (close-input-port p)
                     '()]
                    [else (cons x (f))]))))))))
)
