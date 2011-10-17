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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus.reader.annotated)
  (export read-source-file
	  read-library-source-file
	  read-script-source-file)
  (import (except (ikarus)
		  read-annotated
		  read-library-source-file
		  read-script-annotated)
    (only (ikarus.reader)
	  read-annotated
	  read-script-annotated)
    (only (ikarus.io)
	  open-string-input-port/id))



(define annotated-port open-file-input-port)
#;(define (annotated-port filename)
    (open-string-input-port/id
     (with-input-from-file filename
       (lambda ()
	 (let ((x (get-string-all (current-input-port))))
	   (if (eof-object? x) "" x))))
     filename))

(define (read-library-source-file filename)
  ;;Open FILENAME with  the native transcoder, then read  and return the
  ;;first datum.
  ;;
  (read-annotated (open-input-file filename)))

(define (read-source-file filename)
  ;;Open FILENAME with  the native transcoder, then read  and return all
  ;;the datums in a list.
  ;;
  (let ((port (open-input-file filename)))
    (%read-everything-annotated port (read-annotated port))))

(define (read-script-source-file filename)
  (let ((port (open-input-file filename)))
    (%read-everything-annotated port (read-script-annotated port))))

(define (%read-everything-annotated port obj)
  (if (eof-object? obj)
      (begin
	(close-input-port port)
	'())
    (cons obj (%read-everything-annotated port (read-annotated port)))))


;;;; done

)

;;; end of file
