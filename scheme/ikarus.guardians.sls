;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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

;;To understand guardians see:
;;
;;  Dybvig,  Bruggeman, Eby.  "Guardians  in a  Generation-Based Garbage
;;  Collector".   In  Proceedings  of  the  SIGPLAN  '93  Conference  on
;;  Programming Language Design and Implementation, 207-216, June 1993.
;;
;;The procedure MAKE-GUARDIAN is copied verbatim from this paper.
;;
(library (ikarus guardians)
  (export make-guardian)
  (import (except (ikarus) make-guardian))
  (define (make-guardian)
    (let ((tc (let ((x (cons #f #f)))
		(cons x x))))
      (case-lambda
       (()
	(and (not (eq? (car tc) (cdr tc)))
	     (let* ((x (car tc))
		    (y (car x)))
	       (set-car! tc (cdr x))
	       (set-car! x #f)
	       (set-cdr! x #f)
	       y)))
       ((obj)
	(foreign-call "ikrt_register_guardian_pair" (cons tc obj))
	(void))))))

;;; end of file
