;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2009  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
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


#!r6rs
(library (vicare include)
  (export
    include include/lexical-context
    include/verbose)
  (import (ikarus)
    (only (vicare $posix)
	  file-modification-time
	  real-pathname))


(define-syntax include/lexical-context/form
  (lambda (x)
    (syntax-case x ()
      ((_ filename id form who)
       (let* ((filename
	       (let ((x (syntax->datum #'filename)))
		 (if (and (string? x) (not (string=? x "")))
		     (if (char=? (string-ref x 0) #\/)
			 x
		       (let f ((ls (library-path)))
			 (if (null? ls)
			     (syntax-violation #f
			       "file does not exist in library path"
			       #'form #'filename)
			   (let ((x (string-append (car ls) "/" x)))
			     (if (file-exists? x)
				 (real-pathname x)
			       (f (cdr ls)))))))
		   (syntax-violation #f
		     "file name must be a nonempty string"
		     #'form #'filename))))
	      (content
	       (with-exception-handler
                   (lambda (x)
                     (raise-continuable
		      (condition
		       (make-who-condition (syntax->datum #'who))
		       x)))
		 (lambda ()
		   (with-input-from-file filename
		     (lambda ()
		       (let loop ()
			 (let ((x (get-annotated-datum (current-input-port))))
			   (if (eof-object? x)
			       '()
			     (cons (datum->syntax #'id x)
                                   (loop)))))))))))
	 #`(stale-when
	    (or (not (file-exists? #,filename))
		(> (file-modification-time #,filename) #,(file-modification-time filename)))
	    #,@content))))))

(define-syntax include/lexical-context
  (lambda (x)
    (syntax-case x ()
      ((kwd filename id)
       (if (identifier? #'id)
	   #`(include/lexical-context/form filename id #,x kwd)
	 (syntax-violation #f "not an identifier" x #'id))))))

(define-syntax include
  (lambda (x)
    (syntax-case x ()
      ((kwd filename)
       #`(include/lexical-context/form filename kwd #,x kwd)))))

(define-syntax include/verbose
  (lambda (x)
    (syntax-case x ()
      ((?kwd ?filename)
       (begin
	 (display (string-append " including: " (syntax->datum #'?filename) "\n")
		  (current-error-port))
	 #`(include/lexical-context/form ?filename ?kwd #,x ?kwd))))))


;;;; done

)

;;; end of file
