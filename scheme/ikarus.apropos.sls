;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
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

(library (ikarus.apropos)
  (export apropos)
  (import (except (ikarus)
		  apropos)
    (only (psyntax library-manager)
	  library-subst library-name)
    (prefix (vicare unsafe operations)
	    unsafe.))


(define (apropos key)
  ;;Defined by Ikarus.   Given a string or symbol  KEY, search among the
  ;;internally installed libraries all  the exported bindings having KEY
  ;;as substring of their name and print a report to the standard output
  ;;port.
  ;;
  (for-each (lambda (x)
	      (display "*** in library ")
	      (display (car x))
	      (display ":\n")
	      (pretty-print (cdr x))
	      (newline))
    ($apropos-list key 'apropos)))

(define (match-maker str1)
  ;;Given a matcher  string STR1, return a matcher  closure accepting as
  ;;argument a pair whose car is a string STR2.
  ;;
  ;;The matcher closure compares STR2 to  STR1 returning #t if STR1 is a
  ;;substring of STR2, else it returns #f.
  ;;
  (let ((len1 (unsafe.string-length str1)))
    (lambda (x)
      (let* ((str2 (symbol->string (car x)))
	     (len2 (unsafe.string-length str2))
	     (m    (unsafe.fx- len2 len1)))
	(let outer ((idx2 0))
	  (and (unsafe.fx<= idx2 m)
	       (or (let inner ((idx1 0)
			       (idx2 idx2))
		     (or (unsafe.fx= idx1 len1)
			 (and (unsafe.char= (unsafe.string-ref str1 idx1)
					    (unsafe.string-ref str2 idx2))
			      (inner (unsafe.fxadd1 idx1)
				     (unsafe.fxadd1 idx2)))))
		   (outer (unsafe.fxadd1 idx2)))))))))

(define ($apropos-list name who)
  (define matcher
    (match-maker (cond ((string? name)	name)
		       ((symbol? name)	(symbol->string name))
		       (else
			(assertion-violation who
			  "expected string or symbol as apropos search key" name)))))
  (define (symbol<? s1 s2)
    (string<? (symbol->string s1)
	      (symbol->string s2)))
  (fold-right (lambda (lib rest)
		;;The  return  value of  LIBRARY-SUBST  is  the list  of
		;;substitutions  for  the  identifiers exported  by  the
		;;library.
		(let ((ls (filter matcher (library-subst lib))))
		  (if (null? ls)
		      rest
		    (let ((ls (list-sort symbol<? (map car ls))))
		      (cons (cons (library-name lib) ls) rest)))))
    '()
    (list-sort (lambda (lib1 lib2)
		 ;;Compare the  components of a library  name (which are
		 ;;symbols)  and   return  #t  if   LIB1's  symbols  are
		 ;;lexicographically lesser than LIB2's symbols.
		 ;;
		 (let loop ((ls1 (library-name lib1))
			    (ls2 (library-name lib2)))
		   (and (pair? ls2)
			(or (null? ls1)
			    (let ((s1 (symbol->string (unsafe.car ls1)))
				  (s2 (symbol->string (unsafe.car ls2))))
			      (or (string<? s1 s2)
				  (and (string=? s1 s2)
				       (loop (unsafe.cdr ls1) (unsafe.cdr ls2)))))))))
	       (installed-libraries))))


;;;; done

)

;;; end of file
