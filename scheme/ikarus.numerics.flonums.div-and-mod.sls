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


#!vicare
(library (ikarus flonums div-and-mod)
  (export
    fldiv		flmod
    fldiv0		flmod0
    fldiv-and-mod	fldiv0-and-mod0

    $fldiv		$flmod
    $fldiv0		$flmod0
    $fldiv-and-mod	$fldiv0-and-mod0)
  (import (except (vicare)
		  fldiv			flmod
		  fldiv0		flmod0
		  fldiv-and-mod		fldiv0-and-mod0)
    (vicare system $fx)
    (except (vicare system $flonums)
	    $fldiv		$flmod
	    $fldiv0		$flmod0
	    $fldiv-and-mod	$fldiv0-and-mod0
	    ;;FIXME  This  last except  is  to  be removed  at  the  next boot  image
	    ;;rotation.  (Marco Maggi; Sun Dec 4, 2016)
	    $flexact)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Sun Dec 4,
    ;;2016)
    (only (ikarus numerics flonums)
	  $flexact)
    #| end of IMPORT |# )


;;;; helpers

(define-syntax define-fl-operation/two-args
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define* (?safe-who {x flonum?} {y flonum?})
       (?unsafe-who x y)))
    ))


(define-fl-operation/two-args fldiv		$fldiv)
(define-fl-operation/two-args flmod		$flmod)
(define-fl-operation/two-args fldiv-and-mod	$fldiv-and-mod)
(define-fl-operation/two-args fldiv0		$fldiv0)
(define-fl-operation/two-args flmod0		$flmod0)
(define-fl-operation/two-args fldiv0-and-mod0	$fldiv0-and-mod0)

(define ($flmod n m)
  (cond (($flnan? n)
	 n)
	(($flnan? m)
	 m)
	(else
	 (let ((d0 ($fltruncate ($fl/ n m))))
	   (let ((m0 ($fl- n ($fl* d0 m))))
	     (if ($fl>= m0 0.0)
		 m0
	       (if ($fl>= m 0.0)
		   ($fl+ m0 m)
		 ($fl- m0 m))))))))

(define ($fldiv n m)
  (cond (($flnan? n)
	 n)
	(($flnan? m)
	 m)
	(else
	 (let ((d0 ($fltruncate ($fl/ n m))))
	   (if ($fl>= n ($fl* d0 m))
	       d0
	     (if ($fl>= m 0.0)
		 ($fl- d0 1.0)
	       ($fl+ d0 1.0)))))))

(define ($fldiv-and-mod n m)
  (cond (($flnan? n)
	 (values n n))
	(($flnan? m)
	 (values m m))
	(else
	 (let ((d0 ($fltruncate ($fl/ n m))))
	   (let ((m0 ($fl- n ($fl* d0 m))))
	     (if ($fl>= m0 0.0)
		 (values d0 m0)
	       (if ($fl>= m 0.0)
		   (values ($fl- d0 1.0) ($fl+ m0 m))
		 (values ($fl+ d0 1.0) ($fl- m0 m)))))))))

(define ($fldiv0-and-mod0 n m)
  (cond (($flnan? n)
	 (values n n))
	(($flnan? m)
	 (values m m))
	(else
	 (let ((d0 ($fltruncate ($fl/ n m))))
	   (let ((m0 ($fl- n ($fl* d0 m))))
	     (if ($fl>= m 0.0)
		 (if ($fl< m0 ($fl/ m 2.0))
		     (if ($fl>= m0 ($fl/ m -2.0))
			 (values d0 m0)
		       (values ($fl- d0 1.0) ($fl+ m0 m)))
		   (values ($fl+ d0 1.0) ($fl- m0 m)))
	       (if ($fl< m0 ($fl/ m -2.0))
		   (if ($fl>= m0 ($fl/ m 2.0))
		       (values d0 m0)
		     (values ($fl+ d0 1.0) ($fl- m0 m)))
		 (values ($fl- d0 1.0) ($fl+ m0 m)))))))))

(define ($fldiv0 n m)
  (cond (($flnan? n)
	 n)
	(($flnan? m)
	 m)
	(else
	 (let ((d0 ($fltruncate ($fl/ n m))))
	   (let ((m0 ($fl- n ($fl* d0 m))))
	     (if ($fl>= m 0.0)
		 (if ($fl< m0 ($fl/ m 2.0))
		     (if ($fl>= m0 ($fl/ m -2.0))
			 d0
		       ($fl- d0 1.0))
		   ($fl+ d0 1.0))
	       (if ($fl< m0 ($fl/ m -2.0))
		   (if ($fl>= m0 ($fl/ m 2.0))
		       d0
		     ($fl+ d0 1.0))
		 ($fl- d0 1.0))))))))

(define ($flmod0 n m)
  (cond (($flnan? n)
	 n)
	(($flnan? m)
	 m)
	(else
	 (let ((d0 ($fltruncate ($fl/ n m))))
	   (let ((m0 ($fl- n ($fl* d0 m))))
	     (if ($fl>= m 0.0)
		 (if ($fl< m0 ($fl/ m 2.0))
		     (if ($fl>= m0 ($fl/ m -2.0))
			 m0
		       ($fl+ m0 m))
		   ($fl- m0 m))
	       (if ($fl< m0 ($fl/ m -2.0))
		   (if ($fl>= m0 ($fl/ m 2.0))
		       m0
		     ($fl- m0 m))
		 ($fl+ m0 m))))))))


;;;; done

;; (define end-of-file-dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.numerics.flonums.div-and-mod end")))

#| end of library |# )

;;; end of file
