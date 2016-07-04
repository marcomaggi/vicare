;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: definition of syntax COND-BOOT-EXPANSION
;;;Date: Mon Jul  4, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(define-auxiliary-syntaxes
  inclusion-in-normal-boot-image
  inclusion-in-rotation-boot-image
  bootstrapping-for-normal-boot-image
  bootstrapping-for-rotation-boot-image)

(define-syntax (cond-boot-expansion stx)

  ;;This is true when the libraries are expanded  to be included in a new boot image,
  ;;either normal or  rotation.  It is false  when the libraries are  expanded by the
  ;;build script "makefile.sps".
  ;;
  (define expanding-for-inclusion-in-boot-image?
    (equal? "yes" (getenv "BUILDING_FOR_INCLUSION_IN_BOOT_IMAGE")))

  ;;This is true when the libraries are expanded to build a rotation boot image, both
  ;;to be used by the building infrastructure and to be included in the boot image.
  ;;
  (define expanding-to-build-new-rotation-boot-image?
    (equal? "yes" (getenv "BUILDING_ROTATION_BOOT_IMAGE")))

  (define (log description.stx)
    (fprintf (current-error-port)
	     "conditional expansion for ~a: ~a\n"
	     (syntax->datum description.stx)
	     (cond ((and expanding-for-inclusion-in-boot-image?
			 (not expanding-to-build-new-rotation-boot-image?))
		    "inclusion in normal boot image")
		   ((and expanding-for-inclusion-in-boot-image?
			 expanding-to-build-new-rotation-boot-image?)
		    "inclusion in rotation boot image")
		   ((and (not expanding-for-inclusion-in-boot-image?)
			 (not expanding-to-build-new-rotation-boot-image?))
		    "bootstrapping normal boot image")
		   ((and (not expanding-for-inclusion-in-boot-image?)
			 expanding-to-build-new-rotation-boot-image?)
		    "bootstrapping rotation boot image")
		   (else
		    (syntax-violation 'cond-boot-expansion
		      "invalid values in platform's environment variables set by the building infrastructure"
		      stx #f)))))

  (syntax-case stx (inclusion-in-normal-boot-image
		    inclusion-in-rotation-boot-image
		    bootstrapping-for-normal-boot-image
		    bootstrapping-for-rotation-boot-image)
    ((_ ?description
	((inclusion-in-normal-boot-image)		. ?inclusion-in-normal-body)
	((inclusion-in-rotation-boot-image)		. ?inclusion-in-rotation-body)
	((bootstrapping-for-normal-boot-image)		. ?bootstrapping-for-normal-body)
	((bootstrapping-for-rotation-boot-image)	. ?bootstrapping-for-rotation-body))
     (begin
       (log #'?description)
       (cond ((and expanding-for-inclusion-in-boot-image?
		   (not expanding-to-build-new-rotation-boot-image?))
	      #'(begin . ?inclusion-in-normal-body))
	     ((and expanding-for-inclusion-in-boot-image?
		   expanding-to-build-new-rotation-boot-image?)
	      #'(begin . ?inclusion-in-rotation-body))
	     ((and (not expanding-for-inclusion-in-boot-image?)
		   (not expanding-to-build-new-rotation-boot-image?))
	      #'(begin . ?bootstrapping-for-normal-body))
	     ((and (not expanding-for-inclusion-in-boot-image?)
		   expanding-to-build-new-rotation-boot-image?)
	      #'(begin . ?bootstrapping-for-rotation-body))
	     (else
	      (syntax-violation 'cond-boot-expansion
		"invalid values in platform's environment variables set by the building infrastructure"
		stx #f)))))
    ))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
