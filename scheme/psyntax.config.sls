;;;Copyright (c) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

(library (psyntax.config)
  (export
    initialise-expander
    expander-initialisation/initialise-label-gensyms-and-interned-libraries
    expander-initialisation/initialise-core-prims-tagging)
  (import (rnrs)
    (psyntax.compat))

  (define expander-initialisation/initialise-label-gensyms-and-interned-libraries
    (make-parameter #f))

  (define expander-initialisation/initialise-core-prims-tagging
    (make-parameter #f))

  (define initialise-expander
    (let ((expander-initialised? #f))
      (lambda ()
	(unless expander-initialised?
	  (print-expander-debug-message "initialising expander internals")
	  (let ((func (expander-initialisation/initialise-label-gensyms-and-interned-libraries)))
	    (when func (func)))
	  (let ((func (expander-initialisation/initialise-core-prims-tagging)))
	    (when func (func)))
	  (set! expander-initialised? #t)))))

  #| end of library |# )

;;; end of file
