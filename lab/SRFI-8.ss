(library (SRFI-8)
  (export receive)
  (import (ikarus))
  ;;; these SRFIs are great for filling your "features"
  ;;; page
  (define-syntax receive
    (syntax-rules ()
      ((receive formals expression body body* ...)
       (call-with-values (lambda () expression)
         (lambda formals body body* ...))))))
