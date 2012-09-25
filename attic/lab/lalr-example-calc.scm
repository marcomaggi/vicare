;;;
;;;; Simple calculator in Scheme
;;;
;;
;; @created   "Tue Jan  6 12:47:23 2004"
;; @modified  "Mon Oct 25 11:07:24 2004"
;; @author    "Dominique Boucher"
;; @copyright "Dominique Boucher"
;;
;; Simple arithmetic calculator. 
;; 
;;   This program illustrates the use of the lalr-scm parser generator
;; for Scheme. It is NOT robust, since calling a function with 
;; the wrong number of arguments may generate an error that will
;; cause the calculator to crash.


;;;
;;;;   The LALR(1) parser
;;;


(import (rnrs) (rnrs mutable-pairs) (lalr))

(define calc-parser
  (lalr-parser
   
   ;; --- Options 
   ;; output a parser, called calc-parser, in a separate file - calc.yy.scm, 
   ;(output:    calc-parser "calc.yy.scm")
   ;; output the LALR table to calc.out
   ;(out-table: "calc.out")
   ;; there should be no conflict
   (expect:    0)
   
   ;; --- token definitions
   (ID NUM = LPAREN RPAREN NEWLINE COMMA
    (left: + -)
    (left: * /)
    (nonassoc: uminus))
   
   (lines    (lines line) : (display-result $2)
	     (line)       : (display-result $1))
	   

   ;; --- rules
   (line     (assign NEWLINE)        : $1
	     (expr   NEWLINE)        : $1
	     (error  NEWLINE)        : #f)
   
   (assign   (ID = expr)             : (add-binding $1 $3))
   
   (expr     (expr + expr)           : (+ $1 $3)
	     (expr - expr)           : (- $1 $3)
	     (expr * expr)           : (* $1 $3)
	     (expr / expr)           : (/ $1 $3)
	     (- expr (prec: uminus)) : (- $2)
	     (ID)                    : (get-binding $1)
	     (ID LPAREN args RPAREN) : (invoke-proc $1 $3)
	     (NUM)                   : $1
	     (LPAREN expr RPAREN)    : $2)
    
   (args     ()                      : '()
	     (expr arg-rest)         : (cons $1 $2))
   
   (arg-rest (COMMA expr arg-rest)   : (cons $2 $3)
	     ()                      : '())))


(define (display-result v)
  (if v
      (begin
	(display "==> ")
	(display v)
	(newline))))


;;;
;;;;   The lexer
;;;


(define (make-lexer errorp)
  (lambda ()
    (letrec ((skip-spaces
	      (lambda ()
		(let loop ((c (peek-char)))
		  (if (and (not (eof-object? c))
			   (or (char=? c #\space) (char=? c #\tab)))
		      (begin
			(read-char)
			(loop (peek-char)))))))   
	     (read-number 
	      (lambda (l)
		(let ((c (peek-char)))
		  (if (char-numeric? c)
		      (read-number (cons (read-char) l))
		      (string->number (apply string (reverse l)))))))
	     (read-id
	      (lambda (l)
		(let ((c (peek-char)))
		  (if (char-alphabetic? c)
		      (read-id (cons (read-char) l))
		      (string->symbol (apply string (reverse l))))))))

      ;; -- skip spaces
      (skip-spaces)
      ;; -- read the next token
      (let loop ((c (read-char)))
	(cond
	 ((eof-object? c)      '*eoi*)
	 ((char=? c #\newline) 'NEWLINE)
	 ((char=? c #\+)       '+)
	 ((char=? c #\-)       '-)
	 ((char=? c #\*)       '*)
	 ((char=? c #\/)       '/)
	 ((char=? c #\=)       '=)
	 ((char=? c #\,)       'COMMA)
	 ((char=? c #\()       'LPAREN)
	 ((char=? c #\))       'RPAREN)
	 ((char-numeric? c)    (cons 'NUM (read-number (list c))))
	 ((char-alphabetic? c) (cons 'ID  (read-id (list c))))
	 (else                 
	  (errorp "PARSE ERROR : illegal character: " c)
	  (skip-spaces)
	  (loop (read-char))))))))


(define (read-line)
  (let loop ((c (read-char)))
    (if (and (not (eof-object? c))
	     (not (char=? c #\newline)))
	(loop (read-char)))))


;;;
;;;;   Environment management
;;;


(define *env* (list (cons '$$ 0)))


(define (init-bindings)
  (set-cdr! *env* '())
  (add-binding 'cos cos)
  (add-binding 'sin sin)
  (add-binding 'tan tan)
  (add-binding 'expt expt)
  (add-binding 'sqrt sqrt))


(define (add-binding var val)
  (set! *env* (cons (cons var val) *env*))
  val)


(define (get-binding var)
  (let ((p (assq var *env*)))
    (if p 
	(cdr p)
	0)))


(define (invoke-proc proc-name args)
  (let ((proc (get-binding proc-name)))
    (if (procedure? proc)
	(apply proc args)
	(begin
	  (display "ERROR: invalid procedure:")
	  (display proc-name)
	  (newline)
	  0))))


;;;
;;;;   The main program
;;;


(define calc
  (lambda ()
    (call-with-current-continuation
     (lambda (k)
       (display "********************************") (newline)
       (display "*  Mini calculator in Scheme   *") (newline)
       (display "*                              *") (newline)
       (display "* Enter expressions followed   *") (newline)
       (display "* by [RETURN] or 'quit()' to   *") (newline)
       (display "* exit.                        *") (newline)
       (display "********************************") (newline)
       (init-bindings)
       (add-binding 'quit k)
       (letrec ((errorp
		 (lambda args
		   (for-each display args) (newline)))
		(start
		 (lambda ()
		   (calc-parser (make-lexer errorp) errorp))))
	 (start))))))

(calc)

