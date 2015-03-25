;;;
;;;Part of: Vicare Scheme
;;;Contents: semantic action bindings for SILex
;;;Date: Mon Jan 17, 2011
;;;
;;;Abstract
;;;
;;;	This library  exports bindings used  in the semantic  actions of
;;;	the lexer tables of SILex itself.
;;;
;;;Copyright (c) 2001 Danny Dube' <dube@iro.umontreal.ca>
;;;
;;;Original code  by Danny Dube'.   Port to R6RS Scheme  and integration
;;;into Vicare by Marco Maggi.
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare parser-tools silex semantic)
  (export
    ;; token record
    :tok		:tok-make
    tok?		make-tok
    get-tok-type	get-tok-line
    get-tok-column	get-tok-lexeme
    get-tok-attr	get-tok-2nd-attr

    ;; fonctions auxilliaires du lexer
    parse-escaped-char
    parse-spec-char		parse-digits-char
    parse-hex-digits-char	parse-quoted-char
    parse-ordinary-char
    parse-id			parse-id-ref
    parse-power-m		parse-power-m-inf
    parse-power-m-n		parse-percent-include
    parse-inline-hex-escape

    ;; constants
    eof-tok			hblank-tok
    vblank-tok			pipe-tok
    question-tok		plus-tok
    star-tok			lpar-tok
    rpar-tok			dot-tok
    lbrack-tok			lbrack-rbrack-tok
    lbrack-caret-tok		lbrack-minus-tok
    subst-tok			power-tok
    doublequote-tok		char-tok
    caret-tok			dollar-tok
    <<EOF>>-tok			<<ERROR>>-tok
    percent-percent-tok		id-tok
    rbrack-tok			minus-tok
    illegal-tok			class-tok
    percent-include-tok		string-tok
    open-comment-tok		close-comment-tok
    number-of-tokens

    newline-ch			tab-ch
    dollar-ch			minus-ch
    rbrack-ch			caret-ch
    dot-class			default-action
    default-<<EOF>>-action	default-<<ERROR>>-action
    )
  (import (vicare))


;;; Fonctions de manipulation des tokens

(define-record-type (:tok :tok-make tok?)
  (nongenerative vicare:parser-tools:silex::tok)
  (fields (immutable type		get-tok-type)
	  (immutable line		get-tok-line)
	  (immutable column		get-tok-column)
	  (immutable lexeme		get-tok-lexeme)
	  (immutable attr		get-tok-attr)
	  (immutable second-attr	get-tok-2nd-attr)))

(define (make-tok tok-type lexeme line column . attr)
  (cond ((null? attr)
	 (:tok-make tok-type line column lexeme #f         #f))
	((null? (cdr attr))
	 (:tok-make tok-type line column lexeme (car attr) #f))
	(else
	 (:tok-make tok-type line column lexeme (car attr) (cadr attr)))))


;;;; module util.scm
;;
;;Quelques definitions de constantes
;;

(define-constant eof-tok              0)
(define-constant hblank-tok           1)
(define-constant vblank-tok           2)
(define-constant pipe-tok             3)
(define-constant question-tok         4)
(define-constant plus-tok             5)
(define-constant star-tok             6)
(define-constant lpar-tok             7)
(define-constant rpar-tok             8)
(define-constant dot-tok              9)
(define-constant lbrack-tok          10)
(define-constant lbrack-rbrack-tok   11)
(define-constant lbrack-caret-tok    12)
(define-constant lbrack-minus-tok    13)
(define-constant subst-tok           14)
(define-constant power-tok           15)
(define-constant doublequote-tok     16)
(define-constant char-tok            17)
(define-constant caret-tok           18)
(define-constant dollar-tok          19)
(define-constant <<EOF>>-tok         20)
(define-constant <<ERROR>>-tok       21)
(define-constant percent-percent-tok 22)
(define-constant id-tok              23)
(define-constant rbrack-tok          24)
(define-constant minus-tok           25)
(define-constant illegal-tok         26)
; Tokens agreges
(define-constant class-tok           27)
(define-constant string-tok          28)

(define-constant percent-include-tok 29)
(define-constant open-comment-tok    30)
(define-constant close-comment-tok   31)
(define-constant number-of-tokens 32)

(define-constant newline-ch   (char->integer #\newline))
(define-constant tab-ch       (char->integer #\	))
(define-constant dollar-ch    (char->integer #\$))
(define-constant minus-ch     (char->integer #\-))
(define-constant rbrack-ch    (char->integer #\]))
(define-constant caret-ch     (char->integer #\^))

(define-constant dot-class
  (list (cons 'inf- (- newline-ch 1))
	(cons (+ newline-ch 1) 'inf+)))

(define-constant default-action
  "        (yycontinue)\n")

(define-constant default-<<EOF>>-action
  "       (eof-object)\n")

(define-constant default-<<ERROR>>-action
  "       (assertion-violation #f \"invalid token\")\n")


;;;; module lexparser.scm
;;
;;Fonctions auxilliaires du lexer.
;;

(define (parse-spec-char lexeme line column)
  ;;This is used to parse  lexemes holding the newline character.  It is
  ;;special  because it  signals the  end of  the regexps  in  the macro
  ;;header of table files.
  ;;
  (make-tok char-tok lexeme line column newline-ch))

(define (parse-digits-char lexeme line column)
  ;;Parse lexemes holding character specifications as backslash-integer.
  ;;
  (make-tok char-tok lexeme line column
	    (string->number (substring lexeme 1 (string-length lexeme)))))

(define (parse-hex-digits-char lexeme line column)
  ;;Parse  lexemes  holding   character  specifications  as  hexadecimal
  ;;numbers prefixed by "#x" or "#X".
  ;;
  (make-tok char-tok lexeme line column (string->number lexeme)))

(define (parse-quoted-char lexeme line column)
  ;;This is the  original function to parse "escaped"  characters in the
  ;;string; escaped characters are the ones quoted with a backslash.
  ;;
  (make-tok char-tok lexeme line column (char->integer (string-ref lexeme 1))))

(define (parse-escaped-char lexeme line column ch)
  ;;Parse escaped  characters in  R6RS strings.  CH  must be  the Scheme
  ;;character itself.
  ;;
  (make-tok char-tok lexeme line column (char->integer ch)))

(define (parse-inline-hex-escape lexeme line column)
  ;;Parse  an  "inline  hex  escape" character  specification  inside  a
  ;;string.  These are the "\\xH;" ones,  where "H" is a sequence of hex
  ;;digits.
  ;;
  (let* ((len (string-length lexeme))
	 (num (string->number (substring lexeme 2 (- len 1)) 16)))
    (if (or (<= 0 num #xD7FF) (<= #xE000 num #x10FFFF))
	(make-tok char-tok lexeme line column (integer->char num))
      (assertion-violation #f
	"invalid inline hex escape while parsing string in SILex table"
	lexeme))))

(define (parse-ordinary-char lexeme line column)
  (make-tok char-tok lexeme line column (char->integer (string-ref lexeme 0))))

(define (parse-id lexeme line column)
  ;;Parse the identifier in a macro definition.
  ;;
  ;;(Marco Maggi; Tue  Jan 18, 2011) The one below  is the original code
  ;;from SILex 1.0.  Notice that  the string is converted to downcase in
  ;;the attribute of  the token: the attribute is  the unique value used
  ;;to  compare macro  names.  Converting  it to  downcase  means having
  ;;case-insensitive  macro names;  we have  removed this  conversion to
  ;;have  case-sensitive  macro  names,  which is  consistent  with  the
  ;;convention of R6RS symbols.
  ;;
  ;;	(make-tok id-tok lexeme line column (string-downcase lexeme) lexeme)
  ;;
  (make-tok id-tok lexeme line column lexeme lexeme))

(define (parse-id-ref lexeme line column)
  ;;Parse a macro reference.  LEXEME must be a string with the format:
  ;;
  ;;	{IDENTIFIER}
  ;;
  ;;and we extract IDENTIFIER to build a subst token.
  ;;
  ;;(Marco Maggi;  Tue Jan  18, 2011) The  original code had  a downcase
  ;;ORIG-NAME in the attribute slot of the token record; it was meant to
  ;;match   the  old   code  in   PARSE-ID,  making   macro  identifiers
  ;;case-insensitive.  The  code has been  changed to make it  match the
  ;;new code in PARSE-ID.
  ;;
  (let ((orig-name (substring lexeme 1 (- (string-length lexeme) 1))))
    (make-tok subst-tok lexeme line column orig-name orig-name)))

(define (parse-power-m lexeme line column)
  (let* ((len    (string-length lexeme))
	 (substr (substring lexeme 1 (- len 1)))
	 (m      (string->number substr))
	 (range  (cons m m)))
    (make-tok power-tok lexeme line column range)))

(define (parse-power-m-inf lexeme line column)
  (let* ((len (string-length lexeme))
	 (substr (substring lexeme 1 (- len 2)))
	 (m (string->number substr))
	 (range (cons m 'inf)))
    (make-tok power-tok lexeme line column range)))

(define (parse-power-m-n lexeme line column)
  (let ((len (string-length lexeme)))
    (let loop ((comma 2))
      (if (char=? (string-ref lexeme comma) #\,)
	  (let* ((sub1  (substring lexeme 1 comma))
		 (sub2  (substring lexeme (+ comma 1) (- len 1)))
		 (m     (string->number sub1))
		 (n     (string->number sub2))
		 (range (cons m n)))
	    (make-tok power-tok lexeme line column range))
	(loop (+ comma 1))))))

(define (parse-percent-include lexeme line column)
  ;;Parse a file  inclusion directive from a table  file; return a token
  ;;record.  We expect LEXEME to be a string of the format:
  ;;
  ;;	%[<PATHNAME>]
  ;;
  ;;with  <PATHNAME>  being  a  non-empty  string  representing  a  file
  ;;pathname.  We  extract the  pathname and store  it as string  in the
  ;;attribute of the returned token record.
  ;;
  (make-tok percent-include-tok lexeme line column
  	    (substring lexeme 2 (- (string-length lexeme) 1))))


;;;; done

)

;;; end of file
