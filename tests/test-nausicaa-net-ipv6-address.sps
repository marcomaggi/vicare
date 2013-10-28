;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for IPv6 address object
;;;Date: Wed Jun  9, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (nausicaa net addresses ipv6)
  (nausicaa parser-tools ipv6-addresses)
  (prefix (vicare parser-tools silex lexer) lex.)
  (prefix (nausicaa parser-tools lexical-tokens) lt.)
  (prefix (nausicaa parser-tools source-locations) sl.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: net IPv6 address\n")


;;;; helpers

(define-syntax-rule (values->list ?expr)
  (call-with-values
      (lambda () ?expr)
    (lambda args args)))


(parametrise ((check-test-name	'lexing-low-level))

  (define (tokenise-address string)
    (let* ((IS		(lex.make-IS (lex.string: string) (lex.counters: 'all)))
	   (lexer	(lex.make-lexer ipv6-address-lexer-table IS))
	   (out		'()))
      (define (push-token! (T lt.<lexical-token>))
	(set-cons! out (cons (T category) (T value))))
      (do (((token lt.<lexical-token>) (lexer) (lexer)))
	  ((token special?)
	   (push-token! token)
	   (reverse out))
;;;(write token)(newline)
	(push-token! token))
      ))

  (define-constant eoi `(*eoi* . ,(eof-object)))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "1:2:3:4:5:6:7:8")
    => `((NUMBER . "1")
	 (COLON  . #\:)
	 (NUMBER . "2")
	 (COLON  . #\:)
	 (NUMBER . "3")
	 (COLON  . #\:)
	 (NUMBER . "4")
	 (COLON  . #\:)
	 (NUMBER . "5")
	 (COLON  . #\:)
	 (NUMBER . "6")
	 (COLON  . #\:)
	 (NUMBER . "7")
	 (COLON  . #\:)
	 (NUMBER . "8")
	 ,eoi))

  (check
      (tokenise-address "F:Zciao")
    => '((NUMBER . "F")
	 (COLON  . #\:)
	 (*lexer-error* . "Zciao")))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "1")
    => `((NUMBER . "1")
	 ,eoi))

  (check
      (tokenise-address "10")
    => `((NUMBER . "10")
	 ,eoi))

  (check
      (tokenise-address "100")
    => `((NUMBER . "100")
	 ,eoi))

  (check
      (tokenise-address "190")
    => `((NUMBER . "190")
	 ,eoi))

  (check
      (tokenise-address "210")
    => `((NUMBER . "210")
	 ,eoi))

  (check
      (tokenise-address "250")
    => `((NUMBER . "250")
	 ,eoi))

  (check
      (tokenise-address "255")
    => `((NUMBER . "255")
	 ,eoi))

  (check
      (tokenise-address "256")
    => `((NUMBER . "256")
	 ,eoi))

  (check
      (tokenise-address "567")
    => `((NUMBER . "567")
	 ,eoi))

  (check
      (tokenise-address "1:2:3:4:5.6.7.8")
    => `((NUMBER . "1")
	 (COLON  . #\:)
	 (NUMBER . "2")
	 (COLON  . #\:)
	 (NUMBER . "3")
	 (COLON  . #\:)
	 (NUMBER . "4")
	 (COLON  . #\:)
	 (NUMBER . "5")
	 (DOT    . #\.)
	 (NUMBER . "6")
	 (DOT    . #\.)
	 (NUMBER . "7")
	 (DOT    . #\.)
	 (NUMBER . "8")
	 ,eoi))

  (check
      (tokenise-address "a:b:c:d:5.6.7.8")
    => `((NUMBER . "a")
	 (COLON  . #\:)
	 (NUMBER . "b")
	 (COLON  . #\:)
	 (NUMBER . "c")
	 (COLON  . #\:)
	 (NUMBER . "d")
	 (COLON  . #\:)
	 (NUMBER . "5")
	 (DOT    . #\.)
	 (NUMBER . "6")
	 (DOT    . #\.)
	 (NUMBER . "7")
	 (DOT    . #\.)
	 (NUMBER . "8")
	 ,eoi))

  #t)


(parametrise ((check-test-name	'lexing-high-level))

  (define (tokenise-address string)
    (let* ((lexer	(make-ipv6-address-lexer (string: string)))
	   (out		'()))
      (define (push-token! (T lt.<lexical-token>))
	(set-cons! out (cons (T category) (T value))))
      (do (((token lt.<lexical-token>) (lexer) (lexer)))
	  ((token special?)
	   (push-token! token)
	   (reverse out))
	#;(debug-print token)
	(push-token! token))
      ))

  (define-constant eoi `(*eoi* . ,(eof-object)))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "1:2:3:4:5:6:7:8")
    => `((NUMBER . "1")
	 (COLON  . #\:)
	 (NUMBER . "2")
	 (COLON  . #\:)
	 (NUMBER . "3")
	 (COLON  . #\:)
	 (NUMBER . "4")
	 (COLON  . #\:)
	 (NUMBER . "5")
	 (COLON  . #\:)
	 (NUMBER . "6")
	 (COLON  . #\:)
	 (NUMBER . "7")
	 (COLON  . #\:)
	 (NUMBER . "8")
	 ,eoi))

  (check
      (tokenise-address "F:Zciao")
    => '((NUMBER . "F")
	 (COLON  . #\:)
	 (*lexer-error* . "Zciao")))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "1")
    => `((NUMBER . "1")
	 ,eoi))

  (check
      (tokenise-address "10")
    => `((NUMBER . "10")
	 ,eoi))

  (check
      (tokenise-address "100")
    => `((NUMBER . "100")
	 ,eoi))

  (check
      (tokenise-address "190")
    => `((NUMBER . "190")
	 ,eoi))

  (check
      (tokenise-address "210")
    => `((NUMBER . "210")
	 ,eoi))

  (check
      (tokenise-address "250")
    => `((NUMBER . "250")
	 ,eoi))

  (check
      (tokenise-address "255")
    => `((NUMBER . "255")
	 ,eoi))

  (check
      (tokenise-address "256")
    => `((NUMBER . "256")
	 ,eoi))

  (check
      (tokenise-address "567")
    => `((NUMBER . "567")
	 ,eoi))

  (check
      (tokenise-address "1:2:3:4:5.6.7.8")
    => `((NUMBER . "1")
	 (COLON  . #\:)
	 (NUMBER . "2")
	 (COLON  . #\:)
	 (NUMBER . "3")
	 (COLON  . #\:)
	 (NUMBER . "4")
	 (COLON  . #\:)
	 (NUMBER . "5")
	 (DOT    . #\.)
	 (NUMBER . "6")
	 (DOT    . #\.)
	 (NUMBER . "7")
	 (DOT    . #\.)
	 (NUMBER . "8")
	 ,eoi))

  (check
      (tokenise-address "a:b:c:d:5.6.7.8")
    => `((NUMBER . "a")
	 (COLON  . #\:)
	 (NUMBER . "b")
	 (COLON  . #\:)
	 (NUMBER . "c")
	 (COLON  . #\:)
	 (NUMBER . "d")
	 (COLON  . #\:)
	 (NUMBER . "5")
	 (DOT    . #\.)
	 (NUMBER . "6")
	 (DOT    . #\.)
	 (NUMBER . "7")
	 (DOT    . #\.)
	 (NUMBER . "8")
	 ,eoi))

  #t)


(parametrise ((check-test-name	'parsing-low-level))

  (define (parse-address string)
    (let* ((IS		(lex.make-IS (lex.string: string) (lex.counters: 'all)))
	   (lexer	(lex.make-lexer ipv6-address-lexer-table IS))
	   (parser	(make-ipv6-address-parser)))
      (parser lexer (make-ipv6-address-parser-error-handler 'parse-address string))))

;;; --------------------------------------------------------------------
;;; plain addresses

  (check
      (parse-address "1:2:3:4:5:6:7:8")
    => '(1 2 3 4 5 6 7 8))

;;; --------------------------------------------------------------------
;;; compressed format (omitting zeros)

  (check ;this is a wrong address spec, it must be ruled out with further validation
      (parse-address "ff")
    => '(255))

  (check ;this is a wrong address spec, it must be ruled out with further validation
      (parse-address "ff:ff")
    => '(255 255))

  (check
      (parse-address "::")
    => '(#f))

  (check
      (parse-address "::1")
    => '(#f 1))

  (check
      (parse-address "1::")
    => '(1 #f))

  (check
      (parse-address "1::2")
    => '(1 #f 2))

  (check
      (parse-address "1:2::3")
    => '(1 2 #f 3))

  (check
      (parse-address "1::2:3")
    => '(1 #f 2 3))

  (check
      (parse-address "1:2::3:4")
    => '(1 2 #f 3 4))

  (check
      (parse-address "1:2:3::4:5:6")
    => '(1 2 3 #f 4 5 6))

;;; --------------------------------------------------------------------
;;; IPv4 tail address

  (check
      (parse-address "::192.168.99.1")
    => '(#f #xC0A8 #x6301))

  (check
      (parse-address "1:2:3:4:172.30.67.254")
    => '(1 2 3 4 #xac1e #x43fe))

  (check
      (parse-address "1:2:3:4::172.30.67.254")
    => '(1 2 3 4 #f #xac1e #x43fe))

  (check
      (parse-address "::1:2:3:4:172.30.67.254")
    => '(#f 1 2 3 4 #xac1e #x43fe))

  (check
      (parse-address "1:2::3:4:172.30.67.254")
    => '(1 2 #f 3 4 #xac1e #x43fe))

  (check
      (parse-address "::ffff:192.168.99.1")
    => '(#f #xFFFF #xC0A8 #x6301))

;;; --------------------------------------------------------------------
;;; prefix, compressed format (omitting zeros)

  (check
      (parse-address "::/60")
    => '(#f (60)))

  (check
      (parse-address "::1/60")
    => '(#f 1 (60)))

  (check
      (parse-address "1::/60")
    => '(1 #f (60)))

  (check
      (parse-address "1::2/60")
    => '(1 #f 2 (60)))

  (check
      (parse-address "1:2::3/60")
    => '(1 2 #f 3 (60)))

  (check
      (parse-address "1::2:3/60")
    => '(1 #f 2 3 (60)))

  (check
      (parse-address "1:2::3:4/60")
    => '(1 2 #f 3 4 (60)))

  (check
      (parse-address "1:2:3::4:5:6/60")
    => '(1 2 3 #f 4 5 6 (60)))

;;; --------------------------------------------------------------------
;;; prefix, IPv4 tail address

  (check
      (parse-address "::192.168.99.1/60")
    => '(#f #xC0A8 #x6301 (60)))

  (check
      (parse-address "1:2:3:4:172.30.67.254/60")
    => '(1 2 3 4 #xac1e #x43fe (60)))

  (check
      (parse-address "1:2:3:4::172.30.67.254/60")
    => '(1 2 3 4 #f #xac1e #x43fe (60)))

  (check
      (parse-address "::1:2:3:4:172.30.67.254/60")
    => '(#f 1 2 3 4 #xac1e #x43fe (60)))

  (check
      (parse-address "1:2::3:4:172.30.67.254/60")
    => '(1 2 #f 3 4 #xac1e #x43fe (60)))

  (check
      (parse-address "::ffff:192.168.99.1/60")
    => '(#f #xFFFF #xC0A8 #x6301 (60)))

;;; --------------------------------------------------------------------
;;; errors

  (let-syntax ((check-it (syntax-rules ()
			   ((_ ?string)
			    (try
				(parse-address ?string)
			      (catch E
				(&ipv6-address-parser-error
				 #t)
				(else #f)))))))
    (check-it "1,")
    (check-it "1::2::3")
    (check-it "1::2::")
    (check-it "::2::3")
    (check-it "::::")
    (void))

  #t)


(parametrise ((check-test-name	'parsing-high-level/any))

;;; plain addresses

  (check
      (parse-ipv6-address "1:2:3:4:5:6:7:8")
    => '(1 2 3 4 5 6 7 8 (#f)))

;;; --------------------------------------------------------------------
;;; compressed format (omitting zeros)

  (check
      (parse-ipv6-address "::")
    => '(0 0 0 0 0 0 0 0 (#f)))

  (check
      (parse-ipv6-address "::1")
    => '(0 0 0 0 0 0 0 1 (#f)))

  (check
      (parse-ipv6-address "1::")
    => '(1 0 0 0 0 0 0 0 (#f)))

  (check
      (parse-ipv6-address "1::2")
    => '(1 0 0 0 0 0 0 2 (#f)))

  (check
      (parse-ipv6-address "1:2::3")
    => '(1 2 0 0 0 0 0 3 (#f)))

  (check
      (parse-ipv6-address "1::2:3")
    => '(1 0 0 0 0 0 2 3 (#f)))

  (check
      (parse-ipv6-address "1:2::3:4")
    => '(1 2 0 0 0 0 3 4 (#f)))

  (check
      (parse-ipv6-address "1:2:3::4:5:6")
    => '(1 2 3 0 0 4 5 6 (#f)))

;;; --------------------------------------------------------------------
;;; IPv4 tail address

  (check
      (parse-ipv6-address "::192.168.99.1")
    => '(0 0 0 0 0 0 #xC0A8 #x6301 (#f)))

  (check
      (parse-ipv6-address "1:2:3:4::172.30.67.254")
    => '(1 2 3 4 0 0 #xac1e #x43fe (#f)))

  (check
      (parse-ipv6-address "::1:2:3:4:172.30.67.254")
    => '(0 0 1 2 3 4 #xac1e #x43fe (#f)))

  (check
      (parse-ipv6-address "1:2::3:4:172.30.67.254")
    => '(1 2 0 0 3 4 #xac1e #x43fe (#f)))

  (check
      (parse-ipv6-address "::ffff:192.168.99.1")
    => '(0 0 0 0 0 #xFFFF #xC0A8 #x6301 (#f)))

;;; --------------------------------------------------------------------
;;; prefix, compressed format (omitting zeros)

  (check
      (parse-ipv6-address "::/60")
    => '(0 0 0 0 0 0 0 0 (60)))

  (check
      (parse-ipv6-address "::1/60")
    => '(0 0 0 0 0 0 0 1 (60)))

  (check
      (parse-ipv6-address "1::/60")
    => '(1 0 0 0 0 0 0 0 (60)))

  (check
      (parse-ipv6-address "1::2/60")
    => '(1 0 0 0 0 0 0 2 (60)))

  (check
      (parse-ipv6-address "1:2::3/60")
    => '(1 2 0 0 0 0 0 3 (60)))

  (check
      (parse-ipv6-address "1::2:3/60")
    => '(1 0 0 0 0 0 2 3 (60)))

  (check
      (parse-ipv6-address "1:2::3:4/60")
    => '(1 2 0 0 0 0 3 4 (60)))

  (check
      (parse-ipv6-address "1:2:3::4:5:6/60")
    => '(1 2 3 0 0 4 5 6 (60)))

;;; --------------------------------------------------------------------
;;; prefix, IPv4 tail address

  (check
      (parse-ipv6-address "::192.168.99.1/60")
    => '(0 0 0 0 0 0 #xC0A8 #x6301 (60)))

  (check
      (parse-ipv6-address "1:2:3:4::172.30.67.254/60")
    => '(1 2 3 4 0 0 #xac1e #x43fe (60)))

  (check
      (parse-ipv6-address "::1:2:3:4:172.30.67.254/60")
    => '(0 0 1 2 3 4 #xac1e #x43fe (60)))

  (check
      (parse-ipv6-address "1:2::3:4:172.30.67.254/60")
    => '(1 2 0 0 3 4 #xac1e #x43fe (60)))

  (check
      (parse-ipv6-address "::ffff:192.168.99.1/60")
    => '(0 0 0 0 0 #xFFFF #xC0A8 #x6301 (60)))

;;; --------------------------------------------------------------------
;;; errors

  (let-syntax ((check-it (syntax-rules ()
			   ((_ ?string)
			    (check
				(try
				    (parse-ipv6-address ?string)
				  (catch E
				    (&ipv6-address-parser-error
				     #t)
				    (else #f)))
			      => #t)))))
    (check-it "1,")
    (check-it "1::2::3")
    (check-it "1::2::")
    (check-it "::2::3")
    (check-it "ff")
    (check-it "ff:ff")

    ;;it represents 6 components, 16-bit each
    (check-it "1:2:3:4:172.30.67.254")
    (check-it "1:2:3:4:172.30.67.254/60")
    #f)

  #t)


(parametrise ((check-test-name	'parsing-high-level/address))

;;; plain addresses

  (check
      (parse-ipv6-address-only "1:2:3:4:5:6:7:8")
    => '(1 2 3 4 5 6 7 8))

;;; --------------------------------------------------------------------
;;; compressed format (omitting zeros)

  (check
      (parse-ipv6-address-only "::")
    => '(0 0 0 0 0 0 0 0))

  (check
      (parse-ipv6-address-only "::1")
    => '(0 0 0 0 0 0 0 1))

  (check
      (parse-ipv6-address-only "1::")
    => '(1 0 0 0 0 0 0 0))

  (check
      (parse-ipv6-address-only "1::2")
    => '(1 0 0 0 0 0 0 2))

  (check
      (parse-ipv6-address-only "1:2::3")
    => '(1 2 0 0 0 0 0 3))

  (check
      (parse-ipv6-address-only "1::2:3")
    => '(1 0 0 0 0 0 2 3))

  (check
      (parse-ipv6-address-only "1:2::3:4")
    => '(1 2 0 0 0 0 3 4))

  (check
      (parse-ipv6-address-only "1:2:3::4:5:6")
    => '(1 2 3 0 0 4 5 6))

;;; --------------------------------------------------------------------
;;; IPv4 tail address

  (check
      (parse-ipv6-address-only "::192.168.99.1")
    => '(0 0 0 0 0 0 #xC0A8 #x6301))

  (check
      (parse-ipv6-address-only "1:2:3:4::172.30.67.254")
    => '(1 2 3 4 0 0 #xac1e #x43fe))

  (check
      (parse-ipv6-address-only "::1:2:3:4:172.30.67.254")
    => '(0 0 1 2 3 4 #xac1e #x43fe))

  (check
      (parse-ipv6-address-only "1:2::3:4:172.30.67.254")
    => '(1 2 0 0 3 4 #xac1e #x43fe))

  (check
      (parse-ipv6-address-only "::ffff:192.168.99.1")
    => '(0 0 0 0 0 #xFFFF #xC0A8 #x6301))

;;; --------------------------------------------------------------------
;;; errors

  (let-syntax ((check-it (syntax-rules ()
			   ((_ ?string)
			    (check
				(try
				    (parse-ipv6-address-only ?string)
				  (catch E
				    (&ipv6-address-parser-error
				     #t)
				    (else #f)))
			      => #t)))))
    ;;prefix, compressed format (omitting zeros)
    (check-it "::/60")
    (check-it "::1/60")
    (check-it "1::/60")
    (check-it "1::2/60")
    (check-it "1:2::3/60")
    (check-it "1::2:3/60")
    (check-it "1:2::3:4/60")
    (check-it "1:2:3::4:5:6/60")
    ;;prefix, IPv4 tail address
    (check-it "::192.168.99.1/60")
    (check-it "1:2:3:4:172.30.67.254/60")
    (check-it "1:2:3:4::172.30.67.254/60")
    (check-it "::1:2:3:4:172.30.67.254/60")
    (check-it "1:2::3:4:172.30.67.254/60")
    (check-it "::ffff:192.168.99.1/60")
    ;;errors
    (check-it "1,")
    (check-it "1::2::3")
    (check-it "1::2::")
    (check-it "::2::3")
    (check-it "ff")
    (check-it "ff:ff")
    (check-it "1:2:3:4:172.30.67.254")
    #f)

  #t)


(parametrise ((check-test-name	'parsing-high-level/prefix))

;;; prefix, compressed format (omitting zeros)

  (check
      (values->list (parse-ipv6-address-prefix "::/60"))
    => '((0 0 0 0 0 0 0 0) 60))

  (check
      (values->list (parse-ipv6-address-prefix "::1/60"))
    => '((0 0 0 0 0 0 0 1) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1::/60"))
    => '((1 0 0 0 0 0 0 0) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1::2/60"))
    => '((1 0 0 0 0 0 0 2) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1:2::3/60"))
    => '((1 2 0 0 0 0 0 3) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1::2:3/60"))
    => '((1 0 0 0 0 0 2 3) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1:2::3:4/60"))
    => '((1 2 0 0 0 0 3 4) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1:2:3::4:5:6/60"))
    => '((1 2 3 0 0 4 5 6) 60))

;;; --------------------------------------------------------------------
;;; prefix, IPv4 tail address

  (check
      (values->list (parse-ipv6-address-prefix "::192.168.99.1/60"))
    => '((0 0 0 0 0 0 #xC0A8 #x6301) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1:2:3:4::172.30.67.254/60"))
    => '((1 2 3 4 0 0 #xac1e #x43fe) 60))

  (check
      (values->list (parse-ipv6-address-prefix "::1:2:3:4:172.30.67.254/60"))
    => '((0 0 1 2 3 4 #xac1e #x43fe) 60))

  (check
      (values->list (parse-ipv6-address-prefix "1:2::3:4:172.30.67.254/60"))
    => '((1 2 0 0 3 4 #xac1e #x43fe) 60))

  (check
      (values->list (parse-ipv6-address-prefix "::ffff:192.168.99.1/60"))
    => '((0 0 0 0 0 #xFFFF #xC0A8 #x6301) 60))

;;; --------------------------------------------------------------------
;;; errors

  (let-syntax ((check-it (syntax-rules ()
			   ((_ ?string)
			    (check
				(try
				    (parse-ipv6-address-prefix ?string)
				  (catch E
				    (&ipv6-address-parser-error
				     #t)
				    (else #f)))
			      => #t)))))
    (check-it "1,")
    (check-it "1::2::3")
    (check-it "1::2::")
    (check-it "::2::3")
    (check-it "1:2:3:4:5:6:7:8")
    (check-it "ff")
    (check-it "ff:ff")
    (check-it "::")
    (check-it "::1")
    (check-it "1::")
    (check-it "1::2")
    (check-it "1:2::3")
    (check-it "1::2:3")
    (check-it "1:2::3:4")
    (check-it "1:2:3::4:5:6")
    (check-it "::192.168.99.1")
    (check-it "1:2:3:4:172.30.67.254")
    (check-it "1:2:3:4::172.30.67.254")
    (check-it "::1:2:3:4:172.30.67.254")
    (check-it "1:2::3:4:172.30.67.254")
    (check-it "::ffff:192.168.99.1")
    (check-it "1:2:3:4:172.30.67.254/60")
    #f)

  #t)


(parametrise ((check-test-name	'utilities))

  (check
      (call-with-values
	  (lambda ()
	    (ipv6-address-parsed-list-split '(1 2 3 4 5)))
	vector)
    => '#((1 2 3 4 5) #f))

  (check
      (call-with-values
	  (lambda ()
	    (ipv6-address-parsed-list-split '(1 2 3 4 5 (60))))
	vector)
    => '#((1 2 3 4 5) 60))

  (check
      (call-with-values
	  (lambda ()
	    (ipv6-address-parsed-list-split '(1 (60))))
	vector)
    => '#((1) 60))

  (check
      (call-with-values
	  (lambda ()
	    (ipv6-address-parsed-list-split '(#f (#f))))
	vector)
    => '#((#f) #f))

  (check
      (call-with-values
	  (lambda ()
	    (ipv6-address-parsed-list-split '(1 (#f))))
	vector)
    => '#((1) #f))

;;; --------------------------------------------------------------------

  (check
      (ipv6-address-parsed-list-expand '(1))
    => #f)

  (check
      (ipv6-address-parsed-list-expand '(#f))
    => '(0 0 0 0 0 0 0 0))

  (check
      (ipv6-address-parsed-list-expand '(1 #f))
    => '(1 0 0 0 0 0 0 0))

  (check
      (ipv6-address-parsed-list-expand '(1 2 3 4 5 6 7 8))
    => '(1 2 3 4 5 6 7 8))

  (check
      (ipv6-address-parsed-list-expand '(#f 2 3 4 5 6 7 8))
    => '(0 2 3 4 5 6 7 8))

  (check
      (ipv6-address-parsed-list-expand '(1 2 3 #f 5 6 7 8))
    => '(1 2 3 0 5 6 7 8))

  (check
      (ipv6-address-parsed-list-expand '(1 2 3 4 5 6 7 #f))
    => '(1 2 3 4 5 6 7 0))

  (check
      (ipv6-address-parsed-list-expand '(1 2 #f 6 7 8))
    => '(1 2 0 0 0 6 7 8))

  (check
      (ipv6-address-parsed-list-expand '(#f 4 5 6 7 8))
    => '(0 0 0 4 5 6 7 8))

  (check
      (ipv6-address-parsed-list-expand '(1 2 3 4 5 #f))
    => '(1 2 3 4 5 0 0 0))

  (check
      (ipv6-address-parsed-list-expand '(1 2 3 4 5 6 7 8 9))
    => #f)

  (check
      (ipv6-address-parsed-list-expand '(1 2 3 4 5 6 7 8 #f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (ipv6-address-parsed-list-validate-prefix 60 '(1 2 3 4 0 0 0 0))
    => #t)

  (check
      (ipv6-address-parsed-list-validate-prefix (* 16 7) '(1 2 3 4 0 0 0 0))
    => #t)

  (check
      (ipv6-address-parsed-list-validate-prefix (* 16 2) '(1 2 3 4 0 0 0 0))
    => #f)

  #t)


#;(parametrise ((check-test-name	'address-class))

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "1:2:3:4:5:6:7:8")))))
	(list (o seventh) (o sixth)  (o fifth) (o fourth)
	      (o third)   (o second) (o first) (o zeroth)))
    => '(1 2 3 4 5 6 7 8))

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "1:2:3::7:8")))))
	(list (o seventh) (o sixth) (o fifth) (o fourth)
	      (o third) (o second) (o first) (o zeroth)))
    => '(1 2 3 0 0 0 7 8))

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "1:2:3:4:5:6:7:8")))))
	(o bignum))
    => #x00010002000300040005000600070008)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "1:2:3:4:5:6:7:8")))))
	(o string))
    => "1:2:3:4:5:6:7:8")

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "::0")))))
	(o unspecified?))
    => #t)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "::1")))))
	(o unspecified?))
    => #f)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "::0")))))
	(o loopback?))
    => #f)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "::1")))))
	(o loopback?))
    => #t)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "FF00::")))))
	(o multicast?))
    => #t)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "::1")))))
	(o multicast?))
    => #f)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "FE80::")))))
	(o link-local-unicast?))
    => #t)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "::1")))))
	(o link-local-unicast?))
    => #f)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "FF80::")))))
	(o global-unicast?))
    => #f)

  (check
      (let (((o <ipv6-address>) (<ipv6-address> ((parse-ipv6-address-only "1:2::")))))
	(o global-unicast?))
    => #t)

  #t)


#;(parametrise ((check-test-name	'prefix-class))

  (check
      (let (((o <ipv6-address-prefix>) (receive (addr len)
					   (parse-ipv6-address-prefix "1:2:3:4::/55")
					 (<ipv6-address-prefix> (addr len)))))
	(list (o seventh) (o sixth) (o fifth) (o fourth)
	      (o third) (o second) (o first) (o zeroth)
	      (o prefix-length)))
    => '(1 2 3 4 0 0 0 0 55))

  (check
      (let (((o <ipv6-address-prefix>) (receive (addr len)
					   (parse-ipv6-address-prefix "1:2:3:4::/50")
					 (<ipv6-address-prefix> (addr len)))))
	(o string))
    => "1:2:3:4:0:0:0:0/50")

  #t)


;;;; done

(check-report)

;;; end of file
