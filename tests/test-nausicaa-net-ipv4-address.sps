;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare ausicaa/Scheme
;;;Contents: tests for IPv4 address object
;;;Date: Fri Jun 11, 2010
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
  (nausicaa net ipv4-addresses)
  (nausicaa net helpers ipv4-address-lexer)
  (prefix (vicare parser-tools silex lexer) lex.)
  (prefix (nausicaa parser-tools lexical-tokens) lt.)
  (prefix (nausicaa parser-tools source-locations) sl.)
  (prefix (nausicaa net helpers ipv4-address-parser) parser.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: net IPv4 address\n")


(parametrise ((check-test-name	'lexing))

  (define (tokenise-address string)
    (let* ((IS		(lex.make-IS (lex.string: string) (lex.counters: 'all)))
	   (lexer	(lex.make-lexer ipv4-address-lexer-table IS))
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
      (tokenise-address "1.2.3.4")
    => `((NUMBER . 1)
	 (DOT  . #\.)
	 (NUMBER . 2)
	 (DOT  . #\.)
	 (NUMBER . 3)
	 (DOT  . #\.)
	 (NUMBER . 4)
	 ,eoi))

  (check
      (tokenise-address "1.Zciao")
    => '((NUMBER . 1)
	 (DOT    . #\.)
	 (*lexer-error* . "Zciao")))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "1")
    => `((NUMBER . 1)
	 ,eoi))

  (check
      (tokenise-address "10")
    => `((NUMBER . 10)
	 ,eoi))

  (check
      (tokenise-address "100")
    => `((NUMBER . 100)
	 ,eoi))

  (check
      (tokenise-address "190")
    => `((NUMBER . 190)
	 ,eoi))

  (check
      (tokenise-address "210")
    => `((NUMBER . 210)
	 ,eoi))

  (check
      (tokenise-address "250")
    => `((NUMBER . 250)
	 ,eoi))

  (check
      (tokenise-address "255")
    => `((NUMBER . 255)
	 ,eoi))

  (check
      (tokenise-address "256")
    => `((NUMBER . 25)
	 (NUMBER . 6)
	 ,eoi))

  (check
      (tokenise-address "500")
    => `((NUMBER . 50)
	 (NUMBER . 0)
	 ,eoi))

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (tokenise-address "0xa")
  ;;   => `((NUMBER . 10)
  ;; 	 ,eoi))

  ;; (check
  ;;     (tokenise-address "0xFE")
  ;;   => `((NUMBER . 254)
  ;; 	 ,eoi))

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (tokenise-address "02")
  ;;   => `((NUMBER . 2)
  ;; 	 ,eoi))

  ;; (check
  ;;     (tokenise-address "012")
  ;;   => `((NUMBER . 10)
  ;; 	 ,eoi))

  ;; (check
  ;;     (tokenise-address "0123")
  ;;   => `((NUMBER . 83)
  ;; 	 ,eoi))

  #t)


(parametrise ((check-test-name	'parsing))

  (define-condition-type &parser-error
    (parent &assertion))

  (define (make-ipv4-address-parser-error-handler who string)
    (lambda ((message <string>) (token lt.<lexical-token>))
      (raise
       (condition (make-parser-error-condition)
		  (make-who-condition who)
		  (make-message-condition (let (((pos sl.<source-location>) (token location)))
					    (string-append "invalid Ipv4 address input at column "
							   (pos column string) ": " message)))
		  (make-irritants-condition (list string (token value)))))))

  (define (parse-address string)
    (let* ((IS		(lex.make-IS (lex.string: string) (lex.counters: 'all)))
	   (lexer	(lex.make-lexer ipv4-address-lexer-table IS))
	   (parser	(parser.make-ipv4-address-parser)))
      (parser lexer (make-ipv4-address-parser-error-handler 'parse-address string))))

;;; --------------------------------------------------------------------
;;; plain addresses

  (check
      (parse-address "1.2.3.4")
    => '(1 2 3 4))

  ;; (check
  ;;     (parse-address "0x1.0x2.0x3.0x4")
  ;;   => '(1 2 3 4))

  ;; (check
  ;;     (parse-address "01.02.03.04")
  ;;   => '(1 2 3 4))

  (check
      (parse-address "192.168.99.1")
    => '(192 168 99 1))

;;; --------------------------------------------------------------------
;;; prefix

  (check
      (parse-address "1.2.3.4/8")
    => '(1 2 3 4 (8)))

  ;; (check
  ;;     (parse-address "0x1.0x2.0x3.0x4/8")
  ;;   => '(1 2 3 4 8))

  ;; (check
  ;;     (parse-address "01.02.03.04/8")
  ;;   => '(1 2 3 4 8))

  (check
      (parse-address "192.168.99.1/8")
    => '(192 168 99 1 (8)))

;;; --------------------------------------------------------------------
;;; errors

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1.2.3.4.5"))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1,"))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1..2..3"))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1..2.."))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "..2..3"))
    => #t)

  #t)


(parametrise ((check-test-name	'class-address))

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "1.2.3.4")))))
	(list (o third) (o second) (o first) (o zeroth)))
    => '(1 2 3 4))

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "1.2.3.4")))))
	(o string))
    => "1.2.3.4")

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "10.0.0.1")))))
  	(o private?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "172.16.0.1")))))
  	(o private?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "172.20.0.1")))))
  	(o private?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "192.168.0.1")))))
  	(o private?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "123.0.0.1")))))
  	(o private?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "127.0.0.1")))))
  	(o loopback?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o loopback?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "127.0.0.1")))))
  	(o localhost?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o localhost?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "169.254.0.1")))))
  	(o link-local?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o link-local?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "192.0.0.1")))))
  	(o reserved?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "240.0.0.1")))))
  	(o reserved?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o reserved?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "192.0.2.1")))))
  	(o test-net-1?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o test-net-1?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "192.88.99.1")))))
  	(o six-to-four-relay-anycast?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o six-to-four-relay-anycast?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "198.18.0.1")))))
  	(o benchmark-tests?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o benchmark-tests?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "198.51.100.1")))))
  	(o test-net-2?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o test-net-2?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "203.0.113.1")))))
  	(o test-net-3?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o test-net-3?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "224.0.113.1")))))
  	(o multicast?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o multicast?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "255.255.255.255")))))
  	(o limited-broadcast?))
    => #t)

  (check
      (let (((o <ipv4-address>) (<ipv4-address> ((ipv4-address-parse "100.0.0.1")))))
  	(o limited-broadcast?))
    => #f)

  #t)


(parametrise ((check-test-name	'class-prefix))

  (check
      (let (((o <ipv4-address-prefix>) (receive (addr len)
					   (ipv4-address-prefix-parse "1.2.3.4/10")
					 (<ipv4-address-prefix> (addr len)))))
	(list (o third) (o second) (o first) (o zeroth) (o prefix-length)))
    => '(1 2 3 4 10))

  (check
      (let (((o <ipv4-address-prefix>) (receive (addr len)
					   (ipv4-address-prefix-parse "1.2.3.4/8")
					 (<ipv4-address-prefix> (addr len)))))
	(o string))
    => "1.2.3.4/8")

  #t)


;;;; done

(check-report)

;;; end of file
