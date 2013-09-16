;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for format
;;;Date: Sun Jan 11, 2009
;;;
;;;Abstract
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (except (vicare)
		format)
  (vicare checks)
  (vicare formations))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: format library\n")


(parameterise ((check-test-name 'basic-errors))

  ;;This is signaled by the Scheme implementation.
  (check
      (guard (exc (else 'error))
	(format))
    => 'error)

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else
		   (condition-message exc)))
	(format 'wo))
    => "invalid format string")

  (check
      (guard (exc (else
		   (condition-message exc)))
	(format 'wo))
    => "invalid format string")

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else
		   (condition-message exc)))
	(format #f))
    => "invalid format string")

  (check
      (guard (exc (else
		   (condition-message exc)))
	(format #t))
    => "invalid format string")

  (check
      (guard (exc (else
		   (condition-message exc)))
	(format 123))
    => "invalid format string")

  (check
      (guard (exc (else
		   (condition-message exc)))
	(format (current-output-port)))
    => "invalid format string")

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else
		   (condition-message exc)))
	(format #f 123))
    => "invalid format string")

  )


(parameterise ((check-test-name 'destination-selection))

  (check
      (format "ciao")
    => "ciao")

  (check
      (format "ciao ~s" 123)
    => "ciao 123")

;;; --------------------------------------------------------------------

  (check
      (format #f "ciao")
    => "ciao")

  (check
      (format #f "ciao")
    => "ciao")

  (check
      (begin
	(format #t "ciao, this is text output to the current output port\n")
	(flush-output-port (current-output-port))
	#t)
    => #t)

  (check
      (begin
	(format 1 "ciao, this is text output to the current error port\n")
	#t)
    => #t)

  (check
      (call-with-string-output-port
	  (lambda (port)
	    (format port "ciao")))
    => "ciao")

  )


(parameterise ((check-test-name 'generic-object-output))

;;;; escapes ~a and ~s, generic object output

  (check
      (format #f "ciao ~a" 123)
    => "ciao 123")

  (check
      (format #f "ciao ~s" 123)
    => "ciao 123")

;;; --------------------------------------------------------------------

  (check
      (format #f "ciao ~:a" 123)
    => "ciao 123")

  (check
      (format #f "ciao ~:a" display)
    => "ciao \"#<procedure display>\"")

  (check
      (format #f "ciao ~:s" 123)
    => "ciao 123")

  (check
      (format #f "ciao ~:s" display)
    => "ciao \"#<procedure display>\"")

;;; --------------------------------------------------------------------

  (check
      (list (format "~5a" 123)
	    (format "~5s" 123))
    => '("123  "
	 "123  "))

  (check
      (list (format "~5@a" 123)
	    (format "~5@s" 123))
    => '("  123"
	 "  123"))

  (check
      (list (format "~5,,,'.a" 123)
	    (format "~5,,,'.s" 123))
    => '("123.."
	 "123.."))

  (check
      (list (format "~5,,,'.@a" 123)
	    (format "~5,,,'.@s" 123))
    => '("..123"
	 "..123"))

  (check
      (list (format "~5,,4,'.@a" 123)
	    (format "~5,,4,'.@s" 123))
    => '("....123"
	 "....123"))
;;;     1234

  (check
      (list (format "~10,,,'a@a" 123)
	    (format "~10,,,'a@s" 123))
    => '("aaaaaaa123"
	 "aaaaaaa123"))
;;;     1234567

  (check
      (list (format "~10,3,,'u@a" 123)
	    (format "~10,3,,'u@s" 123))
    => '("uuuuuuuuu123"
	 "uuuuuuuuu123"))
;;;     123456789

  (check
      (list (format "~11,2,,'u@a" 123)
	    (format "~11,2,,'u@s" 123))
    => '("uuuuuuuu123"
	 "uuuuuuuu123"))
;;;     12345678

  (check
      (list (format "~8,2,,'u@a" 1)
	    (format "~8,2,,'u@s" 1))
    => '("uuuuuuuu1"
	 "uuuuuuuu1"))
;;;     12345678

  )


(parameterise ((check-test-name 'characters))

  (check
      (format "~c" #\A)
    => "A")

  (check
      (format "~@c" #\A)
    => "#\\A")

  (check
      (format "~:c" #\newline)
    => "^J")

  (check
      (format "~:c" #\linefeed)
    => "^J")

  (check
      (format "~65c")
    => "A")

  )


(parameterise ((check-test-name 'integer-numbers))

;;;; escapes ~d ~x ~o ~b, integer numbers

  (check
      (format "~d" 123)
    => "123")

  (check
      (format "~x ~x" 3 10)
    => "3 a")

  (check
      (format "~o" 509)
    => "775")

  (check
      (format "~b" 6)
    => "110")

;;; --------------------------------------------------------------------

  (check
      (format "~d" -123)
    => "-123")

  (check
      (format "~x ~x" -3 -10)
    => "-3 -a")

  (check
      (format "~o" -509)
    => "-775")

  (check
      (format "~b" -6)
    => "-110")

;;; --------------------------------------------------------------------

  (check
      (format "~5d" 123)
    => "  123")
;;;   12345

  (check
      (format "~5x" 11)
    => "    b")
;;;   12345

  (check
      (format "~5o" 509)
    => "  775")
;;;   12345

  (check
      (format "~5b" 6)
    => "  110")
;;;   12345

;;; --------------------------------------------------------------------

  (check
      (format "~5,'.d" 123)
    => "..123")
;;;   12345

  (check
      (format "~5,'.x" 11)
    => "....b")
;;;   12345

  (check
      (format "~5,'.o" 509)
    => "..775")
;;;   12345

  (check
      (format "~5,'.b" 6)
    => "..110")
;;;   12345

;;; --------------------------------------------------------------------

  (check
      (format "~@d" 0)
    => "+0")

  (check
      (format "~@d" 123)
    => "+123")

  (check
      (format "~@x ~@x" 3 10)
    => "+3 +a")

  (check
      (format "~@o" 509)
    => "+775")

  (check
      (format "~@b" 6)
    => "+110")

;;; --------------------------------------------------------------------

  (check
      (format "~:d" 123456789)
    => "123,456,789")

  (check
      (format "~:x" #x123456789)
    => "123,456,789")

  (check
      (format "~:o" #o123456712)
    => "123,456,712")

  (check
      (format "~:b" #b10101100)
    => "10,101,100")

  (check
      (format "~,,'b,2:d" 123456789)
    => "1b23b45b67b89")

  (check
      (format "~,,'b,2:x" #x123456789)
    => "1b23b45b67b89")

  (check
      (format "~,,'b,2:o" #o123456712)
    => "1b23b45b67b12")

  (check
      (format "~,,'b,2:b" #b10101100)
    => "10b10b11b00")

;;; --------------------------------------------------------------------

  (check
      (format "~x" 65261)
    => "feed")

  (check
      (format "~:@(~x~)" 65261)
    => "FEED")

  )


(parameterise ((check-test-name 'integer-words))

;;;; escape ~r, integers in words

  (check
      (format "~r" 123)
    => "one hundred twenty-three")

  (check
      (format "~r" -123)
    => "minus one hundred twenty-three")

  (check
      (format "~r" 1000000)
    => "one million")

  (check
      (format "~r" 1000000000)
    => "one billion")

  (check
      (format "~r" 1000000000000)
    => "one trillion")

;;; --------------------------------------------------------------------

  (check
      (format "~:r" 123)
    => "one hundred twenty-third")

  (check
      (format "~:r" 9)
    => "ninth")

;;; --------------------------------------------------------------------

  (check
      (format "~@r" 89)
    => "LXXXIX")

  (check
      (format "~:@r" 89)
    => "LXXXVIIII")

;;; --------------------------------------------------------------------

  (check
      (format "~3r" 0)
    => "0")

  (check
      (format "~3r" 1)
    => "1")

  (check
      (format "~3r" 2)
    => "2")

  (check
      (format "~3r" 3)
    => "10")

  (check
      (format "~3r" 4)
    => "11")

  (check
      (format "~3r" 27)
    => "1000")

  (check
      (format "~3,5r" 26)
    => "  222")

;;; --------------------------------------------------------------------

  (check
      (format "~4r" 0)
    => "0")

  (check
      (format "~4r" 1)
    => "1")

  (check
      (format "~4r" 2)
    => "2")

  (check
      (format "~4r" 3)
    => "3")

  (check
      (format "~4r" 4)
    => "10")

  (check
      (format "~4r" 9)
    => "21")

  )


(parameterise ((check-test-name 'float-basic-tests))

;;;; esape ~f, basic tests

  (check
      (format "~f" 123)
    => "123.0")

  (check	;upper case escape sequence
      (format "~F" 123)
    => "123.0")

  (check
      (format "~f" 0.0)
    => "0.0")

  (check
      (format "~f" 0e-3)
    => "0.0")

  (check
      (format "~f" 123.0)
    => "123.0")

  (check
      (format "~f" 123.4)
    => "123.4")

  (check
      (format "~f" 1e-1)
    => "0.1")

  (check
      (format "~f" +inf.0)
    => "+inf.0")

  (check
      (format "~f" -inf.0)
    => "-inf.0")

  (check
      (format "~f" +nan.0)
    => "+nan.0")

;;; --------------------------------------------------------------------
;;; @ modifier

  (check
      (format "~@f" 123)
    => "+123.0")

  (check
      (format "~@f" 123.0)
    => "+123.0")

  (check
      (format "~@f" 123.4)
    => "+123.4")

  (check
      (format "~@f" 1e-1)
    => "+0.1")

  (check
      (format "~@f" -123.0)
    => "-123.0")

;;; --------------------------------------------------------------------
;;; width and padding

  (check
      (format "~10f" 123.456)
    => "   123.456")
;;;   0123456789

  (check
      (format "~10,,,,'.f" 123.456)
    => "...123.456")
;;;   0123456789

  (check
      (format "~10,,,,'\nf" 123.456)
    => "\n\n\n123.456")
;;;   0 1 2 3456789

  (check
      (format "~10,,,,'.f" 123.456789123)
    => "123.456789")

  (check
      (format "~5,,,,'.f" 1e9)
    => "1000000000.0")

  (check
      (format "~5,,,,'.f" 1000000000.123456)
    => "1000000000.123456")

;;; --------------------------------------------------------------------
;;; strings

  (check
      (format "~12,2f" "1.2345")
    => "        1.23")

  (check
      (format "~f" "#d1.2345")
    => "1.2345")

  (check
      (format "~f" "1.2345")
    => "1.2345")

  (check
      (format "~f" "1.23e4")
    => "12300.0")

  (check
      (format "~f" "-1.23")
    => "-1.23")

  (check
      (format "~f" "+1.23")
    => "1.23")

  (check
      (format "~f" "0.0")
    => "0.0")

  (check
      (format "~f" "00000000.00000000000")
    => "0.0")

  (check
      (format "~f" "0.")
    => "0.0")

  (check
      (format "~f" ".0")
    => "0.0")

  (check
      (format "~f" "0e-3")
    => "0.0")

  (check
      (format "~f" "000.000e-3")
    => "0.0")

  (check
      (format "~f" ".0000123456e2")
    => "0.00123456")

  (check
      (format "~f" ".00123456789e4")
    => "12.3456789")

  (check
      (guard (exc (else 'error))
	(format "~f" ""))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~12,2f" "1.23+45"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~12,2f" "1.23-45"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~12,2f" "1.2345e6-1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~12,2f" "1.2345e6+1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~f" "1.2.3e61"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~f" "1..3e61"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~f" "1.23e6.1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~f" "1.23e6e1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~f" "1.23a61"))
    => 'error)

  #f)


(parameterise ((check-test-name 'float-decimal-number))

;;; escape ~f, number of decimals

  (check
      (format "~6,3f" 1/3)
    => " 0.333")

  (check
      (format "~8,3f" 12.3456)
    => "  12.346")

  (check
      (format "~6,3f" 123.3456)
    => "123.346")

  (check
      (format "~4,3f" 123.3456)
    => "123.346")

  (check
      (format "~8,1f" 32e-45)
    => "     0.0")
;;;   01234567

  (check
      (format "~8,2f" 32e10)
    => "320000000000.00")
;;;   012345678901

;;; --------------------------------------------------------------------

  (check
      (format "~8,2f" 3.4567e11)
    => "345670000000.00")
;;;   012345678901

  (check
      (format "~6,2f" 3.14159)
    => "  3.14")

  (check
      (format "~6,1f" 3.14159)
    => "   3.1")

  (check
      (format "~6,0f" 3.14159)
    => "    3.")

  (check
      (format "~5,1f" 0)
    => "  0.0")

  (check
      (format "~10,7f" 3.14159)
    => " 3.1415900")

  (check
      (format "~10,7f" -3.14159)
    => "-3.1415900")

  (check
      (format "~6,3f" 0.0)
    => " 0.000")

  (check
      (format "~6,4f" 0.007)
    => "0.0070")

  (check
      (format "~8,4f" 0.007)
    => "  0.0070")

  (check
      (format "~6,3f" 0.007)
    => " 0.007")

  (check
      (format "~6,2f" 0.007)
    => "  0.01")

  (check
      (format "~3,2f" 0.007)
    => ".01")

  (check
      (format "~3,2f" -0.007)
    => "-.01")

  (check
      (format "~6,3f" 12345.6789)
    => "12345.679")

  )


(parameterise ((check-test-name 'float-decimal-rounding))

;;;; escape ~f, rounding of decimals

  (check
      (format "~12,2f" 1.2345)
    => "        1.23")
;;;   012345678901

  (check
      (format "~12,3f" 1.2345)
    => "       1.234")
;;;   012345678901

  (check
      (format "~,2f" 0.007)
    => "0.01")

  (check
      (format "~,5f" 12.456e999)
    => "+inf.0")

  (check
      (format "~,5f" -12.456e999)
    => "-inf.0")

  (check
      (format "~,5f" 12.456e10)
    => "124560000000.00000")
;;;     0123456789

  (check
      (format "~,5f" 12.456)
    => "12.45600")

  (check
      (format "~,5f" 12.456)
    => "12.45600")

;;; --------------------------------------------------------------------

  (check
      (format "~,1f" 12.44)
    => "12.4")

  (check
      (format "~,1f" 12.46)
    => "12.5")

  ;;When 5 is the last digit: the number is rounded with the last digit in
  ;;the result being the nearest even.
  (check
      (format "~,1f" 12.45)
    => "12.4")

  (check
      (format "~,1f" 12.451)
    => "12.5")

  (check
      (format "~,1f" 12.454)
    => "12.5")

  (check
      (format "~,1f" 12.456)
    => "12.5")

  ;;Not so weird if you think of it!
  (check
      (format "~,1f" 12.449)
    => "12.4")

  ;;Rounding 55 is done to the nearest even which is 60.
  (check
      (format "~,2f" 12.455)
    => "12.46")

  (check
      (format "~,1f" 12.455)
    => "12.5")

  (check
      (format "~,1f" 12.4555)
    => "12.5")

  (check
      (format "~,1f" 12.4555)
    => "12.5")

  (check
      (format "~,1f" 12.45555)
    => "12.5")

;;; --------------------------------------------------------------------

  (check
      (format "~,0f" 12.456789)
    => "12.")

  ;;Rounding 12.456789  to 1  digit in the  fractional part is  like doing
  ;;these steps:
  ;;
  ;; 12.456789 -> 12.45679 -> 12.4568 -> 12.457 -> 12.46 -> 12.5
  ;;
  (check
      (format "~,1f" 12.456789)
    => "12.5")

  (check
      (format "~,2f" 12.456789)
    => "12.46")

  (check
      (format "~,3f" 12.456789)
    => "12.457")

  (check
      (format "~,4f" 12.456789)
    => "12.4568")

  (check
      (format "~,5f" 12.456789)
    => "12.45679")

  (check
      (format "~,6f" 12.456789)
    => "12.456789")

;;; --------------------------------------------------------------------

  ;;We want  the same behaviour requested  by R6RS for ROUND,  and by IEEE
  ;;754 for rounding to nearest.  When rounding an in-the-middle digit, we
  ;;round it to even.
  (check (format "~,0f" 0.5) => "0.")
  (check (format "~,0f" 1.5) => "2.")
  (check (format "~,0f" 2.5) => "2.")
  (check (format "~,0f" 3.5) => "4.")
  (check (format "~,0f" 4.5) => "4.")
  (check (format "~,0f" 5.5) => "6.")
  (check (format "~,0f" 6.5) => "6.")
  (check (format "~,0f" 7.5) => "8.")
  (check (format "~,0f" 8.5) => "8.")
  (check (format "~,0f" 9.5) => "10.")

  (check (format "~,0f" 0.0)  => "0.")
  (check (format "~,0f" 0.3)  => "0.")
  (check (format "~,0f" 0.51) => "1.")
  (check (format "~,0f" 0.7)  => "1.")

  ;;Remember that the  dot and fractional part are  truncated only if this
  ;;makes  the output  fit the  requested WIDTH,  else they  are  kept and
  ;;rounded only if a number of digits after the dot was requested.
  (check (format "~1,0f" 0.0)  => "0.")
  (check (format "~1,0f" 1.4)  => "1.")
  (check (format "~1,0f" 1.5)  => "2.")
  (check (format "~1,0f" 1.6)  => "2.")

  (check (format "~1f"   0.123) => "0.123")
  (check (format "~1,2f" 0.123) => ".12")
  (check (format "~1,2f" 1.123) => "1.12")
  (check (format "~2,2f" 0.123) => ".12")

  )


(parameterise ((check-test-name 'float-scaling))

;;;; escape ~f, scaling factor

  (check (format "~,,1f"	1)	=> "10.0")
  (check (format "~,,1f"	10)	=> "100.0")
  (check (format "~,,1f"	0.1)	=> "1.0")
  (check (format "~,,1f"	0.01)	=> "0.1")

  (check (format "~,,-1f"	1)	=> "0.1")
  (check (format "~,,-1f"	10)	=> "1.0")
  (check (format "~,,-1f"	0.1)	=> "0.01")
  (check (format "~,,-1f"	0.01)	=> "0.001")

  )


(parameterise ((check-test-name 'float-overflow-char))

;;;; escape ~f, overflow char

  (check (format "~4,,,'xf" 12345) => "xxxx")
  (check (format "~4,,,'xf" 123e5) => "xxxx")

  )


(parameterise ((check-test-name 'exponential-basic))

;;;; escape ~e, basic tests

  (check
      (format "~e" 123)
    => "1.23E+2")

  (check
      (format "~e" 123.0)
    => "1.23E+2")

  (check
      (format "~e" 123.4)
    => "1.234E+2")

  (check
      (format "~e" 1e-1)
    => "1.0E-1")

  (check
      (format "~e" 1e9)
    => "1.0E+9")

  (check
      (format "~e" +inf.0)
    => "+inf.0")

  (check
      (format "~e" -inf.0)
    => "-inf.0")

  (check
      (format "~e" +nan.0)
    => "+nan.0")

  (check
      (format "~,,,,,,'ee" 123)
    => "1.23e+2")

;;; --------------------------------------------------------------------
;;; @ modifier

  (check
      (format "~@e" 123)
    => "+1.23E+2")

  (check
      (format "~@e" 123.0)
    => "+1.23E+2")

  (check
      (format "~@e" 123.4)
    => "+1.234E+2")

  (check
      (format "~@e" 1e-1)
    => "+1.0E-1")

  (check
      (format "~@e" -123.0)
    => "-1.23E+2")

;;; --------------------------------------------------------------------
;;; strings

  (check
      (format "~12,2e" "1.2345")
    => "     1.23E+0")
;;;   012345678901

  (check
      (format "~e" "#d1.2345")
    => "1.2345E+0")

  (check
      (format "~e" "1.2345")
    => "1.2345E+0")

  (check
      (format "~e" "1.23e4")
    => "1.23E+4")

  (check
      (format "~e" "-1.23")
    => "-1.23E+0")

  (check
      (format "~e" "+1.23")
    => "1.23E+0")

  (check
      (guard (exc (else 'error))
	(format "~12,2e" "1.23+45"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~12,2e" "1.23-45"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~12,2e" "1.2345e6-1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~12,2e" "1.2345e6+1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~e" "1.2.3e61"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~e" "1..3e61"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~e" "1.23e6.1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~e" "1.23e6e1"))
    => 'error)

  (check
      (guard (exc (else 'error))
	(format "~e" "1.23a61"))
    => 'error)

  )


(parameterise ((check-test-name 'exponential-width))

;;; escape ~e, width and padding

  (check
      (format "~10e" 123.456)
    => "1.23456E+2")
;;;   0123456789

  (check
      (format "~15,,,,,'.e" 123.456)
    => ".....1.23456E+2")
;;;   012345678901234

  (check
      (format "~10,,,,,'.e" 123.456789123)
    => "1.23457E+2")
;;;   0123456789

  (check
      (format "~5,,,,'.e" 1e9)
    => "1.E+9")
;;;   01234

  (check
      (format "~5,,,,'x,'.e" 1e9)
    => "1.E+9")
;;;   01234

  (check
      (format "~2,,,,'x,'.e" 1e9)
    => "xx")

  (check
      (format "~5,,,,'.e" 1000000000.123456)
;;;                     0123456789
    => "1.E+9")

  (check
      (format "~6,,,,'.e" 1000000000.123456)
;;;                     0123456789
    => "1.0E+9")

  (check
      (format "~5e" 123456)
    => "1.E+5")
;;;   01234

  (check
      (format "~2e" 123456)
    => "1.23456E+5")
;;;   01234

  )


(parameterise ((check-test-name 'exponential-misc))

;;; escape ~e, number of decimals, exponents, integer digits, overflow

  (check
      (format "~6,3e" 1/3)
    => "3.333E-1")
;;;   012345

  (check
      (format "~8,3e" 12.3456)
    => "1.235E+1")
;;;   01234567

  (check
      (format "~6,3e" 123.3456)
    => "1.233E+2")
;;;   012345

;;; --------------------------------------------------------------------
;;; number of exponent digits

  (check
      (format "~,,1e" 1.0e99)
    => "1.0E+99")

  (check
      (format "~,,6e" 1.0e99)
    => "1.0E+000099")

;;; --------------------------------------------------------------------
;;; number of integer digits

  (check
      (format "~,,,0e" 12345.0)
    => "0.12345E+5")

  (check
      (format "~,,,1e" 12345.0)
    => "1.2345E+4")

  (check
      (format "~,,,2e" 12345.0)
    => "12.345E+3")

  (check
      (format "~,,,3e" 12345.0)
    => "123.45E+2")

  (check
      (format "~,,,4e" 12345.0)
    => "1234.5E+1")

  (check
      (format "~,,,5e" 12345.0)
    => "12345.0E+0")

  (check
      (format "~,,,6e" 12345.0)
    => "123450.0E-1")

  (check
      (format "~,,,7e" 12345.0)
    => "1234500.0E-2")

  (check
      (format "~,,,8e" 12345.0)
    => "12345000.0E-3")

  (check
      (format "~,,,-1e" 12345.0)
    => "0.012345E+6")

  (check
      (format "~,,,-2e" 12345.0)
    => "0.0012345E+7")

  (check
      (format "~,,,-3e" 12345.0)
    => "0.00012345E+8")

  (check
      (format "~,,,0e" 1.2345)
    => "0.12345E+1")

  (check
      (format "~,,,1e" 1.2345)
    => "1.2345E+0")

  (check
      (format "~,,,2e" 1.2345)
    => "12.345E-1")

  (check
      (format "~,,,3e" 1.2345)
    => "123.45E-2")

  (check
      (format "~,,,4e" 1.2345)
    => "1234.5E-3")

  (check
      (format "~,,,-1e" 1.2345)
    => "0.012345E+2")

  (check
      (format "~,,,-2e" 1.2345)
    => "0.0012345E+3")

;;; --------------------------------------------------------------------
;;; number of integer digits and decimals

  ;;this does not round
  (check
      (format "~,4,,1e" 123.45)
    => "1.2345E+2")

  ;;this does not round
  (check
      (format "~,3,,2e" 123.45)
    => "12.345E+1")

  (check
      (format "~,5,,2e" 123.45)
    => "12.34500E+1")

  (check
      (format "~,5,,1e" 123.45)
    => "1.23450E+2")

  (check
      (format "~,1,,3e" 12345.0)
    => "123.4E+2")

  (check
      (format "~,3,,3e" 12345.6789)
    => "123.457E+2")

;;; --------------------------------------------------------------------
;;; overflow char

  (check (format "~4,,,,'xe" 12345) => "xxxx")
  (check (format "~4,,,,'xe" 123e5) => "xxxx")

  )


(parameterise ((check-test-name 'complex-numbers))

;;;; complex numbers

  (check
      (format "~i" 1+2i)
    => "1.0+2.0i")

  (check
      (format "~,3i" (sqrt -3.8))
    => "0.000+1.949i")

  (check
      (format "~10,3i" (sqrt -3.8))
    => "     0.000    +1.949i")
;;;   0123456789
;;;             0123456789

  (check
      (format "~8,3i" (sqrt -3.8))
    => "   0.000  +1.949i")
;;;   01234567
;;;           01234567


  #t)


(parameterise ((check-test-name 'plurals))

  (check
      (format "enter name~p" 1)
    => "enter name")

  (check
      (format "enter name~p" 2)
    => "enter names")

;;; --------------------------------------------------------------------

  (check
      (format "pupp~@p" 1)
    => "puppy")

  (check
      (format "pupp~@p" 2)
    => "puppies")

;;; --------------------------------------------------------------------

  (check
      (format "~d cat~:p" 9)
    => "9 cats")

  (check
      (format "~d pupp~:@p" 5)
    => "5 puppies")

  #t)


(parameterise ((check-test-name 'pretty-print))

  (check
      (format "a ~y b" '(1 2))
    => "a (1 2)\n b")

  #t)


(parameterise ((check-test-name 'sub-format))

  (check
      (format "~?" "~d ~d" '(1 2))
    => "1 2")

  (check
      (format "~@? ~s" "~d ~d" 1 2 "foo")
    => "1 2 \"foo\"")

;;; --------------------------------------------------------------------

  (check
      (format "~k" "~d ~d" '(1 2))
    => "1 2")

  (check
      (format "~@k ~s" "~d ~d" 1 2 "foo")
    => "1 2 \"foo\"")

  #t)


(parameterise ((check-test-name 'jumping))

  (check
      (format "~d ~2*~d" 1 2 3 4)
    => "1 4")

  (check
      (format #f "~d ~:*~d" 6)
    => "6 6")

  (check
      (format #f "~d~d again ~@*~d~d" 1 2)
    => "12 again 12")

  (check
      (format #f "~d~d~d ~1@*~d~d" 1 2 3)
    => "123 23")

  (check
      (format "~#*~2:*~a" 'a 'b 'c 'd)
    => "c")

  #t)


(parametrise ((check-test-name	'column-position))

  (check (format "~tX")			=> " X")
  (check (format "~0tX")		=> "X")
  (check (format "~1tX")		=> " X")
  (check (format "~2tX")		=> "  X")
  (check (format "~3tX")		=> "   X")

;;; --------------------------------------------------------------------

  (check (format "~,,'.tX")		=> ".X")
  (check (format "~0,,'.tX")		=> "X")
  (check (format "~1,,'.tX")		=> ".X")
  (check (format "~2,,'.tX")		=> "..X")
  (check (format "~3,,'.tX")		=> "...X")

;;; --------------------------------------------------------------------

  ;; colnum + N * colinc = 0+N*5 = 0+1*5 = 5
  (check (format "abcd~0,5,'.tX")		=> "abcd.X")
;;;                                                 0123456789

  ;; colnum + N * colinc = 1+N*5 = 1+1*5 = 6
  (check (format "abcd~1,5,'.tX")		=> "abcd..X")
;;;                                                 0123456789

  ;; colnum + N * colinc = 2+N*5 = 2+1*5 = 7
  (check (format "abcd~2,5,'.tX")		=> "abcd...X")
;;;                                                 0123456789

  ;; colnum + N * colinc = 3+N*5 = 3+1*5 = 8
  (check (format "abcd~3,5,'.tX")		=> "abcd....X")
;;;                                                 0123456789

;;; --------------------------------------------------------------------

  (check (format "a~3,5'*@tx")			=> "a****x")

  #f)


;;;; done

(check-report)

;;; end of file
