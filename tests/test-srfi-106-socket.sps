;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 106
;;;Date: Thu Aug 22, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (prefix (srfi :106) srfi.)
  (vicare platform constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: 106, basic sockets interface\n")


(parametrise ((check-test-name	'flags))

  (check
      (srfi.address-family inet)
    => AF_INET)

  (check
      (srfi.address-family inet6)
    => AF_INET6)

  (check
      (srfi.address-family unspec)
    => AF_UNSPEC)

;;; --------------------------------------------------------------------

  (check
      (srfi.address-info canoname numerichost v4mapped addrconfig all)
    => (bitwise-ior AI_CANONNAME AI_NUMERICHOST AI_V4MAPPED AI_ADDRCONFIG AI_ALL))

  (check
      (srfi.address-info canonname)
    => AI_CANONNAME)

;;; --------------------------------------------------------------------

  (check
      (srfi.socket-domain stream)
    => SOCK_STREAM)

  (check
      (srfi.socket-domain datagram)
    => SOCK_DGRAM)

;;; --------------------------------------------------------------------

  (check
      (srfi.ip-protocol ip)
    => IPPROTO_IP)

  (check
      (srfi.ip-protocol tcp)
    => IPPROTO_TCP)

  (check
      (srfi.ip-protocol udp)
    => IPPROTO_UDP)

;;; --------------------------------------------------------------------

  (check
      (srfi.message-type none)
    => 0)

  (check
      (srfi.message-type peek oob wait-all)
    => (bitwise-ior MSG_PEEK MSG_OOB MSG_WAITALL))

;;; --------------------------------------------------------------------

  (check
      (srfi.shutdown-method read)
    => SHUT_RD)

  (check
      (srfi.shutdown-method write)
    => SHUT_WR)

  (check
      (srfi.shutdown-method read write)
    => SHUT_RDWR)

  #t)


;;;; done

(check-report)

;;; end of file
