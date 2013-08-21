;;;derived from the reference implementation of SRFI 106
;;;
;;;Copyright (C) 2012 Takashi Kato ktakashi@ymail.com
;;;All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission notice  shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT SHALL  THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
;;;ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

(library (srfi :106)
  (export
    make-client-socket		make-server-socket
    socket?

    socket-input-port		socket-output-port
    call-with-socket

    socket-merge-flags		socket-purge-flags
    socket-accept
    socket-send			socket-recv
    socket-shutdown		socket-close

    *af-unspec*		*af-inet*		*af-inet6*
    *sock-stream*	*sock-dgram*
    *ai-canonname*	*ai-numerichost*
    *ai-v4mapped*	*ai-all*		*ai-addrconfig*
    *ipproto-ip*	*ipproto-tcp*		*ipproto-udp*
    *shut-rd*		*shut-wr*		*shut-rdwr*

    address-family	socket-domain		address-info
    ip-protocol		message-type		shutdown-method)
  (import (srfi :106 socket)))

;;; end of file
