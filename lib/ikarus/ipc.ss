;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2009 Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.




(library (ikarus ipc)

  (export 
    getenv setenv unsetenv

    system process process-nonblocking waitpid
    wstatus-pid wstatus-exit-status wstatus-received-signal kill 

    tcp-connect tcp-connect-nonblocking udp-connect
    udp-connect-nonblocking tcp-server-socket
    tcp-server-socket-nonblocking
    accept-connection accept-connection-nonblocking 
    close-tcp-server-socket register-callback )

  (import (ikarus)))
