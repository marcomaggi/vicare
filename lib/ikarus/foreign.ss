;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2008,2009  Abdulaziz Ghuloum
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


(library (ikarus foreign)
  (export 
          pointer-set-c-char!
          pointer-set-c-short!
          pointer-set-c-int!
          pointer-set-c-long!
          pointer-set-c-long-long!
          pointer-set-c-pointer!
          pointer-set-c-float!
          pointer-set-c-double!
          pointer-ref-c-signed-char
          pointer-ref-c-signed-short
          pointer-ref-c-signed-int
          pointer-ref-c-signed-long
          pointer-ref-c-signed-long-long
          pointer-ref-c-unsigned-char 
          pointer-ref-c-unsigned-short
          pointer-ref-c-unsigned-int
          pointer-ref-c-unsigned-long
          pointer-ref-c-unsigned-long-long
          pointer-ref-c-pointer
          pointer-ref-c-float 
          pointer-ref-c-double
          malloc free memcpy
          pointer->integer integer->pointer pointer? dlopen dlsym
          dlclose dlerror
          make-c-callout make-c-callback 
          pointer-size 
          errno)

  (import (ikarus) (ikarus system $foreign))

  (define (pointer-size)
    (cond
      [(<= (fixnum-width) 32) 4]
      [else 8]))
)

