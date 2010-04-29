#!/usr/bin/env scheme-script


;;; This is a debugging tool for developing the 64-bit
;;; ikarus.  It only works on Mac OS.

(import (ikarus))

(define stub1
  '(#xcf #xfa #xed #xfe #x07 #x00 #x00 #x01
    #x03 #x00 #x00 #x00 #x01 #x00 #x00 #x00
    #x01 #x00 #x00 #x00 #x98 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x19 #x00 #x00 #x00 #x98 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #xb8 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x07 #x00 #x00 #x00 #x07 #x00 #x00 #x00
    #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x5f #x5f #x74 #x65 #x78 #x74 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x5f #x5f #x54 #x45 #x58 #x54 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    ))

(define stub2
  '(#x08 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

(define (mkstub2 len)
  (bytevector->u8-list 
    (let ([v (make-bytevector 8)])
      (bytevector-u64-set! v 0 len 'little)
      v)))

(define stub3
  '(#xb8 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x04 #x00 #x80 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    ))

(define (gen ls)
  (let ([p (open-file-output-port "tmp.o" (file-options no-fail))])
    (for-each 
      (lambda (b) 
        (put-u8 p b))
      (append stub1 (mkstub2 (length ls)) stub3 ls))
    (close-output-port p))
  (system "otool -tv tmp.o"))

(printf "Trying a simple sequence ...\n")

(gen '(#x48 #xc7 #xc3 #x50 #x01 #x00 #x00 #xc3))

(printf "That should've printed the following: \n\
         tmp.o: \n\
         (__TEXT,__text) section\n\
         0000000000000000        movq    $0x00000150,%rbx\n\
         0000000000000007        ret\n\n\
         OK, now you can enter byte sequences like\n\
         \x20;  (72 199 195 80 1 0 0 195)\n\n")

(new-cafe gen)


