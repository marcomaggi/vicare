;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi.
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

(library (ikarus.intel-assembler)
  (export assemble-sources code-entry-adjustment)
  (import (ikarus)
    (except (ikarus.code-objects)
	    procedure-annotation)
    (ikarus system $pairs))


(module (wordsize)
  (import (vicare include))
  (include "ikarus.config.ss"))

(define fold
  (lambda (f init ls)
    (cond
      [(null? ls) init]
      [else
       (f (car ls) (fold f init (cdr ls)))])))


(define convert-instructions
  (lambda (ls)
    (parameterize ([local-labels (uncover-local-labels ls)])
      (fold convert-instruction '() ls))))

(define register-mapping
  ;;; reg  cls  idx  REX.R
  '([%eax   32    0  #f]
    [%ecx   32    1  #f]
    [%edx   32    2  #f]
    [%ebx   32    3  #f]
    [%esp   32    4  #f]
    [%ebp   32    5  #f]
    [%esi   32    6  #f]
    [%edi   32    7  #f]
    [%r8    32    0  #t]
    [%r9    32    1  #t]
    [%r10   32    2  #t]
    [%r11   32    3  #t]
    [%r12   32    4  #t]
    [%r13   32    5  #t]
    [%r14   32    6  #t]
    [%r15   32    7  #t]
    [%al     8    0  #f]
    [%cl     8    1  #f]
    [%dl     8    2  #f]
    [%bl     8    3  #f]
    [%ah     8    4  #f]
    [%ch     8    5  #f]
    [%dh     8    6  #f]
    [%bh     8    7  #f]
    [/0      0    0  #f]
    [/1      0    1  #f]
    [/2      0    2  #f]
    [/3      0    3  #f]
    [/4      0    4  #f]
    [/5      0    5  #f]
    [/6      0    6  #f]
    [/7      0    7  #f]
    [xmm0  xmm    0  #f]
    [xmm1  xmm    1  #f]
    [xmm2  xmm    2  #f]
    [xmm3  xmm    3  #f]
    [xmm4  xmm    4  #f]
    [xmm5  xmm    5  #f]
    [xmm6  xmm    6  #f]
    [xmm7  xmm    7  #f]
    [%r8l    8    0  #t]
    [%r9l    8    1  #t]
    [%r10l   8    2  #t]
    [%r11l   8    3  #t]
    [%r12l   8    4  #t]
    [%r13l   8    5  #t]
    [%r14l   8    6  #t]
    [%r15l   8    7  #t]

    ))

(define register-index
  (lambda (x)
    (cond
      [(assq x register-mapping) => caddr]
      [else (die 'register-index "not a register" x)])))

(define reg32?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (eqv? (cadr x) 32))]
      [else #f])))

(define reg8?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (eqv? (cadr x) 8))]
      [else #f])))

(define xmmreg?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (eqv? (cadr x) 'xmm))]
      [else #f])))

(define reg?
  (lambda (x)
    (assq x register-mapping)))

(define reg-requires-REX?
  (lambda (x)
    (cond
      [(assq x register-mapping) => cadddr]
      [else (error 'reg-required-REX? "not a reg" x)])))

(define-syntax with-args
  (syntax-rules (lambda)
    [(_ x (lambda (a0 a1) b b* ...))
     (let ([t x])
       (if (pair? t)
           (let ([t ($cdr t)])
             (if (pair? t)
                 (let ([a0 ($car t)] [t ($cdr t)])
                   (if (pair? t)
                        (let ([a1 ($car t)])
                           (if (null? ($cdr t))
                               (let () b b* ...)
                               (die 'with-args "too many args")))
                        (die 'with-args "too few args")))
                 (die 'with-args "too few args")))
           (die 'with-args "too few args")))]))



(define-syntax byte
  (syntax-rules ()
    [(_ x)
     (let ([t x])
       (if (integer? t)
           (bitwise-and t 255)
           (error 'byte "invalid" t '(byte x))))]))


(define word
  (lambda (x)
    (cons 'word x)))

(define reloc-word
  (lambda (x)
    (cons 'reloc-word x)))

(define reloc-word+
  (lambda (x d)
    (cons* 'reloc-word+ x d)))

(define byte?
  (lambda (x)
    (and (fixnum? x)
         (fx<= x 127)
         (fx<= -128 x))))

(define mem?
  (lambda (x)
    (and (pair? x)
         (eq? (car x) 'disp))))

(define small-disp?
  (lambda (x)
    (and (mem? x)
         (byte? (cadr x)))))


(define CODE
  (lambda (n ac)
    (cons (byte n) ac)))

(define CODE+r
  (lambda (n r ac)
    (cons (byte (fxlogor n (register-index r))) ac)))

(define ModRM
  (lambda (mod reg r/m ac)
    (cons (byte (fxlogor
                  (register-index r/m)
                  (fxlogor
                    (fxsll (register-index reg) 3)
                    (fxsll mod 6))))
          (if (and (not (fx= mod 3)) (eq? r/m '%esp))
              (cons (byte #x24) ac)
              ac))))
(define IMM32
  (lambda (n ac)
    (cond
      [(= wordsize 4) (IMM n ac)]
      [(imm32? n)
       (cons*
         (byte n)
         (byte (sra n 8))
         (byte (sra n 16))
         (byte (sra n 24))
         ac)]
      [(label? n)
       (cond
         [(local-label? (label-name n))
          (cons (cons 'local-relative (label-name n)) ac)]
         [else
          (cons (cons 'relative (label-name n)) ac)])]
      [else (die 'IMM32 "invalid" n)])))

(define IMM
  (lambda (n ac)
    (cond
      [(int? n)
       (case wordsize
         [(4)
          (cons*
            (byte n)
            (byte (sra n 8))
            (byte (sra n 16))
            (byte (sra n 24))
            ac)]
         [else
          (cons*
            (byte n)
            (byte (sra n 8))
            (byte (sra n 16))
            (byte (sra n 24))
            (byte (sra n 32))
            (byte (sra n 40))
            (byte (sra n 48))
            (byte (sra n 56))
            ac)])]
      [(obj? n)
       (let ([v (cadr n)])
         (if (immediate? v)
             (cons (word v) ac)
             (cons (reloc-word v) ac)))]
      [(obj+? n)
       (let ([v (cadr n)] [d (caddr n)])
         (cons (reloc-word+ v d) ac))]
      [(label-address? n)
       (cons (cons 'label-addr (label-name n)) ac)]
      [(foreign? n)
       (cons (cons 'foreign-label (label-name n)) ac)]
      [(label? n)
       (cond
         [(local-label? (label-name n))
          (cons (cons 'local-relative (label-name n)) ac)]
         [else
          (cons (cons 'relative (label-name n)) ac)])]
       ;(cons (cons 'relative (label-name n)) ac)]
      [else (die 'IMM "invalid" n)])))


(define IMM8
  (lambda (n ac)
    (cond
      [(int? n)
       (cons* (byte n) ac)]
      [else (die 'IMM8 "invalid" n)])))


(define imm?
  (lambda (x)
    (or (int? x)
        (obj? x)
        (obj+? x)
        (label-address? x)
        (foreign? x)
        (label? x))))

(define foreign?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'foreign-label))))


(define imm8?
  (lambda (x)
    (and (int? x) (byte? x))))

(define label?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'label))))

(define label-address?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'label-address))))

(define label-name
  (lambda (x) (cadr x)))

(define int? integer?)

(define obj?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'obj))))

(define obj+?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'obj+))))

(define CODErri
  (lambda (c d s i ac)
    (cond
      [(imm8? i)
       (CODE c (ModRM 1 d s (IMM8 i ac)))]
      [(imm? i)
       (CODE c (ModRM 2 d s (IMM i ac)))]
      [else (die 'CODErri "invalid i" i)])))

(define CODErr
  (lambda (c r1 r2 ac)
    (CODE c (ModRM 3 r1 r2 ac))))

(define RegReg
  (lambda (r1 r2 r3 ac)
    (cond
      [(eq? r3 '%esp) (die 'assembler "BUG: invalid src %esp")]
      [(eq? r1 '%ebp) (die 'assembler "BUG: invalid src %ebp")]
      [else
       (cons*
         (byte (fxlogor 4 (fxsll (register-index r1) 3)))
         (byte (fxlogor (register-index r2)
                        (fxsll (register-index r3) 3)))
         ac)])))

(define IMM*2
  (lambda (i1 i2 ac)
    (cond
      [(and (int? i1) (obj? i2))
       (let ([d i1] [v (cadr i2)])
         (cons (reloc-word+ v d) ac))]
      [(and (int? i2) (obj? i1)) (IMM*2 i2 i1 ac)]
      [(and (int? i1) (int? i2))
       (IMM (bitwise-and (+ i1 i2)
              (- (expt 2 (* wordsize 8)) 1))
            ac)]
      [else (die 'assemble "invalid IMM*2" i1 i2)])))

(define (SIB s i b ac)
  (cons (byte
          (fxlogor
            (register-index b)
            (fxlogor
              (fxsll (register-index i) 3)
              (fxsll s 6))))
        ac))

(define (imm32? x)
  (case wordsize
    [(4) (imm? x)]
    [(8)
     (and (integer? x)
          (<= (- (expt 2 31)) x (- (expt 2 31) 1)))]
    [else (error 'imm32? "invalid wordsize" wordsize)]))

(define *cogen* (gensym "*cogen*"))

(define-syntax add-instruction
  (syntax-rules ()
    [(_ (name instr ac args ...) b b* ...)
     (putprop 'name *cogen*
       (cons (length '(args ...))
             (lambda (instr ac args ...) b b* ...)))]))

(define-syntax add-instructions
  (syntax-rules ()
    [(_ instr ac [(name* arg** ...) b* b** ...] ...)
     (begin
       (add-instruction (name* instr ac arg** ...) b* b** ...) ...)]))


(define (convert-instruction a ac)
  (cond
    [(getprop (car a) *cogen*) =>
     (lambda (p)
       (let ([n (car p)] [proc (cdr p)] [args (cdr a)])
         (cond
           [(fx= n 2)
            (if (fx= (length args) 2)
                (proc a ac (car args) (cadr args))
                (die 'convert-instruction "incorrect args" a))]
           [(fx= n 1)
            (if (fx= (length args) 1)
                (proc a ac (car args))
                (die 'convert-instruction "incorrect args" a))]
           [(fx= n 0)
            (if (fx= (length args) 0)
                (proc a ac)
                (die 'convert-instruction "incorrect args" a))]
           [else
            (if (fx= (length args) n)
                (apply proc a ac args)
                (die 'convert-instruction "incorrect args" a))])))]
    [(eq? (car a) 'seq)
     (fold convert-instruction ac (cdr a))]
    [(eq? (car a) 'pad)
     (let ()
       (define (find-prefix x ls)
         (let f ([ls ls])
           (cond
             [(eq? ls x) '()]
             [else
              (let ([a (car ls)])
                (if (and (pair? a) (eq? (car a) 'bottom-code))
                    (f (cdr ls))
                    (cons a (f (cdr ls)))))])))
       (let ([n (cadr a)] [code (cddr a)])
         (let ([ls (fold convert-instruction ac code)])
           (let ([m (compute-code-size (find-prefix ac ls))])
             (append (make-list (- n m) 0) ls)))))]
    [else (die 'convert-instruction "unknown instruction" a)]))

(define (RM /d dst ac)
  (cond
    [(mem? dst)
     (with-args dst
        (lambda (a0 a1)
          (cond
            [(and (imm8? a0) (reg32? a1))
             (ModRM 1 /d a1 (IMM8 a0 ac))]
            [(and (imm? a0) (reg32? a1))
             (ModRM 2 /d a1 (IMM32 a0 ac))]
            [(and (imm8? a1) (reg32? a0))
             (ModRM 1 /d a0 (IMM8 a1 ac))]
            [(and (imm? a1) (reg32? a0))
             (ModRM 2 /d a0 (IMM32 a1 ac))]
            [(and (reg32? a0) (reg32? a1))
             (RegReg /d a0 a1 ac)]
            [(and (imm? a0) (imm? a1))
             (ModRM 0 /d '/5 (IMM*2 a0 a1 ac))]
            [else (die 'RM "unhandled" a0 a1)])))]
    [(reg? dst) (ModRM 3 /d dst ac)]
    [else (die 'RM "unhandled" dst)]))

(module ()
(define who 'assembler)


(define (REX.R bits ac)
  (when (eqv? wordsize 4)
    (error 'ikarus "BUG: REX.R invalid in 32-bit mode"))
  (cons (fxlogor #b01001000 bits) ac))

(define (REX+r r ac)
  (cond
    [(eqv? wordsize 4) ac]
    [(reg-requires-REX? r) (REX.R #b001 ac)]
    [else                  (REX.R #b000 ac)]))

(define (REX+RM r rm ac)
  (define (C n ac)
    ac)
    ;;;(printf "CASE ~s\n" n)
    ;;;(let f ([ac ac] [i 30])
    ;;;  (unless (or (null? ac) (= i 0))
    ;;;    (if (number? (car ac))
    ;;;        (printf " #x~x" (car ac))
    ;;;        (printf " ~s" (car ac)))
    ;;;    (f (cdr ac) (- i 1))))
    ;;;(newline)
    ;;;ac)
  (cond
    [(eqv? wordsize 4) ac]
    [(mem? rm)
     (if (reg-requires-REX? r)
         (with-args rm
            (lambda (a0 a1)
              (cond
                [(and (imm? a0) (reg32? a1))
                 (if (reg-requires-REX? a1)
                     (REX.R #b101 ac)
                     (REX.R #b100 ac))]
                [(and (imm? a1) (reg32? a0))
                 (if (reg-requires-REX? a0)
                     (REX.R #b101 ac)
                     (REX.R #b100 ac))]
                [(and (reg32? a0) (reg32? a1))
                 (if (reg-requires-REX? a0)
                     (if (reg-requires-REX? a1)
                         (REX.R #b111 ac)
                         (REX.R #b110 ac))
                     (if (reg-requires-REX? a1)
                         (REX.R #b101 ac)
                         (REX.R #b100 ac)))]
                [(and (imm? a0) (imm? a1))
                 (error 'REC+RM "not here 4")
                 (error 'REX+RM "unhandledb" a1)]
                [else (die 'REX+RM "unhandled" a0 a1)])))
         (with-args rm
            (lambda (a0 a1)
              (cond
                [(and (imm? a0) (reg32? a1))
                 (if (reg-requires-REX? a1)
                     (REX.R #b001 ac)
                     (REX.R 0 ac))]
                [(and (imm? a1) (reg32? a0))
                 (if (reg-requires-REX? a0)
                     (REX.R #b001 ac)
                     (REX.R 0 ac))]
                [(and (reg32? a0) (reg32? a1))
                 (if (reg-requires-REX? a0)
                     (if (reg-requires-REX? a1)
                         (error 'REX+RM "unhandled x1" a0 a1)
                         (REX.R #b010 ac))
                     (if (reg-requires-REX? a1)
                         (error 'REX+RM "unhandled x3" a0 a1)
                         (REX.R 0 ac)))]
                [(and (imm? a0) (imm? a1))
                 ;(error 'REC+RM "not here 8")
                 (REX.R 0 ac)]
                [else (die 'REX+RM "unhandled" a0 a1)]))))]
    [(reg? rm)
     (let* ([bits 0]
            [bits
             (if (reg-requires-REX? r)
                 (fxlogor bits #b100)
                 bits)]
            [bits
             (if (reg-requires-REX? rm)
                 (fxlogor bits #b001)
                 bits)])
       (REX.R bits ac))]
    [else (die 'REX+RM "unhandled" rm)]))

(define (C c ac)
  (case wordsize
    [(4) (CODE c ac)]
    [else (REX.R 0 (CODE c ac))]))

;;;Commented out because it is not used (Marco Maggi; Oct 25, 2011).
;;
;; (define trace-ac
;;   (let ((cache '()))
;;     (lambda (ac1 what ac2)
;;       (when (assembler-output)
;;         (let ((diff (let f ((ls ac2))
;; 		      (cond ((eq? ls ac1)
;; 			     '())
;; 			    (else
;; 			     (cons (car ls) (f (cdr ls))))))))
;;           (unless (member diff cache)
;;             (set! cache (cons diff cache))
;;             (printf "~s => ~s\n" what diff))))
;;       ac2)))

(define (CR c r ac)
  (REX+r r (CODE+r c r ac)))
(define (CR* c r rm ac)
  (REX+RM r rm (CODE c (RM r rm ac))))
(define (CR*-no-rex c r rm ac)
  (CODE c (RM r rm ac)))
(define (CCR* c0 c1 r rm ac)
  ;(CODE c0 (CODE c1 (RM r rm ac))))
  (REX+RM r rm (CODE c0 (CODE c1 (RM r rm ac)))))
(define (CCR c0 c1 r ac)
  ;(CODE c0 (CODE+r c1 r ac)))
  (REX+r r (CODE c0 (CODE+r c1 r ac))))
(define (CCCR* c0 c1 c2 r rm ac)
  ;(CODE c0 (CODE c1 (CODE c2 (RM r rm ac)))))
  (REX+RM r rm (CODE c0 (CODE c1 (CODE c2 (RM r rm ac))))))


(define (CCI32 c0 c1 i32 ac)
  (CODE c0 (CODE c1 (IMM32 i32 ac))))


(define (dotrace instr orig ls)
  (printf "TRACE: ~s ~s\n" instr
    (let f ([ls ls])
      (if (eq? ls orig)
          '()
          (cons (car ls) (f (cdr ls))))))
  ls)

(define (jmp-pc-relative code0 code1 dst ac)
  (when (= wordsize 4)
    (error 'intel-assembler "no pc-relative jumps in 32-bit mode"))
  (let ([g (gensym)])
    (CODE code0
      (CODE code1
        (cons*
          `(local-relative . ,g)
          `(bottom-code
             (label . ,g)
             (label-addr . ,(label-name dst)))
          ac)))))

(add-instructions instr ac
   [(ret)                                 (CODE #xC3 ac)]
   [(cltd)                                (C #x99 ac)]
   [(movl src dst)
    (cond
      [(and (imm? src) (reg? dst))      (CR #xB8 dst (IMM src ac))]
      [(and (imm? src) (mem? dst))      (CR* #xC7 '/0 dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))      (CR* #x89 src dst ac)]
      [(and (reg? src) (mem? dst))      (CR* #x89 src dst ac)]
      [(and (mem? src) (reg? dst))      (CR* #x8B dst src ac)]
      [else (die who "invalid" instr)])]
   [(mov32 src dst)
    ;;; FIXME
    (cond
      [(and (imm? src) (reg? dst))
       (error 'mov32 "here1")
       (CR #xB8 dst (IMM32 src ac))]
      [(and (imm? src) (mem? dst))   (CR*-no-rex #xC7 '/0 dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))
       (error 'mov32 "here3")
       (CR* #x89 src dst ac)]
      [(and (reg? src) (mem? dst))   (CR*-no-rex #x89 src dst ac)]
      [(and (mem? src) (reg? dst))
       (if (= wordsize 4)
           (CR* #x8B dst src ac)
           (CR*-no-rex #x8B dst src ac))]
      [else (die who "invalid" instr)])]
   [(movb src dst)
    (cond
      [(and (imm8? src) (mem? dst))       (CR* #xC6 '/0 dst (IMM8 src ac))]
      [(and (reg8? src) (mem? dst))       (CR* #x88 src dst ac)]
      [(and (mem? src) (reg8? dst))       (CR* #x8A dst src ac)]
      [else (die who "invalid" instr)])]
   [(addl src dst)
    (cond
      [(and (imm8? src) (reg? dst))     (CR*  #x83 '/0 dst (IMM8 src ac))]
      [(and (imm32? src) (eq? dst '%eax))   (C #x05 (IMM32 src ac))]
      [(and (imm32? src) (reg? dst))      (CR*  #x81 '/0 dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))      (CR*  #x01 src dst ac)]
      [(and (mem? src) (reg? dst))      (CR*  #x03 dst src ac)]
      [(and (imm32? src) (mem? dst))        (CR*  #x81 '/0 dst (IMM32 src ac))]
      [(and (reg? src) (mem? dst))      (CR*  #x01 src dst ac)]
      [else (die who "invalid" instr)])]
   [(subl src dst)
    (cond
      [(and (imm8? src) (reg? dst))     (CR*  #x83 '/5 dst (IMM8 src ac))]
      [(and (imm32? src) (eq? dst '%eax))   (C #x2D (IMM32 src ac))]
      [(and (imm32? src) (reg? dst))      (CR*  #x81 '/5 dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))    (CR*  #x29 src dst ac)]
      [(and (mem? src) (reg? dst))      (CR*  #x2B dst src ac)]
      [(and (imm32? src) (mem? dst))        (CR*  #x81 '/5 dst (IMM32 src ac))]
      [(and (reg? src) (mem? dst))      (CR*  #x29 src dst ac)]
      [else (die who "invalid" instr)])]
   [(sall src dst)
    (cond
      [(and (equal? 1 src) (reg? dst))  (CR* #xD1 '/4 dst ac)]
      [(and (imm8? src) (reg? dst))     (CR* #xC1 '/4 dst (IMM8 src ac))]
      [(and (imm8? src) (mem? dst))       (CR* #xC1 '/4 dst (IMM8 src ac))]
      [(and (eq? src '%cl) (reg? dst))  (CR* #xD3 '/4 dst ac)]
      [(and (eq? src '%cl) (mem? dst))    (CR* #xD3 '/4 dst ac)]
      [else (die who "invalid" instr)])]
   [(shrl src dst)
    (cond
      [(and (equal? 1 src) (reg? dst))  (CR* #xD1 '/5 dst ac)]
      [(and (imm8? src) (reg? dst))     (CR* #xC1 '/5 dst (IMM8 src ac))]
      [(and (eq? src '%cl) (reg? dst))  (CR* #xD3 '/5 dst ac)]
      [(and (imm8? src) (mem? dst))       (CR* #xC1 '/5 dst (IMM8 src ac))]
      [(and (eq? src '%cl) (mem? dst))    (CR* #xD3 '/5 dst ac)]
      [else (die who "invalid" instr)])]
   [(sarl src dst)
    (cond
      [(and (equal? 1 src) (reg? dst))  (CR* #xD1 '/7 dst ac)]
      [(and (imm8? src) (reg? dst))     (CR* #xC1 '/7 dst (IMM8 src ac))]
      [(and (imm8? src) (mem? dst))     (CR* #xC1 '/7 dst (IMM8 src ac))]
      [(and (eq? src '%cl) (reg? dst))  (CR* #xD3 '/7 dst ac)]
      [(and (eq? src '%cl) (mem? dst))  (CR* #xD3 '/7 dst ac)]
      [else (die who "invalid" instr)])]
   [(andl src dst)
    (cond
      [(and (imm32? src) (mem? dst))       (CR*  #x81 '/4 dst (IMM32 src ac))]
      [(and (imm8? src)  (reg? dst))     (CR*  #x83 '/4 dst (IMM8 src ac))]
      [(and (imm32? src) (eq? dst '%eax))  (C #x25 (IMM32 src ac))]
      [(and (imm32? src) (reg? dst))     (CR*  #x81 '/4 dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))     (CR*  #x21 src dst ac)]
      [(and (reg? src) (mem? dst))       (CR*  #x21 src dst ac)]
      [(and (mem? src)   (reg? dst))     (CR*  #x23 dst src ac)]
      [else (die who "invalid" instr)])]
   [(orl src dst)
    (cond
      [(and (imm32? src) (mem? dst))        (CR*  #x81 '/1 dst (IMM32 src ac))]
      [(and (reg? src) (mem? dst))      (CR*  #x09 src dst ac)]
      [(and (imm8? src) (reg? dst))     (CR*  #x83 '/1 dst (IMM8 src ac))]
      [(and (imm32? src) (eq? dst '%eax))   (C #x0D (IMM32 src ac))]
      [(and (imm32? src) (reg? dst))      (CR*  #x81 '/1 dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))    (CR*  #x09 src dst ac)]
      [(and (mem? src) (reg? dst))      (CR*  #x0B dst src ac)]
      [else (die who "invalid" instr)])]
   [(xorl src dst)
    (cond
      [(and (imm8? src) (reg? dst))       (CR*  #x83 '/6 dst (IMM8 src ac))]
      [(and (imm8? src) (mem? dst))       (CR*  #x83 '/6 dst (IMM8 src ac))]
      [(and (imm32? src) (eq? dst '%eax)) (C #x35 (IMM32 src ac))]
      [(and (reg? src) (reg? dst))        (CR*  #x31 src dst ac)]
      [(and (mem? src) (reg? dst))        (CR*  #x33 dst src ac)]
      [(and (reg? src) (mem? dst))        (CR*  #x31 src dst ac)]
      [else (die who "invalid" instr)])]
   [(leal src dst)
    (cond
      [(and (mem? src) (reg? dst))      (CR* #x8D dst src ac)]
      [else (die who "invalid" instr)])]
   [(cmpl src dst)
    (cond
      [(and (imm8? src) (reg? dst))       (CR*  #x83 '/7 dst (IMM8 src ac))]
      [(and (imm32? src) (eq? dst '%eax)) (C #x3D (IMM32 src ac))]
      [(and (imm32? src) (reg? dst))      (CR*  #x81 '/7 dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))        (CR*  #x39 src dst ac)]
      [(and (mem? src) (reg? dst))        (CR*  #x3B dst src ac)]
      [(and (imm8? src) (mem? dst))       (CR*  #x83 '/7 dst (IMM8 src ac))]
      [(and (imm32? src) (mem? dst))      (CR*  #x81 '/7 dst (IMM32 src ac))]
      [else (die who "invalid" instr)])]
   [(imull src dst)
    (cond
      [(and (imm8? src) (reg? dst))     (CR*  #x6B dst dst (IMM8 src ac))]
      [(and (imm32? src) (reg? dst))    (CR*  #x69 dst dst (IMM32 src ac))]
      [(and (reg? src) (reg? dst))      (CCR* #x0F #xAF dst src ac)]
      [(and (mem? src) (reg? dst))      (CCR* #x0F #xAF dst src ac)]
      [else (die who "invalid" instr)])]
   [(idivl dst)
    (cond
      [(reg? dst)                       (CR* #xF7 '/7 dst ac)]
      [(mem? dst)                       (CR* #xF7 '/7 dst ac)]
      [else (die who "invalid" instr)])]
   [(pushl dst)
    (cond
      [(imm8? dst)                        (CODE #x6A (IMM8 dst ac))]
      [(imm32? dst)                         (CODE #x68 (IMM32 dst ac))]
      [(reg? dst)                       (CR   #x50 dst ac)]
      [(mem? dst)                         (CR*  #xFF '/6 dst ac)]
      [else (die who "invalid" instr)])]
   [(popl dst)
    (cond
      [(reg? dst)                      (CR  #x58 dst ac)]
      [(mem? dst)                        (CR* #x8F '/0 dst ac)]
      [else (die who "invalid" instr)])]
   [(notl dst)
    (cond
      [(reg? dst)                     (CR* #xF7 '/2 dst ac)]
      [(mem? dst)                     (CR* #xF7 '/7 dst ac)]
      [else (die who "invalid" instr)])]
   [(bswap dst)
    (cond
      [(reg? dst)                     (CCR #x0F #xC8 dst ac)]
      [else (die who "invalid" instr)])]
   [(negl dst)
    (cond
      [(reg? dst)                     (CR* #xF7 '/3 dst ac)]
      [else (die who "invalid" instr)])]
   [(jmp dst)
    (cond
      [(and (label? dst) (local-label? (label-name dst)))
       (CODE #xE9 (cons `(local-relative . ,(label-name dst)) ac))]
      [(imm? dst)
       (if (= wordsize 4)
           (CODE #xE9 (IMM32 dst ac))
           (jmp-pc-relative #xFF #x25 dst ac))]
      [(mem? dst)                     (CR*  #xFF '/4 dst ac)]
      [else (die who "invalid jmp target" dst)])]
   [(call dst)
    (cond
      [(and (label? dst) (local-label? (label-name dst)))
       (CODE #xE8 (cons `(local-relative . ,(label-name dst)) ac))]
      [(imm? dst)
       (if (= wordsize 4)
           (CODE #xE8 (IMM32 dst ac))
           (jmp-pc-relative #xFF #x15 dst ac))]
      [(mem? dst)                     (CR* #xFF '/2 dst ac)]
      [(reg? dst)                     (CR* #xFF '/2 dst ac)]
      [else (die who "invalid jmp target" dst)])]
   [(movsd src dst)
    (cond
      [(and (xmmreg? dst) (mem? src)) (CCCR* #xF2 #x0F #x10 dst src ac)]
      [(and (xmmreg? src) (mem? dst)) (CCCR* #xF2 #x0F #x11 src dst ac)]
      [else (die who "invalid" instr)])]
   [(cvtsi2sd src dst)
    (cond
      [(and (xmmreg? dst) (reg? src)) (CCCR* #xF2 #x0F #x2A src dst ac)]
      [(and (xmmreg? dst) (mem? src)) (CCCR* #xF2 #x0F #x2A dst src ac)]
      [else (die who "invalid" instr)])]
   [(cvtsd2ss src dst)
    (cond
      [(and (xmmreg? dst) (xmmreg? src)) (CCCR* #xF2 #x0F #x5A src dst ac)]
      [else (die who "invalid" instr)])]
   [(cvtss2sd src dst)
    (cond
      [(and (xmmreg? dst) (xmmreg? src)) (CCCR* #xF3 #x0F #x5A src dst ac)]
      [else (die who "invalid" instr)])]
   [(movss src dst)
    (cond
      [(and (xmmreg? dst) (mem? src)) (CCCR* #xF3 #x0F #x10 dst src ac)]
      [(and (xmmreg? src) (mem? dst)) (CCCR* #xF3 #x0F #x11 src dst ac)]
      [else (die who "invalid" instr)])]
   [(addsd src dst)
    (cond
      [(and (xmmreg? dst) (mem? src)) (CCCR* #xF2 #x0F #x58 dst src ac)]
      [else (die who "invalid" instr)])]
   [(subsd src dst)
    (cond
      [(and (xmmreg? dst) (mem? src)) (CCCR* #xF2 #x0F #x5C dst src ac)]
      [else (die who "invalid" instr)])]
   [(mulsd src dst)
    (cond
      [(and (xmmreg? dst) (mem? src)) (CCCR* #xF2 #x0F #x59 dst src ac)]
      [else (die who "invalid" instr)])]
   [(divsd src dst)
    (cond
      [(and (xmmreg? dst) (mem? src)) (CCCR* #xF2 #x0F #x5E dst src ac)]
      [else (die who "invalid" instr)])]
   [(ucomisd src dst)
    (cond
      [(and (xmmreg? dst) (mem? src)) (CCCR* #x66 #x0F #x2E dst src ac)]
      [else (die who "invalid" instr)])]
   [(ja dst)     (CCI32 #x0F #x87 dst ac)]
   [(jae dst)    (CCI32 #x0F #x83 dst ac)]
   [(jb dst)     (CCI32 #x0F #x82 dst ac)]
   [(jbe dst)    (CCI32 #x0F #x86 dst ac)]
   [(jg dst)     (CCI32 #x0F #x8F dst ac)]
   [(jge dst)    (CCI32 #x0F #x8D dst ac)]
   [(jl dst)     (CCI32 #x0F #x8C dst ac)]
   [(jle dst)    (CCI32 #x0F #x8E dst ac)]
   [(je dst)     (CCI32 #x0F #x84 dst ac)]
   [(jna dst)    (CCI32 #x0F #x86 dst ac)]
   [(jnae dst)   (CCI32 #x0F #x82 dst ac)]
   [(jnb dst)    (CCI32 #x0F #x83 dst ac)]
   [(jnbe dst)   (CCI32 #x0F #x87 dst ac)]
   [(jng dst)    (CCI32 #x0F #x8E dst ac)]
   [(jnge dst)   (CCI32 #x0F #x8C dst ac)]
   [(jnl dst)    (CCI32 #x0F #x8D dst ac)]
   [(jnle dst)   (CCI32 #x0F #x8F dst ac)]
   [(jne dst)    (CCI32 #x0F #x85 dst ac)]
   [(jo dst)     (CCI32 #x0F #x80 dst ac)]
   [(jp dst)     (CCI32 #x0F #x8A dst ac)]
   [(jnp dst)    (CCI32 #x0F #x8B dst ac)]
   [(byte x)
    (unless (byte? x) (die who "not a byte" x))
    (cons (byte x) ac)]
   [(byte-vector x) (append (map (lambda (x) (byte x)) (vector->list x)) ac)]
   [(int a) (IMM a ac)]
   [(label L)
    (unless (symbol? L) (die who "label is not a symbol" L))
    (cons (cons 'label L) ac)]
   [(label-address L)
    (unless (symbol? L) (die who "label-address is not a symbol" L))
    (cons (cons 'label-addr L) ac)]
   [(current-frame-offset)
    (cons '(current-frame-offset) ac)]
   [(nop) ac]

))


(define compute-code-size
  (lambda (ls)
    (fold (lambda (x ac)
            (if (fixnum? x)
                (fx+ ac 1)
                (case (car x)
                  [(byte) (fx+ ac 1)]
                  [(relative local-relative)
                   (fx+ ac 4)]
                  [(label) ac]
                  [(word reloc-word reloc-word+ label-addr
                    current-frame-offset foreign-label)
                   (fx+ ac wordsize)]
                  [(bottom-code)
                   (fx+ ac (compute-code-size (cdr x)))]
                  [else (die 'compute-code-size "unknown instr" x)])))
          0
          ls)))

(define set-label-loc!
  (lambda (x loc)
    (when (getprop x '*label-loc*)
      (die 'compile "label is already defined" x))
    (putprop x '*label-loc* loc)))

(define label-loc
  (lambda (x)
    (or (getprop x '*label-loc*)
        (die 'compile "undefined label" x))))

(define unset-label-loc!
  (lambda (x)
    (remprop x '*label-loc*)))


(define set-code-word!
  (lambda (code idx x)
    (cond
      [(fixnum? x)
       (case wordsize
         [(4)
          (code-set! code (fx+ idx 0) (fxsll (fxlogand x #x3F) 2))
          (code-set! code (fx+ idx 1) (fxlogand (fxsra x 6) #xFF))
          (code-set! code (fx+ idx 2) (fxlogand (fxsra x 14) #xFF))
          (code-set! code (fx+ idx 3) (fxlogand (fxsra x 22) #xFF))]
         [else
          (code-set! code (fx+ idx 0) (fxsll (fxlogand x #x1F) 3))
          (code-set! code (fx+ idx 1) (fxlogand (fxsra x 5) #xFF))
          (code-set! code (fx+ idx 2) (fxlogand (fxsra x 13) #xFF))
          (code-set! code (fx+ idx 3) (fxlogand (fxsra x 21) #xFF))
          (code-set! code (fx+ idx 4) (fxlogand (fxsra x 29) #xFF))
          (code-set! code (fx+ idx 5) (fxlogand (fxsra x 37) #xFF))
          (code-set! code (fx+ idx 6) (fxlogand (fxsra x 45) #xFF))
          (code-set! code (fx+ idx 7) (fxlogand (fxsra x 53) #xFF))])]
      [else (die 'set-code-word! "unhandled" x)])))

(define local-labels (make-parameter '()))
(define (local-label? x) (and (memq x (local-labels)) #t))

(define (uncover-local-labels ls)
  (define locals '())
  (define find
    (lambda (x)
      (when (pair? x)
        (case (car x)
          [(label)
           (set! locals (cons (label-name x) locals))]
          [(seq pad)
           (for-each find (cdr x))]))))
  (for-each find ls)
  locals)

(define (optimize-local-jumps ls)
  (define locals '())
  (define g (gensym))
  (define (mark x)
    (when (pair? x)
      (case (car x)
        [(label)
         (putprop (cdr x) g 'local)
         (set! locals (cons (cdr x) locals))]
        [(bottom-code) (for-each mark (cdr x))])))
  (define (opt x)
    (when (pair? x)
      (case (car x)
        [(relative)
         (when (eq? (getprop (cdr x) g) 'local)
           (set-car! x 'local-relative))]
        [(bottom-code) (for-each opt (cdr x))])))
  (for-each mark ls)
  (for-each opt ls)
  (for-each (lambda (x) (remprop x g)) locals)
  ls)



(define whack-instructions
  (lambda (x ls)
    (define f
      (lambda (ls idx reloc bot*)
        (cond
          [(null? ls)
           (if (null? bot*)
               reloc
               (f (car bot*) idx reloc (cdr bot*)))]
          [else
           (let ([a (car ls)])
             (if (fixnum? a)
                 (begin
                   (code-set! x idx a)
                   (f (cdr ls) (fxadd1 idx) reloc bot*))
                 (case (car a)
                  [(byte)
                   (code-set! x idx (cdr a))
                   (f (cdr ls) (fx+ idx 1) reloc bot*)]
                  [(relative local-relative)
                   (f (cdr ls) (fx+ idx 4) (cons (cons idx a) reloc) bot*)]
                  [(reloc-word reloc-word+ label-addr foreign-label)
                   (f (cdr ls) (fx+ idx wordsize) (cons (cons idx a) reloc) bot*)]
                  [(word)
                   (let ([v (cdr a)])
                      (set-code-word! x idx v)
                      (f (cdr ls) (fx+ idx wordsize) reloc bot*))]
                  [(current-frame-offset)
                   (set-code-word! x idx idx) ;;; FIXME 64bit
                   (f (cdr ls) (fx+ idx wordsize) reloc bot*)]
                  [(label)
                   (set-label-loc! (cdr a) (list x idx))
                   (f (cdr ls) idx reloc bot*)]
                  [(bottom-code)
                   (f (cdr ls) idx reloc (cons (cdr a) bot*))]
                  [else
                   (die 'whack-instructions "unknown instr" a)])))])))
    (f ls 0 '() '())))

(define compute-reloc-size
  (lambda (ls)
    (fold (lambda (x ac)
            (if (fixnum? x)
                ac
                (case (car x)
                  [(word byte label current-frame-offset local-relative) ac]
                  [(reloc-word foreign-label)        (fx+ ac 2)]
                  [(relative reloc-word+ label-addr) (fx+ ac 3)]
                  [(bottom-code) (fx+ ac (compute-reloc-size (cdr x)))]
                  [else (die 'compute-reloc-size "unknown instr" x)])))
          0
          ls)))

(define foreign-string->bytevector
  (let ([mem '()])
    (lambda (x)
      (let f ([ls mem])
        (cond
          [(null? ls)
           (let ([bv (string->utf8 x)])
             (set! mem (cons (cons x bv) mem))
             bv)]
          [(string=? x (caar ls)) (cdar ls)]
          [else (f (cdr ls))])))))


(define code-entry-adjustment
  (let ([v #f])
    (case-lambda
      [() (or v (die 'code-entry-adjustment "uninitialized"))]
      [(x) (set! v x)])))

(define whack-reloc
  (lambda (thunk?-label code vec)
    (define reloc-idx 0)
    (lambda (r)
      (let ([idx (car r)] [type (cadr r)]
            [v
             (let ([v (cddr r)])
               (cond
                 [(thunk?-label v) =>
                  (lambda (label)
                    (let ([p (label-loc label)])
                      (cond
                        [(fx= (length p) 2)
                         (let ([code (car p)] [idx (cadr p)])
                           (unless (fx= idx 0)
                             (die 'whack-reloc
                               "cannot create a thunk pointing"
                               idx))
                           (let ([thunk (code->thunk code)])
                             (set-cdr! (cdr p) (list thunk))
                             thunk))]
                        [else (caddr p)])))]
                 [else v]))])
        (case type
          [(reloc-word)
           (vector-set! vec reloc-idx (fxsll idx 2))
           (vector-set! vec (fx+ reloc-idx 1) v)
           (set! reloc-idx (fx+ reloc-idx 2))]
          [(foreign-label)
           ;;; FIXME: converted strings should be memoized.
           ;;;        wait for equal? hash tables.
           (let ([name
                  (if (string? v)
                      (foreign-string->bytevector v)
                      (die 'whack-reloc "not a string" v))])
             (vector-set! vec reloc-idx (fxlogor 1 (fxsll idx 2)))
             (vector-set! vec (fx+ reloc-idx 1) name)
             (set! reloc-idx (fx+ reloc-idx 2)))]
          [(reloc-word+)
           (let ([obj (car v)] [disp (cdr v)])
             (vector-set! vec reloc-idx (fxlogor 2 (fxsll idx 2)))
             (vector-set! vec (fx+ reloc-idx 1) disp)
             (vector-set! vec (fx+ reloc-idx 2) obj)
             (set! reloc-idx (fx+ reloc-idx 3)))]
          [(label-addr)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (vector-set! vec reloc-idx (fxlogor 2 (fxsll idx 2)))
               (vector-set! vec (fx+ reloc-idx 1)
                  (fx+ disp (code-entry-adjustment)))
               (vector-set! vec (fx+ reloc-idx 2) obj)))
           (set! reloc-idx (fx+ reloc-idx 3))]
          [(local-relative)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (unless (eq? obj code)
                 (die 'whack-reloc "local-relative differ"))
               (let ([rel (fx- disp (fx+ idx 4))])
                 (code-set! code (fx+ idx 0) (fxlogand rel #xFF))
                 (code-set! code (fx+ idx 1) (fxlogand (fxsra rel 8) #xFF))
                 (code-set! code (fx+ idx 2) (fxlogand (fxsra rel 16) #xFF))
                 (code-set! code (fx+ idx 3) (fxlogand (fxsra rel 24) #xFF)))))]
          [(relative)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (unless (and (code? obj) (fixnum? disp))
                 (die 'whack-reloc "invalid relative jump obj/disp" obj disp))
               (vector-set! vec reloc-idx (fxlogor 3 (fxsll idx 2)))
               (vector-set! vec (fx+ reloc-idx 1)
                 (fx+ disp (code-entry-adjustment)))
               (vector-set! vec (fx+ reloc-idx 2) obj)))
           (set! reloc-idx (fx+ reloc-idx 3))]
          [else (die 'whack-reloc "invalid reloc type" type)]))
      )))



  (define assemble-sources
    (lambda (thunk?-label ls*)
      (define (code-list ls)
        (if (let ([a (cadr ls)])
              (and (pair? a) (eq? (car a) 'name)))
            (cddr ls)
            (cdr ls)))
      (define (code-name ls)
        (let ([a (cadr ls)])
           (if (and (pair? a) (eq? (car a) 'name))
               (cadr a)
               #f)))
      (let ([closure-size* (map car ls*)]
            [code-name* (map code-name ls*)]
            [ls* (map code-list ls*)])
        (let* ([ls* (map convert-instructions ls*)]
               [ls* (map optimize-local-jumps ls*)])
          (let ([n* (map compute-code-size ls*)]
                [m* (map compute-reloc-size ls*)])
            (let ([code* (map make-code n* closure-size*)]
                  [relv* (map make-vector m*)])
              (let ([reloc** (map whack-instructions code* ls*)])
                (for-each
                  (lambda (foo reloc*)
                    (for-each (whack-reloc thunk?-label (car foo) (cdr foo)) reloc*))
                  (map cons code* relv*) reloc**)
                (for-each set-code-reloc-vector! code* relv*)
                (for-each (lambda (code name)
                            (when name
                              (set-code-annotation! code name)))
                          code* code-name*)
                code*)))))))


;;;; done

)

;;; end of file
