;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.

;;;The following constants  definitions must be kept in sync  with the definitions in
;;;the C language header files "internals.h" and "vicare.h".

(define-constant wordshift
  (boot.case-word-size
   ((32) 2)
   ((64) 3)))

;;Whenever we allocate a memory block on the nursery heap to hold a new non-immediate
;;Scheme object: we allocate  a block whose size is twice  the target platform's word
;;size.
;;
;;
(define-constant object-alignment	(* 2 wordsize))
(define-constant align-shift		(+ wordshift 1))
;;(define-constant pagesize		4096)
(define-constant pageshift		12)

(define (align n)
  (fxsll (fxsra (fx+ n (fxsub1 object-alignment))
		align-shift)
	 align-shift))


;;;; built in Scheme values

(define-constant NULL-OBJECT		#x4F)	;the constant ()
(define-constant EOF-OBJECT		#x5F)	;the constant (eof-object)
(define-constant UNBOUND-OBJECT		#x6F)	;the constant (unbound-object)
(define-constant VOID-OBJECT		#x7F)	;the constant (void)
(define-constant BWP-OBJECT		#x8F)	;the constant (bwp-object)


;;;; boolean objects

(define-constant BOOL-FALSE-OBJECT	#x2F)	;the constant #f
(define-constant BOOL-TRUE-OBJECT	#x3F)	;the constant #t
(define-constant bool-mask		#xEF)
(define-constant bool-tag		#x2F)
;;Commented out because unused.
;;
;;(define-constant bool-shift		4)


;;;; vectors

(define-constant vector-tag		5)
(define-constant vector-mask		7)

(define-constant disp-vector-length	0)
(define-constant disp-vector-data	wordsize)

(define-constant off-vector-length	(fx- disp-vector-length vector-tag))
(define-constant off-vector-data	(fx- disp-vector-data   vector-tag))


;;;; fixnums

(define-constant fx-scale		wordsize)
(define-constant fx-shift		wordshift)
(define-constant fx-mask		(- wordsize 1))
(define-constant fx-tag			0)


;;;; bignums

(define-constant bignum-mask		#b111)
(define-constant bignum-tag		#b011)

;;Bit mask to be AND-ed to the  first word in a bignum's memory block to
;;isolate the sign bit.
(define-constant bignum-sign-mask	#b1000)

;;Given the  first word in  a bignum's memory  block, with the  sign bit
;;already isolated: we have to right-shift it by this amount to get 0 if
;;positive and 1 if negative.
(define-constant bignum-sign-shift	3)

;;Given  the  first  word  in  a  bignum's  memory  block:  we  have  to
;;right-shift it by  this amount to get a machine  word (*not* a fixnum)
;;representing the  number of limbs  in the data  area.  Each limb  is a
;;machine word.
(define-constant bignum-length-shift	4)

(define-constant disp-bignum-tag	0)
(define-constant disp-bignum-data	wordsize)

(define-constant off-bignum-tag		(fx- disp-bignum-tag  vector-tag))
(define-constant off-bignum-data	(fx- disp-bignum-data vector-tag))


;;;; flonums

(define-constant flonum-tag		#x17)
(define-constant flonum-size		16)

(define-constant disp-flonum-tag	0)
(define-constant disp-flonum-data	8)

(define-constant off-flonum-tag		(fx- disp-flonum-tag  vector-tag))
(define-constant off-flonum-data	(fx- disp-flonum-data vector-tag))


;;;; ratnums

(define-constant ratnum-tag		#x27)
(define-constant disp-ratnum-tag	0)
(define-constant disp-ratnum-num	(fx* 1 wordsize))
(define-constant disp-ratnum-den	(fx* 2 wordsize))
(define-constant ratnum-size		(fx* 4 wordsize))

(define-constant off-ratnum-tag		(fx- disp-ratnum-tag vector-tag))
(define-constant off-ratnum-num		(fx- disp-ratnum-num vector-tag))
(define-constant off-ratnum-den		(fx- disp-ratnum-den vector-tag))


;;;; compnums

(define-constant compnum-tag		#x37)
(define-constant disp-compnum-tag	0)
(define-constant disp-compnum-real	(* 1 wordsize))
(define-constant disp-compnum-imag	(* 2 wordsize))
(define-constant compnum-size		(* 4 wordsize))

(define-constant off-compnum-tag	(fx- disp-compnum-tag  vector-tag))
(define-constant off-compnum-real	(fx- disp-compnum-real vector-tag))
(define-constant off-compnum-imag	(fx- disp-compnum-imag vector-tag))


;;;; cflonums

(define-constant cflonum-tag		#x47)
(define-constant disp-cflonum-tag	0)
(define-constant disp-cflonum-real	(* 1 wordsize))
(define-constant disp-cflonum-imag	(* 2 wordsize))
(define-constant cflonum-size		(* 4 wordsize))

(define-constant off-cflonum-tag	(fx- disp-cflonum-tag  vector-tag))
(define-constant off-cflonum-real	(fx- disp-cflonum-real vector-tag))
(define-constant off-cflonum-imag	(fx- disp-cflonum-imag vector-tag))


;;;; characters

;;Characters are 32-bit integers, on any platform.
(define-constant char-size		4)
(define-constant char-shift		8)
(define-constant char-tag		#x0F)
(define-constant char-mask		#xFF)


;;;; pairs

(define-constant pair-mask		7)
(define-constant pair-tag		1)

(define-constant disp-car		0)
(define-constant disp-cdr		wordsize)
(define-constant pair-size		(fx* 2 wordsize))

(define-constant off-car		(fx- disp-car pair-tag))
(define-constant off-cdr		(fx- disp-cdr pair-tag))


;;;; bytevectors

(define-constant bytevector-mask	7)
(define-constant bytevector-tag		2)
(define-constant disp-bytevector-length	0)
(define-constant disp-bytevector-data	8)
		;To allow the same displacement on both 32-bit and 64-bit platforms.

(define-constant off-bytevector-length	(fx- disp-bytevector-length bytevector-tag))
(define-constant off-bytevector-data	(fx- disp-bytevector-data   bytevector-tag))


;;;; symbols

(define-constant symbol-primary-tag		vector-tag)
(define-constant symbol-tag			#x5F)
(define-constant symbol-record-tag		#x5F)

(define-constant disp-symbol-record-tag		0 #;(fx* 0 wordsize))
(define-constant disp-symbol-record-string	(fx* 1 wordsize))
(define-constant disp-symbol-record-ustring	(fx* 2 wordsize))
(define-constant disp-symbol-record-value	(fx* 3 wordsize))
(define-constant disp-symbol-record-proc	(fx* 4 wordsize))
(define-constant disp-symbol-record-plist	(fx* 5 wordsize))
(define-constant symbol-record-size		(fx* 6 wordsize))

(define-constant off-symbol-record-tag		(fx- disp-symbol-record-tag     symbol-primary-tag))
(define-constant off-symbol-record-string	(fx- disp-symbol-record-string  symbol-primary-tag))
(define-constant off-symbol-record-ustring	(fx- disp-symbol-record-ustring symbol-primary-tag))
(define-constant off-symbol-record-value	(fx- disp-symbol-record-value   symbol-primary-tag))
(define-constant off-symbol-record-proc		(fx- disp-symbol-record-proc    symbol-primary-tag))
(define-constant off-symbol-record-plist	(fx- disp-symbol-record-plist   symbol-primary-tag))


;;;; structs

(define-constant record-tag			vector-tag)
(define-constant disp-struct-rtd		0)
(define-constant disp-struct-std		0)
(define-constant disp-struct-data		wordsize)

(define-constant off-struct-rtd			(fx- disp-struct-rtd  vector-tag))
(define-constant off-struct-std			(fx- disp-struct-std  vector-tag))
(define-constant off-struct-data		(fx- disp-struct-data vector-tag))

(define-constant idx-std-std			0)
(define-constant idx-std-name			1)
(define-constant idx-std-length			2)
(define-constant idx-std-fields			3)
(define-constant idx-std-printer		4)
(define-constant idx-std-symbol			5)
(define-constant idx-std-destructor		6)

;;Struct type descriptor fields.
(define-constant disp-std-rtd			idx-std-std)
(define-constant disp-std-std			idx-std-std)
(define-constant disp-std-name			(fx* wordsize idx-std-name))
(define-constant disp-std-length		(fx* wordsize idx-std-length))
(define-constant disp-std-fields		(fx* wordsize idx-std-fields))
(define-constant disp-std-printer		(fx* wordsize idx-std-printer))
(define-constant disp-std-symbol		(fx* wordsize idx-std-symbol))
(define-constant disp-std-destructor		(fx* wordsize idx-std-destructor))

(define-constant off-std-rtd			(fx- disp-std-rtd	 vector-tag))
(define-constant off-std-std			(fx- disp-std-std	 vector-tag))
(define-constant off-std-name			(fx- disp-std-name	 vector-tag))
(define-constant off-std-length			(fx- disp-std-length	 vector-tag))
(define-constant off-std-fields			(fx- disp-std-fields	 vector-tag))
(define-constant off-std-printer		(fx- disp-std-printer	 vector-tag))
(define-constant off-std-symbol			(fx- disp-std-symbol	 vector-tag))
(define-constant off-std-destructor		(fx- disp-std-destructor vector-tag))


;;;; strings

(define-constant string-mask			#b111)
(define-constant string-tag			6)

(define-constant disp-string-length		0)
(define-constant disp-string-data		wordsize)

(define-constant off-string-length		(fx- disp-string-length string-tag))
(define-constant off-string-data		(fx- disp-string-data   string-tag))


;;;; code objects

(define-constant code-tag			#x2F)
(define-constant disp-code-tag			0)
(define-constant disp-code-instrsize		(* 1 wordsize))
(define-constant disp-code-relocsize		(* 2 wordsize))
(define-constant disp-code-freevars		(* 3 wordsize))
(define-constant disp-code-annotation		(* 4 wordsize))
(define-constant disp-code-unused		(* 5 wordsize))
(define-constant disp-code-data			(* 6 wordsize))

(define-constant off-code-tag			(fx- disp-code-tag vector-tag))
(define-constant off-code-instrsize		(fx- disp-code-instrsize vector-tag))
(define-constant off-code-relocsize		(fx- disp-code-relocsize vector-tag))
(define-constant off-code-freevars		(fx- disp-code-freevars vector-tag))
(define-constant off-code-annotation		(fx- disp-code-annotation vector-tag))
(define-constant off-code-unused		(fx- disp-code-unused vector-tag))
(define-constant off-code-data			(fx- disp-code-data vector-tag))

;;Notify the assembler about the offset of the binary code area from the beginning of
;;a code object's memory block.
;;
;;   |-----------|-----------------------------------|
;;     meta data           binary data area
;;
;;   |...........|
;;      offset
;;
(module ()
  (code-entry-adjustment off-code-data))


;;;; closure objects

(define-constant closure-mask			7)
(define-constant closure-tag			3)

(define-constant disp-closure-code		0)
(define-constant disp-closure-data		wordsize)

(define-constant off-closure-code		(fx- disp-closure-code closure-tag))
(define-constant off-closure-data		(fx- disp-closure-data closure-tag))


;;;; continuations

(define-constant continuation-tag		#x1F)

(define-constant disp-continuation-tag		0)
(define-constant disp-continuation-top		(* 1 wordsize))
(define-constant disp-continuation-size		(* 2 wordsize))
(define-constant disp-continuation-next		(* 3 wordsize))
(define-constant continuation-size		(* 4 wordsize))

(define-constant off-continuation-tag		(fx- disp-continuation-tag  vector-tag))
(define-constant off-continuation-top		(fx- disp-continuation-top  vector-tag))
(define-constant off-continuation-size		(fx- disp-continuation-size vector-tag))
(define-constant off-continuation-next		(fx- disp-continuation-next vector-tag))


;;;; input/output ports

(define-constant port-tag			#x3F)
(define-constant port-mask			#x3F)

;;How many bits to right-shift the first  word in a port memory block to
;;isolate the port attributes.
(define-constant port-attrs-shift		6)

;;These  values  must  be  kept  in   sync  with  the  ones  defined  in
;;"ikarus.io.sls".
(define-constant INPUT-PORT-TAG			#b00000000000001)
(define-constant OUTPUT-PORT-TAG		#b00000000000010)
(define-constant TEXTUAL-PORT-TAG		#b00000000000100)
(define-constant BINARY-PORT-TAG		#b00000000001000)

(define-constant disp-port-attrs		0)
(define-constant disp-port-index		(fx*  1 wordsize))
(define-constant disp-port-size			(fx*  2 wordsize))
(define-constant disp-port-buffer		(fx*  3 wordsize))
(define-constant disp-port-transcoder		(fx*  4 wordsize))
(define-constant disp-port-id			(fx*  5 wordsize))
(define-constant disp-port-read!		(fx*  6 wordsize))
(define-constant disp-port-write!		(fx*  7 wordsize))
(define-constant disp-port-get-position		(fx*  8 wordsize))
(define-constant disp-port-set-position!	(fx*  9 wordsize))
(define-constant disp-port-close		(fx* 10 wordsize))
(define-constant disp-port-cookie		(fx* 11 wordsize))
(define-constant disp-port-unused1		(fx* 12 wordsize))
(define-constant disp-port-unused2		(fx* 13 wordsize))
(define-constant port-size			(fx* 14 wordsize))

(define-constant off-port-attrs			(fx- disp-port-attrs		vector-tag))
(define-constant off-port-index			(fx- disp-port-index		vector-tag))
(define-constant off-port-size			(fx- disp-port-size		vector-tag))
(define-constant off-port-buffer		(fx- disp-port-buffer		vector-tag))
(define-constant off-port-transcoder		(fx- disp-port-transcoder	vector-tag))
(define-constant off-port-id			(fx- disp-port-id		vector-tag))
(define-constant off-port-read!			(fx- disp-port-read!		vector-tag))
(define-constant off-port-write!		(fx- disp-port-write!		vector-tag))
(define-constant off-port-get-position		(fx- disp-port-get-position	vector-tag))
(define-constant off-port-set-position!		(fx- disp-port-set-position!	vector-tag))
(define-constant off-port-close			(fx- disp-port-close		vector-tag))
(define-constant off-port-cookie		(fx- disp-port-cookie		vector-tag))
(define-constant off-port-unused1		(fx- disp-port-unused1		vector-tag))
(define-constant off-port-unused2		(fx- disp-port-unused2		vector-tag))


;;;; transcoders

(define-constant transcoder-mask			#xFF) ;;; 0011
(define-constant transcoder-tag				#x7F) ;;; 0011
(define-constant transcoder-payload-shift		10)

(define-constant transcoder-write-utf8-mask		#x1000)
(define-constant transcoder-write-byte-mask		#x2000)
(define-constant transcoder-read-utf8-mask		#x4000)
(define-constant transcoder-read-byte-mask		#x8000)
(define-constant transcoder-handling-mode-shift		16)
(define-constant transcoder-handling-mode-bits		2)
(define-constant transcoder-eol-style-shift		18)
(define-constant transcoder-eol-style-bits		3)
(define-constant transcoder-codec-shift			21)
(define-constant transcoder-codec-bits			3)

(define-constant transcoder-handling-mode:none		#b00)
(define-constant transcoder-handling-mode:ignore	#b01)
(define-constant transcoder-handling-mode:raise		#b10)
(define-constant transcoder-handling-mode:replace	#b11)

(define-constant transcoder-eol-style:none		#b000)
(define-constant transcoder-eol-style:lf		#b001)
(define-constant transcoder-eol-style:cr		#b010)
(define-constant transcoder-eol-style:crlf		#b011)
(define-constant transcoder-eol-style:nel		#b100)
(define-constant transcoder-eol-style:crnel		#b101)
(define-constant transcoder-eol-style:ls		#b110)

(define-constant transcoder-codec:none			#b000)
(define-constant transcoder-codec:latin-1		#b001)
(define-constant transcoder-codec:utf-8			#b010)
(define-constant transcoder-codec:utf-16		#b011)


;;;; pointer objects

(define-constant pointer-tag			#x107)
(define-constant disp-pointer-data		wordsize)
(define-constant pointer-size			(fx* 2 wordsize))

(define-constant off-pointer-data		(fx- disp-pointer-data vector-tag))


;;;; tcbuckets

(define-constant disp-tcbucket-tconc		0)
(define-constant disp-tcbucket-key		(fx* 1 wordsize))
(define-constant disp-tcbucket-val		(fx* 2 wordsize))
(define-constant disp-tcbucket-next		(fx* 3 wordsize))
(define-constant tcbucket-size			(fx* 4 wordsize))

(define-constant off-tcbucket-tconc		(fx- disp-tcbucket-tconc vector-tag))
(define-constant off-tcbucket-key		(fx- disp-tcbucket-key   vector-tag))
(define-constant off-tcbucket-val		(fx- disp-tcbucket-val   vector-tag))
(define-constant off-tcbucket-next		(fx- disp-tcbucket-next  vector-tag))


;;;; interfacing with the C language struct PCB
;;
;;The following are offsets to be added to  a PCB pointer to obtain the offset of the
;;specified struct field.
;;

;;(define-constant pcb-allocation-pointer	(fx*  0 wordsize)) NOT USED
(define-constant pcb-allocation-redline		(fx*  1 wordsize))
;;(define-constant pcb-frame-pointer		(fx*  2 wordsize)) NOT USED
(define-constant pcb-frame-base			(fx*  3 wordsize))
(define-constant pcb-frame-redline		(fx*  4 wordsize))
(define-constant pcb-next-continuation		(fx*  5 wordsize)) ;this is the C field "next_k"
;;(define-constant pcb-system-stack		(fx*  6 wordsize)) NOT USED
(define-constant pcb-dirty-vector		(fx*  7 wordsize))
(define-constant pcb-arg-list			(fx*  8 wordsize))
(define-constant pcb-engine-counter		(fx*  9 wordsize))
(define-constant pcb-interrupted		(fx* 10 wordsize))
(define-constant pcb-base-rtd			(fx* 11 wordsize))
(define-constant pcb-collect-key		(fx* 12 wordsize))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
