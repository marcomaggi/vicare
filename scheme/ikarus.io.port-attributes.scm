;;;Copyright (c) 2011-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006,2007,2008  Abdulaziz Ghuloum
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
;;;


;;;; port attributes
;;
;;Each  port has  a tag  retrievable with  the $PORT-TAG  or $PORT-ATTRS
;;primitive  operations.   All  the   tags  have  24  bits.   The  least
;;significant  14 bits  are reserved  as  "fast attributes"  and can  be
;;independently extracted as a fixnum used by the macros:
;;
;;   %CASE-TEXTUAL-INPUT-PORT-FAST-TAG
;;   %CASE-TEXTUAL-OUTPUT-PORT-FAST-TAG
;;   %CASE-BINARY-INPUT-PORT-FAST-TAG
;;   %CASE-BINARY-OUTPUT-PORT-FAST-TAG
;;
;;to  quickly select  code to  run for  I/O operations  on a  port.  The
;;following bitpatterns  are used to  compose the fast  attributes; note
;;that bits  9, 10,  11 and  12 are currently  unused and  available for
;;additional codecs.  Notice that the fixnum  0 is not a valid fast tag,
;;this fact can be used to speed up a little dispatching of evaluation.
;;
;;These  values  must  be  kept  in   sync  with  the  ones  defined  in
;;"ikarus.compiler.sls".
;;
;;					  32109876543210
;;                type bits                         ||||
;;                codec bits                   |||||
;;                unused codec bits        ||||
;;                true-if-closed bit      |
;;					  32109876543210
(define INPUT-PORT-TAG			#b00000000000001)
(define OUTPUT-PORT-TAG			#b00000000000010)
(define TEXTUAL-PORT-TAG		#b00000000000100)
(define BINARY-PORT-TAG			#b00000000001000)
(define FAST-CHAR-TEXT-TAG		#b00000000010000)
(define FAST-U8-TEXT-TAG		#b00000000100000)
(define FAST-LATIN-TEXT-TAG		#b00000001000000)
(define FAST-U16BE-TEXT-TAG		#b00000010000000)
(define FAST-U16LE-TEXT-TAG		#b00000100000000)
(define INIT-U16-TEXT-TAG		#b00000110000000)
(define CLOSED-PORT-TAG			#b10000000000000)

;;The following bitpatterns are  used for additional attributes ("other"
;;attributes in the code).
;;
;;Notice that there  is no BUFFER-MODE-BLOCK-TAG bit: only  for LINE and
;;NONE  buffering  something  must   be  done.   BLOCK  buffer  mode  is
;;represented by setting to zero the 2 block mode bits.
;;
;;                                        321098765432109876543210
;;                      non-fast-tag bits ||||||||||
;;  true if port is both input and output          |
;;                       buffer mode bits        ||
;;        true if port is in the guardian       |
;;   true if port has an extract function      |
;;                         EOL style bits   |||
;;     true if the device is a file desc.  |
;;                            unused bits |
;;                                        321098765432109876543210
(define INPUT/OUTPUT-PORT-TAG		#b000000000100000000000000)
		;Used to tag ports that are both input and output.
(define BUFFER-MODE-NONE-TAG		#b000000001000000000000000)
		;Used to tag ports having NONE as buffer mode.
(define BUFFER-MODE-LINE-TAG		#b000000010000000000000000)
		;Used to tag ports having LINE as buffer mode.
(define GUARDED-PORT-TAG		#b000000100000000000000000)
		;Used  to tag  ports which  must be  closed by  the port
		;guardian   after  being   collected   by  the   garbage
		;collector.  See the definition of PORT-GUARDIAN.
(define PORT-WITH-EXTRACTION-TAG	#b000001000000000000000000)
		;Used to tag binary  output ports which accumulate bytes
		;to be later retrieved by an extraction function.  These
		;ports need special treatement when a transcoded port is
		;created on top of them.  See TRANSCODED-PORT.
(define PORT-WITH-FD-DEVICE		#b010000000000000000000000)
		;Used  to  tag ports  that  have  a  file descriptor  as
		;device.

;;                                                321098765432109876543210
(define EOL-STYLE-MASK				#b001110000000000000000000)
(define EOL-STYLE-NOT-MASK			#b110001111111111111111111)
(define EOL-LINEFEED-TAG			#b000010000000000000000000) ;;symbol -> lf
(define EOL-CARRIAGE-RETURN-TAG			#b000100000000000000000000) ;;symbol -> cr
(define EOL-CARRIAGE-RETURN-LINEFEED-TAG	#b000110000000000000000000) ;;symbol -> crlf
(define EOL-NEXT-LINE-TAG			#b001000000000000000000000) ;;symbol -> nel
(define EOL-CARRIAGE-RETURN-NEXT-LINE-TAG	#b001010000000000000000000) ;;symbol -> crnel
(define EOL-LINE-SEPARATOR-TAG			#b001100000000000000000000) ;;symbol -> ls
		;Used  to  tag  textual  ports  with  end-of-line  (EOL)
		;conversion style.

(define DEFAULT-OTHER-ATTRS			#b000000000000000000000000)
		;Default  non-fast attributes:  non-guarded  port, block
		;buffer mode, no extraction function, EOL style none.

;;If we are just interested in the port type: input or output, binary or
;;textual bits, we can do:
;;
;;  (let ((type-bits ($fxand ($port-attrs port) PORT-TYPE-MASK)))
;;    ($fx= type-bits *-PORT-BITS))
;;
;;or:
;;
;;  (let ((type-bits ($fxand ($port-attrs port) *-PORT-BITS)))
;;    ($fx= type-bits *-PORT-BITS))
;;
;;where *-PORT-BITS  is one  of the constants  below.  Notice  that this
;;predicate is not influenced by the fact that the port is closed.
;;
;;					  32109876543210
(define PORT-TYPE-MASK			#b00000000001111)
(define BINARY-INPUT-PORT-BITS		($fxior BINARY-PORT-TAG  INPUT-PORT-TAG))
(define BINARY-OUTPUT-PORT-BITS		($fxior BINARY-PORT-TAG  OUTPUT-PORT-TAG))
(define TEXTUAL-INPUT-PORT-BITS		($fxior TEXTUAL-PORT-TAG INPUT-PORT-TAG))
(define TEXTUAL-OUTPUT-PORT-BITS	($fxior TEXTUAL-PORT-TAG OUTPUT-PORT-TAG))

;;The following  tag constants allow  fast classification of  open input
;;ports by doing:
;;
;;   ($fx= ($port-fast-attrs port) FAST-GET-*-TAG)
;;
;;where FAST-GET-*-TAG  is one of  the constants below.  Notice  that if
;;the port is closed the predicate will fail because the return value of
;;$PORT-FAST-ATTRS includes the true-if-closed bit.

;;This one is  used for binary input ports,  having a bytevector buffer,
;;from which raw octets must be read.
(define FAST-GET-BYTE-TAG        BINARY-INPUT-PORT-BITS)
;;
;;This one is used for textual input ports, having a string buffer.
(define FAST-GET-CHAR-TAG	($fxior FAST-CHAR-TEXT-TAG  TEXTUAL-INPUT-PORT-BITS))
;;
;;The following  are used for  textual input ports, having  a bytevector
;;buffer and a  transcoder, from which characters in  some encoding must
;;be read.
(define FAST-GET-UTF8-TAG	($fxior FAST-U8-TEXT-TAG    TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-8 transcoder.
(define FAST-GET-LATIN-TAG	($fxior FAST-LATIN-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;Latin-1 transcoder.
(define FAST-GET-UTF16BE-TAG	($fxior FAST-U16BE-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-16 big endian transcoder.
(define FAST-GET-UTF16LE-TAG	($fxior FAST-U16LE-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-16 little endian transcoder.
(define INIT-GET-UTF16-TAG	($fxior INIT-U16-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-16 transcoder  not yet recognised as  little or big
		;endian:  endianness selection  is performed  by reading
		;the Byte Order Mark (BOM) at the beginning of the input
		;data.

;;The following  tag constants allow fast classification  of open output
;;ports by doing:
;;
;;   ($fx= ($port-fast-attrs port) FAST-PUT-*-TAG)
;;
;;where FAST-PUT-*-TAG  is one of  the constants below.  Notice  that if
;;the port is closed the predicate will fail because the return value of
;;$PORT-FAST-ATTRS includes the true-if-closed bit.

;;This one is used for binary output ports, having bytevector buffer, to
;;which raw bytes must be written.
(define FAST-PUT-BYTE-TAG	BINARY-OUTPUT-PORT-BITS)
;;
;;This one is used for textual output ports, having a string buffer.
(define FAST-PUT-CHAR-TAG	($fxior FAST-CHAR-TEXT-TAG  TEXTUAL-OUTPUT-PORT-BITS))
;;
;;The following are  used for textual output ports,  having a bytevector
;;buffer and a transcoder, to  which characters in some encoding must be
;;written.
(define FAST-PUT-UTF8-TAG	($fxior FAST-U8-TEXT-TAG    TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;UTF-8 transcoder.
(define FAST-PUT-LATIN-TAG	($fxior FAST-LATIN-TEXT-TAG TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;Latin-1 transcoder.
(define FAST-PUT-UTF16BE-TAG	($fxior FAST-U16BE-TEXT-TAG TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;UTF-16 big endian transcoder.
(define FAST-PUT-UTF16LE-TAG	($fxior FAST-U16LE-TEXT-TAG TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;UTF-16 little endian transcoder.
(define INIT-PUT-UTF16-TAG	($fxior FAST-PUT-UTF16BE-TAG FAST-PUT-UTF16LE-TAG))
		;Tag for textual output ports with bytevector buffer and
		;UTF-16 transcoder  with data  not yet recognised  to be
		;little  or  big endian:  selection  is performed  after
		;writing  the  Byte Order  Mark  (BOM).   FIXME This  is
		;currently not supported.

;;; --------------------------------------------------------------------

(define-syntax %select-input-fast-tag-from-transcoder
  ;;When using  this macro without specifying the  other attributes: the
  ;;default attributes are automatically included.  When we specify some
  ;;non-fast attribute, it is  our responsibility to specify the default
  ;;attributes if we want them included.
  ;;
  (syntax-rules ()
    ((_ ?who ?transcoder)
     (%%select-input-fast-tag-from-transcoder ?who ?transcoder DEFAULT-OTHER-ATTRS))
    ((_ ?who ?transcoder ?other-attribute)
     (%%select-input-fast-tag-from-transcoder ?who ?transcoder ?other-attribute))
    ((_ ?who ?transcoder ?other-attribute . ?other-attributes)
     (%%select-input-fast-tag-from-transcoder ?who ?transcoder
					      ($fxior ?other-attribute . ?other-attributes)))))

(define (%%select-input-fast-tag-from-transcoder who maybe-transcoder other-attributes)
  ;;Return  a fixnum  containing the  tag attributes  for an  input port
  ;;using   MAYBE-TRANSCODER.   OTHER-ATTRIBUTES   must   be  a   fixnum
  ;;representing the  non-fast attributes  to compose with  the selected
  ;;fast attributes.
  ;;
;;;NOTE We  cannot put assertions  here because this function  is called
;;;upon initialisation  of the library  when the standard port  have not
;;;yet  been  fully constructed.   A  failing  assertion  would use  the
;;;standard ports, causing a segfault.
  (if (not maybe-transcoder)
      ($fxior other-attributes FAST-GET-BYTE-TAG)
    (case (transcoder-codec maybe-transcoder)
      ((utf-8-codec)	($fxior other-attributes FAST-GET-UTF8-TAG))
      ((latin-1-codec)	($fxior other-attributes FAST-GET-LATIN-TAG))
      ((utf-16le-codec)	($fxior other-attributes FAST-GET-UTF16LE-TAG))
      ((utf-16be-codec)	($fxior other-attributes FAST-GET-UTF16BE-TAG))
      ;;The      selection     between      FAST-GET-UTF16LE-TAG     and
      ;;FAST-GET-UTF16BE-TAG is performed as part of the Byte Order Mark
      ;;(BOM) reading operation when the first char is read.
      ((utf-16-codec)	($fxior other-attributes INIT-GET-UTF16-TAG))
      ;;If no codec  is recognised, wait to read  the first character to
      ;;tag the port according to the Byte Order Mark.
      (else		($fxior other-attributes TEXTUAL-INPUT-PORT-BITS)))))

(define-syntax %select-output-fast-tag-from-transcoder
  ;;When using  this macro without specifying the  other attributes: the
  ;;default attributes are automatically included.  When we specify some
  ;;non-fast attribute, it is  our responsibility to specify the default
  ;;attributes if we want them included.
  ;;
  (syntax-rules ()
    ((_ ?who ?transcoder)
     (%%select-output-fast-tag-from-transcoder ?who ?transcoder DEFAULT-OTHER-ATTRS))
    ((_ ?who ?transcoder ?other-attribute)
     (%%select-output-fast-tag-from-transcoder ?who ?transcoder ?other-attribute))
    ((_ ?who ?transcoder ?other-attribute . ?other-attributes)
     (%%select-output-fast-tag-from-transcoder ?who ?transcoder
					       ($fxior ?other-attribute . ?other-attributes)))))

(define (%%select-output-fast-tag-from-transcoder who maybe-transcoder other-attributes)
  ;;Return a  fixnum containing  the tag attributes  for an  output port
  ;;using   MAYBE-TRANSCODER.   OTHER-ATTRIBUTES   must   be  a   fixnum
  ;;representing the  non-fast attributes  to compose with  the selected
  ;;fast attributes.
  ;;
  ;;Notice  that  an output  port  is  never  tagged as  UTF-16  without
  ;;selection  of  endianness; by  default  big  endianness is  selected
  ;;because  it seems  to be  mandated  by the  Unicode Consortium,  see
  ;;<http://unicode.org/faq/utf_bom.html> question: "Why  do some of the
  ;;UTFs have a BE or LE in their label, such as UTF-16LE?"
  ;;
;;;NOTE We  cannot put assertions  here because this function  is called
;;;upon initialisation  of the library  when the standard port  have not
;;;yet  been  fully constructed.   A  failing  assertion  would use  the
;;;standard ports, causing a segfault.
  (if (not maybe-transcoder)
      ($fxior other-attributes FAST-PUT-BYTE-TAG)
    (case (transcoder-codec maybe-transcoder)
      ((utf-8-codec)	($fxior other-attributes FAST-PUT-UTF8-TAG))
      ((latin-1-codec)	($fxior other-attributes FAST-PUT-LATIN-TAG))
      ((utf-16le-codec)	($fxior other-attributes FAST-PUT-UTF16LE-TAG))
      ((utf-16be-codec)	($fxior other-attributes FAST-PUT-UTF16BE-TAG))
      ;;By default we select big endian UTF-16.
      ((utf-16-codec)	($fxior other-attributes FAST-PUT-UTF16BE-TAG))
      (else
       (assertion-violation who "unsupported codec" (transcoder-codec maybe-transcoder))))))

(define-syntax-rule (%select-input/output-fast-tag-from-transcoder . ?args)
  ;;Return  a fixnum  containing the  tag  attributes for  an input  and
  ;;output port.
  (%select-output-fast-tag-from-transcoder . ?args))

;;; --------------------------------------------------------------------

(define-inline ($mark-port-closed! ?port)
  ;;Set the CLOSED?  bit to 1 in the attributes or PORT.
  ;;
  ($set-port-attrs! ?port ($fxior CLOSED-PORT-TAG ($port-attrs ?port))))

;;This  mask  is used  to  nullify  the buffer  mode  bits  in a  fixnum
;;representing port attributes.
;;
;;					  321098765432109876543210
(define BUFFER-MODE-NOT-MASK		#b111111100111111111111111)

(define-inline ($set-port-buffer-mode-to-block! ?port)
  ($set-port-attrs! ?port ($fxand BUFFER-MODE-NOT-MASK ($port-attrs ?port))))

(define-inline ($set-port-buffer-mode-to-none! ?port)
  ($set-port-attrs! ?port ($fxior ($fxand BUFFER-MODE-NOT-MASK ($port-attrs ?port))
					 BUFFER-MODE-NONE-TAG)))

(define-inline ($set-port-buffer-mode-to-line! ?port)
  ($set-port-attrs! ?port ($fxior ($fxand BUFFER-MODE-NOT-MASK ($port-attrs ?port))
					 BUFFER-MODE-LINE-TAG)))

;;; --------------------------------------------------------------------

;;                                 321098765432109876543210
(define FAST-ATTRS-MASK          #b000000000011111111111111)
(define OTHER-ATTRS-MASK         #b111111111100000000000000)

(define-inline ($port-fast-attrs port)
  ;;Extract the fast attributes from the tag of PORT.
  ;;
  ($fxand ($port-attrs port) FAST-ATTRS-MASK))

(define-inline ($port-other-attrs port)
  ;;Extract the non-fast attributes from the tag of PORT.
  ;;
  ($fxand ($port-attrs port) OTHER-ATTRS-MASK))

(define-inline ($set-port-fast-attrs! port fast-attrs)
  ;;Store new fast attributes in the tag of PORT.
  ;;
  ($set-port-attrs! port ($fxior ($port-other-attrs port) fast-attrs)))

(define-inline ($port-fast-attrs-or-zero obj)
  ;;Given  a Scheme  value:  if it  is  a port  value  extract the  fast
  ;;attributes and return  them, else return zero.  Notice  that zero is
  ;;not a valid fast tag.
  ;;
  ($fxand ($port-tag obj) FAST-ATTRS-MASK))


;;; --------------------------------------------------------------------

(define NEWLINE-CODE-POINT		#x000A) ;; U+000A
(define LINEFEED-CODE-POINT		#x000A) ;; U+000A
(define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
(define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
(define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

(define NEWLINE-CHAR			#\x000A) ;; U+000A
(define LINEFEED-CHAR			#\x000A) ;; U+000A
(define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
(define NEXT-LINE-CHAR			#\x0085) ;; U+0085
(define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

(define (%symbol->eol-attrs style)
  (case style
    ((none)	0)
    ((lf)	EOL-LINEFEED-TAG)
    ((cr)	EOL-CARRIAGE-RETURN-TAG)
    ((crlf)	EOL-CARRIAGE-RETURN-LINEFEED-TAG)
    ((nel)	EOL-NEXT-LINE-TAG)
    ((crnel)	EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
    ((ls)	EOL-LINE-SEPARATOR-TAG)
    (else	#f)))

(define (%select-eol-style-from-transcoder who maybe-transcoder)
  ;;Given a  transcoder return the non-fast  attributes representing the
  ;;selected end of line conversion style.
  ;;
;;;NOTE We  cannot put assertions  here because this function  is called
;;;upon initialisation  of the library  when the standard port  have not
;;;yet  been  fully constructed.   A  failing  assertion  would use  the
;;;standard ports, causing a segfault.
  (if (not maybe-transcoder)
      0		;EOL style none
    (let ((style (transcoder-eol-style maybe-transcoder))
	  (codec (transcoder-codec     maybe-transcoder)))
      (define (%unsupported-by-latin-1)
	(assertion-violation who
	  "EOL style conversion unsupported by Latin-1 codec" style))
      (case style
	((none)		0)
	((lf)		EOL-LINEFEED-TAG)
	((cr)		EOL-CARRIAGE-RETURN-TAG)
	((crlf)		EOL-CARRIAGE-RETURN-LINEFEED-TAG)
	((nel)		(cond ((eqv? codec (latin-1-codec))
			       (%unsupported-by-latin-1))
			      (else
			       EOL-NEXT-LINE-TAG)))
	((crnel)	(cond ((eqv? codec (latin-1-codec))
			       (%unsupported-by-latin-1))
			      (else
			       EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)))
	((ls)		(cond ((eqv? codec (latin-1-codec))
			       (%unsupported-by-latin-1))
			      (else
			       EOL-LINE-SEPARATOR-TAG)))
	(else
	 (assertion-violation who
	   "vicare internal error: invalid EOL style extracted from transcoder" style))))))

(define-inline (%unsafe.port-eol-style-bits ?port)
  ($fxand EOL-STYLE-MASK ($port-attrs ?port)))

(define-inline (%unsafe.port-nullify-eol-style-bits attributes)
  ($fxand EOL-STYLE-NOT-MASK attributes))

(define-inline (%unsafe.port-eol-style-is-none? port)
  ($fxzero? (%unsafe.port-eol-style-bits port)))

(define-syntax %case-eol-style
  ;;Select a body to be evaluated  if a port has the selected EOL style.
  ;;?EOL-BITS must be an identifier bound to the result of:
  ;;
  ;;   (%unsafe.port-eol-style-bits port)
  ;;
  (lambda (stx)
    (syntax-case stx ( ;;
		      EOL-LINEFEED-TAG			EOL-CARRIAGE-RETURN-TAG
		      EOL-CARRIAGE-RETURN-LINEFEED-TAG	EOL-NEXT-LINE-TAG
		      EOL-CARRIAGE-RETURN-NEXT-LINE-TAG	EOL-LINE-SEPARATOR-TAG
		      else)
      ((%case-eol-style (?eol-bits ?who)
	 ((EOL-LINEFEED-TAG)			. ?linefeed-body)
	 ((EOL-CARRIAGE-RETURN-TAG)		. ?carriage-return-body)
	 ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)	. ?carriage-return-linefeed-body)
	 ((EOL-NEXT-LINE-TAG)			. ?next-line-body)
	 ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)	. ?carriage-return-next-line-body)
	 ((EOL-LINE-SEPARATOR-TAG)		. ?line-separator-body)
	 (else					. ?none-body))
       (and (identifier? #'?eol-bits)
	    (identifier? #'?who))
       #'($case-fixnums ?eol-bits
	   ((0)					. ?none-body)
	   ((EOL-LINEFEED-TAG)			. ?linefeed-body)
	   ((EOL-CARRIAGE-RETURN-TAG)		. ?carriage-return-body)
	   ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)	. ?carriage-return-linefeed-body)
	   ((EOL-NEXT-LINE-TAG)			. ?next-line-body)
	   ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)	. ?carriage-return-next-line-body)
	   ((EOL-LINE-SEPARATOR-TAG)		. ?line-separator-body))
       ))))

;;; --------------------------------------------------------------------

(define-syntax (%case-binary-input-port-fast-tag stx)
  ;;Assuming ?PORT  has already been  validated as a port  value, select
  ;;code to be evaluated  if it is a binary input port.   If the port is
  ;;I/O and tagged  as binary output: retag it as  binary input.  If the
  ;;port is textual, output or closed: raise an assertion violation.
  ;;
  (syntax-case stx (FAST-GET-BYTE-TAG else)
    ((%case-binary-input-port-fast-tag (?port ?who)
       ((FAST-GET-BYTE-TAG) . ?byte-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging ((m ($port-fast-attrs-or-zero ?port)))
	 (cond (($fx= m FAST-GET-BYTE-TAG)
		(begin . ?byte-tag-body))
	       ((and (port? ?port)
		     ($input/output-port? ?port)
		     ($fx= m FAST-PUT-BYTE-TAG))
		(%unsafe.reconfigure-output-buffer-to-input-buffer ?port ?who)
		($set-port-fast-attrs! ?port FAST-GET-BYTE-TAG)
		(retry-after-tagging FAST-GET-BYTE-TAG))
	       (else
		(if (open-binary-input-port? ?port)
		    (assertion-violation ?who "vicare internal error: corrupted port" ?port)
		  (procedure-argument-violation ?who "expected open binary input port as argument" ?port))))))
    ))

(define-syntax (%case-binary-output-port-fast-tag stx)
  ;;Assuming ?PORT  has already been  validated as a port  value, select
  ;;code to be evaluated if it is  a binary output port.  If the port is
  ;;I/O and tagged  as binary input: retag it as  binary output.  If the
  ;;port is textual, input or closed: raise an assertion violation.
  ;;
  (syntax-case stx (FAST-PUT-BYTE-TAG else)
    ((%case-binary-input-port-fast-tag (?port ?who)
       ((FAST-PUT-BYTE-TAG) . ?byte-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging ((m ($port-fast-attrs-or-zero ?port)))
	 (cond (($fx= m FAST-PUT-BYTE-TAG)
		(begin . ?byte-tag-body))
	       ((and (port? ?port)
		     ($input/output-port? ?port)
		     ($fx= m FAST-GET-BYTE-TAG))
		(%unsafe.reconfigure-input-buffer-to-output-buffer ?port ?who)
		($set-port-fast-attrs! ?port FAST-PUT-BYTE-TAG)
		(retry-after-tagging FAST-PUT-BYTE-TAG))
	       (else
		(if (open-binary-output-port? ?port)
		    (assertion-violation ?who "vicare internal error: corrupted port" ?port)
		  (procedure-argument-violation ?who "expected open binary output port as argument" ?port))))))
    ))

(define-syntax (%case-textual-input-port-fast-tag stx)
  ;;For  a port  fast tagged  for input:  select a  body of  code  to be
  ;;evaluated.
  ;;
  ;;If the port is tagged for output and it is an I/O port: retag it for
  ;;input and select a body of code to be evaluated.
  ;;
  ;;If the  port is  untagged: validate it  as open textual  input port,
  ;;then try to  tag it reading the Byte Order  Mark.  If the validation
  ;;fails:  raise an  assertion violation.   If reading  the  BOM fails:
  ;;raise an exception of type &i/o-read.  If the port is at EOF: return
  ;;the EOF object.
  ;;
  (syntax-case stx ( ;;
		    FAST-GET-UTF8-TAG FAST-GET-CHAR-TAG FAST-GET-LATIN-TAG
		    FAST-GET-UTF16LE-TAG FAST-GET-UTF16BE-TAG)
    ((%case-textual-input-port-fast-tag (?port ?who)
       ((FAST-GET-UTF8-TAG)	. ?utf8-tag-body)
       ((FAST-GET-CHAR-TAG)	. ?char-tag-body)
       ((FAST-GET-LATIN-TAG)	. ?latin-tag-body)
       ((FAST-GET-UTF16LE-TAG)	. ?utf16le-tag-body)
       ((FAST-GET-UTF16BE-TAG)	. ?utf16be-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging-port ((m ($port-fast-attrs-or-zero ?port)))
	 (define (%validate-and-tag)
	   (unless (open-textual-input-port? ?port)
	     (procedure-argument-violation ?who "expected open binary input port as argument" ?port))
	   (%parse-bom-and-add-fast-tag (?who ?port)
	     (if-successful-match:
	      (retry-after-tagging-port ($port-fast-attrs ?port)))
	     (if-end-of-file: (eof-object))
	     (if-no-match-raise-assertion-violation)))
	 (define (%reconfigure-as-input fast-attrs)
	   (%unsafe.reconfigure-output-buffer-to-input-buffer ?port ?who)
	   ($set-port-fast-attrs! ?port fast-attrs)
	   (retry-after-tagging-port fast-attrs))
	 ($case-fixnums m
	   ((FAST-GET-UTF8-TAG)		. ?utf8-tag-body)
	   ((FAST-GET-CHAR-TAG)		. ?char-tag-body)
	   ((FAST-GET-LATIN-TAG)	. ?latin-tag-body)
	   ((FAST-GET-UTF16LE-TAG)	. ?utf16le-tag-body)
	   ((FAST-GET-UTF16BE-TAG)	. ?utf16be-tag-body)
	   ((INIT-GET-UTF16-TAG)
	    (if (%unsafe.parse-utf16-bom-and-add-fast-tag ?who ?port)
		(eof-object)
	      (retry-after-tagging-port ($port-fast-attrs ?port))))
	   ((FAST-GET-BYTE-TAG)
	    (assertion-violation ?who "expected textual port" ?port))
	   (else
	    (if (and (port? ?port)
		     ($input/output-port? ?port))
		($case-fixnums m
		  ((FAST-PUT-UTF8-TAG)
		   (%reconfigure-as-input FAST-GET-UTF8-TAG))
		  ((FAST-PUT-CHAR-TAG)
		   (%reconfigure-as-input FAST-GET-CHAR-TAG))
		  ((FAST-PUT-LATIN-TAG)
		   (%reconfigure-as-input FAST-GET-LATIN-TAG))
		  ((FAST-PUT-UTF16LE-TAG)
		   (%reconfigure-as-input FAST-GET-UTF16LE-TAG))
		  ((FAST-PUT-UTF16BE-TAG)
		   (%reconfigure-as-input FAST-GET-UTF16BE-TAG))
		  ((FAST-PUT-BYTE-TAG)
		   (assertion-violation ?who "expected textual port" ?port))
		  (else
		   (%validate-and-tag)))
	      (%validate-and-tag))))
	 ))))

(define-syntax (%case-textual-output-port-fast-tag stx)
  ;;For  a port fast  tagged for  output: select  a body  of code  to be
  ;;evaluated.
  ;;
  ;;If the port is tagged for input  and it is an I/O port: retag it for
  ;;output and select a body of code to be evaluated.
  ;;
  ;;If the  port is untagged: validate  it as open  textual output port.
  ;;If the validation fails: raise an assertion violation.
  ;;
  (syntax-case stx ( ;;
		    FAST-PUT-UTF8-TAG FAST-PUT-CHAR-TAG FAST-PUT-LATIN-TAG
		    FAST-PUT-UTF16LE-TAG FAST-PUT-UTF16BE-TAG)
    ((%case-textual-output-port-fast-tag (?port ?who)
       ((FAST-PUT-UTF8-TAG)	. ?utf8-tag-body)
       ((FAST-PUT-CHAR-TAG)	. ?char-tag-body)
       ((FAST-PUT-LATIN-TAG)	. ?latin-tag-body)
       ((FAST-PUT-UTF16LE-TAG)	. ?utf16le-tag-body)
       ((FAST-PUT-UTF16BE-TAG)	. ?utf16be-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging-port ((m ($port-fast-attrs-or-zero ?port)))
	 (define (%validate)
	   (if (open-textual-output-port? ?port)
	       (assertion-violation ?who "unsupported port transcoder" ?port)
	     (procedure-argument-violation ?who "expected open textual output port" ?port)))
	 (define (%reconfigure-as-output fast-attrs)
	   (%unsafe.reconfigure-input-buffer-to-output-buffer ?port ?who)
	   ($set-port-fast-attrs! ?port fast-attrs)
	   (retry-after-tagging-port fast-attrs))
	 ($case-fixnums m
	   ((FAST-PUT-UTF8-TAG)		. ?utf8-tag-body)
	   ((FAST-PUT-CHAR-TAG)		. ?char-tag-body)
	   ((FAST-PUT-LATIN-TAG)	. ?latin-tag-body)
	   ((FAST-PUT-UTF16LE-TAG)	. ?utf16le-tag-body)
	   ((FAST-PUT-UTF16BE-TAG)	. ?utf16be-tag-body)
	   ((FAST-PUT-BYTE-TAG)
	    (assertion-violation ?who "expected textual port" ?port))
	   (else
	    (if (and (port? ?port)
		     ($input/output-port? ?port))
		($case-fixnums m
		  ((FAST-GET-UTF8-TAG)
		   (%reconfigure-as-output FAST-PUT-UTF8-TAG))
		  ((FAST-GET-CHAR-TAG)
		   (%reconfigure-as-output FAST-PUT-CHAR-TAG))
		  ((FAST-GET-LATIN-TAG)
		   (%reconfigure-as-output FAST-PUT-LATIN-TAG))
		  ((FAST-GET-UTF16LE-TAG)
		   (%reconfigure-as-output FAST-PUT-UTF16LE-TAG))
		  ((FAST-GET-UTF16BE-TAG)
		   (%reconfigure-as-output FAST-PUT-UTF16BE-TAG))
		  ((FAST-GET-BYTE-TAG)
		   (assertion-violation ?who "expected textual port" ?port))
		  (else
		   (%validate)))
	      (%validate))))
	 ))))

;;; --------------------------------------------------------------------
;;; Backup of original Ikarus values

;;(define PORT-TYPE-MASK		#b00000000001111)
;;(define BINARY-INPUT-PORT-BITS	#b00000000001001)
;;(define BINARY-OUTPUT-PORT-BITS	#b00000000001010)
;;(define TEXTUAL-INPUT-PORT-BITS	#b00000000000101)
;;(define TEXTUAL-OUTPUT-PORT-BITS	#b00000000000110)

;;(define FAST-GET-BYTE-TAG		#b00000000001001)
;;(define FAST-GET-CHAR-TAG		#b00000000010101)
;;(define FAST-GET-UTF8-TAG		#b00000000100101)
;;(define FAST-GET-LATIN-TAG		#b00000001100101)
;;(define FAST-GET-UTF16BE-TAG		#b00000010000101)
;;(define FAST-GET-UTF16LE-TAG		#b00000100000101)

;;(define FAST-PUT-BYTE-TAG		#b00000000001010)
;;(define FAST-PUT-CHAR-TAG		#b00000000010110)
;;(define FAST-PUT-UTF8-TAG		#b00000000100110)
;;(define FAST-PUT-LATIN-TAG		#b00000001100110)
;;(define FAST-PUT-UTF16BE-TAG		#b00000010000110)
;;(define FAST-PUT-UTF16LE-TAG		#b00000100000110)
;;(define INIT-PUT-UTF16-TAG		#b00000110000110)


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
