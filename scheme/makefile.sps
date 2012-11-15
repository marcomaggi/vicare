#!../src/vicare -b vicare.boot --r6rs-script
;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008,2012  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Abstract
;;;
;;;	This  file  is  an  R6RS-compliant Scheme  program,  using  some
;;;	Vicare's  extension.   When  run  in the  appropriate  operating
;;;	system   environment:    it   rebuilds   Vicare's    boot   file
;;;	"vicare.boot".
;;;
;;;	This  program works hand-in-hand  with the  expander, especially
;;;	the    library   (psyntax    library-manager)   in    the   file
;;;	"psyntax.library-manager.sls".
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; adding a primitive operation to an existing system library
;;
;;*NOTE* This  description is a work  in progress (Marco  Maggi; Nov 30,
;;2011).
;;
;;Primitive operations are defined by the macro DEFINE-PRIMOPS; examples
;;are: $CAR, $CDR, $FX+ and $VECTOR-LENGTH but also FIXNUM? and STRING?.
;;
;;Here we want to examine the process of adding a primitive operation to
;;an  existing system library;  we will  not discuss  how to  define the
;;operation using the macro DEFINE-PRIMOPS.
;;
;;What is a primitive operation?  We can think of it as a macro embedded
;;in  the compiler,  which,  when used,  expands  inline the  elementary
;;instructions  to be  converted  to machine  language.  The  elementary
;;instructions are  expressed in Vicare's  high-level assembly language.
;;When building a new boot image we can use in Vicare's source code only
;;the primitive operations compiled in an existing boot image.
;;
;;Let's say  we want to generate  a new boot image  having the operation
;;$SWIRL-PAIR embedded in it and exported by the system library:
;;
;;   (ikarus system $pairs)
;;
;;which  already exists,  and making  use of  the operation  in Vicare's
;;source code; this is the scenario:
;;
;;1. The image BOOT-0 already exists.
;;
;;2. We  generate a  new temporary image,  BOOT-1, having  the operation
;;$SWIRL-PAIR embedded in it, but not using it anywhere.
;;
;;3.  We  generate another new  image, BOOT-2, which  offers $SWIRL-PAIR
;;and also uses it in the source code.
;;
;;Let's go.
;;
;;First  we define  the  $SWIRL-PAIR operation  adding  to the  compiler
;;library (in the appropriate place) a form like:
;;
;;  (define-primop $swirl-pair unsafe ---)
;;
;;this form alone is enough to  make the compiler aware of the existence
;;of the  operation.  Then,  in this  makefile, we add  an entry  to the
;;table IDENTIFIER->LIBRARY-MAP as follows:
;;
;;   (define identifier->library-map
;;     '(($swirl-pair		$pairs)
;;       ---))
;;
;;the order in which the entries appear in this table is not important.
;;
;;With no other changes we use  the image BOOT-0 to build an image which
;;will be BOOT-1.   Now we can use $SWIRL-PAIR  in Vicare's source code,
;;then we use BOOT-1 to compile a new image which will be BOOT-2.
;;


;;;; adding a new system library
;;
;;*NOTE* This  description is a work  in progress (Marco  Maggi; Nov 30,
;;2011).
;;
;;By convention system libraries have names like:
;;
;;   (ikarus system <ID>)
;;
;;where  <ID> is prefixed  with a  $ character;  for good  style, system
;;libraries should export only primitive operations.
;;
;;Let's say we want to add to a boot image the library:
;;
;;  (ikarus system $spiffy)
;;
;;exporting the single primitive operation $SWIRL, this is the scenario:
;;
;;1. The image BOOT-0 already exists.
;;
;;2. We  generate a temporary new  image, BOOT-1, having  the new system
;;library in it but not in a correctly usable state.
;;
;;3. We  generate the another new  image, BOOT-2, having  the new system
;;library in a correct state.
;;
;;Let's go.
;;
;;First we  define the $SWIRL  operation adding to the  compiler library
;;(in the appropriate place) a form like:
;;
;;  (define-primop $swirl unsafe ---)
;;
;;this form alone is enough to  make the compiler aware of the existence
;;of the operation.  Then, in this  makefile, we add an entry at the end
;;of the table LIBRARY-LEGEND as follows:
;;
;;   (define library-legend
;;     '(---
;;       ($spiffy  (ikarus system $spiffy)  #t	#f))
;;
;;marking the library as visible but not required.  Then we add an entry
;;to the table IDENTIFIER->LIBRARY-MAP as follows:
;;
;;   (define identifier->library-map
;;     '(($swirl $spiffy)
;;       ---))
;;
;;the order in which the entries appear in this table is not important.
;;
;;Now we use the image BOOT-0  to build a new boot image, BOOT-1, having
;;the new library in it.  Then  we change the library entry in the table
;;LIBRARY-LEGEND as follows:
;;
;;       ($spiffy  (ikarus system $spiffy)  #t	#t)
;;
;;making it both visible and required.   Then we use the image BOOT-1 to
;;generate a new boot image which will be BOOT-2.
;;


(import (only (ikarus) import))
(import (except (ikarus)
		current-letrec-pass
		current-core-eval
		assembler-output optimize-cp optimize-level
		cp0-size-limit cp0-effort-limit expand/optimize
		expand/scc-letrec expand
		optimizer-output tag-analysis-output perform-tag-analysis))
(import (ikarus.compiler))
(import (except (psyntax system $bootstrap)
                eval-core
                current-primitive-locations
                compile-core-expr-to-port))
(import (ikarus.compiler)) ; just for fun

(optimize-level 2)
(perform-tag-analysis #t)
(pretty-width 160)
((pretty-format 'fix)
 ((pretty-format 'letrec)))
(strip-source-info #t)
(current-letrec-pass 'scc)

;;(set-port-buffer-mode! (current-output-port) (buffer-mode none))


;;;; helpers

(define boot-file-name
  "vicare.boot")

(define src-dir
  (or (getenv "VICARE_SRC_DIR") "."))

(define verbose-output? #t)

(define-syntax each-for
  (syntax-rules ()
    ((_ ?list ?lambda)
     (for-each ?lambda ?list))))

(define (make-collection)
  ;;Return a  closure to handle lists of  elements called "collections".
  ;;When  the closure  is  invoked  with no  arguments:  it returns  the
  ;;collection.   When  the closure  is  invoked  with  an argument:  it
  ;;prepends  the  argument  to  the  collection  without  checking  for
  ;;duplicates.
  ;;
  (let ((set '()))
    (case-lambda
     (()  set)
     ((x)
      (set! set (cons x set))))))

(define debug-printf
  (if verbose-output?
      (lambda args
	(let ((port (console-error-port)))
	  (apply fprintf port args)
	  (flush-output-port port)))
    (case-lambda
     ((str)
      (let ((port (console-error-port)))
	(fprintf port str)
	(flush-output-port port)))
     ((str . args)
      (let ((port (console-error-port)))
	(fprintf port ".")
	(flush-output-port port))))))

(define (pretty-print/stderr thing)
  (let ((port (console-error-port)))
    (pretty-print thing port)
    (flush-output-port port)))


(define scheme-library-files
  ;;Listed in the order in which they're loaded.
  ;;
  ;;Loading of the boot file may  segfault if a library is loaded before
  ;;its dependencies are loaded first.
  ;;
  ;;The  reason  is that  the  base libraries  are  not  a hierarchy  of
  ;;dependencies but rather an eco system in which every part depends on
  ;;the other.
  ;;
  ;;For  example,  the printer  may  call error  if  it  finds an  error
  ;;(e.g. "not an output port"),  while the error procedure may call the
  ;;printer to  display the message.  This  works fine as  long as error
  ;;does  not itself  cause an  error (which  may lead  to  the infamous
  ;;Error: Error: Error: Error: Error: Error: Error: Error: Error: ...).
  ;;
  '("ikarus.emergency.sls"
    "ikarus.options.sls"
    "ikarus.singular-objects.sls"
    "ikarus.handlers.sls"
    "ikarus.multiple-values.sls"
    "ikarus.control.sls"
    "ikarus.exceptions.sls"
    "ikarus.collect.sls"
    "ikarus.apply.sls"
    "ikarus.keywords.sls"
    "ikarus.predicates.sls"
    "ikarus.equal.sls"
    "ikarus.pairs.sls"
    "ikarus.lists.sls"
    "ikarus.fixnums.sls"
    "ikarus.chars.sls"
    "ikarus.structs.sls"
    "ikarus.hash-tables.sls"
    "ikarus.records.procedural.sls"
    "ikarus.strings.sls"
    "ikarus.unicode-conversion.sls"
    "ikarus.symbols.sls"
    "ikarus.vectors.sls"
    "ikarus.unicode.sls"
    "ikarus.string-to-number.sls"
    "ikarus.numerics.flonums.sls"
    "ikarus.numerics.generic-arithmetic.sls"
    "ikarus.numerics.flonum-conversion.sls"
    "ikarus.numerics.rationalize.sls"
    "ikarus.numerics.div-and-mod.sls"
    "ikarus.numerics.flonums.div-and-mod.sls"
    "ikarus.numerics.bitwise.misc.sls"
    "ikarus.numerics.sls"
    "ikarus.conditions.sls"
    "ikarus.guardians.sls"
    "ikarus.symbol-table.sls"
    "ikarus.codecs.sls"
    "ikarus.bytevectors.sls"
    "ikarus.pointers.sls"
    "ikarus.posix.sls"
    "ikarus.io.sls"
    "ikarus.pretty-formats.sls"
    "ikarus.writer.sls"
    "ikarus.foreign-libraries.sls"
    "ikarus.reader.sls"
    "ikarus.code-objects.sls"
    "ikarus.intel-assembler.sls"
    "ikarus.fasl.write.sls"
    "ikarus.fasl.read.sls"
    "ikarus.compiler.sls"
    "psyntax.compat.sls"
    "psyntax.library-manager.sls"
    "psyntax.internal.sls"
    "psyntax.config.sls"
    "psyntax.builders.sls"
    "psyntax.expander.sls"
    "ikarus.apropos.sls"
    "ikarus.load.sls"
    "ikarus.pretty-print.sls"
    "ikarus.readline.sls"
    "ikarus.cafe.sls"
    "ikarus.timer.sls"
    "ikarus.time-and-date.sls"
    "ikarus.sort.sls"
    "ikarus.promises.sls"
    "ikarus.enumerations.sls"
    "ikarus.command-line.sls"
;;; "ikarus.trace.sls"
    "ikarus.debugger.sls"
    "ikarus.main.sls"
    ))


(define ikarus-system-macros
  '((define				(define))
    (define-syntax			(define-syntax))
    (define-fluid-syntax		(define-fluid-syntax))
    (module				(module))
    (library				(library))
    (begin				(begin))
    (import				(import))
    (export				(export))
    (set!				(set!))
    (let-syntax				(let-syntax))
    (letrec-syntax			(letrec-syntax))
    (stale-when				(stale-when))
    (foreign-call			(core-macro . foreign-call))
    (quote				(core-macro . quote))
    (syntax-case			(core-macro . syntax-case))
    (syntax				(core-macro . syntax))
    (lambda					(core-macro . lambda))
    (case-lambda			(core-macro . case-lambda))
    (type-descriptor			(core-macro . type-descriptor))
    (letrec				(core-macro . letrec))
    (letrec*				(core-macro . letrec*))
    (if					(core-macro . if))
    (fluid-let-syntax			(core-macro . fluid-let-syntax))
    (record-type-descriptor		(core-macro . record-type-descriptor))
    (record-constructor-descriptor	(core-macro . record-constructor-descriptor))
    (let-values				(macro . let-values))
    (let*-values			(macro . let*-values))
    (define-struct			(macro . define-struct))
    (case				(macro . case))
    (syntax-rules			(macro . syntax-rules))
    (quasiquote				(macro . quasiquote))
    (quasisyntax			(macro . quasisyntax))
    (with-syntax			(macro . with-syntax))
    (identifier-syntax			(macro . identifier-syntax))
    (parameterize			(macro . parameterize))
    (parametrise			(macro . parameterize))
    (when				(macro . when))
    (unless				(macro . unless))
    (let				(macro . let))
    (let*				(macro . let*))
    (cond				(macro . cond))
    (do					(macro . do))
    (and				(macro . and))
    (or					(macro . or))
    (time				(macro . time))
    (delay				(macro . delay))
    (endianness				(macro . endianness))
    (assert				(macro . assert))
    (...				(macro . ...))
    (=>					(macro . =>))
    (else				(macro . else))
    (_					(macro . _))
    (unquote				(macro . unquote))
    (unquote-splicing			(macro . unquote-splicing))
    (unsyntax				(macro . unsyntax))
    (unsyntax-splicing			(macro . unsyntax-splicing))
    (trace-lambda			(macro . trace-lambda))
    (trace-let				(macro . trace-let))
    (trace-define			(macro . trace-define))
    (trace-define-syntax		(macro . trace-define-syntax))
    (trace-let-syntax			(macro . trace-let-syntax))
    (trace-letrec-syntax		(macro . trace-letrec-syntax))
    (guard				(macro . guard))
    (eol-style				(macro . eol-style))
    (buffer-mode			(macro . buffer-mode))
    (file-options			(macro . file-options))
    (error-handling-mode		(macro . error-handling-mode))
    (fields				(macro . fields))
    (mutable				(macro . mutable))
    (immutable				(macro . immutable))
    (parent				(macro . parent))
    (protocol				(macro . protocol))
    (sealed				(macro . sealed))
    (opaque				(macro . opaque ))
    (nongenerative			(macro . nongenerative))
    (parent-rtd				(macro . parent-rtd))
    (define-record-type			(macro . define-record-type))
    (define-enumeration			(macro . define-enumeration))
    (define-condition-type		(macro . define-condition-type))
;;;
    (&condition				($core-rtd . (&condition-rtd
						      &condition-rcd)))
    (&message				($core-rtd . (&message-rtd
						      &message-rcd)))
    (&warning				($core-rtd . (&warning-rtd
						      &warning-rcd)))
    (&serious				($core-rtd . (&serious-rtd
						      &serious-rcd)))
    (&error				($core-rtd . (&error-rtd
						      &error-rcd)))
    (&violation				($core-rtd . (&violation-rtd
						      &violation-rcd)))
    (&assertion				($core-rtd . (&assertion-rtd
						      &assertion-rcd)))
    (&irritants				($core-rtd . (&irritants-rtd
						      &irritants-rcd)))
    (&who				($core-rtd . (&who-rtd
						      &who-rcd)))
    (&non-continuable			($core-rtd . (&non-continuable-rtd
						      &non-continuable-rcd)))
    (&implementation-restriction	($core-rtd . (&implementation-restriction-rtd
						      &implementation-restriction-rcd)))
    (&lexical				($core-rtd . (&lexical-rtd
						      &lexical-rcd)))
    (&syntax				($core-rtd . (&syntax-rtd
						      &syntax-rcd)))
    (&undefined				($core-rtd . (&undefined-rtd
						      &undefined-rcd)))
    (&i/o				($core-rtd . (&i/o-rtd
						      &i/o-rcd)))
    (&i/o-read				($core-rtd . (&i/o-read-rtd
						      &i/o-read-rcd)))
    (&i/o-write				($core-rtd . (&i/o-write-rtd
						      &i/o-write-rcd)))
    (&i/o-invalid-position		($core-rtd . (&i/o-invalid-position-rtd
						      &i/o-invalid-position-rcd)))
    (&i/o-filename			($core-rtd . (&i/o-filename-rtd
						      &i/o-filename-rcd)))
    (&i/o-file-protection		($core-rtd . (&i/o-file-protection-rtd
						      &i/o-file-protection-rcd)))
    (&i/o-file-is-read-only		($core-rtd . (&i/o-file-is-read-only-rtd
						      &i/o-file-is-read-only-rcd)))
    (&i/o-file-already-exists		($core-rtd . (&i/o-file-already-exists-rtd
						      &i/o-file-already-exists-rcd)))
    (&i/o-file-does-not-exist		($core-rtd . (&i/o-file-does-not-exist-rtd
						      &i/o-file-does-not-exist-rcd)))
    (&i/o-port				($core-rtd . (&i/o-port-rtd
						      &i/o-port-rcd)))
    (&i/o-decoding			($core-rtd . (&i/o-decoding-rtd
						      &i/o-decoding-rcd)))
    (&i/o-encoding			($core-rtd . (&i/o-encoding-rtd
						      &i/o-encoding-rcd)))
    (&i/o-eagain			($core-rtd . (&i/o-eagain-rtd
    						      &i/o-eagain-rcd)))
    (&errno				($core-rtd . (&errno-rtd
    						      &errno-rcd)))
    (&out-of-memory-error		($core-rtd . (&out-of-memory-error-rtd
    						      &out-of-memory-error-rcd)))
    (&h_errno				($core-rtd . (&h_errno-rtd
    						      &h_errno-rcd)))
    (&no-infinities			($core-rtd . (&no-infinities-rtd
						      &no-infinities-rcd)))
    (&no-nans				($core-rtd . (&no-nans-rtd
						      &no-nans-rcd)))
    (&interrupted			($core-rtd . (&interrupted-rtd
						      &interrupted-rcd)))
    (&source				($core-rtd . (&source-rtd
						      &source-rcd)))
    ))


(define library-legend
  ;;Map full library specifications to nicknames: for example "i" is the
  ;;nickname  of  "(ikarus)".   Additionlly  tag  each  library  with  a
  ;;VISIBLE? and a REQUIRED? boolean.
  ;;
  ;;For each library  marked as REQUIRED?: an associated  record of type
  ;;LIBRARY   is  created   and  included   in  the   starting   set  of
  ;;BOOTSTRAP-COLLECTION.
  ;;
  ;;The libraries marked as VISIBLE? are installed in the boot image.
  ;;
  ;;See BOOTSTRAP-COLLECTION for details on how to add a library to this
  ;;list.
  ;;
  ;; abbr.              name			                visible? required?
  '((i			(ikarus)				#t	#t)
    (v			(vicare)				#t	#f)
    (cm			(chez modules)				#t	#t)
    (symbols		(ikarus symbols)			#t	#t)
    (parameters		(ikarus parameters)			#t	#t)
    (r			(rnrs)					#t	#t)
    (r5			(rnrs r5rs)				#t	#t)
    (ct			(rnrs control)				#t	#t)
    (ev			(rnrs eval)				#t	#t)
    (mp			(rnrs mutable-pairs)			#t	#t)
    (ms			(rnrs mutable-strings)			#t	#t)
    (pr			(rnrs programs)				#t	#t)
    (sc			(rnrs syntax-case)			#t	#t)
    (fi			(rnrs files)				#t	#t)
    (sr			(rnrs sorting)				#t	#t)
    (ba			(rnrs base)				#t	#t)
    (ls			(rnrs lists)				#t	#t)
    (is			(rnrs io simple)			#t	#t)
    (bv			(rnrs bytevectors)			#t	#t)
    (uc			(rnrs unicode)				#t	#t)
    (ex			(rnrs exceptions)			#t	#t)
    (bw			(rnrs arithmetic bitwise)		#t	#t)
    (fx			(rnrs arithmetic fixnums)		#t	#t)
    (fl			(rnrs arithmetic flonums)		#t	#t)
    (ht			(rnrs hashtables)			#t	#t)
    (ip			(rnrs io ports)				#t	#t)
    (en			(rnrs enums)				#t	#t)
    (co			(rnrs conditions)			#t	#t)
    (ri			(rnrs records inspection)		#t	#t)
    (rp			(rnrs records procedural)		#t	#t)
    (rs			(rnrs records syntactic)		#t	#t)
;;;
    ($pairs		(ikarus system $pairs)			#f	#t)
    ($lists		(ikarus system $lists)			#f	#t)
    ($chars		(ikarus system $chars)			#f	#t)
    ($strings		(ikarus system $strings)		#f	#t)
    ($vectors		(ikarus system $vectors)		#f	#t)
    ($flonums		(ikarus system $flonums)		#f	#t)
    ($bignums		(ikarus system $bignums)		#f	#t)
    ($bytes		(ikarus system $bytevectors)		#f	#t)
    ($transc		(ikarus system $transcoders)		#f	#t)
    ($fx		(ikarus system $fx)			#f	#t)
    ($rat		(ikarus system $ratnums)		#f	#t)
    ($comp		(ikarus system $compnums)		#f	#t)
    ($symbols		(ikarus system $symbols)		#f	#t)
    ($structs		(ikarus system $structs)		#f	#t)
    ($pointers		(ikarus system $pointers)		#t	#t)
    ($codes		(ikarus system $codes)			#f	#t)
    ($tcbuckets		(ikarus system $tcbuckets)		#f	#t)
    ($arg-list		(ikarus system $arg-list)		#f	#t)
    ($stack		(ikarus system $stack)			#f	#t)
    ($interrupts	(ikarus system $interrupts)		#f	#t)
    ($io		(ikarus system $io)			#f	#t)
    ($for		(ikarus system $foreign)		#f	#t)
    ($all		(psyntax system $all)			#f	#t)
    ($boot		(psyntax system $bootstrap)		#f	#t)
;;;
    (ne			(psyntax null-environment-5)		#f	#f)
    (se			(psyntax scheme-report-environment-5)	#f	#f)
;;;
    (posix		(vicare $posix)				#t	#t)
    ($language		(vicare language-extensions)		#f	#f)
    ($compiler		(ikarus system $compiler)		#f	#f)
    ))


(define identifier->library-map
  ;;Map  all the  identifiers of  exported  bindings (and  more) to  the
  ;;libraries   exporting   them,  using   the   nicknames  defined   by
  ;;LIBRARY-LEGEND.
  ;;
  ;;Notice that  the map includes  LIBRARY, IMPORT and EXPORT  which are
  ;;not bindings.
  ;;
  '((import					i v $language)
    (export					i v $language)
    (foreign-call				i v $language)
    (type-descriptor				i v $language)
    (parameterize				i v $language parameters)
    (parametrise				i v $language parameters)
    (define-struct				i v $language)
    (stale-when					i v $language)
    (time					i v $language)
    (trace-lambda				i v $language)
    (trace-let					i v $language)
    (trace-define				i v $language)
    (trace-define-syntax			i v $language)
    (trace-let-syntax				i v $language)
    (trace-letrec-syntax			i v $language)
    (integer->machine-word			i v $language)
    (machine-word->integer			i v $language)
    (make-list					i v $language)
    (last-pair					i v $language)
    (bwp-object?				i v $language)
    (weak-cons					i v $language)
    (weak-pair?					i v $language)
    (uuid					i v $language)
    (andmap					i v $language)
    (ormap					i v $language)
    (fx<					i v $language)
    (fx<=					i v $language)
    (fx>					i v $language)
    (fx>=					i v $language)
    (fx=					i v $language)
    (fxadd1					i v $language)
    (fxsub1					i v $language)
    (fxquotient					i v $language)
    (fxremainder				i v $language)
    (fxmodulo					i v $language)
    (fxsll					i v $language)
    (fxsra					i v $language)
    (sra					i v $language)
    (sll					i v $language)
    (fxlogand					i v $language)
    (fxlogxor					i v $language)
    (fxlogor					i v $language)
    (fxlognot					i v $language)
    (fixnum->string				i v $language)
    (string->flonum				i v $language)
    (add1					i v $language)
    (sub1					i v $language)
    (bignum?					i v $language)
    (ratnum?					i v $language)
    (compnum?					i v $language)
    (cflonum?					i v $language)
    (flonum-parts				i v $language)
    (flonum-bytes				i v $language)
    (quotient+remainder				i v $language)
    (flonum->string				i v $language)
    (random					i v $language)
    (gensym?					i v $language symbols)
    (getprop					i v $language symbols)
    (putprop					i v $language symbols)
    (remprop					i v $language symbols)
    (property-list				i v $language symbols)
    (gensym->unique-string			i v $language symbols)
    (symbol-bound?				i v $language symbols)
    (top-level-value				i v $language symbols)
    (reset-symbol-proc!				i v $language symbols)
    (make-guardian				i v $language)
    (port-mode					i v $language)
    (set-port-mode!				i v $language)
    (with-input-from-string			i v $language)
    (get-output-string				i v $language)
    (with-output-to-string			i v $language)
    (console-input-port				i v $language)
    (console-error-port				i v $language)
    (console-output-port			i v $language)
    (reset-input-port!				i v $language)
    (reset-output-port!				i v $language)
    (printf					i v $language)
    (fprintf					i v $language)
    (format					i v $language)
    (print-gensym				i v $language symbols)
    (print-graph				i v $language)
    (print-unicode				i v $language)
    (printer-integer-radix			i v $language)
    (unicode-printable-char?			i v $language)
    (gensym-count				i v $language symbols)
    (gensym-prefix				i v $language symbols)
    (make-parameter				i v $language parameters)
    (call/cf					i v $language)
    (print-error				i v $language)
    (interrupt-handler				i v $language)
    (engine-handler				i v $language)
    (assembler-output				i v $language)
    (optimizer-output				i v $language)
    (assembler-property-key			$codes)
    (new-cafe					i v $language)
    (waiter-prompt-string			i v $language)
    (readline-enabled?				i v $language)
    (readline					i v $language)
    (make-readline-input-port			i v $language)
    (expand-form-to-core-language		i v $language)
    (expand-library				i v $language)
    (expand-top-level				i v $language)
    (environment?				i v $language)
    (environment-symbols			i v $language)
    (time-and-gather				i v $language)
    (stats?					i v $language)
    (stats-user-secs				i v $language)
    (stats-user-usecs				i v $language)
    (stats-sys-secs				i v $language)
    (stats-sys-usecs				i v $language)
    (stats-real-secs				i v $language)
    (stats-real-usecs				i v $language)
    (stats-collection-id			i v $language)
    (stats-gc-user-secs				i v $language)
    (stats-gc-user-usecs			i v $language)
    (stats-gc-sys-secs				i v $language)
    (stats-gc-sys-usecs				i v $language)
    (stats-gc-real-secs				i v $language)
    (stats-gc-real-usecs			i v $language)
    (stats-bytes-minor				i v $language)
    (stats-bytes-major				i v $language)
    (time-it					i v $language)
    (verbose-timer				i v $language)
;;;
    (current-time				i v $language)
    (time?					i v $language)
    (time-second				i v $language)
    (time-nanosecond				i v $language)
    (time-gmt-offset				i v $language)
    (date-string				i v $language)
;;;
    (command-line-arguments			i v $language)
    (set-rtd-printer!				i v $language)
    (set-rtd-destructor!			i v $language)
    (struct?					i v $language)
    (make-struct-type				i v $language)
    (struct-type-name				i v $language)
    (struct-type-symbol				i v $language)
    (struct-type-field-names			i v $language)
    (struct-type-destructor			i v $language)
    (struct-constructor				i v $language)
    (struct-predicate				i v $language)
    (struct-field-accessor			i v $language)
    (struct-field-mutator			i v $language)
    (struct-length				i v $language)
    (struct-ref					i v $language)
    (struct-set!				i v $language)
    (struct-printer				i v $language)
    (struct-destructor				i v $language)
    (struct-name				i v $language)
    (struct-type-descriptor			i v $language)
    (struct-rtd					i v $language)
    (struct=?					i v $language)
    (struct-reset				i v $language)
    (struct-guardian-logger			i v $language)
    (struct-guardian-log			i v $language)
    ($struct-guardian				$structs)
    (code?					i v $language)
    (immediate?					i v $language)
    (pointer-value				i v $language)
;;;
    (apropos					i v $language)
    (installed-libraries			i v $language)
    (uninstall-library				i v $language)
    (library-path				i v $language)
    (library-extensions				i v $language)
    (current-primitive-locations		$boot)
    (boot-library-expand			$boot)
    (current-library-collection			$boot)
    (library-name				$boot)
    (find-library-by-name			$boot)
    ($car					$pairs)
    ($cdr					$pairs)
    ($set-car!					$pairs)
    ($set-cdr!					$pairs)
    ($memq					$lists)
    ($memv					$lists)
    ($char?					$chars)
    ($char=					$chars)
    ($char<					$chars)
    ($char>					$chars)
    ($char<=					$chars)
    ($char>=					$chars)
    ($char->fixnum				$chars)
    ($fixnum->char				$chars)
    ($make-string				$strings)
    ($string-ref				$strings)
    ($string-set!				$strings)
    ($string-length				$strings)
    ($make-bytevector				$bytes)
    ($bytevector-length				$bytes)
    ($bytevector-s8-ref				$bytes)
    ($bytevector-u8-ref				$bytes)
    ($bytevector-set!				$bytes)
    ($bytevector-ieee-double-native-ref		$bytes)
    ($bytevector-ieee-double-native-set!	$bytes)
    ($bytevector-ieee-double-nonnative-ref	$bytes)
    ($bytevector-ieee-double-nonnative-set!	$bytes)
    ($bytevector-ieee-single-native-ref		$bytes)
    ($bytevector-ieee-single-native-set!	$bytes)
    ($bytevector-ieee-single-nonnative-ref	$bytes)
    ($bytevector-ieee-single-nonnative-set!	$bytes)
    ($flonum-u8-ref				$flonums)
    ($make-flonum				$flonums)
    ($flonum-set!				$flonums)
    ($flonum-signed-biased-exponent		$flonums)
    ($flonum-rational?				$flonums)
    ($flonum-integer?				$flonums)
    ($fl+					$flonums)
    ($fl-					$flonums)
    ($fl*					$flonums)
    ($fl/					$flonums)
    ($fl=					$flonums)
    ($fl<					$flonums)
    ($fl<=					$flonums)
    ($fl>					$flonums)
    ($fl>=					$flonums)
;;;($flround					$flonums)
    ($fixnum->flonum				$flonums)
    ($flonum-sbe				$flonums)
    ($make-bignum				$bignums)
    ($bignum-positive?				$bignums)
    ($bignum-size				$bignums)
    ($bignum-byte-ref				$bignums)
    ($bignum-byte-set!				$bignums)
    ($make-ratnum				$rat)
    ($ratnum-n					$rat)
    ($ratnum-d					$rat)
    ($make-compnum				$comp)
    ($compnum-real				$comp)
    ($compnum-imag				$comp)
    ($make-cflonum				$comp)
    ($cflonum-real				$comp)
    ($cflonum-imag				$comp)
    ($make-vector				$vectors)
    ($vector-length				$vectors)
    ($vector-ref				$vectors)
    ($vector-set!				$vectors)
    ($fxzero?					$fx)
    ($fxadd1					$fx)
    ($fxsub1					$fx)
    ($fx>=					$fx)
    ($fx<=					$fx)
    ($fx>					$fx)
    ($fx<					$fx)
    ($fx=					$fx)
    ($fxsll					$fx)
    ($fxsra					$fx)
    ($fxquotient				$fx)
    ($fxmodulo					fx)
;;;($fxmodulo					$fx)
    ($int-quotient				$fx)
    ($int-remainder				$fx)
    ($fxlogxor					$fx)
    ($fxlogor					$fx)
    ($fxlognot					$fx)
    ($fxlogand					$fx)
    ($fx+					$fx)
    ($fx*					$fx)
    ($fx-					$fx)
    ($fxinthash					$fx)
    ($make-symbol				$symbols)
    ($symbol-unique-string			$symbols)
    ($symbol-value				$symbols)
    ($symbol-proc				$symbols)
    ($symbol-string				$symbols)
    ($symbol-plist				$symbols)
    ($set-symbol-value!				$symbols)
    ($set-symbol-proc!				$symbols)
    ($set-symbol-string!			$symbols)
    ($set-symbol-unique-string!			$symbols)
    ($set-symbol-plist!				$symbols)
    ($unintern-gensym				$symbols)
    ($init-symbol-value!)
    ($unbound-object?				$symbols)
    ($symbol-table-size				$symbols)
    ($log-symbol-table-status			$symbols)
;;;
    (base-rtd					$structs)
    ($struct-set!				$structs)
    ($struct-ref				$structs)
    ($struct-rtd				$structs)
    ($struct					$structs)
    ($make-struct				$structs)
    ($struct?					$structs)
    ($struct/rtd?				$structs)

;;; --------------------------------------------------------------------
;;; (ikarus system $pointers)
    ($pointer?					$pointers)
    ($pointer=					$pointers)

;;;
    ($closure-code				$codes)
    ($code->closure				$codes)
    ($code-reloc-vector				$codes)
    ($code-freevars				$codes)
    ($code-size					$codes)
    ($code-annotation				$codes)
    ($code-ref					$codes)
    ($code-set!					$codes)
    ($set-code-annotation!			$codes)
    (procedure-annotation			i v $language)
    ($make-annotated-procedure			$codes)
    ($annotated-procedure-annotation		$codes)
    ($cpref					$codes)
    ($make-tcbucket				$tcbuckets)
    ($tcbucket-key				$tcbuckets)
    ($tcbucket-val				$tcbuckets)
    ($tcbucket-next				$tcbuckets)
    ($set-tcbucket-val!				$tcbuckets)
    ($set-tcbucket-next!			$tcbuckets)
    ($set-tcbucket-tconc!			$tcbuckets)
    ($arg-list					$arg-list)
    ($collect-key				$arg-list)
    ($$apply					$stack)
    ($fp-at-base				$stack)
    ($primitive-call/cc				$stack)
    ($frame->continuation			$stack)
    ($current-frame				$stack)
    ($seal-frame-and-call			$stack)
    ($make-call-with-values-procedure		$stack)
    ($make-values-procedure			$stack)
    ($interrupted?				$interrupts)
    ($unset-interrupted!			$interrupts)
    ($swap-engine-counter!			$interrupts)
;;;
    (interrupted-condition?			i v $language)
    (make-interrupted-condition			i v $language)
    (source-position-condition?			i v $language)
    (make-source-position-condition		i v $language)
    (source-position-port-id			i v $language)
    (source-position-byte			i v $language)
    (source-position-character			i v $language)
    (source-position-line			i v $language)
    (source-position-column			i v $language)

    ($apply-nonprocedure-error-handler)
    ($incorrect-args-error-handler)
    ($multiple-values-error)
    ($debug)
    ($underflow-misaligned-error)
    (top-level-value-error)
    (car-error)
    (cdr-error)
    (fxadd1-error)
    (fxsub1-error)
    (cadr-error)
    (fx+-type-error)
    (fx+-types-error)
    (fx+-overflow-error)
    ($do-event)
    (do-overflow)
    (do-overflow-words)
    (do-vararg-overflow)
    (collect					i v $language)
    (collect-key				i v $language)
    (post-gc-hooks				i v $language)
    (register-to-avoid-collecting		i v $language)
    (forget-to-avoid-collecting			i v $language)
    (replace-to-avoid-collecting		i v $language)
    (retrieve-to-avoid-collecting		i v $language)
    (collection-avoidance-list			i v $language)
    (purge-collection-avoidance-list		i v $language)
    (do-stack-overflow)
    (make-promise)
    (make-traced-procedure			i v $language)
    (make-traced-macro				i v $language)
    (error@fx+)
    (error@fxarithmetic-shift-left)
    (error@fxarithmetic-shift-right)
    (error@fx*)
    (error@fx-)
    (error@add1)
    (error@sub1)
    (error@fxadd1)
    (error@fxsub1)
    (fasl-write					i v $language)
    (fasl-read					i v $language)
    (fasl-directory				i v $language)
    (fasl-path					i v $language)
    (fasl-search-path				i v $language)
    (lambda						i v r ba se ne)
    (and					i v r ba se ne)
    (begin					i v r ba se ne)
    (case					i v r ba se ne)
    (cond					i v r ba se ne)
    (define					i v r ba se ne)
    (define-syntax				i v r ba se ne)
    (define-fluid-syntax			i v $language)
    (identifier-syntax				i v r ba)
    (if						i v r ba se ne)
    (let					i v r ba se ne)
    (let*					i v r ba se ne)
    (let*-values				i v r ba)
    (let-syntax					i v r ba se ne)
    (let-values					i v r ba)
    (fluid-let-syntax				i v $language)
    (letrec					i v r ba se ne)
    (letrec*					i v r ba)
    (letrec-syntax				i v r ba se ne)
    (or						i v r ba se ne)
    (quasiquote					i v r ba se ne)
    (quote					i v r ba se ne)
    (set!					i v r ba se ne)
    (syntax-rules				i v r ba se ne)
    (unquote					i v r ba se ne)
    (unquote-splicing				i v r ba se ne)
    (<						i v r ba se)
    (<=						i v r ba se)
    (=						i v r ba se)
    (>						i v r ba se)
    (>=						i v r ba se)
    (+						i v r ba se)
    (-						i v r ba se)
    (*						i v r ba se)
    (/						i v r ba se)
    (abs					i v r ba se)
    (asin					i v r ba se)
    (acos					i v r ba se)
    (atan					i v r ba se)
    (sinh					i v $language)
    (cosh					i v $language)
    (tanh					i v $language)
    (asinh					i v $language)
    (acosh					i v $language)
    (atanh					i v $language)
    (angle					i v r ba se)
    (bignum->bytevector				i v $language)
    (bytevector->bignum				i v $language)
    (append					i v r ba se)
    (apply					i v r ba se)
    (assert					i v r ba)
    (assertion-error) ;empty?!?
    (assertion-violation			i v r ba)
    (boolean=?					i v r ba)
    (boolean?					i v r ba se)
    (car					i v r ba se)
    (cdr					i v r ba se)
    (caar					i v r ba se)
    (cadr					i v r ba se)
    (cdar					i v r ba se)
    (cddr					i v r ba se)
    (caaar					i v r ba se)
    (caadr					i v r ba se)
    (cadar					i v r ba se)
    (caddr					i v r ba se)
    (cdaar					i v r ba se)
    (cdadr					i v r ba se)
    (cddar					i v r ba se)
    (cdddr					i v r ba se)
    (caaaar					i v r ba se)
    (caaadr					i v r ba se)
    (caadar					i v r ba se)
    (caaddr					i v r ba se)
    (cadaar					i v r ba se)
    (cadadr					i v r ba se)
    (caddar					i v r ba se)
    (cadddr					i v r ba se)
    (cdaaar					i v r ba se)
    (cdaadr					i v r ba se)
    (cdadar					i v r ba se)
    (cdaddr					i v r ba se)
    (cddaar					i v r ba se)
    (cddadr					i v r ba se)
    (cdddar					i v r ba se)
    (cddddr					i v r ba se)
    (call-with-current-continuation		i v r ba se)
    (call/cc					i v r ba)
    (call-with-values				i v r ba se)
    (ceiling					i v r ba se)
    (char->integer				i v r ba se)
    (char<=?					i v r ba se)
    (char<?					i v r ba se)
    (char=?					i v r ba se)
    (char>=?					i v r ba se)
    (char>?					i v r ba se)
    (char?					i v r ba se)
    (complex?					i v r ba se)
    (cons					i v r ba se)
    (cos					i v r ba se)
    (denominator				i v r ba se)
    (div					i v r ba)
    (mod					i v r ba)
    (div-and-mod				i v r ba)
    (div0					i v r ba)
    (mod0					i v r ba)
    (div0-and-mod0				i v r ba)
    (dynamic-wind				i v r ba se)
    (eq?					i v r ba se)
    (neq?					i v $language)
    (equal?					i v r ba se)
    (eqv?					i v r ba se)
    (error					i v r ba)
    (warning					i v $language)
    (die					i v $language)
    (even?					i v r ba se)
    (exact					i v r ba)
    (exact-integer-sqrt				i v r ba)
    (exact?					i v r ba se)
    (exp					i v r ba se)
    (expt					i v r ba se)
    (finite?					i v r ba)
    (floor					i v r ba se)
    (for-each					i v r ba se)
    (gcd					i v r ba se)
    (imag-part					i v r ba se)
    (inexact					i v r ba)
    (inexact?					i v r ba se)
    (infinite?					i v r ba)
    (integer->char				i v r ba se)
    (integer-valued?				i v r ba)
    (integer?					i v r ba se)
    (lcm					i v r ba se)
    (length					i v r ba se)
    (list					i v r ba se)
    (list->string				i v r ba se)
    (list->vector				i v r ba se)
    (list-ref					i v r ba se)
    (list-tail					i v r ba se)
    (list?					i v r ba se)
    (log					i v r ba se)
    (magnitude					i v r ba se)
    (make-polar					i v r ba se)
    (make-rectangular				i v r ba se)
    ($make-rectangular				$comp)
    (make-string				i v r ba se)
    (make-vector				i v r ba se)
    (map					i v r ba se)
    (max					i v r ba se)
    (min					i v r ba se)
    (nan?					i v r ba)
    (negative?					i v r ba se)
    (not					i v r ba se)
    (null?					i v r ba se)
    (number->string				i v r ba se)
    (number?					i v r ba se)
    (numerator					i v r ba se)
    (odd?					i v r ba se)
    (pair?					i v r ba se)
    (positive?					i v r ba se)
    (procedure?					i v r ba se)
    (rational-valued?				i v r ba)
    (rational?					i v r ba se)
    (rationalize				i v r ba se)
    (real-part					i v r ba se)
    (real-valued?				i v r ba)
    (real?					i v r ba se)
    (reverse					i v r ba se)
    (round					i v r ba se)
    (sin					i v r ba se)
    (sqrt					i v r ba se)
    (string					i v r ba se)
    (string->list				i v r ba se)
    (string->number				i v r ba se)
    (string->symbol				i v symbols r ba se)
    (string-append				i v r ba se)
    (string-copy				i v r ba se)
    (string-for-each				i v r ba)
    (string-length				i v r ba se)
    (string-ref					i v r ba se)
    (string<=?					i v r ba se)
    (string<?					i v r ba se)
    (string=?					i v r ba se)
    (string>=?					i v r ba se)
    (string>?					i v r ba se)
    (string?					i v r ba se)
    (substring					i v r ba se)
    (string->latin1				i v $language)
    (latin1->string				i v $language)
    (string->ascii				i v $language)
    (ascii->string				i v $language)
    (symbol->string				i v symbols r ba se)
    (symbol=?					i v symbols r ba)
    (symbol?					i v symbols r ba se)
    (tan					i v r ba se)
    (truncate					i v r ba se)
    (values					i v r ba se)
    (vector					i v r ba se)
    (vector->list				i v r ba se)
    (vector-fill!				i v r ba se)
    (vector-for-each				i v r ba)
    (vector-length				i v r ba se)
    (vector-map					i v r ba)
    (vector-for-all				i v $language)
    (vector-exists				i v $language)
    (vector-ref					i v r ba se)
    (vector-set!				i v r ba se)
    (subvector					i v $language)
    (vector-append				i v $language)
    (vector-copy				i v $language)
    (vector-copy!				i v $language)
    (vector?					i v r ba se)
    (zero?					i v r ba se)
    (...					i v ne r ba sc se)
    (=>						i v ne r ba ex se)
    (_						i v ne r ba sc)
    (else					i v ne r ba ex se)
    (bitwise-arithmetic-shift			i v r bw)
    (bitwise-arithmetic-shift-left		i v r bw)
    (bitwise-arithmetic-shift-right		i v r bw)
    (bitwise-not				i v r bw)
    (bitwise-and				i v r bw)
    (bitwise-ior				i v r bw)
    (bitwise-xor				i v r bw)
    (bitwise-bit-count				i v r bw)
    (bitwise-bit-field				i v r bw)
    (bitwise-bit-set?				i v r bw)
    (bitwise-copy-bit				i v r bw)
    (bitwise-copy-bit-field			i v r bw)
    (bitwise-first-bit-set			i v r bw)
    (bitwise-if					i v r bw)
    (bitwise-length				i v r bw)
    (bitwise-reverse-bit-field			i v r bw)
    (bitwise-rotate-bit-field			i v r bw)
    (fixnum?					i v r fx)
    (fixnum-width				i v r fx)
    (least-fixnum				i v r fx)
    (greatest-fixnum				i v r fx)
    (fx*					i v r fx)
    (fx*/carry					i v r fx)
    (fx+					i v r fx)
    (fx+/carry					i v r fx)
    (fx-					i v r fx)
    (fx-/carry					i v r fx)
    (fx<=?					i v r fx)
    (fx<?					i v r fx)
    (fx=?					i v r fx)
    (fx>=?					i v r fx)
    (fx>?					i v r fx)
    (fxand					i v r fx)
    (fxarithmetic-shift				i v r fx)
    (fxarithmetic-shift-left			i v r fx)
    (fxarithmetic-shift-right			i v r fx)
    (fxbit-count				i v r fx)
    (fxbit-field				i v r fx)
    (fxbit-set?					i v r fx)
    (fxcopy-bit					i v r fx)
    (fxcopy-bit-field				i v r fx)
    (fxdiv					i v r fx)
    (fxdiv-and-mod				i v r fx)
    (fxdiv0					i v r fx)
    (fxdiv0-and-mod0				i v r fx)
    (fxeven?					i v r fx)
    (fxfirst-bit-set				i v r fx)
    (fxif					i v r fx)
    (fxior					i v r fx)
    (fxlength					i v r fx)
    (fxmax					i v r fx)
    (fxmin					i v r fx)
    (fxmod					i v r fx)
    (fxmod0					i v r fx)
    (fxnegative?				i v r fx)
    (fxnot					i v r fx)
    (fxodd?					i v r fx)
    (fxpositive?				i v r fx)
    (fxreverse-bit-field			i v r fx)
    (fxrotate-bit-field				i v r fx)
    (fxxor					i v r fx)
    (fxzero?					i v r fx)
    (fixnum->flonum				i v r fl)
    (fl*					i v r fl)
    (fl+					i v r fl)
    (fl-					i v r fl)
    (fl/					i v r fl)
    (fl<=?					i v r fl)
    (fl<?					i v r fl)
    (fl=?					i v r fl)
    (fl>=?					i v r fl)
    (fl>?					i v r fl)
    (flabs					i v r fl)
    (flacos					i v r fl)
    (flasin					i v r fl)
    (flatan					i v r fl)
    (flceiling					i v r fl)
    (flcos					i v r fl)
    (fldenominator				i v r fl)
    (fldiv					i v r fl)
    (fldiv-and-mod				i v r fl)
    (fldiv0					i v r fl)
    (fldiv0-and-mod0				i v r fl)
    (fleven?					i v r fl)
    (flexp					i v r fl)
    (flexpm1					i v $language)
    (flexpt					i v r fl)
    (flfinite?					i v r fl)
    (flfloor					i v r fl)
    (flinfinite?				i v r fl)
    (flinteger?					i v r fl)
    (fllog					i v r fl)
    (fllog1p					i v $language)
    (flmax					i v r fl)
    (flmin					i v r fl)
    (flmod					i v r fl)
    (flmod0					i v r fl)
    (flnan?					i v r fl)
    (flnegative?				i v r fl)
    (flnumerator				i v r fl)
    (flodd?					i v r fl)
    (flonum?					i v r fl)
    (flpositive?				i v r fl)
    (flround					i v r fl)
    (flsin					i v r fl)
    (flsqrt					i v r fl)
    (fltan					i v r fl)
    (fltruncate					i v r fl)
    (flzero?					i v r fl)
    (real->flonum				i v r fl)
    (bytevector->flonum				i v $language)
    (flonum->bytevector				i v $language)
    (make-no-infinities-violation		i v r fl)
    (make-no-nans-violation			i v r fl)
    (&no-infinities				i v r fl)
    (no-infinities-violation?			i v r fl)
    (&no-nans					i v r fl)
    (no-nans-violation?				i v r fl)
    (bytevector->sint-list			i v r bv)
    (bytevector->u8-list			i v r bv)
    (bytevector->s8-list			i v $language)
    (bytevector->u16l-list			i v $language)
    (bytevector->u16b-list			i v $language)
    (bytevector->u16n-list			i v $language)
    (bytevector->s16l-list			i v $language)
    (bytevector->s16b-list			i v $language)
    (bytevector->s16n-list			i v $language)
    (bytevector->u32l-list			i v $language)
    (bytevector->u32b-list			i v $language)
    (bytevector->u32n-list			i v $language)
    (bytevector->s32l-list			i v $language)
    (bytevector->s32b-list			i v $language)
    (bytevector->s32n-list			i v $language)
    (bytevector->u64l-list			i v $language)
    (bytevector->u64b-list			i v $language)
    (bytevector->u64n-list			i v $language)
    (bytevector->s64l-list			i v $language)
    (bytevector->s64b-list			i v $language)
    (bytevector->s64n-list			i v $language)
    (bytevector->uint-list			i v r bv)
    (bytevector->f4l-list			i v $language)
    (bytevector->f4b-list			i v $language)
    (bytevector->f4n-list			i v $language)
    (bytevector->f8l-list			i v $language)
    (bytevector->f8b-list			i v $language)
    (bytevector->f8n-list			i v $language)
    (bytevector->c4l-list			i v $language)
    (bytevector->c4b-list			i v $language)
    (bytevector->c4n-list			i v $language)
    (bytevector->c8l-list			i v $language)
    (bytevector->c8b-list			i v $language)
    (bytevector->c8n-list			i v $language)
    (bytevector-copy				i v r bv)
    (string-copy!				i v $language)
    (bytevector-copy!				i v r bv)
    (bytevector-fill!				i v r bv)
    (bytevector-ieee-double-native-ref		i v r bv)
    (bytevector-ieee-double-native-set!		i v r bv)
    (bytevector-ieee-double-ref			i v r bv)
    (bytevector-ieee-double-set!		i v r bv)
    (bytevector-ieee-single-native-ref		i v r bv)
    (bytevector-ieee-single-native-set!		i v r bv)
    (bytevector-ieee-single-ref			i v r bv)
    (bytevector-ieee-single-set!		i v r bv)
    (bytevector-length				i v r bv)
    (bytevector-s16-native-ref			i v r bv)
    (bytevector-s16-native-set!			i v r bv)
    (bytevector-s16-ref				i v r bv)
    (bytevector-s16-set!			i v r bv)
    (bytevector-s32-native-ref			i v r bv)
    (bytevector-s32-native-set!			i v r bv)
    (bytevector-s32-ref				i v r bv)
    (bytevector-s32-set!			i v r bv)
    (bytevector-s64-native-ref			i v r bv)
    (bytevector-s64-native-set!			i v r bv)
    (bytevector-s64-ref				i v r bv)
    (bytevector-s64-set!			i v r bv)
    (bytevector-s8-ref				i v r bv)
    (bytevector-s8-set!				i v r bv)
    (bytevector-sint-ref			i v r bv)
    (bytevector-sint-set!			i v r bv)
    (bytevector-u16-native-ref			i v r bv)
    (bytevector-u16-native-set!			i v r bv)
    (bytevector-u16-ref				i v r bv)
    (bytevector-u16-set!			i v r bv)
    (bytevector-u32-native-ref			i v r bv)
    (bytevector-u32-native-set!			i v r bv)
    (bytevector-u32-ref				i v r bv)
    (bytevector-u32-set!			i v r bv)
    (bytevector-u64-native-ref			i v r bv)
    (bytevector-u64-native-set!			i v r bv)
    (bytevector-u64-ref				i v r bv)
    (bytevector-u64-set!			i v r bv)
    (bytevector-u8-ref				i v r bv)
    (bytevector-u8-set!				i v r bv)
    (bytevector-uint-ref			i v r bv)
    (bytevector-uint-set!			i v r bv)
    (f4l-list->bytevector			i v $language)
    (f4b-list->bytevector			i v $language)
    (f4n-list->bytevector			i v $language)
    (f8l-list->bytevector			i v $language)
    (f8b-list->bytevector			i v $language)
    (f8n-list->bytevector			i v $language)
    (c4l-list->bytevector			i v $language)
    (c4b-list->bytevector			i v $language)
    (c4n-list->bytevector			i v $language)
    (c8l-list->bytevector			i v $language)
    (c8b-list->bytevector			i v $language)
    (c8n-list->bytevector			i v $language)
    (bytevector=?				i v r bv)
    (bytevector?				i v r bv)
    (subbytevector-u8				i v $language)
    (subbytevector-u8/count			i v $language)
    (subbytevector-s8				i v $language)
    (subbytevector-s8/count			i v $language)
    (bytevector-append				i v $language)
    (endianness					i v r bv)
    (native-endianness				i v r bv)
    (sint-list->bytevector			i v r bv)
    (string->utf16				i v r bv)
    (string->utf32				i v r bv)
    (string->utf8				i v r bv)
    (string->utf16le				i v $language)
    (string->utf16be				i v $language)
    (string->utf16n				i v $language)
    (u8-list->bytevector			i v r bv)
    (s8-list->bytevector			i v $language)
    (u16l-list->bytevector			i v $language)
    (u16b-list->bytevector			i v $language)
    (u16n-list->bytevector			i v $language)
    (s16l-list->bytevector			i v $language)
    (s16b-list->bytevector			i v $language)
    (s16n-list->bytevector			i v $language)
    (u32l-list->bytevector			i v $language)
    (u32b-list->bytevector			i v $language)
    (u32n-list->bytevector			i v $language)
    (s32l-list->bytevector			i v $language)
    (s32b-list->bytevector			i v $language)
    (s32n-list->bytevector			i v $language)
    (u64l-list->bytevector			i v $language)
    (u64b-list->bytevector			i v $language)
    (u64n-list->bytevector			i v $language)
    (s64l-list->bytevector			i v $language)
    (s64b-list->bytevector			i v $language)
    (s64n-list->bytevector			i v $language)
    (uint-list->bytevector			i v r bv)
    (utf8->string				i v r bv)
    (utf16->string				i v r bv)
    (utf16le->string				i v $language)
    (utf16n->string				i v $language)
    (utf16be->string				i v $language)
    (utf32->string				i v r bv)
    (print-condition				i v $language)
    (condition?					i v r co)
    (&assertion					i v r co)
    (assertion-violation?			i v r co)
    (&condition					i v r co)
    (condition					i v r co)
    (condition-accessor				i v r co)
    (condition-irritants			i v r co)
    (condition-message				i v r co)
    (condition-predicate			i v r co)
    (condition-who				i v r co)
    (define-condition-type			i v r co)
    (&error					i v r co)
    (error?					i v r co)
    (&implementation-restriction		i v r co)
    (implementation-restriction-violation?	i v r co)
    (&irritants					i v r co)
    (irritants-condition?			i v r co)
    (&lexical					i v r co)
    (lexical-violation?				i v r co)
    (make-assertion-violation			i v r co)
    (make-error					i v r co)
    (make-implementation-restriction-violation	i v r co)
    (make-irritants-condition			i v r co)
    (make-lexical-violation			i v r co)
    (make-message-condition			i v r co)
    (make-non-continuable-violation		i v r co)
    (make-serious-condition			i v r co)
    (make-syntax-violation			i v r co)
    (make-undefined-violation			i v r co)
    (make-violation				i v r co)
    (make-warning				i v r co)
    (make-who-condition				i v r co)
    (&message					i v r co)
    (message-condition?				i v r co)
    (&non-continuable				i v r co)
    (non-continuable-violation?			i v r co)
    (&serious					i v r co)
    (serious-condition?				i v r co)
    (simple-conditions				i v r co)
    (&syntax					i v r co)
    (syntax-violation-form			i v r co)
    (syntax-violation-subform			i v r co)
    (syntax-violation?				i v r co)
    (&undefined					i v r co)
    (undefined-violation?			i v r co)
    (&violation					i v r co)
    (violation?					i v r co)
    (&warning					i v r co)
    (warning?					i v r co)
    (&who					i v r co)
    (who-condition?				i v r co)
    (case-lambda				i v r ct)
    (do						i v r ct se ne)
    (unless					i v r ct)
    (when					i v r ct)
    (define-enumeration				i v r en)
    (enum-set->list				i v r en)
    (enum-set-complement			i v r en)
    (enum-set-constructor			i v r en)
    (enum-set-difference			i v r en)
    (enum-set-indexer				i v r en)
    (enum-set-intersection			i v r en)
    (enum-set-member?				i v r en)
    (enum-set-projection			i v r en)
    (enum-set-subset?				i v r en)
    (enum-set-union				i v r en)
    (enum-set-universe				i v r en)
    (enum-set=?					i v r en)
    (make-enumeration				i v r en)
    (enum-set?					i v $language)
    (environment				i v ev)
    (eval					i v ev se)
    (raise					i v r ex)
    (raise-continuable				i v r ex)
    (with-exception-handler			i v r ex)
    (guard					i v r ex)
    (binary-port?				i v r ip)
    (buffer-mode				i v r ip)
    (buffer-mode?				i v r ip)
    (bytevector->string				i v r ip)
    (call-with-bytevector-output-port		i v r ip)
    (call-with-port				i v r ip)
    (call-with-string-output-port		i v r ip)
    (assoc					i v r ls se)
    (assp					i v r ls)
    (assq					i v r ls se)
    (assv					i v r ls se)
    (cons*					i v r ls)
    (filter					i v r ls)
    (find					i v r ls)
    (fold-left					i v r ls)
    (fold-right					i v r ls)
    (for-all					i v r ls)
    (exists					i v r ls)
    (member					i v r ls se)
    (memp					i v r ls)
    (memq					i v r ls se)
    (memv					i v r ls se)
    (partition					i v r ls)
    (remq					i v r ls)
    (remp					i v r ls)
    (remv					i v r ls)
    (remove					i v r ls)
    (set-car!					i v mp se)
    (set-cdr!					i v mp se)
    (string-set!				i v ms se)
    (string-fill!				i v ms se)
    (command-line				i v r pr)
    (exit					i v r pr)
    (delay					i v r5 se ne)
    (exact->inexact				i v r5 se)
    (force					i v r5 se)
    (inexact->exact				i v r5 se)
    (modulo					i v r5 se)
    (remainder					i v r5 se)
    (null-environment				i v r5 se)
    (quotient					i v r5 se)
    (scheme-report-environment			i v r5 se)
    (interaction-environment			i v $language)
    (new-interaction-environment		i v $language)
    (close-port					i v r ip)
    (eol-style					i v r ip)
    (error-handling-mode			i v r ip)
    (file-options				i v r ip)
    (flush-output-port				i v r ip)
    (get-bytevector-all				i v r ip)
    (get-bytevector-n				i v r ip)
    (get-bytevector-n!				i v r ip)
    (get-bytevector-some			i v r ip)
    (get-char					i v r ip)
    (get-datum					i v r ip)
    (get-line					i v r ip)
    (read-line					i v $language)
    (get-string-all				i v r ip)
    (get-string-n				i v r ip)
    (get-string-n!				i v r ip)
    (get-u8					i v r ip)
    (&i/o					i v r ip is fi)
    (&i/o-decoding				i v r ip)
    (i/o-decoding-error?			i v r ip)
    (&i/o-encoding				i v r ip)
    (i/o-encoding-error-char			i v r ip)
    (i/o-encoding-error?			i v r ip)
    (i/o-error-filename				i v r ip is fi)
    (i/o-error-port				i v r ip is fi)
    (i/o-error-position				i v r ip is fi)
    (i/o-error?					i v r ip is fi)
    (&i/o-file-already-exists			i v r ip is fi)
    (i/o-file-already-exists-error?		i v r ip is fi)
    (&i/o-file-does-not-exist			i v r ip is fi)
    (i/o-file-does-not-exist-error?		i v r ip is fi)
    (&i/o-file-is-read-only			i v r ip is fi)
    (i/o-file-is-read-only-error?		i v r ip is fi)
    (&i/o-file-protection			i v r ip is fi)
    (i/o-file-protection-error?			i v r ip is fi)
    (&i/o-filename				i v r ip is fi)
    (i/o-filename-error?			i v r ip is fi)
    (&i/o-invalid-position			i v r ip is fi)
    (i/o-invalid-position-error?		i v r ip is fi)
    (&i/o-port					i v r ip is fi)
    (i/o-port-error?				i v r ip is fi)
    (&i/o-read					i v r ip is fi)
    (i/o-read-error?				i v r ip is fi)
    (&i/o-write					i v r ip is fi)
    (i/o-write-error?				i v r ip is fi)
    (&i/o-eagain				i v $language)
    (i/o-eagain-error?				i v $language)
    (&errno					i v $language)
    (errno-condition?				i v $language)
    (&h_errno					i v $language)
    (h_errno-condition?				i v $language)
    (lookahead-char				i v r ip)
    (lookahead-u8				i v r ip)
    (lookahead-two-u8				i v $language)
    (make-bytevector				i v r bv)
    (make-custom-binary-input-port		i v r ip)
    (make-custom-binary-output-port		i v r ip)
    (make-custom-textual-input-port		i v r ip)
    (make-custom-textual-output-port		i v r ip)
    (make-custom-binary-input/output-port	i v r ip)
    (make-custom-textual-input/output-port	i v r ip)
    (make-binary-file-descriptor-input-port	i v $language)
    (make-binary-file-descriptor-input-port*	i v $language)
    (make-binary-file-descriptor-output-port	i v $language)
    (make-binary-file-descriptor-output-port*	i v $language)
    (make-binary-file-descriptor-input/output-port	i v $language)
    (make-binary-file-descriptor-input/output-port*	i v $language)
    (make-binary-socket-input/output-port	i v $language)
    (make-binary-socket-input/output-port*	i v $language)
    (make-textual-file-descriptor-input-port	i v $language)
    (make-textual-file-descriptor-input-port*	i v $language)
    (make-textual-file-descriptor-output-port	i v $language)
    (make-textual-file-descriptor-output-port*	i v $language)
    (make-textual-file-descriptor-input/output-port	i v $language)
    (make-textual-file-descriptor-input/output-port*	i v $language)
    (make-textual-socket-input/output-port	i v $language)
    (make-textual-socket-input/output-port*	i v $language)
    (make-i/o-decoding-error			i v r ip)
    (make-i/o-encoding-error			i v r ip)
    (make-i/o-error				i v r ip is fi)
    (make-i/o-file-already-exists-error		i v r ip is fi)
    (make-i/o-file-does-not-exist-error		i v r ip is fi)
    (make-i/o-file-is-read-only-error		i v r ip is fi)
    (make-i/o-file-protection-error		i v r ip is fi)
    (make-i/o-filename-error			i v r ip is fi)
    (make-i/o-invalid-position-error		i v r ip is fi)
    (make-i/o-port-error			i v r ip is fi)
    (make-i/o-read-error			i v r ip is fi)
    (make-i/o-write-error			i v r ip is fi)
    (make-i/o-eagain				i v $language)
    (make-errno-condition			i v $language)
    (condition-errno				i v $language)
    (make-h_errno-condition			i v $language)
    (condition-h_errno				i v $language)
    (latin-1-codec				i v r ip)
    (make-transcoder				i v r ip)
    (native-eol-style				i v r ip)
    (native-transcoder				i v r ip)
    (transcoder?				i v $language)
    (open-bytevector-input-port			i v r ip)
    (open-bytevector-output-port		i v r ip)
    (open-file-input-port			i v r ip)
    (open-file-input/output-port		i v r ip)
    (open-file-output-port			i v r ip)
    (open-string-input-port			i v r ip)
    (open-string-output-port			i v r ip)
    (bytevector-port-buffer-size		i v $language)
    (string-port-buffer-size			i v $language)
    (input-file-buffer-size			i v $language)
    (output-file-buffer-size			i v $language)
    (input/output-file-buffer-size		i v $language)
    (input/output-socket-buffer-size		i v $language)
    (output-port-buffer-mode			i v r ip)
    (set-port-buffer-mode!			i v $language)
    (port-eof?					i v r ip)
    (port-has-port-position?			i v r ip)
    (port-has-set-port-position!?		i v r ip)
    (port-position				i v r ip)
    (get-char-and-track-textual-position	i v $language)
    (port-textual-position			i v $language)
    (port-transcoder				i v r ip)
    (port?					i v r ip)
    (put-bytevector				i v r ip)
    (put-char					i v r ip)
    (put-datum					i v r ip)
    (put-string					i v r ip)
    (put-u8					i v r ip)
    (set-port-position!				i v r ip)
    (standard-error-port			i v r ip)
    (standard-input-port			i v r ip)
    (standard-output-port			i v r ip)
    (string->bytevector				i v r ip)
    (textual-port?				i v r ip)
    (transcoded-port				i v r ip)
    (transcoder-codec				i v r ip)
    (transcoder-eol-style			i v r ip)
    (transcoder-error-handling-mode		i v r ip)
    (utf-8-codec				i v r ip)
    (utf-16-codec				i v r ip)
    (utf-16le-codec				i v $language)
    (utf-16be-codec				i v $language)
    (utf-16n-codec				i v $language)
    (utf-bom-codec				i v $language)
    (input-port?				i v r is ip se)
    (output-port?				i v r is ip se)
    (input/output-port?				i v)
    (current-input-port				i v r ip is se)
    (current-output-port			i v r ip is se)
    (current-error-port				i v r ip is)
    (eof-object					i v r ip is)
    (eof-object?				i v r ip is se)
    (close-input-port				i v r is se)
    (close-output-port				i v r is se)
    (display					i v r is se)
    (newline					i v r is se)
    (open-input-file				i v r is se)
    (open-output-file				i v r is se)
    (peek-char					i v r is se)
    (read					i v r is se)
    (read-char					i v r is se)
    (with-input-from-file			i v r is se)
    (with-output-to-file			i v r is se)
    (with-output-to-port			i v $language)
    (write					i v r is se)
    (write-char					i v r is se)
    (call-with-input-file			i v r is se)
    (call-with-output-file			i v r is se)
    (hashtable-clear!				i v r ht)
    (hashtable-contains?			i v r ht)
    (hashtable-copy				i v r ht)
    (hashtable-delete!				i v r ht)
    (hashtable-entries				i v r ht)
    (hashtable-keys				i v r ht)
    (hashtable-mutable?				i v r ht)
    (hashtable-ref				i v r ht)
    (hashtable-set!				i v r ht)
    (hashtable-size				i v r ht)
    (hashtable-update!				i v r ht)
    (hashtable?					i v r ht)
    (make-eq-hashtable				i v r ht)
    (make-eqv-hashtable				i v r ht)
    (hashtable-hash-function			i v r ht)
    (make-hashtable				i v r ht)
    (hashtable-equivalence-function		i v r ht)
    (equal-hash					i v r ht)
    (string-hash				i v r ht)
    (string-ci-hash				i v r ht)
    (symbol-hash				i v r ht)
    (list-sort					i v r sr)
    (vector-sort				i v r sr)
    (vector-sort!				i v r sr)
    (file-exists?				i v r fi)
    (delete-file				i v r fi)
    (define-record-type				i v r rs)
    (fields					i v r rs)
    (immutable					i v r rs)
    (mutable					i v r rs)
    (opaque					i v r rs)
    (parent					i v r rs)
    (parent-rtd					i v r rs)
    (protocol					i v r rs)
    (record-constructor-descriptor		i v r rs)
    (record-type-descriptor			i v r rs)
    (sealed					i v r rs)
    (nongenerative				i v r rs)
    (record-field-mutable?			i v r ri)
    (record-rtd					i v r ri)
    (record-type-field-names			i v r ri)
    (record-type-generative?			i v r ri)
    (record-type-name				i v r ri)
    (record-type-opaque?			i v r ri)
    (record-type-parent				i v r ri)
    (record-type-sealed?			i v r ri)
    (record-type-uid				i v r ri)
    (record?					i v r ri)
    (make-record-constructor-descriptor		i v r rp)
    (make-record-type-descriptor		i v r rp)
    (record-accessor				i v r rp)
    (record-constructor				i v r rp)
    (record-mutator				i v r rp)
    (record-predicate				i v r rp)
    (record-type-descriptor?			i v r rp)
    (syntax-violation				i v r sc)
    (bound-identifier=?				i v r sc)
    (datum->syntax				i v r sc)
    (syntax					i v r sc)
    (syntax->datum				i v r sc)
    (syntax-case				i v r sc)
    (unsyntax					i v r sc)
    (unsyntax-splicing				i v r sc)
    (quasisyntax				i v r sc)
    (with-syntax				i v r sc)
    (free-identifier=?				i v r sc)
    (generate-temporaries			i v r sc)
    (identifier?				i v r sc)
    (make-variable-transformer			i v r sc)
    (variable-transformer?			i v $language)
    (variable-transformer-procedure		i v $language)
    (make-compile-time-value			i v $language)
    (syntax-transpose				i v $language)
    (syntax-object?				i v $language)
    (syntax-object-expression			i v $language)
    (syntax-object-marks			i v $language)
    (syntax-object-substs			i v $language)
    (syntax-object-source-objects		i v $language)
    (char-alphabetic?				i v r uc se)
    (char-ci<=?					i v r uc se)
    (char-ci<?					i v r uc se)
    (char-ci=?					i v r uc se)
    (char-ci>=?					i v r uc se)
    (char-ci>?					i v r uc se)
    (char-downcase				i v r uc se)
    (char-foldcase				i v r uc)
    (char-titlecase				i v r uc)
    (char-upcase				i v r uc se)
    (char-general-category			i v r uc)
    (char-lower-case?				i v r uc se)
    (char-numeric?				i v r uc se)
    (char-title-case?				i v r uc)
    (char-upper-case?				i v r uc se)
    (char-whitespace?				i v r uc se)
    (string-ci<=?				i v r uc se)
    (string-ci<?				i v r uc se)
    (string-ci=?				i v r uc se)
    (string-ci>=?				i v r uc se)
    (string-ci>?				i v r uc se)
    (string-downcase				i v r uc)
    (string-foldcase				i v r uc)
    (string-normalize-nfc			i v r uc)
    (string-normalize-nfd			i v r uc)
    (string-normalize-nfkc			i v r uc)
    (string-normalize-nfkd			i v r uc)
    (string-titlecase				i v r uc)
    (string-upcase				i v r uc)
    (load					i v $language)
    (load-r6rs-script				i v $language)
    (void					i v $language $boot)
    (gensym					i v $language symbols $boot)
    (symbol-value				i v $language symbols $boot)
    (system-value				i v $language)
    (set-symbol-value!				i v $language symbols $boot)
    (eval-core					$boot)
    (current-core-eval				i v $language) ;;; temp
    (pretty-print				i v $language $boot)
    (pretty-print*				i v $language)
    (pretty-format				i v $language)
    (pretty-width				i v $language)
    (module					i v $language cm)
    (library					i v $language)
    (syntax-dispatch				)
    (syntax-error				i v $language)
    ($transcoder->data				$transc)
    ($data->transcoder				$transc)
    (make-file-options				i v $language)
;;;
    (port-id					i v $language)
    (port-fd					i v $language)
    (string->filename-func			i v $language)
    (filename->string-func			i v $language)
    (string->pathname-func			i v $language)
    (pathname->string-func			i v $language)
    (port-dump-status				i v $language)
    (port-closed?				i v $language)
;;; (ikarus system $io)
    ($make-port					$io)
    ($port-tag					$io)
    ($port-id					$io)
    ($port-cookie				$io)
    ($port-transcoder				$io)
    ($port-index				$io)
    ($port-size					$io)
    ($port-buffer				$io)
    ($port-get-position				$io)
    ($port-set-position!			$io)
    ($port-close				$io)
    ($port-read!				$io)
    ($port-write!				$io)
    ($set-port-index!				$io)
    ($set-port-size!				$io)
    ($port-attrs				$io)
    ($set-port-attrs!				$io)
;;;
    (get-annotated-datum			i v $language)
    (annotation?				i v $language)
    (annotation-expression			i v $language)
    (annotation-source				i v $language)
    (annotation-stripped			i v $language)
    (annotation-textual-position		i v $language)
;;;
    (&condition-rtd)
    (&condition-rcd)
    (&message-rtd)
    (&message-rcd)
    (&warning-rtd)
    (&warning-rcd)
    (&serious-rtd)
    (&serious-rcd)
    (&error-rtd)
    (&error-rcd)
    (&violation-rtd)
    (&violation-rcd)
    (&assertion-rtd)
    (&assertion-rcd)
    (&irritants-rtd)
    (&irritants-rcd)
    (&who-rtd)
    (&who-rcd)
    (&non-continuable-rtd)
    (&non-continuable-rcd)
    (&implementation-restriction-rtd)
    (&implementation-restriction-rcd)
    (&lexical-rtd)
    (&lexical-rcd)
    (&syntax-rtd)
    (&syntax-rcd)
    (&undefined-rtd)
    (&undefined-rcd)
    (&i/o-rtd)
    (&i/o-rcd)
    (&i/o-read-rtd)
    (&i/o-read-rcd)
    (&i/o-write-rtd)
    (&i/o-write-rcd)
    (&i/o-invalid-position-rtd)
    (&i/o-invalid-position-rcd)
    (&i/o-filename-rtd)
    (&i/o-filename-rcd)
    (&i/o-file-protection-rtd)
    (&i/o-file-protection-rcd)
    (&i/o-file-is-read-only-rtd)
    (&i/o-file-is-read-only-rcd)
    (&i/o-file-already-exists-rtd)
    (&i/o-file-already-exists-rcd)
    (&i/o-file-does-not-exist-rtd)
    (&i/o-file-does-not-exist-rcd)
    (&i/o-port-rtd)
    (&i/o-port-rcd)
    (&i/o-decoding-rtd)
    (&i/o-decoding-rcd)
    (&i/o-encoding-rtd)
    (&i/o-encoding-rcd)
    (&no-infinities-rtd)
    (&no-infinities-rcd)
    (&no-nans-rtd)
    (&no-nans-rcd)
    (&interrupted-rtd)
    (&interrupted-rcd)
    (&source-rtd)
    (&source-rcd)

;;; --------------------------------------------------------------------
;;; POSIX functions

    (strerror					i v $language)
    (errno->string				posix)
    (getenv					i v $language posix)
    (mkdir					posix)
    (mkdir/parents				posix)
    (real-pathname				posix)
    (file-modification-time			posix)
    (split-file-name				posix)
    (vicare-argv0				i v $language posix)
    (vicare-argv0-string			i v $language posix)

;;; --------------------------------------------------------------------
;;; (ikarus system $foreign)
    (errno					$for i v $language)
    (pointer?					$for i v $language)
    (null-pointer				$for i v $language)
    (pointer->integer				$for i v $language)
    (integer->pointer				$for i v $language)
    (pointer-clone				$for i v $language)
    (pointer-null?				$for i v $language)
    (pointer-diff				$for i v $language)
    (pointer-add				$for i v $language)
    (pointer-and-offset?			$for i v $language)
    (pointer=?					$for i v $language)
    (pointer<>?					$for i v $language)
    (pointer<?					$for i v $language)
    (pointer>?					$for i v $language)
    (pointer<=?					$for i v $language)
    (pointer>=?					$for i v $language)
    (set-pointer-null!				$for i v $language)
;;;
    (make-memory-block				$for i v $language)
    (make-memory-block/guarded			$for i v $language)
    (null-memory-block				$for i v $language)
    (memory-block?				$for i v $language)
    (memory-block?/non-null			$for i v $language)
    (memory-block?/not-null			$for i v $language)
    (memory-block-pointer			$for i v $language)
    (memory-block-size				$for i v $language)
    (memory-block-reset				$for i v $language)
;;;
    (&out-of-memory-error			$for i v $language)
    (make-out-of-memory-error			$for i v $language)
    (out-of-memory-error?			$for i v $language)
    (out-of-memory-error.old-pointer		$for i v $language)
    (out-of-memory-error.number-of-bytes	$for i v $language)
    (out-of-memory-error.clean?			$for i v $language)
    (malloc					$for i v $language)
    (realloc					$for i v $language)
    (calloc					$for i v $language)
    (guarded-malloc				$for i v $language)
    (guarded-realloc				$for i v $language)
    (guarded-calloc				$for i v $language)
    (malloc*					$for i v $language)
    (realloc*					$for i v $language)
    (calloc*					$for i v $language)
    (guarded-malloc*				$for i v $language)
    (guarded-realloc*				$for i v $language)
    (guarded-calloc*				$for i v $language)
    (free					$for i v $language)
    (memcpy					$for i v $language)
    (memcmp					$for i v $language)
    (memmove					$for i v $language)
    (memset					$for i v $language)
    (memory-copy				$for i v $language)
    (memory->bytevector				$for i v $language)
    (bytevector->memory				$for i v $language)
    (bytevector->guarded-memory			$for i v $language)
    (bytevector->memory*			$for i v $language)
    (bytevector->guarded-memory*		$for i v $language)
;;;
    (with-local-storage				$for i v $language)
;;;
    (bytevector->cstring			$for i v $language)
    (bytevector->guarded-cstring		$for i v $language)
    (cstring->bytevector			$for i v $language)
    (cstring16->bytevector			$for i v $language)
    (cstring16n->string				$for i v $language)
    (cstring16le->string			$for i v $language)
    (cstring16be->string			$for i v $language)
    (string->cstring				$for i v $language)
    (string->guarded-cstring			$for i v $language)
    (bytevector->cstring*			$for i v $language)
    (bytevector->guarded-cstring*		$for i v $language)
    (cstring->bytevector*			$for i v $language)
    (string->cstring*				$for i v $language)
    (string->guarded-cstring*			$for i v $language)
    (cstring->string				$for i v $language)
    (strlen					$for i v $language)
    (strcmp					$for i v $language)
    (strncmp					$for i v $language)
    (strdup					$for i v $language)
    (strndup					$for i v $language)
    (guarded-strdup				$for i v $language)
    (guarded-strndup				$for i v $language)
    (strdup*					$for i v $language)
    (strndup*					$for i v $language)
    (guarded-strdup*				$for i v $language)
    (guarded-strndup*				$for i v $language)

    (argv->bytevectors				$for i v $language)
    (argv-length				$for i v $language)
    (argv->strings				$for i v $language)
    (bytevectors->argv				$for i v $language)
    (bytevectors->argv*				$for i v $language)
    (bytevectors->guarded-argv			$for i v $language)
    (bytevectors->guarded-argv*			$for i v $language)
    (strings->argv				$for i v $language)
    (strings->argv*				$for i v $language)
    (strings->guarded-argv			$for i v $language)
    (strings->guarded-argv*			$for i v $language)

;;;
    (pointer-ref-c-uint8			$for i v $language)
    (pointer-ref-c-sint8			$for i v $language)
    (pointer-ref-c-uint16			$for i v $language)
    (pointer-ref-c-sint16			$for i v $language)
    (pointer-ref-c-uint32			$for i v $language)
    (pointer-ref-c-sint32			$for i v $language)
    (pointer-ref-c-uint64			$for i v $language)
    (pointer-ref-c-sint64			$for i v $language)
;;;
    (pointer-ref-c-signed-char			$for i v $language)
    (pointer-ref-c-signed-short			$for i v $language)
    (pointer-ref-c-signed-int			$for i v $language)
    (pointer-ref-c-signed-long			$for i v $language)
    (pointer-ref-c-signed-long-long		$for i v $language)
    (pointer-ref-c-unsigned-char		$for i v $language)
    (pointer-ref-c-unsigned-short		$for i v $language)
    (pointer-ref-c-unsigned-int			$for i v $language)
    (pointer-ref-c-unsigned-long		$for i v $language)
    (pointer-ref-c-unsigned-long-long		$for i v $language)
;;;
    (pointer-ref-c-float			$for i v $language)
    (pointer-ref-c-double			$for i v $language)
    (pointer-ref-c-pointer			$for i v $language)
;;;
    (pointer-ref-c-size_t			$for i v $language)
    (pointer-ref-c-ssize_t			$for i v $language)
    (pointer-ref-c-off_t			$for i v $language)
    (pointer-ref-c-ptrdiff_t			$for i v $language)
;;;
    (pointer-set-c-uint8!			$for i v $language)
    (pointer-set-c-sint8!			$for i v $language)
    (pointer-set-c-uint16!			$for i v $language)
    (pointer-set-c-sint16!			$for i v $language)
    (pointer-set-c-uint32!			$for i v $language)
    (pointer-set-c-sint32!			$for i v $language)
    (pointer-set-c-uint64!			$for i v $language)
    (pointer-set-c-sint64!			$for i v $language)
;;;
    (pointer-set-c-signed-char!			$for i v $language)
    (pointer-set-c-signed-short!		$for i v $language)
    (pointer-set-c-signed-int!			$for i v $language)
    (pointer-set-c-signed-long!			$for i v $language)
    (pointer-set-c-signed-long-long!		$for i v $language)
    (pointer-set-c-unsigned-char!		$for i v $language)
    (pointer-set-c-unsigned-short!		$for i v $language)
    (pointer-set-c-unsigned-int!		$for i v $language)
    (pointer-set-c-unsigned-long!		$for i v $language)
    (pointer-set-c-unsigned-long-long!		$for i v $language)
;;;
    (pointer-set-c-float!			$for i v $language)
    (pointer-set-c-double!			$for i v $language)
    (pointer-set-c-pointer!			$for i v $language)
;;;
    (pointer-set-c-size_t!			$for i v $language)
    (pointer-set-c-ssize_t!			$for i v $language)
    (pointer-set-c-off_t!			$for i v $language)
    (pointer-set-c-ptrdiff_t!			$for i v $language)
;;;
    (array-ref-c-uint8				$for i v $language)
    (array-ref-c-sint8				$for i v $language)
    (array-ref-c-uint16				$for i v $language)
    (array-ref-c-sint16				$for i v $language)
    (array-ref-c-uint32				$for i v $language)
    (array-ref-c-sint32				$for i v $language)
    (array-ref-c-uint64				$for i v $language)
    (array-ref-c-sint64				$for i v $language)
;;;
    (array-ref-c-signed-char			$for i v $language)
    (array-ref-c-unsigned-char			$for i v $language)
    (array-ref-c-signed-short			$for i v $language)
    (array-ref-c-unsigned-short			$for i v $language)
    (array-ref-c-signed-int			$for i v $language)
    (array-ref-c-unsigned-int			$for i v $language)
    (array-ref-c-signed-long			$for i v $language)
    (array-ref-c-unsigned-long			$for i v $language)
    (array-ref-c-signed-long-long		$for i v $language)
    (array-ref-c-unsigned-long-long		$for i v $language)
;;;
    (array-ref-c-float				$for i v $language)
    (array-ref-c-double				$for i v $language)
    (array-ref-c-pointer			$for i v $language)
;;;
    (array-ref-c-size_t				$for i v $language)
    (array-ref-c-ssize_t			$for i v $language)
    (array-ref-c-off_t				$for i v $language)
    (array-ref-c-ptrdiff_t			$for i v $language)
;;;
    (array-set-c-uint8!				$for i v $language)
    (array-set-c-sint8!				$for i v $language)
    (array-set-c-uint16!			$for i v $language)
    (array-set-c-sint16!			$for i v $language)
    (array-set-c-uint32!			$for i v $language)
    (array-set-c-sint32!			$for i v $language)
    (array-set-c-uint64!			$for i v $language)
    (array-set-c-sint64!			$for i v $language)
;;;
    (array-set-c-signed-char!			$for i v $language)
    (array-set-c-unsigned-char!			$for i v $language)
    (array-set-c-signed-short!			$for i v $language)
    (array-set-c-unsigned-short!		$for i v $language)
    (array-set-c-signed-int!			$for i v $language)
    (array-set-c-unsigned-int!			$for i v $language)
    (array-set-c-signed-long!			$for i v $language)
    (array-set-c-unsigned-long!			$for i v $language)
    (array-set-c-signed-long-long!		$for i v $language)
    (array-set-c-unsigned-long-long!		$for i v $language)
;;;
    (array-set-c-float!				$for i v $language)
    (array-set-c-double!			$for i v $language)
    (array-set-c-pointer!			$for i v $language)
;;;
    (array-set-c-size_t!			$for i v $language)
    (array-set-c-ssize_t!			$for i v $language)
    (array-set-c-off_t!				$for i v $language)
    (array-set-c-ptrdiff_t!			$for i v $language)
;;;
    (dlopen					$for)
    (dlerror					$for)
    (dlclose					$for)
    (dlsym					$for)
;;;
    (make-c-callout-maker			$for)
    (make-c-callout-maker/with-errno		$for)
    (make-c-callback-maker			$for)
    (free-c-callback				$for)

;;; --------------------------------------------------------------------

    (ellipsis-map)
    (optimize-cp				i v $language)
    (optimize-level				i v $language)
    (cp0-size-limit				i v $language)
    (cp0-effort-limit				i v $language)
    (tag-analysis-output			i v $language)
    (perform-tag-analysis			i v $language)
    (current-letrec-pass			i v $language)
    (host-info					i v $language)
    (debug-call)

;;; --------------------------------------------------------------------

    (symbol->keyword				i v $language)
    (keyword->symbol				i v $language)
    (keyword?					i v $language)
    (keyword=?					i v $language)
    (keyword-hash				i v $language)

;;; --------------------------------------------------------------------

    ($current-letrec-pass			$compiler)
    ($check-for-illegal-letrec			$compiler)
    ($source-optimizer-passes-count		$compiler)
    ($open-mvcalls				$compiler)

    ($compile-core-expr->code			$compiler)
    ($recordize					$compiler)
    ($optimize-direct-calls			$compiler)
    ($optimize-letrec				$compiler)
    ($source-optimize				$compiler)
    ($rewrite-references-and-assignments	$compiler)
    ($introduce-tags				$compiler)
    ($introduce-vars				$compiler)
    ($sanitize-bindings				$compiler)
    ($optimize-for-direct-jumps			$compiler)
    ($insert-global-assignments			$compiler)
    ($convert-closures				$compiler)
    ($optimize-closures/lift-codes		$compiler)
    ($alt-cogen					$compiler)
    ($assemble-sources				$compiler)

    ($introduce-primcalls			$compiler)
    ($eliminate-fix				$compiler)
    ($insert-engine-checks			$compiler)
    ($insert-stack-overflow-check		$compiler)
    ($specify-representation			$compiler)
    ($impose-calling-convention/evaluation-order $compiler)
    ($assign-frame-sizes			$compiler)
    ($color-by-chaitin				$compiler)
    ($flatten-codes				$compiler)

    ($unparse-recordized-code			$compiler)
    ($unparse-recordized-code/pretty		$compiler)
    ))


(define bootstrap-collection
  ;;A collection of LIBRARY  structures accessed through a closure.  The
  ;;LIBRARY structure type is defined in the psyntax modules.
  ;;
  ;;This  function works  somewhat like  a parameter  function; it  is a
  ;;closure   with  the  same   interface  of   the  ones   returned  by
  ;;MAKE-COLLECTION,  but it  has an  initial  value and  it checks  for
  ;;duplicates to avoid them.
  ;;
  ;;If the  function is called with  no arguments: it  returns the whole
  ;;collection, which is a list  of LIBRARY structures.  If the function
  ;;is  called  with one  argument:  such  argument  must be  a  LIBRARY
  ;;structure and it is added to the collection if not already there.
  ;;
  ;;The initial  value is a list  of LIBRARY structures  built by adding
  ;;all the  libraries in LIBRARY-LEGEND which are  marked as REQUIRED?.
  ;;Notice that such structures are built by FIND-LIBRARY-BY-NAME, which
  ;;means  that  the  libraries  marked  as REQUIRED?  must  be  already
  ;;installed in the boot image running this program.
  ;;
  ;;To add a REQUIRED? library to a  boot image: first we have to add an
  ;;entry to  LIBRARY-LEGEND marked as  VISIBLE?  and build  a temporary
  ;;boot image, then mark the entry as REQUIRED? and using the temporary
  ;;boot image build another boot  image which will have the new library
  ;;as REQUIRED?.
  ;;
  (let ((list-of-library-records
	 (let next-library-entry ((entries library-legend))
	   (define required?	cadddr)
	   (define library-name	cadr)
	   (cond ((null? entries)
		  '())
		 ((required? (car entries))
		  (cons (find-library-by-name (library-name (car entries)))
			(next-library-entry (cdr entries))))
		 (else
		  (next-library-entry (cdr entries)))))))
    (case-lambda
     (()
      list-of-library-records)
     ((x)
      (unless (memq x list-of-library-records)
	(set! list-of-library-records (cons x list-of-library-records)))))))


(define (make-system-data subst env)
  ;;SUBST  is  an  alist  representing  the  substitutions  of  all  the
  ;;libraries  in the  boot  image,  ENV is  an  alist representing  the
  ;;environment of all the libraries in the boot image.
  ;;
  (define who 'make-system-data)
  (define (macro-identifier? x)
    (and (assq x ikarus-system-macros) #t))
  (define (procedure-identifier? x)
    (not (macro-identifier? x)))
  (define (assq1 x ls)
    (let loop ((x x) (ls ls) (p #f))
      (cond ((null? ls)
	     p)
	    ((eq? x (caar ls))
	     (if p
		 (if (pair? p)
		     (if (eq? (cdr p) (cdar ls))
			 (loop x (cdr ls) p)
		       (loop x (cdr ls) 2))
		   (loop x (cdr ls) (+ p 1)))
	       (loop x (cdr ls) (car ls))))
	    (else
	     (loop x (cdr ls) p)))))

  (let ((export-subst    (make-collection))
        (export-env      (make-collection))
        (export-primlocs (make-collection)))

    (each-for ikarus-system-macros
      (lambda (x)
	(let ((name	(car  x))
	      (binding	(cadr x))
	      (label	(gensym)))
	  (export-subst (cons name label))
	  (export-env   (cons label binding)))))
    (each-for (map car identifier->library-map)
      (lambda (x)
	(when (procedure-identifier? x)
	  (cond ((assq x (export-subst))
		 (error who "ambiguous export" x))
		((assq1 x subst) =>
		 ;;Primitive  defined  (exported)  within  the  compiled
		 ;;libraries.
		 (lambda (p)
		   (unless (pair? p)
		     (error who "invalid exports" p x))
		   (let ((label (cdr p)))
		     (cond ((assq label env) =>
			    (lambda (p)
			      (let ((binding (cdr p)))
				(case (car binding)
				  ((global)
				   (export-subst (cons x label))
				   (export-env   (cons label (cons 'core-prim x)))
				   (export-primlocs (cons x (cdr binding))))
				  (else
				   (error who "invalid binding for identifier" p x))))))
			   (else
			    (error who "cannot find binding" x label))))))
		(else
		 ;;Core primitive with no backing definition, assumed to
		 ;;be defined in other strata of the system
;;;		 (fprintf (console-error-port) "undefined primitive ~s\n" x)
		 (let ((label (gensym)))
		   (export-subst (cons x label))
		   (export-env (cons label (cons 'core-prim x)))))))))

    (values (export-subst) (export-env) (export-primlocs))))


(define (build-system-library export-subst export-env primlocs)
  (define (main export-subst export-env primlocs)
    (let ((code `(library (ikarus primlocs)
		   (export) ;;; must be empty
		   (import (only (ikarus.symbols) system-value-gensym)
		     (only (psyntax library-manager)
			   install-library)
		     (only (ikarus.compiler)
			   current-primitive-locations)
		     (ikarus))
		   (let ((g system-value-gensym))
		     (for-each (lambda (x)
				 (putprop (car x) g (cdr x)))
		       ',primlocs)
		     (let ((proc (lambda (x) (getprop x g))))
		       (current-primitive-locations proc)))
		   ;;This evaluates to a spliced list of INSTALL-LIBRARY
		   ;;forms.
		   ,@(map build-install-library-form library-legend))))
      ;;Expand  the library in  CODE; we  know that  the EXPORT  form is
      ;;empty,  so  we  know  that  the  last  two  values  returned  by
      ;;BOOT-LIBRARY-EXPAND are empty.
      ;;
      (let-values (((name code empty-subst empty-env)
		    (boot-library-expand code)))
	(values name code))))

  (define (build-install-library-form legend-entry)
    (let* ((nickname	(car	legend-entry))
	   (name	(cadr	legend-entry))
	   (visible?	(caddr	legend-entry))
	   (id		(gensym))
	   (version	(if (eq? 'rnrs (car name)) '(6) '()))
	   (system-all?	(equal? name '(psyntax system $all)))
	   (env		(if system-all? export-env '()))
	   (subst	(if system-all?
			    export-subst
			  (get-export-subset nickname export-subst))))
      ;;Datums  embedded in  this  symbolic expression  are quoted  to
      ;;allow the sexp to be handed to EVAL (I guess; Marco Maggi, Aug
      ;;26, 2011).
      `(install-library ',id ',name ',version
			'() ;; import-libs
			'() ;; visit-libs
			'() ;; invoke-libs
			',subst ',env void void '#f '#f '#f '() ',visible? '#f)))

  (define (get-export-subset nickname subst)
    ;;Given  the alist  of  substitutions SUBST,  build  and return  the
    ;;subset  of  substitutions  corresponding  to  identifiers  in  the
    ;;library selected by NICKNAME.
    ;;
    (let loop ((ls subst))
      (if (null? ls)
	  '()
	(let ((x (car ls)))
	  (let ((name (car x)))
	    (cond ((assq name identifier->library-map)
		   => (lambda (q)
			(if (memq nickname (cdr q))
			    (cons x (loop (cdr ls)))
			  (loop (cdr ls)))))
		  (else ;not going to any library?
		   (loop (cdr ls)))))))))

  (main export-subst export-env primlocs))


(define (make-init-code)
  ;;The first  code to  run on  the system is  one that  initializes the
  ;;value  and  proc  fields  of  the  location  of  $init-symbol-value!
  ;;Otherwise,  all  subsequent  inits   to  any  global  variable  will
  ;;segfault.
  ;;
  (let ((proc	(gensym))
	(loc	(gensym))
	(label	(gensym))
	(sym	(gensym))
	(val	(gensym))
	(args	(gensym)))
    (values (list '(ikarus.init))
	    (list `((case-lambda
		     ((,proc) (,proc ',loc ,proc)))
		    (case-lambda
		     ((,sym ,val)
		      (begin
			((primitive $set-symbol-value!) ,sym ,val)
			(if ((primitive procedure?) ,val)
			    ((primitive $set-symbol-proc!) ,sym ,val)
			  ((primitive $set-symbol-proc!) ,sym
			   (case-lambda
			    (,args
			     ((primitive error) 'apply
			      (quote "not a procedure") ((primitive $symbol-value) ,sym)))))))))))
	    `(($init-symbol-value! . ,label))
	    `((,label . (global . ,loc))))))


(define (expand-all files)
  ;;Expand all the  libraries in FILES, which must be  a list of strings
  ;;representing  file  pathnames  under  SRC-DIR.
  ;;
  ;;Return  3  values:  the  list  of  library  specifications,  a  list
  ;;representing  all  the  code  forms  from  all  the  libraries,  the
  ;;EXPORT-LOCS.
  ;;
  ;;Notice that the  last code to be executed is the  one of the (ikarus
  ;;main)  library,  and  the one  before  it  is  the one  returned  by
  ;;BUILD-SYSTEM-LIBRARY.
  ;;
  (define (prune-subst subst env)
    ;;Remove all re-exported identifiers (those with labels in SUBST but
    ;;no binding in ENV).
    ;;
    (cond ((null? subst)
	   '())
	  ((not (assq (cdar subst) env))
	   (prune-subst (cdr subst) env))
	  (else
	   (cons (car subst) (prune-subst (cdr subst) env)))))

  ;;For each library: accumulate all the code in the CODE* variable, all
  ;;the substitutions in SUBST, the whole environment in ENV.
  (let-values (((name* code* subst env) (make-init-code)))
    (debug-printf "Expanding:\n")
    (for-each (lambda (file)
		(debug-printf " ~s\n" file)
		;;For each  library in the  file apply the  function for
		;;its side effects.
		(load (string-append src-dir "/" file)
		      (lambda (x)
			(let-values (((name code export-subst export-env)
				      (boot-library-expand x)))
			  (set! name* (cons name name*))
			  (set! code* (cons code code*))
			  (set! subst (append export-subst subst))
			  (set! env   (append export-env   env))))))
      files)
    ;;;(debug-printf "\n")
    (let-values (((export-subst export-env export-locs)
                  (make-system-data (prune-subst subst env) env)))
      (let-values (((name code) (build-system-library export-subst export-env export-locs)))
        (values (reverse (cons* (car name*) name (cdr name*)))
		(reverse (cons* (car code*) code (cdr code*)))
		export-locs)))))


;;;; Go!

;;Internal consistency check: verify that all the library nicknames used
;;in IDENTIFIER->LIBRARY-MAP are defined by LIBRARY-LEGEND.
;;
(for-each (lambda (x)
	    (for-each (lambda (x)
			(unless (assq x library-legend)
			  (error 'identifier->library-map "not in the libraries list" x)))
	      (cdr x)))
  identifier->library-map)

;;;(pretty-print/stderr (bootstrap-collection))

;;Perform the bootstrap process generating the boot image.
;;
(time-it "the entire bootstrap process"
  (lambda ()
    (let-values (((name* core* locs)
		  (time-it "macro expansion"
		    (lambda ()
		      (parameterize ((current-library-collection bootstrap-collection))
			(expand-all scheme-library-files))))))
      (current-primitive-locations (lambda (x)
;;;(pretty-print/stderr (list x (assq x locs)))
				     (cond ((assq x locs) => cdr)
					   (else
					    (error 'bootstrap "no location for primitive" x)))))
      (let ((port (open-file-output-port boot-file-name (file-options no-fail))))
	(time-it "code generation and serialization"
	  (lambda ()
	    (debug-printf "Compiling ")
	    (for-each (lambda (name core)
			(debug-printf " ~s" name)
			(compile-core-expr-to-port core port))
	      name*
	      core*)
	    (debug-printf "\n")))
	(close-output-port port)))))

;(print-missing-prims)

(fprintf (console-error-port) "Happy Happy Joy Joy\n")

;;; end of file
;;; Local Variables:
;;; eval: (put 'time-it 'scheme-indent-function 1)
;;; eval: (put 'each-for 'scheme-indent-function 1)
;;; End:
