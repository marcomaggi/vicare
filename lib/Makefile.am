## Process this file with automake to produce Makefile.in

EXTRA_DIST = \
	vicare/platform/configuration.sls.in	\
	vicare/platform/constants.sls.in	\
	vicare/platform/errno.sls.in		\
	vicare/platform/words.sls.in

dist_pkglibexec_SCRIPTS	= compile-all.sps

libvicaredir		= $(libdir)/vicare-scheme

nobase_dist_libvicare_DATA	= \
	vicare/unsafe/capi.sls						\
	vicare/unsafe/operations.sls					\
	vicare/unsafe/unicode.sls					\
	\
	vicare/platform/utilities.sls					\
	\
	vicare/language-extensions/cond-expand.sls			\
	vicare/language-extensions/cond-expand/OS-id-features.sls	\
	vicare/language-extensions/cond-expand/feature-cond.sls		\
	vicare/language-extensions/cond-expand/helpers.sls		\
	vicare/language-extensions/cond-expand/platform-features.sls	\
	vicare/language-extensions/cond-expand/configuration-features.sls \
	vicare/language-extensions/cond-expand/registry.sls		\
	\
	vicare/arguments/general-c-buffers.sls				\
	vicare/arguments/validation.sls					\
	\
	vicare/language-extensions/amb.sls				\
	vicare/language-extensions/ascii-chars.sls			\
	vicare/language-extensions/c-enumerations.sls			\
	vicare/language-extensions/callables.sls			\
	vicare/language-extensions/case-identifiers.sls			\
	vicare/language-extensions/comparisons.sls			\
	vicare/language-extensions/coroutines.sls			\
	vicare/language-extensions/custom-ports.sls			\
	vicare/language-extensions/define-record-extended.sls		\
	vicare/language-extensions/hooks.sls				\
	vicare/language-extensions/identifier-alists.sls		\
	vicare/language-extensions/identifier-properties.sls		\
	vicare/language-extensions/identifier-substitutions.sls		\
	vicare/language-extensions/increments.sls			\
	vicare/language-extensions/infix.sls				\
	vicare/language-extensions/makers.sls				\
	vicare/language-extensions/keywords.sls				\
	vicare/language-extensions/let-constants.sls			\
	vicare/language-extensions/loops.sls				\
	vicare/language-extensions/namespaces.sls			\
	vicare/language-extensions/sentinels.sls			\
	vicare/language-extensions/simple-match.sls			\
	vicare/language-extensions/streams.sls				\
	vicare/language-extensions/syntaxes.sls				\
	vicare/language-extensions/variables.sls			\
	\
	vicare/crypto/randomisations.sls				\
	vicare/crypto/randomisations/low.sls				\
	vicare/crypto/randomisations/blum-blum-shub.sls			\
	vicare/crypto/randomisations/borosh.sls				\
	vicare/crypto/randomisations/cmrg.sls				\
	vicare/crypto/randomisations/distributions.sls			\
	vicare/crypto/randomisations/lists.sls				\
	vicare/crypto/randomisations/marsaglia.sls			\
	vicare/crypto/randomisations/mersenne.sls			\
	vicare/crypto/randomisations/strings.sls			\
	vicare/crypto/randomisations/vectors.sls			\
	\
	vicare/numerics/constants.sls					\
	vicare/numerics/flonum-formatter.sls				\
	vicare/numerics/flonum-parser.sls				\
	\
	vicare/containers/auxiliary-syntaxes.sls			\
	vicare/containers/bytevectors.sls				\
	vicare/containers/bytevector-compounds.sls			\
	vicare/containers/bytevector-compounds/core.sls			\
	vicare/containers/bytevector-compounds/unsafe.sls		\
	vicare/containers/weak-hashtables.sls				\
	vicare/containers/object-properties.sls				\
	vicare/containers/lists/stx.sls					\
	vicare/containers/lists/low.sls					\
	vicare/containers/lists.sls					\
	vicare/containers/vectors/low.sls				\
	vicare/containers/vectors.sls					\
	vicare/containers/strings/low.sls				\
	vicare/containers/strings.sls					\
	vicare/containers/strings/rabin-karp.sls			\
	vicare/containers/knuth-morris-pratt.sls			\
	vicare/containers/char-sets.sls					\
	vicare/containers/char-sets/blocks.sls				\
	vicare/containers/char-sets/categories.sls			\
	vicare/containers/levenshtein.sls				\
	vicare/containers/one-dimension-co.sls				\
	vicare/containers/one-dimension-cc.sls				\
	vicare/containers/bytevectors/generic-low.sls			\
	vicare/containers/bytevectors/generic.sls			\
	vicare/containers/bytevectors/s8.sls				\
	vicare/containers/bytevectors/s8low.sls				\
	vicare/containers/bytevectors/u8.sls				\
	vicare/containers/bytevectors/u8low.sls				\
	vicare/containers/arrays.sls					\
	vicare/containers/stacks.sls					\
	vicare/containers/queues.sls					\
	\
	vicare/assembler/inspection.sls					\
	vicare/checks.sls						\
	vicare/debugging/compiler.sls					\
	vicare/parser-logic.sls						\
	\
	vicare/net/channels.sls						\
	\
	vicare/irregex.sls						\
	vicare/irregex/compat.vicare.sls				\
	vicare/pregexp.sls						\
	vicare/getopts.sls						\
	vicare/formations.sls						\
	\
	vicare/parser-tools/silex/action.l				\
	vicare/parser-tools/silex/action-l.sls				\
	vicare/parser-tools/silex/char-lexer-driver.sls			\
	vicare/parser-tools/silex/class.l				\
	vicare/parser-tools/silex/class-l.sls				\
	vicare/parser-tools/silex/code-lexer-driver.sls			\
	vicare/parser-tools/silex/input-system.sls			\
	vicare/parser-tools/silex/lexer.sls				\
	vicare/parser-tools/silex/macro.l				\
	vicare/parser-tools/silex/macro-l.sls				\
	vicare/parser-tools/silex/make-tables.sps			\
	vicare/parser-tools/silex/nested-comment.l			\
	vicare/parser-tools/silex/nested-comment-l.sls			\
	vicare/parser-tools/silex/regexp.l				\
	vicare/parser-tools/silex/regexp-l.sls				\
	vicare/parser-tools/silex/semantic.sls				\
	vicare/parser-tools/silex.sls					\
	vicare/parser-tools/silex/string.l				\
	vicare/parser-tools/silex/string-l.sls				\
	vicare/parser-tools/silex/tree-lexer-driver.sls			\
	vicare/parser-tools/silex/utilities.sls				\
	\
	vicare/parser-tools/silex/macros/identifier.macros.l		\
	vicare/parser-tools/silex/macros/string.macros.l		\
	vicare/parser-tools/silex/macros/unicode.macros.l		\
	vicare/parser-tools/silex/macros/whitespaces-and-comments.macros.l

nobase_nodist_libvicare_DATA	= \
	vicare/platform/configuration.sls	\
	vicare/platform/constants.sls		\
	vicare/platform/errno.sls		\
	vicare/platform/features.sls		\
	vicare/platform/words.sls

if WANT_SRFI
nobase_dist_libvicare_DATA	+= \
	srfi/%3a0.sls					\
	srfi/%3a0/cond-expand.sls			\
	\
	srfi/%3a1.sls					\
	srfi/%3a1/lists.sls				\
	\
	srfi/%3a2.sls					\
	srfi/%3a2/and-let%2a.sls			\
	\
	srfi/%3a6.sls					\
	srfi/%3a6/basic-string-ports.sls		\
	\
	srfi/%3a8.sls					\
	srfi/%3a8/receive.sls				\
	\
	srfi/%3a9.sls					\
	srfi/%3a9/records.sls				\
	\
	srfi/%3a11.sls					\
	srfi/%3a11/let-values.sls			\
	\
	srfi/%3a13.sls					\
	srfi/%3a13/strings.sls				\
	\
	srfi/%3a14.sls					\
	srfi/%3a14/char-sets.sls			\
	\
	srfi/%3a16.sls					\
	srfi/%3a16/case-lambda.sls			\
	\
	srfi/%3a19.sls					\
	srfi/%3a19/time.sls				\
	\
	srfi/%3a23.sls					\
	srfi/%3a23/error.sls				\
	\
	srfi/%3a25.sls					\
	srfi/%3a25/multi-dimensional-arrays.sls		\
	srfi/%3a25/arlib.scm				\
	srfi/%3a25/list.scm				\
	srfi/%3a25/multi-dimensional-arrays/arlib.sls	\
	\
	srfi/%3a26.sls					\
	srfi/%3a26/cut.sls				\
	\
	srfi/%3a27.sls					\
	srfi/%3a27/random-bits.sls			\
	\
	srfi/%3a28.sls					\
	srfi/%3a28/basic-format-strings.sls		\
	\
	srfi/%3a31.sls					\
	srfi/%3a31/rec.sls				\
	\
	srfi/%3a37.sls					\
	srfi/%3a37/args-fold.sls			\
	\
	srfi/%3a38.sls					\
	srfi/%3a38/with-shared-structure.sls		\
	\
	srfi/%3a39.sls					\
	srfi/%3a39/parameters.sls			\
	\
	srfi/%3a41.sls					\
	srfi/%3a41/streams.sls				\
	srfi/%3a41/streams/derived.sls			\
	srfi/%3a41/streams/primitive.sls		\
	\
	srfi/%3a42.sls					\
	srfi/%3a42/eager-comprehensions.sls		\
	\
	srfi/%3a43.sls					\
	srfi/%3a43/vectors.sls				\
	\
	srfi/%3a45.sls					\
	srfi/%3a45/lazy.sls				\
	\
	srfi/%3a48.sls					\
	srfi/%3a48/intermediate-format-strings.sls	\
	\
	srfi/%3a61/cond.sls				\
	srfi/%3a61.sls					\
	\
	srfi/%3a64.sls					\
	srfi/%3a64/testing.sls				\
	\
	srfi/%3a67/compare-procedures.sls		\
	srfi/%3a67.sls					\
	\
	srfi/%3a69/basic-hash-tables.sls		\
	srfi/%3a69.sls					\
	\
	srfi/%3a78.sls					\
	srfi/%3a78/lightweight-testing.sls		\
	\
	srfi/%3a98.sls					\
	srfi/%3a98/os-environment-variables.sls		\
	\
	srfi/%3a99.sls					\
	srfi/%3a99/records.sls				\
	srfi/%3a99/records/helper.sls			\
	srfi/%3a99/records/inspection.sls		\
	srfi/%3a99/records/procedural.sls		\
	srfi/%3a99/records/syntactic.sls		\
	\
	srfi/%3a101.sls					\
	srfi/%3a101/random-access-lists.sls		\
	srfi/%3a101/random-access-lists/procedures.sls	\
	srfi/%3a101/random-access-lists/syntax.sls	\
	srfi/%3a101/random-access-lists/equal.sls	\
	\
	srfi/%3a111.sls					\
	srfi/%3a111/boxes.sls				\
	\
	srfi/%3a112.sls					\
	srfi/%3a112/environment-inquiry.sls

dist_pkglibexec_SCRIPTS		+= compile-srfi.sps
endif

if WANT_SRFI
if WANT_POSIX
nobase_dist_libvicare_DATA	+= \
	srfi/%3a106.sls					\
	srfi/%3a106/socket.sls				\
	srfi/%3a106/compat.vicare.sls
dist_pkglibexec_SCRIPTS		+= compile-srfi-posix.sps
endif
endif

if WANT_NAUSICAA
EXTRA_DIST			+= \
	nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.l		\
	nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.l		\
	nausicaa/parser-tools/ip-addresses/make-tables.sps

nobase_dist_libvicare_DATA	+= \
	nausicaa/language/auxiliary-syntaxes.sls			\
	nausicaa/language/symbols-tree.sls				\
	\
	nausicaa/language/oopp.sls					\
	nausicaa/language/oopp/auxiliary-syntaxes.sls			\
	nausicaa/language/oopp/configuration.sls			\
	nausicaa/language/oopp/helpers.sls				\
	\
	nausicaa/language/multimethods.sls				\
	nausicaa/language/multimethods/auxiliary-syntaxes.sls		\
	nausicaa/language/multimethods/helpers-for-expand.sls		\
	nausicaa/language/multimethods/methods-table.sls		\
	nausicaa/language/multimethods/types.sls			\
	\
	nausicaa/language/builtins.sls					\
	nausicaa/language/conditions.sls				\
	nausicaa/language/increments.sls				\
	nausicaa/language/simple-match.sls				\
	nausicaa/language/infix.sls					\
	\
	nausicaa.sls							\
	\
	nausicaa/containers/lists.sls					\
	nausicaa/containers/vectors.sls					\
	nausicaa/containers/strings.sls					\
	nausicaa/containers/arrays.sls					\
	nausicaa/containers/stacks.sls					\
	nausicaa/containers/queues.sls					\
	nausicaa/containers/bitvectors.sls				\
	nausicaa/containers/iterators.sls				\
	\
	nausicaa/parser-tools/source-locations.sls			\
	nausicaa/parser-tools/lexical-tokens.sls			\
	nausicaa/parser-tools/silex/default-error-handler.sls		\
	nausicaa/parser-tools/lalr/lr-driver.sls			\
	nausicaa/parser-tools/lalr/glr-driver.sls			\
	nausicaa/parser-tools/lalr.sls					\
	\
	nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.sls	\
	nausicaa/parser-tools/ip-addresses/ipv4-address-parser.sls	\
	nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.sls	\
	nausicaa/parser-tools/ip-addresses/ipv6-address-parser.sls	\
	nausicaa/parser-tools/ipv4-addresses.sls			\
	nausicaa/parser-tools/ipv6-addresses.sls			\
	nausicaa/parser-tools/uri.sls					\
	\
	nausicaa/net/addresses/ipv4.sls					\
	nausicaa/net/addresses/ipv6.sls					\
	nausicaa/net/addresses/uri.sls

dist_pkglibexec_SCRIPTS		+= \
	compile-nausicaa-language.sps		\
	compile-nausicaa-containers.sps		\
	compile-nausicaa-parser-tools.sps	\
	compile-nausicaa-misc.sps
endif

if WANT_LIBFFI
nobase_dist_libvicare_DATA	+= \
	vicare/ffi.sls				\
	vicare/ffi/foreign-pointer-wrapper.sls
dist_pkglibexec_SCRIPTS		+= compile-ffi.sps
endif

if WANT_POSIX
nobase_dist_libvicare_DATA	+= \
	vicare/posix.sls			\
	vicare/posix/pid-files.sls		\
	vicare/posix/lock-pid-files.sls		\
	vicare/posix/log-files.sls		\
	vicare/posix/daemonisations.sls		\
	vicare/posix/simple-event-loop.sls	\
	vicare/posix/tcp-server-sockets.sls
dist_pkglibexec_SCRIPTS		+= compile-posix.sps
endif

if WANT_GLIBC
nobase_dist_libvicare_DATA	+= vicare/glibc.sls
dist_pkglibexec_SCRIPTS		+= compile-glibc.sps
endif

if WANT_LIBICONV
nobase_dist_libvicare_DATA	+= vicare/iconv.sls
dist_pkglibexec_SCRIPTS		+= compile-iconv.sps
endif

if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
nobase_dist_libvicare_DATA	+= vicare/gcc.sls
dist_pkglibexec_SCRIPTS		+= compile-gcc.sps
endif
endif
endif

if WANT_LINUX
nobase_dist_libvicare_DATA	+= vicare/linux.sls
dist_pkglibexec_SCRIPTS		+= compile-linux.sps
endif

if WANT_READLINE
nobase_dist_libvicare_DATA	+= vicare/readline.sls
dist_pkglibexec_SCRIPTS		+= compile-readline.sps
endif

if WANT_CRE2
nobase_dist_libvicare_DATA	+= vicare/cre2.sls
dist_pkglibexec_SCRIPTS		+= compile-cre2.sps
endif

#page

VICARE_FASL_DIRECTORY		= $(builddir)/fasl
VICARE_COMPILE_LIBRARY_PATH	= $(builddir):$(srcdir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)
VICARE_COMPILE_LD_PATH		= $(builddir)/../src/.libs

# The variable "VFLAGS" is for the user  to be set on the command line
# of "make" or "configure", for example:
#
#   make VFLAGS="-g -O2 --print-loaded-libraries"
#
VFLAGS		= @VFLAGS@

VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH);		export VICARE_LIBRARY_PATH;	\
		  VICARE_FASL_PATH=$(VICARE_COMPILE_FASL_PATH);			export VICARE_FASL_PATH;	\
		  SILEX_PATH=$(srcdir)/vicare/parser-tools/silex/macros;	export SILEX_PATH;		\
		  LD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH);			export LD_LIBRARY_PATH;
VICARE_FLAGS	= -b $(VICARE_BOOT) --fasl-directory $(VICARE_FASL_DIRECTORY) $(VFLAGS)
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

VICARE_INST	= $(bindir)/vicare
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH);			export VICARE_LIBRARY_PATH;
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE_INST)

.PHONY: vfasl vfasl-installed

vfasl:
	test -d $(VICARE_FASL_DIRECTORY) || $(MKDIR_P) $(VICARE_FASL_DIRECTORY)
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_SRFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-srfi.sps
endif
if WANT_NAUSICAA
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-language.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-containers.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-parser-tools.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-misc.sps
endif
if WANT_LIBFFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-ffi.sps
endif
if WANT_POSIX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-posix.sps
if WANT_SRFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-srfi-posix.sps
endif
endif
if WANT_GLIBC
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-glibc.sps
endif
if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-gcc.sps
endif
endif
endif
if WANT_LINUX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-linux.sps
endif
if WANT_LIBICONV
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-iconv.sps
endif
if WANT_READLINE
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-readline.sps
endif
if WANT_CRE2
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_SRFI
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-srfi.sps
endif
if WANT_NAUSICAA
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-language.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-containers.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-parser-tools.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-misc.sps
endif
if WANT_LIBFFI
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-ffi.sps
endif
if WANT_POSIX
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-posix.sps
endif
if WANT_GLIBC
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-glibc.sps
endif
if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-gcc.sps
endif
endif
endif
if WANT_LINUX
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-linux.sps
endif
if WANT_READLINE
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-readline.sps
endif
if WANT_CRE2
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

## --------------------------------------------------------------------

noinst_PROGRAMS		= features
features_SOURCES	= features.c

vicare/platform/features.sls : features
	$(MKDIR_P) vicare/platform && ./features >$@

## --------------------------------------------------------------------

CLEANFILES	= vicare/platform/features.sls

clean-local:
	-rm -rf $(VICARE_FASL_DIRECTORY)

#page
#### SILex lexers building

SILEX_SRCDIR		= $(srcdir)/vicare/parser-tools/silex
NAUIPADDR_SRCDIR	= $(srcdir)/nausicaa/parser-tools/ip-addresses

.PHONY: silex-internals silex-backup ip-address-tables

silex-internals:
	@echo; echo "*** Special rule: rebuilding SILex internals lexer tables"; echo
	@export SILEX_SRCDIR=$(SILEX_SRCDIR); $(VICARE_RUN) $(SILEX_SRCDIR)/make-tables.sps

silex-backup:
	@echo; echo "*** Special rule: backup of SILex internals lexer tables"; echo
	cp --verbose $(SILEX_SRCDIR)/*-l.sls $(SILEX_SRCDIR)/*.l $(SILEX_SRCDIR)/backup

ip-address-tables:
	@echo; echo "*** Special rule: rebuilding net libraries tables"; echo
	@export NAUIPADDR_SRCDIR=$(NAUIPADDR_SRCDIR); $(VICARE_RUN) $(NAUIPADDR_SRCDIR)/make-tables.sps

### end of file
