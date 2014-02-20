## Process this file with automake to produce Makefile.in

fasldir		= $(builddir)

EXTRA_DIST = \
	vicare/platform/configuration.sls.in				\
	vicare/platform/constants.sls.in				\
	vicare/platform/errno.sls.in					\
	vicare/platform/words.sls.in					\
	nausicaa/uri/pathnames.sls.in					\
	\
	vicare/parser-tools/silex/make-tables.sps			\
	vicare/parser-tools/silex/action.l				\
	vicare/parser-tools/silex/class.l				\
	vicare/parser-tools/silex/macro.l				\
	vicare/parser-tools/silex/nested-comment.l			\
	vicare/parser-tools/silex/regexp.l				\
	vicare/parser-tools/silex/string.l				\
	vicare/parser-tools/silex/macros/identifier.macros.l		\
	vicare/parser-tools/silex/macros/string.macros.l		\
	vicare/parser-tools/silex/macros/unicode.macros.l		\
	vicare/parser-tools/silex/macros/whitespaces-and-comments.macros.l

EXTRA_DIST += \
	compile-vicare-platform.sps					\
	compile-vicare-unsafe.sps					\
	compile-vicare-language-extensions.sps				\
	compile-vicare-crypto.sps					\
	compile-vicare-numerics.sps					\
	compile-vicare-containers.sps					\
	compile-vicare-parser-tools.sps					\
	compile-vicare-net.sps						\
	compile-vicare-misc.sps

libvicaredir		= $(libdir)/vicare-scheme

nobase_dist_libvicare_DATA	= \
	$(fasldir)/vicare/unsafe/capi.fasl				\
	$(fasldir)/vicare/unsafe/operations.fasl			\
	$(fasldir)/vicare/unsafe/unicode.fasl				\
	\
	$(fasldir)/vicare/platform/configuration.fasl			\
	$(fasldir)/vicare/platform/constants.fasl			\
	$(fasldir)/vicare/platform/errno.fasl				\
	$(fasldir)/vicare/platform/features.fasl			\
	$(fasldir)/vicare/platform/words.fasl				\
	$(fasldir)/vicare/platform/utilities.fasl			\
	\
	$(fasldir)/vicare/language-extensions/cond-expand.fasl		\
	$(fasldir)/vicare/language-extensions/cond-expand/OS-id-features.fasl	\
	$(fasldir)/vicare/language-extensions/cond-expand/feature-cond.fasl	\
	$(fasldir)/vicare/language-extensions/cond-expand/helpers.fasl	\
	$(fasldir)/vicare/language-extensions/cond-expand/platform-features.fasl	\
	$(fasldir)/vicare/language-extensions/cond-expand/configuration-features.fasl	\
	$(fasldir)/vicare/language-extensions/cond-expand/registry.fasl	\
	\
	$(fasldir)/vicare/arguments/general-c-buffers.fasl		\
	$(fasldir)/vicare/arguments/validation.fasl			\
	\
	$(fasldir)/vicare/language-extensions/amb.fasl			\
	$(fasldir)/vicare/language-extensions/ascii-chars/syntaxes.fasl	\
	$(fasldir)/vicare/language-extensions/ascii-chars.fasl		\
	$(fasldir)/vicare/language-extensions/c-enumerations.fasl	\
	$(fasldir)/vicare/language-extensions/callables.fasl		\
	$(fasldir)/vicare/language-extensions/comparisons.fasl		\
	$(fasldir)/vicare/language-extensions/coroutines.fasl		\
	$(fasldir)/vicare/language-extensions/custom-ports.fasl		\
	$(fasldir)/vicare/language-extensions/define-record-extended.fasl	\
	$(fasldir)/vicare/language-extensions/hooks.fasl		\
	$(fasldir)/vicare/language-extensions/identifier-alists.fasl	\
	$(fasldir)/vicare/language-extensions/identifier-properties.fasl	\
	$(fasldir)/vicare/language-extensions/identifier-substitutions.fasl	\
	$(fasldir)/vicare/language-extensions/increments.fasl		\
	$(fasldir)/vicare/language-extensions/infix/auxiliary-syntaxes.fasl	\
	$(fasldir)/vicare/language-extensions/infix/lr-driver.fasl	\
	$(fasldir)/vicare/language-extensions/infix/parser-table.fasl	\
	$(fasldir)/vicare/language-extensions/infix/parser-utils.fasl	\
	$(fasldir)/vicare/language-extensions/infix/tokens.fasl		\
	$(fasldir)/vicare/language-extensions/infix.fasl		\
	$(fasldir)/vicare/language-extensions/makers.fasl		\
	$(fasldir)/vicare/language-extensions/keywords.fasl		\
	$(fasldir)/vicare/language-extensions/loops.fasl		\
	$(fasldir)/vicare/language-extensions/namespaces.fasl		\
	$(fasldir)/vicare/language-extensions/sentinels.fasl		\
	$(fasldir)/vicare/language-extensions/simple-match.fasl		\
	$(fasldir)/vicare/language-extensions/streams.fasl		\
	$(fasldir)/vicare/language-extensions/syntaxes.fasl		\
	$(fasldir)/vicare/language-extensions/try.fasl			\
	$(fasldir)/vicare/language-extensions/variables.fasl		\
	\
	$(fasldir)/vicare/crypto/randomisations.fasl			\
	$(fasldir)/vicare/crypto/randomisations/low.fasl		\
	$(fasldir)/vicare/crypto/randomisations/blum-blum-shub.fasl	\
	$(fasldir)/vicare/crypto/randomisations/borosh.fasl		\
	$(fasldir)/vicare/crypto/randomisations/cmrg.fasl		\
	$(fasldir)/vicare/crypto/randomisations/distributions.fasl	\
	$(fasldir)/vicare/crypto/randomisations/lists.fasl		\
	$(fasldir)/vicare/crypto/randomisations/marsaglia.fasl		\
	$(fasldir)/vicare/crypto/randomisations/mersenne.fasl		\
	$(fasldir)/vicare/crypto/randomisations/strings.fasl		\
	$(fasldir)/vicare/crypto/randomisations/vectors.fasl		\
	\
	$(fasldir)/vicare/numerics/constants.fasl			\
	$(fasldir)/vicare/numerics/flonum-formatter.fasl		\
	$(fasldir)/vicare/numerics/flonum-parser.fasl			\
	\
	$(fasldir)/vicare/containers/auxiliary-syntaxes.fasl		\
	$(fasldir)/vicare/containers/bytevectors.fasl			\
	$(fasldir)/vicare/containers/bytevector-compounds.fasl		\
	$(fasldir)/vicare/containers/bytevector-compounds/core.fasl	\
	$(fasldir)/vicare/containers/bytevector-compounds/unsafe.fasl	\
	$(fasldir)/vicare/containers/weak-hashtables.fasl		\
	$(fasldir)/vicare/containers/object-properties.fasl		\
	$(fasldir)/vicare/containers/lists/stx.fasl			\
	$(fasldir)/vicare/containers/lists/low.fasl			\
	$(fasldir)/vicare/containers/lists.fasl				\
	$(fasldir)/vicare/containers/vectors/low.fasl			\
	$(fasldir)/vicare/containers/vectors.fasl			\
	$(fasldir)/vicare/containers/strings/low.fasl			\
	$(fasldir)/vicare/containers/strings.fasl			\
	$(fasldir)/vicare/containers/strings/rabin-karp.fasl		\
	$(fasldir)/vicare/containers/knuth-morris-pratt.fasl		\
	$(fasldir)/vicare/containers/char-sets.fasl			\
	$(fasldir)/vicare/containers/char-sets/blocks.fasl		\
	$(fasldir)/vicare/containers/char-sets/categories.fasl		\
	$(fasldir)/vicare/containers/levenshtein.fasl			\
	$(fasldir)/vicare/containers/one-dimension-co.fasl		\
	$(fasldir)/vicare/containers/one-dimension-cc.fasl		\
	$(fasldir)/vicare/containers/bytevectors/generic-low.fasl	\
	$(fasldir)/vicare/containers/bytevectors/generic.fasl		\
	$(fasldir)/vicare/containers/bytevectors/s8.fasl		\
	$(fasldir)/vicare/containers/bytevectors/s8low.fasl		\
	$(fasldir)/vicare/containers/bytevectors/u8.fasl		\
	$(fasldir)/vicare/containers/bytevectors/u8low.fasl		\
	$(fasldir)/vicare/containers/arrays.fasl			\
	$(fasldir)/vicare/containers/stacks.fasl			\
	$(fasldir)/vicare/containers/queues.fasl			\
	\
	$(fasldir)/vicare/assembler/inspection.fasl			\
	$(fasldir)/vicare/checks.fasl					\
	$(fasldir)/vicare/debugging/compiler.fasl			\
	$(fasldir)/vicare/parser-logic.fasl				\
	\
	$(fasldir)/vicare/net/channels.fasl				\
	\
	$(fasldir)/vicare/irregex.fasl					\
	$(fasldir)/vicare/irregex/compat.fasl			\
	$(fasldir)/vicare/pregexp.fasl					\
	$(fasldir)/vicare/getopts.fasl					\
	$(fasldir)/vicare/formations.fasl				\
	\
	$(fasldir)/vicare/parser-tools/silex/action-l.fasl		\
	$(fasldir)/vicare/parser-tools/silex/char-lexer-driver.fasl	\
	$(fasldir)/vicare/parser-tools/silex/class-l.fasl		\
	$(fasldir)/vicare/parser-tools/silex/code-lexer-driver.fasl	\
	$(fasldir)/vicare/parser-tools/silex/input-system.fasl		\
	$(fasldir)/vicare/parser-tools/silex/lexer.fasl			\
	$(fasldir)/vicare/parser-tools/silex/macro-l.fasl		\
	$(fasldir)/vicare/parser-tools/silex/nested-comment-l.fasl	\
	$(fasldir)/vicare/parser-tools/silex/regexp-l.fasl		\
	$(fasldir)/vicare/parser-tools/silex/semantic.fasl		\
	$(fasldir)/vicare/parser-tools/silex.fasl			\
	$(fasldir)/vicare/parser-tools/silex/string-l.fasl		\
	$(fasldir)/vicare/parser-tools/silex/tree-lexer-driver.fasl	\
	$(fasldir)/vicare/parser-tools/silex/utilities.fasl		\
	\
	$(fasldir)/vicare/parser-tools/unix-pathnames.fasl

if WANT_SRFI
EXTRA_DIST += \
	$(fasldir)/srfi/%3a25/arlib.scm					\
	$(fasldir)/srfi/%3a25/list.scm

libsrfidir			= $(libdir)/vicare-scheme

nobase_dist_libsrfi_DATA	= \
	$(fasldir)/srfi/%3a0.fasl					\
	$(fasldir)/srfi/%3a0/cond-expand.fasl				\
	\
	$(fasldir)/srfi/%3a1.fasl					\
	$(fasldir)/srfi/%3a1/lists.fasl					\
	\
	$(fasldir)/srfi/%3a2.fasl					\
	$(fasldir)/srfi/%3a2/and-let%2a.fasl				\
	\
	$(fasldir)/srfi/%3a6.fasl					\
	$(fasldir)/srfi/%3a6/basic-string-ports.fasl			\
	\
	$(fasldir)/srfi/%3a8.fasl					\
	$(fasldir)/srfi/%3a8/receive.fasl				\
	\
	$(fasldir)/srfi/%3a9.fasl					\
	$(fasldir)/srfi/%3a9/records.fasl				\
	\
	$(fasldir)/srfi/%3a11.fasl					\
	$(fasldir)/srfi/%3a11/let-values.fasl				\
	\
	$(fasldir)/srfi/%3a13.fasl					\
	$(fasldir)/srfi/%3a13/strings.fasl				\
	\
	$(fasldir)/srfi/%3a14.fasl					\
	$(fasldir)/srfi/%3a14/char-sets.fasl				\
	\
	$(fasldir)/srfi/%3a16.fasl					\
	$(fasldir)/srfi/%3a16/case-lambda.fasl				\
	\
	$(fasldir)/srfi/%3a19.fasl					\
	$(fasldir)/srfi/%3a19/time.fasl					\
	\
	$(fasldir)/srfi/%3a23.fasl					\
	$(fasldir)/srfi/%3a23/error.fasl				\
	\
	$(fasldir)/srfi/%3a25.fasl					\
	$(fasldir)/srfi/%3a25/multi-dimensional-arrays.fasl		\
	\
	$(fasldir)/srfi/%3a26.fasl					\
	$(fasldir)/srfi/%3a26/cut.fasl					\
	\
	$(fasldir)/srfi/%3a27.fasl					\
	$(fasldir)/srfi/%3a27/random-bits.fasl				\
	\
	$(fasldir)/srfi/%3a28.fasl					\
	$(fasldir)/srfi/%3a28/basic-format-strings.fasl			\
	\
	$(fasldir)/srfi/%3a31.fasl					\
	$(fasldir)/srfi/%3a31/rec.fasl					\
	\
	$(fasldir)/srfi/%3a37.fasl					\
	$(fasldir)/srfi/%3a37/args-fold.fasl				\
	\
	$(fasldir)/srfi/%3a38.fasl					\
	$(fasldir)/srfi/%3a38/with-shared-structure.fasl		\
	\
	$(fasldir)/srfi/%3a39.fasl					\
	$(fasldir)/srfi/%3a39/parameters.fasl				\
	\
	$(fasldir)/srfi/%3a41.fasl					\
	$(fasldir)/srfi/%3a41/streams.fasl				\
	$(fasldir)/srfi/%3a41/streams/derived.fasl			\
	$(fasldir)/srfi/%3a41/streams/primitive.fasl			\
	\
	$(fasldir)/srfi/%3a42.fasl					\
	$(fasldir)/srfi/%3a42/eager-comprehensions.fasl			\
	\
	$(fasldir)/srfi/%3a43.fasl					\
	$(fasldir)/srfi/%3a43/vectors.fasl				\
	\
	$(fasldir)/srfi/%3a45.fasl					\
	$(fasldir)/srfi/%3a45/lazy.fasl					\
	\
	$(fasldir)/srfi/%3a48.fasl					\
	$(fasldir)/srfi/%3a48/intermediate-format-strings.fasl		\
	\
	$(fasldir)/srfi/%3a61/cond.fasl					\
	$(fasldir)/srfi/%3a61.fasl					\
	\
	$(fasldir)/srfi/%3a64.fasl					\
	$(fasldir)/srfi/%3a64/testing.fasl				\
	\
	$(fasldir)/srfi/%3a67/compare-procedures.fasl			\
	$(fasldir)/srfi/%3a67.fasl					\
	\
	$(fasldir)/srfi/%3a69/basic-hash-tables.fasl			\
	$(fasldir)/srfi/%3a69.fasl					\
	\
	$(fasldir)/srfi/%3a78.fasl					\
	$(fasldir)/srfi/%3a78/lightweight-testing.fasl			\
	\
	$(fasldir)/srfi/%3a98.fasl					\
	$(fasldir)/srfi/%3a98/os-environment-variables.fasl		\
	\
	$(fasldir)/srfi/%3a99.fasl					\
	$(fasldir)/srfi/%3a99/records.fasl				\
	$(fasldir)/srfi/%3a99/records/helper.fasl			\
	$(fasldir)/srfi/%3a99/records/inspection.fasl			\
	$(fasldir)/srfi/%3a99/records/procedural.fasl			\
	$(fasldir)/srfi/%3a99/records/syntactic.fasl			\
	\
	$(fasldir)/srfi/%3a101.fasl					\
	$(fasldir)/srfi/%3a101/random-access-lists.fasl			\
	$(fasldir)/srfi/%3a101/random-access-lists/procedures.fasl	\
	$(fasldir)/srfi/%3a101/random-access-lists/syntax.fasl		\
	$(fasldir)/srfi/%3a101/random-access-lists/equal.fasl		\
	\
	$(fasldir)/srfi/%3a111.fasl					\
	$(fasldir)/srfi/%3a111/boxes.fasl				\
	\
	$(fasldir)/srfi/%3a112.fasl					\
	$(fasldir)/srfi/%3a112/environment-inquiry.fasl

EXTRA_DIST		+= compile-srfi.sps
endif

if WANT_SRFI
if WANT_POSIX
libsrfiposixdir			= $(libdir)/vicare-scheme
nobase_dist_libsrfiposix_DATA	= \
	$(fasldir)/srfi/%3a106.fasl					\
	$(fasldir)/srfi/%3a106/socket.fasl				\
	$(fasldir)/srfi/%3a106/compat.fasl
EXTRA_DIST		+= compile-srfi-posix.sps
endif
endif

if WANT_NAUSICAA
EXTRA_DIST			+= \
	nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.l		\
	nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.l		\
	nausicaa/parser-tools/ip-addresses/make-tables.sps

libnausicaadir			= $(libdir)/vicare-scheme

nobase_dist_libnausicaa_DATA	= \
	$(fasldir)/nausicaa/language/auxiliary-syntaxes.fasl		\
	$(fasldir)/nausicaa/language/symbols-tree.fasl			\
	\
	$(fasldir)/nausicaa/language/oopp.fasl				\
	$(fasldir)/nausicaa/language/oopp/auxiliary-syntaxes.fasl	\
	$(fasldir)/nausicaa/language/oopp/configuration.fasl		\
	$(fasldir)/nausicaa/language/oopp/conditions.fasl		\
	$(fasldir)/nausicaa/language/oopp/definition-parser-helpers.fasl \
	$(fasldir)/nausicaa/language/oopp/oopp-syntax-helpers.fasl	\
	\
	$(fasldir)/nausicaa/language/multimethods.fasl			\
	$(fasldir)/nausicaa/language/multimethods/auxiliary-syntaxes.fasl \
	$(fasldir)/nausicaa/language/multimethods/methods-table.fasl	\
	$(fasldir)/nausicaa/language/multimethods/types.fasl		\
	\
	$(fasldir)/nausicaa/language/builtins.fasl			\
	$(fasldir)/nausicaa/language/conditions.fasl			\
	$(fasldir)/nausicaa/language/increments.fasl			\
	$(fasldir)/nausicaa/language/simple-match.fasl			\
	$(fasldir)/nausicaa/language/infix.fasl				\
	\
	$(fasldir)/nausicaa.fasl					\
	\
	$(fasldir)/nausicaa/containers/lists.fasl			\
	$(fasldir)/nausicaa/containers/vectors.fasl			\
	$(fasldir)/nausicaa/containers/strings.fasl			\
	$(fasldir)/nausicaa/containers/arrays.fasl			\
	$(fasldir)/nausicaa/containers/stacks.fasl			\
	$(fasldir)/nausicaa/containers/queues.fasl			\
	$(fasldir)/nausicaa/containers/bitvectors.fasl			\
	$(fasldir)/nausicaa/containers/iterators.fasl			\
	\
	$(fasldir)/nausicaa/parser-tools/source-locations.fasl		\
	$(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl		\
	$(fasldir)/nausicaa/parser-tools/silex/default-error-handler.fasl \
	$(fasldir)/nausicaa/parser-tools/lalr/lr-driver.fasl		\
	$(fasldir)/nausicaa/parser-tools/lalr/glr-driver.fasl		\
	$(fasldir)/nausicaa/parser-tools/lalr.fasl			\
	\
	$(fasldir)/nausicaa/uri/ip.fasl					\
	$(fasldir)/nausicaa/uri.fasl					\
	$(fasldir)/nausicaa/uri/pathnames.fasl				\
	$(fasldir)/nausicaa/uri/pathnames/unix.fasl			\
	$(fasldir)/nausicaa/uri/pathnames/abstract.fasl			\
	\
	$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.fasl	\
	$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.fasl	\
	$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.fasl	\
	$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.fasl	\
	$(fasldir)/nausicaa/parser-tools/ipv4-addresses.fasl		\
	$(fasldir)/nausicaa/parser-tools/ipv6-addresses.fasl		\
	$(fasldir)/nausicaa/parser-tools/uri.fasl			\
	$(fasldir)/nausicaa/parser-tools/uri/utilities.fasl

nobase_dist_libnausicaa_DATA	+= \
	$(fasldir)/nausicaa/mehve.fasl					\
	$(fasldir)/nausicaa/mehve/language/numerics/predicates.fasl	\
	$(fasldir)/nausicaa/mehve/language/numerics/arithmetics.fasl	\
	$(fasldir)/nausicaa/mehve/language/numerics/parts.fasl		\
	$(fasldir)/nausicaa/mehve/language/numerics/transcendental.fasl	\
	$(fasldir)/nausicaa/mehve/language/infix.fasl			\
	$(fasldir)/nausicaa/mehve/language/input-output.fasl

EXTRA_DIST		+= \
	compile-nausicaa-language.sps		\
	compile-nausicaa-containers.sps		\
	compile-nausicaa-parser-tools.sps	\
	compile-nausicaa-misc.sps		\
	compile-mehve-language.sps
endif

if WANT_LIBFFI
libffidir			= $(libdir)/vicare-scheme
nobase_dist_libffi_DATA	= \
	$(fasldir)/vicare/ffi.fasl					\
	$(fasldir)/vicare/ffi/foreign-pointer-wrapper.fasl
EXTRA_DIST			+= compile-ffi.sps
endif

if WANT_POSIX
libposixdir			= $(libdir)/vicare-scheme
nobase_dist_libposix_DATA	= \
	$(fasldir)/vicare/posix.fasl					\
	$(fasldir)/vicare/posix/pid-files.fasl				\
	$(fasldir)/vicare/posix/lock-pid-files.fasl			\
	$(fasldir)/vicare/posix/log-files.fasl				\
	$(fasldir)/vicare/posix/daemonisations.fasl			\
	$(fasldir)/vicare/posix/simple-event-loop.fasl			\
	$(fasldir)/vicare/posix/tcp-server-sockets.fasl
EXTRA_DIST			+= compile-posix.sps
endif

if WANT_GLIBC
libglibcdir			= $(libdir)/vicare-scheme
nobase_dist_libglibc_DATA	= $(fasldir)/vicare/glibc.fasl
EXTRA_DIST			+= compile-glibc.sps
endif

if WANT_LIBICONV
libiconvdir			= $(libdir)/vicare-scheme
nobase_dist_libiconv_DATA	= $(fasldir)/vicare/iconv.fasl
EXTRA_DIST			+= compile-iconv.sps
endif

if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
libgccdir			= $(libdir)/vicare-scheme
nobase_dist_libgcc_DATA		= $(fasldir)/vicare/gcc.fasl
EXTRA_DIST			+= compile-gcc.sps
endif
endif
endif

if WANT_LINUX
liblinuxdir			= $(libdir)/vicare-scheme
nobase_dist_liblinux_DATA	= $(fasldir)/vicare/linux.fasl
EXTRA_DIST			+= compile-linux.sps
endif

if WANT_READLINE
libreadlinedir			= $(libdir)/vicare-scheme
nobase_dist_libreadline_DATA	= $(fasldir)/vicare/readline.fasl
EXTRA_DIST			+= compile-readline.sps
endif

if WANT_CRE2
libcre2dir			= $(libdir)/vicare-scheme
nobase_dist_libcre2_DATA	= $(fasldir)/vicare/cre2.fasl
EXTRA_DIST			+= compile-cre2.sps
endif

#page
## library compilation rules

NEW_BOOT_FILE			= $(builddir)/../scheme/vicare.boot
VICARE_FASL_DIRECTORY		= $(builddir)
VICARE_COMPILE_LIBRARY_PATH	= $(builddir):$(srcdir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)
VICARE_COMPILE_LD_PATH		= $(builddir)/../src/.libs

VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH);		export VICARE_LIBRARY_PATH;	\
		  VICARE_FASL_PATH=$(VICARE_COMPILE_FASL_PATH);			export VICARE_FASL_PATH;	\
		  SILEX_PATH=$(srcdir)/vicare/parser-tools/silex/macros;	export SILEX_PATH;		\
		  LD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH);			export LD_LIBRARY_PATH;		\
		  DYLD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH);			export DYLD_LIBRARY_PATH;
VICARE_FLAGS	= -b $(VICARE_BOOT) --fasl-directory $(VICARE_FASL_DIRECTORY)  $(AX_VFLAGS) $(VFLAGS)
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

VICARE_INST	= $(bindir)/vicare
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH);			export VICARE_LIBRARY_PATH;
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE_INST)

ALLFASL	= \
	$(nobase_dist_libvicare_DATA)		\
	$(nobase_dist_libsrfi_DATA)		\
	$(nobase_dist_libnausicaa_DATA)		\
	$(nobase_dist_libffi_DATA)		\
	$(nobase_dist_libposix_DATA)		\
	$(nobase_dist_libsrfiposix_DATA)	\
	$(nobase_dist_libglibc_DATA)		\
	$(nobase_dist_libgcc_DATA)		\
	$(nobase_dist_liblinux_DATA)		\
	$(nobase_dist_libiconv_DATA)		\
	$(nobase_dist_libreadline_DATA)		\
	$(nobase_dist_libcre2_DATA)

.PHONY: vfasl vfasl-installed

vfasl: $(ALLFASL)

$(nobase_dist_libvicare_DATA): $(NEW_BOOT_FILE)
	test -d $(VICARE_FASL_DIRECTORY) || $(MKDIR_P) $(VICARE_FASL_DIRECTORY)
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-platform.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-unsafe.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-language-extensions.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-crypto.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-numerics.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-containers.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-parser-tools.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-net.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-vicare-misc.sps

$(nobase_dist_libsrfi_DATA): $(NEW_BOOT_FILE)
if WANT_SRFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-srfi.sps
endif

$(nobase_dist_libnausicaa_DATA): $(NEW_BOOT_FILE)
if WANT_NAUSICAA
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-language.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-containers.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-parser-tools.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-misc.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-mehve-language.sps
endif

$(nobase_dist_libffi_DATA): $(NEW_BOOT_FILE)
if WANT_LIBFFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-ffi.sps
endif

$(nobase_dist_libposix_DATA): $(NEW_BOOT_FILE)
if WANT_POSIX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-posix.sps
endif

$(nobase_dist_libsrfiposix_DATA): $(NEW_BOOT_FILE)
if WANT_POSIX
if WANT_SRFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-srfi-posix.sps
endif
endif

$(nobase_dist_libglibc_DATA): $(NEW_BOOT_FILE)
if WANT_GLIBC
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-glibc.sps
endif

$(nobase_dist_libgcc_DATA): $(NEW_BOOT_FILE)
if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-gcc.sps
endif
endif
endif

$(nobase_dist_liblinux_DATA): $(NEW_BOOT_FILE)
if WANT_LINUX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-linux.sps
endif

$(nobase_dist_libiconv_DATA): $(NEW_BOOT_FILE)
if WANT_LIBICONV
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-iconv.sps
endif

$(nobase_dist_libreadline_DATA): $(NEW_BOOT_FILE)
if WANT_READLINE
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-readline.sps
endif

$(nobase_dist_libcre2_DATA): $(NEW_BOOT_FILE)
if WANT_CRE2
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-platform.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-unsafe.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-language-extensions.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-crypto.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-numerics.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-containers.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-parser-tools.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-net.sps
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-vicare-misc.sps
if WANT_SRFI
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-srfi.sps
endif
if WANT_NAUSICAA
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-language.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-containers.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-parser-tools.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa-misc.sps
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-mehve-language.sps
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

CLEANFILES	= vicare/platform/features.sls $(ALLFASL)

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
