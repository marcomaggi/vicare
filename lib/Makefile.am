## Process this file with automake to produce Makefile.in

EXTRA_DIST = \
	vicare/errno.sls.in			\
	vicare/platform-constants.sls.in	\
	vicare/platform/constants.sls.in	\
	vicare/words.sls.in			\
	vicare/installation-configuration.sls.in

dist_pkglibexec_SCRIPTS	= compile-all.sps

libvicaredir		= $(libdir)/vicare-scheme

nobase_dist_libvicare_DATA	= \
	vicare/arguments/validation.sls		\
	vicare/arguments/general-c-buffers.sls	\
	vicare/assembler/inspection.sls		\
	vicare/debugging/compiler.sls		\
	vicare/flonum-formatter.sls		\
	vicare/flonum-parser.sls		\
	vicare/include.sls			\
	vicare/infix.sls			\
	vicare/numerics/constants.sls		\
	vicare/keywords.sls			\
	vicare/parser-logic.sls			\
	vicare/syntactic-extensions.sls		\
	vicare/unsafe-capi.sls			\
	vicare/unsafe-operations.sls		\
	vicare/unsafe-unicode.sls		\
	vicare/platform/utilities.sls		\
	vicare/weak-hashtables.sls		\
	vicare/checks.sls			\
	\
	srfi/%3a0.sls					\
	srfi/%3a0/cond-expand.sls			\
	\
	srfi/%3a1.sls					\
	srfi/%3a1/lists.sls				\
	srfi/%3a1/srfi-1-reference.scm			\
	\
	srfi/%3a2.sls					\
	srfi/%3a2/and-let%2a.sls			\
	\
	srfi/%3a6.sls					\
	srfi/%3a6/basic-string-ports.sls		\
	srfi/%3a6/basic-string-ports/compat.vicare.sls	\
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
	srfi/%3a13/srfi-13.scm				\
	srfi/%3a13/strings.sls				\
	\
	srfi/%3a14.sls					\
	srfi/%3a14/char-sets.sls			\
	srfi/%3a14/srfi-14.scm				\
	\
	srfi/%3a16.sls					\
	srfi/%3a16/case-lambda.sls			\
	\
	srfi/%3a19.sls					\
	srfi/%3a19/time.sls				\
	srfi/%3a19/srfi-19.scm				\
	srfi/%3a19/srfi-19-test-suite.scm		\
	srfi/%3a19/time/compat.vicare.sls		\
	srfi/%3a19/time/not-implemented.sls		\
	\
	srfi/%3a23.sls					\
	srfi/%3a23/error.sls				\
	srfi/%3a23/error/tricks.sls			\
	\
	srfi/%3a25.sls					\
	srfi/%3a25/multi-dimensional-arrays.sls		\
	srfi/%3a25/arlib.scm				\
	srfi/%3a25/array.scm				\
	srfi/%3a25/ix-ctor.scm				\
	srfi/%3a25/list.scm				\
	srfi/%3a25/multi-dimensional-arrays/all.sls	\
	srfi/%3a25/multi-dimensional-arrays/arlib.sls	\
	srfi/%3a25/op-ctor.scm				\
	srfi/%3a25/test.scm				\
	\
	srfi/%3a26.sls					\
	srfi/%3a26/cut.sls				\
	\
	srfi/%3a27.sls					\
	srfi/%3a27/random-bits.sls			\
	srfi/%3a27/conftest.scm				\
	srfi/%3a27/mrg32k3a-a.scm			\
	srfi/%3a27/mrg32k3a.scm				\
	\
	srfi/%3a31.sls					\
	srfi/%3a31/rec.sls				\
	\
	srfi/%3a37.sls					\
	srfi/%3a37/args-fold.sls			\
	\
	srfi/%3a38.sls					\
	srfi/%3a38/with-shared-structure.vicare.sls	\
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
	srfi/%3a42/ec.scm				\
	srfi/%3a42/eager-comprehensions.sls		\
	srfi/%3a42/examples.scm				\
	\
	srfi/%3a43.sls					\
	srfi/%3a43/vectors.sls				\
	srfi/%3a43/vector-lib.scm			\
	\
	srfi/%3a45.sls					\
	srfi/%3a45/lazy.sls				\
	\
	srfi/%3a48.sls					\
	srfi/%3a48/intermediate-format-strings.sls			\
	srfi/%3a48/intermediate-format-strings/compat.vicare.sls	\
	\
	srfi/%3a61/cond.sls				\
	srfi/%3a61.sls					\
	\
	srfi/%3a64.sls					\
	srfi/%3a64/srfi-64-test.scm			\
	srfi/%3a64/testing.scm				\
	srfi/%3a64/testing.sls				\
	\
	srfi/%3a67/compare-procedures.sls		\
	srfi/%3a67/compare.scm				\
	srfi/%3a67/examples.scm				\
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
	srfi/private/check-arg.sls			\
	srfi/private/feature-cond.sls			\
	srfi/private/include/compat.vicare.sls		\
	srfi/private/include.sls			\
	srfi/private/let-opt.sls			\
	srfi/private/make-aliased-libraries.sps		\
	srfi/private/OS-id-features.sls			\
	srfi/private/platform-features.vicare.sls	\
	srfi/private/registry.sls			\
	srfi/private/vanish.sls

nobase_nodist_libvicare_DATA	= \
	vicare/errno.sls			\
	vicare/installation-configuration.sls	\
	vicare/platform-constants.sls		\
	vicare/platform/constants.sls		\
	vicare/platform/features.sls		\
	vicare/words.sls

if WANT_LIBFFI
nobase_dist_libvicare_DATA	+= vicare/ffi.sls
dist_pkglibexec_SCRIPTS		+= compile-ffi.sps
endif

if WANT_POSIX
nobase_dist_libvicare_DATA	+= vicare/posix.sls	\
				   vicare/simple-event-loop.sls
dist_pkglibexec_SCRIPTS		+= compile-posix.sps
endif

if WANT_GLIBC
nobase_dist_libvicare_DATA	+= vicare/glibc.sls
dist_pkglibexec_SCRIPTS		+= compile-glibc.sps
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

## --------------------------------------------------------------------

VICARE_FASL_DIRECTORY		= $(builddir)/fasl
VICARE_COMPILE_LIBRARY_PATH	= $(builddir):$(srcdir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)
VICARE_COMPILE_LD_PATH		= $(builddir)/../src/.libs

# The variable "user_flags"  is for the user to be  set on the command
# line of "make", for example "user_flags=--print-loaded-libraries".
VICARE_FLAGS	= -b $(VICARE_BOOT) --fasl-directory $(VICARE_FASL_DIRECTORY) $(user_flags)
if WANT_DEBUG
VICARE_FLAGS	+= --debug
endif

VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH)	\
		  VICARE_FASL_PATH=$(VICARE_COMPILE_FASL_PATH)		\
		  LD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH)
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

VICARE_INST	= $(bindir)/vicare
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE_INST)

.PHONY: vfasl-installed

vfasl:
	test -d $(VICARE_FASL_DIRECTORY) || $(MKDIR_P) $(VICARE_FASL_DIRECTORY)
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_LIBFFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-ffi.sps
endif
if WANT_POSIX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-posix.sps
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
if WANT_READLINE
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-readline.sps
endif
if WANT_CRE2
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps
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

### end of file
