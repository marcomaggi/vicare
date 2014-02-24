## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I autotools
EXTRA_DIST		= configure.sh configure32.sh configure64.sh	\
			  bare-configure.sh cygwin-configure.sh		\
			  scripts/make-last-revision			\
			  scripts/vie					\
			  autotools/config.rpath

fasldir			= $(builddir)/lib
instdir			= $(libdir)/vicare-scheme
testdir			= $(srcdir)/tests

CLEANFILES		=
MAINTAINERCLEANFILES	=
BUILT_SOURCES		=

#page
#### documentation

#AM_MAKEINFOFLAGS	= --no-split

EXTRA_DIST += \
	doc/QUEST-FOR-ISSUE-35				\
	doc/RFC/rfc4291.ipv6-address-achitecture.txt	\
	doc/RFC/rfc5735.ipv4-special-use-addresses.txt	\
	doc/RFC/rfc3986.uri.txt

dist_doc_DATA	= README COPYING GPL-3		\
		  LICENSE.irregex LICENSE.pregexp LICENSE.srfi

dist_man_MANS = doc/vicare.1

info_TEXINFOS		= \
	doc/vicare-scheme.texi		\
	doc/vicare-libs.texi		\
	doc/nausicaa.texi		\
	doc/mehve.texi

doc_vicare_scheme_TEXINFOS	= \
	doc/gpl-3.0.texi				\
	doc/macros.texi					\
	\
	doc/overview.texi				\
	doc/using.texi					\
	doc/scheme.texi					\
	doc/baselib.texi				\
	doc/stdlib.texi					\
	doc/exception-handlers-implementation.texi	\
	doc/iklib.texi					\
	doc/libutils.texi				\
	doc/ffi.texi					\
	doc/syslib.texi					\
	doc/capi.texi					\
	doc/objects.texi				\
	doc/debug.texi					\
	doc/machinery.texi				\
	doc/compiler.texi				\
	\
	doc/missing.texi				\
	doc/building.texi				\
	doc/credits.texi				\
	doc/unicode.texi				\
	doc/fasl.texi					\
	doc/shmem.texi					\
	doc/binfmt.texi					\
	doc/history.texi				\
	doc/references.texi

doc_vicare_libs_TEXINFOS = \
	doc/gpl-3.0.texi				\
	doc/macros.texi					\
	\
	doc/lang.texi					\
	doc/posix.texi					\
	doc/glibc.texi					\
	doc/iconv.texi					\
	doc/linux.texi					\
	doc/cre2.texi					\
	\
	doc/libs-arguments-validation.texi		\
	doc/libs-arrays.texi				\
	doc/libs-bytevectors.texi			\
	doc/libs-bytevector-compounds.texi		\
	doc/libs-cbuffers.texi				\
	doc/libs-char-sets.texi				\
	doc/libs-comparisons.texi			\
	doc/libs-debugging.texi				\
	doc/libs-flonum-format.texi			\
	doc/libs-flonum-parser.texi			\
	doc/libs-formations.texi			\
	doc/libs-getopts.texi				\
	doc/libs-gcc.texi				\
	doc/libs-irregex.texi				\
	doc/libs-knuth-morris-pratt.texi		\
	doc/libs-levenshtein.texi			\
	doc/libs-lists.texi				\
	doc/libs-loops.texi				\
	doc/libs-object-properties.texi			\
	doc/libs-one-dimension.texi			\
	doc/libs-parser-logic.texi			\
	doc/libs-platform.texi				\
	doc/libs-pregexp.texi				\
	doc/libs-queues.texi				\
	doc/libs-randomisations.texi			\
	doc/libs-readline.texi				\
	doc/libs-stacks.texi				\
	doc/libs-streams.texi				\
	doc/libs-strings.texi				\
	doc/libs-vectors.texi				\
	doc/libs-weak-hashtables.texi			\
	\
	doc/libs-parser-tools-silex.texi		\
	doc/libs-parser-tools-unix-pathnames.texi	\
	\
	doc/libs-net-channels.texi			\
	\
	doc/srfi.texi					\
	doc/srfi-and-let-star.texi			\
	doc/srfi-args-fold.texi				\
	doc/srfi-basic-format.texi			\
	doc/srfi-basic-socket-interface.texi		\
	doc/srfi-boxes.texi				\
	doc/srfi-case-lambda.texi			\
	doc/srfi-char-sets.texi				\
	doc/srfi-compare-procedures.texi		\
	doc/srfi-cond-expand.texi			\
	doc/srfi-eager-comp.texi			\
	doc/srfi-environment-variables.texi		\
	doc/srfi-environment-inquiry.texi		\
	doc/srfi-err5rs-records.texi			\
	doc/srfi-error-reporting.texi			\
	doc/srfi-general-cond.texi			\
	doc/srfi-intermediate-format.texi		\
	doc/srfi-hash-tables.texi			\
	doc/srfi-lazy.texi				\
	doc/srfi-let-values.texi			\
	doc/srfi-lightweight-testing.texi		\
	doc/srfi-list.texi				\
	doc/srfi-multidimensional-array.texi		\
	doc/srfi-parameters.texi			\
	doc/srfi-random.texi				\
	doc/srfi-rec.texi				\
	doc/srfi-receive.texi				\
	doc/srfi-records.texi				\
	doc/srfi-shared-structures.texi			\
	doc/srfi-specialize-procedures.texi		\
	doc/srfi-streams.texi				\
	doc/srfi-string-ports.texi			\
	doc/srfi-strings.texi				\
	doc/srfi-testing.texi				\
	doc/srfi-time.texi				\
	doc/srfi-vectors.texi				\
	doc/srfi-functional-pairs.texi

doc_nausicaa_TEXINFOS	= \
	doc/nausicaa-language.texi			\
	doc/nausicaa-oopp.texi				\
	doc/nausicaa-multimethods.texi			\
	doc/nausicaa-builtins.texi			\
	doc/nausicaa-conditions.texi			\
	doc/nausicaa-increments.texi			\
	doc/nausicaa-simple-match.texi			\
	doc/nausicaa-infix.texi				\
	\
	doc/nausicaa-containers-lists.texi		\
	doc/nausicaa-containers-vectors.texi		\
	doc/nausicaa-containers-strings.texi		\
	doc/nausicaa-containers-arrays.texi		\
	doc/nausicaa-containers-stacks.texi		\
	doc/nausicaa-containers-queues.texi		\
	doc/nausicaa-containers-bitvectors.texi		\
	doc/nausicaa-containers-iterators.texi		\
	\
	doc/nausicaa-parser-tools-source-locations.texi	\
	doc/nausicaa-parser-tools-lexical-tokens.texi	\
	doc/nausicaa-parser-tools-lalr.texi		\
	doc/nausicaa-parser-tools-ip-addresses.texi	\
	doc/nausicaa-parser-tools-uri.texi		\
	\
	doc/nausicaa-uri-uri.texi			\
	doc/nausicaa-uri-pathnames.texi			\
	\
	doc/macros.texi					\
	doc/fdl-1.3.texi				\
	doc/gpl-3.0.texi

doc_mehve_TEXINFOS	= \
	doc/mehve-language.texi				\
	\
	doc/macros.texi					\
	doc/fdl-1.3.texi				\
	doc/gpl-3.0.texi

#page
#### C language runtime executable

AM_CPPFLAGS	= -I $(srcdir)/src -I $(builddir)/src
AM_CFLAGS	= -Wall
EXTRA_DIST	+= src/last-revision.h src/vicare-platform.h.in

bin_PROGRAMS	= vicare
vicare_SOURCES	= \
	src/ikarus.c			\
	src/cpu_has_sse2.S		\
	src/ikarus-collect.c		\
	src/ikarus-enter.S		\
	src/ikarus-exec.c		\
	src/ikarus-fasl.c		\
	src/ikarus-ffi.c		\
	src/ikarus-flonums.c		\
	src/ikarus-getaddrinfo.c	\
	src/ikarus-io.c			\
	src/ikarus-main.c		\
	src/ikarus-numerics.c		\
	src/ikarus-objects.c		\
	src/ikarus-pointers.c		\
	src/ikarus-posix.c		\
	src/ikarus-print.c		\
	src/ikarus-runtime.c		\
	src/ikarus-symbol-table.c	\
	src/ikarus-verify-integrity.c	\
	src/ikarus-weak-pairs.c		\
	src/ikarus-winmmap.c		\
	src/ikarus-glibc.c		\
	src/ikarus-linux.c		\
	src/ikarus-readline.c		\
	src/ikarus-debugging.c		\
	src/last-revision.h		\
	src/internals.h

nodist_vicare_SOURCES	= bootfileloc.h
vicare_DEPENDENCIES	= src/vicare.h
include_HEADERS		= src/vicare.h src/vicare-platform.h

ikarus.$(OBJEXT) ikarus-main.$(OBJEXT): bootfileloc.h

MAINTAINERCLEANFILES	+= $(srcdir)/src/last-revision.h
CLEANFILES		+= bootfileloc.h

BUILT_SOURCES		+= $(srcdir)/src/last-revision.h bootfileloc.h

bootfileloc.h: Makefile
	echo '#define BOOTFILE "$(pkglibdir)/vicare.boot"' >$@
	echo '#define EXEFILE "$(bindir)/vicare"' >>$@

$(srcdir)/src/last-revision.h:
	echo '#define LAST_REVISION "no-branch/no-commit"' >$@

# List of flags to link libraries with the "vicare" executable.
vicare_LDADD		=

# Add Libiconv if requested by the configuration script.
if WANT_LIBICONV
vicare_LDADD		+= @LTLIBICONV@
endif

#page
#### additional C language libraries

# List of  library specifications:  tell GNU  Automake to  build these
# libraries, too.
lib_LTLIBRARIES			=

# Build   the  Vicare/CRE2   shared  library   if  requested   by  the
# configuration script.
if WANT_CRE2
lib_LTLIBRARIES			+= libvicarecre2.la
libvicarecre2_la_LDFLAGS	= -version-info 0:0:0
libvicarecre2_la_SOURCES	= src/ikarus-cre2.c
libvicarecre2_la_LIBADD		= @CRE2_LIBS@
endif

#page
#### compiling the boot image: source files

EXTRA_DIST += \
	$(VICARE_BOOT_SRCS)					\
	scheme/makefile.sps					\
	scheme/vicare.boot.4.prebuilt				\
	scheme/vicare.boot.8.prebuilt				\
	scheme/unicode-data.sls					\
	scheme/unicode/unicode-char-cases.ss			\
	scheme/unicode/unicode-charinfo.ss			\
	scheme/unicode/unicode-data.ss				\
	scheme/unicode/UNIDATA/CaseFolding.txt			\
	scheme/unicode/UNIDATA/CompositionExclusions.txt	\
	scheme/unicode/UNIDATA/NormalizationTest.txt		\
	scheme/unicode/UNIDATA/PropList.txt			\
	scheme/unicode/UNIDATA/SpecialCasing.txt		\
	scheme/unicode/UNIDATA/UnicodeData.txt			\
	scheme/unicode/UNIDATA/WordBreakProperty.txt

VICARE_BOOT_SRCS = \
	scheme/ikarus.wordsize.scm				\
	scheme/ikarus.compiler.sls				\
	scheme/ikarus.compiler.altcogen.ss			\
	scheme/ikarus.compiler.ontology.ss			\
	scheme/ikarus.compiler.optimize-letrec.ss		\
	scheme/ikarus.compiler.source-optimizer.ss		\
	scheme/ikarus.compiler.tag-annotation-analysis.ss	\
	scheme/pass-specify-rep-primops.ss			\
	scheme/pass-specify-rep.ss				\
	\
	scheme/ikarus.apply.sls					\
	scheme/ikarus.apropos.sls				\
	scheme/ikarus.bignums.sls				\
	scheme/ikarus.bytevectors.sls				\
	scheme/ikarus.cafe.sls					\
	scheme/ikarus.chars.sls					\
	scheme/ikarus.code-objects.sls				\
	scheme/ikarus.codecs.sls				\
	scheme/ikarus.collect.sls				\
	scheme/ikarus.compensations.sls				\
	scheme/ikarus.command-line.sls				\
	scheme/ikarus.conditions.sls				\
	scheme/ikarus.control.sls				\
	scheme/ikarus.debugger.sls				\
	scheme/ikarus.emergency.sls				\
	scheme/ikarus.enumerations.sls				\
	scheme/ikarus.environment-inquiry.sls			\
	scheme/ikarus.equal.sls					\
	scheme/ikarus.exceptions.sls				\
	scheme/ikarus.fasl.read.sls				\
	scheme/ikarus.fasl.write.sls				\
	scheme/ikarus.fixnums.sls				\
	scheme/ikarus.guardians.sls				\
	scheme/ikarus.handlers.sls				\
	scheme/ikarus.hash-tables.sls				\
	scheme/ikarus.keywords.sls				\
	scheme/ikarus.intel-assembler.sls			\
	scheme/ikarus.io.sls					\
	scheme/ikarus.library-utils.sls				\
	scheme/ikarus.lists.sls					\
	scheme/ikarus.load.sls					\
	scheme/ikarus.main.sls					\
	scheme/ikarus.multiple-values.sls			\
	scheme/ikarus.numerics.flonums.sls			\
	scheme/ikarus.numerics.generic-arithmetic.sls		\
	scheme/ikarus.numerics.flonum-conversion.sls		\
	scheme/ikarus.numerics.rationalize.sls			\
	scheme/ikarus.numerics.div-and-mod.sls			\
	scheme/ikarus.numerics.flonums.div-and-mod.sls		\
	scheme/ikarus.numerics.bitwise.misc.sls			\
	scheme/ikarus.numerics.complex-numbers.sls		\
	scheme/ikarus.options.sls				\
	scheme/ikarus.pairs.sls					\
	scheme/ikarus.pointers.sls				\
	scheme/ikarus.posix.sls					\
	scheme/ikarus.predicates.sls				\
	scheme/ikarus.pretty-formats.sls			\
	scheme/ikarus.pretty-print.sls				\
	scheme/ikarus.promises.sls				\
	scheme/ikarus.foreign-libraries.sls			\
	scheme/ikarus.ratnums.sls				\
	scheme/ikarus.reader.sls				\
	scheme/ikarus.readline.sls				\
	scheme/ikarus.records.procedural.sls			\
	scheme/ikarus.singular-objects.sls			\
	scheme/ikarus.sort.sls					\
	scheme/ikarus.string-to-number.sls 			\
	scheme/ikarus.strings.sls				\
	scheme/ikarus.strings-table.sls				\
	scheme/ikarus.structs.sls				\
	scheme/ikarus.symbol-table.sls				\
	scheme/ikarus.symbols.sls				\
	scheme/ikarus.syntax-utilities.sls			\
	scheme/ikarus.time-and-date.sls				\
	scheme/ikarus.timer.sls					\
	scheme/ikarus.unicode-conversion.sls			\
	scheme/ikarus.unicode.sls				\
	scheme/ikarus.vectors.sls				\
	scheme/ikarus.writer.sls				\
	\
	scheme/psyntax.builders.sls				\
	scheme/psyntax.compat.sls				\
	scheme/psyntax.config.sls				\
	scheme/psyntax.expander.sls				\
	scheme/psyntax.internal.sls				\
	scheme/psyntax.library-manager.sls

#page
#### compiling the boot image: build rules

VICARE_NEW_BOOT		= vicare.boot

# The  "exec" portion  of the  Makefile variable  names will  instruct
# Automake to  generate installation rules  to install the  boot image
# with the "make install-exec" rule.
bootexecdir		= $(pkglibdir)
bootexec_DATA		= $(VICARE_NEW_BOOT)

BOOT_SEARCH_PATH	= $(srcdir)/scheme:$(builddir)/scheme:$(srcdir)/lib:$(builddir)/lib:$(builddir)
VICARE_PREBUILT_BOOT	= $(srcdir)/scheme/vicare.boot.$(VICARE_SIZE_OF_VOIDP).prebuilt

VICARE_BOOT_ENV		= \
	VICARE_SRC_DIR=$(srcdir)/scheme; export VICARE_SRC_DIR; \
	VICARE_BUILD_DIR=$(builddir)/scheme; export VICARE_BUILD_DIR; \
	VICARE_LIBRARY_PATH=$(BOOT_SEARCH_PATH); export VICARE_LIBRARY_PATH;

CLEANFILES		+= ikarus.config.ss $(VICARE_NEW_BOOT)

## --------------------------------------------------------------------

ikarus.config.ss: Makefile
	echo '(define vicare-version "$(PACKAGE_VERSION)")' >$@
	echo '(define vicare-lib-dir "$(pkglibdir)")' >>$@
	echo '(define scheme-lib-dir "$(libdir)/scheme/r6rs")' >>$@
	echo '(define (host-info) "$(target)")' >>$@
	echo '(define target-os-uid (quote $(VICARE_TARGET_OS_UID)))' >>$@
	echo '(define wordsize $(VICARE_SIZE_OF_VOIDP))' >>$@
	echo '(define platform-endianness (quote $(VICARE_PLATFORM_ENDIANNESS)))' >>$@
	echo '(define arguments-validation #$(VICARE_ARGUMENTS_VALIDATION))' >>$@
	echo '(define bootfile "$(pkglibdir)/vicare.boot")' >>$@
	echo '(define VICARE_BUILT_WITH_SRFI_ENABLED     #$(VICARE_BUILT_WITH_SRFI_ENABLED))' >>$@
	echo '(define VICARE_BUILT_WITH_ICONV_ENABLED    #$(VICARE_BUILT_WITH_ICONV_ENABLED))' >>$@
	echo '(define VICARE_BUILT_WITH_FFI_ENABLED      #$(VICARE_BUILT_WITH_FFI_ENABLED))' >>$@
	echo '(define VICARE_BUILT_WITH_NAUSICAA_ENABLED #$(VICARE_BUILT_WITH_NAUSICAA_ENABLED))' >>$@
	echo '(define VICARE_BUILT_WITH_POSIX_ENABLED    #$(VICARE_BUILT_WITH_POSIX_ENABLED))' >>$@
	echo '(define VICARE_BUILT_WITH_GLIBC_ENABLED    #$(VICARE_BUILT_WITH_GLIBC_ENABLED))' >>$@
	echo '(define VICARE_BUILT_WITH_LINUX_ENABLED    #$(VICARE_BUILT_WITH_LINUX_ENABLED))' >>$@

# To  create the  new boot  image we  use a  prebuilt boot  image; the
# environment variable names are  hardcoded in the prebuilt boot image
# and are the Vicare's ones.
#
# Notice that VICARE_FASL_DIRECTORY  is set to the  empty string: this
# is because  the fasl  files are  not generated  by the  prebuilt and
# distributed boot image, and so they cannot be used here.
#
# The boot image is  "special": when building it we do  not use any of
# the command  line options selected  by AX_VFLAGS and  VFLAGS; rather
# all the options are set in the script "makefile.sps".
#
$(VICARE_NEW_BOOT): $(VICARE_BOOT_SRCS) vicare ikarus.config.ss
	$(VICARE_BOOT_ENV) $(GDB) vicare -b $(VICARE_PREBUILT_BOOT) \
		--r6rs-script $(srcdir)/scheme/makefile.sps

## --------------------------------------------------------------------

.PHONY: boot-image-32-bit boot-image-64-bit

boot-image-32-bit: vicare
	echo '(define vicare-version "$(PACKAGE_VERSION)")'				>ikarus.config.ss
	echo '(define vicare-lib-dir "$(pkglibdir)")'					>>ikarus.config.ss
	echo '(define scheme-lib-dir "$(libdir)/scheme/r6rs")'				>>ikarus.config.ss
	echo '(define (host-info) "$(target)")'						>>ikarus.config.ss
	echo '(define target-os-uid (quote $(VICARE_TARGET_OS_UID)))'			>>ikarus.config.ss
	echo '(define wordsize 4)'							>>ikarus.config.ss
	echo '(define platform-endianness (quote $(VICARE_PLATFORM_ENDIANNESS)))'	>>ikarus.config.ss
	echo '(define arguments-validation #$(VICARE_ARGUMENTS_VALIDATION))'		>>ikarus.config.ss
	echo '(define bootfile "$(pkglibdir)/vicare.boot")'				>>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_SRFI_ENABLED     #$(VICARE_BUILT_WITH_SRFI_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_ICONV_ENABLED    #$(VICARE_BUILT_WITH_ICONV_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_FFI_ENABLED      #$(VICARE_BUILT_WITH_FFI_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_NAUSICAA_ENABLED #$(VICARE_BUILT_WITH_NAUSICAA_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_POSIX_ENABLED    #$(VICARE_BUILT_WITH_POSIX_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_GLIBC_ENABLED    #$(VICARE_BUILT_WITH_GLIBC_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_LINUX_ENABLED    #$(VICARE_BUILT_WITH_LINUX_ENABLED))' >>ikarus.config.ss
	$(VICARE_BOOT_ENV) vicare -b $(VICARE_PREBUILT_BOOT)	\
		--r6rs-script $(srcdir)/scheme/makefile.sps
	mv $(builddir)/vicare.boot $(builddir)/vicare.boot.4.prebuilt

boot-image-64-bit: vicare
	echo '(define vicare-version "$(PACKAGE_VERSION)")'				>ikarus.config.ss
	echo '(define vicare-lib-dir "$(pkglibdir)")'					>>ikarus.config.ss
	echo '(define scheme-lib-dir "$(libdir)/scheme/r6rs")'				>>ikarus.config.ss
	echo '(define (host-info) "$(target)")'						>>ikarus.config.ss
	echo '(define target-os-uid (quote $(VICARE_TARGET_OS_UID)))'			>>ikarus.config.ss
	echo '(define wordsize 8)'							>>ikarus.config.ss
	echo '(define platform-endianness (quote $(VICARE_PLATFORM_ENDIANNESS)))'	>>ikarus.config.ss
	echo '(define arguments-validation #$(VICARE_ARGUMENTS_VALIDATION))'		>>ikarus.config.ss
	echo '(define bootfile "$(pkglibdir)/vicare.boot")'				>>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_SRFI_ENABLED     #$(VICARE_BUILT_WITH_SRFI_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_ICONV_ENABLED    #$(VICARE_BUILT_WITH_ICONV_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_FFI_ENABLED      #$(VICARE_BUILT_WITH_FFI_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_NAUSICAA_ENABLED #$(VICARE_BUILT_WITH_NAUSICAA_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_POSIX_ENABLED    #$(VICARE_BUILT_WITH_POSIX_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_GLIBC_ENABLED    #$(VICARE_BUILT_WITH_GLIBC_ENABLED))' >>ikarus.config.ss
	echo '(define VICARE_BUILT_WITH_LINUX_ENABLED    #$(VICARE_BUILT_WITH_LINUX_ENABLED))' >>ikarus.config.ss
	$(VICARE_BOOT_ENV) vicare -b $(VICARE_PREBUILT_BOOT)	\
		--r6rs-script $(srcdir)/scheme/makefile.sps
	mv $(builddir)/vicare.boot $(builddir)/vicare.boot.8.prebuilt

## --------------------------------------------------------------------

# We can  try to build  Vicare itself with  a newly built  boot image;
# do this from the build directory:
#
#   $ cp scheme/vicare.boot scheme/vicare.boot.try
#
# then:
#
#   $ make try-vicare-boot
#
# when we  are sure that  the newly built  boot image is fine,  we can
# adopt it as boot image to be distributed:
#
#   $ cp scheme/vicare.boot.try $(top_srcdir)/scheme/vicare.boot.4.prebuilt
#
# but keep backups of the original one!!!
#
try-vicare-boot: $(VICARE_BOOT_SRCS) vicare ikarus.config.ss
	$(VICARE_BOOT_ENV) vicare -b $(builddir)/vicare.boot.try	\
		--r6rs-script $(srcdir)/scheme/makefile.sps

#page
#### compiling bundled libraries: source files

EXTRA_DIST += lib/compile-all.sps

EXTRA_DIST += \
	lib/vicare/platform/configuration.sls.in			\
	lib/vicare/platform/constants.sls.in				\
	lib/vicare/platform/errno.sls.in				\
	lib/vicare/platform/words.sls.in				\
	lib/nausicaa/uri/pathnames.sls.in				\
	\
	lib/vicare/unsafe/capi.sls					\
	lib/vicare/unsafe/operations.sls				\
	lib/vicare/unsafe/unicode.sls					\
	\
	lib/vicare/platform/utilities.sls				\
	\
	lib/vicare/language-extensions/cond-expand.sls			\
	lib/vicare/language-extensions/cond-expand/OS-id-features.sls	\
	lib/vicare/language-extensions/cond-expand/feature-cond.sls	\
	lib/vicare/language-extensions/cond-expand/helpers.sls		\
	lib/vicare/language-extensions/cond-expand/platform-features.sls\
	lib/vicare/language-extensions/cond-expand/configuration-features.sls	\
	lib/vicare/language-extensions/cond-expand/registry.sls		\
	\
	lib/vicare/arguments/general-c-buffers.sls			\
	lib/vicare/arguments/validation.sls				\
	\
	lib/vicare/language-extensions/amb.sls				\
	lib/vicare/language-extensions/ascii-chars/syntaxes.sls		\
	lib/vicare/language-extensions/ascii-chars.sls			\
	lib/vicare/language-extensions/c-enumerations.sls		\
	lib/vicare/language-extensions/callables.sls			\
	lib/vicare/language-extensions/comparisons.sls			\
	lib/vicare/language-extensions/coroutines.sls			\
	lib/vicare/language-extensions/custom-ports.sls			\
	lib/vicare/language-extensions/define-record-extended.sls	\
	lib/vicare/language-extensions/hooks.sls			\
	lib/vicare/language-extensions/identifier-substitutions.sls	\
	lib/vicare/language-extensions/increments.sls			\
	lib/vicare/language-extensions/infix/auxiliary-syntaxes.sls	\
	lib/vicare/language-extensions/infix/lr-driver.sls		\
	lib/vicare/language-extensions/infix/parser-table.sls		\
	lib/vicare/language-extensions/infix/parser-utils.sls		\
	lib/vicare/language-extensions/infix/tokens.sls			\
	lib/vicare/language-extensions/infix.sls			\
	lib/vicare/language-extensions/makers.sls			\
	lib/vicare/language-extensions/keywords.sls			\
	lib/vicare/language-extensions/loops.sls			\
	lib/vicare/language-extensions/namespaces.sls			\
	lib/vicare/language-extensions/sentinels.sls			\
	lib/vicare/language-extensions/simple-match.sls			\
	lib/vicare/language-extensions/streams.sls			\
	lib/vicare/language-extensions/syntaxes.sls			\
	lib/vicare/language-extensions/try.sls				\
	lib/vicare/language-extensions/variables.sls			\
	\
	lib/vicare/crypto/randomisations.sls				\
	lib/vicare/crypto/randomisations/low.sls			\
	lib/vicare/crypto/randomisations/blum-blum-shub.sls		\
	lib/vicare/crypto/randomisations/borosh.sls			\
	lib/vicare/crypto/randomisations/cmrg.sls			\
	lib/vicare/crypto/randomisations/distributions.sls		\
	lib/vicare/crypto/randomisations/lists.sls			\
	lib/vicare/crypto/randomisations/marsaglia.sls			\
	lib/vicare/crypto/randomisations/mersenne.sls			\
	lib/vicare/crypto/randomisations/strings.sls			\
	lib/vicare/crypto/randomisations/vectors.sls			\
	\
	lib/vicare/numerics/constants.sls				\
	lib/vicare/numerics/flonum-formatter.sls			\
	lib/vicare/numerics/flonum-parser.sls				\
	\
	lib/vicare/containers/auxiliary-syntaxes.sls			\
	lib/vicare/containers/bytevectors.sls				\
	lib/vicare/containers/bytevector-compounds.sls			\
	lib/vicare/containers/bytevector-compounds/core.sls		\
	lib/vicare/containers/bytevector-compounds/unsafe.sls		\
	lib/vicare/containers/weak-hashtables.sls			\
	lib/vicare/containers/object-properties.sls			\
	lib/vicare/containers/lists/stx.sls				\
	lib/vicare/containers/lists/low.sls				\
	lib/vicare/containers/lists.sls					\
	lib/vicare/containers/vectors/low.sls				\
	lib/vicare/containers/vectors.sls				\
	lib/vicare/containers/strings/low.sls				\
	lib/vicare/containers/strings.sls				\
	lib/vicare/containers/strings/rabin-karp.sls			\
	lib/vicare/containers/knuth-morris-pratt.sls			\
	lib/vicare/containers/char-sets.sls				\
	lib/vicare/containers/char-sets/blocks.sls			\
	lib/vicare/containers/char-sets/categories.sls			\
	lib/vicare/containers/levenshtein.sls				\
	lib/vicare/containers/one-dimension-co.sls			\
	lib/vicare/containers/one-dimension-cc.sls			\
	lib/vicare/containers/bytevectors/generic-low.sls		\
	lib/vicare/containers/bytevectors/generic.sls			\
	lib/vicare/containers/bytevectors/s8.sls			\
	lib/vicare/containers/bytevectors/s8low.sls			\
	lib/vicare/containers/bytevectors/u8.sls			\
	lib/vicare/containers/bytevectors/u8low.sls			\
	lib/vicare/containers/arrays.sls				\
	lib/vicare/containers/stacks.sls				\
	lib/vicare/containers/queues.sls				\
	\
	lib/vicare/assembler/inspection.sls				\
	lib/vicare/checks.sls						\
	lib/vicare/debugging/compiler.sls				\
	lib/vicare/parser-logic.sls					\
	\
	lib/vicare/net/channels.sls					\
	\
	lib/vicare/irregex.sls						\
	lib/vicare/irregex/compat.vicare.sls				\
	lib/vicare/pregexp.sls						\
	lib/vicare/getopts.sls						\
	lib/vicare/formations.sls					\
	\
	lib/vicare/parser-tools/silex/make-tables.sps			\
	lib/vicare/parser-tools/silex/action.l				\
	lib/vicare/parser-tools/silex/class.l				\
	lib/vicare/parser-tools/silex/macro.l				\
	lib/vicare/parser-tools/silex/nested-comment.l			\
	lib/vicare/parser-tools/silex/regexp.l				\
	lib/vicare/parser-tools/silex/string.l				\
	lib/vicare/parser-tools/silex/macros/identifier.macros.l	\
	lib/vicare/parser-tools/silex/macros/string.macros.l		\
	lib/vicare/parser-tools/silex/macros/unicode.macros.l		\
	lib/vicare/parser-tools/silex/macros/whitespaces-and-comments.macros.l \
	\
	lib/vicare/parser-tools/silex/action-l.sls			\
	lib/vicare/parser-tools/silex/char-lexer-driver.sls		\
	lib/vicare/parser-tools/silex/class-l.sls			\
	lib/vicare/parser-tools/silex/code-lexer-driver.sls		\
	lib/vicare/parser-tools/silex/input-system.sls			\
	lib/vicare/parser-tools/silex/lexer.sls				\
	lib/vicare/parser-tools/silex/macro-l.sls			\
	lib/vicare/parser-tools/silex/nested-comment-l.sls		\
	lib/vicare/parser-tools/silex/regexp-l.sls			\
	lib/vicare/parser-tools/silex/semantic.sls			\
	lib/vicare/parser-tools/silex.sls				\
	lib/vicare/parser-tools/silex/string-l.sls			\
	lib/vicare/parser-tools/silex/tree-lexer-driver.sls		\
	lib/vicare/parser-tools/silex/utilities.sls			\
	\
	lib/vicare/parser-tools/unix-pathnames.sls			\
	\
	lib/srfi/%3a0.sls						\
	lib/srfi/%3a0/cond-expand.sls					\
	\
	lib/srfi/%3a1.sls						\
	lib/srfi/%3a1/lists.sls						\
	\
	lib/srfi/%3a2.sls						\
	lib/srfi/%3a2/and-let%2a.sls					\
	\
	lib/srfi/%3a6.sls						\
	lib/srfi/%3a6/basic-string-ports.sls				\
	\
	lib/srfi/%3a8.sls						\
	lib/srfi/%3a8/receive.sls					\
	\
	lib/srfi/%3a9.sls						\
	lib/srfi/%3a9/records.sls					\
	\
	lib/srfi/%3a11.sls						\
	lib/srfi/%3a11/let-values.sls					\
	\
	lib/srfi/%3a13.sls						\
	lib/srfi/%3a13/strings.sls					\
	\
	lib/srfi/%3a14.sls						\
	lib/srfi/%3a14/char-sets.sls					\
	\
	lib/srfi/%3a16.sls						\
	lib/srfi/%3a16/case-lambda.sls					\
	\
	lib/srfi/%3a19.sls						\
	lib/srfi/%3a19/time.sls						\
	\
	lib/srfi/%3a23.sls						\
	lib/srfi/%3a23/error.sls					\
	\
	lib/srfi/%3a25.sls						\
	lib/srfi/%3a25/multi-dimensional-arrays.sls			\
	lib/srfi/%3a25/arlib.scm					\
	lib/srfi/%3a25/list.scm						\
	\
	lib/srfi/%3a26.sls						\
	lib/srfi/%3a26/cut.sls						\
	\
	lib/srfi/%3a27.sls						\
	lib/srfi/%3a27/random-bits.sls					\
	\
	lib/srfi/%3a28.sls						\
	lib/srfi/%3a28/basic-format-strings.sls				\
	\
	lib/srfi/%3a31.sls						\
	lib/srfi/%3a31/rec.sls						\
	\
	lib/srfi/%3a37.sls						\
	lib/srfi/%3a37/args-fold.sls					\
	\
	lib/srfi/%3a38.sls						\
	lib/srfi/%3a38/with-shared-structure.sls			\
	\
	lib/srfi/%3a39.sls						\
	lib/srfi/%3a39/parameters.sls					\
	\
	lib/srfi/%3a41.sls						\
	lib/srfi/%3a41/streams.sls					\
	lib/srfi/%3a41/streams/derived.sls				\
	lib/srfi/%3a41/streams/primitive.sls				\
	\
	lib/srfi/%3a42.sls						\
	lib/srfi/%3a42/eager-comprehensions.sls				\
	\
	lib/srfi/%3a43.sls						\
	lib/srfi/%3a43/vectors.sls					\
	\
	lib/srfi/%3a45.sls						\
	lib/srfi/%3a45/lazy.sls						\
	\
	lib/srfi/%3a48.sls						\
	lib/srfi/%3a48/intermediate-format-strings.sls			\
	\
	lib/srfi/%3a61/cond.sls						\
	lib/srfi/%3a61.sls						\
	\
	lib/srfi/%3a64.sls						\
	lib/srfi/%3a64/testing.sls					\
	\
	lib/srfi/%3a67/compare-procedures.sls				\
	lib/srfi/%3a67.sls						\
	\
	lib/srfi/%3a69/basic-hash-tables.sls				\
	lib/srfi/%3a69.sls						\
	\
	lib/srfi/%3a78.sls						\
	lib/srfi/%3a78/lightweight-testing.sls				\
	\
	lib/srfi/%3a98.sls						\
	lib/srfi/%3a98/os-environment-variables.sls			\
	\
	lib/srfi/%3a99.sls						\
	lib/srfi/%3a99/records.sls					\
	lib/srfi/%3a99/records/helper.sls				\
	lib/srfi/%3a99/records/inspection.sls				\
	lib/srfi/%3a99/records/procedural.sls				\
	lib/srfi/%3a99/records/syntactic.sls				\
	\
	lib/srfi/%3a101.sls						\
	lib/srfi/%3a101/random-access-lists.sls				\
	lib/srfi/%3a101/random-access-lists/procedures.sls		\
	lib/srfi/%3a101/random-access-lists/syntax.sls			\
	lib/srfi/%3a101/random-access-lists/equal.sls			\
	\
	lib/srfi/%3a111.sls						\
	lib/srfi/%3a111/boxes.sls					\
	\
	lib/srfi/%3a112.sls						\
	lib/srfi/%3a112/environment-inquiry.sls				\
	\
	lib/srfi/%3a106.sls						\
	lib/srfi/%3a106/socket.sls					\
	lib/srfi/%3a106/compat.vicare.sls				\
	\
	lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.l	\
	lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.l	\
	lib/nausicaa/parser-tools/ip-addresses/make-tables.sps		\
	\
	lib/nausicaa/language/auxiliary-syntaxes.sls			\
	lib/nausicaa/language/symbols-tree.sls				\
	\
	lib/nausicaa/language/oopp.sls					\
	lib/nausicaa/language/oopp/auxiliary-syntaxes.sls		\
	lib/nausicaa/language/oopp/configuration.sls			\
	lib/nausicaa/language/oopp/conditions.sls			\
	lib/nausicaa/language/oopp/definition-parser-helpers.sls	\
	lib/nausicaa/language/oopp/oopp-syntax-helpers.sls		\
	\
	lib/nausicaa/language/multimethods.sls				\
	lib/nausicaa/language/multimethods/auxiliary-syntaxes.sls	\
	lib/nausicaa/language/multimethods/methods-table.sls		\
	lib/nausicaa/language/multimethods/types.sls			\
	\
	lib/nausicaa/language/builtins.sls				\
	lib/nausicaa/language/conditions.sls				\
	lib/nausicaa/language/increments.sls				\
	lib/nausicaa/language/simple-match.sls				\
	lib/nausicaa/language/infix.sls					\
	\
	lib/nausicaa.sls						\
	\
	lib/nausicaa/containers/lists.sls				\
	lib/nausicaa/containers/vectors.sls				\
	lib/nausicaa/containers/strings.sls				\
	lib/nausicaa/containers/arrays.sls				\
	lib/nausicaa/containers/stacks.sls				\
	lib/nausicaa/containers/queues.sls				\
	lib/nausicaa/containers/bitvectors.sls				\
	lib/nausicaa/containers/iterators.sls				\
	\
	lib/nausicaa/parser-tools/source-locations.sls			\
	lib/nausicaa/parser-tools/lexical-tokens.sls			\
	lib/nausicaa/parser-tools/silex/default-error-handler.sls	\
	lib/nausicaa/parser-tools/lalr/lr-driver.sls			\
	lib/nausicaa/parser-tools/lalr/glr-driver.sls			\
	lib/nausicaa/parser-tools/lalr.sls				\
	\
	lib/nausicaa/uri/ip.sls						\
	lib/nausicaa/uri.sls						\
	lib/nausicaa/uri/pathnames/unix.sls				\
	lib/nausicaa/uri/pathnames/abstract.sls				\
	\
	lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.sls	\
	lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.sls	\
	lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.sls	\
	lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.sls	\
	lib/nausicaa/parser-tools/ipv4-addresses.sls			\
	lib/nausicaa/parser-tools/ipv6-addresses.sls			\
	lib/nausicaa/parser-tools/uri.sls				\
	lib/nausicaa/parser-tools/uri/utilities.sls			\
	\
	lib/nausicaa/mehve.sls						\
	lib/nausicaa/mehve/language/numerics/predicates.sls		\
	lib/nausicaa/mehve/language/numerics/arithmetics.sls		\
	lib/nausicaa/mehve/language/numerics/parts.sls			\
	lib/nausicaa/mehve/language/numerics/transcendental.sls		\
	lib/nausicaa/mehve/language/infix.sls				\
	lib/nausicaa/mehve/language/input-output.sls			\
	\
	lib/vicare/ffi.sls						\
	lib/vicare/ffi/foreign-pointer-wrapper.sls			\
	\
	lib/vicare/posix.sls						\
	lib/vicare/posix/pid-files.sls					\
	lib/vicare/posix/lock-pid-files.sls				\
	lib/vicare/posix/log-files.sls					\
	lib/vicare/posix/daemonisations.sls				\
	lib/vicare/posix/simple-event-loop.sls				\
	lib/vicare/posix/tcp-server-sockets.sls				\
	\
	lib/vicare/glibc.sls						\
	lib/vicare/iconv.sls						\
	lib/vicare/gcc.sls						\
	lib/vicare/linux.sls						\
	lib/vicare/readline.sls						\
	lib/vicare/cre2.sls

#page
#### compiling bundled libraries: fasl files

libvicaredir	= $(instdir)
nobase_nodist_libvicare_DATA	= \
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
	$(fasldir)/vicare/language-extensions/cond-expand.fasl				\
	$(fasldir)/vicare/language-extensions/cond-expand/OS-id-features.fasl		\
	$(fasldir)/vicare/language-extensions/cond-expand/feature-cond.fasl		\
	$(fasldir)/vicare/language-extensions/cond-expand/helpers.fasl			\
	$(fasldir)/vicare/language-extensions/cond-expand/platform-features.fasl	\
	$(fasldir)/vicare/language-extensions/cond-expand/configuration-features.fasl	\
	$(fasldir)/vicare/language-extensions/cond-expand/registry.fasl			\
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
	$(fasldir)/vicare/irregex/compat.fasl				\
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
libsrfidir			= $(instdir)
nobase_nodist_libsrfi_DATA	= \
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
endif

if WANT_SRFI
if WANT_POSIX
libsrfiposixdir			= $(instdir)
nobase_nodist_libsrfiposix_DATA	= \
	$(fasldir)/srfi/%3a106.fasl					\
	$(fasldir)/srfi/%3a106/socket.fasl				\
	$(fasldir)/srfi/%3a106/compat.fasl
endif
endif

if WANT_NAUSICAA
libnausicaadir			= $(instdir)
nobase_nodist_libnausicaa_DATA	= \
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
	\
	$(fasldir)/nausicaa/mehve.fasl					\
	$(fasldir)/nausicaa/mehve/language/numerics/predicates.fasl	\
	$(fasldir)/nausicaa/mehve/language/numerics/arithmetics.fasl	\
	$(fasldir)/nausicaa/mehve/language/numerics/parts.fasl		\
	$(fasldir)/nausicaa/mehve/language/numerics/transcendental.fasl	\
	$(fasldir)/nausicaa/mehve/language/infix.fasl			\
	$(fasldir)/nausicaa/mehve/language/input-output.fasl
endif

if WANT_LIBFFI
libffidir		= $(instdir)
nobase_nodist_libffi_DATA	= \
	$(fasldir)/vicare/ffi.fasl					\
	$(fasldir)/vicare/ffi/foreign-pointer-wrapper.fasl
endif

if WANT_POSIX
libposixdir			= $(instdir)
nobase_nodist_libposix_DATA	= \
	$(fasldir)/vicare/posix.fasl					\
	$(fasldir)/vicare/posix/pid-files.fasl				\
	$(fasldir)/vicare/posix/lock-pid-files.fasl			\
	$(fasldir)/vicare/posix/log-files.fasl				\
	$(fasldir)/vicare/posix/daemonisations.fasl			\
	$(fasldir)/vicare/posix/simple-event-loop.fasl			\
	$(fasldir)/vicare/posix/tcp-server-sockets.fasl
endif

if WANT_GLIBC
libglibcdir			= $(instdir)
nobase_nodist_libglibc_DATA	= $(fasldir)/vicare/glibc.fasl
endif

if WANT_LIBICONV
libiconvdir			= $(instdir)
nobase_nodist_libiconv_DATA	= $(fasldir)/vicare/iconv.fasl
endif

if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
libgccdir			= $(instdir)
nobase_nodist_libgcc_DATA		= $(fasldir)/vicare/gcc.fasl
endif
endif
endif

if WANT_LINUX
liblinuxdir			= $(instdir)
nobase_nodist_liblinux_DATA	= $(fasldir)/vicare/linux.fasl
endif

if WANT_READLINE
libreadlinedir			= $(instdir)
nobase_nodist_libreadline_DATA	= $(fasldir)/vicare/readline.fasl
endif

if WANT_CRE2
libcre2dir			= $(instdir)
nobase_nodist_libcre2_DATA	= $(fasldir)/vicare/cre2.fasl
endif

#page
#### compiling bundled libraries: features library

BUILT_SOURCES		+= lib/vicare/platform/features.sls

noinst_PROGRAMS		= features
features_SOURCES	= lib/features.c

lib/vicare/platform/features.sls : features
	$(MKDIR_P) lib/vicare/platform && ./features >$@

lib/vicare/platform/features.fasl : lib/vicare/platform/features.sls

CLEANFILES	+= vicare/platform/features.sls

#page
#### compiling bundled libraries: build rules

VICARE_COMPILE_LIBRARY_PATH	= $(builddir)/lib:$(srcdir)/lib:$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_FASL_PATH	= $(fasldir):$(VICARE_FASL_PATH)
VICARE_COMPILE_LD_PATH		= $(builddir)/.libs

VICARE_ENV	= \
	VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	VICARE_FASL_PATH=$(VICARE_COMPILE_FASL_PATH);		export VICARE_FASL_PATH;	\
	SILEX_PATH=$(srcdir)/vicare/parser-tools/silex/macros;	export SILEX_PATH;		\
	LD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH);		export DYLD_LIBRARY_PATH;
VICARE_FLAGS	= -b $(VICARE_NEW_BOOT) --fasl-directory $(fasldir) $(AX_VFLAGS) $(VFLAGS)
VICARE_RUN	= $(VICARE_ENV) vicare $(VICARE_FLAGS)

lib/vicare/irregex/compat.fasl : lib/vicare/irregex/compat.vicare.sls $(VICARE_NEW_BOOT)
	$(VICARE_RUN) --output $@ --compile-library $<

lib/srfi/%3a106/compat.fasl : lib/srfi/%3a106/compat.vicare.sls $(VICARE_NEW_BOOT)
	$(VICARE_RUN) --output $@ --compile-library $<

%.fasl : %.sls $(VICARE_NEW_BOOT)
	$(VICARE_RUN) --output $@ --compile-library $<

## --------------------------------------------------------------------

.PHONY: dependencies

dependencies:
	$(VICARE_RUN) --print-dependencies $(srcdir)/lib/compile-all.sps >$(srcdir)/lib/dependencies.make

include $(srcdir)/lib/dependencies.make

## --------------------------------------------------------------------

CLEANFILES	+= \
	$(nobase_nodist_libvicare_DATA)		\
	$(nobase_nodist_libsrfi_DATA)		\
	$(nobase_nodist_libsrfiposix_DATA)	\
	$(nobase_nodist_libnausicaa_DATA)	\
	$(nobase_nodist_libffi_DATA)		\
	$(nobase_nodist_libposix_DATA)		\
	$(nobase_nodist_libglibc_DATA)		\
	$(nobase_nodist_libiconv_DATA)		\
	$(nobase_nodist_libgcc_DATA)		\
	$(nobase_nodist_liblinux_DATA)		\
	$(nobase_nodist_libreadline_DATA)	\
	$(nobase_nodist_libcre2_DATA)

#page
#### special library rules: SILex lexers building

SILEX_SRCDIR		= $(srcdir)/lib/vicare/parser-tools/silex
NAUIPADDR_SRCDIR	= $(srcdir)/lib/nausicaa/parser-tools/ip-addresses

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

#page
#### running the test suite: test files

VICARE_SCHEME_CORE_TESTS	= \
	tests/test-r6rs-arithmetic-bitwise.sps				\
	tests/test-r6rs-arithmetic-fixnums.sps				\
	tests/test-r6rs-arithmetic-flonums.sps				\
	tests/test-r6rs-base.sps					\
	tests/test-r6rs-bytevectors.sps					\
	tests/test-r6rs-conditions.sps					\
	tests/test-r6rs-contrib.sps					\
	tests/test-r6rs-control.sps					\
	tests/test-r6rs-enums.sps					\
	tests/test-r6rs-eval.sps					\
	tests/test-r6rs-exceptions.sps					\
	tests/test-r6rs-hashtables.sps					\
	tests/test-r6rs-io-ports.sps					\
	tests/test-r6rs-io-simple.sps					\
	tests/test-r6rs-lists.sps					\
	tests/test-r6rs-mutable-pairs.sps				\
	tests/test-r6rs-mutable-strings.sps				\
	tests/test-r6rs-programs.sps					\
	tests/test-r6rs-r5rs.sps					\
	tests/test-r6rs-reader.sps					\
	tests/test-r6rs-records-procedural.sps				\
	tests/test-r6rs-records-syntactic.sps				\
	tests/test-r6rs-sorting.sps					\
	tests/test-r6rs-syntax-case.sps					\
	tests/test-r6rs-unicode.sps					\
	\
	tests/test-ikarus-bignum-to-flonum.sps				\
	tests/test-ikarus-bitwise.sps					\
	tests/test-ikarus-bytevectors.sps				\
	tests/test-ikarus-case-folding.sps				\
	tests/test-ikarus-div-and-mod.sps				\
	tests/test-ikarus-enums.sps					\
	tests/test-ikarus-fasl.sps					\
	tests/test-ikarus-fixnums.sps					\
	tests/test-ikarus-fldiv-and-mod.sps				\
	tests/test-ikarus-guardians.sps					\
	tests/test-ikarus-hashtables.sps				\
	tests/test-ikarus-lists.sps					\
	tests/test-ikarus-normalization.sps				\
	tests/test-ikarus-numerics.sps					\
	tests/test-ikarus-r6rs-records-procedural.sps			\
	tests/test-ikarus-reader.sps					\
	tests/test-ikarus-set-position.sps				\
	tests/test-ikarus-sorting.sps					\
	tests/test-ikarus-strings.sps					\
	tests/test-ikarus-symbol-table.sps				\
	tests/test-ikarus-unicode.sps					\
	\
	tests/test-vicare-bignums.sps					\
	tests/test-vicare-bytevectors.sps				\
	tests/test-vicare-builtin-posix.sps				\
	tests/test-vicare-chars.sps					\
	tests/test-vicare-collect.sps					\
	tests/test-vicare-compensations.sps				\
	tests/test-vicare-conditions.sps				\
	tests/test-vicare-enumerations.sps				\
	tests/test-vicare-environment-inquiry.sps			\
	tests/test-vicare-equality.sps					\
	tests/test-vicare-errno.sps					\
	tests/test-vicare-expander.sps					\
	tests/test-vicare-expand-time-specs.sps				\
	tests/test-vicare-debugging.sps					\
	tests/test-vicare-fasl.sps					\
	tests/test-vicare-fixnums.sps					\
	tests/test-vicare-flonum-base.sps				\
	tests/test-vicare-flonum-formatter.sps				\
	tests/test-vicare-flonum-parser.sps				\
	tests/test-vicare-io.sps					\
	tests/test-vicare-letrec-syntax.sps				\
	tests/test-vicare-library-utils.sps				\
	tests/test-vicare-lists.sps					\
	tests/test-vicare-log.sps					\
	tests/test-vicare-numerics-generic.sps				\
	tests/test-vicare-numerics-neg.sps				\
	tests/test-vicare-numerics-inv.sps				\
	tests/test-vicare-numerics-add.sps				\
	tests/test-vicare-numerics-sub.sps				\
	tests/test-vicare-numerics-mul.sps				\
	tests/test-vicare-numerics-div.sps				\
	tests/test-vicare-numerics-gcd.sps				\
	tests/test-vicare-numerics-lcm.sps				\
	tests/test-vicare-numerics-quotient.sps				\
	tests/test-vicare-numerics-remainder.sps			\
	tests/test-vicare-numerics-modulo.sps				\
	tests/test-vicare-numerics-square.sps				\
	tests/test-vicare-numerics-cube.sps				\
	tests/test-vicare-numerics-sign.sps				\
	tests/test-vicare-numerics-expt-fixnums.sps			\
	tests/test-vicare-numerics-expt-bignums.sps			\
	tests/test-vicare-numerics-expt-flonums.sps			\
	tests/test-vicare-numerics-expt-ratnums.sps			\
	tests/test-vicare-numerics-expt-cflonums.sps			\
	tests/test-vicare-numerics-expt-compnums.sps			\
	tests/test-vicare-numerics-max.sps				\
	tests/test-vicare-numerics-min.sps				\
	tests/test-vicare-numerics-abs.sps				\
	tests/test-vicare-numerics-log.sps				\
	tests/test-vicare-numerics-exponential.sps			\
	tests/test-vicare-numerics-trigon-sin.sps			\
	tests/test-vicare-numerics-trigon-cos.sps			\
	tests/test-vicare-numerics-trigon-tan.sps			\
	tests/test-vicare-numerics-trigon-asin.sps			\
	tests/test-vicare-numerics-trigon-acos.sps			\
	tests/test-vicare-numerics-trigon-atan.sps			\
	tests/test-vicare-numerics-hyper-sinh.sps			\
	tests/test-vicare-numerics-hyper-cosh.sps			\
	tests/test-vicare-numerics-hyper-tanh.sps			\
	tests/test-vicare-numerics-hyper-asinh.sps			\
	tests/test-vicare-numerics-hyper-acosh.sps			\
	tests/test-vicare-numerics-hyper-atanh.sps			\
	tests/test-vicare-numerics-complex-numbers.sps			\
	tests/test-vicare-numerics-sqrt.sps				\
	tests/test-vicare-numerics-cbrt.sps				\
	tests/test-vicare-numerics-bitwise.sps				\
	tests/test-vicare-parameters.sps				\
	tests/test-vicare-parser-logic.sps				\
	tests/test-vicare-pointers.sps					\
	tests/test-vicare-promises.sps					\
	tests/test-vicare-time.sps					\
	tests/test-vicare-ratnums.sps					\
	tests/test-vicare-reader.sps					\
	tests/test-vicare-records-procedural.sps			\
	tests/test-vicare-records-syntactic.sps				\
	tests/test-vicare-round.sps					\
	tests/test-vicare-string-to-number.sps				\
	tests/test-vicare-strings.sps					\
	tests/test-vicare-structs.sps					\
	tests/test-vicare-symbols.sps					\
	tests/test-vicare-syntactic-extensions.sps			\
	tests/test-vicare-syntax-utilities.sps				\
	tests/test-vicare-system.sps					\
	tests/test-vicare-vectors.sps					\
	\
	tests/test-issue-001-equal-hash.sps				\
	tests/test-issue-003.sps					\
	tests/test-issue-006-fxreverse-bit-field.sps			\
	tests/test-issue-007.sps					\
	tests/test-issue-008.sps					\
	tests/test-issue-009.sps					\
	tests/test-issue-010.sps					\
	tests/test-issue-012.sps					\
	tests/test-issue-013.sps					\
	tests/test-issue-015.sps					\
	tests/test-issue-017.sps					\
	tests/test-issue-020.sps					\
	tests/test-issue-021.sps					\
	tests/test-issue-022.sps					\
	tests/test-issue-023.sps					\
	tests/test-issue-024.sps					\
	tests/test-issue-025-sign-in-exact-rationals.sps		\
	\
	tests/test-vicare-unsafe-operations.sps				\
	\
	tests/test-vicare-platform-utils.sps				\
	tests/test-vicare-platform-words.sps				\
	\
	tests/test-vicare-arguments-validation.sps			\
	tests/test-vicare-arguments-general-c-buffers.sps		\
	\
	tests/test-vicare-language-extensions.sps			\
	tests/test-vicare-language-extensions-amb.sps			\
	tests/test-vicare-language-extensions-ascii-chars.sps		\
	tests/test-vicare-language-extensions-c-enumerations.sps	\
	tests/test-vicare-language-extensions-callables.sps		\
	tests/test-vicare-language-extensions-comparisons.sps		\
	tests/test-vicare-language-extensions-cond-expand.sps		\
	tests/test-vicare-language-extensions-coroutines.sps		\
	tests/test-vicare-language-extensions-custom-ports.sps		\
	tests/test-vicare-language-extensions-hooks.sps			\
	tests/test-vicare-language-extensions-identifier-substitutions.sps	\
	tests/test-vicare-language-extensions-infix.sps			\
	tests/test-vicare-language-extensions-keywords.sps		\
	tests/test-vicare-language-extensions-loops.sps			\
	tests/test-vicare-language-extensions-makers.sps		\
	tests/test-vicare-language-extensions-namespaces.sps		\
	tests/test-vicare-language-extensions-sentinels.sps		\
	tests/test-vicare-language-extensions-simple-match.sps		\
	tests/test-vicare-language-extensions-streams.sps		\
	tests/test-vicare-language-extensions-variables.sps		\
	\
	tests/test-vicare-containers-arrays.sps				\
	tests/test-vicare-containers-bytevector-compounds.sps		\
	tests/test-vicare-containers-bytevectors-s8-high.sps		\
	tests/test-vicare-containers-bytevectors-s8-low.sps		\
	tests/test-vicare-containers-bytevectors-u8-high.sps		\
	tests/test-vicare-containers-bytevectors-u8-low.sps		\
	tests/test-vicare-containers-char-sets.sps			\
	tests/test-vicare-containers-kmp.sps				\
	tests/test-vicare-containers-levenshtein.sps			\
	tests/test-vicare-containers-lists-fun.sps			\
	tests/test-vicare-containers-lists-low.sps			\
	tests/test-vicare-containers-lists-stx.sps			\
	tests/test-vicare-containers-object-properties.sps		\
	tests/test-vicare-containers-one-dimension-cc.sps		\
	tests/test-vicare-containers-one-dimension-co.sps		\
	tests/test-vicare-containers-queues.sps				\
	tests/test-vicare-containers-stacks.sps				\
	tests/test-vicare-containers-strings-high.sps			\
	tests/test-vicare-containers-strings-low.sps			\
	tests/test-vicare-containers-strings-rabin-karp.sps		\
	tests/test-vicare-containers-vectors-high.sps			\
	tests/test-vicare-containers-weak-hashtables.sps		\
	\
	tests/test-vicare-crypto-randomisations.sps			\
	\
	tests/test-vicare-net-channels-binary.sps			\
	tests/test-vicare-net-channels-textual.sps			\
	\
	tests/test-irregex.sps						\
	tests/test-pregexp.sps						\
	tests/test-vicare-getopts.sps					\
	tests/test-formations-round.sps					\
	tests/test-formations-lib.sps					\
	\
	tests/test-vicare-parser-tools-silex-file.sps			\
	tests/test-vicare-parser-tools-silex-misc.sps			\
	tests/test-vicare-parser-tools-silex-online.sps			\
	tests/test-vicare-parser-tools-unix-pathnames.sps

VICARE_SCHEME_LONG_TESTS	= \
	tests/long-test-r6rs-run.sps					\
	tests/long-test-r6rs-run-via-eval.sps				\
	tests/long-test-ikarus-bignums.sps				\
	tests/long-test-ikarus-io.sps					\
	tests/long-test-ikarus-parse-flonums.sps			\
	tests/long-test-ikarus-string-to-number.sps

VICARE_SCHEME_SRFI_TESTS	= \
	tests/test-srfi-0-cond-expand.sps				\
	tests/test-srfi-1-lists.sps					\
	tests/test-srfi-2-and-let-star.sps				\
	tests/test-srfi-6-basic-string-ports.sps			\
	tests/test-srfi-8-receive.sps					\
	tests/test-srfi-9-records.sps					\
	tests/test-srfi-11-let-values.sps				\
	tests/test-srfi-13-strings.sps					\
	tests/test-srfi-14-char-sets.sps				\
	tests/test-srfi-19-time.sps					\
	tests/test-srfi-26-cut.sps					\
	tests/test-srfi-27-random-bits.sps				\
	tests/test-srfi-28-basic-format.sps				\
	tests/test-srfi-31-rec.sps					\
	tests/test-srfi-37-args-fold.sps				\
	tests/test-srfi-39-parameters.sps				\
	tests/test-srfi-41-streams.sps					\
	tests/test-srfi-42-eager-comprehensions.sps			\
	tests/test-srfi-43-vectors.sps					\
	tests/test-srfi-45-lazy.sps					\
	tests/test-srfi-48-intermediate-format-strings.sps		\
	tests/test-srfi-61-general-cond.sps				\
	tests/test-srfi-64-testing.sps					\
	tests/test-srfi-67-compare-procedures.sps			\
	tests/test-srfi-69-basic-hash-tables.sps			\
	tests/test-srfi-78-lightweight-testing.sps			\
	tests/test-srfi-98-os-environment-variables.sps			\
	tests/test-srfi-101-functional-pairs.sps			\
	tests/test-srfi-111-boxes.sps					\
	tests/test-srfi-112-environment-inquiry.sps

VICARE_SCHEME_SRFI_POSIX_TESTS	= \
	tests/test-srfi-106-socket.sps

VICARE_SCHEME_NAUSICAA_TESTS	= \
	tests/test-nausicaa-tags-core-classes.sps			\
	tests/test-nausicaa-tags-core-labels.sps			\
	tests/test-nausicaa-tags-docs.sps				\
	tests/test-nausicaa-oopp-generics.sps				\
	tests/test-nausicaa-oopp-binding-constructs.sps			\
	tests/test-nausicaa-oopp-builtin-labels.sps			\
	tests/test-nausicaa-oopp-core.sps				\
	tests/test-nausicaa-oopp-labels-and-interfaces.sps		\
	tests/test-nausicaa-oopp-label-core.sps				\
	tests/test-nausicaa-oopp-label-shadowing.sps			\
	tests/test-nausicaa-oopp-mixins.sps				\
	tests/test-nausicaa-oopp-setter-and-getter.sps			\
	tests/test-nausicaa-oopp-unsafe-field-selectors.sps		\
	tests/test-nausicaa-oopp-nested-calls.sps			\
	tests/test-nausicaa-oopp-cast-operator.sps			\
	tests/test-nausicaa-multimethods-ordinary.sps			\
	tests/test-nausicaa-multimethods-starred.sps			\
	tests/test-nausicaa-conditions.sps				\
	tests/test-nausicaa-simple-match.sps				\
	tests/test-nausicaa-language-infix.sps				\
	\
	tests/test-nausicaa-containers-lists.sps			\
	tests/test-nausicaa-containers-vectors.sps			\
	tests/test-nausicaa-containers-strings.sps			\
	tests/test-nausicaa-containers-arrays.sps			\
	tests/test-nausicaa-containers-stacks.sps			\
	tests/test-nausicaa-containers-queues.sps			\
	tests/test-nausicaa-containers-bitvectors.sps			\
	tests/test-nausicaa-containers-iterators.sps			\
	\
	tests/test-nausicaa-parser-tools-lexical-token.sps		\
	tests/test-nausicaa-parser-tools-source-location.sps		\
	tests/test-nausicaa-parser-tools-lalr-lr.sps			\
	tests/test-nausicaa-parser-tools-lalr-glr.sps			\
	tests/test-nausicaa-parser-tools-lalr-calc.sps			\
	tests/test-nausicaa-parser-tools-uri.sps			\
	\
	tests/test-nausicaa-uri-ip-address.sps				\
	tests/test-nausicaa-uri-ipv4-address.sps			\
	tests/test-nausicaa-uri-ipv6-address.sps			\
	tests/test-nausicaa-uri-uri.sps					\
	tests/test-nausicaa-uri-pathnames-unix.sps			\
	\
	tests/test-issue-058-lalr-unary-prec.sps			\
	\
	tests/test-mehve-language-numerics-arithmetics.sps		\
	tests/test-mehve-language-numerics-predicates.sps		\
	tests/test-mehve-language-numerics-parts.sps			\
	tests/test-mehve-language-numerics-transcendental.sps		\
	tests/test-mehve-language-infix.sps				\
	tests/test-mehve-language-input-output.sps

VICARE_SCHEME_FFI_TESTS	= \
	tests/test-vicare-ffi-core.sps					\
	tests/test-vicare-ffi-foreign-pointer-wrapper.sps

VICARE_SCHEME_POSIX_TESTS	= \
	tests/test-vicare-posix-basic.sps				\
	tests/test-vicare-posix-non-blocking.sps			\
	tests/test-vicare-posix-close-on-exec.sps			\
	tests/test-vicare-posix-sockets.sps				\
	tests/test-vicare-posix-mmap.sps				\
	tests/test-vicare-posix-bub.sps					\
	tests/test-vicare-posix-time.sps				\
	tests/test-vicare-posix-shared-memory.sps			\
	\
	tests/test-vicare-posix-processes-shared-memory.sps		\
	tests/test-vicare-posix-sel.sps					\
	tests/test-vicare-posix-pid-files.sps				\
	tests/test-vicare-posix-lock-pid-files.sps			\
	tests/test-vicare-posix-log-files.sps				\
	\
	tests/test-vicare-posix-net-channels-binary.sps			\
	tests/test-vicare-posix-net-channels-textual.sps

if TARGET_OS_DARWIN
else
VICARE_SCHEME_POSIX_TESTS	+= \
	tests/test-vicare-posix-message-queue.sps			\
	tests/test-vicare-posix-semaphores.sps				\
	tests/test-vicare-posix-sigwait.sps				\
	tests/test-vicare-posix-timers.sps
endif

VICARE_SCHEME_GLIBC_TESTS	= tests/test-vicare-glibc.sps

VICARE_SCHEME_ICONV_TESTS	= tests/test-vicare-iconv.sps

VICARE_SCHEME_LINUX_TESTS	= tests/test-vicare-linux.sps

VICARE_SCHEME_CRE2_TESTS	= tests/foreign-test-vicare-cre2.sps

#page
#### running the test suite: distribution files

EXTRA_DIST	+= \
	tests/ikarus-test-framework.sls			\
	tests/lists.sls					\
	tests/lists/low.sls				\
	tests/lists/stx.sls				\
	tests/numerics/helpers.sls			\
	tests/libtest/calc-code-lexer.sls		\
	tests/libtest/calc-parser-helper.sls		\
	tests/libtest/calc-parser-lexer.sls		\
	tests/libtest/calc-parser.sls			\
	tests/libtest/calc-portable-lexer.sls		\
	tests/libtest/calc-tree-lexer.sls		\
	tests/libtest/classes-lib.sls			\
	tests/libtest/makers-lib.sls			\
	tests/libtest/records-lib.sls			\
	tests/libtest/silex-test.sls			\
	tests/make-silex-calc.sps			\
	tests/make-lalr-calc.sps			\
	tests/calc.l					\
	tests/TEST-SOURCE-FILE.txt			\
	tests/rn100					\
	\
	tests/r6rs/README.txt				\
	tests/r6rs/arithmetic				\
	tests/r6rs/arithmetic/bitwise.sls		\
	tests/r6rs/arithmetic/fixnums.sls		\
	tests/r6rs/arithmetic/flonums.sls		\
	tests/r6rs/base.sls				\
	tests/r6rs/bytevectors.sls			\
	tests/r6rs/conditions.sls			\
	tests/r6rs/contrib				\
	tests/r6rs/contrib.sls				\
	tests/r6rs/contrib/helper1.sls			\
	tests/r6rs/control.sls				\
	tests/r6rs/enums.sls				\
	tests/r6rs/eval.sls				\
	tests/r6rs/exceptions.sls			\
	tests/r6rs/hashtables.sls			\
	tests/r6rs/io					\
	tests/r6rs/io/ports.sls				\
	tests/r6rs/io/simple.sls			\
	tests/r6rs/lists.sls				\
	tests/r6rs/mutable-pairs.sls			\
	tests/r6rs/mutable-strings.sls			\
	tests/r6rs/programs.sls				\
	tests/r6rs/r5rs.sls				\
	tests/r6rs/reader.sls				\
	tests/r6rs/records				\
	tests/r6rs/records/procedural.sls		\
	tests/r6rs/records/syntactic.sls		\
	tests/r6rs/sorting.sls				\
	tests/r6rs/syntax-case.sls			\
	tests/r6rs/test.sls				\
	tests/r6rs/unicode.sls				\
	\
	tests/exec-modes-helpers/r6rs-forms		\
	tests/exec-modes-helpers/r6rs-program.sps	\
	tests/test-exec-modes.sh			\
	\
	tests/demo-vicare-gcc.sps			\
	tests/demo-vicare-readline.sps			\
	tests/demo-vicare-timerfd.sps			\
	tests/demo-issue-35.sps				\
	tests/demo-tcp-connect.sps			\
	tests/demo-srfi-106.sps				\
	\
	$(VICARE_SCHEME_CORE_TESTS)			\
	$(VICARE_SCHEME_LONG_TESTS)			\
	$(VICARE_SCHEME_SRFI_TESTS)			\
	$(VICARE_SCHEME_SRFI_POSIX_TESTS)		\
	$(VICARE_SCHEME_NAUSICAA_TESTS)			\
	$(VICARE_SCHEME_FFI_TESTS)			\
	$(VICARE_SCHEME_POSIX_TESTS)			\
	$(VICARE_SCHEME_GLIBC_TESTS)			\
	$(VICARE_SCHEME_ICONV_TESTS)			\
	$(VICARE_SCHEME_LINUX_TESTS)			\
	$(VICARE_SCHEME_CRE2_TESTS)

#page
#### running the test suite: selecting tests for "make check"

TESTS		= $(VICARE_SCHEME_CORE_TESTS)
XFAIL_TESTS	= tests/test-issue-058-lalr-unary-prec.sps

if WANT_SRFI
TESTS		+= $(VICARE_SCHEME_SRFI_TESTS)
if WANT_POSIX
TESTS		+= $(VICARE_SCHEME_SRFI_POSIX_TESTS)
endif
endif

if WANT_NAUSICAA
TESTS		+= $(VICARE_SCHEME_NAUSICAA_TESTS)
endif

if WANT_LIBFFI
TESTS		+= $(VICARE_SCHEME_FFI_TESTS)
endif

if WANT_POSIX
TESTS		+= $(VICARE_SCHEME_POSIX_TESTS)
endif

if WANT_GLIBC
TESTS		+= $(VICARE_SCHEME_GLIBC_TESTS)
endif

if WANT_LIBICONV
TESTS		+= $(VICARE_SCHEME_ICONV_TESTS)
endif

if WANT_LINUX
TESTS		+= $(VICARE_SCHEME_LINUX_TESTS)
endif

if WANT_CRE2
TESTS		+= $(VICARE_SCHEME_CRE2_TESTS)
endif

#page
#### running the test suite: customised test rules

VICARE_SELECTED_TESTS		= $(testdir)/test-*$(file)*.sps
VICARE_SELECTED_LONG_TESTS	= $(testdir)/long-test-*$(file)*.sps
VICARE_SELECTED_FOREIGN_TESTS	= $(testdir)/foreign-test-*$(file)*.sps
VICARE_SELECTED_DEMOS		= $(testdir)/demo-*$(file)*.sps

VICARE_TEST_FASL_PATH	= $(fasldir):$(VICARE_FASL_PATH)
VICARE_TEST_SEARCH_PATH	= $(srcdir)/tests:$(srcdir)/lib:$(builddir)/lib:$(srcdir)/scheme

# Enable timing  of test files  execution.  We cannot include  this in
# the separator because it needs  to be placed between the environment
# variables and the actual command line.
if WANT_TIME_TESTS
VICARE_TIME_TESTS	= $(TIME_PROGRAM) -p
endif

VICARE_TEST_FLAGS	= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS) --r6rs-script
VICARE_TEST_ENV		= \
	VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH); export VICARE_LIBRARY_PATH;	\
	VICARE_FASL_PATH=$(VICARE_TEST_FASL_PATH); export VICARE_FASL_PATH;		\
	CHECK_TEST_NAME=$(name); export CHECK_TEST_NAME;				\
	VICARE_SRC_DIR=$(srcdir); export VICARE_SRC_DIR;				\
	VICARE_BUILDDIR=$(builddir); export VICARE_BUILDDIR;				\
	SILEX_PATH=$(srcdir)/../lib/vicare/parser-tools/silex/macros; export SILEX_PATH;\
	LD_LIBRARY_PATH=$(builddir)/../src/.libs; export LD_LIBRARY_PATH;
	DYLD_LIBRARY_PATH=$(builddir)/../src/.libs; export DYLD_LIBRARY_PATH;
VICARE_TEST_RUN		= $(VICARE_TEST_ENV) $(VICARE_TIME_TESTS) $(GDB) vicare $(VICARE_TEST_FLAGS)

## --------------------------------------------------------------------
## interface to "make check"

# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#
# We must make sure that this ends with a semicolon!!!
AM_TESTS_ENVIRONMENT	= VICARE_CHECK_QUIET=yes; export VICARE_CHECK_QUIET; $(VICARE_TEST_ENV)
TEST_EXTENSIONS		= .sps
SPS_LOG_COMPILER	= vicare
AM_SPS_LOG_FLAGS	= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS)

## --------------------------------------------------------------------

.PHONY: test tests test-r6rs long-test long-tests
.PHONY: demo demos foreign-test foreign-tests

test tests:
	@echo
	@for f in $(VICARE_SELECTED_TESTS); do $(VICARE_TEST_RUN) $$f; done
	@echo

long-test long-tests:
	@echo
	@for f in $(VICARE_SELECTED_LONG_TESTS); do $(VICARE_TEST_RUN) $$f; done
	@echo

foreign-test foreign-tests:
	@echo
	@for f in $(VICARE_SELECTED_FOREIGN_TESTS); do $(VICARE_TEST_RUN) $$f; done
	@echo

demo demos:
	@echo
	@for f in $(VICARE_SELECTED_DEMOS); do $(VICARE_TEST_RUN) $$f; done
	@echo

## --------------------------------------------------------------------

## SILex test files

.PHONY: silex-test

silex-test:
	@for f in $(testdir)/make-silex-*.sps; do export testdir=$(testdir); $(VICARE_TEST_RUN) $$f; done

## special rules: LALR test parsers building

.PHONY: lalr-test

lalr-test:
	@for f in $(testdir)/make-lalr-*.sps; do export testdir=$(testdir); $(VICARE_TEST_RUN) $$f; done

#page
#### interface to "make instcheck"

VICARE_INSTCHECK_LIBRARY_PATH = $(VICARE_LIBRARY_PATH):$(srcdir):$(srcdir)/scheme

VICARE_INSTCHECK_FLAGS	= --print-loaded-libraries $(AX_VFLAGS) $(VFLAGS)
VICARE_INSTCHECK_ENV	= \
	VICARE_LIBRARY_PATH=$(VICARE_INSTCHECK_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	LD_LIBRARY_PATH=$(DESTDIR)$(libdir);			export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(DESTDIR)$(libdir);			export DYLD_LIBRARY_PATH;	\
	VICARE_SRC_DIR=$(srcdir);				export VICARE_SRC_DIR;		\
	VICARE_CHECK_QUIET=no;					export VICARE_CHECK_QUIET;	\
	SILEX_PATH=$(srcdir)/../lib/vicare/parser-tools/silex;	export SILEX_PATH;
VICARE_INSTCHECK_RUN	= \
	$(VICARE_INSTCHECK_ENV) $(DESTDIR)$(bindir)/vicare $(VICARE_INSTCHECK_FLAGS)

installcheck-local:
	@for f in $(TESTS); do $(VICARE_INSTCHECK_RUN) $(srcdir)/$$f; done

#page
#### running vicare on the terminal from the build directory

VIE			= $(srcdir)/scripts/vie

VICARE_TERMINAL_RUN_FLAGS	= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS)
VICARE_TERMINAL_RUN_ENV	= \
	VICARE_FASL_PATH=$(fasldir):$(VICARE_FASL_PATH); export VICARE_FASL_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH); export VICARE_LIBRARY_PATH;	\
	VICARE_SRC_DIR=$(srcdir); export VICARE_SRC_DIR;				\
	VICARE_BUILDDIR=$(builddir); export VICARE_BUILDDIR;				\
	SILEX_PATH=$(srcdir)/lib/vicare/parser-tools/silex; export SILEX_PATH;
VICARE_TERMINAL_RUN	= $(VICARE_TERMINAL_RUN_ENV) vicare $(VICARE_TERMINAL_RUN_FLAGS)
VICARE_TERMINAL_VIE	= $(VICARE_TERMINAL_RUN_ENV) $(VIE) vicare --raw-repl $(VICARE_TERMINAL_RUN_FLAGS)

.PHONY: test-run test-vie test-exec-modes

test-run:
	$(VICARE_TERMINAL_RUN)

test-vie:
	$(VICARE_TERMINAL_VIE)

test-exec-modes:
	$(VICARE_TERMINAL_RUN_ENV) $(srcdir)/tests/test-exec-modes.sh vicare $(VICARE_NEW_BOOT) $(srcdir)/tests

#page
#### Linux binfmt support

if WANT_BINFMT
rcddir			= $(sysconfdir)/rc.d
rcd_SCRIPTS		= rc.vicare-scheme

rc.vicare-scheme: Makefile
	@echo '#!/bin/sh' >$(@)
	@echo '#' >>$(@)
	@echo '# Setup the  Linux module  "binfmt_misc" to automatically  recognise and' >>$(@)
	@echo '# run Vicare Scheme programs using the installed "vicare" executable.' >>$(@)
	@echo '' >>$(@)
	@echo 'if test -f /proc/sys/fs/binfmt_misc/register' >>$(@)
	@echo 'then' >>$(@)
	@echo '  if test -f /proc/sys/fs/binfmt_misc/VICARE' >>$(@)
	@echo '  then echo -1 >/proc/sys/fs/binfmt_misc/VICARE' >>$(@)
	@echo '  fi' >>$(@)
	@echo '  echo -n enabling binfmt Vicare Scheme scripts execution...' >>$(@)
	@echo '  echo ":VICARE:M:0:;;;!vicare::$(bindir)/vicare:" >/proc/sys/fs/binfmt_misc/register' >>$(@)
	@echo '  echo done' >>$(@)
	@echo 'fi' >>$(@)
	@echo '' >>$(@)
	@echo '### end of file' >>$(@)

endif

#page
#### demos

EXTRA_DIST	+= \
	demos/README			\
	demos/httpd.sps			\
	demos/run-httpd.sh		\
	demos/echod.sps			\
	demos/run-echod.sh		\
	demos/shelld.sps		\
	demos/run-shelld.sh		\
	demos/connect.sps		\
	demos/run-connect.sh		\
	demos/srfi-106-echo-client.sps	\
	demos/srfi-106-echo-server.sps

### end of file
