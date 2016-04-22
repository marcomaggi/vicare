## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
EXTRA_DIST		= configure.sh configure32.sh configure64.sh	\
			  bare-configure.sh cygwin-configure.sh		\
			  meta/autotools/config.rpath INSTALL

slsdir			= $(srcdir)/lib
fasldir			= $(builddir)/lib
testdir			= $(srcdir)/tests
bundledlibsdir		= $(libdir)/vicare-scheme
instdir			= $(bundledlibsdir)

CLEANFILES		=
MAINTAINERCLEANFILES	=
BUILT_SOURCES		=

# This must be the pathname to the SILex macros files.
SILEX_PATH		= $(srcdir)/lib/vicare/parser-tools/silex/macros

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

nodist_doc_DATA = typed-core-primitives/README

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
	doc/progutils.texi				\
	doc/includes.texi				\
	doc/foreign.texi				\
	doc/ffi.texi					\
	doc/syslib.texi					\
	doc/types.texi					\
	doc/objects.texi				\
	doc/debug.texi					\
	doc/expander.texi				\
	doc/machinery.texi				\
	doc/compiler.texi				\
	doc/boot-image.texi				\
	\
	doc/missing.texi				\
	doc/building.texi				\
	doc/credits.texi				\
	doc/dynamic-environment.texi			\
	doc/resources.texi				\
	doc/unicode.texi				\
	doc/fasl.texi					\
	doc/shmem.texi					\
	doc/binfmt.texi					\
	doc/notes.texi					\
	doc/history.texi				\
	doc/notes.texi					\
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
	\
	doc/libs-arguments-validation.texi		\
	doc/libs-arrays.texi				\
	doc/libs-binary-heaps.texi			\
	doc/libs-binary-search-trees.texi		\
	doc/libs-bytevectors.texi			\
	doc/libs-bytevector-compounds.texi		\
	doc/libs-cbuffers.texi				\
	doc/libs-chains.texi				\
	doc/libs-char-sets.texi				\
	doc/libs-checks.texi				\
	doc/libs-comparators.texi			\
	doc/libs-comparisons.texi			\
	doc/libs-conditions-and-restarts.texi		\
	doc/libs-debugging.texi				\
	doc/libs-deques.texi				\
	doc/libs-dynamic-arrays.texi			\
	doc/libs-flonum-format.texi			\
	doc/libs-flonum-parser.texi			\
	doc/libs-formations.texi			\
	doc/libs-getopts.texi				\
	doc/libs-gcc.texi				\
	doc/libs-ilists.texi				\
	doc/libs-irregex.texi				\
	doc/libs-iteration-thunks.texi			\
	doc/libs-ideques.texi				\
	doc/libs-iqueues.texi				\
	doc/libs-istacks.texi				\
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
	doc/libs-ralists.texi				\
	doc/libs-randomisations.texi			\
	doc/libs-readline.texi				\
	doc/libs-sets-and-bags.texi			\
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
	doc/srfi-comparators.texi			\
	doc/srfi-compare-procedures.texi		\
	doc/srfi-cond-expand.texi			\
	doc/srfi-eager-comp.texi			\
	doc/srfi-environment-variables.texi		\
	doc/srfi-environment-inquiry.texi		\
	doc/srfi-err5rs-records.texi			\
	doc/srfi-error-reporting.texi			\
	doc/srfi-general-cond.texi			\
	doc/srfi-immutable-lists.texi			\
	doc/srfi-intermediate-format.texi		\
	doc/srfi-hash-tables.texi			\
	doc/srfi-lazy.texi				\
	doc/srfi-let-values.texi			\
	doc/srfi-lightweight-testing.texi		\
	doc/srfi-list.texi				\
	doc/srfi-multidimensional-array.texi		\
	doc/srfi-parameters.texi			\
	doc/srfi-ralists.texi				\
	doc/srfi-random.texi				\
	doc/srfi-rec.texi				\
	doc/srfi-receive.texi				\
	doc/srfi-records.texi				\
	doc/srfi-regular-expressions.texi		\
	doc/srfi-sets-and-bags.texi			\
	doc/srfi-shared-structures.texi			\
	doc/srfi-specialize-procedures.texi		\
	doc/srfi-streams.texi				\
	doc/srfi-string-ports.texi			\
	doc/srfi-strings.texi				\
	doc/srfi-testing.texi				\
	doc/srfi-time.texi				\
	doc/srfi-vectors.texi

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

VICARE_NEW_EXECUTABLE	= $(builddir)/vicare

AM_CPPFLAGS	= -I $(srcdir)/src -I $(builddir)/src
AM_CFLAGS	= -Wall
EXTRA_DIST	+= src/vicare-platform.h.in

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
	src/internals.h

nodist_vicare_SOURCES	= src/bootfileloc.h
vicare_DEPENDENCIES	= src/vicare.h
include_HEADERS		= src/vicare.h src/vicare-platform.h

CLEANFILES		+= src/bootfileloc.h
BUILT_SOURCES		+= src/bootfileloc.h

# The target "src/bootfileloc.h" is  automatically built by "make all"
# and "make  check" because we  have included it in  the BUILT_SOURCES
# variable.  But  when doint "make vicare"  it is not built  unless we
# declare the dependency below.
#
src/ikarus.$(OBJEXT): src/bootfileloc.h

# Strictly speaking: this  target should depend on  Makfile, but doing
# it that way causes a lot of unnecessary rebuilds.
src/bootfileloc.h:
	echo '#define BOOTFILE "$(pkglibdir)/vicare.boot"' >$@
	echo '#define EXEFILE "$(bindir)/vicare"' >>$@

# List of flags to link libraries with the "vicare" executable.
vicare_LDADD		=

# Add Libiconv if requested by the configuration script.
if WANT_LIBICONV
vicare_LDADD		+= @LTLIBICONV@
endif

#page
#### compiling the boot image: source files

VICARE_BOOT_SRCS = \
	scheme/ikarus.wordsize.scm							\
	scheme/ikarus.intel-assembler.sls						\
	scheme/ikarus.compiler.scheme-objects-layout.scm				\
	scheme/ikarus.compiler.condition-types.sls					\
	\
	scheme/ikarus.compiler.core-primitive-properties.base.sls			\
	scheme/ikarus.compiler.core-primitive-properties.configuration.sls		\
	scheme/ikarus.compiler.core-primitive-properties.booleans.sls			\
	scheme/ikarus.compiler.core-primitive-properties.characters.sls			\
	scheme/ikarus.compiler.core-primitive-properties.fixnums.sls			\
	scheme/ikarus.compiler.core-primitive-properties.bignums.sls			\
	scheme/ikarus.compiler.core-primitive-properties.ratnums.sls			\
	scheme/ikarus.compiler.core-primitive-properties.flonums.sls			\
	scheme/ikarus.compiler.core-primitive-properties.cflonums.sls			\
	scheme/ikarus.compiler.core-primitive-properties.compnums.sls			\
	scheme/ikarus.compiler.core-primitive-properties.code-objects.sls		\
	scheme/ikarus.compiler.core-primitive-properties.strings.sls			\
	scheme/ikarus.compiler.core-primitive-properties.symbols.sls			\
	scheme/ikarus.compiler.core-primitive-properties.keywords.sls			\
	scheme/ikarus.compiler.core-primitive-properties.pointers.sls			\
	scheme/ikarus.compiler.core-primitive-properties.bytevectors.sls		\
	scheme/ikarus.compiler.core-primitive-properties.pairs-and-lists.sls		\
	scheme/ikarus.compiler.core-primitive-properties.vectors.sls			\
	scheme/ikarus.compiler.core-primitive-properties.structs.sls			\
	scheme/ikarus.compiler.core-primitive-properties.records.sls			\
	scheme/ikarus.compiler.core-primitive-properties.hash-tables.sls		\
	scheme/ikarus.compiler.core-primitive-properties.annotation-objects.sls		\
	scheme/ikarus.compiler.core-primitive-properties.enum-sets.sls			\
	scheme/ikarus.compiler.core-primitive-properties.condition-objects.sls		\
	scheme/ikarus.compiler.core-primitive-properties.transcoder-objects.sls		\
	scheme/ikarus.compiler.core-primitive-properties.control.sls			\
	scheme/ikarus.compiler.core-primitive-properties.generic-primitives.sls		\
	scheme/ikarus.compiler.core-primitive-properties.input-output.sls		\
	scheme/ikarus.compiler.core-primitive-properties.environment-inquiry.sls	\
	scheme/ikarus.compiler.core-primitive-properties.numerics.sls			\
	scheme/ikarus.compiler.core-primitive-properties.times-and-dates.sls		\
	scheme/ikarus.compiler.core-primitive-properties.library-utils.sls		\
	scheme/ikarus.compiler.core-primitive-properties.expander.sls			\
	scheme/ikarus.compiler.core-primitive-properties.eval-and-environments.sls	\
	scheme/ikarus.compiler.core-primitive-properties.ffi.sls			\
	scheme/ikarus.compiler.core-primitive-properties.posix.sls			\
	scheme/ikarus.compiler.core-primitive-properties.sls				\
	\
	scheme/ikarus.compiler.compat.sls						\
	scheme/ikarus.compiler.config.sls						\
	scheme/ikarus.compiler.helpers.sls						\
	scheme/ikarus.compiler.system-value.sls						\
	scheme/ikarus.compiler.typedefs.sls						\
	scheme/ikarus.compiler.scheme-objects-ontology.sls				\
	scheme/ikarus.compiler.unparse-recordised-code.sls				\
	scheme/ikarus.compiler.core-primitive-operation-names.sls			\
	scheme/ikarus.compiler.pass-recordise.sls					\
	scheme/ikarus.compiler.pass-optimize-direct-calls.sls				\
	scheme/ikarus.compiler.pass-letrec-optimizer.sls				\
	scheme/ikarus.compiler.pass-source-optimizer.sls				\
	scheme/ikarus.compiler.pass-rewrite-references-and-assignments.sls		\
	scheme/ikarus.compiler.pass-core-type-inference.sls				\
	scheme/ikarus.compiler.pass-introduce-unsafe-primrefs.sls			\
	scheme/ikarus.compiler.pass-sanitize-bindings.sls				\
	scheme/ikarus.compiler.pass-optimize-for-direct-jumps.sls			\
	scheme/ikarus.compiler.pass-insert-global-assignments.sls			\
	scheme/ikarus.compiler.pass-introduce-vars.sls					\
	scheme/ikarus.compiler.pass-introduce-closure-makers.sls			\
	scheme/ikarus.compiler.pass-optimize-combinator-calls-lift-clambdas.sls		\
	scheme/ikarus.compiler.pass-introduce-primitive-operation-calls.sls		\
	scheme/ikarus.compiler.pass-rewrite-freevar-references.sls			\
	scheme/ikarus.compiler.pass-insert-engine-checks.sls				\
	scheme/ikarus.compiler.pass-insert-stack-overflow-check.sls			\
	scheme/ikarus.compiler.code-generation.sls					\
	scheme/ikarus.compiler.pass-specify-representation.sls				\
	scheme/ikarus.compiler.core-primitive-operations.sls				\
	scheme/ikarus.compiler.pass-impose-evaluation-order.sls				\
	scheme/ikarus.compiler.pass-assign-frame-sizes.sls				\
	scheme/ikarus.compiler.pass-color-by-chaitin.sls				\
	scheme/ikarus.compiler.pass-flatten-codes.sls					\
	scheme/ikarus.compiler.intel-assembly.sls					\
	scheme/ikarus.compiler.common-assembly-subroutines.sls				\
	scheme/ikarus.compiler.sls							\
	\
	scheme/ikarus.apply.sls						\
	scheme/ikarus.apropos.sls					\
	scheme/ikarus.bignums.sls					\
	scheme/ikarus.bytevectors.sls					\
	scheme/ikarus.cafe.sls						\
	scheme/ikarus.chars.sls						\
	scheme/ikarus.code-objects.sls					\
	scheme/ikarus.codecs.sls					\
	scheme/ikarus.collect.sls					\
	scheme/ikarus.compensations.sls					\
	scheme/ikarus.command-line.sls					\
	scheme/ikarus.conditions.sls					\
	scheme/ikarus.control.sls					\
	scheme/ikarus.coroutines.sls					\
	scheme/ikarus.debugger.sls					\
	scheme/ikarus.emergency.sls					\
	scheme/ikarus.enumerations.sls					\
	scheme/ikarus.environment-inquiry.sls				\
	scheme/ikarus.equal.sls						\
	scheme/ikarus.exceptions.sls					\
	scheme/ikarus.fasl.read.sls					\
	scheme/ikarus.fasl.write.sls					\
	scheme/ikarus.fixnums.sls					\
	scheme/ikarus.foldable.sls					\
	scheme/ikarus.guardians.sls					\
	scheme/ikarus.handlers.sls					\
	scheme/ikarus.hash-tables.sls					\
	scheme/ikarus.helpers.sls					\
	scheme/ikarus.keywords.sls					\
	scheme/ikarus.io.sls						\
	scheme/ikarus.io.port-attributes.scm				\
	scheme/ikarus.io.port-constructors.scm				\
	scheme/ikarus.io.port-predicates.scm				\
	scheme/ikarus.io.port-procedures.scm				\
	scheme/ikarus.io.reading.scm					\
	scheme/ikarus.io.writing.scm					\
	scheme/ikarus.io.file-descriptor-ports.scm			\
	scheme/ikarus.lists.sls						\
	scheme/ikarus.load.sls						\
	scheme/ikarus.load.dynamic-library-loading.sls			\
	scheme/ikarus.main.sls						\
	scheme/ikarus.multiple-values.sls				\
	scheme/ikarus.numerics.flonums.sls				\
	scheme/ikarus.numerics.generic-arithmetic.sls			\
	scheme/ikarus.numerics.flonum-conversion.sls			\
	scheme/ikarus.numerics.rationalize.sls				\
	scheme/ikarus.numerics.div-and-mod.sls				\
	scheme/ikarus.numerics.flonums.div-and-mod.sls			\
	scheme/ikarus.numerics.bitwise.misc.sls				\
	scheme/ikarus.numerics.complex-numbers.sls			\
	scheme/ikarus.object-utilities.sls				\
	scheme/ikarus.options.sls					\
	scheme/ikarus.printing-messages.sls				\
	scheme/ikarus.pairs.sls						\
	scheme/ikarus.immutable-pairs.sls				\
	scheme/ikarus.pointers.sls					\
	scheme/ikarus.posix.sls						\
	scheme/ikarus.predicates.sls					\
	scheme/ikarus.booleans.sls					\
	scheme/ikarus.pretty-formats.sls				\
	scheme/ikarus.pretty-print.sls					\
	scheme/ikarus.promises.sls					\
	scheme/ikarus.foreign-libraries.sls				\
	scheme/ikarus.ratnums.sls					\
	scheme/ikarus.reader.sls					\
	scheme/ikarus.readline.sls					\
	scheme/ikarus.records.procedural.sls				\
	scheme/ikarus.run-time-configuration.sls			\
	scheme/ikarus.singular-objects.sls				\
	scheme/ikarus.sort.sls						\
	scheme/ikarus.string-to-number.sls				\
	scheme/ikarus.strings.sls					\
	scheme/ikarus.strings-table.sls					\
	scheme/ikarus.structs.sls					\
	scheme/ikarus.symbol-table.sls					\
	scheme/ikarus.symbols.sls					\
	scheme/ikarus.time-and-date.sls					\
	scheme/ikarus.timer.sls						\
	scheme/ikarus.string-bytevector-conversion.sls			\
	scheme/ikarus.unicode.sls					\
	scheme/ikarus.unique-objects.sls				\
	scheme/ikarus.unwind-protection.sls				\
	scheme/ikarus.vectors.sls					\
	scheme/ikarus.writer.sls					\
	\
	scheme/psyntax.config.sls					\
	scheme/psyntax.setup.sls					\
	scheme/psyntax.helpers.scm					\
	scheme/psyntax.builders.sls					\
	scheme/psyntax.compat.sls					\
	scheme/psyntax.special-transformers.sls				\
	scheme/psyntax.lexical-environment.sls				\
	scheme/psyntax.lexical-environment.object-type-specs.scm	\
	scheme/psyntax.lexical-environment.typed-variable-specs.scm	\
	scheme/psyntax.lexical-environment.syntactic-bindings.scm	\
	scheme/psyntax.lexical-environment.syntax-match.scm		\
	scheme/psyntax.lexical-environment.syntax-utilities.scm		\
	scheme/psyntax.lexical-environment.type-signatures.scm		\
	scheme/psyntax.lexical-environment.type-callables.scm		\
	scheme/psyntax.lexical-environment.type-syntax-objects.scm	\
	scheme/psyntax.lexical-environment.condition-objects.scm	\
	scheme/psyntax.syntax-utilities.sls				\
	scheme/psyntax.syntactic-binding-properties.sls			\
	scheme/psyntax.library-collectors.sls				\
	scheme/psyntax.import-spec-parser.sls				\
	scheme/psyntax.export-spec-parser.sls				\
	scheme/psyntax.non-core-macro-transformers.sls			\
	scheme/psyntax.non-core-macro-transformers.define-record-type.scm	\
	scheme/psyntax.non-core-macro-transformers.define-struct.scm	\
	scheme/psyntax.non-core-macro-transformers.infix-macro.scm	\
	scheme/psyntax.chi-procedures.sls				\
	scheme/psyntax.chi-procedures.body.scm				\
	scheme/psyntax.chi-procedures.qdef.scm				\
	scheme/psyntax.chi-procedures.lambda.scm			\
	scheme/psyntax.chi-procedures.application.scm			\
	scheme/psyntax.chi-procedures.core-macro-transformers.scm	\
	scheme/psyntax.chi-procedures.type-macro-transformers.scm	\
	scheme/psyntax.expander.sls					\
	scheme/psyntax.internal.sls					\
	scheme/psyntax.library-utils.sls				\
	scheme/psyntax.library-manager.sls

EXTRA_DIST += \
	$(VICARE_BOOT_SRCS)					\
	boot/vicare.boot.4.prebuilt				\
	boot/vicare.boot.8.prebuilt				\
	scheme/makefile.sps					\
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

#page
#### compiling the boot image: build rules

VICARE_NEW_BOOT		= vicare.boot
VICARE_BOOT_MAKEFILES	= \
	scheme/makefile.sps					\
	scheme/makefile.scheme-object-types.scm			\
	scheme/makefile.built-in-record-types.scm		\
	scheme/makefile.built-in-condition-object-types.scm	\
	scheme/makefile.built-in-type-annotations.scm

VICARE_BOOT_TYPED_CORE_PRIMITIVES_LIBS = \
	typed-core-primitives/build-specs.sps			\
	typed-core-primitives/syntaxes.sls			\
	\
	typed-core-primitives/annotation-objects.sls		\
	typed-core-primitives/bignums.sls			\
	typed-core-primitives/booleans.sls			\
	typed-core-primitives/bytevectors.sls			\
	typed-core-primitives/cflonums.sls			\
	typed-core-primitives/characters.sls			\
	typed-core-primitives/code-objects.sls			\
	typed-core-primitives/compnums.sls			\
	typed-core-primitives/compiler.sls			\
	typed-core-primitives/condition-objects.sls		\
	typed-core-primitives/configuration.sls			\
	typed-core-primitives/collect.sls			\
	typed-core-primitives/control.sls			\
	typed-core-primitives/enum-sets.sls			\
	typed-core-primitives/environment-inquiry.sls		\
	typed-core-primitives/eval-and-environments.sls		\
	typed-core-primitives/expander.sls			\
	typed-core-primitives/ffi.sls				\
	typed-core-primitives/fixnums.sls			\
	typed-core-primitives/flonums.sls			\
	typed-core-primitives/generic-primitives.sls		\
	typed-core-primitives/hash-tables.sls			\
	typed-core-primitives/input-output.sls			\
	typed-core-primitives/keywords.sls			\
	typed-core-primitives/library-utils.sls			\
	typed-core-primitives/numerics.sls			\
	typed-core-primitives/object-utilities.sls		\
	typed-core-primitives/pairs-and-lists.sls		\
	typed-core-primitives/pointers.sls			\
	typed-core-primitives/posix.sls				\
	typed-core-primitives/ratnums.sls			\
	typed-core-primitives/records.sls			\
	typed-core-primitives/strings.sls			\
	typed-core-primitives/structs.sls			\
	typed-core-primitives/symbols.sls			\
	typed-core-primitives/times-and-dates.sls		\
	typed-core-primitives/transcoders.sls			\
	typed-core-primitives/vectors.sls

EXTRA_DIST		+= \
	$(VICARE_BOOT_MAKEFILES)				\
	$(VICARE_BOOT_TYPED_CORE_PRIMITIVES_LIBS)

# The  "exec" portion  of the  Makefile variable  names will  instruct
# Automake to  generate installation rules  to install the  boot image
# with the "make install-exec" rule.
bootexecdir		= $(pkglibdir)
bootexec_DATA		= $(VICARE_NEW_BOOT)

BOOT_SEARCH_PATH	= $(srcdir)/scheme:$(builddir)/scheme:$(slsdir):$(fasldir):$(builddir)
VICARE_PREBUILT_BOOT	= $(srcdir)/boot/vicare.boot.$(VICARE_SIZE_OF_VOIDP).prebuilt
VICARE_BOOT_ENV		= \
	VICARE_WORDSIZE=$(VICARE_SIZE_OF_VOIDP);	export VICARE_WORDSIZE;		\
	VICARE_SRC_DIR=$(srcdir)/scheme;		export VICARE_SRC_DIR;		\
	VICARE_BUILD_DIR=$(builddir)/scheme;		export VICARE_BUILD_DIR;	\
	VICARE_LIBRARY_PATH=$(BOOT_SEARCH_PATH);	export VICARE_LIBRARY_PATH;	\
	VICARE_SOURCE_PATH=$(BOOT_SEARCH_PATH);		export VICARE_SOURCE_PATH;
VICARE_BOOT_FLAGS	= $(AX_BOOT_VFLAGS) $(BOOT_VFLAGS)				\
			  --no-rcfile --library-locator source --r6rs-script

CLEANFILES		+= $(VICARE_NEW_BOOT) typed-core-primitives-specs.scm

VICARE_BOOT_PREREQUISITES	= \
	vicare						\
	scheme/ikarus.config.scm			\
	$(VICARE_BOOT_MAKEFILES)			\
	typed-core-primitives-specs.scm			\
	\
	lib/vicare/platform/words.vicare.sls		\
	lib/vicare/platform/constants.vicare.sls	\
	lib/vicare/platform/errno.vicare.sls		\
	\
	lib/vicare/unsafe/unicode.vicare.sls		\
	lib/vicare/unsafe/capi.vicare.sls		\
	lib/vicare/unsafe/operations.vicare.sls

## --------------------------------------------------------------------

typed-core-primitives-specs.scm: $(VICARE_BOOT_TYPED_CORE_PRIMITIVES_LIBS) vicare
	$(VICARE_NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT) --source-path $(srcdir)	\
		--option print-loaded-libraries						\
		--r6rs-script $(srcdir)/typed-core-primitives/build-specs.sps >$@

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
$(VICARE_NEW_BOOT): $(VICARE_BOOT_SRCS) $(VICARE_BOOT_PREREQUISITES)
	$(VICARE_BOOT_ENV) $(GDB) $(VICARE_NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT) \
		$(VICARE_BOOT_FLAGS) $(srcdir)/scheme/makefile.sps

## --------------------------------------------------------------------

.PHONY: boot-image-32-bit boot-image-64-bit

boot-image-32-bit: vicare
	$(VICARE_BOOT_ENV); VICARE_WORDSIZE=4; export VICARE_WORDSIZE; \
	$(VICARE_NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT) \
		$(VICARE_BOOT_FLAGS) $(srcdir)/scheme/makefile.sps
	mv $(builddir)/vicare.boot $(builddir)/vicare.boot.4.prebuilt

boot-image-64-bit: vicare
	$(VICARE_BOOT_ENV); VICARE_WORDSIZE=8; export VICARE_WORDSIZE; \
	$(VICARE_NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT) \
		$(VICARE_BOOT_FLAGS) $(srcdir)/scheme/makefile.sps
	mv $(builddir)/vicare.boot $(builddir)/vicare.boot.8.prebuilt

## --------------------------------------------------------------------

# We can  try to build  Vicare itself with  a newly built  boot image;
# do this from the build directory:
#
#   $ cp vicare.boot vicare.boot.try
#
# then:
#
#   $ make try-vicare-boot
#
# when we  are sure that  the newly built  boot image is fine,  we can
# adopt it as boot image to be distributed; on a 32-bit platform:
#
#   $ cp vicare.boot.try $(top_srcdir)/boot/vicare.boot.4.prebuilt
#
# on a 64-bit platform:
#
#   $ cp vicare.boot.try $(top_srcdir)/boot/vicare.boot.8.prebuilt
#
# but keep backups of the original one!!!
#
try-vicare-boot: $(VICARE_BOOT_SRCS) vicare
	BUILDING_ROTATION_BOOT_IMAGE=yes; export BUILDING_ROTATION_BOOT_IMAGE;		\
	$(VICARE_BOOT_ENV) $(VICARE_NEW_EXECUTABLE) -b $(builddir)/vicare.boot.try	\
		$(VICARE_BOOT_FLAGS) $(srcdir)/scheme/makefile.sps

#page
#### compiling bundled libraries: build rules

VICARE_COMPILE_SOURCE_PATH	= $(fasldir):$(slsdir)
VICARE_COMPILE_LIBRARY_PATH	= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_LD_PATH		= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_COMPILE_DYLD_PATH	= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

VICARE_COMPILE_ENV	= \
	VICARE_SOURCE_PATH=$(VICARE_COMPILE_SOURCE_PATH);	export VICARE_SOURCE_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	SILEX_PATH=$(SILEX_PATH);				export SILEX_PATH;		\
	LD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_COMPILE_DYLD_PATH);		export DYLD_LIBRARY_PATH;
VICARE_COMPILE_FLAGS	= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS) \
			  --library-locator compile-time --build-directory $(fasldir)
VICARE_COMPILE_RUN	= $(VICARE_COMPILE_ENV) $(GDB) $(VICARE_NEW_EXECUTABLE) $(VICARE_COMPILE_FLAGS)

FASL_PREREQUISITES	= $(VICARE_NEW_BOOT) vicare

## --------------------------------------------------------------------

BUILT_SOURCES		+= lib/vicare/platform/features.vicare.sls
CLEANFILES		+= lib/vicare/platform/features.vicare.sls
noinst_PROGRAMS		= features
features_SOURCES	= lib/features.c

lib/vicare/platform/features.vicare.sls : features
	$(MKDIR_P) lib/vicare/platform && ./features >$@

## --------------------------------------------------------------------

EXTRA_DIST		+= \
	lib/libraries.scm				\
	scripts/compile-all.sps				\
	scripts/build-makefile-rules.sps		\
	\
	lib/srfi/%3a19/read-leap-seconds-table.sps	\
	lib/srfi/%3a19/tai-utc.dat

VICARE_BUILD_DEPS	= \
	$(VICARE_COMPILE_RUN) \
		--source-path $(testdir) --library-locator source \
		--r6rs-script $(srcdir)/scripts/build-makefile-rules.sps --

.PHONY: dependencies

dependencies:
	$(VICARE_BUILD_DEPS) $(slsdir)/libraries.scm >$(slsdir)/dependencies.make

# NOTE This  INCLUDE directive is  processed by Automake.   To specify
# the  pathname  of the  include  file:  we  can use  only  predefined
# makefile variables like "top_srcdir"; we cannot use custom variables
# defined by this very makefile.
include $(top_srcdir)/lib/dependencies.make

## --------------------------------------------------------------------

.PHONY: clean-fasl

clean-fasl:
	find $(fasldir)        -name \*.fasl -execdir rm -v '{}' ';'
	find $(builddir)/tests -name \*.fasl -execdir rm -v '{}' ';'

#page
#### special library rules: SILex lexers building

SILEX_SRCDIR		= $(slsdir)/vicare/parser-tools/silex
NAUIPADDR_SRCDIR	= $(slsdir)/nausicaa/parser-tools/ip-addresses

.PHONY: silex-internals silex-backup ip-address-tables

silex-internals:
	@echo; echo "*** Special rule: rebuilding SILex internals lexer tables"; echo
	@export SILEX_SRCDIR=$(SILEX_SRCDIR); $(VICARE_COMPILE_RUN) $(SILEX_SRCDIR)/make-tables.sps

silex-backup:
	@echo; echo "*** Special rule: backup of SILex internals lexer tables"; echo
	cp --verbose $(SILEX_SRCDIR)/*-l.sls $(SILEX_SRCDIR)/*.l $(SILEX_SRCDIR)/backup

ip-address-tables:
	@echo; echo "*** Special rule: rebuilding net libraries tables"; echo
	@export NAUIPADDR_SRCDIR=$(NAUIPADDR_SRCDIR); $(VICARE_COMPILE_RUN) $(NAUIPADDR_SRCDIR)/make-tables.sps

#page
#### running the test suite: test files

VICARE_SCHEME_R6RS_TESTS	= \
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
	tests/test-r6rs-unicode.sps

VICARE_SCHEME_CORE_TESTS	= \
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
	tests/test-vicare-booleans.sps					\
	tests/test-vicare-bytevectors.sps				\
	tests/test-vicare-builtin-posix.sps				\
	tests/test-vicare-chars.sps					\
	tests/test-vicare-collect.sps					\
	tests/test-vicare-compensations.sps				\
	tests/test-vicare-conditions-core.sps				\
	tests/test-vicare-convert-bytevectors.sps			\
	tests/test-vicare-coroutines.sps				\
	tests/test-vicare-enumerations.sps				\
	tests/test-vicare-environment-inquiry.sps			\
	tests/test-vicare-equality.sps					\
	tests/test-vicare-errno.sps					\
	tests/test-vicare-expander-basic.sps				\
	tests/test-vicare-expander-modules.sps				\
	tests/test-vicare-expander-lambda-extensions.sps		\
	tests/test-vicare-expander-lambda-star.sps			\
	tests/test-vicare-expander-loops.sps				\
	tests/test-vicare-expander-dynamic-environment.sps		\
	tests/test-vicare-debugging.sps					\
	tests/test-vicare-dynamic-environment.sps			\
	tests/test-vicare-dynamic-library-loading.sps			\
	tests/test-vicare-fasl.sps					\
	tests/test-vicare-fixnums.sps					\
	tests/test-vicare-flonum-base.sps				\
	tests/test-vicare-flonum-formatter.sps				\
	tests/test-vicare-flonum-parser.sps				\
	tests/test-vicare-hashtables.sps				\
	tests/test-vicare-infix.sps					\
	tests/test-vicare-io.sps					\
	tests/test-vicare-letrec-syntax.sps				\
	tests/test-vicare-library-utils.sps				\
	tests/test-vicare-library-object-type.sps			\
	tests/test-vicare-program-utils.sps				\
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
	tests/test-vicare-try.sps					\
	tests/test-vicare-quotations.sps				\
	tests/test-vicare-ratnums.sps					\
	tests/test-vicare-reader.sps					\
	tests/test-vicare-records-procedural.sps			\
	tests/test-vicare-records-syntactic.sps				\
	tests/test-vicare-round.sps					\
	tests/test-vicare-string-to-number.sps				\
	tests/test-vicare-strings.sps					\
	tests/test-vicare-structs-core.sps				\
	tests/test-vicare-symbols.sps					\
	tests/test-vicare-syntactic-extensions.sps			\
	tests/test-vicare-syntax-utilities.sps				\
	tests/test-vicare-system.sps					\
	tests/test-vicare-unwind-protection.sps				\
	tests/test-vicare-vectors.sps					\
	tests/test-vicare-writer.sps					\
	\
	tests/test-types-syntaxes.sps					\
	tests/test-types-enumerations.sps				\
	tests/test-types-objects-core.sps				\
	tests/test-types-objects-alists.sps				\
	tests/test-types-objects-booleans.sps				\
	tests/test-types-objects-chars.sps				\
	tests/test-types-objects-hashtables.sps				\
	tests/test-types-objects-ipairs.sps				\
	tests/test-types-objects-keywords.sps				\
	tests/test-types-objects-lists.sps				\
	tests/test-types-objects-numerics.sps				\
	tests/test-types-objects-pairs.sps				\
	tests/test-types-objects-pointers.sps				\
	tests/test-types-objects-ports.sps				\
	tests/test-types-objects-procedures.sps				\
	tests/test-types-objects-strings.sps				\
	tests/test-types-objects-symbols.sps				\
	tests/test-types-objects-transcoders.sps			\
	tests/test-types-objects-vectors.sps				\
	tests/test-types-structs.sps					\
	tests/test-types-records.sps					\
	tests/test-types-reader-annotations.sps				\
	tests/test-types-conditions.sps					\
	tests/test-types-unions.sps					\
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
	tests/test-issue-070.sps					\
	tests/test-issue-086.sps					\
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
	tests/test-vicare-language-extensions-custom-ports.sps		\
	tests/test-vicare-language-extensions-ensure.sps		\
	tests/test-vicare-language-extensions-hooks.sps			\
	tests/test-vicare-language-extensions-identifier-substitutions.sps	\
	tests/test-vicare-language-extensions-keywords.sps		\
	tests/test-vicare-language-extensions-loops.sps			\
	tests/test-vicare-language-extensions-makers.sps		\
	tests/test-vicare-language-extensions-pipelines.sps		\
	tests/test-vicare-language-extensions-restarts.sps		\
	tests/test-vicare-language-extensions-simple-match.sps		\
	tests/test-vicare-language-extensions-streams.sps		\
	tests/test-vicare-language-extensions-variables.sps		\
	\
	tests/test-vicare-containers-arrays.sps				\
	tests/test-vicare-containers-binary-heaps.sps			\
	tests/test-vicare-containers-binary-search-trees.sps		\
	tests/test-vicare-containers-bytevector-compounds.sps		\
	tests/test-vicare-containers-bytevectors-s8-high.sps		\
	tests/test-vicare-containers-bytevectors-s8-low.sps		\
	tests/test-vicare-containers-bytevectors-u8-high.sps		\
	tests/test-vicare-containers-bytevectors-u8-low.sps		\
	tests/test-vicare-containers-char-sets.sps			\
	tests/test-vicare-containers-chains.sps				\
	tests/test-vicare-containers-dynamic-arrays.sps			\
	tests/test-vicare-containers-kmp.sps				\
	tests/test-vicare-containers-iteration-thunks.sps		\
	tests/test-vicare-containers-levenshtein.sps			\
	tests/test-vicare-containers-lists-fun.sps			\
	tests/test-vicare-containers-lists-low.sps			\
	tests/test-vicare-containers-lists-stx.sps			\
	tests/test-vicare-containers-object-properties.sps		\
	tests/test-vicare-containers-one-dimension-cc.sps		\
	tests/test-vicare-containers-one-dimension-co.sps		\
	tests/test-vicare-containers-queues.sps				\
	tests/test-vicare-containers-deques.sps				\
	tests/test-vicare-containers-stacks.sps				\
	tests/test-vicare-containers-strings-high.sps			\
	tests/test-vicare-containers-strings-low.sps			\
	tests/test-vicare-containers-strings-rabin-karp.sps		\
	tests/test-vicare-containers-vectors-high.sps			\
	tests/test-vicare-containers-weak-hashtables.sps		\
	\
	tests/test-vicare-containers-istacks.sps			\
	tests/test-vicare-containers-iqueues.sps			\
	tests/test-vicare-containers-ideques.sps			\
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
	tests/test-vicare-parser-tools-silex-online.sps			\
	tests/test-vicare-parser-tools-unix-pathnames.sps

# Tests for the compiler's internals are written only for 64-bit platforms.
VICARE_SCHEME_COMPILER_TESTS	= \
	tests/test-vicare-compiler-scheme-objects-ontology.sps		\
	tests/test-vicare-compiler-internals.sps			\
	tests/test-vicare-compiler-pass-source-optimiser.sps		\
	tests/test-vicare-compiler-pass-core-type-inference.sps		\
	tests/test-vicare-compiler-pass-introduce-unsafe-primrefs.sps	\
	tests/test-vicare-compiler-pass-specify-representation.sps	\
	tests/test-vicare-compiler-pass-impose-eval-order.sps		\
	tests/test-vicare-compiler-pass-assign-frame-sizes.sps		\
	tests/test-vicare-compiler-pass-color-by-chaitin.sps		\
	tests/test-vicare-compiler-pass-flatten-codes.sps

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
	tests/test-srfi-101-ralists.sps					\
	tests/test-srfi-111-boxes.sps					\
	tests/test-srfi-112-environment-inquiry.sps			\
	tests/test-srfi-113-sets-and-bags.sps				\
	tests/test-srfi-114-comparators.sps				\
	tests/test-srfi-115-regexps.sps					\
	tests/test-srfi-116-ilists.sps

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
	tests/test-nausicaa-parser-tools-silex-misc.sps			\
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

#page
#### running the test suite: distribution files

EXTRA_DIST	+= \
	tests/libtest/ikarus-test-framework.sls		\
	tests/libtest/lists.sls				\
	tests/libtest/lists/low.sls			\
	tests/libtest/lists/stx.sls			\
	tests/libtest/numerics-helpers.sls		\
	tests/libtest/calc-code-lexer.sls		\
	tests/libtest/calc-parser-helper.sls		\
	tests/libtest/calc-parser-lexer.sls		\
	tests/libtest/calc-parser.sls			\
	tests/libtest/calc-portable-lexer.sls		\
	tests/libtest/calc-tree-lexer.sls		\
	tests/libtest/classes-lib.sls			\
	tests/libtest/compiler-internals.sls		\
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
	tests/demo-sendmail.sps				\
	tests/demo-mailx.sps				\
	tests/demo-curl.sps				\
	tests/demo-wget.sps				\
	tests/demo-find.sps				\
	tests/demo-parallel-process.sps			\
	\
	$(VICARE_SCHEME_R6RS_TESTS)			\
	$(VICARE_SCHEME_CORE_TESTS)			\
	$(VICARE_SCHEME_LONG_TESTS)			\
	$(VICARE_SCHEME_COMPILER_TESTS)			\
	$(VICARE_SCHEME_SRFI_TESTS)			\
	$(VICARE_SCHEME_SRFI_POSIX_TESTS)		\
	$(VICARE_SCHEME_NAUSICAA_TESTS)			\
	$(VICARE_SCHEME_FFI_TESTS)			\
	$(VICARE_SCHEME_POSIX_TESTS)			\
	$(VICARE_SCHEME_GLIBC_TESTS)			\
	$(VICARE_SCHEME_ICONV_TESTS)			\
	$(VICARE_SCHEME_LINUX_TESTS)

#page
#### interface for "make check": selecting test programs

TESTS		= $(VICARE_SCHEME_R6RS_TESTS) $(VICARE_SCHEME_CORE_TESTS) $(VICARE_SCHEME_LONG_TESTS)
XFAIL_TESTS	= \
	tests/test-issue-058-lalr-unary-prec.sps

# The following tests fails because Nausicaa and the half-written type
# system  in  (vicare)  are  incompatible.  In  future  they  will  be
# converted and fixed to work.  (Marco Maggi; Tue Jun 16, 2015)
XFAIL_TESTS	+= \
	tests/test-nausicaa-tags-docs.sps			\
	tests/test-nausicaa-oopp-binding-constructs.sps		\
	tests/test-nausicaa-oopp-cast-operator.sps		\
	tests/test-nausicaa-oopp-core.sps			\
	tests/test-nausicaa-oopp-builtin-labels.sps		\
	tests/test-nausicaa-conditions.sps			\
	tests/test-nausicaa-containers-iterators.sps		\
	tests/test-mehve-language-numerics-arithmetics.sps	\
	tests/test-mehve-language-infix.sps

if WANT_64BIT_SUPPORT
TESTS		+= $(VICARE_SCHEME_COMPILER_TESTS)
XFAIL_TESTS	+= tests/test-issue-086.sps
endif

if WANT_SRFI
TESTS		+= $(VICARE_SCHEME_SRFI_TESTS)
if WANT_POSIX
TESTS		+= $(VICARE_SCHEME_SRFI_POSIX_TESTS)
endif
endif

# if WANT_NAUSICAA
# TESTS		+= $(VICARE_SCHEME_NAUSICAA_TESTS)
# endif

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

#page
#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

VICARE_CHECK_LIBRARY_PATH	= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_CHECK_SOURCE_PATH	= $(testdir):$(slsdir):$(fasldir):$(srcdir)/scheme
VICARE_CHECK_LD_PATH		= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_CHECK_DYLD_PATH		= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

VICARE_CHECK_FLAGS		= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS) --r6rs-script
VICARE_CHECK_ENV		= \
	VICARE_CHECK_QUIET=yes;					export VICARE_CHECK_QUIET;	\
	VICARE_SOURCE_PATH=$(VICARE_CHECK_SOURCE_PATH);		export VICARE_SOURCE_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_CHECK_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	CHECK_TEST_NAME=$(name);				export CHECK_TEST_NAME;		\
	VICARE_SRC_DIR=$(srcdir)/tests;				export VICARE_SRC_DIR;		\
	VICARE_BUILDDIR=$(builddir);				export VICARE_BUILDDIR;		\
	SILEX_PATH=$(SILEX_PATH);				export SILEX_PATH;		\
	LD_LIBRARY_PATH=$(VICARE_CHECK_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_CHECK_DYLD_PATH);		export DYLD_LIBRARY_PATH;

# We must make sure that this ends with a semicolon!!!
AM_TESTS_ENVIRONMENT	= $(VICARE_CHECK_ENV)
TEST_EXTENSIONS		= .sps
SPS_LOG_COMPILER	= $(GDB) $(VICARE_NEW_EXECUTABLE)
AM_SPS_LOG_FLAGS	= $(VICARE_CHECK_FLAGS)

#page
#### interface to "make instcheck"

VICARE_INSTCHECK_LIBRARY_PATH	= $(DESTDIR)$(bundledlibsdir):$(testdir)
VICARE_INSTCHECK_SOURCE_PATH	= $(DESTDIR)$(bundledlibsdir):$(testdir):$(srcdir)/scheme
VICARE_INSTCHECK_LD_PATH	= $(DESTDIR)$(libdir)
VICARE_INSTCHECK_DYLD_PATH	= $(DESTDIR)$(libdir)

VICARE_INSTCHECK_FLAGS	= $(AX_VFLAGS) $(VFLAGS)
VICARE_INSTCHECK_ENV	= \
	VICARE_LIBRARY_PATH=$(VICARE_INSTCHECK_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	VICARE_SOURCE_PATH=$(VICARE_INSTCHECK_SOURCE_PATH);	export VICARE_SOURCE_PATH;	\
	LD_LIBRARY_PATH=$(VICARE_INSTCHECK_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_INSTCHECK_DYLD_PATH);	export DYLD_LIBRARY_PATH;	\
	VICARE_SRC_DIR=$(srcdir)/tests;				export VICARE_SRC_DIR;		\
	VICARE_CHECK_QUIET=no;					export VICARE_CHECK_QUIET;	\
	SILEX_PATH=$(SILEX_PATH);				export SILEX_PATH;
VICARE_INSTCHECK_RUN	= \
	$(VICARE_INSTCHECK_ENV) $(DESTDIR)$(bindir)/vicare $(VICARE_INSTCHECK_FLAGS)

installcheck-local:
	@for f in $(TESTS); do $(VICARE_INSTCHECK_RUN) $(srcdir)/$$f; done

#page
#### running the test suite: customised test rules

VICARE_TEST_SELECTED_TESTS		= $(testdir)/test-*$(file)*.sps
VICARE_TEST_SELECTED_LONG_TESTS		= $(testdir)/long-test-*$(file)*.sps
VICARE_TEST_SELECTED_FOREIGN_TESTS	= $(testdir)/foreign-test-*$(file)*.sps
VICARE_TEST_SELECTED_DEMOS		= $(testdir)/demo-*$(file)*.sps

VICARE_TEST_LIBRARY_PATH		= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_TEST_SOURCE_PATH			= $(testdir):$(slsdir):$(fasldir):$(srcdir)/scheme
VICARE_TEST_LD_PATH			= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_TEST_DYLD_PATH			= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

# Enable timing  of test files  execution.  We cannot include  this in
# the separator because it needs  to be placed between the environment
# variables and the actual command line.
if WANT_TIME_TESTS
VICARE_TIME_TESTS	= $(TIME_PROGRAM) -p
endif

VICARE_TEST_FLAGS	= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS) --r6rs-script
VICARE_TEST_ENV		= \
	VICARE_SOURCE_PATH=$(VICARE_TEST_SOURCE_PATH);		export VICARE_SOURCE_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_TEST_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	CHECK_TEST_NAME=$(name);				export CHECK_TEST_NAME;		\
	VICARE_SRC_DIR=$(srcdir)/tests;				export VICARE_SRC_DIR;		\
	VICARE_BUILDDIR=$(builddir);				export VICARE_BUILDDIR;		\
	SILEX_PATH=$(SILEX_PATH);				export SILEX_PATH;		\
	LD_LIBRARY_PATH=$(VICARE_TEST_LD_PATH);			export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_TEST_DYLD_PATH);		export DYLD_LIBRARY_PATH;
VICARE_TEST_RUN		= $(VICARE_TEST_ENV) $(VICARE_TIME_TESTS) $(GDB) $(VICARE_NEW_EXECUTABLE) $(VICARE_TEST_FLAGS)

## --------------------------------------------------------------------

.PHONY: test tests test-r6rs long-test long-tests
.PHONY: demo demos foreign-test foreign-tests

test tests:
	@echo
	@for f in $(VICARE_TEST_SELECTED_TESTS); do echo "running test: $$f"; $(VICARE_TEST_RUN) $$f; done
	@echo

long-test long-tests:
	@echo
	@for f in $(VICARE_TEST_SELECTED_LONG_TESTS); do echo "running test: $$f"; $(VICARE_TEST_RUN) $$f; done
	@echo

foreign-test foreign-tests:
	@echo
	@for f in $(VICARE_TEST_SELECTED_FOREIGN_TESTS); do echo "running test: $$f"; $(VICARE_TEST_RUN) $$f; done
	@echo

demo demos:
	@echo
	@for f in $(VICARE_TEST_SELECTED_DEMOS); do echo "running demo: $$f"; $(VICARE_TEST_RUN) $$f; done
	@echo

## --------------------------------------------------------------------
## special test rules

## SILex test files

.PHONY: silex-test

silex-test:
	@for f in $(testdir)/make-silex-*.sps; do export testdir=$(testdir); $(VICARE_TEST_RUN) $$f; done

## LALR test parsers building

.PHONY: lalr-test

lalr-test:
	@for f in $(testdir)/make-lalr-*.sps; do export testdir=$(testdir); $(VICARE_TEST_RUN) $$f; done

#page
#### compiling test files
#
# These  are very  rough rules,  not  integrated in  the GNU  Automake
# infrastructure.   The   purpose,  here,  is  to   excercise  program
# compilation to handle the libraries used by test programs.
#

VICARE_COMPILED_SELECTED_TESTS		= $(testdir)/test-*.sps $(testdir)/long-test-*.sps

VICARE_COMPILED_TESTS_SEARCH_PATH	= $(testdir):$(fasldir):$(srcdir)/scheme
VICARE_COMPILED_TESTS_LIBRARY_PATH		= $(fasldir):$(HOME)/.vicare/precompiled:$(VICARE_LIBRARY_PATH)
VICARE_COMPILED_TESTS_LD_PATH		= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_COMPILED_TESTS_DYLD_PATH		= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

VICARE_COMPILED_TESTS_COMPILE_FLAGS	= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS)	\
		--build-directory $(fasldir)						\
		--compile-program
VICARE_COMPILED_TESTS_RUN_FLAGS		= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS)	\
		--library-path $(fasldir)						\
		--build-directory $(fasldir)						\
		--binary-program
VICARE_COMPILED_TESTS_ENV	= \
	VICARE_SOURCE_PATH=$(VICARE_COMPILED_TESTS_SEARCH_PATH);   export VICARE_SOURCE_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_COMPILED_TESTS_LIBRARY_PATH); export VICARE_LIBRARY_PATH;	\
	VICARE_SRC_DIR=$(srcdir)/tests;				export VICARE_SRC_DIR;		\
	VICARE_BUILDDIR=$(builddir);				export VICARE_BUILDDIR;		\
	SILEX_PATH=$(SILEX_PATH);				export SILEX_PATH;		\
	LD_LIBRARY_PATH=$(VICARE_COMPILED_TESTS_LD_PATH);	export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_COMPILED_TESTS_DYLD_PATH);	export DYLD_LIBRARY_PATH;
VICARE_COMPILED_TESTS_COMPILE	= $(VICARE_COMPILED_TESTS_ENV) $(VICARE_NEW_EXECUTABLE) $(VICARE_COMPILED_TESTS_COMPILE_FLAGS)
VICARE_COMPILED_TESTS_RUN	= $(VICARE_COMPILED_TESTS_ENV) $(VICARE_NEW_EXECUTABLE) $(VICARE_COMPILED_TESTS_RUN_FLAGS)

CLEANFILES	+= $(builddir)/tests/*.fasl

## --------------------------------------------------------------------

.PHONY: compile-tests run-compiled-tests

compile-tests: all
	@echo
	@for f in $(VICARE_COMPILED_SELECTED_TESTS); do \
	    FASL=$(builddir)/tests/$$(basename $$f).fasl ; \
	    test -x $$FASL || ($(VICARE_COMPILED_TESTS_COMPILE) $$f -o $$FASL); \
	 done
	@echo

run-compiled-tests:
	@echo
	@for f in $(builddir)/tests/*.fasl; do $(VICARE_COMPILED_TESTS_RUN) $$f; done
	@echo

#page
#### running vicare on the terminal from the build directory

RLWRAP_TRAMPOLINE	= $(srcdir)/scripts/rlwrap-trampoline
EXTRA_DIST		+= $(RLWRAP_TRAMPOLINE)

VICARE_TERMINAL_LIBRARY_PATH	= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_TERMINAL_SOURCE_PATH	= $(slsdir)
VICARE_TERMINAL_LD_PATH		= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_TERMINAL_DYLD_PATH	= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

VICARE_TERMINAL_RUN_FLAGS	= -b $(VICARE_NEW_BOOT) $(AX_VFLAGS) $(VFLAGS)
VICARE_TERMINAL_RUN_ENV		= \
	VICARE_LIBRARY_PATH=$(VICARE_TERMINAL_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	VICARE_SOURCE_PATH=$(VICARE_TERMINAL_SOURCE_PATH);	export VICARE_SOURCE_PATH;	\
	VICARE_SRC_DIR=$(srcdir);				export VICARE_SRC_DIR;		\
	VICARE_BUILDDIR=$(builddir);				export VICARE_BUILDDIR;		\
	SILEX_PATH=$(SILEX_PATH);				export SILEX_PATH;		\
	LD_LIBRARY_PATH=$(VICARE_TERMINAL_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_TERMINAL_DYLD_PATH);		export DYLD_LIBRARY_PATH;
VICARE_TERMINAL_RUN	= $(VICARE_TERMINAL_RUN_ENV) $(VICARE_NEW_EXECUTABLE) $(VICARE_TERMINAL_RUN_FLAGS)

VICARE_TERMINAL_VIE_FLAGS	=  --raw-repl $(VICARE_TERMINAL_RUN_FLAGS)
VICARE_TERMINAL_VIE_ENV	= $(VICARE_TERMINAL_RUN_ENV)
VICARE_TERMINAL_VIE	= $(VICARE_TERMINAL_VIE_ENV) $(RLWRAP_TRAMPOLINE) $(VICARE_NEW_EXECUTABLE) $(VICARE_TERMINAL_VIE_FLAGS)

.PHONY: test-run test-vie test-exec-modes

test-run:
	$(VICARE_TERMINAL_RUN)

test-vie:
	$(VICARE_TERMINAL_VIE)

test-exec-modes:
	$(VICARE_TERMINAL_RUN_ENV) $(testdir)/test-exec-modes.sh $(VICARE_NEW_EXECUTABLE) $(VICARE_NEW_BOOT) $(testdir)

#page
#### installation of scheme-script

if WANT_INSTALL_SCHEME_SCRIPT

scheme_scriptdir	= $(bindir)
scheme_script_SCRIPTS	= scripts/scheme-script

endif

#page
#### Linux binfmt support

if WANT_BINFMT

binfmt_rcd_scriptsdir		= $(sysconfdir)/rc.d
binfmt_rcd_scripts_SCRIPTS	= scripts/rc.vicare-scheme

binfmt_launcher_scriptsdir	= $(libexecdir)/vicare-scheme
binfmt_launcher_scripts_SCRIPTS	= \
	scripts/binfmt-script-launcher.sh	\
	scripts/binfmt-program-launcher.sh

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
