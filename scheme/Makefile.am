## Process this file with automake to produce Makefile.in

NEW_BOOT_FILE		= vicare.boot
NEW_EXECUTABLE		= ../src/vicare

# The  "exec" portion  of the  Makefile variable  names  will instruct
# Automake to  generate installation rules  to install the  boot image
# with the "make install-exec" rule.
bootexecdir		= $(pkglibdir)
bootexec_DATA		= $(NEW_BOOT_FILE)
#nodist_pkglib_DATA	= $(NEW_BOOT_FILE)

BOOT_IMAGE_SEARCH_PATH	= .:$(srcdir):$(srcdir)/../lib:$(builddir)/../lib
VICARE_FASL_DIRECTORY	= $(builddir)/../lib/fasl

#VICARE_SIZE_OF_VOIDP		= $(shell grep SIZEOF_VOID_P ../config.h | sed "s/.*\(.\)/\1/g")
VICARE_SIZE_OF_VOIDP		= @VICARE_SIZE_OF_VOIDP@
VICARE_PLATFORM_ENDIANNESS	= @VICARE_PLATFORM_ENDIANNESS@
VICARE_ARGUMENTS_VALIDATION	= @VICARE_ARGUMENTS_VALIDATION@

IKARUS_PREBUILT_BOOT	= $(srcdir)/ikarus.boot.$(VICARE_SIZE_OF_VOIDP).prebuilt
VICARE_PREBUILT_BOOT	= $(srcdir)/vicare.boot.$(VICARE_SIZE_OF_VOIDP).prebuilt

CLEANFILES		= $(nodist_pkglib_DATA) ikarus.config.ss vicare.boot

all: $(nodist_pkglib_DATA)

## --------------------------------------------------------------------

ikarus.config.ss: Makefile ../config.h
	echo '(define vicare-version "$(PACKAGE_VERSION)")' >$@
	echo '(define vicare-lib-dir "$(pkglibdir)")' >>$@
	echo '(define scheme-lib-dir "$(libdir)/scheme/r6rs")' >>$@
	echo '(define (host-info) "$(target)")' >>$@
	echo '(define wordsize $(VICARE_SIZE_OF_VOIDP))' >>$@
	echo '(define platform-endianness (quote $(VICARE_PLATFORM_ENDIANNESS)))' >>$@
	echo '(define arguments-validation (positive? $(VICARE_ARGUMENTS_VALIDATION)))' >>$@
	echo '(define bootfile "$(pkglibdir)/vicare.boot")' >>$@

#page
# To  create the  new boot  image we  use a  prebuilt boot  image; the
# environment variable names are  hardcoded in the prebuilt boot image
# and are the Ikarus' ones.
#
#Notice that VICARE_FASL_DIRECTORY is set to the empty string: this is
#because  the  fasl  files  are  not generated  by  the  prebuilt  and
#distributed boot image, and so they cannot be used here.
#
$(NEW_BOOT_FILE): $(EXTRA_DIST) ikarus.config.ss
	VICARE_SRC_DIR=$(srcdir)			\
  VICARE_BUILD_DIR=$(builddir)				\
  VICARE_FASL_DIRECTORY=''				\
  VICARE_LIBRARY_PATH=$(BOOT_IMAGE_SEARCH_PATH)		\
  $(GDB) $(NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT)	\
  $(user_flags) --r6rs-script $(srcdir)/makefile.sps

#$(NEW_BOOT_FILE): $(EXTRA_DIST) ikarus.config.ss
# 	VICARE_SRC_DIR=$(srcdir)			\
#   VICARE_BUILD_DIR=$(builddir)			\
#   VICARE_FASL_DIRECTORY=''				\
#   VICARE_LIBRARY_PATH=$(BOOT_IMAGE_SEARCH_PATH)	\
#   $(NEW_EXECUTABLE) -b $(IKARUS_PREBUILT_BOOT)	\
#   --r6rs-script $(srcdir)/makefile.sps

## --------------------------------------------------------------------

.PHONY: boot-image-64-bit

boot-image-64-bit:
	echo '(define vicare-version "$(PACKAGE_VERSION)")' >ikarus.config.ss
	echo '(define vicare-lib-dir "$(pkglibdir)")' >>ikarus.config.ss
	echo '(define scheme-lib-dir "$(libdir)/scheme/r6rs")' >>ikarus.config.ss
	echo '(define (host-info) "$(target)")' >>ikarus.config.ss
	echo '(define wordsize 8)' >>ikarus.config.ss
	echo '(define platform-endianness (quote $(VICARE_PLATFORM_ENDIANNESS)))' >>ikarus.config.ss
	echo '(define arguments-validation (positive? $(VICARE_ARGUMENTS_VALIDATION)))' >>ikarus.config.ss
	echo '(define bootfile "$(pkglibdir)/vicare.boot")' >>ikarus.config.ss
	VICARE_SRC_DIR=$(srcdir)			\
  VICARE_BUILD_DIR=$(builddir)				\
  VICARE_FASL_DIRECTORY=''				\
  VICARE_LIBRARY_PATH=$(BOOT_IMAGE_SEARCH_PATH)		\
  $(NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT)		\
  --r6rs-script $(srcdir)/makefile.sps
	cp $(builddir)/vicare.boot $(builddir)/vicare.boot.8.prebuilt

#page
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
try-vicare-boot: $(EXTRA_DIST) ikarus.config.ss
	VICARE_SRC_DIR=$(srcdir)			\
   VICARE_BUILD_DIR=$(builddir)				\
   VICARE_FASL_DIRECTORY=$(VICARE_FASL_DIRECTORY)	\
   VICARE_LIBRARY_PATH=$(BOOT_IMAGE_SEARCH_PATH)	\
   $(NEW_EXECUTABLE) -b vicare.boot.try			\
   $(user_flags) --r6rs-script $(srcdir)/makefile.sps

#page
#### stuff needed by GNU Automake

EXTRA_DIST= \
	vicare.boot.4.prebuilt				\
	vicare.boot.8.prebuilt				\
	\
	ikarus.compiler.sls				\
	ikarus.compiler.altcogen.ss			\
	ikarus.compiler.ontology.ss			\
	ikarus.compiler.optimize-letrec.ss		\
	ikarus.compiler.source-optimizer.ss		\
	ikarus.compiler.tag-annotation-analysis.ss	\
	pass-specify-rep-primops.ss			\
	pass-specify-rep.ss				\
	\
	ikarus.apply.sls				\
	ikarus.apropos.sls				\
	ikarus.bignums.sls				\
	ikarus.bytevectors.sls				\
	ikarus.cafe.sls					\
	ikarus.chars.sls				\
	ikarus.code-objects.sls				\
	ikarus.codecs.sls				\
	ikarus.collect.sls				\
	ikarus.compensations.sls			\
	ikarus.command-line.sls				\
	ikarus.conditions.sls				\
	ikarus.control.sls				\
	ikarus.debugger.sls				\
	ikarus.emergency.sls				\
	ikarus.enumerations.sls				\
	ikarus.equal.sls				\
	ikarus.exceptions.sls				\
	ikarus.fasl.read.sls				\
	ikarus.fasl.write.sls				\
	ikarus.fixnums.sls				\
	ikarus.guardians.sls				\
	ikarus.handlers.sls				\
	ikarus.hash-tables.sls				\
	ikarus.keywords.sls				\
	ikarus.intel-assembler.sls			\
	ikarus.io.sls					\
	ikarus.lists.sls				\
	ikarus.load.sls					\
	ikarus.main.sls					\
	ikarus.multiple-values.sls			\
	ikarus.numerics.flonums.sls			\
	ikarus.numerics.generic-arithmetic.sls		\
	ikarus.numerics.flonum-conversion.sls		\
	ikarus.numerics.rationalize.sls			\
	ikarus.numerics.div-and-mod.sls			\
	ikarus.numerics.flonums.div-and-mod.sls		\
	ikarus.numerics.bitwise.misc.sls		\
	ikarus.numerics.complex-numbers.sls		\
	ikarus.options.sls				\
	ikarus.pairs.sls				\
	ikarus.pointers.sls				\
	ikarus.posix.sls				\
	ikarus.predicates.sls				\
	ikarus.pretty-formats.sls			\
	ikarus.pretty-print.sls				\
	ikarus.promises.sls				\
	ikarus.foreign-libraries.sls			\
	ikarus.ratnums.sls				\
	ikarus.reader.sls				\
	ikarus.readline.sls				\
	ikarus.records.procedural.sls			\
	ikarus.singular-objects.sls			\
	ikarus.sort.sls					\
	ikarus.string-to-number.sls 			\
	ikarus.strings.sls				\
	ikarus.structs.sls				\
	ikarus.symbol-table.sls				\
	ikarus.symbols.sls				\
	ikarus.time-and-date.sls			\
	ikarus.timer.sls				\
	ikarus.unicode-conversion.sls			\
	ikarus.unicode.sls				\
	ikarus.vectors.sls				\
	ikarus.writer.sls				\
	\
	makefile.sps					\
	\
	psyntax.builders.sls				\
	psyntax.compat.sls				\
	psyntax.config.sls				\
	psyntax.expander.sls				\
	psyntax.internal.sls				\
	psyntax.library-manager.sls			\
	\
	unicode-data.sls				\
	unicode/unicode-char-cases.ss			\
	unicode/unicode-charinfo.ss			\
	unicode/unicode-data.ss				\
	unicode/UNIDATA/CaseFolding.txt			\
	unicode/UNIDATA/CompositionExclusions.txt	\
	unicode/UNIDATA/NormalizationTest.txt		\
	unicode/UNIDATA/PropList.txt			\
	unicode/UNIDATA/SpecialCasing.txt		\
	unicode/UNIDATA/UnicodeData.txt			\
	unicode/UNIDATA/WordBreakProperty.txt

### end of file
