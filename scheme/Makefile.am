## Process this file with automake to produce Makefile.in

nodist_pkglib_DATA	= vicare.boot
NEW_BOOT_FILE		= vicare.boot
NEW_EXECUTABLE		= ../src/vicare

BOOT_IMAGE_SEARCH_PATH	= .:$(srcdir):$(srcdir)/../lib:$(builddir)/../lib

#VICARE_SIZE_OF_VOIDP		= $(shell grep SIZEOF_VOID_P ../config.h | sed "s/.*\(.\)/\1/g")
VICARE_SIZE_OF_VOIDP		= @VICARE_SIZE_OF_VOIDP@
VICARE_PLATFORM_ENDIANNESS	= @VICARE_PLATFORM_ENDIANNESS@
VICARE_ARGUMENTS_VALIDATION	= @VICARE_ARGUMENTS_VALIDATION@

IKARUS_PREBUILT_BOOT	= $(srcdir)/ikarus.boot.$(VICARE_SIZE_OF_VOIDP).prebuilt
VICARE_PREBUILT_BOOT	= $(srcdir)/vicare.boot.$(VICARE_SIZE_OF_VOIDP).prebuilt

CLEANFILES		= $(nodist_pkglib_DATA) ikarus.config.ss
MAINTAINERCLEANFILES	= last-revision

all: $(nodist_pkglib_DATA)

## --------------------------------------------------------------------

ikarus.config.ss: Makefile last-revision ../config.h
	echo '(define vicare-version "$(PACKAGE_VERSION)")' >$@
	echo '(define vicare-revision "$(shell cat $(srcdir)/last-revision)")' >>$@
	echo '(define vicare-lib-dir "$(pkglibdir)")' >>$@
	echo '(define target "$(target)")' >>$@
	echo '(define wordsize $(VICARE_SIZE_OF_VOIDP))' >>$@
	echo '(define platform-endianness (quote $(VICARE_PLATFORM_ENDIANNESS)))' >>$@
	echo '(define arguments-validation (positive? $(VICARE_ARGUMENTS_VALIDATION)))' >>$@

#page
# To  create the  new boot  image we  use a  prebuilt boot  image; the
# environment variable names are  hardcoded in the prebuilt boot image
# and are the Ikarus' ones.
#
$(NEW_BOOT_FILE): $(EXTRA_DIST) ikarus.config.ss
	IKARUS_SRC_DIR=$(srcdir)			\
  IKARUS_BUILD_DIR=$(builddir)				\
  VICARE_FASL_DIRECTORY=''				\
  VICARE_LIBRARY_PATH=$(BOOT_IMAGE_SEARCH_PATH)		\
  $(NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT)		\
  --r6rs-script $(srcdir)/makefile.sps

#$(NEW_BOOT_FILE): $(EXTRA_DIST) ikarus.config.ss
# 	IKARUS_SRC_DIR=$(srcdir)			\
#   IKARUS_BUILD_DIR=$(builddir)			\
#   IKARUS_FASL_DIRECTORY=''				\
#   IKARUS_LIBRARY_PATH=$(BOOT_IMAGE_SEARCH_PATH)	\
#   $(NEW_EXECUTABLE) -b $(IKARUS_PREBUILT_BOOT)	\
#   --r6rs-script $(srcdir)/makefile.sps

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
	IKARUS_SRC_DIR=$(srcdir)			\
   IKARUS_BUILD_DIR=$(builddir)				\
   VICARE_FASL_DIRECTORY=''				\
   VICARE_LIBRARY_PATH=$(BOOT_IMAGE_SEARCH_PATH)	\
   $(NEW_EXECUTABLE) -b vicare.boot.try			\
   --r6rs-script $(srcdir)/makefile.sps

#page
#### stuff needed by GNU Automake

EXTRA_DIST= last-revision						\
  ikarus.boot.4.prebuilt ikarus.boot.8.prebuilt				\
  vicare.boot.4.prebuilt						\
  ikarus.enumerations.ss						\
  ikarus.exceptions.ss ikarus.apply.ss ikarus.bytevectors.ss		\
  ikarus.cafe.ss ikarus.chars.ss ikarus.code-objects.ss			\
  ikarus.codecs.ss ikarus.collect.ss ikarus.command-line.ss		\
  ikarus.compiler.altcogen.ss ikarus.compiler.ss			\
  ikarus.compiler.source-optimizer.ss					\
  ikarus.compiler.optimize-letrec.ss					\
  ikarus.control.ss							\
  ikarus.date-string.ss ikarus.fasl.ss ikarus.fasl.write.ss		\
  ikarus.fixnums.ss ikarus.guardians.ss ikarus.handlers.ss		\
  ikarus.hash-tables.ss ikarus.intel-assembler.ss			\
  ikarus.lists.ss ikarus.load.ss ikarus.main.ss				\
  ikarus.multiple-values.ss ikarus.numerics.ss				\
  ikarus.pairs.ss ikarus.posix.ss ikarus.predicates.ss			\
  ikarus.pretty-print.ss ikarus.pretty-formats.ss			\
  ikarus.promises.ss ikarus.reader.ss					\
  ikarus.records.procedural.ss ikarus.conditions.ss			\
  ikarus.singular-objects.ss ikarus.sort.ss ikarus.strings.ss		\
  ikarus.structs.ss ikarus.symbols.ss ikarus.timer.ss			\
  ikarus.unicode-conversion.ss ikarus.unicode.ss			\
  ikarus.vectors.ss ikarus.writer.ss makefile.sps			\
  pass-specify-rep-primops.ss pass-specify-rep.ss psyntax.builders.ss	\
  psyntax.compat.ss psyntax.config.ss psyntax.expander.ss		\
  psyntax.internal.ss psyntax.library-manager.ss			\
  unicode/unicode-char-cases.ss unicode/unicode-charinfo.ss		\
  unicode-data.ss unicode/unicode-data.ss				\
  unicode/UNIDATA/CaseFolding.txt					\
  unicode/UNIDATA/NormalizationTest.txt					\
  unicode/UNIDATA/SpecialCasing.txt					\
  unicode/UNIDATA/WordBreakProperty.txt					\
  unicode/UNIDATA/CompositionExclusions.txt				\
  unicode/UNIDATA/PropList.txt						\
  unicode/UNIDATA/UnicodeData.txt					\
  ikarus.io.ss ikarus.time-and-date.ss ikarus.not-yet-implemented.ss	\
  ikarus.string-to-number.ss ikarus.compiler.source-optimizer.ss	\
  ikarus.compiler.tag-annotation-analysis.ss ikarus.ontology.ss		\
  ikarus.pointers.ss ikarus.equal.ss					\
  ikarus.symbol-table.ss ikarus.apropos.ss				\
  ikarus.debugger.ss

### end of file
