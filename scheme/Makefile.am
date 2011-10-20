## Process this file with automake to produce Makefile.in

nodist_pkglib_DATA=vicare.boot

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

#page
NEW_BOOT_FILE		= vicare.boot
NEW_EXECUTABLE		= ../src/vicare

IKARUS_PREBUILT_BOOT_FILE	= $(srcdir)/ikarus.boot.$(sizeofvoidp).prebuilt
VICARE_PREBUILT_BOOT_FILE	= $(srcdir)/vicare.boot.$(sizeofvoidp).prebuilt

all: $(nodist_pkglib_DATA)

$(srcdir)/last-revision:
	git show | head -1 | cut -d ' ' -f2 >$@

sizeofvoidp		= $(shell grep SIZEOF_VOID_P ../config.h | sed "s/.*\(.\)/\1/g")
PLATFORM_ENDIANNESS	= @PLATFORM_ENDIANNESS@

ikarus.config.ss: Makefile last-revision ../config.h
	echo '(define ikarus-version "$(PACKAGE_VERSION)")' >$@
	echo '(define ikarus-revision "$(shell cat $(srcdir)/last-revision)")' >>$@
	echo '(define ikarus-lib-dir "$(pkglibdir)")' >>$@
	echo '(define target "$(target)")' >>$@
	echo '(define wordsize $(sizeofvoidp))' >>$@
	echo '(define platform-endianness (quote $(PLATFORM_ENDIANNESS)))' >>$@


CLEANFILES=$(nodist_pkglib_DATA) ikarus.config.ss
MAINTAINERCLEANFILES=last-revision

# To  create the  new  boot image  we  use a  prebuilt  boot image;  the
# environment variable  names are hardcoded  in the prebuilt  boot image
# and are the Ikarus' ones.
$(NEW_BOOT_FILE): $(EXTRA_DIST) ikarus.config.ss
	IKARUS_SRC_DIR=$(srcdir) \
  IKARUS_BUILD_DIR=$(builddir) \
  VICARE_FASL_DIRECTORY='' \
  VICARE_LIBRARY_PATH=.:$(srcdir):$(srcdir)/../lib \
  $(NEW_EXECUTABLE) -b $(VICARE_PREBUILT_BOOT_FILE) \
  --r6rs-script $(srcdir)/makefile.sps
  # 	IKARUS_SRC_DIR=$(srcdir) \
  # IKARUS_BUILD_DIR=$(builddir) \
  # IKARUS_FASL_DIRECTORY='' \
  # IKARUS_LIBRARY_PATH=.:$(srcdir):$(srcdir)/../lib \
  # $(NEW_EXECUTABLE) -b $(IKARUS_PREBUILT_BOOT_FILE) \
  # --r6rs-script $(srcdir)/makefile.sps

try-vicare-boot: $(EXTRA_DIST) ikarus.config.ss
	IKARUS_SRC_DIR=$(srcdir) \
   IKARUS_BUILD_DIR=$(builddir) \
   VICARE_FASL_DIRECTORY='' \
   VICARE_LIBRARY_PATH=.:$(srcdir):$(srcdir)/../lib \
   $(NEW_EXECUTABLE) -b vicare.boot.try \
   --r6rs-script $(srcdir)/makefile.sps

### end of file
