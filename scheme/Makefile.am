## Process this file with automake to produce Makefile.in

nodist_pkglib_DATA=vicare.boot

EXTRA_DIST=ikarus.boot.4.prebuilt ikarus.boot.8.prebuilt last-revision \
  ikarus.enumerations.ss run-tests.ss \
  ikarus.exceptions.ss ikarus.apply.ss ikarus.bytevectors.ss \
  ikarus.cafe.ss ikarus.chars.ss ikarus.code-objects.ss \
  ikarus.codecs.ss ikarus.collect.ss ikarus.command-line.ss \
  ikarus.compiler.altcogen.ss ikarus.compiler.ss \
  ikarus.compiler.source-optimizer.ss \
  ikarus.compiler.optimize-letrec.ss \
  ikarus.control.ss \
  ikarus.date-string.ss ikarus.fasl.ss ikarus.fasl.write.ss \
  ikarus.fixnums.ss ikarus.guardians.ss ikarus.handlers.ss \
  ikarus.hash-tables.ss ikarus.intel-assembler.ss \
  ikarus.lists.ss ikarus.load.ss ikarus.main.ss \
  ikarus.multiple-values.ss ikarus.numerics.ss \
  ikarus.pairs.ss ikarus.posix.ss ikarus.predicates.ss \
  ikarus.pretty-print.ss ikarus.pretty-formats.ss \
  ikarus.promises.ss ikarus.reader.ss \
  ikarus.records.procedural.ss ikarus.conditions.ss \
  ikarus.singular-objects.ss ikarus.sort.ss ikarus.strings.ss \
  ikarus.structs.ss ikarus.symbols.ss ikarus.timer.ss \
  ikarus.unicode-conversion.ss ikarus.unicode.ss \
  ikarus.vectors.ss ikarus.writer.ss makefile.sps \
  pass-specify-rep-primops.ss pass-specify-rep.ss psyntax.builders.ss \
  psyntax.compat.ss psyntax.config.ss psyntax.expander.ss \
  psyntax.internal.ss psyntax.library-manager.ss \
  unicode/unicode-char-cases.ss unicode/unicode-charinfo.ss \
  unicode-data.ss unicode/unicode-data.ss \
  unicode/UNIDATA/CaseFolding.txt \
  unicode/UNIDATA/NormalizationTest.txt \
  unicode/UNIDATA/SpecialCasing.txt \
  unicode/UNIDATA/WordBreakProperty.txt \
  unicode/UNIDATA/CompositionExclusions.txt \
  unicode/UNIDATA/PropList.txt \
  unicode/UNIDATA/UnicodeData.txt \
  ikarus.io.ss ikarus.time-and-date.ss ikarus.not-yet-implemented.ss \
  ikarus.string-to-number.ss ikarus.compiler.source-optimizer.ss \
  ikarus.compiler.tag-annotation-analysis.ss ikarus.ontology.ss \
  ikarus.reader.annotated.ss ikarus.pointers.ss ikarus.equal.ss \
  ikarus.symbol-table.ss ikarus.apropos.ss \
  ikarus.debugger.ss \
  tests/SRFI-1.ss \
  tests/bignum-to-flonum.ss \
  tests/bignums.ss \
  tests/bitwise.ss \
  tests/case-folding.ss \
  tests/div-and-mod.ss \
  tests/enums.ss \
  tests/fasl.ss \
  tests/fixnums.ss \
  tests/fldiv-and-mod.ss \
  tests/framework.ss \
  tests/guardians.ss \
  tests/hashtables.ss \
  tests/lists.ss \
  tests/normalization.ss \
  tests/numerics.ss \
  tests/parse-flonums.ss \
  tests/pointers.ss \
  tests/r6rs-records-procedural.ss \
  tests/reader.ss \
  tests/repl.ss \
  tests/rn100 \
  tests/set-position.ss \
  tests/sorting.ss \
  tests/string-to-number.ss \
  tests/strings.ss \
  tests/scribble.ss \
  tests/symbol-table.ss \
  tests/tests-1.1-req.scm \
  tests/tests-1.2-req.scm \
  tests/tests-1.3-req.scm \
  tests/tests-1.4-req.scm \
  tests/tests-1.5-req.scm \
  tests/tests-1.6-req.scm \
  tests/tests-1.7-req.scm \
  tests/tests-1.8-req.scm \
  tests/tests-1.9-req.scm \
  tests/tests-2.1-req.scm \
  tests/tests-2.2-req.scm \
  tests/tests-2.3-req.scm \
  tests/tests-2.4-req.scm \
  tests/tests-2.6-req.scm \
  tests/tests-2.8-req.scm \
  tests/tests-2.9-req.scm \
  tests/tests-3.1-req.scm \
  tests/tests-3.2-req.scm \
  tests/tests-3.3-req.scm \
  tests/tests-3.4-req.scm \
  tests/tests-4.1-req.scm \
  tests/tests-4.2-req.scm \
  tests/tests-4.3-req.scm \
  tests/tests-5.1-req.scm \
  tests/tests-5.2-req.scm \
  tests/tests-5.3-req.scm \
  tests/tests-5.6-req.scm \
  tests/unicode.ss

NEW_BOOT_FILE	= vicare.boot
NEW_EXECUTABLE	= ../src/vicare

all: $(nodist_pkglib_DATA)

$(srcdir)/last-revision:
	git show | head -1 | cut -d ' ' -f2 >$@

sizeofvoidp = $(shell grep SIZEOF_VOID_P ../config.h | sed "s/.*\(.\)/\1/g")

ikarus.config.ss: Makefile last-revision ../config.h
	echo '(define ikarus-version "$(PACKAGE_VERSION)")' >$@
	echo '(define ikarus-revision "$(shell cat $(srcdir)/last-revision)")' >>$@
	echo '(define ikarus-lib-dir "$(pkglibdir)")' >>$@
	echo '(define target "$(target)")' >>$@
	echo '(define wordsize $(sizeofvoidp))' >>$@


CLEANFILES=$(nodist_pkglib_DATA) ikarus.config.ss
MAINTAINERCLEANFILES=last-revision

# To  create the  new  boot image  we  use a  prebuilt  boot image;  the
# environment variable  names are hardcoded  in the prebuilt  boot image
# and are the Ikarus' ones.
$(NEW_BOOT_FILE): $(EXTRA_DIST) ikarus.config.ss
	IKARUS_SRC_DIR=$(srcdir) \
  IKARUS_BUILD_DIR=$(builddir) \
  IKARUS_FASL_DIRECTORY='' \
  IKARUS_LIBRARY_PATH=.:$(srcdir):$(srcdir)/../lib \
  $(NEW_EXECUTABLE) -b $(srcdir)/ikarus.boot.$(sizeofvoidp).prebuilt \
  --r6rs-script $(srcdir)/makefile.sps

ikarus-tests: $(NEW_BOOT_FILE)
	IKARUS_SRC_DIR=$(srcdir) \
		VICARE_LIBRARY_PATH=$(srcdir) \
		$(NEW_EXECUTABLE) -b $(NEW_BOOT_FILE) --r6rs-script $(srcdir)/run-tests.ss

### end of file
