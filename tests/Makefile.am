## Process this file with automake to produce Makefile.in

testdir				= $(srcdir)
VICARE_SELECTED_TESTS		= $(wildcard $(testdir)/test-*$(file)*.sps)
VICARE_SELECTED_LONG_TESTS	= $(wildcard $(testdir)/long-test-*$(file)*.sps)

VICARE_TEST_SEARCH_PATH	= $(srcdir):$(srcdir)/../lib:$(builddir)/../lib:$(srcdir)/../scheme

# The variable "user_flags"  is for the user to be  set on the command
# line of "make".
VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_FLAGS	= -b $(VICARE_BOOT) --debug $(user_flags) --r6rs-script
VICARE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH)	\
		  CHECK_TEST_NAME=$(name)				\
		  VICARE_SRC_DIR=$(srcdir) VICARE_BUILDDIR=$(builddir)
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests test-r6rs

test tests:
	@echo
	@$(foreach f,$(VICARE_SELECTED_TESTS),$(VICARE_RUN) $(f);)
	@echo

long-test long-tests:
	@echo
	@$(foreach f,$(VICARE_SELECTED_LONG_TESTS),$(VICARE_RUN) $(f);)
	@echo

#page
#### running vicare from the build directory

# The variable "user_flags"  is for the user to be  set on the command
# line of "make".
VICARE_TEST_RUN_FLAGS	= -b $(VICARE_BOOT) $(user_flags)
VICARE_TEST_RUN_ENV	= VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH)	\
			  VICARE_SRC_DIR=$(srcdir) VICARE_BUILDDIR=$(builddir)
VICARE_TEST_RUN		= $(VICARE_TEST_RUN_ENV) $(VICARE) $(VICARE_TEST_RUN_FLAGS)


.PHONY: test-run test-exec-modes

test-run:
	$(VICARE_TEST_RUN)

test-exec-modes:
	$(VICARE_TEST_RUN_ENV) $(srcdir)/test-exec-modes.sh $(VICARE) $(VICARE_BOOT) $(srcdir)

#page
#### interface to "make check"

# TESTS and TESTS_ENVIRONMENT are Automake variables.
TESTS_ENVIRONMENT	= $(VICARE_RUN)
TESTS			=	\
	test-r6rs-arithmetic-bitwise.sps		\
	test-r6rs-arithmetic-fixnums.sps		\
	test-r6rs-arithmetic-flonums.sps		\
	test-r6rs-base.sps				\
	test-r6rs-bytevectors.sps			\
	test-r6rs-conditions.sps			\
	test-r6rs-contrib.sps				\
	test-r6rs-control.sps				\
	test-r6rs-enums.sps				\
	test-r6rs-eval.sps				\
	test-r6rs-exceptions.sps			\
	test-r6rs-hashtables.sps			\
	test-r6rs-io-ports.sps				\
	test-r6rs-io-simple.sps				\
	test-r6rs-lists.sps				\
	test-r6rs-mutable-pairs.sps			\
	test-r6rs-mutable-strings.sps			\
	test-r6rs-programs.sps				\
	test-r6rs-r5rs.sps				\
	test-r6rs-reader.sps				\
	test-r6rs-records-procedural.sps		\
	test-r6rs-records-syntactic.sps			\
	test-r6rs-sorting.sps				\
	test-r6rs-syntax-case.sps			\
	test-r6rs-unicode.sps				\
	\
	test-ikarus-bignum-to-flonum.sps		\
	test-ikarus-bytevectors.sps			\
	test-ikarus-case-folding.sps			\
	test-ikarus-div-and-mod.sps			\
	test-ikarus-enums.sps				\
	test-ikarus-fasl.sps				\
	test-ikarus-fldiv-and-mod.sps			\
	test-ikarus-guardians.sps			\
	test-ikarus-hashtables.sps			\
	test-ikarus-lists.sps				\
	test-ikarus-normalization.sps			\
	test-ikarus-numerics.sps			\
	test-ikarus-pointers.sps			\
	test-ikarus-r6rs-records-procedural.sps		\
	test-ikarus-reader.sps				\
	test-ikarus-repl.sps				\
	test-ikarus-set-position.sps			\
	test-ikarus-sorting.sps				\
	test-ikarus-strings.sps				\
	test-ikarus-symbol-table.sps			\
	test-ikarus-unicode.sps				\
	\
	test-vicare-io.sps				\
	test-vicare-log.sps				\
	test-vicare-posix.sps				\
	test-vicare-reader.sps				\
	test-vicare-symbols.sps				\
	\
	test-vicare-system.sps				\
	\
	test-issue-001-equal-hash.sps			\
	test-issue-006-fxreverse-bit-field.sps		\
	test-issue-007.sps				\
	test-issue-008.sps				\
	test-issue-009.sps				\
	test-issue-010.sps				\
	test-issue-012.sps				\
	test-issue-013.sps				\
	test-issue-015.sps				\
	test-issue-017.sps				\
	test-issue-020.sps				\
	test-issue-021.sps				\
	test-issue-022.sps				\
	test-issue-023.sps				\
	test-issue-024.sps				\
	test-issue-025-sign-in-exact-rationals.sps

#page
#### extra files to be distributed

EXTRA_DIST =	\
	checks.sls					\
	r6rs						\
	r6rs/README.txt					\
	r6rs/arithmetic					\
	r6rs/arithmetic/bitwise.sls			\
	r6rs/arithmetic/fixnums.sls			\
	r6rs/arithmetic/flonums.sls			\
	r6rs/base.sls					\
	r6rs/bytevectors.sls				\
	r6rs/conditions.sls				\
	r6rs/contrib					\
	r6rs/contrib.sls				\
	r6rs/contrib/helper1.sls			\
	r6rs/control.sls				\
	r6rs/enums.sls					\
	r6rs/eval.sls					\
	r6rs/exceptions.sls				\
	r6rs/hashtables.sls				\
	r6rs/io						\
	r6rs/io/ports.sls				\
	r6rs/io/simple.sls				\
	r6rs/lists.sls					\
	r6rs/mutable-pairs.sls				\
	r6rs/mutable-strings.sls			\
	r6rs/programs.sls				\
	r6rs/r5rs.sls					\
	r6rs/reader.sls					\
	r6rs/records					\
	r6rs/records/procedural.sls			\
	r6rs/records/syntactic.sls			\
	r6rs/sorting.sls				\
	r6rs/syntax-case.sls				\
	r6rs/test.sls					\
	r6rs/unicode.sls				\
	\
	exec-modes-helpers/r6rs-forms			\
	exec-modes-helpers/r6rs-program.sps		\
	test-exec-modes.sh				\
	\
	long-test-r6rs-run.sps				\
	long-test-r6rs-run-via-eval.sps			\
	long-test-ikarus-bignums.sps			\
	long-test-ikarus-bitwise.sps			\
	long-test-ikarus-fixnums.sps			\
	long-test-ikarus-io.sps				\
	long-test-ikarus-parse-flonums.sps		\
	long-test-ikarus-string-to-number.sps		\
	\
	test-r6rs-arithmetic-bitwise.sps		\
	test-r6rs-arithmetic-fixnums.sps		\
	test-r6rs-arithmetic-flonums.sps		\
	test-r6rs-base.sps				\
	test-r6rs-bytevectors.sps			\
	test-r6rs-conditions.sps			\
	test-r6rs-contrib.sps				\
	test-r6rs-control.sps				\
	test-r6rs-enums.sps				\
	test-r6rs-eval.sps				\
	test-r6rs-exceptions.sps			\
	test-r6rs-hashtables.sps			\
	test-r6rs-io-ports.sps				\
	test-r6rs-io-simple.sps				\
	test-r6rs-lists.sps				\
	test-r6rs-mutable-pairs.sps			\
	test-r6rs-mutable-strings.sps			\
	test-r6rs-programs.sps				\
	test-r6rs-r5rs.sps				\
	test-r6rs-reader.sps				\
	test-r6rs-records-procedural.sps		\
	test-r6rs-records-syntactic.sps			\
	test-r6rs-sorting.sps				\
	test-r6rs-syntax-case.sps			\
	test-r6rs-unicode.sps				\
	\
	test-ikarus-bignum-to-flonum.sps		\
	test-ikarus-bytevectors.sps			\
	test-ikarus-case-folding.sps			\
	test-ikarus-div-and-mod.sps			\
	test-ikarus-enums.sps				\
	test-ikarus-fasl.sps				\
	test-ikarus-fldiv-and-mod.sps			\
	test-ikarus-guardians.sps			\
	test-ikarus-hashtables.sps			\
	test-ikarus-lists.sps				\
	test-ikarus-normalization.sps			\
	test-ikarus-numerics.sps			\
	test-ikarus-pointers.sps			\
	test-ikarus-r6rs-records-procedural.sps		\
	test-ikarus-reader.sps				\
	test-ikarus-repl.sps				\
	test-ikarus-set-position.sps			\
	test-ikarus-sorting.sps				\
	test-ikarus-strings.sps				\
	test-ikarus-symbol-table.sps			\
	test-ikarus-unicode.sps				\
	\
	test-vicare-io.sps				\
	test-vicare-log.sps				\
	test-vicare-posix.sps				\
	test-vicare-reader.sps				\
	test-vicare-symbols.sps				\
	\
	test-vicare-system.sps				\
	\
	test-issue-001-equal-hash.sps			\
	test-issue-006-fxreverse-bit-field.sps		\
	test-issue-007.sps				\
	test-issue-008.sps				\
	test-issue-009.sps				\
	test-issue-010.sps				\
	test-issue-012.sps				\
	test-issue-013.sps				\
	test-issue-015.sps				\
	test-issue-017.sps				\
	test-issue-020.sps				\
	test-issue-021.sps				\
	test-issue-022.sps				\
	test-issue-023.sps				\
	test-issue-024.sps				\
	test-issue-025-sign-in-exact-rationals.sps

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
