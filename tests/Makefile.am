## Process this file with automake to produce Makefile.in

testdir			= $(srcdir)
VICARE_SELECTED_TESTS	= $(wildcard $(testdir)/test-*$(file)*.sps)

VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_FLAGS	= -b $(VICARE_BOOT) --debug --r6rs-script
VICARE_ENV	= VICARE_LIBRARY_PATH=$(srcdir):$(srcdir)/../lib CHECK_TEST_NAME=$(name)
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests test-r6rs

test tests:
	@echo
	$(foreach f,$(VICARE_SELECTED_TESTS),$(VICARE_RUN) $(f);)
	@echo

#page
## --------------------------------------------------------------------
## Raw interface for Flatt's R6RS test suite.
## --------------------------------------------------------------------

vicare_r6rs_subtest_RULES	=	\
	test-r6rs-control		\
	test-r6rs-base			\
	test-r6rs-io-ports		\
	test-r6rs-io-simple		\
	test-r6rs-eval			\
	test-r6rs-syntax-case		\
	test-r6rs-mutable-strings	\
	test-r6rs-records-procedural	\
	test-r6rs-records-syntactic	\
	test-r6rs-conditions		\
	test-r6rs-hashtables		\
	test-r6rs-programs		\
	test-r6rs-contrib		\
	test-r6rs-unicode		\
	test-r6rs-test			\
	test-r6rs-run			\
	test-r6rs-r5rs			\
	test-r6rs-reader		\
	test-r6rs-mutable-pairs		\
	test-r6rs-lists			\
	test-r6rs-arithmetic-fixnums	\
	test-r6rs-arithmetic-bitwise	\
	test-r6rs-arithmetic-flonums	\
	test-r6rs-bytevectors		\
	test-r6rs-exceptions		\
	test-r6rs-enums			\
	test-r6rs-sorting

.PHONY: test-r6rs $(vicare_r6rs_subtest_RULES)

test-r6rs: $(vicare_r6rs_subtest_RULES)

test-r6rs-control:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/control.sps

test-r6rs-base:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/base.sps

test-r6rs-io-ports:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/io/ports.sps

test-r6rs-io-simple:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/io/simple.sps

test-r6rs-eval:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/eval.sps

test-r6rs-syntax-case:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/syntax-case.sps

test-r6rs-mutable-strings:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/mutable-strings.sps

test-r6rs-records-procedural:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/records/procedural.sps

test-r6rs-records-syntactic:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/records/syntactic.sps

test-r6rs-conditions:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/conditions.sps

test-r6rs-hashtables:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/hashtables.sps

test-r6rs-programs:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/programs.sps

test-r6rs-contrib:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/contrib.sps

test-r6rs-unicode:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/unicode.sps

test-r6rs-r5rs:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/r5rs.sps

test-r6rs-reader:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/reader.sps

test-r6rs-mutable-pairs:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/mutable-pairs.sps

test-r6rs-lists:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/lists.sps

test-r6rs-arithmetic-fixnums:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/arithmetic/fixnums.sps

test-r6rs-arithmetic-bitwise:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/arithmetic/bitwise.sps

test-r6rs-arithmetic-flonums:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/arithmetic/flonums.sps

test-r6rs-bytevectors:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/bytevectors.sps

test-r6rs-exceptions:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/exceptions.sps

test-r6rs-enums:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/enums.sps

test-r6rs-sorting:
	@$(VICARE_RUN) $(srcdir)/r6rs/run/sorting.sps

#page
## --------------------------------------------------------------------
## Interface to "make check".
## --------------------------------------------------------------------

TESTS_ENVIRONMENT	= $(VICARE_RUN)
TESTS			=	\
	$(srcdir)/r6rs/run/control.sps			\
	$(srcdir)/r6rs/run/base.sps			\
	$(srcdir)/r6rs/run/io/ports.sps			\
	$(srcdir)/r6rs/run/io/simple.sps		\
	$(srcdir)/r6rs/run/eval.sps			\
	$(srcdir)/r6rs/run/syntax-case.sps		\
	$(srcdir)/r6rs/run/mutable-strings.sps		\
	$(srcdir)/r6rs/run/records/procedural.sps	\
	$(srcdir)/r6rs/run/records/syntactic.sps	\
	$(srcdir)/r6rs/run/conditions.sps		\
	$(srcdir)/r6rs/run/hashtables.sps		\
	$(srcdir)/r6rs/run/programs.sps			\
	$(srcdir)/r6rs/run/contrib.sps			\
	$(srcdir)/r6rs/run/unicode.sps			\
	$(srcdir)/r6rs/run/r5rs.sps			\
	$(srcdir)/r6rs/run/reader.sps			\
	$(srcdir)/r6rs/run/mutable-pairs.sps		\
	$(srcdir)/r6rs/run/lists.sps			\
	$(srcdir)/r6rs/run/arithmetic/fixnums.sps	\
	$(srcdir)/r6rs/run/arithmetic/bitwise.sps	\
	$(srcdir)/r6rs/run/arithmetic/flonums.sps	\
	$(srcdir)/r6rs/run/bytevectors.sps		\
	$(srcdir)/r6rs/run/exceptions.sps		\
	$(srcdir)/r6rs/run/enums.sps			\
	$(srcdir)/r6rs/run/sorting.sps
	\
	test-getpid.sps					\
	test-log.sps					\
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
	test-issue-018.sps				\
	test-issue-020.sps				\
	test-issue-021.sps				\
	test-issue-022.sps				\
	test-issue-023.sps				\
	test-issue-024.sps				\
	test-issue-025.sps

#page
## --------------------------------------------------------------------
## Extra files to be distributed.
## --------------------------------------------------------------------

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
	r6rs/run					\
	r6rs/run-via-eval.sps				\
	r6rs/run.sps					\
	r6rs/run/arithmetic				\
	r6rs/run/arithmetic/bitwise.sps			\
	r6rs/run/arithmetic/fixnums.sps			\
	r6rs/run/arithmetic/flonums.sps			\
	r6rs/run/base.sps				\
	r6rs/run/bytevectors.sps			\
	r6rs/run/conditions.sps				\
	r6rs/run/contrib.sps				\
	r6rs/run/control.sps				\
	r6rs/run/enums.sps				\
	r6rs/run/eval.sps				\
	r6rs/run/exceptions.sps				\
	r6rs/run/hashtables.sps				\
	r6rs/run/io					\
	r6rs/run/io/ports.sps				\
	r6rs/run/io/simple.sps				\
	r6rs/run/lists.sps				\
	r6rs/run/mutable-pairs.sps			\
	r6rs/run/mutable-strings.sps			\
	r6rs/run/programs.sps				\
	r6rs/run/r5rs.sps				\
	r6rs/run/reader.sps				\
	r6rs/run/records				\
	r6rs/run/records/procedural.sps			\
	r6rs/run/records/syntactic.sps			\
	r6rs/run/sorting.sps				\
	r6rs/run/syntax-case.sps			\
	r6rs/run/unicode.sps				\
	r6rs/sorting.sls				\
	r6rs/syntax-case.sls				\
	r6rs/test.sls					\
	r6rs/unicode.sls				\
	\
	test-getpid.sps					\
	test-log.sps					\
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
	test-issue-018.sps				\
	test-issue-020.sps				\
	test-issue-021.sps				\
	test-issue-022.sps				\
	test-issue-023.sps				\
	test-issue-024.sps				\
	test-issue-025.sps

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
