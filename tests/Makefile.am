## Process this file with automake to produce Makefile.in

EXTRA_DIST=	checks.sls		\
		test-equal-hash.sps	\
		test-log.sps

testdir			= $(srcdir)
VICARE_SELECTED_TESTS	= $(wildcard $(testdir)/test-*$(name)*.sps)

VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_FLAGS	= -b $(VICARE_BOOT) --debug --r6rs-script
VICARE_ENV	= VICARE_LIBRARY_PATH=$(srcdir):$(srcdir)/../lib
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)
VICARE_R6RS_ENV	= VICARE_LIBRARY_PATH=$(srcdir)/../tests:$(srcdir)/..:$(srcdir)/../lib
VICARE_R6RS_RUN	= $(VICARE_R6RS_ENV) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests test-r6rs

test tests:
	@echo
	@$(foreach f,$(VICARE_SELECTED_TESTS),$(VICARE_RUN) $(f);)
	@echo

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
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/control.sps

test-r6rs-base:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/base.sps

test-r6rs-io-ports:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/io/ports.sps

test-r6rs-io-simple:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/io/simple.sps

test-r6rs-eval:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/eval.sps

test-r6rs-syntax-case:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/syntax-case.sps

test-r6rs-mutable-strings:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/mutable-strings.sps

test-r6rs-records-procedural:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/records/procedural.sps

test-r6rs-records-syntactic:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/records/syntactic.sps

test-r6rs-conditions:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/conditions.sps

test-r6rs-hashtables:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/hashtables.sps

test-r6rs-programs:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/programs.sps

test-r6rs-contrib:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/contrib.sps

test-r6rs-unicode:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/unicode.sps

test-r6rs-r5rs:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/r5rs.sps

test-r6rs-reader:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/reader.sps

test-r6rs-mutable-pairs:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/mutable-pairs.sps

test-r6rs-lists:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/lists.sps

test-r6rs-arithmetic-fixnums:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/arithmetic/fixnums.sps

test-r6rs-arithmetic-bitwise:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/arithmetic/bitwise.sps

test-r6rs-arithmetic-flonums:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/arithmetic/flonums.sps

test-r6rs-bytevectors:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/bytevectors.sps

test-r6rs-exceptions:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/exceptions.sps

test-r6rs-enums:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/enums.sps

test-r6rs-sorting:
	@$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run/sorting.sps

### end of file
