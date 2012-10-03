## Process this file with automake to produce Makefile.in

testdir				= $(srcdir)
VICARE_SELECTED_TESTS		= $(testdir)/test-*$(file)*.sps
VICARE_SELECTED_LONG_TESTS	= $(testdir)/long-test-*$(file)*.sps
VICARE_SELECTED_FOREIGN_TESTS	= $(testdir)/foreign-test-*$(file)*.sps
VICARE_SELECTED_DEMOS		= $(testdir)/demo-*$(file)*.sps

VICARE_FASL_DIRECTORY	= $(builddir)/../lib/fasl
VICARE_TEST_SEARCH_PATH	= $(srcdir):$(srcdir)/../lib:$(builddir)/../lib:$(srcdir)/../scheme
VICARE_TEST_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)

# The variable "user_flags"  is for the user to be  set on the command
# line of "make".
VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_FLAGS	= -b $(VICARE_BOOT) $(user_flags) --r6rs-script
VICARE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH)	\
		  VICARE_FASL_DIRECTORY=$(VICARE_FASL_DIRECTORY)	\
		  CHECK_TEST_NAME=$(name)				\
		  VICARE_SRC_DIR=$(srcdir) VICARE_BUILDDIR=$(builddir)	\
		  LD_LIBRARY_PATH=$(builddir)/../src/.libs
VICARE_RUN	= $(VICARE_ENV) $(GDB) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests test-r6rs long-test long-tests
.PHONY: demo demos foreign-test foreign-tests

test tests:
	@echo
	@for f in $(VICARE_SELECTED_TESTS); do $(VICARE_RUN) $$f; done
	@echo

long-test long-tests:
	@echo
	@for f in $(VICARE_SELECTED_LONG_TESTS); do $(VICARE_RUN) $$f; done
	@echo

foreign-test foreign-tests:
	@echo
	@for f in $(VICARE_SELECTED_FOREIGN_TESTS); do $(VICARE_RUN) $$f; done
	@echo

demo demos:
	@echo
	@for f in $(VICARE_SELECTED_DEMOS); do $(VICARE_RUN) $$f; done
	@echo

#page
#### running vicare from the build directory

VIE			= $(srcdir)/../scripts/vie

# The variable "user_flags"  is for the user to be  set on the command
# line of "make".
VICARE_TEST_RUN_FLAGS	= -b $(VICARE_BOOT) $(user_flags)
VICARE_TEST_RUN_ENV	= VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH)	\
			  VICARE_SRC_DIR=$(srcdir) VICARE_BUILDDIR=$(builddir)
VICARE_TEST_RUN		= $(VICARE_TEST_RUN_ENV) $(VICARE) $(VICARE_TEST_RUN_FLAGS)
VICARE_TEST_VIE		= $(VICARE_TEST_RUN_ENV) $(VIE) $(VICARE) --raw-repl $(VICARE_TEST_RUN_FLAGS)


.PHONY: test-run test-vie test-exec-modes

test-run:
	$(VICARE_TEST_RUN)

test-vie:
	$(VICARE_TEST_VIE)

test-exec-modes:
	$(VICARE_TEST_RUN_ENV) $(srcdir)/test-exec-modes.sh $(VICARE) $(VICARE_BOOT) $(srcdir)

#page
#### interface to "make check"

VICARE_CHECK_FLAGS	= -b $(VICARE_BOOT) $(user_flags)
VICARE_CHECK_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_CHECK_FLAGS)

VICARE_INSTCHECK_FLAGS	= --print-loaded-libraries $(user_flags)
VICARE_LIBDIR		= $(libdir)/scheme
VICARE_INSTCHECK_ENV	= VICARE_LIBRARY_PATH=$(DESTDIR)$(VICARE_LIBDIR):$(VICARE_LIBRARY_PATH):$(srcdir):$(srcdir)/../scheme LD_LIBRARY_PATH=$(DESTDIR)$(libdir) VICARE_CHECK_QUIET=yes
VICARE_INSTCHECK_RUN	= $(VICARE_INSTCHECK_ENV) $(DESTDIR)$(bindir)/vicare $(VICARE_INSTCHECK_FLAGS)

# TESTS and TESTS_ENVIRONMENT are Automake variables.
TESTS_ENVIRONMENT	= VICARE_CHECK_QUIET=yes $(VICARE_CHECK_RUN)
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
	test-ikarus-r6rs-records-procedural.sps		\
	test-ikarus-reader.sps				\
	test-ikarus-set-position.sps			\
	test-ikarus-sorting.sps				\
	test-ikarus-strings.sps				\
	test-ikarus-symbol-table.sps			\
	test-ikarus-unicode.sps				\
	\
	test-vicare-arguments-validation.sps		\
	test-vicare-bignums.sps				\
	test-vicare-bytevectors.sps			\
	test-vicare-chars.sps				\
	test-vicare-collect.sps				\
	test-vicare-enumerations.sps			\
	test-vicare-equality.sps			\
	test-vicare-errno.sps				\
	test-vicare-expander.sps			\
	test-vicare-debugging.sps			\
	test-vicare-flonum-formatter.sps		\
	test-vicare-flonum-parser.sps			\
	test-vicare-general-c-buffers.sps		\
	test-vicare-keywords.sps			\
	test-vicare-infix.sps				\
	test-vicare-io.sps				\
	test-vicare-language-extensions.sps		\
	test-vicare-lists.sps				\
	test-vicare-log.sps				\
	test-vicare-parameters.sps			\
	test-vicare-parser-logic.sps			\
	test-vicare-platform.sps			\
	test-vicare-pointers.sps			\
	test-vicare-promises.sps			\
	test-vicare-reader.sps				\
	test-vicare-records-procedural.sps		\
	test-vicare-records-syntactic.sps		\
	test-vicare-round.sps				\
	test-vicare-string-to-number.sps		\
	test-vicare-strings.sps				\
	test-vicare-structs.sps				\
	test-vicare-symbols.sps				\
	test-vicare-syntactic-extensions.sps		\
	test-vicare-system.sps				\
	test-vicare-vectors.sps				\
	test-vicare-weak-tables.sps			\
	test-vicare-words.sps				\
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

if WANT_LIBFFI
TESTS	+= test-vicare-ffi.sps
endif

if WANT_POSIX
TESTS	+= \
	test-vicare-posix-basic.sps		\
	test-vicare-posix-net.sps		\
	test-vicare-posix-mmap.sps		\
	test-vicare-posix-bub.sps		\
	test-vicare-posix-time.sps		\
	test-vicare-posix-timers.sps		\
	test-vicare-posix-message-queue.sps	\
	test-vicare-posix-shared-memory.sps	\
	test-vicare-posix-semaphores.sps	\
	test-vicare-posix-sigwait.sps		\
	\
	test-vicare-shared-memory.sps		\
	test-vicare-sel.sps
endif

if WANT_GLIBC
TESTS	+= test-vicare-glibc.sps
endif

if WANT_LINUX
TESTS	+= test-vicare-linux.sps
endif

if WANT_CRE2
TESTS	+= foreign-test-vicare-cre2.sps
endif

installcheck-local:
	@for f in $(TESTS); do $(VICARE_INSTCHECK_RUN) $(srcdir)/$$f; done

XFAIL_TESTS = \
	test-vicare-log.sps			\
	test-issue-008.sps

#page
#### extra files to be distributed

EXTRA_DIST = \
	ikarus-test-framework.sls			\
	lists.sls					\
	lists/low.sls					\
	lists/stx.sls					\
	TEST-SOURCE-FILE.txt				\
	rn100						\
	\
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
	demo-vicare-gcc.sps				\
	demo-vicare-readline.sps			\
	demo-vicare-timerfd.sps				\
	demo-issue-35.sps				\
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
	test-ikarus-r6rs-records-procedural.sps		\
	test-ikarus-reader.sps				\
	test-ikarus-set-position.sps			\
	test-ikarus-sorting.sps				\
	test-ikarus-strings.sps				\
	test-ikarus-symbol-table.sps			\
	test-ikarus-unicode.sps				\
	\
	test-vicare-arguments-validation.sps		\
	test-vicare-bignums.sps				\
	test-vicare-bytevectors.sps			\
	test-vicare-chars.sps				\
	test-vicare-collect.sps				\
	test-vicare-enumerations.sps			\
	test-vicare-equality.sps			\
	test-vicare-errno.sps				\
	test-vicare-expander.sps			\
	test-vicare-debugging.sps			\
	test-vicare-ffi.sps				\
	test-vicare-flonum-formatter.sps		\
	test-vicare-flonum-parser.sps			\
	test-vicare-general-c-buffers.sps		\
	test-vicare-glibc.sps				\
	test-vicare-keywords.sps			\
	test-vicare-infix.sps				\
	test-vicare-io.sps				\
	test-vicare-language-extensions.sps		\
	test-vicare-linux.sps				\
	test-vicare-lists.sps				\
	test-vicare-log.sps				\
	test-vicare-parameters.sps			\
	test-vicare-parser-logic.sps			\
	test-vicare-platform.sps			\
	test-vicare-pointers.sps			\
	\
	test-vicare-posix-basic.sps			\
	test-vicare-posix-net.sps			\
	test-vicare-posix-mmap.sps			\
	test-vicare-posix-bub.sps			\
	test-vicare-posix-time.sps			\
	test-vicare-posix-timers.sps			\
	test-vicare-posix-message-queue.sps		\
	test-vicare-posix-shared-memory.sps		\
	test-vicare-posix-semaphores.sps		\
	test-vicare-posix-sigwait.sps			\
	\
	test-vicare-promises.sps			\
	test-vicare-reader.sps				\
	test-vicare-records-procedural.sps		\
	test-vicare-records-syntactic.sps		\
	test-vicare-round.sps				\
	test-vicare-sel.sps				\
	test-vicare-shared-memory.sps			\
	test-vicare-string-to-number.sps		\
	test-vicare-strings.sps				\
	test-vicare-structs.sps				\
	test-vicare-symbols.sps				\
	test-vicare-syntactic-extensions.sps		\
	test-vicare-system.sps				\
	test-vicare-vectors.sps				\
	test-vicare-weak-tables.sps			\
	test-vicare-words.sps				\
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
	test-issue-025-sign-in-exact-rationals.sps	\
	\
	foreign-test-vicare-cre2.sps

### end of file
