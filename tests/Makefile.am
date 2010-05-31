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
VICARE_R6RS_ENV	= VICARE_LIBRARY_PATH=$(srcdir)/..:$(srcdir)/../lib
VICARE_R6RS_RUN	= $(VICARE_R6RS_ENV) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests test-r6rs

test tests:
	@echo
	@$(foreach f,$(VICARE_SELECTED_TESTS),$(VICARE_RUN) $(f);)
	@echo

test-r6rs:
	$(VICARE_R6RS_RUN) $(srcdir)/r6rs/run.sps

### end of file
