## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = src scheme doc lib benchmarks
EXTRA_DIST = README COPYING GPL-3
dist_doc_DATA=README COPYING GPL-3

## --------------------------------------------------------------------

testdir			= $(srcdir)/tests
VICARE_SELECTED_TESTS	= $(wildcard $(testdir)/test-*$(name)*.sps)

VICARE		= $(builddir)/src/vicare
VICARE_BOOT	= $(builddir)/scheme/vicare.boot
VICARE_FLAGS	= -b $(VICARE_BOOT) --debug --r6rs-script
VICARE_ENV	= VICARE_LIBRARY_PATH=$(srcdir)/tests:$(srcdir)/lib
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests

test tests:
	@$(foreach f,$(VICARE_SELECTED_TESTS),$(VICARE_RUN) $(f))


### end of file
