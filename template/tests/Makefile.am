## Process this file with automake to produce Makefile.in

testdir			= $(srcdir)
VICARE_SELECTED_TESTS	= $(testdir)/test-*$(file)*.sps
VICARE_SELECTED_PROOFS	= $(testdir)/proof-*$(file)*.sps

VICARE_TEST_SEARCH_PATH	= $(srcdir):$(srcdir)/../lib:$(builddir)/../lib

# The variable "user_flags"  is for the user to be  set on the command
# line of "make".
VICARE		= @VICARE@
VICARE_FLAGS	= --debug $(user_flags)
VICARE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH)	\
		  CHECK_TEST_NAME=$(name)				\
		  VICARE_SRC_DIR=$(srcdir) VICARE_BUILDDIR=$(builddir)	\
		  LD_LIBRARY_PATH=$(builddir)/../src/.libs
VICARE_RUN	= $(VICARE_ENV) $(GDB) $(VICARE) $(VICARE_FLAGS)

.PHONY: test tests proof proofs

test tests:
	@echo
	@for f in $(VICARE_SELECTED_TESTS); do $(VICARE_RUN) $$f; done
	@echo

proof proofs:
	@echo
	@for f in $(VICARE_SELECTED_PROOFS); do $(VICARE_RUN) $$f; done
	@echo

### interface to "make check"

# TESTS and TESTS_ENVIRONMENT are Automake variables.
TESTS_ENVIRONMENT	= $(VICARE_RUN)
TESTS			=	\
	test-vicare-template.sps

EXTRA_DIST =	\
	test-vicare-template.sps			\
	\
	proof-for-documentation.sps			\
	\
	checks.sls

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
