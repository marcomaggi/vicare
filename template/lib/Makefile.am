## Process this file with automake to produce Makefile.in

libvicaredir	= $(libdir)/scheme

EXTRA_DIST	=			\
	compile-all.sps			\
	vicare/template/constants.sls.in

nobase_dist_libvicare_DATA =		\
	vicare/template.sls		\
	vicare/template/constants.sls

## --------------------------------------------------------------------

VICARE_FLAGS	= $(user_flags)

VICARE		= @VICARE@
VICARE_ENV	= VICARE_LIBRARY_PATH=$(builddir):$(srcdir)	\
		  LD_LIBRARY_PATH=$(builddir)/../src/.libs
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

VICARE_INST	= $(bindir)/vicare
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE_INST)

.PHONY: vfasl vfasl-installed

vfasl:
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-all.sps

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps

### end of file
