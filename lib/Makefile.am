## Process this file with automake to produce Makefile.in

libvicaredir		= $(pkglibdir)/vicare

EXTRA_DIST		= compile-all.sps			\
			  vicare/platform-constants.sls.in	\
			  vicare/words.sls.in			\
			  vicare/installation-configuration.sls.in

dist_libvicare_DATA	= vicare/ffi.sls			\
			  vicare/platform-constants.sls		\
			  vicare/include.sls			\
			  vicare/syntactic-extensions.sls	\
			  vicare/flonum-parser.sls		\
			  vicare/flonum-formatter.sls		\
			  vicare/unsafe-operations.sls		\
			  vicare/unsafe-unicode.sls		\
			  vicare/unsafe-capi.sls		\
			  vicare/words.sls			\
			  vicare/installation-configuration.sls	\
			  vicare/gcc.sls

## --------------------------------------------------------------------

VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_FLAGS	= -b $(VICARE_BOOT) $(user_flags)
VICARE_ENV	= VICARE_LIBRARY_PATH=$(builddir):$(srcdir)
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE) $(VICARE_FLAGS)

.PHONY: vfasl-installed

vfasl:
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-all.sps

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps

### end of file
