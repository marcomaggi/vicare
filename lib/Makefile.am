## Process this file with automake to produce Makefile.in

libvicaredir		= $(pkglibdir)/vicare

EXTRA_DIST		= compile-all.sps			\
			  compile-ffi.sps			\
			  compile-glibc.sps			\
			  compile-linux.sps			\
			  compile-cre2.sps			\
			  vicare/platform-constants.sls.in	\
			  vicare/words.sls.in			\
			  vicare/installation-configuration.sls.in

dist_libvicare_DATA	= \
	vicare/platform-constants.sls		\
	vicare/include.sls			\
	vicare/syntactic-extensions.sls		\
	vicare/flonum-parser.sls		\
	vicare/flonum-formatter.sls		\
	vicare/unsafe-operations.sls		\
	vicare/unsafe-unicode.sls		\
	vicare/unsafe-capi.sls			\
	vicare/words.sls			\
	vicare/installation-configuration.sls	\
	vicare/gcc.sls

if WANT_LIBFFI
dist_libvicare_DATA	+= vicare/ffi.sls
endif

if WANT_GLIBC
dist_libvicare_DATA	+= vicare/glibc.sls
endif

if WANT_LINUX
dist_libvicare_DATA	+= vicare/linux.sls
endif

if WANT_CRE2
dist_libvicare_DATA	+= vicare/cre2.sls
endif

## --------------------------------------------------------------------

VICARE_FLAGS	= -b $(VICARE_BOOT) $(user_flags)

VICARE		= $(builddir)/../src/vicare
VICARE_BOOT	= $(builddir)/../scheme/vicare.boot
VICARE_ENV	= VICARE_LIBRARY_PATH=$(builddir):$(srcdir)	\
		  LD_LIBRARY_PATH=$(builddir)/../src/.libs
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

VICARE_INST	= $(bindir)/vicare
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE_INST)

.PHONY: vfasl-installed

vfasl:
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_LIBFFI
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-libffi.sps
endif
if WANT_GLIBC
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-glibc.sps
endif
if WANT_LINUX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-linux.sps
endif
if WANT_CRE2
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_LIBFFI
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-libffi.sps
endif
if WANT_GLIBC
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-glibc.sps
endif
if WANT_LINUX
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-linux.sps
endif
if WANT_CRE2
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

### end of file
