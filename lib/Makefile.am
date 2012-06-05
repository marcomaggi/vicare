## Process this file with automake to produce Makefile.in

EXTRA_DIST		= \
			  vicare/errno.sls.in			\
			  vicare/platform-constants.sls.in	\
			  vicare/words.sls.in			\
			  vicare/installation-configuration.sls.in

dist_pkglibexec_SCRIPTS	= compile-all.sps

libvicaredir		= $(libdir)/scheme

nobase_dist_libvicare_DATA	= \
	vicare/errno.sls			\
	vicare/flonum-formatter.sls		\
	vicare/flonum-parser.sls		\
	vicare/include.sls			\
	vicare/installation-configuration.sls	\
	vicare/keywords.sls			\
	vicare/parser-logic.sls			\
	vicare/platform-constants.sls		\
	vicare/syntactic-extensions.sls		\
	vicare/unsafe-capi.sls			\
	vicare/unsafe-operations.sls		\
	vicare/unsafe-unicode.sls		\
	vicare/weak-hashtables.sls		\
	vicare/words.sls			\
	vicare/checks.sls

if WANT_LIBFFI
nobase_dist_libvicare_DATA	+= vicare/ffi.sls
dist_pkglibexec_SCRIPTS		+= compile-ffi.sps
endif

if WANT_POSIX
nobase_dist_libvicare_DATA	+= vicare/posix.sls	\
				   vicare/simple-event-loop.sls
dist_pkglibexec_SCRIPTS		+= compile-posix.sps
endif

if WANT_GLIBC
nobase_dist_libvicare_DATA	+= vicare/glibc.sls
dist_pkglibexec_SCRIPTS		+= compile-glibc.sps
endif

if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
nobase_dist_libvicare_DATA	+= vicare/gcc.sls
dist_pkglibexec_SCRIPTS		+= compile-gcc.sps
endif
endif
endif

if WANT_LINUX
nobase_dist_libvicare_DATA	+= vicare/linux.sls
dist_pkglibexec_SCRIPTS		+= compile-linux.sps
endif

if WANT_READLINE
nobase_dist_libvicare_DATA	+= vicare/readline.sls
dist_pkglibexec_SCRIPTS		+= compile-readline.sps
endif

if WANT_CRE2
nobase_dist_libvicare_DATA	+= vicare/cre2.sls
dist_pkglibexec_SCRIPTS		+= compile-cre2.sps
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
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-ffi.sps
endif
if WANT_POSIX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-posix.sps
endif
if WANT_GLIBC
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-glibc.sps
endif
if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-gcc.sps
endif
endif
endif
if WANT_LINUX
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-linux.sps
endif
if WANT_READLINE
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-readline.sps
endif
if WANT_CRE2
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_LIBFFI
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-ffi.sps
endif
if WANT_POSIX
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-posix.sps
endif
if WANT_GLIBC
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-glibc.sps
endif
if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-gcc.sps
endif
endif
endif
if WANT_LINUX
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-linux.sps
endif
if WANT_READLINE
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-readline.sps
endif
if WANT_CRE2
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-cre2.sps
endif

### end of file
