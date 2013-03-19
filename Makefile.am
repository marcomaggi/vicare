## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I autotools
SUBDIRS			= src scheme doc lib tests demos
EXTRA_DIST		= configure.sh			\
			  scripts/make-last-revision	\
			  scripts/vie
dist_doc_DATA		= README COPYING GPL-3

vicare_test_RULES	= test tests long-test long-tests \
			  test-run test-vie test-exec-modes \
			  demo demos foreign-test foreign-tests

vicare_lib_RULES	= vfasl vfasl-installed

if WANT_BINFMT
rcddir			= $(sysconfdir)/rc.d
rcd_SCRIPTS		= rc.vicare-scheme

rc.vicare-scheme: Makefile
	@echo '#!/bin/sh' >$(@)
	@echo '#' >>$(@)
	@echo '# Setup the  Linux module  "binfmt_misc" to automatically  recognise and' >>$(@)
	@echo '# run Vicare Scheme programs using the installed "vicare" executable.' >>$(@)
	@echo '' >>$(@)
	@echo 'if test -f /proc/sys/fs/binfmt_misc/register' >>$(@)
	@echo 'then' >>$(@)
	@echo '  if test -f /proc/sys/fs/binfmt_misc/VICARE' >>$(@)
	@echo '  then echo -1 >/proc/sys/fs/binfmt_misc/VICARE' >>$(@)
	@echo '  fi' >>$(@)
	@echo '  echo -n enabling binfmt Vicare Scheme scripts execution...' >>$(@)
	@echo '  echo ":VICARE:M:0:;;;!vicare::$(bindir)/vicare:" >/proc/sys/fs/binfmt_misc/register' >>$(@)
	@echo '  echo done' >>$(@)
	@echo 'fi' >>$(@)
	@echo '' >>$(@)
	@echo '### end of file' >>$(@)

endif

.PHONY: $(vicare_test_RULES) $(vicare_lib_RULES) \
	try-vicare-boot boot-image-64-bit


$(vicare_test_RULES):
	cd tests ; $(MAKE) $(@)

try-vicare-boot:
	cd scheme; $(MAKE) $(@)

boot-image-64-bit:
	cd scheme; $(MAKE) $(@)

$(vicare_lib_RULES):
	cd lib; $(MAKE) $(@)

### end of file
