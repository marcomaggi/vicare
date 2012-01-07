## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I m4
AUTOMAKE_OPTIONS	= foreign
SUBDIRS			= src scheme doc lib tests
EXTRA_DIST		= README COPYING GPL-3		\
			  configure.sh			\
			  scripts/make-last-revision	\
			  scripts/vie
dist_doc_DATA		= README COPYING GPL-3

vicare_test_RULES	= test tests long-test long-tests \
			  test-run test-vie test-exec-modes \
			  proof proofs

vicare_lib_RULES	= vfasl vfasl-installed

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
