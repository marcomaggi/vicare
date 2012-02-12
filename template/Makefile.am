## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I m4
AUTOMAKE_OPTIONS	= foreign
SUBDIRS			= src doc tests lib
EXTRA_DIST		= README COPYING INSTALL configure.sh
dist_doc_DATA		= README COPYING

vicare_test_RULES	= test tests proof proofs
vicare_lib_RULES	= vfasl vfasl-installed

.PHONY: $(vicare_test_RULES) $(vicare_lib_RULES)

$(vicare_test_RULES):
	cd tests ; $(MAKE) $(@)

$(vicare_lib_RULES):
	cd lib; $(MAKE) $(@)

### end of file
