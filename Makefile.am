## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS	= foreign
SUBDIRS			= src scheme doc lib benchmarks tests
EXTRA_DIST		= README COPYING GPL-3 configure.sh
dist_doc_DATA		= README COPYING GPL-3

vicare_test_RULES	= test tests long-test long-tests

.PHONY: $(vicare_test_RULES) try-vicare-boot

$(vicare_test_RULES):
	cd tests ; $(MAKE) $(@)

ikarus-tests:
	cd scheme; $(MAKE) $(@)

try-vicare-boot:
	cd scheme; $(MAKE) $(@)

### end of file
