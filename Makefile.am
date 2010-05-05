## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS	= foreign
SUBDIRS			= src scheme doc lib benchmarks tests
EXTRA_DIST		= README COPYING GPL-3 configure.sh
dist_doc_DATA		= README COPYING GPL-3

.PHONY: test tests

test tests:
	cd tests ; $(MAKE) $(@)

test-r6rs:
	cd tests ; $(MAKE) $(@)

### end of file
