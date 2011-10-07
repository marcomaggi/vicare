## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS	= foreign
SUBDIRS			= src scheme doc lib benchmarks tests
EXTRA_DIST		= README COPYING GPL-3 configure.sh
dist_doc_DATA		= README COPYING GPL-3

vicare_test_RULES	= test tests test-r6rs \
	test-r6rs-control		\
	test-r6rs-base			\
	test-r6rs-io-ports		\
	test-r6rs-io-simple		\
	test-r6rs-eval			\
	test-r6rs-syntax-case		\
	test-r6rs-mutable-strings	\
	test-r6rs-records-procedural	\
	test-r6rs-records-syntactic	\
	test-r6rs-conditions		\
	test-r6rs-hashtables		\
	test-r6rs-programs		\
	test-r6rs-contrib		\
	test-r6rs-unicode		\
	test-r6rs-r5rs			\
	test-r6rs-reader		\
	test-r6rs-mutable-pairs		\
	test-r6rs-lists			\
	test-r6rs-arithmetic-fixnums	\
	test-r6rs-arithmetic-bitwise	\
	test-r6rs-arithmetic-flonums	\
	test-r6rs-bytevectors		\
	test-r6rs-exceptions		\
	test-r6rs-enums			\
	test-r6rs-sorting

.PHONY: $(vicare_test_RULES) try-vicare-boot

$(vicare_test_RULES):
	cd tests ; $(MAKE) $(@)

ikarus-tests:
	cd scheme; $(MAKE) $(@)

try-vicare-boot:
	cd scheme; $(MAKE) $(@)

### end of file
