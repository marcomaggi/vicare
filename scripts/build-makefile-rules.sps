;; build-makefile-rules.sps --
;;
;;This script should be run from the build directory with a command line similar to:
;;
;;   $ ./vicare -b vicare.boot							\
;;	   --source-path $PWD/../lib --source-path $PWD/lib			\
;;	   --print-loaded-libraries --library-locator source			\
;;         --r6rs-script $(top_srcdir)/scripts/build-makefile-rules.sps		\
;;         --									\
;;         $(top_srcdir)/lib/libraries.scm >$(top_srcdir)/lib/dependencies.make
;;
;;FIXME We are  not tracking the include files among  the libraries' precedences, but
;;we should.   At present the  include files are not  collected by the  expander, but
;;maybe they should.  (Marco Maggi; Tue Feb 25, 2014)
;;

#!r6rs
(import (vicare))

(define-constant INCLUDE-FILE
  (cadr (command-line)))

(eval `(let ()
	 (module (FROM-TEMPLATES-SOURCE-FILES
		  BUILT-SOURCE-FILES
		  LIBRARIES-SPECS
		  INCLUDE-LIBRARY-BUILD-HIERARCHIES
		  INCLUDE-LIBRARY-DEPENDENCIES-HIERARCHIES
		  INCLUDE-INSTALL-RULES)
	   (include ,INCLUDE-FILE))

	 (display (parametrise
		      ((include-library-build-hierarchies		INCLUDE-LIBRARY-BUILD-HIERARCHIES)
		       (include-library-dependencies-hierarchies	INCLUDE-LIBRARY-DEPENDENCIES-HIERARCHIES)
		       (include-install-rules				INCLUDE-INSTALL-RULES))
		    (build-library-dependencies FROM-TEMPLATES-SOURCE-FILES
						BUILT-SOURCE-FILES
						LIBRARIES-SPECS))
		  stdout)

	 (flush-output-port stdout))
      (environment '(vicare)
		   '(vicare build-tools automake)))

(exit 0)

;;; end of file
