;; build-makefile-rules.sps --
;;
;;FIXME  We are  not tracking  the  include files  among the  libraries'
;;precedences,  but we  should.  At  present the  include files  are not
;;collected by the  expander, but maybe they should.   (Marco Maggi; Tue
;;Feb 25, 2014)
;;
#!r6rs
(import (vicare)
  (vicare build-tools automake))

(module (FROM-TEMPLATES-SOURCE-FILES
	 BUILT-SOURCE-FILES
	 LIBRARIES-SPECS)
  (include "libraries.scm"))

(display (build-library-dependencies FROM-TEMPLATES-SOURCE-FILES
				     BUILT-SOURCE-FILES
				     LIBRARIES-SPECS)
	 stdout)
(flush-output-port stdout)
(exit 0)

;;; end of file
