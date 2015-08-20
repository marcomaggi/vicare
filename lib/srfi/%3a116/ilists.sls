;;; SRFI-116 ilist-processing library 			-*- Scheme -*-
;;; Sample implementation
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers.
;;; Modifications Copyright (c) 2014 by John Cowan.
;;; Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;; You may do as you please with
;;; this code as long as you do not remove either copyright notice
;;; or hold us liable for its use.  Please send bug reports to
;;; <srfi-116@srfi.schemers.org>.


(library (srfi :116 ilists)
  (export
    iq
    ipair ilist xipair ipair* make-ilist ilist-tabulate iiota ilist-copy
    ipair?
    proper-ilist? ilist? dotted-ilist? not-ipair? null-ilist? ilist=
    icar icdr ilist-ref
    ifirst isecond ithird ifourth ififth isixth iseventh ieighth ininth itenth
    icaar icadr icdar icddr
    icaaar icaadr icadar icaddr icdaar icdadr icddar icdddr
    icaaaar icaaadr icaadar icaaddr icadaar icadadr icaddar icadddr
    icdaaar icdaadr icdadar icdaddr icddaar icddadr icdddar icddddr
    icar+icdr itake idrop ilist-tail
    (rename (itake	itake-left)
	    (idrop	idrop-left))
    itake-right idrop-right isplit-at ilast last-ipair
    ilength iappend iconcatenate ireverse iappend-reverse
    izip iunzip1 iunzip2 iunzip3 iunzip4 iunzip5
    icount imap ifor-each ifold iunfold ipair-fold ireduce
    ifold-right iunfold-right ipair-fold-right ireduce-right
    iappend-map ipair-for-each ifilter-map imap-in-order
    ifilter ipartition iremove imember imemq imemv
    ifind ifind-tail iany ievery
    ilist-index itake-while idrop-while ispan ibreak
    idelete idelete-duplicates
    iassoc iassq iassv ialist-cons ialist-copy ialist-delete
    replace-icar replace-icdr
    pair->ipair			ipair->pair
    list->ilist			ilist->list
    ilist->vector		vector->ilist
    tree->itree itree->tree gtree->itree gtree->tree
    iapply
    make-ilist-iteration-thunk)
  (import (vicare containers ilists)))

;;; end of file
