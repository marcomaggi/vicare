;; regexp.scm -- simple non-bactracking NFA implementation
;; Copyright (c) 2013-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; A regular expression engine implementing SRFI 115 using a
;; non-backtracking Thompson NFA algorithm.

;;Copyright (c) 2000-2015 Alex Shinn
;;All rights reserved.
;;
;;Redistribution and  use in source and  binary forms, with or  without modification,
;;are permitted provided that the following conditions are met:
;;
;;1. Redistributions of source code must retain the above copyright notice, this list
;;   of conditions and the following disclaimer.
;;
;;2. Redistributions in  binary form must reproduce the above  copyright notice, this
;;   list  of conditions  and the  following disclaimer  in the  documentation and/or
;;   other materials provided with the distribution.
;;
;;3. The name  of the author may not  be used to endorse or  promote products derived
;;   from this software without specific prior written permission.
;;
;;THIS  SOFTWARE IS  PROVIDED BY  THE AUTHOR  ``AS IS''  AND ANY  EXPRESS OR  IMPLIED
;;WARRANTIES,   INCLUDING,  BUT   NOT   LIMITED  TO,   THE   IMPLIED  WARRANTIES   OF
;;MERCHANTABILITY AND FITNESS  FOR A PARTICULAR PURPOSE ARE DISCLAIMED.   IN NO EVENT
;;SHALL  THE  AUTHOR  BE  LIABLE  FOR  ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,
;;EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  BUT NOT LIMITED TO, PROCUREMENT OF
;;SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF  USE,  DATA,  OR  PROFITS;  OR  BUSINESS
;;INTERRUPTION) HOWEVER CAUSED  AND ON ANY THEORY OF LIABILITY,  WHETHER IN CONTRACT,
;;STRICT LIABILITY,  OR TORT (INCLUDING NEGLIGENCE  OR OTHERWISE) ARISING IN  ANY WAY
;;OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

(library (srfi :115)
  (export
    regexp regexp? valid-sre? rx regexp->sre char-set->sre
    regexp-matches regexp-matches? regexp-search
    regexp-replace regexp-replace-all
    regexp-fold regexp-extract regexp-split regexp-partition
    regexp-match? regexp-match-count
    regexp-match-submatch regexp-match-submatch/list
    regexp-match-submatch-start regexp-match-submatch-end
    regexp-match->list regexp-match->sexp)
  (import (srfi :115 regexps)))

;;; end of file
