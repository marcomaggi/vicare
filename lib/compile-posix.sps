;;;; compile script for POSIX-specific libraries

#!r6rs
(import (only (vicare posix))
  (only (vicare posix pid-files))
  (only (vicare posix lock-pid-files))
  (only (vicare posix log-files))
  (only (vicare posix daemonisations))
  (only (vicare posix simple-event-loop))
  (only (vicare posix tcp-server-sockets)))

;;; end of file
