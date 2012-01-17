/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov	 6, 2011

  Abstract

	Interface to POSIX functions.  For the full documentation of the
	functions in this module,  see the official Vicare documentation
	in Texinfo format.

  Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
  Copyright (C) 2006,2007,2008	Abdulaziz Ghuloum

  This program is  free software: you can redistribute	it and/or modify
  it under the	terms of the GNU General Public	 License as published by
  the Free Software Foundation, either	version 3 of the License, or (at
  your option) any later version.

  This program	is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY	 WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See	 the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

/* This causes inclusion of "pread".  (Marco Maggi; Nov 11, 2011) */
#define _GNU_SOURCE	1

#include "internals.h"
#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <pwd.h>
#include <signal.h>
#include <stdint.h>
#include <termios.h>
#include <time.h>
#include <utime.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/wait.h>

#define SIZEOF_STRUCT_IN_ADDR		sizeof(struct in_addr)
#define SIZEOF_STRUCT_IN6_ADDR		sizeof(struct in6_addr)

/* process identifiers */
#define IK_PID_TO_NUM(pid)		IK_FIX(pid)
#define IK_NUM_TO_PID(pid)		IK_UNFIX(pid)

/* user identifiers */
#define IK_UID_TO_NUM(uid)		IK_FIX(uid)
#define IK_NUM_TO_UID(uid)		IK_UNFIX(uid)

/* group identifiers */
#define IK_GID_TO_NUM(gid)		IK_FIX(gid)
#define IK_NUM_TO_GID(gid)		IK_UNFIX(gid)

/* file descriptors */
#define IK_FD_TO_NUM(fd)		IK_FIX(fd)
#define IK_NUM_TO_FD(fd)		IK_UNFIX(fd)

/* interprocess signal numbers */
#define IK_SIGNUM_TO_NUM(signum)	IK_FIX(signum)
#define IK_NUM_TO_SIGNUM(signum)	IK_UNFIX(signum)

/* file access mode */
#define IK_FILEMODE_TO_NUM(filemode)	IK_FIX(filemode)
#define IK_NUM_TO_FILEMODE(filemode)	IK_UNFIX(filemode)


/** --------------------------------------------------------------------
 ** Errno handling.
 ** ----------------------------------------------------------------- */

ikptr
ik_errno_to_code (void)
/* Negate the current errno value and convert it into a fixnum. */
{
  int negated_errno_code = - errno;
  return IK_FIX(negated_errno_code);
}
ikptr
ikrt_posix_strerror (ikptr negated_errno_code, ikpcb* pcb)
{
  int	 code = - IK_UNFIX(negated_errno_code);
  errno = 0;
  char * error_message = strerror(code);
  return errno? false_object : ika_bytevector_from_cstring(pcb, error_message);
}


/** --------------------------------------------------------------------
 ** Operating system environment variables.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getenv (ikptr bv, ikpcb* pcb)
{
  char *  str = getenv(IK_BYTEVECTOR_DATA_CHARP(bv));
  return (str)? ika_bytevector_from_cstring(pcb, str) : false_object;
}
ikptr
ikrt_posix_setenv (ikptr key, ikptr val, ikptr overwrite)
{
  int	err = setenv(IK_BYTEVECTOR_DATA_CHARP(key),
		     IK_BYTEVECTOR_DATA_CHARP(val),
		     (overwrite != false_object));
  return (err)? false_object : true_object;
}
ikptr
ikrt_posix_unsetenv (ikptr key)
{
  char *	varname = IK_BYTEVECTOR_DATA_CHARP(key);
#if (1 == UNSETENV_HAS_RETURN_VALUE)
  int		rv = unsetenv(varname);
  return (0 == rv)? true_object : false_object;
#else
  unsetenv(varname);
  return true_object;
#endif
}
ikptr
ikrt_posix_environ (ikpcb* pcb)
{
  ikptr		s_list	= null_object;
  ikptr		s_spine = null_object;
  int		i;
  s_list = s_spine = ika_pair_alloc(pcb);
  pcb->root0 = &s_list;
  pcb->root1 = &s_spine;
  {
    for (i=0; environ[i];) {
      IK_ASS(IK_CAR(s_spine), ika_bytevector_from_cstring(pcb, environ[i]));
      if (environ[++i]) {
	IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	s_spine = IK_CDR(s_spine);
      } else
	IK_CDR(s_spine) = null_object;
    }
  }
  pcb->root1 = NULL;
  pcb->root0 = NULL;
  return s_list;
}


/** --------------------------------------------------------------------
 ** Process identifiers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getpid(void)
{
  return IK_PID_TO_NUM(getpid());
}
ikptr
ikrt_posix_getppid(void)
{
  return IK_PID_TO_NUM(getppid());
}


/** --------------------------------------------------------------------
 ** Executing processes.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_system (ikptr command)
{
  int		rv;
  errno = 0;
  rv	= system(IK_BYTEVECTOR_DATA_CHARP(command));
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_fork (void)
{
  int	pid;
  errno = 0;
  pid	= fork();
  return (0 <= pid)? IK_PID_TO_NUM(pid) : ik_errno_to_code();
}
ikptr
ikrt_posix_execv (ikptr filename_bv, ikptr argv_list)
{
  char *  filename = IK_BYTEVECTOR_DATA_CHARP(filename_bv);
  int	  argc	   = ik_list_length(argv_list);
  char *  argv[1+argc];
  ik_list_to_argv(argv_list, argv);
  errno	  = 0;
  execv(filename, argv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}
ikptr
ikrt_posix_execve (ikptr filename_bv, ikptr argv_list, ikptr envv_list)
{
  char *  filename = IK_BYTEVECTOR_DATA_CHARP(filename_bv);
  int	  argc = ik_list_length(argv_list);
  char *  argv[1+argc];
  int	  envc = ik_list_length(envv_list);
  char *  envv[1+envc];
  ik_list_to_argv(argv_list, argv);
  ik_list_to_argv(envv_list, envv);
  errno	 = 0;
  execve(filename, argv, envv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}
ikptr
ikrt_posix_execvp (ikptr filename_bv, ikptr argv_list)
{
  char *  filename = IK_BYTEVECTOR_DATA_CHARP(filename_bv);
  int	  argc = ik_list_length(argv_list);
  char *  argv[1+argc];
  ik_list_to_argv(argv_list, argv);
  errno	 = 0;
  execvp(filename, argv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Process exit status.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_waitpid (ikptr s_pid, ikptr s_options, ikpcb * pcb)
{
  int	status;
  pid_t rv;
  errno	 = 0;
  rv	 = waitpid(IK_NUM_TO_PID(s_pid), &status, IK_UNFIX(s_options));
  return (0 <= rv)? ika_integer_from_int(pcb, status) : ik_errno_to_code();
}
ikptr
ikrt_posix_wait (ikpcb * pcb)
{
  int	status;
  pid_t rv;
  errno	 = 0;
  rv	 = wait(&status);
  return (0 <= rv)? ika_integer_from_int(pcb, status) : ik_errno_to_code();
}
ikptr
ikrt_posix_WIFEXITED (ikptr s_status)
{
  int	status = ik_integer_to_int(s_status);
  return (WIFEXITED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WEXITSTATUS (ikptr s_status, ikpcb * pcb)
{
  int	status = ik_integer_to_int(s_status);
  return ika_integer_from_int(pcb, WEXITSTATUS(status));
}
ikptr
ikrt_posix_WIFSIGNALED (ikptr s_status)
{
  int	status = ik_integer_to_int(s_status);
  return (WIFSIGNALED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WTERMSIG (ikptr s_status, ikpcb * pcb)
{
  int	status = ik_integer_to_int(s_status);
  return ika_integer_from_int(pcb, WTERMSIG(status));
}
ikptr
ikrt_posix_WCOREDUMP (ikptr s_status)
{
  int	status = ik_integer_to_int(s_status);
  return (WCOREDUMP(status))? true_object : false_object;
}
ikptr
ikrt_posix_WIFSTOPPED (ikptr s_status)
{
  int	status = ik_integer_to_int(s_status);
  return (WIFSTOPPED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WSTOPSIG (ikptr s_status, ikpcb * pcb)
{
  int	status = ik_integer_to_int(s_status);
  return ika_integer_from_int(pcb, WSTOPSIG(status));
}


/** --------------------------------------------------------------------
 ** Delivering interprocess signals.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_raise (ikptr s_signum)
{
  int r = raise(IK_NUM_TO_SIGNUM(s_signum));
  return (0 == r)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_kill (ikptr s_pid, ikptr s_signum)
{
  pid_t pid    = IK_NUM_TO_PID(s_pid);
  int	signum = IK_NUM_TO_SIGNUM(s_signum);
  int	r = kill(pid, signum);
  return (0 == r)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_pause (void)
{
  pause();
  return void_object;
}


/** --------------------------------------------------------------------
 ** Miscellaneous stat functions.
 ** ----------------------------------------------------------------- */

static ikptr
fill_stat_struct (struct stat * S, ikptr D, ikpcb* pcb)
{
  pcb->root9 = &D;
  {
#if (4 == IK_SIZE_OF_VOIDP)
    /* 32-bit platforms */
    IK_ASS(IK_FIELD(D, 0), ika_integer_from_ulong(pcb, (ik_ulong)S->st_mode));
    IK_ASS(IK_FIELD(D, 1), ika_integer_from_ulong(pcb, (ik_ulong)S->st_ino));
    IK_ASS(IK_FIELD(D, 2), ika_integer_from_long (pcb, (long)S->st_dev));
    IK_ASS(IK_FIELD(D, 3), ika_integer_from_ulong(pcb, (long)S->st_nlink));

    IK_ASS(IK_FIELD(D, 4), ika_integer_from_ulong(pcb, (ik_ulong)S->st_uid));
    IK_ASS(IK_FIELD(D, 5), ika_integer_from_ulong(pcb, (ik_ulong)S->st_gid));
    IK_ASS(IK_FIELD(D, 6), ika_integer_from_ullong(pcb,(ik_ullong)S->st_size));

    IK_ASS(IK_FIELD(D, 7), ika_integer_from_long(pcb, (long)S->st_atime));
#ifdef HAVE_STAT_ST_ATIME_USEC
    IK_ASS(IK_FIELD(D, 8), ika_integer_from_ulong(pcb, (ik_ulong)S->st_atime_usec));
#else
    IK_ASS(IK_FIELD(D, 8), false_object);
#endif

    IK_ASS(IK_FIELD(D, 9) , ika_integer_from_long(pcb, (long)S->st_mtime));
#ifdef HAVE_STAT_ST_MTIME_USEC
    IK_ASS(IK_FIELD(D, 10), ika_integer_from_ulong(pcb, (ik_ulong)S->st_mtime_usec));
#else
    IK_ASS(IK_FIELD(D, 10), false_object);
#endif

    IK_ASS(IK_FIELD(D, 11), ika_integer_from_long(pcb, (long)S->st_ctime));
#ifdef HAVE_STAT_ST_CTIME_USEC
    IK_ASS(IK_FIELD(D, 12), ika_integer_from_ulong(pcb, (ik_ulong)S->st_ctime_usec));
#else
    IK_ASS(IK_FIELD(D, 12), false_object);
#endif

    IK_ASS(IK_FIELD(D, 13), ika_integer_from_ullong(pcb, (ik_ullong)S->st_blocks));
    IK_ASS(IK_FIELD(D, 14), ika_integer_from_ullong(pcb, (ik_ullong)S->st_blksize));
#else
    /* 64-bit platforms */
    IK_ASS(IK_FIELD(D, 0), ika_integer_from_ullong(pcb, (ik_ullong)S->st_mode));
    IK_ASS(IK_FIELD(D, 1), ika_integer_from_ullong(pcb, (ik_ullong)S->st_ino));
    IK_ASS(IK_FIELD(D, 2), ika_integer_from_llong(pcb, (long)S->st_dev));
    IK_ASS(IK_FIELD(D, 3), ika_integer_from_ullong(pcb, (long)S->st_nlink));

    IK_ASS(IK_FIELD(D, 4), ika_integer_from_ullong(pcb, (ik_ullong)S->st_uid));
    IK_ASS(IK_FIELD(D, 5), ika_integer_from_ullong(pcb, (ik_ullong)S->st_gid));
    IK_ASS(IK_FIELD(D, 6), ika_integer_from_ullong(pcb, (ik_ullong)S->st_size));

    IK_ASS(IK_FIELD(D, 7), ika_integer_from_llong(pcb, (long)S->st_atime));
#ifdef HAVE_STAT_ST_ATIME_USEC
    IK_ASS(IK_FIELD(D, 8), ika_integer_from_ullong(pcb, (ik_ullong)S->st_atime_usec));
#else
    IK_ASS(IK_FIELD(D, 8), false_object);
#endif

    IK_ASS(IK_FIELD(D, 9) , ika_integer_from_llong(pcb, (long)S->st_mtime));
#ifdef HAVE_STAT_ST_MTIME_USEC
    IK_ASS(IK_FIELD(D, 10), ika_integer_from_ullong(pcb, (ik_ullong)S->st_mtime_usec));
#else
    IK_ASS(IK_FIELD(D, 10), false_object);
#endif

    IK_ASS(IK_FIELD(D, 11), ika_integer_from_llong(pcb, (long)S->st_ctime));
#ifdef HAVE_STAT_ST_CTIME_USEC
    IK_ASS(IK_FIELD(D, 12), ika_integer_from_ullong(pcb, (ik_ullong)S->st_ctime_usec));
#else
    IK_ASS(IK_FIELD(D, 12), false_object);
#endif

    IK_ASS(IK_FIELD(D, 13), ika_integer_from_ullong(pcb, (ik_ullong)S->st_blocks));
    IK_ASS(IK_FIELD(D, 14), ika_integer_from_ullong(pcb, (ik_ullong)S->st_blksize));
#endif
  }
  pcb->root9 = NULL;
  return 0;
}
static ikptr
posix_stat (ikptr filename_bv, ikptr s_stat_struct, int follow_symlinks, ikpcb* pcb)
{
  char *	filename;
  struct stat	S;
  int		rv;
  filename = IK_BYTEVECTOR_DATA_CHARP(filename_bv);
  errno	   = 0;
  rv = (follow_symlinks)? stat(filename, &S) : lstat(filename, &S);
  return (0 == rv)? fill_stat_struct(&S, s_stat_struct, pcb) : ik_errno_to_code();
}
ikptr
ikrt_posix_stat (ikptr filename_bv, ikptr s_stat_struct, ikpcb* pcb)
{
  return posix_stat(filename_bv, s_stat_struct, 1, pcb);
}
ikptr
ikrt_posix_lstat (ikptr filename_bv, ikptr s_stat_struct, ikpcb* pcb)
{
  return posix_stat(filename_bv, s_stat_struct, 0, pcb);
}
ikptr
ikrt_posix_fstat (ikptr s_fd, ikptr s_stat_struct, ikpcb* pcb)
{
  struct stat	S;
  int		rv;
  errno = 0;
  rv	= fstat(IK_NUM_TO_FD(s_fd), &S);
  return (0 == rv)? fill_stat_struct(&S, s_stat_struct, pcb) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_file_size(ikptr s_filename, ikpcb* pcb)
{
  char *	filename;
  struct stat	S;
  int		rv;
  filename = IK_BYTEVECTOR_DATA_CHARP(s_filename);
  errno	   = 0;
  rv	   = stat(filename, &S);
  if (0 == rv) {
    if (sizeof(off_t) == sizeof(long))
      return ika_integer_from_ulong(pcb, S.st_size);
    else if (sizeof(off_t) == sizeof(long long))
      return ika_integer_from_ullong(pcb, S.st_size);
    else if (sizeof(off_t) == sizeof(int))
      return ika_integer_from_uint(pcb, S.st_size);
    else {
      ik_abort("unexpected off_t size %d", sizeof(off_t));
      return void_object;
    }
  } else
    return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** File type predicates.
 ** ----------------------------------------------------------------- */

static ikptr
file_is_p (ikptr s_pathname, ikptr s_follow_symlinks, int flag)
{
  char *	pathname;
  struct stat	S;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = (false_object == s_follow_symlinks)? lstat(pathname, &S) : stat(pathname, &S);
  if (0 == rv)
    /* NOTE It is not enough to do "S.st_mode & flag", we really have to
       do "flag == (S.st_mode & flag)". */
    return (flag == (S.st_mode & flag))? true_object : false_object;
  else
    return ik_errno_to_code();
}

#define FILE_IS_P(WHO,FLAG)					\
  ikptr WHO (ikptr s_pathname, ikptr s_follow_symlinks)		\
  { return file_is_p(s_pathname, s_follow_symlinks, FLAG); }

FILE_IS_P(ikrt_file_is_directory,	S_IFDIR)
FILE_IS_P(ikrt_file_is_char_device,	S_IFCHR)
FILE_IS_P(ikrt_file_is_block_device,	S_IFBLK)
FILE_IS_P(ikrt_file_is_regular_file,	S_IFREG)
FILE_IS_P(ikrt_file_is_symbolic_link,	S_IFLNK)
FILE_IS_P(ikrt_file_is_socket,		S_IFSOCK)
FILE_IS_P(ikrt_file_is_fifo,		S_IFIFO)

#define SPECIAL_FILE_IS_P(WHO,PRED)					\
   ikptr								\
   WHO (ikptr s_pathname, ikptr s_follow_symlinks)			\
   {									\
     char *	   pathname;						\
     struct stat   S;							\
     int	   rv;							\
     pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);			\
     errno    = 0;							\
     rv = (false_object == s_follow_symlinks)? lstat(pathname, &S) : stat(pathname, &S);	\
     if (0 == rv) {							\
       return (PRED(&S))? true_object : false_object;			\
     } else {								\
       return ik_errno_to_code();					\
     }									\
   }

SPECIAL_FILE_IS_P(ikrt_file_is_message_queue,S_TYPEISMQ)
SPECIAL_FILE_IS_P(ikrt_file_is_semaphore,S_TYPEISSEM)
SPECIAL_FILE_IS_P(ikrt_file_is_shared_memory,S_TYPEISSHM)


/** --------------------------------------------------------------------
 ** Testing file access.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_access (ikptr s_pathname, ikptr how)
{
  char *	pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  int		rv;
  errno = 0;
  rv	= access(pathname, IK_UNFIX(how));
  if (0 == rv)
    return true_object;
  else if ((errno == EACCES) ||
	   (errno == EROFS)  ||
	   (errno == ETXTBSY))
    return false_object;
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_file_exists (ikptr s_pathname)
{
  char *	pathname;
  struct stat	S;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = stat(pathname, &S);
  if (0 == rv)
    return true_object;
  else if ((ENOENT == errno) || (ENOTDIR == errno))
    return false_object;
  else
    return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** File times.
 ** ----------------------------------------------------------------- */

static ikptr
timespec_vector (struct timespec * T, ikptr s_vector, ikpcb* pcb)
{
  pcb->root9 = &s_vector;
  {
    IK_ASS(IK_ITEM(s_vector, 0), ika_integer_from_long(pcb, (long)(T->tv_sec )));
    IK_ASS(IK_ITEM(s_vector, 1), ika_integer_from_long(pcb, (long)(T->tv_nsec)));
  }
  pcb->root9 = NULL;
  return IK_FIX(0);
}
ikptr
ikrt_posix_file_ctime (ikptr s_pathname, ikptr s_vector, ikpcb* pcb)
{
  char*		pathname;
  struct stat	S;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_CTIMESPEC
    return timespec_vector(&S.st_ctimespec, s_vector, pcb);
#elif HAVE_STAT_ST_CTIM
    return timespec_vector(&S.st_ctim, s_vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_ctime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, s_vector, pcb);
#endif
  } else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_file_mtime (ikptr s_pathname, ikptr s_vector, ikpcb* pcb)
{
  char*		pathname;
  struct stat	S;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_MTIMESPEC
    return timespec_vector(&S.st_mtimespec, s_vector, pcb);
#elif HAVE_STAT_ST_MTIM
    return timespec_vector(&S.st_mtim, s_vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_mtime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, s_vector, pcb);
#endif
  } else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_file_atime (ikptr s_pathname, ikptr s_vector, ikpcb* pcb)
{
  char*		pathname;
  struct stat	S;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_ATIMESPEC
    return timespec_vector(&S.st_atimespec, s_vector, pcb);
#elif HAVE_STAT_ST_ATIM
    return timespec_vector(&S.st_atim, s_vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_atime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, s_vector, pcb);
#endif
  } else
    return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Setting onwers, permissions, times.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_chown (ikptr s_pathname, ikptr s_owner, ikptr s_group)
{
  char *  pathname;
  int	  rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = chown(pathname, IK_NUM_TO_UID(s_owner), IK_NUM_TO_GID(s_group));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_fchown (ikptr s_fd, ikptr s_owner, ikptr s_group)
{
  int	  rv;
  errno	   = 0;
  rv	   = fchown(IK_NUM_TO_FD(s_fd), IK_NUM_TO_UID(s_owner), IK_NUM_TO_GID(s_group));
  return (0 == rv)? fix(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_chmod (ikptr s_pathname, ikptr s_mode)
{
  char *	pathname;
  int		rv;
  mode_t	mode;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  mode	   = IK_NUM_TO_FILEMODE(s_mode);
  errno	   = 0;
  rv	   = chmod(pathname, mode);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_fchmod (ikptr s_fd, ikptr s_mode)
{
  int		rv;
  mode_t	mode;
  mode	   = IK_NUM_TO_FILEMODE(s_mode);
  errno	   = 0;
  rv	   = fchmod(IK_NUM_TO_FD(s_fd), mode);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_umask (ikptr s_mask)
{
  mode_t  mask = IK_NUM_TO_FILEMODE(s_mask);
  mode_t  rv   = umask(mask);
  return IK_FILEMODE_TO_NUM(rv);
}
ikptr
ikrt_posix_getumask (void)
{
  mode_t  mask = umask(0);
  umask(mask);
  return IK_FILEMODE_TO_NUM(mask);
}
ikptr
ikrt_posix_utime (ikptr s_pathname, ikptr s_atime_sec, ikptr s_mtime_sec)
{
  char *	  pathname;
  struct utimbuf  T;
  int		  rv;
  pathname  = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  T.actime  = (time_t)ik_integer_to_long(s_atime_sec);
  T.modtime = (time_t)ik_integer_to_long(s_mtime_sec);
  errno	    = 0;
  rv	    = utime(pathname, &T);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_utimes (ikptr s_pathname,
		   ikptr s_atime_sec, ikptr s_atime_usec,
		   ikptr s_mtime_sec, ikptr s_mtime_usec)
{
  char *	  pathname;
  struct timeval  T[2];
  int		  rv;
  pathname     = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  T[0].tv_sec  = (long)ik_integer_to_long(s_atime_sec);
  T[0].tv_usec = (long)ik_integer_to_long(s_atime_usec);
  T[1].tv_sec  = (long)ik_integer_to_long(s_mtime_sec);
  T[1].tv_usec = (long)ik_integer_to_long(s_mtime_usec);
  errno	       = 0;
  rv	       = utimes(pathname, T);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_lutimes (ikptr s_pathname,
		    ikptr s_atime_sec, ikptr s_atime_usec,
		    ikptr s_mtime_sec, ikptr s_mtime_usec)
{
  char *	  pathname;
  struct timeval  T[2];
  int		  rv;
  pathname     = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  T[0].tv_sec  = (long)ik_integer_to_long(s_atime_sec);
  T[0].tv_usec = (long)ik_integer_to_long(s_atime_usec);
  T[1].tv_sec  = (long)ik_integer_to_long(s_mtime_sec);
  T[1].tv_usec = (long)ik_integer_to_long(s_mtime_usec);
  errno	       = 0;
  rv	       = lutimes(pathname, T);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_futimes (ikptr s_fd,
		    ikptr s_atime_sec, ikptr s_atime_usec,
		    ikptr s_mtime_sec, ikptr s_mtime_usec)
{
  struct timeval  T[2];
  int		  rv;
  T[0].tv_sec  = (long)ik_integer_to_long(s_atime_sec);
  T[0].tv_usec = (long)ik_integer_to_long(s_atime_usec);
  T[1].tv_sec  = (long)ik_integer_to_long(s_mtime_sec);
  T[1].tv_usec = (long)ik_integer_to_long(s_mtime_usec);
  errno	       = 0;
  rv	       = futimes(IK_NUM_TO_FD(s_fd), T);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Hard and symbolic links.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_link (ikptr s_old_pathname, ikptr s_new_pathname)
{
  char *	old_pathname = IK_BYTEVECTOR_DATA_CHARP(s_old_pathname);
  char *	new_pathname = IK_BYTEVECTOR_DATA_CHARP(s_new_pathname);
  int		rv;
  errno = 0;
  rv	= link(old_pathname, new_pathname);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_symlink (ikptr s_true_pathname, ikptr s_link_pathname)
{
  char *	true_pathname = IK_BYTEVECTOR_DATA_CHARP(s_true_pathname);
  char *	link_pathname = IK_BYTEVECTOR_DATA_CHARP(s_link_pathname);
  int		rv;
  errno = 0;
  rv	= symlink(true_pathname, link_pathname);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_readlink (ikptr s_link_pathname, ikpcb * pcb)
{
  char *	link_pathname = IK_BYTEVECTOR_DATA_CHARP(s_link_pathname);
  size_t	max_len;
  int		rv;
  for (max_len=PATH_MAX;; max_len *= 2) {
    char	true_pathname[max_len];
    errno = 0;
    rv	  = readlink(link_pathname, true_pathname, max_len);
    if (rv < 0)
      return ik_errno_to_code();
    else if (rv == max_len)
      continue;
    else
      return ika_bytevector_from_cstring_len(pcb, true_pathname, rv);
  }
}
ikptr
ikrt_posix_realpath (ikptr s_link_pathname, ikpcb* pcb)
{
  char *	link_pathname;
  char *	true_pathname;
  char		buff[PATH_MAX];
  link_pathname = IK_BYTEVECTOR_DATA_CHARP(s_link_pathname);
  errno		= 0;
  true_pathname = realpath(link_pathname, buff);
  return (true_pathname)? ika_bytevector_from_cstring(pcb, true_pathname) : ik_errno_to_code();
}
ikptr
ikrt_posix_unlink (ikptr s_pathname)
{
  char * pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  int	 rv;
  errno = 0;
  rv	= unlink(pathname);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_remove (ikptr s_pathname)
{
  char * pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  int	 rv;
  errno = 0;
  rv	= remove(pathname);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_rename (ikptr s_old_pathname, ikptr s_new_pathname)
{
  char *	old_pathname = IK_BYTEVECTOR_DATA_CHARP(s_old_pathname);
  char *	new_pathname = IK_BYTEVECTOR_DATA_CHARP(s_new_pathname);
  int		rv;
  errno = 0;
  rv	= rename(old_pathname, new_pathname);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** File system directories.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_mkdir (ikptr s_pathname, ikptr s_mode)
{
  char *	pathname;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = mkdir(pathname, IK_NUM_TO_FILEMODE(s_mode));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_rmdir (ikptr s_pathname)
{
  char *	pathname;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = rmdir(pathname);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_getcwd (ikpcb * pcb)
{
  size_t	max_len;
  char *	pathname;
  for (max_len=256;; max_len*=2) {
    char	buffer[max_len];
    errno    = 0;
    pathname = getcwd(buffer, max_len);
    if (NULL == pathname) {
      if (ERANGE == errno)
	continue;
      else
	return ik_errno_to_code();
    } else
      return ika_bytevector_from_cstring(pcb, pathname);
  }
}
ikptr
ikrt_posix_chdir (ikptr s_pathname)
{
  char *	pathname;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = chdir(pathname);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_fchdir (ikptr s_fd)
{
  int	rv;
  errno	   = 0;
  rv	   = fchdir(IK_NUM_TO_FD(s_fd));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_opendir (ikptr s_pathname, ikpcb * pcb)
{
  char *	pathname;
  DIR *		stream;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  stream   = opendir(pathname);
  return (stream)? ika_pointer_alloc(pcb, (ik_ulong)stream) : ik_errno_to_code();
}
ikptr
ikrt_posix_fdopendir (ikptr s_fd, ikpcb * pcb)
{
  DIR *		stream;
  errno	   = 0;
  stream   = fdopendir(IK_NUM_TO_FD(s_fd));
  return (stream)? ika_pointer_alloc(pcb, (ik_ulong)stream) : ik_errno_to_code();
}
ikptr
ikrt_posix_readdir (ikptr s_pointer, ikpcb * pcb)
{
  DIR *		  stream = (DIR *) IK_POINTER_DATA_VOIDP(s_pointer);
  struct dirent * entry;
  errno = 0;
  entry = readdir(stream);
  if (NULL == entry) {
    closedir(stream);
    if (0 == errno)
      return false_object;
    else
      return ik_errno_to_code();
  } else
    /* The  only field	in  "struct dirent"  we	 can trust  to exist  is
       "d_name".   Notice that	the documentation  of glibc  describes a
       "d_namlen"  field  which	 does	not  exist  on	Linux,	see  the
       "readdir(3)" manual page. */
    return ika_bytevector_from_cstring(pcb, entry->d_name);
}
ikptr
ikrt_posix_closedir (ikptr pointer, ikpcb * pcb)
{
  DIR *	 stream = (DIR *) IK_POINTER_DATA_VOIDP(pointer);
  int	 rv;
  errno = 0;
  rv	= closedir(stream);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_rewinddir (ikptr s_pointer)
{
  DIR *	 stream = (DIR *) IK_POINTER_DATA_VOIDP(s_pointer);
  rewinddir(stream);
  return void_object;
}
ikptr
ikrt_posix_telldir (ikptr s_pointer, ikpcb * pcb)
{
  DIR *	 stream = (DIR *) IK_POINTER_DATA_VOIDP(s_pointer);
  long	 pos;
  pos = telldir(stream);
  return ika_integer_from_long(pcb, pos);
}
ikptr
ikrt_posix_seekdir (ikptr s_pointer, ikptr s_pos)
{
  DIR *	 stream = (DIR *) IK_POINTER_DATA_VOIDP(s_pointer);
  long	 pos	= ik_integer_to_long(s_pos);
  seekdir(stream, pos);
  return void_object;
}


/** --------------------------------------------------------------------
 ** File descriptors.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_open (ikptr s_pathname, ikptr s_flags, ikptr s_mode)
{
  char *	pathname;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = open(pathname, IK_UNFIX(s_flags), IK_NUM_TO_FILEMODE(s_mode));
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_close (ikptr s_fd)
{
  int		rv;
  errno	   = 0;
  rv	   = close(IK_NUM_TO_FD(s_fd));
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_read (ikptr s_fd, ikptr s_buffer, ikptr s_size)
{
  void *	buffer;
  size_t	size;
  ssize_t	rv;
  buffer   = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	   = (size_t)((false_object!=s_size)? IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  errno	   = 0;
  rv	   = read(IK_NUM_TO_FD(s_fd), buffer, size);
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_pread (ikptr s_fd, ikptr s_buffer, ikptr s_size, ikptr s_off)
{
  void *	buffer;
  size_t	size;
  off_t		off;
  ssize_t	rv;
  buffer   = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	   = (size_t)((false_object!=s_size)?
		      IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  off	   = (off_t) ik_integer_to_llong(s_off);
  errno	   = 0;
  rv	   = pread(IK_NUM_TO_FD(s_fd), buffer, size, off);
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_write (ikptr s_fd, ikptr s_buffer, ikptr s_size)
{
  void *	buffer;
  size_t	size;
  ssize_t	rv;
  buffer   = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	   = (size_t)((false_object!=s_size)?
		      IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  errno	   = 0;
  rv	   = write(IK_NUM_TO_FD(s_fd), buffer, size);
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_pwrite (ikptr s_fd, ikptr s_buffer, ikptr s_size, ikptr off_num)
{
  void *	buffer;
  size_t	size;
  off_t		off;
  ssize_t	rv;
  buffer   = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	   = (size_t)((false_object!=s_size)?
		      IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  off	   = (off_t) ik_integer_to_llong(off_num);
  errno	   = 0;
  rv	   = pwrite(IK_NUM_TO_FD(s_fd), buffer, size, off);
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_lseek (ikptr s_fd, ikptr s_off, ikptr s_whence, ikpcb * pcb)
{
  off_t		off;
  off_t		rv;
  off	 = ik_integer_to_llong(s_off);
  errno	 = 0;
  rv	 = lseek(IK_NUM_TO_FD(s_fd), off, IK_UNFIX(s_whence));
  return (0 <= rv)? ika_integer_from_llong(pcb, (long long)rv) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_readv (ikptr s_fd, ikptr s_buffers, ikpcb * pcb)
{
  int		number_of_buffers = ik_list_length(s_buffers);
  struct iovec	bufs[number_of_buffers];
  ikptr		bv;
  int		i;
  ssize_t	rv;
  for (i=0; pair_tag == IK_TAGOF(s_buffers); s_buffers=IK_CDR(s_buffers), ++i) {
    bv	    = IK_REF(s_buffers, off_car);
    bufs[i].iov_base = IK_BYTEVECTOR_DATA_VOIDP(bv);
    bufs[i].iov_len  = IK_BYTEVECTOR_LENGTH(bv);
  }
  errno	   = 0;
  rv	   = readv(IK_NUM_TO_FD(s_fd), bufs, number_of_buffers);
  return (0 <= rv)? ika_integer_from_llong(pcb, (long long)rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_writev (ikptr s_fd, ikptr s_buffers, ikpcb * pcb)
{
  int		number_of_buffers = ik_list_length(s_buffers);
  struct iovec	bufs[number_of_buffers];
  ikptr		bv;
  int		i;
  ssize_t	rv;
  for (i=0; pair_tag == IK_TAGOF(s_buffers); s_buffers=IK_CDR(s_buffers), ++i) {
    bv	    = IK_REF(s_buffers, off_car);
    bufs[i].iov_base = IK_BYTEVECTOR_DATA_VOIDP(bv);
    bufs[i].iov_len  = IK_BYTEVECTOR_LENGTH(bv);
  }
  errno	   = 0;
  rv	   = writev(IK_NUM_TO_FD(s_fd), bufs, number_of_buffers);
  return (0 <= rv)? ika_integer_from_llong(pcb, (long long)rv) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_select (ikptr nfds_fx,
		   ikptr read_fds_ell, ikptr write_fds_ell, ikptr except_fds_ell,
		   ikptr sec, ikptr usec,
		   ikpcb * pcb)
{
  ikptr			L;	/* iterator for input lists */
  ikptr			R;	/* output list of read-ready fds */
  ikptr			W;	/* output list of write-ready fds */
  ikptr			E;	/* output list of except-ready fds */
  ikptr			vec;	/* the vector to be returned to the caller */
  fd_set		read_fds;
  fd_set		write_fds;
  fd_set		except_fds;
  struct timeval	timeout;
  int			fd, nfds=0;
  int			rv;
  /* Fill the fdset for read-ready descriptors. */
  FD_ZERO(&read_fds);
  for (L=read_fds_ell; pair_tag == IK_TAGOF(L); L = IK_REF(L, off_cdr)) {
    fd = IK_UNFIX(IK_REF(L, off_car));
    if (nfds < fd)
      nfds = fd;
    FD_SET(fd, &read_fds);
  }
  /* Fill the fdset for write-ready descriptors. */
  FD_ZERO(&write_fds);
  for (L=write_fds_ell; pair_tag == IK_TAGOF(L); L = IK_REF(L, off_cdr)) {
    fd = IK_UNFIX(IK_REF(L, off_car));
    if (nfds < fd)
      nfds = fd;
    FD_SET(fd, &write_fds);
  }
  /* Fill the fdset for except-ready descriptors. */
  FD_ZERO(&except_fds);
  for (L=except_fds_ell; pair_tag == IK_TAGOF(L); L = IK_REF(L, off_cdr)) {
    fd = IK_UNFIX(IK_REF(L, off_car));
    if (nfds < fd)
      nfds = fd;
    FD_SET(fd, &except_fds);
  }
  /* Perform the selection. */
  if (false_object == nfds_fx)
    ++nfds;
  else
    nfds = IK_UNFIX(nfds_fx);
  timeout.tv_sec  = IK_UNFIX(sec);
  timeout.tv_usec = IK_UNFIX(usec);
  errno = 0;
  rv	= select(nfds, &read_fds, &write_fds, &except_fds, &timeout);
  if (0 == rv) { /* timeout has expired */
    return fix(0);
  } else if (-1 == rv) { /* an error occurred */
    return ik_errno_to_code();
  } else { /* success, let's harvest the fds */
    /* Build the vector	 to be returned and prevent  it from being garbage
       collected while building other objects. */
    vec = ik_safe_alloc(pcb, IK_ALIGN(disp_vector_data+3*wordsize)) | vector_tag;
    IK_REF(vec, off_vector_length) = fix(3);
    IK_REF(vec, off_vector_data+0*wordsize) = null_object;
    IK_REF(vec, off_vector_data+1*wordsize) = null_object;
    IK_REF(vec, off_vector_data+2*wordsize) = null_object;
    pcb->root0 = &vec;
    {
      /* Build a list of read-ready file descriptors. */
      for (L=read_fds_ell, R=null_object; pair_tag == IK_TAGOF(L); L=IK_REF(L,off_cdr)) {
	ikptr fdx = IK_REF(L, off_car);
	if (FD_ISSET(IK_UNFIX(fdx), &read_fds)) {
	  ikptr P = ika_pair_alloc(pcb);
	  IK_CAR(P) = fdx;
	  IK_CDR(P) = R;
	  IK_ITEM(vec, 0) = P;
	  R = P;
	}
      }
      /* Build a list of write-ready file descriptors. */
      for (L=write_fds_ell, W=null_object; pair_tag == IK_TAGOF(L); L = IK_REF(L, off_cdr)) {
	ikptr fdx = IK_REF(L, off_car);
	if (FD_ISSET(IK_UNFIX(fdx), &write_fds)) {
	  ikptr P = ika_pair_alloc(pcb);
	  IK_CAR(P) = fdx;
	  IK_CDR(P) = W;
	  IK_ITEM(vec, 1) = W = P;
	}
      }
      /* Build a list of except-ready file descriptors. */
      for (L=except_fds_ell, E=null_object; pair_tag == IK_TAGOF(L); L = IK_REF(L, off_cdr)) {
	ikptr fdx = IK_REF(L, off_car);
	if (FD_ISSET(IK_UNFIX(fdx), &except_fds)) {
	  ikptr P = ika_pair_alloc(pcb);
	  IK_CAR(P) = fdx;
	  IK_CDR(P) = E;
	  IK_ITEM(vec, 2) = E = P;
	}
      }
    }
    pcb->root0 = NULL;
    return vec;
  }
}
ikptr
ikrt_posix_select_fd (ikptr fdx, ikptr sec, ikptr usec, ikpcb * pcb)
{
  ikptr			vec;	/* the vector to be returned to the caller */
  fd_set		read_fds;
  fd_set		write_fds;
  fd_set		except_fds;
  struct timeval	timeout;
  int			fd;
  int			rv;
  FD_ZERO(&read_fds);
  FD_ZERO(&write_fds);
  FD_ZERO(&except_fds);
  fd = IK_UNFIX(fdx);
  FD_SET(fd, &read_fds);
  FD_SET(fd, &write_fds);
  FD_SET(fd, &except_fds);
  timeout.tv_sec  = IK_UNFIX(sec);
  timeout.tv_usec = IK_UNFIX(usec);
  errno = 0;
  rv	= select(1+fd, &read_fds, &write_fds, &except_fds, &timeout);
  if (0 == rv) { /* timeout has expired */
    return IK_FIX(0);
  } else if (-1 == rv) { /* an error occurred */
    return ik_errno_to_code();
  } else { /* success, let's harvest the events */
    vec = ika_vector_alloc_no_init(pcb, 3);
    IK_ITEM(vec, 0) = (FD_ISSET(fd, &read_fds))?   fdx : false_object;
    IK_ITEM(vec, 1) = (FD_ISSET(fd, &write_fds))?  fdx : false_object;
    IK_ITEM(vec, 2) = (FD_ISSET(fd, &except_fds))? fdx : false_object;
    return vec;
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_fcntl (ikptr fd, ikptr command, ikptr arg)
{
  int		rv = -1;
  errno = 0;
  if (IK_IS_FIXNUM(arg)) {
    long	val = (long)IK_UNFIX(arg);
    rv = fcntl(IK_UNFIX(fd), IK_UNFIX(command), val);
  } else if (false_object == arg) {
    rv = fcntl(IK_UNFIX(fd), IK_UNFIX(command));
  } else if (IK_IS_BYTEVECTOR(arg)) {
    void *	val = IK_BYTEVECTOR_DATA_VOIDP(arg);
    rv = fcntl(IK_UNFIX(fd), IK_UNFIX(command), val);
  } else if (ikrt_is_pointer(arg)) {
    void *	val = IK_POINTER_DATA_VOIDP(arg);
    rv = fcntl(IK_UNFIX(fd), IK_UNFIX(command), val);
  } else
    ik_abort("invalid last argument to fcntl()");
  return (-1 != rv)? fix(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_ioctl (ikptr fd, ikptr command, ikptr arg)
{
  int		rv = -1;
  errno = 0;
  if (IK_IS_FIXNUM(arg)) {
    long	val = (long)IK_UNFIX(arg);
    rv = ioctl(IK_UNFIX(fd), IK_UNFIX(command), val);
  } else if (false_object == arg) {
    rv = ioctl(IK_UNFIX(fd), IK_UNFIX(command));
  } else if (IK_IS_BYTEVECTOR(arg)) {
    void *	val = IK_BYTEVECTOR_DATA_VOIDP(arg);
    rv = ioctl(IK_UNFIX(fd), IK_UNFIX(command), val);
  } else if (ikrt_is_pointer(arg)) {
    void *	val = IK_POINTER_DATA_VOIDP(arg);
    rv = ioctl(IK_UNFIX(fd), IK_UNFIX(command), val);
  } else
    ik_abort("invalid last argument to ioctl()");
  return (-1 != rv)? fix(rv) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_dup (ikptr fd)
{
  int		rv;
  errno = 0;
  rv	= dup(IK_UNFIX(fd));
  return (-1 != rv)? fix(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_dup2 (ikptr old, ikptr new)
{
  int		rv;
  errno = 0;
  rv	= dup2(IK_UNFIX(old), IK_UNFIX(new));
  return (-1 != rv)? fix(0) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_pipe (ikpcb * pcb)
{
  int		rv;
  int		fds[2];
  errno = 0;
  rv	= pipe(fds);
  if (-1 == rv)
    return ik_errno_to_code();
  else {
    ikptr  pair = ika_pair_alloc(pcb);
    IK_CAR(pair) = IK_FIX(fds[0]);
    IK_CDR(pair) = IK_FIX(fds[1]);
    return pair;
  }
}
ikptr
ikrt_posix_mkfifo (ikptr s_pathname, ikptr mode)
{
  char *	pathname;
  int		rv;
  pathname = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  errno	   = 0;
  rv	   = mkfifo(pathname, IK_UNFIX(mode));
  return (0 <= rv)? fix(rv) : ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Network sockets: local namespace.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_make_sockaddr_un (ikptr s_pathname, ikpcb * pcb)
{
#undef SIZE
#define SIZE	(sizeof(struct sockaddr_un)+pathname_len) /* better safe than sorry */
  char *	pathname     = IK_BYTEVECTOR_DATA_CHARP(s_pathname);
  long		pathname_len = IK_BYTEVECTOR_LENGTH(s_pathname);
  uint8_t	bytes[SIZE];
  struct sockaddr_un *	name = (void *)bytes;
  name->sun_family = AF_LOCAL;
  memcpy(name->sun_path, pathname, pathname_len+1);
  return ika_bytevector_from_memory_block(pcb, name, SUN_LEN(name));
}
ikptr
ikrt_posix_sockaddr_un_pathname (ikptr s_addr, ikpcb * pcb)
{
  struct sockaddr_un *	addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr);
  if (AF_LOCAL == addr->sun_family) {
    ikptr	s_bv;
    long	len = strlen(addr->sun_path);
    void *	buf;
    pcb->root0 = &s_addr;
    {
      /* WARNING  We  do  not use  "ika_bytevector_from_cstring()"  here
	 because the C	string is embedded in the  bytevector S_ADDR and
	 we cannot preserve it by calling that function. */
      s_bv = ika_bytevector_alloc(pcb, len);
      buf  = IK_BYTEVECTOR_DATA_VOIDP(s_bv);
      addr = IK_BYTEVECTOR_DATA_VOIDP(s_addr);
      memcpy(buf, addr->sun_path, len);
    }
    pcb->root0 = NULL;
    return s_bv;
  } else
    return false_object;
}


/** --------------------------------------------------------------------
 ** Network sockets: IPv4 namespace.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_make_sockaddr_in (ikptr s_host_address, ikptr s_port, ikpcb * pcb)
{
#undef BV_LEN
#define BV_LEN	sizeof(struct sockaddr_in)
  ikptr			s_socket_address;
  pcb->root0 = &s_host_address;
  pcb->root1 = &s_port;
  {
    struct in_addr *		host_address;
    struct sockaddr_in *	socket_address;
    s_socket_address	       = ika_bytevector_alloc(pcb, BV_LEN);
    socket_address	       = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
    socket_address->sin_family = AF_INET;
    socket_address->sin_port   = (unsigned short int)IK_UNFIX(s_port);
    host_address	       = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
    memcpy(&(socket_address->sin_addr), host_address, sizeof(struct in_addr));
  }
  pcb->root1 = NULL;
  pcb->root0 = NULL;
  return s_socket_address;
}
ikptr
ikrt_posix_sockaddr_in_in_addr (ikptr s_socket_address, ikpcb * pcb)
{
  struct sockaddr_in *	socket_address = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
  if (AF_INET == socket_address->sin_family) {
#undef BV_LEN
#define BV_LEN	sizeof(struct in_addr)
    ikptr		s_host_address;
    pcb->root0 = &s_socket_address;
    {
      struct in_addr *	host_address;
      s_host_address = ika_bytevector_alloc(pcb, BV_LEN);
      host_address   = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
      socket_address = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
      memcpy(host_address, &(socket_address->sin_addr), BV_LEN);
    }
    pcb->root0 = NULL;
    return s_host_address;
  } else
    return false_object;
}
ikptr
ikrt_posix_sockaddr_in_in_port (ikptr s_socket_address)
{
  struct sockaddr_in *	socket_address = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
  return (AF_INET == socket_address->sin_family)?
    IK_FIX((long)socket_address->sin_port) : false_object;
}


/** --------------------------------------------------------------------
 ** Network sockets: IPv6 namespace.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_make_sockaddr_in6 (ikptr s_host_address, ikptr s_port, ikpcb * pcb)
{
#undef BV_LEN
#define BV_LEN	sizeof(struct sockaddr_in6)
  struct in6_addr *	host_address;
  struct sockaddr_in6 * socket_address;
  ikptr			s_socket_address;
  pcb->root0 = &s_host_address;
  pcb->root1 = &s_port;
  {
    s_socket_address	        = ika_bytevector_alloc(pcb, BV_LEN);
    socket_address	        = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
    socket_address->sin6_family = AF_INET6;
    socket_address->sin6_port   = (unsigned short int)IK_UNFIX(s_port);
    host_address                = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
    memcpy(&(socket_address->sin6_addr), host_address, sizeof(struct in6_addr));
  }
  pcb->root1 = NULL;
  pcb->root0 = NULL;
  return s_socket_address;
}
ikptr
ikrt_posix_sockaddr_in6_in6_addr (ikptr s_socket_address, ikpcb * pcb)
{
  struct sockaddr_in6 *	 socket_address = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
  if (AF_INET6 == socket_address->sin6_family) {
#undef BV_LEN
#define BV_LEN	sizeof(struct in6_addr)
    ikptr		s_host_address;
    struct in6_addr *	host_address;
    pcb->root0 = &s_socket_address;
    {
      s_host_address = ika_bytevector_alloc(pcb, BV_LEN);
      host_address   = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
      socket_address = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
      memcpy(host_address, &(socket_address->sin6_addr), BV_LEN);
    }
    pcb->root0 = NULL;
    return s_host_address;
  } else
    return false_object;
}
ikptr
ikrt_posix_sockaddr_in6_in6_port (ikptr s_socket_address)
{
  struct sockaddr_in6 *	 socket_address = IK_BYTEVECTOR_DATA_VOIDP(s_socket_address);
  return (AF_INET6 == socket_address->sin6_family)?
    IK_FIX((long)socket_address->sin6_port) : false_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_in6addr_loopback (ikpcb * pcb)
{
  static const struct in6_addr constant_host_address = IN6ADDR_LOOPBACK_INIT;
  ikptr			s_host_address;
  struct in6_addr *	host_address;
#undef BV_LEN
#define BV_LEN		sizeof(struct in6_addr)
  s_host_address = ika_bytevector_alloc(pcb, BV_LEN);
  host_address	 = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
  memcpy(host_address, &constant_host_address, BV_LEN);
  return s_host_address;
}
ikptr
ikrt_posix_in6addr_any (ikpcb * pcb)
{
  static const struct in6_addr constant_host_address = IN6ADDR_ANY_INIT;
  ikptr			s_host_address;
  struct in6_addr *	host_address;
#undef BV_LEN
#define BV_LEN		sizeof(struct in6_addr)
  s_host_address = ika_bytevector_alloc(pcb, BV_LEN);
  host_address	 = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
  memcpy(host_address, &constant_host_address, BV_LEN);
  return s_host_address;
}


/** --------------------------------------------------------------------
 ** Network address conversion functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_inet_aton (ikptr s_dotted_quad, ikpcb * pcb)
{
  void *		dotted_quad;
  ikptr			s_host_address;
  struct in_addr *	host_address;
  int			rv;
#undef BV_LEN
#define BV_LEN		sizeof(struct in_addr)
  pcb->root0 = &s_dotted_quad;
  {
    s_host_address = ika_bytevector_alloc(pcb, BV_LEN);
    host_address   = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
    dotted_quad    = IK_BYTEVECTOR_DATA_VOIDP(s_dotted_quad);
    rv = inet_aton(dotted_quad, host_address);
  }
  pcb->root0 = NULL;
  return (0 != rv)? s_host_address : false_object;
}
ikptr
ikrt_posix_inet_ntoa (ikptr s_host_address, ikpcb * pcb)
{
  struct in_addr *	host_address;
  ikptr			s_dotted_quad;
  char *		dotted_quad;
  char *		data;
  long			data_len;
  host_address  = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
  data          = inet_ntoa(*host_address);
  data_len      = strlen(data);
  /* WARNING  The string referenced  by DATA  is a  statically allocated
     buffer, so  we do  not need to  register S_HOST_ADDRESS  as garbage
     collection root while doing the following allocation. */
  s_dotted_quad = ika_bytevector_alloc(pcb, data_len);
  dotted_quad   = IK_BYTEVECTOR_DATA_CHARP(s_dotted_quad);
  memcpy(dotted_quad, data, data_len);
  pcb->root0 = NULL;
  return s_dotted_quad;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_inet_pton (ikptr s_af, ikptr s_presentation, ikpcb * pcb)
{
  void *	presentation;
  ikptr		s_host_address;
  int		rv;
  switch (IK_UNFIX(s_af)) {
  case AF_INET:
    {
#undef BV_LEN
#define BV_LEN		sizeof(struct in_addr)
      struct in_addr *	    host_address;
      pcb->root0 = &s_presentation;
      {
	s_host_address = ika_bytevector_alloc(pcb, BV_LEN);
	host_address   = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
	presentation   = IK_BYTEVECTOR_DATA_VOIDP(s_presentation);
	rv = inet_pton(IK_UNFIX(s_af), presentation, host_address);
      }
      pcb->root0 = NULL;
      return (0 < rv)? s_host_address : false_object;
    }
  case AF_INET6:
    {
#undef BV_LEN
#define BV_LEN		sizeof(struct in6_addr)
      struct in6_addr *	     host_address;
      pcb->root0 = &s_presentation;
      {
	s_host_address = ika_bytevector_alloc(pcb, BV_LEN);
	host_address   = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
	presentation   = IK_BYTEVECTOR_DATA_VOIDP(s_presentation);
	rv = inet_pton(IK_UNFIX(s_af), presentation, host_address);
      }
      pcb->root0 = NULL;
      return (0 < rv)? s_host_address : false_object;
    }
  default:
    return false_object;
  }
}
ikptr
ikrt_posix_inet_ntop (ikptr s_af, ikptr s_host_address, ikpcb * pcb)
{
  switch (IK_UNFIX(s_af)) {
  case AF_INET:
  case AF_INET6:
    {
      void *		host_address;
      socklen_t		buflen = 256;
      char		buffer[buflen];
      const char *	rv;
      host_address = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
      rv = inet_ntop(IK_UNFIX(s_af), host_address, buffer, buflen);
      if (NULL != rv) {
	ikptr	  s_presentation;
	char *	  presentation;
	long	  presentation_len;
	presentation_len = strlen(buffer);
	s_presentation	 = ika_bytevector_alloc(pcb, presentation_len);
	presentation	 = IK_BYTEVECTOR_DATA_CHARP(s_presentation);
	memcpy(presentation, buffer, presentation_len);
	return s_presentation;
      } else
	return false_object;
    }
  default:
    return false_object;
  }
}


/** --------------------------------------------------------------------
 ** Host names database.
 ** ----------------------------------------------------------------- */

static ikptr
hostent_to_struct (ikptr s_rtd, struct hostent * src, ikpcb * pcb)
/* Convert  a  C  language  "struct  hostent"  into  a  Scheme  language
   "struct-hostent".  Makes use of "pcb->root6,7,8". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root8 = &s_dst;
  { /* store the official host name */
    IK_ASS(IK_FIELD(s_dst, 0), ika_bytevector_from_cstring(pcb, src->h_name));
  }
  { /* store the list of aliases */
    if (src->h_aliases[0]) {
      ikptr	s_list_of_aliases, s_spine;
      s_list_of_aliases = s_spine = ika_pair_alloc(pcb);
      pcb->root6 = &s_list_of_aliases;
      pcb->root7 = &s_spine;
      {
	int	i;
	for (i=0; src->h_aliases[i];) {
	  IK_ASS(IK_CAR(s_spine), ika_bytevector_from_cstring(pcb, src->h_aliases[i]));
	  if (src->h_aliases[++i]) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = null_object;
	    break;
	  }
	}
      }
      pcb->root7 = NULL;
      pcb->root6 = NULL;
      IK_FIELD(s_dst, 1) = s_list_of_aliases;
    } else
      IK_FIELD(s_dst, 1) = null_object;
  }
  { /* store the host address type */
    IK_FIELD(s_dst, 2) = IK_FIX(src->h_addrtype);
  }
  { /* store the host address structure length */
    IK_FIELD(s_dst, 3) = IK_FIX(src->h_length);
  }
  { /* store the reversed list of addresses */
    if (src->h_addr_list[0]) {
      ikptr	s_list_of_addrs, s_spine;
      s_list_of_addrs = s_spine = ika_pair_alloc(pcb);
      pcb->root6 = &s_list_of_addrs;
      pcb->root7 = &s_spine;
      {
	int	i;
	for (i=0; src->h_addr_list[i];) {
	  IK_ASS(IK_CAR(s_spine),
		 ika_bytevector_from_memory_block(pcb, src->h_addr_list[i], src->h_length));
	  if (src->h_addr_list[++i]) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = null_object;
	    break;
	  }
	}
      }
      pcb->root7 = NULL;
      pcb->root6 = NULL;
      IK_FIELD(s_dst, 4) = s_list_of_addrs;
    } else
      IK_FIELD(s_dst, 4) = null_object;
  }
  {/* store the first in the list of addresses */
    IK_FIELD(s_dst, 5) = IK_CAR(IK_FIELD(s_dst, 4));
  }
  pcb->root8 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_gethostbyname (ikptr s_rtd, ikptr s_hostname, ikpcb * pcb)
{
  char *		hostname;
  struct hostent *	rv;
  hostname = IK_BYTEVECTOR_DATA_CHARP(s_hostname);
  errno	   = 0;
  h_errno  = 0;
  rv	   = gethostbyname(hostname);
  return (NULL != rv)? hostent_to_struct(s_rtd, rv, pcb) : IK_FIX(-h_errno);
}
ikptr
ikrt_posix_gethostbyname2 (ikptr s_rtd, ikptr s_hostname, ikptr s_af, ikpcb * pcb)
{
  char *		hostname;
  struct hostent *	rv;
  hostname = IK_BYTEVECTOR_DATA_CHARP(s_hostname);
  errno	   = 0;
  h_errno  = 0;
  rv	   = gethostbyname2(hostname, IK_UNFIX(s_af));
  return (NULL != rv)? hostent_to_struct(s_rtd, rv, pcb) : IK_FIX(-h_errno);
}
ikptr
ikrt_posix_gethostbyaddr (ikptr s_rtd, ikptr s_host_address, ikpcb * pcb)
{
  void *		host_address;
  size_t		host_address_len;
  int			format;
  struct hostent *	rv;
  host_address	   = IK_BYTEVECTOR_DATA_VOIDP(s_host_address);
  host_address_len = (size_t)IK_BYTEVECTOR_LENGTH(s_host_address);
  format   = (sizeof(struct in_addr) == host_address_len)? AF_INET : AF_INET6;
  errno	   = 0;
  h_errno  = 0;
  rv	   = gethostbyaddr(host_address, host_address_len, format);
  return (NULL != rv)? hostent_to_struct(s_rtd, rv, pcb) : IK_FIX(-h_errno);
}
ikptr
ikrt_posix_host_entries (ikptr s_rtd, ikpcb * pcb)
{
  struct hostent *	entry;
  ikptr			s_list_of_entries;
  sethostent(1);
  {
    entry = gethostent();
    if (entry) {
      ikptr	s_spine;
      pcb->root0 = &s_rtd;
      {
	s_spine = s_list_of_entries = ika_pair_alloc(pcb);
	pcb->root1 = &s_list_of_entries;
	pcb->root2 = &s_spine;
	{
	  while (entry) {
	    IK_ASS(IK_CAR(s_spine), hostent_to_struct(s_rtd, entry, pcb));
	    entry = gethostent();
	    if (entry) {
	      IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	      s_spine = IK_CDR(s_spine);
	    } else {
	      IK_CDR(s_spine) = null_object;
	      break;
	    }
	  }
	}
	pcb->root2 = NULL;
	pcb->root1 = NULL;
      }
      pcb->root0 = NULL;
    } else
      s_list_of_entries = null_object;
  }
  endhostent();
  return s_list_of_entries;
}


/** --------------------------------------------------------------------
 ** Network address informations.
 ** ----------------------------------------------------------------- */

static ikptr
addrinfo_to_struct (ikpcb * pcb, ikptr s_rtd, struct addrinfo * src, int with_canon_name)
/* Convert  a  C  language  "struct  addrinfo" into  a  Scheme  language
   "struct-addrinfo".  Make use of "pcb->root9". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
    IK_ASS(IK_FIELD(s_dst, 0), IK_FIX(src->ai_flags));
    IK_ASS(IK_FIELD(s_dst, 1), IK_FIX(src->ai_family));
    IK_ASS(IK_FIELD(s_dst, 2), IK_FIX(src->ai_socktype));
    IK_ASS(IK_FIELD(s_dst, 3), IK_FIX(src->ai_protocol));
    IK_ASS(IK_FIELD(s_dst, 4), IK_FIX(src->ai_addrlen));
    /* fill the field "ai_addr" */
    IK_ASS(IK_FIELD(s_dst, 5),
	   ika_bytevector_from_memory_block(pcb, src->ai_addr, src->ai_addrlen));
    /* fill the field "ai_canonname" */
    if (with_canon_name && src->ai_canonname) {
      IK_ASS(IK_FIELD(s_dst, 6),
	     ika_bytevector_from_cstring(pcb, src->ai_canonname));
    } else
      IK_FIELD(s_dst, 6) = false_object;
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_getaddrinfo (ikptr s_rtd, ikptr s_node, ikptr s_service, ikptr s_hints_struct, ikpcb * pcb)
{
  const char *		node;
  const char *		service;
  struct addrinfo	hints;
  struct addrinfo *	hints_p;
  struct addrinfo *	result;
  struct addrinfo *	iter;
  int			rv, with_canon_name;
  node	  = (false_object != s_node)?    IK_BYTEVECTOR_DATA_CHARP(s_node)    : NULL;
  service = (false_object != s_service)? IK_BYTEVECTOR_DATA_CHARP(s_service) : NULL;
  memset(&hints, '\0', sizeof(struct addrinfo));
  if (false_object == s_hints_struct) {
    hints_p		= NULL;
    with_canon_name	= 0;
  } else {
    hints_p		= &hints;
    hints.ai_flags	= IK_UNFIX(IK_FIELD(s_hints_struct, 0));
    hints.ai_family	= IK_UNFIX(IK_FIELD(s_hints_struct, 1));
    hints.ai_socktype	= IK_UNFIX(IK_FIELD(s_hints_struct, 2));
    hints.ai_protocol	= IK_UNFIX(IK_FIELD(s_hints_struct, 3));
    with_canon_name	= AI_CANONNAME & hints.ai_flags;
  }
  rv = getaddrinfo(node, service, hints_p, &result);
  if (0 == rv) {
    ikptr	s_list_of_addrinfo;
    if (result) {
      pcb->root0 = &s_rtd;
      {
	ikptr	s_spine;
	s_list_of_addrinfo = s_spine = ika_pair_alloc(pcb);
	pcb->root1 = &s_list_of_addrinfo;
	pcb->root2 = &s_spine;
	{
	  for (iter = result; iter;) {
	    IK_ASS(IK_CAR(s_spine), addrinfo_to_struct(pcb, s_rtd, iter, with_canon_name));
	    iter = iter->ai_next;
	    if (iter) {
	      IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	      s_spine = IK_CDR(s_spine);
	    } else {
	      IK_CDR(s_spine) = null_object;
	      break;
	    }
	  }
	}
	pcb->root2 = NULL;
	pcb->root1 = NULL;
      }
      pcb->root0 = NULL;
      freeaddrinfo(result);
    } else
      s_list_of_addrinfo = null_object;
    return s_list_of_addrinfo;
  } else {
    /* The  GAI_  codes	 are  already  negative in  the	 GNU  C	 Library
       headers. */
    return IK_FIX(rv);
  }
}
ikptr
ikrt_posix_gai_strerror (ikptr s_error_code, ikpcb * pcb)
{
  return ika_bytevector_from_cstring(pcb, gai_strerror(IK_UNFIX(s_error_code)));
}


/** --------------------------------------------------------------------
 ** Netword protocols database.
 ** ----------------------------------------------------------------- */

static ikptr
protoent_to_struct (ikpcb * pcb, ikptr s_rtd, struct protoent * src)
/* Convert  a  C  language  "struct  protoent" into  a  Scheme  language
   "struct-protoent".  Make use of "pcb->root7,8,9". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
    /* fill the field "p_name" */
    IK_ASS(IK_FIELD(s_dst, 0), ika_bytevector_from_cstring(pcb, src->p_name));
    /* fill the field "p_aliases" */
    if (src->p_aliases[0]) {
      ikptr	s_list_of_aliases, s_spine;
      int	i;
      s_list_of_aliases = s_spine = ika_pair_alloc(pcb);
      pcb->root8 = &s_list_of_aliases;
      pcb->root7 = &s_spine;
      {
	for (i=0; src->p_aliases[i];) {
	  IK_ASS(IK_CAR(s_spine), ika_bytevector_from_cstring(pcb, src->p_aliases[i]));
	  if (src->p_aliases[++i]) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = null_object;
	    break;
	  }
	}
      }
      pcb->root7 = NULL;
      pcb->root8 = NULL;
      IK_FIELD(s_dst, 1) = s_list_of_aliases;
    } else
      IK_FIELD(s_dst, 1) = null_object;
    /* fill the field "p_proto" */
    IK_FIELD(s_dst, 2) = IK_FIX(src->p_proto);
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_getprotobyname (ikptr s_rtd, ikptr s_name, ikpcb * pcb)
{
  char *		name;
  struct protoent *	entry;
  name	= IK_BYTEVECTOR_DATA_CHARP(s_name);
  entry = getprotobyname(name);
  return (entry)? protoent_to_struct(pcb, s_rtd, entry) : false_object;
}
ikptr
ikrt_posix_getprotobynumber (ikptr s_rtd, ikptr s_proto_num, ikpcb * pcb)
{
  struct protoent *	entry;
  entry = getprotobynumber(IK_UNFIX(s_proto_num));
  return (entry)? protoent_to_struct(pcb, s_rtd, entry) : false_object;
}
ikptr
ikrt_posix_protocol_entries (ikptr s_rtd, ikpcb * pcb)
{
  ikptr			s_list_of_entries;
  struct protoent *	entry;
  setprotoent(1);
  {
    pcb->root0 = &s_rtd;
    {
      entry = getprotoent();
      if (entry) {
	ikptr	s_spine;
	s_list_of_entries = s_spine = ika_pair_alloc(pcb);
	pcb->root1 = &s_list_of_entries;
	pcb->root2 = &s_spine;
	{
	  while (entry) {
	    IK_ASS(IK_CAR(s_spine), protoent_to_struct(pcb, s_rtd, entry));
	    entry = getprotoent();
	    if (entry) {
	      IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	      s_spine = IK_CDR(s_spine);
	    } else {
	      IK_CDR(s_spine) = null_object;
	      break;
	    }
	  }
	}
	pcb->root2 = NULL;
	pcb->root1 = NULL;
      } else
	s_list_of_entries = null_object;
    }
    pcb->root0 = NULL;
  }
  endprotoent();
  return s_list_of_entries;
}


/** --------------------------------------------------------------------
 ** Network services database.
 ** ----------------------------------------------------------------- */

static ikptr
servent_to_struct (ikpcb * pcb, ikptr s_rtd, struct servent * src)
/* Convert  a  C  language  "struct  servent"  into  a  Scheme  language
   "struct-servent". Make use of "pcb->root7,8,9". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
    /* fill the field "s_name" */
    IK_ASS(IK_FIELD(s_dst, 0), ika_bytevector_from_cstring(pcb, src->s_name));
    /* fill the field "s_aliases" */
    if (src->s_aliases[0]) {
      ikptr	s_list_of_aliases, s_spine;
      int	i;
      s_list_of_aliases = s_spine = ika_pair_alloc(pcb);
      pcb->root8 = &s_list_of_aliases;
      pcb->root7 = &s_spine;
      {
	for (i=0; src->s_aliases[i];) {
	  IK_ASS(IK_CAR(s_spine), ika_bytevector_from_cstring(pcb, src->s_aliases[i]));
	  if (src->s_aliases[++i]) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = null_object;
	    break;
	  }
	}
      }
      pcb->root7 = NULL;
      pcb->root8 = NULL;
      IK_FIELD(s_dst, 1) = s_list_of_aliases;
    } else
      IK_FIELD(s_dst, 1) = null_object;
    /* fill the field "s_port" */
    IK_FIELD(s_dst, 2) = IK_FIX(ntohs((uint16_t)(src->s_port)));
    /* fill the field "s_proto" */
    IK_ASS(IK_FIELD(s_dst, 3), ika_bytevector_from_cstring(pcb, src->s_proto));
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_getservbyname (ikptr s_rtd, ikptr s_name, ikptr s_proto, ikpcb * pcb)
{
  char *		name;
  char *		proto;
  struct servent *	entry;
  name	= IK_BYTEVECTOR_DATA_CHARP(s_name);
  proto = IK_BYTEVECTOR_DATA_CHARP(s_proto);
  entry = getservbyname(name, proto);
  return (entry)? servent_to_struct(pcb, s_rtd, entry) : false_object;
}
ikptr
ikrt_posix_getservbyport (ikptr s_rtd, ikptr s_port, ikptr s_proto, ikpcb * pcb)
{
  char *		proto;
  struct servent *	entry;
  proto = IK_BYTEVECTOR_DATA_CHARP(s_proto);
  entry = getservbyport((int)htons((uint16_t)IK_UNFIX(s_port)), proto);
  return (entry)? servent_to_struct(pcb, s_rtd, entry) : false_object;
}
ikptr
ikrt_posix_service_entries (ikptr s_rtd, ikpcb * pcb)
{
  ikptr			s_list_of_entries;
  struct servent *	entry;
  setservent(1);
  {
    entry = getservent();
    if (entry) {
      pcb->root0 = &s_rtd;
      {
	ikptr	s_spine;
	s_list_of_entries = s_spine = ika_pair_alloc(pcb);
	pcb->root1 = &s_list_of_entries;
	pcb->root2 = &s_spine;
	{
	  while (entry) {
	    IK_ASS(IK_CAR(s_spine), servent_to_struct(pcb, s_rtd, entry));
	    entry = getservent();
	    if (entry) {
	      IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	      s_spine = IK_CDR(s_spine);
	    } else {
	      IK_CDR(s_spine) = null_object;
	      break;
	    }
	  }
	}
	pcb->root2 = NULL;
	pcb->root1 = NULL;
      }
      pcb->root0 = NULL;
    } else
      s_list_of_entries = null_object;
  }
  endservent();
  return s_list_of_entries;
}


/** --------------------------------------------------------------------
 ** Networks database.
 ** ----------------------------------------------------------------- */

static ikptr
netent_to_struct (ikpcb * pcb, ikptr s_rtd, struct netent * src)
/* Convert  a  C  language   "struct  netent"  into  a  Scheme  language
   "struct-netent".  Make use of "pcb->root7,8,9". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
    IK_ASS(IK_FIELD(s_dst, 0), ika_bytevector_from_cstring(pcb, src->n_name));
    if (src->n_aliases[0]) {
      ikptr	s_list_of_aliases, s_spine;
      int	i;
      s_list_of_aliases = s_spine = ika_pair_alloc(pcb);
      pcb->root8 = &s_list_of_aliases;
      pcb->root7 = &s_spine;
      {
	for (i=0; src->n_aliases[i];) {
	  IK_ASS(IK_CAR(s_spine), ika_bytevector_from_cstring(pcb, src->n_aliases[i]));
	  if (src->n_aliases[++i]) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = null_object;
	    break;
	  }
	}
      }
      pcb->root7 = NULL;
      pcb->root8 = NULL;
      IK_FIELD(s_dst, 1) = s_list_of_aliases;
    } else
      IK_FIELD(s_dst, 1) = null_object;
    IK_FIELD(s_dst, 2) = IK_FIX(src->n_addrtype);
    IK_ASS(IK_FIELD(s_dst, 3), ika_integer_from_ullong(pcb, (ik_ullong)src->n_net));
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_getnetbyname (ikptr s_rtd, ikptr s_name, ikpcb * pcb)
{
  char *		name;
  struct netent *	entry;
  name	= IK_BYTEVECTOR_DATA_CHARP(s_name);
  entry = getnetbyname(name);
  return (entry)? netent_to_struct(pcb, s_rtd, entry) : false_object;
}
ikptr
ikrt_posix_getnetbyaddr (ikptr s_rtd, ikptr s_net_num, ikptr s_type, ikpcb * pcb)
{
  uint32_t		net;
  struct netent *	entry;
  net   = (uint32_t)ik_integer_to_ulong(s_net_num);
  entry = getnetbyaddr(net, IK_UNFIX(s_type));
  return (entry)? netent_to_struct(pcb, s_rtd, entry) : false_object;
}
ikptr
ikrt_posix_network_entries (ikptr s_rtd, ikpcb * pcb)
{
  struct netent *	entry;
  ikptr			s_list_of_entries;
  setnetent(1);
  entry = getnetent();
  if (entry) {
    pcb->root0 = &s_rtd;
    {
      ikptr	s_spine;
      s_list_of_entries = s_spine = ika_pair_alloc(pcb);
      pcb->root1 = &s_list_of_entries;
      pcb->root2 = &s_spine;
      {
	while (entry) {
	  IK_ASS(IK_CAR(s_spine), netent_to_struct(pcb, s_rtd, entry));
	  entry = getnetent();
	  if (entry) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = null_object;
	    break;
	  }
	}
      }
      pcb->root2 = NULL;
      pcb->root1 = NULL;
    }
    pcb->root0 = NULL;
  } else
    s_list_of_entries = null_object;
  endnetent();
  return s_list_of_entries;
}


/** --------------------------------------------------------------------
 ** Network socket constructors.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_socket (ikptr s_namespace, ikptr s_style, ikptr s_protocol)
{
  int	rv;
  errno = 0;
  rv	= socket(IK_UNFIX(s_namespace), IK_UNFIX(s_style), IK_UNFIX(s_protocol));
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_shutdown (ikptr s_sock, ikptr s_how)
{
  int	rv;
  errno = 0;
  rv	= shutdown(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_how));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_socketpair (ikptr s_namespace, ikptr s_style, ikptr s_protocol, ikpcb * pcb)
{
  int	rv;
  int	fds[2];
  errno = 0;
  rv	= socketpair(IK_UNFIX(s_namespace), IK_UNFIX(s_style), IK_UNFIX(s_protocol), fds);
  if (0 == rv) {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_CAR(s_pair) = IK_FD_TO_NUM(fds[0]);
      IK_CDR(s_pair) = IK_FD_TO_NUM(fds[1]);
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_connect (ikptr s_sock, ikptr s_addr)
{
  struct sockaddr *	addr;
  socklen_t		addr_len;
  int			rv;
  addr	   = IK_BYTEVECTOR_DATA_VOIDP(s_addr);
  addr_len = (socklen_t)IK_BYTEVECTOR_LENGTH(s_addr);
  errno	   = 0;
  rv	   = connect(IK_NUM_TO_FD(s_sock), addr, addr_len);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_listen (ikptr s_sock, ikptr s_number_of_pending_connections)
{
  int	rv;
  errno	   = 0;
  rv	   = listen(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_number_of_pending_connections));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_accept (ikptr s_sock, ikpcb * pcb)
{
#undef SIZE
#define SIZE		512
  uint8_t		bytes[SIZE];
  struct sockaddr *	addr = (struct sockaddr *)bytes;
  socklen_t		addr_len = SIZE;
  int			rv;
  errno	   = 0;
  rv	   = accept(IK_NUM_TO_FD(s_sock), addr, &addr_len);
  if (0 <= rv) {
    ikptr	s_pair;
    ikptr	s_addr;
    void *	addr_data;
    s_pair     = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      s_addr	 = ika_bytevector_alloc(pcb, addr_len);
      addr_data	 = IK_BYTEVECTOR_DATA_VOIDP(s_addr);
      memcpy(addr_data, addr, addr_len);
      IK_CAR(s_pair) = IK_FIX(rv);
      IK_CDR(s_pair) = s_addr;
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_bind (ikptr s_sock, ikptr s_socks_addr)
{
  struct sockaddr *	addr;
  socklen_t		len;
  int			rv;
  addr	= IK_BYTEVECTOR_DATA_VOIDP(s_socks_addr);
  len	= (socklen_t)IK_BYTEVECTOR_LENGTH(s_socks_addr);
  errno = 0;
  rv	= bind(IK_NUM_TO_FD(s_sock), addr, len);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_getpeername (ikptr s_sock, ikpcb * pcb)
{
#undef SIZE
#define SIZE		512
  uint8_t		bytes[SIZE];
  struct sockaddr *	addr = (struct sockaddr *)bytes;
  socklen_t		addr_len = SIZE;
  int			rv;
  errno	   = 0;
  rv	   = getpeername(IK_NUM_TO_FD(s_sock), addr, &addr_len);
  if (0 == rv)
    return ika_bytevector_from_memory_block(pcb, addr, addr_len);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_getsockname (ikptr s_sock, ikpcb * pcb)
{
#undef SIZE
#define SIZE		512
  uint8_t		bytes[SIZE];
  struct sockaddr *	addr = (struct sockaddr *)bytes;
  socklen_t		addr_len = SIZE;
  int			rv;
  errno = 0;
  rv	= getsockname(IK_NUM_TO_FD(s_sock), addr, &addr_len);
  if (0 == rv)
    return ika_bytevector_from_memory_block(pcb, addr, addr_len);
  else
    return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Network sockets: sending and receiving data.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_send (ikptr s_sock, ikptr s_buffer, ikptr s_size, ikptr s_flags)
{
  void *	buffer;
  size_t	size;
  int		rv;
  buffer     = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	     = (size_t)((false_object != s_size)?
			IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  errno	     = 0;
  rv	     = send(IK_NUM_TO_FD(s_sock), buffer, size, IK_UNFIX(s_flags));
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_recv (ikptr s_sock, ikptr s_buffer, ikptr s_size, ikptr s_flags)
{
  void *	buffer;
  size_t	size;
  int		rv;
  buffer     = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	     = (size_t)((false_object != s_size)?
			IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  errno	     = 0;
  rv	     = recv(IK_NUM_TO_FD(s_sock), buffer, size, IK_UNFIX(s_flags));
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_sendto (ikptr s_sock, ikptr s_buffer, ikptr s_size, ikptr s_flags, ikptr s_addr)
{
  void *		buffer;
  struct sockaddr *	addr;
  socklen_t		addr_len;
  size_t		size;
  int			rv;
  buffer   = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	   = (size_t)((false_object != s_size)?
		      IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  addr	   = IK_BYTEVECTOR_DATA_VOIDP(s_addr);
  addr_len = (socklen_t)IK_BYTEVECTOR_LENGTH(s_addr);
  errno	   = 0;
  rv	   = sendto(IK_NUM_TO_FD(s_sock), buffer, size, IK_UNFIX(s_flags), addr, addr_len);
  return (0 <= rv)? IK_FIX(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_recvfrom (ikptr s_sock, ikptr s_buffer, ikptr s_size, ikptr s_flags, ikpcb * pcb)
{
#undef SIZE
#define SIZE		512
  uint8_t		bytes[SIZE];
  struct sockaddr *	addr = (struct sockaddr *)bytes;
  socklen_t		addr_len = SIZE;
  void *		buffer;
  size_t		size;
  int			rv;
  buffer     = IK_BYTEVECTOR_DATA_VOIDP(s_buffer);
  size	     = (size_t)((false_object != s_size)?
			IK_UNFIX(s_size) : IK_BYTEVECTOR_LENGTH(s_buffer));
  errno	     = 0;
  rv	     = recvfrom(IK_NUM_TO_FD(s_sock), buffer, size, IK_UNFIX(s_flags), addr, &addr_len);
  if (0 <= rv) {
    ikptr	s_pair = ika_pair_alloc(pcb);
    ikptr	s_addr;
    void *	addr_data;
    pcb->root0 = &s_pair;
    {
      s_addr	= ika_bytevector_alloc(pcb, addr_len);
      addr_data = IK_BYTEVECTOR_DATA_VOIDP(s_addr);
      memcpy(addr_data, addr, addr_len);
      IK_CAR(s_pair) = IK_FIX(rv);
      IK_CDR(s_pair) = s_addr;
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Network socket options.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getsockopt (ikptr s_sock, ikptr s_level, ikptr s_optname, ikptr s_optval)
{
  void *	optval = IK_BYTEVECTOR_DATA_VOIDP(s_optval);
  socklen_t	optlen = (socklen_t)IK_BYTEVECTOR_LENGTH(s_optval);
  int		rv;
  errno = 0;
  rv	= getsockopt(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_level), IK_UNFIX(s_optname), optval, &optlen);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_setsockopt (ikptr s_sock, ikptr s_level, ikptr s_optname, ikptr s_optval)
{
  void *	optval = IK_BYTEVECTOR_DATA_VOIDP(s_optval);
  socklen_t	optlen = (socklen_t)IK_BYTEVECTOR_LENGTH(s_optval);
  int		rv;
  errno = 0;
  rv	= setsockopt(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_level), IK_UNFIX(s_optname), optval, optlen);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_setsockopt_int (ikptr s_sock, ikptr s_level, ikptr s_optname, ikptr s_optval_num)
{
  int		optval;
  socklen_t	optlen = sizeof(int);
  int		rv;
  if (true_object == s_optval_num)
    optval = 1;
  else if (false_object == s_optval_num)
    optval = 0;
  else {
    long	num = ik_integer_to_long(s_optval_num);
    optval = (int)num;
  }
  errno = 0;
  rv	= setsockopt(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_level), IK_UNFIX(s_optname), &optval, optlen);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_getsockopt_int (ikptr s_sock, ikptr s_level, ikptr s_optname, ikpcb * pcb)
{
  int		optval;
  socklen_t	optlen = sizeof(int);
  int		rv;
  errno = 0;
  rv	= getsockopt(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_level), IK_UNFIX(s_optname), &optval, &optlen);
  if (0 == rv) {
    /* Return  a pair  to distinguish  the value  from an  encoded errno
       value. */
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_long(pcb, (long)optval));
      IK_CDR(s_pair) = true_object;
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_setsockopt_size_t (ikptr s_sock, ikptr s_level, ikptr s_optname, ikptr s_optval_num)
{
  size_t	optval = (size_t)ik_integer_to_long(s_optval_num);
  socklen_t	optlen = sizeof(size_t);
  int		rv;
  errno = 0;
  rv	= setsockopt(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_level), IK_UNFIX(s_optname), &optval, optlen);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_getsockopt_size_t (ikptr s_sock, ikptr s_level, ikptr s_optname, ikpcb * pcb)
{
  size_t	optval;
  socklen_t	optlen = sizeof(size_t);
  int		rv;
  errno = 0;
  rv	= getsockopt(IK_NUM_TO_FD(s_sock), IK_UNFIX(s_level), IK_UNFIX(s_optname), &optval, &optlen);
  if (0 == rv) {
    /* Return  a pair  to distinguish  the value  from an  encoded errno
       value. */
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_long(pcb, (long)optval));
      IK_CDR(s_pair) = true_object;
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_setsockopt_linger (ikptr s_sock, ikptr onoff, ikptr linger)
{
  struct linger optval;
  socklen_t	optlen = sizeof(struct linger);
  int		rv;
  optval.l_onoff  = (true_object == onoff)? 1 : 0;
  optval.l_linger = IK_UNFIX(linger);
  errno = 0;
  rv	= setsockopt(IK_NUM_TO_FD(s_sock), SOL_SOCKET, SO_LINGER, &optval, optlen);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_getsockopt_linger (ikptr s_sock, ikpcb * pcb)
{
  struct linger optval;
  socklen_t	optlen = sizeof(struct linger);
  int		rv;
  errno = 0;
  rv	= getsockopt(IK_NUM_TO_FD(s_sock), SOL_SOCKET, SO_LINGER, &optval, &optlen);
  if (0 == rv) {
    /* Return  a pair  to distinguish  the value  from an  encoded errno
       value. */
    ikptr	s_pair = ika_pair_alloc(pcb);
    IK_CAR(s_pair) = optval.l_onoff? true_object : false_object;
    IK_CDR(s_pair) = IK_FIX(optval.l_linger);
    return s_pair;
  } else
    return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Users and groups.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getuid (void)
{
  return IK_UID_TO_NUM(getuid());
}
ikptr
ikrt_posix_getgid (void)
{
  return IK_GID_TO_NUM(getgid());
}
ikptr
ikrt_posix_geteuid (void)
{
  return IK_UID_TO_NUM(geteuid());
}
ikptr
ikrt_posix_getegid (void)
{
  return IK_GID_TO_NUM(getegid());
}
ikptr
ikrt_posix_getgroups (ikpcb * pcb)
{
  int	count;
  errno = 0;
  count = getgroups(0, NULL);
  if (errno)
    return ik_errno_to_code();
  else {
    gid_t	gids[count];
    errno = 0;
    count = getgroups(count, gids);
    if (-1 == count)
      return ik_errno_to_code();
    else if (0 == count)
      return null_object;
    else {
      ikptr	s_list_of_gids, s_spine;
      int	i;
      s_list_of_gids = s_spine = ika_pair_alloc(pcb);
      pcb->root0 = &s_list_of_gids;
      pcb->root1 = &s_spine;
      {
	for (i=0; i<count;) {
	  IK_CAR(s_spine) = IK_GID_TO_NUM(gids[i]);
	  if (++i<count) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = null_object;
	    break;
	  }
	}
      }
      pcb->root1 = NULL;
      pcb->root0 = NULL;
      return s_list_of_gids;
    }
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_seteuid (ikptr s_new_uid)
{
  int	rv;
  errno = 0;
  rv	= seteuid(IK_NUM_TO_UID(s_new_uid));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_setuid (ikptr s_new_uid)
{
  int	rv;
  errno = 0;
  rv	= setuid(IK_NUM_TO_UID(s_new_uid));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_setreuid (ikptr s_real_uid, ikptr s_effective_uid)
{
  int	rv;
  errno = 0;
  rv	= setreuid(IK_NUM_TO_UID(s_real_uid), IK_NUM_TO_UID(s_effective_uid));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_setegid (ikptr s_new_gid)
{
  int	rv;
  errno = 0;
  rv	= setegid(IK_NUM_TO_GID(s_new_gid));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_setgid (ikptr s_new_gid)
{
  int	rv;
  errno = 0;
  rv	= setgid(IK_NUM_TO_GID(s_new_gid));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_setregid (ikptr s_real_gid, ikptr s_effective_gid)
{
  int	rv;
  errno = 0;
  rv	= setregid(IK_NUM_TO_GID(s_real_gid), IK_NUM_TO_GID(s_effective_gid));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_getlogin (ikpcb * pcb)
{
  char *	username;
  username = getlogin();
  return (username)? ika_bytevector_from_cstring(pcb, username) : false_object;
}


/** --------------------------------------------------------------------
 ** Passwords database.
 ** ----------------------------------------------------------------- */

static ikptr
passwd_to_struct (ikptr s_rtd, struct passwd * src, ikpcb * pcb)
/* Convert  a  C  language   "struct  passwd"  into  a  Scheme  language
   "struct-passwd".  Makes use of "pcb->root9". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
    IK_ASS(IK_FIELD(s_dst, 0), ika_bytevector_from_cstring(pcb, src->pw_name));
    IK_ASS(IK_FIELD(s_dst, 1), ika_bytevector_from_cstring(pcb, src->pw_passwd));
    IK_ASS(IK_FIELD(s_dst, 2), IK_FIX(src->pw_uid));
    IK_ASS(IK_FIELD(s_dst, 3), IK_FIX(src->pw_gid));
    IK_ASS(IK_FIELD(s_dst, 4), ika_bytevector_from_cstring(pcb, src->pw_gecos));
    IK_ASS(IK_FIELD(s_dst, 5), ika_bytevector_from_cstring(pcb, src->pw_dir));
    IK_ASS(IK_FIELD(s_dst, 6), ika_bytevector_from_cstring(pcb, src->pw_shell));
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_getpwuid (ikptr s_rtd, ikptr s_uid, ikpcb * pcb)
{
  struct passwd *	entry;
  entry = getpwuid(IK_NUM_TO_UID(s_uid));
  return (entry)? passwd_to_struct(s_rtd, entry, pcb) : false_object;
}
ikptr
ikrt_posix_getpwnam (ikptr s_rtd, ikptr s_name, ikpcb * pcb)
{
  char *		name;
  struct passwd *	entry;
  name	= IK_BYTEVECTOR_DATA_CHARP(s_name);
  entry = getpwnam(name);
  return (entry)? passwd_to_struct(s_rtd, entry, pcb) : false_object;
}
ikptr
ikrt_posix_user_entries (ikptr s_rtd, ikpcb * pcb)
{
  struct passwd *	entry;
  ikptr			s_list_of_entries;
  setpwent();
  {
    entry = getpwent();
    if (entry) {
      ikptr	s_spine;
      pcb->root0 = &s_rtd;
      {
	s_list_of_entries = s_spine = ika_pair_alloc(pcb);
	pcb->root1 = &s_list_of_entries;
	pcb->root2 = &s_spine;
	{
	  while (entry) {
	    IK_ASS(IK_CAR(s_spine), passwd_to_struct(s_rtd, entry, pcb));
	    entry = getpwent();
	    if (entry) {
	      IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	      s_spine = IK_CDR(s_spine);
	    } else {
	      IK_CDR(s_spine) = null_object;
	      break;
	    }
	  }
	}
	pcb->root2 = NULL;
	pcb->root1 = NULL;
      }
      pcb->root0 = NULL;
    } else
      s_list_of_entries = null_object;
  }
  endpwent();
  return s_list_of_entries;
}


/** --------------------------------------------------------------------
 ** Groups database.
 ** ----------------------------------------------------------------- */

static ikptr
group_to_struct (ikptr s_rtd, struct group * src, ikpcb * pcb)
/* Convert  a   C  language  "struct  group"  into   a  Scheme  language
   "struct-group".  Makes use of "pcb->root7,8,9". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
    IK_ASS(IK_FIELD(s_dst, 0), ika_bytevector_from_cstring(pcb, src->gr_name));
    IK_FIELD(s_dst, 1) = IK_FIX(src->gr_gid);
    {
      ikptr	s_list_of_users;
      if (src->gr_mem[0]) {
	ikptr	s_spine;
	int	i;
	s_list_of_users = s_spine = ika_pair_alloc(pcb);
	pcb->root8 = &s_list_of_users;
	pcb->root7 = &s_spine;
	{
	  for (i=0; src->gr_mem[i];) {
	    IK_ASS(IK_CAR(s_spine), ika_bytevector_from_cstring(pcb, src->gr_mem[i]));
	    if (src->gr_mem[++i]) {
	      IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	      s_spine = IK_CDR(s_spine);
	    } else {
	      IK_CDR(s_spine) = null_object;
	      break;
	    }
	  }
	}
	pcb->root7 = NULL;
	pcb->root8 = NULL;
      } else
	s_list_of_users = null_object;
      IK_FIELD(s_dst, 2) = s_list_of_users;
    }
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_getgrgid (ikptr s_rtd, ikptr s_gid, ikpcb * pcb)
{
  struct group *       entry;
  entry = getgrgid(IK_NUM_TO_GID(s_gid));
  return (entry)? group_to_struct(s_rtd, entry, pcb) : false_object;
}
ikptr
ikrt_posix_getgrnam (ikptr s_rtd, ikptr s_name, ikpcb * pcb)
{
  char *		name;
  struct group *       entry;
  name	= IK_BYTEVECTOR_DATA_CHARP(s_name);
  entry = getgrnam(name);
  return (entry)? group_to_struct(s_rtd, entry, pcb) : false_object;
}
ikptr
ikrt_posix_group_entries (ikptr s_rtd, ikpcb * pcb)
{
  struct group *	entry;
  ikptr			s_list_of_entries;
  setgrent();
  {
    entry = getgrent();
    if (entry) {
      pcb->root0 = &s_rtd;
      {
	ikptr	s_spine = s_list_of_entries = ika_pair_alloc(pcb);
	pcb->root1 = &s_list_of_entries;
	pcb->root2 = &s_spine;
	{
	  while (entry) {
	    IK_ASS(IK_CAR(s_spine), group_to_struct(s_rtd, entry, pcb));
	    entry = getgrent();
	    if (entry) {
	      IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	      s_spine = IK_CDR(s_spine);
	    } else {
	      IK_CDR(s_spine) = null_object;
	      break;
	    }
	  }
	}
	pcb->root2 = NULL;
	pcb->root1 = NULL;
      }
      pcb->root0 = NULL;
    } else
      s_list_of_entries = null_object;
  }
  endgrent();
  return s_list_of_entries;
}


/** --------------------------------------------------------------------
 ** Job control.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_ctermid (ikpcb * pcb)
{
  char		id[L_ctermid];
  ctermid(id);
  return ika_bytevector_from_cstring(pcb, id);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_setsid (void)
{
  int	rv;
  errno = 0;
  rv	= setsid();
  return (-1 != rv)? IK_PID_TO_NUM(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_getsid (ikptr s_pid)
{
  int	rv;
  errno = 0;
  rv	= getsid(IK_NUM_TO_PID(s_pid));
  return (-1 != rv)? IK_PID_TO_NUM(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_getpgrp (void)
/* About  this	 function:  notice  that  we   define  "_GNU_SOURCE"  in
   "configure.ac".  See the GNU C Library documentation for details. */
{
  return IK_PID_TO_NUM(getpgrp());
}
ikptr
ikrt_posix_setpgid (ikptr s_pid, ikptr s_pgid)
{
  int	rv;
  errno = 0;
  rv	= setpgid(IK_NUM_TO_PID(s_pid), IK_NUM_TO_PID(s_pgid));
  return (-1 != rv)? IK_FIX(0) : ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_tcgetpgrp (ikptr s_fd)
{
  pid_t	rv;
  errno = 0;
  rv	= tcgetpgrp(IK_NUM_TO_FD(s_fd));
  return (-1 != rv)? IK_PID_TO_NUM(rv) : ik_errno_to_code();
}
ikptr
ikrt_posix_tcsetpgrp (ikptr s_fd, ikptr s_pgid)
{
  int	rv;
  errno = 0;
  rv	= tcsetpgrp(IK_NUM_TO_PID(s_fd), IK_NUM_TO_PID(s_pgid));
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_tcgetsid (ikptr s_fd)
{
  pid_t	rv;
  errno = 0;
  rv	= tcgetsid(IK_NUM_TO_FD(s_fd));
  return (-1 != rv)? IK_PID_TO_NUM(rv) : ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Date and time related functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_clock (ikpcb * pcb)
{
  clock_t	T;
  T = clock();
  return (((clock_t)-1) != T)? ika_flonum_from_double(pcb, (double)T) : false_object;
}
ikptr
ikrt_posix_time (ikpcb * pcb)
{
  time_t	T;
  T = time(NULL);
  return (((time_t)-1) != T)? ika_flonum_from_double(pcb, (double)T) : false_object;
}

/* ------------------------------------------------------------------ */

static ikptr
tms_to_struct (ikptr s_rtd, struct tms * src, ikpcb * pcb)
/* Convert   a  C  language   "struct  tms"   into  a   Scheme  language
   "struct-tms".  Makes use of "pcb->root9". */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
#if 0
    fprintf(stderr, "struct tms = %f, %f, %f, %f\n",
	    (double)(src->tms_utime),  (double)(src->tms_stime),
	    (double)(src->tms_cutime), (double)(src->tms_cstime));
#endif
    IK_ASS(IK_FIELD(s_dst, 0), ika_flonum_from_double(pcb, (double)(src->tms_utime)));
    IK_ASS(IK_FIELD(s_dst, 1), ika_flonum_from_double(pcb, (double)(src->tms_stime)));
    IK_ASS(IK_FIELD(s_dst, 2), ika_flonum_from_double(pcb, (double)(src->tms_cutime)));
    IK_ASS(IK_FIELD(s_dst, 3), ika_flonum_from_double(pcb, (double)(src->tms_cstime)));
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_times (ikptr s_rtd, ikpcb * pcb)
{
  struct tms	T = { 0, 0, 0, 0 };
  clock_t	rv;
  rv = times(&T);
  return (((clock_t)-1) == rv)? false_object : tms_to_struct(s_rtd, &T, pcb);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_gettimeofday (ikptr s_rtd, ikpcb * pcb)
{
  struct timeval	T;
  int			rv;
  errno = 0;
  rv	= gettimeofday(&T, NULL);
  if (0 == rv) {
    ikptr	s_stru = ika_struct_alloc(pcb, s_rtd);
    pcb->root0 = &s_stru;
    {
      IK_ASS(IK_FIELD(s_stru, 0), ika_integer_from_long(pcb, T.tv_sec));
      IK_ASS(IK_FIELD(s_stru, 1), ika_integer_from_long(pcb, T.tv_usec));
    }
    pcb->root0 = NULL;
    return s_stru;
  } else
    return ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

static ikptr
tm_to_struct (ikptr s_rtd, struct tm * src, ikpcb * pcb)
/* Convert a C language "struct  tm" into a Scheme language "struct-tm".
   Makes use of "pcb->root9" only. */
{
  ikptr s_dst = ika_struct_alloc(pcb, s_rtd); /* this uses "pcb->root9" */
  pcb->root9 = &s_dst;
  {
    IK_ASS(IK_FIELD(s_dst, 0), ika_integer_from_long(pcb, (long)(src->tm_sec)));
    IK_ASS(IK_FIELD(s_dst, 1), ika_integer_from_long(pcb, (long)(src->tm_min)));
    IK_ASS(IK_FIELD(s_dst, 2), ika_integer_from_long(pcb, (long)(src->tm_hour)));
    IK_ASS(IK_FIELD(s_dst, 3), ika_integer_from_long(pcb, (long)(src->tm_mday)));
    IK_ASS(IK_FIELD(s_dst, 4), ika_integer_from_long(pcb, (long)(src->tm_mon)));
    IK_ASS(IK_FIELD(s_dst, 5), ika_integer_from_long(pcb, (long)(src->tm_year)));
    IK_ASS(IK_FIELD(s_dst, 6), ika_integer_from_long(pcb, (long)(src->tm_wday)));
    IK_ASS(IK_FIELD(s_dst, 7), ika_integer_from_long(pcb, (long)(src->tm_yday)));
    IK_FIELD(s_dst, 8) = (src->tm_isdst)? true_object : false_object;
    IK_ASS(IK_FIELD(s_dst, 9), ika_integer_from_long(pcb, src->tm_gmtoff));
    IK_ASS(IK_FIELD(s_dst,10), ika_bytevector_from_cstring(pcb, src->tm_zone));
  }
  pcb->root9 = NULL;
  return s_dst;
}
ikptr
ikrt_posix_localtime (ikptr s_rtd, ikptr s_time_num, ikpcb * pcb)
{
  time_t	time = (time_t)ik_integer_to_ulong(s_time_num);
  struct tm	T;
  struct tm *	rv;
  rv	= localtime_r(&time, &T);
  return (rv)? tm_to_struct(s_rtd, &T, pcb) : false_object;
}
ikptr
ikrt_posix_gmtime (ikptr s_rtd, ikptr s_time_num, ikpcb * pcb)
{
  time_t	time = (time_t)ik_integer_to_ulong(s_time_num);
  struct tm	T;
  struct tm *	rv;
  errno = 0;
  rv	= gmtime_r(&time, &T);
  return (rv)? tm_to_struct(s_rtd, &T, pcb) : false_object;
}

/* ------------------------------------------------------------------ */

static void
struct_to_tm (ikptr s_src, struct tm * dst)
/* Convert  a Scheme  language	"struct-tm" into  a  C language	 "struct
   tm". */
{
  dst->tm_sec	= ik_integer_to_long(IK_FIELD(s_src, 0));
  dst->tm_min	= ik_integer_to_long(IK_FIELD(s_src, 1));
  dst->tm_hour	= ik_integer_to_long(IK_FIELD(s_src, 2));
  dst->tm_mday	= ik_integer_to_long(IK_FIELD(s_src, 3));
  dst->tm_mon	= ik_integer_to_long(IK_FIELD(s_src, 4));
  dst->tm_year	= ik_integer_to_long(IK_FIELD(s_src, 5));
  dst->tm_wday	= ik_integer_to_long(IK_FIELD(s_src, 6));
  dst->tm_yday	= ik_integer_to_long(IK_FIELD(s_src, 7));
  dst->tm_isdst = (true_object == IK_FIELD(s_src, 8))? 1 : 0;
  dst->tm_yday	= ik_integer_to_long(IK_FIELD(s_src, 9));
  dst->tm_zone	= IK_BYTEVECTOR_DATA_CHARP(IK_FIELD(s_src, 10));
}
ikptr
ikrt_posix_timelocal (ikptr s_tm_struct, ikpcb * pcb)
{
  struct tm	T;
  time_t	rv;
  struct_to_tm(s_tm_struct, &T);
  rv = timelocal(&T);
  return (((time_t)-1) != rv)? ika_flonum_from_double(pcb, (double)rv) : false_object;
}
ikptr
ikrt_posix_timegm (ikptr s_tm_struct, ikpcb * pcb)
{
  struct tm	T;
  time_t	rv;
  struct_to_tm(s_tm_struct, &T);
  rv = timegm(&T);
  return (((time_t)-1) != rv)? ika_flonum_from_double(pcb, (double)rv) : false_object;
}
ikptr
ikrt_posix_strftime (ikptr s_template, ikptr s_tm_struct, ikpcb *pcb)
{
  struct tm	T;
  const char *	template;
#undef SIZE
#define SIZE	4096
  size_t	size=SIZE;
  char		output[SIZE+1];
  template = IK_BYTEVECTOR_DATA_CHARP(s_template);
  struct_to_tm(s_tm_struct, &T);
  output[0] = '\1';
  size = strftime(output, size, template, &T);
  return (0 == size && '\0' != output[0])?
    false_object : ika_bytevector_from_cstring_len(pcb, output, size);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_nanosleep (ikptr s_secs, ikptr s_nsecs, ikpcb * pcb)
{
  struct timespec	requested;
  struct timespec	remaining = { 0, 0 }; /* required!!! */
  int			rv;
  requested.tv_sec  = IK_IS_FIXNUM(s_secs)?  (ik_ulong)IK_UNFIX(s_secs) :IK_REF(s_secs, off_bignum_data);
  requested.tv_nsec = IK_IS_FIXNUM(s_nsecs)? (ik_ulong)IK_UNFIX(s_nsecs):IK_REF(s_nsecs,off_bignum_data);
  errno = 0;
  rv	= nanosleep(&requested, &remaining);
  if (0 == rv) {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair),
	     remaining.tv_sec?  ika_integer_from_long(pcb, remaining.tv_sec)  : false_object);
      IK_ASS(IK_CDR(s_pair),
	     remaining.tv_nsec? ika_integer_from_long(pcb, remaining.tv_nsec) : false_object);
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_current_time (ikptr t)
{
  struct timeval s;
  gettimeofday(&s, 0);
  /* this will break in 8,727,224 years if we stay in 32-bit ptrs */
  IK_FIELD(t, 0) = IK_FIX(s.tv_sec / 1000000);
  IK_FIELD(t, 1) = IK_FIX(s.tv_sec % 1000000);
  IK_FIELD(t, 2) = IK_FIX(s.tv_usec);
  return t;
}
ikptr
ikrt_gmt_offset (ikptr t)
{
  time_t	clock;
  struct tm *	m;
  time_t	gmtclock;
  clock	   = IK_UNFIX(IK_FIELD(t, 0)) * 1000000 + IK_UNFIX(IK_FIELD(t, 1));
  m	   = gmtime(&clock);
  gmtclock = mktime(m);
  return IK_FIX(clock - gmtclock);
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */



/* end of file */
