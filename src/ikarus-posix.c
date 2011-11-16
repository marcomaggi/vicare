/*
  Part of: Vicare
  Contents: interface to POSIX functions
  Date: Sun Nov  6, 2011

  Abstract

        Interface to POSIX functions.  For the full documentation of the
        functions in this module,  see the official Vicare documentation
        in Texinfo format.

  Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

/* This causes inclusion of "pread".  (Marco Maggi; Nov 11, 2011) */
#define _GNU_SOURCE     1

#include "ikarus.h"
#include <dirent.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdint.h>
#include <time.h>
#include <utime.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/wait.h>

#define SIZEOF_STRUCT_IN_ADDR           sizeof(struct in_addr)
#define SIZEOF_STRUCT_IN6_ADDR          sizeof(struct in6_addr)


/** --------------------------------------------------------------------
 ** Errno handling.
 ** ----------------------------------------------------------------- */

ikptr
ik_errno_to_code (void)
/* Negate the current errno value and convert it into a fixnum. */
{
  int en = - errno;
  return fix(en);
}
ikptr
ikrt_posix_strerror (ikptr negated_errno_code, ikpcb* pcb)
{
  int   code = - unfix(negated_errno_code);
  errno = 0;
  char* es = strerror(code);
  if (errno) {
    return false_object;
  } else {
    int   len = strlen(es);
    ikptr bv  = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1))
      + bytevector_tag;
    ref(bv, off_bytevector_length) = fix(len);
    memcpy((char*)(bv+off_bytevector_data), es, len+1);
    return bv;
  }
}


/** --------------------------------------------------------------------
 ** Operating system environment variables.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getenv (ikptr bv, ikpcb* pcb)
{
  char * v = getenv((char*)(long)(bv + off_bytevector_data));
  if (v) {
    long int n = strlen(v);
    ikptr s = ik_safe_alloc(pcb, align(n+disp_bytevector_data+1))
      + bytevector_tag;
    ref(s, -bytevector_tag) = fix(n);
    memcpy((char*)(long)(s+off_bytevector_data), v, n+1);
    return s;
  } else {
    return false_object;
  }
}
ikptr
ikrt_posix_setenv (ikptr key, ikptr val, ikptr overwrite)
{
  int err = setenv((char*)(key+off_bytevector_data),
                   (char*)(val+off_bytevector_data),
                   overwrite!=false_object);
  return (err)? false_object : true_object;
}
ikptr
ikrt_posix_unsetenv (ikptr key)
{
#if (1 == UNSETENV_HAS_RETURN_VALUE)
  int rv = unsetenv((char*)(key+off_bytevector_data));
  return (0 == rv)? true_object : false_object;
#else
  unsetenv((char*)(key+off_bytevector_data));
  return true_object;
#endif
}
ikptr
ikrt_posix_environ (ikpcb* pcb)
{
  char **       es = environ;
  int           i;
  char *        e;
  ikptr         ac = null_object;
  pcb->root0 = &ac;
  for (i=0; (e=es[i]); i++) {
    long int bv_len = strlen(e);
    ikptr    s = ik_safe_alloc(pcb, align(bv_len+disp_bytevector_data+1)) + bytevector_tag;
    ref(s, off_bytevector_length) = fix(bv_len);
    memcpy((char*)(long)(s+off_bytevector_data), e, bv_len+1);
    pcb->root1 = &s;
    ikptr p = ik_safe_alloc(pcb, align(pair_size)) + pair_tag;
    pcb->root1 = 0;
    ref(p, off_cdr) = ac;
    ref(p, off_car) = s;
    ac = p;
  }
  pcb->root0 = 0;
  return ac;
}


/** --------------------------------------------------------------------
 ** Process identifiers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_getpid(void)
{
  return fix(getpid());
}
ikptr
ikrt_posix_getppid(void)
{
  return fix(getppid());
}


/** --------------------------------------------------------------------
 ** Executing processes.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_system (ikptr str)
{
  assert(bytevector_tag == tagof(str));
  errno = 0;
  int retval = system((char*)(long)(str+off_bytevector_data));
  if (retval >= 0)
    return fix(retval);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_fork (void)
{
  errno = 0;
  int pid = fork();
  if (pid >= 0) {
    return fix(pid);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_execv (ikptr bv_filename, ikptr list_argv)
{
  char *  filename = (char *)(long)(bv_filename + off_bytevector_data);
  int     argc     = ik_list_length(list_argv);
  char *  argv[1+argc];
  ik_list_to_argv(list_argv, argv);
  errno   = 0;
  execv(filename, argv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}
ikptr
ikrt_posix_execve (ikptr bv_filename, ikptr list_argv, ikptr list_envv)
{
  char *  filename = (char *)(long)(bv_filename + off_bytevector_data);
  int     argc = ik_list_length(list_argv);
  char *  argv[1+argc];
  int     envc = ik_list_length(list_envv);
  char *  envv[1+envc];
  ik_list_to_argv(list_argv, argv);
  ik_list_to_argv(list_envv, envv);
  errno  = 0;
  execve(filename, argv, envv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}
ikptr
ikrt_posix_execvp (ikptr bv_filename, ikptr list_argv)
{
  char *  filename = (char *)(long)(bv_filename + off_bytevector_data);
  int     argc = ik_list_length(list_argv);
  char *  argv[1+argc];
  ik_list_to_argv(list_argv, argv);
  errno  = 0;
  execvp(filename, argv);
  /* If we are here: an error occurred. */
  return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Process exit status.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_waitpid (ikptr pid, ikptr options)
{
  int   status;
  pid_t retval;
  errno  = 0;
  retval = waitpid(unfix(pid), &status, unfix(options));
  if (0 <= retval)
    return fix(status);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_wait (void)
{
  int   status;
  pid_t retval;
  errno  = 0;
  retval = wait(&status);
  if (0 <= retval)
    return fix(status);
  else
    return ik_errno_to_code();
}

ikptr
ikrt_posix_WIFEXITED (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WIFEXITED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WEXITSTATUS (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return fix(WEXITSTATUS(status));
}
ikptr
ikrt_posix_WIFSIGNALED (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WIFSIGNALED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WTERMSIG (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return fix(WTERMSIG(status));
}
ikptr
ikrt_posix_WCOREDUMP (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WCOREDUMP(status))? true_object : false_object;
}
ikptr
ikrt_posix_WIFSTOPPED (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return (WIFSTOPPED(status))? true_object : false_object;
}
ikptr
ikrt_posix_WSTOPSIG (ikptr fx_status)
{
  int   status = unfix(fx_status);
  return fix(WSTOPSIG(status));
}


/** --------------------------------------------------------------------
 ** Delivering interprocess signals.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_raise (ikptr fx_signum)
{
  int r = raise(unfix(fx_signum));
  return (0 == r)? fix(0) : ik_errno_to_code();
}
ikptr
ikrt_posix_kill (ikptr fx_pid, ikptr fx_signum)
{
  pid_t pid    = unfix(fx_pid);
  int   signum = unfix(fx_signum);
  int   r = kill(pid, signum);
  return (0 == r)? fix(0) : ik_errno_to_code();
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
#if (4 == VICARE_SIZE_OF_VOIDP)
  /* 32-bit platforms */
  ref(D, off_record_data+0*wordsize) = u_to_number((unsigned long)S->st_mode, pcb);
  ref(D, off_record_data+1*wordsize) = u_to_number((unsigned long)S->st_ino, pcb);
  ref(D, off_record_data+2*wordsize) = s_to_number((long)S->st_dev, pcb);
  ref(D, off_record_data+3*wordsize) = u_to_number((long)S->st_nlink, pcb);

  ref(D, off_record_data+4*wordsize) = u_to_number((unsigned long)S->st_uid, pcb);
  ref(D, off_record_data+5*wordsize) = u_to_number((unsigned long)S->st_gid, pcb);
  ref(D, off_record_data+6*wordsize) = ull_to_number((unsigned long long)S->st_size, pcb);

  ref(D, off_record_data+7*wordsize) = s_to_number((long)S->st_atime, pcb);
#ifdef HAVE_STAT_ST_ATIME_USEC
  ref(D, off_record_data+8*wordsize) = u_to_number((unsigned long)S->st_atime_usec, pcb);
#else
  ref(D, off_record_data+8*wordsize) = false_object;
#endif

  ref(D, off_record_data+9*wordsize)  = s_to_number((long)S->st_mtime, pcb);
#ifdef HAVE_STAT_ST_MTIME_USEC
  ref(D, off_record_data+10*wordsize) = u_to_number((unsigned long)S->st_mtime_usec, pcb);
#else
  ref(D, off_record_data+10*wordsize) = false_object;
#endif

  ref(D, off_record_data+11*wordsize) = s_to_number((long)S->st_ctime, pcb);
#ifdef HAVE_STAT_ST_CTIME_USEC
  ref(D, off_record_data+12*wordsize) = u_to_number((unsigned long)S->st_ctime_usec, pcb);
#else
  ref(D, off_record_data+12*wordsize) = false_object;
#endif

  ref(D, off_record_data+13*wordsize) = ull_to_number((unsigned long long)S->st_blocks, pcb);
  ref(D, off_record_data+14*wordsize) = ull_to_number((unsigned long long)S->st_blksize, pcb);
#else
  /* 64-bit platforms */
  ref(D, off_record_data+0*wordsize) = ull_to_number((unsigned long long)S->st_mode, pcb);
  ref(D, off_record_data+1*wordsize) = ull_to_number((unsigned long long)S->st_ino, pcb);
  ref(D, off_record_data+2*wordsize) = sll_to_number((long)S->st_dev, pcb);
  ref(D, off_record_data+3*wordsize) = ull_to_number((long)S->st_nlink, pcb);

  ref(D, off_record_data+4*wordsize) = ull_to_number((unsigned long long)S->st_uid, pcb);
  ref(D, off_record_data+5*wordsize) = ull_to_number((unsigned long long)S->st_gid, pcb);
  ref(D, off_record_data+6*wordsize) = ull_to_number((unsigned long long)S->st_size, pcb);

  ref(D, off_record_data+7*wordsize) = sll_to_number((long)S->st_atime, pcb);
#ifdef HAVE_STAT_ST_ATIME_USEC
  ref(D, off_record_data+8*wordsize) = ull_to_number((unsigned long long)S->st_atime_usec, pcb);
#else
  ref(D, off_record_data+8*wordsize) = false_object;
#endif

  ref(D, off_record_data+9*wordsize)  = sll_to_number((long)S->st_mtime, pcb);
#ifdef HAVE_STAT_ST_MTIME_USEC
  ref(D, off_record_data+10*wordsize) = ull_to_number((unsigned long long)S->st_mtime_usec, pcb);
#else
  ref(D, off_record_data+10*wordsize) = false_object;
#endif

  ref(D, off_record_data+11*wordsize) = sll_to_number((long)S->st_ctime, pcb);
#ifdef HAVE_STAT_ST_CTIME_USEC
  ref(D, off_record_data+12*wordsize) = ull_to_number((unsigned long long)S->st_ctime_usec, pcb);
#else
  ref(D, off_record_data+12*wordsize) = false_object;
#endif

  ref(D, off_record_data+13*wordsize) = ull_to_number((unsigned long long)S->st_blocks, pcb);
  ref(D, off_record_data+14*wordsize) = ull_to_number((unsigned long long)S->st_blksize, pcb);
#endif
  return 0;
}
static ikptr
posix_stat (ikptr filename_bv, ikptr stat_struct, int follow_symlinks, ikpcb* pcb)
{
  char *        filename;
  struct stat   S;
  int           rv;
  filename = (char*)(long)(filename_bv + off_bytevector_data);
  errno    = 0;
  if (follow_symlinks) {
    rv = stat(filename, &S);
  } else {
    rv = lstat(filename, &S);
  }
  if (0 == rv) {
    return fill_stat_struct(&S, stat_struct, pcb);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_stat (ikptr filename_bv, ikptr stat_struct, ikpcb* pcb)
{
  return posix_stat(filename_bv, stat_struct, 1, pcb);
}
ikptr
ikrt_posix_lstat (ikptr filename_bv, ikptr stat_struct, ikpcb* pcb)
{
  return posix_stat(filename_bv, stat_struct, 0, pcb);
}
ikptr
ikrt_posix_fstat (ikptr fd_fx, ikptr stat_struct, ikpcb* pcb)
{
  struct stat   S;
  int           rv;
  errno = 0;
  rv = fstat(unfix(fd_fx), &S);
  if (0 == rv) {
    return fill_stat_struct(&S, stat_struct, pcb);
  } else {
    return ik_errno_to_code();
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_file_size(ikptr filename, ikpcb* pcb) {
  char *        pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(filename + off_bytevector_data);
  errno    = 0;
  rv       = stat(pathname, &S);
  if (0 == rv) {
    if (sizeof(off_t) == sizeof(long)) {
      return u_to_number(S.st_size, pcb);
    } else if (sizeof(off_t) == sizeof(long long)) {
      return ull_to_number(S.st_size, pcb);
    } else {
      fprintf(stderr, "vicare internal error: invalid off_t size\n");
      exit(EXIT_FAILURE);
    }
  } else {
    return ik_errno_to_code();
  }
}


/** --------------------------------------------------------------------
 ** File type predicates.
 ** ----------------------------------------------------------------- */

static ikptr
file_is_p (ikptr pathname_bv, ikptr follow_symlinks, int flag)
{
  char *        pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno    = 0;
  if (false_object == follow_symlinks) {
    rv = lstat(pathname, &S);
  } else {
    rv = stat(pathname, &S);
  }
  if (0 == rv) {
    /* NOTE It is not enough to do "S.st_mode & flag", we really have to
       do "flag == (S.st_mode & flag)". */
    return (flag == (S.st_mode & flag))? true_object : false_object;
  } else {
    return ik_errno_to_code();
  }
}

#define FILE_IS_P(WHO,FLAG)                                     \
  ikptr WHO (ikptr pathname_bv, ikptr follow_symlinks)          \
  { return file_is_p(pathname_bv, follow_symlinks, FLAG); }

FILE_IS_P(ikrt_file_is_directory,       S_IFDIR)
FILE_IS_P(ikrt_file_is_char_device,     S_IFCHR)
FILE_IS_P(ikrt_file_is_block_device,    S_IFBLK)
FILE_IS_P(ikrt_file_is_regular_file,    S_IFREG)
FILE_IS_P(ikrt_file_is_symbolic_link,   S_IFLNK)
FILE_IS_P(ikrt_file_is_socket,          S_IFSOCK)
FILE_IS_P(ikrt_file_is_fifo,            S_IFIFO)

#define SPECIAL_FILE_IS_P(WHO,PRED)                                     \
   ikptr                                                                \
   WHO (ikptr pathname_bv, ikptr follow_symlinks)                       \
   {                                                                    \
     char *        pathname;                                            \
     struct stat   S;                                                   \
     int           rv;                                                  \
     pathname = (char*)(long)(pathname_bv + off_bytevector_data);       \
     errno    = 0;                                                      \
     if (false_object == follow_symlinks) {                             \
       rv = lstat(pathname, &S);                                        \
     } else {                                                           \
       rv = stat(pathname, &S);                                         \
     }                                                                  \
     if (0 == rv) {                                                     \
       return (PRED(&S))? true_object : false_object;                   \
     } else {                                                           \
       return ik_errno_to_code();                                       \
     }                                                                  \
   }

SPECIAL_FILE_IS_P(ikrt_file_is_message_queue,S_TYPEISMQ)
SPECIAL_FILE_IS_P(ikrt_file_is_semaphore,S_TYPEISSEM)
SPECIAL_FILE_IS_P(ikrt_file_is_shared_memory,S_TYPEISSHM)


/** --------------------------------------------------------------------
 ** Testing file access.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_access (ikptr filename, ikptr how)
{
  char* pathname = (char*)(filename + off_bytevector_data);
  int   rv;
  errno = 0;
  rv    = access(pathname, unfix(how));
  if (0 == rv) {
    return true_object;
  } else if ((errno == EACCES) ||
             (errno == EROFS)  ||
             (errno == ETXTBSY)) {
    return false_object;
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_file_exists (ikptr pathname_bv)
{
  char *        pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  errno    = 0;
  rv = stat(pathname, &S);
  if (0 == rv) {
    return true_object;
  } else if ((ENOENT == errno) || (ENOTDIR == errno)) {
    return false_object;
  } else {
    return ik_errno_to_code();
  }
}


/** --------------------------------------------------------------------
 ** File times.
 ** ----------------------------------------------------------------- */

static ikptr
timespec_vector (struct timespec * T, ikptr vector, ikpcb* pcb)
{
  ref(vector, off_vector_data+0*wordsize) = s_to_number((long)(T->tv_sec),  pcb);
  ref(vector, off_vector_data+1*wordsize) = s_to_number((long)(T->tv_nsec), pcb);
  return fix(0);
}
ikptr
ikrt_posix_file_ctime (ikptr pathname_bv, ikptr vector, ikpcb* pcb) {
  char*         pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno = 0;
  rv    = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_CTIMESPEC
    return timespec_vector(&S.st_ctimespec, vector, pcb);
#elif HAVE_STAT_ST_CTIM
    return timespec_vector(&S.st_ctim, vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_ctime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, vector, pcb);
#endif
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_posix_file_mtime (ikptr pathname_bv, ikptr vector, ikpcb* pcb) {
  char*         pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno = 0;
  rv    = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_MTIMESPEC
    return timespec_vector(&S.st_mtimespec, vector, pcb);
#elif HAVE_STAT_ST_MTIM
    return timespec_vector(&S.st_mtim, vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_mtime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, vector, pcb);
#endif
  } else {
    return ik_errno_to_code();
  }
}

ikptr
ikrt_posix_file_atime (ikptr pathname_bv, ikptr vector, ikpcb* pcb) {
  char*         pathname;
  struct stat   S;
  int           rv;
  pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  errno = 0;
  rv    = stat(pathname, &S);
  if (0 == rv) {
#if HAVE_STAT_ST_ATIMESPEC
    return timespec_vector(&S.st_atimespec, vector, pcb);
#elif HAVE_STAT_ST_ATIM
    return timespec_vector(&S.st_atim, vector, pcb);
#else
    struct timespec ts;
    ts.tv_sec  = s.st_atime;
    ts.tv_nsec = 0;
    return timespec_vector(&ts, vector, pcb);
#endif
  } else {
    return ik_errno_to_code();
  }
}


/** --------------------------------------------------------------------
 ** Setting onwers, permissions, times.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_chown (ikptr pathname_bv, ikptr owner_fx, ikptr group_fx)
{
  char *  pathname;
  int     rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  errno    = 0;
  rv       = chown(pathname, unfix(owner_fx), unfix(group_fx));
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_fchown (ikptr fd, ikptr owner_fx, ikptr group_fx)
{
  int     rv;
  errno    = 0;
  rv       = fchown(unfix(fd), unfix(owner_fx), unfix(group_fx));
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_chmod (ikptr pathname_bv, ikptr mode_fx)
{
  char *        pathname;
  int           rv;
  mode_t        mode;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  mode     = unfix(mode_fx);
  errno    = 0;
  rv       = chmod(pathname, mode);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_fchmod (ikptr fd, ikptr mode_fx)
{
  int           rv;
  mode_t        mode;
  mode     = unfix(mode_fx);
  errno    = 0;
  rv       = fchmod(unfix(fd), mode);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_umask (ikptr mask_fx)
{
  mode_t  mask = unfix(mask_fx);
  mode_t  rv   = umask(mask);
  return fix((long) rv);
}
ikptr
ikrt_posix_getumask (void)
{
  mode_t  mask = umask(0);
  umask(mask);
  return fix((long) mask);
}
ikptr
ikrt_posix_utime (ikptr pathname_bv, ikptr atime_sec_fx, ikptr mtime_sec_fx)
{
  char *          pathname;
  struct utimbuf  T;
  int             rv;
  pathname  = (char*)(long)(pathname_bv+off_bytevector_data);
  T.actime  = unfix(atime_sec_fx);
  T.modtime = unfix(mtime_sec_fx);
  errno     = 0;
  rv        = utime(pathname, &T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_utimes (ikptr pathname_bv,
                   ikptr atime_sec_fx, ikptr atime_usec_fx,
                   ikptr mtime_sec_fx, ikptr mtime_usec_fx)
{
  char *          pathname;
  struct timeval  T[2];
  int             rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  T[0].tv_sec  = unfix(atime_sec_fx);
  T[0].tv_usec = unfix(atime_usec_fx);
  T[1].tv_sec  = unfix(mtime_sec_fx);
  T[1].tv_usec = unfix(mtime_usec_fx);
  errno        = 0;
  rv           = utimes(pathname, T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_lutimes (ikptr pathname_bv,
                    ikptr atime_sec_fx, ikptr atime_usec_fx,
                    ikptr mtime_sec_fx, ikptr mtime_usec_fx)
{
  char *          pathname;
  struct timeval  T[2];
  int             rv;
  pathname = (char*)(long)(pathname_bv+off_bytevector_data);
  T[0].tv_sec  = unfix(atime_sec_fx);
  T[0].tv_usec = unfix(atime_usec_fx);
  T[1].tv_sec  = unfix(mtime_sec_fx);
  T[1].tv_usec = unfix(mtime_usec_fx);
  errno        = 0;
  rv           = lutimes(pathname, T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_futimes (ikptr fd,
                    ikptr atime_sec_fx, ikptr atime_usec_fx,
                    ikptr mtime_sec_fx, ikptr mtime_usec_fx)
{
  struct timeval  T[2];
  int             rv;
  T[0].tv_sec  = unfix(atime_sec_fx);
  T[0].tv_usec = unfix(atime_usec_fx);
  T[1].tv_sec  = unfix(mtime_sec_fx);
  T[1].tv_usec = unfix(mtime_usec_fx);
  errno        = 0;
  rv           = futimes(unfix(fd), T);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}


/** --------------------------------------------------------------------
 ** Hard and symbolic links.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_link (ikptr old_pathname_bv, ikptr new_pathname_bv)
{
  char *        old_pathname = (char*)(long)(old_pathname_bv+off_bytevector_data);
  char *        new_pathname = (char*)(long)(new_pathname_bv+off_bytevector_data);
  int           rv;
  errno = 0;
  rv    = link(old_pathname, new_pathname);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_symlink (ikptr file_pathname_bv, ikptr link_pathname_bv)
{
  char *        file_pathname = (char*)(long)(file_pathname_bv+off_bytevector_data);
  char *        link_pathname = (char*)(long)(link_pathname_bv+off_bytevector_data);
  int           rv;
  errno = 0;
  rv    = symlink(file_pathname, link_pathname);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_readlink (ikptr link_pathname_bv, ikpcb * pcb)
{
  char *        link_pathname = (char*)(long)(link_pathname_bv+off_bytevector_data);
  size_t        max_len;
  int           rv;
  for (max_len=PATH_MAX;; max_len *= 2) {
    char        file_pathname[max_len];
    errno = 0;
    rv    = readlink(link_pathname, file_pathname, max_len);
    if (rv < 0) {
      return ik_errno_to_code();
    } else if (rv == max_len) {
      continue;
    } else {
      ikptr     bv  = ik_safe_alloc(pcb, align(disp_bytevector_data+rv+1)) + bytevector_tag;
      char *    data;
      ref(bv, off_bytevector_length) = fix(rv);
      data = (char*)(bv+off_bytevector_data);
      memcpy(data, file_pathname, rv);
      data[rv] = '\0';
      return bv;
    }
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_realpath (ikptr link_pathname_bv, ikpcb* pcb)
{
  char *        link_pathname;
  char *        file_pathname;
  char          buff[PATH_MAX];
  link_pathname = (char*)(long)(link_pathname_bv+off_bytevector_data);
  errno         = 0;
  file_pathname = realpath(link_pathname, buff);
  if (NULL == file_pathname) {
    return ik_errno_to_code();
  } else {
    int         len  = strlen(file_pathname);
    ikptr       bv   = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1)) + bytevector_tag;
    char *      data = (char*)(long)(bv+off_bytevector_data);
    ref(bv, off_bytevector_length) = fix(len);
    memcpy(data, file_pathname, len+1);
    return bv;
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_unlink (ikptr pathname_bv)
{
  char * pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  int    rv;
  errno = 0;
  rv    = unlink(pathname);
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_remove (ikptr pathname_bv)
{
  char * pathname = (char*)(long)(pathname_bv + off_bytevector_data);
  int    rv;
  errno = 0;
  rv    = remove(pathname);
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}

ikptr
ikrt_posix_rename (ikptr old_pathname_bv, ikptr new_pathname_bv)
{
  char *        old_pathname = (char*)(long)(old_pathname_bv+off_bytevector_data);
  char *        new_pathname = (char*)(long)(new_pathname_bv+off_bytevector_data);
  int           rv;
  errno = 0;
  rv    = rename(old_pathname, new_pathname);
  if (0 == rv) {
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}


/** --------------------------------------------------------------------
 ** File system directories.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_mkdir (ikptr pathname_bv, ikptr mode)
{
  char *        pathname;
  int           rv;
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  rv       = mkdir(pathname, unfix(mode));
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_rmdir (ikptr pathname_bv)
{
  char *        pathname;
  int           rv;
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  rv       = rmdir(pathname);
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_getcwd (ikpcb * pcb)
{
  size_t        max_len;
  char *        pathname;
  for (max_len=256;; max_len*=2) {
    char        buffer[max_len];
    errno    = 0;
    pathname = getcwd(buffer, max_len);
    if (NULL == pathname) {
      if (ERANGE == errno)
        continue;
      else
        return ik_errno_to_code();
    } else {
      int       len  = strlen(pathname);
      ikptr     bv   = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1)) + bytevector_tag;
      char *    data = VICARE_BYTEVECTOR_DATA_CHARP(bv);
      ref(bv, off_bytevector_length) = fix(len);
      memcpy(data, pathname, len+1);
      return bv;
    }
  }
}
ikptr
ikrt_posix_chdir (ikptr pathname_bv)
{
  char *        pathname;
  int           rv;
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  rv       = chdir(pathname);
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_fchdir (ikptr fd)
{
  int           rv;
  errno    = 0;
  rv       = fchdir(unfix(fd));
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_opendir (ikptr pathname_bv, ikpcb * pcb)
{
  char *        pathname;
  DIR *         stream;
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  stream   = opendir(pathname);
  if (NULL == stream) {
    return ik_errno_to_code();
  } else {
    ikptr       pointer = make_pointer((long)stream, pcb);
    return pointer;
  }
}
ikptr
ikrt_posix_fdopendir (ikptr fd, ikpcb * pcb)
{
  DIR *         stream;
  errno    = 0;
  stream   = fdopendir(unfix(fd));
  if (NULL == stream) {
    return ik_errno_to_code();
  } else {
    ikptr       pointer = make_pointer((long)stream, pcb);
    return pointer;
  }
}
ikptr
ikrt_posix_readdir (ikptr pointer, ikpcb * pcb)
{
  DIR *           stream = (DIR *) ref(pointer, off_pointer_data);
  struct dirent * entry;
  errno = 0;
  entry = readdir(stream);
  if (NULL == entry) {
    closedir(stream);
    if (0 == errno)
      return false_object;
    else
      return ik_errno_to_code();
  } else {
    /* The  only field  in  "struct dirent"  we  can trust  to exist  is
       "d_name".   Notice that  the documentation  of glibc  describes a
       "d_namlen"  field  which  does   not  exist  on  Linux,  see  the
       "readdir(3)" manual page. */
    int     len  = strlen(entry->d_name);
    ikptr   bv   = ik_safe_alloc(pcb, align(disp_bytevector_data+len+1)) + bytevector_tag;
    char *  data = VICARE_BYTEVECTOR_DATA_CHARP(bv);
    ref(bv, off_bytevector_length) = fix(len);
    memcpy(data, entry->d_name, len+1);
    return bv;
  }
}
ikptr
ikrt_posix_closedir (ikptr pointer, ikpcb * pcb)
{
  DIR *  stream = (DIR *) ref(pointer, off_pointer_data);
  int    rv;
  errno = 0;
  rv    = closedir(stream);
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_rewinddir (ikptr pointer)
{
  DIR *  stream = (DIR *) ref(pointer, off_pointer_data);
  rewinddir(stream);
  return void_object;
}
ikptr
ikrt_posix_telldir (ikptr pointer, ikpcb * pcb)
{
  DIR *  stream = (DIR *) ref(pointer, off_pointer_data);
  long   pos;
  pos = telldir(stream);
  return s_to_number(pos, pcb);
}
ikptr
ikrt_posix_seekdir (ikptr pointer, ikptr pos_num)
{
  DIR *  stream = (DIR *) ref(pointer, off_pointer_data);
  long   pos    = extract_num(pos_num);
  seekdir(stream, pos);
  return void_object;
}


/** --------------------------------------------------------------------
 ** File descriptors.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_open (ikptr pathname_bv, ikptr flags, ikptr mode)
{
  char *        pathname;
  int           rv;
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  rv       = open(pathname, unfix(flags), unfix(mode));
  if (0 <= rv)
    return fix(rv);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_close (ikptr fd)
{
  int           rv;
  errno    = 0;
  rv       = close(unfix(fd));
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_read (ikptr fd, ikptr buffer_bv, ikptr size_fx)
{
  void *        buffer;
  size_t        size;
  ssize_t       rv;
  buffer   = VICARE_BYTEVECTOR_DATA_VOIDP(buffer_bv);
  size     = (size_t)unfix(size_fx);
  errno    = 0;
  rv       = read(unfix(fd), buffer, size);
  if (0 <= rv)
    return fix(rv);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_pread (ikptr fd, ikptr buffer_bv, ikptr size_fx, ikptr off_num)
{
  void *        buffer;
  size_t        size;
  off_t         off;
  ssize_t       rv;
  buffer   = VICARE_BYTEVECTOR_DATA_VOIDP(buffer_bv);
  size     = (size_t)unfix(size_fx);
  off      = (off_t) extract_num_longlong(off_num);
  errno    = 0;
  rv       = pread(unfix(fd), buffer, size, off);
  if (0 <= rv)
    return fix(rv);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_write (ikptr fd, ikptr buffer_bv, ikptr size_fx)
{
  void *        buffer;
  size_t        size;
  ssize_t       rv;
  buffer   = VICARE_BYTEVECTOR_DATA_VOIDP(buffer_bv);
  size     = (size_t)unfix(size_fx);
  errno    = 0;
  rv       = write(unfix(fd), buffer, size);
  if (0 <= rv)
    return fix(rv);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_pwrite (ikptr fd, ikptr buffer_bv, ikptr size_fx, ikptr off_num)
{
  void *        buffer;
  size_t        size;
  off_t         off;
  ssize_t       rv;
  buffer   = VICARE_BYTEVECTOR_DATA_VOIDP(buffer_bv);
  size     = (size_t)unfix(size_fx);
  off      = (off_t) extract_num_longlong(off_num);
  errno    = 0;
  rv       = pwrite(unfix(fd), buffer, size, off);
  if (0 <= rv)
    return fix(rv);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_lseek (ikptr fd, ikptr off_num, ikptr whence_fx, ikpcb * pcb)
{
  off_t         off;
  off_t         rv;
  off    = extract_num_longlong(off_num);
  errno  = 0;
  rv     = lseek(unfix(fd), off, unfix(whence_fx));
  if (0 <= rv)
    return sll_to_number((long long)rv, pcb);
  else
    return ik_errno_to_code();
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_readv (ikptr fd, ikptr buffers, ikpcb * pcb)
{
  int           number_of_buffers = ik_list_length(buffers);
  {
    struct iovec  bufs[number_of_buffers];
    ikptr         bv;
    int           i;
    ssize_t       rv;
    for (i=0; pair_tag == tagof(buffers); buffers=ref(buffers, off_cdr), ++i) {
      bv      = ref(buffers, off_car);
      bufs[i].iov_base = VICARE_BYTEVECTOR_DATA_VOIDP(bv);
      bufs[i].iov_len  = unfix(VICARE_BYTEVECTOR_LENGTH_FX(bv));
    }
    errno    = 0;
    rv       = readv(unfix(fd), bufs, number_of_buffers);
    if (0 <= rv)
      return sll_to_number((long long)rv, pcb);
    else
      return ik_errno_to_code();
  }
}
ikptr
ikrt_posix_writev (ikptr fd, ikptr buffers, ikpcb * pcb)
{
  int           number_of_buffers = ik_list_length(buffers);
  {
    struct iovec  bufs[number_of_buffers];
    ikptr         bv;
    int           i;
    ssize_t       rv;
    for (i=0; pair_tag == tagof(buffers); buffers=ref(buffers, off_cdr), ++i) {
      bv      = ref(buffers, off_car);
      bufs[i].iov_base = VICARE_BYTEVECTOR_DATA_VOIDP(bv);
      bufs[i].iov_len  = unfix(VICARE_BYTEVECTOR_LENGTH_FX(bv));
    }
    errno    = 0;
    rv       = writev(unfix(fd), bufs, number_of_buffers);
    if (0 <= rv)
      return sll_to_number((long long)rv, pcb);
    else
      return ik_errno_to_code();
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_select (ikptr nfds_fx,
                   ikptr read_fds_ell, ikptr write_fds_ell, ikptr except_fds_ell,
                   ikptr sec, ikptr usec,
                   ikpcb * pcb)
{
  ikptr                 L;      /* iterator for input lists */
  ikptr                 R;      /* output list of read-ready fds */
  ikptr                 W;      /* output list of write-ready fds */
  ikptr                 E;      /* output list of except-ready fds */
  ikptr                 vec;    /* the vector to be returned to the caller */
  fd_set                read_fds;
  fd_set                write_fds;
  fd_set                except_fds;
  struct timeval        timeout;
  int                   fd, nfds=0;
  int                   rv;
  /* Fill the fdset for read-ready descriptors. */
  FD_ZERO(&read_fds);
  for (L=read_fds_ell; pair_tag == tagof(L); L = ref(L, off_cdr)) {
    fd = unfix(ref(L, off_car));
    if (nfds < fd)
      nfds = fd;
    FD_SET(fd, &read_fds);
  }
  /* Fill the fdset for write-ready descriptors. */
  FD_ZERO(&write_fds);
  for (L=write_fds_ell; pair_tag == tagof(L); L = ref(L, off_cdr)) {
    fd = unfix(ref(L, off_car));
    if (nfds < fd)
      nfds = fd;
    FD_SET(fd, &write_fds);
  }
  /* Fill the fdset for except-ready descriptors. */
  FD_ZERO(&except_fds);
  for (L=except_fds_ell; pair_tag == tagof(L); L = ref(L, off_cdr)) {
    fd = unfix(ref(L, off_car));
    if (nfds < fd)
      nfds = fd;
    FD_SET(fd, &except_fds);
  }
  /* Perform the selection. */
  if (false_object == nfds_fx)
    ++nfds;
  else
    nfds = unfix(nfds_fx);
  timeout.tv_sec  = unfix(sec);
  timeout.tv_usec = unfix(usec);
  errno = 0;
  rv    = select(nfds, &read_fds, &write_fds, &except_fds, &timeout);
  if (0 == rv) { /* timeout has expired */
    return fix(0);
  } else if (-1 == rv) { /* an error occurred */
    return ik_errno_to_code();
  } else { /* success, let's harvest the fds */
    /* Build the vector  to be returned and prevent  it from being garbage
       collected while building other objects. */
    vec = ik_safe_alloc(pcb, align(disp_vector_data+3*wordsize)) + vector_tag;
    ref(vec, off_vector_length) = fix(3);
    ref(vec, off_vector_data+0*wordsize) = null_object;
    ref(vec, off_vector_data+1*wordsize) = null_object;
    ref(vec, off_vector_data+2*wordsize) = null_object;
    pcb->root0 = &vec;
    {
      /* Build a list of read-ready file descriptors. */
      for (L=read_fds_ell, R=null_object; pair_tag == tagof(L); L=ref(L,off_cdr)) {
        ikptr fdx = ref(L, off_car);
        if (FD_ISSET(unfix(fdx), &read_fds)) {
          ikptr P = ik_safe_alloc(pcb, align(pair_size)) + pair_tag;
          ref(P, off_car) = fdx;
          ref(P, off_cdr) = R;
          ref(vec, off_vector_data+0*wordsize) = P;
          R = P;
        }
      }
      /* Build a list of write-ready file descriptors. */
      for (L=write_fds_ell, W=null_object; pair_tag == tagof(L); L = ref(L, off_cdr)) {
        ikptr fdx = ref(L, off_car);
        if (FD_ISSET(unfix(fdx), &write_fds)) {
          ikptr P = ik_safe_alloc(pcb, align(pair_size)) + pair_tag;
          ref(P, off_car) = fdx;
          ref(P, off_cdr) = W;
          ref(vec, off_vector_data+1*wordsize) = W = P;
        }
      }
      /* Build a list of except-ready file descriptors. */
      for (L=except_fds_ell, E=null_object; pair_tag == tagof(L); L = ref(L, off_cdr)) {
        ikptr fdx = ref(L, off_car);
        if (FD_ISSET(unfix(fdx), &except_fds)) {
          ikptr P = ik_safe_alloc(pcb, align(pair_size)) + pair_tag;
          ref(P, off_car) = fdx;
          ref(P, off_cdr) = E;
          ref(vec, off_vector_data+2*wordsize) = E = P;
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
  ikptr                 vec;    /* the vector to be returned to the caller */
  fd_set                read_fds;
  fd_set                write_fds;
  fd_set                except_fds;
  struct timeval        timeout;
  int                   fd;
  int                   rv;
  FD_ZERO(&read_fds);
  FD_ZERO(&write_fds);
  FD_ZERO(&except_fds);
  fd = unfix(fdx);
  FD_SET(fd, &read_fds);
  FD_SET(fd, &write_fds);
  FD_SET(fd, &except_fds);
  timeout.tv_sec  = unfix(sec);
  timeout.tv_usec = unfix(usec);
  errno = 0;
  rv    = select(1+fd, &read_fds, &write_fds, &except_fds, &timeout);
  if (0 == rv) { /* timeout has expired */
    return fix(0);
  } else if (-1 == rv) { /* an error occurred */
    return ik_errno_to_code();
  } else { /* success, let's harvest the events */
    /* Build the vector to be returned. */
    vec = ik_safe_alloc(pcb, align(disp_vector_data+3)) + vector_tag;
    ref(vec, off_vector_length) = fix(3);
    ref(vec, off_vector_data+0*wordsize) = (FD_ISSET(fd, &read_fds))?   fdx : false_object;
    ref(vec, off_vector_data+1*wordsize) = (FD_ISSET(fd, &write_fds))?  fdx : false_object;
    ref(vec, off_vector_data+2*wordsize) = (FD_ISSET(fd, &except_fds))? fdx : false_object;
    return vec;
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_fcntl (ikptr fd, ikptr command, ikptr arg)
{

  int           rv = -1;
  errno = 0;
  if (is_fixnum(arg)) {
    long        val = (long)unfix(arg);
    rv = fcntl(unfix(fd), unfix(command), val);
  } else if (false_object == arg) {
    rv = fcntl(unfix(fd), unfix(command));
  } else {
    void *      val = (void *) ref(arg, off_pointer_data);
    rv = fcntl(unfix(fd), unfix(command), val);
  }
  if (-1 == rv)
    return ik_errno_to_code();
  else
    return fix(rv);
}
ikptr
ikrt_posix_ioctl (ikptr fd, ikptr command, ikptr arg)
{

  int           rv = -1;
  errno = 0;
  if (is_fixnum(arg)) {
    long        val = (long)unfix(arg);
    rv = ioctl(unfix(fd), unfix(command), val);
  } else if (false_object == arg) {
    rv = ioctl(unfix(fd), unfix(command));
  } else {
    void *      val = (void *) ref(arg, off_pointer_data);
    rv = ioctl(unfix(fd), unfix(command), val);
  }
  if (-1 == rv)
    return ik_errno_to_code();
  else
    return fix(rv);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_dup (ikptr fd)
{

  int           rv;
  errno = 0;
  rv    = dup(unfix(fd));
  if (-1 == rv)
    return ik_errno_to_code();
  else
    return fix(rv);
}
ikptr
ikrt_posix_dup2 (ikptr old, ikptr new)
{

  int           rv;
  errno = 0;
  rv    = dup2(unfix(old), unfix(new));
  if (-1 == rv)
    return ik_errno_to_code();
  else
    return fix(0);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_pipe (ikpcb * pcb)
{
  int           rv;
  int           fds[2];
  errno = 0;
  rv    = pipe(fds);
  if (-1 == rv)
    return ik_errno_to_code();
  else {
    ikptr pair = ik_safe_alloc(pcb, align(pair_size)) + pair_tag;
    ref(pair, off_car) = fix(fds[0]);
    ref(pair, off_cdr) = fix(fds[1]);
    return pair;
  }
}
ikptr
ikrt_posix_mkfifo (ikptr pathname_bv, ikptr mode)
{
  char *        pathname;
  int           rv;
  pathname = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  errno    = 0;
  rv       = mkfifo(pathname, unfix(mode));
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}


/** --------------------------------------------------------------------
 ** Network sockets.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_posix_bind (ikptr sock, ikptr sockaddr_bv)
{
  struct sockaddr *     addr;
  socklen_t             len;
  int                   rv;
  addr  = VICARE_BYTEVECTOR_DATA_VOIDP(sockaddr_bv);
  len   = unfix(VICARE_BYTEVECTOR_LENGTH_FX(sockaddr_bv));
  errno = 0;
  rv    = bind(unfix(sock), addr, len);
  if (0 == rv)
    return fix(0);
  else
    return ik_errno_to_code();
}
ikptr
ikrt_posix_getsockname (ikptr sock, ikpcb * pcb)
{
  char                  buffer[1024]; /* enough? */
  struct sockaddr *     addr = (void *)buffer;
  socklen_t             len;
  int                   rv;
  errno = 0;
  rv    = getsockname(unfix(sock), addr, &len);
  if (0 == rv) {
    ikptr  bv   = ik_bytevector_alloc(pcb, len);
    char * data = VICARE_BYTEVECTOR_DATA_CHARP(bv);
    buffer[len] = '\0';
    memcpy(data, addr, len+1);
    return bv;
  } else {
    return ik_errno_to_code();
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_make_sockaddr_un (ikptr pathname_bv, ikpcb * pcb)
{
  char *                pathname;
  long                  pathname_len;
  pathname     = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
  pathname_len = unfix(VICARE_BYTEVECTOR_LENGTH_FX(pathname_bv));
  {
#undef SIZE
#define SIZE    (sizeof(struct sockaddr_un)+pathname_len) /* better safe than sorry */
    uint8_t               buffer[SIZE];
    struct sockaddr_un *  name = (void *)buffer;
    ikptr                 addr_bv;
    long                  addr_len;
    char *                data;
    name->sun_family = AF_LOCAL;
    memcpy(name->sun_path, pathname, pathname_len+1);
    addr_len = SUN_LEN(name);
    addr_bv  = ik_bytevector_alloc(pcb, addr_len);
    data     = VICARE_BYTEVECTOR_DATA_CHARP(addr_bv);
    memcpy(data, name, addr_len);
    return addr_bv;
  }
}
ikptr
ikrt_posix_sockaddr_un_pathname (ikptr addr_bv, ikpcb * pcb)
{
  struct sockaddr_un *  addr = VICARE_BYTEVECTOR_DATA_VOIDP(addr_bv);
  if (AF_LOCAL == addr->sun_family) {
    ikptr               pathname_bv;
    long                pathname_len;
    char *              pathname;
    pathname_len = strlen(addr->sun_path);
    pathname_bv  = ik_bytevector_alloc(pcb, pathname_len);
    pathname     = VICARE_BYTEVECTOR_DATA_CHARP(pathname_bv);
    memcpy(pathname, addr->sun_path, pathname_len+1);
    return pathname_bv;
  } else {
    return false_object;
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_make_sockaddr_in (ikptr host_address_bv, ikptr port, ikpcb * pcb)
{
  struct in_addr *      host_address = (void *)VICARE_BYTEVECTOR_DATA_CHARP(host_address_bv);
  struct sockaddr_in *  socket_address;
  ikptr                 socket_address_bv;
#undef BV_LEN
#define BV_LEN  sizeof(struct sockaddr_in)
  socket_address_bv          = ik_bytevector_alloc(pcb, BV_LEN);
  socket_address             = VICARE_BYTEVECTOR_DATA_VOIDP(socket_address_bv);
  socket_address->sin_family = AF_INET;
  socket_address->sin_port   = (unsigned short int)unfix(port);
  memcpy(&(socket_address->sin_addr), host_address, sizeof(struct in_addr));
  return socket_address_bv;
}
ikptr
ikrt_posix_sockaddr_in_in_addr (ikptr socket_address_bv, ikpcb * pcb)
{
  struct sockaddr_in *  socket_address = VICARE_BYTEVECTOR_DATA_VOIDP(socket_address_bv);
  if (AF_INET == socket_address->sin_family) {
#undef BV_LEN
#define BV_LEN  sizeof(struct in_addr)
    ikptr               host_address_bv;
    struct in_addr *    host_address;
    host_address_bv = ik_bytevector_alloc(pcb, BV_LEN);
    host_address    = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
    memcpy(host_address, &(socket_address->sin_addr), BV_LEN);
    return host_address_bv;
  } else {
    return false_object;
  }
}
ikptr
ikrt_posix_sockaddr_in_in_port (ikptr socket_address_bv)
{
  struct sockaddr_in *  socket_address = VICARE_BYTEVECTOR_DATA_VOIDP(socket_address_bv);
  return (AF_INET == socket_address->sin_family)?
    fix((long)socket_address->sin_port) : false_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_make_sockaddr_in6 (ikptr host_address_bv, ikptr port, ikpcb * pcb)
{
  struct in6_addr *     host_address = (void *)VICARE_BYTEVECTOR_DATA_CHARP(host_address_bv);
  struct sockaddr_in6 * socket_address;
  ikptr                 socket_address_bv;
#undef BV_LEN
#define BV_LEN  sizeof(struct sockaddr_in6)
  socket_address_bv           = ik_bytevector_alloc(pcb, BV_LEN);
  socket_address              = VICARE_BYTEVECTOR_DATA_VOIDP(socket_address_bv);
  socket_address->sin6_family = AF_INET6;
  socket_address->sin6_port   = (unsigned short int)unfix(port);
  memcpy(&(socket_address->sin6_addr), host_address, sizeof(struct in6_addr));
  return socket_address_bv;
}
ikptr
ikrt_posix_sockaddr_in6_in6_addr (ikptr socket_address_bv, ikpcb * pcb)
{
  struct sockaddr_in6 *  socket_address = VICARE_BYTEVECTOR_DATA_VOIDP(socket_address_bv);
  if (AF_INET6 == socket_address->sin6_family) {
#undef BV_LEN
#define BV_LEN  sizeof(struct in6_addr)
    ikptr               host_address_bv;
    struct in6_addr *   host_address;
    host_address_bv = ik_bytevector_alloc(pcb, BV_LEN);
    host_address    =  VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
    memcpy(host_address, &(socket_address->sin6_addr), BV_LEN);
    return host_address_bv;
  } else {
    return false_object;
  }
}
ikptr
ikrt_posix_sockaddr_in6_in6_port (ikptr socket_address_bv)
{
  struct sockaddr_in6 *  socket_address = VICARE_BYTEVECTOR_DATA_VOIDP(socket_address_bv);
  return (AF_INET6 == socket_address->sin6_family)?
    fix((long)socket_address->sin6_port) : false_object;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_in6addr_loopback (ikpcb * pcb)
{
  static const struct in6_addr constant_host_address = IN6ADDR_LOOPBACK_INIT;
  ikptr                 host_address_bv;
  struct in6_addr *     host_address;
#undef BV_LEN
#define BV_LEN          sizeof(struct in6_addr)
  host_address_bv = ik_bytevector_alloc(pcb, BV_LEN);
  host_address    = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
  memcpy(host_address, &constant_host_address, BV_LEN);
  return host_address_bv;
}
ikptr
ikrt_posix_in6addr_any (ikpcb * pcb)
{
  static const struct in6_addr constant_host_address = IN6ADDR_ANY_INIT;
  ikptr                 host_address_bv;
  struct in6_addr *     host_address;
#undef BV_LEN
#define BV_LEN          sizeof(struct in6_addr)
  host_address_bv = ik_bytevector_alloc(pcb, BV_LEN);
  host_address    = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
  memcpy(host_address, &constant_host_address, BV_LEN);
  return host_address_bv;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_inet_aton (ikptr dotted_quad_bv, ikpcb * pcb)
{
  void *                dotted_quad;
  ikptr                 host_address_bv;
  struct in_addr *      host_address;
  int                   rv;
#undef BV_LEN
#define BV_LEN          sizeof(struct in_addr)
  host_address_bv = ik_bytevector_alloc(pcb, BV_LEN);
  host_address    = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
  dotted_quad     = VICARE_BYTEVECTOR_DATA_VOIDP(dotted_quad_bv);
  rv = inet_aton(dotted_quad, host_address);
  return (0 != rv)? host_address_bv : false_object;
}
ikptr
ikrt_posix_inet_ntoa (ikptr host_address_bv, ikpcb * pcb)
{
  struct in_addr *      host_address;
  ikptr                 dotted_quad_bv;
  char *                dotted_quad;
  char *                data;
  long                  data_len;
  host_address   = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
  data           = inet_ntoa(*host_address);
  data_len       = strlen(data);
  dotted_quad_bv = ik_bytevector_alloc(pcb, data_len);
  dotted_quad    = VICARE_BYTEVECTOR_DATA_CHARP(dotted_quad_bv);
  memcpy(dotted_quad, data, data_len);
  return dotted_quad_bv;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_inet_pton (ikptr af, ikptr presentation_bv, ikpcb * pcb)
{
  void *        presentation;
  ikptr         host_address_bv;
  int           rv;
  switch (unfix(af)) {
  case AF_INET:
    {
#undef BV_LEN
#define BV_LEN          sizeof(struct in_addr)
      struct in_addr *      host_address;
      host_address_bv = ik_bytevector_alloc(pcb, BV_LEN);
      host_address    = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
      presentation    = VICARE_BYTEVECTOR_DATA_VOIDP(presentation_bv);
      rv = inet_pton(unfix(af), presentation, host_address);
      return (0 < rv)? host_address_bv : false_object;
    }
  case AF_INET6:
    {
#undef BV_LEN
#define BV_LEN          sizeof(struct in6_addr)
      struct in6_addr *      host_address;
      host_address_bv = ik_bytevector_alloc(pcb, BV_LEN);
      host_address    = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
      presentation    = VICARE_BYTEVECTOR_DATA_VOIDP(presentation_bv);
      rv = inet_pton(unfix(af), presentation, host_address);
      return (0 < rv)? host_address_bv : false_object;
    }
  default:
    return false_object;
  }
}
ikptr
ikrt_posix_inet_ntop (ikptr af, ikptr host_address_bv, ikpcb * pcb)
{
  switch (unfix(af)) {
  case AF_INET:
  case AF_INET6:
    {
      void *            host_address;
      socklen_t         buflen = 256;
      char              buffer[buflen];
      const char *      rv;
      host_address = VICARE_BYTEVECTOR_DATA_VOIDP(host_address_bv);
      rv = inet_ntop(unfix(af), host_address, buffer, buflen);
      if (NULL != rv) {
        ikptr     presentation_bv;
        char *    presentation;
        long      presentation_len;
        presentation_len = strlen(buffer);
        presentation_bv  = ik_bytevector_alloc(pcb, presentation_len);
        presentation     = VICARE_BYTEVECTOR_DATA_CHARP(presentation_bv);
        memcpy(presentation, buffer, presentation_len);
        return presentation_bv;
      } else {
        return false_object;
      }
    }
  default:
    return false_object;
  }
}

/* ------------------------------------------------------------------ */

static ikptr
hostent_to_struct (ikptr rtd, struct hostent * src, ikpcb * pcb)
{
  ikptr dst = ik_safe_alloc(pcb, align(disp_record_data+6*wordsize)) + vector_tag;
  pcb->root0 = &dst;
  ref(dst, off_record_rtd) = rtd;
#if 0
  ref(dst, off_record_data+0*wordsize) = false_object;
  ref(dst, off_record_data+1*wordsize) = false_object;
  ref(dst, off_record_data+2*wordsize) = false_object;
  ref(dst, off_record_data+3*wordsize) = false_object;
#else
  { /* store the official host name */
    long        name_len = strlen(src->h_name);
    ikptr       name_bv  = ik_bytevector_alloc(pcb, name_len);
    char *      name     = VICARE_BYTEVECTOR_DATA_CHARP(name_bv);
    memcpy(name, src->h_name, name_len+1);
    ref(dst, off_record_data+0*wordsize) = name_bv;
  }
  { /* store the list of aliases */
    ikptr       list_of_aliases = null_object;
    int         i;
    pcb->root1 = &list_of_aliases;
    for (i=0; NULL!=src->h_aliases[i]; ++i) {
      ikptr     pair      = ik_safe_alloc(pcb, align(pair_size)) + pair_tag;
      ref(pair, off_cdr)  = list_of_aliases;
      list_of_aliases     = pair;
      long      alias_len = strlen(src->h_aliases[i]);
      ikptr     alias_bv  = ik_bytevector_alloc(pcb, alias_len);
      char *    alias     = VICARE_BYTEVECTOR_DATA_CHARP(alias_bv);
      memcpy(alias, src->h_aliases[i], alias_len);
      ref(pair, off_car) = alias_bv;
    }
    ref(dst, off_record_data+1*wordsize) = list_of_aliases;
    pcb->root1 = NULL;
  }
  { /* store the host address type */
    ref(dst, off_record_data+2*wordsize) = fix(src->h_addrtype);
  }
  { /* store the host address structure length */
    ref(dst, off_record_data+3*wordsize) = fix(src->h_length);
  }
  ikptr first_addr;
  { /* store the reversed list of addresses */
    ikptr       list_of_addrs = null_object;
    int         i;
    pcb->root1 = &list_of_addrs;
    for (i=0; NULL!=src->h_addr_list[i]; ++i) {
      ikptr     pair     = ik_safe_alloc(pcb, align(pair_size)) + pair_tag;
      ref(pair, off_cdr) = list_of_addrs;
      list_of_addrs      = pair;
      ikptr     addr_bv  = ik_bytevector_alloc(pcb, src->h_length);
      char *    addr     = VICARE_BYTEVECTOR_DATA_CHARP(addr_bv);
      memcpy(addr, src->h_addr_list[i], src->h_length);
      ref(pair, off_car) = addr_bv;
      if (0 == i)
        first_addr = addr_bv;
    }
    pcb->root1 = NULL;
    ref(dst, off_record_data+4*wordsize) = list_of_addrs;
  }
  { /* store the first in the list of addresses */
    ref(dst, off_record_data+5*wordsize) = first_addr;
  }
#endif
  pcb->root0 = NULL;
  return dst;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_posix_gethostbyname (ikptr rtd, ikptr hostname_bv, ikpcb * pcb)
{
  char *                hostname;
  struct hostent *      rv;
  hostname = VICARE_BYTEVECTOR_DATA_CHARP(hostname_bv);
  errno    = 0;
  h_errno  = 0;
  rv       = gethostbyname(hostname);
  if (NULL != rv) {
    return hostent_to_struct(rtd, rv, pcb);
  } else {
    return fix(-h_errno);
  }
}


/** --------------------------------------------------------------------
 ** Time related functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_current_time(ikptr t){
  struct timeval s;
  gettimeofday(&s, 0);
  /* this will break in 8,727,224 years if we stay in 32-bit ptrs */
  ref(t, off_record_data + 0*wordsize) = fix(s.tv_sec / 1000000);
  ref(t, off_record_data + 1*wordsize) = fix(s.tv_sec % 1000000);
  ref(t, off_record_data + 2*wordsize) = fix(s.tv_usec);
  return t;
}

ikptr
ikrt_gmt_offset(ikptr t){
  time_t clock =
    unfix(ref(t, off_record_data + 0*wordsize)) * 1000000
    + unfix(ref(t, off_record_data + 1*wordsize));
  struct tm* m = gmtime(&clock);
  time_t gmtclock = mktime(m);
  return fix(clock - gmtclock);
  /*
  struct tm* m = localtime(&clock);
  ikptr r = fix(m->tm_gmtoff);
  return r;
  */
}
ikptr
ikrt_nanosleep(ikptr secs, ikptr nsecs /*, ikpcb* pcb */){
  struct timespec t;
  t.tv_sec =
    is_fixnum(secs)
      ? (unsigned long) unfix(secs)
      : ref(secs, off_bignum_data);
  t.tv_nsec =
    is_fixnum(nsecs)
      ? (unsigned long) unfix(nsecs)
      : ref(nsecs, off_bignum_data);
  return fix(nanosleep(&t, NULL));
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */



/* end of file */
