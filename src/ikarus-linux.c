/*
  Part of: Vicare
  Contents: interface to Linux functions
  Date: Mon Nov  7, 2011

  Abstract



  Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

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

#include "internals.h"
#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_SYS_EPOLL_H
#  include <sys/epoll.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#  include <sys/param.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif
#ifdef HAVE_SYS_SIGNALFD_H
#  include <sys/signalfd.h>
#endif
#ifdef HAVE_SYS_TIMERFD_H
#  include <sys/timerfd.h>
#endif
#ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif

/* file descriptors */
#define IK_FD_TO_NUM(fd)		IK_FIX(fd)
#define IK_NUM_TO_FD(fd)		IK_UNFIX(fd)

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called GNU+Linux specific function, %s", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return void_object; }


/** --------------------------------------------------------------------
 ** Process exit status.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_linux_WIFCONTINUED (ikptr fx_status)
{
#ifdef HAVE_WIFCONTINUED
  int   status = IK_UNFIX(fx_status);
  return (WIFCONTINUED(status))? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_linux_waitid (ikptr fx_idtype, ikptr fx_id, ikptr struct_info, ikptr fx_options)
{
#ifdef HAVE_WAITID
  idtype_t  idtype  = IK_UNFIX(fx_idtype);
  id_t      id      = IK_UNFIX(fx_id);
  siginfo_t info;
  int       options = IK_UNFIX(fx_options);
  int       retval;
  errno  = 0;
  retval = waitid(idtype, id, &info, options);
  if (0 <= retval) {
    IK_FIELD(struct_info, 0) = IK_FIX(info.si_pid);
    IK_FIELD(struct_info, 1) = IK_FIX(info.si_uid);
    IK_FIELD(struct_info, 2) = IK_FIX(info.si_signo);
    IK_FIELD(struct_info, 3) = IK_FIX(info.si_status);
    IK_FIELD(struct_info, 4) = IK_FIX(info.si_code);
    return struct_info;
  } else {
    return ik_errno_to_code();
  }
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Epoll.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_linux_epoll_create (ikptr s_size)
{
#ifdef HAVE_EPOLL_CREATE
  int   size = ik_integer_to_int(s_size);
  int	rv;
  errno = 0;
  rv    = epoll_create(size);
  return (-1 != rv)? IK_FD_TO_NUM(rv) : ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_create1 (ikptr s_flags)
{
#ifdef HAVE_EPOLL_CREATE1
  int   flags = ik_integer_to_int(s_flags);
  int	rv;
  errno = 0;
  rv    = epoll_create1(flags);
  return (-1 != rv)? IK_FD_TO_NUM(rv) : ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_ctl (ikptr s_epfd, ikptr s_op, ikptr s_fd, ikptr s_event_struct)
{
#ifdef HAVE_EPOLL_CTL
  struct epoll_event *	event = (false_object == s_event_struct)?
    NULL : IK_POINTER_DATA_VOIDP(s_event_struct);
  int	rv;
  errno = 0;
  rv    = epoll_ctl(IK_NUM_TO_FD(s_epfd),
		    ik_integer_to_int(s_op),
		    IK_NUM_TO_FD(s_fd), event);
  return (0 == rv)? IK_FIX(0) : ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_wait (ikptr s_epfd, ikptr s_events_array,
		       ikptr s_maxevents, ikptr s_timeout_ms, ikpcb * pcb)
{
#ifdef HAVE_EPOLL_WAIT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  int	rv;
  errno = 0;
  rv    = epoll_wait(IK_NUM_TO_FD(s_epfd), event,
		     ik_integer_to_int(s_maxevents),
		     ik_integer_to_int(s_timeout_ms));
  return (-1 != rv)? ika_integer_from_int(pcb, rv) : ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_linux_epoll_event_alloc (ikptr s_number_of_entries, ikpcb * pcb)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  void * event = malloc(IK_UNFIX(s_number_of_entries) * sizeof(struct epoll_event));
  return (event)? ika_pointer_alloc(pcb, (ik_ulong)event) : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_event_size (void)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  return IK_FIX(sizeof(struct epoll_event));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_linux_epoll_event_set_events (ikptr s_events_array, ikptr s_index, ikptr s_field_events)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  event[IK_UNFIX(s_index)].events = ik_integer_to_uint32(s_field_events);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_event_ref_events (ikptr s_events_array, ikptr s_index, ikpcb * pcb)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  return ika_integer_from_uint32(pcb, event[IK_UNFIX(s_index)].events);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_linux_epoll_event_set_data_ptr (ikptr s_events_array, ikptr s_index, ikptr s_field_ptr)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  event[IK_UNFIX(s_index)].data.ptr = IK_POINTER_DATA_VOIDP(s_field_ptr);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_event_ref_data_ptr (ikptr s_events_array, ikptr s_index, ikpcb * pcb)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  return ika_pointer_alloc(pcb, (ik_ulong)event[IK_UNFIX(s_index)].data.ptr);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_linux_epoll_event_set_data_fd (ikptr s_events_array, ikptr s_index, ikptr s_field_fd)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  event[IK_UNFIX(s_index)].data.fd = IK_NUM_TO_FD(s_field_fd);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_event_ref_data_fd (ikptr s_events_array, ikptr s_index, ikpcb * pcb)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  return IK_FD_TO_NUM(event[IK_UNFIX(s_index)].data.fd);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_linux_epoll_event_set_data_u32 (ikptr s_events_array, ikptr s_index, ikptr s_field_u32)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  event[IK_UNFIX(s_index)].data.u32 = ik_integer_to_uint32(s_field_u32);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_event_ref_data_u32 (ikptr s_events_array, ikptr s_index, ikpcb * pcb)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  return ika_integer_from_uint32(pcb, event[IK_UNFIX(s_index)].data.u32);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_linux_epoll_event_set_data_u64 (ikptr s_events_array, ikptr s_index, ikptr s_field_u64)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  event[IK_UNFIX(s_index)].data.u64 = ik_integer_to_uint64(s_field_u64);
  return void_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_epoll_event_ref_data_u64 (ikptr s_events_array, ikptr s_index, ikpcb * pcb)
{
#ifdef HAVE_STRUCT_EPOLL_EVENT
  struct epoll_event *	event = IK_POINTER_DATA_VOIDP(s_events_array);
  return ika_integer_from_uint64(pcb, event[IK_UNFIX(s_index)].data.u64);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Signal file descriptors.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_linux_signalfd (ikptr s_fd, ikptr s_mask, ikptr s_flags)
{
#ifdef HAVE_SIGNALFD
  int		fd = IK_NUM_TO_FD(s_fd);
  sigset_t	mask;
  int		i, mask_len;
  int		rv;
  sigemptyset(&mask);
  mask_len = IK_VECTOR_LENGTH(s_mask);
  for (i=0; i<mask_len; ++i) {
    errno = 0;
    rv = sigaddset(&mask, IK_UNFIX(IK_ITEM(s_mask, i)));
    if (-1 == rv)
      return ik_errno_to_code();
  }
  errno = 0;
  rv    = signalfd(fd, &mask, IK_UNFIX(s_flags));
  return (-1 != rv)? IK_FD_TO_NUM(rv) : ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_read_signalfd_siginfo (ikptr s_fd, ikptr s_info, ikpcb * pcb)
{
#ifdef HAVE_SIGNALFD
  struct signalfd_siginfo	info;
  ssize_t			rv;
  errno = 0;
  rv    = read(IK_NUM_TO_FD(s_fd), &info, sizeof(struct signalfd_siginfo));
  if (-1 != rv) {
    pcb->root0 = &s_info;
    {
      IK_ASS(IK_FIELD(s_info,  0), ika_integer_from_uint32(pcb, info.ssi_signo));
      IK_ASS(IK_FIELD(s_info,  1), ika_integer_from_sint32(pcb, info.ssi_errno));
      IK_ASS(IK_FIELD(s_info,  2), ika_integer_from_sint32(pcb, info.ssi_code));
      IK_ASS(IK_FIELD(s_info,  3), ika_integer_from_uint32(pcb, info.ssi_pid));
      IK_ASS(IK_FIELD(s_info,  4), ika_integer_from_uint32(pcb, info.ssi_uid));
      IK_ASS(IK_FIELD(s_info,  5), ika_integer_from_sint32(pcb, info.ssi_fd));
      IK_ASS(IK_FIELD(s_info,  6), ika_integer_from_uint32(pcb, info.ssi_tid));
      IK_ASS(IK_FIELD(s_info,  7), ika_integer_from_uint32(pcb, info.ssi_band));
      IK_ASS(IK_FIELD(s_info,  8), ika_integer_from_uint32(pcb, info.ssi_overrun));
      IK_ASS(IK_FIELD(s_info,  9), ika_integer_from_uint32(pcb, info.ssi_trapno));
      IK_ASS(IK_FIELD(s_info, 10), ika_integer_from_sint32(pcb, info.ssi_status));
      IK_ASS(IK_FIELD(s_info, 11), ika_integer_from_sint32(pcb, info.ssi_int));
      IK_ASS(IK_FIELD(s_info, 12), ika_integer_from_uint64(pcb, info.ssi_ptr));
      IK_ASS(IK_FIELD(s_info, 13), ika_integer_from_uint64(pcb, info.ssi_utime));
      IK_ASS(IK_FIELD(s_info, 14), ika_integer_from_uint64(pcb, info.ssi_stime));
      IK_ASS(IK_FIELD(s_info, 15), ika_integer_from_uint64(pcb, info.ssi_addr));
    }
    pcb->root0 = NULL;
    return IK_FIX(0);
  } else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Timer file descriptors.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_linux_timerfd_create (ikptr s_clockid, ikptr s_flags)
/* Interface to the  C function "timerfd_create()".  Create  a new timer
   object and a file descriptor that refers to that timer; if successful
   return  a fixnum  representing the  file descriptor,  else return  an
   encoded errno value.

   S_CLOCKID  must   be  one  among:   CLOCK_REALTIME,  CLOCK_MONOTONIC.
   S_FLAGS  can be  the  fixnum zero  or a  bitwise  OR combination  of:
   TFD_CLOEXEC, TFD_NONBLOCK. */
{
#ifdef HAVE_TIMERFD_CREATE
  int		rv;
  errno = 0;
  rv    = timerfd_create(IK_UNFIX(s_clockid), IK_UNFIX(s_flags));
  return (-1 != rv)? IK_FD_TO_NUM(rv) : ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_timerfd_settime (ikptr s_fd, ikptr s_flags, ikptr s_new, ikptr s_old, ikpcb * pcb)
/* Interface to the  C function "timerfd_settime()".  Start  or stop the
   timer referred to by the  file descriptor S_FD.  If successful return
   the fixnum zero; if an error occurs return an encoded "errno" value.

   S_FLAGS can  be either the  fixnum zero or  TFD_TIMER_ABSTIME.  S_NEW
   must be a valid instance of  STRUCT-ITIMERSPEC, and it is used to set
   the timer specification.   S_OLD can be false or a  valid instance of
   STRUCT-ITIMERSPEC;  when  given: it  is  filled  with the  old  timer
   specification. */
{
#ifdef HAVE_TIMERFD_SETTIME
  ikptr			s_it_interval = IK_FIELD(s_new, 0);
  ikptr			s_it_value    = IK_FIELD(s_new, 1);
  struct itimerspec	new;
  struct itimerspec	old = { { 0, 0 }, { 0, 0 } };
  int			rv;
  new.it_interval.tv_sec  = (time_t)ik_integer_to_long(IK_FIELD(s_it_interval, 0));
  new.it_interval.tv_nsec = ik_integer_to_long(IK_FIELD(s_it_interval, 1));
  new.it_value.tv_sec     = ik_integer_to_long(IK_FIELD(s_it_value,    0));
  new.it_value.tv_nsec    = ik_integer_to_long(IK_FIELD(s_it_value,    1));
  errno = 0;
  rv    = timerfd_settime(IK_NUM_TO_FD(s_fd), IK_UNFIX(s_flags), &new, &old);
  if (0 == rv) {
    if (false_object != s_old) {
      pcb->root0 = &s_old;
      {
	IK_ASS(IK_FIELD(IK_FIELD(s_old, 0), 0),
	       ika_integer_from_long(pcb, (long)old.it_interval.tv_sec));
	IK_ASS(IK_FIELD(IK_FIELD(s_old, 0), 1),
	       ika_integer_from_long(pcb, old.it_interval.tv_nsec));
	IK_ASS(IK_FIELD(IK_FIELD(s_old, 1), 0),
	       ika_integer_from_long(pcb, old.it_value.tv_sec));
	IK_ASS(IK_FIELD(IK_FIELD(s_old, 1), 1),
	       ika_integer_from_long(pcb, old.it_value.tv_nsec));
      }
      pcb->root0 = NULL;
    }
    return IK_FIX(0);
  } else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_linux_timerfd_gettime (ikptr s_fd, ikptr s_curr, ikpcb * pcb)
/* Interface  to  the  C  function  "timerfd_gettime()".   Retrieve  the
   current timer  specification associated to the  file descriptor S_FD.
   If successful  return S_CURR; if  an error occurs: return  an encoded
   "errno" value.

   S_CURR must  be a valid  instance of STRUCT-ITIMERSPEC: it  is filled
   with the current timer specification. */
{
#ifdef HAVE_TIMERFD_GETTIME
  struct itimerspec	curr;
  int			rv;
  errno = 0;
  rv    = timerfd_gettime(IK_NUM_TO_FD(s_fd), &curr);
  if (0 == rv) {
    pcb->root0 = &s_curr;
    {
      IK_ASS(IK_FIELD(IK_FIELD(s_curr, 0), 0),
	     ika_integer_from_long(pcb, (long)curr.it_interval.tv_sec));
      IK_ASS(IK_FIELD(IK_FIELD(s_curr, 0), 1),
	     ika_integer_from_long(pcb, curr.it_interval.tv_nsec));
      IK_ASS(IK_FIELD(IK_FIELD(s_curr, 1), 0),
	     ika_integer_from_long(pcb, curr.it_value.tv_sec));
      IK_ASS(IK_FIELD(IK_FIELD(s_curr, 1), 1),
	     ika_integer_from_long(pcb, curr.it_value.tv_nsec));
    }
    pcb->root0 = NULL;
    return IK_FIX(0);
  } else
    return ik_errno_to_code();
#else
  feature_failure(__func__);
#endif
}

/* end of file */
