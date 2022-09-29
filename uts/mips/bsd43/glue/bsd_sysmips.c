/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: bsd_sysmips.c,v 1.3.1.2 90/05/10 04:40:04 wje Exp $"

/*
 * MIPS specific syscall interface (BSD compatible)
 *
 * To Use:
 * 
 * Get Yourself a Number from the list, if you want to write any
 * code which will reside in sysmips(), use a number from 0x0 to 0xff.
 * If you just need a function vector, use a number above 0x100.
 *
 * Slap together a routine that takes up to four arguments.  Do not use
 * typical system call interface, you will be passed up to four arguments
 * using the normal procedure interface.  Remember also, you are not
 * to touch u.u_error in your function. You should return an error
 * value, and sysmips() will deal with u.u_error.
 *
 * If you will not be using the simple function vector mechanism, add your
 * number and appropriate code or call to your routine to the
 * switch statement in sysmips().
 *
 * Have a homebrew :-)
 */

#include "sys/types.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/immu.h"
#include "sys/sbd.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/errno.h"
#include "sys/utsname.h"
#include "sys/fpu.h"

#ifndef KERNEL
#define KERNEL 1
#endif KERNEL

#include "bsd43/mips/hwconf.h"
#include "bsd43/sys/sysmips.h"

extern unsigned fptype_word;
extern struct proc *fpowner;

extern int errsys();

/*
 *	Entry vector table
 */

int bsd_mipskopt();
int bsd_mipshwconf();
int mips_getrusage();
int mips_wait();
int bsd_cacheflush();
int bsd_cachectl();

int (*func_vector[])() = {
	bsd_mipskopt,
	bsd_mipshwconf,
	mips_getrusage,
	mips_wait,
	bsd_cacheflush,
	bsd_cachectl
	};

bsd_sysmips()
{
	register struct a {
		int vector;
		int arg1,arg2,arg3,arg4;
	} *uap;

	uap = (struct a *)u.u_ap;

	if (uap->vector < 0 || uap->vector > BSD43_MIPS_VECTOR_SIZE)
		{
		u.u_error = EINVAL;
		return;
		}

	if (uap->vector >= BSD43_MIPS_VECTOR_DIVIDER)
		{
		if ((uap->vector - BSD43_MIPS_VECTOR_DIVIDER) >
			(sizeof(func_vector) / sizeof(func_vector[0])))
			{
			u.u_error = EINVAL;
			return;
			}

		u.u_error = (*func_vector[uap->vector - BSD43_MIPS_VECTOR_DIVIDER])
			(uap->arg1,uap->arg2,uap->arg3,uap->arg4);
		} /* if */
	else
		switch (uap->vector)
			{
			case BSD43_MIPS_UNAME:
				u.u_error = bsd_copyout(&utsname, uap->arg1,
					sizeof(utsname));
				return;

			case BSD43_MIPS_FPSIGINTR:
				u.u_procp->p_fp = uap->arg1;
				u.u_error = 0; /* should I do this? */
				return;

			case BSD43_MIPS_FPU:
				/*
				 * You must be super-user to do this.
				 * If the argument is non-zero turn the fpu
				 * back on. Else turn it off.
				 */
				if (!suser())
					return;
				if(uap->arg1){
					fptype_word =
						bsd43_hwconf.fpu_processor.ri_uint &
						IRR_IMP_MASK;
				}
				else{
					if(fpowner != 0)
						checkfp(fpowner, 0);
					fptype_word = 0;
				}
				u.u_error = 0; /* should I do this? */
				return;

			case BSD43_MIPS_FIXADE:
				if(uap->arg1)
					u.u_procp->p_flag |= SFIXADE;
				else
					u.u_procp->p_flag &= ~SFIXADE;
				u.u_error = 0; /* should I do this? */
				return;

			default:
				u.u_error = EINVAL;
				return;
			} /* switch vector*/
	return;
} /* sysmips() */
