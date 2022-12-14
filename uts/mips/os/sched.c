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
#ident	"$Header: sched.c,v 1.14.1.4 90/05/10 05:52:58 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/param.h"
#include "sys/types.h"
#include "sys/psw.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "bsd/sys/time.h"
#include "sys/proc.h"
#include "sys/systm.h"
#include "sys/sysinfo.h"
#include "sys/var.h"
#include "sys/tuneable.h"
#include "sys/getpages.h"
#include "sys/debug.h"
#include "bsd43/sys/vmmeter.h"

extern unsigned int	sxbrkcnt;

/*	The scheduler sleeps on runout when there is no one
**	to swap in.  It sleeps on runin when it could not 
**	find space to swap someone in or after swapping
**	someone in.
*/

char	runout;
char	runin;

/*
 * Memory scheduler
 */

sched()
{
	register struct proc *rp, *p;
	register outage, inage;
	int maxbad;
	int tmp;
	int runable;
	int ospl;
	proc_t	*justloaded = NULL;

	/*
	 * find user to swap in;
	 * of users ready, select one out longest
	 */

	/*
	 * Otherwise, find user to reactivate
	 * Of users ready, select one out longest
	 */

loop:
	ospl = splclock();
loop_nospl:
	outage = -20000;
	runable = 0;
	for (rp = &proc[0]; rp < (struct proc *)v.ve_proc; rp++) {
		if ((rp->p_stat==SXBRK ||
		    (rp->p_stat==SRUN && (rp->p_flag&SLOAD) == 0)) &&
		    (int)rp->p_time > outage) {
			p = rp;
			outage = rp->p_time;
		}
		if (rp->p_stat == SRUN  &&
		   (rp->p_flag & (SLOAD | SSYS)) == SLOAD)
			runable = 1;
	}

	/*
	 * If there is no one there, wait.
	 */
	if (outage == -20000) {
		runout++;
		sleep(&runout, PSWP);
		goto loop_nospl;
	}
	splx(ospl);

	/*
	 * See if there is memory for that process;
	 * if so, let it go and then delay in order to
	 * let things settle
	 */

	if (!runable  ||  freemem > minfree) {
		if (p->p_stat == SXBRK) {
			ASSERT(p->p_wchan == 0);
			p->p_stat = SRUN;
			--sxbrkcnt;
			setrq(p);
		}
		p->p_flag |= SLOAD;
		p->p_time = 0;
		cnt.v_swpin++;
		if (freemem > minfree)
			goto delay;
		justloaded = p;
	}

	/*
	 * none found.
	 * look around for memory.
	 * Select the largest of those sleeping
	 * at bad priority; if none, select the oldest.
	 */


	p = NULL;
	maxbad = 0;
	inage = 0;
	ospl = splclock();
	for (rp = &proc[0]; rp < (struct proc *)v.ve_proc; rp++) {
		if (rp->p_stat==SZOMB)
			continue;
		if ((rp->p_flag&(SSYS|SLOAD|SLOCK))!=SLOAD)
			continue;
		if (rp == justloaded)
			continue;
		if (rp->p_stat==SSLEEP || rp->p_stat==SSTOP) {
			tmp = rp->p_pri - PZERO + rp->p_time;
			if (maxbad < tmp) {
				p = rp;
				maxbad = tmp;
			}
		} else
		if (maxbad<=0 && (rp->p_stat==SRUN || rp->p_stat==SXBRK)) {
			tmp = rp->p_time + rp->p_nice - NZERO;
			if (tmp > inage) {
				p = rp;
				inage = tmp;
			}
		}
	}
	splx(ospl);
	/*
	 * Swap out and deactivate process if
	 * sleeping at bad priority, or if it has spent at least
	 * 2 seconds in memory and the other process has spent
	 * at least 2 seconds out.
	 * Otherwise wait a bit and try again.
	 */

	if (maxbad > 0 || (outage>=2 && inage>=2)) {
		if (maxbad > 0) {
			if (!(p->p_stat == SSLEEP || p->p_stat == SSTOP))
				goto loop;
		} else if (p->p_stat != SRUN  &&  p->p_stat != SXBRK) {
			goto loop;
		}
		if (p->p_flag & SLOCK)
			goto loop;

		p->p_flag &= ~SLOAD;
		if (!swapout(p)) {
			p->p_flag |= SLOAD;
			goto delay;
		} else {
			p->p_time = 0;
			goto loop;
		}
			
	}

	/*
	 * Delay for 1 second and look again later
	 */
delay:
	ospl = splclock();
	runin++;
	sleep(&runin, PSWP);
	goto loop_nospl;
}


/*	Swap out process p
 */

swapout(p)
register struct proc *p;
{
	register preg_t *prp;
	register reg_t *rp;
	register int flg;
	register int rtn; 


	/*	Walk through process regions
	 *	Private regions or shared regions that are being used
	 *	exclusively by this process get paged out en masse.
	 *	Other shared regions are just pruned.
	 *
	 *	return 1 - every region gets paged out 
	 *      return 0 - find region(s) locked
	 */

	rtn = 1;
#ifdef mips
	checkfp(p, 0);
#endif
	cnt.v_swpout++;
	for (prp = p->p_region; rp = prp->p_reg; prp++) {
		if (rp->r_flags & RG_LOCK) {
			rtn = 0;
			continue;
		}

		reglock(rp);
		flg = (rp->r_type == RT_PRIVATE || rp->r_refcnt == 1);

		/*	if (flg == 0) only take unreferenced pages */
		/*	if (flg == 1) take all valid pages */

		(void) swapout_region(rp, flg);
		regrele(rp);
	}

	return(rtn);
}
