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
#ident	"$Header: slp.c,v 1.14.1.3 90/05/10 05:53:21 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/sysmacros.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/user.h"
#include "sys/systm.h"
#include "sys/vnode.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"

/*
 * sleep-wakeup hashing:  Each entry in hsque[] points
 * to a linked list of sleeping processes.   NHSQUE must
 * be a power of 2.  Sqhash(x) is used to index into
 * hsque[] based on the sleep channel.
 */

#define NHSQUE		64
#define sqhash(X)	(&hsque[((int)X >> 3) & (NHSQUE-1)])

proc_t			*hsque[NHSQUE];

extern int		userstack[];

/*
 * Give up the processor till a wakeup occurs
 * on chan, at which time the process
 * enters the scheduling queue at priority pri.
 * The most important effect of pri is that when
 * pri<=PZERO a signal cannot disturb the sleep;
 * if pri>PZERO signals will be processed.
 * When pri > PZERO, SNWAKE bit in p_flag will be set
 * to indicate no disturbance during sleep.
 * This is different from the old way that compares
 * p_pri and PZERO.
 * Callers of this routine must be prepared for
 * premature return, and check that the reason for
 * sleeping has gone away.
 */

#define	TZERO	10

sleep(chan, disp)
caddr_t chan;
{
	register proc_t *rp = u.u_procp;
	register proc_t **q = sqhash(chan);
	register s;
	register tmpdisp;

	s = splhi();
	if (panicstr) {
		spl0();
		splx(s);
		return(0);
	}

#ifdef DEBUG
	/* BUG CATCHER.
	 * 
	 * We've seen bugs where processes on the runq somehow end up
	 * running, and then trying to call sleep.  That corrupts the
	 * runq and sleep queues and can crash the system.
	 *
	 * So this code is really just a bug catcher.  If any customer
	 * reports the message below, we need to work with them to get
	 * the bug out.
	 *
	 * If we don't see this for a while, we should remove this code.
	 *
	 * hitz 7-13-88
	 */
	{
	register proc_t **pp = &runq;	/* Proc pointer pointer */
	register proc_t *tp = NULL;	/* Temp proc pointer */

	for (pp = &runq; *pp != NULL; pp = &(*pp)->p_link)
	    if (*pp == rp) {
		cmn_err(CE_WARN, 
		    "sleep: proc %s (pid=%d) (wchan=0x%x) on runq\n",
		    u.u_comm, rp->p_pid, chan);
		cmn_err(CE_CONT, "Please notify the MIPS OS group.\n");

		/*
		 * Running proc shouldn't be on runq.  Since it is, maybe
		 * someone wants it to keep running for some reason.  Let
		 * it keep running.  Remove it from runq and keep going.
		 */
		tp = *pp;
		*pp = tp->p_link;
		tp->p_link = NULL;
		return 0;
	    }
	}
#endif DEBUG

	/* put on sleep queue */

	ASSERT(rp->p_link == 0);

	tmpdisp = disp & PMASK;
	rp->p_wchan = chan;
	rp->p_link = *q;
	*q = rp;
	if (rp->p_time > TZERO)
		rp->p_time = TZERO;
	if(rp->p_pri > tmpdisp)
		rp->p_pri = tmpdisp;
	if (tmpdisp > PZERO) {
		rp->p_flag &= ~SNWAKE;
 		if (rp->p_cursig || 
		    (rp->p_sig && issig()) || (rp->p_flag & SPRSTOP)) {

			/* signal pending: take off sleep queue */
			/* if not taken off during a stop() in issig() */

			if (rp->p_wchan != 0) {
				rp->p_wchan = 0;
				*q = rp->p_link;
				rp->p_link = NULL;
			};
			goto psig;
		}

		if (runin != 0) {
		  	runin = 0;
			if (rp->p_wchan) {
				rp->p_wchan = 0;
				*q = rp->p_link;
				rp->p_link = NULL;
			};
			wakeup((caddr_t) &runin);
			if (chan != (caddr_t) &runin) {
				rp->p_wchan = chan;
				rp->p_link = *q;
				*q = rp;
			}  
		};

		if (rp->p_wchan == 0)
		  	goto return_normally;

		u.u_ru.ru_nvcsw++;
		rp->p_stat = SSLEEP;
		rp->p_slptime = 0;
		swtch();
 		if (rp->p_cursig ||
		    (rp->p_sig && issig()) || (rp->p_flag & SPRSTOP))
			goto psig;
	} else {
		rp->p_flag |= SNWAKE;
		u.u_ru.ru_nvcsw++;
		rp->p_stat = SSLEEP;
		rp->p_slptime = 0;
		swtch();
	}
return_normally:
	splx(s);
	return(0);

	/*
	 * If priority was low (>PZERO) and there has been a signal,
	 * if PCATCH is set, return 1, else
	 * execute non-local goto to the qsav location.
	 */

psig:
	splx(s);
	if (disp & PCATCH)
		return(1);

	MONITOR('L', u.u_qsav[7], 0, 0, 0);

	longjmp(u.u_qsav);

	/* NOTREACHED */
}

/*
 * Remove a process from its wait queue.
 */
unsleep(p)
register struct proc *p;
{
	register struct proc **q;
	register s;

	s = splhi();
	if (p->p_wchan) {
		for (q = sqhash(p->p_wchan); *q != p; q = &(*q)->p_link)
			;
		*q = p->p_link;
		p->p_link = 0;
		p->p_wchan = 0;
	}
	splx(s);
}

/*
 * Wake up all processes sleeping on chan.
 */

wakeup(chan)
register caddr_t chan;
{
	register proc_t *p;
	register proc_t	**q;
	register int	runsched = 0;
	register int	s;
	register proc_t *plist = NULL;	/* tmp list of procs to be awakened */

	s = splhi();
	for (q = sqhash(chan); p = *q; ) {
		ASSERT(p->p_stat == SSLEEP || p->p_stat == SSTOP);
#if 1
		if (p->p_stat != SSLEEP && p->p_stat != SSTOP)
			cmn_err(CE_PANIC, "wakeup p_stat");
#endif
		if (p->p_wchan == chan) {
			/* 
			 * take off sleep queue, put on run queue
			 * if not SSTOP.
			 */
			p->p_wchan = 0;
			*q = p->p_link;
			p->p_link = 0;
			if (p->p_stat == SSLEEP) {
				p->p_stat = SRUN;
				p->p_link = plist;
				plist = p;

				MONITOR('W', p, chan, 0, 0);

				/*
				 * Make arrangements for swapin
				 * or preemption if necessary
				 */

				if (!(p->p_flag&SLOAD)) {

					/* we must not call setrun here!*/

					p->p_time = 0;
					if (runout > 0)
						runsched = 1;
				} else if (p->p_pri < curpri)
					runrun++;
			}
		} else
			q = &p->p_link;
	} /* for */

	/*
	 *	For fair process scheduling, processes should be entered
	 *	into the run queue in the same order in which they slept.
	 *	Saving procs into plist instead of setrq()ing them directly
	 *	provides this correct ordering.
	 */
	while (plist) {
		p = plist;
		plist = p->p_link;
		p->p_link = 0;
		setrq(p);
	}

	if (runsched) {
		runout = 0;
		setrun(&proc[0]);
	}
	splx(s);
}

/*
 * Wake up the FIRST process sleeping on chan.
 */

wakeup_one(chan)
register caddr_t chan;
{
	register proc_t *p;
	register proc_t	**q;
	register int	runsched = 0;
	register int	s;
	register proc_t *plist = NULL;	/* tmp list of procs to be awakened */

	s = splhi();
	for (q = sqhash(chan); p = *q; ) {
		ASSERT(p->p_stat == SSLEEP || p->p_stat == SSTOP);
#if 1
		if (p->p_stat != SSLEEP && p->p_stat != SSTOP)
			cmn_err(CE_PANIC, "wakeup p_stat");
#endif
		if (p->p_wchan == chan) {
			/* 
			 * take off sleep queue, put on run queue
			 * if not SSTOP.
			 */
			p->p_wchan = 0;
			*q = p->p_link;
			p->p_link = 0;
			if (p->p_stat == SSLEEP) {
				p->p_stat = SRUN;
				p->p_link = plist;
				plist = p;

				MONITOR('W', p, chan, 0, 0);

				/*
				 * Make arrangements for swapin
				 * or preemption if necessary
				 */

				if (!(p->p_flag&SLOAD)) {

					/* we must not call setrun here!*/

					p->p_time = 0;
					if (runout > 0)
						runsched = 1;
				} else if (p->p_pri < curpri)
					runrun++;
			}
			break;
		} else
			q = &p->p_link;
	} /* for */

	/*
	 *	For fair process scheduling, processes should be entered
	 *	into the run queue in the same order in which they slept.
	 *	Saving procs into plist instead of setrq()ing them directly
	 *	provides this correct ordering.
	 *	In the single wakeup case this should be only one entry aka
	 *      the trivial case.
	 */
	while (plist) {
		p = plist;
		plist = p->p_link;
		p->p_link = 0;
		setrq(p);
	}

	if (runsched) {
		runout = 0;
		setrun(&proc[0]);
	}
	splx(s);
}

/*
 * Set the process running;
 * arrange for it to be swapped in if necessary.
 */

setrun(p)
register struct proc *p;
{
	register s;

	s = splhi();
	if (p->p_stat == SSLEEP || p->p_stat == SSTOP) {

		/* take off sleep queue */

		unsleep(p);

	} else if (p->p_stat == SRUN) {

		/* already on run queue */

		splx(s);
		return;
	}

	/* put on run queue */

	ASSERT(p->p_wchan == 0);
	p->p_stat = SRUN;
	setrq(p);
  	p->p_slptime = 0;

	/*
	 * Make arrangements for swapin
	 * or preemption if necessary
	 */

	if (!(p->p_flag&SLOAD)) {
		p->p_time = 0;
		if (runout > 0) {
			runout = 0;
			setrun(&proc[0]);
		}
	} else if (p->p_pri < curpri)
		runrun++;

	splx(s);
}
