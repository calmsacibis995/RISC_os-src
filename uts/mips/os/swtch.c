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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: swtch.c,v 1.21.1.5.1.1.1.3 90/11/15 13:55:02 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/param.h"
#include "sys/psw.h"
#include "sys/sysmacros.h"
#include "sys/fs/s5dir.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/immu.h"
#include "sys/systm.h"
#include "sys/sysinfo.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/debug.h"
#include "sys/reg.h"

extern int	userstack[];
extern proc_t	*panicproc;

int	fpowner;
#ifdef R6000_BUG_FPINTR
unsigned r6000_bug_fpintr;	/* nonzero means need workaround */
#endif R6000_BUG_FPINTR

/*
 * put the current process on
 * the Q of running processes and
 * call the scheduler. (called by trap.c)
 */
qswtch()
{
	setrq(u.u_procp);
	swtch();
}


/*
 * This routine is called to reschedule the CPU.
 * if the calling process is not in RUN state,
 * arrangements for it to restart must have
 * been made elsewhere, usually by calling via sleep.
 * There is a race here. A process may become
 * ready after it has been examined.
 * In this case, idle() will be called and
 * will return in at most 1HZ time.
 * i.e. its not worth putting an spl() in.
 */

char		runrun;
int		switching;
struct proc	*lastproc;	/* used to get rid of zombies */
extern int	request_tlb_flush;

extern int nofault;
extern int nested_interrupt;

swtch()
{
	register struct proc *p;
	struct proc *disp();
	int s;

	sysinfo.pswitch++;

	ASSERT(nofault == 0);
	ASSERT(nested_interrupt == 0);

	/*
	 * Flush tlb before switching to another process 
	 * if requested by vhand or swapper.
	 */
	if(request_tlb_flush) {
		request_tlb_flush = 0;
		flush_tlb();
	}

	s = splall();

	MONITOR('c', curproc->p_pid, 0, 0, 0);

	XPR3(XPR_SWTCH, "swtch: procp=0x%x, fpowner=0x%x", u.u_procp, fpowner);

#ifdef R6000_BUG_FPINTR
	if (r6000_bug_fpintr) {
	    /*
	     *  The R6010 will potentially signal an FP interrupt, even when
	     *  the SR says "coprocessor not usable", which will trash the FPC
	     *  CSR for the real fpowner.  Therefore, we need to save FPC state
	     *  on the context-switch.  Slow, but sure!
	     */
	    checkfp( u.u_procp, 0 );
	}
#endif R6000_BUG_FPINTR

	if (save()) { /* 0 on save, 1 on resume */
		XPR3(XPR_SWTCH, "swtch: resuming procp=0x%x, fpowner=0x%x", 
			u.u_procp, fpowner);
		if (fpowner == (int)u.u_procp) {
			USER_REG(EF_SR) |= SR_CU1;
		}
		else {
			USER_REG(EF_SR) &= ~SR_CU1;
		}
		
		/*
		 * RESUME:
		 * When we resume a new process, get rid of previous proc
		 * if it was a zombie
		 */
		MONITOR('r', curproc->p_pid, 0, 0, 0);
		switch (lastproc->p_stat) {
			case SZOMB:

				/*	Free up remaining memory used by the
			 	 *	zombie process here.  This is just
			 	 *	the u-block and the sdt's.
			 	 */

				ubfree(lastproc);
				break;
		}
		p = curproc;
		ASSERT(p->p_wchan == 0);
		p->p_stat = SRUN;
		splx(s);
		return;

	}
	switching = 1;
	lastproc = p = curproc;

	if ( p->p_stat == SONPROC ) {
		p->p_stat = SRUN;
	}

loop:
	splhi();

	/*
	 * Search for highest-priority runnable process
	 * If no process is runnable, idle.
	 */
	runrun = 0;
	if ((p = disp()) == NULL) {
		curpri = PIDLE;
		curproc = &proc[0];
		idle();
		goto loop;
	}

	switching = 0;

	if (p->p_tlbpid == -1)
		new_tlbpid(p);

	resume(p);
}


int		cur_tlbpid;			/* last tlbpid assigned */

/*
 * list of tlbpid owners
 */
static struct proc *tlbpid_owner[TLBHI_NPID];

/*
 * Assign the next available tlbpid to process p.
 * Un-assign all tlbpids and start over if we run out.
 * Remember this process so that we can un-assign tlbpids the next time
 * we run out.
 */

new_tlbpid(p)
 struct proc *p;
{
	register i;
	int	 s;

	s = splhi();
	if (++cur_tlbpid >= TLBHI_NPID) {
		/*
		 * tlbpid overflow
		 */
		for (i=SYS_TLBPID_UPB+1; i < TLBHI_NPID; i++)
			tlbpid_owner[i]->p_tlbpid = -1;
		flush_tlb();
		cur_tlbpid = SYS_TLBPID_UPB+1;	/* <= reserved for sys procs */
		/*
		 *  The currently executing process has now been officially
		 *  stripped of its tlbpid, so we need to reassign a new one.
		 *  Furthermore, the virtual cache tags [if any] have reappeared
		 *  for the U-area/kernelstack of the currently executing
		 *  process, so we need to invalidate these tags after we
		 *  assign a new tlbpid.
		 *  If the reason why we're in this routine in the first place
		 *  is to get a new tlbpid for the currently executing process,
		 *  then we'll do all this later on in the routine; otherwise, 
		 *  we've clobbered the currently executing process as a side-
		 *  effect of allocating a new tlbpid for some other process,
		 *  so we have to deal with the currently executing process now.
		 */
		if (p != u.u_procp) {
			u.u_procp->p_tlbpid = cur_tlbpid;
			set_tlbpid(cur_tlbpid);
			/* invalidate virt tags with the old tlbpid */
			invalidate_virt_dcache(UADDR, KERNELSTACK-UADDR);
			invalidate_virt_scache(
			    ptosv(u.u_procp->p_ubptbl[0].pgm.pg_pfn),
			    KERNELSTACK-UADDR);
			tlbpid_owner[cur_tlbpid] = u.u_procp;
			minfo.tlbpid++;
			cur_tlbpid++;
		}
	}
	p->p_tlbpid = cur_tlbpid;
	if (p == u.u_procp) {
		set_tlbpid(cur_tlbpid);
		/*
		 * If the pid rolls over, we need to invalidate
		 * the u-area (see above comment).
		 */
		if (cur_tlbpid == SYS_TLBPID_UPB+1) {
			invalidate_virt_dcache(UADDR, KERNELSTACK-UADDR);
			invalidate_virt_scache(
			    ptosv(p->p_ubptbl[0].pgm.pg_pfn),
			    KERNELSTACK-UADDR);
		}
	}
	tlbpid_owner[cur_tlbpid] = p;
	minfo.tlbpid++;
	splx(s);
}
