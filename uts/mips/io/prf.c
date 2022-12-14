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
#ident	"$Header: prf.c,v 1.7.4.2 90/05/10 05:24:34 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX Operating System Profiler
 *
 *	Sorted Kernel text addresses are written into driver.  At each
 *	clock interrupt a binary search locates the counter for the
 *	interval containing the captured PC and increments it.
 *	The last counter is used to hold the User mode counts.
 */

#include "sys/sbd.h"
#include "sys/signal.h"
#include "sys/errno.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/fs/s5dir.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/nvram.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/buf.h"

# define PRFMAX  2048		/* maximum number of text addresses */
# define PRF_ON    1		/* profiler collecting samples */
# define PRF_VAL   2		/* profiler contains valid text symbols */
# define BPW	   4		/* bytes per word */
# define L2BPW	   2		/* log2(BPW) */

extern unsigned  prfstat;	/* state of profiler */
unsigned  prfmax;		/* number of loaded text symbols */
unsigned  prfctr[PRFMAX + 1];	/* counters for symbols; last used for User */
unsigned  prfsym[PRFMAX];	/* text symbols */

prfread()
{
	unsigned  min();

	if ((prfstat & PRF_VAL) == 0) {
		u.u_error = ENXIO;
		return;
	}
	iomove((caddr_t) prfsym, min(u.u_count, prfmax * BPW), B_READ);
	iomove((caddr_t) prfctr, min(u.u_count, (prfmax + 1) * BPW), B_READ);
}

prfwrite()
{
	register  unsigned  *ip;

	if (u.u_count > sizeof prfsym)
		u.u_error = ENOSPC;
	else if (u.u_count & (BPW - 1) || u.u_count < 3 * BPW)
		u.u_error = E2BIG;
	else if (prfstat & PRF_ON)
		u.u_error = EBUSY;
	if (u.u_error)
		return;
	for (ip = prfctr; ip != &prfctr[PRFMAX + 1];)
		*ip++ = 0;
	prfmax = u.u_count >> L2BPW;
	iomove((caddr_t) prfsym, u.u_count, B_WRITE);
	for (ip = &prfsym[1]; ip != &prfsym[prfmax]; ip++)
		if (*ip < ip[-1]) {
			u.u_error = EINVAL;
			break;
		}
	if (u.u_error)
		prfstat = 0;
	else
		prfstat = PRF_VAL;
}

prfioctl(dev, cmd, arg, mode)
{
	switch (cmd) {
	case 1:
		u.u_r.r_reg.r_val1 = prfstat;
		break;
	case 2:
		u.u_r.r_reg.r_val1 = prfmax;
		break;
	case 3:
		if (prfstat & PRF_VAL) {
			prfstat = PRF_VAL | arg & PRF_ON;
			break;
		}
	default:
		u.u_error = EINVAL;
	}
}

prfintr(pc, ps)
	register  unsigned  pc;
	psw_t  ps;
{
	register  int  h, l, m;

	if (USERMODE(ps))
		prfctr[prfmax]++;
	else {
		l = 0;
		h = prfmax;
		while ((m = (l + h) / 2) != l)
			if (pc >= prfsym[m])
				l = m;
			else
				h = m;
		prfctr[m]++;
	}
}

/* null routines to keep lboot happy, otherwise it puts "nodev" into cdevsw */
prfopen() { nulldev(); }
prfclose() { nulldev(); }
