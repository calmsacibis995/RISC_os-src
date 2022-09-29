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
#ident	"$Header: lock.c,v 1.6.4.3 90/05/10 05:49:07 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/param.h"
#include "sys/dir.h"
#include "sys/signal.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/lock.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/debug.h"
#include "sys/tuneable.h"
#include "sys/cmn_err.h"

lock()
{
	struct a {
		long oper;
	};

	if (!suser())
		return;

	switch ((int)(((struct a *)u.u_ap)->oper)) {
	case TXTLOCK:
		if ((u.u_lock & (PROCLOCK|TXTLOCK)) || (textlock() == 0))
			goto bad;
		break;
	case PROCLOCK:
		if ((u.u_lock&(PROCLOCK|TXTLOCK|DATLOCK)) || (textlock() == 0))
			goto bad;
		if (datalock() == 0) {
			tunlock();
			goto bad;
		}
		proclock();
		break;
	case DATLOCK:
		if ((u.u_lock&(PROCLOCK|DATLOCK))  ||  datalock() == 0)
			goto bad;
		break;
	case UNLOCK:
		if (punlock() == 0)
			goto bad;
		break;

	default:
bad:
		if (u.u_error == 0)
			u.u_error = EINVAL;
	}
}

textlock()
{
	register preg_t	*prp;
	register reg_t	*rp;


	prp = findpreg(u.u_procp, PT_TEXT);
	if (prp == NULL)
		return(0);
	rp = prp->p_reg;
	reglock(rp);

	ASSERT(rp->r_noswapcnt >= 0);

	if (rp->r_noswapcnt == 0) {
		register int s;

		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		if (availrmem - rp->r_pgsz < tune.t_minarmem) {
			splx(s);
			regrele(rp);
			nomemmsg("textlock", rp->r_pgsz, 0, 1);
			u.u_error = EAGAIN;
			return(0);
		} else {
			availrmem -= rp->r_pgsz;
		}
		splx(s);
	}
	++rp->r_noswapcnt;
	regrele(rp);
	u.u_lock |= TXTLOCK;
	return(1);
}
		
tunlock()
{
	register preg_t	*prp;
	register reg_t	*rp;

	prp = findpreg(u.u_procp, PT_TEXT);
	if (prp == NULL)
		return(0);
	rp = prp->p_reg;
	reglock(rp);
	ASSERT(rp->r_noswapcnt);
	if (rp->r_noswapcnt == 1) {
		/* avail[rs]mem can be modified at interrupt level */
		register int s;

		s = splhi();
		availrmem += rp->r_pgsz;
		splx(s);
	}
	--rp->r_noswapcnt;
	regrele(rp);
	u.u_lock &= ~TXTLOCK;
	return(1);
}

datalock()
{
	register preg_t	*prp;
	register reg_t	*rp;
	register reg_t	*rp2;
	register int s;


	prp = findpreg(u.u_procp, PT_DATA);
	if (prp == NULL)
		return(0);
	rp = prp->p_reg;
	reglock(rp);

	ASSERT(rp->r_noswapcnt >= 0);

	/* avail[rs]mem can be modified at interrupt level */
	if (rp->r_noswapcnt == 0) {
		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		if (availrmem - rp->r_pgsz < tune.t_minarmem) {
			splx(s);
			regrele(rp);
			nomemmsg("datalock", rp->r_pgsz, 0, 1);
			u.u_error = EAGAIN;
			return(0);
		} else {
			availrmem -= rp->r_pgsz;
		}
		splx(s);
	}
	++rp->r_noswapcnt;
	prp = findpreg(u.u_procp, PT_STACK);
	if (prp == NULL) {
		--rp->r_noswapcnt;
		if (rp->r_noswapcnt == 0) {
			/* avail[rs]mem can be modified at interrupt level */
			s = splhi();
			availrmem += rp->r_pgsz;
			splx(s);
		}
		regrele(rp);
		return(0);
	}
	rp2 = prp->p_reg;
	reglock(rp2);

	ASSERT(rp2->r_noswapcnt >= 0);

	if (rp2->r_noswapcnt == 0) {
		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		if (availrmem - rp2->r_pgsz < tune.t_minarmem) {
			regrele(rp2);
			--rp->r_noswapcnt;
			if (rp->r_noswapcnt == 0)
				availrmem += rp->r_pgsz;
			splx(s);
			regrele(rp);
			nomemmsg("datalock(stack)", rp2->r_pgsz, 0, 1);
			u.u_error = EAGAIN;
			return(0);
		} else {
			availrmem -= rp2->r_pgsz;
		}
		splx(s);
	}
	++rp2->r_noswapcnt;
	regrele(rp);
	regrele(rp2);
	u.u_lock |= DATLOCK;
	return(1);
}
		
dunlock()
{
	register preg_t	*prp;
	register reg_t	*rp;
	register int s;


			
	prp = findpreg(u.u_procp, PT_DATA);
	if (prp == NULL)
		return(0);
	rp = prp->p_reg;
	reglock(rp);

	ASSERT(rp->r_noswapcnt);

	/* avail[rs]mem can be modified at interrupt level */
	if (rp->r_noswapcnt == 1) {
		s = splhi();
		availrmem += rp->r_pgsz;
		splx(s);
	}
	--rp->r_noswapcnt;
	regrele(rp);
			
	prp = findpreg(u.u_procp, PT_STACK);
	if (prp == NULL)
		return(0);
	rp = prp->p_reg;
	reglock(rp);

	ASSERT(rp->r_noswapcnt);

	if (rp->r_noswapcnt == 1) {
		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		availrmem += rp->r_pgsz;
		splx(s);
	}
	--rp->r_noswapcnt;
	regrele(rp);

	u.u_lock &= ~DATLOCK;
	return(1);
}

proclock()
{
	u.u_procp->p_flag |= SSYS;
	u.u_lock |= PROCLOCK;
}

punlock()
{
	if ((u.u_lock&(PROCLOCK|TXTLOCK|DATLOCK)) == 0)
		return(0);
	u.u_procp->p_flag &= ~SSYS;
	u.u_lock &= ~PROCLOCK;
	if (u.u_lock & TXTLOCK)
		tunlock();
	if (u.u_lock & DATLOCK)
		dunlock();
	return(1);
}
