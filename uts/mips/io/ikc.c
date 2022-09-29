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
#ident	"$Header: ikc.c,v 1.4.4.2 90/05/10 05:21:01 wje Exp $"

/*
 * ikc.c
 *
 * driver for the Ikon 10088, VMEbus hardcopy interface, revision A
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/io/RCS/ikc.c,v $
 * $Revision: 1.4.4.2 $
 * $Date: 90/05/10 05:21:01 $
 */

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/file.h"
#include "sys/signal.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/vmevar.h"
#include "sys/vmereg.h"
#include "sys/elog.h"
#include "sys/ioctl.h"
#include "sys/edt.h"

#include "sys/sg.h"
# define IK_DRIVER
#include "sys/ikcreg.h"
# undef  IK_DRIVER
#include "sys/ik_ioctl.h"
#include "sys/vcmd.h"

# define DBGFLAG		ik_debug


# ifdef DBGFLAG
# define dprintf(x)		(ik_debug?printf x:0)
# define STATIC
# else  DBGFLAG
# define dprintf(x)
# define STATIC			static
# endif DBGFLAG


# define COPYIN(v, s) \
		if (copyin(arg, (caddr_t)&v, s) < 0) { \
			u.u_error = EFAULT; \
			break; \
		}
# define COPYOUT(v, s) \
		if (copyout((caddr_t)&v, arg, s) < 0) { \
			u.u_error = EFAULT; \
			break; \
		}


# define printf			dri_printf


/* lboot variables */
extern int ik_ccnt;			/* # of ctlrs */
extern struct iksoftc ik_softc[];	/* ctlr state info */
extern struct iksoftc *ik_msoftc[];	/* ptrs to ctlr state */

# define CINFOPTR(u)		(ik_msoftc[u])


/* ikc globals */
int ik_cnext = 0;		/* next avail ctlr state struct */
STATIC short ik_debug = 0;	/* enable debugging printouts */
STATIC short ik_ticks = HZ;	/* ticks per ik time interval */
STATIC short ik_timo = 5*60;	/* timeout in secs */


/*
 * standard init routine, called exactly once at system startup time
 */
ikcinit()
{
}

/*
 * standard edtinit routine, called exactly once per ctlr at system
 * startup time
 */
ikcedtinit(e)
	register struct edt *e;
{
	register struct vme_intrs *v;
	register CINFO *ci;

	v = e->e_intr_info;
	printf("ikc%d: vec%x ipl%d at 0x%x...", v->v_unit,
			v->v_vec, v->v_brl, e->e_base);
	if (v->v_unit >= IK_MAXCTLR) {
		printf("invalid ctlr %d; must be < %d; ignored\n",
			v->v_unit, IK_MAXCTLR);
		return;
	}
	if (ik_cnext >= ik_ccnt) {
		printf("too many ctlrs; max %d; ignored\n", ik_ccnt);
		return;
	}
	if (CINFOPTR(v->v_unit) != 0) {
		printf("duplicate ctlr %d; ignored\n", v->v_unit);
		return;
	}
	ci = ik_softc + ik_cnext++;
	CINFOPTR(v->v_unit) = ci;
	ik_defctlr(ci, e->e_base,
			(int)v->v_unit, (int)v->v_vec, (int)v->v_brl);
	if (badaddr(ci->c_base+IK_SR, sizeof (ushort))) {
		printf("not found\n");
		CINFOPTR(v->v_unit) = 0;
		ik_cnext--;
		return;
	}
	ik_cprobe(ci);
}

/*
 * standard open routine.
 *
 * the ctlr may be attached to various types of device.
 *	versatec
 *	centronics
 * only one type may be open at a time.
 */
ikcopen(dev, flag)
	dev_t dev; int flag;
{
	register CINFO *ci;
	register int c;
	register short printer;

	dprintf((" O"));
	c = DEVCTLR(dev);
	if (c >= IK_MAXCTLR || (ci = CINFOPTR(c)) == 0) {
		u.u_error = ENXIO;
		return;
	}

	printer = DEVTYPE(dev);
	if (printer == RAW)
		return;

	ik_lock(ci);

	if (ci->c_flags & CTLR_OPEN) {
		if (ci->c_dtype == printer)
			goto out;
		u.u_error = EBUSY;
		goto out;
	}

	ci->c_dtype = printer;
	ci->c_flags |= CTLR_OPEN;

	if (!(ci->c_flags & CTLR_TICKING))
		ik_gong(ci->c_unit);

	switch (printer) {

	case VERS:
	case CENTRONICS:
		if (printer == VERS)
			ci->c_mode &= ~LR_OPTION;
		else
			ci->c_mode |= LR_OPTION;
		if (!DEVNORESET(dev))
			ik_reset(ci);
		else
			ik_mode(ci, 0);
		break;

	default:
		u.u_error = ENXIO;
		break;
	}

	if (u.u_error)
		ci->c_flags &= ~CTLR_OPEN;

out:
	ik_unlock(ci);
}

/*
 * standard close routine
 */
ikcclose(dev)
	dev_t dev;
{
	register CINFO *ci;

	dprintf((" C"));
	if (DEVTYPE(dev) == RAW)
		return;

	ci = CINFOPTR(DEVCTLR(dev));
	ASSERT(ci != 0);
	if (ci->c_flags & CTLR_HUNG)
		ik_reset(ci);
	ci->c_flags &= ~CTLR_OPEN;
}

/*
 * standard read routine, not used
 */
ikcread(dev)
	dev_t dev;
{
	dprintf((" R"));
	u.u_error = EIO;
}

/*
 * standard write routine
 */
ikcwrite(dev)
	dev_t dev;
{
	extern int ik_strategy();
	register struct buf *bp;
	register CINFO *ci;

	dprintf((" W"));
	ci = CINFOPTR(DEVCTLR(dev));
	ASSERT(ci != 0);
	ik_lock(ci);
	bp = &ci->c_buf;
	bp->b_dev = dev;
	bp->b_flags = 0;
	physio(ik_strategy, bp, dev, B_WRITE);
	ik_unlock(ci);
}

/*
 * standard ioctl routine
 */
ikcioctl(dev, cmd, arg, mode)
	dev_t dev;
	int cmd;
	caddr_t arg;
	int mode;
{
	register CINFO *ci;
	union {
		struct poke p;
		struct vstate v;
		int i;
	} j;

	dprintf((" I"));
	ci = CINFOPTR(DEVCTLR(dev));
	ASSERT(ci != 0);

	switch (cmd) {

	case IKIODEBUG:
		if (!suser())
			return;
		COPYIN(j.i, sizeof j.i);
		ik_debug = j.i;
		break;

	case IKIOPIO:
		COPYIN(j.i, sizeof j.i);
		ik_lock(ci);
		ik_cmdreg(ci, IK_DOR, j.i);
		ik_unlock(ci);
		return;

	case IKIOPEEK:
		if (!suser())
			return;
		COPYIN(j.i, sizeof j.i);
		j.i = IK_INREG(ci, j.i);
		COPYOUT(j.i, sizeof j.i);
		break;

	case IKIOPOKE:
		if (!suser())
			return;
		COPYIN(j.p, sizeof j.p);
		IK_OUTREG(ci, j.p.f, j.p.v);
		WBFLUSH();
		break;

	case IKIORESET:
		ik_lock(ci);
		ik_reset(ci);
		ik_unlock(ci);
		break;

	case VGETSTATE:				/* XXX */
	case IKIOGETVSTATE:
		ik_getvstate(ci, &j.v);
		COPYOUT(j.v, sizeof j.v);
		break;

	case VSETSTATE:				/* XXX */
	case IKIOSETVSTATE:
		COPYIN(j.v, sizeof j.v);
		ik_lock(ci);
		(void)ik_setvstate(ci, &j.v);
		ik_unlock(ci);
		break;

	default:
		u.u_error = EINVAL;
		break;
	}
}

/*
 * standard interrupt routine.  advance i/o queue.  return true iff the
 * intr was ours
 */
int
ikcintr(unit)
	int unit;
{
	register CINFO *ci;
	register int r, sr;

	dprintf((" {{"));
	r = 0;
	/* safety checks */
	if (unit >= IK_MAXCTLR || (ci = CINFOPTR(unit)) == 0)
		goto out;
	/* check if ikon interrupted */
	sr = IK_INREG(ci, IK_SR);
	dprintf((" sr%x", sr));
	if (!(sr & SR_INTR))
		goto out;
	/* check if ikon ready */
	r = 1;
	if (!(sr & SR_READY))
		goto done;
	ik_update(ci, sr);
	ik_start(ci->c_unit);
done:
	/* clear ikon intr */
	IK_OUTREG(ci, IK_PR, PR_IACK);
out:
	dprintf((" }}"));
	return r;
}


/* ----- auxiliary routines */
/*
 * initialiize ctlr state variables
 */
static
ik_defctlr(ci, base, unit, vec, brl)
	register CINFO *ci;
	char *base;
	int unit, vec, brl;
{
	register struct buf *tab;
	ci->c_flags = 0;
	ci->c_base = (ushort *)base;
	ci->c_unit = unit;
	ci->c_modvec = (PHYSPACE<<8)|vec;
	ci->c_mode = LR_IENABLE;
	ci->c_brl = brl;
	ci->c_iostate = CTLR_IDLE;
	ci->c_timo = ik_timo;
	tab = &ci->c_tab;
	tab->av_forw = tab->av_back = tab;
}

/*
 * probe ctlr
 */
static
ik_cprobe(ci)
	register CINFO *ci;
{
	printf("found\n");
}

/*
 * strategy routine.  enqueue the given buf and start i/o if it's
 * not already.
 */
STATIC
ik_strategy(bp)
	register struct buf *bp;
{
	register CINFO *ci;
	register struct buf *tab;
	USEPRI;

	dprintf((" S"));
	ci = CINFOPTR(DEVCTLR(bp->b_dev));
	ASSERT(ci != 0);
	RAISE;
	tab = &ci->c_tab;
	bp->av_forw = tab;
	bp->av_back = tab->av_back;
	bp->av_forw->av_back = bp->av_back->av_forw = bp;
	ik_start(ci->c_unit);
	LOWER;
}

/*
 * update the ctlr state after an interrupt.  advance the state machine
 */
STATIC
ik_update(ci, sr)
	register CINFO *ci;
	int sr;
{
	register struct buf *bp;

	if (!(ci->c_flags & CTLR_ACTIVE))
		goto out;

	dprintf((" u"));
	bp = ci->c_tab.av_forw;
	ASSERT(bp != &ci->c_tab);

	if (sr & SR_BERR) {
		bp->b_flags |= B_ERROR;
		dprintf((" berr addr=%x.%x",
				IK_INREG(ci, IK_DMAHI),
				IK_INREG(ci, IK_DMALO)));
		dprintf((" modvec=%x", IK_INREG(ci, IK_MODVEC)));
		dprintf((" ddata=%x", IK_INREG(ci, IK_DDR)));
	}

	if (bp->b_flags & B_ERROR)
		goto done;

	switch (ci->c_iostate) {

	case CTLR_IO:
		bp->b_resid -= ci->c_sg[ci->c_cursg].sg_bcount;
		ci->c_cursg++;
		if (ci->c_cursg < ci->c_nsg)
			goto out;
		goto done;

	case CTLR_CMD:
		goto done;
	}

done:
	ci->c_iostate = CTLR_IDLE;
	bp->av_forw->av_back = bp->av_back;
	bp->av_back->av_forw = bp->av_forw;
	iodone(bp);
out:
	ci->c_timer = 0;
	ci->c_flags &= ~CTLR_ACTIVE;
}

/*
 * start i/o on the given ctlr.
 */
STATIC
ik_start(unit)
	int unit;
{
	register CINFO *ci;
	register struct buf *tab;
	register struct buf *bp;

	ci = CINFOPTR(unit);
	ASSERT(ci != 0);
	tab = &ci->c_tab;
	while (!(ci->c_flags & CTLR_ACTIVE)) {

		if ((bp = tab->av_forw) == tab)
			return;

		ci->c_flags |= CTLR_ACTIVE;
		ci->c_timer = ci->c_timo;

		if (ci->c_flags & CTLR_HUNG) {
			bp->b_flags |= B_ERROR;
			ik_update(ci, 0);
			continue;
		}

		switch (ci->c_iostate) {

		case CTLR_IDLE:
			ci->c_iostate = CTLR_IO;
			ik_sgset(ci, bp);
			ik_dma(ci);
			break;

		case CTLR_IO:
			ik_dma(ci);
			break;

		case CTLR_CMD:
			ik_ienable(ci);
			IK_OUTREG(ci, ci->c_reg, ci->c_val);
			break;
		}
	}
}

/*
 * set up scatter / gather vector for given buf.  the caller has
 * already locked the private variables.  currently called only
 * from user!
 */
STATIC
ik_sgset(ci, bp)
	register CINFO *ci;
	register struct buf *bp;
{
	int nvec;
	unsigned resid;

	if (bp->b_bcount > IK_IOLIM)
		bp->b_bcount = IK_IOLIM;
	bp->b_resid = bp->b_bcount;
	ci->c_nsg = sgset(bp, ci->c_sg, IK_NSG, &resid);
	ci->c_cursg = 0;
}

/*
 * arrange to time out commands after some amount of time.  called from
 * either user or timer
 */
static
ik_gong(unit)
	int unit;
{
	register CINFO *ci;

	ci = CINFOPTR(unit);
	ASSERT(ci != 0);
	if (!(ci->c_flags & CTLR_OPEN)) {
		ci->c_flags &= ~CTLR_TICKING;
		return;
	}

	timeout(ik_gong, unit, ik_ticks);
	ci->c_flags |= CTLR_TICKING;

	if (ci->c_timer > 0 && --ci->c_timer == 0)
		ik_kill(ci);
}

/*
 * kill a timed-out i/o operation.  called from timer
 */
STATIC
ik_kill(ci)
	register CINFO *ci;
{
	dprintf((" k"));
	if (!(ci->c_flags & CTLR_ACTIVE))
		return;

	dprintf((" K"));
	ik_update(ci, SR_BERR);
	ci->c_flags |= CTLR_HUNG;
	IK_OUTREG(ci, IK_PR, PR_RESET);
	printf("ik%d hung\n", ci->c_unit);
}

/*
 * lock ctlr private variables.  called only from user
 */
static
ik_lock(ci)
	register CINFO *ci;
{
	USEPRI;

	RAISE;
	while (ci->c_lock & LOCK_BUSY) {
		ci->c_lock |= LOCK_WANTED;
		sleep((caddr_t)&ci->c_lock, IKPRI);
	}
	ci->c_lock |= LOCK_BUSY;
	LOWER;
}

/*
 * release ctlr private variables
 */
static
ik_unlock(ci)
	register CINFO *ci;
{
	if (ci->c_lock & LOCK_WANTED)
		wakeup((caddr_t)&ci->c_lock);
	ci->c_lock &= ~(LOCK_WANTED|LOCK_BUSY);
}

/*
 * reset ctlr and dev.  called only from user
 */
STATIC
ik_reset(ci)
	register CINFO *ci;
{
	register int i;
	USEPRI;

	RAISE;
	ik_ifreset(ci);

	if (ci->c_dtype == VERS) {
		IK_OUTREG(ci, IK_PR, PR_CLEAR);
	}
	else {
		ik_mode(ci, LR_IPRIME);
		delay(2);
		ik_mode(ci, 0);
	}

	for (i = HZ/2; --i >= 0; delay(2) )
		if (IK_INREG(ci, IK_SR) & SR_READY)
			break;

	ci->c_flags &= ~CTLR_HUNG;

	LOWER;
}

/*
 * reset the iface.  called only from user
 */
static
ik_ifreset(ci)
	register CINFO *ci;
{
	register int t;

	dprintf((" ^"));
	t = IK_INREG(ci, IK_SR);
	IK_OUTREG(ci, IK_PR, PR_RESET|PR_SOFTACK);
	delay(2);

	t = IK_INREG(ci, IK_SR);
	ci->c_mode &= ~(LR_VPLOT|LR_VPPLOT);
	ik_mode(ci, 0);
}

/*
 * set the iface mode (interrupt, address modifier, and option)
 */
static
ik_mode(ci, m)
	register CINFO *ci;
	int m;
{
	USEPRI;
	RAISE;
	IK_OUTREG(ci, IK_MODVEC, ci->c_modvec);
	IK_OUTREG(ci, IK_LR, ci->c_mode|m);
	WBFLUSH();
	LOWER;
}

ik_ienable(ci)
	register CINFO *ci;
{
	register int i, o;
	USEPRI;
	RAISE;
	i = IK_INREG(ci, IK_DSR);
	o = ci->c_mode;
	if (i & DSR_VPLOT)
		o |= LR_VPLOT;
	if (i & DSR_VPPLOT)
		o |= LR_VPPLOT;
	IK_OUTREG(ci, IK_MODVEC, ci->c_modvec);
	IK_OUTREG(ci, IK_LR, o);
	WBFLUSH();
	LOWER;
}

/*
 * start up dma on the current sg entry
 */
static
ik_dma(ci)
	register CINFO *ci;
{
	register struct sg *sg;

	sg = ci->c_sg+ci->c_cursg;
	dprintf((" go 0x%x+%d", sg->sg_ioaddr, sg->sg_bcount));
	IK_OUTREG(ci, IK_DMAHI, sg->sg_ioaddr>>16);
	IK_OUTREG(ci, IK_DMALO, sg->sg_ioaddr>>0);
	IK_OUTREG(ci, IK_BC, sg->sg_bcount);
	ik_ienable(ci);
	IK_OUTREG(ci, IK_PR, PR_GO);
	WBFLUSH();
}

/*
 * create an i/o request to write a register
 */
static
ik_cmdreg(ci, r, v)
	register CINFO *ci;
	int r, v;
{
	register struct buf *bp;
	ci->c_iostate = CTLR_CMD;
	ci->c_reg = r;
	ci->c_val = v;
	bp = &ci->c_buf;
	ik_strategy(bp);
	iowait(bp);
}
/* ----- */


/* ----- versatec specific code */
static
ik_getvstate(ci, vp)
	register CINFO *ci;
	register struct vstate *vp;
{
	register int t;

	bzero((caddr_t)vp, sizeof *vp);
	t = IK_INREG(ci, IK_DSR);
	if (t & DSR_VPLOT)
		vp->f |= VPLOT;
	else
		vp->f |= VPRINT;
	if (t & DSR_VPPLOT)
		vp->f |= VPRINTPLOT;
	vp->timo = ci->c_timo;
}

static int
ik_setvstate(ci, vp)
	register CINFO *ci;
	register struct vstate *vp;
{
	register int t;

	t = 0;
	if (vp->f & (VPRINTPLOT|VPLOT|VPRINT))
		t |= ik_vmode(ci, vp->f);
	if (vp->f & (VLF|VFF|VREOT))
		t |= ik_vfunc(ci, vp->f);
	if (vp->timo != 0)
		ci->c_timo = vp->timo;
	return t;
}

static
ik_vmode(ci, f)
	register CINFO *ci;
	int f;
{
	register int t;

	t = 0;
	if (f & VPRINTPLOT) {
		t |= LR_VPPLOT;
		if (IK_INREG(ci, IK_DSR) & DSR_VPLOT)
			t |= LR_VPLOT;
	}
	if (f & VPRINT)
		t &= ~LR_VPLOT;
	if (f & VPLOT)
		t |= LR_VPLOT;
	ci->c_mode = (ci->c_mode & ~(LR_VPLOT|LR_VPPLOT)) | t;
	ik_cmdreg(ci, IK_MODVEC, ci->c_modvec);
	return 0;
}

static int
ik_vfunc(ci, f)
	register CINFO *ci;
	int f;
{
	register int t;

	t = 0;
	if (f & VLF)
		t |= PR_EOL;
	if (f & VFF)
		t |= PR_FF;
	if (f & VREOT)
		t |= PR_EOT;
	ik_cmdreg(ci, IK_PR, t);
	return 0;
}
/* ----- */
