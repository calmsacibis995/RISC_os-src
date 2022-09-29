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
#ident	"$Header: xm.c,v 1.8.5.2 90/05/10 05:39:00 wje Exp $"

/*
 * driver for the Xylogics 772, 1/2 inch magtape controller
 *
 * the number of controllers and drives is determined by the edt entries.
 * only one controller and one drive has been tested.
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/io/RCS/xm.c,v $
 * $Revision: 1.8.5.2 $
 * $Date: 90/05/10 05:39:00 $
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

#include "sys/mtio.h"
#include "sys/sg.h"
# define XM_DRIVER
#include "sys/xmreg.h"
# undef  XM_DRIVER
#include "sys/xm_ioctl.h"


# undef  XGONG		1		/* enable watchdog code */
# define DBGFLAG	xm_debug	/* enable debugging */


# define printf		dri_printf
# ifdef DBGFLAG
# define dprintf(b, x)	(xm_debug&(b)?printf x:0)
# define ifdebug(b, x)	(xm_debug&(b)?x:0)
# define STATIC
# else  DBGFLAG
# define dprintf(b, x)
# define ifdebug(b, x)
# define dump_iopb(i, b, c)
# define STATIC		static
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

# define CINFOPTR(unit)		(xm_msoftc[unit])
# define DRINFOPTR(drv)		(xm_mdrives[drv])


/* lboot variables */
extern int xm_dcnt;			/* # of drives */
extern DRINFO xm_drives[];		/* drive state */
extern int xm_mcnt;			/* # of drive ptrs */
extern DRINFO *xm_mdrives[];		/* drive ptrs */
extern int xm_ccnt;			/* # of ctlrs */
extern CINFO xm_softc[];		/* ctlr state */
extern CINFO *xm_msoftc[];		/* ctlr ptrs */


/* other globals */
int xm_dnext = 0;			/* next avail drive state struct */
int xm_cnext = 0;			/* next avail ctlr state struct */
int xm_probing;				/* flag - probe in progress */
int xm_maxretries = 0;			/* # of i/o retries */
int xm_interval = 0;			/* watchdog interval */
int xm_debug = 0x0;			/* debugging flags */


/* manuf. dependent drive info tables */
static unchar Default_info[] = {0x00, 0x00, 0x00, 0x00, 0x00};
static unchar Anritsu_info[] = {0x00, 0x1C, 0x1D, 0x00, 0x00};
static unchar Cipher_info[] = {0x00, 0x1C, 0x1D, 0x03, 0x00};
static unchar CDC_info[] = {0x00, 0x1C, 0x00, 0x1D, 0x82};
static unchar Fujitsu_info[] = {0x00, 0x1C, 0x00, 0x1D, 0x00};
static unchar Kennedy_info[] = {0x0E, 0x1C, 0x00, 0x00, 0x00};
static unchar Pertec_info[] = {0x13, 0x1C, 0x1D, 0x03, 0x00};
static unchar STC_info[] = {0x00, 0x1C, 0x00, 0x1D, 0x00};
static unchar Telex_info[] = {0x9C, 0x1C, 0x00, 0x1D, 0x00};
static unchar Thorn_info[] = {0x0A, 0x1C, 0x1D, 0x00, 0x0E};


/*
 * standard init routine
 */
xminit()
{
}

/*
 * standard edtinit routine, called exactly once per ctlr at system
 * startup time, after xminit().  probe the ctlr
 */
xmedtinit(e)
	register struct edt *e;
{
	register int c;
	register CINFO *ci;
	register struct vme_intrs *v;
	v = e->e_intr_info;
	printf("xm%d: vec%x ipl%d at 0x%x...", v->v_unit,
		v->v_vec, v->v_brl, e->e_base);
	if (v->v_unit >= XM_MAXCTLR) {
		printf("invalid ctlr %d; must be < %d; ignored\n",
				v->v_unit, XM_MAXCTLR);
		return;
	}
	if (xm_cnext >= xm_ccnt) {
		printf("too many ctlrs; max %d; ignored\n", xm_ccnt);
		return;
	}
	if (CINFOPTR(v->v_unit) != 0) {
		printf("duplicate ctlr %d; ignored", v->v_unit);
		return;
	}

	ci = xm_softc + xm_cnext++;
	CINFOPTR(v->v_unit) = ci;
	xm_defctlr(ci, (unchar *)e->e_base, (int)v->v_unit,
			(int)v->v_vec, (int)v->v_brl);
	if (badaddr(ci->c_base+XM_SR, 1)) {
		printf("not found\n");
		CINFOPTR(ci->c_unit) = 0;
		xm_cnext--;
		return;
	}
	xm_cprobe(ci);
}

/*
 * standard open routine
 *
 * there are many aliases for the same physical drive, eg,
 *	raw/cooked
 *	rew/norew
 *	lo/medium/hidens
 * only one can be opened at a time.
 */
xmopen(dev, flag)
	dev_t dev; int flag;
{
	register int drv, dens;
	register DRINFO *dp;
	register CINFO *ci;

	dprintf(1, (" O"));
	if ((drv = DEVDRIVE(dev)) >= xm_mcnt) {
		u.u_error = ENXIO;
		return;
	}
	ci = CINFOPTR(DEVCTLR(dev));
	if (ci == 0) {
		u.u_error = ENXIO;
		return;
	}

	/*
	 * when the special debugging device is open,
	 * we try to unwedge the ctlr
	 */
	if (DEBUGSLAVE(dev)) { xm_unwedge(dp); return; }

	/*
	 * if this slave has not been accessed before, it must be
	 * attached to its ctlr.  xm_dcnt limits the number of slaves
	 * which can be accessed.
	 */
	if ((dp = DRINFOPTR(drv)) == 0) {
		if (xm_dattach(ci, drv) < 0) {
			u.u_error = ENXIO;
			return;
		}
		dp = DRINFOPTR(drv);
		ASSERT(dp != 0);
		ASSERT(dp->d_ci == ci);
	}

	/*
	 * init the controller if not already done.
	 * this could also be triggered after an unwedge.
	 * lock out other opens while the action is in progress.
	 */
	xm_lock(&ci->c_lock);
	if (!(ci->c_status & CTLR_INITED)) {
		ci->c_status = (ci->c_status & ~CTLR_ERROR) | CTLR_INITED;
		xm_creset(ci);
		xm_command(dp, C_CINIT, 0);
	}
	xm_unlock(&ci->c_lock);

	xm_lock(&dp->d_lock);

	/*
	 * reset the drive if not already done.
	 * this can only be done when the drive is at bot.
	 */
	dens = DEVDENS(dev);
	if (!(dp->d_status & DRV_INITED)) {
		dp->d_status |= DRV_INITED;
		xm_command(dp, C_DRESET, 0);
		if (u.u_error)
			goto out;
		xm_dorewind(dp);
		if (u.u_error)
			goto out;
		xm_command(dp, C_DINIT, dens);
	}

	if (dp->d_status & DRV_OPEN) {
		u.u_error = EBUSY;
		goto out;
	}

	/*
	 * check status from most recent drive reset.
	 */
	if (!dp->d_online) {
		printf("xm%d: not online\n", drv);
		u.u_error = EIO;
		goto out;
	}

	if (flag&FWRITE && dp->d_wprot) {
		printf("xm%d: no write ring\n", drv);
		u.u_error = EIO;
		goto out;
	}

	if (dens != dp->d_dens) {
		xm_command(dp, C_DENS, dens);
		if (u.u_error) {
			printf("xm%d: can't set density%d\n", drv, dens);
			goto out;
		}
	}

	/*
	 * the drive is now open for business.
	 */
	dp->d_status |= DRV_OPEN;
	dp->d_rec = 0;
	ci->c_opens |= 1<<DEVSLAVE(dev);

# ifdef XGONG
	if (!(ci->c_status & CTLR_GONG) && xm_interval > 0)
		xm_gong(ci->c_unit);
# endif XGONG

out:
	xm_unlock(&dp->d_lock);
}

/*
 * standard close routine
 *
 * if the drive was written to while open, put out an EOF consisting
 * of 2 file marks.  reposition to just after the 1st of these so
 * that subsequent writes if any will erase the 2nd.  if the close is
 * for the auto-rewind device, then rewind.
 */
xmclose(dev, flag)
	dev_t dev; int flag;
{
	register DRINFO *dp;
	register CINFO *ci;

	dprintf(1, (" C"));
	dp = DRINFOPTR(DEVDRIVE(dev));
	ASSERT(dp != 0);
	ci = dp->d_ci;

	if (DEBUGSLAVE(dev))
		return;

	xm_lock(&dp->d_lock);

	if (dp->d_status&DRV_WRITTEN || flag&FWRITE) {
		dp->d_status &= ~DRV_WRITTEN;
		xm_command(dp, C_WFMARK, 2);
		if (u.u_error)
			goto rew;
		xm_command(dp, C_REVFILE, 1);
		if (u.u_error)
			goto rew;
	}
	else {
		xm_command(dp, C_NOP, 0);
	}

rew:
	if (!DEVNOREWIND(dev))
		xm_dorewind(dp);

out:
	dp->d_status &= ~DRV_OPEN;

	xm_unlock(&dp->d_lock);

	dprintf(1, (" drvclose"));
	ci->c_opens &= ~(1<<DEVSLAVE(dev));
}

/*
 * standard raw read routine.  lock private variables and call
 * raw strategy routine
 */
xmread(dev)
	dev_t dev;
{
	extern rxmstrategy();
	register DRINFO *dp;
	register struct buf *bp;
	dp = DRINFOPTR(DEVDRIVE(dev));
	ASSERT(dp != 0);
	bp = RAWBUF(dp);
	SETRAWBUF(bp);
	xm_lock(&dp->d_lock);
	dprintf(1, (" R"));
	physio(rxmstrategy, bp, dev, B_READ);
	xm_unlock(&dp->d_lock);
}

/*
 * standard raw write routine.  lock private variables and call
 * raw strategy routine
 *
 */
xmwrite(dev)
	dev_t dev;
{
	extern rxmstrategy();
	register DRINFO *dp;
	register struct buf *bp;
	dp = DRINFOPTR(DEVDRIVE(dev));
	ASSERT(dp != 0);
	bp = RAWBUF(dp);
	SETRAWBUF(bp);
	xm_lock(&dp->d_lock);
	dprintf(1, (" W"));
	physio(rxmstrategy, bp, dev, B_WRITE);
	xm_unlock(&dp->d_lock);
}

/*
 * raw i/o strategy routine.
 *
 * we pre-compute the scatter/gather vector here to avoid possibly
 * sleeping at interrupt time.  the caller has already locked the
 * private parts.  the request can then go into the standard strategy
 * routine
 */
static
rxmstrategy(bp)
	register struct buf *bp;
{
	register DRINFO *dp;
	dp = DRINFOPTR(DEVDRIVE(bp->b_dev));
	ASSERT(dp != 0);
	xm_ioinit(dp->d_rawipb, dp, bp);
	xmstrategy(bp);
}

/*
 * standard strategy routine
 *
 * we permit only one outstanding request per drive.  that request
 * owns the variables in its DRINFO structure for the duration.
 */
xmstrategy(bp)
	register struct buf *bp;
{
	register DRINFO *dp;
	register struct buf *tab;
	USEPRI;

	dprintf(1, (" S"));
	dp = DRINFOPTR(DEVDRIVE(bp->b_dev));
	ASSERT(dp != 0);
	RAISE;
	tab = &dp->d_tab;
	bp->av_forw = tab;
	bp->av_back = tab->av_back;
	bp->av_forw->av_back = bp->av_back->av_forw = bp;
	xm_start(dp->d_ci);
	LOWER;
}

/*
 * standard interrupt routine.  return true if the intr was ours
 */
xmintr(unit)
	int unit;
{
	extern IOPB *unstuff_addr();

	register IOPB *ipb;
	register int r;
	register CINFO *ci;

	dprintf(2, (" {{"));

	r = 0;
	if (unit >= XM_MAXCTLR || (ci = CINFOPTR(unit)) == 0)
		goto out;

	r = XM_INREG(ci, XM_SR) & (SR_FERR|SR_RIO);
	if (r & SR_FERR) {
		printf("xm: sr $%x err $%x\n", r, XM_INREG(ci, XM_FERR));
		ci->c_status |= CTLR_ERROR;
	}
	if (r & SR_RIO) {
		ipb = unstuff_addr(ci);
		if (ipb == 0)
			goto out;
		update_buf(ipb, DRINFOPTR(ipb->drv));
		xm_start(ci);
	}

out:
	dprintf(2, (" }}"));
	return r;
}

/*
 * standard raw ioctl routine
 */
xmioctl(dev, cmd, arg, mode)
	dev_t dev;
	int cmd; caddr_t arg; int mode;
{
	register DRINFO *dp;

	dprintf(1, (" I"));
	dp = DRINFOPTR(DEVDRIVE(dev));
	ASSERT(dp != 0);
	if (DEBUGSLAVE(dev)) {
		xm_unwedge(dp);
		return;
	}
	xm_mtioctl(dp, cmd, arg);
	if (u.u_error == EINVAL) {
		u.u_error = 0;
		xm_xyioctl(dp, cmd, arg);
	}
}

/*
 * peculiar xm ioctls
 */
static
xm_xyioctl(dp, cmd, arg)
	register DRINFO *dp;
	int cmd; caddr_t arg;
{
	auto int junk;
	IOPB i;
	dprintf(1, (" xyioctl"));
	xm_lock(&dp->d_lock);
	switch (cmd) {
	case XMIODEBUG:
		COPYIN(junk, sizeof junk);
		xm_debug = junk;
		break;
	case XMIOGCPARAMS:
		i = dp->d_ci->c_params;
		COPYOUT(i.i[0], sizeof i.i);
		break;
	case XMIOGDPARAMS:
		i = dp->d_params;
		COPYOUT(i.i[0], sizeof i.i);
		break;
	case XMIOSCPARAMS:
		if (!suser())
			break;
		COPYIN(i.i[0], sizeof i.i);
		dp->d_ci->c_params = i;
		dp->d_ci->c_status &= ~CTLR_INITED;
		dp->d_status &= ~DRV_INITED;
		break;
	case XMIOSDPARAMS:
		if (!suser())
			break;
		COPYIN(i.i[0], sizeof i.i);
		dp->d_params = i;
		dp->d_status &= ~DRV_INITED;
		break;
	case XMIODRESET:
		xm_command(dp, C_DRESET, 0);
		break;
	case XMIOCRESET:
		if (!suser())
			return;
		xm_creset(dp->d_ci);
		break;
	case XMIOLOAD:
		xm_command(dp, C_LOAD, 0);
		break;
	case XMIOUNLOAD:
		xm_command(dp, C_UNLOAD, 0);
		break;
	case XMIOREWIND:
		xm_dorewind(dp);
		break;
	default:
		u.u_error = EINVAL;
		break;
	}
	xm_unlock(&dp->d_lock);
}

/*
 * the usual tape ioctls
 */
static
xm_mtioctl(dp, cmd, arg)
	register DRINFO *dp;
	int cmd; caddr_t arg;
{
	struct mtop mtop;
	struct mtget mtget;
	xm_lock(&dp->d_lock);
	dprintf(1, (" mtioctl"));
	switch(cmd) {
	case MTIOCGET:
		COPYIN(mtop, sizeof mtop);
		xm_mtget(dp, &mtop, &mtget);
		COPYOUT(mtget, sizeof mtget);
		break;
	case MTIOCTOP:
		COPYIN(mtop, sizeof mtop);
		xm_mtop(dp, &mtop);
		break;
	default:
		u.u_error = EINVAL;
		break;
	}
	xm_unlock(&dp->d_lock);
}
static
xm_mtget(dp, i, x)
	register DRINFO *dp;
	register struct mtop *i;
	register struct mtget *x;
{
	switch(i->mt_op) {
# ifdef MTBLKSIZE
	case MTBLKSIZE:
		x->mt_blkno = btod(XM_IOLIM);
		break;
# endif MTBLKSIZE
	case MTNOP:
		x->mt_type = 0;
# ifdef MT_ISXM
		x->mt_type = MT_ISXM;
# endif MT_ISXM
		x->mt_dsreg = x->mt_erreg = 0;
		x->mt_resid = 0;
		x->mt_fileno = dp->d_fileno;
		x->mt_blkno = dp->d_rec;
		break;
	default:
		u.u_error = EINVAL;
		break;
	}
}
static
xm_mtop(dp, x)
	register DRINFO *dp;
	register struct mtop *x;
{
	switch(x->mt_op) {
	case MTREW:
		xm_dorewind(dp);
		break;
	case MTWEOF:
		xm_command(dp, C_WFMARK, (int)x->mt_count);
		break;
	case MTFSF:
		xm_command(dp, C_FWDFILE, (int)x->mt_count);
		break;
	case MTFSR:
		xm_command(dp, C_FWDREC, (int)x->mt_count);
		break;
	case MTBSF:
		xm_command(dp, C_REVFILE, (int)x->mt_count);
		dp->d_fileno -= x->mt_count;
		break;
	case MTBSR:
		xm_command(dp, C_REVREC, (int)x->mt_count);
		break;
	case MTOFFL:
		xm_command(dp, C_UNLOAD, (int)x->mt_count);
		break;
	default:
		u.u_error = EINVAL;
		break;
	}
}

/*
 * do a rewind and wait until it's finished.
 * the caller has already locked the private variables.
 */
static
xm_dorewind(dp)
	register DRINFO *dp;
{
	register IOPB *ipb;

	dprintf(1, (" <<"));
	xm_command(dp, C_REWIND, 0);
	if (u.u_error)
		return;
	ipb = dp->d_rawipb;
	while ((ipb->i[IO_STAT3]&(STAT3_BOT|STAT3_REW|STAT3_DRRDY))
				!= (STAT3_BOT|STAT3_DRRDY)) {
		delay(HZ);
		xm_command(dp, C_NOP, 0);
		if (u.u_error)
			return;
	}
}

/*
 * queue up a raw command.
 * the caller has already locked the private variables if necessary
 */
static
xm_command(dp, cmd, param)
	register DRINFO *dp;
	int cmd, param;
{
	register struct buf *bp;
	register IOPB *ipb;
	register CINFO *ci;

	ipb = dp->d_rawipb;
	bp = RAWBUF(dp);
	SETRAWBUF(bp);
	ci = dp->d_ci;

	dprintf(2, (" X%d", cmd));
	switch(cmd) {

	case C_DRESET:
		init_iopb(ipb, dp, CMD_DRESET, DRESET_HALT);
		break;

	case C_LOAD:
		init_iopb(ipb, dp, CMD_DRESET, DRESET_LOAD);
                break;

	case C_REWIND:
		init_iopb(ipb, dp, CMD_DRESET, DRESET_REWIND);
                break;

	case C_UNLOAD:
		init_iopb(ipb, dp, CMD_DRESET, DRESET_UNLOAD);
                break;

	case C_WFMARK:
		init_iopb(ipb, dp, CMD_XWRITE, XWRITE_FMK);
		ipb->i[IO_BCHI] = BCHI(param);
		ipb->i[IO_BCLO] = BCLO(param);
		break;

	case C_REVFILE:
		init_iopb(ipb, dp, CMD_SEEK, SEEK_FILEBACK);
		ipb->i[IO_BCHI] = BCHI(param);
		ipb->i[IO_BCLO] = BCLO(param);
		break;

	case C_FWDFILE:
		init_iopb(ipb, dp, CMD_SEEK, SEEK_FILEFORW);
		ipb->i[IO_BCHI] = BCHI(param);
		ipb->i[IO_BCLO] = BCLO(param);
		break;

	case C_REVREC:
		init_iopb(ipb, dp, CMD_SEEK, SEEK_RECBACK);
		ipb->i[IO_BCHI] = BCHI(param);
		ipb->i[IO_BCLO] = BCLO(param);
		break;

	case C_FWDREC:
		init_iopb(ipb, dp, CMD_SEEK, SEEK_RECFORW);
		ipb->i[IO_BCHI] = BCHI(param);
		ipb->i[IO_BCLO] = BCLO(param);
		break;

	case C_CINIT:
		init_iopb(ipb, dp, CMD_WPAR, WPAR_CTLR);
		ipb->i[IO_CPARA] = ci->c_params.i[IO_CPARA];
		ipb->i[IO_CPARB] = ci->c_params.i[IO_CPARB];
		ipb->i[IO_CPARC] = ci->c_params.i[IO_CPARC];
		ipb->i[IO_CTHROTTLE] = ci->c_params.i[IO_CTHROTTLE];
		break;

	case C_DENS:
	case C_DINIT:
		init_iopb(ipb, dp, CMD_WPAR, WPAR_DRIVE);
		bcopy((char *)dp->d_params.i+8, (char *)ipb->i+8, XM_ISIZE-8);
		if (dp->d_params.i[IO_DPARA]&DPARA_DSB)
			param = (param<<DPARB_DENS_SHIFT)+DENS_LO;
		else
			param = (param<<DPARB_DENS_SHIFT)+DENS_800;
		ipb->i[IO_DPARB] = (ipb->i[IO_DPARB]&~DPARB_DENS_MASK)
				| param;
		break;

	case C_NOP:
		init_iopb(ipb, dp, CMD_SPAR, SPAR_NOP);
		break;

	case C_READPARAMS:
		init_iopb(ipb, dp, CMD_RPAR, RPAR_CTLR);
		break;
	}

	checksum(ipb);
	xmstrategy(bp);
	if (xm_probing)
		xm_buzz(dp->d_ci);
	else
		iowait(bp);

	if (ipb->i[IO_CMD]&CMD_ERRS) {
		u.u_error = EIO;
		return;
	}

	switch(cmd) {

	case C_DRESET:
	case C_LOAD:
	case C_REWIND:
	case C_UNLOAD:
		dp->d_fileno = dp->d_rec = 0;
		dp->d_wrec = -1;
		xm_drupdate(dp, ipb);
		break;

	case C_WFMARK:
		dp->d_fileno += param;
		dp->d_wrec = dp->d_rec = 0;
		break;

	case C_REVFILE:
		dp->d_fileno -= param;
		dp->d_rec = 0;
		dp->d_wrec = -1;
		break;

	case C_FWDFILE:
		dp->d_fileno += param;
		dp->d_rec = 0;
		dp->d_wrec = -1;
		break;

	case C_REVREC:
		dp->d_rec -= param;
		break;

	case C_FWDREC:
		dp->d_rec += param;
		break;

	case C_CINIT:
		break;

	case C_DENS:
	case C_DINIT:
		xm_drupdate(dp, ipb);
		dp->d_dens = param;
		break;

	case C_NOP:
		break;

	case C_READPARAMS:
		printf("ctlr=7%x fw=%d.%d\n",
			ipb->i[IO_CTYPE],
			ipb->i[IO_CFWREV], ipb->i[IO_CSUBREV]);
		break;
	}
}

/*
 * update drive status after a reset of some kind
 */
static
xm_drupdate(dp, ipb)
	register DRINFO *dp;
	register IOPB *ipb;
{
	register unchar i;

	xm_upstat(dp, ipb);
	dp->d_wprot = (ipb->i[IO_STAT2] & STAT2_WPT) != 0;
	dp->d_online = (ipb->i[IO_STAT3] & STAT3_ONLINE) != 0;
	i = (ipb->i[IO_DPARB] & DPARB_DENS_MASK) >> DPARB_DENS_SHIFT;
	if (dp->d_params.i[IO_DPARA]&DPARA_DSB)
		dp->d_dens = i - (DENS_LO>>DPARB_DENS_SHIFT);
	else
		dp->d_dens = i - (DENS_800>>DPARB_DENS_SHIFT);
	dprintf(4, ("dens%d", dp->d_dens));
}

/*
 * update drive status after most any command
 */
static
xm_upstat(dp, ipb)
	register DRINFO *dp;
	register IOPB *ipb;
{
	dp->d_bot = (ipb->i[IO_STAT3] & STAT3_BOT) != 0;
	dp->d_eot = (ipb->i[IO_STAT2] & STAT2_EOT) != 0;
	dp->d_atfmark = (ipb->i[IO_STAT2] & STAT2_FMK) != 0;
}

/*
 * buzz until controller responds.  called only when probing.
 */
static
xm_buzz(ci)
	register CINFO *ci;
{
	register int i;
	for (i = 300; --i >= 0; msdelay(33))
		if (XM_INREG(ci, XM_SR) & (SR_RIO|SR_FERR))
			break;
	(void)xmintr(ci->c_unit);
}

/*
 * establish checksum in an iopb
 */
static
checksum(ipb)
	register IOPB *ipb;
{
	register ushort sum;
	register int i;
	sum = 0;
	for (i = 0; i < IO_CHI; i++)
		sum += ipb->i[i];
	ipb->i[IO_CHI] = BCHI(sum);
	ipb->i[IO_CLO] = BCLO(sum);
}

/*
 * set an iopb to standard parameters plus the given cmd and subfunc
 */
static
init_iopb(ipb, dp, cmd, sub_funct)
	register IOPB *ipb;
	register DRINFO *dp;
	int cmd, sub_funct;
{
	bzero((char *)ipb->i, XM_ISIZE);

	ipb->i[IO_CMD] = cmd;
	ipb->i[IO_FUNC] = sub_funct;
	ipb->i[IO_DATAMOD] = PHYSPACE;
	ipb->i[IO_LINKMOD] = PHYSPACE;
	ipb->i[IO_UNIT] = dp->d_params.i[IO_UNIT];
	ipb->i[IO_LEVEL] = dp->d_params.i[IO_LEVEL];
	ipb->i[IO_IVEC] = dp->d_params.i[IO_IVEC];
	if (xm_probing)
		ipb->i[IO_LEVEL] = 0;
}

/*
 * set up the iopb (chain) for the given buf.
 * there may be side-effects on the state of the drive info.
 * this routine, together with update_buf(), manipulates the
 * drive state machine.
 *
 * this various cases are:
 *	a raw command or i/o
 *		copy the RAWIOPB
 *	a cooked i/o
 *		set up to pre-seek if necessary
 *		if reading just past the end, fake 0's so getblk() will work
 *
 * return 0 if an i/o is setup.
 * this routine is called only at hi pri.
 */
static int
make_buf(dp, bp)
	register DRINFO *dp;
	register struct buf *bp;
{
	register IOPB *ipb;
	int cmd;
	daddr_t rec;

	ASSERT(dp->d_free != 0);
	ipb = dp->d_free;
	dp->d_free = dp->d_free->next;

	switch(dp->d_iostate) {

	case DRV_IO:
	case DRV_CMD:
	case DRV_PRESEEK:
		if (ISRAWBUF(bp)) {
			/*
			 * if this is a raw command or i/o, copy the
			 * RAWIOPB information to the real iopb.
			 */
			dprintf(2, (" rawbuf"));
			*ipb = *dp->d_rawipb;
			bp->b_blkno = 0;
			dp->d_iostate = DRV_CMD;
			cmd = ipb->i[IO_CMD] & CMD_MASK;
			if (cmd == CMD_READ || cmd == CMD_WRITE)
				dp->d_iostate = DRV_IO;
		}
		else
		if ((rec = bp->b_blkno/btod(bp->b_bcount)) == dp->d_rec) {
			dp->d_iostate = DRV_IO;
			if (bp->b_flags & B_READ && rec == dp->d_wrec) {
				dprintf(2, (" fio"));
				bp->b_resid = 0;
				bp->av_forw->av_back = bp->av_back;
				bp->av_back->av_forw = bp->av_forw;
				bp->av_forw = bp->av_back = 0;
				iodone(bp);
				ipb->next = dp->d_free;
				dp->d_free = ipb;
				return -1;
			}
			xm_ioinit(ipb, dp, bp);
		}
		else {
			dp->d_iostate = DRV_PRESEEK;
			dprintf(3, (" >>%d,%d", dp->d_rec, rec));
			rec -= dp->d_rec;
			if (rec > 0) {
				cmd = SEEK_RECFORW;
			}
			else {
				cmd = SEEK_RECBACK;
				rec = -rec;
			}
			if (rec > XM_RWLIM)
				rec = XM_RWLIM;
			init_iopb(ipb, dp, CMD_SEEK, cmd);
			ipb->i[IO_BCHI] = BCHI(rec);
			ipb->i[IO_BCLO] = BCLO(rec);
		}
		break;

	case DRV_RETRYSEEK:
		dprintf(3, (" \\\\"));
		init_iopb(ipb, dp, SEEK_RECBACK, 1);
		break;
	}

	ipb->drv = dp->d_drv;
	dp->d_curipb = ipb;
	dp->d_status |= DRV_ACTIVE;

	return 0;
}

/*
 * update a buf with the results of a completed iopb.
 * there may be side-effects to the drive info.
 * this routine, with together with make_buf(), manipulates the
 * drive state machine.
 *
 * this may have been:
 *	a raw command
 *		pass back status in RAWIOPB
 *	a raw i/o
 *		and set resid
 *	a cooked i/o (or retry)
 *		set resid
 *		if error, possibly change to re-position state
 *	a cooked pre-seek to the correct position
 *		if no error, change to i/o state
 *	a cooked re-position after error
 *		if no error, change to i/o state
 */
static
update_buf(ipb, dp)
	register IOPB *ipb;
	register DRINFO *dp;
{
	register ulong a;
	register struct buf *bp;
	register int acount;
	unchar errno, cmd;

	bp = dp->d_tab.av_forw;
	ASSERT(bp != &dp->d_tab);
	ASSERT(ipb == dp->d_curipb);

	errno = ipb->i[IO_ERRNO];
	cmd = ipb->i[IO_CMD] & CMD_MASK;
	acount = BCCONS(ipb->i[IO_ACHI], ipb->i[IO_ACLO]);

	switch(dp->d_iostate) {

	case DRV_CMD:
		dprintf(2, (" cmd"));
		/*
		 * if this is a raw command, save the real
		 * iopb status so the caller can examine it.
		 */
		*dp->d_rawipb = *ipb;
		goto done;

	case DRV_IO:
		dprintf(2, (" io"));
		/*
		 * if writing, remember to write EOF on close.
		 * if reading, ignore short or long read errors.
		 */
		if (cmd == CMD_WRITE) {
			dp->d_status |= DRV_WRITTEN;
		}
		else {
			if (errno == E_REC_SHORT || errno == E_REC_LONG) {
				errno = 0;
			}
			else
			if (errno == E_RFMK) {
				dprintf(2, (" fmk"));
				dp->d_fileno++;
				errno = 0;
			}
		}
		/*
		 * maybe it worked ok?
		 */
		if (errno == 0) {
			dp->d_rec++;
			if (cmd == CMD_WRITE)
				dp->d_wrec = dp->d_rec;
			bp->b_resid = bp->b_bcount - acount;
			bp->b_flags &= ~B_ERROR;
if(bp->b_resid!=0)dprintf(2, (" resid%d", bp->b_resid));
			goto done;
		}
		/*
		 * if not, retry some number of times
		 */
		if (dp->d_retries < xm_maxretries) {
			dp->d_retries++;
			printf("xm: retry#%d\n", dp->d_retries);
			dp->d_iostate = DRV_RETRYSEEK;
			goto out;
		}
		goto bad;

	case DRV_PRESEEK:
		if (errno == 0) {
			dprintf(2, (" acount%d", acount));
			if (cmd == SEEK_RECBACK)
				dp->d_rec -= acount;
			else
				dp->d_rec += acount;
			goto out;
		}
		goto bad;

	case DRV_RETRYSEEK:
		if (errno == 0) {
			dp->d_iostate = DRV_IO;
			goto out;
		}
		goto bad;
	}

bad:
	/* an error occurred */
	bp->b_flags |= B_ERROR;
	dump_iopb(ipb, 0, XM_ISIZE);

done:
	/* finished with the current buf */
	bp->av_forw->av_back = bp->av_back;
	bp->av_back->av_forw = bp->av_forw;
	bp->av_forw = bp->av_back = 0;
	iodone(bp);
	dp->d_retries = 0;
	dp->d_iostate = DRV_IO;

out:
	/* finished with the current cmd */
	dp->d_status &= ~DRV_ACTIVE;
	ipb->drv = -1;
	dp->d_curipb = 0;
	ipb->next = dp->d_free;
	dp->d_free = ipb;
}

/*
 * set up a read or write request.
 * for now, truncate requests that are too large.
 */
static
xm_ioinit(ipb, dp, bp)
	register IOPB *ipb;
	register DRINFO *dp;
	register struct buf *bp;
{
	if (bp->b_bcount > XM_RWLIM)
		bp->b_bcount = XM_RWLIM;
	init_iopb(ipb, dp, bp->b_flags&B_READ?CMD_READ:CMD_WRITE, 0);
	xm_sgset(bp, ipb);
}

/*
 * start i/o on all drives attached to the given ctlr
 */
static
xm_start(ci)
	register CINFO *ci;
{
	register DRINFO *dp;

	for (dp = ci->c_drives; dp != 0; dp = dp->d_cchain)
		xm_dstart(dp);
}

/*
 * start i/o on a drive.  if there is already a command pending,
 * do nothing.  if there is nothing to do, do nothing.  else start
 * up the command of the frontmost buffer.  we use iopb's very
 * conservatively.
 */
static
xm_dstart(dp)
	register DRINFO *dp;
{
	register CINFO *ci;
	register struct buf *bp;
	register struct buf *tab;

	for (;;) {
		if (dp->d_status & DRV_ACTIVE)
			return;
		tab = &dp->d_tab;
		if ((bp = tab->av_forw) == tab)
			return;
		ci = dp->d_ci;
		if (XM_INREG(ci, XM_SR) & SR_AIOP)
			return;
		dprintf(2, (" D"));
		if (make_buf(dp, bp) == 0) {
ifdebug(128, dump_iopb(dp->d_curipb, 0, XM_ISIZE));
			stuff_addr(ci, dp->d_curipb);
		}
	}
}

/*
 * sanity check on returned iopb address
 */
static int
valid_iopb(ipb)
	register IOPB *ipb;
{
	if (ipb->drv >= xm_mcnt
	 || DRINFOPTR(ipb->drv) == 0
	 || DRINFOPTR(ipb->drv)->d_curipb != ipb)
		return -1;
	return 0;
}

/*
 * pass iopb address to ctlr.  ipb is in K1SPACE
 */
static
stuff_addr(ci, ipb)
	register CINFO *ci;
	IOPB *ipb;
{
	register ulong a;
	ASSERT(valid_iopb(ipb) == 0);
	dprintf(4, (" stuff $%x", ipb));
	a = K1_TO_PHYS(ipb);
	XM_OUTREG(ci, XM_AR3, ADDR3(a));
	XM_OUTREG(ci, XM_AR2, ADDR2(a));
	XM_OUTREG(ci, XM_AR1, ADDR1(a));
	XM_OUTREG(ci, XM_AR0, ADDR0(a));
	XM_OUTREG(ci, XM_AMOD, PHYSPACE);
	WBFLUSH();
	XM_OUTREG(ci, XM_CR, CR_AIO);
	WBFLUSH();
}

/*
 * fetch iopb address from ctlr.  return K1SPACE ptr
 */
static IOPB *
unstuff_addr(ci)
	register CINFO *ci;
{
	register ulong a;
	a = ADDRCONS(XM_INREG(ci, XM_AR0), XM_INREG(ci, XM_AR1),
			XM_INREG(ci, XM_AR2), XM_INREG(ci, XM_AR3));
	XM_OUTREG(ci, XM_CR, CR_CRIO);
	WBFLUSH();
	a = PHYS_TO_K1(a);
	if (valid_iopb((IOPB *)a) < 0) {
		printf("xm: invalid read addr $%x\n", a);
		return 0;
	}
	dprintf(4, (" unstuff $%x", a));
	return (IOPB *)a;
}

/*
 * ctlr reset
 */
static
xm_creset(ci)
	register CINFO *ci;
{
	register int i;
	dprintf(1, (" creset!"));
	XM_OUTREG(ci, XM_CR, CR_RST);
	WBFLUSH();
	for (i = 300; --i >= 0; xm_probing ? msdelay(33) : delay(1))
		if (!(XM_INREG(ci, XM_SR) & SR_RSTA))
			break;
}

/*
 * lock ctlr or drive variables
 */
static
xm_lock(lp)
	register int *lp;
{
	USEPRI;
	RAISE;
	while (*lp & LOCKBUSY) {
		*lp |= LOCKWANTED;
		sleep((caddr_t)lp, XMPRI);
	}
	*lp |= LOCKBUSY;
	LOWER;
}

/*
 * unlock ctlr or drive variables
 */
static
xm_unlock(lp)
	register int *lp;
{
	if (*lp & LOCKWANTED)
		wakeup((caddr_t)lp);
	*lp &= ~(LOCKBUSY|LOCKWANTED);
}

/*
 * initialize ctlr state with edt info
 */
static
xm_defctlr(ci, base, unit, vec, brl)
	register CINFO *ci;
	unchar *base;
	int unit, vec, brl;
{
	bzero((char *)ci, sizeof *ci);
	ci->c_base = base;
	ci->c_unit = unit;
	ci->c_params.i[IO_IVEC] = vec;
	ci->c_params.i[IO_LEVEL] = brl;
	ci->c_params.i[IO_CPARA] = CPARA_AUD;
	/* NOTE: as of fwrev A7, OVR not implemented */
	ci->c_params.i[IO_CPARB] = CPARB_ROR|(1<<CPARB_TDT_SHIFT);
	ci->c_params.i[IO_CPARC] = CPARC_WWD|CPARC_IEC;
	ci->c_params.i[IO_CTHROTTLE] = 0;
}

/*
 * probe a ctlr
 */
static
xm_cprobe(ci)
	register CINFO *ci;
{
	xm_dattach(ci, 0);
	xm_probing = 1;
	xm_creset(ci);
	xm_command(ci->c_drives, C_READPARAMS, 0);
	xm_probing = 0;
	xm_detach(ci->c_drives);
}

/*
 * attach a drive to this ctlr.  this consists of allocating an
 * unused drive state structure, initializing it, and chaining
 * it to the drive list
 */
static int
xm_dattach(ci, drv)
	register CINFO *ci;
	int drv;
{
	register DRINFO *dp;

	if (xm_dnext >= xm_dcnt)
		return -1;
	dp = xm_drives + xm_dnext++;
	DRINFOPTR(drv) = dp;
	bzero((char *)dp, sizeof *dp);
	dp->d_ci = ci;
	dp->d_cchain = ci->c_drives;
	ci->c_drives = dp;
	xm_defdrive(dp, drv, Cipher_info);
	return 0;
}

/*
 * initialize drive state when attaching
 */
xm_defdrive(dp, drv, info)
	register DRINFO *dp;
	int drv;
	register unchar *info;
{
	register struct buf *tab;
	register int i;
	register IOPB *ipb;

	dp->d_dens = -1;
	dp->d_iostate = DRV_IO;
	tab = &dp->d_tab;
	tab->av_forw = tab->av_back = tab;

	ipb = (IOPB *)K0_TO_K1(dp->d_iopbs);
	for (i = 0; i < XM_ICNT; i++) {
		ipb->next = dp->d_free;
		dp->d_free = ipb;
		ipb++;
	}

	dp->d_rawipb = dp->d_free;
	dp->d_free = dp->d_free->next;
	RAWBUF(dp)->b_dev = makedev(0, drv);
	dp->d_drv = drv;

	/* NOTE: the below iopb fields must Not overlap each other */
	dp->d_params.i[IO_IVEC] = dp->d_ci->c_params.i[IO_IVEC];
	dp->d_params.i[IO_LEVEL] = dp->d_ci->c_params.i[IO_LEVEL];
	dp->d_params.i[IO_UNIT] = DEVSLAVE(RAWBUF(dp)->b_dev);
	/* NOTE: GRTY overrides WRTY */
	dp->d_params.i[IO_DPARA] = DPARA_DSB|DPARA_RRTY
			|DPARA_GRTY|DPARA_WRTY;
	/* NOTE: cipher drive must use ~DSB method of setting densities.*/
	dp->d_params.i[IO_DPARA] &= ~DPARA_DSB;
	/* NOTE: as of fwrev A7, ASS not implemented */
	dp->d_params.i[IO_DPARB] = DPARB_IPOW|DPARB_SPD;
	dp->d_params.i[IO_D800] = *info++;
	dp->d_params.i[IO_D1600] = *info++;
	dp->d_params.i[IO_D3200] = *info++;
	dp->d_params.i[IO_D6250] = *info++;
	/* NOTE: as of fwrev A7, variable IRG not implemented */
	dp->d_params.i[IO_DIRG] = *info;
	dp->d_params.i[IO_DBUSYTIMER] = 0xFF;
}

/*
 * un-attach a drive (must be the last attached)
 */
static
xm_detach(dp)
	register DRINFO *dp;
{
	register CINFO *ci;
	register DRINFO **p;
	
	DRINFOPTR(dp->d_drv) = 0;
	ci = dp->d_ci;
	for (p = &ci->c_drives; *p != dp;)
		p = &dp->d_cchain;
	*p = dp->d_cchain;
	xm_dnext--;
}

/*
 * set up an iopb for a scatter / gather transfer.
 * for now, we do not use extended reads / write
 */
static
xm_sgset(bp, ipb)
	register struct buf *bp;
	register IOPB *ipb;
{
	struct sg vec[XM_MAXSG];
	register DRINFO *dp;
	register struct sg *sg;
	register struct xm_sg *xp;
	register ulong a;
	unsigned resid;
	register int nvec;

	dp = DRINFOPTR(DEVDRIVE(bp->b_dev));
	ASSERT(dp != 0);
	bp->b_resid = bp->b_bcount;
	nvec = sgset(bp, vec, XM_MAXSG, &resid);

	ipb->i[IO_LEVEL] = (ipb->i[IO_LEVEL]&~LEVEL_NSG_MASK)
			| (nvec<<LEVEL_NSG_SHIFT);

	xp = dp->d_sg;
	sg = vec;
	while (--nvec >= 0) {
		xp->sg_bchi = BCHI(sg->sg_bcount);
		xp->sg_bclo = BCLO(sg->sg_bcount);
		xp->sg_mod = PHYSPACE;
		xp->sg_ba0 = ADDR0(sg->sg_ioaddr);
		xp->sg_ba1 = ADDR1(sg->sg_ioaddr);
		xp->sg_ba2 = ADDR2(sg->sg_ioaddr);
		xp->sg_ba3 = ADDR3(sg->sg_ioaddr);
		xp++;
		sg++;
	}

 	a = K0_TO_PHYS(dp->d_sg);
	ipb->i[IO_DATA0] = ADDR0(a);
	ipb->i[IO_DATA1] = ADDR1(a);
	ipb->i[IO_DATA2] = ADDR2(a);
	ipb->i[IO_DATA3] = ADDR3(a);
	ipb->i[IO_BCHI] = BCHI(bp->b_bcount);
	ipb->i[IO_BCLO] = BCLO(bp->b_bcount);
	ipb->i[IO_CMD] |= CMD_SGM;
ifdebug(4, xm_sgprint(ipb));
}

/*
 * try to unwedge any hung magtape users, and set up to reset
 * on the next open.
 */
static
xm_unwedge(dp)
	register DRINFO *dp;
{
	register CINFO *ci;
	register struct buf *bp;

	if (!suser())
		return;
	dprintf(1, (" unwedge"));
	ci = dp->d_ci;
	ci->c_status &= ~CTLR_INITED;
	for (dp = ci->c_drives; dp != 0; dp = dp->d_cchain) {
		bp = RAWBUF(dp);
		bp->b_flags |= B_ERROR|B_DONE;
		wakeup((caddr_t)bp);
		dp->d_status &= ~DRV_INITED;
		dp++;
	}
}

# ifdef XGONG
/*
 * watchdog routine, arranges to call interrupt routine every few secs
 * just in case we miss an interrupt
 */
static
xm_gong(unit)
	int unit;
{
	register CINFO *ci;
	USEPRI;

	if ((ci = CINFOPTR(unit)) == 0 || ci->c_opens <= 0) {
		ci->c_status &= ~CTLR_GONG;
		return;
	}
	RAISE;
	(void)xmintr(unit);
	LOWER;
	ci->c_status |= CTLR_GONG;
	timeout(xm_gong, (caddr_t)unit, xm_interval*HZ);
}
# endif XGONG

# ifdef DBGFLAG
/*
 * debugging printouts
 */
static
dump_iopb(ipb, first, last)
	register IOPB *ipb;
	register int first, last;
{
	if (last >= XM_ISIZE-1)
		last = XM_ISIZE-1;
	while (first <= last)
		display_iopb(ipb, first++);
	printf("\n");
}
struct bitname {
	char *name;
	int shift;
	int mask;
	int val;
};
static
struct bitname xm_cmdbits[] = {
# define AAA(n, b) {n, 0, CMD_MASK, b}
	AAA("NOP", CMD_NOP),
	AAA("WRITE", CMD_WRITE),
	AAA("READ", CMD_READ),
	AAA("POSN", CMD_SEEK),
	AAA("DRST", CMD_DRESET),
	AAA("WPAR", CMD_WPAR),
	AAA("RPAR", CMD_RPAR),
	AAA("XWRITE", CMD_XWRITE),
	AAA("XREAD", CMD_XREAD),
	AAA("ABORT", CMD_ABORT),
	AAA("SPARAM", CMD_SPAR),
# undef AAA
# define AAA(n, b) {n, 0, b, b}
	AAA("SGM", CMD_SGM),
	AAA("CHEN", CMD_CHEN),
	AAA("DONE", CMD_DONE),
	AAA("ERRS", CMD_ERRS),
	{"-", 0, ~CMD_MASK, 0},
	0,
};
static
struct bitname xm_ccbits[] = {
	0
};
static
struct bitname xm_tsbits[] = {
	AAA("WPT", STAT2_WPT),
	AAA("EOT", STAT2_EOT),
	AAA("PEID", STAT2_PEID),
	AAA("FMK", STAT2_FMK),
	AAA("RLL", STAT2_RLL),
	AAA("RLS", STAT2_RLS),
	AAA("CER", STAT2_CER),
	AAA("HER", STAT2_HER),
	0
};
static
struct bitname xm_stat3bits[] = {
	AAA("ONLIN", STAT3_ONLINE),
	AAA("DRRDY", STAT3_DRRDY),
	AAA("FBSY", STAT3_FBSY),
	AAA("DBSY", STAT3_DBSY),
	AAA("REW", STAT3_REW),
	AAA("BOT", STAT3_BOT),
	AAA("HISD", STAT3_HISPD),
	AAA("GC", STAT3_GCNRZ),
	0
};
static
struct bitname xm_nullbits[] = {
	0
};
static
display_iopb(ipb, offset)
	register IOPB *ipb;
	register int offset;
{
	if (ipb == 0) {
		printf(" NULLIOPB");
		return;
	}
	switch (offset) {
        case IO_CMD:
		xm_showreg("CMD:", xm_cmdbits, ipb->i[offset]);
		break;
        case IO_ERRNO:
		xm_showreg("CCODE:", xm_ccbits, ipb->i[offset]);
		break;
        case IO_STAT2:
		xm_showreg("STAT2:", xm_tsbits, ipb->i[offset]);
		break;
        case IO_STAT3:
		xm_showreg("STAT3:", xm_stat3bits, ipb->i[offset]);
		break;
        case IO_FUNC:
		xm_showreg("SUBFUNC:", xm_nullbits, ipb->i[offset]);
		break;
        default:
		printf(" [$%x:0x%x]",offset, ipb->i[offset]);
		break;
        }
}
static
xm_showreg(str, btab, val)
	char *str;
	struct bitname *btab;
	unchar val;
{
	printf(" %s", str);
	xm_showbits(btab, (int)val);
}
static
xm_showbits(btab, val)
	struct bitname *btab;
	register int val;
{
	register struct bitname *x;
	register int matched;
	matched = 0;
	for (x = btab; x->name != 0; x++)
		if (((val>>x->shift)&x->mask) == x->val) {
			printf("%s%s", matched ? "|" : "", x->name);
			matched ++;
			
			val &= ~(x->mask<<x->shift);
		}
	if (val != 0 || !matched)
		printf("%s$%x", matched ? "|" : "", val);
}
static
xm_sgprint(ipb)
	register IOPB *ipb;
{
	register int i, nvec;
	register struct xm_sg *xp;
	register ulong a;
	register ushort bcount, ibcount;

	printf(" &$%x", ipb);
	nvec = ipb->i[IO_LEVEL]>>LEVEL_NSG_SHIFT;
	a = ADDRCONS(ipb->i[IO_DATA0], ipb->i[IO_DATA1],
			ipb->i[IO_DATA2], ipb->i[IO_DATA3]);
	ibcount = BCCONS(ipb->i[IO_BCHI], ipb->i[IO_BCLO]);
	printf("->$%x/%d", a, ibcount);
	a = PHYS_TO_K1(a);
	xp = (struct xm_sg *)a;
	bcount = 0;
	for (i = 0; i < nvec; i++) {
		bcount += BCCONS(xp->sg_bchi, xp->sg_bclo);
		xp++;
	}
	xp = (struct xm_sg *)a;
	printf(" {%d:", bcount);
	for (i = 0; i < nvec; i++) {
		a = ADDRCONS(xp->sg_ba0, xp->sg_ba1,
				xp->sg_ba2, xp->sg_ba3);
		bcount = BCCONS(xp->sg_bchi, xp->sg_bclo);
		printf(" $%x/%d", a, bcount);
		xp++;
	}
	printf("}\n");
}
# endif DBGFLAG

/*
 * spin for awhile (supposedly n ms) when can't sleep
 */
static
msdelay(n)
	int n;
{
	register VOLATILE i;
	while (--n >= 0)
		for (i = 8000; --i >= 0;)
			;
}
