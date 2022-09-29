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
#ident	"$Header: xyl712.c,v 1.7.3.4 90/05/10 05:39:14 wje Exp $"

/*
 * Driver for the Xylogics 712 disk controller.
 */
#include "sys/debug.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/dir.h"
#include "sys/errno.h"
#include "sys/pcb.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/cmn_err.h"
#include "sys/edt.h"
#include "sys/vmereg.h"
#include "sys/dvh.h"
#include "sys/dkio.h"
#include "sys/buf.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/region.h"
#define	b_cyl		b_resid
#define	b_active	b_bcount
#include "sys/xyl712reg.h"

/* random stats */
long	xylintr_starts;

/* maximum number of controllers we support */
#define	CTLRS	2

/* XXX */
char	xylvhbuf[BBTOB(BTOBB(sizeof(struct volume_header)))];

paddr_t	xyl_addr[CTLRS];

/* forward references */
int	xylprobe(), xylslave(), xylstrategy(), xylintr(), xylpoll(),
	xylResetCtlr(), xylResetDrive(), xylcommand();
void	xylattach(), xylformat();

/*
 * Probe the given controller and if it exists, initialize it.
 */
int
xyledtinit(edt)
	struct edt *edt;
{
	register volatile struct xyldevice *dp;
	register struct xylctlrinfo *ci;
	register struct xylunitinfo *ui;
	int ctlr;
	int i;

	dp = (struct xyldevice *)edt->e_base;
	ctlr = edt->e_intr_info->v_unit;

	/* probe for existence of board and put board in known state */
	if (badaddr(&dp->d_csr, sizeof(dp->d_csr))) {
		printf("xyl%d: missing\n", ctlr);
		return (0);
	}

	/* setup software state */
	ci = &xylctlrinfo[ctlr];
	ci->ci_ctlr = ctlr;
	ci->ci_vec = edt->e_intr_info->v_vec;
	ci->ci_ipl = edt->e_intr_info->v_brl;
	ci->ci_device = (struct xyldevice *) dp;

	ui = &ci->ci_unit[0];
	for (i = 0; i < XYLUPC; i++, ui++) {
		ui->ui_ci = ci;
		ui->ui_unit = i;
	}

	/* reset the controller and gets its parameters */
	if (xylResetCtlr(ci, dp) == 0) {
		return (0);
	}
	ci->ci_probed = 1;
	return (1);
}

/*
 * See if the given sub-device is present on the given controller
 */
int
xylslave(ui)
	struct xylunitinfo *ui;
{
	register volatile struct xylDriveIopb *xi;

	xi = iopb_drv(ui, 0);
	if (xylpoll(ui, CMDIOPB(xi), CMD_NOP, 0)) {
		/* oops, busted controller */
		return (0);
	}
	if (!(xi->xi_status2 & ST2_DRDY)) {
		/* no such drive */
		return (0);
	}
	if (xi->xi_status2 & ST2_ATTN) {
		/* try to clear drive fault */
		if (xylpoll(ui, CMDIOPB(xi), CMD_RESET, CMD_RESET_FAULT)) {
			/* oops, busted contoroller */
			return (0);
		}
	}
	return (1);
}

/*
 * Attach a drive to the controller
 */
void
xylattach(ui)
	struct xylunitinfo *ui;
{
	register volatile struct xylCmdIopb *xi;
	register struct volume_header *vh;

	/*
	 * Try to read in the label
	 */
	xi = iopb_cmd(ui, 0);
	xi->xi_addr = K0_TO_PHYS(xylvhbuf);
	xi->xi_addrMod = VME_A32NPAMOD;
	xi->xi_count = BTOBB(sizeof(struct volume_header));
	xi->xi_cyl = 0;
	xi->xi_head = 0;
	xi->xi_sector = 0;
	if (xylpoll(ui, xi, CMD_READ, 0)) {
		printf("XXX xylattach: can't read volume header\n");
		goto bad;
	}
	vh = (struct volume_header *) K0_TO_K1(&xylvhbuf[0]);
	if (vh->vh_magic != VHMAGIC) {
		printf("XXX xylattach, magic is wrong: %x\n", vh->vh_magic);
		goto bad;
	}
	bcopy(vh, &ui->ui_vh, sizeof(ui->ui_vh));
	ui->ui_spc = ui->ui_vh.vh_dp.dp_secs * ui->ui_vh.vh_dp.dp_trks0;

	/*
	 * Tell controller about the drive
	 */
	if (xylpoll(ui, xi, CMD_RPARAMS, CMD_RPARAMS_CONFIG)) {
		printf("XXX xylattach, can't read params\n");
		goto bad;
	}
printDriveParams(xi, "Before");

	DRVIOPB(xi)->xi_driveParam = 0;
	DRVIOPB(xi)->xi_headOffset = 0;
	DRVIOPB(xi)->xi_maxHead = ui->ui_vh.vh_dp.dp_trks0 - 1;
	DRVIOPB(xi)->xi_maxCyl = ui->ui_vh.vh_dp.dp_cyls - 1;
	DRVIOPB(xi)->xi_maxSector = ui->ui_vh.vh_dp.dp_secs - 1;
	DRVIOPB(xi)->xi_maxSpareSector = DRVIOPB(xi)->xi_maxSector;
	DRVIOPB(xi)->xi_sectorsPerTrack = ui->ui_vh.vh_dp.dp_secs;
printDriveParams(xi, "After");
	if (xyldopoll(ui, xi, CMD_WPARAMS, CMD_WPARAMS_DRIVE)) {
		printf("XXX xylattach, can't write params\n");
		goto bad;
	}

bad:
	/*
	 * Mark attached, even when an error occurs, so that we get a
	 * chance to format the drive
	 */
	ui->ui_attached = 1;
}

/*
 * Open the given device.  If controller or unit is out of range, return
 * an error.  If the controller didn't probe, return an error.  If the
 * sub-device doesn't exist, or doesn't work, return an error.
 */
xylopen(dev, flag)
	dev_t dev;
	int flag;
{
	int ctlr;
	int unit;
	struct xylctlrinfo *ci;
	struct xylunitinfo *ui;

	ctlr = CTLR(dev);
	unit = UNIT(dev);
	if ((ctlr >= CTLRS) || (unit >= XYLUPC) ||
	    (!(ci = &xylctlrinfo[ctlr])->ci_probed)) {
		u.u_error = ENXIO;
		return;
	}

	/* attach drive if its not already attached */
	ui = &ci->ci_unit[unit];
	if (!(ui->ui_attached)) {
		if (xylslave(ui))
			xylattach(ui);
		if (!ui->ui_attached)
			u.u_error = ENXIO;
	}
}

xylclose(dev, flag)
	dev_t dev;
	int flag;
{
	/* NOP */
}

xylread(dev)
	dev_t dev;
{
	physio(xylstrategy, 0, dev, B_READ);
}

xylwrite(dev)
	dev_t dev;
{
	physio(xylstrategy, 0, dev, B_WRITE);
}

xylprint(dev, str)
	dev_t dev;
	char *str;
{
	cmn_err(CE_NOTE, "xyl%dd%ds%d: %s on dev 0x%x\n",
			 CTLR(dev), UNIT(dev), SLICE(dev), str, dev);
}

struct fmtdata {
	int	f1, f2, f3, f4, f5, f6, f7;
};
xylioctl(dev, cmd, arg, flag)
	dev_t dev;
	int cmd;
	caddr_t arg;
	int flag;
{
	struct fmtdata fmtdata;
#ifdef	notdef
	struct fmt_map_info fmi;

	switch (cmd) {
	  case 0:
		xylfmt(dev);
		break;
	  case DIOCFMTTRK:
		if (copyin(arg, (caddr_t) &fmi, sizeof(fmi)) < 0)
			u.u_error = EFAULT;
		else
			xylformat(dev, &fmi);
		break;
	  case DIOCVFYSEC:
		break;
	  case DIOCGETCTLR:
		break;
	  case DIOCDIAG:
		break;
	  case DIOCSETDP:
		break;
	  case DIOCGETVH:
		break;
	  default:
		u.u_error = ENXIO;
		break;
	}
#endif
	switch (cmd) {
	  case 0:

		if (copyin(arg, &fmtdata, sizeof(fmtdata)) < 0)
			u.u_error = EFAULT;
		else
			xylfmt(dev, &fmtdata);
		break;
	  case 1:
		xylPrintDriveInfo(dev);
		break;
	  default:
		u.u_error = ENXIO;
		break;
	}
}

/*
 * Put a buffer on the controller queue
 */
xylstrategy(bp)
	struct buf *bp;
{
	struct xylctlrinfo *ci;
	struct xylunitinfo *ui;
	struct partition_table *pt;
	int ctlr, unit, slice;
	int s;
	int secs;

	ctlr = CTLR(bp->b_dev);
	unit = UNIT(bp->b_dev);
	slice = SLICE(bp->b_dev);
	if (ctlr >= CTLRS)
		goto bad;

	ci = &xylctlrinfo[ctlr];
	ui = &ci->ci_unit[unit];

	/*
	 * Make sure request is within the partitions boundaries
	 */
	pt = &ui->ui_vh.vh_pt[slice];
	secs = (bp->b_bcount + NBPSCTR - 1) >> SCTRSHFT;
	if ((bp->b_blkno < 0) || ((bp->b_blkno + secs) > pt->pt_nblks)) {
		goto bad;
	}
/* XXX temporary */
if (bp->b_bcount & (NBPSCTR - 1)) {
	printf("XXX xylstrat: bad count\n");
	goto bad;
}
if ((long) bp->b_un.b_addr & 1) {
	printf("XXX xylstrat: odd addr\n");
	goto bad;
}
if ((long) bp->b_un.b_addr & (NBPSCTR - 1)) {
	printf("XXX xylstrat: bad addr\n");
	goto bad;
}

	/* set cylinder number for disksort() */
	bp->b_cyl = (bp->b_blkno + pt->pt_firstlbn) / ui->ui_spc;
	bp->av_forw = NULL;

	/* queue request */
	s = splclock();
	iomap(bp);
	disksort(&ui->ui_tab, bp);
	if (!ui->ui_tab.b_active) {
		xylustart(ui);
		bp = &ci->ci_tab;
		if (bp->av_forw && !bp->b_active)
			xylcstart(ci);
	}
	splx(s);

#ifdef	notdef
	/* setup accouting info for sar */
	bp->b_start = lbolt;				/* XXX fix me */
	dktime[xyloffset][unit].io_cnt++;		/* XXX fix me */
	dktime[xyloffset][unit].io_bcnt += secs;	/* XXX fix me */
#endif
	return;

bad:
	bp->b_flags |= B_ERROR;
	iounmap(bp);
	iodone(bp);
}

/*
 * unit start routine
 */
xylustart(ui)
	struct xylunitinfo *ui;
{
	register struct xylctlrinfo *ci;
	register struct buf *utab;

	/*
	 * If unit is already active, or if the units queue is empty,
	 * then don't do anything
	 */
	utab = &ui->ui_tab;
	if (utab->b_active || !utab->av_forw)
		return;

	/*
	 * Put unit on controller queue.  Unit table headers are put on
	 * the controller queue using the unit table b_forw pointer.
	 * XXX doesn't need to be doubly linked.
	 * XXX fix this to use the b_forw pointer in the controller queue
	 * XXX for consistencies sake.
	 */
	ci = ui->ui_ci;
	if (ci->ci_tab.av_forw == NULL)
		ci->ci_tab.av_forw = utab;
	else
		ci->ci_tab.av_back->b_forw = utab;
	ci->ci_tab.av_back = utab;
	utab->b_forw = NULL;
	utab->b_active = 1;

	/* note start of unit for sar */
	utab->b_start = lbolt;				/* XXX fix me */
}

/*
 * Controller start routine
 */
xylcstart(ci)
	register struct xylctlrinfo *ci;
{
	register struct buf *utab;
	register struct buf *bp;
	register struct xylunitinfo *ui;

loop:
	/*
	 * Get first unit off of controllers queue
	 */
	if ((utab = ci->ci_tab.av_forw) == NULL) {
		/* queue is empty */
		return;
	}
	/*
	 * Get first command off of unit table
	 */
	if ((bp = utab->av_forw) == NULL) {
		/* unit on controller queue is idle */
		utab->b_active = 0;
		ci->ci_tab.av_forw = utab->b_forw;
		utab->b_forw = NULL;
		goto loop;
	}
	ci->ci_tab.b_active = 1;
	ui = &ci->ci_unit[UNIT(bp->b_dev)];

	/* start command going */
	bp->b_resid = bp->b_bcount;
	ui->ui_bn = bp->b_blkno +
		ui->ui_vh.vh_pt[SLICE(bp->b_dev)].pt_firstlbn;
	if (xylcommand(ui, bp))
		goto loop;
	return;

bad:
	ci->ci_tab.b_active = 0;
	utab->av_forw = bp->av_forw;
	bp->b_flags |= B_ERROR;
	iounmap(bp);
	iodone(bp);
	goto loop;
}

/*
 * Setup and fire off a command to the controller
 */
int
xylcommand(ui, bp)
	register struct xylunitinfo *ui;
	register struct buf *bp;
{
	register volatile struct xylCmdIopb *xi;
	register volatile struct xyldevice *dp;
	unsigned amount;
	int timo;
	int temp;
	long offset;
	long dmaaddr;

#ifdef	SGWORKS
{
	register volatile struct xylsg *sg;
	int links;

	xi = iopb_cmd(ui, 0);
	if (bp->b_flags & B_READ)
		xi->xi_cmd = CMD_READ;
	else
		xi->xi_cmd = CMD_WRITE;
	xi->xi_subCmd = 0;
	xi->xi_unit = ui->ui_unit | UNIT_FIXD;
	xi->xi_count = bp->b_bcount >> SCTRSHFT;
	xi->xi_cyl = ui->ui_bn / ui->ui_spc;
	temp = ui->ui_bn % ui->ui_spc;
	xi->xi_head = temp / ui->ui_vh.vh_dp.dp_secs;
	xi->xi_sector = temp % ui->ui_vh.vh_dp.dp_secs;

	/*
	 * Now fill in scatter gather info
	 */
	sg = (volatile struct xylsg *) K0_TO_K1(&ui->ui_sg[0]);
	dmaaddr = (long) bp->b_dmaaddr;
	links = 0;
	while (bp->b_resid) {
		/*
		 * Figure out how much we can transfer, before crossing
		 * a page boundary
		 */
		amount = bp->b_resid;
		offset = dmaaddr & (NBPP - 1);
		if (offset + amount > NBPP)
			amount = NBPP - offset;
		sg->sg_count = amount;
		sg->sg_addr = pttob(kvtokptbl(dmaaddr)->pgm.pg_pfn) | offset;
		sg->sg_mod = VME_A32NPAMOD;
		sg++;
		dmaaddr += amount;
		bp->b_resid -= amount;
		links++;
		if (links > NSCAT) {
printf("XXX xylcommand: too many links needed, %d\n", links);
			bp->b_flags |= B_ERROR;
			iodone(bp);
			return (1);
			break;
		}
	}
	xi->xi_intLevel = ui->ui_ci->ci_ipl;
	xi->xi_intVector = ui->ui_ci->ci_vec;
	if (links != 1) {
		xi->xi_intLevel |= links << ILEVEL_LLL_SHIFT;
		xi->xi_cmd |= CMD_SGM;
		xi->xi_addr = K0_TO_PHYS(&ui->ui_sg[0]);
	} else {
		xi->xi_addr = ui->ui_sg[0].sg_addr;
	}
	xi->xi_addrMod = VME_A32NPAMOD;
	xi->xi_nextIopbMod = 0;
	xi->xi_nextIopb = 0;
	xi->xi_iopbChecksum = 0;
	wbflush();
}
#else
{
	register daddr_t bn;

	/*
	 * Build up iopbs
	 */
	dmaaddr = (long) bp->b_dmaaddr;
	ASSERT((dmaaddr & (NBPSCTR - 1)) == 0);
	bn = ui->ui_bn;
	xi = iopb_cmd(ui, 0);
	while (bp->b_resid) {
		if (bp->b_flags & B_READ)
			xi->xi_cmd = CMD_READ;
		else
			xi->xi_cmd = CMD_WRITE;
		xi->xi_subCmd = 0;
		xi->xi_unit = ui->ui_unit | UNIT_FIXD;
		xi->xi_cyl = bn / ui->ui_spc;
		temp = bn % ui->ui_spc;
		xi->xi_head = temp / ui->ui_vh.vh_dp.dp_secs;
		xi->xi_sector = temp % ui->ui_vh.vh_dp.dp_secs;

		/* figure number of sectors to xfer in this page */
		amount = bp->b_resid;
		offset = dmaaddr & (NBPP - 1);
		if (offset + amount > NBPP)
			amount = NBPP - offset;
		ASSERT((amount & (NBPSCTR - 1)) == 0);
		xi->xi_count = amount >> SCTRSHFT;

		xi->xi_addr = pttob(kvtokptbl(dmaaddr)->pgm.pg_pfn) | offset;
		xi->xi_addrMod = VME_A32NPAMOD;
		xi->xi_intLevel = ui->ui_ci->ci_ipl;
		xi->xi_intVector = ui->ui_ci->ci_vec;
		xi->xi_iopbChecksum = 0;

		bp->b_resid -= amount;
		if (bp->b_resid) {
			dmaaddr += amount;
			bn += amount >> SCTRSHFT;
			xi->xi_cmd |= CMD_CHEN;
			xi->xi_nextIopbMod = VME_A32NPAMOD;
			xi->xi_nextIopb = K0_TO_PHYS(xi + 1);
			xi++;
		} else {
			xi->xi_nextIopbMod = 0;
			xi->xi_nextIopb = 0;
		}
	}
	wbflush();
	/* restore xi to point to the **first** iopb in chain */
	xi = iopb_cmd(ui, 0);
}
#endif

	/* start command */
	dp = ui->ui_ci->ci_device;
	timo = 100000;
	while ((dp->d_csr & CSR_AIO) && timo) {
		timo--;
	}
	if (timo == 0) {
printf("XXX xylcommand: timeout waiting for AIO to clear, csr=%x\n", dp->d_csr);
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return (1);
	}

	/* give controller the iopb */
	timo = K0_TO_PHYS((long) xi);
	dp->d_iopbAddr0 = ((long) timo);
	dp->d_iopbAddr1 = ((long) timo) >> 8;
	dp->d_iopbAddr2 = ((long) timo) >> 16;
	dp->d_iopbAddr3 = ((long) timo) >> 24;
	dp->d_iopbMod = VME_A32NPAMOD;
	dp->d_csr = CSR_AIO;			/* go! */
	wbflush();

	return (0);
}

/*
 * Handle an interrupt for the given controller
 */
xylintr(ctlr)
	int ctlr;
{
	struct xylctlrinfo *ci;
	struct xylunitinfo *ui;
	volatile struct xyldevice *dp;
	volatile struct xylCmdIopb *xi;
	struct buf *bp, *utab;

	ci = &xylctlrinfo[ctlr];

	/* get first command waiting to complete off of queues */
	utab = ci->ci_tab.av_forw;		/* get unit table */
	bp = utab->av_forw;
	ui = &ci->ci_unit[UNIT(bp->b_dev)];
	xi = iopb_cmd(ui, 0);
	dp = ci->ci_device;

	/* machine dependent */
	if (dp->d_csr & CSR_FATAL) {
printf("XXX xylintr: fatal error\n");
		bp->b_flags |= B_ERROR;
	} else
	if (xi->xi_status1) {
		if (xi->xi_status1 != 0x30) {
printf("XXX xylintr: error, s1=%x s2=%x\n", xi->xi_status1, xi->xi_status2);
			bp->b_flags |= B_ERROR;
			XXXdumpiopb("i/o err", (char *) xi);
		} else {
printf("XXX xylintr: soft err at cyl=%d head=%d sector=%d\n",
	    xi->xi_cyl, xi->xi_head, xi->xi_sector);
		}
	} else {
		/* no errors... */
	}

	/* clear interrupt */
	dp->d_csr = CSR_RIO;
	wbflush();

	/* advance ctlr queue */
	ci->ci_tab.b_active = 0;
	ci->ci_tab.av_forw = utab->b_forw;
	utab->b_forw = NULL;

	/* advance unit queue */
	utab->b_active = 0;
	utab->av_forw = bp->av_forw;

#ifdef	notdef
	/* update accounting */
	dktime[xyloffset][unit].io_resp += lbolt - bp->b_start; /* xyl sar */
	dktime[xyloffset][unit].io_act += lbolt - dp->b_start;  /* xyl sar */
#endif

	/* start next activity */
	if (utab->av_forw)
		xylustart(ui);
	if (ci->ci_tab.av_forw && !ci->ci_tab.b_active) {
		xylintr_starts++;
		xylcstart(ci);
	}

	/* finally, mark the completed buffer done */
	iounmap(bp);
	iodone(bp);
}

/*
 * Reset the given controller.  Then get its configuration info
 * and print it.
 */
int
xylResetCtlr(ci, dp)
	struct xylctlrinfo *ci;
	register volatile struct xyldevice *dp;
{
	register volatile struct xylCtlrIopb *xi;
	int timo;

	/* reset controller */
	dp->d_csr = CSR_RESET;
	wbflush();
	timo = 5000;
	while ((dp->d_csr & CSR_RESET) && timo) {
		timo--;
		DELAY(1000);
	}
	if (timo == 0)
		return (0);

	/* get controller parameters */
	xi = iopb_ctlr(&ci->ci_unit[0], 0);
	if (xylpoll(&ci->ci_unit[0], CMDIOPB(xi), CMD_RPARAMS,
				     CMD_RPARAMS_CTLR)) {
		printf("xyl%d: can't get controller parameters\n", ci->ci_ctlr);
		return (0);
	}
	printf("xyl%d prom part number 0x%x%x, rev %d.%d\n",
		      ci->ci_ctlr, xi->xi_epromHigh, xi->xi_epromLow,
		      xi->xi_rev, xi->xi_subRev);
XXXdumpiopb("ctlr", (char *) xi);

	/* setup controller parameters */
	xi->xi_cparamA = CPA_TMOD | CPA_EDT;
	xi->xi_cparamB = 0;
	xi->xi_cparamC = CPC_OVS | CPC_IEC | CPC_COP | CPC_ASR | CPC_ZLR |
				CPC_RBC | CPC_ECCM2;
	xi->xi_throttle = 0;
	if (xylpoll(&ci->ci_unit[0], CMDIOPB(xi), CMD_WPARAMS,
				     CMD_WPARAMS_CTLR)) {
		printf("xyl%d: can't set controller parameters\n", ci->ci_ctlr);
		return (0);
	}
	return (1);
}

int
xylpoll(ui, xi, cmd, subcmd)
	struct xylunitinfo *ui;
	volatile struct xylCmdIopb *xi;
	int cmd, subcmd;
{
	xi->xi_intLevel = 0;
	return (xyldopoll(ui, xi, cmd, subcmd));
}

int
xyldopoll(ui, xi, cmd, subcmd)
	struct xylunitinfo *ui;
	register volatile struct xylCmdIopb *xi;
	int cmd, subcmd;
{
	register volatile struct xyldevice *dp;
	long timo;

	xi->xi_cmd = cmd;
	xi->xi_subCmd = subcmd;
	xi->xi_intVector = ui->ui_ci->ci_vec;
	xi->xi_unit = ui->ui_unit | UNIT_FIXD;
	xi->xi_nextIopbMod = 0;
	xi->xi_nextIopb = 0;
	xi->xi_iopbChecksum = 0;
	wbflush();

	/* poll controller until we can give it an iopb */
	dp = ui->ui_ci->ci_device;
	timo = 100000;
	while ((dp->d_csr & CSR_AIO) && timo) {
		timo--;
	}
	if (timo == 0) {
printf("XXX xylcmd: timeout waiting for AIO to clear, csr=%x\n", dp->d_csr);
		return (1);
	}

	/* give controller the iopb */
	timo = K0_TO_PHYS((long) xi);
	dp->d_iopbAddr0 = ((long) timo);
	dp->d_iopbAddr1 = ((long) timo) >> 8;
	dp->d_iopbAddr2 = ((long) timo) >> 16;
	dp->d_iopbAddr3 = ((long) timo) >> 24;
	dp->d_iopbMod = VME_A32NPAMOD;
	dp->d_csr = CSR_AIO;			/* go! */
	wbflush();

	/* wait for command to complete */
	timo = 1000000;
	while (!(dp->d_csr & CSR_RIO) && timo) {
		timo--;
		DELAY(10);
	}
	if (timo == 0) {
		printf("XXX xylPoll timeout: csr=%x err=%x\n",
			    dp->d_csr, dp->d_err);
		return (1);
	}
	if (dp->d_err) {
		printf("XXX xylPoll: fatal err=%x\n", dp->d_err);
		xylResetCtlr(ui->ui_ci, dp);
		return (1);
	}
	if (xi->xi_status1) {
		printf("XXX xylpoll: csr=%x s1=%x s2=%x\n",
			    dp->d_csr, xi->xi_status1, xi->xi_status2);
XXXdumpiopb("err dump", (char *) xi);
		dp->d_csr = CSR_RIO;
		wbflush();
		return (1);
	}

	/* now clear RIO to let controller know we have the iopb */
	dp->d_csr = CSR_RIO;
	wbflush();
	return (0);
}

/*
 * Format the given drive.
 */
xylfmt(dev, fmtdata)
	dev_t dev;
	struct fmtdata *fmtdata;
{
	register struct xylunitinfo *ui;
	register struct volume_header *vh;
	register volatile union xyliopb *xi;
	int maxhead, maxsec, maxcyl;
	register int cyl;

	/*
	 * Read drive configuration
	 */
	ui = &xylctlrinfo[CTLR(dev)].ci_unit[UNIT(dev)];
	xi = iopb_u(ui, 0);
	if (xylpoll(ui, CMDIOPB(xi), CMD_RPARAMS, CMD_RPARAMS_CONFIG)) {
		printf("xylfmt: read config failed\n");
		u.u_error = EIO;
		return;
	}
	maxhead = DRVIOPB(xi)->xi_maxHead + 1;
	maxsec = DRVIOPB(xi)->xi_maxSector + 1;
	maxcyl = DRVIOPB(xi)->xi_maxCyl + 1;
	ui->ui_spc = maxhead * maxsec;
printDriveParams(xi, "FMT");

	/* write drive parameters back */
	DRVIOPB(xi)->xi_sectorsPerTrack = DRVIOPB(xi)->xi_maxSector;
	DRVIOPB(xi)->xi_maxSpareSector = DRVIOPB(xi)->xi_maxSector;
	if (xyldopoll(ui, CMDIOPB(xi), CMD_WPARAMS, CMD_WPARAMS_DRIVE)) {
		printf("xylfmt: write params failed\n");
		u.u_error = EIO;
		return;
	}

	/* read format parameters */
	if (xylpoll(ui, CMDIOPB(xi), CMD_RPARAMS, CMD_RPARAMS_FORMAT)) {
		printf("xylfmt: read format failed\n");
		u.u_error = EIO;
		return;
	}
printf("OLD: f1=%x f2=%x f3=%x f4=%x f5=%x f6=%x f7=%x\n",
	     FMTIOPB(xi)->xi_field1, FMTIOPB(xi)->xi_field2,
	     FMTIOPB(xi)->xi_field3, FMTIOPB(xi)->xi_field4,
	     FMTIOPB(xi)->xi_sectorSize,
	     FMTIOPB(xi)->xi_field6, FMTIOPB(xi)->xi_field7);

	/*
	 * Write format parameters.  The book states that one should only
	 * modify the sector size, so thats all we do.
	 */
/*
	FMTIOPB(xi)->xi_field1 = 0x0f;
	FMTIOPB(xi)->xi_field2 = 0x05;
	FMTIOPB(xi)->xi_field3 = 0x1a;
	FMTIOPB(xi)->xi_field4 = 0x0c;
	FMTIOPB(xi)->xi_field6 = 0x05;
	FMTIOPB(xi)->xi_field7 = 0x02;

	FMTIOPB(xi)->xi_field1 = 12 + 3;
	FMTIOPB(xi)->xi_field2 = 13;
	FMTIOPB(xi)->xi_field3 = 12 + 3 + 13;
	FMTIOPB(xi)->xi_field4 = 2 + 13;
	FMTIOPB(xi)->xi_field6 = 13;
	FMTIOPB(xi)->xi_field7 = 2;
*/
	FMTIOPB(xi)->xi_field1 = fmtdata->f1;
	FMTIOPB(xi)->xi_field2 = fmtdata->f2;
	FMTIOPB(xi)->xi_field3 = fmtdata->f3;
	FMTIOPB(xi)->xi_field4 = fmtdata->f4;
	FMTIOPB(xi)->xi_field6 = fmtdata->f6;
	FMTIOPB(xi)->xi_field7 = fmtdata->f7;

	FMTIOPB(xi)->xi_sectorSize = NBPSCTR;
	FMTIOPB(xi)->xi_otherSectorSize = NBPSCTR;
printf("NEW: f1=%x f2=%x f3=%x f4=%x f5=%x f6=%x f7=%x\n",
	     FMTIOPB(xi)->xi_field1, FMTIOPB(xi)->xi_field2,
	     FMTIOPB(xi)->xi_field3, FMTIOPB(xi)->xi_field4,
	     FMTIOPB(xi)->xi_sectorSize,
	     FMTIOPB(xi)->xi_field6, FMTIOPB(xi)->xi_field7);
	if (xylpoll(ui, CMDIOPB(xi), CMD_WPARAMS, CMD_WPARAMS_FORMAT)) {
		printf("xylfmt: write format failed\n");
		u.u_error = EIO;
		return;
	}

	/* format the drive */
	for (cyl = 0; cyl < maxcyl; cyl++) {
		CMDIOPB(xi)->xi_count = maxhead;
		CMDIOPB(xi)->xi_cyl = cyl;
		CMDIOPB(xi)->xi_head = 0;
		if (xylpoll(ui, CMDIOPB(xi), CMD_EWRITE, CMD_EWRITE_FORMAT)) {
			printf("xylfmt: format cyl failed\n");
			u.u_error = EIO;
			return;
		}
		if ((cyl % 10) == 0)
			printf("%d ", cyl);
	}
	printf("\nFormat complete\n");

	/* fill in label */
	vh = &ui->ui_vh;
	bzero(vh, sizeof(*vh));
	vh->vh_magic = VHMAGIC;
	vh->vh_rootpt = 0;
	vh->vh_swappt = 1;
	strcpy(vh->vh_bootfile, "sash");
	vh->vh_dp.dp_cyls = maxcyl;
	vh->vh_dp.dp_trks0 = maxhead;
	vh->vh_dp.dp_secs = maxsec;
	vh->vh_dp.dp_secbytes = NBPSCTR;

	vh->vh_pt[0].pt_nblks = 100 * maxhead * maxsec;
	vh->vh_pt[0].pt_firstlbn = 4 * maxhead * maxsec;
	vh->vh_pt[0].pt_type = PTYPE_EFS;

	vh->vh_pt[1].pt_nblks = 300 * maxhead * maxsec;
	vh->vh_pt[1].pt_firstlbn =
		vh->vh_pt[0].pt_firstlbn + vh->vh_pt[0].pt_nblks;
	vh->vh_pt[1].pt_type = PTYPE_EFS;

	vh->vh_pt[6].pt_nblks = 419 * maxhead * maxsec;
	vh->vh_pt[6].pt_firstlbn =
		vh->vh_pt[1].pt_firstlbn + vh->vh_pt[1].pt_nblks;
	vh->vh_pt[6].pt_type = PTYPE_EFS;

	vh->vh_pt[8].pt_nblks = 2 * maxhead * maxsec;
	vh->vh_pt[8].pt_firstlbn = 0;
	vh->vh_pt[8].pt_type = PTYPE_EFS;

	vh->vh_pt[9].pt_nblks = 2 * maxhead * maxsec;
	vh->vh_pt[9].pt_firstlbn = 2 * maxhead * maxsec;
	vh->vh_pt[9].pt_type = PTYPE_EFS;
	vh->vh_csum = -vhchecksum(vh);

	/* write label to disk */
	CMDIOPB(xi)->xi_addr = K0_TO_PHYS(vh);
	CMDIOPB(xi)->xi_addrMod = VME_A32NPAMOD;
	CMDIOPB(xi)->xi_count = BTOBB(sizeof(*vh));
	CMDIOPB(xi)->xi_cyl = 0;
	CMDIOPB(xi)->xi_head = 0;
	CMDIOPB(xi)->xi_sector = 0;
	if (xylpoll(ui, CMDIOPB(xi), CMD_WRITE, 0)) {
		printf("xylfmt: write of vh failed\n");
		u.u_error = EIO;
		return;
	}
}

printDriveParams(xi, msg)
	volatile struct xylDriveIopb *xi;
	char *msg;
{
printf("%s:\tdparam=0x%x maxSector=%d headOffset=%d maxCyl=%d maxHead=%d\n",
			 msg,
			 DRVIOPB(xi)->xi_driveParam,
			 DRVIOPB(xi)->xi_maxSector,
			 DRVIOPB(xi)->xi_headOffset,
			 DRVIOPB(xi)->xi_maxCyl,
			 DRVIOPB(xi)->xi_maxHead);
printf("\tspt=%d maxSpare=%d\n",
		 DRVIOPB(xi)->xi_sectorsPerTrack,
		 DRVIOPB(xi)->xi_maxSpareSector);
}

/*
 * Print everything we know about the given drive
 */
xylPrintDriveInfo(dev)
	dev_t dev;
{
	register struct xylunitinfo *ui;
	register volatile union xyliopb *xi;

	/*
	 * Read drive configuration
	 */
	ui = &xylctlrinfo[CTLR(dev)].ci_unit[UNIT(dev)];
	xi = iopb_u(ui, 0);
	if (xylpoll(ui, CMDIOPB(xi), CMD_RPARAMS, CMD_RPARAMS_CONFIG)) {
		printf("xylfmt: read config failed\n");
		u.u_error = EIO;
		return;
	}
	printDriveParams(xi, "PRD");

	/* read format parameters */
	if (xylpoll(ui, CMDIOPB(xi), CMD_RPARAMS, CMD_RPARAMS_FORMAT)) {
		printf("xylfmt: read format failed\n");
		u.u_error = EIO;
		return;
	}
printf("f1=%x f2=%x f3=%x f4=%x f5=%x f6=%x f7=%x\n",
	      FMTIOPB(xi)->xi_field1, FMTIOPB(xi)->xi_field2,
	      FMTIOPB(xi)->xi_field3, FMTIOPB(xi)->xi_field4,
	      FMTIOPB(xi)->xi_sectorSize,
	      FMTIOPB(xi)->xi_field6, FMTIOPB(xi)->xi_field7);
}

/*
 * check for valid volume header
 */
int
vhchecksum(vh)
	register struct volume_header *vh;
{
	register int csum;
	register int *ip;

	csum = 0;
	for (ip = (int *)vh; ip < (int *)(vh + 1); ip++)
		csum += *ip;
	return (csum);
}

XXXdumpiopb(msg, xi)
	char *msg;
	register char *xi;
{
	printf("%s:\n", msg);
	printf("00: %x %x %x %x %x %x %x %x\n",
		    xi[0], xi[1], xi[2], xi[3],
		    xi[4], xi[5], xi[6], xi[7]);
	printf("08: %x %x %x %x %x %x %x %x\n",
		   xi[8], xi[9], xi[10], xi[11],
		   xi[12], xi[13], xi[14], xi[15]);
	printf("10: %x %x %x %x %x %x %x %x\n",
		   xi[16], xi[17], xi[18], xi[19],
		   xi[20], xi[21], xi[22], xi[23]);
	printf("18: %x %x\n", xi[24], xi[25]);
}
#ifndef STANDALONE
extern struct devtable *Devboot;
extern struct devtable Dev_dkxyl[];
int has_dkxyl()
{
	Devboot = Dev_dkxyl;
	return(1);
}
#endif
