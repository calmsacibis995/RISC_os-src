#ident "$Header: dkis.c,v 1.18 90/05/30 08:56:44 hal Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * dkis.c -- Intrepid scsi standalone disk/tape driver
 */

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/cmn_err.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "machine/cpu.h"
#include "machine/dvh.h"
#include "saio/saio.h"
#define 	i_lun i_ctlr
#include "saio/saioctl.h"

#define NOREGS
#include "mips/am9516.h"
#include "mips/m120scsi.h"
#include "mips/scsi.h"

#define	NLUN		1	/* max luns per target */
#define SCSI_MAJOR	16

struct scsi_unit dkis_un[NTARGET*NLUN];
struct scsisge dkis_sge[NTARGET*NLUN][MAX_SGENTRY];
struct iobuf dkis_tab[NTARGET*NLUN];
struct volume_header dkis_vh[NTARGET*NLUN];
int dkis_Ntarget = NTARGET;
int dkis_Nlun = NLUN;
static int dkis_majors[NLUN] = { SCSI_MAJOR };

static void scsilowinit();
static void scsisetupdma();
static int scsistartop();

struct low_scsi dkis_low_scsi;

static int init_called;

#define ALIGN		4

/*
 * _dkinit -- initialize driver global data
 */
_dkinit()
{
    register struct scsi_unit *un;
    register int target, lun;

    if (init_called)
	return;
    scsilowinit();

    bzero(dkis_un, sizeof(dkis_un));
    bzero(dkis_vh, sizeof(dkis_vh));
    bzero(dkis_sge, sizeof(dkis_sge));
    bzero(dkis_tab, sizeof(dkis_tab));

    dkis_low_scsi.low_scsi_init = scsilowinit;
    dkis_low_scsi.low_scsi_setupdma = scsisetupdma;
    dkis_low_scsi.low_scsi_startop = scsistartop;
    dkis_low_scsi.low_scsi_un = dkis_un;
    dkis_low_scsi.low_scsi_Ntarget = dkis_Ntarget;
    dkis_low_scsi.low_scsi_Nlun = dkis_Nlun;
    for (target = 0; target < dkis_Ntarget; ++target) {
	for (lun = 0; lun < dkis_Nlun; ++lun) {
	    common_scsi_registerme(dkis_majors[lun], &dkis_low_scsi);
	    un = &dkis_un[TAR_LUN(target,lun,dkis_Nlun)];
	    un->un_extra = (u_int)&dkis_sge[TAR_LUN(target,lun,dkis_Nlun)][0];
	    un->un_dp = &dkis_tab[TAR_LUN(target,lun,dkis_Nlun)];
	    un->un_dp->b_dev = TARMAJOR_DEV(target,dkis_majors[lun]);
	    un->un_iopbp = (struct scsi_iopb *)K0_TO_K1(&un->un_iopb);
	    un->un_target = target;
	    un->un_lun = lun;
	    un->un_dmaalign = ALIGN;
	    un->un_dmastartmask = ALIGN - 1;
	    un->un_dmaaddmask = ALIGN - 1;
	    un->un_dmacntmask = ALIGN - 1;
	    un->un_maxsg = MAX_SGENTRY - 1;
	}
    }
    init_called = 1;
}

_tsinit()
{
    if (init_called)
	return;
    _dkinit();
}

/*
 * _dkopen -- initialize SPC, UDC. wait for drive to spin-up
 *  		  and read in volume header.
 */
_dkopen(io)
register struct iob *io;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(dkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_open(io,dev,INT_DISK));
}

/*
 * _tsopen
 */
_tsopen(io)
register struct iob *io;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	if (io->i_unit == 0)
	    io->i_unit = 6;		/* default Unit ID is 6 */
	dev = makedev(dkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_open(io,dev,INT_TAPE));
}

/*
 * _dkclose
 */
_dkclose(io)
register struct iob *io;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(dkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_close(io,dev));
}

/*
 * _tsclose
 */
_tsclose(io)
register struct iob *io;
{
    return(_dkclose(io));
}

/*
 * _dkstrategy -- perform io
 */
_dkstrategy(io, func)
register struct iob *io;
register int func;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(dkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_strategy(io,dev,func));
}

/*
 * _tsstrategy -- perform io
 */
_tsstrategy(io, func)
register struct iob *io;
register int func;
{
    return(_dkstrategy(io, func));
}

/*
 * _dkioctl -- io controls
 */
_dkioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(dkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return (common_scsi_ioctl(io,dev,cmd,arg));
}

/*
 * _tsioctl -- io controls
 */
_tsioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
    return(_dkioctl(io, cmd, arg));
}

static void 
scsisetupdma(un,s_g,r_w,bcount,physaddr,entry)
register struct scsi_unit *un;
int s_g, r_w, bcount, physaddr, entry;
{
    register struct scsi_iopb *ip;
    register struct scsisge *dmaptr;
    register hold_addr;

    if (!s_g && !(un->un_command == C0_READ || un->un_command == C0_WRITE ||
                  un->un_command == C1_READ || un->un_command == C1_WRITE ||
                  un->un_command == C1_READDEF))
	return;
    ASSERT((bcount%un->un_dmaalign) == 0); /* bcount MUST be modulo dmaalign */

    ip = un->un_iopbp;
    ip->scsi_flags |= DMA_XFER;

    /* dma chain ptr */
    dmaptr = (struct scsisge *)un->un_extra + entry;

    hold_addr = K0_TO_PHYS(physaddr);
    /*
     * turn our memory pointer into something usable by the UDC 
     */
    physaddr = UDC_ADDR(K0_TO_PHYS(physaddr));
    physaddr |= INC_ADDR; /* no soft wait states */
    if (r_w) { /* read from a scsi device */
	dmaptr->reload_word = LD_SCSI_R;
	dmaptr->chanmode[0] = HS(CHMODE_R);	
	dmaptr->chanmode[1] = LS(CHMODE_R);	
    } else { /* write to a scsi device */
	dmaptr->reload_word = LD_SCSI_W;
	dmaptr->chanmode[0] = HS(CHMODE_W);	
	dmaptr->chanmode[1] = LS(CHMODE_W);	
    }
#ifdef SABLE
    dmaptr->chanmode[0] |= HWMASK|SOFTREQ;	/* simile requires this */
#endif SABLE
    /* enable chaining on terminal count 0 (regardless) */
    dmaptr->chanmode[1] |= CE_TC;	
    if (hold_addr & ADDR_24) physaddr |= UDC_A24;
    if (!(hold_addr & ADDR_25)) physaddr |= UDC_A25;
    dmaptr->mem_ptr[0] = HS(physaddr);
    dmaptr->mem_ptr[1] = LS(physaddr);
    dmaptr->count = bcount/sizeof(short);
    physaddr = K0_TO_PHYS(dmaptr + 1);
    dmaptr->next_blk[0] = (HB(physaddr) << 8) | ONEWAIT;
    dmaptr->next_blk[1] = LS(physaddr);
    /* turn off dreq input so that fuji transfer command
     * can be issued before the start chain dma command,
     * all of this in the name of performance */
    ++dmaptr;
    dmaptr->reload_word = LD_CHAN_MODE; /* only load ch mode */
    dmaptr->mem_ptr[0] = HWMASK_R;	/* don't listen to DReq input */
    dmaptr->mem_ptr[1] = 0;
}

/*
** fire off command
*/
static int
scsistartop(un, mode)
register struct scsi_unit *un;
register int mode;
{
    register struct scsi_iopb *ip;
    int tv, s;

    ip = un->un_iopbp;

    ip->scsi_time = common_scsi_timeval(un,ip);
    scsicmd(ip); /* drop down to low-level routines */
    if (ip->scsi_hwstatus) {
	if (ip->scsi_hwstatus != SELTMO)
	    cmn_err(CE_CONT,"\nSCSI %dL%d: hw status; %s\n",
		un->un_target,un->un_lun,scsiprinterr(ip->scsi_hwstatus));
	return(ip->scsi_hwstatus);
    }
    if (ip->scsi_status) {
	if (ip->scsi_status != SCSI_CHECK)
	    cmn_err(CE_CONT,"\nSCSI %dL%d: status; %s\n",
		un->un_target, un->un_lun,scsiprinterr(ip->scsi_status));
	return(ip->scsi_status);
    }
    return 0;
}


static void
scsilowinit()
{
    _scsi_init();	/* initialize fuji mb87030 spc chip */
}
