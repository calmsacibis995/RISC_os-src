#ident "$Header: pdkis.c,v 1.8 90/11/07 15:21:35 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * pdkis.c -- Pizazz scsi standalone disk driver
#define DEBUG
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
#include "machine/cpu_board.h"
#include "mips/rambo.h"
#include "mips/r3030scsi.h"
#include "mips/scsi.h"

#define	NLUN		1	/* max luns per target */
#define SCSI_MAJOR      33

struct scsi_unit pdkis_un[(NTARGET+1)*NLUN];
struct scsisge pdkis_sge[(NTARGET+1)*NLUN][MAX_SGENTRY];
struct iobuf pdkis_tab[(NTARGET+1)*NLUN];
struct volume_header pdkis_vh[(NTARGET+1)*NLUN];
int pdkis_Ntarget = NTARGET;
int pdkis_Nlun = NLUN;
static int pdkis_majors[NLUN] = { SCSI_MAJOR };

static void scsilowinit();
static void scsisetupdma();
static int scsistartop();

struct low_scsi pdkis_low_scsi;

static int init_called;

#define ALIGN           64
#define ALIGN_G          4	/* Genesis alignment */

/*
 * _pdkinit -- initialize driver global data
 */
_pdkinit()
{
    register struct scsi_unit *un;
    register int target, lun;
    char *scsi_id;
    char sid;
    int target_id;
    extern char *getenv();

    if (init_called)
	return;
    scsilowinit();

    target_id = 7;
    if (IS_R3030) {
	sid = (scsi_id = getenv("scsi_id")) ? *scsi_id : 0;
        if ((sid >= '0') && (sid < '8')) target_id = sid - '0';
    }

    bzero(pdkis_un, sizeof(pdkis_un));
    bzero(pdkis_vh, sizeof(pdkis_vh));
    bzero(pdkis_sge, sizeof(pdkis_sge));
    bzero(pdkis_tab, sizeof(pdkis_tab));

    pdkis_low_scsi.low_scsi_init = scsilowinit;
    pdkis_low_scsi.low_scsi_setupdma = scsisetupdma;
    pdkis_low_scsi.low_scsi_startop = scsistartop;
    pdkis_low_scsi.low_scsi_un = pdkis_un;
    pdkis_low_scsi.low_scsi_Ntarget = target_id;
    pdkis_low_scsi.low_scsi_Nlun = pdkis_Nlun;
    for (target = 0; target <= NTARGET; ++target) {
	for (lun = 0; lun < pdkis_Nlun; ++lun) {
	    common_scsi_registerme(pdkis_majors[lun], &pdkis_low_scsi);
	    un = &pdkis_un[TAR_LUN(target,lun,pdkis_Nlun)];
	    un->un_extra = (u_int)&pdkis_sge[TAR_LUN(target,lun,pdkis_Nlun)][0];
	    un->un_dp = &pdkis_tab[TAR_LUN(target,lun,pdkis_Nlun)];
	    un->un_dp->b_dev = TARMAJOR_DEV(target,pdkis_majors[lun]);
	    un->un_iopbp = (struct scsi_iopb *)K0_TO_K1(&un->un_iopb);
	    un->un_target = target;
	    un->un_lun = lun;
	    un->un_maxsg = MAX_SGENTRY - 1;
	    if (IS_R3030) {
		    un->un_dmaalign = ALIGN;
		    un->un_dmastartmask = ALIGN - 1;
		    un->un_dmaaddmask = ALIGN - 1;
		    un->un_dmacntmask = ALIGN - 1;
	    } else { /* Genesis */
		    un->un_dmaalign = ALIGN_G;
		    un->un_dmastartmask = ALIGN_G - 1;
		    un->un_dmaaddmask = ALIGN_G - 1;
		    un->un_dmacntmask = ALIGN_G - 1;
	    }
	}
    }
    init_called = 1;
}

/*
 * _ptsinit -- initialize driver global data
 */
_ptsinit()
{
    if (init_called)
	return;
    _pdkinit();
}

/*
 * _pdkopen -- initialize ncr ASC. wait for drive to spin-up
 *  		  and read in volume header.
 */
_pdkopen(io)
register struct iob *io;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(pdkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_open(io,dev,INT_DISK));
}

/*
 * _ptsopen
 */
_ptsopen(io)
register struct iob *io;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	if (io->i_unit == 0)
	    io->i_unit = 6;
	dev = makedev(pdkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_open(io,dev,INT_TAPE));
}

/*
 * _pdkclose
 */
_pdkclose(io)
register struct iob *io;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(pdkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_close(io,dev));
}

/*
 * _ptsclose
 */
_ptsclose(io)
register struct iob *io;
{
    return(_pdkclose(io));
}

/*
 * _pdkstrategy -- perform io
 */
_pdkstrategy(io, func)
register struct iob *io;
register int func;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(pdkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return(common_scsi_strategy(io,dev,func));
}

/*
 * _ptsstrategy -- perform io
 */
_ptsstrategy(io, func)
register struct iob *io;
register int func;
{
    return(_pdkstrategy(io, func));
}

/*
 * _pdkioctl -- io controls
 */
_pdkioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
	register int dev;

	if (io->i_ctlr >= NLUN)
	    return(-1);
	dev = makedev(pdkis_majors[io->i_ctlr],(io->i_unit<<4)|io->i_part);
	return (common_scsi_ioctl(io,dev,cmd,arg));
}

/*
 * _ptsioctl -- io controls
 */
_ptsioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
    return(_pdkioctl(io, cmd, arg));
}

/*
** RAMBO and Genesis dma chain entry setup
*/
static void
scsisetupdma(un, s_g, r_w, bcount, physaddr, entry)
register struct scsi_unit *un;
int s_g, r_w, bcount, physaddr, entry;
{
    register struct scsi_iopb *ip;
    register struct scsisge *sgeptr;
    register struct scsisge_g *sgeptr_g;
    register mode = 0;
    u_int count;

    if (!s_g && !(un->un_command == C0_READ || un->un_command == C0_WRITE ||
                  un->un_command == C1_READ || un->un_command == C1_WRITE ||
                  un->un_command == C1_READDEF))
	return;
    if (IS_R3030)
	    ASSERT(bcount%64 == 0); /* bcount MUST be modulo 64 for pizazz */
    else /* Genesis */
	    ASSERT(bcount%4 == 0);  /* bcount MUST be modulo 4 for genesis */
    ip = un->un_iopbp;
    ip->scsi_flags |= DMA_XFER;

    /* dma chain ptr */
    if (IS_R3030) {
	    sgeptr = (struct scsisge *)un->un_extra + entry;

	    sgeptr->mem_ptr = K0_TO_PHYS(physaddr);
	    if (r_w) /* read from a scsi device */
		sgeptr->chanmode = TO_MEMORY|INTR_EN|CHANNEL_EN;
	    else
		sgeptr->chanmode = INTR_EN|CHANNEL_EN;
	    sgeptr->count = bcount >> BLOCK_SHIFT;
	    sgeptr++;
	    sgeptr->count = NULL; /* set chain termination condition in case */
    } else { /* Genesis */
	    if (entry) { /* enable chaining on previous entry first */
		sgeptr_g = (struct scsisge_g *)un->un_extra + (entry-1);
		sgeptr_g->control_count &= ~NO_CHAIN_ENABLE; /* active low */
		sgeptr_g++; /* now point to our current element */
	    } else 
		sgeptr_g = (struct scsisge_g *)un->un_extra;
	    sgeptr_g->mem_ptr = K0_TO_PHYS(physaddr);
	    count = bcount << DMA_BYTE_SHIFT;/* convert and position byte cnt */
	    count = ~count & 0xffff0000;
	    if (!r_w) /* write to a scsi device */
		count |= TO_MEMORYB; /* set for vme mem reads (scsi write) */
	    /* disable auto chaining till we know we need to use it */
	    count |= (NO_CHAIN_ENABLE|NO_DMA_RESET|NO_FLUSH_PIPE|NO_CLR_DMA);
	    sgeptr_g->control_count = count;
	    sgeptr_g++; /* point to next element */
	    sgeptr_g->control_count = NULL; /* clear out next elements count */
    }
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

    ip = un->un_iopbp;

    ip->scsi_time = common_scsi_timeval(un,ip);
    scsicmdp(ip); /* drop down to low-level routines */
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
    _scsi_initp();	/* initialize ncr 53c94 asc chip and relevent dma */
}
