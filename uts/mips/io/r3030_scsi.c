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
#ident	"$Header: r3030_scsi.c,v 1.1.1.11.1.3.1.3 90/11/16 16:31:55 beacker Exp $"
/*
** Pizazz System V Disk and Tape SCSI driver
**
*/ 


#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/buf.h"
#define		B_SPL B_FORMAT
#include "sys/file.h"
#include "sys/iobuf.h"
#define		b_cylin	b_resid
#define		b_cmd	b_resid
#include "sys/vmevar.h"
#include "sys/dvh.h"
#include "sys/rambo.h"	    /* dma defines */
#include "sys/vmereg.h"
#include "sys/elog.h"
#include "sys/r3030scsi.h"  /* pizazz specific scsi defines */
#include "sys/scsi.h"
#include "sys/ioctl.h"
#include "sys/dkio.h"
#include "sys/edt.h"
#include "sys/dump.h"
#include "sys/gen_ioctl.h"
#include "sys/mtio.h"		/* needed for "mt" command ioctl stuff */

/* this value must be large (or a method to use the DELAY macro implemented)
 * cause polled commands can be queed up amongst interrupt driven commands */
#define LOOPS		0x7fffffff

int r3030_min_timesup = 0x7fffffff;

/*
 * routines which must be supplied for common_scsi_.c
 */
static int scsisetupdma();
static int scsistartop();
static void scsilowinit();

/* this structure communicates variables and functions to the common scsi
 *	driver.
 */
struct low_scsi scsi_low_scsi = {
	scsilowinit,
	scsisetupdma,
	scsistartop,
	scsi_un,
	0,
	0
};
int stat_cross = 0;
int stat_miss = 0;
int stat_hit = 0;

set_cpu_con()
{
}

void
scsiinit()
{
    register struct scsi_unit *un;
    register i, target, lun;
    register unsigned temp;
    extern SCSI_BUF_EXT *scsi_buf_ext_hdr, scsi_buf_ext[];

    if (scsi_buf_ext_hdr == 0) {
	for (i = 0; i < (SCSI_MAX_BUF_EXT - 1); i++)
	    scsi_buf_ext[i].nxt = &scsi_buf_ext[i+1];
	scsi_buf_ext[i].nxt = (SCSI_BUF_EXT *)0;
	scsi_buf_ext_hdr = &scsi_buf_ext[0];
    }

    scsilowinit(0);
    binit();
    /*
     * establish pointers (don't cache iopb access'!!)
     */
    scsi_low_scsi.low_scsi_Ntarget = scsi_Ntarget;
    scsi_low_scsi.low_scsi_Nlun = scsi_Nlun;
    for (target=0; target < scsi_Ntarget; target++) {
	for (lun=0; lun < scsi_Nlun; lun++) {
	    common_scsi_registerme(scsi_majors[lun], &scsi_low_scsi);
	    un = &scsi_un[TAR_LUN(target,lun,scsi_Nlun)];
	    un->un_extra = (u_int)&scsi_sge[TAR_LUN(target,lun,scsi_Nlun)][0];
	    un->un_iotime = &isd_iotime[target];
	    un->un_dp = &scsi_tab[TAR_LUN(target,lun,scsi_Nlun)];
	    un->un_dp->b_dev = TARMAJOR_DEV(target,scsi_majors[lun]);
	    un->un_iopbp = (struct scsi_iopb*)K0_TO_K1(&un->un_iopb);
	    un->un_target = target;
	    un->un_lun = lun;
	    /* minimum dma granularity for RAMBO is 64 bytes 16-word alligned */
	    un->un_dmaalign = 0x40;
	    un->un_dmastartmask = 0x3f;
	    un->un_dmaaddmask   = 0x3f;
	    un->un_dmacntmask   = 0x3f;
            un->un_maxsg = MAX_SGENTRY - 1;
	    if (lun == 0) { /* lun 0 only, additional luns on 1st open */
		if (common_scsi_slave(un,POLLED))
		    common_scsi_attach(un,POLLED);
	    }
	}
    }
}

void
scsiopen(dev,flag,type)
dev_t dev;
int flag;
{
    common_scsi_open(dev,flag,type);
}

void
scsiclose(dev,flag,type)
dev_t dev;
int flag;
{
    common_scsi_close(dev,flag,type);
}

void
scsistrategy(bp)
register struct buf *bp;
{
    common_scsi_strategy(bp);
}

int
scsidump(dev,flag,bn,physaddr,count)
dev_t dev;
int flag;
daddr_t bn;
caddr_t physaddr;
int count;
{
    return(common_scsi_dump(dev,flag,bn,physaddr,count));
}

int
scsisize(dev)
dev_t dev;
{
    return(common_scsi_size(dev));
}

void 
scsiread(dev)
dev_t dev;
{
    common_scsi_read(dev);
}

void
scsiwrite(dev)
dev_t dev;
{
    common_scsi_write(dev);
}

void
scsiioctl(dev,cmd,arg)
register dev_t dev;
register unsigned int cmd;
register caddr_t arg;
{
    common_scsi_ioctl(dev,cmd,arg);
}

/*
** RAMBO dma chain entry setup
*/
static int
scsisetupdma(un, s_g, r_w, bcount, physaddr, entry)
register struct scsi_unit *un;
int s_g, r_w, bcount, physaddr, entry;
{
    register struct scsi_iopb *ip;
    register struct scsisge *sgeptr, *sgeptr_prev;
    u_int count, cross, mem_ptr;
    register cmd;

    cmd = un->un_command;
    if (!s_g && !(cmd == C0_READ || cmd == C0_WRITE || cmd == C1_READ || 
		  cmd == C1_WRITE || cmd == C1_READDEF))
	return(0);
    ASSERT(bcount%64 == 0); /* bcount MUST be modulo 64 for pizazz */
    ip = un->un_iopbp;
    ip->scsi_flags |= DMA_XFER;

    /* dma chain ptr */
    sgeptr = (struct scsisge *)un->un_extra + entry;
    if (entry) { /* we have to have already done at least one first! */
    	sgeptr_prev = sgeptr - 1;
	count  = sgeptr_prev->count << BLOCK_SHIFT;
	cross = sgeptr_prev->mem_ptr & 0x80000;
	mem_ptr = sgeptr_prev->mem_ptr + count;
	/* make sure we don't cross a 1/2 Meg Physical Memory boundary
	 * and don't usr a new element if this segment is contiguous!
	 * (the boundary condition check is for a RAMBO bug)
	 */
	if ((mem_ptr & 0x80000) != cross)
		stat_cross++;
	else if (mem_ptr != physaddr)
		stat_miss++;
	else {
		stat_hit++;
		count += bcount;
    		sgeptr_prev->count = count >> BLOCK_SHIFT;
		return(0);
	}
    }
    sgeptr->mem_ptr = physaddr;
    if (r_w) /* read from a scsi device */
    	sgeptr->chanmode = TO_MEMORY|INTR_EN|CHANNEL_EN;
    else
    	sgeptr->chanmode = INTR_EN|CHANNEL_EN;
    sgeptr->count = bcount >> BLOCK_SHIFT;
    sgeptr++;
    sgeptr->count = NULL; /* set chain termination condition in case it is */
    return(1);
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
    register int timesup = LOOPS;
    register volatile char *hwstat;
    int tv;

    ip = un->un_iopbp;

    if (mode & INTERRUPT) {
	/* tell low-level code how long before a time-out occurs */
	ASSERT(ip->scsi_timeid == 0);
	ip->scsi_time = common_scsi_timeval(un,ip) * HZ;
	un->un_flags |= INT_BUSY;
    }
    ascstart(ip, HIGHLEVEL); /* drop down to low-level routines */
    if (mode & WAIT) {    /* tape only */
	/* wait for command completion */
	while (un->un_flags & INT_BUSY) {
	    un->un_flags |= INT_WAITING;
	    (void) sleep((caddr_t) un, PRIBIO);
	}
    }
    if (mode & INTERRUPT)
	return 0;
    hwstat = &ip->scsi_hwstatus;
    while (*hwstat == 0xAA && --timesup)
	;
    if (timesup && (timesup < r3030_min_timesup))
	r3030_min_timesup = timesup;
    if (ip->scsi_hwstatus) {
	if (scsiexterr || ip->scsi_hwstatus != SELTMO)
	    cmn_err(CE_CONT,"\nSCSI %dL%d: hw status; %s\n",
		un->un_target,un->un_lun,scsiprinterr(ip->scsi_hwstatus));
	return(ip->scsi_hwstatus);
    }
    if (ip->scsi_status) {
	if (scsiexterr ||
	    !(ip->scsi_status == SCSI_CHECK || ip->scsi_status == SCSI_BUSY))
	    cmn_err(CE_CONT,"\nSCSI %dL%d: status; %s\n",
		un->un_target, un->un_lun,scsiprinterr(ip->scsi_status));
	return(ip->scsi_status);
    }
    return 0;
}


static void
scsilowinit(flag)
int flag;
{
    ascinit(flag);	/* initialize ncr 53c94 asc chip and rambo channel 1 */
}
#ifndef STANDALONE
extern struct devtable *Devboot;
extern struct devtable Dev_dkisd[];
int has_dkisd()
{
	Devboot = Dev_dkisd;
	return(1);
}
#endif
