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
#ident	"$Header: scsi.c,v 1.39.1.13.1.3.1.3 90/11/16 16:30:59 beacker Exp $"
/*
** Intrepid System V Disk and Tape SCSI driver
**
** TODO:
** + test forward and back space files and blocks.
** + test tape EOM handling
** + implement residual handling for space ioctl's
** + for efficiency check out NOT loading the chain address
**   since the s/g entries are contiguous (chain address reg will be correct)
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
#include "sys/am9516.h"
#include "sys/vmereg.h"
#include "sys/elog.h"
#include "sys/m120scsi.h"
#include "sys/scsi.h"
#include "sys/ioctl.h"
#include "sys/dkio.h"
#include "sys/edt.h"
#include "sys/dump.h"
#include "sys/gen_ioctl.h"
#include "sys/mtio.h"		/* needed for "mt" command ioctl stuff */


#define LOOPS		0x7fffffff

int scsi_min_timesup = 0x7fffffff;

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

/* This is the dma address alignment factor (must be modulo 2) */
#define ALIGN	4

void
scsiinit()
{
    register struct scsi_unit *un;
    register i, target, lun;
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
	    un->un_iotime = &scsiiotime[target];
	    un->un_dp = &scsi_tab[TAR_LUN(target,lun,scsi_Nlun)];
	    un->un_dp->b_dev = TARMAJOR_DEV(target,scsi_majors[lun]);
	    un->un_iopbp = (struct scsi_iopb*)K0_TO_K1(&un->un_iopb);
	    un->un_target = target;
	    un->un_lun = lun;
	    un->un_dmaalign = ALIGN;
	    un->un_dmastartmask = ALIGN-1;
	    un->un_dmaaddmask = ALIGN-1;
	    un->un_dmacntmask = ALIGN-1;
            un->un_maxsg = MAX_SGENTRY - 1;
	    if (lun == 0) { /* lun 0 only, additional luns on 1st open*/
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
** udc chain entry setup
*/
static int
scsisetupdma(un, s_g, r_w, bcount, physaddr, entry)
register struct scsi_unit *un;
int s_g, r_w, bcount, physaddr, entry;
{
    register struct scsi_iopb *ip;
    register struct scsisge *dmaptr;
    register hold_addr, cmd;

    cmd = un->un_command;
    if (!s_g && !(cmd == C0_READ || cmd == C0_WRITE || cmd == C1_READ || 
		  cmd == C1_WRITE || cmd == C1_READDEF))
	return(0);
    ASSERT(bcount%4 == 0); /* bcount MUST be modulo 4 */

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
    int tv, s;

    s = splbio();
    ip = un->un_iopbp;

    if (mode & INTERRUPT) {
	/* tell low-level code how long before a time-out occurs */
	ASSERT(ip->scsi_timeid == 0);
	ip->scsi_time = common_scsi_timeval(un,ip) * HZ;
	un->un_flags |= INT_BUSY;
    }
    spcstart(ip, HIGHLEVEL); /* drop down to low-level routines */
    if (mode & WAIT) {    /* tape only */
	/* wait for command completion */
	while (un->un_flags & INT_BUSY) {
	    un->un_flags |= INT_WAITING;
	    (void) sleep((caddr_t) un, PRIBIO);
	}
    }
    splx(s);
    if (mode & INTERRUPT)
	return 0;
    hwstat = &ip->scsi_hwstatus;
    while (*hwstat == 0xAA && --timesup)
	;
    if (timesup && (timesup < scsi_min_timesup))
	scsi_min_timesup = timesup;
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
    spcinit(flag);	/* initialize fuji mb87030 spc chip */
    udcinit();		/* initialize amd am9516 udc chip */
}
#ifndef STANDALONE
extern struct devtable *Devboot;
extern struct devtable Dev_scsi[];
int has_scsi()
{
	Devboot = Dev_scsi;
	return(1);
}
#endif
