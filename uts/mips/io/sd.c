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
#ident	"$Header: sd.c,v 1.14.1.5.1.2.1.4 90/11/16 16:31:30 beacker Exp $"
/*
 * $Header: sd.c,v 1.14.1.5.1.2.1.4 90/11/16 16:31:30 beacker Exp $
 */
/*
 * IOP-SCSI disk driver
 */

#ifndef STANDALONE
#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "sys/dvh.h"
#include "sys/immu.h"
#include "sys/m12scsi.h"
#include "sys/scsi.h"
#include "sys/iop.h"
#include "sys/shareg.h"
#include "sys/sha_errs.h"
#include "sys/elog.h"
#include "sys/sysmacros.h"
#include "sys/cmn_err.h"
#else STANDALONE
#include "sysv/types.h"
#include "sysv/param.h"
#include "sys/dir.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "machine/dvh.h"
#include "sys/immu.h"
#include "machine/cpu.h"
#include "machine/m12scsi.h"
#include "machine/scsi.h"
#include "machine/iop.h"
#include "machine/shareg.h"
#include "sys/elog.h"
#include "sys/errno.h"
#include "sys/sysmacros.h"
#include "sys/cmn_err.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#endif

#ifdef STANDALONE
/*
 * DELAY(n) should be n microseconds, roughly.  This is a first guess.
 */
#define	DELAY(n)	{ register int N = 3*(n); while (--N > 0); }
#endif

extern int	sha_disc;
extern int	isd_syncmode;
#ifndef STANDALONE
extern int	showconfig;
#endif STANDALONE

static int	isdstartop();
static int	isdissue ();
static int	isdtimeout();
static int	isdsetupdma();
static void	isdinit();

int isd_newproms = 1;		/* say new proms for now */

void	isdintr();

/* this structure communicates variables and functions to the common scsi
 *	driver.
 */
struct low_scsi isd_low_scsi = {
	isdinit,
	isdsetupdma,
	isdstartop,
	isd_un
};

#ifdef STANDALONE

#define NLUN	1
int isd_Ntarget = NTARGET;
int isd_Nlun = NLUN;
struct scsi_unit isd_un[NTARGET*NLUN];
struct iobuf isd_tab[NTARGET*NLUN];
int isd_majors[NLUN] = { 0 };
int sha_disc = 0;
int showconfig = 0;
int scsiexterr = 0;

/* large buffer, allocated flag and conversion defines for a fast tape */
extern char *common_scsi_tapebuf;

_istopen(io)
register struct iob *io;
{
    return(common_scsi_open(io));
}

_isdopen(io)
register struct iob *io;
{
    return(common_scsi_open(io));
}

_istclose(io)
register struct iob *io;
{
    return(common_scsi_close(io));
}

_isdclose(io)
register struct iob *io;
{
    return(common_scsi_close(io));
}

_istioctl(io, cmd, arg)
register struct iob *io;
register u_int cmd;
register caddr_t arg;
{
    return(common_scsi_ioctl(io, cmd, arg));
}

_isdioctl(io, cmd, arg)
register struct iob *io;
register u_int cmd;
register caddr_t arg;
{
    return(common_scsi_ioctl(io, cmd, arg));
}

_iststrategy(io, func)
register struct iob *io;
register int func;
{
    return (common_scsi_strategy(io,func));
}

_isdstrategy(io, func)
register struct iob *io;
register int func;
{
    return (common_scsi_strategy(io,func));
}

_istinit()
{
    return(0);
}
#else
/*
** device open routine
*/
void
isdopen(dev,flag,type)
dev_t dev;
int flag;
{
    common_scsi_open(dev,flag,type);
}

/*
** device close routine
*/
void
isdclose(dev, flag,type)
dev_t dev;
register int flag;
{
    common_scsi_close(dev,flag,type);
}

void
isdread(dev)
register dev_t dev;
{
    common_scsi_read(dev);
}

void
isdwrite(dev)
register dev_t dev;
{
    common_scsi_write(dev);
}

void
isdioctl(dev, cmd, arg, flag)
register dev_t	dev;
register u_int cmd;
register caddr_t arg;
register int flag;
{
    common_scsi_ioctl(dev,cmd,arg,flag);
}

void
isdstrategy(bp)
register struct buf	*bp;
{
    common_scsi_strategy(bp);
}

int
isddump(dev, flag, bn, physaddr, count)
dev_t	dev;
{
    return(common_scsi_dump(dev,flag,bn,physaddr,count));
}

int
isdsize(dev)
dev_t	dev;
{
    return(common_scsi_size(dev));
}
#endif

#ifdef STANDALONE
_isdinit()
{
    register struct shadevice *sha;
    register struct iobuf *dp;
    register struct scsi_unit *un;
    register int target, lun;
    struct shadevice *iop_alloc();
    
    bzero(&isd_un[0], sizeof(isd_un));
    bzero(&isd_tab[0], sizeof(isd_tab));
    common_scsi_tapebuf = 0;
#else 
void
isdedtinit(edt)
register struct edt *edt;
{
    register struct shadevice *sha;
    register struct iobuf *dp;
    register struct scsi_unit *un;
    register int i, target, lun;
    struct shadevice *iop_alloc();
    static int already = 0;
    extern SCSI_BUF_EXT *scsi_buf_ext_hdr, scsi_buf_ext[];

    if (already)
	return;
    already++;
#endif

    if (scsi_buf_ext_hdr == 0) {
	for (i = 0; i < (SCSI_MAX_BUF_EXT - 1); i++)
	    scsi_buf_ext[i].nxt = &scsi_buf_ext[i+1];
	scsi_buf_ext[i].nxt = (SCSI_BUF_EXT *)0;
	scsi_buf_ext_hdr = &scsi_buf_ext[0];
    }
    if ((sha = iop_alloc(PSCSIIOCB,sizeof(*sha))) == NULL)
	return;
    sha->sha_cmd = SHA_INIT;
    sha->sha_dlen = MAXPHYS + NBPC;
    wbflush();
    iop_poke(PSCSIIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(sha), 0);
    if (isdissue(0,0,sha))
	return;
    sha->sha_cmd = SHA_VERSION;
    sha->sha_timeout = 0;
    wbflush();
    iop_poke(PSCSIIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(sha), 0);
    if (isdissue(0,0,sha) || sha->sha_timeout < 1) {
        isd_newproms = 0;
    }
    if (showconfig)
        cmn_err(CE_CONT,"V50 PROM version number %d\n",
                sha->sha_timeout);
    isd_low_scsi.low_scsi_Ntarget = isd_Ntarget;
    isd_low_scsi.low_scsi_Nlun = isd_Nlun;

    for (target=0; target < isd_Ntarget; target++) {
	for (lun=0; lun < isd_Nlun; lun++) {
	    common_scsi_registerme(isd_majors[lun], &isd_low_scsi);
	    sha = iop_alloc(PSCSIIOCB + target,sizeof(*sha));
	    dp = &isd_tab[TAR_LUN(target,lun,isd_Nlun)];
	    dp->io_addr = (paddr_t)sha;
	    dp->b_dev = TARMAJOR_DEV(target,isd_majors[lun]);
	    if (sha == NULL)
		continue;
	    un = &isd_un[TAR_LUN(target,lun,isd_Nlun)];
	    un->un_dp = dp;
#ifndef STANDALONE
	    un->un_iotime = &isd_iotime[target];
#endif
	    un->un_iopbp = (struct scsi_iopb*)K0_TO_K1(&un->un_iopb);
	    un->un_iopbp->scsi_syncxfer = 1;
	    un->un_target = target;
	    un->un_lun = lun;
	    un->un_dmaalign = 0x02;
	    un->un_dmastartmask = 0x01;
	    un->un_dmaaddmask = 0x00;
	    un->un_dmacntmask = 0x0;
	    un->un_maxsg = MAX_SGENTRY - 1;
#ifdef STANDALONE
	    un->un_tpxfer = 0;
	    un->un_tpnxtrec = -1;
#else
            if (target == 7)            /* cannot do initiator id */
                continue;
	    if (lun == 0) { /* lun 0 only, additional luns on 1st open*/
		if (common_scsi_slave(un, POLLED))
		    common_scsi_attach(un, POLLED);
	    }
#endif
	}
    }
}

/*
** fire off command
*/
static int
isdstartop(un, mode)
register struct scsi_unit *un;
register int mode;
{
    register struct scsi_iopb *ip;
    register struct shadevice *sha;
    register int timesup = POLL_DELAY;
    register count, target, lun;
    int status, s;

#ifndef STANDALONE
    s = splbio();
#endif
    ip = un->un_iopbp;
    target = un->un_target;
    lun = un->un_lun;
    sha = (struct shadevice *)isd_tab[TAR_LUN(target,lun,isd_Nlun)].io_addr;
    sha->sha_cmd = SHA_SCSI;
    sha->sha_cdb = ip->cmd_blk;
    sha->sha_cerr = sha->sha_scsi_status = 0;
    count = ip->scsi_count + ip->scsi_count0 + ip->scsi_count1;
    sha->sha_dlen = (count & 1) ? count + 1 : count;
    if (!sha_disc)
	    sha->sha_dcr |= SHA_NODISC;
    ip->scsi_time = common_scsi_timeval(un,ip); /* seconds before timeout */
    if (isd_newproms) {
        /* tell low-level code how long before a time-out occurs */
        sha->sha_timeout = ip->scsi_time;
    } else {
	switch (ip->cmd_blk.cdb_0.cdb_0_cmd & 0xe0) {
	    case 0x00:				/* group 0 commands */
		sha->sha_timeout = 0x600;
		break;
	    case 0x20:				/* group 1 commands */
		sha->sha_timeout = 0xa00;
		break;
	    case 0xe0:				/* group 7 commands */
		sha->sha_timeout = 0x200;
		break;
	    default:
		cmn_err(CE_PANIC,"Invalid group code: command = 0x%x\n",
		    ip->cmd_blk.cdb_0.cdb_0_cmd);
	}
    }

#ifndef STANDALONE
    if (mode & INTERRUPT) {
        un->un_flags |= INT_BUSY;
        sha->sha_dcr |= SHA_IE;
    } else {
        sha->sha_dcr |= SHA_NOIE;
    }
#else
    sha->sha_dcr |= SHA_NOIE;
#endif
    wbflush();
    iop_poke(PSCSIIOCB + target, IOPB_SPIN, K1_TO_IOPB_OFFSET(sha), 0);

#ifndef STANDALONE
    if (!(mode & POLLED)) {
        if (isd_newproms)
	    ip->scsi_timeid =
		    timeout (isdtimeout, target, (ip->scsi_time + 2) * HZ);
        else
	    ip->scsi_timeid =
		    timeout (isdtimeout, target, ip->scsi_time * HZ);
    }
    if (mode & WAIT) {    /* tape only */
	/* wait for command completion */
	while (un->un_flags & INT_BUSY) {
	    un->un_flags |= INT_WAITING;
	    (void) sleep((caddr_t) un, PRIBIO);
	}
    }
    splx(s);
    if (mode & INTERRUPT) {
	return(0);
    }
#endif
    return(isdissue(target, lun, sha));
}

static int
isdissue(target, lun, shaaddr)
    register int target;
    register int lun;
    register struct shadevice *shaaddr;
{
    long l = 8000000, status;

    while (l--) {
	if (iop_wait(PSCSIIOCB + target, IOPB_NOWAIT, &status, 0) == 0) {
	    iop_clear(PSCSIIOCB + target);
	    return(convshaerr(shaaddr));
	}
	DELAY(400);
    } 
    cmn_err(CE_CONT,"\nSCSI %dL%d: POLLED timeout\n",
	    target, lun);
    return(SELTMO);
}

#ifndef STANDALONE
static int
isdtimeout(target)
register int target;
{
    register struct shadevice *sha;
    int status;
    
    if (iop_wait (PSCSIIOCB + target, IOPB_NOWAIT, &status, 0) == 0) {
	cmn_err (CE_NOTE, "SCSI %d: cmd timed out/semaphore set\n", target);
	isdintr (target, 0, 0);
    } else {
        if (isd_newproms) {
	    cmn_err(CE_NOTE,"SCSI %d: cmd timed out - resetting SCSI bus\n",
		target);
            sha = (struct shadevice *)isd_tab[TAR_LUN(7,0,isd_Nlun)].io_addr;
            sha->sha_cmd = SHA_RESET;
            wbflush();
	    iop_poke(PSCSIIOCB + 7, IOPB_SPIN, K1_TO_IOPB_OFFSET(sha), 0);
            if (isdissue(7,0,sha))
                cmn_err(CE_PANIC,"SCSI %d: SHA_RESET cmd timed out\n", target);
        } else {
            cmn_err(CE_PANIC,"SCSI %d: cmd timed out\n", target);
        }
    }
}

void
isdintr (target, dummy, iocb)
    register int target;
    unsigned dummy;
    struct iocb *iocb;
{
    register struct scsi_unit *un;
    register struct scsi_iopb *ip;
    register struct shadevice *sha;

    un = &isd_un[TAR_LUN(target,0,isd_Nlun)];
    ip = un->un_iopbp;
    if (ip->scsi_timeid) {
	untimeout (ip->scsi_timeid);
	ip->scsi_timeid = 0;
    } else
	cmn_err(CE_CONT,"SCSI %d: no timeout set\n",target);
    sha = (struct shadevice *)isd_tab[TAR_LUN(target,0,isd_Nlun)].io_addr;
    if (sha->sha_cerr == 0 &&  sha->sha_scsi_status == 0) {
	ip->scsi_taskid = 0;
    } else {
	ip->scsi_taskid = ERROR;
	ip->scsi_status = convshaerr(sha);
    }
    iop_clear(PSCSIIOCB + target);
    common_scsi_intr(un);
}
#endif

int
convshaerr(shaaddr)
register struct shadevice *shaaddr;
{
    int status;

    switch (shaaddr->sha_cerr) {
	case SHA_CERR_PARITY:
	    status = PARITY;
	    break;
	case SHA_CERR_TMOERR:
	    status = SCSITMO;
	    break;
	case SHA_CERR_PCERR:
	    status = PHASE_ERROR;
	    break;
	case SHA_CERR_SELERR:
	    status = SELTMO;
	    break;
	case SHA_CERR_RECERR:
	    status = INVTARGET;
	    break;
	case SHA_CERR_OK:
	    status = shaaddr->sha_scsi_status;
	    break;
	default:
	    status = HWERROR;
    }
    return(status);
}

static int
isdsetupdma(un, s_g, r_w, bcount, physaddr, entry)
register struct scsi_unit *un;
int s_g, r_w, bcount, physaddr, entry;
{
    register u_long *ptr;
    register target, lun;
    register struct shadevice *sha;

    target = un->un_target;
    lun = un->un_lun;
    sha = (struct shadevice *)isd_tab[TAR_LUN(target,lun,isd_Nlun)].io_addr;
    ptr = &sha->sha_pfn[entry];
    if (entry == 0) {
	if (r_w == DMA_READ)
	    sha->sha_dcr = SHA_DIR_IN;
	else
	    sha->sha_dcr = SHA_DIR_OUT;
	sha->sha_pgofset = (u_short)physaddr & POFFMASK;
	if (isd_syncmode)
	    sha->sha_dcr |= SHA_SYNC_MODE;
    }
    *ptr = (u_long)physaddr >> BPCSHIFT;
    return(1);
}

static void
isdinit()
{
}

#ifdef STANDALONE
void
sha_dbg(arg)
register int arg;
{
    register struct shadevice *sha;
    struct shadevice *iop_alloc();
    
    if ((sha = iop_alloc(PSCSIIOCB, sizeof(*sha))) == NULL) {
	return;
    }
    sha->sha_cmd = arg ? SHA_DBGON : SHA_DBGOFF;
    sha->sha_timeout = arg;
    wbflush();
    iop_poke(PSCSIIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(sha), 0);
    isdissue(0 ,0, sha);
}
#endif
#ifndef STANDALONE
extern struct devtable *Devboot;
extern struct devtable Dev_dkisd[];
int has_dkisd()
{
	Devboot = Dev_dkisd;
	return(1);
}
#endif
