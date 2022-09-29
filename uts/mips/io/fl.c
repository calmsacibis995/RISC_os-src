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
#ident	"$Header: fl.c,v 1.6.1.2 90/05/10 05:18:06 wje Exp $"
/*
 * Integrated Solutions IOP-PSEUDO-SCSI floppy disk driver
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
#include "sys/iobuf.h"
#include "sys/ioctl.h"
#include "sys/elog.h"
#include "sys/flio.h"
#include "sys/edt.h"
#include "sys/dvh.h"
#include "sys/dump.h"
#include "sys/bsd_glue.h"
#include "sys/iop.h"
#include "sys/iopvar.h"
#include "sys/m12scsi.h"
#include "sys/scsi.h"
#include "sys/shareg.h"

#define	SET_SHA(sha, ie, dir, disc, timeval, dadr, dlen) {\
		(sha)->sha_cerr = 0;					\
		(sha)->sha_scsi_status = 0;				\
		(sha)->sha_dcr = (ie)|(dir)|(disc);			\
		(sha)->sha_timeout = timeval;				\
		(sha)->sha_pgofset = ((long)dadr & PGOFSET);		\
		(sha)->sha_dlen = dlen;					\
		wbflush();						}

#define SCSI_HMML(x) ((x/**/1<<24)|(x/**/2<<16)|(x/**/3<<8)|(x/**/4))
#define NSCLUN		8

#define	SCSI_HL_SET(x, v)	{ x/**/_h=v>>8; x/**/_l=v;}

#define	SCSI_HML_SET(x, v)	{ x/**/_h=v>>16; x/**/_m=v>>8; x/**/_l=v;}

#define	SCSI_HMML_SET(x, v)	\
		    { x/**/_h=v>>24; x/**/_mh=v>>16; x/**/_ml=v>>8; x/**/_l=v;}

#define	SET_CDB_0(cdb, cmd, lba, len, fr, link)		{	\
		bzero(cdb, sizeof (struct cdb_0));			\
		(cdb)->cdb_0.cdb_0_cmd = cmd;				\
		(cdb)->cdb_0.cdb_0_lun = 0;				\
		SCSI_HML_SET((cdb)->cdb_0.cdb_0_lba, lba);		\
		(cdb)->cdb_0.cdb_0_len = len;				\
		(cdb)->cdb_0.cdb_0_fr = fr;				\
		(cdb)->cdb_0.cdb_0_link = link;				\
		wbflush();						}

#define	SET_CDB_1(cdb, cmd, reladr, lba, len, fr, link)	{	\
		bzero(cdb, sizeof (struct cdb_1));			\
		(cdb)->cdb_1.cdb_1_cmd = cmd;				\
		(cdb)->cdb_1.cdb_1_lun = 0;				\
		(cdb)->cdb_1.cdb_1_reladr = reladr;			\
		SCSI_HMML_SET((cdb)->cdb_1.cdb_1_lba, lba);		\
		SCSI_HL_SET((cdb)->cdb_1.cdb_1_len, len);		\
		(cdb)->cdb_1.cdb_1_fr = fr;				\
		(cdb)->cdb_1.cdb_1_link = link;				\
		wbflush();						}

struct	flunitinfo {	       		/* pseudoctlr status block */
	u_char		ui_open;	/* number of open references */
	u_char		ui_dev;		/* type of disk first opened as */
	u_char		ui_sense;	/* sense was requested */
	u_char		ui_errcnt;
	u_int		ui_lba;		/* block number */
	int		ui_bleft;	/* bytes left to transfer */
	int		ui_bpart;	/* bytes transferred */
	caddr_t		ui_addr;	/* address of transfer */
	struct iobuf	*ui_utab;	/* pointer to drive queue */
	struct iotime	*ui_iotime;	/* pointer to drive queue */
	struct flop_msel ui_msel;
	struct buf	ui_cflbuf;	/* buffer header for special commands */
	struct shadevice	ui_dcb;	/* out of desperation */
	struct volume_header	ui_vh;	/* disk volume header for filesystems */
	SCSI_EXT_SENSE	ui_sns;		/* for sense on normal r/w */
};

struct iotime fliotime;	/* io statistics */
struct flunitinfo flunitinfo, *flalive;
struct iobuf flutab;
extern	int flmajor;				/* internal major # */
extern  struct flop_msel fldisktypes[];
extern  int nfltypes, flexterr;
extern char *ascii_sha_cerr[];
extern char *ascii_sns_7_key[];

/*                _________________________________
 * minor(dev):    | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *                ---------------------------------
 *                \___type_______/ \__partition__/
 */

#define	FL_PART(dev)		((minor(dev)) & 0xF)
#define	FL_TYPE(dev)		(((minor(dev)) >> 4) & 0xF)

#define FL_MAXCNT	0x1FE00
#define	FL_MAXRETRY	3

struct shadevice *flstart();
int flstrategy();
int fl_noaccess = 0;

flopen(dev)
	dev_t	dev;
{
	register int                type = FL_TYPE(dev);
	register struct flunitinfo *ui = flalive;

	if (ui == NULL) {
		u.u_error = ENXIO;
		return;
	}
	if (!ui->ui_open) {
		if (type < nfltypes) {
			ui->ui_msel = fldisktypes[type];
			if (flmodslct(ui))
				u.u_error = ENXIO;
		}
		ui->ui_dev = dev;
	} else if (FL_TYPE(ui->ui_dev) != type)
		u.u_error = ENXIO;
	if (!u.u_error) {
		flgetvh(ui);
		ui->ui_open++;
	}
}

flclose(dev)
	dev_t	dev;
{
	register struct flunitinfo *ui = flalive;

	fl_noaccess = 0;
	ui->ui_open = 0;
}

flread(dev)
	register dev_t dev;
{
	register struct flunitinfo *ui = flalive;

	if (physck(ui->ui_vh.vh_pt[FL_PART(dev)].pt_nblks, B_READ)) /**/
		physio(flstrategy, 0, dev, B_READ);
}


flwrite(dev)
	register dev_t dev;
{
	register struct flunitinfo *ui = flalive;

	if (physck(ui->ui_vh.vh_pt[FL_PART(dev)].pt_nblks, B_WRITE)) /**/
		physio(flstrategy, 0, dev, B_WRITE);
}

flsize(dev)
	register dev_t dev;
{
	register struct flunitinfo *ui = flalive;

	if (ui == NULL)
		return(-1);
	return (ui->ui_vh.vh_pt[FL_PART(dev)].pt_nblks);
}


flioctl(dev, cmd, arg, flag)
	register dev_t	dev;
	register u_int cmd;
	register caddr_t arg;
	register int flag;
{
	register struct flunitinfo 	*ui = flalive;
	struct io_arg 			io_arg;
	struct shadevice		*dcbp = &ui->ui_dcb;
	u_long				ilv;

	if (ui->ui_open != 1 || FL_PART(dev) != 10) {
		u.u_error = EBUSY;
		return;
	}
	switch (cmd) {

	    case FLIOCMODSNS:
		/*
		 * get specialized disk parameters
		 */
		if ((copyin(arg, &io_arg, sizeof (io_arg)) < 0)
		    || (copyout(&ui->ui_msel, io_arg.memaddr,
			sizeof(ui->ui_msel)) < 0)) {
			u.u_error = EFAULT;
			return;
		}
		break;

	    case FLIOCMODSLCT:
		/*
		 * set specialized disk parameters
		 */
		if (FL_TYPE(dev) < nfltypes) {
			u.u_error = EINVAL;
			return;
		}
		if ((copyin(arg, &io_arg, sizeof (io_arg)) < 0)
		    || (copyin(io_arg.memaddr, &ui->ui_msel,
			sizeof(ui->ui_msel)) < 0)) {
			u.u_error = EFAULT;
			return;
		}
		if (flmodslct(ui))
			u.u_error = EIO;
		break;

	    case FLIOCFORMAT:
		if ((copyin(arg, &io_arg, sizeof (io_arg)) < 0)
		    || (copyin(io_arg.memaddr, &ilv,
			sizeof(ilv)) < 0)) {
			u.u_error = EFAULT;
			return;
		}
		SET_CDB_0(&dcbp->sha_cdb, C0_FORMAT, 0, ilv, 0, 0);
		dcbp->sha_pfn[0] = 0;
		SET_SHA(dcbp, SHA_IE, SHA_DIR_IN, SHA_NODISC, sizeof(struct cdb_0)<<8,0,0);
		if (fldocmd(ui, dcbp))
			u.u_error = EIO;
		break;

	    default:
		u.u_error = EIO;
		break;
	}
}

flstrategy(bp)
	register struct buf	*bp;
{
	register struct flunitinfo *ui = flalive;
	register int s;
	register u_long nbps = ui->ui_msel.msel_desc.desc_pg.flop_nbyte;
	register struct iobuf *dp;

	if (fl_noaccess) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	dp = ui->ui_utab;
	s = splclock();
	if (bp->b_bcount)
		iomap(bp);
	if (bp == &ui->ui_cflbuf) {
		/* Special ioctl commands */
		bp->av_forw = NULL;
		if (dp->b_actf == NULL) {
			dp->b_actf = bp;
			dp->b_actl= bp;
		} else {
			dp->b_actl->av_forw = bp;
			dp->b_actl = bp;
		}
	} else {
		if (bp->b_blkno < 0 ||
		    bp->b_blkno * nbps + bp->b_bcount >
		        ui->ui_vh.vh_pt[FL_PART(bp->b_dev)].pt_nblks * nbps) {
				bp->b_flags |= B_ERROR;
				iounmap(bp);
				iodone(bp);
				return;
		}
		bp->b_resid = bp->b_blkno + 
		        ui->ui_vh.vh_pt[FL_PART(bp->b_dev)].pt_firstlbn;
		bp->b_start = lbolt;
		ui->ui_iotime->io_cnt++;
		ui->ui_iotime->io_bcnt += bp->b_bcount / nbps;
		dp->qcnt++;
		disksort(dp, bp);
	}
	if (dp->b_active == 0)
		fl_start(dp);
	splx(s);
	return;
}

struct shadevice *
flstart(dp)
	register struct iobuf *dp;
{
	register struct buf *bp;
	register u_long baddr, bcnt, nbps;
	register struct flunitinfo *ui;
	register struct shadevice *shaaddr, *dcbp;
	register SCSI_EXT_SENSE	*sns;

	bp = dp->b_actf;
	ui = flalive;
	shaaddr = (struct shadevice *)dp->io_addr;
	sns = (SCSI_EXT_SENSE *)(K0_TO_K1(&ui->ui_sns));

	if (ui->ui_sense) {
		SET_CDB_0(&shaaddr->sha_cdb, C0_REQSENSE, 0,
		    sizeof(SCSI_EXT_SENSE), 0, 0);
		iop_ptes(shaaddr->sha_pfn, sns, sizeof(*sns));
		SET_SHA(shaaddr, SHA_IE, SHA_DIR_IN, SHA_NODISC, 
		    sizeof(struct cdb_0)<<8, sns, sizeof(*sns));
	} else if (bp == &ui->ui_cflbuf) {
		dcbp = (struct shadevice *)bp->b_dmaaddr;
		bcopy(dcbp, shaaddr, sizeof(struct shadevice));
		iop_ptes(shaaddr->sha_pfn, shaaddr->sha_pfn[0], dcbp->sha_dlen);
		shaaddr->sha_pgofset = (long )dcbp->sha_pfn[0] & PGOFSET;
		wbflush();
	} else {
		nbps = ui->ui_msel.msel_desc.desc_pg.flop_nbyte;
		if (ui->ui_bleft == 0) {
		    dp->io_start = lbolt;
		    ui->ui_lba = bp->b_blkno +
			ui->ui_vh.vh_pt[FL_PART(bp->b_dev)].pt_firstlbn;
		    bp->b_resid = ui->ui_bleft = bp->b_bcount;
		    ui->ui_addr = bp->b_dmaaddr;
		}

	    	
		ui->ui_bpart = ui->ui_bleft > FL_MAXCNT 
			? FL_MAXCNT : ui->ui_bleft;;
		bcnt = (ui->ui_bpart + nbps - 1) / nbps;
		SET_CDB_1(&shaaddr->sha_cdb, 
			(bp->b_flags & B_READ) ? C1_READ:C1_WRITE, 0,
			ui->ui_lba, bcnt, 0, 0);
		iop_ptes(shaaddr->sha_pfn, (u_long)ui->ui_addr, ui->ui_bpart);
		SET_SHA(shaaddr, SHA_IE,
			(bp->b_flags & B_READ) ? SHA_DIR_IN : SHA_DIR_OUT,
			SHA_NODISC, sizeof(struct cdb_1)<<8, (u_long)ui->ui_addr,
			ui->ui_bpart);
	}
	return (shaaddr);
}

flgetvh(ui)
	register struct flunitinfo *ui;
{
	register struct volume_header	*vh;
	struct shadevice		*dcbp = &ui->ui_dcb;
	register struct pg_flop		*pg = &ui->ui_msel.msel_desc.desc_pg;

	vh = (struct volume_header *)K0_TO_K1(&ui->ui_vh);

	if (pg->flop_nbyte != 0x200) {
		fl_set_default(vh, pg);
		return(0);
	}
	SET_CDB_1(&dcbp->sha_cdb, C1_READ, 0, 0, 1, 0, 0);
	dcbp->sha_pfn[0] = (u_long )vh;
	SET_SHA(dcbp, SHA_IE, SHA_DIR_IN, SHA_NODISC, 
		sizeof(struct cdb_1)<<8, 0, sizeof(*vh));
	if (fldocmd(ui, dcbp) || vh->vh_magic != VHMAGIC || csum_vh(vh))
		fl_set_default(vh, pg);
	return(0);
}

fl_set_default(vh, pg)
	struct volume_header	*vh;
	struct pg_flop		*pg;
{
	cmn_err(CE_CONT, "FLOPPY: Cannot get volume header: using default\n");
	bzero(vh, sizeof(*vh));
	vh->vh_magic = VHMAGIC;
	vh->vh_pt[10].pt_nblks = pg->flop_ncyl * pg->flop_nhead * pg->flop_nsec;
	vh->vh_pt[10].pt_type = PTYPE_VOLUME;
}

csum_vh(vh)
	register struct volume_header *vh;
{
	register csum = 0;
	register int *ip = (int *)vh;

	while (ip < (int *)(vh + 1))
		csum += *ip++;
	return(csum);
}

flmodslct(ui)
	register struct flunitinfo *ui;
{
	register struct flop_msel  *ms;
	struct shadevice 	   *dcbp = &ui->ui_dcb;

	ms = (struct flop_msel *)K0_TO_K1(&ui->ui_msel);
	SET_CDB_0(&dcbp->sha_cdb, C0_MODESEL, 0, sizeof(*ms), 0, 0);
	dcbp->sha_pfn[0] = (u_long )ms;
	SET_SHA(dcbp, SHA_IE, SHA_DIR_OUT, SHA_NODISC, 
		sizeof(struct cdb_0)<<8, 0, sizeof(*ms));
	if (fldocmd(ui, dcbp))
		return (-1);
	return (0);
}

fldocmd(ui, dcbp)
	register struct flunitinfo *ui;
	register struct shadevice  *dcbp;
{
	register struct buf 	*bp = &ui->ui_cflbuf;
	register int 		err, s = splclock();

	while (bp->b_flags & B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY;
	splx(s);
	bp->b_dev = ui->ui_dev;
	bp->b_dmaaddr = (char *)dcbp;
	err = u.u_error;
	flstrategy(bp);
	iowait(bp);
	u.u_error = err;
	bp->b_flags &= ~B_BUSY;
	if (bp->b_flags & B_WANTED)
		wakeup((caddr_t)bp);
	return (bp->b_flags & B_ERROR);
}

struct iobuf flque;
SCSI_INQUIRY fl_inq;

fledtinit (edt)
    register struct edt *edt;
{
    register struct flunitinfo *ui;
    register struct iobuf *dp;
    register struct shadevice *shaaddr;
    register SCSI_INQUIRY *inq;
    register int retry;
    extern int showconfig;
    
    if (shaaddr = (struct shadevice *)fl_probe()) {
	bzero (&fl_inq, sizeof(fl_inq));/**/
	fl_inq.device_type = 255;
	inq = (SCSI_INQUIRY *)K0_TO_K1(&fl_inq);
	SET_CDB_0(&shaaddr->sha_cdb, C0_INQUIRY, 0,
		  sizeof(fl_inq), 0, 0);
	iop_ptes(shaaddr->sha_pfn, inq, sizeof(fl_inq));
	SET_SHA(shaaddr, SHA_NOIE, SHA_DIR_IN, SHA_NODISC,
		sizeof(struct cdb_0)<<8, inq, sizeof(fl_inq));
	shaaddr->sha_cmd = SHA_SCSI;
	if (fl_issue(shaaddr) == 0) {
	    if (inq->device_type == TYPE_M12_FLOPPY) {
		ui = flalive = &flunitinfo;
		ui->ui_utab = dp = &flutab;
		ui->ui_iotime = &fliotime;
		dp->io_addr = (paddr_t)shaaddr;
		ui->ui_open = 0;
	    }
	    if (showconfig)
		cmn_err (CE_CONT, "FLOPPY %s", inq->vendor_id);
	}
    }
    else
      edt->e_base = -1;
    if (showconfig)
      cmn_err (CE_CONT, "\n");
}

/*
 * Check that target exists
 */
fl_probe ()
{
    register struct shadevice *shaaddr;
    struct shadevice *iop_alloc();
    
    if ((shaaddr = iop_alloc(FLOPPYIOCB, sizeof (*shaaddr))) == NULL)
      return 0;
    
    shaaddr->sha_cmd = SHA_INIT;
    shaaddr->sha_dlen = MAXPHYS + NBPG;
    
    wbflush();
    if (fl_issue (shaaddr))
      return 0;
    else
      return (int)shaaddr;
}

fl_issue (shaaddr)
    register struct shadevice *shaaddr;
{
    long l = 8000000, status;

    iop_poke (FLOPPYIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(shaaddr), 0);
    while (l--) {
	if (iop_wait(FLOPPYIOCB, IOPB_NOWAIT, &status, 0) == 0) {
	    iop_clear(FLOPPYIOCB);
	    return(0);
	}
	DELAY(400);
    } 
    return -1;
}

fl_start(dp)
    register struct iobuf *dp;
{
    register struct shadevice *shaaddr;
    register struct iobuf *tp;
    
    dp->b_active = 1;
    dp->b_forw = NULL;
    tp = &flque;

    if ((tp->b_forw) == NULL) {
	tp->b_forw = (struct buf *)dp;
	tp->b_back = (struct buf *)dp;
    } else {
	((struct iobuf *)tp->b_back)->b_forw = (struct buf *)dp;
	tp->b_back = (struct buf *)dp;
    }

    if (tp->b_active == 0) {
	tp->b_active = 1;
	shaaddr = flstart(tp->b_forw);
	fl_run(shaaddr);
    }
}

fltimeout()
{
    int status;
    
    if (iop_wait (FLOPPYIOCB, IOPB_NOWAIT, &status, 0) == 0) {
	cmn_err (CE_NOTE, "FLOPPY: cmd timed out/semaphore set\n");
	flintr (0, 0, 0);
    }
    else
      (void)timeout (fltimeout, 0, HZ * 2);
}

flintr (dummy0, dummy, iocb)
    register int dummy0;
    unsigned dummy;
    register struct iocb *iocb;
{
    register struct iobuf *tp;
    register struct shadevice *shaaddr;
    register struct iobuf *dp;
    
    tp = &flque;
    if (!tp->b_forw) {
	cmn_err (CE_CONT, "FLOPPY: stray interrupt\n");
	iop_clear (FLOPPYIOCB);
	return;
    }

    untimeout_func (fltimeout, 0);
    dp = (struct iobuf *)tp->b_forw;

    tp->b_forw = dp->b_forw;
    iop_clear(FLOPPYIOCB);
    tp->b_active = 0;
    dp->b_active = 0;

    /*
     * Device specific interrupt processing.
     */
    flopintr(dp);
    /*
     * If target is not active and there's more work to be done
     * let's start it up.
     */
    if (tp->b_active == 0 && tp->b_forw) {
	tp->b_active = 1;
	shaaddr = flstart(tp->b_forw);
	fl_run(shaaddr);
    }
}

flopintr(dp)
register struct iobuf *dp;
{
	register struct buf *bp;
	register u_long baddr, nbps;
	register struct flunitinfo *ui;
	register SCSI_EXT_SENSE	*sns;
	register struct shadevice *shaaddr;
	int not_ready = 0;

	bp = dp->b_actf;
	ui = flalive;
	sns = (SCSI_EXT_SENSE *)(K0_TO_K1(&ui->ui_sns));
	shaaddr = (struct shadevice *)dp->io_addr;

	/* if call was a sense request; finish off processing.  */
	if (ui->ui_sense) {
		if (shaaddr->sha_cerr) {
		    if (flexterr)
			cmn_err(CE_CONT, "FLOPPY sense failed: %s: scsi %b",
				ascii_sha_cerr[shaaddr->sha_cerr],
				shaaddr->sha_scsi_status, SCSI_STATUS_BITS);
			if ((ui->ui_sense++ < 3) &&
			    (shaaddr->sha_scsi_status == SCSI_CHECK)) {
		        	if (dp->b_active == 0)
				    fl_start(dp);
				return;
			}
			if (flexterr) cmn_err(CE_CONT, "no more retries");
			bzero(sns, sizeof(*sns));
			shaaddr->sha_cerr = 0;
		}
		ui->ui_sense = 0;
		if (sns->class != 7) {
		    if (flexterr)
			cmn_err(CE_CONT, "invalid sense class %d", 
				sns->class);
			bzero(sns, sizeof(SCSI_EXT_SENSE));
		}
		if (sns->key) {
		    if (flexterr)
			cmn_err(CE_CONT, "%s: code %x",
				ascii_sns_7_key[sns->key],
				sns->sense_code);
		    if (sns->key == UNIT_ATN && ui->ui_open) {
			cmn_err(CE_CONT,"\nFLOPPY removed before close\n");
			fl_noaccess++;
		    }
		    if (sns->key == NOT_RDY)
			not_ready = 1;
		}
		if (sns->valid)
		    if (flexterr)
			cmn_err(CE_CONT, ": bn %d (0x%x)",
			    SCSI_HMML(sns->info),
			    SCSI_HMML(sns->info));
		if (sns->key == REC_ERR) {
		    if (flexterr)
			cmn_err(CE_CONT, ": soft error\n");
		} else if (++ui->ui_errcnt < FL_MAXRETRY) {
		    if (flexterr)
			cmn_err(CE_CONT, ": reissued\n");
			if (not_ready)
			    (void)timeout(fl_start,dp,HZ*2);
			else
			    fl_start(dp);
			return;
		} else {
			bp->b_flags |= B_ERROR;
		    if (flexterr)
			cmn_err(CE_CONT, "\n");
		}
	}

	/* check errors, get sense if available */
	if (shaaddr->sha_cerr || shaaddr->sha_scsi_status) {
	    if (flexterr)
		cmn_err(CE_CONT, "FLOPPY: cmd 0x%x: ", 
			shaaddr->sha_cdb.cdb_raw[0]);
	    if (shaaddr->sha_cerr) {
		if (flexterr)
		    cmn_err(CE_CONT, "%s\n",ascii_sha_cerr[shaaddr->sha_cerr]);
	    } else if (shaaddr->sha_scsi_status == SCSI_CHECK) {
		/* start sense request processing */
		ui->ui_sense = 1;
		fl_start(dp);
		return;
	    } else if (flexterr)
		cmn_err(CE_CONT, "scsi status 0x%x",
				shaaddr->sha_scsi_status);
	    if (++ui->ui_errcnt < FL_MAXRETRY) {
		fl_start(dp);
		return;
	    }
	    if (flexterr) cmn_err(CE_CONT, "\n");
	    bp->b_flags |= B_ERROR;
	}
	ui->ui_errcnt = 0;

	if (bp != &ui->ui_cflbuf) {
		/*
		 * check if more data from previous request;
		 * if so, update fl struct.
		 */
		if (((bp->b_flags & B_ERROR) == 0) &&
		    ((ui->ui_bleft -= ui->ui_bpart) > 0)) {
			nbps = ui->ui_msel.msel_desc.desc_pg.flop_nbyte;
			ui->ui_lba += ((ui->ui_bpart + nbps - 1) / nbps);
			ui->ui_addr += ui->ui_bpart;
		        bp->b_resid = ui->ui_bleft;
			if (dp->b_active == 0) {
				fl_start(dp);
			}
			return;
		}
		dp->qcnt--;
		ui->ui_iotime->io_resp += lbolt - bp->b_start;
		ui->ui_iotime->io_act += lbolt - dp->io_start;
		bp->b_resid = ui->ui_bleft;
		bp->b_resid = ui->ui_bleft = 0;
	}
	/*
	 * if no more data, or had unrecoverable error,
	 */
	dp->b_actf = bp->av_forw;
	if (bp->b_bcount)
		iounmap(bp);
	iodone(bp);
	if ((dp->b_actf != NULL) && (dp->b_active == 0))
		fl_start(dp);
}

fl_run(shaaddr)
    register struct shadevice *shaaddr;
{
    shaaddr->sha_cmd = SHA_SCSI;
    iop_poke(FLOPPYIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(shaaddr), 0);
    (void)timeout (fltimeout, 0, HZ * 2);
}

fl_checkstat(shaaddr)
    register struct shadevice *shaaddr;
{
    struct shadevice sha_saved;
    SCSI_EXT_SENSE sns;
    int retry;
    
    if (shaaddr->sha_cerr != SHA_CERR_OK)
	return -2;
    else {
	switch (shaaddr->sha_scsi_status) {
	  case 0:
	    return 0;
	    
	  case SCSI_CHECK:
	    sha_saved = *shaaddr;
	    SET_CDB_0 (&shaaddr->sha_cdb, C0_REQSENSE, 0,
		       sizeof (sns), 0, 0);
	    iop_ptes (shaaddr->sha_pfn, K1_TO_PHYS(&sns), sizeof (sns));
	    SET_SHA(shaaddr, SHA_NOIE, SHA_DIR_IN, SHA_NODISC, sizeof(struct cdb_0)<<8,
		    &sns, sizeof (sns));
	    if (fl_issue (shaaddr)
		|| (shaaddr->sha_cerr != SHA_CERR_OK)) {
		    break;
	    }
	    if (sns.class != 7) {
		return 0;
	    }

	    switch (sns.key) {
	      case REC_ERR:
		return 0;

	      case CMD_ABORT:
	      case UNIT_ATN:
		retry = 0;
		while (retry++ < 3) {
		    *shaaddr = sha_saved;
		    fl_issue (shaaddr);
		    if (shaaddr->sha_scsi_status == 0)
		      return 0;
		}
		break;

	      default:
		break;
	    }
	    break;

	  default:
	    break;
	}
	return -1;
    }
}

sha_dbg (arg)
    register int arg;
{
    register struct shadevice *shaaddr;
    
    shaaddr = (struct shadevice *)iop_alloc(SCSIIOCB,
					    sizeof (struct shadevice));
    
    shaaddr->sha_cmd = arg ? SHA_DBGON : SHA_DBGOFF;
    shaaddr->sha_timeout = arg;
    wbflush();
    if (fl_issue (shaaddr))
      return (0);
    else
      return ((int)shaaddr);
}

#define DCR_BITS "\020\03INTR\02DIR_OUT\01NODISC"
char *ascii_sha_commands[] = {
    "Invalid command",
    "Initialize",
    "SCSI command",
    "Debug on",
    "Debug off",
};

dump_sha (sha)
    register struct shadevice *sha;
{
    register int i;
    register char *p;

    cmn_err (CE_CONT, "Command: %s    Cerr: %s\n",
	     ascii_sha_commands[sha->sha_cmd], ascii_sha_cerr[sha->sha_cmd]);
    cmn_err (CE_CONT, "Status: %d    dcr: %b\n", sha->sha_scsi_status, sha->sha_dcr,
	     DCR_BITS);
    cmn_err (CE_CONT, "timeout: %d    pgofset: 0x%x:    datalen 0x%x\n",
	     sha->sha_timeout, sha->sha_pgofset, sha->sha_dlen);
    cmn_err (CE_CONT, "Page numbers\n");
    for (i = 0; i < (MAXPHYS/NBPG + 1); i++)
      cmn_err (CE_CONT, "0x%x ", sha->sha_pfn[i]);
    cmn_err (CE_CONT, "\nCommand descriptor block [in hex]\n");
    for (i = 0; i < 16; i++)
      cmn_err (CE_CONT, "%x ", sha->sha_cdb.cdb_raw[i]);
    cmn_err (CE_CONT, "\n");
}
