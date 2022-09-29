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
#ident	"$Header: fl8272.c,v 1.8.1.4.1.3.1.8 91/01/14 19:28:05 beacker Exp $"
/* floppy disk driver for the 8272 controller */

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
#include "sys/m12scsi.h"
#include "sys/scsi.h"
#include "sys/shareg.h"
#include "sys/file.h"


#include "sys/fd8272.h"
#define MAX_LOOP 100000
#define DEFAULT_NRETRIES 16
static int fl_retry = DEFAULT_NRETRIES;
#define FL_RETRY	fl_retry
/*
#define cmn_err
*/

#define FD_DEBUG  

#define spl_floppy splfloppy 

extern int showconfig;

#define STATIC

STATIC
int fdinited = 0, last_xfer = 0, fd_io_delay= 4;



static struct flop_msel fldisktypes[] = {	/* preset disk parameters */
/*  default type  double density ) .722 M */
	{0, {PD_FLOP, sizeof(struct pg_flop), 

/*       ncy nbyte   nsec  nhead */

	{80, 512,    9,    2,

/*      xfer  gap  fgap  mon moff  hsd   srt   hlt   hut   mfm   hcap */

	0x2, 0x1b, 0x54, 0x5, 0x6, 0x0, 0x0a, 0x01, 0x0f, 0x01, 0x00}}},

/*	High density format 1.44 M */

	{0, {PD_FLOP, sizeof(struct pg_flop), 
	{80, 512,    18,    2,
	0x0, 0x1b, 0x54, 0x5, 0x6, 0x0, 0x0a, 0x01, 0x0f, 0x01, 0x01}}},

/*	Single density format 0.4 M */

	{0, {PD_FLOP, sizeof(struct pg_flop), 
	{80, 512,    5,    2,
	0x2, 0x1b, 0x3a, 0x5, 0x6, 0x0, 0x0a, 0x01, 0x0f, 0x00, 0x00}}},

/*	Double density single sided	*/

	{0, {PD_FLOP, sizeof(struct pg_flop), 
	{80, 512,    9,    1,
	0x2, 0x1b, 0x54, 0x5, 0x6, 0x0, 0x0a, 0x01, 0x0f, 0x01, 0x00}}},

/*	High density single sided	*/

	{0, {PD_FLOP, sizeof(struct pg_flop), 
	{80, 512,    18,    1,
	0x0, 0x1b, 0x54, 0x5, 0x6, 0x0, 0x0a, 0x01, 0x0f, 0x01, 0x01}}},

/*	Single density format single sided */

	{0, {PD_FLOP, sizeof(struct pg_flop), 
	{80, 512,    5,    1,
	0x2, 0x1b, 0x3a, 0x5, 0x6, 0x0, 0x0a, 0x01, 0x0f, 0x00, 0x00}}},

};
static int nfltypes = 6;


#define FD_CYLINDER(sfd,pf) (sfd->sf_block_no / (pf->flop_nsec * pf->flop_nhead))

#define FD_HEAD(sfd,pf) ((sfd->sf_block_no / pf->flop_nsec) % pf->flop_nhead)

#define FD_SECTOR(sfd,pf)	((sfd->sf_block_no % pf->flop_nsec) + 1)

#define FD_BTON(x)	(x >> 8)
#define FD_NOSECTOR	0xff

#define	FL_PART(dev)		((minor(dev)) & 0xF)
#define	FL_TYPE(dev)		(((minor(dev)) >> 4) & 0x7)

struct sfd8272 sfd0;
extern struct fd8272 fd0;


/*	fd_start is the scheduler and must be called at spl_floppy
	all requests are posted via the buf structure and queued up at
	sf_iobuf.  On completion of each of the request, iodone will be
	called.
*/


/*	define IMPLIED_SEEK here to enable implied seek else manual seek */

#define IMPLIED_SEEK

#define FD_DELAY DELAY(50)
#define FD_TIMEOUT 5

/*	this table is used by the low level layer to dispatch commands
	to the 82072.  The flags field is the key factor determining
	how a commands should be executed.  */

STATIC 
struct fd_cmd cmd_desc[] = {
	{FD_SEEK,FL_SEEK,FR_SEEK,FC_TURN_MOTOR_ON|FC_WAIT_PHASE | FC_REG0},
	{FD_READ_ID,FL_READ_ID,FR_READ_ID,FC_TURN_MOTOR_ON|FC_EXEC_PHASE|FC_REGS},
	{FD_READ_DATA,FL_READ_WRITE,FR_READ_WRITE,FC_PAD|FC_TURN_MOTOR_ON|FC_TERMINATE|FC_IO_FROM_FD|FC_REGS},
	{FD_WRITE_DATA,FL_READ_WRITE,FR_READ_WRITE,FC_PAD|FC_TURN_MOTOR_ON|FC_TERMINATE|FC_IO_TO_FD|FC_REGS},
	{FD_RECALIBRATE,FL_RECALIBRATE,FR_RECALIBRATE,FC_TURN_MOTOR_ON|FC_WAIT_PHASE | FC_REG0},
	{FD_SENSE_INTR,FL_SENSE_INTR,FR_SENSE_INTR,FC_RES_PHASE|FC_REG0},
	{FD_SPECIFY,FL_SPECIFY,FR_SPECIFY,FC_SAVE},
	{FD_CONFIGURE,FL_CONFIGURE,FR_CONFIGURE,FC_SAVE},
	{FD_MOTOR,FL_MOTOR,FR_MOTOR,FC_TURN_MOTOR_ON},
	{FD_REL_SEEK,FL_REL_SEEK,FR_REL_SEEK,FC_TURN_MOTOR_ON|FC_WAIT_PHASE|FC_REG0},
	{FD_DUMP_REG,FL_DUMP_REG,FR_DUMP_REG,FC_RES_PHASE},
	{FD_SENSE_DRIVE,FL_SENSE_DRIVE,FR_SENSE_DRIVE,FC_TURN_MOTOR_ON|FC_WAIT_RES_PHASE},
	{FD_FORMAT_TRACK,FL_FORMAT_TRACK,FR_READ_WRITE,FC_TURN_MOTOR_ON|FC_FORMAT|FC_IO_TO_FD|FC_REGS},
	{FD_NOOP,FL_NOOP,FR_NOOP,FC_RES_PHASE|FC_REG_NOOP},
	{FD_READ_DEL_DATA,FL_READ_WRITE,FR_READ_WRITE,FC_PAD|FC_TURN_MOTOR_ON|FC_TERMINATE|FC_IO_FROM_FD|FC_REGS},
	{FD_WRITE_DEL_DATA,FL_READ_WRITE,FR_READ_WRITE,FC_PAD|FC_TURN_MOTOR_ON|FC_TERMINATE|FC_IO_TO_FD|FC_REGS},
	{FD_READ_TRACK,FL_READ_WRITE,FR_READ_WRITE,FC_PAD|FC_TURN_MOTOR_ON|FC_TERMINATE|FC_IO_FROM_FD|FC_REGS},
/*
	scan commands are not supported on 82072 

	{FD_SCAN_EQ,FL_READ_WRITE,FR_READ_WRITE,FC_IO_TO_FD|FC_REGS},
	{FD_SCAN_LOW,FL_READ_WRITE,FR_READ_WRITE,FC_IO_TO_FD|FC_REGS},
	{FD_SCAN_HIGH,FL_READ_WRITE,FR_READ_WRITE,FC_IO_TO_FD|FC_REGS}
*/
};


int flinited = 0;

unsigned fifo_depth=FD_FIFO_DEPTH;

/*	user interface section */

flopen(dev,flags)
dev_t dev;
int flags;
{
	register type = FL_TYPE(dev);
	register struct buf *bp = &sfd0.sf_ioctl;
	int last_nretry;

	if(flinited == 0) fl_init();
	if(sfd0.sf_open == 0) {
		if(type < nfltypes) {
			sfd0.sf_flop = fldisktypes[type];
			sfd0.sf_bs = sfd0.sf_flop.msel_desc.desc_pg.flop_nbyte;
			sfd0.sf_need_recal = 1;
			fd_get_bp(bp);
			u.u_error = fl_command(&sfd0,bp,FDR_MODE_SELECT);
			fd_release_bp(bp);
		} else
			u.u_error = ENXIO;
		sfd0.sf_dev = dev;
	} else if(FL_TYPE(sfd0.sf_dev) != type)
		u.u_error = ENXIO;
	if(!u.u_error) {
		fl_set_default(&sfd0);
		if(!((FL_PART(dev) == 10) && (flags & FSYNC))) {
			fd_get_bp(bp);
			last_nretry = fl_retry;
			fl_retry = 4;
			fl_get_vhb(&sfd0,bp);
			fl_retry = last_nretry;
			fd_release_bp(bp);
		}
		sfd0.sf_open++;
	}
}

flsense()
{
	register struct buf *bp = &sfd0.sf_ioctl;
	fd_get_bp(bp);
	u.u_error = fl_command(&sfd0,bp,FDR_SENSE_DRIVE);
	fd_release_bp(bp);
}

flclose(dev)
dev_t dev;
{
	register struct buf *bp = &sfd0.sf_ioctl;
	register s;

	sfd0.sf_need_recal = 1;
	sfd0.sf_open = 0;
	fd_get_bp(bp);
	sfd0.sf_req = FDR_CLOSE;
	bp->b_flags &= ~B_DONE;
	flstrategy(bp);

	s = spl_floppy();
	iowait(bp);

	splx(s);
	
	fd_release_bp(bp);

/* restore the number of retries */

	fl_retry = DEFAULT_NRETRIES;
}

flread(dev)
register dev_t dev;
{
	register struct sfd8272 ;
	if(u.u_offset % FD_SECTSIZE) {
		u.u_error = EIO;
		return;
	}
	if(physck(sfd0.sf_vhb.vh_pt[FL_PART(dev)].pt_nblks,B_READ))
		physio(flstrategy,0,dev,B_READ);
}

flwrite(dev)
register dev_t dev;
{
	register struct sfd8272;
	if(u.u_offset % FD_SECTSIZE) {
		u.u_error = EIO;
		return;
	}
	if(physck(sfd0.sf_vhb.vh_pt[FL_PART(dev)].pt_nblks,B_WRITE))
		physio(flstrategy,0,dev,B_WRITE);
}



flioctl(dev,cmd,arg,flag)
dev_t dev;
u_int cmd;
caddr_t arg;
int flag;
{
	struct io_arg io_arg;
	register struct buf *bp;
	u_long ilv;

	/* for now only support low level controller level commands */
	if(sfd0.sf_open != 1 || FL_PART(dev) != 10) {
		u.u_error = EBUSY;
		return;
	}
	bp = &sfd0.sf_ioctl;
	fd_get_bp(bp);
	switch(cmd) {
	case FLIOC_SENSE_DRIVE: 
	{
		struct fl_io fl_io;
		fl_io.clen = 2;
		fl_io.rlen = 2;
		fl_io.dlen = 0;
		fl_io.cmd[0] = FD_RECALIBRATE;
		fl_io.cmd[1] = 0;
		fl_do_low_level(&sfd0,bp,&fl_io,dev,0);
		if(u.u_error) {
			break;
		}
		fl_io.clen = 2;
		fl_io.cmd[0] = FD_SENSE_DRIVE;
		fl_io.cmd[1] = 0;
		fl_io.rlen = 1;
		fl_io.dlen = 0;
		fl_do_low_level(&sfd0,bp,&fl_io,dev,0);
		if(u.u_error) {
			break;
		}
		if(!(fl_io.result[0] & FD_R3_READY))
			u.u_error = ENXIO;
	}
		break;
	case FLIOC_SET_NRETRIES:
		ilv = (u_long) arg;
		if(ilv <= DEFAULT_NRETRIES)
			fl_retry = ilv;
		else
			u.u_error = EINVAL;
		break;
	case FLIOC_LOW_LEVEL:
		if(suser() == 0)
			u.u_error = EPERM;
		else 
			fl_do_low_level(&sfd0,bp,arg,dev,1);
		break;
	case FLIOCMODSNS:
		if((copyin(arg, &io_arg, sizeof(io_arg)) < 0)
			|| (copyout(&sfd0.sf_flop, io_arg.memaddr,
				sizeof(sfd0.sf_flop)) < 0)) {
			u.u_error = EFAULT;
		}
		break;
	case FLIOCMODSLCT:
		if((copyin(arg, &io_arg, sizeof(io_arg)) < 0)
			|| (copyin(io_arg.memaddr, &sfd0.sf_flop, 
				sizeof(sfd0.sf_flop)) < 0)) {
			u.u_error = EFAULT;
			break;
		}
		sfd0.sf_bs = sfd0.sf_flop.msel_desc.desc_pg.flop_nbyte;
		sfd0.sf_need_recal = 1;
		u.u_error = fl_command(&sfd0,bp,FDR_MODE_SELECT); 
		break; 
	case FLIOC_SET_XFER_RATE:
FD_DEBUG(CE_CONT,"set xfer rate\n");
		if(suser() == 0) {
			u.u_error = EPERM;
			break;
		}
		if(copyin(arg,&ilv,sizeof(ilv)) < 0)
			u.u_error = EFAULT;
		else {
			sfd0.sf_flop.msel_desc.desc_pg.flop_xfer = ilv;
			sfd0.sf_need_recal = 1;
			u.u_error = fl_command(&sfd0,bp,FDR_MODE_SELECT);
		}
		break;
/* this is needed to tune the driver */
	case FLIOC_SET_FIFO_DEPTH:
FD_DEBUG(CE_CONT,"fifo\n");
		if(suser() == 0) {
			u.u_error = EPERM;
			break;
		}
		if((copyin(arg,&fifo_depth,sizeof(fifo_depth)) < 0)) {
			u.u_error = EFAULT;
		} 
		break;
	case FLIOCFORMAT:
		if((copyin(arg,&io_arg,sizeof(io_arg)) < 0)
			|| (copyin(io_arg.memaddr, &ilv, sizeof(ilv)) < 0)) {
			u.u_error = EFAULT;
			break;
		}
		sfd0.sf_interleave = ilv;
		u.u_error = fl_format_disk(&sfd0,bp);
		break;
	default:
		u.u_error = ENXIO;
		break;
	}
done:
	fd_release_bp(bp);
}


STATIC int fd_cmd_done();

fl_init()
{
	sfd0.sf_fd = &fd0;
	sfd0.sf_state = SF_READY;
	sfd0.sf_bp = 0;
	sfd0.sf_iobuf.b_actf = 0;
	nfltypes = sizeof(fldisktypes)/sizeof(struct flop_msel); 
	fdinit();
	flinited = 1;
	sfd0.sf_82077 = fd0.fd_82077;
	if(sfd0.sf_82077) {
		if(showconfig)
		cmn_err(CE_CONT,"Floppy disk controller: 82077\n");
	} else {
		if(showconfig)
		cmn_err(CE_CONT,"Floppy disk controller: 82072\n");
	}
}

/*	strategy and start routine */


flstrategy(bp)
register struct buf *bp;
{
	register s;
	register struct iobuf *dp;

	s = spl_floppy();
	/* for now, only support one floppy controller */
	dp = &sfd0.sf_iobuf;
	if(bp != &sfd0.sf_ioctl) {
		register nbps = sfd0.sf_bs;
		if(bp->b_blkno < 0 ||
			bp->b_blkno * nbps + bp->b_bcount > 
			sfd0.sf_vhb.vh_pt[FL_PART(bp->b_dev)].pt_nblks * nbps) {
				bp->b_error = EINVAL;
				bp->b_flags |= B_ERROR;
				iodone(bp);
				return;
		}
		if(bp->b_bcount)
			iomap(bp);
/*
		if disksort is called make sure the algo below calling fd_start
		also reflects the change

		disksort(dp,bp);
*/
	}
	bp->b_error = 0;
	bp->av_forw = NULL;
	if(dp->b_actf == NULL) {
		dp->b_actf = dp->b_actl = bp;
		fd_start(&sfd0);
	} else {
		dp->b_actl->av_forw = bp;
		dp->b_actl = bp;
	}
	splx(s);
}

fd_start(sfd)
register struct sfd8272 *sfd;
{
	register struct buf *bp;
	register struct iobuf *dp = &sfd->sf_iobuf;

	if(sfd->sf_busy || sfd->sf_state != SF_READY) return;
	sfd->sf_busy = 1;
again:
	if(sfd->sf_state != SF_READY) goto done;

	if((bp = dp->b_actf) == 0) goto done;
	dp->b_actf = bp->av_forw;
	sfd->sf_bp = bp;

	if(bp == &sfd->sf_ioctl) {
		fd_do_ioctl(sfd);
	} else
		fd_do_rw(sfd,bp);
	goto again;
done:
	sfd->sf_busy = 0;
}

/*	high level utilities with user context */

fl_do_low_level(sfd,bp,arg,dev,flag)
register struct sfd8272 *sfd;
register struct buf *bp;
dev_t dev;
{
	struct fl_io fl_io;

	if(flag) {
	if(copyin(arg,&fl_io,sizeof(fl_io)) < 0) {
		u.u_error = EFAULT;
		return;
	}
	} else
		bcopy(arg,&fl_io,sizeof(fl_io));
	if(fl_io.dlen > 0) {
		if(userdma(fl_io.data,fl_io.dlen,B_READ) == 0) {
			u.u_error = EFAULT;
			return;
		}
	bp->b_flags = B_BUSY | B_PHYS | B_READ;
	bp->b_error = 0;
	bp->b_proc = u.u_procp;
	bp->b_dev = dev;
		bp->b_bcount = fl_io.dlen;
		bp->b_un.b_addr = fl_io.data;
		bp->b_dmaaddr = 0;
		bp->b_dmalen = 0;
		iomap(bp);
		sfd->sf_ioctl_data = bp->b_dmaaddr;
		sfd->sf_ioctl_dlen = fl_io.dlen;
	}
	sfd0.sf_ioctl_clen = fl_io.clen;
	sfd0.sf_ioctl_rlen = fl_io.rlen;
	bcopy(fl_io.cmd,sfd->sf_ioctl_cmd,fl_io.clen);
	sfd->sf_req = FDR_LOW_LEVEL;

	bp->b_flags &= ~B_DONE;
	flstrategy(bp);

	iowait(bp);

	if(fl_io.dlen > 0) {
		iounmap(bp);	
		undma(fl_io.data,fl_io.dlen,B_READ,NULL);
	}
	if(bp->b_error) {
		u.u_error = bp->b_error;
	}
	if(flag) {
	if(copyout(sfd->sf_ioctl_res,&(((struct fl_io *)arg)->result[0]),fl_io.rlen) < 0) {
		u.u_error = EFAULT;
	}
	} else 
		bcopy(sfd->sf_ioctl_res,&(((struct fl_io *)arg)->result[0]),fl_io.rlen);
}

STATIC 
fl_get_vhb(sfd,bp)
register struct sfd8272 *sfd;
register struct buf *bp;
{
	register s;
	register rv = 0;

	sfd->sf_req = FDR_GET_VHB;
	bp->b_flags &= ~B_DONE;
	flstrategy(bp);

	s = spl_floppy();
	iowait(bp);
	u.u_error = 0;
	splx(s);

	if(bp->b_error || sfd->sf_vhb.vh_magic != VHMAGIC || csum_vh(sfd))
		fl_set_default(sfd);
}

STATIC 
fl_set_default(sfd)
register struct sfd8272 *sfd;
{
	register struct volume_header *vh = &sfd->sf_vhb;
	register struct pg_flop *pg = &sfd->sf_flop.msel_desc.desc_pg;
	bzero(vh, sizeof(*vh));
        vh->vh_magic = VHMAGIC;
	/* partition 10 is the entire disk */
        vh->vh_pt[10].pt_nblks = pg->flop_ncyl * pg->flop_nhead * pg->flop_nsec;
        vh->vh_pt[10].pt_type = PTYPE_VOLUME;
}

STATIC 
csum_vh(sfd)
register struct sfd8272 *sfd;
{
	register struct volume_header *vh = &sfd->sf_vhb;
	register csum = 0;
	register int *ip = (int *)vh;

	while (ip < (int *)(vh + 1))
		csum += *ip++;
	return(csum);
}

/* note that the 82072 chip has a bug - it always asks for 16 bytes more
   in the format command.  We can either supply these 16 extra bytes in
   the format command or handle it in the result phase.  The later solution
   is used now because it does not depend on the bug!  However, it may
   hold the system at high spl longer since we have to handle that in the
   result loop which polls.
*/

struct fd_format {
	u_char fd_cyl,fd_head,fd_sect,fd_nbyte;
} fd_format[22];

STATIC 
fl_format_disk(sfd,bp)
register struct sfd8272 *sfd;
register struct buf *bp;
{
	register int cyl,head,sector,index,
		nsec = sfd->sf_flop.msel_desc.desc_pg.flop_nsec;
	register s;
	int rv,nretries;

	if(fl_command(sfd,bp,FDR_SENSE_DRIVE)) {
		FD_DEBUG(CE_CONT,"cannot sense drive before format\n");
		goto done;
	}

	index = FD_BTON(sfd->sf_flop.msel_desc.desc_pg.flop_nbyte);

	for(sector = 0; sector < nsec; sector++) {
		fd_format[sector].fd_sect = FD_NOSECTOR;
		fd_format[sector].fd_nbyte = index;
	}
	index = 0;
	for(sector = 0; sector < nsec; sector++) {
		while(fd_format[index].fd_sect != FD_NOSECTOR)
			index = (index + 1) % nsec;
		fd_format[index].fd_sect = sector + 1;
		index = (index + sfd->sf_interleave) % nsec;
	}
	for(cyl = 0; cyl < sfd->sf_flop.msel_desc.desc_pg.flop_ncyl; cyl++) {
		for(head = 0; head < sfd->sf_flop.msel_desc.desc_pg.flop_nhead; head++) {
			for(sector = 0; sector < nsec; sector++) {
				fd_format[sector].fd_cyl = cyl;
				fd_format[sector].fd_head = head;
			}
			nretries = 0;
again:
			bp->b_error = 0;
			sfd->sf_req = FDR_FORMAT_TRACK;
			sfd->sf_format_cylinder = cyl;
			sfd->sf_format_head = head;
			s = spl_floppy();
			bp->b_flags &= ~B_DONE;
			flstrategy(bp);
			iowait(bp);
			splx(s);
			if(bp->b_error) {
				if(nretries++ < FL_RETRY)
					goto again;
				else
					goto done;
			}
		}
	}
done:
	return(bp->b_error);
}

STATIC 
fl_command(sfd,bp,cmd)
register struct sfd8272 *sfd;
register struct buf *bp;
{
	register s;

	sfd->sf_req = cmd;
	s = spl_floppy();
	bp->b_flags &= ~B_DONE;
	flstrategy(bp);
	iowait(bp);
	splx(s);

	return(bp->b_error);
}

/*	high level floppy functions and utilities - do not assume user context */

STATIC 
fd_do_rw(sfd,bp)
register struct sfd8272 *sfd;
register struct buf *bp;
{
	register nbps = sfd->sf_bs;
	register struct pg_flop *p = &sfd->sf_flop.msel_desc.desc_pg;

	sfd->sf_block_no = bp->b_blkno +
		sfd->sf_vhb.vh_pt[FL_PART(bp->b_dev)].pt_firstlbn;
	sfd->sf_addr = bp->b_dmaaddr;
	sfd->sf_nleft = bp->b_bcount;

	if(bp->b_flags & B_READ)
		sfd->sf_io = FD_READ_DATA;
	else
		sfd->sf_io = FD_WRITE_DATA;
	sfd->sf_state = SF_IO;
	sfd->sf_rw_retries = 0;
	fds_io(sfd,0);
}


STATIC 
fd_ioctl_to_cmd(sfd)
register struct sfd8272 *sfd;
{
	sfd->sf_clen = sfd->sf_ioctl_clen;
	sfd->sf_dlen = sfd->sf_ioctl_dlen;
	sfd->sf_rlen = sfd->sf_ioctl_rlen;
	bcopy(sfd->sf_ioctl_cmd,sfd->sf_cmd,sfd->sf_clen);
	sfd->sf_data = sfd->sf_ioctl_data;
}

STATIC 
fd_cmd_to_ioctl(sfd)
register struct sfd8272 *sfd;
{
	if(sfd->sf_rlen > 0)
		bcopy(sfd->sf_res,sfd->sf_ioctl_res,sfd->sf_rlen);
}

STATIC 
fd_do_ioctl(sfd)
register struct sfd8272 *sfd;
{
	switch(sfd->sf_req) {
	case FDR_LOW_LEVEL:
		sfd->sf_state = SF_EXEC_CMD;
		fd_ioctl_to_cmd(sfd);
		fdcmd(sfd,fd_cmd_done);
		break;
	case FDR_MODE_SELECT:
		sfd->sf_state = SF_MODE_SELECT;
		fds_mode_select(sfd,0);
		break;
	case FDR_SENSE_DRIVE:
		sfd->sf_state = SF_SENSE_DRIVE;
		fds_sense_drive(sfd,0);
		break;
	case FDR_CLOSE:
		sfd->sf_state = SF_CLOSE;
		fds_close(sfd,0);
		break;
	case FDR_RECAL:
		fdu_recal(sfd,fd_cmd_done);
		break;
	case FDR_GET_VHB:
		sfd->sf_state = SF_GET_VHB;
		fds_get_vhb(sfd,0);
		break;
	case FDR_FORMAT_TRACK:
		sfd->sf_state = SF_FORMAT_TRACK;
		fds_format_track(sfd,0);
		break;
	}
}

STATIC 
fd_cmd_done(sfd,err)
register struct sfd8272 *sfd;
int err;
{
	register struct buf *bp = sfd->sf_bp;
	if(bp == &sfd->sf_ioctl)
		fd_cmd_to_ioctl(sfd);
	else {
	    	if (bp == NULL) {
			goto nobp;
		} else if(bp->b_bcount)
			iounmap(bp);
	}
	if(err) {
		if(sfd->sf_state == SF_RECAL)
			sfd->sf_need_recal = 1;
		bp->b_flags |= B_ERROR;
		bp->b_error = err;
	} else {
		bp->b_flags &= ~B_ERROR;
		bp->b_error = 0;
	}
	iodone(bp);
nobp:
	sfd->sf_state = SF_READY;
	sfd->sf_bp = 0;
	if(sfd->sf_iobuf.b_actf)
		fd_start(sfd);
	else if(!(sfd->sf_82077))
		fd_light(sfd);
}

STATIC
fd_light_is_off(sfd,err)
register struct sfd8272 *sfd;
int err;
{
	sfd->sf_state = SF_READY;
	if(sfd->sf_iobuf.b_actf)
		fd_start(sfd);
}

STATIC
fd_turn_light_off(sfd)
register struct sfd8272 *sfd;
{
	int s;
	register u_char *cmd;

	s = spl_floppy();
	sfd->sf_flags &= ~SF_TIMEOUT;
	if(sfd->sf_state != SF_READY) {
		splx(s);
		return;
	}
	sfd->sf_state = SF_LIGHT;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_SENSE_DRIVE;
	cmd[1] = 1;
	sfd->sf_clen = 2;
	sfd->sf_dlen = 0;
	sfd->sf_rlen = 1;
	fdcmd(sfd,fd_light_is_off);
	splx(s);
}

#define SF_TIMEOUT_LENGTH 5*HZ
	
STATIC
fd_light(sfd)
register struct sfd8272 *sfd;
{
	if(sfd->sf_flags & SF_TIMEOUT) {
		untimeout(sfd->sf_timeout_id);
	} 
	sfd->sf_timeout_id = timeout(fd_turn_light_off,sfd,SF_TIMEOUT_LENGTH);
	sfd->sf_flags |= SF_TIMEOUT;
}


fd_get_bp(bp)
register struct buf *bp;
{
	register s;
	s = spl_floppy();
	while(bp->b_flags & B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t) bp, PRIBIO);
	}
	bp->b_flags = B_BUSY;
	bp->b_error = 0;
	splx(s);
}

fd_release_bp(bp)
register struct buf *bp;
{
	register s;
	s = spl_floppy();
	bp->b_flags &= ~B_BUSY;
	if(bp->b_flags & B_WANTED)
		wakeup((caddr_t) bp);
	splx(s);
}

/*	high level command sequences without user context
	format:
	fds_xxxxx(sfd,err)
*/

STATIC 
fds_format_track(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	register u_char *cmd,*p;
	register i;
	sfd->sf_new_cylinder = sfd->sf_format_cylinder;
	sfd->sf_new_head = sfd->sf_format_head;
	p = sfd->sf_addr = (u_char *)fd_format;
	sfd->sf_nleft = sizeof(struct fd_format) * 
		(sfd->sf_flop.msel_desc.desc_pg.flop_nsec);
	sfd->sf_io = FD_FORMAT_TRACK;
	sfd->sf_state = SF_IO;
	fds_fio(sfd,err);
}

STATIC 
fds_get_vhb(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	register u_char *cmd;
	sfd->sf_block_no = 0;
	sfd->sf_new_cylinder = 0;
	sfd->sf_new_head = 0;
	sfd->sf_new_sector = 1;
	sfd->sf_addr = (u_char *) &sfd->sf_vhb;
	sfd->sf_nleft = sizeof(sfd->sf_vhb);
	sfd->sf_io = FD_READ_DATA;
	sfd->sf_state = SF_IO;
	sfd->sf_rw_retries = 0;
	fds_io(sfd,err);
}

STATIC
fds_mode_select(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	if(err)
		fd_cmd_done(sfd,err);
	switch(sfd->sf_state) {
	case SF_MODE_SELECT:
		fdu_setup(sfd,fds_mode_select);
		break;
	case SF_SETUP_DONE:
		fdu_configure(sfd,fds_mode_select);
		break;
	case SF_CONFIGURE_DONE:
		fdu_mode_select(sfd,fd_cmd_done);
		break;
	default:
		fd_cmd_done(sfd,EINVAL);
		break;
	}
}
	
STATIC
fds_close(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	if(err)
		fd_cmd_done(sfd,err);
	switch(sfd->sf_state) {
	case SF_CLOSE:
		fdu_recal(sfd,fds_close);
		break;
	case SF_RECAL:
		fdu_noop(sfd,fd_cmd_done);
		break;
	default:
		fd_cmd_done(sfd,EINVAL);
		break;
	}
}

STATIC
fds_sense_drive(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	if(err)
		fd_cmd_done(sfd,err);
	switch(sfd->sf_state) {
	case SF_SENSE_DRIVE:
		fdu_recal(sfd,fds_sense_drive);
		break;
	case SF_RECAL:
		fdu_sense_drive(sfd,fds_sense_drive);
		break;
	case SF_SENSE_DONE:
		if(!(sfd->sf_res[0] &  FD_R3_TRACK0))
			fd_cmd_done(sfd,ENXIO);
		else
			fd_cmd_done(sfd,0);
		break;
	default:
		fd_cmd_done(sfd,EINVAL);
		break;
	}
}
	

STATIC 
fds_io(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	register new_cylinder;
	register struct buf *bp = sfd->sf_bp;
	register struct pg_flop *pf = &sfd->sf_flop.msel_desc.desc_pg;

again:
	switch(sfd->sf_state) {
	case SF_RW:
	case SF_IO:
		if(sfd->sf_state == SF_RW) {
			if(err) {
				if(sfd->sf_rw_retries++ < FL_RETRY) {
					sfd->sf_state = SF_IO;
					err = 0;
					sfd->sf_need_recal = 1;
					goto again;
				}
				bp->b_resid = sfd->sf_nleft;
				fd_cmd_done(sfd,err);
				return;
			}
			sfd->sf_block_no += (sfd->sf_xlen/sfd->sf_bs);
			sfd->sf_nleft -= sfd->sf_xlen;
			sfd->sf_nxfr += sfd->sf_xlen;
			sfd->sf_addr += sfd->sf_xlen;
		} else
			sfd->sf_nxfr = 0;
		if(sfd->sf_nleft == 0) {
			bp->b_resid = 0;
			fd_cmd_done(sfd,0);
			return;
		}
		sfd->sf_new_cylinder = FD_CYLINDER(sfd,pf);
		sfd->sf_new_head = FD_HEAD(sfd,pf);
		sfd->sf_new_sector = FD_SECTOR(sfd,pf);
		sfd->sf_xlen = fd_optimize_xlen(sfd,pf);
		sfd->sf_retries = 0;
		sfd->sf_state = SF_DO_SEEK;
		fds_seek(sfd,0);
		break;
	case SF_SEEKING:
		if(err) {
			if(sfd->sf_retries++ < FL_RETRY) {
				fds_seek(sfd,0);
			} else
				fd_cmd_done(sfd,err);
			return;
		}
		sfd->sf_state = SF_READ_ID;
		fdu_read_id(sfd,fds_io);
		break;
	case SF_READ_ID:
		if(err) {
			fd_cmd_done(sfd,err);
			return;
		}
		if(sfd->sf_res[3] != sfd->sf_cylinder)
		if(sfd->sf_retries++ < FL_RETRY) {
FD_DEBUG(CE_CONT,"seek to wrong cyl %d %d\n",sfd->sf_res[3],sfd->sf_cylinder);
			sfd->sf_new_cylinder = sfd->sf_cylinder;
			sfd->sf_need_recal = 1;
			sfd->sf_state = SF_DO_SEEK;
			fds_seek(sfd,0);
			return;
		} else {
FD_DEBUG(CE_CONT,"seek to wrong cyl %d %d DONE\n",sfd->sf_res[3],sfd->sf_cylinder);
			fd_cmd_done(sfd,err);
			return;
		}
		fdu_rw(sfd,fds_io);
	}
}

STATIC
fd_optimize_xlen(sfd,pf)
register struct sfd8272 *sfd;
register struct pg_flop *pf;
{
	register i,nsec = pf->flop_nsec;

	i = (nsec - sfd->sf_new_sector + 1) * sfd->sf_bs;
	if(i < sfd->sf_nleft)
		return(i);
	else
		return(sfd->sf_nleft);
}

/* note that in format we do not want to do a readid since it is probrably
   not there bu twe do not care.
*/


STATIC 
fds_fio(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	register new_cylinder;
	register struct buf *bp = sfd->sf_bp;
if(err) 
FD_DEBUG(CE_CONT,"io err %x %d ",err,sfd->sf_state);
	switch(sfd->sf_state) {
	case SF_RW:
	case SF_IO:
		if(sfd->sf_state == SF_RW) {
			if(err) {
				bp->b_resid = sfd->sf_nleft;
				fd_cmd_done(sfd,err);
				return;
			}
			sfd->sf_nleft -= sfd->sf_xlen;
			sfd->sf_nxfr += sfd->sf_xlen;
			sfd->sf_addr += sfd->sf_xlen;
		} else
			sfd->sf_nxfr = 0;
		if(sfd->sf_nleft == 0) {
			bp->b_resid = 0;
			fd_cmd_done(sfd,0);
			return;
		}
		sfd->sf_xlen = (sfd->sf_nleft <= sfd->sf_bs) ? sfd->sf_nleft :
			sfd->sf_bs;
		sfd->sf_retries = 0;
		sfd->sf_state = SF_DO_SEEK;
		fds_fseek(sfd,0);
		break;
	case SF_SEEKING:
		if(err) {
			if(sfd->sf_retries++ < FL_RETRY) {
				fds_fseek(sfd,0);
			} else
				fd_cmd_done(sfd,err);
			return;
		}
		fdu_rw(sfd,fds_fio);
	}
}

STATIC 
fds_fseek(sfd,err)
register struct sfd8272 *sfd;
register err;
{
	if(sfd->sf_state == SF_DO_SEEK) {
		if(!sfd->sf_need_recal && (sfd->sf_new_cylinder == 
		sfd->sf_cylinder)) {
			sfd->sf_state = SF_SEEKING;
			fds_fio(sfd,0);
			return;
		}
	}
	switch(sfd->sf_state) {
	case SF_RECAL:
		if(err) {
			sfd->sf_need_recal = 1;
			fd_cmd_done(sfd,err);
			return;
		}
	case SF_DO_SEEK:
		if(sfd->sf_need_recal) {
			fdu_recal(sfd,fds_fseek);
			return;
		}
		if(sfd->sf_new_cylinder != sfd->sf_cylinder) {
			fdu_seeking(sfd,fds_fseek);
			return;
		}
		sfd->sf_state = SF_SEEKING;
	case SF_SEEKING:
		if(err)
			sfd->sf_need_recal = 1;
		fds_fio(sfd,err);
	}
}

STATIC 
fds_seek(sfd,err)
register struct sfd8272 *sfd;
register err;
{
#ifdef IMPLIED_SEEK
	if(err) {
		sfd->sf_need_recal = 1;
		fd_cmd_done(sfd,err);
		return;
	}
	switch(sfd->sf_state) {
	case SF_RECAL:
	case SF_DO_SEEK:
		if(sfd->sf_need_recal) {
			fdu_recal(sfd,fds_seek);
			return;
		}
	default:
		fdu_rw(sfd,fds_io);
		break;
	}
#else
	if(sfd->sf_state == SF_DO_SEEK) {
		if(!sfd->sf_need_recal && (sfd->sf_new_cylinder == 
		sfd->sf_cylinder)) {
			sfd->sf_res[3] = sfd->sf_cylinder;
			sfd->sf_state = SF_READ_ID;
			fds_io(sfd,0);
			return;
		}
	}
	switch(sfd->sf_state) {
	case SF_RECAL:
		if(err) {
			sfd->sf_need_recal = 1;
			fd_cmd_done(sfd,err);
			return;
		}
	case SF_DO_SEEK:
		if(sfd->sf_need_recal) {
			fdu_recal(sfd,fds_seek);
			return;
		}
		if(sfd->sf_new_cylinder != sfd->sf_cylinder) {
			fdu_seeking(sfd,fds_seek);
			return;
		}
		sfd->sf_state = SF_SEEKING;
	case SF_SEEKING:
		if(err)
			sfd->sf_need_recal = 1;
		fds_io(sfd,err);
	}
#endif
}

/*	low level controller level commands
	this routines do one command at a time
	they should be simplified by having a table like cmd_desc to do the job
	then we can reduce them to one routine and a table smart enough
	to do the equivalent.  I just did not have enough time to do so.

	format:
	fdu_xxxx(sfd,reply)
*/

STATIC 
fdu_setup(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register struct pg_flop *p = &sfd->sf_flop.msel_desc.desc_pg;

	sfd->sf_state = SF_SETUP_DONE;
	
	fdreset(sfd,reply,p->flop_xfer);

	switch(p->flop_xfer & 0x3) {
	case 0: /* 16 us per byte */
		fd_io_delay = 4;
		break;
	default:
		fd_io_delay = 6;
		break;
	}
	if(p->flop_mfm == 0)
		fd_io_delay <<= 1;
}

STATIC 
fdu_configure(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register u_char *cmd;
	register struct pg_flop *p = &sfd->sf_flop.msel_desc.desc_pg;
	sfd->sf_state = SF_CONFIGURE_DONE;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_CONFIGURE;
	cmd[1] = sfd->sf_flop.msel_desc.desc_pg.flop_moff << 4 | 
		sfd->sf_flop.msel_desc.desc_pg.flop_mon;
	cmd[1] = 0;
FD_DEBUG(CE_CONT,"fifo depth %d\n",fifo_depth & 0xf);
#ifdef IMPLIED_SEEK
	cmd[2] = FD_DISABLE_POLL|FD_ENABLE_IS|(fifo_depth & 0xf);
#else
	cmd[2] = FD_DISABLE_POLL|(fifo_depth & 0xf);
#endif
	cmd[3] = 0;
	sfd->sf_clen = 4;
	sfd->sf_dlen = sfd->sf_rlen = 0;
	
	fdcmd(sfd,reply);
}

STATIC 
fdu_noop(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register u_char *cmd;
	sfd->sf_state = SF_NOOP;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_NOOP;
	sfd->sf_clen = 1;
	sfd->sf_dlen = 0;
	sfd->sf_rlen = 1;
	
	fdcmd(sfd,reply);
}


STATIC 
fdu_mode_select(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register u_char *cmd;
	register struct pg_flop *p = &sfd->sf_flop.msel_desc.desc_pg;
	sfd->sf_state = SF_MODE_SELECT_DONE;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_SPECIFY;
	cmd[1] = ((u_char) (p->flop_step_rate) << 4) | p->flop_hut;
	cmd[2] = ((u_char) (p->flop_hlt) << 1) | 1; /* always no dma */
	sfd->sf_clen = 3;
	sfd->sf_dlen = sfd->sf_rlen = 0;
	
	fdcmd(sfd,reply);
}

STATIC 
fdu_sense_drive(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register u_char *cmd;
	sfd->sf_state = SF_SENSE_DONE;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_SENSE_DRIVE;
	cmd[1] = 0;
	sfd->sf_clen = 2;
	sfd->sf_dlen = 0;
	sfd->sf_rlen = 1;
	
	fdcmd(sfd,reply);
}

STATIC 
fdu_recal(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register u_char *cmd;
	sfd->sf_state = SF_RECAL;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_RECALIBRATE;
	cmd[1] = 0;
	sfd->sf_clen = 2;
	sfd->sf_dlen = 0;
	sfd->sf_rlen = 2;
	sfd->sf_cylinder = 0;
	sfd->sf_need_recal = 0;
	
	fdcmd(sfd,reply);
}

STATIC 
fdu_rw(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register u_char *cmd;
	register struct pg_flop *p = &sfd->sf_flop.msel_desc.desc_pg;
	sfd->sf_state = SF_RW;
	cmd = sfd->sf_cmd;
	switch(cmd[0] = sfd->sf_io) {
	case FD_READ_DATA:
	case FD_WRITE_DATA:
		cmd[1] = sfd->sf_new_head << 2;;
		cmd[2] = sfd->sf_new_cylinder;
		cmd[3] = sfd->sf_new_head;
		cmd[4] = sfd->sf_new_sector;
		cmd[5] = FD_BTON(p->flop_nbyte);

/*		old implementation sets to total number of sectors on track
		new one sets to the last io sector since terminal count
		does not work.  We expect to get abnormal termination with error
		code end of cylinder.
 		cmd[6] = p->flop_nsec;
*/
		cmd[6] = sfd->sf_new_sector + (sfd->sf_xlen - 1)/sfd->sf_bs;
/* norm gap should be 1b */
		cmd[7] = p->flop_norm_gap;	
		cmd[8] = 0xff;
		sfd->sf_clen = 9;
		break;
	case FD_FORMAT_TRACK:
		cmd[1] = sfd->sf_new_head << 2;;
		cmd[2] = FD_BTON(p->flop_nbyte);
		cmd[3] = p->flop_nsec;

/* gap should be 0x54 for formatting 0x1b for read/write */

		cmd[4] = p->flop_fmt_gap;
		cmd[5] = 0xbd;
		sfd->sf_clen = 6;
		break;
	default:
		(*reply)(sfd,EINVAL);
	}
	if(p->flop_mfm)
		cmd[0] |= FD_MFM;
	sfd->sf_data = sfd->sf_addr;
	sfd->sf_dlen = sfd->sf_xlen;
	sfd->sf_rlen = 7;
	
	fdcmd(sfd,reply);
}

STATIC 
fdu_read_id(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register struct pg_flop *p = &sfd->sf_flop.msel_desc.desc_pg;
	register u_char *cmd;
	sfd->sf_state = SF_READ_ID;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_READ_ID;
	cmd[1] = sfd->sf_new_head << 2;;
	sfd->sf_clen = 2;
	sfd->sf_dlen = 0;
	sfd->sf_rlen = 7;
	if(p->flop_mfm)
		cmd[0] |= FD_MFM;
	
	fdcmd(sfd,reply);
}

STATIC 
fdu_seeking(sfd,reply)
register struct sfd8272 *sfd;
int (*reply)();
{
	register u_char *cmd;

	sfd->sf_state = SF_SEEKING;
	cmd = sfd->sf_cmd;
	cmd[0] = FD_SEEK;
	cmd[1] = sfd->sf_new_head << 2;;
	cmd[2] = sfd->sf_new_cylinder;
	sfd->sf_clen = 3;
	sfd->sf_dlen = 0;
	sfd->sf_rlen = 2;
	sfd->sf_cylinder = sfd->sf_new_cylinder;
	
	fdcmd(sfd,reply);
}

/*	physical layer	*/


/*	low level state machine for floppy controller 8272A
*/


struct fd8272 fd0;

STATIC int fd_cmd_phase(),fd_data_phase(),fd_res_phase();
STATIC int fd_wait_phase(),fd_default_setup();
int (*fd_state_func[FDS_TOTAL])();


STATIC int dummy()
{ }

#define NTRACK 40
#define NSEC 18
#define NHEAD 2
#define NBYTE 512


fdinit()
{
	int s,s1;
	unsigned char tmp,pattern;
	volatile register unsigned char *dor;
	
	if(fdinited == 0) {
		fdinited = 1;
		fd0.fd_status_reg = (unsigned char *)FD_STATUS_REG;
		fd0.fd_data_reg = (unsigned char *)FD_DATA_REG;
		fd0.fd_terminal_count = (unsigned char *)FD_TERMINAL_COUNT;
		dor = fd0.fd_dor = (unsigned char *)FD_DOR;
		fd0.fd_state = FDS_READY;
		fd0.cur_cylinder = -1;
		fd0.fd_flags = 0;
		fd_state_func[FDS_READY] = fd_default_setup;
		fd_state_func[FDS_CMD_PHASE] = fd_cmd_phase;
		fd_state_func[FDS_DATA_PHASE] = fd_data_phase;
		fd_state_func[FDS_RES_PHASE] = fd_res_phase;
		fd_state_func[FDS_WAIT_PHASE] = fd_wait_phase;
/* on power up the floppy will always hold the intr line high and we must
   read the status reg to clear the intr
*/
		tmp = *fd0.fd_status_reg;
		pattern = 0xc;
		*dor = pattern;
		wbflush();
		tmp = *dor;
		if(tmp == pattern) {
			fd0.fd_82077 = 1;
		} else {
			fd0.fd_82077 = 0;
		}
	}
}

flclearintr()
{
	unsigned char tmp;
	fd0.fd_status_reg = (unsigned char *)FD_STATUS_REG;
	tmp = *fd0.fd_status_reg;
}

fdreset(sfd,reply,xfer)
int (*reply)();
register struct sfd8272 *sfd;
{
	register s;
	register struct fd8272 *fd = sfd->sf_fd;
	s = spl_floppy();
	last_xfer = xfer;
	fd_reset(fd,xfer);
	(*reply)(sfd,0);
	splx(s);
}

fdcmd(sfd,reply)
int (*reply)();
register struct sfd8272 *sfd;
{
	register s;
	register struct fd8272 *fd = sfd->sf_fd;
	s = spl_floppy();
	if(fd->fd_state != FDS_READY) {
		splx(s);
		/* reply */
		(*reply)(sfd,EBUSY);
		return;
	}
	fd->fd_command = sfd->sf_cmd; fd->fd_clen = sfd->sf_clen;
	fd->fd_data = sfd->sf_data; fd->fd_dlen = sfd->sf_dlen;
	fd->fd_result = sfd->sf_res; fd->fd_rlen = sfd->sf_rlen;
	fd->fd_reply = reply;
	fd->fd_arg = sfd;
	fd->fd_pad = 0;

	fd_dispatch(fd);
	splx(s);
}

unsigned char configure_cmd[] = {
	FD_CONFIGURE,
	0x65,
#ifdef IMPLIED_SEEK
	FD_DISABLE_POLL|FD_ENABLE_IS|FD_FIFO_DEPTH,
#else
	FD_DISABLE_POLL|FD_FIFO_DEPTH,
#endif
	0
};

unsigned char specify_cmd[] = {
	FD_SPECIFY,
	0xd1,
	3
};

fd_default_setup(fd)
register struct fd8272 *fd;
{
	fd_reset(fd,last_xfer);
	fdcsend(fd,configure_cmd,4);
	fdcsend(fd,specify_cmd,3);
}

flintr()
{
/*	we only support 1 controller for now but this can
	be expanded easily
*/
	if(fdinited == 0) {
		fdinit();
		return;
	}
	(*fd_state_func[fd0.fd_state])(&fd0);

}


STATIC 
fd_wait(fd)
register struct fd8272 *fd;
{
	register i;
	for(i = 0; i < MAX_LOOP; i++)
		if(!(*fd->fd_status_reg & FD_BUSY))
			return;
	if(i >= MAX_LOOP) {
		fd_default_setup(fd);
	}
}

STATIC
fd_dump_cmd(cmd,n)
register unsigned char *cmd;
{
	register int i;
	FD_DEBUG(CE_CONT,"(");
	for(i = 0; i < n; i++)
		FD_DEBUG(CE_CONT,"%x ",(unsigned) *cmd++);
	FD_DEBUG(CE_CONT,")\n");
}

STATIC
fd_dump_res(res,rlen)
register unsigned char *res;
register rlen;
{
	register i;
	FD_DEBUG(CE_CONT,"[");
	for(i = 0; i < rlen; i++)
		FD_DEBUG(CE_CONT,"%x ",(unsigned) *res++);
	FD_DEBUG(CE_CONT,"]\n");
}

/*	this will be called when the floppy timeouts
	it is used for command that should intr but at an indefintie amount
	of time like read, write, readid
	On timeout, we call default_setup to reset the chip to avoid
	further problems.  Note that the values os specify and configure
	commands are saved everything the high level module issues those
	commands.  This ensures that we are using the parameters that the
	user want.
*/

STATIC fd_wakeup(fd)
register struct fd8272 *fd;
{
	register s;

	FD_DEBUG(CE_CONT,"floppy timeout\n");
	s = spl_floppy();
	if(fd->fd_timeout) {
	fd->fd_timeout = 0;
	switch(fd->fd_state) {
	case FDS_READY:
		break;
	case FDS_DATA_PHASE:
	case FDS_RES_PHASE:
	case FDS_WAIT_PHASE:
	default:
		fd_default_setup(fd);
	} 
	}
	splx(s);
}

/* set up the flags field and validate various lengths */

STATIC
fd_setup(fd)
register struct fd8272 *fd;
{
	register i,n;
	register struct fd_cmd *p;
	register unsigned cmd = ((unsigned) fd->fd_command[0]);

	if(cmd != FD_REL_SEEK)
		cmd &= FD_CMD_MASK;

	fd->fd_timeout = 0;
	fd->err = 0;
	n = sizeof(cmd_desc)/sizeof(struct fd_cmd);
	for(i = 0, p = cmd_desc; i < n; i++, p++)
		if(p->fc_cmd == cmd)
			break;
	if(i >= n) {
		return(1);
	} else {
		if(p->fc_clen != fd->fd_clen || p->fc_rlen != fd->fd_rlen) {
			return(1);
		}
		fd->fd_flags = p->fc_flags;
	}
	return(0);
}

STATIC
fd_save(fd)
register struct fd8272 *fd;
{
	register unsigned char *cmd,*p = fd->fd_command;
	register i;
	switch(fd->fd_command[0] & FD_CMD_MASK) {
	case FD_SPECIFY:
		cmd = specify_cmd;
		break;
	case FD_CONFIGURE:
		cmd = configure_cmd;
		break;
	default:
		return;
	}
	i = fd->fd_clen;
	while(i-- > 0)
		*cmd++ = *p++;
}

static char fd_padding[FD_SECTSIZE];

STATIC 
fd_dispatch(fd)
register struct fd8272 *fd;
{
	register unsigned char *cmd = fd->fd_command;
	/* now dispatch the request */
	if(fd_setup(fd)) {
		fd_done(fd,EINVAL);
		return;
	}
	if(fd->fd_flags & FC_TURN_MOTOR_ON) {
		if(fd->fd_82077 && !(fd->fd_sflags & FDS_MOTOR_ON)) {
			fd_turn_motor_on(fd);
			return;
		}
	}
	fd_wait(fd);
	if(fd->fd_flags & FC_SAVE)
		fd_save(fd);
	if(fd->fd_82077 && (fd->fd_flags & FC_PAD)) {
		fd->fd_pad =  (FD_SECTSIZE - (fd->fd_dlen & (FD_SECTSIZE - 1))) & (FD_SECTSIZE - 1);
		if((fd->fd_flags & FC_IO_TO_FD) && (fd->fd_pad > 0))
			bzero(fd_padding,FD_SECTSIZE);
	}
	fd_dump_cmd(cmd,fd->fd_clen);
	fd->fd_state = FDS_CMD_PHASE;
	fd_cmd_phase(fd);
}



STATIC 
fdc_send(fd,p,n)
register n;
register struct fd8272 *fd;
register unsigned char *p;
{
	register unsigned char *status = fd->fd_status_reg;
	volatile register unsigned char *data = fd->fd_data_reg;
	register unsigned char tmp;
	register i;
	for(i = 0; i < 6; i++)
	while(n > 0) {
		tmp = *status;
		if(!(tmp & FD_READY)) {
			DELAY(fd_io_delay)
			break;
		}
		if((tmp & (FD_NON_DMA|FD_TO_CPU|FD_READY)) == (FD_NON_DMA|FD_READY)) {
			*data = *p++;
			wbflush();
/* we have to reset the loop counter here to stay in this loop longer else
   we might get data overrun/underrun errors.
*/
			i = 0;
			n--;
			continue;
		}
/*	when the chip thinks it is in data phase in nondma mode, the FD_NON_DMA
	bit should be set,  hence it must not be in data phase if not set and
	there must be some kind of error
*/
		if(!(tmp & (FD_NON_DMA))) {
FD_DEBUG(CE_CONT,"fdcsend:Unexpected termination %x\n",tmp);
			fd->err = EIO;
			return(0);
		}
		break;
	}
	return(n);
}

STATIC 
fdc_rec(fd,p,n)
register unsigned char *p;
register n;
register struct fd8272 *fd;
{
	register unsigned char *status = fd->fd_status_reg,
		*data = fd->fd_data_reg;
	register unsigned char tmp;
	register i;
	for(i = 0; i < 6; i++)
	while(n > 0) {
		tmp = *status;
		if(!(tmp & FD_READY)) {
			DELAY(fd_io_delay);
			break;
		}
		if((tmp & (FD_NON_DMA|FD_TO_CPU|FD_READY)) 
			== (FD_NON_DMA|FD_TO_CPU|FD_READY)) {
			*p++ = *data;
			n--;
/* we have to reset the loop counter here to stay in this loop longer else
   we might get data overrun/underrun errors.
*/
			i = 0;
			continue;
		} 
		if(!(tmp & (FD_NON_DMA))) {
FD_DEBUG(CE_CONT,"fdc_rec:Unexpected termination %x\n",tmp);
			fd->err = EIO;
			return(0);
		}
		break;
	}
	return(n);
}

/*	this polls until all data are sent because the chip does not intr
	the CPU during result phase or command phase
*/

STATIC 
fdcsend(fd,p,n)
struct fd8272 *fd;
register unsigned char *p;
register n;
{
	register k;
	register unsigned char *status, *data;
	register unsigned char tmp;
	register max_loop = MAX_LOOP;
	status = fd->fd_status_reg;
	data = fd->fd_data_reg;
	while(n > 0) {
		tmp = *status;
		if((tmp & (FD_TO_CPU|FD_READY)) == FD_READY) {
			max_loop = MAX_LOOP;
			*data = *p++; wbflush();
			n--;
		} else if((tmp & (FD_READY|FD_TO_CPU)) == (FD_READY|FD_TO_CPU)) {
FD_DEBUG(CE_CONT,"fdcsend:status %x not eqaul to %x\n",tmp,FD_READY|FD_TO_CPU);
			fd->err = EIO;
			return;
		}
		if(max_loop-- <= 0) {
FD_DEBUG(CE_CONT,"fdcsend:max_loop expires %d\n",MAX_LOOP);
			fd->err = EIO;
			return;
		}
	}
}



STATIC
fd_reset(fd,xfer)
register struct fd8272 *fd;
{
	struct sfd8272 *sfd;

/*	this will reset the controller */
	*fd->fd_status_reg = FD_DR_RESET;

	FD_DELAY;
FD_DEBUG(CE_CONT,"fd_reset %x\n",xfer & 0x1f);
/*	set up data transfer rate
	Note that when writing to the status reg it becomes data rate select
	reg
*/
	*fd->fd_status_reg = xfer & 0x1f;
	if(fd->fd_sflags & FDS_TIMEOUT) {
		untimeout(fd->fd_motor_timeout);
		fd->fd_sflags &= ~(FDS_MOTOR_ON|FDS_TIMEOUT);
	} 
	FD_DELAY;
	if(fd->fd_state != FDS_READY) {
	register i;
	register unsigned char *p;
		fd->fd_state = FDS_READY;
		sfd = fd->fd_arg;
		sfd->sf_need_recal = 1;
		for(i = 0, p = fd->fd_result; i < fd->fd_rlen; i++)
			*p++ = 0xff;
FD_DEBUG(CE_CONT,"fd_reset\n");
		fd_done(fd,EIO);
	}
}

STATIC
fd_standby(fd)
register struct fd8272 *fd;
{
	char tmp = FD_NOOP;;
	if(*fd->fd_status_reg == FD_READY) {
		fdcsend(fd,&tmp,1);
		if(fd->err)
			return;
		fdrec(fd,&tmp,1);
	}
}

STATIC 
fdrec(fd,p,n)
struct fd8272 *fd;
register unsigned char *p;
register n;
{
	register k;
	register unsigned char *status,*data;
	register unsigned char tmp;
	register max_loop = MAX_LOOP;

	status = fd->fd_status_reg;
	data = fd->fd_data_reg;
	while(n > 0) {
		tmp = *status;
		if((tmp & (FD_TO_CPU|FD_READY)) == (FD_TO_CPU|FD_READY)) {
			max_loop = MAX_LOOP;
			*p++ = *data;
			n--;
			continue;
		} 
		if((tmp & (FD_READY|FD_TO_CPU|FD_NON_DMA|FD_BUSY)) == (FD_READY|FD_NON_DMA|FD_BUSY)) {
/*	this has to be done in order to satisfy the 82072 during format cmd
	which demands 16 extra bytes
*/
			max_loop = MAX_LOOP;
			*data = 0;
			continue;
		}
		if((tmp & (FD_READY|FD_TO_CPU|FD_NON_DMA|FD_BUSY)) == (FD_READY)) {
FD_DEBUG(CE_CONT,"fdrec:status %x not equal %x\n",tmp,FD_READY);
			fd->err = EIO;
			return;
		}
		if(max_loop-- <= 0) {
FD_DEBUG(CE_CONT,"fdrec:max %d\n",MAX_LOOP);
			fd->err = EIO;
			return;
		}
	}
}

STATIC 
fderror(fd,err)
register struct fd8272 *fd;
register err;
{
	register s;

	s = spl_floppy();
	fd->fd_state = FDS_READY;
	if(fd->fd_timeout) {
		untimeout(fd->fd_timeout);
		fd->fd_timeout = 0;
	}
	(*fd->fd_reply)(fd->fd_arg,err);
	splx(s);
}

/*	delay reply to release the cpu from high spl
	this should be schedule immediately since we use timeout(,,1)
*/

STATIC
fd_delay_res(fd)
register struct fd8272 *fd;
{
	register s;
	s = spl_floppy();
	fd_res_phase(fd);
	splx(s);
}

STATIC
fd_cmd_phase(fd)
register struct fd8272 *fd;
{
	register unsigned flags = fd->fd_flags;
	if(fd->fd_state != FDS_CMD_PHASE) {
		fd_terminate(fd);
		return;
	}
	fdcsend(fd,fd->fd_command,fd->fd_clen);
	if(fd->err) {
		fd_terminate(fd);
		return;
	}
	if(flags & FC_DATA_PHASE) {
		fd->fd_state = FDS_DATA_PHASE;
		fd_timeout(fd,FD_TIMEOUT*HZ);
		if(flags & FC_FORMAT) {
			fd_data_phase(fd);
		}
	} else if(flags & FC_WAIT_PHASE) {
		fd_timeout(fd,FD_TIMEOUT*HZ);
		fd->fd_state = FDS_WAIT_PHASE;
	} else if(flags & FC_RES_PHASE) {
		fd->fd_state = FDS_RES_PHASE;
		fd_res_phase(fd);
	} else if(flags & FC_EXEC_PHASE) {
		fd->fd_state = FDS_RES_PHASE;
		fd_timeout(fd,FD_TIMEOUT*HZ);
	} else if(flags & FC_WAIT_RES_PHASE) {
		fd->fd_state = FDS_RES_PHASE;
		timeout(fd_delay_res,fd,4*HZ);
	} else {
/* could be a specify command */
		fd_done(fd,0);
	}
}

fd_terminate(fd)
register struct fd8272 *fd;
{
	if(!fd->fd_82077)
		*fd->fd_terminal_count = 1;
	wbflush();
	fd->fd_state = FDS_RES_PHASE;
	fd_res_phase(fd);
}

#ifdef FD_DEBUG1
int fd_extra = 0;
#endif

 
STATIC 
fd_data_phase(fd)
register struct fd8272 *fd;
{
	register n,k;
	register unsigned flags = fd->fd_flags;
	if(fd->fd_state != FDS_DATA_PHASE) {
		return;
	}

again:
	if(fd->fd_dlen <= 0) {
		if(!fd->err && fd->fd_pad > 0) {
			fd->fd_data = &fd_padding[0];
			fd->fd_dlen = fd->fd_pad;
			fd->fd_pad = 0;
			goto more;
		}
		if(!fd->err && fd->fd_flags & FC_TERMINATE) {
			if(!fd->fd_82077)
				*fd->fd_terminal_count = 1;
			wbflush();
		}
#ifdef FD_DEBUG1
		fd_extra = 0;
#endif
		fd->fd_state = FDS_RES_PHASE;
		if(fd->err) {
			fd_res_phase(fd);
		}
		return;
	} 
more:
	n = fd->fd_dlen;
	if(flags & FC_IO_FROM_FD)
		k = fdc_rec(fd,fd->fd_data,n);
	else
		k = fdc_send(fd,fd->fd_data,n);
	fd->fd_data += n - k;
	fd->fd_dlen = k;
	if(k < n) goto again;
}


STATIC 
fd_res_phase(fd)
register struct fd8272 *fd;
{
	register i = 0,j;
	unsigned char tmp;
	if(fd->fd_state != FDS_RES_PHASE) {
		return;
	}
#ifdef FD_DEBUG1
for(j = 0; j < 60; j++) {
		tmp = *fd->fd_status_reg;
		if((tmp & (FD_NON_DMA|FD_TO_CPU|FD_READY)) 
			== (FD_NON_DMA|FD_TO_CPU|FD_READY)) {
			i++;
			if(fd_extra < 1024) {
				fd_extra++;
				tmp = *fd->fd_data_reg;
			} else {
				i = 0;
				break;
			}
		}
		FD_DELAY;
	}
	if(i > 0) return;
#endif

/* we have to do the padding here for foramt or ... */

	while(*(fd->fd_status_reg) & FD_NON_DMA) {
		tmp = *fd->fd_status_reg;
		if((tmp & (FD_NON_DMA|FD_TO_CPU|FD_READY)) 
			== (FD_NON_DMA|FD_TO_CPU|FD_READY)) {
			tmp = *fd->fd_data_reg;
		} else if((tmp & (FD_NON_DMA|FD_TO_CPU|FD_READY)) 
			== (FD_NON_DMA|FD_READY)) {
			*fd->fd_data_reg = 0;
		}
		FD_DELAY;
		if(i++ > 512) {
		FD_DEBUG(CE_CONT,"floppy drive is still in command mode\n");
			break;
		}
	}
	fdrec(fd,fd->fd_result,fd->fd_rlen);
	if(fd->err)
		fd_default_setup(fd);
	else {
fd_dump_res(fd->fd_result,fd->fd_rlen);
	fd_done(fd,0);
	}
}

int fd_delay_call();

STATIC 
fd_done(fd,err)
register struct fd8272 *fd;
register err;
{
	fd->fd_state = FDS_READY;
	if(fd->fd_timeout) {
		untimeout(fd->fd_timeout);
		fd->fd_timeout = 0;
	}
	if(!err) {
		if(!fd->err)
			fd_check_error(fd);
		err = fd->err;
	} else
		fd->err = err;

/*	delay call - scheduled immediately */

	if(fd->fd_82077 && (fd->fd_sflags & FDS_MOTOR_ON)) {
		fd_turn_motor_off(fd);
	}
	timeout(fd_delay_call,fd,1);
/*
	(*fd->fd_reply)(fd->fd_arg,err);
*/
}

STATIC
fd_delay_call(fd)
register struct fd8272 *fd;
{
	register s;
	s = spl_floppy();
	(*fd->fd_reply)(fd->fd_arg,fd->err);
	splx(s);
}

STATIC
fd_check_error(fd)
register struct fd8272 *fd;
{
	register unsigned char *res = fd->fd_result;
	unsigned flags = fd->fd_flags;
	if(fd->err) return;
	if(flags & FC_REG0 && (fd->err = fd_check_reg0(res[0])))
		return;
	if(flags & FC_REG1 && (fd->err = fd_check_reg1(res[1])))
		return;
	if(flags & FC_REG2 && (fd->err = fd_check_reg2(res[2])))
		return;
	if(flags & FC_REG3 && (fd->err = fd_check_reg3(res[3])))
		return;
}

STATIC
fd_check_reg0(r)
register char r;
{
	if(r & (FD_R0_CHECK|FD_R0_NOT_READY)) {
		return(EIO);
	}
	return(0);
}

STATIC
fd_check_reg1(r)
register char r;
{
	if(r & (FD_R1_NOT_WRITABLE|FD_R1_MISS_ADDR|FD_R1_NO_DATA|FD_R1_OVERRUN|FD_R1_CRC)) {
		return(EIO);
	}
	return(0);
}

STATIC
fd_check_reg2(r)
register char r;
{
	if(r & (FD_R2_DEL_MARK|FD_R2_MISS_ADDR|FD_R2_BAD_CYL|FD_R2_WRONG_CYL|FD_R2_CRC)) {
		return(EIO);
	}
	return(0);
}

STATIC
fd_check_reg3(r)
register char r;
{
	return(0);
}

/*	this should be called to start the command timeout.
	fd_timeout should be cleared for each new command and when the command
	is done or timeouted.
*/

STATIC
fd_timeout(fd,n)
register struct fd8272 *fd;
int n;
{
	if(!fd->fd_timeout) {
		fd->fd_timeout = timeout(fd_wakeup,fd,n);
	}
}

/*	this is only used for seek related commands to do an implied sense
	intr
*/

STATIC 
fd_wait_phase(fd)
register struct fd8272 *fd;
{
	char sense_intr = FD_SENSE_INTR;

	fdcsend(fd,&sense_intr,1);
	if(fd->err) {
		fd_terminate(fd);
	} else {
		fd->fd_state = FDS_RES_PHASE;
		fd_res_phase(fd);
	}
}


STATIC 
fd_motor_is_on(fd)
register struct fd8272 *fd;
{
	register s;
	s = spl_floppy();
	fd->fd_sflags &= ~FDS_MOTOR;
	fd->fd_sflags |= FDS_MOTOR_ON;
	fd->fd_state = FDS_READY;
	fd_dispatch(fd);
	splx(s);
}

STATIC 
fd_turn_motor_on(fd)
register struct fd8272 *fd;
{
	if(fd->fd_sflags & (FDS_MOTOR | FDS_MOTOR_ON)) {
		if(fd->fd_sflags & FDS_TIMEOUT) {
			untimeout(fd->fd_motor_timeout);
		} 
		return;
	}
	fd_motor_on(fd);
	fd->fd_sflags |= FDS_MOTOR;
	fd->fd_state = FDS_WAIT_MOTOR;
	timeout(fd_motor_is_on,fd,2*HZ);
}

STATIC 
fd_motor_on(fd)
register struct fd8272 *fd;
{
	volatile register unsigned char *dor = fd->fd_dor;

	*dor = 0x1c;
	wbflush();
}

STATIC 
fd_motor_off(fd)
register struct fd8272 *fd;
{
	volatile register unsigned char *dor = fd->fd_dor;
	register s;

	s = spl_floppy();
	if(fd->fd_state != FDS_READY) {
		splx(s);
		return;
	}
	*dor = 0xc;
	wbflush();
	fd->fd_sflags &= ~(FDS_MOTOR_ON|FDS_TIMEOUT);
	splx(s);
}

STATIC
fd_turn_motor_off(fd)
register struct fd8272 *fd;
{
	if(!(fd->fd_sflags & FDS_MOTOR_ON)) return;
	if(fd->fd_sflags & FDS_TIMEOUT) {
		untimeout(fd->fd_motor_timeout);
	} 
	fd->fd_motor_timeout = timeout(fd_motor_off,fd,SF_TIMEOUT_LENGTH);
	fd->fd_sflags |= FDS_TIMEOUT;
}

