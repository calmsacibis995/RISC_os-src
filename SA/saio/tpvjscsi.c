#ident "$Header: tpvjscsi.c,v 1.13 90/03/22 13:27:21 srinath Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
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

/* #define DEBUG 1*/
/***
 ***      tpvjscsi.c    1.0     88/02/18        Standalone tape driver for sash
 ***
 ***  NOTE: Some of the functions are defined in dkvjscsi.c
 ***
 *** Copyright (C) Interphase Corporation - Dallas, TX 75229
 ***        Author: Keith Wiles, Enoch Suen
 ***        Date:    June 10, 1987
 ***
 ***
 ***/
#include "sys/errno.h"
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/buf.h"
#include "sys/dir.h"
#include "machine/cpu.h"
#include "mipsvme/vmereg.h"
#include "machine/dvh.h"
#include "mipsvme/dkvj_IPtypes.h"
#include "mipsvme/dkvj_struct.h"
#include "mipsvme/dkvj_scsi.h"
#include "mipsvme/dkvj_reg.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "sys/cmn_err.h"


extern int Nvjctlr;
extern VJ_CTLR *vjctlrs[];
extern char *vj_tpbuf;
extern int vj_didinit[];
extern int vjerror();
extern csh_type ctlr_csh[];

/*
 * Determine existence of controller
 */
_tpvjinit()
{
}

_tpvjopen(io)
register struct iob *io;
{
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    register struct tp_dir *tpd;
    register int unit = io->i_unit;
    register int ctlr = io->i_ctlr;
    register int part = io->i_part;
    u_int count;
    sah_type temp_sph;
    ioaddr_t io_addr;

    if (ctlr >= Nvjctlr)
	goto badio;
    if( !vj_didinit[ctlr] ){
	dkvjinit(io);
	vj_didinit[ctlr] = 1;
    }

    c = vjctlrs[ctlr];
    un = c->c_unit[unit];

    if ( un == 0 ) {
        vjattach( io );
	un = c->c_unit[unit];
    }

    if (!un) {
        printf("tqijopen: no device\n");
	io->i_errno = ENODEV;
	return (-1);
    }
    if (!(un->un_flags & IVJ_TAPE)) {
        printf("tqijopen: not a tape device\n");
	io->i_errno = ENXIO;
	return (-1);
    }
    if (un->un_flags & IVJ_OPEN) {
	io->i_errno = ENXIO;	 /* already opened */
	return (-1);
    }
    vjcmd(un, VJ_REWIND, 0, 0, 0, NO_INTERRUPT);
    if (vjwait(c, M_CRSW_CC)) {
        printf("tqijopen: tape rewinding error\n");
	goto badio;
    }
    clear_page_cache(un, sizeof(VJ_UNIT));
#ifndef PROM
    vjcmd(un, VJ_RDBLKLIM_CMD, 0, 0, 0, NO_INTERRUPT);
    if (vjwait(c, M_CRSW_CC)) {
        printf("tqijopen: read block limit error\n");
	goto badio;
    }
    vjcmd(un, VJ_MODE_SENSE_CMD, 0, 0, 0, NO_INTERRUPT);
    if (vjwait(c, M_CRSW_CC)) {
        printf("tqijopen: mode sense error\n");
	goto badio;
    }
    if (un->un_msense.hdr.WP) {
	if (!(io->i_flgs & F_READ)) {
	    printf("tqijopen: tape %d write protected\n",un->un_slave);
	    goto badio;
	}
	un->un_flags |= IVJ_READONLY;
    }
    else
	un->un_flags &= ~IVJ_READONLY;
    if (un->un_blklim.maxlen != un->un_blklim.minlen) {
	un->un_msense.hdr.sense_data_len = 0;
	un->un_msense.hdr.medium_type = 0;
	un->un_msense.hdr.WP = 0;
	un->un_msense.hdr.blk_desc_len = 8;
	un->un_msense.blk_desc.density_code = 0;
	un->un_msense.blk_desc.nrblks = 0;
	un->un_msense.blk_desc.blk_len = 0;
	vjcmd(un, VJ_MODE_SELECT_CMD, 0, 0, 0, NO_INTERRUPT);
	if (vjwait(c, M_CRSW_CC)) {
	    printf("tqijopen: mode select error\n");
	    goto badio;
	}
	un->un_flags |= IVJ_VARIABLE;
    }
#endif PROM
    if(part)
    {
        count = part;
	vjcmd(un, VJ_SPACE, 0, SP_FILEMARK, count, NO_INTERRUPT);
	if (vjwait(c, M_CRSW_CC))
		printf("tqijopen: tape spacing files error\n");
    }
    if (io->i_flgs & F_READ) {

	if(!vme_iomap(ctlr_csh[un->un_ctlr], vj_tpbuf, TP_BLKSIZ, 
	GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr)) 
	  cmn_err(CE_PANIC, "Can't map TP structure !\n");

        clear_page_cache(vj_tpbuf, TP_BLKSIZ);

	un->un_xfer = TP_BLKSIZ;
	un->un_resid = 0;
	if (un->un_flags & IVJ_VARIABLE)
		count = TP_BLKSIZ;
	else
		count = TP_BLKSIZ >> DEV_BSHIFT;
	vjcmd(un, VJ_READ, io_addr, 0, count, NO_INTERRUPT);

	if (vjwait(c, M_CRSW_CC))
	    printf("tqijopen: tape read error\n");
	
	clear_page_cache(vj_tpbuf, TP_BLKSIZ);
	
	  if(!vme_iounmap(temp_sph))
	   cmn_err(CE_PANIC,"Can't unmap TP buffer !\n");


	un->un_xfer -= un->un_resid;
	tpd = (struct tp_dir *)io->i_fs_tape;
	bcopy(vj_tpbuf, tpd, sizeof(struct tp_dir));
	if (io->i_fstype != DTFS_NONE) {
	    switch (io->i_fstype) {
		case DTFS_AUTO:
		    if (!is_tpd (tpd)) {
			io->i_fstype = DTFS_NONE;
			break;
		    }
		    else {
			/*
			 * If more than one type of volume header is ever 
			 * possible we would then need to do some mapping
			 * like vh_mapfstype();
			 */
			io->i_fstype = DTFS_TPD;
			    break;
		}

		case DTFS_TPD:
		    if (!is_tpd (tpd)) {
			printf ("bad volume header\n");
			goto badio;
		    }
	    }
	}
    }
    /*
     * open successful
     */
    un->un_curfile = 0;
    un->un_lastiow = 0;
    un->un_nxtrec = 0;
    return (0);

badio:
    io->i_errno = EIO;
    return (-1);
}

_tpvjclose(io)
register struct iob *io;
{
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    register int count;

    c = vjctlrs[io->i_ctlr];
    un = c->c_unit[io->i_unit];

    if ((io->i_flgs & F_WRITE) || ((io->i_flgs & F_WRITE) && un->un_lastiow))
    {				/* write file marks */
	if (un->un_flags & IVJ_VARIABLE)
		count = 2;
	else
		count = 1;
	vjcmd(un, VJ_W_FM, 0, 0, count, NO_INTERRUPT);
    	if (vjwait(c, M_CRSW_CC))
		printf("tqijclose: Error in writing file marks\n");

	if (un->un_flags & IVJ_VARIABLE) {
		vjcmd(un, VJ_SPACE, 0, SP_FILEMARK, ~1L + 1, NO_INTERRUPT);
		if (vjwait(c, M_CRSW_CC))
				printf("tqijclose: Error in bsf\n");
	}
    }

    vjcmd(un, VJ_REWIND, 0, 0, 0, NO_INTERRUPT);
    if (vjwait(c, M_CRSW_CC))
	printf("tqijclose: Error in tape rewinding.\n");
    un->un_lastiow = 0;
    un->un_flags &= ~(IVJ_OPEN|IVJ_WRITTEN|IVJ_READ|IVJ_FM|IVJ_RFM|IVJ_EOM|
			IVJ_ATN|IVJ_NOT_RDY|IVJ_VARIABLE);
    return (0);
}

#define BLOCKS(x)	(((x) + (DEV_BSIZE-1)) >> DEV_BSHIFT)
_tpvjstrategy(io, func)
register struct iob *io;
register int func;
{
    register VJ_UNIT *un;
    register VJ_CTLR *c;
    register struct tp_dir *tpd;
    daddr_t blkno;
    int repcnt, off, newblk, xfer;
    int blks, status = 0;
    int ctlr = io->i_ctlr;
    int unit = io->i_unit;
    u_int count, i;
    sah_type temp_sph;
    ioaddr_t io_addr;

    c = vjctlrs[ctlr];
    un = c->c_unit[unit];

    un->un_lastiow = 0;
    /*
     * If the request is for block zero, func equals a read
     * and we have a valid tape volume header just return it.
     * (Normally called from inside the file systems open routine)
     */
    if (func == READ) {
	tpd = (struct tp_dir *)io->i_fs_tape;
	if (io->i_bn == 0 && (tpd->td_magic == TP_MAGIC) &&
	    io->i_cc == sizeof (struct tp_dir)) {
		bcopy (tpd, io->i_ma, io->i_cc);
		return (io->i_cc);
	}
    }
    /*
     * Normally this flag is set in the open routine.  In this
     * environment it's not safe to set it there.  If you're
     * opening a tape with a file system and you specifiy a name
     * to use.  You could pass the tape controllers open only to
     * fail inside the file systems open routine. (which calls
     * strategy). You would then
     * be left with this flag set and no way to reopen the correct
     * file unless you rebooted or restarted the standalone program.
     * When you have reached this point in _tpqicstrategy I know
     * that you have completed the open.
     */
    un->un_flags |= IVJ_OPEN;
    blks = BLOCKS(io->i_cc);
    newblk = un->un_nxtrec + BLOCKS(un->un_xfer);

    if (func == WRITE) {
	    if (io->i_bn != un->un_nxtrec)
		    return (-1);

	    un->un_resid = 0;
	    un->un_xfer = blks * 512;
	    if (un->un_flags & IVJ_VARIABLE)
		count = un->un_xfer;
	    else
		count = blks;

	    if(!vme_iomap(ctlr_csh[un->un_ctlr], io->i_ma,
	    io->i_cc, GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr))
	       cmn_err(CE_PANIC, "Can't map i_ma  structure !\n");

	    clear_page_cache(io->i_ma ,io->i_cc);

	    vjcmd(un, VJ_WRITE, io_addr, 0, count, NO_INTERRUPT);

	    if (status = vjwait(c, M_CRSW_CC))
		    printf("tqijstrategy: write error.\n");
            
	    clear_page_cache(io->i_ma, io->i_cc);
	    
	       if(!vme_iounmap( temp_sph ))
	       cmn_err(CE_PANIC, "Can't flush/unmap i_ma !\n");


	    un->un_xfer -= un->un_resid;
	    un->un_nxtrec += blks;
    }
    else {
	if (un->un_xfer && (io->i_bn >= un->un_nxtrec) && (io->i_bn < newblk))
	    goto haveit;
	if (un->un_flags & IVJ_RFM)
	    return(0);
	if (io->i_bn > newblk) {
	    count = io->i_bn - newblk;
	    vjcmd(un, VJ_SPACE, 0, SP_BLOCK,count, NO_INTERRUPT);
	    if (status = vjwait(c, M_CRSW_CC))
		    printf("tqijstrategy: space block error.\n");
	} else if (io->i_bn < un->un_nxtrec) {
	    count = un->un_nxtrec - io->i_bn;
	    count = ~count + 1; /* two's complement */
	    vjcmd(un, VJ_SPACE, 0, SP_BLOCK,count, NO_INTERRUPT);
	    if (status = vjwait(c, M_CRSW_CC))
		    printf("tqijstrategy: space block error.\n");
	}
	un->un_nxtrec = io->i_bn;
	if (status) {
	    printf("tqij(%d,%d,%d): SPACE cmd error\n",
			    ctlr,unit,io->i_part);
	    goto badio;
	}
	if (un->un_flags & IVJ_VARIABLE) {
	    blks = BLOCKS(io->i_cc);
	    count = blks * 512;
	    if (count > TP_BLKSIZ) {
		printf("tqij(%d,%d,%d): Block size too big\n",
			    ctlr,unit,io->i_part);
		goto badio;
	    }
	} else {
	    blks = BLOCKS(TP_BLKSIZ);
	    count = blks;
	}

	un->un_xfer = blks * 512;
	un->un_resid = 0;

	if(!vme_iomap(ctlr_csh[un->un_ctlr], vj_tpbuf, un->un_xfer,
	GBA_CONTIG_ADDR+GBA_NOPART_MAP, &temp_sph, &io_addr)) 
	  cmn_err(CE_PANIC, "Can't map TP structure !\n");

        clear_page_cache(vj_tpbuf, un->un_xfer);

	vjcmd(un, VJ_READ, io_addr, 0, count, NO_INTERRUPT);

	if (status = vjwait(c, M_CRSW_CC))
	{
	    printf("tqijstrategy: read block error.\n");
	    goto badio;
	}
	
	clear_page_cache(vj_tpbuf, un->un_xfer);

	  if(!vme_iounmap(temp_sph))
	   cmn_err(CE_PANIC,"Can't unmap TP buffer !\n");


	un->un_xfer -= un->un_resid;
haveit:
	/*
	 * Using i_bn here because i_offset is valid for a file
	 * not the tape_file. It's the file systems job to worry
	 * about file offsets.
	 */
	off = (io->i_bn - un->un_nxtrec) << DEV_BSHIFT;
	xfer = _min((un->un_xfer - off), io->i_cc);
	bcopy (vj_tpbuf+off, io->i_ma, xfer);
	un->un_resid = io->i_cc - xfer;
    }
    return (io->i_cc - un->un_resid);

badio:
    if (status) {
	printf("tqij(%d,%d,%d): ",ctlr,unit,io->i_part);
	vjerror(status);
    }
    io->i_errno = EIO;
    return (-1);
}

/*
 * ioctl routine
 *	See the vjsplxxxxx routines for support in ioctl handling
 */
_tpvjioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
    io->i_errno = EINVAL;
    return(-1);
}
