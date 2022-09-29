#ident "$Header: dvh.c,v 1.4 90/05/24 10:36:08 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
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
 * dvh.c -- disk volume header routines
 */

#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "sys/errno.h"
#include "mips/dvh.h"
#include "mips/cpu_board.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

#define cvd(x)	((struct volume_directory *)(x->i_ino_dir))

/*
** This routine is used to temporarily resolved 
** the common SCSI disk IOCTL interface promblems.
** It is called by _dvhopen and kernel_name now.
*/
Is_Internal_SCSI(DeviceName)
char *DeviceName;
{
if (strncmp("dkis", DeviceName, 4) == 0) /* M120/M180 Internal SCSI */	
	return(1);
else if (strncmp("tqis", DeviceName, 4) == 0)
	return(1);
else if (strncmp("dksd", DeviceName, 4) == 0) /* Pizazz/Genesis Internal SCSI*/
	return(1);
else if (strncmp("tqsd", DeviceName, 4) == 0)
	return(1);
else
	return(0);
}

_dvhinit()
{
	if (sizeof (struct volume_directory) > IOB_INODE)
		_io_abort ("bad size in iob for dvh");
}

/*
 * _dvhopen -- lookup file in volume header directory
 */
_dvhopen(io, file, flags)
register struct iob *io;
char *file;
int flags;
{
	register struct volume_directory *vd;
	struct io_arg io_arg;
	struct volume_header vh;
	extern char *_get_iobbuf();

	/*
	 * Get volume directory from device driver
	 */
	if (IS_COMMON_SCSI && Is_Internal_SCSI(io->i_dp->dt_string)) {
		io_arg.memaddr = (unsigned long)&vh;
		io_arg.datasz = (unsigned long)sizeof(vh);
		(*io->i_dp->dt_ioctl)(io, DIOCGETVH, &io_arg);
	} else 
		(*io->i_dp->dt_ioctl)(io, DIOCGETVH, &vh);
	/*
	 * Search the volume directory for the requested file
	 */
	cvd(io)->vd_nbytes = 0;
	io->i_bn = -1;
	/*
	 * checking lbn != -1 is crock for dvhtool
	 */
	for (vd = vh.vh_vd; vd < &vh.vh_vd[NVDIR]; vd++) {
		if (strncmp(file, vd->vd_name, VDNAMESIZE) == 0
		    && vd->vd_lbn != -1) {
			*cvd(io) = *vd;
			/*
			 * Allocate a buffer for block io
			 */
			io->i_buf = _get_iobbuf();
			return(0);
		}
	}

	printf("%s not found, known files are:\n", file);
	for (vd = vh.vh_vd; vd < &vh.vh_vd[NVDIR]; vd++) {
		if (vd->vd_nbytes && vd->vd_lbn != -1)
			printf("\t%s\t%d bytes\n", vd->vd_name, vd->vd_nbytes);
	}
	return(-1);
}

/*
 * _dvhread -- read from a volume header file
 */
_dvhread(io, buf, cnt)
register struct iob *io;
char *buf;
int cnt;
{
	int bn, cc, ocnt, off, bcnt;

	/*
	 * Make sure we don't read past end of file
	 */
	if (io->i_offset + cnt > cvd(io)->vd_nbytes) {
		cnt = cvd(io)->vd_nbytes - io->i_offset;
		if (cnt < 0)
			cnt = 0;
	}
	ocnt = cnt;
	while (cnt) {
		/*
		 * get block containing current offset in buffer if necessary
		 */
		bn = (io->i_offset / DEV_BSIZE) + cvd(io)->vd_lbn;
		if (io->i_bn != bn) {
			io->i_bn = bn;
			io->i_ma = io->i_buf;
			io->i_cc = DEV_BSIZE;
			cc = (*io->i_dp->dt_strategy)(io, READ);
			if (cc != DEV_BSIZE) {
				printf("error on dvhread\n");
				io->i_errno = EIO;
				return(-1);
			}
		}
		/*
		 * copy appropriate bytes for current buffer
		 * update offset and cnt
		 */
		off = io->i_offset % DEV_BSIZE;
		bcnt = _min(DEV_BSIZE - off, cnt);
		bcopy(&io->i_buf[off], buf, bcnt);
		io->i_offset += bcnt;
		buf += bcnt;
		cnt -= bcnt;
	}
	return(ocnt);
}

/*
 * _dvhclose -- free the buffer
 */
_dvhclose(io)
struct iob *io;
{
	_free_iobbuf(io->i_buf);
}

/*
 * is_vh -- decide if we're looking at a reasonable volume_header
 */
is_vh(vhp)
struct volume_header *vhp;
{
	register csum;
	register int *ip;

	if (vhp->vh_magic != VHMAGIC)
		return(0);

	csum = 0;
	for (ip = (int *)vhp; ip < (int *)(vhp + 1); ip++)
		csum += *ip;
	return(csum == 0);
}

/*
 * vh_mapfstype -- map volume header partition type to fs type
 */
vh_mapfstype(pt_type)
{
	int newtype;

	newtype = DTFS_AUTO;

	switch (pt_type) {
	case PTYPE_VOLHDR:
		newtype = DTFS_DVH;
		break;

	case PTYPE_BSD:
		newtype = DTFS_BSD42;
		break;
	
	case PTYPE_SYSV:
		newtype = DTFS_SYSV;
		break;
	}
	return(newtype);
}
