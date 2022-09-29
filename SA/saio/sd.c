#ident "$Header: sd.c,v 1.2 90/01/16 17:52:46 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * standalone disk driver for sable virtual disk device.
 */

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "machine/cpu.h"
#include "mipsvme/sdreg.h"
#include "machine/dvh.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

#define	volatile

#define	NUNITS	1

static struct sddevice *sdstd[] = {
	(struct sddevice *)SDISK_BASE
};
#define	NCTLRS	(sizeof(sdstd)/sizeof(sdstd[0]))

static char *sd_name = "SABLE pseudo disk";

static struct volume_header sdvh[NCTLRS][NUNITS];
static int vhbuf[DEV_BSIZE/sizeof(int)];

_sdinit()
{
	bzero(sdvh, sizeof(sdvh));
}

_sdopen(io)
register struct iob *io;
{
	register volatile struct sddevice *sd;
	register struct volume_header *sdv;
	register part;

	if (io->i_unit >= NUNITS) {
		printf("sd bad unit\n");
		goto bad;
	}
	/*
	 * No support for negative controllers (which are explicit csr's)
	 * in sable disk
	 */
	if ((unsigned)io->i_ctlr >= NCTLRS) {
		printf("sd bad controller number\n");
		goto bad;
	}
	if (io->i_part < 0 || io->i_part > NPARTAB) {
		printf("sd bad partition\n");
		goto bad;
	}
	sd = sdstd[io->i_ctlr];
	if (badaddr(&sd->sd_status, sizeof(sd->sd_status))) {
		printf("no sable controller at 0x%x\n", sd);
		goto bad;
	}
	io->i_devaddr = (unsigned)sd;
	sdv = &sdvh[io->i_ctlr][io->i_unit];
	if (sdv->vh_magic == VHMAGIC)
		goto checkfs;
	/*
	 * Read block 0 to obtain the volume header
	 */
	io->i_ma = (char *)vhbuf;
	io->i_cc = DEV_BSIZE;
	io->i_bn = 0;
	part = io->i_part;
	io->i_part = -1;
	_sdstrategy(io, READ);
	io->i_part = part;
	if (!is_vh((struct volume_header *)vhbuf)) {
		printf("can't read volume header\n");
		return(0);
	}
	*sdv = *(struct volume_header *)vhbuf;
checkfs:
	if (sdv->vh_pt[io->i_part].pt_nblks == 0) {
		printf("sd null partition\n");
		goto bad;
	}
	if (io->i_fstype == DTFS_AUTO)
		io->i_fstype = vh_mapfstype(sdv->vh_pt[io->i_part].pt_type);
	return (0);

bad:
	io->i_errno = ENXIO;
	return(-1);
}

_sdstrategy(io, func)
register struct iob *io;
int func;
{
	register volatile struct sddevice *sd;
	register struct partition_table *pt;
	unsigned pbn, sz, offset;
	int npf, i;

	sd = (struct sddevice *)io->i_devaddr;
	pt = &sdvh[io->i_ctlr][io->i_unit].vh_pt[io->i_part];

	/* calc number of 512byte blocks in transfer */
	sz = (io->i_cc + DEV_BSIZE - 1) / DEV_BSIZE;

	/*
	 * minor kludge for reading vol header:
	 * if partition is negative, i_bn is physical block number
	 * and need not be relocated via partition table
	 */
	if (io->i_part < 0)
		pbn = io->i_bn;
	else {
		/* make sure the request is contained in the partition */
		if ((unsigned)io->i_bn > pt->pt_nblks) {
			printf("read beyond end of partition\n");
			io->i_errno = ENXIO;
			return(-1);
		}
		pbn = io->i_bn + pt->pt_firstlbn;
	}

	/* calc number of 4k pages in transfer */
	offset = (int)io->i_ma & PGOFSET;
	npf = (io->i_cc + offset + SD_PGOFFSET) >> SD_PGSHIFT;
	if (npf > SD_NMAP)
		goto bad;

	/* fill in the disk controller registers */
	sd->sd_blocknumber = pbn;
	sd->sd_byteoffset = offset;
	sd->sd_bytecount = io->i_cc;
	for (i=0; i<npf; i++)
		sd->sd_map[i] = (unsigned)K1_TO_PHYS(io->i_ma) + i*NBPG;

	sd->sd_command = func == READ ? SD_READ : SD_WRITE;
	if (sd->sd_status == SD_ERR)
		goto bad;
	return(io->i_cc);
bad:
	printf("sable disk error\n");
	return(-1);
}

_sdioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
	register error = 0;
	register struct volume_header *sdv;

	sdv = &sdvh[io->i_ctlr][io->i_unit];

	switch (cmd) {
		
	case DIOCGETVH:
		bcopy(sdv, arg, sizeof(*sdv));
		break;

	case DIOCSETVH:
		bcopy(arg, sdv, sizeof(*sdv));
		break;

	case DIOCGETCTLR:
		/*
		 * fill-in and return a controller capability struct
		 */
		strncpy(((struct ctlr_info *)arg)->ci_type,sd_name,CITYPESIZE);
		((struct ctlr_info *)arg)->ci_flags = 0;
		break;

	default:
		io->i_errno = EINVAL;
		error = -1;
		break;
	}
	return(error);
}
