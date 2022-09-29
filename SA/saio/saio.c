#ident "$Header: saio.c,v 1.29 90/08/03 09:40:43 chungc Exp $"
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
 * saio.c -- standalone io system
 * Mimics UNIX io as much as possible.
 */

#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "sys/file.h"
#include "sys/errno.h"
#include "netinet/in.h"
#include "machine/dvh.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/ioa.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/setjmp.h"
#include "saio/ctype.h"
#include "saio/debug.h"

#ifdef PROM
#define	NIOB		7		/* max number of open files */
#define	NBUF		3		/* buffers for iob's */
#else
#define	NIOB		10		/* max number of open files */
#define	NBUF		4		/* buffers for iob's */
#endif

#define	CTRL(x)		('x'&0x1f)
#define	INTR		CTRL(C)

int errno;

int machine_type;	/* Used to inform software how to run on this machine */

int malloc_start;	/* Beginning of malloc region for sanity checks */
int malloc_end;		/* End of malloc region for sanity checks */
int malloc_ptr;		/* Current pointer into malloc region */

extern int IDPROM_ADDR[];		/* See saio/machaddr.c for */
extern int LED_REG[];			/* these definitions	   */
extern (*get_tod[])();			/* Pointer to machdep get_tod */

extern struct device_table *_device_table[];
extern struct fs_table _fs_table[];
extern int _ndevices[];
extern int _nfstypes[];

extern u_int ioa_ctlspace_vaddr[];

extern char *index();
extern char *atob();
extern char *getenv();
static char *parsefile();

struct iob _ioblock[NIOB+1];	/* allocate space for 1 more for alignment */	
struct iob *_iob[NIOB];
static struct iob_buf {
	int ib_flags;
#ifdef PROM
	char ib_buf[DEV_BSIZE+64];
#else
	char ib_buf[MAXBSIZE+64];
#endif
} _iob_buf[NBUF];

#define	IB_INUSE	1		/* buffer is in use */

static struct iob *new_iob();
static struct iob *get_iob();

int Debug;
int Verbose;
int ignore_xoff;		/* set during autoboots */
int *_intr_jmpbuf;		/* jmpbuf ptr for console interrupts */

/*
 * This is a hack so that the uarts will be usable as soon as
 * possible.  It may be removed after the proms are stable.
 */
early_init()
{
	struct iob iob;
	extern char **environ;
	char **envsave;

	envsave = environ;
	environ = 0;
	_iob[0] = (struct iob *)((((unsigned int) _ioblock) + 63 ) & ~63);
	iob = *_iob[0];

#ifdef MIPS
#ifdef PROM
#if	(R3030 || RB3125)
	if (IS_R3030)		/* error goes to tty1 in Rx3230 */
		iob.i_ctlr = 1;
	else 
		iob.i_ctlr = 0;
	_sccopen(&iob);
#else
	iob.i_ctlr = 0;
	_s2681open(&iob);
#endif R3030 || RB3125
#else
	if (IS_R3030)		/* error goes to tty1 in Rx3230 */
		iob.i_ctlr = 1;
	else 
		iob.i_ctlr = 0;
	if (IS_R3030 || IS_RB3125) {
		_sccopen(&iob);
	} else {
		_s2681open(&iob);
	}
#endif PROM
#endif MIPS
#ifdef SABLE
	_sconsopen(&iob);
#endif SABLE
	environ = envsave;
}

/*
 * _init_saio -- call all saio initialization routines
 * _init_saio is called after aborts and after re-entry to the
 * monitor from external programs, its jobs is to make sure that
 * the prom data structures are consistent enough to run.  It avoids
 * trashing data structures that contain user entered info that should
 * persist across aborts; things like environment variables, enabled
 * consoles, and current uart baud rates.
 */
int stdio_init;
char *saio_versref;

_init_saio()
{
	extern char saio_version[];
	extern int _udpcksum;

	stdio_init = 0;
	ignore_xoff = 0;
 	_intr_jmpbuf = NULL;		/* don't vector console interrupts */
	atob(getenv("DEBUG"), &Debug);	/* initialize debugging flags */
	atob(getenv("VERBOSE"), &Verbose);
	atob(getenv("UDPCKSUM"), &_udpcksum);
	_init_timer();			/* timer interrupts */
	_init_devices();		/* device drivers */
	_init_fs();			/* file systems */
	_init_iob();			/* io control blocks */
	_init_iobbuf();			/* block io buffers */
	_init_mbufs();			/* network buffers */
	_init_sockets();		/* sockets buffers */
	_init_stdio();			/* stdin and stdout */
	_init_pcons();			/* pseudo console */
	(*MACHDEP(get_tod))();		/* tod clock */
	stdio_init = 1;			/* safe to use stdio now */
	saio_versref = saio_version;	/* force module to be loaded */
}

/*
 * _init_devices -- call all device driver initialization entry
 * points.  Device driver init routines should basically cleanup
 * all global storage.
 */
_init_devices()
{
	register struct device_table *dt;

	for (dt = MACHDEP(_device_table);
		 dt < &MACHDEP(_device_table)[MACHDEP(_ndevices)]; dt++)
		(*dt->dt_init)();
}

/*
 * _init_fs -- call all file system initialization entry points.
 * Fs init routines should cleanup global storage.
 */
_init_fs()
{
	register struct fs_table *fs;

	for (fs = _fs_table; fs < &_fs_table[_nfstypes[0]]; fs++)
		(*fs->fs_init)();
}

/*
 * _init_iob -- make sure iob's are in known state
 */
_init_iob()
{
	register i;

	bzero(_ioblock, sizeof(_ioblock));	
	/* align iobs to 64 byte boundaries */
	_iob[0] = (struct iob *)((((unsigned int) _ioblock) + 63 ) & ~63);
	/* set up _iobs to point to consecutive _ioblock's */
	for (i=1; i<NIOB; i++) 
		_iob[i] = _iob[i-1] + 1;
}

/*
 * _init_iobbuf -- initialize block io buffer pool
 */
_init_iobbuf()
{
	register struct iob_buf *ib;

	for (ib = _iob_buf; ib < &_iob_buf[NBUF]; ib++)
		ib->ib_flags = 0;
}

/*
 * set-up file descriptors 0 and 1
 */
_init_stdio()
{
	close(0);
	close(1);
	if (open("console(0)", O_RDONLY) != 0)
		_errputs("can't open console(0) for input\n");
	if (open("console(0)", O_WRONLY) != 1)
		_errputs("can't open console(0) for output\n");
}

/*
 * _get_iobbuf -- obtain block io buffer
 * May be called by file system open routine to obtain buffer.  If
 * fs open calls this, it's close routine MUST free this buffer.
 */
char *
_get_iobbuf()
{
	register struct iob_buf *ib;
	register char* addr; 
	register unsigned int temp;

	for (ib = _iob_buf; ib < &_iob_buf[NBUF]; ib++)
		if ((ib->ib_flags & IB_INUSE) == 0) {
			ib->ib_flags |= IB_INUSE;
			addr = ib->ib_buf;
			if ((unsigned int)addr & 0x3f) {
				temp = ((unsigned int)addr & 0x3f);
				addr += (0x40-temp);
			}
			return(addr);
		}
	_io_abort("out of io buffers");
}

/*
 * _free_iobbuf -- free block io buffer
 */
_free_iobbuf(cp)
char *cp;
{
	register struct iob_buf *ib;

	for (ib = _iob_buf; ib < &_iob_buf[NBUF]; ib++)
/* because of the roundup code above for 64 byte alignment 'cp' will not
   necessarily equal ib->ib_buf; this needs to be fixed */
/*		if (ib->ib_buf == cp && (ib->ib_flags & IB_INUSE)) { */
		if (ib->ib_flags & IB_INUSE) {
			ib->ib_flags = 0;
			return;
		}
	_io_abort("iob_buf screwup");
}

/*
 * open -- open file
 * Models 2 argument UNIX open
 */
open(filename, flags)
char *filename;
int flags;
{
	register struct iob *io;
	char *file;
	int errflg;
	struct device_table *dp;
	int ctlr, unit, part;

	io = new_iob();
	if (io == NULL)
		return(-1);
	file = parsefile(filename, &dp, &ctlr, &unit, &part);
	if (file == (char *)-1)
		return(-1);
	io->i_ctlr = ctlr;
	io->i_unit = unit;
	io->i_part = part;

	switch (flags & 0x3) {
	case O_RDONLY:
		io->i_flgs = F_READ;
		break;

	case O_WRONLY:
		io->i_flgs = F_WRITE;
		break;

	case O_RDWR:
		io->i_flgs = F_READ | F_WRITE;
		break;
	}

	io->i_dp = dp;
	/*
	 * There's now a new class of devices.  If dt_type has
	 * the DTTYPE_RAW bit set the file system switch will
	 * be used.  The normal method is to require that a filename
	 * exist before the fs switch is used.
	 * NOTE: Raw io is still constrained to lie within the
	 * partition for disks, so you need a partition that maps
	 * the entire drive for formatting, etc.
	 */
	if (dp->dt_type & DTTYPE_RAW)
		io->i_fstype = dp->dt_fs;
	else
		io->i_fstype = *file ? dp->dt_fs : DTFS_NONE;
	errflg = (*dp->dt_open)(io);
	if (errflg)
		goto bad;

#ifdef EFS
	/*
	 * Now that device is open, try to find the real filesystem type
	 * if and only if a path name is given.
	 */
	if (*file && (dp->dt_type & DTTYPE_BLOCK)) {
		register struct fs_table *fs;
		extern int _nodev(), _nulldev();

		for (fs = _fs_table; fs < &_fs_table[_nfstypes[0]]; fs++) {
			if ((fs->fs_checkfs != NULL) &&
			    (fs->fs_checkfs != _nodev) &&
			    (fs->fs_checkfs != _nulldev) &&
			    ((*fs->fs_checkfs)(io) == 0)) {
				io->i_fstype = fs - _fs_table;
				break;
			}
		}
	}
#endif EFS

	/*
	 * If DTFS_AUTO, device open routine is given a chance to
	 * determine the fs type (i.e. from partition table),
	 * if it doesn't, give up.
	 */
	if (io->i_fstype == DTFS_AUTO) {
		printf("couldn't determine fs type\n");
		io->i_errno = ENXIO;
		goto bad;
	}
	if (io->i_fstype >= _nfstypes[0]) {
		printf("file system type not supported\n");
		io->i_errno = ENXIO;
		goto bad;
	}
	/* puke if filename given and this is a raw device */
	if (*file && io->i_fstype == DTFS_NONE) {
		printf("no file system\n");
		io->i_errno = ENXIO;
		goto bad;
	}

	if (io->i_fstype) {
		errflg = (*_fs_table[io->i_fstype].fs_open)(io, file, flags);
		if (errflg) {
bad:
			errno = io->i_errno;
			io->i_flgs = 0;
			return(-1);
		}
	}
	io->i_offset = 0;
/*	return(io - _iob);	*/
	return(io - _iob[0]);
}

/*
 * lseek(fd, offset, how) -- set file offset
 */
lseek(fd, offset, how)
int fd;
u_int offset;
int how;
{
	register struct iob *io = get_iob(fd);

	if (io == NULL || io->i_flgs == 0) {
		errno = EBADF;
		return(-1);
	}

	switch (how) {
	case 0:
		io->i_offset = offset;
		break;

	case 1:
		io->i_offset += offset;
		break;
	
	case 2:
		printf("lseek mode 2 not supported\n");
		return(-1);

	default:
		printf("invalid lseek arg");
		errno = EINVAL;
		return(-1);
	}
	return (io->i_offset);
}

/*
 * read -- standalone read
 */
int
read(fd, buf, cnt)
int fd;
char *buf;
int cnt;
{
	register struct iob *io = get_iob(fd);
	int retval;

	if (io == NULL || io->i_flgs == 0) {
		errno = EBADF;
		return(-1);
	}

	_scandevs();

	if (io->i_fstype)
		/*
		 * fs routines take the current file offset and perform
		 * the appropriate transfer by calling the device strategy
		 * routines directly.  By convention, i_bn indicates the
		 * current block in the buffer pointed to by i_buf (if
		 * one exists).
		 *
		 * The fs routine is expected to update i_offset and
		 * return the actual count of characters transferred.
		 * If an error occurs on the transfer, the return
		 * value should be -1 and i_errno should indicate the
		 * error (a la errno.h).
		 */
		retval = (*_fs_table[io->i_fstype].fs_read)(io, buf, cnt);
	else {
		/*
		 * device strategy routines expect to be called with
		 * i_ma set to the buffer address, i_cc the character
		 * count, and for block devices, i_bn set to the
		 * appropriate block number (blocks are of size DEV_BSIZE).
		 *
		 * The device strategy routines are expected to return
		 * the actual count of bytes transferred, if an error
		 * occurs, the return value should be -1 and i_errno
		 * should be set to the appropriate errno.h error number.
		 */
		io->i_ma = buf;
		io->i_cc = cnt;
		if (io->i_dp->dt_type & DTTYPE_BLOCK) {
			if (io->i_offset % DEV_BSIZE) {
				printf("offset not on block boundry\n");
				return(-1);
			}
			io->i_bn = io->i_offset / DEV_BSIZE;
		}
		retval =(*io->i_dp->dt_strategy)(io, READ);
		if (retval > 0)
			io->i_offset += retval;
	}
	if (retval < 0)
		errno = io->i_errno;
	return (retval);
}

/*
 * write -- standalone write
 */
int
write(fd, buf, cnt)
int fd;
char *buf;
int cnt;
{
	register struct iob *io = get_iob(fd);
	int retval;

	if (io == NULL || io->i_flgs == 0) {
		errno = EBADF;
		return(-1);
	}

	_scandevs();

	if (io->i_fstype)
		retval = (*_fs_table[io->i_fstype].fs_write)(io, buf, cnt);
	else {
		io->i_ma = buf;
		io->i_cc = cnt;
		if (io->i_dp->dt_type & DTTYPE_BLOCK) {
			if (io->i_offset % DEV_BSIZE) {
				printf("offset not on block boundry\n");
				return(-1);
			}
			io->i_bn = io->i_offset / DEV_BSIZE;
		}
		retval = (*io->i_dp->dt_strategy)(io, WRITE);
		if (retval > 0)
			io->i_offset += retval;
	}
	if (retval < 0)
		errno = io->i_errno;
	return (retval);
}

/*
 * close -- close file
 * Calls fs and device close routines
 */
close(fd)
int fd;
{
	register struct iob *io = get_iob(fd);
	int retval;

	if (io == NULL || io->i_flgs == 0) {
		errno = EBADF;
		return(-1);
	}

	retval = 0;
	if (io->i_fstype)
		retval = (*_fs_table[io->i_fstype].fs_close)(io);

	retval |= (*io->i_dp->dt_close)(io);
done:
	if (retval)
		errno = io->i_errno;
	io->i_flgs = 0;
	return (retval);
}

/*
 * ioctl -- io control
 * A place to hide all those device and fs specific operations
 * (kinda ugly, but necessary)
 */
ioctl(fd, cmd, arg)
int fd;
int cmd;
int arg;
{
	register struct iob *io = get_iob(fd);
	int errflg;

	switch (cmd) {
	case TIOCPROTO:
		_cons_ctl(fd, arg);
		return(0);

	case FIOCNBLOCK:
		if (arg)
			io->i_flgs |= F_NBLOCK;
		else
			io->i_flgs &= ~F_NBLOCK;
		return(0);
	}

	/*
	 * ??? THIS SHOULD PROBABLY RELY ON THE FS ROUTINE TO CALL
	 * THE DEV IOCTL ROUTINE IF NECESSARY, RATHER THAN CALLING
	 * THE DEV ROUTINE IF THE FS ROUTINE DIDN'T ERROR RETURN ???
	 */
	if (io->i_fstype) {
		errflg = (*_fs_table[io->i_fstype].fs_ioctl)(io, cmd, arg);
		if (errflg < 0)
			goto done;
	}

	errflg = (*io->i_dp->dt_ioctl)(io, cmd, arg);
done:
	if (errflg < 0)
		errno = io->i_errno;
	return(errflg);
}

#define LED_PERIOD	1024
static unsigned led_count;
static int led_direction;
static int led_pattern;
int intrepid_led_pattern;
unsigned char patterns[] = { 0x80, 0xc0, 0xe0, 0x70, 0x38, 0x1c, 0x0e, 0x07,
		    0x03, 0x01, 0x03, 0x07, 0x0e, 0x1c, 0x38, 0x70,
		    0xe0, 0xc0 };
int size_patterns = (sizeof(patterns) / sizeof(char));

/*
 * scandevs -- scan all enabled getc devices for input
 * Called periodically to avoid receiver overruns
 */
_scandevs()
{
	if ( ((IS_R2300 || IS_R3200 || IS_RB3125 || IS_R6300) && 
	     (++led_count > LED_PERIOD)) ||
	     ((IS_R2400)  && (++led_count > LED_PERIOD/6))) {
		if (IS_R2300 || IS_R3200 || IS_RB3125 || IS_R6300) {
			led_count = 0;
			if (led_pattern == 0) {
				led_pattern = 1;
				led_direction = 1;
			}
			if (led_direction > 0  && (led_pattern & 0x20))
				led_direction = -1;
			if (led_direction < 0 && (led_pattern & 1))
				led_direction = 1;
			led_pattern = led_direction > 0
			    ? led_pattern<<1 : led_pattern>>1;
			set_leds(led_pattern);
		} else if (IS_R2400) {
			led_count = 0;
			set_leds(patterns[intrepid_led_pattern]);
			intrepid_led_pattern++;
			if (intrepid_led_pattern >= size_patterns)
				intrepid_led_pattern = 0;
		}
	}
	if ((led_count & 0xf) == 0)
		_check_timer();	/* check alarm clock */
	__scandevs();
#if !PROM || !R3030
	if (IS_R6300)
	  _ioa_error_scan(0);  /* Scan all IOAs checking for errors */
#endif
}

__scandevs()
{
	register struct iob *io;
	register i;

/*	for (io = _iob; io < &_iob[NIOB]; io++)		*/
	for (i=0; i<NIOB; i++) {
		io = _iob[i];
		if (io->i_flgs & F_SCAN)  {
		  	reset_ioc_retry_count();	/* reset DBE count */
/*			ioctl(io - _iob, FIOCSCAN, 0);	*/
			ioctl(io - _iob[0], FIOCSCAN, 0);
		      }
	}
}

/*
 * set_leds -- write pattern into cpu board leds
 */
set_leds(pattern)
{
	register char *led_address = (char *)PHYS_TO_K1(MACHDEP(LED_REG));

	if (IS_R2300 || IS_R3200 || IS_RB3125)
		/*
		 * or in 0xC0 to avoid resetting local memory interface and
		 * coprocessor interface
		 */
		*led_address = ~pattern | 0xC0;	/* 1=off 0=on */
	else if (IS_R2400)
		*led_address = ~pattern;	/* 1=off 0=on */
	else if (IS_R6300)
		*led_address = pattern & 0x3F;	/* 1=on 0=off */
}

/*
 * clear_nofault -- clear any existing fault handlers
 */
clear_nofault()
{
	extern int *nofault;

	ignore_xoff = 0;
	_init_timer();		/* cancel any timers */
	nofault = 0;		/* cancel nofault handling */
}

/*
 * _io_abort -- something terrible happened, give up
 * Clear timers and nofault handling so we don't end up somewhere
 * we don't expect to.
 */
_io_abort(msg, arg1, arg2, arg3)
char *msg;
{
	clear_nofault();
	printf("%s\n", msg, arg1, arg2, arg3);
	exit(-1);
}

/*
 * exit -- terminate existence gracefully
 */
exit(exitno)
{
	register int fd;

	clear_nofault();
	printf("exit(%d) called\n", exitno);
	for (fd = 0; fd < NIOB; fd++)
		close(fd);

#ifdef PROM
#ifdef R3030
	save_gsparam();
#endif R3030
#else PROM
	if (IS_R3030) save_gsparam();
#endif PROM
	_exit(exitno);
}

/*
 * new_iob -- obtain a new standalone io control block
 */
static
struct iob *
new_iob()
{
	register struct iob *io;
	register i;

/*	for (io = _iob; io < &_iob[NIOB]; io++)		*/
	for (i=0; i<NIOB; i++) {
		io = _iob[i];
		if (io->i_flgs == 0)
			return(io);
	}
	_io_abort("out of file descriptors");	/* doesn't return */
	return(NULL);	/* keep lint happy */
}

/*
 * get_iob -- find io control block for fd
 */
static
struct iob *
get_iob(fd)
int fd;
{
	if (fd < 0 || fd >= NIOB)
		return(NULL);
/*	return(&_iob[fd]);	*/
	return(_iob[fd]);
}

/*
 * lookup_dev -- search device_table
 */
static
struct device_table *
lookup_dev(cp)
char *cp;
{
	register struct device_table *dp;

	for (dp = MACHDEP(_device_table); 
		dp < &MACHDEP(_device_table)[MACHDEP(_ndevices)]; dp++) {
		if (strcmp(dp->dt_string, cp) == 0)
			return(dp);
	}

	ignore_xoff = 0;
	printf("%s is not known device\n", cp);
	printf("Known devices are:\n");
	for (dp = MACHDEP(_device_table); 
		dp < &MACHDEP(_device_table)[MACHDEP(_ndevices)]; dp++)
		printf("\t%s:\t%s\n", dp->dt_string, dp->dt_desc);
	return(NULL);
}

/*
 * parsefile -- break filename into dev, ctlr, unit, partition, and file
 */
static char *
parsefile(filename, dpp, ctlrp, unitp, partp)
char *filename;
struct device_table **dpp;
int *ctlrp, *unitp, *partp;
{
	register char *cp, *dp, *tp;
	struct	device_table	*tpp;
	char device[32];
	char type[32];

	/*
	 * filename syntax:
	 *  dev(ctlr, unit, part)file
	 */
	cp = filename;
	dp = device;
	while (*cp && *cp != '(' && dp < &device[sizeof(device)-1])
		*dp++ = *cp++;
	if (*cp != '(') {
		goto bad;
	}
	*dp = 0;
	cp++;

	*dpp = lookup_dev(device);
	if (*dpp == NULL) {
		return((char *)-1);
	}

	/*
	 * Ctlr, unit, and partition default to zero if not specified
	 */
	*ctlrp = *unitp = *partp = 0;

	while (isspace(*cp))
		cp++;

	/*
 	 * The syntax for filename is slightly different if the device
	 * is bfs or bootp :
	 *    dev(ctrl, type)file
	 * Where type is the ascii name of the real hardware device
	 * ie, cmc for the enp10 or egl for the 4210 ethernet controller
	 */

	if ( !strcmp( device,"bfs" ) || !strcmp( device,"bootp" ) ) {
	    if (*cp != ')') {
		cp = atob(cp, ctlrp);
		while (isspace(*cp))
		    cp++;
		if (*cp == ',') {
		    tp = type;
		    cp++;
		    while (*cp && *cp != ')' && tp < &type[sizeof(type)-1]){
			if (isspace(*cp))
			    continue;
			*tp++ = *cp++;
		    }
		    *tp = 0;
		    *dpp = lookup_dev(type);
		    if (*dpp == NULL) {
			printf("unknown device: %s\n", type);
			return((char *)-1);
		    }
		    /* malloc some space and copy dpp */
		    tpp = (struct device_table *)
				malloc( sizeof(struct device_table) );
		    (*tpp) = (**dpp);
		    (*dpp) = tpp;
		    if ( !strcmp(device,"bfs") )
			(*dpp)->dt_fs = DTFS_BFS;
		    else
			(*dpp)->dt_fs = DTFS_BOOTP;
		}
		if (*cp != ')') {
		    goto bad;
		}
	    }
	}
	else
	if (*cp != ')') {
		cp = atob(cp, ctlrp);
		while (isspace(*cp))
			cp++;
		if (*cp == ',') {
			cp = atob(++cp, unitp);
			while (isspace(*cp))
				cp++;
			if (*cp == ',') {
				if (isalpha(*++cp)) {
					*partp = *cp++;
					*partp -= isupper(*partp) ? 'A' :'a';
				} else
					cp = atob(cp, partp);
				while (isspace(*cp))
					cp++;
			}
		}
	}
	if (*cp++ != ')') {
bad:
		ignore_xoff = 0;
		printf("bad filename: %s\n", filename);
		return((char *)-1);
	}
	return(cp);
}

/*
 * _is_samedev -- determine if to fd's refer to same device
 */
_is_samedev(fd1, fd2)
unsigned fd1, fd2;
{
	register struct iob *i1, *i2;

	if (fd1 >= NIOB || fd2 >= NIOB)
		_io_abort("_is_samedev");
	i1 = _iob[fd1];
	i2 = _iob[fd2];
/*
	i1 = &_iob[fd1];
	i2 = &_iob[fd2];
 */
	if (i1->i_dp == i2->i_dp && i1->i_ctlr == i2->i_ctlr &&
	    i1->i_unit == i2->i_unit && i1->i_part == i2->i_part)
		return (1);
	else
		return (0);
}

/*
 * isatty -- determine if fd refers to a character device
 */
isatty(fd)
int fd;
{
	register struct iob *io = get_iob(fd);

	return (io && (io->i_dp->dt_type & DTTYPE_CONS));
}


/*
 * _ttyinput -- called by char driver scandev ioctl routines to deal with
 * circ_buf and special characters
 */
void
_ttyinput(db, c)
register struct device_buf *db;
register char c;
{
	if (db->db_flags & DB_RAWRAW) {
		goto ok;
	}
	if ((c & 0xff) == CTRL(S)) {
		if (!ignore_xoff)
			db->db_flags |= DB_STOPPED;
		return;
	}
	db->db_flags &= ~DB_STOPPED;
	if ((c & 0xff) == CTRL(Q))
		return;
	if ((c & 0xff) == INTR) {
		console_intr();
		/* doesn't return */
	}
	if (c && (db->db_flags & DB_RAW) == 0) {
		if ((c & 0x7f) == CTRL(D))
			exit(0);
		if ((c & 0x7f) == CTRL(Z)) {
			exec_brkpt();
			return;
		}
		/*
		 * duplicate this stuff here so that in cooked
		 * mode they aren't sensitive to the parity bit
		 */
		if ((c & 0x7f) == CTRL(S)) {
			if (!ignore_xoff)
				db->db_flags |= DB_STOPPED;
			return;
		}
		db->db_flags &= ~DB_STOPPED;
		if ((c & 0x7f) == CTRL(Q))
			return;
		if ((c & 0x7f) == INTR) {
			console_intr();
			/* doesn't return */
		}
	}
ok:
	_circ_putc(c, db);
}

/*
 * console_intr -- deal with console interrupts
 */
console_intr()
{
	int *jb_ptr;

	if (_intr_jmpbuf) {
		jb_ptr = _intr_jmpbuf;
		_intr_jmpbuf = NULL;
		longjmp(jb_ptr, 1);
	}
	_io_abort("\nCONSOLE ABORT\n");
}

/*
 * _circ_getc -- remove character from circular buffer
 */
_circ_getc(db)
register struct device_buf *db;
{
	int c;

	if (CIRC_EMPTY(db))	/* should always check before calling */
		_io_abort("_circ_getc");

	c = *db->db_out++ & 0xff;
	if (db->db_out >= &db->db_buf[sizeof(db->db_buf)])
		db->db_out = db->db_buf;
	return (c);
}

/*
 * _circ_putc -- insert character in circular buffer
 */
_circ_putc(c, db)
int c;
register struct device_buf *db;
{
	char *cp;
	extern Verbose;

	cp = db->db_in + 1 >= &db->db_buf[sizeof(db->db_buf)]
	    ? db->db_buf
	    : db->db_in + 1;

	/*
	 * if buffer is full, ignore the character
	 */
	if (cp == db->db_out) {
		if (Verbose)
			_errputs("\ndropped char\n");
		return;
	}

	*db->db_in = c;
	db->db_in = cp;
}

/*
 * _nodev -- device and fs table stub for routines that should never
 * be called
 */
_nodev(io)
struct iob *io;
{
	printf("io request for unsupported operation\n");
	io->i_errno = ENXIO;
	return (-1);
}

/*
 * _nulldev -- device and fs table routine for routines that are
 * nop's
 */
_nulldev()
{
	return(0);
}

/*
 * _min -- SIGNED minimum function (C really needs inline functions!
 * macro's screwup when args have side-effects)
 */
_min(a, b)
register a, b;
{
	return(a < b ? a : b);
}

/*
 * _max -- SIGNED maximum function
 */
_max(a, b)
register a, b;
{
	return(a > b ? a : b);
}

/*
 * badaddr -- verify address is read accessable
 */
badaddr(addr, size)
unsigned addr;
unsigned size;
{
	jmp_buf ba_buf;
#ifdef COMPILER_FIXED
	register int junk;
#else
	static int junk;
#endif
	extern int *nofault;

	if (setjmp(ba_buf)) {
		sa_spl();
#if !PROM || !R3030
		if (IS_R6300) {
			reset_ioa_force_busy(addr);
			_ioa_error_scan(addr);  /* Clear IOA error from BERR */
		}
#endif
		return(1);
	}

	nofault = ba_buf;
	switch (size) {
	case sizeof(char):
		junk = *(volatile char *)addr;
		break;

	case sizeof(short):
		junk = *(volatile short *)addr;
		break;

	case sizeof(int):
		junk = *(volatile int *)addr;
		break;

	default:
		printf("badaddr: bad size");
	}
	nofault = 0;
	return(0);
}

#ifdef PROM
extern int memlimit;
#endif

/*
 * wbadaddr -- verify address is write accessable
 */
wbadaddr(addr, size)
unsigned addr;
unsigned size;
{
	jmp_buf ba_buf;
	extern int *nofault;

#ifdef PROM
	if (IS_R3030) {
		if (addr >= (K1BASE + memlimit)) return (1);
		else return(0);
	}
#endif	
	if (setjmp(ba_buf)) {
		sa_spl();
		return(1);
	}

	nofault = ba_buf;
	switch (size) {
	case sizeof(char):
		*(char *)addr = 0;
		break;

	case sizeof(short):
		*(short *)addr = 0;
		break;

	case sizeof(int):
		*(int *)addr = 0;
		break;

	default:
		printf("wbadaddr: bad size");
	}
	nofault = 0;
	return(0);
}

#if !PROM || !R3030
/*
 * reset_ioa_force_busy
 *	If 'addr' lies within GBA space, then reset the appropriate IOC's
 *	ForceBusy state to permit subsequent GBA space accesses.
 */
reset_ioa_force_busy (addr)
unsigned addr;
{
	u_int	ctlspace_vaddr;

	ctlspace_vaddr = get_ioa_ctlspace_addr(addr);
	if (ctlspace_vaddr) {		/* in GBA space? */
		*(volatile u_int *)(ctlspace_vaddr + IOA_ERRORINFO_REG)
			= IOC_BUSY_RESET;
	}
}
#endif

#ifdef PROM
/*
 * _init_malloc_prom -- stub to call _init_malloc with right parameters
 *	Can not have malloc region outside of 16 megabyte barrier because
 *	of 24 bit addressed devices such as SCSI/LANCE/DMA on Intrepid.
 *
 *	Can not have malloc region outside of 8 meg for non intrepids
 *
 *	XXX - what will kernel touch of top of memory, must allow for
 *	      kernel xpr buffer, cmn_err, etc.  Maybe not.
 */
_init_malloc_prom()
{
	extern int memsize;

	if (IS_R3030) {
	    _init_malloc(0x700000,MALLOC_SIZE_PROM);	
	} else if (IS_R2400) { 
	    _init_malloc((memsize < 0x1000000 ? memsize - 4 : 0x1000000 - 4),
		MALLOC_SIZE_PROM);
	} else {
	    _init_malloc((memsize < 0x800000 ? memsize - 4 : 0x800000 - 4),
		MALLOC_SIZE_PROM);
	}  
}
#endif

/*
 * _init_malloc_saio -- stub to call _init_malloc with right parameters
 */

_init_malloc_saio()
{
	extern end;
	char *amalloc_size;
	extern char *atob();
	int malloc_size;

	/*
	 *  Look for an environment variable "malloc_size_saio" to override
	 *  the default size.
	 */
	amalloc_size = getenv("malloc_size_saio");
	if (!amalloc_size || *atob(amalloc_size, &malloc_size)) {
		malloc_size = MALLOC_SIZE_SAIO;
	} else {
		if (malloc_size < MALLOC_SIZE_SAIO/2) {
			printf("_init_malloc_saio: 'malloc_size_saio' too small, using default %d \n", MALLOC_SIZE_SAIO);
			malloc_size = MALLOC_SIZE_SAIO;
		}
	}
	_init_malloc((int)&end + 4 + malloc_size, malloc_size);
}

_init_malloc(end_of_malloc, size_of_malloc)
unsigned long end_of_malloc, size_of_malloc;
{
    unsigned long testsp;

    if (end_of_malloc & 0x3) {
	printf("_init_malloc: end_of_malloc is NOT longword aligned\n");
	/* Is this a problem... */
	return;
    }

    malloc_start = malloc_ptr = PHYS_TO_K1((int)end_of_malloc - size_of_malloc);
    malloc_end = PHYS_TO_K1(end_of_malloc);
    if (IS_KSEG0(end_of_malloc)) {
	/* this is a cacheable program -- make malloc region cacheable */
	malloc_start = malloc_ptr = K1_TO_K0(malloc_start);
	malloc_end   = K1_TO_K0(malloc_end);
    }

#ifndef PROM
    if( (unsigned long *)end_of_malloc > &testsp ){
	printf("PANIC: malloc space 0x%x clobbers stack 0x%x\n", malloc_end, &testsp);
	exit(-1);
    }
#endif
#ifndef SABLE
    bzero(malloc_start,size_of_malloc);
#endif !SABLE
}

/*
 * Malloc/release routines.  This is a very simple implementation of
 * malloc and free.  Malloc returns you the number of bytes you
 * requested.  Release can ONLY be used to free memory grabbed by the
 * last malloc.  Release will free memory up to the pointer given as
 * the argument.  Unless you know what you are doing, just use malloc.
 * Returns -1 if failure results.
 */

malloc(size)
int size;
{
    int tmp;

    if (size == 0)		/* Special case, return current pointer */
      return(malloc_ptr);	/* so that people may align if needed   */

    if ( (malloc_ptr + size) > malloc_end) {
	printf("malloc: ran out of malloc space\n");
	return(-1);
    }

    tmp = malloc_ptr;
    malloc_ptr += size;
    XPR3(XPR_MEM,"malloc: 0x%x size 0x%x\n",tmp,size);
#ifndef PROM
	/* We have sort of promised a zeroed malloc region, but in
	 * standalones we want to wait as long as possible before
	 * zeroing the memory in case we are getting close to someone
	 */
    bzero(tmp, size);
#endif
    return(tmp);
}

/*
 * Aligned malloc.  This will allocate "size" bytes aligned on "align"
 * byte boundry.  Some code requires that buffer be aligned on certain
 * byte boundries.  This only works if the alignment is a power of 2.
 * Returns -1 if failure results.
 */

align_malloc(size,align)
int size, align;
{
    int tmp, junk;

    if ( (align % 2) != 0 ) {
	printf("align_malloc: alignment 0x%x NOT power of 2\n",align);
	return(-1);
    }

    tmp = malloc(0);		/* Get current pointer */
    if (tmp & (align-1)) {	/* We are NOT aligned, so align */
	junk = ( (tmp + align) & ~(align-1) ) - tmp;
	XPR4(XPR_MEM,"align_malloc: junk 0x%x align 0x%x tmp 0x%x\n",
	       junk,align,tmp);
	if (malloc(junk) == -1) {	/* This room lost */
		printf("align_malloc: out of memory allocating 0x%x junk\n");
		return(-1);
	}
    }
    if ((tmp = malloc(size)) == -1) {
	printf("align_malloc: out of memory allocating 0x%x bytes\n",size);
	return(-1);
    }
    return(tmp);
}

/*
 * Release all memory back to ptr.  This can be very dangerous.  If you are
 * going to use free, you must use malloc/free pairs to expect to be
 * sane.  For example, if you malloc(a) bytes, then malloc(b) bytes, then
 * free the ptr returned from malloc(a), you have also freed the memory
 * return from malloc(b).
 */
release(ptr)
int ptr;
{

    if ( (ptr < malloc_start) || (ptr > malloc_end) || (ptr > malloc_ptr) ) {
	printf("release: bogus pointer to free 0x%x start 0x%x end 0x%x\n",
	       ptr, malloc_start, malloc_end);
	return(-1);
    }
    XPR3(XPR_MEM,"release: 0x%x size 0x%x\n",ptr, ptr-malloc_ptr);
    malloc_ptr = ptr;

}


