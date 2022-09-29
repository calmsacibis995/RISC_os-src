#ident "$Header: pcons.c,v 1.6 90/05/04 16:04:04 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright
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
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * pcons.c -- pseudo console routines
 * allows for multiple consoles
 */

#include "sys/param.h"
#include "sys/file.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/ctype.h"

#define	NENABLES	4
#define	ETNLEN		32

/*
 * enable_table records currently open consoles
 */
static struct enable_table {
	int et_flags;
	int et_fd;
	char et_name[ETNLEN];
} enable_table[NENABLES];
/* extern struct iob _iob[];		*/
extern struct iob *_iob[];

#define	ET_INUSE	1	/* this enable_table slot is inuse */
#define	ET_PROTO	2	/* currently speaking protocol */

extern caddr_t frm_addr;
int color_cons, mono_cons;

/*
 * init_pcons -- initialize pseudo console
 *
 *   3030 console strategy:
 *      0:      tty0
 *      1:      tty1
 *      r,t:    tty0, tty1
 *      m:      if keyboard then mono else tty1
 *      c:      if colour & keyboard then colour else tty1
 *      a:      tty0, tty1, mono, colour
 *      v:      if keyboard then (mono ALSO if colour then color) else tty1
 *      g,l:    if colour & keyboard then colour else if keyboard then mono 
 *		else tty1
 */
_init_pcons()
{
	register struct enable_table *et;
	char c;
	char *cp;
	char names[NENABLES][ETNLEN];
	int i,do_0,do_1;
	extern char *getenv();

	/*
	 * run through enable table and remember currently enabled
	 * consoles so they can be re-enabled so prom restarts will
	 * work correctly and not forget current consoles
	 */
	do_0 = do_1 = 0;
	i = 0;
	for (et = enable_table; et < &enable_table[NENABLES]; et++) {
		if (et->et_flags & ET_INUSE) {
			strncpy(names[i], et->et_name, sizeof(names[0]));
			names[i++][sizeof(names[0])-1] = 0;
		}
		et->et_flags = 0;
	}
	if (i) {
		while (--i >= 0)
			cons_enable(names[i]);
		return;
	}

	/*
	 * No consoles currently enabled, always enable local console
	 * and any consoles indicated in environment variable "console"
	 * THIS OPEN HAD BETTER WORK, OR WE'RE IN DEEP .... TROUBLE
	 */
	c = (cp = getenv("console")) ? *cp : 0;
	if (c == 't') c = 'r';
	if (IS_R3030) {
	        if (c == 'r' || c == 'a' || c == '0') do_0 = 1;
	} else do_0 = 1;			
	if (c == 'r' || c == 'a' || c == '1') do_1 = 1;	
#ifdef MIPS
	if (IS_R3030) {
	    if (color_cons || mono_cons) cons_enable("video(0)");
	    /* if no other console devices, enable tty1 */
	    if (!color_cons && !mono_cons && !do_0) do_1 = 1;
	}
#endif
	if (do_1)
		cons_enable("tty(1)");
	if (do_0)
		cons_enable("tty(0)");
#ifdef NCP
	if (c == 'n' || c == 'a')
		cons_enable("ncp(0)");
#endif NCP
}

/*
 * cons_enable -- enable console device
 */
static
cons_enable(dev)
char *dev;
{
	register struct enable_table *et, *eet;
	register newfd;

	newfd = open(dev, O_RDWR);
	if (newfd < 0) {
		printf("can't open %s\n", dev);
		return(-1);
	}
	if (!isatty(newfd)) {
		close(newfd);
		printf("%s is not a char device\n", dev);
		return(-1);
	}

	eet = NULL;
	for (et = enable_table; et < &enable_table[NENABLES]; et++) {
		if ((et->et_flags & ET_INUSE) == 0) {
			if (eet == NULL)
				eet = et;
			continue;
		}
		if (_is_samedev(newfd, et->et_fd)) {
			close(newfd);
			printf("%s is already enabled\n", dev);
			return(-1);
		}
	}

	if (eet == NULL) {
		close(newfd);
		printf("too many consoles enabled\n");
		return(-1);
	}

	ioctl(newfd, FIOCNBLOCK, 1);
	proto_disable(newfd); 	/* make sure we're in cooked mode */
	eet->et_fd = newfd;
	eet->et_flags = ET_INUSE;
	strncpy(eet->et_name, dev, sizeof(eet->et_name));
	return(0);
}

/*
 * disable -- disable console device
 */
static
cons_disable(dev)
char *dev;
{
	register struct enable_table *et;
	register newfd;

	newfd = open(dev, O_RDWR);
	if (newfd < 0) {
		printf("can't open %s\n", dev);
		return(-1);
	}

	for (et = enable_table; et < &enable_table[NENABLES]; et++)
		if (_is_samedev(newfd, et->et_fd)) {
			close(et->et_fd);
			close(newfd);
			et->et_flags = 0;
			return(0);
		}

	close(newfd);
	printf("%s not enabled\n", dev);
	return(-1);
}

/*
 * show_enables -- show current getc devices that are enabled
 */
static
show_enables()
{
	register struct enable_table *et;

	for (et = enable_table; et < &enable_table[NENABLES]; et++) {
		if ((et->et_flags & ET_INUSE) == 0)
			continue;
		printf("%s\n", et->et_name);
	}
}

/*
 * _cons_ctl -- hack so that TIOCPROTO ioctl can disable output
 * on an enabled console while that device is speaking protocol
 * (otherwise protocol code gets real confused!).  See ioctl() TIOCPROTO.
 */
void
_cons_ctl(fd, proto_flag)
int fd;
int proto_flag;
{
	register struct enable_table *et;

	for (et = enable_table; et < &enable_table[NENABLES]; et++) {
		if ((et->et_flags & ET_INUSE) == 0)
			continue;
		if (_is_samedev(et->et_fd, fd)) {
			if (proto_flag)
				et->et_flags |= ET_PROTO;
			else	
				et->et_flags &= ~ET_PROTO;
		}
	}
}

/*
 * _pconsioctl -- distributes ioctl's to individual devices
 */
_pconsioctl(io, cmd, arg)
struct iob *io;
int cmd;
int arg;
{
	register struct enable_table *tet;
	int retval = 0;

	switch (cmd) {
	case PIOCENABLE:
		retval = cons_enable(arg);
		break;

	case PIOCDISABLE:
		retval = cons_disable(arg);
		break;

	case PIOCSHOW:
		show_enables();
		break;

	default:
		for (tet = enable_table; tet < &enable_table[NENABLES]; tet++) {
			if ((tet->et_flags & ET_INUSE) == 0)
				continue;
			retval |= ioctl(tet, cmd, arg);
		}
	}
	return(retval);
}

/*
 * _pconsstrategy -- distribute io to individual consoles
 * returns count of chars transferred
 */
_pconsstrategy(io, func)
struct iob *io;
int func;
{
	int ocnt;

	if (func == READ)
		return(pconsread(io));
	else if (func == WRITE)
		return(pconswrite(io));
	else
		/*
		 * What's the proper method to deal with these
		 */
		_io_abort("pconsstrategy");
}

/*
 * pconsread -- read from any of enabled consoles
 * returns count of characters transferred
 */
static
pconsread(io)
register struct iob *io;
{
	register struct enable_table *tet;
	register struct iob *tio;
	int cc, oldcc;
	int ocnt = io->i_cc;
	extern struct fs_table _fs_table[];

	while (io->i_cc > 0) {
		oldcc = io->i_cc;
		for (tet = enable_table;
		    io->i_cc > 0 && tet < &enable_table[NENABLES];
		    tet++) {
			if ((tet->et_flags & ET_INUSE) == 0 ||
			    (tet->et_flags & ET_PROTO))
				continue;

			/*
			 * inline of iob_read to avoid extraneous
			 * scandevs();
			 */
/*			tio = &_iob[tet->et_fd];	*/
			tio = _iob[tet->et_fd];
			/*
			 * fs_type here refers to protocol routines
			 * (rather than file systems)
			 */
			if (tio->i_fstype)
				cc = (*_fs_table[tio->i_fstype].fs_read)
				    (tio, io->i_ma, io->i_cc);
			else {
				tio->i_ma = io->i_ma;
				tio->i_cc = io->i_cc;
				cc = (*tio->i_dp->dt_strategy)(tio, READ);
			}
			if (cc <= 0)
				continue;
			io->i_ma += cc;
			io->i_cc -= cc;
		}
		/*
		 * If non-block and made a complete pass without picking up
		 * any characters, return
		 */
		if ((io->i_flgs & F_NBLOCK) && oldcc == io->i_cc)
			break;
		if (io->i_cc)
			_scandevs();
	}
	return(ocnt - io->i_cc);
}

/*
 * pconswrite -- write to all enabled consoles
 * return count of chars transferred
 */
static
pconswrite(io)
register struct iob *io;
{
	register struct enable_table *tet;
	register struct iob *tio;
	int cc;
	int ocnt = io->i_cc;

#ifdef notdef
	_scandevs();
#endif
	while (io->i_cc > 0) {
		for (tet = enable_table; tet < &enable_table[NENABLES]; tet++) {
			if ((tet->et_flags & ET_INUSE) == 0 ||
			    (tet->et_flags & ET_PROTO))
				continue;
/*			tio = &_iob[tet->et_fd];	*/
			tio = _iob[tet->et_fd];
			if (tio->i_fstype)
				cc = (*_fs_table[tio->i_fstype].fs_write)
				    (tio, io->i_ma, 1);
			else {
				tio->i_ma = io->i_ma;
				tio->i_cc = 1;
				cc = (*tio->i_dp->dt_strategy)(tio, WRITE);
			}
		}
		if (cc <= 0)
			continue;
		io->i_ma += cc;
		io->i_cc -= cc;
	}
	return(ocnt);
}
