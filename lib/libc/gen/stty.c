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
#ident	"$Header: stty.c,v 1.1.1.2 90/05/10 01:40:04 wje Exp $"


/*
 * Writearound to old stty system call.
 */

#include <sgtty.h>

stty(fd, ap)
	struct sgttyb *ap;
{
	struct {
		char	sg_ispeed;		/* input speed */
		char	sg_ospeed;		/* output speed */
		char	sg_erase;		/* erase character */
		char	sg_kill;		/* kill character */
		short	sg_flags;		/* mode flags */
	} _bsd_sgtty;
	int	r;

	_bsd_sgtty.sg_ispeed = ap->sg_ispeed;
	_bsd_sgtty.sg_ospeed = ap->sg_ospeed;
	_bsd_sgtty.sg_erase = ap->sg_erase;
	_bsd_sgtty.sg_kill = ap->sg_kill;
	_bsd_sgtty.sg_flags = ap->sg_flags;
	r = ioctl(fd, TIOCSETP, &_bsd_sgtty);
	return(r);
}
