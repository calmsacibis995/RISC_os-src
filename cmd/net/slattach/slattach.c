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
#ident	"$Header: slattach.c,v 1.1.2.2 90/05/09 17:23:23 wje Exp $"


/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 *	@(#)slattach.c	4.3 (Berkeley) 6/30/88
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Adams.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)slattach.c	4.3 (Berkeley) 6/30/88";
#endif /* not lint */

#include <stdio.h>
#include <sys/param.h>
#if mips
#include <termio.h>
#include <stropts.h>
#include <net/soioctl.h>
#include <sys/slip.h>
#include <syslog.h>
#include <sys/socket.h>
#include <netinet/in.h>
#else
#include <sgtty.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netdb.h>
#endif
#include <fcntl.h>


#define DEFAULT_BAUD	9600
#if !mips
int	slipdisc = SLIPDISC;
char	hostname[MAXHOSTNAMELEN];
#endif

char	devname[32];

main(argc, argv)
	int argc;
	char *argv[];
{
	register int fd;
	register char *dev = argv[1];
#if mips
	struct	termio	tios;
#else
	struct sgttyb sgtty;
#endif
	int	speed;

	if (argc < 2 || argc > 3) {
		fprintf(stderr, "usage: %s ttyname [baudrate]\n", argv[0]);
		exit(1);
	}
	speed = argc == 3 ? findspeed(atoi(argv[2])) : findspeed(DEFAULT_BAUD);
	if (speed == 0) {
		fprintf(stderr, "unknown speed %s", argv[2]);
		exit(1);
	}
	if (strncmp("/dev/", dev, 5)) {
		(void)sprintf(devname, "/dev/%s", dev);
		dev = devname;
	}
	if ((fd = open(dev, O_RDWR | O_NDELAY)) < 0) {
		perror(dev);
		exit(1);
	}
#if mips
	/* pop all streams modules */
	while (ioctl(fd, I_POP, 0) == 0)
		continue;

	if (ioctl(fd, TCGETA, (caddr_t)&tios) < 0) {
		syslog(LOG_ERR, "ioctl (TCGETA)");
		exit(1);
	}
	tios.c_cflag = speed;	/* set the new speed */
	tios.c_cflag |= CS8|CREAD|HUPCL;
	tios.c_iflag = IGNBRK;
	if (ioctl(fd, TCSETA, (caddr_t)&tios) < 0) {
		syslog(LOG_ERR, "ioctl (TCSETA)");
		exit(1);
	}

	/* push the SLIP module */
	if (ioctl(fd, I_PUSH, "slip") < 0)
	{
		syslog(LOG_ERR, "ioctl (I_PUSH)");
		exit(1);
	}
#else
	sgtty.sg_flags = RAW | ANYP;
	sgtty.sg_ispeed = sgtty.sg_ospeed = speed;
	if (ioctl(fd, TIOCSETP, &sgtty) < 0) {
		perror("ioctl(TIOCSETP)");
		exit(1);
	}
	if (ioctl(fd, TIOCSETD, &slipdisc) < 0) {
		perror("ioctl(TIOCSETD)");
		exit(1);
	}
#endif

	if (fork() > 0)
		exit(0);
	for (;;)
		sigpause(0L);
}



struct sg_spds {
	int sp_val, sp_name;
}       spds[] = {
	{50,	B50 },
	{75,	B75 },
	{110,	B110 },
	{134,	B134 },
	{150,	B150 },
	{200,	B200 },
	{300,	B300 },
	{600,	B600 },
	{1200,	B1200 },
	{1800,	B1800 },
	{2400,	B2400 },
	{4800,	B4800 },
	{9600,	B9600 },
	{19200, B19200 },
	{38400,	B38400 },
	{0,	0 }
};


findspeed(speed)
	register int speed;
{
	register struct sg_spds *sp;

	sp = spds;
	while (sp->sp_val && sp->sp_val != speed)
		sp++;
	return (sp->sp_name);
}
