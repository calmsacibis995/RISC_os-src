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
#ident	"$Header: slconfig.c,v 1.3.1.2 90/05/09 17:23:43 wje Exp $"

/*
 *
 * This program initializes the specified (or its own) tty port to be 
 * an async TCP/IP interface.  It merely sets up the SLIP module all
 * by its lonesome on the STREAMS stack, initializes the network 
 * interface, and pauses forever waiting for hangup.
 *
 */

#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#include <pwd.h>

#include <bsd/sys/socket.h>
#include <stropts.h>
#include <termio.h>
#include <bsd/net/soioctl.h>
#include <fcntl.h>
#include <sys/slip.h>
#include <bsd/syslog.h>

#include <bsd/netinet/in.h>
#include <bsd/net/if.h>
#include <bsd/netdb.h>
#include <bsd/sys/ioctl.h>


char	*Accessfile = "/etc/hosts.slip";

extern char *malloc(), *ttyname();
extern struct passwd *getpwuid();
char	Usage[] = "Usage: %s [-t ttyname] [-b baudrate] [-c command] {localaddr dstaddr [mask] | name}\n";

int	debugflg;
char	*dstaddr, *localaddr, *netmask;
char	devname[32];

#define	MAXMODULES	128
int	modcnt = 0;				/* # popped modules */
char	*modules[MAXMODULES];		/* pointer to popped module names */
char	modname[1024];

#define	SLIPMOD	"slip"

int	slipfd;	 			/* file descriptor for TTY line */
struct	termio	otios;			/* OLD tty state */
int	unit;				/* SLIP unit# */

sigcatch(signo)
{
	/* When we receive a signal, restore TTY line state and 
	 * exit.  The exit value will be 128 + signo.
	 */

	restore_state(slipfd, &otios);
	exit(128+signo);
}

main(argc, argv)
int argc;
char *argv[];
{
	int	c, errflg = 0;
	extern	int	optind;
	extern	char	*optarg;
	int	pgrp, opgrp;
	int	fd = 0;

	int	i, s;
	struct	termio	tios, otios;
	struct	ifreq ifr;

	int	speed = 0;
	char	*cmd = NULL;

	s = getdtablesize();
	for (i = 3 ; i < s ; i++)
		close(i);

	while ((c = (getopt(argc, argv, "#t:b:c:"))) != EOF)
	    switch(c) {
		case '#':
			debugflg++;
			break;

		case 't':
			if (strncmp("/dev/", optarg, 5) != 0) 
			    sprintf(devname, "/dev/%s", optarg);
			else
			    strcpy(devname, optarg);
			fd = open(devname, O_RDWR|O_NDELAY);
			if (fd < 0) {
			    prerror("Can't open %s\n", optarg);
			    exit(1);
			}
			if (speed == 0)
				speed = B9600;
			break;

		case 'b':
			speed = findspeed(atoi(optarg));
			if (speed <= 0) {
			    fprintf(stderr, "unknown speed %s\n", optarg);
			    exit(1);
			}
			break;

		case 'c':
			cmd = optarg;
			break;

		default:
			errflg++;
			continue;
	}
	if(errflg){
		fprintf(stderr, Usage, argv[0]);
		exit(1);
	}

	if (!isatty(fd)) {
		/* Trying to set up slip over non-serial interface */
		fprintf(stderr, "Not a tty.\n");
		exit(1);
	}
	slipfd = fd;

	/* Make sure that we don't already have a SLIP module pushed on
	 * this stream.
	 */
	if (ioctl(fd, I_FIND, SLIPMOD) == 1) {
		fprintf(stderr,"SLIP already active on this serial line.\n");
		exit(1);
	}

	/* Get current state */
	if (ioctl(fd, TCGETA, (caddr_t)&tios) < 0) {
		prerror("slconfig: ioctl(TCGETA) failed\n");
		exit(1);
	}
	otios = tios;

	/* Set up signal handler to catch SIGHUP, SIGINT and SIGTERM
	 * signals.
	 */

	signal(SIGHUP, sigcatch);
	signal(SIGINT, sigcatch);
	signal(SIGTERM, sigcatch);

	openlog("slconfig", LOG_PID, LOG_USER);

	if (getuid() == 0) {
		switch (argc-optind) {
		    case 1:
			findid(argv[optind]);
			break;

		    case 2:
			localaddr = argv[optind];
			dstaddr = argv[optind+1];
			netmask = "default";
			break;

		    case 3:
			localaddr = argv[optind];
			dstaddr = argv[optind+1];
			netmask = argv[optind+2];
			break;

		    default:
			fprintf(stderr, Usage, argv[0]);
			exit(1);
		}
	} else
		findid((char *)0);

	fchmod(fd, 0600);

	/* pop all streams modules and remeber their names.  */
	for (modcnt = 0; ioctl(fd, I_LOOK, modname) != -1; modcnt++) {
	    if ((modules[modcnt] = malloc(strlen(modname)+1)) == NULL) {
		prerror("Can't allocate %d bytes to save module name\n",
				strlen(modname)+1);
		restore_modules(fd);
		exit(1);
	    }
	    strcpy(modules[modcnt], modname);

	    if (debugflg)
		fprintf(stderr,"Popping module: %s\n", modules[modcnt]);

	    if (ioctl(fd, I_POP, 0) == -1) {
		prerror("ioctl(I_POP) failed. Top module: %s\n",modname);
		restore_modules(fd);
	    }
	}
		
	/* Set up new line parameters */
	if (speed)
	    tios.c_cflag = (char) speed;	/* set the new speed */
	else
	    tios.c_cflag &= CBAUD;		/* only save the speed */
	tios.c_cflag |= CS8|CREAD|HUPCL;
	tios.c_iflag = IGNBRK;
	if (ioctl(fd, TCSETA, (caddr_t)&tios) < 0) {
		prerror( "ioctl (TCSETA) failed\n");
		restore_modules(fd);
		exit(1);
	}

	/* Flush both input and output queues */
	{
	    int	arg = 0;
	    ioctl(fd, TIOCFLUSH, &arg);
	}

	/* push the SLIP module */
	if (ioctl(fd, I_PUSH, SLIPMOD) < 0) {
		prerror("ioctl (I_PUSH) of %s module failed\n",SLIPMOD);
		restore_state(fd, &otios);
		exit(1);
	}

	/* find out what unit number we were assigned */
	if (ioctl(fd, SLIOGUNIT, (caddr_t)&unit) < 0) {
		prerror("ioctl (SLIOGUNIT) failed.\n");
		restore_state(fd, &otios);
		exit(1);
	}

	syslog(LOG_INFO, "Attaching slip%d local: %s remote: %s mask: %s\n",
		unit, localaddr, dstaddr, netmask);

	/* set the local and remote interface addresses */
	s = socket(AF_INET, SOCK_DGRAM, 0);

	if (getuid() != 0 || (argc-optind) == 3) {
		(void) sprintf(ifr.ifr_name, "%s%d", SLIPIFNAME, unit);
		if (in_getaddr(netmask, &ifr.ifr_addr) < 0) {
			restore_state(fd, &otios);
			exit(1);
		}
		if (ioctl(s, SIOCSIFNETMASK, (caddr_t)&ifr) < 0) {
			prerror("ioctl (SIOCSIFNETMASK) failed.\n");
			restore_state(fd, &otios);
			exit(1);
		}
	}

	(void) sprintf(ifr.ifr_name, "%s%d", SLIPIFNAME, unit);
	if (in_getaddr(dstaddr, &ifr.ifr_addr) < 0) {
		restore_state(fd, &otios);
		exit(1);
	}
	if (ioctl(s, SIOCSIFDSTADDR, (caddr_t)&ifr) < 0) {
		prerror("ioctl (SIOCSIFDSTADDR) failed.\n");
		restore_state(fd, &otios);
		exit(1);
	}

	(void) sprintf(ifr.ifr_name, "%s%d", SLIPIFNAME, unit);
	if (in_getaddr(localaddr, &ifr.ifr_addr) < 0) {
		restore_state(fd, &otios);
		exit(1);
	}
	/* this has the side-effect of marking the interface up */
	if (ioctl(s, SIOCSIFADDR, (caddr_t)&ifr) < 0) {
		prerror("ioctl (SIOCSIFADDR) failed.\n");
		restore_state(fd, &otios);
		exit(1);
	}

	/* If no controlling terminal set, then set the controlling
	 * terminal to that of the terminal line.
	 */
	if (ioctl(fd, TIOCGPGRP, &opgrp) < 0) {
		prerror("ioctl(TIOCSPGRP) failed.\n");
		restore_state(fd, &otios);
		exit(1);
	}

	/* Set up new controlling terminal */
	pgrp = getpid();
	if (ioctl(fd, TIOCSPGRP, &pgrp) < 0) {
	    prerror("ioctl(TIOCSPGRP) failed.\n");
	    restore_state(fd, &otios);
	    exit(1);
	}

	if (cmd)
	    system(cmd);
	else
	    pause();

	/* restore state (both popped modules as well as the TTY state) */
	restore_state(fd, &otios);

	/* closing the descriptor should pop the slip module */
	exit(0);

}

restore_modules(fd)
int	fd;
{
	while (--modcnt >= 0) {
	    if (debugflg)
		fprintf(stderr,"Restoring module: %s\n", modules[modcnt]);

	    if (ioctl(fd, I_PUSH, modules[modcnt]) == -1) {
		prerror("Can't restore module %s\n", modules[modcnt]);
		return(-1);
	    }
	}
	return(0);
}

restore_state(fd, tiosp)
struct	termio	*tiosp;
{
	/* Pop SLIP module */
	if (ioctl(fd, I_POP, 0) != 0)
		printf(stderr, "Can't pop \"%s\" module.", SLIPMOD);
	else
	    syslog(LOG_INFO, "Detached slip%d\n", unit);

	/* Restore old popped modules */
	restore_modules(fd);

	/* Restore TTY state */
	if (ioctl(fd, TCSETA, (caddr_t)tiosp) < 0) {
		prerror("Can't restore serial line state.\n");
	}
}

findid(name)
	char *name;
{
	char buf[BUFSIZ];
	static char mode[16];
	static char laddr[16];
	static char raddr[16];
	static char mask[16];
	char user[16];
	FILE *fp;
	struct passwd *pw;
	int n;

	if (name == NULL && (pw = getpwuid(getuid())) == NULL) {
		fprintf(stderr, "Your UID (%d) is unknown\n", getuid());
		syslog(LOG_ERR, "UID (%d) is unknown\n", getuid());
		exit(1);
	} else if (name == NULL)
		name = pw->pw_name;
	if ((fp = fopen(Accessfile, "r")) == NULL) {
		perror(Accessfile);
		syslog(LOG_ERR, "%s: %m\n", Accessfile);
		exit(3);
	}
	while (fgets(buf, sizeof(buf) - 1, fp)) {
		if (ferror(fp))
			break;
		n = sscanf(buf, "%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s\n",
			user, mode, laddr, raddr, mask);
		if (user[0] == '#' || n != 5)
			continue;
		if (strcmp(user, name) == 0) {
			/* eventually deal with "mode" */
			localaddr = laddr;
			dstaddr = raddr;
			netmask = mask;
			fclose(fp);
			return 0;
		}
		if (feof(fp))
			break;
	}
	fputs("SLIP access denied\n", stderr);
	syslog(LOG_ERR, "SLIP access denied for %s\n", name);
	exit(4);
}

in_getaddr(s, saddr)
	char *s;
	struct sockaddr *saddr;
{
	register struct sockaddr_in *sin = (struct sockaddr_in *)saddr;
	struct hostent *hp;
	struct netent *np;
	int val;
	extern struct in_addr inet_makeaddr();
 
	bzero((caddr_t)saddr, sizeof *saddr);
	sin->sin_family = AF_INET;
	val = inet_addr(s);
	if (val != -1) {
		sin->sin_addr.s_addr = val;
		return(0);
	}
	hp = gethostbyname(s);
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (char *)&sin->sin_addr, hp->h_length);
		return(0);
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_family = np->n_addrtype;
		sin->sin_addr = inet_makeaddr(np->n_net, INADDR_ANY);
		return(0);
	}

	fprintf(stderr, "slconfig: inet_addr %s: bad value\n", s);
	return(-1);
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


prerror(fmt, arg1, arg2, arg3, arg4)
char	*fmt;
int	arg1, arg2, arg3, arg4;
{
	fprintf(stderr, fmt, arg1, arg2, arg3, arg4);
	perror("Error");
}
