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
#ident	"$Header: sysconinit.c,v 1.4.1.5 90/06/06 12:08:57 wje Exp $"
/*
 * $Header: sysconinit.c,v 1.4.1.5 90/06/06 12:08:57 wje Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/errno.h>
#include <sys/signal.h>
#include <sys/grafreg.h>
#include <sys/nvram.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/mipskopt.h>
#include <bsd43/machine/hwconf.h>
#include <bsd43/sys/syscall.h>
#include <bsd43/sys/sysmips.h>
#include <sys/cpu_board.h>

#define TTYKEYBD	"/dev/ttykeybd"
#define TTY0		"/dev/tty0"
#define TTY1		"/dev/tty1"
#define CONSOLE_DEFAULT	'l'
#define LOGFILE	"/tmp/sysconinit.log"

#define bsd43_sysmips(x,y,z,q) syscall(BSD43_SYS_sysmips,(x),(y),(z),(q))
#define bsd43_mips_hwconf(x,y) bsd43_sysmips(BSD43_MIPS_HWCONF,(x),(y),0)
#define get_hwconf(x) bsd43_mips_hwconf(BSD43_HWCONF_GET,(x))

struct	bsd43_hw_config conf;	/* configuration - what machine we are etc.. */
int	debug = 0;
char 	*consolename = TTYKEYBD;/* name to link console to */
char	*console_default;
FILE	*logfile;
extern int errno;

main (argc, argv)
int argc;
char **argv;
{
	unsigned char consoletype;		/* t,0,1,l,r, etc. */
	unsigned long display_only = 0;
	int	i;
	
	/* -n arg says show what we'd do but do not do it */
	if (argc > 1 && strcmp(argv[1], "-n") == 0) {
		display_only = 1;
		argv++;
		argc--;
	} /* if */

	if ((argc > 1) && (strcmp(argv[1], "-d") == 0)) {
		debug = 1;
		argv++;
		argc--;
	}

	/*
	 *  Read console variable from kernel and set
	 *  consolename to appropriate name.
	 */
	consoletype = get_consoletype();

	if (debug)
		printf("Got console as '%c'\n", consoletype);
	
	/* if the consoletype is the default and there is an argument
	 *	to sysconinit let it override.
	 */
	if (consoletype == CONSOLE_DEFAULT && argc == 2) {
	    set_consolename(argv[1][0]);
	} else {
	    set_consolename(consoletype);
	} /* if */

	/*
	 * Check for video board.  If not present, ignore kernel
	 * setting for console, force linking to TTYFORCE.
	 */
	if (strcmp(consolename, TTYKEYBD) == 0) {
		int fd;

		if ((fd = open(TTYKEYBD, O_RDWR)) < 0) {
			if (debug) {
				perror("Failed to open keyboard device");
			}
			consolename = console_default;
		} else {
			close(fd);
		}
		switch(conf.cpubd_type) {
		case BRDTYPE_R3030:
			if (shmget(GREGKEY, R3030_GRAPHICS_REG_SIZE, 0666) < 0) {
				if (debug)
					perror("Failed GREGKEY");
				if (shmget(GBMONCH, MONO_FRAME_SIZE, 0666) < 0) {
					if (debug)
						perror("Failed to open either Frame Buffer");
					consolename = console_default;
				}
			}
			break;

		default:
			if (shmget(GREGKEY, GRAPHICS_REG_SIZE, 0666) < 0) {
				if (debug)
					perror("Failed to open Frame Buffer");
				consolename = console_default;
			}
			break;
		}
	}
	if (display_only) {
	    openlog();
	    fprintf(logfile,"type = %c; link /dev/console to %s\n", consoletype, 
		consolename);
	} else {
	    unlink("/dev/console");	link(consolename, "/dev/console");
	    unlink("/dev/syscon");	link("/dev/console", "/dev/syscon");
	    unlink("/dev/systty");	link("/dev/console", "/dev/systty");
	} /* if */
	exit(0);
} /* main */


get_consoletype()
{
	int	i;
	int	memfd, addr;
	char	con_val;

	if (get_hwconf(&conf) == -1) {
		perror("hwconf");
		exit(1);
	}
	switch (conf.cpubd_type) {
	case BRDTYPE_R3030:
		console_default = TTY1;
		con_val = '1';
		break;

	default:
		console_default = TTY0;
		con_val = '0';
		break;
	}
	errno = 0;
	addr = bsd43_sysmips(BSD43_MIPS_KOPT, "console", 0, KOPT_GET);
	if (errno != 0) {
	    openlog();
	    fprintf(logfile,"sysconinit: kopting console var (errno=%d)",errno);
	    exit(2);
	}
	if ((memfd = open("/dev/kmem", O_RDONLY)) < 0) {
	    	openlog();
		fprintf(logfile,"sysconinit: opening /dev/kmem (errno=%d)",
									errno);
		exit(2);
	}
	if ( lseek(memfd,(long)(addr&0x7fffffff),0)  == -1 ) {
		fprintf(logfile,
			"sysconinit: seeking for 'console' value (errno=%d)",
									errno);
		exit(2);
	}
	if ( read(memfd, &con_val, (int)sizeof(con_val)) !=
				(int)sizeof(con_val) ) {
	    	openlog();
		fprintf(logfile,
			"sysconinit: reading 'console' value (errno=%d)",
									errno);
		exit(2);
	}
	return con_val;
}

set_consolename(consoletype)
unsigned char consoletype;
{
	switch (consoletype) {
	case 'T':
	case 't':
	case '1':
		consolename = TTY1;
		break;
	case 'r':
	case '0':
		consolename = TTY0;
		break;

	default:
	    	openlog();
		fprintf(logfile,
		"sysconinit: warning: '%c' unknown console setting, using 'l'",
		    consoletype);
		/* fall through */
	case 'a':
	case 'l':
	case 'g':
	case 'v':
	case 'm':
	case 'c':
		consolename = TTYKEYBD;
		break;
	}
}

/*
 *  Open log file if not already open.
 */
unsigned logfileopen = 0;
openlog()
{
	if ( !logfileopen ) {
		/*
	 	 *  Open log file to log errors and -n information.
	 	 */
		logfile = fopen(LOGFILE,"w");
		logfileopen++;
	}
}
