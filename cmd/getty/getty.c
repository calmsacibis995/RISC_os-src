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
#ident	"$Header: getty.c,v 1.21.2.9.1.1.1.4 90/11/06 12:28:45 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	getty, uugetty - sets up speed, various terminal flags, */
/*			line discipline,			*/
/*	and waits for new prospective user to enter name, before	*/
/*	calling "login".						*/

#ifdef UUGETTY
#include	"uucp.h"

/*	This program, when compiled as uugetty, is the			*/
/*	standard getty modified						*/
/*	to allow a tty line to be used by uucp/cu/tip or a dial in.	*/
/*	For it to work with 801/212 dialers,				*/
/*	put an entry into inittab like					*/
/*	   30:2:respawn:/usr/lib/uucp/uugetty -t 60 ttymh4 1200		*/
/*	For direct lines, or intelligent modems use			*/
/*	   30:2:respawn:/usr/lib/uucp/uugetty -r -t 60 ttymh4 9600	*/
/*	   When this line is used, the Systems file that is		*/
/*	   used to call into it must have the following script		*/
/*	    "" \r\d\r\d\r\d\r in:--in: ...				*/
/*	   This is because the uugetty expects to read a character	*/
/*	   before the login message is output.				*/

/*	It will only work on systems that permit kill(0, pid).		*/
/*	This scheme was proposed by Larry Wehr and works as follows:	*/
/*	     The fopen of the line hangs until carrier is detected.	*/
/*	At this point, uugetty attempts to create a LCK..line file	*/
/*	If if fails, that means that uucico or cu are using the line.	*/
/*	  In this case, just do a busy wait, periodically checking	*/
/*	  for the LCK..line file.  When it goes away, then exit and	*/
/*	  a new uugetty will be spawned.				*/
/*	If it succeeds, then this is someone logging in--do normal	*/
/*	  getty processing.						*/
/*	NOTE:								*/
/*	  When the person hangs up (login case) the LCK..line file	*/
/*	will remain, but this is ok because the ulockf() function	*/
/*	used by uucico and uugetty and cu check the pid in the LCK	*/
/*	file and if it doesn't exist, the LCK file is removed when	*/
/*	it is needed.							*/
/*	  Also, uugetty always sets the owner of the line to uucp.	*/
/*	This is so that uucico/cu/tip can get at the line.  (cu and tip	*/
/*	must run setuid uucp--or this will not work.)			*/
/*									*/
/*	There is an additional option for direct lines and lines	*/
/*	that have intelligent modems (ones that return on open		*/
/*	immediately.  The -r option means wait for one character	*/
/*	before putting out the login message.  If the character		*/
/*	comes in, then check the LCK file and proceed as above.		*/

#endif UUGETTY

/*									*/
/*	Usage:	getty [-h] [-t time] line speed_label terminal		*/
/*		    line_disc						*/
/*									*/
/*	Usage:	uugetty  [-r] [-h] [-t time] line speed_label terminal	*/
/*									*/
/*	-h says don't hangup by dropping carrier during the		*/
/*		initialization phase.  Normally carrier is dropped to	*/
/*		make the dataswitch release the line.			*/
/*	-t says timeout after the number of seconds in "time" have	*/
/*		elapsed even if nothing is typed.  This is useful	*/
/*		for making sure dialup lines release if someone calls	*/
/*		in and then doesn't actually login in.			*/
/*	-r says wait for a character before putting out login message	*/
/*		(This is for uugetty.c with intelligent modems		*/
/*	"line" is the device in "/dev".					*/
/*	"speed_label" is a pointer into the "/etc/getty_defs"		*/
/*			where the definition for the speeds and		*/
/*			other associated flags are to be found.		*/
/*	"terminal" is the name of the terminal type.			*/
/*	"line_disc" is the name of the line discipline.			*/
/*									*/
/*	Usage:  getty -c gettydefs_like_file				*/
/*	Usage:  uugetty -c gettydefs_like_file				*/
/*									*/
/*	The "-c" flag is used to have "getty" check a gettydefs file.	*/
/*	"getty" parses the entire file and prints out its findings so	*/
/*	that the user can make sure that the file contains the proper	*/
/*	information.							*/
/*									*/

#ifdef UUGETTY
/* dummies for using uucp .o routines */
void assert(){}
cleanup(){}
void logent(){}		/* so we can load ulockf() */
#endif UUGETTY

#ifdef UUGETTY
#define GETTY_NAME "uugetty"
#define GETTY_UID UUCPUID
#else UUGETTY
#define GETTY_NAME "getty"
#define GETTY_UID 0
#endif UUGETTY

#undef HANGUP
#define HANGUP

#ifdef RISCOS

#undef VHANGUP
#define VHANGUP

#endif RISCOS

#ifdef UUGETTY
/* undefine DEBUG - it is defined in uucp.h */
#undef DEBUG
#endif UUGETTY

#ifndef UUGETTY
#include	<stdio.h>
#ifdef RISCOS
#include	<sys/types.h>
#endif RISCOS
#include	<sys/param.h>
#include	<termio.h>
#include	<signal.h>
#include	<sys/stat.h>
#endif UUGETTY
#include	<utmp.h>
#include	<sys/utsname.h>
#ifndef UUGETTY
#include	<ctype.h>
#include	<fcntl.h>
#endif UUGETTY
#include	<unistd.h>      /* For locking utmp file */
#include	<sys/crtctl.h>

#if	RISCOS
#include	<bsd/syslog.h>
#ifdef VHANGUP
#include	<bsd43/sys/syscall.h>
#define	vhangup() syscall(BSD43_SYS_vhangup)
#endif VHANGUP

#include <grp.h>

extern struct group *getgrnam();

#define TTYGRPNAME	"tty"		/* name of group to own ttys */

#endif	RISCOS

#ifndef UUGETTY
#define		TRUE		1
#define		FALSE		0
#endif UUGETTY
#define		FAILURE		(-1)

#ifndef UUGETTY
#define		SUCCESS		0
#endif UUGETTY
#define		ID		1
#define		IFLAGS		2
#define		FFLAGS		3
#define		MESSAGE		4
#define		NEXTID		5

#define		ACTIVE		1
#define		FINISHED	0

#define		ABORT		CINTR		/* Delete */
#define		QUIT		CQUIT
#define		ERASE		CERASE
#define		BACKSPACE	'\b'
#define		KILL		CKILL

#ifndef	ESC
#define		ESC		27
#endif
#ifndef	CS
#define		CS		'H',ESC,'J'
#endif

#ifdef OSS
/*	The following three characters are the standard OSS erase,	*/
/*	kill, and abort characters.					*/

#define		STDERASE	'_'
#define		STDKILL		'$'
#define		STDABORT	'&'
#endif

#ifndef CTRL
#define	CTRL(X)	('X' & 037)
#endif

#define		GOODNAME	1
#define		NONAME		0
#define		BADSPEED	(-1)

#ifndef		fioctl
#define		fioctl(x,y,z)	ioctl(fileno(x),y,z)
#endif

struct Gdef {
	char	*g_id;			/* identification for modes & speeds */
	struct termio	g_iflags;	/* initial terminal flags */
	struct termio	g_fflags;	/* final terminal flags */
	char	*g_message;		/* login message */
	char	*g_nextid;		/* next id if this speed is wrong */
};

#define	MAXIDLENGTH	15	/* Maximum length the "g_id" and "g_nextid"
				 * strings can take.  Longer ones will be
				 * truncated.
				 */
#define OMAXMESS	79	/* old max message length */
#define MAXMESSAGE	512	/* Maximum length the "g_message" string
				 * can be.  Longer ones are truncated.
				 */

/*	Maximum length of line in /etc/gettydefs file and the maximum	*/
/*	length of the user response to the "login" message.		*/

#define OMAXLINE	255				/* original MAXLINE */
#define	MAXLINE		OMAXLINE-OMAXMESS+MAXMESSAGE    /* 688 */
#define	MAXARGS		64	/* Maximum number of arguments that can be
				 * passed to "login"
				 */

struct Symbols {
	char		*s_symbol;	/* Name of symbol */
	unsigned	s_value	;	/* Value of symbol */
};

/*	The following four symbols define the "SANE" state.		*/

#define	ISANE	(BRKINT|IGNPAR|ISTRIP|ICRNL|IXON)
#define	OSANE	(OPOST|ONLCR)
#define	CSANE	(CS8|CREAD)
#define	LSANE	(ISIG|ICANON|ECHO|ECHOE|ECHOK)

/*	Modes set with the TCSETAW ioctl command.			*/

struct Symbols imodes[] = {
	"IGNBRK",	IGNBRK,
	"BRKINT",	BRKINT,
	"IGNPAR",	IGNPAR,
	"PARMRK",	PARMRK,
	"INPCK",	INPCK,
	"ISTRIP",	ISTRIP,
	"INLCR",	INLCR,
	"IGNCR",	IGNCR,
	"ICRNL",	ICRNL,
	"IUCLC",	IUCLC,
	"IXON",		IXON,
	"IXANY",	IXANY,
	"IXOFF",	IXOFF,
	NULL,		0
};

struct Symbols omodes[] = {
	"OPOST",	OPOST,
	"OLCUC",	OLCUC,
	"ONLCR",	ONLCR,
	"OCRNL",	OCRNL,
	"ONOCR",	ONOCR,
	"ONLRET",	ONLRET,
	"OFILL",	OFILL,
	"OFDEL",	OFDEL,
	"NLDLY",	NLDLY,
	"NL0",		NL0,
	"NL1",		NL1,
	"CRDLY",	CRDLY,
	"CR0",		CR0,
	"CR1",		CR1,
	"CR2",		CR2,
	"CR3",		CR3,
	"TABDLY",	TABDLY,
	"TAB0",		TAB0,
	"TABS",		TAB0,
	"TAB1",		TAB1,
	"TAB2",		TAB2,
	"TAB3",		TAB3,
	"BSDLY",	BSDLY,
	"BS0",		BS0,
	"BS1",		BS1,
	"VTDLY",	VTDLY,
	"VT0",		VT0,
	"VT1",		VT1,
	"FFDLY",	FFDLY,
	"FF0",		FF0,
	"FF1",		FF1,
	NULL,	0
};

struct Symbols cmodes[] = {
	"B0",		B0,
	"B50",		B50,
	"B75", 		B75,
	"B110",		B110,
	"B134",		B134,
	"B150",		B150,
	"B200",		B200,
	"B300",		B300,
	"B600",		B600,
	"B1200",	B1200,
	"B1800",	B1800,
	"B2400",	B2400,
	"B4800",	B4800,
	"B9600",	B9600,
#ifdef EXTA
	"EXTA",		EXTA,
#endif
#ifdef EXTB
	"EXTB",		EXTB,
#endif
#ifdef B19200
	"B19200",	B19200,
#endif
#ifdef B38400
	"B38400",	B38400,
#endif
	"CS5",		CS5,
	"CS6",		CS6,
	"CS7",		CS7,
	"CS8",		CS8,
	"CSTOPB",	CSTOPB,
	"CREAD",	CREAD,
	"PARENB",	PARENB,
	"PARODD",	PARODD,
	"HUPCL",	HUPCL,
	"CLOCAL",	CLOCAL,
	"RTSCTS",	CNEW_RTSCTS,
	"MDMBUF",	CNEW_MDMBUF,
	NULL,		0
};

struct Symbols lmodes[] = {
	"ISIG",		ISIG,
	"ICANON",	ICANON,
	"XCASE",	XCASE,
	"ECHO",		ECHO,
	"ECHOE",	ECHOE,
	"ECHOK",	ECHOK,
	"ECHONL",	ECHONL,
	"NOFLSH",	NOFLSH,
	"CTLECH",	LNEW_CTLECH,
	"CTLECHO",	LNEW_CTLECH,
	"PRTERA",	LNEW_PRTERA,
	"PRTERASE",	LNEW_PRTERA,
	"FLUSHO",	LNEW_FLUSHO,
	"CRTBS",	LNEW_CRTBS,
	"PENDIN",	LNEW_PENDIN,
	"TOSTOP",	TOSTOP,
	NULL,		0
};

/*	Terminal types set with the LDSETT ioctl command.		*/

struct Symbols terminals[] = {
	"none",		TERM_NONE,
#ifdef	TERM_V10
	"vt100",	TERM_V10,
#endif
#ifdef	TERM_H45
	"hp45",		TERM_H45,
#endif
#ifdef	TERM_C10
	"c100",		TERM_C100,
#endif
#ifdef	TERM_TEX
	"tektronix",	TERM_TEX,
	"tek",		TERM_TEX,
#endif
#ifdef	TERM_D40
	"ds40-1",	TERM_D40,
#endif
#ifdef	TERM_V61
	"vt61",		TERM_V61,
#endif
#ifdef	TERM_TEC
	"tec",		TERM_TEC,
#endif
	NULL,		0
};

/*	Line disciplines set by the TIOCSETD ioctl command.		*/

#ifndef	LDISC0
#define	LDISC0		0
#endif

struct Symbols linedisc[] = {
	"LDISC0",	LDISC0,
	"LDISC1",	LDISC1,
	"LDISC_OLD",	LDISC0,
	"LDISC2",	LDISC_NEW,
	"LDISC_NEW",	LDISC_NEW,
	NULL,		0
};

/*	If the /etc/gettydefs file can't be opened, the following	*/
/*	default is used.						*/

struct Gdef DEFAULT = {
	"default",
	{ ICRNL,0,B300+CREAD+HUPCL,0,
	  LDISC0,
	  { ABORT,QUIT,ERASE,KILL,
	    CNUL,CNUL,CNUL,CNUL,
	    CSTART, CSTOP, CDEL, CDEL, 
	    CRPRNT, CFLUSH, CWERASE, CESC,
	    CDEL, CDEL, CDEL, CDEL } },
	{ ICRNL,OPOST+ONLCR+NLDLY+TAB3,B300+CS7+CREAD+HUPCL,
	  ISIG+ICANON+ECHO+ECHOE+ECHOK,
	  LDISC0,
	  { ABORT,QUIT,ERASE,KILL,
	    CNUL,CNUL,CNUL,CNUL,
	    CSTART, CSTOP, CDEL, CDEL, 
	    CRPRNT, CFLUSH, CWERASE, CESC,
	    CDEL, CDEL, CDEL, CDEL } },
	"LOGIN: ",
	"default"
};

#ifndef	DEBUG
char	*CTTY		=	"/dev/syscon";
#else
char	*CTTY		=	"/dev/sysconx";
#endif

char	*ISSUE_FILE	=	"/etc/issue";
char	*GETTY_DEFS	=	"/etc/gettydefs";

int 	check = {
	FALSE
};
char	*checkgdfile;		/* Name of gettydefs file during
				 * check mode.
				 */

main(argc,argv)
int argc;
char **argv;
{
	char *line;
	register struct Gdef *speedef;
	char oldspeed[MAXIDLENGTH+1],newspeed[MAXIDLENGTH+1];
	extern struct Gdef *find_def();
	int termtype,lined;
	extern char *ISSUE_FILE,*GETTY_DEFS;
	extern int check;
	int hangup,timeout;
	extern char *checkgdfile;
	extern struct Symbols *search(),terminals[],linedisc[];
	extern int timedout();
	register struct Symbols *answer;
	char user[MAXLINE],*largs[MAXARGS],*ptr,buffer[MAXLINE];
	int wait_read = FALSE;	/* wait for read before output "login" */
	FILE *fp;
	FILE *fdup();
	struct utsname utsname;
	struct termcb termcb;
	struct termio termio;
	static char clrscreen[] = {
		ESC,CS
	};

#ifdef RISCOS
#define TZ_LEN 20
	FILE *tz_fp = NULL;
	char buf[TZ_LEN+1], comm[TZ_LEN+4];
	char *getenv();
	int i;

	/* Get timezone info so syslog will record correct time */
	if (getenv("TZ") == NULL) {
		if ((tz_fp=fopen("/etc/TZ","r")) == NULL) {
        		syslog(LOG_ERR, "WARNING: Can't open '/etc/TZ'");
			syslog(LOG_ERR, "   Will not be able to set timezone");
		} else {
			for (i=0; i<TZ_LEN; i++) {/* read until space or EOLN */
				if ((buf[i]=fgetc(tz_fp)) == EOF) break;
				if (isspace(buf[i]) || buf[i]=='\n') break;
			}
			buf[i] = '\0';
			if (i == 0) {
				syslog(LOG_ERR, "WARNING: Read err on /etc/TZ");
			} else {
				buf[TZ_LEN] = '\0';
				strcpy(comm,"TZ=");
				strcat(comm, buf);
				putenv(comm);
			}
			fclose(tz_fp);
		}
	}
#undef TZ_LEN
#endif	
	signal(SIGINT,SIG_IGN);
	signal(SIGQUIT,SIG_DFL);
#ifdef RISCOS
	signal(SIGTSTP,SIG_IGN);
	signal(SIGTTIN,SIG_IGN);
	signal(SIGTTOU,SIG_IGN);
#endif RISCOS

#ifdef	RISCOS
	open("/dev/null",O_RDWR);
	open("/dev/null",O_RDWR);
	open("/dev/null",O_RDWR);
	openlog(GETTY_NAME, LOG_ODELAY|LOG_CONS, LOG_AUTH);
#endif	RISCOS

	hangup = TRUE;
	timeout = 0;
	while(--argc && **++argv == '-') {
		for(ptr = *argv + 1; *ptr;ptr++) switch(*ptr) {
		case 'h':
			hangup = FALSE;
			break;
#ifdef UUGETTY
		case 'r':
			wait_read = TRUE;
			break;
#endif UUGETTY
		case 't':
			if(isdigit(*++ptr)) {
				sscanf(ptr,"%d",&timeout);

/* Advance "ptr" so that it is pointing to the last digit of the */
/* timeout argument. */
				while(isdigit(*++ptr));
				ptr--;
			} else if(--argc) {
				if(isdigit(*(ptr = *++argv)))
					sscanf(ptr,"%d",&timeout);
				else error("timeout argument invalid. \"%s\"\n", *argv);
			}
			break;

/* Check a "gettydefs" file mode. */
		case 'c':
			signal(SIGINT,SIG_DFL);
			if(--argc == 0) {
				fprintf(stderr,
				    "Check Mode Usage: %s -c gettydefs-like-file\n",
					GETTY_NAME);
				exit(1);
			}
			check = TRUE;
			checkgdfile = *++argv;

/* Attempt to open the check gettydefs file. */
			if((fp = fopen(checkgdfile,"r")) == NULL) {
				fprintf(stderr,"Cannot open %s\n",checkgdfile);
				exit(1);
			}
			fclose(fp);

/* Call "find_def" to check the check file.  With the "check" flag */
/* set, it will parse the entire file, printing out the results. */
			find_def(NULL);
			exit(0);
		default:
			break;
		}
	}

/* There must be at least one argument.  If there isn't, complain */
/* and then die after 20 seconds.  The 20 second sleep is to keep */
/* "init" from working too hard. */
	if(argc < 1) {
		error("no terminal line specified.\n");
		sleep(20);
		exit(1);
	} else line = *argv;

/* If a "speed_label" was provided, search for it in the */
/* "getty_defs" file.  If none was provided, take the first entry */
/* of the "getty_defs" file as the initial settings. */
	if(--argc > 0 ) {
		if((speedef = find_def(*++argv)) == NULL) {
			error("unable to find %s in \"%s\".\n",
			    *argv,GETTY_DEFS);

/* Use the default value instead. */
			speedef = find_def(NULL);
		}
	} else speedef = find_def(NULL);

/* If a terminal type was supplied, try to find it in list. */
	if(--argc > 0) {
		if((answer = search(*++argv,terminals)) == NULL) {
			error("%s is an undefined terminal type.\n",
			    *argv);
			termtype = TERM_NONE;
		} else termtype = answer->s_value;
	} else termtype = TERM_NONE;

/* If a line discipline was supplied, try to find it in list. */
	if(--argc > 0) {
		if((answer = search(*++argv,linedisc)) == NULL) {
			error("%s is an undefined line discipline.\n",
			    *argv);
			lined = LDISC0;
		} else lined = answer->s_value;
	} else lined = LDISC0;

/* Perform "utmp" accounting. */
	account(line);

/* Attempt to open standard input, output, and error on specified */
/* line. */
	chdir("/dev");
	openline(line,speedef,termtype,lined,hangup,wait_read);

/* Loop until user is successful in requesting login. */
	for(;;) {

/* If there is no terminal type, just advance a line. */
		if(termtype == TERM_NONE) {

/* A bug in the stdio package requires that the first output on */
/* the newly reopened stderr stream be a putc rather than an */
/* fprintf. */
			putc('\r',stderr);
			putc('\n',stderr);

/* If there is a terminal type, clear the screen with the common */
/* crt language.  Note that the characters have to be written in */
/* one write, and hence can't go through standard io, which is */
/* currently unbuffered. */
		} else write(fileno(stderr),clrscreen,sizeof(clrscreen));

/* If getty is supposed to die if no one logs in after a */
/* predetermined amount of time, set the timer. */
		if(timeout) {
			signal(SIGALRM,timedout);
			alarm(timeout);
		}

#ifdef SYS_NAME
/* Generate a message with the system identification in it. */
		if (uname(&utsname) != FAILURE) {
			sprintf(buffer,"%.9s\r\n", utsname.nodename);

#ifdef	UPPERCASE_ONLY
/* Make all the alphabetics upper case. */
			for (ptr= buffer; *ptr;ptr++) *ptr = tolower(*ptr);
#endif
			fputs(buffer,stderr);
		}

/* Print out the issue file. */
		if ((fp = fopen(ISSUE_FILE,"r")) != NULL) {
			while ((ptr = fgets(buffer,sizeof(buffer),fp)) != NULL) {
				fputs(ptr,stderr);

/* In "raw" mode, a carriage return must be supplied at the end of */
/* each line. */
				putc('\r',stderr);
			}
			fclose(fp);
		}
#endif

/* flush input to get rid of noise */
		fioctl(stdin,TCGETA,&termio);
		fioctl(stdin,TCSETAF,&termio);

/* Print the login message. */
		fprintf(stderr,"%s",speedef->g_message);

/* Get the user's typed response and respond appropriately. */
		switch(getname(user,&termio)) {
		case GOODNAME:
			if (timeout) alarm(0);

/* If a terminal type was specified, keep only those parts of */
/* the gettydef final settings which were not explicitely turned */
/* on when the terminal type was set. */
			if (termtype != TERM_NONE) {
				termio.c_iflag |= (ISTRIP|ICRNL|IXON|IXANY)
				  | (speedef->g_fflags.c_iflag
				  & ~(ISTRIP|ICRNL|IXON|IXANY));
				termio.c_oflag |= (OPOST|ONLCR)
				  | (speedef->g_fflags.c_oflag
				  & ~(OPOST|ONLCR));
				termio.c_cflag = speedef->g_fflags.c_cflag;
				termio.c_lflag = (ISIG|ICANON|ECHO|ECHOE|ECHOK)
				  | (speedef->g_fflags.c_lflag
				  & ~(ISIG|ICANON|ECHO|ECHOE|ECHOK));
			} else {
				termio.c_iflag |= speedef->g_fflags.c_iflag;
				termio.c_oflag |= speedef->g_fflags.c_oflag;
				termio.c_cflag |= speedef->g_fflags.c_cflag;
				termio.c_lflag |= speedef->g_fflags.c_lflag;
			}
			termio.c_line = lined;
			fioctl(stdin,TCSETAW,&termio);

/* Parse the input line from the user, breaking it at white */
/* spaces. */
			largs[0] = "login";
#ifdef RISCOS
	/* -N is used by getty, uugetty to avoid the security problem
	 * where a user could login without giving a passwd if he types
	 * '-r fromhost user' at the getty generated login prompt, and
	 * fake the 'rlogin' handshake (remoteuser^@localuser^@termtype^@).
	 */
			largs[1] = "-N";
			parse(user,&largs[2],MAXARGS-2);
#else
			parse(user,&largs[1],MAXARGS-1);
#endif

/* Exec "login". */

#ifndef	DEBUG
			execv("/bin/login",largs);
			execv("/etc/login",largs);
			exit(1);
#else
			exit(0);
#endif

/* If the speed supplied was bad, try the next speed in the list. */
		case BADSPEED:

/* Save the name of the old speed definition incase new one is */
/* bad.  Copy the new speed out of the static so that "find_def" */
/* won't overwrite it in the process of looking for new entry. */
			strcpy(oldspeed,speedef->g_id);
			strcpy(newspeed,speedef->g_nextid);
			if ((speedef = find_def(newspeed)) == NULL) {
				error("pointer to next speed in entry %s is bad.\n",
				oldspeed);

/* In case of error, go back to the original entry. */
				if((speedef = find_def(oldspeed)) == NULL) {

/* If the old entry has disappeared, then quit and let next GETTY_NAME try. */
					error("unable to find %s again.\n",
						oldspeed);
					exit(1);
				}
			}

/* Setup the terminal for the new information. */
			setupline(speedef,termtype,lined);
			break;

/* If no name was supplied, not nothing, but try again. */
		case NONAME:
			break;
		}
	}
}

account(line)
char *line;
{
	register int ownpid;
	register struct utmp *u;
	extern struct utmp *getutent(), *pututline();
	register FILE *fp;
	extern int lockf() ;
	int i;

/* Look in "utmp" file for our own entry and change it to LOGIN. */
	ownpid = getpid();

	while ((u = getutent()) != NULL) {

/* Is this our own entry? */
		if (u->ut_type == INIT_PROCESS && u->ut_pid == ownpid) {
			strncpy(u->ut_line,line,sizeof(u->ut_line));
			strncpy(u->ut_user,"LOGIN",sizeof(u->ut_user));
			u->ut_type = LOGIN_PROCESS;

/* Write out the updated entry. */
			pututline(u);
			break;
		}
	}

/* If we were successful in finding an entry for ourself in the */
/* utmp file, then attempt to append to the end of the wtmp file. */
	if (u != NULL && (fp = fopen(WTMP_FILE,"r+")) != NULL) {
                for ( i=0 ; i<10 ; i++ ) {
                        if ( lockf(fileno(fp), F_TLOCK, 0) == -1 ) {
                                if ( i < 9 ) sleep (1) ;
                                else error("unable to lock accounting file.\n") ;
                        }
                        else {
                                fseek(fp,0L,2) ; /* Seek to end */
                                fwrite(u,sizeof(*u),1,fp) ;
                                rewind(fp) ;
                                (void) lockf(fileno(fp), F_ULOCK, 0) ;
                                i = 10 ;
                        }
                }
		fclose(fp);
	}

/* Close the utmp file. */
	endutent();
}

/*	"search" scans through a table of Symbols trying to find a	*/
/*	match for the supplied string.  If it does, it returns the	*/
/*	pointer to the Symbols structure, otherwise it returns NULL.	*/

struct Symbols *search(target,symbols)
register char *target;
register struct Symbols *symbols;
{

/* Each symbol array terminates with a null pointer for an */
/* "s_symbol".  Scan until a match is found, or the null pointer */
/* is reached. */
	for (;symbols->s_symbol != NULL; symbols++)
		if (strcmp(target,symbols->s_symbol) == 0) return(symbols);
	return(NULL);
}

error(format,arg1,arg2,arg3,arg4)
char *format;
int arg1,arg2,arg3,arg4;
{
	register FILE *fp;

#ifdef	RISCOS
	syslog(LOG_ERR, format, arg1, arg2, arg3, arg4);
	closelog();
#else	!RISCOS
	if ((fp = fopen(CTTY,"w")) == NULL) return;
	else {
		fprintf(fp,"%s: ",GETTY_NAME);
		fprintf(fp,format,arg1,arg2,arg3,arg4);
		fclose(fp);
	}
#endif	!RISCOS
}

debugmsg(format,arg1,arg2,arg3,arg4)
char *format;
int arg1,arg2,arg3,arg4;
{
#ifdef DEBUGMSG
	register FILE *fp;

#if	RISCOS
	syslog(LOG_DEBUG, format, arg1, arg2, arg3, arg4);
	closelog();
#else	!RISCOS
	if ((fp = fopen(CTTY,"w")) == NULL) return;
	else {
		fprintf(fp,"%s: ",GETTY_NAME);
		fprintf(fp,format,arg1,arg2,arg3,arg4);
		fclose(fp);
	}
#endif	!RISCOS
#endif DEBUGMSG
}

struct winsize win = { 0, 0, 0, 0 };

openline(line,speedef,termtype,lined,hangup,wait_read)
register char *line;
register struct Gdef *speedef;
int termtype,lined,hangup;
int	wait_read;
{
	register FILE *fpin,*fp;
	struct stat statb;
	extern int errno;
	register FILE *f;
	register int lfd;
	struct termio lterm;
	int	repcnt = 0;
#ifdef RISCOS
	int null_pgrp = 0;
	struct group *gr;
	char domain[256];
#endif RISCOS
#ifdef UUGETTY
	int	have_mlock = 0;
	char	buffer[MAXLINE];
#endif UUGETTY

	close(0);
	close(1);
	close(2);
	fclose(stdin);
	fclose(stdout);
	fclose(stderr);

/* Change the ownership of the terminal line to GETTY_UID and set */
/* the protections to only allow GETTY_UID to read the line. */
	stat(line,&statb);
#ifdef RISCOS
	/* Follow BSD 4.3 semantics wrt tty groups and write privileges
	 * only if the following two conditions are met.  A tty group
	 * must exist and the sentinel file must exist.  Otherwise, follow
	 * SysV semantics.
	 */
	gr = getgrnam(TTYGRPNAME);
	/* YP will have opened a socket that winds up on fd 0, messing up
	 * the tests for lfd being 0.
	 * YES, I KNOW THIS IS A KLUDGE!  (It beats a wholesale mod to
	 * the YP libs and making them slower and different from Sun.)
	 */
	getdomainname(domain,sizeof(domain));
	if (domain != NULL)
	    yp_unbind(domain);
	closelog();
	if (gr != (struct group *) NULL &&
	    access("/etc/getty.ttygroup.ok", 0) != -1) {
		chown(line,GETTY_UID,gr->gr_gid);
		chmod(line, 0620);
	} else 
#endif RISCOS
	{
		chown(line,GETTY_UID,statb.st_gid);
		chmod(line, 0622);
	};

	lfd = open(line,O_RDWR|O_NDELAY);
#ifdef UUGETTY
/* Code added for shared line - input/output		*/
/* For use by uucico/cu/ct				*/
/* Method: -- wait for open				*/
/*	When success, check to see if LCK..line exists	*/
/*	  If it does, that means that uucico/cu/ct is	*/
/*	  using the line, so busy wait and exit when	*/
/*	  the LCK..line file goes away			*/
/*	  If no LCK..line file, normal processing--	*/
/*	    close the line and use openline() as usual	*/
/* If the -r option, wait for first character also.	*/

	if (lfd == 0) {
		if (mlock(line)) { /*  There is a lock file already */
		    char lckname[MAXNAMESIZE];	/* lock file name LCK..line */

wait_for_mlock:
		    /* some process is using the line for output */
		    (void) close(0);
		    (void) close(1);
		    (void) close(2);
		    (void) sprintf(lckname, "%s.%s", LOCKPRE, line);
		    for (;;) {	/* busy wait for LCK..line to go away */
			sleep(60);
			if (checkLock(lckname) == 0) /* LCK..line gone */
			    break;
		    }
		    exit(0);
		}
		have_mlock = 1;
	};
#endif UUGETTY

#ifdef HANGUP
/* Make sure that DTR has been dropped and reasserted */
	if (lfd == 0
	    && hangup
	    && 0 <= ioctl(lfd,TCGETA,&lterm)
	    && (speedef->g_fflags.c_cflag & HUPCL)) {
	  	debugmsg("Dropping carrier on %s\n",line);
		sigset(SIGHUP, SIG_IGN);
		if (ioctl(lfd,TIOCNOTTY,0) == -1)
			error("TIOCNOTTY on %s returned error %d\n",line,errno);
		lterm.c_cflag &= ~CBAUD;
		lterm.c_cflag |= B0;
		if (ioctl(lfd,TCSETAF,&lterm) == -1) 
			error("TCSETAF on %s returned error %d\n",line,errno);
		sleep(2);
		(void)close(lfd);
		setpgrp(getpid());
#ifdef UUGETTY
		if (have_mlock) {
			delock(line);
			have_mlock = 0;
		};
#endif UUGETTY
#ifdef VHANGUP
		lfd = open(line,O_RDWR|O_NDELAY);
#else VHANGUP
		lfd = open(line,O_RDWR);	/* wait for carrier */
#endif VHANGUP
		if (lfd == -1) {
			error("open of %s after carrier drop returned error %d\n",line,errno);
		} else {
			debugmsg("open of %s after carrier drop returned fd %d\n",line,lfd);
#ifdef UUGETTY
			if (mlock(line))
				goto wait_for_mlock;
			have_mlock = 1;
#endif UUGETTY
		};
		sigset(SIGHUP, SIG_DFL);
	}
#endif

#ifdef VHANGUP
	if (lfd == 0) {
		signal(SIGHUP,SIG_IGN);
#ifdef RISCOS
		ioctl (lfd, TIOCSPGRP, &null_pgrp);
		/* try to turn off echo and input processing */
		if (ioctl(lfd,TCGETA,&lterm) != -1) {
			lterm.c_lflag &= ~(ISIG|ICANON|ECHO|ECHOE|ECHOK|ECHONL);
			ioctl(lfd,TCSETAF,&lterm);
		};
#ifdef UUGETTY
		if (have_mlock) {
			delock(line);
			have_mlock = 0;
		};
#endif UUGETTY

		lfd = open (line, O_RDWR);
		if (lfd == -1) {
			error("open of %s after TIOCSPGRP returned error %d\n",line,errno);
		} else {
			debugmsg("open of %s after TIOCSPGRP returned fd %d\n",line,lfd);
#ifdef UUGETTY
			if (mlock(line)) {
				sleep(3); /* time for ct to remove its lock, if ct */
				if (mlock(line))
					goto wait_for_mlock;
			};
			have_mlock = 1;
#endif UUGETTY
		}
#endif RISCOS
		vhangup();
#ifdef UUGETTY
		if (have_mlock) {
			delock(line);
			have_mlock = 0;
		};
#endif UUGETTY
		lfd = open(line,O_RDWR);
		if (lfd == -1) {
			error("open of %s after vhangup() returned error %d\n",line,errno);
		} else {
			debugmsg("open of %s after vhangup() returned fd %d\n",line,lfd);
#ifdef UUGETTY
			if (mlock(line))
				goto wait_for_mlock;
			have_mlock = 1;
#endif UUGETTY
		}
		close(0);
#ifdef RISCOS
		close (1);
		if (lfd == 2) {
			lfd = dup(2);
			close(2);
		}
#else RISCOS
		if (lfd == 1) {
			lfd = dup(1);
			close(1);
		};
#endif RISCOS
		signal(SIGHUP,SIG_DFL);
	};
#endif VHANGUP	

#ifdef UUGETTY
	if (lfd == 0) {
		if (wait_read) {
			  /* wait to read the first character */
			/* Set the terminal type and line discipline. */
			setupline(speedef,termtype,lined);
			/*
			 * Check for read failure or and EOT sent
			 * (EOT may come from a cu on the other side.)
			 * This code is to prevent the situation of
			 * "login" program getting started here while
			 * a uugetty is running on the other end of the
			 * line.
			 * NOTE: Cu on a direct line when ~. is encountered will
			 * send EOTs to the other side.  EOT=\004
			 */
			if ( read(0, buffer, 1) < 0
			  || *buffer == '\004') {
				(void) close(0);
				if (have_mlock) {
					delock(line);
					have_mlock = 0;
				};
				sleep(10);
				exit(0);
			}
		}

	}
#endif UUGETTY

	if (lfd != -1) {
		int	old_flags;

		old_flags = fcntl(lfd,F_GETFL,0);
		if (old_flags == -1) {
			error("F_GETFL on %s returned error %d\n",line,errno);
			goto no_fcntl;
		};
		if (fcntl(lfd, F_SETFL,old_flags & ~O_NDELAY) == -1)
			error("F_SETFL on %s returned error %d\n",line,errno);
	};
no_fcntl:
/* Attempt to open the line.  It should become "stdin".  If not, */
/* then close. */
	if (lfd != 0 || (f = fdopen(lfd, "r+")) == NULL) {
		error("cannot open \"%s\". errno=%d fd=%d\n",
			      line,errno,lfd);
		sleep(10);
		exit(1);
	}
	fdup(f);
	fdup(f);
	setbuf(stdin,NULL);
	setbuf(stdout,NULL);
	setbuf(stderr,NULL);

#ifdef RISCOS
	ioctl(0, TIOCSWINSZ, &win);
#endif RISCOS

/* Set the terminal type and line discipline. */
	setupline(speedef,termtype,lined);
}

timedout()
{
	exit(1);
}

setupline(speedef,termtype,lined)
register struct Gdef *speedef;
int termtype,lined;
{
	struct termio termio;
	struct termcb termcb;

/* Set the terminal type to "none", which will clear all old */
/* special flags, if a terminal type was set from before. */
	termcb.st_flgs = 0;
	termcb.st_termt = TERM_NONE;
	termcb.st_vrow = 0;
	fioctl(stdin,LDSETT,&termcb);
	termcb.st_termt = termtype;
	fioctl(stdin,LDSETT,&termcb);

/* Get the current state of the modes and such for the terminal. */
	fioctl(stdin,TCGETA,&termio);
	if (termtype != TERM_NONE) {

/* If there is a terminal type, take away settings so that */
/* terminal is "raw" and "no echo".  Also take away the orginal */
/* speed setting. */
		termio.c_iflag = 0;
		termio.c_cflag &= ~(CSIZE|PARENB|CBAUD);
		termio.c_cflag |= CS8|CREAD|HUPCL;
		termio.c_lflag &= ~(ISIG|ICANON|ECHO|ECHOE|ECHOK);

/* Add in the speed. */
		termio.c_cflag |= (speedef->g_iflags.c_cflag & CBAUD);
	} else {
		termio.c_iflag = speedef->g_iflags.c_iflag;
		termio.c_oflag = speedef->g_iflags.c_oflag;
		termio.c_cflag = speedef->g_iflags.c_cflag;
		termio.c_lflag = speedef->g_iflags.c_lflag;
	}

/* Make sure that raw reads are 1 character at a time with no */
/* timeout. */
	termio.c_cc[VMIN] = 1;
	termio.c_cc[VTIME] = 0;

/* Add the line discipline. */
	termio.c_line = lined;
	if (lined == LDISC_NEW) {
		if (termio.c_cc[V_LNEXT] == CESC)
			termio.c_cc[V_LNEXT] = CLNEXT;
		if (termio.c_cc[V_SUSP] == CDEL)
			termio.c_cc[V_SUSP] = CSUSP;
		if (termio.c_cc[V_DSUSP] == CDEL)
			termio.c_cc[V_DSUSP] = CDSUSP;
	}
	fioctl(stdin,TCSETAF,&termio);

#ifdef UUGETTY
/* Pause briefly while terminal settles. */
	sleep(1);
#endif UUGETTY
}

/*	"getname" picks up the user's name from the standard input.	*/
/*	It makes certain						*/
/*	determinations about the modes that should be set up for the	*/
/*	terminal depending upon what it sees.  If it sees all UPPER	*/
/*	case characters, it sets the IUCLC & OLCUC flags.  If it sees	*/
/*	a line terminated with a <linefeed>, it sets ICRNL.  If it sees	*/
/*	the user using the "standard" OSS erase, kill, abort, or line	*/
/*	termination characters ( '_','$','&','/','!' respectively)	*/
/*	it resets the erase, kill, and end of line characters.		*/

#ifndef CEOF
#define CEOF ('D' & 037)
#endif CEOF
#ifndef CNUL
#define CNUL 0
#endif CNUL

int getname(user,termio)
char *user;
struct termio *termio;
{
	register char *ptr,c;
	register int rawc;
	int upper,lower;

/* Get the previous modes, erase, and kill characters and speeds. */
	fioctl(stdin,TCGETA,termio);

/* Set the flags to 0 and the erase and kill to the standard */
/* characters. */
	termio->c_iflag &= ICRNL;
	termio->c_oflag = 0;
	termio->c_cflag = 0;
	termio->c_lflag &= ECHO;
	for (ptr= (char*)termio->c_cc; ptr < (char*)&termio->c_cc[NCC];)
		*ptr++ = CDEL;
	termio->c_cc[VINTR] = ABORT;
	termio->c_cc[VQUIT] = QUIT;
	termio->c_cc[VERASE] = ERASE;
	termio->c_cc[VKILL] = KILL;
	termio->c_cc[VEOF] = CEOF;
	termio->c_cc[VEOL] = CNUL;
	termio->c_cc[VEOL2] = CNUL;
	termio->c_cc[VSWTCH] = CNUL;
#ifdef RISCOS
	termio->c_cc[V_START] = CSTART;
	termio->c_cc[V_STOP] = CSTOP;
	termio->c_cc[V_RPRNT] = CRPRNT;
	termio->c_cc[V_FLUSH] = CFLUSH;
	termio->c_cc[V_WERAS] = CWERASE;
	termio->c_cc[V_LNEXT] = CESC;
	termio->c_line = LDISC0;
#endif RISCOS
	
	ptr = user;
	upper = 0;
	lower = 0;
	do {

/* If it isn't possible to read line, exit. */
		if ((rawc = getc(stdin)) == EOF)
			exit(0);

/* If a null character was typed, return 0. */
		if ((c = (rawc & 0177)) == '\0')
			return(BADSPEED);
		if (c == ERASE || c == BACKSPACE) {

#ifdef OSS
/* Store this character as the "erase" character. */
			termio->c_cc[VERASE] = c;
#endif OSS

/* If there is anything to erase, erase a character. */
			if (ptr > user) {
				--ptr;
				if (c == BACKSPACE) {
				    if (*ptr >= ' ' && *ptr < 127) {
					if ((termio->c_lflag & ECHO) == 0)
						putc(rawc,stdout);
					printf(" \b");
				    };
				} else  {
				/* Echo the character if ECHO is off. */
				    if( (termio->c_lflag&ECHO) == 0 )
					putc(rawc,stdout);
				}
			};
		}

#ifdef OSS
		else if (c == STDERASE) {
			/* Echo the character if ECHO is off. */
			if( (termio->c_lflag&ECHO) == 0 )
				putc(rawc,stdout);

			if (ptr > user) --ptr;

/* Set up the "standard OSS" erase, kill, etc. characters. */
			termio->c_cc[VINTR] = STDABORT;
			termio->c_cc[VERASE] = STDERASE;
			termio->c_cc[VKILL] = STDKILL;
			termio->c_cc[VEOL] = '/';
			termio->c_cc[VEOL2] = '!';
		}
#endif

/* If the character is a kill line or abort character, reset the */
/* line. */
		else if (c == KILL || c == ABORT || c == CTRL(U)
		  || c == CTRL(X)) {
			if (c != KILL) {
				while (ptr > user) {
					ptr--;
					if (*ptr >= ' ' && *ptr < 127)
						printf("\b \b");
				};
			} else {
				/* Echo the character if ECHO is off. */
				if( (termio->c_lflag&ECHO) == 0 )
					putc(rawc,stdout);
				ptr = user;
				fputs("\r\n",stdout);
			}

/* Make sure the erase, kill, etc. are set to the UNIX standard. */
			termio->c_cc[VINTR] = ABORT;
			termio->c_cc[VERASE] = ERASE;
			termio->c_cc[VKILL] = KILL;
			termio->c_cc[VEOL] = CNUL;
			termio->c_cc[VEOL2] = CNUL;
		}

#ifdef OSS
		else if (c == STDKILL || c == STDABORT) {
			ptr = user;
			fputs("\r\n",stdout);

/* Set up the "standard OSS" erase, kill, etc. characters. */
			termio->c_cc[VINTR] = STDABORT;
			termio->c_cc[VERASE] = STDERASE;
			termio->c_cc[VKILL] = STDKILL;
			termio->c_cc[VEOL] = '/';
			termio->c_cc[VEOL2] = '!';
		}
#endif

		else {
			if ((termio->c_lflag&ECHO) == 0)
				putc(rawc,stdout);
			if (islower(c))
/* If the character is lower case, increment the flag for lower case. */
				lower++;
			else if (isupper(c))
/* If the character is upper case, increment the flag. */
				upper++;
			*ptr++ = c;
		}
	}

/* Continue the above loop until a line terminator is found or */
/* until user name array is full. */

#ifdef OSS
	while (c != '\n' && c != '\r' && c != '/' && c != '!' &&
	    ptr < (user + MAXLINE))
		;
#else
	while (c != '\n' && c != '\r' && ptr < (user + MAXLINE))
		;
#endif
			/* Remove the last character from name. */
	*--ptr = '\0';
	if (ptr == user) return(NONAME);

#ifdef OSS
/* If the line was terminated with one of the printing OSS line */
/* termination characters or is a <cr>, add a <newline>. */
	if (c == '/' || c == '!') {
		putc('\n',stdout);

/* Set up the "standard OSS" erase, kill, etc. characters. */
		termio->c_cc[VINTR] = STDABORT;
		termio->c_cc[VERASE] = STDERASE;
		termio->c_cc[VKILL] = STDKILL;
		termio->c_cc[VEOL] = '/';
		termio->c_cc[VEOL2] = '!';
	} else
#endif
		if (c == '\r')
			putc('\n',stdout);

/* If the line terminated with a <lf>, put ICRNL and ONLCR into */
/* into the modes. */
	if (c == '\r') {
		termio->c_iflag |= ICRNL;
		termio->c_oflag |= ONLCR;

/* When line ends with a <lf>, then add the <cr>. */
	} else putc('\r',stdout);

/* Set the upper-lower case conversion switchs if only upper */
/* case characters were seen in the login and no lower case. */
/* Also convert all the upper case characters to lower case. */

	if (upper > 0 && lower == 0) {
		termio->c_iflag |= IUCLC;
		termio->c_oflag |= OLCUC;
		termio->c_lflag |= XCASE;
		for (ptr=user; *ptr; ptr++)
			if (*ptr >= 'A' && *ptr <= 'Z' ) *ptr += ('a' - 'A');
	}
	return(GOODNAME);
}

/*	"find_def" scans "/etc/gettydefs" for a string with the		*/
/*	requested "id".  If the "id" is NULL, then the first entry is	*/
/*	taken, hence the first entry must be the default entry.		*/
/*	If a match for the "id" is found, then the line is parsed and	*/
/*	the Gdef structure filled.  Errors in parsing generate error	*/
/*	messages on the system console.					*/

struct Gdef *find_def(id)
char *id;
{
	register struct Gdef *gptr;
	register char *ptr,c;
	FILE *fp;
	int i,input,state,size,rawc,field;
	char oldc,*optr,quoted(),*gdfile;
	char line[MAXLINE+1];
	static struct Gdef def;
	extern struct Gdef DEFAULT;
	static char d_id[MAXIDLENGTH+1],d_nextid[MAXIDLENGTH+1];
	static char d_message[MAXMESSAGE+1];
	extern char *GETTY_DEFS;
	extern char *getword(),*fields(),*speed();
	extern int check;
	extern char *checkgdfile;
	static char *states[] = {
		"","id","initial flags","final flags","message","next id"
	};

/* Decide whether to read the real /etc/gettydefs or the supplied */
/* check file. */
	if (check) gdfile = checkgdfile;
	else gdfile = GETTY_DEFS;

/* Open the "/etc/gettydefs" file.  Be persistent. */
	for (i=0; i < 3;i++) {
		if ((fp = fopen(gdfile,"r")) != NULL) break;
		else sleep(3);	/* Wait a little and then try again. */
	}

/* If unable to open, complain and then use the built in default. */
	if (fp == NULL) {
		error("can't open \"%s\".\n",gdfile);
		return(&DEFAULT);
	}

/* Start searching for the line with the proper "id". */
	input = ACTIVE;
	do {
		for(ptr= line,oldc='\0'; ptr < &line[sizeof(line)] &&
		    (rawc = getc(fp)) != EOF; ptr++,oldc = c) {
			c = *ptr = rawc;

/* Search for two \n's in a row. */
			if (c == '\n' && oldc == '\n') break;
		}

/* If we didn't end with a '\n' or EOF, then the line is too long. */
/* Skip over the remainder of the stuff in the line so that we */
/* start correctly on next line. */
		if (rawc != EOF && c != '\n') {
			for (oldc='\0'; (rawc = getc(fp)) != EOF;oldc=c) {
				c = rawc;
				if (c == '\n' && oldc != '\n') break;
			}
			if (check) fprintf(stdout,"Entry too long.\n");
		}

/* If we ended at the end of the file, then if there is no */
/* input, break out immediately otherwise set the "input" */
/* flag to FINISHED so that the "do" loop will terminate. */
		if (rawc == EOF) {
			if (ptr == line) break;
			else input = FINISHED;
		}

/* If the last character stored was an EOF or '\n', replace it */
/* with a '\0'. */
		if (*ptr == (EOF & 0377) || *ptr == '\n') *ptr = '\0';

/* If the buffer is full, then make sure there is a null after the */
/* last character stored. */
		else *++ptr == '\0';
		if (check) fprintf(stdout,"\n**** Next Entry ****\n%s\n",line);

/* If line starts with #, treat as comment */
		if(line[0] == '#') continue;

/* Initialize "def" and "gptr". */
		gptr = &def;
		gptr->g_id = (char*)NULL;
		gptr->g_iflags.c_iflag = 0;
		gptr->g_iflags.c_oflag = 0;
		gptr->g_iflags.c_cflag = 0;
		gptr->g_iflags.c_lflag = 0;
		gptr->g_fflags.c_iflag = 0;
		gptr->g_fflags.c_oflag = 0;
		gptr->g_fflags.c_cflag = 0;
		gptr->g_fflags.c_lflag = 0;
		gptr->g_message = (char*)NULL;
		gptr->g_nextid = (char*)NULL;

/* Now that we have the complete line, scan if for the various */
/* fields.  Advance to new field at each unquoted '#'. */
		for (state=ID,ptr= line; state != FAILURE && state != SUCCESS;) {
			switch(state) {
			case ID:

/* Find word in ID field and move it to "d_id" array. */
				strncpy(d_id,getword(ptr,&size),MAXIDLENGTH);
				gptr->g_id = d_id;

/* Move to the next field.  If there is anything but white space */
/* following the id up until the '#', then set state to FAILURE. */
				ptr += size;
				while (isspace(*ptr)) ptr++;
				if (*ptr != '#') {
					field = state;
					state = FAILURE;
				} else {
					ptr++;	/* Skip the '#' */
					state = IFLAGS;
				}
				break;

/* Extract the "g_iflags" */
			case IFLAGS:
				if ((ptr = fields(ptr,&gptr->g_iflags)) == NULL) {
					field = state;
					state = FAILURE;
				} else {
					gptr->g_iflags.c_iflag &= ICRNL;
					if((gptr->g_iflags.c_cflag & CSIZE) == 0)
						gptr->g_iflags.c_cflag |= CS8;
					gptr->g_iflags.c_cflag |= CREAD|HUPCL;
					gptr->g_iflags.c_lflag &= ~(ISIG|ICANON
						|XCASE|ECHOE|ECHOK);
					ptr++;
					state = FFLAGS;
				}
				break;

/* Extract the "g_fflags". */
			case FFLAGS:
				if ((ptr = fields(ptr,&gptr->g_fflags)) == NULL) {
					field = state;
					state = FAILURE;
				} else {

/* Force the CREAD mode in regardless of what the user specified. */
					gptr->g_fflags.c_cflag |= CREAD;
					ptr++;
					state = MESSAGE;
				}
				break;

/* Take the entire next field as the "login" message. */
/* Follow usual quoting procedures for control characters. */
			case MESSAGE:
				for (optr= d_message; (c = *ptr) != '\0'
				    && c != '#';ptr++) {

/* If the next character is a backslash, then get the quoted */
/* character as one item. */
					if (c == '\\') {
						c = quoted(ptr,&size);
/* -1 accounts for ++ that takes place later. */
						ptr += size - 1;
					}

/* If the next character is a dollar sign, get the variable name */
/* following it and interpolate its value into d_message */
					if (c == '$') {
						char *value, *getvar();
						register int length;

						value = getvar(ptr, &size);
						ptr += size;
						length = strlen(value);
						if (optr + length
						    < &d_message[MAXMESSAGE]) {
							bcopy(value, optr,
							    length);
							optr += length;
						}
					} else
/* If there is room, store the next character in d_message. */
					if (optr < &d_message[MAXMESSAGE]) {
						*optr++ = c;
					}
				}

/* If we ended on a '#', then all is okay.  Move state to NEXTID. */
/* If we didn't, then set state to FAILURE. */
				if (c == '#') {
					gptr->g_message = d_message;
					state = NEXTID;

/* Make sure message is null terminated. */
					*optr++ = '\0';
					ptr++;
				} else {
					field = state;
					state = FAILURE;
				}
				break;

/* Finally get the "g_nextid" field.  If this is successful, then */
/* the line parsed okay. */
			case NEXTID:

/* Find the first word in the field and save it as the next id. */
				strncpy(d_nextid,getword(ptr,&size),MAXIDLENGTH);
				gptr->g_nextid = d_nextid;

/* There should be nothing else on the line.  Starting after the */
/* word found, scan to end of line.  If anything beside white */
/* space, set state to FAILURE. */
				ptr += size;
				while (isspace(*ptr)) ptr++;
				if (*ptr != '\0') {
					field = state;
					state = FAILURE;
				} else state = SUCCESS;
				break;
			}
		}

/* If a line was successfully picked up and parsed, compare the */
/* "g_id" field with the "id" we are looking for. */
		if (state == SUCCESS) {

/* If there is an "id", compare them. */
			if (id != NULL) {
				if (strcmp(id,gptr->g_id) == 0) {
					fclose(fp);
					return(gptr);
				}

/* If there is no "id", then return this first successfully */
/* parsed line outright. */
			} else if (check == FALSE) {
				fclose(fp);
				return(gptr);

/* In check mode print out the results of the parsing. */
			} else {
				fprintf(stdout,"id: %s\n",gptr->g_id);
				fprintf(stdout,"initial flags:\niflag- %o oflag- %o cflag- %o lflag- %o\n",
					gptr->g_iflags.c_iflag,
					gptr->g_iflags.c_oflag,
					gptr->g_iflags.c_cflag,
					gptr->g_iflags.c_lflag);
				fprintf(stdout,"final flags:\niflag- %o oflag- %o cflag- %o lflag- %o\n",
					gptr->g_fflags.c_iflag,
					gptr->g_fflags.c_oflag,
					gptr->g_fflags.c_cflag,
					gptr->g_fflags.c_lflag);
				fprintf(stdout,"message: %s\n",gptr->g_message);
				fprintf(stdout,"next id: %s\n",gptr->g_nextid);
			}

/* If parsing failed in check mode, complain, otherwise ignore */
/* the bad line. */
		} else if (check) {
			*++ptr = '\0';
			fprintf(stdout,"Parsing failure in the \"%s\" field\n\
%s<--error detected here\n",
				states[field],line);
		}
	} while (input == ACTIVE);

/* If no match was found, then return NULL. */
	fclose(fp);
	return(NULL);
}

char *getword(ptr,size)
register char *ptr;
int *size;
{
	register char *optr,c;
	char quoted();
	static char word[MAXIDLENGTH+1];
	int qsize;

/* Skip over all white spaces including quoted spaces and tabs. */
	for (*size=0; isspace(*ptr) || *ptr == '\\';) {
		if (*ptr == '\\') {
			c = quoted(ptr,&qsize);
			(*size) += qsize;
			ptr += qsize+1;

/* If this quoted character is not a space or a tab or a newline */
/* then break. */
			if (isspace(c) == 0) break;
		} else {
			(*size)++;
			ptr++;
		}
	}

/* Put all characters from here to next white space or '#' or '\0' */
/* into the word, up to the size of the word. */
	for (optr= word,*optr='\0'; isspace(*ptr) == 0 &&
	    *ptr != '\0' && *ptr != '#'; ptr++,(*size)++) {

/* If the character is quoted, analyze it. */
		if (*ptr == '\\') {
			c = quoted(ptr,&qsize);
			(*size) += qsize;
			ptr += qsize;
		} else c = *ptr;

/* If there is room, add this character to the word. */
		if (optr < &word[MAXIDLENGTH+1] ) *optr++ = c;
	}

/* Make sure the line is null terminated. */
	*optr++ = '\0';
	return(word);
}

/*	"quoted" takes a quoted character, starting at the quote	*/
/*	character, and returns a single character plus the size of	*/
/*	the quote string.  "quoted" recognizes the following as		*/
/*	special, \n,\r,\v,\t,\b,\f as well as the \nnn notation.	*/

char quoted(ptr,qsize)
char *ptr;
int *qsize;
{
	register char c,*rptr;
	register int i;

	rptr = ptr;
	switch(*++rptr) {
	case 'n':
		c = '\n';
		break;
	case 'r':
		c = '\r';
		break;
	case 'v':
		c = '\013';
		break;
	case 'b':
		c = '\b';
		break;
	case 't':
		c = '\t';
		break;
	case 'f':
		c = '\f';
		break;
	default:

/* If this is a numeric string, take up to three characters of */
/* it as the value of the quoted character. */
		if (*rptr >= '0' && *rptr <= '7') {
			for (i=0,c=0; i < 3;i++) {
				c = c*8 + (*rptr - '0');
				if (*++rptr < '0' || *rptr > '7') break;
			}
			rptr--;

/* If the character following the '\\' is a NULL, back up the */
/* ptr so that the NULL won't be missed.  The sequence */
/* backslash null is essentually illegal. */
		} else if (*rptr == '\0') {
			c = '\0';
			rptr--;

/* In all other cases the quoting does nothing. */
		} else c = *rptr;
		break;
	}

/* Compute the size of the quoted character. */
	(*qsize) = rptr - ptr + 1;
	return(c);
}

/*
 * Get the variable name which follows the '$' pointed at by cp.
 * Return a pointer to its value directly.  Return the variable name
 * length indirectly through sizep.
 */
char *
getvar(cp, sizep)
	register char *cp;
	register int *sizep;
{
	register char *vp;
	static char hostname[MAXHOSTNAMELEN];

	vp = getword(cp + 1, sizep);
	if (strcmp(vp, "HOSTNAME") == 0) {
		if (hostname[0] == '\0') {
			gethostname(hostname, MAXHOSTNAMELEN);
		}
		return hostname;
	} else {
		fprintf(stderr, "undefined variable %.*s\n",
		    *sizep, cp + 1);
		return "?";
	}
}

/*	"fields" picks up the words in the next field and converts all	*/
/*	recognized words into the proper mask and puts it in the target	*/
/*	field.								*/

char *fields(ptr,termio)
register char *ptr;
struct termio *termio;
{
	extern struct Symbols imodes[],omodes[],cmodes[],lmodes[];
	extern struct Symbols *search();
	register struct Symbols *symbol;
	char *word,*getword();
	int size;
	extern int check;

	termio->c_iflag = 0;
	termio->c_oflag = 0;
	termio->c_cflag = 0;
	termio->c_lflag = 0;
	while (*ptr != '#' && *ptr != '\0') {

/* Pick up the next word in the sequence. */
		word = getword(ptr,&size);

/* If there is a word, scan the two mode tables for it. */
		if (*word != '\0') {

/* If the word is the special word "SANE", put in all the flags */
/* that are needed for SANE tty behavior. */
			if (strcmp(word,"SANE") == 0) {
				termio->c_iflag |= ISANE;
				termio->c_oflag |= OSANE;
				termio->c_cflag |= CSANE;
				termio->c_lflag |= LSANE;
			} else if ((symbol = search(word,imodes)) != NULL)
				termio->c_iflag |= symbol->s_value;
			else if ((symbol = search(word,omodes)) != NULL)
				termio->c_oflag |= symbol->s_value;
			else if ((symbol = search(word,cmodes)) != NULL)
				termio->c_cflag |= symbol->s_value;
			else if ((symbol = search(word,lmodes)) != NULL)
				termio->c_lflag |= symbol->s_value;
			else if (check) fprintf(stdout,"Undefined: %s\n",word);
		}

/* Advance pointer to after the word. */
		ptr += size;
	}

/* If we didn't end on a '#', return NULL, otherwise return the */
/* updated pointer. */
	return(*ptr != '#' ? NULL : ptr);
}

/*	"parse" breaks up the user's response into seperate arguments	*/
/*	and fills the supplied array with those arguments.  Quoting	*/
/*	with the backspace is allowed.					*/

parse(string,args,cnt)
char *string,**args;
int cnt;
{
	register char *ptrin,*ptrout;
	register int i;
	extern char quoted();
	int qsize;

	for (i=0; i < cnt; i++) args[i] = (char *)NULL;
	for (ptrin = ptrout = string,i=0; *ptrin != '\0' && i < cnt; i++) {

/* Skip excess white spaces between arguments. */
		while(*ptrin == ' ' || *ptrin == '\t') {
			ptrin++;
			ptrout++;
		}

/* Save the address of the argument if there is something there. */
		if (*ptrin == '\0') break;
		else args[i] = ptrout;

/* Span the argument itself.  The '\' character causes quoting */
/* of the next character to take place (except for '\0'). */
		while (*ptrin != '\0') {

/* Is this the quote character? */
			if (*ptrin == '\\') {
				*ptrout++ = quoted(ptrin,&qsize);
				ptrin += qsize;

/* Is this the end of the argument?  If so quit loop. */
			} else if (*ptrin == ' ' || *ptrin == '\t') {
				ptrin++;
				break;

/* If this is a normal letter of the argument, save it, advancing */
/* the pointers at the same time. */
			} else *ptrout++ = *ptrin++;
		}

/* Null terminate the string. */
		*ptrout++ = '\0';
	}
}

FILE *fdup(fp)
register FILE *fp;
{
	register int newfd;
	register char *mode;

/* Dup the file descriptor for the specified stream and then */
/* convert it to a stream pointer with the modes of the original */
/* stream pointer. */
	if ((newfd = dup(fileno(fp))) != FAILURE) {

/* Determine the proper mode.  If the old file was _IORW, then */
/* use the "r+" option, if _IOREAD, the "r" option, or if _IOWRT */
/* the "w" option.  Note that since none of these force an lseek */
/* by "fdopen", the dupped file pointer will be at the same spot */
/* as the original. */
		if (fp->_flag & _IORW) mode = "r+";
		else if (fp->_flag & _IOREAD) mode = "r";
		else if (fp->_flag & _IOWRT) mode = "w";

/* Something is wrong, close dupped descriptor and return NULL. */
		else {
			close(newfd);
			return(NULL);
		}

/* Now have fdopen finish the job of establishing a new file pointer. */
		return(fdopen(newfd,mode));
	} else return(NULL);
}

