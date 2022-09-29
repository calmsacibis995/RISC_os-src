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
#ident	"$Header: login.c,v 1.25.1.13.1.1.1.2 90/10/05 10:03:16 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any	*/
/*	actual or intended publication of such source code.	*/

/*
 * login [ name ] [ environment args ]
 *
 *	Conditional assemblies:
 *
 *	PASSREQ	causes password to be required
 *	NO_MAIL	causes the MAIL environment variable not to be set
 *	NOSHELL does not put non-standard shell names into environment
 *	CONSOLE, if defined, only allows root logins on the device
 *		specified by CONSOLE.  CONSOLE MUST NOT be defined as
 *		either "/dev/syscon" or "/dev/systty"!!
 *	MAXTRYS is the number of attempts permitted.  0 is "no limit".
 *	MAXTIME is the number of seconds before giving up.  0 is "no limit".
 */

#include <sys/types.h>
#include <utmp.h>
#include <signal.h>
#include <pwd.h>
#include <shadow.h>
#include <stdio.h>
#ifdef	SECURITY
#include <fcntl.h>
#endif	/* SECURITY */
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <sys/param.h>
#include <sys/termio.h>

#if	RISCOS
#include <sys/fcntl.h>
#include <bsd/syslog.h>
#include <grp.h>
#include <sys/fs/ufs_quota.h>

#include <bsd43/sys/syscall.h>
#define quotactl(a,b,c,d) syscall(BSD43_SYS_quotactl,a,b,c,d)

extern struct group *getgrnam();

#define TTYGRPNAME	"tty"		/* name of group to own ttys */
#define TTYGID(gid)	tty_gid(gid)	/* gid that owns all ttys */

char	nolog[] =	"/etc/nologin";
char	qlog[]  =	".hushlogin";
char	lastlog[] =	"/usr/adm/lastlog";
#if !defined(TAHOE_QUOTA)
char QUOTAWARN[] =	"/usr/ucb/quota";	/* warn user about quotas */
char CANTRUN[] =	"login: Can't run ";
#endif

#include <bsd43/sys/types.h>
#include <bsd43/lastlog.h>

#include <rpc/types.h>
#include <rpc/auth.h>
#include <rpc/key_prot.h>
#endif	RISCOS


#ifndef	MAXTRYS
#	define	MAXTRYS	5	/* default */
#endif
#ifndef	MAXTIME
#	define	MAXTIME	60	/* default */
#endif
#ifdef	SECURITY
#	define	CONS "/dev/console"	/* default */
#endif	/* SECURITY */

/*
 * In standard System V, the time you wait between bad login attempts
 * is (60-20)/10, or 4, seconds.  We have had complaints that this is
 * too long, and that 1 second would do fine for security.  So, if
 * LONGSLEEP is not defined, use BADSLEEP instead.
 */

#ifndef LONGSLEEP
#	define	BADSLEEP 1
#endif

#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define EQN(a, b)	(!strncmp(a, b, sizeof(a)-1))
#define DIAL_FILE	"/etc/dialups"
#define DPASS_FILE	"/etc/d_passwd"
#define SHELL		"/bin/sh"
#define	PATH		"PATH=/usr/net:/bin:/usr/bin:/usr/ucb"
#define	ROOTPATH	"PATH=/usr/net:/bin:/usr/bin:/etc:/usr/ucb"
#define SUBLOGIN	"<!sublogin>"

#define	ROOTUID		0

#define	MAXARGS		63
#define	MAXENV		1024
#define MAXLINE		256

/*	Illegal passwd entry.
*/
static struct	passwd nouser = { "", "no:password", ~ROOTUID };
static struct	spwd  noupass = { "", "no:password"};

static struct utsname	un;
static char	hostname[MAXHOSTNAMELEN+1];
static struct	utmp utmp;
static char	u_name[64];
static char	minusnam[16]	= "-";
static char	shell[64] = { "SHELL=" };
static char	home[64] = { "HOME=" };
static char	logname[30] = { "LOGNAME=" };
static char	user[30] = { "USER=" };
static char	tz[12] = { "TZ=" };
static char	*tp;
static char	term[64];
static char	env_term[5+64]	= { "TERM=" };
static char	env_disp[8+64]	= { "DISPLAY=" };

static char	loginmsg[] = "login: ";
static char	passwdmsg[] = "Password:";
static char	incorrectmsg[] = "Login incorrect\n";

#ifndef	NO_MAIL
static char	mail[30] = { "MAIL=/usr/mail/" };
#endif

static char	*envinit[8+MAXARGS] = {
		home,PATH,logname,0,0
	};
static int	basicenv;
static int	tzfd;		/* file descriptor for TZFILE */
static char	envblk[MAXENV];
static struct	passwd *pwd;
static struct	spwd *sp;

struct	passwd *getpwnam();
int	setpwent();
char	*crypt();
char	*getpass();
char	*getenv();
long	lseek();
char	*strrchr(),*strchr(),*strcat();
extern	char **environ;

#define	WEEK	(24L * 7 * 60 * 60) /* 1 week in seconds */
static time_t	when;
static time_t	maxweeks;
static time_t	minweeks;
static time_t	now;
extern long	a64l(), time();


#define NMAX	sizeof(utmp.ut_name)

#ifdef RISCOS

struct winsize win = { 0, 0, 0, 0 };

char	utmp_ut_host[16];
char	me[MAXHOSTNAMELEN];
char	p3[8];  /* to store user passwd */
struct 	termio	termp;
char	cpyrt_buf[BUFSIZ];	/* buffer to read /etc/copyright */
int	cpfd;			/* fd for it */
int	ncprd;			/* read count.  generate LOG_WARNING if too big*/

#endif RISCOS

int	rflag = 0;
int	hflag = 0;
int	usererr = -1;
char	rusername[NMAX+1], lusername[NMAX+1];
char	rpassword[NMAX+1];

#ifdef RISCOS
int login_timeout();
char *login_timeout_ttyn = NULL;
#endif RISCOS

int shad_flag;
main(argc, argv ,renvp)
char **argv,**renvp;
{
	register char *namep;
	int j,k,l_index,length;
	char *ttyn,*ttyntail;
#ifdef PASSREQ
	int	nopassword = 1;
#endif
	register int i;
	register struct utmp *u;
	struct utmp *getutent(), *pututline();
	FILE *fp;
	char **envp,*ptr,*endptr;
	int sublogin;
	extern char **getargs();
	extern char *terminal();
	char	inputline[ MAXLINE ];
	int	n;
	char *cp;
#if	MAXTRYS > 0
	int	trys = 0;
#endif
	struct stat	sb;
#ifdef RISCOS
	int	quietlog;
	int	f;
	int	zero = 0;
	char	*p, *domain, *index();
	FILE 	*nlfd;
#endif RISCOS

/*	Set flag to disable the pid check if you find that you are	*/
/*	a subsystem login.						*/

	sublogin = 0;
	if (*renvp && strcmp(*renvp,SUBLOGIN) == 0)
		sublogin = 1;

	umask(0);
#ifdef RISCOS
	signal(SIGALRM, login_timeout);
#endif RISCOS
	alarm( MAXTIME );
	signal(SIGQUIT, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	nice(0);
	ttyn = terminal(0);
	if (ttyn==0)
		ttyn = "/dev/tty??";
	if (ttyntail = strrchr(ttyn,'/'))
		ttyntail++;
	else
		ttyntail = ttyn;
	login_timeout_ttyn = ttyn;

#if	RISCOS
	openlog("login", LOG_ODELAY, LOG_AUTH);

	(void) gethostname(me, sizeof(me));
	domain = index(me, '.');

	ioctl(0, FIONBIO, &zero);
	ioctl(0, FIOASYNC, &zero);
	ioctl(0, TCGETA, &termp);
	termp.c_cc[VINTR] = CINTR;
	termp.c_cc[VQUIT] = CQUIT;
	termp.c_cc[VERASE] = CERASE;
	termp.c_cc[VKILL] = CKILL;
	termp.c_cc[VEOF] = CEOF;
	termp.c_cc[VEOL] = CNUL;
	termp.c_cc[VEOL2] = CNUL;
	termp.c_cc[VSWTCH] = CDEL;
	termp.c_cc[VSTART] = CSTART;
	termp.c_cc[VSTOP] = CSTOP;
	termp.c_cc[VSUSP] = CDEL;
	termp.c_cc[V_DSUSP] = CDEL;
	termp.c_cc[V_RPRNT] = CRPRNT;
	termp.c_cc[V_FLUSH] = CFLUSH;
	termp.c_cc[V_WERAS] = CWERASE;
	termp.c_cc[V_LNEXT] = CESC;
	termp.c_cc[V_STATUS] = CDEL;
	termp.c_cc[V_SAVED_EOF] = CDEL;
	termp.c_cc[V_SAVED_EOL] = CDEL;
#endif	RISCOS

	/*** Arg processing from 4.3bsd
	 * -r is used by rlogind to cause the autologin protocol;
	 * -h is used by other servers to pass the name of the
	 *	remote host to login so that it may be placed in utmp and
	 *	wtmp, but we have to just ignore it.
	 */
	while (argc > 1) {
#ifdef RISCOS
	/* -N is used by getty, uugetty to avoid the security problem
	 * where a user could login without giving a passwd if he types
	 * '-r fromhost user' at the getty generated login prompt, and
	 * fake the 'rlogin' handshake (remoteuser^@localuser^@termtype^@).
	 *
	 *   login is invoked from getty as '/bin/login -N arg2 arg3 ....'
	 *
	 * Note: No further flag processing is done after -N, and all
	 * remaining arguments will be placed in environments.
	 */
	  	if (strcmp(argv[1], "-N") == 0) {
			argc--;
			argv++;
			goto brkloop;
		}
#endif
		if (strcmp(argv[1], "-r") == 0) {
			if (rflag || hflag) {
				printf("Only one of -r and -h allowed\n");
				exit(1);
			}
			rflag = 1;
#ifdef RISCOS
			if ((p = index(argv[2], '.')) &&
			    domain != NULL &&
			    strcmp(p, domain) == 0)
				*p = 0;
			SCPYN(utmp_ut_host, argv[2]);
#endif RISCOS
			usererr = doremotelogin(argv[2]);
			doremoteterm(term);
			argc -= 2;
			argv += 2;
			continue;
		}
		if (strcmp(argv[1], "-h") == 0 && getuid() == 0) {
			if (rflag || hflag) {
				printf("Only one of -r and -h allowed\n");
				exit(1);
			}
			hflag = 1;
#ifdef RISCOS
			if ((p = index(argv[2], '.')) &&
			    domain != NULL &&
			    strcmp(p, domain) == 0)
				*p = 0;
			SCPYN(utmp_ut_host, argv[2]);
#endif RISCOS
			argc -= 2;
			argv += 2;
			continue;
		}
#ifdef RISCOS
brkloop:
#endif
		SCPYN(utmp.ut_user, argv[1]);
		SCPYN(u_name, argv[1]);
		strcpy( inputline, u_name );
		strcat( inputline, "   \n" );
		envp = &argv[2];
		goto first;
	}
	if (rflag)
		goto first;
loop:
	if (!usererr)			/* no extra tries for remote login */
		exit(1);		/* that is what 4.3 does, so... */
#if	MAXTRYS > 0
	if( ++trys > MAXTRYS ) {
#ifdef	SECURITY
		badlogin(u_name, ttyn);
#endif	/* SECURITY */
		alarm(0);
		sleep( MAXTIME );
		exit( 1 );
	}
#endif MAXTRYS
	u_name[0] = utmp.ut_user[0] = '\0';
	rflag = 0;			/* forget remote name after failure */
first:

#if	RISCOS
	(void)ioctl(0, TCSETA, &termp);
#endif	RISCOS
	if (!rflag) {
		while (utmp.ut_user[0] == '\0') {
			printf( loginmsg );
			if ((envp = getargs( inputline )) != (char**)NULL) {
				SCPYN(utmp.ut_user,*envp);
				SCPYN(u_name, *envp++);
			}
		}

	/*	If any of the common login messages was the input, we must be
		looking at a tty that is running login.  We exit because
		they will chat at each other until one times out otherwise.
		In time, init(1M) sees this and decides something is amiss.
	*/
		if( EQN(loginmsg, inputline) || EQN( passwdmsg, inputline ) ||
		    EQN( incorrectmsg, inputline ) ) {
			printf( "Looking at a login line.\n" );
			exit( 8 );
		}

		setpwent();
		if ((pwd = getpwnam(u_name)) == NULL)
			pwd = &nouser;
		endpwent();
#ifdef CONSOLE
		if( pwd->pw_uid == ROOTUID &&
#ifdef RISCOS
		    access("/etc/login.root.console.ok",0) != -1 &&
#endif RISCOS
		    ! is_same_tty(ttyn,CONSOLE)) {
			printf("Not on system console\n");
			exit(10);
		}
#endif
		if (access(SHADOW,0) != -1)  {
			shad_flag = 1;
			setspent();
			if ((sp = getspnam(u_name)) == NULL)
				if (pwd == &nouser)
				/* if user's name is not in /etc/passwd
				   , yp database and /etc/shadow then
				   continue 
				 */
					sp = &noupass;
				else
				/* if user's name is in /etc/passwd or
				   yp database, but not in /etc/shadow,
				   then use pwd info
				 */
					shad_flag = 0;
			endspent();
		}
	}
#ifdef PASSREQ
	if (*(shad_flag? sp->sp_pwdp : pwd->pw_passwd) != '\0')
		nopassword = 0;
#endif PASSREQ

	if (usererr == -1 && *pwd->pw_passwd != '\0') {
		if(gpass(passwdmsg, (shad_flag? sp->sp_pwdp : pwd->pw_passwd)))
			goto loop;
	}

	/*
	 * get dialup password, if necessary
	 */
	if(dialpass(ttyn))
		goto loop;

#ifdef RISCOS
	/*
	 * If user not super-user, check for logins disabled.
	 */
	if (pwd->pw_uid != 0 && (nlfd = fopen(nolog, "r")) != 0) {
		int 	c;

		while ((c = getc(nlfd)) != EOF)
			putchar(c);
		fflush(stdout);
		sleep(5);
		exit(0);
	}
#endif RISCOS

	/*
	 * optionally adjust nice(2)
	 */
	if (strncmp("pri=", pwd->pw_gecos, 4) == 0) {
		int	mflg, pri;

		pri = 0;
		mflg = 0;
		i = 4;
		if (pwd->pw_gecos[i] == '-') {
			mflg++;
			i++;
		}
		while(pwd->pw_gecos[i] >= '0' && pwd->pw_gecos[i] <= '9')
			pri = (pri * 10) + pwd->pw_gecos[i++] - '0';
		if (mflg)
			pri = -pri;
		nice(pri);
	}

	if(chdir(pwd->pw_dir) < 0) {
		if (access ("/etc/login.nohome.ok", 0) != -1) {
			if (chdir("/") < 0) {
				printf("No directory!\n");
				goto loop;
			} else {
				printf("No directory!  Logging in with home=/\n");
				pwd->pw_dir = "/";
			}
		} else {
			printf("Unable to change directory to \"%s\"\n",pwd->pw_dir);
			goto loop;
		}
	}
/* committed to login turn off timeout */
	alarm(0);

	time(&utmp.ut_time);
	utmp.ut_pid = getpid();

/*	Find the entry for this pid (or line if we are a sublogin) in	*/
/*	the utmp file.							*/

	while ((u = getutent()) != NULL) {
		if ((u->ut_type == INIT_PROCESS || u->ut_type ==
		    LOGIN_PROCESS || u->ut_type == USER_PROCESS)
		    && ( (sublogin && strncmp(u->ut_line,ttyntail,
		    sizeof(u->ut_line)) == 0) || u->ut_pid == utmp.ut_pid) ) {

/*	Copy in the name of the tty minus the "/dev/", the id, and set	*/
/*	the type of entry to USER_PROCESS.				*/

			SCPYN(utmp.ut_line,ttyntail);
			utmp.ut_id[0] = u->ut_id[0];
			utmp.ut_id[1] = u->ut_id[1];
			utmp.ut_id[2] = u->ut_id[2];
			utmp.ut_id[3] = u->ut_id[3];
			utmp.ut_type = USER_PROCESS;

/*	Return the new updated utmp file entry.				*/

			pututline(&utmp);
			break;
		}
	}
	endutent();		/* Close utmp file */

	if (u == (struct utmp *)NULL){
		printf("No utmp entry.  You must exec \"login\" from\
 the lowest level \"sh\".\n");
		exit(1);
	}

/*	Now attempt to write out this entry to the wtmp file if we	*/
/*	were successful in getting it from the utmp file and the	*/
/*	wtmp file exists.						*/

	if (u != NULL && (((fp = fopen(WTMP_FILE,"r+")) != NULL)
		    )) {
		fseek(fp,0L,2);		/* Seek to end of file. */
		fwrite(&utmp,sizeof(utmp),1,fp);
		fclose(fp);
	}

#ifdef RISCOS
	quietlog = (stat(qlog,&sb) != -1);
	if ((f = open(lastlog, O_RDWR)) >= 0) {
		struct lastlog ll;

		lseek(f, (long)pwd->pw_uid * sizeof (struct lastlog), 0);
		if (read(f, (char *) &ll, sizeof ll) == sizeof ll &&
		    ll.ll_time != 0 && !quietlog) {
			printf("Last login: %.*s ",
			    24-5, (char *)ctime(&ll.ll_time));
			if (*ll.ll_host != '\0')
				printf("from %.*s\n",
				    sizeof (ll.ll_host), ll.ll_host);
			else
				printf("on %.*s\n",
				    sizeof (ll.ll_line), ll.ll_line);
		}
		lseek(f, (long)pwd->pw_uid * sizeof (struct lastlog), 0);
		time(&ll.ll_time);
		SCPYN(ll.ll_line, ttyntail);
		SCPYN(ll.ll_host, utmp_ut_host);
		write(f, (char *) &ll, sizeof ll);
		close(f);
	}
#endif RISCOS

	chown(ttyn, pwd->pw_uid, (
#ifdef RISCOS
	      	(access ("/etc/login.ttygroup.ok", 0) != -1) 
	       		? TTYGID(pwd->pw_gid) :
#endif RISCOS
			   pwd->pw_gid));
#ifdef RISCOS
	if (!hflag && !rflag)					/* XXX */
		ioctl(0, TIOCSWINSZ, &win);

	/* Follow BSD 4.3 semantics wrt tty groups and write privileges
	 * only if the following two conditions are met.  A tty group
	 * must exist and the sentinel file must exist.  Otherwise, follow
	 * SysV semantics.
	 */
	if (getgrnam(TTYGRPNAME) != 0 &&
	    access("/etc/login.ttygroup.ok", 0) != -1)
		chmod(ttyn, 0620);
	else
		chmod(ttyn, 0622);
#endif RISCOS

/*	If the shell field starts with a '*', do a chroot to the home	*/
/*	directory and perform a new login.				*/

	if(*pwd->pw_shell == '*') {
		if(chroot(pwd->pw_dir) < 0) {
			printf("No Root Directory\n");
			goto loop;
		}

/*	Set the environment flag <!sublogin> so that the next login	*/
/*	knows that it is a sublogin.					*/

		envinit[0] = SUBLOGIN;
		envinit[1] = (char*)NULL;
		printf("Subsystem root: %s\n",pwd->pw_dir);
		execle("/bin/login", "login", (char*)0, &envinit[0]);
		execle("/etc/login", "login", (char*)0, &envinit[0]);
		printf("No /bin/login or /etc/login on root\n");
		goto loop;
	}

	if (setgid(pwd->pw_gid) == -1) {
		printf("Bad group id.\n");
		exit(1);
	}
	if (access("/etc/login.initgroups.ok",0) != -1)
		initgroups(pwd->pw_name, pwd->pw_gid);
	if (setuid(pwd->pw_uid) == -1) {
		printf("Bad user id.\n");
		exit(1);
	}
#ifdef RISCOS
	/*
         * Set key only after setuid()
         */
        setsecretkey(p3);
#endif
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	quotactl(Q_DOWARN, 0, pwd->pw_uid, 0);
#endif

	alarm(0);  /* give user time to come up with new password if needed */

#ifdef PASSREQ
	if (nopassword
#ifdef RISCOS
		&&
	    access("/etc/login.passreq.ok",0) != -1
#endif RISCOS
	    ) {
		printf("You don't have a password.  Choose one.\n");
		printf("passwd %s\n", u_name);
		n = system("/bin/passwd");
		if (n > 0)
			goto loop;
		if (n < 0) {
			printf("Cannot execute /bin/passwd\n");
			exit(1);
		}
	}
#endif

/* is the age of the password to be checked? */

	if (  shad_flag  )		/* with shadow */
	{
		now	= DAY_NOW ;
		if( ( sp->sp_lstchg == 0 )	||
		    ( sp->sp_lstchg > now )	||
		    ( ( sp->sp_max >= 0 )	&&
		      ( now > (sp->sp_lstchg + sp->sp_max) ) &&
		      ( sp->sp_max >= sp->sp_min )
		    ) )
			goto pwdexpired;
	}  else  {
		if (*pwd->pw_age != NULL) {
			/* retrieve (a) week of previous change;
				(b) maximum number of valid weeks	*/
			when = a64l (pwd->pw_age);
			/* max, min and weeks since last change are packed radix 64 */
			maxweeks = when & 077;
			minweeks = (when >> 6) & 077;
			when >>= 12;
			now  = time(0)/WEEK;
			if (when > now || (now > when + maxweeks) && (maxweeks >= minweeks)) {
pwdexpired:
				printf ("Your password has expired. Choose\
 a new one\n");
				n = system("/bin/passwd");
				if (n > 0)
					goto loop;
				if (n < 0) {
					printf("Cannot execute /bin/passwd\n");
					exit(9);
				}
			}
		}
	}

/*	Set up the basic environment for the exec.  This includes	*/


/*	Set up the basic environment for the exec.  This includes	*/
/*	HOME, PATH, LOGNAME, SHELL, and MAIL, and TZ, USER, and TERM.	*/

	strcat(home, pwd->pw_dir);
	strcat(logname, pwd->pw_name);
	strcat(user, pwd->pw_name);	/* remember name for environment */
	if(pwd->pw_uid == ROOTUID) {
		envinit[1] = ROOTPATH;
	}
	if (*pwd->pw_shell == '\0') {
		pwd->pw_shell = SHELL;
	}
#ifndef NOSHELL
	else {
		envinit[3] = shell;
	}
#endif
	strcat(shell, pwd->pw_shell);

/*	Find the name of the shell.					*/

	if ((namep = strrchr(pwd->pw_shell, '/')) == NULL)
		namep = pwd->pw_shell;
	else
		namep++;

/*	Generate the name of the shell with a '-' sign in front of it.	*/
/*	This causes .profile processing when a shell is exec'ed.	*/

	strcat(minusnam, namep);

#ifndef	NO_MAIL
	if (envinit[3] == (char*)NULL)
		envinit[3] = mail;
	else
		envinit[4] = mail;
	strcat(mail,pwd->pw_name);
#endif

/*	Find the end of the basic environment.				*/

	for (basicenv=3; envinit[basicenv];basicenv++);

	tp = getenv("TZ");		/* add in time-zone */
#ifdef RISCOS
	if (tp != 0)
#else
	if (tp > 0)
#endif
		strncpy(tz+3,tp,sizeof(tz)-3);
	else if ((tzfd=open("/etc/TZ",0)) >= 0) {
		read(tzfd,tz+3,sizeof(tz)-3);
		tp = strchr(tz,'\n');
		if (tp)
			*tp = '\0';
		close(tzfd);
	} else {
		strcpy(tz+3,"PST8PDT");
	}
	envinit[basicenv++] = tz;	/* allow for TZ */
	envinit[basicenv++] = user;	/* allow for USER */
	if (term[0] != '\0')		/* install terminal type */
		envinit[basicenv++] = strcat(env_term,term);
	if (term[0] == '\0' && (cp = getenv("TERM")))
		envinit[basicenv++] = strcat(env_term,cp);
	if (cp = getenv("DISPLAY"))
		envinit[basicenv++] = strcat(env_disp,cp);

/*	Add in all the environment variables picked up from the		*/
/*	argument list to "login" or from the user response to the	*/
/*	"login" request.						*/

	if (envp != NULL)
		for (j=0,k=0,l_index=0,ptr= &envblk[0]; *envp && j < MAXARGS-1
		     ;j++,envp++) {

/*	Scan each string provided.  If it doesn't have the format	*/
/*	xxx=yyy, then add the string "Ln=" to the beginning.		*/

		if ((endptr = strchr(*envp,'=')) == (char*)NULL) {
			envinit[basicenv+k] = ptr;
			sprintf(ptr,"L%d=%s",l_index,*envp);

/*	Advance "ptr" to the beginning of the next argument.		*/

			while (*ptr++);
			k++;
			l_index++;
		}

/*	Is this an environmental variable we permit?			*/

		else if ( !legalenvvar( *envp ) ) {
			continue;
		}


/*	Check to see whether this string replaces any previously	*/
/*	defined string.							*/

		else {
			for (i=0,length= endptr+1-*envp; i < basicenv+k;i++) {
				if (strncmp(*envp,envinit[i],length) == 0) {
					envinit[i] = *envp;
					break;
				}
			}

/*	If it doesn't, place it at the end of environment array.	*/

			if (i == basicenv+k) {
				envinit[basicenv+k] = *envp;
				k++;
			}
		}
	}

/*	Switch to the new environment.					*/

	environ = envinit;
	alarm(0);

	signal(SIGQUIT, SIG_DFL);
	signal(SIGINT, SIG_DFL);
	signal(SIGTSTP, SIG_IGN);
	uname( &un );
	gethostname(hostname, sizeof(hostname));
	fixus(un.release);
	if ( (cpfd = open("/etc/copyright",O_RDONLY)) < 0 )
	{
	printf("RISC/os (UMIPS) %s %s\n\
(C) Copyright 1986-1990, MIPS Computer Systems\nAll Rights Reserved\n",
		un.release, hostname);
	}
	else
	{
	    printf(
"(C) Copyright 1986-1990, MIPS Computer Systems\nAll Rights Reserved\n");
	    ncprd = read(cpfd,cpyrt_buf,sizeof(cpyrt_buf));
	    close(cpfd);
	    if (ncprd == sizeof(cpyrt_buf))
		syslog(LOG_WARNING,"/etc/copyright may be too large");
	    printf(cpyrt_buf,un.release, hostname);
	}

#ifdef	SECURITY
/*
 *	Advise the user the time and date that this login-id
 *	was last used
 */
#ifdef RISCOS
	if (access("/etc/login.lastlogin.ok",0) != -1)
#endif RISCOS
		lastlogin();
#endif	/* SECURITY */

#ifdef	RISCOS
	if (! fstat(0,&sb) &&
	    (minor(sb.st_rdev) & 0x80) != 0 &&
	    (ttyntail[3] < 'p' ||
	     ttyntail[3] > 'z'))
		syslog(LOG_INFO, "DIALUP %s, %s", ttyntail, pwd->pw_name);
	if (pwd->pw_uid == 0)
		syslog(LOG_NOTICE, "ROOT LOGIN %s", ttyntail);
#endif	RISCOS
#if !defined(TAHOE_QUOTA)
	if (!quietlog &&
	    access("/etc/login.quotawarn.ok",0) != -1) {
		int pid, w;

		signal(SIGCLD, SIG_DFL);
		if ((pid = fork()) == 0) {
			execl(QUOTAWARN, QUOTAWARN, (char *)0);
			_exit(0);
		} else if (pid == -1) {
			fprintf(stderr, CANTRUN);
			perror(QUOTAWARN);
		} else {
			while ((w = wait((int *)NULL)) != pid && w != -1)
				;
		}
	}
#endif
	execl(pwd->pw_shell, minusnam, (char*)0);

	/*	pwd->pw_shell was not an executable object file, maybe it
		is a shell proceedure or a command line with arguments.
		If so, turn off the SHELL= environment variable.
	*/
	if( !strncmp( envinit[3], shell, 6 ) )
		envinit[3][6] = '\0';
	if( access( pwd->pw_shell, 05 ) == 0 )
		execl(SHELL, "sh", pwd->pw_shell, minusnam, (char*)0);

	printf("No shell\n");
	exit(1);
	/* NOTREACHED */
}

static dialpass(ttyn)
char *ttyn;
{
	register FILE *fp;
	char defpass[30];
	char line[80];
	register char *p1, *p2;

	if((fp=fopen(DIAL_FILE, "r")) == NULL)
		return(0);
	while((p1 = fgets(line, sizeof(line), fp)) != NULL) {
		while(*p1 != '\n' && *p1 != ' ' && *p1 != '\t')
			p1++;
		*p1 = '\0';
		if(strcmp(line, ttyn) == 0)
			break;
	}
	fclose(fp);
	if(p1 == NULL || (fp = fopen(DPASS_FILE, "r")) == NULL)
		return(0);
	defpass[0] = '\0';
	p2 = 0;
	while((p1 = fgets(line, sizeof(line)-1, fp)) != NULL) {
		while(*p1 && *p1 != ':')
			p1++;
		*p1++ = '\0';
		p2 = p1;
		while(*p1 && *p1 != ':')
			p1++;
		*p1 = '\0';
		if(strcmp(pwd->pw_shell, line) == 0) {
			break;
		}
		if(strcmp(SHELL, line) == 0)
			SCPYN(defpass, p2);
		p2 = 0;
	}
	fclose(fp);
	if( !p2 )
		p2 = defpass;
	if( *p2 != '\0' )
		return(gpass("Dialup Password:", p2));
	return(0);
}

static gpass(prmt, pswd)
char *prmt, *pswd;
{
	register char *p1;

	p1 = getpass(prmt);
	if (p1 == NULL) {
	  	printf("Sorry: could not read your password\n");
		return(1);
	};
#ifdef RISCOS
	/* save non-crypted password */
	bcopy(p1, p3, 8);
#endif
	p1 = crypt(p1, pswd);
	if(strcmp(p1, pswd)) {
#ifdef BADSLEEP
		sleep(BADSLEEP);
#else
#if  MAXTRYS > 0  &&  MAXTIME > 0  &&  (MAXTIME - 2*MAXTRYS) > 0
		sleep( (MAXTIME - 2*MAXTRYS)/MAXTRYS );
#endif
#endif
		printf( incorrectmsg );
		return(1);
	}
	return(0);
}

#define	WHITESPACE	0
#define	ARGUMENT	1

static
char **getargs( inline )
char	*inline;
{
	static char envbuf[MAXLINE];
	static char *args[MAXARGS];
	register char *ptr,**answer;
	register int c;
	int state;
	extern int quotec();

	for (ptr= &envbuf[0]; ptr < &envbuf[sizeof(envbuf)];) *ptr++ = '\0';
	for (answer= &args[0]; answer < &args[MAXARGS];)
		*answer++ = (char *)NULL;
	for (ptr= &envbuf[0],answer= &args[0],state = WHITESPACE;
	     (c = getc(stdin)) != EOF;) {

		*(inline++) = c;
		switch (c) {
		case '\n' :
			if (ptr == &envbuf[0]) return((char **)NULL);
			else return(&args[0]);
		case ' ' :
		case '\t' :
			if (state == ARGUMENT) {
				*ptr++ = '\0';
				state = WHITESPACE;
			}
			break;
		case '\\' :
			c = quotec();
		default :
			if (state == WHITESPACE) {
				*answer++ = ptr;
				state = ARGUMENT;
			}
			*ptr++ = c;
		}

/*	If the buffer is full, force the next character to be read to	*/
/*	be a <newline>.							*/

		if (ptr == &envbuf[sizeof(envbuf)-1]) {
			ungetc('\n',stdin);
			putc('\n',stdout);
		}
	}

/*	If we left loop because an EOF was received, exit immediately.	*/

	exit(0);
	/* NOTREACHED */
}

static
int quotec()
{
	register int c,i,num;

	switch(c = getc(stdin)) {
	case 'n' :
		c = '\n';
		break;
	case 'r' :
		c = '\r';
		break;
	case 'v' :
		c = 013;
		break;
	case 'b' :
		c = '\b';
		break;
	case 't' :
		c = '\t';
		break;
	case 'f' :
		c = '\f';
		break;
	case '0' :
	case '1' :
	case '2' :
	case '3' :
	case '4' :
	case '5' :
	case '6' :
	case '7' :
		for (num=0,i=0; i < 3;i++) {
			num = num * 8 + (c - '0');
			if ((c = getc(stdin)) < '0' || c > '7')
				break;
		}
		ungetc(c,stdin);
		c = num & 0377;
		break;
	default :
		break;
	}
	return (c);
}
/*
 * terminal(f): return "/dev/ttyXX" which the the name of the
 * tty belonging to file f.  This routine is the same as ttyname()
 * except that it rejects /dev/syscon and /dev/systty, which are
 * links to other device names.
 *
 * This program works in two passes: the first pass tries to
 * find the device by matching device and inode numbers; if
 * that doesn't work, it tries a second time, this time doing a
 * stat on every file in /dev and trying to match device numbers
 * only. If that fails too, NULL is returned.
 */

static
char *
terminal(f)
int	f;
{
	struct stat	fsb, tsb;
	struct dirent	*dp;
	register DIR	*df;
	register int	pass1;
	static char	rbuf[64];
	static char	dev[]="/dev/";

	if(isatty(f) == 0)
		return(NULL);
	if(fstat( f, &fsb ) < 0)
		return(NULL);
	if((fsb.st_mode & S_IFMT) != S_IFCHR)
		return(NULL);
	if( (df = opendir(dev)) == NULL )
		return(NULL);
	pass1 = 1;
	do {
		while( (dp = readdir(df)) != NULL ) {
			if(pass1  &&  dp->d_ino != fsb.st_ino)
				continue;
			if (strcmp(dp->d_name, "syscon") == 0 ||
				strcmp(dp->d_name, "systty") == 0)
				continue;
			(void) strcpy(rbuf, dev);
			(void) strncat(rbuf, dp->d_name,
				sizeof rbuf - sizeof dev);
			if(stat(rbuf, &tsb) < 0)
				continue;
			if(tsb.st_rdev == fsb.st_rdev &&
				(tsb.st_mode&S_IFMT) == S_IFCHR &&
				(!pass1 || tsb.st_ino == fsb.st_ino)) {
				closedir(df);
				return(rbuf);
			}
		}
		rewinddir(df);
	} while(pass1--);
	closedir(df);
	return(NULL);
}


static
char	*illegal[] = {
		"SHELL=",
		"HOME=",
		"LOGNAME=",
#ifndef	NO_MAIL
		"MAIL=",
#endif
		"CDPATH=",
		"IFS=",
		"PATH=",
		0
	};

/*	Is it legal to insert this environmental variable.
*/
static
int
legalenvvar( s )
char	*s;
{
	register char	**p;

	for( p = illegal;  *p;  p++ )
		if( !strncmp( s, *p, strlen( *p ) ) )
			return  0;
	return  1;
}

#ifdef	SECURITY
/*
 *  lastlogin() -- used to inform the user of the time and
 *        date that this login was last used.  Uses a
 *        file in the login directory called .lastlogin to
 *        keep track of the date/time.  Inode change time is
 *        used to prevent unauthorized changing of file times.
 */
lastlogin()
{
	register int		fd;
	static char		fname[] = ".lastlogin";
	struct stat		s;
	struct utimbuf {
		time_t	actime;
		time_t	modtime;
	} utbuf;
	int	retries;
#define LASTLOGIN_RETRIES 4

	if (stat(fname, &s) == 0) {
		if (s.st_atime != s.st_mtime
		    || s.st_ctime - s.st_atime != 2) {
			printf("Warning:  %s was altered since last login\n",
				fname);
		}
		printf("Login last used: %s", ctime(&s.st_ctime));
	} else {
		printf("Warning: %s did not exist, creating it\n", fname);
		if ((fd = open(fname, O_RDONLY|O_CREAT|O_TRUNC, 0400)) == -1) {
			perror("cannot write file in login directory");
			return;
		}
		close(fd);
	}

	for (retries = 1; ; retries++) {
		utbuf.actime = utbuf.modtime = time((long *)0) - 2;
		utime(fname, &utbuf);
		stat(fname, &s);
		if ( s.st_ctime - s.st_atime == 2 )
			break;
		if (retries >= LASTLOGIN_RETRIES) {
			printf("Warning:  cannot set time for %s\n",fname);
			break;
		};
	};
}

/*
 * badlogin() - log to the system console after
 *     NFAILURES unsuccessful attempts
 */
badlogin(name, ttyn)
char *name;
char *ttyn;
{
#ifdef	RISCOS
	syslog(LOG_WARNING, "REPEATED LOGIN FAILURES ON %s, %.*s", 
		ttyn, NMAX, utmp.ut_name);
#else	RISCOS
	FILE	*console;
	int	clock;

	if ((console = fopen(CONS, "w")) == NULL) {
		perror("cannot write to system console");
		return;
	}
	time(&clock);
	fprintf(console, "\nUNSUCCESSFUL LOGIN: %s on %s: %s", name, ttyn,
		ctime(&clock));
	fclose(console);
#endif	RISCOS
}
#endif	/* SECURITY */


doremotelogin(host)
	char *host;
{
	getstr(rusername, sizeof (rusername), "remuser");
	getstr(lusername, sizeof (lusername), "locuser");
	getstr(term, sizeof(term), "Terminal type");
	if (access(SHADOW,0) == 0) shad_flag = 1;
	if (getuid()) {
		if (shad_flag)
			sp = &noupass;
		pwd = &nouser;
		return(-1);
	}
	pwd = getpwnam(lusername);
	if (pwd == NULL) {
		if (shad_flag)
			sp = &noupass;
		pwd = &nouser;
		return(-1);
	}
	if (shad_flag) {
		sp = getspnam(lusername);
		if (sp == NULL) 	/* entry in passwd file but */
			exit(1);	/* no entry in shadow file  */
	}
	SCPYN(utmp.ut_user, lusername);
	SCPYN(u_name, lusername);
	return(ruserok(host, (pwd->pw_uid == 0), rusername, lusername));
}

char	*speeds[] =
    { "0", "50", "75", "110", "134", "150", "200", "300",
      "600", "1200", "1800", "2400", "4800", "9600", "19200", "38400" };
#define	NSPEEDS	(sizeof (speeds) / sizeof (speeds[0]))

doremoteterm(term)
	char *term;
{
	register char *cp, **cpp;
	char *speed;

	cp = strchr(term, '/');
	if (cp) {
		*cp++ = '\0';
		speed = cp;
		cp = strchr(speed, '/');
		if (cp)
			*cp++ = '\0';
		for (cpp = speeds; cpp < &speeds[NSPEEDS]; cpp++)
			if (strcmp(*cpp, speed) == 0) {
				termp.c_cflag &= ~CBAUD;
				termp.c_cflag |= (cpp-speeds);
				break;
		}
	}

	termp.c_iflag |= (BRKINT|IGNPAR|ISTRIP|ICRNL|IXON);
	termp.c_oflag |= (OPOST|ONLCR);
	termp.c_lflag |= (ISIG|ICANON|ECHO|ECHOE|ECHOK);
}

getstr(buf, cnt, err)
	char *buf;
	int cnt;
	char *err;
{
	char c;

	do {
		if (read(0, &c, 1) != 1)
			exit(1);
		if (--cnt < 0) {
			printf("%s too long\r\n", err);
			exit(1);
		}
		*buf++ = c;
	} while (c != 0);
}

/*
 * Turn all underscores into periods.
 */

fixus(str)
	char *str;
{
	while (str && *str) {
		if (*str == '_') {
			*str = '.';
		}
		str++;
	}
}


#ifdef RISCOS
tty_gid(default_gid)
	int default_gid;
{
	struct group *getgrnam(), *gr;
	int gid = default_gid;

	gr = getgrnam(TTYGRPNAME);
	if (gr != (struct group *) 0)
		gid = gr->gr_gid;

	endgrent();

	return (gid);
}

/*
 * If network is running secure rpc, decrypt this user's key and
 * have it stored away.
 */
setsecretkey(passwd)
	char *passwd;
{
	char fullname[MAXNETNAMELEN + 1];
        char secret[HEXKEYBYTES + 1];

	if (passwd == NULL) {
		return;
	}
	getnetname(fullname);
        if (!getsecretkey(fullname, secret, passwd)) {
		/*
		 * quiet: network does not run secure authentication
		 */
		return;
	}
	if (secret[0] == 0) {
		syslog(LOG_WARNING,
"Password does not decrypt secret key for %s.\r\n", fullname);
		return;
	}
	if (key_setsecret(secret) < 0) {
		syslog(LOG_WARNING,
"Could not set %s's secret key: is the keyserv daemon running?\n", fullname);
		return;
	}
}

#endif


#ifdef RISCOS
int login_timeout()
{
	syslog(LOG_WARNING, "LOGIN TIMEOUT ON %s, %.*s", 
		(login_timeout_ttyn != NULL ? login_timeout_ttyn : "?"), 
		NMAX, utmp.ut_name);
	exit(1);
}
#endif RISCOS


#ifdef CONSOLE
is_same_tty(a,b)
	char	*a;
	char	*b;
{
	struct	stat	sa;
	struct	stat	sb;

	if ( (stat(a, &sa) != -1)
	   && (stat(b, &sb) != -1)
	   && ((sa.st_mode & S_IFMT) == S_IFCHR)
	   && ((sb.st_mode & S_IFMT) == S_IFCHR)
	   && (sa.st_rdev == sb.st_rdev) )
		return 1;
	else
		return 0;
}
#endif CONSOLE
