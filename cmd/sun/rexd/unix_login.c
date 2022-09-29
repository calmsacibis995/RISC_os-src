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
#ident	"$Header: unix_login.c,v 1.1.1.7 90/05/22 18:25:38 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)unix_login.c	1.4 88/07/26 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

# include <rpc/types.h>
# include <sys/ioctl.h>
# include <sys/signal.h>
# include <sys/file.h>
# include <pwd.h>
# include <errno.h>
# include <stdio.h>
# include <utmp.h>
# include <signal.h>

#include <rpcsvc/rex.h>

#ifdef RISCOS
#include <sys/syslog.h>
#include <sys/stat.h>
extern int errno;
char line[20];
static struct utmp entry;
#endif
/*
 * unix_login - hairy junk to simulate logins for Unix
 *
 * Copyright (c) 1985 Sun Microsystems, Inc.
 */

char Ttys[] = "/etc/ttys";	/* file to get index of utmp */
char Utmp[] = "/etc/utmp";	/* the tty slots */
#ifdef RISCOS
char Wtmp[] = WTMP_FILE;	/* the log information */
#else
char Wtmp[] = "/usr/adm/wtmp";	/* the log information */
#endif

int Master, Slave;		/* sides of the pty */
int InputSocket, OutputSocket;	/* Network sockets */
int Helper1, Helper2;		/* pids of the helpers */
char UserName[256];		/* saves the user name for loging */
char HostName[256];		/* saves the host name for loging */
char PtyName[16] = "/dev/ttypn";/* name of the tty we allocated */
static int TtySlot;		/* slot number in Utmp */

extern fd_set svc_fdset;	/* master file descriptor set */
extern int child;		/* pid of the executed process */
extern int ChildDied;		/* flag */
extern int HasHelper;		/* flag */

/*
 * Check for user being able to run on this machine.
 * returns 0 if OK, TRUE if problem, error message in "error"
 * copies name of shell and home directory if user is valid.
 */
ValidUser(host, uid, error, shell, dir, cmd)
    char *host;		/* passed in */
    int uid;
    char *error;	/* filled in on return */
    char *shell;	/* filled in on return */
    char *cmd;		/* passed in */
{
    struct passwd *pw, *getpwuid();
    struct passwd_adjunct *apw, *getpwanam();

    if (uid == 0) {
    	errprintf(error,"rexd: root execution not allowed\n",uid);
	return(1);
    }
    pw = getpwuid(uid);
    if (pw == NULL || pw->pw_name == NULL) {
    	errprintf(error,"rexd: User id %d not valid\n",uid);
	return(1);
    }
    strncpy(UserName, pw->pw_name, sizeof(UserName)-1 );
    strncpy(HostName, host, sizeof(HostName)-1 );
    strcpy(shell,pw->pw_shell);
    strcpy(dir,pw->pw_dir);
    setproctitle(pw->pw_name, host);
    return(0);
}

/*
 *  eliminate any controlling terminal that we have already
 */
NoControl()
{
    int devtty;

    devtty = open("/dev/tty",O_RDWR);
    if (devtty > 0) {
    	    ioctl(devtty, TIOCNOTTY, NULL);
	    close(devtty);
    }
}

/*
 * Allocate a pseudo-terminal
 * sets the global variables Master and Slave.
 * returns 1 on error, 0 if OK
 */
AllocatePtyMaster(socket0, socket1)
    int socket0, socket1;
{
# define Sequence "0123456789abcdef"
# define MajorPos 8	/* /dev/ptyXx */
# define MinorPos 9	/* /dev/ptyxX */
# define SidePos 5	/* /dev/Ptyxx */
#ifdef RISCOS
/* do this with the streams clone mechansism to be consistant with other
 * applications
 */
    struct stat stb;
    int	i,p;
    int on = 1;
    int	rdev;

    p = open("/dev/ptcm", O_RDWR | O_NDELAY);
    if (p < 0 || fstat(p, &stb) < 0) {
	    if (p >= 0) {
		    close(p);
		    p = -1;
	    };
    } else {
	    rdev = stb.st_rdev;
	    for (i = 0; i <= 9; i++) {
		    sprintf(&line[0],"/dev/ptmc%d",i);
		    if (stat(line, &stb) >= 0 &&
			major(rdev) == major(stb.st_rdev))
			    break;
	    }
	    if (i > 9) {
		    close(p);
		    p = -1;
	    } else {
		    sprintf(&line[0],"/dev/ttyq%d",
			    (i * 256) + minor(rdev));
	    }
    }
    if (p < 0) {
	    p = open("/dev/ptc", O_RDWR | O_NDELAY);
	    if (p < 0 || fstat(p, &stb) < 0)
		    return(1);	/* caller complains and makes log entry */
	    sprintf(&line[0], "/dev/ttyq%d", minor(stb.st_rdev));
    }
    Master = p;
    strcpy(PtyName,line);
    NoControl();
    LoginUser();
    InputSocket = socket0;
    OutputSocket = socket1;
    ioctl(Master, FIONBIO, &on);
    FD_SET(InputSocket, &svc_fdset);
    FD_SET(Master, &svc_fdset);
    return(0);
#else
    static char ptySequence[] = Sequence;
    char maj, min;
    int pgrp;
    int on = 1;

    signal(SIGHUP,SIG_IGN);
    signal(SIGTTOU,SIG_IGN);
    signal(SIGTTIN,SIG_IGN);
    for (maj = 'p'; maj <= 's'; maj++)
      for (min = 0; min < strlen(Sequence); min++ ) {
	    PtyName[MajorPos] = maj;
	    PtyName[MinorPos] = ptySequence[min];
	    PtyName[SidePos] = 'p';
	    Master = open(PtyName, O_RDWR);
	    if (Master < 0) {
		continue;
	    }
	    NoControl();
	    LoginUser();
	    pgrp = getpid();
	    setpgrp(pgrp, pgrp);
	    InputSocket = socket0;
	    OutputSocket = socket1;
	    ioctl(Master, FIONBIO, &on);
	    FD_SET(InputSocket, &svc_fdset);
	    FD_SET(Master, &svc_fdset);
	    return(0);
	}
    /*
     * No pty found!
     */
    return(1);
#endif
}


AllocatePtySlave()
{
#ifdef RISCOS
  int pgrp;
#endif
  PtyName[SidePos] = 't';

#ifdef RISCOS
  close(0);
  pgrp = getpid();
  if (setpgrp(0, pgrp) < 0)
  {
    syslog(LOG_ERR,"setpgrp failed in AllocatePtySlave");
  }
#endif
  Slave = open (PtyName, O_RDWR);
  if (Slave < 0) {
#ifdef RISCOS
    syslog(LOG_ERR,"open of slave failed %s",PtyName);
#else
    perror (PtyName);
#endif
    exit (1);
  }
#ifdef RISCOS
  ioctl(Slave, TIOCSPGRP, &pgrp);
#endif
}



  /*
   * Special processing for interactive operation.
   * Given pointers to three standard file descriptors,
   * which get set to point to the pty.
   */
DoHelper(pfd0, pfd1, pfd2)
    int *pfd0, *pfd1, *pfd2;
{
    int pgrp;

#ifndef RISCOS
    /* this doesn't work for RISCos because the next open of a terminal
     * is what sets the controlling terminal, and at this pt, we have the
     * slave open already
     */
    pgrp = getpid();
    if (setpgrp(0, pgrp) < 0)
    {
	syslog(LOG_ERR,"setpgrp failed in DoHelper");
    }
    ioctl(Slave, TIOCSPGRP, &pgrp);
#endif

    signal( SIGINT, SIG_IGN);
    close(Master);
    close(InputSocket);
    close(OutputSocket);

    *pfd0 = Slave;
    *pfd1 = Slave;
    *pfd2 = Slave;
}


/*
 * destroy the helpers when the executing process dies
 */
KillHelper(grp)
    int grp;
{
    close(Master);
    FD_CLR(Master, &svc_fdset);
    close(InputSocket);
    FD_CLR(InputSocket, &svc_fdset);
    close(OutputSocket);
#ifdef RISCOS
    FD_CLR(OutputSocket, &svc_fdset);
#endif
    LogoutUser();
    if (grp) killpg(grp,SIGKILL);
}


/*
 * edit the Unix traditional data files that tell who is logged
 * into "the system"
 */
#ifdef RISCOS
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#endif
LoginUser()
{
#ifdef RISCOS
	register FILE *fp;

	SCPYN(entry.ut_user, UserName);
	SCPYN(entry.ut_id, &line[(sizeof("/dev/tty") - 1)]);
	SCPYN(entry.ut_line, line+sizeof("/dev/")-1);
	entry.ut_pid = getpid();
	entry.ut_type = USER_PROCESS;
	entry.ut_time = time(0);
	setutent();
	(void)pututline(&entry);
	endutent();

/* Now attempt to add to the end of the wtmp file.  Do not create
 * if it does not already exist.  **  Note  ** This is the reason
 * "r+" is used instead of "a+".  "r+" will not create a file, while
 * "a+" will. */
	if ((fp = fopen("/etc/wtmp","r+")) != NULL) {
		fseek(fp,0L,2);
		fwrite((char*)&entry,sizeof(entry),1,fp);
		fclose(fp);
	}
#else
  FILE *ttysFile;
  register char *last = PtyName + sizeof("/dev");
  char line[256];
  int count;
  int utf;
  struct utmp utmp;
  
  ttysFile = fopen(Ttys,"r");
  TtySlot = 0;
  count = 0;
  if (ttysFile != NULL) {
      while (fgets(line, sizeof(line), ttysFile) != NULL) {
        register char *lp;
	lp = line + strlen(line) - 1;
	if (*lp == '\n') *lp = '\0';
	count++;
	if (strcmp(last,line+2)==0) {
	  TtySlot = count;
	  break;
	}
      }
      fclose(ttysFile);
  }
  if (TtySlot > 0 && (utf = open(Utmp,O_WRONLY)) >= 0) {
      lseek(utf, TtySlot*sizeof(utmp), L_SET);
      strncpy(utmp.ut_line,last,sizeof(utmp.ut_line));
      strncpy(utmp.ut_name,UserName,sizeof(utmp.ut_name));
#ifndef RISCOS
      strncpy(utmp.ut_host,HostName,sizeof(utmp.ut_host));
#endif
      time(&utmp.ut_time);
      write(utf, (char *)&utmp, sizeof(utmp));
      close(utf);
  }
  if (TtySlot > 0 && (utf = open(Wtmp,O_WRONLY)) >= 0) {
      lseek(utf, (long)0, L_XTND);
      write(utf, (char *)&utmp, sizeof(utmp));
      close(utf);
  }
#endif
}


/*
 * edit the Unix traditional data files that tell who is logged
 * into "the system".
 */
LogoutUser()
{
#ifdef RISCOS
	register FILE *fp;

	SCPYN(entry.ut_user, "rexd");
	SCPYN(entry.ut_id, &line[(sizeof("/dev/tty") - 1)]);
	SCPYN(entry.ut_line, line+sizeof("/dev/")-1);
	entry.ut_pid = getpid();
	entry.ut_type = DEAD_PROCESS;
	entry.ut_time = time(0);
	setutent();
	(void)pututline(&entry);
	endutent();
	if ((fp = fopen("/etc/wtmp","r+")) != NULL) {
		entry.ut_user[0] = '\0';
		fseek(fp,0L,2);
		fwrite((char*)&entry,sizeof(entry),1,fp);
		fclose(fp);
	}
#else
  int utf;
  register char *last = PtyName + sizeof("/dev");
  struct utmp utmp;

  if (TtySlot > 0 && (utf = open(Utmp,O_RDWR)) >= 0) {
      lseek(utf, TtySlot*sizeof(utmp), L_SET);
      read(utf, (char *)&utmp, sizeof(utmp));
      if (strncmp(last,utmp.ut_line,sizeof(utmp.ut_line))==0) {
	lseek(utf, TtySlot*sizeof(utmp), L_SET);
	strcpy(utmp.ut_name,"");
#ifndef RISCOS
	strcpy(utmp.ut_host,"");
#endif
	time(&utmp.ut_time);
	write(utf, (char *)&utmp, sizeof(utmp));
      }
      close(utf);
  }
  if (TtySlot > 0 && (utf = open(Wtmp,O_WRONLY)) >= 0) {
      lseek(utf, (long)0, L_XTND);
      strncpy(utmp.ut_line,last,sizeof(utmp.ut_line));
      write(utf, (char *)&utmp, sizeof(utmp));
      close(utf);
  }
  TtySlot = 0;
#endif
}


/*
 * set the pty modes to the given values
 */
SetPtyMode(mode)
    struct rex_ttymode *mode;
{
    int ldisc = NTTYDISC;
    
    PtyName[SidePos] = 't';
    Slave = open (PtyName, O_RDWR);
    if (Slave < 0) {
#ifdef RISCOS
      syslog(LOG_ERR,"open of pty failed %s",PtyName);
#else
      perror (PtyName);
#endif
      exit (1);
    }
    ioctl(Slave, TIOCSETD, &ldisc);
    ioctl(Slave, TIOCSETN, &mode->basic);
    ioctl(Slave, TIOCSETC, &mode->more);
    ioctl(Slave, TIOCSLTC, &mode->yetmore);
    ioctl(Slave, TIOCLSET, &mode->andmore);
    close (Slave);
}

/*
 * set the pty window size to the given value
 */
SetPtySize(size)
#ifdef RISCOS
    struct rex_ttysize *size;
#else
    struct ttysize *size;
#endif
{
    int pgrp;
    struct ttysize OurSize;

    PtyName[SidePos] = 't';
    Slave = open (PtyName, O_RDWR);
    if (Slave < 0) {
#ifdef RISCOS
	syslog(LOG_ERR,"cannot open %s, errno %d",PtyName,errno);
#else
      perror (PtyName);
#endif
      exit (1);
    }
#ifdef RISCOS
    OurSize.ts_cols = size->ts_cols;
    OurSize.ts_lines = size->ts_lines;
    if (ioctl(Slave, TIOCSSIZE, &OurSize) < 0)
    {
	syslog(LOG_ERR,"TIOCSSIZE fails for %s",PtyName);
    }
#else
    (void) ioctl(Slave, TIOCSSIZE, size);
#endif
    SendSignal(SIGWINCH);
    close (Slave);
}


/*
 * send the given signal to the group controlling the terminal
 */
SendSignal(sig)
    int sig;
{
    int pgrp;

    if (ioctl(Slave, TIOCGPGRP, &pgrp) >= 0)
    	(void) killpg( pgrp, sig);
}


/*
 * called when the main select loop detects that we might want to
 * read something.
 */
HelperRead(fdp)
	fd_set *fdp;
{
    char buf[128];
    int cc;
    extern int errno;
    int mask;

    mask = sigsetmask (sigmask (SIGCHLD));
    if (FD_ISSET(Master, fdp)) {
	FD_CLR(Master, fdp);
    	cc = read(Master, buf, sizeof buf);
	if (cc > 0)
		(void) write(OutputSocket, buf, cc);
	else {
	  	shutdown(OutputSocket, 1);
		FD_CLR(Master, &svc_fdset);
		if (cc < 0 && errno != EINTR && errno != EWOULDBLOCK &&
		    errno != EIO)
#ifdef RISCOS
		    syslog(LOG_ERR,"pty master read error errno %d",errno);
#else
		        perror ("pty read");
#endif
		if (ChildDied) {
		  KillHelper (child);
		  HasHelper = 0;
		  if (FD_ISSET(InputSocket, fdp))
		    FD_CLR(InputSocket, fdp);
		  goto done;
		}
	}
    }
    if (FD_ISSET(InputSocket, fdp)) {
	FD_CLR(InputSocket, fdp);
    	cc = read(InputSocket, buf, sizeof buf);
	if (cc > 0)
		(void) write(Master, buf, cc);
	else {
		if (cc < 0 && errno != EINTR && errno != EWOULDBLOCK)
#ifdef RISCOS
			syslog(LOG_ERR,"socket read error errno %d",errno);
#else
			perror("socket read");
#endif
		FD_CLR(InputSocket, &svc_fdset);
	}
    }
  done:
    sigsetmask (mask);
}
