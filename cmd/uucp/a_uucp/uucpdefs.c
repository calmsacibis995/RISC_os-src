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
#ident	"$Header: uucpdefs.c,v 1.4.2.2 90/05/10 00:37:42 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "uucp.h"

int	Ifn, Ofn;
int	Debug = 0;
int	Uid, Euid;		/* user-id and effective-uid */
char	Progname[NAMESIZE];
char	Pchar;
char	Rmtname[MAXFULLNAME];
char	RemSpool[MAXFULLNAME];	/* spool subdirectory for remote system */
char	User[MAXFULLNAME];
char	Uucp[NAMESIZE];
char	Loginuser[NAMESIZE];
char	Myname[MAXBASENAME+1];
char	Wrkdir[MAXFULLNAME];
char	Logfile[MAXFULLNAME];
char	*Spool = SPOOL;
char	*Pubdir = PUBDIR;
char	**Env;

extern int	read();
extern int	write();
extern int	ioctl();

long	Retrytime = 0;
struct	nstat Nstat;
char	Dc[50];			/* line name				*/
int	Seqn;			/* sequence #				*/
int	Role;
char	*Bnptr;			/* used when BASENAME macro is expanded */
char	Jobid[NAMESIZE] = "";	/* Jobid of current C. file */
int	Uerror;			/* global error code */

#ifdef STANDALONE
int	Verbose = 0;	/* for cu and ct only */
#endif

/* used for READANY and READSOME macros */
struct stat __s_;

/* messages */
char	*Ct_OPEN =	"CAN'T OPEN";
char	*Ct_WRITE =	"CAN'T WRITE";
char	*Ct_READ =	"CAN'T READ";
char	*Ct_CREATE =	"CAN'T CREATE";
char	*Ct_ALLOCATE =	"CAN'T ALLOCATE";
char	*Ct_LOCK =	"CAN'T LOCK";
char	*Ct_STAT =	"CAN'T STAT";
char	*Ct_CHOWN =	"CAN'T CHOWN";
char	*Ct_CHMOD =	"CAN'T CHMOD";
char	*Ct_LINK =	"CAN'T LINK";
char	*Ct_CHDIR =	"CAN'T CHDIR";
char	*Ct_UNLINK =	"CAN'T UNLINK";
char	*Wr_ROLE =	"WRONG ROLE";
char	*Ct_CORRUPT =	"CAN'T MOVE TO CORRUPTDIR";
char	*Ct_CLOSE =	"CAN'T CLOSE";
char	*Ct_FORK =	"CAN'T FORK";
char	*Fl_EXISTS =	"FILE EXISTS";

char *UerrorText[] = {
  /* SS_OK			0 */ "SUCCESSFUL",
  /* SS_NO_DEVICE		1 */ "NO DEVICES AVAILABLE",
  /* SS_TIME_WRONG		2 */ "WRONG TIME TO CALL",
  /* SS_INPROGRESS		3 */ "TALKING",
  /* SS_CONVERSATION		4 */ "CONVERSATION FAILED",
  /* SS_SEQBAD			5 */ "BAD SEQUENCE CHECK",
  /* SS_LOGIN_FAILED		6 */ "LOGIN FAILED",
  /* SS_DIAL_FAILED		7 */ "DIAL FAILED",
  /* SS_BAD_LOG_MCH		8 */ "BAD LOGIN/MACHINE COMBINATION",
  /* SS_LOCKED_DEVICE		9 */ "DEVICE LOCKED",
  /* SS_ASSERT_ERROR		10 */ "ASSERT ERROR",
  /* SS_BADSYSTEM		11 */ "SYSTEM NOT IN Systems FILE",
  /* SS_CANT_ACCESS_DEVICE	12 */ "CAN'T ACCESS DEVICE",
  /* SS_DEVICE_FAILED		13 */ "DEVICE FAILED",
  /* SS_WRONG_MCH		14 */ "WRONG MACHINE NAME",
  /* SS_CALLBACK		15 */ "CALLBACK REQUIRED",
  /* SS_RLOCKED			16 */ "REMOTE HAS A LCK FILE FOR ME",
  /* SS_RUNKNOWN		17 */ "REMOTE DOES NOT KNOW ME",
  /* SS_RLOGIN			18 */ "REMOTE REJECT AFTER LOGIN",
  /* SS_UNKNOWN_RESPONSE	19 */ "REMOTE REJECT, UNKNOWN MESSAGE",
  /* SS_STARTUP			20 */ "STARTUP FAILED",
  /* SS_CHAT_FAILED		21 */ "CALLER SCRIPT FAILED",
};
