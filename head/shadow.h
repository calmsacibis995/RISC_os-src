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
/* $Header: shadow.h,v 1.1.1.3 90/05/10 01:04:02 wje Exp $ */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#define PASSWD 		"/etc/passwd"
#define SHADOW		"/etc/shadow"
#define OPASSWD		"/etc/opasswd"
#define OSHADOW 	"/etc/oshadow"
#define PASSTEMP	"/etc/ptmp"
#define SHADTEMP	"/etc/stmp"

#define	DAY		(24L * 60 * 60) /* 1 day in seconds */
#define DAY_NOW		(long)time((long *)0) / DAY

/* The spwd structure is used in the retreval of information from
   /etc/shadow.  It is used by routines in the libsec library */

struct spwd {
	char *sp_namp ; /* user name */
	char *sp_pwdp ; /* user password */
	long sp_lstchg ; /* password lastchanged date */
	long sp_min ; /* minimum number of days between password changes */
	long sp_max ; /* number of days password is valid */
} ;

/* Declare all shadow password functions */

void 		setspent(), endspent() ;
struct spwd 	*getspent(), *fgetspent(), *getspnam() ;
int 		putspent(), lckpwdf(), ulckpwdf() ;
