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
#ident	"$Header: msgsys.c,v 1.7.2.2 90/05/09 20:02:04 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"sys/types.h"
#include	"sys/ipc.h"
#include	"sys/msg.h"
#include	"sys.s"

#define	MSGSYS	SYS_msgsys

#define	MSGGET	0
#define	MSGCTL	1
#define	MSGRCV	2
#define	MSGSND	3

msgget(key, msgflg)
key_t key;
int msgflg;
{
	return(syscall(MSGSYS, MSGGET, key, msgflg));
}

msgctl(msqid, cmd, buf)
int msqid, cmd;
struct msqid_ds *buf;
{
	return(syscall(MSGSYS, MSGCTL, msqid, cmd, buf));
}

msgrcv(msqid, msgp, msgsz, msgtyp, msgflg)
int msqid;
struct msgbuf *msgp;
int msgsz;
long msgtyp;
int msgflg;
{
	return(syscall(MSGSYS, MSGRCV, msqid, msgp, msgsz, msgtyp, msgflg));
}

msgsnd(msqid, msgp, msgsz, msgflg)
int msqid;
struct msgbuf *msgp;
int msgsz, msgflg;
{
	return(syscall(MSGSYS, MSGSND, msqid, msgp, msgsz, msgflg));
}
