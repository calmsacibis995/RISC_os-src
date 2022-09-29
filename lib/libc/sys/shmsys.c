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
#ident	"$Header: shmsys.c,v 1.7.2.2 90/05/09 20:02:15 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef	pdp11
#include	"sys/types.h"
#include	"sys/ipc.h"
#include	"sys/shm.h"
#include	"sys.s"

#define	SHMSYS	SYS_shmsys

#define	SHMAT	0
#define	SHMCTL	1
#define	SHMDT	2
#define	SHMGET	3

shmat(shmid, shmaddr, shmflg)
int shmid;
char *shmaddr;
int shmflg;
{
	return(syscall(SHMSYS, SHMAT, shmid, shmaddr, shmflg));
}

shmctl(shmid, cmd, buf)
int shmid, cmd;
struct shmid_ds *buf;
{
	return(syscall(SHMSYS, SHMCTL, shmid, cmd, buf));
}

shmdt(shmaddr)
char *shmaddr;
{
	return(syscall(SHMSYS, SHMDT, shmaddr));
}

shmget(key, size, shmflg)
key_t key;
int size, shmflg;
{
	return(syscall(SHMSYS, SHMGET, key, size, shmflg));
}
#endif
