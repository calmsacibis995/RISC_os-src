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
#ident	"$Header: semsys.c,v 1.7.2.2 90/05/09 20:02:09 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"sys/types.h"
#include	"sys/ipc.h"
#include	"sys/sem.h"
#include	"sys.s"

#define	SEMSYS	SYS_semsys

#define	SEMCTL	0
#define	SEMGET	1
#define	SEMOP	2

semctl(semid, semnum, cmd, arg)
int semid, cmd;
int semnum;
union semun {
	int val;
	struct semid_ds *buf;
	ushort array[1];
} arg;
{
	return(syscall(SEMSYS, SEMCTL, semid, semnum, cmd, arg));
}

semget(key, nsems, semflg)
key_t key;
int nsems, semflg;
{
	return(syscall(SEMSYS, SEMGET, key, nsems, semflg));
}

semop(semid, sops, nsops)
int semid;
struct sembuf (*sops)[];
int nsops;
{
	return(syscall(SEMSYS, SEMOP, semid, sops, nsops));
}
