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
#ident	"$Header: getrusage.c,v 1.1.2.2 90/05/07 21:17:55 wje Exp $"

#include <sys/time.h>
#include <sys/resource.h>

/*
 * Getrusage uses the "real" mips_getrusage() system call so that when rusage
 * is extended to contain mips specific information old programs will work
 * that used this.
 */
getrusage(who, rusage)
int who;
struct rusage *rusage;
{
	return(mips_getrusage(who, rusage, sizeof(struct rusage)));
}
