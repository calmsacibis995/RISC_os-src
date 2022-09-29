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
#ident	"$Header: havenfs.c,v 1.8.1.2 90/05/09 19:12:02 wje Exp $"

/*
 * Exit 0 if the running kernel supports nfs, non-zero otherwise.
 */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/errno.h>
typedef int bool_t;
#include <nfs/nfs.h>
#include <signal.h>

havent_nfs()
{
	exit(-1);
}

main()
{
	fhandle_t fh;

	(void) signal(SIGSYS, havent_nfs);
	if (getfh(0, &fh) < 0) {
		exit(0);
	}
	exit(0);
}
