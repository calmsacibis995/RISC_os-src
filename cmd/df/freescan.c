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
#ident	"$Header: freescan.c,v 1.4.1.2 90/05/09 15:42:25 wje Exp $"

#include <stdio.h>
#include <errno.h>
#include <sun/mntent.h>
#include <sys/types.h>
#include <sys/buf.h>

#include <sys/statfs.h>

/*
 * Filesystem-dependent code.
 */
#include <values.h>

int
scan_freelist(mntp, sfbp)
	register struct mntent *mntp;
	register struct statfs *sfbp;
{

	if (strcmp(mntp->mnt_type, MNTTYPE_BELL) != 0) {
		return 0;
	}
	fprintf(stderr, "df: %s file system no longer supported\n");
	return -1;
}

