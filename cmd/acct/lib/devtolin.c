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
#ident	"$Header: devtolin.c,v 1.1.2.2 90/05/09 15:05:57 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	convert device to linename (as in /dev/linename)
 *	return ptr to LSZ-byte string, "?" if not found
 *	device must be character device
 *	maintains small list in tlist for speed
 */

#include <sys/types.h>
#include "acctdef.h"
#include <stdio.h>
#include <sys/fs/s5dir.h>

#define TSIZE1	50	/* # distinct names, for speed only */
static	tsize1;
static struct tlist {
	char	tname[LSZ];	/* linename */
	dev_t	tdev;		/* device */
} tl[TSIZE1];

static struct direct d;

dev_t	lintodev();

char *
devtolin(device)
dev_t device;
{
	register struct tlist *tp;
	FILE *fdev;

	for (tp = tl; tp < &tl[tsize1]; tp++)
		if (device == tp->tdev)
			return(tp->tname);

	if ((fdev = fopen("/dev", "r")) == NULL)
		return("?");
	while (fread(&d, sizeof(d), 1, fdev) == 1)
		if (d.d_ino != 0 && lintodev(d.d_name) == device) {
			if (tsize1 < TSIZE1) {
				tp->tdev = device;
				CPYN(tp->tname, d.d_name);
				tsize1++;
			}
			fclose(fdev);
			return(d.d_name);
		}
	fclose(fdev);
	return("?");
}
