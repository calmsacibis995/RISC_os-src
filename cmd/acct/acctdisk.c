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
#ident	"$Header: acctdisk.c,v 1.1.2.2 90/05/09 15:02:20 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	acctdisk <dtmp >dtacct
 *	reads std.input & converts to tacct.h format, writes to output
 *	input:
 *	uid	name	#blocks
 */

#include <sys/types.h>
#include "acctdef.h"
#include <stdio.h>
#include "tacct.h"

struct	tacct	tb;
char	ntmp[NSZ+1];

main(argc, argv)
char **argv;
{
	tb.ta_dc = 1;
	while(scanf("%hu\t%s\t%f",
		&tb.ta_uid,
		ntmp,
		&tb.ta_du) != EOF) {

		CPYN(tb.ta_name, ntmp);
		fwrite(&tb, sizeof(tb), 1, stdout);
	}
}
