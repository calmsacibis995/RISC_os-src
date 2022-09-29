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
#ident	"$Header: version.c,v 1.2.2.3 90/05/09 18:11:23 wje Exp $"

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "INTERN.h"
#include "patchlevel.h"
#include "version.h"
#include <sys/utsname.h>
static struct utsname	un;

/* Print out the version number and die. */

void
version()
{
    extern char rcsid[];
    char *osname;
    char buf[100];

#ifdef lint
    rcsid[0] = rcsid[0];
#else
    uname( &un );
    fixus(un.release);
    osname = un.version;
    if (! strcmp(osname,"UMIPS")) 
	osname = "RISC/os";
    (void)sprintf(buf,"%s Version: %s",osname,un.release);
    fatal3("%s\nPatch level: %d\n", buf, PATCHLEVEL);
#endif
}
/*
 * Turn all underscores into periods.
 */

static fixus(str)
	char *str;
{
	while (str && *str) {
		if (*str == '_') {
			*str = '.';
		}
		str++;
	}
}
