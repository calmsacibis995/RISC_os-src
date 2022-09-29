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
#ident	"$Header: utmp2wtmp.c,v 1.1.1.2 90/05/10 20:15:23 wje Exp $"
#ifndef lint
static char * _Sccs_Id_utmp2wtmp_c = "@(#)utmp2wtmp.c   1.2 2/21/90 - DR";
#endif !lint

/* create entries for users who are still logged on when accounting
 *      is being run. Look at utmp, and update the time from utmp to
 *      wtmp. Call by runacct.
 */

#include <stdio.h>
#include <sys/types.h>
#include <utmp.h>

long time();    /* no see */

main()
{
        struct utmp *getutent(), *utmp;
        FILE *fp;

        fp = fopen(WTMP_FILE, "r+");
        while ((utmp=getutent()) != NULL) {
                if (utmp->ut_type == USER_PROCESS) {
                        time( &utmp->ut_time );
                        fseek(fp,0L,2);
                        fwrite( utmp, sizeof(*utmp), 1, fp);
                }
        }
        fclose(fp);
}
