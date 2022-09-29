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
#ident	"$Header: closewtmp.c,v 1.1.1.2 90/05/10 20:14:18 wje Exp $"
#ifndef lint
static char * _Sccs_Id_closewtmp_c = "@(#)closewtmp.c   1.2 2/21/90 - DR";
#endif !lint

/*      fludge an entry to wtmp for each user who is still logs on when
 *      acct is being run. This entry marks a DEAD_PROCESS, and the
 *      current time as time stamp. This should be done before connect
 *      time is processed. Call by runacct.
 */

#include <stdio.h>
#include <sys/types.h>
#include <utmp.h>

main()
{
        struct utmp *getutent(), *utmp;
        FILE *fp;

        fp = fopen(WTMP_FILE, "r+");
        while ((utmp=getutent()) != NULL) {
                if (utmp->ut_type == USER_PROCESS) {
                        utmp->ut_type = DEAD_PROCESS;
                        time( &utmp->ut_time );
                        fseek(fp,0L,2);
                        fwrite( utmp, sizeof(*utmp), 1, fp);
                }
        }
        fclose(fp);
}
