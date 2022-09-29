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
#ident	"$Header: uname.c,v 1.13.2.2 90/05/10 00:31:53 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include        <stdio.h>
#include        <fcntl.h>
#include        "sys/limits.h"
#include        "sys/utsname.h"

struct utsname  unstr, *un;

extern void exit();
extern int fprintf();
extern int uname();
extern int optind;
extern char *optarg;

main(argc, argv)
char **argv;
int argc;
{
	char *optstring="asnrvmAtb";
        int sflg=0, nflg=0, rflg=0, vflg=0, mflg=0, errflg=0, optlet;
	/* MIPS' specific flags */
	int tflg=0, bflg=0;

        char new_sysname[SYS_NMLN]; /* buffer for new system name */
	char *sysname;
	int len;

        un = &unstr;
        uname(un);

        while((optlet=getopt(argc, argv, optstring)) != EOF)
		switch(optlet) {
		/* "-A" returns all "-a" info plus all MIPS' info */
		case 'A':
			tflg++; bflg++;
		case 'a':
			sflg++; nflg++; rflg++; vflg++; mflg++;
			break;
		case 's':
			sflg++;
			break;
		case 'n':
			nflg++;
			break;
		case 'r':
			rflg++;
			break;
		case 'v':
			vflg++;
			break;
		case 'm':
			mflg++;
			break;
		case 't':
			tflg++;
			break;
		case 'b':
			bflg++;
			break;
		case '?':
			errflg++;
	        }

        if(errflg || (optind != argc))
		usage();

        if( !(sflg || nflg || rflg || vflg || mflg || tflg || bflg))
		sflg++;			/* "uname -s" is the default */
        if(sflg)
                (void) fprintf(stdout, "%.*s", sizeof(un->sysname), un->sysname);
        if(nflg) {
                if(sflg) (void) putchar(' ');
                (void) fprintf(stdout, "%.*s", sizeof(un->nodename), un->nodename);
        }
        if(rflg) {
                if(sflg || nflg) (void) putchar(' ');
                (void) fprintf(stdout, "%.*s", sizeof(un->release), un->release);
        }
        if(vflg) {
                if(sflg || nflg || rflg) (void) putchar(' ');
                (void) fprintf(stdout, "%.*s", sizeof(un->version), un->version);
        }
        if(mflg) {
                if(sflg || nflg || rflg || vflg) (void) putchar(' ');
                (void) fprintf(stdout, "%.*s", sizeof(un->machine), un->machine);
        }
        if(tflg) {
                if(sflg || nflg || rflg || vflg || mflg) (void) putchar(' ');
                (void) fprintf(stdout, "%.*s", sizeof(un->m_type), un->m_type);
        }
        if(bflg) {
                if(sflg || nflg || rflg || vflg || mflg || tflg)
			(void) putchar(' ');
                (void) fprintf(stdout, "%.*s", sizeof(un->base_rel),
			un->base_rel);
        }
        (void) putchar('\n');
        exit(0);
}

usage()
{
	(void) fprintf(stderr, "usage:  uname [-snrvmatbA]\n");

	exit(1);
}
