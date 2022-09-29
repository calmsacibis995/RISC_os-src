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
#ident	"$Header: rcsdiff.c,v 1.3.1.4 90/05/10 00:28:03 wje Exp $"

/*
 *                     RCS rcsdiff operation
 */
/*****************************************************************************
 *                       generate difference between RCS revisions
 *****************************************************************************
 *
 * Copyright (C) 1982 by Walter F. Tichy
 *                       Purdue University
 *                       Computer Science Department
 *                       West Lafayette, IN 47907
 *
 * All rights reserved. No part of this software may be sold or distributed
 * in any form or by any means without the prior written permission of the
 * author.
 * Report problems and direct all inquiries to Tichy@purdue (ARPA net).
 */

#include "rcsbase.h"

extern int    cleanup();            /* cleanup after signals                */
extern char * mktempfile();         /*temporary file name generator         */
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern int    nerror;               /*counter for errors                    */
extern FILE * finptr;               /* RCS input file                       */

char *RCSfilename;
char *workfilename;
char * temp1file, * temp2file;

char bops[10] = "-";

main (argc, argv)
int argc; char **argv;
{
        char * cmdusage;
        char command[NCPPN+revlength+40];
        int  revnums;                 /* counter for revision numbers given */
        char * rev1, * rev2;          /* revision numbers from command line */
        char numericrev[revlength];   /* holds expanded revision number     */
        char * xrev1, * xrev2;        /* expanded revision numbers          */
        struct hshentry * gendeltas[hshsize];/*stores deltas to be generated*/
        struct hshentry * target;
        char * boption, * otheroption;
        int  exit_stats;
        int  filecounter;
	char *argp;
	register c;

        catchints();
        otheroption="";
	boption = bops + 1;
        cmdid = "rcsdiff";
        cmdusage = "command format:\n    rcsdiff [-biwt] [-cefhn] [-rrev1] [-rrev2] file";
        filecounter=revnums=0;
        while (--argc,++argv, argc>=1 && argv[0][0] == '-') {
	    argp = &argv[0][1];
	    while (c = *argp++) switch (c) {
                case 'r':
                        if (*argp != '\0') {
                            if (revnums==0) {
                                    rev1= argp; revnums=1;
                            } elif (revnums==1) {
                                    rev2= argp; revnums=2;
                            } else {
                                    faterror("too many revision numbers");
                            }
                        } /* do nothing for empty -r */
			argp += strlen(argp);
                        break;
                case 'b':
                case 'i':
                case 'w':
                case 't':
			*boption++ = c;
                        break;
                case 'c':
                case 'e':
                case 'f':
                case 'h':
                case 'n':
                        if (*otheroption=='\0') {
                                otheroption= argp-2;
                        } else {
                                faterror("Options c,e,f,h,n are mutually exclusive");
                        }
                        break;
                default:
                        faterror("unknown option: %s\n%s", *argv,cmdusage);
                };
        } /* end of option processing */
	if (boption != bops + 1) {
	    *boption = ' ';
	    boption = bops;
	}
        if (argc<1) faterror("No input file\n%s",cmdusage);

        /* now handle all filenames */
        do {
                finptr=NULL;

                if (pairfilenames(argc,argv,true,false)!=1) continue;
                if (++filecounter>1)
                        diagnose("===================================================================");
                diagnose("RCS file: %s",RCSfilename);
                if (revnums<2 && !(access(workfilename,4)==0)) {
                        error("Can't open %s",workfilename);
                        continue;
                }
                if (!trysema(RCSfilename,false)) continue; /* give up */


                gettree(); /* reads in the delta tree */

                if (Head==nil) {
                        error("no revisions present");
                        continue;
                }
                if (revnums==0) rev1=Head->num; /* default rev1 */

                if (!expandsym(rev1,numericrev)) continue;
                if (!(target=genrevs(numericrev,(char *)nil,(char *)nil,(char *)nil,gendeltas))) continue;
                xrev1=target->num;

                if (revnums==2) {
                        if (!expandsym(rev2,numericrev)) continue;
                        if (!(target=genrevs(numericrev,(char *)nil,(char *)nil,(char *)nil,gendeltas))) continue;
                        xrev2=target->num;
                }


                temp1file=mktempfile("/tmp/",TMPFILE1);
                diagnose("retrieving revision %s",xrev1);
                VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                        TARGETDIR,xrev1,RCSfilename,temp1file);
                if (system(command)){
                        error("co failed");
                        continue;
                }
                if (revnums<=1) {
                        temp2file=workfilename;
                        diagnose("diff %s%s -r%s %s",boption,otheroption,xrev1,workfilename);
                } else {
                        temp2file=mktempfile("/tmp/",TMPFILE2);
                        diagnose("retrieving revision %s",xrev2);
                        VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                                TARGETDIR,xrev2,RCSfilename,temp2file);
                        if (system(command)){
                                error("co failed");
                                continue;
                        }
                        diagnose("diff %s%s -r%s -r%s",boption,otheroption,xrev1,xrev2);
                }
                VOID sprintf(command,"%s %s %s %s %s\n",DIFF,boption,
                        otheroption, temp1file, temp2file);
                exit_stats = system (command);
                if (exit_stats != 0 && exit_stats != (1 << BYTESIZ)) {
                        error ("diff failed");
                        continue;
                }
        } while (cleanup(),
                 ++argv, --argc >=1);


        exit(nerror!=0);

}
