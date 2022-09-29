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
#ident	"$Header: rcsmerge.c,v 1.3.1.4 90/05/10 00:28:18 wje Exp $"

/*
 *                       rcsmerge operation
 */
/*****************************************************************************
 *                       join 2 revisions with respect to a third
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

extern int  cleanup();              /* cleanup after signals                */
extern char * mktempfile();         /*temporary file name generator         */
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern int  nerror;                 /*counter for errors                    */

char *RCSfilename;
char *workfilename;
char * temp1file, * temp2file;

main (argc, argv)
int argc; char **argv;
{
        char * cmdusage;
        char command[NCPPN+revlength+40];
        int  revnums; /* counter for revision numbers given */
        int tostdout;
        char * rev1, * rev2; /*revision numbers*/
        char numericrev[revlength];   /* holds expanded revision number     */
        struct hshentry * gendeltas[hshsize];/*stores deltas to be generated*/
        struct hshentry * target;

        catchints();
        cmdid = "rcsmerge";
        cmdusage = "command format:\n    rcsmerge -p -rrev1 -rrev2 file\n    rcsmerge -p -rrev1 file";
        revnums=0;tostdout=false;

        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {
                case 'p':
                        tostdout=true;
                        /* falls into -r */
                case 'r':
                        if ((*argv)[2]!='\0') {
                            if (revnums==0) {
                                    rev1= *argv+2; revnums=1;
                            } elif (revnums==1) {
                                    rev2= *argv+2; revnums=2;
                            } else {
                                    faterror("too many revision numbers");
                            }
                        } /* do nothing for empty -r or -p */
                        break;

                default:
                        faterror("unknown option: %s\n%s", *argv,cmdusage);
                };
        } /* end of option processing */

        if (argc<1) faterror("No input file\n%s",cmdusage);
        if (revnums<1) faterror("no base revision number given");

        /* now handle all filenames */

        if (pairfilenames(argc,argv,true,false)==1) {

                if (argc>2 || (argc==2&&argv[1]!=nil))
                        warn("too many arguments");
                diagnose("RCS file: %s",RCSfilename);
                if (!(access(workfilename,4)==0))
                        faterror("Can't open %s",workfilename);

                if (!trysema(RCSfilename,false)) goto end; /* give up */

                gettree();  /* reads in the delta tree */

                if (Head==nil) faterror("no revisions present");


                if (!expandsym(rev1,numericrev)) goto end;
                if (!(target=genrevs(numericrev,(char *)nil,(char *)nil,(char *)nil,gendeltas))) goto end;
                rev1=target->num;
                if (revnums==1) rev2=Head->num; /* default for rev2 */
                if (!expandsym(rev2,numericrev)) goto end;
                if (!(target=genrevs(numericrev,(char *)nil,(char *)nil,(char *)nil,gendeltas))) goto end;
                rev2=target->num;

                temp1file=mktempfile("/tmp/",TMPFILE1);
                temp2file=mktempfile("/tmp/",TMPFILE2);

                diagnose("retrieving revision %s",rev1);
                VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                        TARGETDIR,rev1,RCSfilename,temp1file);
                if (system(command)){
                        faterror("co failed");
                }
                diagnose("retrieving revision %s",rev2);
                VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                         TARGETDIR,rev2,RCSfilename,temp2file);
                if (system(command)){
                        faterror("co failed");
                }
                diagnose("Merging differences between %s and %s into %s%s",
                         rev1, rev2, workfilename,
                         tostdout?"; result to stdout":"");

                VOID sprintf(command,"%s %s%s %s %s %s %s\n",MERGE,tostdout?"-p ":"",
                        workfilename,temp1file,temp2file,workfilename,rev2);
                if (system(command)) {
                        faterror("merge failed");
                }
        }

end:
        cleanup();
        exit(nerror!=0);

}
