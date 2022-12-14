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
#ident	"$Header: rcsfcmp.c,v 1.2.2.2 90/05/10 00:22:39 wje Exp $"

/*
 *                     RCS file comparison
 */
/*****************************************************************************
 *                       rcsfcmp()
 *                       Testprogram: define FCMPTEST
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



/*
#define FCMPTEST
/* Testprogram; prints out whether two files are identical,
 * except for keywords
 */

#include  "rcsbase.h"
extern FILE * fopen();


rcsfcmp(xfname,uxfname,delta)
char * xfname, *uxfname; struct hshentry *delta;
/* Function: compares the files xfname and uxfname. Returns true
 * if xfname has the same contents as uxfname, while disregarding
 * keyword values. For the LOG-keyword, rcsfcmp skips the log message
 * given by the parameter delta in xfname. Thus, rcsfcmp returns true
 * if xfname contains the same as uxfname, with the keywords expanded.
 */
{
    register int xc,uxc;
    char xkeyword[keylength+2], uxkeyword[keylength+2];
    register char * tp;
    FILE * xfp, * uxfp;
    int result;

    if ((xfp=fopen(tp=xfname,"r"))==NULL || (uxfp=fopen(tp=uxfname,"r"))==NULL) {
       faterror("Can't open %s\n", tp);
       return false;
    }
    result=false;
    xc=getc(xfp); uxc=getc(uxfp);
    while( xc == uxc) { /* comparison loop */
        if (xc==EOF) { /* finished; everything is the same*/
            result=true;
            break;
        }
        if ( xc!=KDELIM) {
            /* get the next characters */
            xc=getc(xfp); uxc=getc(uxfp);
        } else {
            /* try to get both keywords */
            tp = xkeyword;
            while( ctab[(xc=getc(xfp))]==LETTER && tp< xkeyword+keylength)
                *tp++ = xc;
            *tp='\0';
            tp = uxkeyword;
            while( ctab[(uxc=getc(uxfp))]==LETTER && tp< uxkeyword+keylength)
                *tp++ = uxc;
            *tp='\0';
            /* now we have 2 keywords, or something thal looks like it.*/
            if (strcmp(xkeyword,uxkeyword)!=0) break; /* not the same! */
            /* now check whether it's really a keyword */
            if (!(strcmp(xkeyword,AUTHOR)==0 || strcmp(xkeyword,DATE)    ==0 ||
                  strcmp(xkeyword,HEADER)==0 || strcmp(xkeyword,LOCKER)  ==0 ||
                  strcmp(xkeyword,LOG)   ==0 || strcmp(xkeyword,REVISION)==0 ||
                  strcmp(xkeyword,SOURCE)==0 || strcmp(xkeyword,STATE)   ==0 )) {
                /* it's not a keyword, so continue normally */
                continue;
            } else {
                /* it's a keyword, so skip value */
                while (xc!=KDELIM && xc!='\n' && xc!=EOF) xc=getc(xfp);
                while (uxc!=KDELIM && uxc!='\n' && uxc!=EOF) uxc=getc(uxfp);
                if (xc==uxc && xc==KDELIM) {
                    xc=getc(xfp); uxc=getc(uxfp); /* skip KDELIM */
                    /* if the keyword is LOG, also skip the log message in xfp*/
                    if (strcmp(xkeyword,LOG)==0) {
                        /* first, compute the number of line feeds in log msg */
                        int lncnt;
                        lncnt=2; tp=delta->log;
                        while(*tp) if(*tp++=='\n') lncnt++;
                        while(xc!=EOF) {
                            if (xc=='\n')
                                if(--lncnt==0) break;
                            xc=getc(xfp);
                        }
                        /* skip last comment leader */
                        for (lncnt=strlen(Comment); lncnt>=0; lncnt--) xc=getc(xfp);
                    }
                }
            }
        }
    }
    VOID fclose(xfp);VOID fclose(uxfp);
    return result;
}



#ifdef FCMPTEST
cleanup(){} /* dummy */

char * Comment;

main(argc, argv)
int  argc; char  *argv[];
/* first argument: comment leader; 2nd: log message, 3rd: expanded file,
 * 4th: unexpanded file
 */
{       struct hshentry delta;

        cmdid="rcsfcmp";
        Comment=argv[1];
        delta.log=argv[2];
        if (rcsfcmp(argv[3],argv[4],&delta))
                VOID printf("files are the same\n");
        else    VOID printf("files are different\n");
}
#endif
