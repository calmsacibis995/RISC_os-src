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
#ident	"$Header: ident.c,v 1.3.2.2 90/05/10 00:26:32 wje Exp $"

/*
 *                     RCS identification operation
 */
/*****************************************************************************
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

#include  "rcsbase.h"
#define fflsbuf _flsbuf
/* redefinition of _flsbuf in putc not needed */

main(argc, argv)
int  argc; char  *argv[];
/*  Ident searches the named files for all occurrences
 *  of the pattern $keyword:...$, where the keywords are
 *  Author, Date, Header, Log, Revision, Source, and State.
 */

{
   FILE *fp, *fopen();
   register int c;
   register int matchcount;

   if (argc<2) {
        VOID fprintf(stderr,  "ident error: no input file\n");
        exit(1);
   }
   while ( --argc > 0 ) {
      if ( (fp = fopen(*++argv, "r") ) == NULL ) {
         VOID fprintf(stderr,  "ident error: can't open %s\n", *argv);
         continue;
      } else {
         matchcount = 0;
         VOID printf( "%s:\n", *argv);   /*  print file name  */
         while( (c=getc(fp)) != EOF) {
            if ( (char)c==KDELIM)
               matchcount += match(fp);
         }
         if (matchcount == 0) {
            printf("ident warning: no id keywords in %s\n", *argv);
	 }
         if (argc>1) VOID putchar('\n');
      }
      VOID fclose(fp);
   }
}

match(fp)   /* group substring between two KDELIM's; then do pattern match */
FILE *fp;

{
   char line[keyvallength];
   register int c;
   register char * tp;

   tp = line;
   while( (c = getc(fp)) != KDELIM ) {
      *tp++ = c;
      if ( c==EOF || c=='\n' || tp>= line+keyvallength-2)
         return(0);
      }
   *tp++=c;     /*append trailing KDELIM*/
   *tp='\0';
   if (isprefix(line, AUTHOR))
       ;
   else if ( isprefix(line,DATE) )
       ;
   else if ( isprefix(line, HEADER) )
       ;
   else if ( isprefix(line, LOCKER) )
       ;
   else if ( isprefix(line, LOG) )
       ;
   else if (isprefix(line, REVISION) )
       ;
   else if (isprefix(line, SOURCE) )
       ;
   else if ( isprefix(line, STATE) )
       ;
   else {
      /* no match; put trailing KDELIM back into input */
      VOID ungetc(c,fp );
      return(0);
   }
   VOID fprintf(stdout,"     $%s\n",line);
   return(1);
}


isprefix(s,t) /*  return true if t is prefix of s; false otherwise  */
char s[], t[];
{
   int  j, k;

   for (j=0, k=0; t[k] != '\0' && s[j] != '\0' && s[j] == t[k]; ++j,++k)
      ;
   if ( t[k] != '\0' )
      return(false);
   if (s[j] == VDELIM)
      return(true);
   return(false);
}
