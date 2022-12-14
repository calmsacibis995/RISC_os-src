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
#ident	"$Header: ckiboo.c,v 1.1.1.2 90/05/09 17:48:38 wje Exp $"
/*
 * MSBPCT.C
 *
 * Howie Kaye -- Columbia University 3/11/86
 *
 * sibling program to MSBMKB.C.  It is used to unpack BOO files, used for
 * encoding binary files into text, and back.  This program does the decoding.
 * It is meant to replace the program "MSBPCT.BAS", and runs approximately
 * 200 times faster.
 *
 * For documentation on BOO file format, see MSBMKB.C
 * This program runs, as is, under Microsoft C on MSDOS, and under UNIX(4.2).
 *
 * Modification history:
 *
 * 3/23/86 - Davide P. Cervone -- University of Rochester
 *   added AMIGA and VAX11C support
 *
 * 3/24/86 - Martin Knoblauch -- TH-Darmstadt              (MK001)
 *   test if 1. line of inputfile is delimited by "\r\n" instead of "\n"
 *
 * 5/5/86 - John Matthews, U of Delaware.
 *   Explicitly close the files.
 */

#include <stdio.h>
#ifdef AMIGA
   #include <fcntl.h>
#else
   #ifdef MSDOS
      #include <fcntl.h>
   #else
      #ifdef vax11c
         #include <file.h>
      #else
         #include <sys/file.h>
      #endif
   #endif
#endif

#define fixchr(x) ((x) -'0')
#define NULLCHR fixchr('~')

yes_or_no_p(arg) char *arg; {
  int c,x;
  while (1) {
    printf("%s",arg);
    c = getchar();
    if (c == '\n') continue;
    while ((x = getchar()) != '\n')
      if (x == EOF) return(0);
    if ((c == 'Y') || (c == 'y')) return(1);
    if ((c == 'N') || (c == 'n') || (c == EOF)) return(0);
    printf("Please answer 'Y' or 'N'\n");
  }
}

main(argc,argv) char **argv; {
#ifdef AMIGA
  char *infile = "CKIKER.BOO";          /* input file name, with default */
#else
  char *infile = "MSKERMIT.BOO";        /* input file name, with default */
#endif
  char outfile[100];                    /* output file name */
  FILE *ifp, *ofp;                      /* i/o files */
  char inline[100],outline[200];
  int f;

  if (argc > 2) {                       /* check for too many args */
    printf("too many args\n");
    exit(1);
  }
  if (argc > 1) {                       /* check for input file */
    infile = argv[1];
  }
  if ((ifp = fopen(infile,"r")) == NULL) { /* open input file */
    printf("%s not found.\n",infile);   /* failure? */
    exit(1);
  }

  fgets(outfile,100,ifp);               /* get output file name */
  if ((outfile[strlen(outfile)-2] == '\r')|    /* MK001 */
      (outfile[strlen(outfile)-2] == '\n')) {
    outfile[strlen(outfile)-2] = '\0';
  }
  else {
    outfile[strlen(outfile)-1] = '\0';
  }

  if ((ofp = fopen(outfile,"r")) != NULL) {
    char msg[100];
    sprintf(msg,"output file '%s' already exists.  continue (y/n)? ",outfile);
    if (!yes_or_no_p(msg)) {
      printf("ok.  bye\n");
      exit(0);
    }
    else {
      fclose(ofp);
    }
  }

#ifndef MSDOS
#ifndef O_BINARY
#define O_BINARY 0
#endif
#endif

#ifdef AMIGA
  if ((ofp = fopen(outfile,"w")) == NULL) {
    printf("could not open %s\n",outfile); /* failure */
    exit(0);
  }
#else
  f = open(outfile,O_CREAT|O_WRONLY|O_TRUNC|O_BINARY,0x1ff);
  if ((ofp = fdopen(f,"w")) == NULL) { /* open it */
    printf("could not open %s\n",outfile); /* failure? */
    exit(1);
  }
#endif
  printf("%s ==> %s\n",infile,outfile); /* announce our intentions */

  while(fgets(inline,100,ifp) != NULL) { /* till EOF */
    int index=0,outindex=0;
    while (index < strlen(inline) && inline[index] != '\n' &&
           inline[index] != '\r')       /* end of line? */
      if (fixchr(inline[index]) == NULLCHR) {   /* null compress char... */
        int rptcnt;
        int i;

        index++;
        rptcnt = fixchr(inline[index]); /* get repeat count */
        for (i=0; i<rptcnt; i++)        /* output the nulls */
          putc('\0',ofp);
        index++;                        /* pass the count field */
      }
      else {                            /* a quad to decode... */
        int a, b, c ,d;

        a = fixchr(inline[index]);
        index++;
        b = fixchr(inline[index]);
        index++;
        c = fixchr(inline[index]);
        index++;
        d = fixchr(inline[index]);
        index++;

                                        /* output the bytes */
        putc(((a*4)+(b/16)) & 255,ofp);
        putc(((b*16)+(c/4)) & 255,ofp);
        putc(((c*64)+d) & 255,ofp);
      }
    }
#ifdef AMIGA
    putc('\032',ofp);                   /* final ^Z for micros*/
#endif
#ifdef MSDOS
    putc('\032',ofp);                   /* final ^Z */
#endif

    fclose(ofp);			/* Close the files */
    fclose(ifp);
}					/* End of program */
