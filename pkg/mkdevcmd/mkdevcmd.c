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
#ident	"$Header: mkdevcmd.c,v 1.1.2.2 90/05/10 03:57:02 wje Exp $"
#include <stdio.h>

/*
** This program embodies all of the commands (named in extern statements,
** below. It takes the ouput from MKDEV -p -n and executes the commands
** it does so about three times as fast as MKDEV itself can do them, which
** is why it was written.
*/

extern chgrp();
extern chmod();
extern chown();
extern echo();
extern mv();
extern mkdir();
extern mknod();
extern rmcmd();

main(argc, argv)
int argc;
char *argv[];
{
  char line[265];
  char *l;
  int myargc;
  char *myargv[256];

  while (gets(line) != NULL)
    {
#ifdef DEBUG
      printf("%s\n", line);
#endif
      l = &line[0];
      myargc = 0;
      while (*l)
        {
          while (*l && (*l == ' ' || *l == '\t')) l++;
          if (*l) myargv[myargc++] = l;
	  while (*l && *l != ' ' && *l != '\t') l++;
	  if (*l) *l++ = '\0';
	}

#ifdef DEBUG
      printf("%s\nargc=%d, argv[0]=%10s, argv[1]=%10s ...\n", line, myargc,
        myargv[0], myargv[1]);
#endif
      if (strcmp(myargv[0], "chgrp") == 0)
        chgrpcmd(myargc, myargv);
      else if (strcmp(myargv[0], "chmod") == 0)
        chmodcmd(myargc, myargv);
      else if (strcmp(myargv[0], "chown") == 0)
        chowncmd(myargc, myargv);
      else if (strcmp(myargv[0], "echo") == 0)
        echocmd(myargc, myargv);
      else if (strcmp(myargv[0], "ln") == 0)
        mvcmd(myargc, myargv);
      else if (strcmp(myargv[0], "mkdir") == 0)
        mkdircmd(myargc, myargv);
      else if (strcmp(myargv[0], "mknod") == 0)
        mknodcmd(myargc, myargv);
      else if (strcmp(myargv[0], "rm") == 0)
        rmcmd(myargc, myargv);

    }
}      
