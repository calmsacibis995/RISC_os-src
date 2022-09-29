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
#ident	"$Header: devstat.c,v 1.1.2.2 90/05/10 03:46:27 wje Exp $"
#include <sys/types.h>
#include <sys/stat.h>

extern int errno;

main(argc, argv)
int argc;
char *argv[];
{
  struct stat sbuf;

  if (stat(argv[1], &sbuf) == -1)
    {
      perror("can't stat file");
      exit(1);
    }

  if (sbuf.st_mode & (S_IFCHR | S_IFBLK))
    {
      printf("%c %d %d\n", ((sbuf.st_mode & S_IFMT) == S_IFCHR) ? 'c' : 'b',
        (sbuf.st_rdev >> 8) & 0xff, sbuf.st_rdev & 0xff);
      exit(0);
    }
  else
    {
      printf("file is not a device\n");
      exit(1);
    }

}
