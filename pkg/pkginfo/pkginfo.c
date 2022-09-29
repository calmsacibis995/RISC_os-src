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
#ident	"$Header: pkginfo.c,v 2.1.1.2 90/05/10 04:02:53 wje Exp $"
#define MAIN 1
#include "pkginfo.h"

static char rcs[]="$Header: pkginfo.c,v 2.1.1.2 90/05/10 04:02:53 wje Exp $";

/*
** command functions go here
*/

extern bomname();
extern optional();
extern os();
extern os2();
extern mronly();
extern pkgid();
extern pkgname();
extern position();
extern splitboms();
extern subpkgalias();
extern subpkgid();
extern subpkgnames();
extern timestamp();
extern version();
extern volume();
extern media_routine();


struct cmdent cmds[] = 
  {
    "bomname", bomname,
    "optional", optional,
    "os", os,
    "os2", os2,
    "media", media_routine,
    "minirootonly", mronly,
    "pkgid", pkgid,
    "pkgname", pkgname,
    "position", position,
    "splitboms", splitboms,
    "subpkgalias", subpkgalias,
    "subpkgid", subpkgid,
    "subpkgnames", subpkgnames,
    "timestamp", timestamp,
    "version", version,
    "volume", volume,
  };

#define NCMD sizeof(cmds)/sizeof(struct cmdent)

main(argc, argv)
int argc;
char *argv[];
{
  int cmdind;
  int i;

  setbuf(stdout, NULL);
  setbuf(stderr, NULL);

  /* copy the argument vector into a global */
  for (i = 0; i < argc; i++)
    gargv[i] = argv[i];
  gargc = argc;

  if (argc < 2)
    usage();

  for (cmdind = 0; cmdind < NCMD; cmdind++)
    if (strcmp(cmds[cmdind].cmdname, argv[1]) == 0) break;

  if (cmdind >= NCMD)
    {
      sprintf(emsg, "unrecognized command: %s", argv[1]);      
      error(ERRTYPE_FATAL, emsg);
    }

  parseinfo();  
  exit(cmds[cmdind].func());

}


