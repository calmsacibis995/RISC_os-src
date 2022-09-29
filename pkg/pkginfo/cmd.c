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
#ident	"$Header: cmd.c,v 2.0.1.2 90/05/10 04:02:40 wje Exp $"
#include "pkginfo.h"

static char rcs[]="$Header: cmd.c,v 2.0.1.2 90/05/10 04:02:40 wje Exp $";

/*
** These are the pkginfo commands
*/

bomname()
{
  int ind;

  if (gargc != 3)
    error(ERRTYPE_FATAL, "no subpackage specified");
  if ((ind = subpkgind(gargv[2])) < 0)
    error(ERRTYPE_FATAL, "subpackage not found");
  else
    printf("%s\n", subpkg[ind].bom);
  return(0);
}

mronly()
{
  int ind;
  if (gargc != 3)
    error(ERRTYPE_FATAL, "no subpackage specified");
  if ((ind = subpkgind(gargv[2])) < 0)
    error(ERRTYPE_FATAL, "subpackage not found");
  else
    if (subpkg[ind].flags & FL_MRO)
      return 0;
    else
      return 1;
}

optional()
{
  int ind;

  if (gargc != 3)
    error(ERRTYPE_FATAL, "no subpackage specified");
  if ((ind = subpkgind(gargv[2])) < 0)
    error(ERRTYPE_FATAL, "subpackage not found");
  else
    if (subpkg[ind].flags & FL_OPT)
      return 0;
    else
      return 1;
}

os()
{
  if (pkg.flags & PKGFL_OS || pkg.flags & PKGFL_OS2)
    return 0;
  else
    return 1;
}

os2()
{
  if (pkg.flags & PKGFL_OS2)
    return 0;
  else
    return 1;
}

pkgid()
{
  if (strlen(pkg.id))
    {
      printf("%s\n", pkg.id);
      return 0;
    }
  else
    return 1;
}

pkgname()
{
  if (strlen(pkg.name))
    {
      printf("%s\n", pkg.name);
      return 0;
    }
  else
    return 1;
}

splitboms()
{
  int ind;
  if (gargc != 3)
    error(ERRTYPE_FATAL, "no subpackage specified");
  if ((ind = subpkgind(gargv[2])) < 0)
    error(ERRTYPE_FATAL, "subpackage not found");
  else
    printf("%s\n", subpkg[ind].splitboms);
  return 0;
}

subpkgalias()
{
  int ind;
  int alias;

  for (ind = 0; ind <= cursubpkg; ind++)
    for (alias = 0; alias < subpkg[ind].aliasnum; alias++)
      printf("%s\n", subpkg[ind].name[alias]);
  return 0;
}

subpkgid()
{
  int ind;
  if (gargc != 3)
    error(ERRTYPE_FATAL, "no subpackage specified");
  if ((ind = subpkgind(gargv[2])) < 0)
    error(ERRTYPE_FATAL, "subpackage not found");
  else
    printf("%s\n", subpkg[ind].id);
  return 0;
}

subpkgnames()
{
  int ind;

  for (ind = 0; ind <= cursubpkg; ind++)
    printf("%s\n", subpkg[ind].name[0]);
  return 0;
}

timestamp()
{
  if (strlen(pkg.timestamp))
    {
      printf("%s\n", pkg.timestamp);
      return 0;
    }
  else
    return 1;
}

version()
{
  int ind;

  if (gargc != 3)
    error(ERRTYPE_FATAL, "no [sub]package specified");
  if (strcmp(pkg.name, gargv[2]) == 0)
    {
      printf("%s\n", pkg.ver);
      return 0;
    }
  else
    if ((ind = subpkgind(gargv[2])) < 0)
      error(ERRTYPE_FATAL, "[sub]package not found");
    else
      {
        printf("%s\n", subpkg[ind].ver);
	return 0;
      }
}

media_routine()
{
    printf("%s\n", media[0].name);
}

volume()
{
  int ind;
  int alias;

  if (gargc != 4)
    error(ERRTYPE_FATAL, "bad arguments to volume command");
  if ((ind = mediaind(gargv[2])) < 0)
    error(ERRTYPE_FATAL, "requested media type not found");
  else
    {
      int vol = atoi(gargv[3]) - 1;
      int first, last;
      
      if (vol < 0)
        error(ERRTYPE_FATAL, "volume number must be greater than 0");
      if (vol > media[ind].curvol)
        error(ERRTYPE_FATAL, "not that many volumes for this media type");
      first = media[ind].firstinvols[vol];
      if (vol == media[ind].curvol)
	last = cursubpkg;
      else
	last = (media[ind].firstinvols[vol+1])-1;
      for (ind = first; ind <= last; ind ++)
        printf("%s\n", subpkg[ind].name[0]);
      return 0;
    }

}

position()
{
  int mind, sind, vol=0;
  int first, last;

  if (gargc != 4)
    error(ERRTYPE_FATAL, "bad arguments to position command");
  if (strcmp(gargv[2],"RemoteTape") == 0) {
    if ((mind = mediaind(media[0].name)) < 0)
      error(ERRTYPE_FATAL, "requested media type not found");
  } else {
    if ((mind = mediaind(gargv[2])) < 0)
      error(ERRTYPE_FATAL, "requested media type not found");
  }
  if ((sind = subpkgind(gargv[3])) < 0)
    error(ERRTYPE_FATAL, "subpackage not found");

  for (vol = 0; vol <= media[mind].curvol; vol++)
    {
      first = media[mind].firstinvols[vol];
      if (vol == media[mind].curvol)
        last = cursubpkg;
      else
        last = (media[mind].firstinvols[vol+1])-1;
      if ((sind >= first) && (sind <= last))
        {
          printf("%d %d\n", vol+1, sind - first + 1);
          return 0;
	}
    }
  error(ERRTYPE_FATAL, "internal error: could not determine volume number");
}

