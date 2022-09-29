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
#ident	"$Header: parse.c,v 2.0.1.2 90/05/10 04:02:47 wje Exp $"
#include "pkginfo.h"

static char rcs[]="$Header: parse.c,v 2.0.1.2 90/05/10 04:02:47 wje Exp $";

char *st_init();
char *st_pkg();
char *st_media();
char *st_subpkg();
char *st_alias();
char *st_switch();

struct stateent states[] =
  {
    "init", st_init,
    "pkg", st_pkg,
    "subpkg", st_subpkg,
    "alias", st_alias,
    "switch", st_switch,
  };

#define NSTATE sizeof(states)/sizeof(struct stateent)


parseinfo()
{
  char *state = "init";
  int stind;
 
  while (strcmp(state, "terminate"))
    {
      for (stind = 0; stind < NSTATE; stind++)
        if (strcmp(states[stind].stname, state) == 0) break;

      if (stind >= NSTATE)
        {
          sprintf(emsg, "internal error; unknown state: %s", state);
          error (ERRTYPE_FATAL, emsg);
        }
      state = states[stind].func();
    }
}


char *st_init()
{
  int i, j;
  
  strcpy(pkg.name, "");
  strcpy(pkg.id, "");
  strcpy(pkg.ver, "");
  strcpy(pkg.timestamp, "");
  pkg.flags = 0;

  cursubpkg = -1;
  for (i = 0; i < MAXSUBPKG; i++)
    {
      for (j = 0; j < MAXALIAS; j++)
        strcpy(subpkg[i].name[j], "");
      subpkg[i].aliasnum = 0;
      strcpy(subpkg[i].id, "");
      strcpy(subpkg[i].ver, "");
      strcpy(subpkg[i].bom, "");
      strcpy(subpkg[i].splitboms, "");
      subpkg[i].flags = 0;
    }

  curmedia = -1;

  return "switch";
} 


char *st_switch()
{
  int i;
  struct token *t;

  if ((t = toke()) == NULL)
    return "terminate";

  if (strcmp(t->name, "package") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "package attribute must be named");
      strcpy(pkg.name, t->value);
      return "pkg";
    }
  else
    if (strcmp(t->name, "subpackage") == 0)
      {
        if (! strlen(t->value))
          error(ERRTYPE_FATAL, "subpackage attribute must be named");
	if (++cursubpkg >= MAXSUBPKG)
          error(ERRTYPE_FATAL, "too many subpackages");
        strcpy(subpkg[cursubpkg].name[subpkg[cursubpkg].aliasnum++], t->value);
	/* inheretance */
	strcpy(subpkg[cursubpkg].ver, pkg.ver);
        return "alias";
      }
    else
      if (strcmp(t->name, "media") == 0)
	{
	  int scnmedia;

          if (! strlen(t->value))
            error(ERRTYPE_FATAL, "media attribute must be named");
	  for (scnmedia = 0; scnmedia <= curmedia; scnmedia++)
	    if (strcmp(media[scnmedia].name, t->value) == 0) break;
	  if (scnmedia > curmedia)
            {		
  	      if (++curmedia >= MAXMEDIA)
                error(ERRTYPE_FATAL, "too many media types");
              strcpy(media[curmedia].name, t->value);
	      for (i = i; i < MAXVOLS; i++) 
		media[curmedia].firstinvols[i] = 0;
	      media[curmedia].curvol = -1;
	    }
	  media[scnmedia].curvol++;
	  media[scnmedia].firstinvols[media[scnmedia].curvol]
	    = cursubpkg+1;
	   
          return "switch";
        }
      else
        {
	  sprintf(emsg, "unrecognized attribute: %s", t->name);
          error(ERRTYPE_FATAL, emsg);
	}
}  

char *st_pkg()
{
  struct token *t;

  if ((t = toke()) == NULL)
    return "terminate";
  
  if (strcmp(t->name, "id") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "missing id value");
      strcpy(pkg.id, t->value);
      return "pkg";
    }
  else
  if (strcmp(t->name, "os2") == 0)
    {
      pkg.flags |= PKGFL_OS2;
      return "pkg";
    }
  else
  if (strcmp(t->name, "os") == 0)
    {
      pkg.flags |= PKGFL_OS;
      return "pkg";
    }
  else
  if (strcmp(t->name, "timestamp") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "missing timestamp value");
      strcpy(pkg.timestamp, t->value);
      return "pkg";
    }
  else
  if (strcmp(t->name, "version") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "missing version value");
      strcpy(pkg.ver, t->value);
      return "pkg";
    }
  else
    {
      untoke();
      return("switch");
    }
}


char *st_alias()
{
  struct token *t;

  if ((t = toke()) == NULL)
    error(ERRTYPE_FATAL, "missing subpackage description");
  
  if (strcmp(t->name, "subpackage") == 0)
    {
      if (subpkg[cursubpkg].aliasnum >= MAXALIAS)
        error(ERRTYPE_FATAL, "too many subpackage aliases");
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "subpackage attribute must be named");
      strcpy(subpkg[cursubpkg].name[subpkg[cursubpkg].aliasnum++], t->value);
      return "alias";
    }
  else
    {
      untoke();
      return("subpkg");
    }
}

char *st_subpkg()
{
  struct token *t;

  if ((t = toke()) == NULL)
    return "terminate";
  
  if (strcmp(t->name, "bom") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "missing bom value");
      strcpy(subpkg[cursubpkg].bom, t->value);
      return "subpkg";
    }
  else
  if (strcmp(t->name, "id") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "missing id value");
      strcpy(subpkg[cursubpkg].id, t->value);
      return "subpkg";
    }
  else
  if (strcmp(t->name, "minirootonly") == 0)
    {
      subpkg[cursubpkg].flags |= FL_MRO;
      return "subpkg";
    }
  else
  if (strcmp(t->name, "optional") == 0)
    {
      subpkg[cursubpkg].flags |= FL_OPT;
      return "subpkg";
    }
  else
  if (strcmp(t->name, "splitboms") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "missing splitboms value");
      strcpy(subpkg[cursubpkg].splitboms, t->value);
      return "subpkg";
    }
  else
  if (strcmp(t->name, "version") == 0)
    {
      if (! strlen(t->value))
        error(ERRTYPE_FATAL, "missing version value");
      strcpy(subpkg[cursubpkg].ver, t->value);
      return "subpkg";
    }
  else
    {
      untoke();
      return("switch");
    }
}


char *x()
  {
    struct token *p;
    while ((p = toke()) != NULL) printf ("%s <%s>\n", p->name, p->value);
    return "terminate";
  }


