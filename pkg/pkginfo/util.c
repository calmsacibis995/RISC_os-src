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
#ident	"$Header: util.c,v 2.0.1.2 90/05/10 04:03:04 wje Exp $"
#include "pkginfo.h"

static char rcs[]="$Header: util.c,v 2.0.1.2 90/05/10 04:03:04 wje Exp $";

error(etype, s)
int etype;
char *s;
{
  static char *typstr[] = { "warning", "error", "fatal"};
  char errstr[1024];

  fprintf(stderr, "pkginfo: %s: %s\n", typstr[etype], s);
  if (etype == ERRTYPE_FATAL)
    exit(1);
}

usage()
{
  fprintf(stderr,
    "\
pkginfo: commands:\n\
  bomname <subpkgname>\n\
  optional <subpkgname>\n\
  os\n\
  os2\n\
  media\n\
  minirootonly <subpkgname>\n\
  pkgid\n\
  pkgname\n\
  position <medianame> <subpkgname>\n\
  splitboms <subpkgname>\n\
  subpkgalias\n\
  subpkgid <subpkgname>\n\
  subpkgnames\n\
  timestamp\n\
  version <[sub]pkgname>\n\
  volume <medianame> <volnumber>\n\
");

  exit(1);
}

/*
** This is the only function which reads the input file
*/
char *rawtoke()
{
  static int first = 1;
  static FILE *pkgfile;

  char line[PATH_MAX*2+1];
  char tline[PATH_MAX*2+1];

  static char *tpt;
  char *rtpt;

  if (first)
    {
      char *rel;
      char pkginfofile[PATH_MAX+1];

      first = 0;

      if ((rel = getenv("Pkginfo")) != NULL)
        strcpy(pkginfofile, rel);
      else
        {
          if ((rel = getenv("Pkg")) == NULL)
            error(ERRTYPE_FATAL,
	      "\"Pkg\" or \"Pkginfo\" not specified in environment");
          strcpy(pkginfofile, rel);
          strcat(pkginfofile, "/pkginfo");
        }
   
      if ((pkgfile = fopen(pkginfofile, "r")) == NULL)
        {
          sprintf(emsg, "cannot open %s", pkginfofile);
          error(ERRTYPE_FATAL, emsg);
        }

      do
	{
          if (fgets(line, sizeof(line)-1, pkgfile) == NULL)
            return NULL;
	} while (line[0] == '#');
      strcpy(tline, line);
      tpt = tline;      

    } /* if (first) */

  do
    {
      while ((*tpt) && ((*tpt == ' ') || (*tpt == '\t') || ( *tpt == '\n')))
        tpt++;
      if (*tpt != '\0')
        break;
      else
	{
          do
	    {
              if (fgets(line, sizeof(line)-1, pkgfile) == NULL)
                return NULL;
	    } while (line[0] == '#');
          strcpy(tline, line);
          tpt = tline;    
        }
    } while (1);

  rtpt = tpt;

  do
    {
      tpt++;
      if (*tpt == '"')
        {
	  tpt++;
          while ( (*tpt) && (*tpt != '"') ) tpt++;    
          if (*tpt) tpt++;
        }
    } 
      while ((*tpt) && ( (*tpt != ' ') && (*tpt != '\t') && (*tpt != '\n')));

  *tpt = '\0';
  if (*(tpt+1)) tpt++;
  return rtpt;
}
  
static int untoked = 0;

untoke()
{
  untoked = 1;
}

struct token *toke()
{
  static struct token t;
  char line[PATH_MAX*2+1];
  char *tokpt, *scnpt;
  int hasval = 0, quoted = 0;

  if (untoked)
    {
      untoked = 0;
      return &t;
    }
    
  /* initialize both name and value to empty */

  strcpy(t.name, "");
  strcpy(t.value, "");

  /* get the raw token (name + possibly quoted value) */
  if ((tokpt = rawtoke()) == NULL) return NULL;

  strcpy(line, tokpt);

  tokpt = line;
  scnpt = tokpt;

  /* scan till we hit eol or an equals sign */
  while ((*scnpt) && (*scnpt != '=')) scnpt++;

  /* if there's an equal sign, then this is a valued attribute */
  if (*scnpt == '=') hasval = 1;
  *scnpt = '\0';

  /* be sure the attribute name isn't too long */
  if (strlen(tokpt) > sizeof(t.name))
   {
     sprintf(emsg, "attribute name too long: %s", tokpt);
     error(ERRTYPE_FATAL, emsg);
   }

  /* copy the attribute name into the token structure */
  strcpy(t.name, tokpt);

  /* now do the attribute value, if present */
  if (hasval)
    {
      scnpt++;
      if (*scnpt == '"') scnpt++, quoted++;
      tokpt = scnpt;
      while ((*scnpt) && (*scnpt != '"')) scnpt++;

      /* handle both combinations of mismatched quotes, but only a warning */
      if (quoted)
        if (*scnpt != '"')
          {
	    sprintf(emsg, "no close quote: %s", tokpt);
	    error(ERRTYPE_WARN, emsg);
          }
        else
	  ;
      else
        if (*scnpt == '"')
          {
	    sprintf(emsg, "no opening quote: %s", tokpt);
	    error(ERRTYPE_WARN, emsg);
          }
      if (*scnpt == '"') *scnpt = '\0';

      /* make sure the attribute value fits in the token structure */
      if (strlen(tokpt) > sizeof(t.value))
        {
          sprintf(emsg, "attribute value too long: %s", tokpt);
          error(ERRTYPE_FATAL, emsg);
        }
      /* copy in the attribute value */
      strcpy(t.value, tokpt);
    }
  return &t;
}


subpkgind(s)
char *s;
{
  int ind;
  int alias = 0;

  for (ind = 0; ind <= cursubpkg; ind++)
    for (alias = 0; alias <= subpkg[ind].aliasnum; alias++)
      if (strcmp(s, subpkg[ind].name[alias]) == 0) return (ind);
  
  return (-1);
}
  

mediaind(s)
char *s;
{
  int ind;

  if (strcmp(s, "RemoteTape") == 0) return(1);
  for (ind = 0; ind <= curmedia; ind++)
    if (strcmp(s, media[ind].name) == 0) break;

  if (ind > curmedia)
    return (-1);
  else
    return (ind);
}
  
