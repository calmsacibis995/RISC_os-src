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
#ident	"$Header: util.c,v 1.3.2.2 90/05/10 04:03:37 wje Exp $"

static char rcs[]="$Header: util.c,v 1.3.2.2 90/05/10 04:03:37 wje Exp $";

#include "preserve.h"

extern char *strrchr();

error(etype, s)
int etype;
char *s;
{
  static char *typstr[] = { "warning", "error", "fatal"};
  char errstr[1024];

  sprintf(errstr, "%s: %s", typstr[etype], s);
  errmsg(errstr);
  if (etype == ERRTYPE_FATAL)
    exit(1);
}

errmsg(s)
char *s;
{
  fprintf(stderr, "preserve: %s\n", s);
}

usage()
{
  errmsg(
  "usage: preserve [-a savroot] [-s | -r] version");

  exit(1);
}

#ifdef DEBUG
strcpyfld(to, from, len)
char **to, **from;
int len;
{
  while((**from) && (! isspace(**from)) && len--)
    *(*to)++ = *(*from)++;
  if (len)
    {
      **to = '\0';
      return (1);
    }
  else
    return(0);
}
#endif 

char *savpath(f)
char *f;
{
  static char path[256];
  strcpy(path, f);
  strcat(path,savsuffix);
  return(path);
}


char *updpath(f)
char *f;
{
  static char path[256];
  strcpy(path, f);
  strcat(path,updsuffix);
  return(path);
}


#ifdef BSD
#else
char *index(s, c)
char *s, c;
{
  while (*s && *s != c) s++;
  if (*s == c) 
    return s;
  else
    return 0;
}
#endif

#ifdef BSD
#else
char *basedir(s)
char *s;
{
  static char bn[256];
  char *p;

  strcpy(bn, s);
  p = strrchr(bn, '/');
  if (*p == '/') *p = '\0';
  return(bn);
}    
#endif

/*
**  This is a file copy function; create intermediate directories in the
**  destination path if necessary. Return 0 on failure, 1 on success.
*/
cpy(f, t)
char *f, *t;
{
  char cmd[512];
  char *basename, *tp = t;
  struct stat sbuf;

  if ((basename = strrchr(t, '/')) == NULL)
    basename = t;
  else
    basename++;
  if (*tp == '/') tp++;
  while (tp < basename)
    {
      while (*tp != '/') tp++;
      *tp = '\0';
      if (stat(t, &sbuf) < 0)
        if (errno != ENOENT)
	  return(0);
	else
	  {
	    sprintf(cmd, "/bin/mkdir %s", t);
#ifdef DEBUG
	    printf("%s\n", cmd);
#endif
	    system(cmd);
	  }
      else
        if ((sbuf.st_mode & S_IFMT) != S_IFDIR)
	  return(0);
      *tp++ = '/';
    }
  sprintf(cmd, "/bin/cp %s %s", f, t);
#ifdef DEBUG
  printf("%s\n", cmd);
#endif
  return (system(cmd) == 0);
}
