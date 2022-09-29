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
#ident	"$Header: extra.c,v 1.5.2.2 90/05/10 03:44:45 wje Exp $"
static char rcs[]="$Header: extra.c,v 1.5.2.2 90/05/10 03:44:45 wje Exp $";

#include "comply.h"

#define MAXEXARGS 100 

/* In keeping with the new packaging system's philospohy of using PATH... */
#define EXTRAPATH "extra"

char extracmd[512];

initextra(argc, argv, first)
int argc;
char *argv[];
int first;
{
  strcpy(extracmd, EXTRAPATH);
  strcat(extracmd, " . "); /* vestigal from when comply required root path */
  while (first < argc) /* might be nice to check length of command */
    {
      strcat(extracmd, " '");
      strcat(extracmd, argv[first++]);
      strcat(extracmd, "'");
    }
}

doextra()
{
  FILE *extrafil;
  char exstr[1024];

  if ((extrafil = (FILE *)popen(extracmd, "r")) == (FILE *) NULL)
    {
      sprintf(emsg, "can't execute \"%s\"", extracmd);     
      error(ERRTYPE_ERROR, emsg);
      conform = 0;
    }
  else
    {
      if (silent == 0)
        fprintf(stderr, "comply: checking for extra files...\n");
      while (fgets(exstr, 1024, extrafil) != NULL)
        {
          char *tmp;

	  /* chop trailing \n off off extra string */
          for (tmp = exstr; *tmp && *tmp != '\n'; tmp++) ;
          if (*tmp == '\n') *tmp = '\0';
          sprintf(emsg, "extra file: %s", exstr);
          error(ERRTYPE_WARN, emsg);
	  if (delete)
            {
	      char delpath[MAXPATHLEN+1];
 	      struct stat sbuf;

	      strcpy(delpath, targetpath);
              strcat(delpath, "/");
              strcat(delpath, exstr);
	
	      if (lstat(delpath, &sbuf) == -1)
		{
		  sprintf(emsg, "internal error; couldn't stat %s",
	            delpath);
		  error(ERRTYPE_ERROR, emsg);
		  conform = 0;
		}
	      else
		if ((sbuf.st_mode & S_IFMT) == S_IFDIR)
		  {
		    if (rmdir(delpath) == -1)
	  	      {
		        error(ERRTYPE_WARN, "not removed");
		        conform = 0;
		      }
		    else
		      error(ERRTYPE_WARN, "removed");
		  }
		else
		  {
		    if (unlink(delpath) == -1)
		      {
		        error(ERRTYPE_WARN, "not removed");
			conform = 0;
		      }
		    else
		      error(ERRTYPE_WARN, "removed");
		  }
	    }
        }
     }
  pclose(extrafil);
}
