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
#ident	"$Header: preserve.c,v 1.3.2.2 90/05/10 04:03:23 wje Exp $"

#define MAIN 1
#include "preserve.h"

static char rcs[]="$Header: preserve.c,v 1.3.2.2 90/05/10 04:03:23 wje Exp $";

/*
**    preserve
**
**    usage: preserve [-a altsavroot] [-sr] version
**
**	list of preserve input records is supplied on stdin
*/

#define OPTIONS	"a:r:s:"
int save, restore;
char *altsavp;
char *versavp;

char stdoutbuf[BUFSIZ];
char stderrbuf[BUFSIZ];

extern int optind;
extern char *optarg;
extern char *place;

#define EMSG	""

main(argc, argv)
int argc;
char *argv[];
{
   struct stat sbuf;
   int c;
   char *tmp;

   save = restore = 0;
   altsavp = NULL;

#ifdef DEBUG
   setbuf(stdout, stdoutbuf);
   setbuf(stderr, stderrbuf);
#endif
   
   while ((c = getopt(argc, argv, OPTIONS)) != EOF)
     switch (c)
       {
	 case 's': save = 1; versavp = optarg; break;
	 case 'r': restore = 1; versavp = optarg; break;
	 case 'a': altsavp = optarg; break;
	 default : usage(); break;
       }

    if ((save + restore) != 1) /* who said you never want the logical xor? */
      usage();

    strcpy(updsuffix,":");
    strcat(updsuffix,versavp);
    strcat(updsuffix,"+");

    strcpy(savsuffix,":");
    strcat(savsuffix,versavp);
    strcat(savsuffix,":");
    
    targetpath = "./"; 
    
    process(stdin,"stdin"); /* The rest is just details... :-) */

}

#define POPTIONS "kudc"

process(presfil,presfilnam)
FILE *presfil;
char *presfilnam;
{
  char presrec[MAXPRESREC+1]; /* well... there are some long paths... */
  char path[MAXFILPATH];
  char targetfile[MAXFILPATH+2]; /* enough for ./path */
  struct stat sbuf;
  int pargc;
  char *pargv[100]; /* massive overkill! */
  int k, u, d, c, nopts;
  char file[256], *arg;
  char emsg[80];
  int ch;

  while (fgets(presrec, MAXPRESREC, presfil) != NULL)
    {
      if (presrec[0] == '#') continue; /* skip comments */

      /* get path, any options */

      pargc = 0;
      arg = presrec;
      while (*arg)
	{
          while (*arg && isspace(*arg)) arg++;
          if (*arg) pargv[pargc++] = arg;
          while (*arg && (! isspace(*arg))) arg++;
	  *arg++ = '\0';
	}

      if (pargc == 0) continue; /* empty record */      

      pargv[pargc] = "\0";
      strcpy(targetfile, "./");
      strcat(targetfile, pargv[0]);
      
      place = EMSG;
      optind = 1;
      k = u = d = c = 0;
      nopts = 1;
      while ((ch = getopt(pargc, pargv, POPTIONS)) != EOF)
        switch (ch)
          {
       	    case 'k': k++; nopts = 0; break;
	    case 'u': u++; nopts = 0; break;
	    case 'd': d++; nopts = 0; break;
	    case 'c': c++; if (optind == pargc)
			     {
			       error(ERRTYPE_WARN,
			         "missing -c command for %s\n",
			         targetfile);   
			       c = 0;
			     }
			   break;
	    case '?': break; /* getopt will print a message */
         }
     
      if (k && u)
        {
          if (save)
	    {
	      sprintf(emsg,
           "preserve record contains -k & -u (will use -u):\n%s\n",
	      pargv[0]);	    
	      error (ERRTYPE_WARN, emsg);
	    }
          k--; 
	}

      if (d && (k || u))
        {
          if (save)
	    {
	      sprintf(emsg,
"preserve record contains -d and (-k or -u) (will use -%c):\n%s\n",
	      u ? 'u' : 'k', pargv[0]);	    
	      error (ERRTYPE_WARN, emsg);
	    }
          d--; 
	}


      if (lstat(targetfile,&sbuf) == -1)
        {
	  /* The file doesn't exist on the system to be updated, */
	  /* don't have to worry about overwriting it 	         */
	  if (restore)
	    if (nopts || k)
  	      frestore(targetfile);
          if (c) pexec (save ? "-s" : "-r", targetfile, &pargv[optind]);
        }
      else
        {   
          /* the file exists in target tree */
	  if (save)
            if (nopts || k || u)
	      fsave(targetfile);
	    else 
	      ;
	  else
	    if (nopts)
	      ;
	    else
	      if (u)
	        fforceup(targetfile);
    	      else
	        if (k) 
 	          fsaveup(targetfile);
	        else 
	          if (d)
      		    fdel(targetfile);	     
          if (c) pexec (save ? "-s" : "-r", targetfile, &pargv[optind]);
	} /* else existing file */
    } /* while fgets pres record */
} /* process() */


#ifdef DEBUG

link(s1, s2)
char *s1, *s2;
{
  printf("link(%s,%s)\n", s1, s2);
}

unlnk(s1)
char *s1;
{
  printf("unlink(%s)\n", s1);
}

#else

unlnk(s1)
char *s1;
{
  struct stat fbuf;
  char rmcmd[256];

  if (lstat(s1,&fbuf) == -1)
    ;
  else
    {
      if ((fbuf.st_mode & S_IFDIR) == S_IFDIR) /* delete any old */
        {
          sprintf(rmcmd, "/bin/rm -rf %s >/dev/null 2>&1", s1);
          system(rmcmd);
	}
      else
        unlink(s1);
    }
}
#endif

#ifdef BSD
#else
struct utimbuf {
	time_t actime; 	/* access time */
	time_t modtime;	/* modification time */
};
#endif


sametimes(f1, f2)
char *f1, *f2;
{
  struct stat fbuf1, fbuf2;

  if ( (lstat(f1,&fbuf1) == -1) || (lstat(f2,&fbuf2) == -1) )
    return (0);
  else
    return (fbuf1.st_mtime == fbuf2.st_mtime);
}


/*
**  In the previous version, times were "touched" to "now". In this
**  version, times are "touched" to be 1 second younger.
*/

touch(f)
char *f;
{
  struct stat fbuf;
#ifdef BSD
  struct timeval tval[2];
#else
  struct utimbuf times;
#endif

  if (lstat(f,&fbuf) == -1)
    ;
  else
    {
#ifdef BSD
      tval[0].tv_sec  = fbuf.st_atime;
      tval[0].tv_usec = 0;
      tval[1].tv_sec  = fbuf.st_mtime + 1;
      tval[1].tv_usec = 0;
      utimes(f, tval);
#else
      times.actime = fbuf.st_atime;
      times.modtime = fbuf.st_mtime + 1;
      utime (f, &times);
#endif
    }
}


exists(f)
char *f;
{
  struct stat fbuf;

  if (lstat(f,&fbuf) == -1)
    return 0;
  else
    return 1;
}


fforceup(f)
char *f;
{
  char up[256];
  char sp[256];

  strcpy(up,updpath(f));
  strcpy(sp,savpath(f));

  if(exists(up))
    {
      printf("force update %s\n", f);
      if (exists(f))
	unlnk(sp); /* delete any old ones */
      rename(f, sp);
      if (sametimes(sp, up))
        touch(sp);
      rename(up, f);
    }
}
      

fsave(f)
char *f;
{
  char sp[256];

  if (exists(f)) /* if not, was saved in findmods list */
    {
      printf ("preserve %s\n", f);
      if (altsavp != NULL)
        {
	  char altsp[MAXFILPATH];
	  strcpy(altsp, altsavp);
	  strcat(altsp, "/");
	  strcat(altsp, f);
	  strcat(altsp, savsuffix);
	  cpy(f, altsp);
	  unlnk(f);
	}
      else
	{
	  strcpy(sp, savpath(f));
          rename(f, sp);
	}
    }
}

frestore(f)
char *f;
{
  char sp[256];

  printf ("restore %s\n", f);
  if (altsavp != NULL)
    {
      char altsp[MAXFILPATH];
      strcpy(altsp, altsavp);
      strcat(altsp, "/");
      strcat(altsp, f);
      strcat(altsp, savsuffix);
      cpy(altsp, f);
    }
  else
    {
      strcpy(sp, savpath(f));
      rename(sp, f);
    }
}

fsaveup(f)
char *f;
{
  char up[256], sp[256];

  strcpy(up,updpath(f));
  strcpy(sp,savpath(f));

  if (exists(sp))
    {
      printf("restore & save update of %s\n", f);
      unlnk(up); /* in case there was one there before */
      rename(f, up);
      if (altsavp != NULL)
        {
	  char altsp[MAXFILPATH];
	  strcpy(altsp, altsavp);
	  strcat(altsp, "/");
	  strcat(altsp, f);
	  strcat(altsp, savsuffix);
	  cpy(altsp, f);
	}
      else
        {
          strcpy(sp,savpath(f));
          rename(sp, f);
	}

      if (sametimes(f, up))
        touch(f);
    }
}


fdel(f)
char *f;
{
  unlnk(f);
}

pexec(s, t, v)
char *s, *t, *v[];
{
  char *myv[100];
  int c = 0;
  int vc = 1;
  char mynam[256], *mynamp;
  int chldpid, waitpid;
  int status;

  for (mynamp = v[0]; *mynamp; mynamp++) ;
  while (*(mynamp-1) && (*(mynamp-1) != '/')) mynamp--;
  myv[c++] = mynamp;
  myv[c++] = s;
  myv[c++] = t;
  while (*v[vc]) myv[c++] = v[vc++];
  myv[c] = 0;

  if (chldpid = fork())
    while (((waitpid = wait(&status)) != chldpid) && (waitpid != -1)) ;
  else
    {
#ifdef DEBUG
      char emsg[80];

      execv(mynamp, myv);
      sprintf(emsg, "couldn't exec %s, errno = %d\n", mynamp, errno);
      error(ERRTYPE_FATAL, emsg);
      exit(1);
#else
       int i;

       printf("exec: %s\n", v[0]);
       for (i == 0; myv[i] != 0; i++)
         printf("argv[%d] = \"%s\"\n", i, myv[i]);
#endif
    }

}  
