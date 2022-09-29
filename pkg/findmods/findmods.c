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
#ident	"$Header: findmods.c,v 1.2.2.2 90/05/10 03:47:52 wje Exp $"
/*
**    findmods package bomfile ...
*/

#define MAIN 1
#include "findmods.h"

static char rcs[]="$Header: findmods.c,v 1.2.2.2 90/05/10 03:47:52 wje Exp $";

#define OPTIONS	""

char *bomfilnam;
FILE *bomfil;

char stdoutbuf[BUFSIZ];
char stderrbuf[BUFSIZ];

long modstamps[MAXSTAMPS];
int  nstamps;
int  foundmods = 0;

#ifdef BSD
int  doingumips;
#endif

main(argc, argv)
int argc;
char *argv[];
{
   extern int optind;
   extern char *optarg;

   FILE *log;
   char lognam[80];

   char lpkgnam[80];
   char pkgnam[80];
   char spllpkgnam[80];
   char splpkgnam[80];
   char *spllpkg, *spllsubpkg;
   char *splpkg, *splsubpkg;

   char version[80];
   struct stat sbuf;
   int c;
   char *tmp;
   long stamp;
   int match;

   setbuf(stdout, stdoutbuf);
   setbuf(stderr, stderrbuf);
   
   while ((c = getopt(argc, argv, OPTIONS)) != EOF)
     switch (c)
       {
	 default : usage(); break;
       }

    if ((optind >= argc) || (argc < 3)) 
      usage();
    
    targetpath = "./"; 
    
    strcpy(lognam, targetpath);
    strcat(lognam, "etc/installlog");
    if ((log = fopen(lognam, "r")) == NULL)
      {
	exit(1);
#if 0
        char emsg[80];
        sprintf(emsg, "couldn't open installation log file \"%s\"", lognam);
	error(ERRTYPE_FATAL, emsg);
#endif
      }
   optind = 2;

   /*
   ** Build a list of the stamps for this package.
   **
   ** The zero'th element of the list, if nonzero, is treated as
   ** a special case; preserve will treat any file with a stamp
   ** predating this stamp as having come from a release. This
   ** special case is presently used only when installing on systems
   ** on which UMIPS-BSD, 1.0 or earlier has been installed.
   ** Those releases used an older definition of timestamps, 
   ** where anything earlier than the given time was from the release.
   ** 
   ** All other timestamps are kept starting with the 1'th element
   ** of the array modstamps[];
   */

   strcpy(pkgnam, argv[1]);
   strcpy(splpkgnam, argv[1]);

   splpkg = splpkgnam;
   for (splsubpkg = splpkg;
	 (*splsubpkg) && (*splsubpkg != '.');
	   splsubpkg++) ;

   if (*splsubpkg == '.')
     {
       *splsubpkg = '\0';
       splsubpkg++;
     }

#ifdef debug
   printf ("    package <%s> pkg <%s> subpkg<%s>\n", pkgnam,
	   splpkg, splsubpkg);
#endif


   nstamps = 0;
   modstamps[0] = 0; /* means no 1.0 or earlier release */
#ifdef BSD
   doingumips = (strcmp(argv[1], "umips") == 0);
#endif

   while
     ((c = fscanf(log, "%s %s %ld", lpkgnam, version, &stamp)) != EOF)
       {
         if (c != 3)
           {
 	     char emsg[80];
             sprintf(emsg, "bad log file \"%s\"", lognam);
	     error(ERRTYPE_FATAL, emsg);
           }

         strcpy(spllpkgnam, lpkgnam);

         spllpkg = spllpkgnam;
         for (spllsubpkg = spllpkg;
	       (*spllsubpkg) && (*spllsubpkg != '.');
	         spllsubpkg++) ;

         if (*spllsubpkg == '.')
           {
             *spllsubpkg = '\0';
             spllsubpkg++;
           }

#ifdef debug
         printf ("log package <%s> pkg <%s> subpkg<%s>\n", lpkgnam,
	   spllpkg, spllsubpkg);
#endif

	 match = 0;

	 /*
	 ** OK, this stuff gets weird, mainly for backwards compatibility.
         ** The idea is that you should be able to install and old package,
	 ** which runs findmods with just a pkgname onto a system with an
         ** installlog containing pkgname.subpkgname entries, or vice versa.
         */

	 switch (strlen(spllsubpkg))
	   {
	      case 0: switch (strlen(splsubpkg))
		        {
			   case 0: match =
			             (strcmp(lpkgnam, pkgnam) == 0);
			           break;
			  default: match =
			             ((strcmp(splsubpkg, lpkgnam) == 0) ||
				      (strcmp(splpkg, lpkgnam) == 0));
			           break;
			}
		      break;
	     default: switch (strlen(splsubpkg))
		        {
			   case 0: match =
			             ((strcmp(spllsubpkg, pkgnam) == 0) ||
				      (strcmp(spllpkg, pkgnam) == 0));
			           break;
			  default: match =
			             (strcmp(lpkgnam, pkgnam) == 0);
			           break;
			}
           }

	 if (match)
	   {

#ifdef debug
	     printf ("match!\n");
#endif

#ifdef BSD
       	     if (doingumips && isoldver(version))
               modstamps[0] = stamp;
	     else
#endif
               {
		 if (++nstamps >= MAXSTAMPS)
		   {
		     char emsg[80];
		     sprintf(emsg, "too many updates");
		     error(ERRTYPE_FATAL, emsg);
		     exit (1);
		   }
	         modstamps[nstamps] = stamp;
	       }
	   }
	 while ((c = fgetc(log)) != '\n' && c != EOF) ;
         if (c == NULL) break;
       }
   
#ifdef DEBUG
   {
     int i;
     fprintf(stderr, "nstamps = %d\n", nstamps);
     for (i = 0; i <= nstamps; i++) 
       fprintf(stderr, "modstamps[%d] = %d\n", i, modstamps[i]);
     fflush(stderr);
   }
#endif

   if ((modstamps[0] == 0) && (nstamps == 0))
     exit(2); /* no need to search boms, first install of this package */

   printf("#\n# findmods files:\n#\n"); /* a comment in preserve input */

   while (optind < argc) /* all additional arguments are bomfile names.. */
      {
        bomfilnam = argv[optind++];
	if ((bomfil = fopen(bomfilnam, "r")) == NULL)
	  {
	    sprintf(emsg, "can't open bom file \"%s\"", bomfilnam);
	    error(ERRTYPE_FATAL, emsg);
	  }
	else
	  process(bomfil,bomfilnam); /* The rest is just details... :-) */
      }
  exit(foundmods == 0);
}

#define MAXPRESREC 512   /* maximum size of record in a bomfile */
#define MAXFILPATH 256  /* maximum size for a file or link target path */

process(bomfil,bomfilnam)
FILE *bomfil;
char *bomfilnam;
{
  char bomrec[MAXPRESREC+1]; /* well... there are some long paths... */
  char path[MAXFILPATH], linktarg[MAXFILPATH];
  char targetfile[MAXFILPATH+2]; /* enough for ./path */
  int  mode, owner, group, devmaj, devmin, nlinks;
  int  modechkd, ownerchkd, groupchkd, linkschkd;
  struct stat sbuf;
  int  ncheck;

  while (fgets(bomrec, MAXPRESREC, bomfil) != NULL)
    {
      if (bomrec[0] == '#') continue; /* skip comments */

      if ((! parserec(bomrec, path, &mode, &owner, &group, &devmaj, &devmin,
	               &nlinks, linktarg)) || (bomcheck != 0))
        continue;
      
      strcpy(targetfile, targetpath);
      strcat(targetfile, path);
      
      if (lstat(targetfile,&sbuf) == -1)
        {
	  /* can't fear overwrite on nenexistent file! */
        }
      else
        {
	  /* don't bother symbolic links */
	  if ((sbuf.st_mode & S_IFLNK) == S_IFLNK) continue;

#ifdef BSD
	  /* or sockets (though the bom shouldn't have any! */
	  if ((sbuf.st_mode & S_IFSOCK) == S_IFSOCK) continue;
#endif
 	  /* don't bother special files */
	  if ((sbuf.st_mode & S_IFCHR) == S_IFCHR) continue;
	  if ((sbuf.st_mode & S_IFBLK) == S_IFBLK) continue;

	  /* or directories in the bom (only explicitly named dirs) */
	  if ((sbuf.st_mode & S_IFDIR) == S_IFDIR) continue;

	  /* see if the mod time indicates a user modified file */

	  ncheck = 1; 
	  if (sbuf.st_mtime > modstamps[0]) /* might be user mod */
	    {
	      while
		((ncheck <= nstamps) && (sbuf.st_mtime != modstamps[ncheck]))
	          ncheck++;
	      if (ncheck > nstamps)
	        {
      	          printf("%s\n", path);
		  foundmods = 1;
	        }
	    }
	} /* else existing file */
    } /* while fgets bom record */
} /* process() */


parserec(rec, path, mode, owner, group, devmaj, devmin, nlinks, linktarg)
char *rec, *path;
int *mode, *owner, *group, *devmaj, *devmin, *nlinks;
char *linktarg;
{
  char *recsav = rec, *linktargsav = linktarg;
  char uname[MAXUNAM], *unamp = uname;
  char gname[MAXGNAM], *gnamp = gname;
  int tmod;

  skipspace(rec);
  if (! strcpyfld(&path, &rec, MAXFILPATH))
    { 
      badrec(recsav, "file path too long");
      return(0);
    }
  skipspace(rec);

  /* parse mode */

  tmod = 0;
  switch(*rec++)
    {
      case '-': tmod |= S_IFREG; break;
      case 'd': tmod |= S_IFDIR; break;
      case 'b': tmod |= S_IFBLK; break;
      case 'c': tmod |= S_IFCHR; break;
      case 'l': tmod |= S_IFLNK; break;
#ifdef BSD
      case 's': tmod |= S_IFSOCK; break; /* in a bomfile?! */
#endif
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 'r': tmod |= IREAD_O; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 'w': tmod |= IWRITE_O; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 's': tmod |= (IEXEC_O | S_ISUID); break;
      case 'x': tmod |= IEXEC_O; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 'r': tmod |= IREAD_G; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 'w': tmod |= IWRITE_G; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 's': tmod |= (IEXEC_G | S_ISGID); break;
      case 'x': tmod |= IEXEC_G; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 'r': tmod |= IREAD_A; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      case 'w': tmod |= IWRITE_A; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  switch(*rec++)
    {
      /* will interpret ---------t as implying ---------x */
      case 't': tmod |= (IEXEC_A | S_ISVTX); break;
      case 'x': tmod |= IEXEC_A; break;
      case '-': break;
      default:
        badrec(recsav, "bad mode");
	return(0);
	break;
    }
  *mode = tmod;

  /* parse owner */

  skipspace(rec);
  if (*rec == '\n')  
    {
      badrec(recsav, "missing owner");
      return(0);
    }
  if (! strcpyfld(&unamp, &rec, MAXUNAM))
    { 
      badrec(recsav, "owner name too long");
      return(0);
    }

  /*  *owner = owneruid(uname); see if I care! */

  /* parse group */

  skipspace(rec);
  if (*rec == '\n')  
    {
      badrec(recsav, "missing group");
      return(0);
    }
  if (! strcpyfld(&gnamp, &rec, MAXGNAM))
    { 
      badrec(recsav, "group name too long");
      return(0);
    }

  /*  *group = groupgid(gname); I don't give a whoot! */

  /* if special file, parse major & minor device numbers */

  if (((*mode & S_IFMT) == S_IFCHR) ||
      ((*mode & S_IFMT) == S_IFBLK ))
    {  
      skipspace(rec);
      if (! isdigit(*rec))
        {
          badrec(recsav, "bad major device number");
          return(0);
	}
      *devmaj = atoi(rec);
      while (isdigit(*rec)) rec++;
   
      skipspace(rec);
      if (! isdigit(*rec))
        {
          badrec(recsav, "bad minor device number");
          return(0);
        }
      *devmin = atoi(rec);
      while (isdigit(*rec)) rec++;
    }

  /* parse link count */

  skipspace(rec);

  if (! isdigit(*rec))
    {
      badrec(recsav, "missing link count");
      return(0);
    }
  *nlinks = atoi(rec);
  while (isdigit(*rec)) rec++;

  /* parse link target */

  skipspace(rec);
  if (! strcpyfld(&linktarg, &rec, MAXFILPATH))
    { 
      badrec(recsav, "link target name too long");
      return(0);
    }

  /* any sanity checks for semantically bad bom records go here */

  switch (*mode & S_IFMT)
    {
      case S_IFDIR:  break;
      case S_IFLNK:  if (strlen(linktargsav) == 0)
		     {
  		       badrec(recsav, "missing symbolic link target");
		       return(0);
		     }
	           break;
      case S_IFREG:
      case S_IFCHR:
      case S_IFBLK:  if ((*nlinks == 1) && (strlen(linktargsav) != 0))
		     {
		       badrec(recsav,
    		         "link count = 1 and non-empty link target");
		       return(0);
		     }
		   break;
#ifdef BSD
      case S_IFSOCK:
#endif
      default:     break;
    }

  return(1);
}
        
badrec(r, why)
char *r, *why;
{
  char bomtmp[MAXPRESREC+1];
  char *tmp;
  
  strcpy(bomtmp, r);
  for (tmp = bomtmp; *tmp && *tmp != '\n'; tmp++) ;
  if (*tmp == '\n') *tmp = '\0';
  
  sprintf(emsg, "bad input record (%s):\n%s", why, bomtmp);
  error(ERRTYPE_ERROR, emsg);
}    
