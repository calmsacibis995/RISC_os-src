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
#ident	"$Header: mksizes.c,v 1.3.2.4 90/05/10 03:58:11 wje Exp $"
/*
**    mksizes bomfile ...
*/

#define MAIN 1
#include "mksizes.h"

static char rcs[]="$Header: mksizes.c,v 1.3.2.4 90/05/10 03:58:11 wje Exp $";

#define OPTIONS	""
#define MEG ((float)1024.0*1024.0)

char *bomfilnam;
FILE *bomfil;

char stdoutbuf[BUFSIZ];
char stderrbuf[BUFSIZ];


main(argc, argv)
int argc;
char *argv[];
{
   extern int optind;
   extern char *optarg;

   struct stat sbuf;
   int c;
   char *tmp;

   totalbytes = totalinodes = 0;

   setbuf(stdout, stdoutbuf);
   setbuf(stderr, stderrbuf);
   
   while ((c = getopt(argc, argv, OPTIONS)) != EOF)
     switch (c)
       {
	 default : usage(); break;
       }

    if ((optind >= argc) || (argc < 2)) 
      usage();
    
   targetpath = "./"; 
    
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
  fprintf(stderr, "total size: %ld bytes (~ %4.2fMb), %ld inodes\n", 
    totalbytes, (float)totalbytes / MEG, totalinodes);
  exit(0);
}

#define MAXPRESREC 512   /* maximum size of record in a bomfile */
#define MAXFILPATH 256   /* maximum size for a file or link target path */

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
	  if (!(((mode & S_IFMT) == S_IFCHR) || ((mode & S_IFMT) == S_IFBLK))) {
	    sprintf(emsg, "can't stat %s\n", targetfile);
	    error(ERRTYPE_WARN, emsg);
	  }
	  /*
	  ** trust the bom, count an inode anyway; may be a special
	  ** file that's not in the packaging tree, but which will be
	  ** created by MKDEV at install time...
	  */
	  totalinodes++; 
        }
      else
	{
	  totalinodes++;
          if ( ((mode & S_IFMT) == S_IFCHR) ||
	       ((mode & S_IFMT) == S_IFBLK) )
    	      printf("%s %d\n", path, (long)0);
	  else
	    {
   	      printf("%s %d\n", path, sbuf.st_size);
	      totalbytes += sbuf.st_size;
	    }
	}

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
