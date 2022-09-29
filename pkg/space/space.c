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
#ident	"$Header: space.c,v 2.2.1.5 90/05/28 17:58:24 wje Exp $"
/*
** space [-m margin] [-d] sizefile ...
*/

#define MAIN 1
#include "space.h"

static char rcs[]="$Header: space.c,v 2.2.1.5 90/05/28 17:58:24 wje Exp $";

#define OPTIONS	"dm:"

char *sizfilnam;
FILE *sizfil;

char stdoutbuf[BUFSIZ];
char stderrbuf[BUFSIZ];

int dump = 0;

long nonsublocks;


confirm(prompt,def,was)
char *prompt;
int def;
char *was;
{
  int c,d,t,fini;
  fini = 0;
  while (! fini) {
    if (! strcmp(was,"")) {
      fprintf(stderr,"%s",prompt);
    } else {
      fprintf(stderr,"%s (%s)",prompt,was);
    }
    if (def == 1) fprintf(stderr," [yes] ");
    if (def == 0) fprintf(stderr," [no] ");
    d = getchar();
    while (d == 32 || d == 9) {
      d = getchar();
    }
    t = d;
    while (t != 10) {
      t = getchar();
    }
    if (d == 10 && (def == 1 || def == 0)) return(def);
    if (d == 'y' || d == 'Y') return(1);
    if (d == 'n' || d == 'N') return(0);
    fprintf(stderr,"\n");
    fprintf(stderr,"Please answer with \"yes\" or \"no\"\n");
    fprintf(stderr,"\n");
  }
}

bail()
{
  cleanup();
  exit(1);
}

main(argc, argv)
int argc;
char *argv[];
{
   extern int optind;
   extern char *optarg;

   struct stat sbuf;
   int c;
   char *tmp;

   setbuf(stdout, stdoutbuf);
   setbuf(stderr, stderrbuf);
   
   excode = 1;
   margin = 0.0;

   while ((c = getopt(argc, argv, OPTIONS)) != EOF)
     switch (c)
       {
	 case 'd': dump = 1;
	           break;
         case 'm': margin = atof(optarg);
	           break;
	 default : usage(); break;
       }

    if ((optind >= argc) || (argc < 2)) 
      usage();
    
   inocreds = lastinocred = NULL;
   dirnames = lastdirname = NULL;
   touchfile[0] = '\0';
   targetpath = "./"; 
    
   signal(SIGHUP, bail);
   signal(SIGINT, bail);
   signal(SIGQUIT, bail);
   signal(SIGTERM, bail);

   while (optind < argc) /* all additional arguments are sizfile names.. */
      {
        sizfilnam = argv[optind++];
	if ((sizfil = fopen(sizfilnam, "r")) == NULL)
	  {
	    sprintf(emsg, "can't open size file \"%s\"", sizfilnam);
	    error(ERRTYPE_FATAL, emsg);
	  }
	else
	  process(sizfil,sizfilnam); /* The rest is just details... :-) */
      }
  if (dump) dumpresults();
  excode = checkresults();
  cleanup();
/*
  if (excode != 0) excode = 1;
*/
  exit(excode);
}


process(sizfil,sizfilnam)
FILE *sizfil;
char *sizfilnam;
{
  char sizrec[MAXPRESREC+1]; /* well... there are some long paths... */
  char path[MAXFILPATH];
  char targetfile[MAXFILPATH+2]; /* enough for ./path */
  int  size;
  struct stat sbuf;
  int  touchfd;

  while (fgets(sizrec, MAXPRESREC, sizfil) != NULL)
    {
      if (sizrec[0] == '#') continue; /* skip comments */

      parserec(sizrec, path, &size);
            
      strcpy(targetfile, targetpath);
      strcat(targetfile, path);
      
      if (lstat(targetfile,&sbuf) == -1)
        {
	  /*
	  ** File of this pathname does not currently exist;
	  ** touch a file with the pathname to determine which
	  ** filesystem it will go to when installed, then
	  ** remove the file.
	  */
	  
	  insuredir(targetfile);
	  strcpy(touchfile, targetfile);
	  
	  if ((touchfd = open(targetfile, O_RDWR | O_CREAT, 0600)) == -1)
	    if (errno == ENOENT)
	      {
  	        sprintf(emsg, "couldn't touch %s ENOENT", targetfile);
	        error(ERRTYPE_FATAL, emsg);
	      }
	    else
	      {
  	        sprintf(emsg, "couldn't touch %s, errno = %d", targetfile,
			errno);
	        error(ERRTYPE_FATAL, emsg);
	      }

	  if (lstat(targetfile, &sbuf) == -1)
	    {
	      sprintf(emsg, "couldn't stat touched %s", targetfile);
	      error(ERRTYPE_FATAL, emsg);
	    }

	  /* tally the space requirement for this file */
	  tallyreq(sbuf.st_dev, size, targetfile);

	  close(touchfd);

	  if(unlink(targetfile) == -1)
	    {
	      sprintf(emsg, "couldn't unlink touched %s", targetfile);
	      error(ERRTYPE_FATAL, emsg);
	    }
	  touchfile[0] = '\0';
          
        }
      else
	{
	  tallyreq(sbuf.st_dev, size, targetfile);

	  /*
	  ** A file with this pathname already exists; it will be
	  ** overwritten when the release is installed, so credit
	  ** its space to the filesystem. 
	  */
	  
	  /*
	  ** But... don't credit space from directories, as it cannot
	  ** be assumed that any of the files in existing directories
	  ** will be replaced with files from the release. This is the
	  ** conservative thing to do.
	  ** We are just a bunch of conservative folks, I guess.
	  */

	  /*
	  ** Also.. for symlinks, the st_size field is the size of the symlink
	  ** path, so don't credit sizes for symlinks.
	  */
	    
 	  if (((sbuf.st_mode & S_IFMT) == S_IFDIR)
	      || (sbuf.st_mode & S_IFMT) == S_IFLNK)
	    {
  	      sbuf.st_size = 0;
	      sbuf.st_nlink = 0;
	    }
          if (sbuf.st_nlink != 1)
	    if (credited(sbuf.st_ino))
              sbuf.st_size = 0;
	    else
	      credit(sbuf.st_ino);

          /* can't safely credit more than the current file size */
          if (sbuf.st_size > size) {
            sbuf.st_size = size;
          }
	  tallycred(sbuf.st_dev, sbuf.st_size, targetfile);
	}

    } /* while fgets size record */
} /* process() */


parserec(rec, path, size)
char *rec, *path;
int *size;
{
  char *recsav = rec;

  skipspace(rec);
  if (! strcpyfld(&path, &rec, MAXFILPATH))
    { 
      badrec(recsav, "file path too long");
      return(0);
    }
  skipspace(rec);

  /* parse size */

  skipspace(rec);

  if ((! isdigit(*rec)) && (*rec != '-'))
    {
      badrec(recsav, "bad or missing size");
      return(0);
    }
  *size = atoi(rec);
  while (isdigit(*rec)) rec++;

  /* any sanity checks for semantically bad size records go here */

  return(1);
}
        
badrec(r, why)
char *r, *why;
{
  char siztmp[MAXPRESREC+1];
  char *tmp;
  
  strcpy(siztmp, r);
  for (tmp = siztmp; *tmp && *tmp != '\n'; tmp++) ;
  if (*tmp == '\n') *tmp = '\0';
  
  sprintf(emsg, "bad input record (%s):\n%s", why, siztmp);
  error(ERRTYPE_ERROR, emsg);
}    


checkresults()
{
  int i;
  long bavail, breq;
  long iavail, ireq;

  long shortfall = 0;
  int percent = 0;
  int worstpercent = 0;

  for (i = 0; i < nfs; i++)
    {
      /*
      ** To be on the safe side, we inflate the requirements a bit
      */
      nonsublocks = (long)(fstab[i].blocks / 10);
      breq = fstab[i].req + (long)(fstab[i].req * margin);
      ireq = fstab[i].ino_req + (long)(fstab[i].ino_req * margin);
      bavail = fstab[i].free - breq + fstab[i].cred - nonsublocks;
      iavail = fstab[i].ino_free - ireq + fstab[i].ino_cred;

      percent = (((long)(fstab[i].blocks)-fstab[i].cred-fstab[i].free)+breq) * 100 / (long)(fstab[i].blocks);
/*
printf("Percent of disk used: %ld\n",percent);
printf("breq is %ld   total disk avail is %ld\n",breq,fstab[i].free + fstab[i].cred);
printf("bavail is %ld   nonsublocks is %d\n",bavail,nonsublocks);
*/
      if (percent > worstpercent) worstpercent = percent;

      if (bavail < 0)
        {
          if (bavail + nonsublocks > 0) {
          /* if no shortage on any other disk, print warning and exit with 9 */
            if (shortfall == 0) shortfall = 9;
          } else {
	    shortfall = 1;
	    fprintf(stderr,
	  "%s short %ld blocks", fstab[i].devpath, -1 * (bavail + nonsublocks));
            if (margin > 0.0)
	      fprintf(stderr, " (including %ld blocks of margin)", 
	        (long)(fstab[i].req * margin));
            fprintf(stderr, "\n");
          }
        }

      if (iavail < 0)
        {
	  shortfall = 1;
	  fprintf(stderr,
            "%s short %ld inodes", fstab[i].devpath, -1 * iavail);
          if (margin > 0.0)
	    fprintf(stderr, " (including %ld inodes of margin)", 
	      (long)(fstab[i].ino_req * margin));
          fprintf(stderr, "\n");
        }
	
    }
    if (shortfall == 9) {
            fprintf(stderr,"\n");
fprintf(stderr,"       WARNING!  This package will fit on the disk,\n");
fprintf(stderr,"       but it will cause more than %ld%% of the disk to be used.\n",worstpercent);
fprintf(stderr,"       This may cause problems for non-root users.\n");
fprintf(stderr,"       It is recommended that you abort the installation now.\n");
            fprintf(stderr,"\n");
    }

  return shortfall;

}


dumpresults()
{
  int i;
      

  printf("%18s %9s %9s %9s %9s %9s %9s\n",
    "device",
    "bfree",
    "ifree",
    "breq",
    "ireq",
    "bcred",
    "icred"
    );

  for (i = 0; i < nfs; i++)
    printf("%18s %9ld %9ld %9ld %9ld %9ld %9ld\n",
      fstab[i].devpath,
      fstab[i].free,
      fstab[i].ino_free,
      fstab[i].req,
      fstab[i].ino_req,
      fstab[i].cred,
      fstab[i].ino_cred
      );

}
