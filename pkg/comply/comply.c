/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */

#define MAIN 1
#include "comply.h"
static char rcs[]="$Header: comply.c,v 1.12.2.3.1.2 90/07/11 18:20:40 hawkes Exp $";

#define OPTIONS	"xF:bdefg:k:p:st:"

extern char *ownernam();
extern char *groupnam();

char *bomfilnam;
FILE *bomfil;

int xflag;

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

   umask(0); /* so things we create will mode from bom */
   setbuf(stdout, stdoutbuf);
   setbuf(stderr, stderrbuf);
   
   passfile = "/etc/passwd";
   groupfile = "/etc/group";
   
   xflag = fixmode = silent = extra = delete = 0;
   kthresh = 0;
   bomcheck = 0;
   conform = 1;
   tstamp = 0;

   while ((c = getopt(argc, argv, OPTIONS)) != EOF)
     switch (c)
       {
	 case 'F': 
#ifndef DEBUG
		   if (getuid() != 0)
		     error (ERRTYPE_FATAL, "comply -F must be run by root");
#else
		   printf ("(DEBUG MODE; -f ok for all users!)\n");
#endif
		   while (*optarg)
		     {
  		       switch (*optarg)
			 {
		           case 'm': fixmode |= FIXMODE_m; break;
		           case 't': fixmode |= FIXMODE_t; break;
		           case 'o': fixmode |= FIXMODE_o; break;
		           case 'g': fixmode |= FIXMODE_g; break;
		           case 'p': fixmode |= FIXMODE_p; break;
		           case 's': fixmode |= FIXMODE_s; break;
		           case 'l': fixmode |= FIXMODE_l; break;
			    default: usage(); break;
			 }
		       optarg++;
		     }
		   break;
	 case 'd': delete++;
#ifndef DEBUG
		   if (getuid() != 0)
		     error (ERRTYPE_FATAL, "comply -d must be run by root");
#else
		   printf ("(DEBUG MODE; -d ok for all users!)\n");
#endif
		   break;
	 case 'e': extra++; break;
	 case 'f': fixmode =
		     FIXMODE_m | FIXMODE_t | FIXMODE_o | FIXMODE_g |
		     FIXMODE_p | FIXMODE_s | FIXMODE_l;
#ifndef DEBUG
		   if (getuid() != 0)
		     error (ERRTYPE_FATAL, "comply -f must be run by root");
#else
		   printf ("(DEBUG MODE; -f ok for all users!)\n");
#endif
                   break;
	 case 'g': groupfile = optarg; break;
	 case 'k': kthresh = atoi(optarg); break;
 	 case 'p': passfile = optarg; break;
	 case 's': silent++; break;
	 case 'b': bomcheck++; break;
	 case 't': tstamp = atol(optarg); break;
	 case 'x': xflag++; break;
	 default : usage(); break;
       }

    if ((optind >= argc) || (argc < 2)) 
      usage();

    if (extra) 
      initextra(argc, argv, optind); /* initialization stuff for extra... */

    targetpath = "."; 
    
    initpw(passfile, groupfile); /* scarf up the password & group files */

    while (optind < argc) /* all additional arguments are bomfile names.. */
      {
        bomfilnam = argv[optind++];
	if ((bomfil = fopen(bomfilnam, "r")) == NULL)
	  {
	    sprintf(emsg, "can't open bomfile \"%s\"", bomfilnam);
	    error(ERRTYPE_FATAL, emsg);
	  }
	else
	  process(bomfil,bomfilnam); /* The rest is just details... :-) */
      }
  if (extra) 
    doextra();
  exit(conform == 0);
}

#define MAXBOMREC 512   /* maximum size of record in a bomfile */
#define MAXFILPATH 256  /* maximum size for a file or link target path */
#define RECHECK_LIM 5   /* maximum number of re-checks to allow on a file */

process(bomfil,bomfilnam)
FILE *bomfil;
char *bomfilnam;
{
  char bomrec[MAXBOMREC+1]; /* well... there are some long paths... */
  char path[MAXFILPATH], linktarg[MAXFILPATH]; 
  char targetfile[MAXFILPATH+2]; /* enough for ./path */
  int  mode, owner, group, devmaj, devmin, nlinks;
  int  modechkd, ownerchkd, groupchkd, linkschkd;
  struct stat sbuf;
  int recheck;

  while (fgets(bomrec, MAXBOMREC, bomfil) != NULL)
    {
      if (bomrec[0] == '#') continue; /* skip comments */

      if ((! parserec(bomrec, path, &mode, &owner, &group, &devmaj, &devmin,
	               &nlinks, linktarg)) || (bomcheck != 0))
        continue;
      
      strcpy(targetfile, "./");
      strcat(targetfile, path);
      
      if (lstat(targetfile,&sbuf) == -1)
        {
	  char *ty;

	  if (errno != ENOENT)
	    {
	      sprintf(emsg, "can't stat %s: %s: %s\n", ty, path,
	 	sys_errlist[errno]);
	      error(ERRTYPE_ERROR, emsg);
	      continue;
	    }

          setype(mode, &ty);
	  if (!xflag ||
	      !(((mode & S_IFMT) == S_IFCHR) || ((mode & S_IFMT) == S_IFBLK))) {
            sprintf(emsg, "missing %s: %s", ty, path);
  	    error(ERRTYPE_ERROR, emsg);
	  }
	  /* see if we can make one */
	  if (fixmode & FIXMODE_m)
            domknew(targetfile, owner, group, mode, linktarg);
        }

      if (lstat(targetfile,&sbuf) == -1)
        {
	  char *ty;

	  if (errno != ENOENT)
	    {
	      sprintf(emsg, "can't stat %s: %s: %s\n", ty, path,
	 	sys_errlist[errno]);
	      error(ERRTYPE_ERROR, emsg);
	      continue;
	    }
	  conform = 0; /* we weren't able to create it- */
        }
      else
        {   
          /* the file exists in target tree... now check/fix attributes */
	  
	  /* these flags allow us to be silent when individual checks */
	  /* fail in re-check passes 				      */
	  modechkd = ownerchkd = groupchkd = linkschkd = 0;

 	  recheck = 0; 
	  while(1) /* we need to re-check mode after owner fix, etc */
	    {
	      if (recheck > RECHECK_LIM) /* sanity check */
		{
		  sprintf(emsg, "internal error; too many re-checks on %s",
			targetfile);
		  error(ERRTYPE_ERROR, emsg);
		  conform = 0;
		  break; /* give up, go on to next file */
		}

	      /* make sure it's the right type of entity; */
	      if ((mode & S_IFMT) != (sbuf.st_mode & S_IFMT))
		{
  	          char *tyfound, *tyinbom;
		  struct stat tmpsbuf;

		  setype(mode, &tyinbom);
		  setype(sbuf.st_mode, &tyfound);
                  sprintf(emsg, "type of %s %s should be: %s",
	            path, tyfound, tyinbom);
	          error(ERRTYPE_ERROR, emsg);
		  
 	          /* we now try to fix anything that should be link, */
	          /* symlink, or dir...				     */
		  if ( (fixmode & FIXMODE_t) && (
		       ( (mode & S_IFMT) == S_IFDIR ) ||
		       ( (mode & S_IFMT) == S_IFLNK ) ||
		       ( ((mode & S_IFMT) == S_IFREG) &&
			 *linktarg && (stat(linktarg, tmpsbuf) > 0))
		     				       ) )
		    {
		      char cmd[512];

  		      sprintf(cmd, "/bin/rm -rf %s", targetfile);
		      system(cmd);
        	      domknew(targetfile, owner, group, mode, linktarg);
		      recheck++;		  
		    }
		  else
		    {
		      conform = 0;
		      break; /* give up, go to next file */
		    }
		}

	      if (recheck) /* this is a re-check pass after making a fix */
			   /* so get new stat info to reflect the change */
	        {
                  if (lstat(targetfile,&sbuf) == -1) /* should never be */
                    {
                      error(ERRTYPE_FATAL,
		        "internal error; file dissappeared on re-check!");
		      conform = 0;
                    }
		}

    	      /* check owner */

#ifdef BSD
	      if (sbuf.st_uid != owner)
#else
	      if ((sbuf.st_uid != owner))
#endif
	        {
		  if (ownerchkd == 0)
		    {
  	              sprintf(emsg, "owner %s %s should be: %s", 
	                path, ownernam(sbuf.st_uid), ownernam(owner));
	              error(ERRTYPE_ERROR, emsg);
		    }
	   	  ownerchkd++;
	          if (fixmode & FIXMODE_o)
	            {
#ifdef BSD
  	              if (dofixowner(targetfile, owner))
#else
  	              if (dofixowner(targetfile, owner, sbuf.st_gid))
#endif
			{
		          recheck++;
		          continue;
			}
		      else 
			conform = 0;
		    }
	        }
	    
	      /* check group */

#ifdef BSD
	      if (sbuf.st_gid != group)
#else
	      if ((sbuf.st_gid != group))
#endif
	        {
		  if (groupchkd == 0)
 		    {
	              sprintf(emsg, "group %s %s should be: %s", 
	                path, groupnam(sbuf.st_gid), groupnam(group));
	              error(ERRTYPE_ERROR, emsg);
		    }
	          groupchkd++;
	          if (fixmode & FIXMODE_g)
	            {
#ifdef BSD
  	              if (dofixgroup(targetfile, group))
#else
  	              if (dofixowner(targetfile, sbuf.st_uid, group))
#endif
		        {
 		          recheck++;
		          continue;
			}
		      else
			conform = 0;
		    }
	        }

   	      /* check mode */

	      if ((mode & S_IFMT) == S_IFLNK)
		{
		  /* don't check mode for symlinks */
		}
	      else
	        {
                  if (sbuf.st_mode != mode)
 	            {
	              char modstr1[19], modstr2[19];

		      if (modechkd == 0)
		        {
   		          strcpy(modstr1, ascmode(sbuf.st_mode));
	                  strcpy(modstr2, ascmode(mode));
	                  sprintf(emsg, "mode %s %s should be: %s", 
	                    path, modstr1, modstr2);
	                  error(ERRTYPE_ERROR, emsg);
		        }
		      modechkd++; 
	              if (fixmode & FIXMODE_p)
	                {
		          if (dofixmode(targetfile, sbuf.st_mode, mode))
	                    {
 	   	              recheck++;
		              continue;
			    }
			  else
			    conform = 0;
			}
		    }
		}

	      /* if it's a special file, check device #s... */

	      if (((mode & S_IFMT) == S_IFCHR) ||
                  ((mode & S_IFMT) == S_IFBLK))
		{
		  int imaj, imin;
		  
		  imaj = (sbuf.st_rdev & 0xff00) >> 8;
		  imin = (sbuf.st_rdev & 0x00ff);
		
		  if (imaj != devmaj)
		    {
		      sprintf(emsg, "major device %s %d should be: %d",
		        path, imaj, devmaj);
		      error(ERRTYPE_ERROR, emsg);
		      conform = 0;
		    }
		  if (imin != devmin)
		    {
		      sprintf(emsg, "minor device %s %d should be: %d",
		        path, imin, devmin);
		      error(ERRTYPE_ERROR, emsg);
		      conform = 0;
		    }
		}

              /* check links */
	
	      switch (sbuf.st_mode & S_IFMT)
		{
	          case S_IFDIR: break; 
		  case S_IFLNK: 
		    /* is the symbolic link correct? */
		    {
		      char lbuf[MAXPATHLEN+1]; /* rediculously long! */
		      int n;
	
		      if ((n = readlink(targetfile, lbuf, MAXPATHLEN)) == -1)
			{
			  sprintf(emsg, 
			    "internal error: can't readlink(%s), errno = %d",
		            targetfile, errno);
			  error(ERRTYPE_ERROR, emsg);
			  conform = 0;
			}
		      else
		        {
		          lbuf[n] = '\0';
			  if (strcmp(lbuf, linktarg) != 0)
		            {
			      sprintf(emsg, 
			       "incorrect symbolic link; %s %s should be: %s",
			        path, lbuf, linktarg);
			      error(ERRTYPE_ERROR, emsg);
		              if (fixmode & FIXMODE_s)
	                        {
		                  if
		                   (dofixsym(targetfile, linktarg))
	                            {
 	   	                      recheck++;
		                      continue;
			            }
				  else
 			            conform = 0;
			        }
			    }
			}
		    }		
	  	    break;
		  case S_IFREG:	
		  case S_IFCHR:
		  case S_IFBLK:
  	            if (sbuf.st_nlink != nlinks)
		      {
	                if (linkschkd == 0)
 		          {
	                    sprintf(emsg, "link count %s %d should be: %d", 
	                      path, sbuf.st_nlink, nlinks);
	                    error(ERRTYPE_ERROR, emsg);
			    conform = 0;
		          }
		      }
	            /* check link identity by comparing i-numbers */
	            if (*linktarg != '\0') /* only check "non-master" */
		      {
			struct stat ltargsbuf;
		        char ltargpath[MAXFILPATH+2]; 

	                strcpy(ltargpath, "./");
			strcat(ltargpath, linktarg);

			if (lstat(ltargpath, &ltargsbuf) == -1)
			  {
			    sprintf(emsg, 
			      "missing link target of %s \"%s\"",
			      path, linktarg);
			    error(ERRTYPE_ERROR, emsg);
			    conform = 0;
			  }
			else
			  if ((sbuf.st_ino != ltargsbuf.st_ino) ||
			      (sbuf.st_dev != ltargsbuf.st_dev))
  			    {
			      sprintf(emsg, 
			        "incorrect link; %s not linked to \"%s\"",
			        path, linktarg);
			      error(ERRTYPE_ERROR, emsg);
			      if (fixmode & FIXMODE_l)
		    		{
		                  char cmd[512];

  		                  sprintf(cmd, "/bin/rm -rf %s", targetfile);
		                  system(cmd);
        	                  domknew(targetfile, 
				            owner, group, mode, linktarg);
		      	          recheck++;		  
		    		}
			      else
			        conform = 0;
			    }
		      } /* if (*linktarg != '\0') */
	            break; 
	          default: break;
		} /* switch */
              linkschkd++;

	      /* Do -t if requested */
	
	      if (tstamp != (long)0)
		touch(targetfile, tstamp);

	      /* we've done all fixes which can be done - break while(1) */
	      break;

            } /* while (1) */
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

  *owner = owneruid(uname);

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

  *group = groupgid(gname);

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
  char bomtmp[MAXBOMREC+1];
  char *tmp;
  
  strcpy(bomtmp, r);
  for (tmp = bomtmp; *tmp && *tmp != '\n'; tmp++) ;
  if (*tmp == '\n') *tmp = '\0';
  
  sprintf(emsg, "bad bom record in file \"%s\" (%s):\n%s", 
    bomfilnam, why, bomtmp);
  error(ERRTYPE_ERROR, emsg);
  conform = 0;
}    
