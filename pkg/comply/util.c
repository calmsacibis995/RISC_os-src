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
#ident	"$Header: util.c,v 1.7.2.2 90/05/10 03:44:57 wje Exp $"
static char rcs[]="$Header: util.c,v 1.7.2.2 90/05/10 03:44:57 wje Exp $";

#include "comply.h"

error(etype, s)
int etype;
char *s;
{
  static int kcount = 0;
  static char *typstr[] = { "warning", "error", "fatal"};
  char errstr[1024];

  sprintf(errstr, "%s: %s", typstr[etype], s);
  errmsg(errstr);
  if ( fixmode && (etype == ERRTYPE_ERROR) && (kthresh) &&
                                                  (++kcount >= kthresh))
    {
      errmsg("error threshold reached; terminating");
      exit(1); 
    }
  if (etype == ERRTYPE_FATAL)
    exit(1);
}

errmsg(s)
char *s;
{
  if (silent == 0)
    fprintf(stderr, "comply: %s\n", s);
}

usage()
{
  errmsg(
 "usage: [-befds] [-F mtogpsl] [-k n] [-t time] [-p passfile]\n\
               [-g groupfile] bomfile ...");

  exit(1);
}

/* 
**  The following structures are used to hold the necessary passwd & group
**  file information. The following approach will be used: 
**  The user/group names for users/groups with numerical id's 0-N[UG]IDDIR,
**  respectively, will be held in array whose index is the numerical uid
**  gid itself; up to N[UG]IDIND additional id's will be stored in an
**  array of structures which must be linearly searched. This should
**  be satisfactory, since we're dealing with distribution software with
**  just the standard user's & groups, so most, if not all, ids will be
**  low, and thus indexed directly...
**
**  In cases where multiple user/group names share an id, do those as
**  indirects...
**
*/

struct uidind
  {
    int  uid;
    char uname[MAXUNAM];
  };

char uidlo[NUIDDIR][MAXUNAM];
struct uidind uidin[NUIDIND];

struct gidind
  {
    int  gid;
    char gname[MAXGNAM];
  };

char gidlo[NGIDDIR][MAXGNAM];
struct gidind gidin[NGIDIND];

#define MAXPWREC 256
#define MAXGRPREC 256

initpw(pw,grp)
char *pw, *grp;
{
  char pwrec[MAXPWREC+1];
  char grprec[MAXGRPREC+1];
  FILE *pwfil;
  int  nuidind = 0, ngidind = 0;
  int  uid, gid;
  char pwunam[MAXUNAM], grpnam[MAXGNAM];  

  for (uid = 0; uid < NUIDDIR; uid++) uidlo[uid][0] = '\0';
  for (gid = 0; gid < NUIDDIR; gid++) gidlo[gid][0] = '\0';

  if ((pwfil = fopen(pw, "r")) == NULL)
    {
      sprintf(emsg, "can't open password file \"%s\"\n", pw);
      errmsg(emsg);
      exit(1);
    }
  else
    {
      while (fgets(pwrec, MAXPWREC, pwfil) != NULL)
        {
          if(strlen(pwrec) == MAXPWREC)
  	    {
  	      errmsg("password file record too long");
  	      exit(1);
  	    }
          /* get name & id from record */
	  getnamid(pwrec, pwunam, &uid);
	  /* now update our tables */
	  if ((uid < NUIDDIR) && (uidlo[uid][0] == '\0'))
	    strcpy(uidlo[uid], pwunam);
          else
 	    if (nuidind >= NUIDIND)
	      {
		errmsg("too many \"indirect\" uids in password file");
	        exit(1);
	      }
	    else 
	      {
		uidin[nuidind].uid = uid;
		strcpy(uidin[nuidind++].uname, pwunam);
	      }
	  }
        fclose(pwfil);
      }
       
  if ((pwfil = fopen(grp, "r")) == NULL)
    {
      sprintf(emsg, "can't open group file \"%s\"\n", grp);
      errmsg(emsg);
      exit(1);
    }
  else
    {
      while (fgets(grprec, MAXGRPREC, pwfil) != NULL)
	{
	  if(strlen(grprec) == MAXGRPREC)
	    {
 	      errmsg("group file record too long");
      	      exit(1);
	    }
	  /* get name & id from record */
	  getnamid(grprec, grpnam, &gid);
	  /* now update our tables */
	  if ((gid < NGIDDIR) && (gidlo[gid][0] == '\0'))
	    strcpy(gidlo[gid], grpnam);
	  else
	    if (ngidind >= NGIDIND)
	      {
	        errmsg("too many \"indirect\" gids in group file");
	        exit(1);
	      }
	    else 
	      {
	        gidin[ngidind].gid = gid;
	        strcpy(gidin[ngidind++].gname, grpnam);
	      }
        }
      fclose(pwfil);
    }
} 

/*
** currently depends on valid password/group file entry
*/

getnamid(rec, name, id)
char *rec;  /* password/group record */
char *name; /* filled in with name from record */
int  *id;   /* filled in with user/group numerical id */
{
  while (*rec != ':') *name++ = *rec++;
  *name = '\0';
  rec++;
  while (*rec++ != ':') ;
  *id = atoi(rec);
}

static char unknown[] = "???";

char *ownernam(owner)
int owner;
{
  int uscan = 0;

  if (owner < NUIDDIR)
    return(uidlo[owner]);
  else
    {
      while ((uscan < NUIDIND) && (uidin[uscan].uid != owner)) uscan++;
      if (uscan >= NUIDIND)
	  return(unknown);
      else
	  return(uidin[uscan].uname);
    }
}      

char *groupnam(group)
int group;
{
  int gscan = 0;

  if (group < NGIDDIR)
    return(gidlo[group]);
  else
    {
      while ((gscan < NGIDIND) && (gidin[gscan].gid != group)) gscan++;
      if (gscan >= NGIDIND)
	  return(unknown);
      else
	  return(gidin[gscan].gname);
    }
}      

#define DEFOWNUID 0

owneruid(owner)
char *owner;
{
  int n;

  for (n = 0; n < NUIDDIR; n++)
    if (strcmp(uidlo[n], owner) == 0)
      return(n);

  for (n = 0; n < NUIDIND; n++)
    if (strcmp(uidin[n].uname, owner) == 0)
      return(uidin[n].uid);

  sprintf(emsg, "user name %s missing in password file; uid set to %d",
    owner, DEFOWNUID);
  error(ERRTYPE_WARN, emsg);
  conform = 0;
  return(DEFOWNUID);
}

#define DEFGRPUID 0

groupgid(group)
char *group;
{
  int n;

  for (n = 0; n < NGIDDIR; n++)
    if (strcmp(gidlo[n], group) == 0)
      return(n);

  for (n = 0; n < NGIDIND; n++)
    if (strcmp(gidin[n].gname, group) == 0)
      return(gidin[n].gid);

  sprintf(emsg, "group name %s missing in group file; gid set to %d",
    group, DEFGRPUID);
  error(ERRTYPE_WARN, emsg);
  conform = 0;
  return(DEFGRPUID);
}

char amode[] = "0000000 ----------";

char *ascmode(mode)
int mode;
{
#ifdef 0
  char *m = &amode[8];
  
  sprintf(amode, "0%6o ", mode);
#else
  char *m = amode;
#endif


  switch(mode & S_IFMT)
    {
      case S_IFCHR: *m++ = 'c'; break;
      case S_IFDIR: *m++ = 'd'; break;
      case S_IFBLK: *m++ = 'b'; break;
      case S_IFREG: *m++ = '-'; break;
      case S_IFLNK: *m++ = 'l'; break;
#ifdef BSD
      case S_IFSOCK: *m++ = 's'; break;
#endif
      default: *m++ = '?'; break;
    }

  if (mode & IREAD_O)
    *m++ = 'r';
  else
    *m++ = '-';
  if (mode & IWRITE_O)
    *m++ = 'w';
  else
    *m++ = '-';
  if (mode & S_ISUID)
    *m++ = 's';
  else if (mode & IEXEC_O)
	 *m++ = 'x';
       else
	 *m++ = '-';
  
  if (mode & IREAD_G)
    *m++ = 'r';
  else
    *m++ = '-';
  if (mode & IWRITE_G)
    *m++ = 'w';
  else
    *m++ = '-';
  if (mode & S_ISGID)
    *m++ = 's';
  else if (mode & IEXEC_G)
	 *m++ = 'x';
       else
	 *m++ = '-';
  
  if (mode & IREAD_A)
    *m++ = 'r';
  else
    *m++ = '-';
  if (mode & IWRITE_A)
    *m++ = 'w';
  else
    *m++ = '-';
  if (mode & S_ISVTX)
    *m++ = 's';
  else if (mode & IEXEC_A)
	 *m++ = 'x';
       else
	 *m++ = '-';
  
  *m = '\0';
  return(amode);
}

setype(mode, pt)
int mode;
char **pt;
{
  switch (mode & S_IFMT)
    {
      case S_IFDIR: *pt = "directory"; break;
      case S_IFLNK: *pt = "symbolic link"; break;
      case S_IFREG: *pt = "file"; break;
      case S_IFCHR: *pt = "character special file"; break;
      case S_IFBLK: *pt = "block special file"; break;
#ifdef BSD
      case S_IFSOCK: *pt = "socket file"; break; /* in bom? */
#endif
      default:    *pt = "???"; break;
    }
}

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


#ifdef BSD
#else
struct utimbuf {
	time_t actime; 	/* access time */
	time_t modtime;	/* modification time */
};
#endif

touch(f, t)
char *f;
long t;
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
      tval[1].tv_sec  = t;
      tval[1].tv_usec = 0;
      utimes(f, tval);
#else
      times.actime = fbuf.st_atime;
      times.modtime = t;
      utime (f, &times);
#endif
    }
}
