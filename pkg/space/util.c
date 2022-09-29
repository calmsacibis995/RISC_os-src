/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserve.                             | */
/* --------------------------------------------------- */

static char rcs[]="$Header: util.c,v 2.1.1.3.1.2 90/07/11 18:25:17 hawkes Exp $";

#include "space.h"

error(etype, s)
int etype;
char *s;
{
  static char *typstr[] = { "warning", "error", "fatal"};
  char errstr[1024];

  sprintf(errstr, "%s: %s", typstr[etype], s);
  errmsg(errstr);
  if (etype == ERRTYPE_FATAL)
    {
      cleanup();
      exit(1);
    }
}

errmsg(s)
char *s;
{
  fprintf(stderr, "space: %s\n", s);
}

usage()
{
  errmsg(
  "usage: space sizefile ...");

  exit(1);
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

#define btofr(b, f) (((long)b / f) + ((long)b % f != 0 ? 1 : 0))

tallyreq(dev, size, file)
dev_t dev;
long size;
char *file;
{
  int fsind = fstabind(dev, file);

  fstab[fsind].req += btofr(size,fstab[fsind].frsize);
  fstab[fsind].ino_req++;
}

tallycred(dev, size, file)
dev_t dev;
long size;
char *file;
{
  int fsind = fstabind(dev, file);

  fstab[fsind].cred += btofr(size,fstab[fsind].frsize);
  fstab[fsind].ino_cred++;
}

fstabind(dev, file)
dev_t dev;
char *file;
{
  static int first = 1;
  struct mntent *fsent;
  struct stat sbuf;
  struct statfs sbuffs;
  int i;
  int foundfs = 0;

  if (first) { first = 0; nfs = 0; }

  for (i = 0; i < nfs; i++) if (fstab[i].dev == dev) break;

  if (i < nfs) return i;
 
  fstab[nfs].dev = dev;
  fstab[nfs].req = 0;
  fstab[nfs].cred = 0;
  fstab[nfs].ino_req = 0;
  fstab[nfs].ino_cred = 0;

  if ((fss = setmntent("/etc/mtab", "r")) == NULL)
    {
      sprintf(emsg, "setmntent failed, errno = %d", errno);
      error(ERRTYPE_FATAL, emsg);
    }

  do
    {
      if ((fsent = getmntent(fss)) == NULL)
	break;

#if defined(SYSTYPE_BSD43)
#if !defined(IS_RISCOS_FS_TYPE)
/* for release 4.50, this is correct */
#define IS_RISCOS_FS_TYPE(_type) \
	(strcmp(_type, "ffs") == 0 || \
	 strcmp(_type, "ufs") == 0 || \
	 strcmp(_type, MNTTYPE_43)  == 0)
#endif
      if (!IS_RISCOS_FS_TYPE(fsent->mnt_type))
#else
      if (strcmp(fsent->mnt_type, FSID_BFS) != 0)
#endif
	continue;

      if (stat(fsent->mnt_fsname, &sbuf) == -1)
	{
	  sprintf(emsg, "fstabind(): stat failed, errno = %d", errno);
	  error(ERRTYPE_FATAL, emsg);
	}

      if (((sbuf.st_mode & S_IFMT) == S_IFBLK) &&
           (sbuf.st_rdev == dev))
	foundfs = 1;
    }
      while (! foundfs);

    if (! foundfs)
      {
        sprintf(emsg,
          "fstabind(): couldn't find device file for dev = 0x%04x;", dev);
        error(ERRTYPE_ERROR, emsg);
        sprintf(emsg, "pathname: %s", file);
	error(ERRTYPE_FATAL, emsg);
      }

  if ((fstab[nfs].devpath = (char *)malloc(strlen(fsent->mnt_fsname)+1))
          == NULL)
    {
      sprintf(emsg, "fstabind(): malloc for name failed, errno = %d", errno);
      error(ERRTYPE_FATAL, emsg);
    }

  strcpy(fstab[nfs].devpath, fsent->mnt_fsname);

#if defined(SYSTYPE_BSD43)
  if (statfs(file, &sbuffs) == -1)
    {
      sprintf(emsg, "statfs failed, errno = %d", errno);
      error(ERRTYPE_FATAL, emsg);
    }
#else
  {
#define NFSTY 100
    int fsty;

    for (fsty = 1; fsty <= NFSTY; fsty++)
      if (statfs(fsent->mnt_fsname, &sbuffs, sizeof sbuffs, fsty) == -1)
	if (errno != EINVAL)
          {
            sprintf(emsg, "statfs failed, errno = %d", errno);
            error(ERRTYPE_FATAL, emsg);
          }
        else
	  continue;
      else
	break;
    if (fsty > NFSTY)
      {
        sprintf(emsg, "couldn't determine filesystem type for %s",
	  fsent->mnt_fsname);
	error(ERRTYPE_FATAL, emsg);
      }
  }
#endif
  
#if defined(DEBUG)
#if defined(SYSTYPE_BSD43)
  pstatfs(file, &sbuffs);
#else
  pstatfs(fsent->mnt_fsname, &sbuffs);
#endif
#endif

#if defined(SYSTYPE_BSD43)
  fstab[nfs].frsize = sbuffs.f_bsize;
  fstab[nfs].free = sbuffs.f_bfree;
  fstab[nfs].blocks = sbuffs.f_blocks;
#else
  fstab[nfs].frsize = sbuffs.f_frsize;
  fstab[nfs].free = btofr(sbuffs.f_bfree*512,fstab[nfs].frsize);
  fstab[nfs].blocks = btofr(sbuffs.f_blocks*512,fstab[nfs].frsize);
#endif
  fstab[nfs].ino_free = sbuffs.f_ffree;
  nfs++;
  return nfs-1;
}
  

char *dirname(p)
char *p;
{
  static char dpath[MAXFILPATH];
  char *s = p;
 
  strcpy(dpath, p);
  for (s = dpath; *s; s++);
  if (s != dpath) s--;
  for ( ; *s && *s != '/'; s--);
  *s = '\0';
  return dpath;
}

/*
** Given a pathname p, insure that all directories in the path exist,
** creating them where necessary. Keep a list of directories created,
** to allow them to be cleaned up when we're all done.
*/
insuredir(p)
char *p;
{
  struct stat sbuf;
  struct nondirnam *dp;
  char targetdir[MAXFILPATH];
 
  strcpy(targetdir, dirname(p));

  if (stat(targetdir,&sbuf) == -1)
    if (errno == ENOENT)
      {
        insuredir(targetdir);
	creatdir(targetdir, 0600);
      }
    else
      { 
	sprintf(emsg, "stat failed on %s, errno = %d", targetdir, errno);
	error(ERRTYPE_FATAL, emsg);
      }
  else
    if ((sbuf.st_mode & S_IFMT) != S_IFDIR)
      {
	/* oops, it's there, but it isn't a directory!... this gets ugly */

        if ((dp = (struct nondirnam*)malloc(sizeof(struct nondirnam)))
               == NULL)
          {
            sprintf(emsg, 
	      "insuredir(): malloc for dp failed, errno = %d", errno);
            error(ERRTYPE_FATAL, emsg);
          }

        if ((dp->oldname = (char *)malloc(strlen(p)+1))
               == NULL)
          {
            sprintf(emsg, 
	      "creatdir(): malloc for oldname failed, errno = %d", errno);
            error(ERRTYPE_FATAL, emsg);
	  }

        if ((dp->newname = (char *)malloc(strlen(p)+7))
               == NULL)
          {
            sprintf(emsg, 
	      "creatdir(): malloc for newname failed, errno = %d", errno);
            error(ERRTYPE_FATAL, emsg);
	  }

        strcpy(dp->oldname, targetdir);
        strcpy(dp->newname, targetdir);
	strcat(dp->newname, ":save:");
	dp->next = NULL;
	dp->prev = lastnondirname;
        if (nondirnames == NULL)
          nondirnames = dp;
        else
          lastnondirname->next = dp;
        lastnondirname = dp;
	if ((rename (dp->oldname, dp->newname)) == -1)
          {
            sprintf(emsg, 
	      "creatdir(): rename failed, errno = %d", errno);
            error(ERRTYPE_FATAL, emsg);
	  }
	creatdir(targetdir, 0600);
      }
}


creatdir(p, m)
char *p;
int m;
{
  struct dirnam *dp;

  if ((dp = (struct dirnam*)malloc(sizeof(struct dirnam)))
          == NULL)
    {
      sprintf(emsg, "creatdir(): malloc for dp failed, errno = %d", errno);
      error(ERRTYPE_FATAL, emsg);
    }

  if ((dp->name = (char *)malloc(strlen(p)+1))
          == NULL)
    {
      sprintf(emsg, "creatdir(): malloc for name failed, errno = %d", errno);
      error(ERRTYPE_FATAL, emsg);
    }

  strcpy(dp->name, p);
  dp->next = NULL;
  dp->prev = lastdirname;
  if (dirnames == NULL)
    dirnames = dp;
  else
    lastdirname->next = dp;
  lastdirname = dp;
  mkdir(p, m);

}
  

#if 0
wecreated(path)
char *path;
{
  struct dirnam *dp;

  for(dp = dirnames; dp != NULL; dp = dp->next)
    if (strcmp(dp->name,  path) == 0)
      return 1;

  return 0;
}
#endif

cleanup()
{
  struct dirnam *dp;
  struct nondirnam *ndp;
  endmntent(fss); /* just in case we got signal while it was open */

  if (strlen(touchfile) > 0)
    if (unlink(touchfile) == -1)
      {
	sprintf(emsg, "cleanup(): unlink failed on %s, errno = %d",
	  touchfile, errno);
	error(ERRTYPE_WARN, emsg);
      }

  for (dp = lastdirname; dp != NULL; dp = dp->prev)
    if (rmdir(dp->name) == -1)
      {
	sprintf(emsg, "cleanup(): rmdir failed on %s, errno = %d",
          dp->name, errno);
	error(ERRTYPE_WARN, emsg);
      }

  if (excode != 0)
    {
      for (ndp = lastnondirname; ndp != NULL; ndp = ndp->prev)
        if (rename(ndp->newname, ndp->oldname) == -1)
          {
	    sprintf(emsg, "cleanup(): rename failed on %s, errno = %d",
              ndp->newname, errno);
	    error(ERRTYPE_WARN, emsg);
          }
    }
  else
    {
      /* display messages about renamed directories */
      for (ndp = nondirnames; ndp != NULL; ndp = ndp->next)
	{
          sprintf(emsg, 
          "a non-directory file becomes a directory in the new package:");
	  error(ERRTYPE_WARN, emsg);
          sprintf(emsg, "  %s renamed to", ndp->oldname);
	  error(ERRTYPE_WARN, emsg);
          sprintf(emsg, "  %s", ndp->newname);
	  error(ERRTYPE_WARN, emsg);
	}
    }
}


#ifdef DEBUG
pstatfs(name, sfbp)
	char *name;
	register struct statfs *sfbp;
{
#if defined(SYSTYPE_BSD43)
	fprintf(stderr, "\
statfs(%s) = {\n\
	type %d,\n\
	bsize %ld,\n\
	blocks %ld,\n\
	bfree %ld,\n\
        bavail %ld,\n\
	files %ld,\n\
	ffree %ld,\n\
        fsid  ---\n\
}\n",
#else
	fprintf(stderr, "\
statfs(%s) = {\n\
	fstyp %d,\n\
	bsize %ld,\n\
	frsize %ld,\n\
	blocks %ld,\n\
	bfree %ld,\n\
	files %ld,\n\
	ffree %ld,\n\
	fname %.6s,\n\
	fpack %.6s\n\
}\n",
#endif
	    name,
#if defined(SYSTYPE_BSD43)
	    sfbp->f_type,
#else
	    sfbp->f_fstyp,
#endif
	    sfbp->f_bsize,
#if defined(SYSTYPE_BSD43)
#else
	    sfbp->f_frsize,
#endif
	    sfbp->f_blocks,
	    sfbp->f_bfree,
#if defined(SYSTYPE_BSD43)
	    sfbp->f_bavail,
#endif
	    sfbp->f_files,
#if defined(SYSTYPE_BSD43)
	    sfbp->f_ffree);
#else
	    sfbp->f_ffree,
	    sfbp->f_fname,
	    sfbp->f_fpack);
#endif
}
#endif

credited(ino)
ino_t ino;
{
  struct inocred *scan;
  for (scan = inocreds; scan && (scan->ino != ino); scan = scan->next) ;
  return (scan != NULL);
}
    

credit(ino)
ino_t ino;
{
  struct inocred *newcred;

  if ((newcred = (struct inocred *)malloc(sizeof(struct inocred))) == NULL)
    {
      sprintf(emsg, 
        "fstabind(): malloc for inocred failed, errno = %d", errno);
      error(ERRTYPE_FATAL, emsg);
    }

  newcred->ino = ino;
  if (inocreds == NULL)
    inocreds = newcred;
  if (lastinocred != NULL)
    lastinocred->next = newcred;  
  lastinocred = newcred;
}

