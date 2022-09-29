/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */

static char rcs[]="$Header: fix.c,v 1.7.2.2.1.2 90/07/11 18:20:59 hawkes Exp $";


#include "comply.h"

dofixmode(targf, curmode, mode)
char *targf;
int curmode, mode;
{
  if ((curmode & S_IFMT) != (mode & S_IFMT))
    return(0); /* only fix permissions & suid for now */
  return (chmod(targf, mode) == 0);
}

#ifdef BSD
dofixowner(targf, owner)
char *targf;
int owner;
{
  return(chown(targf, owner, -1) == 0);
}

dofixgroup(targf, group)
char *targf;
int group;
{
  return(chown(targf, -1, group) == 0);
}
#else
dofixowner(targf, owner, group)
char *targf;
int owner;
int group;
{
  return(chown(targf, owner, group) == 0);
}
#endif

dofixsym(file, targ)
char *file, *targ;
{
  unlink(file); /* unlink the existing one, if it's there */
  if (symlink(targ, file) == -1)
    {
      sprintf("internal error: can't symlink(%s, %s), errno = %d",
        targ, file, errno);
      error(ERRTYPE_ERROR, emsg);
      return (0);
    }
  return(1);
}      

mksym(file, mode, owner, group, targ)
char *file;
int mode, owner, group;
char *targ;
{
  if (dofixsym(file, targ))  
    {
      /* We should never change the mode of a link because it actually
         changes the target of the link, not the link itself */

      /*
      chmod(file, mode);
      */
      chown(file, owner, group);
    }
}

mklink(file, targ)
char *file, *targ;
{
  struct stat sbuf;

  if (lstat(targ, &sbuf) == -1)
    {
      sprintf(emsg, "cannot link(%s, %s); target missing", targ, file);
      error(ERRTYPE_ERROR, emsg);
    }
  else
    link(targ, file);
}

domknew(targetfile, owner, group, mode, linktarg)
char *targetfile;
int owner, group, mode;
char *linktarg;
{
  switch (mode & S_IFMT)
    {
      case S_IFDIR: 
	mkdir(targetfile,mode);
#ifdef BSD
	dofixowner(targetfile, owner);
	dofixgroup(targetfile, group);
#else
	dofixowner(targetfile, owner, group);
#endif
	break;
      case S_IFLNK: 
	mksym(targetfile, mode, owner, group, linktarg);
	break;
      case S_IFREG:
	if (strlen(linktarg))
	  mklink(targetfile, linktarg);
	break;
      case S_IFCHR: /* <tbd> mknod */
      case S_IFBLK: /* <tbd> mknod */
	if (strlen(linktarg))
	  mklink(targetfile, linktarg);
	else
	  /* <tbd> mknod */ ;
	break;
      default: break;
    }
}
