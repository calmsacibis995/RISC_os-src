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
#ident	"$Header: pwd.c,v 1.6.2.3 90/05/09 18:59:32 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX shell
 *
 *	DAG -- Two ways of handling symbolic links in directory paths:
 *		Define SYMLINK if you want "cd .." to do just that.
 *		Otherwise, "cd .." takes you back the way you came.
 *	The first method was implemented by Ron Natalie and the
 *	second method was implemented by Douglas Gwyn, both of BRL.
 */

#include		"defs.h"	/* DAG -- was just "mac.h" */
#include	<sys/types.h>
#ifndef BSD_SYS
#include	<dirent.h>
#endif BSD_SYS
#include	<sys/stat.h>

#if defined(SYMLINK) && !defined(S_IFLNK)
#define	S_IFLNK	0120000
#endif

#define	DOT		'.'
#define	SLASH	'/'
#if defined(RISCOS) || defined(BSD_SYS) || defined(BRL) && !defined(pdp11) 
#define MAXPWD	(24*256)	/* who knows */
#else
#define MAXPWD	512
#endif

#ifdef RISCOS
#ifdef MAXPATHLEN
#undef MAXPWD
#define MAXPWD MAXPATHLEN
#endif MAXPATHLEN
#if MAXPWD < 1024
#undef MAXPWD
#define MAXPWD 1024
#endif 
#endif RISCOS

extern char	longpwd[];

static char cwdname[MAXPWD + 1];
static int 	didpwd = FALSE;
#if SYMLINK
/* Stuff to handle chdirs around symbolic links */
static char	*symlink = 0;	/* Pointer to after last symlink in cwdname */
#endif

cwd(dir)
	register char *dir;
{
#if SYMLINK
	struct stat statb;
#endif
	register char *pcwd;
	register char *pdir;

	/* First remove extra /'s */

	rmslash(dir);

	/* Now remove any .'s */

	pdir = dir;
	if(*dir == SLASH)
	  	pdir++;
	while(*pdir) 			/* remove /./ by itself */
	{
		if((*pdir==DOT) && (*(pdir+1)==SLASH))
		{
			movstr(pdir+2, pdir);
			continue;
		}
		pdir++;
		while ((*pdir) && (*pdir != SLASH)) 
			pdir++;
		if (*pdir) 
			pdir++;
	}

	/* take care of trailing /. */
	if(*(--pdir)==DOT && pdir > dir && *(--pdir)==SLASH) {
		if(pdir > dir) {
			*pdir = NULL;
		} else {
			*(pdir+1) = NULL;
		}
	}

	/* Remove extra /'s */

	rmslash(dir);

	/* Now that the dir is canonicalized, process it */

	if(*dir==DOT && *(dir+1)==NULL)
	{
		return;
	}

	if(*dir==SLASH)
	{
		/* Absolute path */

		pcwd = cwdname;
	 	if (pcwd >= &cwdname[MAXPWD])
		{
			didpwd=FALSE;
			return;
		}
		*pcwd++ = *dir++;
		didpwd = TRUE;
#if SYMLINK
		symlink = 0;		/* Starting over, no links yet */
#endif
	}
	else
	{
		/* Relative path */

		if (didpwd == FALSE) 
#if !defined(SYMLINK) && (defined(RISCOS) || defined(BSD_SYS) || defined(BRL) && !defined(pdp11))
			pwd();		/* Get absolute pathname into cwdname[] */
#else
			return;
#endif
			
		pcwd = cwdname + length(cwdname) - 1;
		if(pcwd != cwdname+1)
		{
			*pcwd++ = SLASH;
		}
	}
	while(*dir)
	{
		if(*dir==DOT && 
		   *(dir+1)==DOT &&
		   (*(dir+2)==SLASH || *(dir+2)==NULL))
		{
			/* Parent directory, so backup one */

			if( pcwd > cwdname+2 )
				--pcwd;
			while(*(--pcwd) != SLASH)
				;
			pcwd++;
			dir += 2;
#if SYMLINK
			/* Just undid symlink, so pwd the hard way */
			if(pcwd < symlink)
			{
				pwd();
				return;
			}
#endif
			if(*dir==SLASH)
			{
				dir++;
			}
			continue;
		}
	 	if (pcwd >= &cwdname[MAXPWD])
		{
			didpwd=FALSE;
			return;
		}
		*pcwd++ = *dir++;
		while((*dir) && (*dir != SLASH))
		{  
	 		if (pcwd >= &cwdname[MAXPWD])
			{
				didpwd=FALSE;
				return;
			}
			*pcwd++ = *dir++;
		} 
#if SYMLINK
		/* Check to see if this path component is a symbolic link */
		*pcwd = 0;
		if(lstat(cwdname, &statb) != 0)
		{
			prs(cwdname);	/* DAG */
			error(nolstat);	/* DAG -- made string sharable */
			pwd();
			return;
		}
		if((statb.st_mode & S_IFMT) == S_IFLNK)
			/* Set fence so that when we attempt to
			 * "cd .." past it, we know that it is invalid
			 * and have to do it the hard way
			 */
			symlink = dir;
#endif
		if (*dir) 
		{
	 		if (pcwd >= &cwdname[MAXPWD])
			{
			didpwd=FALSE;
			return;
			}
			*pcwd++ = *dir++;
		}
	}
	if (pcwd >= &cwdname[MAXPWD])
	{
		didpwd=FALSE;
		return;
	}
	*pcwd = '\0';	/* DAG -- not NULL */

	--pcwd;
	if(pcwd>cwdname && *pcwd==SLASH)
	{
		/* Remove trailing / */

		*pcwd = '\0';	/* DAG -- not NULL */
	}
	return;
}

/*
 *	Print the current working directory.
 */

cwdprint()
{
	if (didpwd == FALSE)
		pwd();

	prs_buff(cwdname);
	prc_buff(NL);
	return;
}

/*
 *	This routine will remove repeated slashes from string.
 */

static
rmslash(string)
	char *string;
{
	register char *pstring;

	pstring = string;
	while(*pstring)
	{
		if(*pstring==SLASH && *(pstring+1)==SLASH)
		{
			/* Remove repeated SLASH's */

			movstr(pstring+1, pstring);
			continue;
		}
		pstring++;
	}

	--pstring;
	if(pstring>string && *pstring==SLASH)
	{
		/* Remove trailing / */

		*pstring = NULL;
	}
	return;
}

/*
 *	Find the current directory the hard way.
 */

#ifdef RISCOS

extern char *getwd();

static
pwd()
{
	char	*path;
	char	error_msg[MAXPWD + 20];

	path = getwd(cwdname);
	if (path == NULL) {
		  sprintf(error_msg,"sh error: %s",cwdname);
		  strcpy(cwdname,".");
		  error(error_msg);
	};
}

#else RISCOS

#if defined(BRL) && !defined(BSD_SYS)
#include	<dir.h>
#else
#ifdef BSD_SYS
#include	<sys/dir.h>
#endif
#endif


static char dotdots[] =
"../../../../../../../../../../../../../../../../../../../../../../../..";


static
pwd()
{
	struct stat		cdir;	/* current directory status */
	struct stat		tdir;
	struct stat		pdir;	/* parent directory status */
	DIR			*pdfd;	/* parent directory stream */

#if ! (BSD_SYS || BRL)
	struct dirent	*dir;
#else 
	struct direct	*dir;
#endif 
	char 			*dot = dotdots + sizeof(dotdots) - 3;
	int				index = sizeof(dotdots) - 2;
	int				cwdindex = MAXPWD - 1;
	int 			i;
	
#if SYMLINK
	symlink = 0;			/* Starting over, no links yet */
#endif
	cwdname[cwdindex] = 0;
	dotdots[index] = 0;

	if(stat(dot, &pdir) < 0)
	{
		error("pwd: cannot stat .");
	}

	dotdots[index] = '.';

	for(;;)
	{
		cdir = pdir;
		if ((pdfd = opendir(dot)) == (DIR *)0)
		{
			error("pwd: cannot open ..");
		}

		if(fstat(pdfd->dd_fd, &pdir) < 0)
		{
			(void)closedir(pdfd);
			error("pwd: cannot stat ..");
		}

		if(cdir.st_dev == pdir.st_dev)
		{
			if(cdir.st_ino == pdir.st_ino)
			{
#if BRL && pdp11
				/* JHU/BRL PDP-11 UNIX also "tops out" at
				   the root of a mounted filesystem, alas */
				register int	mt;	/* mount table fd */
#endif

				didpwd = TRUE;
				(void)closedir(pdfd);
#if BRL && pdp11
				/* find parent by scanning mount table */

				if ( (mt = _open( "/etc/mtab", 0 )) >= 0 )
					{
#include	<mnttab.h>
					struct mnttab	mtb;	/* table entry */

					while ( _read( mt, (char *)&mtb, sizeof mtb )
						== sizeof mtb
					      )
						if ( mtb.mt_filsys[0] != '\0'
						  && stat( mtb.mt_filsys, &pdir ) == 0
						  && pdir.st_dev == cdir.st_dev
						  && pdir.st_ino == cdir.st_ino
						   )	/* found parent entry */
							{
							for ( i = 0; ; ++i )
								if ( mtb.mt_filsys[i]
								     == '\0'
								   )
									break;

							if ( i > cwdindex - 1 )
								error( longpwd );
							else	{
								cwdindex -= i;
								movstrn( mtb.mt_filsys,
								    &cwdname[cwdindex],
									i
								       );
								break;
								}
							}
					/* if no matching entry, must be root */
					(void)close( mt );
					}
				/* else no /etc/mtab; must be single-user root */
#endif	/* BRL && pdp11 */
				if (cwdindex == (MAXPWD - 1))
					cwdname[--cwdindex] = SLASH;

				movstr(&cwdname[cwdindex], cwdname);
				return;
			}

			do
			{
				if ((dir = readdir(pdfd)) == 0)
				{
					(void)closedir(pdfd);
					error("pwd: read error in ..");
				}
			}
			while (dir->d_ino != cdir.st_ino);
		}
#if !defined(BRL) || !defined(pdp11)
		else
		{
			char name[256+MAXPWD];	/* DAG -- (was 512) */
			
			movstr(dot, name);
			i = length(name) - 1;

			name[i++] = '/';

			tdir.st_dev = pdir.st_dev;	/* DAG -- (safety) */
			do
			{
				if ((dir = readdir(pdfd)) == 0)
				{
					(void)closedir(pdfd);
					error("pwd: read error in ..");
				}
				movstr(dir->d_name, &name[i]);
				stat(name, &tdir);
			}		
			while(tdir.st_ino != cdir.st_ino || tdir.st_dev != cdir.st_dev);
		}
#endif
		(void)closedir(pdfd);

#if (BSD_SYS || BRL) && ! defined(RISCOS)
		i = dir->d_namlen;
#else
		for (i = 0; i < DIRSIZ; i++)
			if (dir->d_name[i] == 0)
				break;
#endif

		if (i > cwdindex - 1)
				error(longpwd);
		else
		{
			cwdindex -= i;
			movstrn(dir->d_name, &cwdname[cwdindex], i);
			cwdname[--cwdindex] = SLASH;
		}

		dot -= 3;
		if (dot<dotdots) 
			error(longpwd);
	}
}

#if !defined(SYMLINK) && (defined(BSD_SYS) || defined(BRL) && !defined(pdp11))

/* The following chdir() interface is used to outwit symbolic links. */

int
cwdir( dir )				/* attempt a full-pathname chdir */
	char		*dir;		/* name to feed to chdir() */
	{
	char		savname[MAXPWD];	/* saved cwdname[] */
	register int	retval;		/* chdir() return value */

	(void)movstr( cwdname, savname );	/* save current name */

	cwd( dir );			/* adjust cwdname[] */

	if ( (retval = chdir( cwdname )) < 0 )
		(void)movstr( savname, cwdname );	/* restore name */

	return retval;
	}
#endif
#endif RISCOS

#ifdef TILDE_SUB
char *
retcwd()
	{
	if ( didpwd == FALSE )
		pwd();		/* Get absolute pathname into cwdname[] */
	return cwdname;
	}
#endif
