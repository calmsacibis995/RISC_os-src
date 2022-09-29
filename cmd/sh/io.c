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
#ident	"$Header: io.c,v 1.7.2.2 90/05/09 18:58:01 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * UNIX shell
 */

#include	"defs.h"
#ifdef RES	/* DAG -- conditionalize */
#include	"dup.h"
#endif
#if defined(RISCOS) || defined(RES)
#include	<sys/types.h>
#include	<sys/stat.h>
#endif
#include	<fcntl.h>

short topfd;

/* ========	input output and file copying ======== */

initf(fd)
int	fd;
{
	register struct fileblk *f = standin;

	f->fdes = fd;
	f->fsiz = ((flags & oneflg) == 0 ? BUFSIZ : 1);
	f->fnxt = f->fend = f->fbuf;
	f->feval = 0;
	f->flin = 1;
	f->feof = FALSE;
}

estabf(s)
register char *s;
{
	register struct fileblk *f;

	(f = standin)->fdes = -1;
	f->fend = length(s) + (f->fnxt = s);
	f->flin = 1;
	return(f->feof = (s == 0));
}

push(af)
struct fileblk *af;
{
	register struct fileblk *f;

	(f = af)->fstak = standin;
	f->feof = 0;
	f->feval = 0;
	standin = f;
}

pop()
{
	register struct fileblk *f;

	f = standin;
	if (f &&
	    f->fstak) {
		if (f->fdes >= 0)
			close(f->fdes);
		standin = f->fstak;
		return(TRUE);
	}
	else
		return(FALSE);
}

struct tempblk *tmpfptr;

pushtemp(fd,tb)
	int fd;
	struct tempblk *tb;
{
	tb->fdes = fd;
	tb->fstak = tmpfptr;
	tmpfptr = tb;
}

poptemp()
{
	if (tmpfptr)
	{
		close(tmpfptr->fdes);
		tmpfptr = tmpfptr->fstak;
		return(TRUE);
	}
	else
		return(FALSE);
}
	
chkpipe(pv)
int	*pv;
{
	if (pipe(pv) < 0 || pv[INPIPE] < 0 || pv[OTPIPE] < 0)
		error(piperr);
}

chkopen(idf)
char *idf;
{
	register int	rc;

	if ((rc = open(idf, 0)) < 0)
		failed(idf, badopen);
		/*NOTREACHED*/
	else
		return(rc);
#ifdef gould
	return 0;	/* DAG -- added */
#endif
}

rename(f1, f2)
register int	f1, f2;
{
#if defined(RES) || defined(RISCOS)
	if (f1 != f2)
	{
#ifdef RISCOS
		dup2(f1,f2);
#else RISCOS
		dup(f1 | DUPFLG, f2);
#endif RISCOS
		close(f1);
		if (f2 == 0)
			ioset |= 1;
	}
#else
	int	fs;

	if (f1 != f2)
	{
		fs = fcntl(f2, F_GETFD, 0);	/* DAG -- use defines */
		close(f2);
		fcntl(f1, F_DUPFD, f2);	/* DAG */
		close(f1);
		if (fs == 1)
			fcntl(f2, F_SETFD, 1);	/* DAG */
		if (f2 == 0)
			ioset |= 1;
	}
#endif
}

create(s)
char *s;
{
	register int	rc;

	if ((rc = creat(s, 0666)) < 0)
		failed(s, badcreate);
		/*NOTREACHED*/
	else
		return(rc);
#ifdef gould
	return 0;	/* DAG -- added */
#endif
}

tmpfil(tb)
	struct tempblk *tb;
{
	int fd;

	itos(serial++);
	movstr(numbuf, tmpnam);
	fd = create(tmpout);
	pushtemp(fd,tb);
	return(fd);
}

/*
 * set by trim
 */
extern BOOL		nosubst;
#define			CPYSIZ		512

copy(ioparg)
struct ionod	*ioparg;
{
	register char	*cline;
	register char	*clinep;
	register struct ionod	*iop;
	char	c;
	char	*ends;
	char	*start;
	int		fd;
	int		i;
	int		stripflg;
	

	if (iop = ioparg)
	{
		struct tempblk tb;

		copy(iop->iolst);
		ends = mactrim(iop->ioname);
		stripflg = iop->iofile & IOSTRIP;
		if (nosubst)
			iop->iofile &= ~IODOC;
		fd = tmpfil(&tb);

		if (fndef)
			iop->ioname = make(tmpout);
		else
			iop->ioname = cpystak(tmpout);

		iop->iolst = iotemp;
		iotemp = iop;

		cline = clinep = start = locstak();
		if (stripflg)
		{
			iop->iofile &= ~IOSTRIP;
			while (*ends == '\t')
				ends++;
		}
		for (;;)
		{
			chkpr();
			if (nosubst)
			{
				c = readc();
				if (stripflg)
					while (c == '\t')
						c = readc();

				while (!eolchar(c))
				{
					*clinep++ = c;
					c = readc();
				}
			}
			else
			{
				c = nextc(*ends);
				if (stripflg)
					while (c == '\t')
						c = nextc(*ends);
				
				while (!eolchar(c))
				{
					*clinep++ = c;
					if (c == '\\')
						*clinep++ = readc();
					c = nextc(*ends);
				}
			}

			*clinep = 0;
			if (eof || eq(cline, ends))
			{
				if ((i = cline - start) > 0)
					write(fd, start, i);
				break;
			}
			else
				*clinep++ = NL;

			if ((i = clinep - start) < CPYSIZ)
				cline = clinep;
			else
			{
				write(fd, start, i);
				cline = clinep = start;
			}
		}

		poptemp();		/* pushed in tmpfil -- bug fix for problem
					   deleting in-line scripts */
	}
}


link_iodocs(i)
	struct ionod	*i;
{
	while(i)
	{
		free(i->iolink);

		itos(serial++);
		movstr(numbuf, tmpnam);
		i->iolink = make(tmpout);
		link(i->ioname, i->iolink);

		i = i->iolst;
	}
}


swap_iodoc_nm(i)
	struct ionod	*i;
{
	while(i)
	{
		free(i->ioname);
		i->ioname = i->iolink;
		i->iolink = 0;

		i = i->iolst;
	}
}


savefd(fd)
	int fd;
{
	register int	f;

#if defined(RES) || defined(RISCOS)
	for ( f = 10; f <= INIO; ++f )
		{
		struct stat	statb;

		if (fstat(f, &statb) == -1)	/* if already in use, try another */
			{
#ifdef RISCOS
			dup2(fd,f);
#else RISCOS
			dup(fd | DUPFLG, f);
#endif RISCOS
			return f;
			}
		}
	return -1;	/* no free file descriptors */
#else 
	f = fcntl(fd, F_DUPFD, 10);
	return(f);
#endif 
}


restore(last)
	register int	last;
{
	register int 	i;
	register int	dupfd;

	for (i = topfd - 1; i >= last; i--)
	{
		if ((dupfd = fdmap[i].dup_fd) > 0)
			rename(dupfd, fdmap[i].org_fd);
		else
			close(fdmap[i].org_fd);
	}
	topfd = last;
}
