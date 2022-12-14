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
#ident	"$Header: ls.c,v 1.12.2.2.1.2 90/08/06 14:49:24 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
* 	list file or directory;
* 	define DOTSUP to suppress listing of files beginning with dot
*/

#include	<ctype.h>
#include	<sys/param.h>
#include	<sys/types.h>
#include	<sys/sysmacros.h>
#include	<sys/stat.h>
#include	<stdio.h>
#include	<dirent.h>

# ifdef S_IFLNK
#include	<sys/nami.h>
# endif

#ifndef STANDALONE
#define TERMINFO
#endif

/* -DNOTERMINFO can be defined on the cc command line to prevent
 * the use of terminfo.  This should be done on systems not having
 * the terminfo feature (pre 6.0 sytems ?).
 * As a result, columnar listings assume 80 columns for output,
 * unless told otherwise via the COLUMNS environment variable.
 */
#ifdef NOTERMINFO
#undef TERMINFO
#endif

#ifdef TERMINFO
#include	<curses.h>
#include	<term.h>
#endif

#if defined(VIS)
#include <grp.h>
#include <pwd.h>
#endif

#define	DOTSUP	1
#define ISARG   0100000 /* this bit equals 1 in lflags of structure lbuf 
                        *  if *namep is to be used;
                        */
#define DIRECT	10	/* Number of direct blocks */

#ifdef u370
	/* Number of pointers in an indirect block */
#define INDIR	(NBPSCTR/sizeof(daddr_t))
/* Number of right shifts to divide by INDIR */
#define INSHFT	10
#else
	/* Number of pointers in an indirect block */
#define INDIR	128
	/* Number of right shifts to divide by INDIR */
#define INSHFT	7
#endif

/*	Date and time	formats   */
#define FORMAT1	 " %b %e  %Y "
#define FORMAT2  " %b %e %H:%M "
					  /*
 					   * b --- abbreviated month name
					   e --- day number
					   Y --- year in the form ccyy
					   H --- hour (24 hour version)
					   M --- minute   */


struct	lbuf	{
	union	{
		char	lname[MAXNAMLEN];   /* used for filename in a directory */
		char	*namep;          /* for name in ls-command; */
	} ln;
	char    *linktext;      /* for symbolic links */
	char	ltype;  	/* filetype */
	ino_t	lnum;   	/* inode number of file */
	short	lflags; 	/* 0777 bits used as r,w,x permissions */
	short	lnl;    	/* number of links to file */
	unsigned short	luid;
	unsigned short	lgid;
	long	lsize;  	/* filesize or major/minor dev numbers */
	long	lmtime;
};

struct dchain {
	char *dc_name;		/* path name */
	struct dchain *dc_next;	/* next directory in the chain */
};

struct dchain *dfirst;	/* start of the dir chain */
struct dchain *cdfirst;	/* start of the durrent dir chain */
struct dchain *dtemp;	/* temporary - used for linking */
char *curdir;		/* the current directory */

int	nfiles = 0;	/* number of flist entries in current use */
int	nargs = 0;	/* number of flist entries used for arguments */
int	maxfils = 0;	/* number of flist/lbuf entries allocated */
int	maxn = 0;	/* number of flist entries with lbufs assigned */
int	quantn = 64;	/* allocation growth quantum */

struct	lbuf	*nxtlbf;	/* pointer to next lbuf to be assigned */
struct	lbuf	**flist;	/* pointer to list of lbuf pointers */
struct	lbuf	*gstat();

#if !defined(VIS)
FILE	*pwdfu, *pwdfg;
#endif

int	aflg, bflg, cflg, dflg, fflg, gflg, iflg, lflg, mflg;
int	nflg, oflg, pflg, qflg, sflg, tflg, uflg, xflg;
int	Cflg, Fflg, Rflg, Hflg, Lflg;
int	rflg = 1;   /* initialized to 1 for special use in compar() */
int	flags;
int	err = 0;	/* Contains return code */

char	*dmark;	/* Used if -p option active. Contains "/" or NULL. */

unsigned	lastuid	= -1, lastgid = -1;
int	statreq;    /* is > 0 if any of sflg, (n)lflg, tflg are on */

char	*dotp = ".";
char	*makename();
char	tbufu[16], tbufg[16];  /* assumed 15 = max. length of user/group name */

long	nblock();
long	tblocks;  /* total number of blocks of files in a directory */
long	year, now;

int	num_cols = 80;
int	colwidth;
int	filewidth;
int	fixedwidth;
int	curcol;
int	compar();

static char	time_buf[50];	/* array to hold day and time */

main(argc, argv)
int argc;
char *argv[];
{
	extern char	*optarg;
	extern int	optind;
	int	amino, opterr=0;
	int	c;
	register struct lbuf *ep;
	struct	lbuf	lb;
	int	i, width;
	long	time();
	char *malloc();
	void qsort(), exit();

#ifdef STANDALONE
	if (argv[0][0] == '\0')
		argc = getargv("ls", &argv, 0);
#endif

	lb.lmtime = time((long *) NULL);
	lb.linktext = 0;
	year = lb.lmtime - 6L*30L*24L*60L*60L; /* 6 months ago */
	now = lb.lmtime + 60;
	while ((c=getopt(argc, argv,
			"RadCLHxmnlogrtucpFbqisf")) != EOF) switch(c) {
		case 'R':
			Rflg++;
			statreq++;
			continue;
		case 'a':
			aflg++;
			continue;
		case 'd':
			dflg++;
			continue;
		case 'C':
			Cflg = 1;
			mflg = 0;
			continue;
		case 'x':
			xflg = 1;
			Cflg = 1;
			mflg = 0;
			continue;
		case 'm':
			Cflg = 0;
			mflg = 1;
			continue;
		case 'n':
			nflg++;
		case 'l':
			lflg++;
			statreq++;
			continue;
		case 'o':
			oflg++;
			lflg++;
			statreq++;
			continue;
		case 'g':
			gflg++;
			lflg++;
			statreq++;
			continue;
		case 'r':
			rflg = -1;
			continue;
		case 't':
			tflg++;
			statreq++;
			continue;
		case 'u':
			uflg++;
			continue;
		case 'c':
			cflg++;
			continue;
		case 'p':
			pflg++;
			statreq++;
			continue;
		case 'F':
			Fflg++;
			statreq++;
			continue;
		case 'b':
			bflg = 1;
			qflg = 0;
			continue;
		case 'q':
			qflg = 1;
			bflg = 0;
			continue;
		case 'i':
			iflg++;
			continue;
		case 's':
			sflg++;
			statreq++;
			continue;
		case 'f':
			fflg++;
			continue;
		case '?':
			opterr++;
			continue;
#ifdef	S_IFLNK
		case 'H':
			Hflg++;
			Lflg = 0;
			continue;

		case 'L':
			Lflg++;
			Hflg = 0;
			continue;
#endif	S_IFLNK

		}
	if(opterr) {
#ifdef	S_IFLNK
		fprintf(stderr,"usage: ls -RadCLHxmnlogrtucpFbqisf [files]\n");
#else	S_IFLNK

		fprintf(stderr,"usage: ls -RadCxmnlogrtucpFbqisf [files]\n");
#endif	S_IFLNK
		exit(2);
	}

	if (fflg) {
		aflg++;
		lflg = 0;
		sflg = 0;
		tflg = 0;
		statreq = 0;
	}

	fixedwidth = 2;
	if (pflg || Fflg)
		fixedwidth++;
	if (iflg)
		fixedwidth += 7;
	if (sflg)
		fixedwidth += 5;

	if (lflg) {
		if (!gflg && !oflg)
			gflg = oflg = 1;
		else
		if (gflg && oflg)
			gflg = oflg = 0;
		Cflg = mflg = 0;
#if !defined(VIS)
		if (oflg > 0)
			if ((pwdfu = fopen("/etc/passwd", "r")) == NULL) {
				fprintf(stderr,"%s file cannot be opened for reading\n","/etc/passwd");
				exit(2);
			}
		if (gflg > 0)
			if ((pwdfg = fopen("/etc/group", "r")) == NULL) {
				fprintf(stderr,"%s file cannot be opened for reading\n","/etc/group");
				exit(2);
			}
#endif /* !VIS */
	}

	if (Cflg || mflg) {
		char *getenv();
		char *clptr;
		if ((clptr = getenv("COLUMNS")) != NULL)
			num_cols = atoi(clptr);
#ifdef TERMINFO
		else {
			setupterm(0,1,&i); /* get term description */
			resetterm();	/* undo what setupterm changed */
			if (i == 1)
				num_cols = columns;
		}
#endif
		if (num_cols < 20 || num_cols > 160)
			/* assume it is an error */
			num_cols = 80;
	}

	/* allocate space for flist and the associated	*/
	/* data structures (lbufs)			*/
	maxfils = quantn;
	if((flist=(struct lbuf **)malloc((unsigned)(maxfils * sizeof(struct lbuf *)))) == NULL
	|| (nxtlbf = (struct lbuf *)malloc((unsigned)(quantn * sizeof(struct lbuf)))) == NULL) {
		fprintf(stderr, "ls: out of memory\n");
		exit(2);
	}
	if ((amino=(argc-optind))==0) { /* case when no names are given
					* in ls-command and current 
					* directory is to be used 
 					*/
		argv[optind] = dotp;
	}
	for (i=0; i < (amino ? amino : 1); i++) {
		if (Cflg || mflg) {
			width = strlen(argv[optind]);
			if (width > filewidth)
				filewidth = width;
		}
		if ((ep = gstat((*argv[optind] ? argv[optind] : dotp), 1))==NULL)
		{
			err = 2;
			optind++;
			continue;
		}
		ep->ln.namep = (*argv[optind] ? argv[optind] : dotp);
		ep->lflags |= ISARG;
		optind++;
		nargs++;	/* count good arguments stored in flist */
	}
	colwidth = fixedwidth + filewidth;
	qsort(flist, (unsigned)nargs, sizeof(struct lbuf *), compar);
	for (i=0; i<nargs; i++)
		if (flist[i]->ltype=='d' && dflg==0 || fflg)
			break;
	pem(&flist[0],&flist[i], 0);
	for (; i<nargs; i++) {
		pdirectory(flist[i]->ln.namep, (amino>1), nargs);
		/* -R: print subdirectories found */
		while (dfirst || cdfirst) {
			/* Place direct subdirs on front in right order */
			while (cdfirst) {
				/* reverse cdfirst onto front of dfirst */
				dtemp = cdfirst;
				cdfirst = cdfirst -> dc_next;
				dtemp -> dc_next = dfirst;
				dfirst = dtemp;
			}
			/* take off first dir on dfirst & print it */
			dtemp = dfirst;
			dfirst = dfirst->dc_next;
			pdirectory (dtemp->dc_name, 1, nargs);
			free (dtemp->dc_name);
			free ((char *)dtemp);
		}
	}
	exit(err);
	/*NOTREACHED*/
}

/*
 * pdirectory: print the directory name, labelling it if title is
 * nonzero, using lp as the place to start reading in the dir.
 */
pdirectory (name, title, lp)
char *name;
int title;
int lp;
{
	register struct dchain *dp;
	register struct lbuf *ap;
	register char *pname;
	register int j;

	filewidth = 0;
	curdir = name;
	if (title) {
		putc('\n', stdout);
		pprintf(name, ":");
		new_line();
	}
	nfiles = lp;
	rddir(name);
	if (fflg==0)
		qsort(&flist[lp],(unsigned)(nfiles - lp),sizeof(struct lbuf *),compar);
	if (Rflg) for (j = nfiles - 1; j >= lp; j--) {
		ap = flist[j];
		if (ap->ltype == 'd' && strcmp(ap->ln.lname, ".") &&
				strcmp(ap->ln.lname, "..")) {
			dp = (struct dchain *)calloc(1,sizeof(struct dchain));
			if (dp == NULL)
				fprintf(stderr,"ls: out of memory\n");
			pname = makename(curdir, ap->ln.lname);
			dp->dc_name = (char *)calloc(1,strlen(pname)+1);
			if (dp->dc_name == NULL) {
				fprintf(stderr,"ls: out of memory\n");
				free(dp);
			}
			else {
				strcpy(dp->dc_name, pname);
				dp -> dc_next = dfirst;
				dfirst = dp;
			}
		}
	}

/*
 * If you want the total to appear on a separate line, define
 * BSD_TOTAL. Otherwise, the total will be placed in the first
 * column of the first line.
 */

	if (lflg || sflg) {
#ifdef BSD_TOTAL
		printf("total %ld\n", tblocks);
#else
		curcol += printf("total %ld", tblocks);
#endif
	}
	pem(&flist[lp],&flist[nfiles],lflg||sflg);
}

/*
 * pem: print 'em.  Print a list of files (e.g. a directory) bounded
 * by slp and lp.
 */
pem(slp, lp, tot_flag)
	register struct lbuf **slp, **lp;
	int tot_flag;
{
	int ncols, nrows, row, col;
	register struct lbuf **ep;

	if (Cflg || mflg)
	{		/* don't let ncols = 0, it breaks ls */
			/* it occurs when len of filename > COLUMNS   */
		if ((ncols=num_cols/colwidth)==0)
			ncols = 1;
	}

	if (ncols == 1 || mflg || xflg || !Cflg) {
		for (ep = slp; ep < lp; ep++)
			pentry(*ep);
		new_line();
		return;
	}
	/* otherwise print -C columns */
#ifndef BSD_TOTAL
	if (tot_flag)
		slp--;
#endif
	nrows = (lp - slp - 1) / ncols + 1;
	for (row = 0; row < nrows; row++) {
#ifdef BSD_TOTAL
		col = 0;
#else
		col = (row == 0 && tot_flag);
#endif
		for (; col < ncols; col++) {
			ep = slp + (nrows * col) + row;
			if (ep < lp)
				pentry(*ep);
		}
		new_line();
	}
}

pentry(ap)  /* print one output entry;
            *  if uid/gid is not found in the appropriate
            *  file (passwd/group), then print uid/gid instead of 
            *  user/group name;
            */
struct lbuf *ap;
{
	struct	{
		char	dminor,
			dmajor;
	};
	register struct lbuf *p;
	register char *cp;

	p = ap;
	column();
	if (iflg)
		if (mflg && !lflg)
			curcol += printf("%u ", p->lnum);
		else
			curcol += printf("%6u ", p->lnum);
	if (sflg)
		curcol += printf( (mflg && !lflg) ? "%ld " : "%4ld " ,
			(p->ltype != 'b' && p->ltype != 'c') ?
				nblock(p->lsize) : 0L );
	if (lflg) {
		putchar(p->ltype);
		curcol++;
		pmode(p->lflags);
		curcol += printf("%4d ", p->lnl);
		if (oflg)
			if(!nflg && getname(p->luid, tbufu, 0)==0)
				curcol += printf("%-9.9s", tbufu);
			else
				curcol += printf("%-9u", p->luid);
		if (gflg)
			if(!nflg && getname(p->lgid, tbufg, 1)==0)
				curcol += printf("%-9.9s", tbufg);
			else
				curcol += printf("%-9u", p->lgid);
		if (p->ltype=='b' || p->ltype=='c')
			curcol += printf("%3d,%3d", major((int)p->lsize), minor((int)p->lsize));
		else
			curcol += printf("%7ld", p->lsize);
		if((p->lmtime < year) || (p->lmtime > now))
			{
			cftime(time_buf, FORMAT1, &p->lmtime);
			curcol += printf("%s", time_buf);
			}
		else
			{
			cftime(time_buf, FORMAT2, &p->lmtime);
			curcol += printf("%s", time_buf);
			}
	}
	if ((pflg || Fflg) && p->ltype == 'd')
		dmark = "/";
	else {
	   dmark = "";
	   if (Fflg) {
		if (p->lflags & 0111)
		dmark = "*";
		else if (p->ltype == 'l')
			dmark = "@";

	   }
	}
	if (p->lflags & ISARG) {
		if (qflg || bflg)
			pprintf(p->ln.namep,dmark);
		else
			curcol += printf("%s%s",p->ln.namep,dmark);
	}
	else {
		if (qflg || bflg)
			pprintf(p->ln.lname,dmark);
		else
			curcol += printf("%s%s",p->ln.lname,dmark);
	}
#ifdef S_IFLNK
	if (lflg && ap->linktext != 0) {
		printf(" -> %s", ap->linktext);
		free (ap->linktext);
	}
#endif 
}

/* print various r,w,x permissions 
 */
pmode(aflag)
{
        /* these arrays are declared static to allow initializations */
	static int	m0[] = { 1, S_IREAD>>0, 'r', '-' };
	static int	m1[] = { 1, S_IWRITE>>0, 'w', '-' };
	static int	m2[] = { 3, S_ISUID|S_IEXEC, 's', S_IEXEC, 'x', S_ISUID, 'S', '-' };
	static int	m3[] = { 1, S_IREAD>>3, 'r', '-' };
	static int	m4[] = { 1, S_IWRITE>>3, 'w', '-' };
	static int	m5[] = { 3, S_ISGID|(S_IEXEC>>3),'s', S_IEXEC>>3,'x', S_ISGID,'l', '-'};
	static int	m6[] = { 1, S_IREAD>>6, 'r', '-' };
	static int	m7[] = { 1, S_IWRITE>>6, 'w', '-' };
	static int	m8[] = { 3, S_ISVTX|(S_IEXEC>>6),'t', S_IEXEC>>6,'x', S_ISVTX,'T', '-'};

        static int  *m[] = { m0, m1, m2, m3, m4, m5, m6, m7, m8};

	register int **mp;

	flags = aflag;
	for (mp = &m[0]; mp < &m[sizeof(m)/sizeof(m[0])];)
		select(*mp++);
}

select(pairp)
register int *pairp;
{
	register int n;

	n = *pairp++;
	while (n-->0) {
		if((flags & *pairp) == *pairp) {
			pairp++;
			break;
		}else {
			pairp += 2;
		}
	}
	putchar(*pairp);
	curcol++;
}

/*
 * column: get to the beginning of the next column.
 */
column()
{

	if (curcol == 0)
		return;
	if (mflg) {
		putc(',', stdout);
		curcol++;
		if (curcol + colwidth + 2 > num_cols) {
			putc('\n', stdout);
			curcol = 0;
			return;
		}
		putc(' ', stdout);
		curcol++;
		return;
	}
	if (Cflg == 0) {
		putc('\n', stdout);
		curcol = 0;
		return;
	}
	if ((curcol / colwidth + 2) * colwidth > num_cols) {
		putc('\n', stdout);
		curcol = 0;
		return;
	}
	do {
		putc(' ', stdout);
		curcol++;
	} while (curcol % colwidth);
}

new_line()
{
	if (curcol) {
		putc('\n',stdout);
		curcol = 0;
	}
}

/* read each filename in directory dir and store its
 *  status in flist[nfiles] 
 *  use makename() to form pathname dir/filename;
 */
rddir(dir)
char *dir;
{
	struct dirent *dentry;
	DIR *dirf;
	register int j;
	register struct lbuf *ep;
	register int width;

	if ((dirf = opendir(dir)) == NULL) {
		fflush(stdout);
		fprintf(stderr, "can not access directory %s\n", dir);
		err = 2;
		return;
	}
        else {
          	tblocks = 0;
		while (dentry = readdir(dirf)) {
          		if (aflg==0 && dentry->d_name[0]=='.' 
# ifndef DOTSUP
          			&& (dentry->d_name[1]=='\0' || dentry->d_name[1]=='.'
          			&& dentry->d_name[2]=='\0')
# endif
          			)  /* check for directory items '.', '..', 
                                   *  and items without valid inode-number;
                                   */
          			continue;
			if (Cflg || mflg) {
				width = strlen(dentry->d_name);
				if (width > filewidth)
					filewidth = width;
			}
          		ep = gstat(makename(dir, dentry->d_name), 0);
          		if (ep==NULL)
          			continue;
                        else {
          		     ep->lnum = dentry->d_ino;
			     for (j=0; dentry->d_name[j] != '\0'; j++)
          		         ep->ln.lname[j] = dentry->d_name[j];
			     ep->ln.lname[j] = '\0';
                        }
          	}
          	(void) closedir(dirf);
		colwidth = fixedwidth + filewidth;
	}
}

/* get status of file and recomputes tblocks;
 * argfl = 1 if file is a name in ls-command and  = 0
 * for filename in a directory whose name is an
 * argument in the command;
 * stores a pointer in flist[nfiles] and
 * returns that pointer;
 * returns NULL if failed;
 */
struct lbuf *
gstat(file, argfl)
char *file;
{
	struct stat statb;
	register struct lbuf *rep;
	static int nomocore;
	char *malloc(), *realloc();
#ifdef S_IFLNK
	/* for symbolic link text */
	char *cbuf;
#endif 

	if (nomocore)
		return(NULL);
	else if (nfiles >= maxfils) { 
/* all flist/lbuf pair assigned files time to get some more space */
		maxfils += quantn;
		if((flist=(struct lbuf **)realloc((char *)flist, (unsigned)(maxfils * sizeof(struct lbuf *)))) == NULL
		|| (nxtlbf = (struct lbuf *)malloc((unsigned)(quantn * sizeof(struct lbuf)))) == NULL) {
			fprintf(stderr, "ls: out of memory\n");
			nomocore = 1;
			return(NULL);
		}
	}

/* nfiles is reset to nargs for each directory
 * that is given as an argument maxn is checked
 * to prevent the assignment of an lbuf to a flist entry
 * that already has one assigned.
 */
	if(nfiles >= maxn) {
		rep = nxtlbf++;
		flist[nfiles++] = rep;
		maxn = nfiles;
	}else {
		rep = flist[nfiles++];
	}
	rep->lflags = 0;
	rep->linktext = 0;
	if (argfl || statreq) {
		if (lstat(file, &statb)<0) {
			perror(file);
			nfiles--;
			return(NULL);
		}
                else {
#ifdef	S_IFLNK
		if (!Hflg && (statb.st_mode&S_IFMT) == S_IFLNK) {
			struct stat nstatb;

			if (stat(file,&nstatb) == 0)
				if (Lflg || (nstatb.st_mode&S_IFMT) == S_IFDIR
				  && !lflg)
					statb = nstatb;
		}
#endif	S_IFLNK

	            	rep->lnum = statb.st_ino;
	            	rep->lsize = statb.st_size;
	            	switch(statb.st_mode&S_IFMT) {
		
#if	RISCOS
			case S_IFSOCK:
				rep->ltype = 's';
				break;
#endif	RISCOS
	            	case S_IFDIR:
	            		rep->ltype = 'd';
	            		break;

	            	case S_IFBLK:
	            		rep->ltype = 'b';
	            		rep->lsize = statb.st_rdev;
	            		break;

	            	case S_IFCHR:
	            		rep->ltype = 'c';
	            		rep->lsize = statb.st_rdev;
	            		break;

	            	case S_IFIFO:
                 		rep->ltype = 'p';
                 		break;
#ifdef	S_IFLNK
			case S_IFLNK:
				rep->ltype = 'l';
				if (lflg) {
				   int readerr;
				   if ((cbuf= (char *) calloc (1,MAXPATHLEN))==NULL) {
				        fprintf(stderr, "ls: out of memory\n");
				        nomocore = 1;
				        return(NULL);
				   }
				   readerr = readlink(file,cbuf,MAXPATHLEN);
				   if (readerr > 0) {
					    cbuf[readerr] = NULL;
					    rep->linktext = cbuf;
				   }
				}
				break;
#endif	S_IFLNK

                        default:
                                rep->ltype = '-';
                 	}
	          	rep->lflags = statb.st_mode & ~S_IFMT;
                                    /* mask ISARG and other file-type bits */
	          	rep->luid = statb.st_uid;
	          	rep->lgid = statb.st_gid;
	          	rep->lnl = statb.st_nlink;
	          	if(uflg)
	          		rep->lmtime = statb.st_atime;
	          	else if (cflg)
	          		rep->lmtime = statb.st_ctime;
	          	else
	          		rep->lmtime = statb.st_mtime;
                        if (rep->ltype != 'b' && rep->ltype != 'c')
	          	   tblocks += nblock(statb.st_size);
                }
	}
        return(rep);
}

long nblock(size)
long size;
{
	long blocks, tot;

	blocks = tot = (size + NBPSCTR - 1) >> SCTRSHFT;
	if(blocks > DIRECT)
		tot += ((blocks - DIRECT - 1) >> INSHFT) + 1;
	if(blocks > DIRECT + INDIR)
		tot += ((blocks - DIRECT - INDIR - 1) >> (INSHFT * 2)) + 1;
	if(blocks > DIRECT + INDIR + INDIR*INDIR)
		tot++;
	return(tot);
}

/* returns pathname of the form dir/file;
 *  dir is a null-terminated string;
 */
char *
makename(dir, file) 
char *dir, *file;
{
	static char dfile[MAXNAMLEN];  /*  Maximum length of a
                                        *  file/dir name in ls-command;
                                        *  dfile is static as this is returned
                                        *  by makename();
                                        */
	register char *dp, *fp;
	register int i;

	dp = dfile;
	fp = dir;
	while (*fp)
		*dp++ = *fp++;
	if (dp > dfile && *(dp - 1) != '/')
		*dp++ = '/';
	fp = file;
	for (i=0; *fp != '\0'; i++)
		*dp++ = *fp++;
	*dp = '\0';
	return(dfile);
}

/* get name from passwd/group file for a given uid/gid
 *  and store it in buf; lastuid is set to uid;
 *  returns -1 if uid is not in file
 */
getname(uid, buf, type)
unsigned uid;
int type;
char buf[];
{
        int c;
        register i, j, n;

	if (uid==(type ? lastgid : lastuid))
		return(0);
#if defined(VIS)
	if (type) {
		struct group *grp, *getgrgid();

		lastgid = -1;
		if ((grp = getgrgid(uid)) == NULL)
			return (-1);
		strcpy(buf, grp->gr_name);
		lastgid = uid;
	} else {
		struct passwd *pwd, *getpwuid();

		lastuid = -1;
		if ((pwd = getpwuid(uid)) == NULL)
			return (-1);
		strcpy(buf, pwd->pw_name);
		lastuid = uid;
	}
#else
	rewind(type ? pwdfg : pwdfu);

	if(type)
		lastgid = -1;
	else	lastuid = -1;
	do {
		i = 0;
		j = 0;
		n = 0;
		while((c=fgetc(type ? pwdfg : pwdfu)) != '\n') {
			/* '\n' indicates end of 
			 *  a per user/group record
			 *  in passwd/group file;
			 */
		    if (c==EOF)
			return(-1);  
		    else if (c==':') j++;
		    	else if (j==0) buf[i++] = c;
				else if (j==2)
					n = n*10 + (c-'0');
		}
	} while (n != uid);
	buf[i] = '\0';
	if (type)
		lastgid = uid;
	else	lastuid = uid;
#endif /* VIS */
	return(0);
}

compar(pp1, pp2)  /* return >0 if item pointed by pp2 should appear first */
struct lbuf **pp1, **pp2;
{
	register struct lbuf *p1, *p2;

	p1 = *pp1;
	p2 = *pp2;
	if (dflg==0) {
/* compare two names in ls-command one of which is file
 *  and the other is a directory;
 *  this portion is not used for comparing files within
 *  a directory name of ls-command;
 */
		if (p1->lflags&ISARG && p1->ltype=='d') {
			if (!(p2->lflags&ISARG && p2->ltype=='d'))
				return(1);
                }
                else {
			if (p2->lflags&ISARG && p2->ltype=='d')
				return(-1);
		}
	}
	if (tflg) {
		if(p2->lmtime == p1->lmtime)
			return(0);
		else if(p2->lmtime > p1->lmtime)
			     return(rflg);
		else return(-rflg);
	}
        else
             return(rflg * strcmp(p1->lflags&ISARG? p1->ln.namep: p1->ln.lname,
				p2->lflags&ISARG? p2->ln.namep: p2->ln.lname));
}

pprintf(s1,s2)
	char *s1, *s2;
{
	register char *s;
	register int   c;
	register int  cc;
	int i;

	for (s = s1, i = 0; i < 2; i++, s = s2)
		while(c = *s++) {
			if ( ! isprint(c) ) {
				if (qflg)
					c = '?';
				else if (bflg) {
					curcol += 3;
					putc ('\\', stdout);
					cc = '0' + (c>>6 & 07);
					putc (cc, stdout);
					cc = '0' + (c>>3 & 07);
					putc (cc, stdout);
					c = '0' + (c & 07);
				}
			}
			curcol++;
			putc(c, stdout);
		}
}
