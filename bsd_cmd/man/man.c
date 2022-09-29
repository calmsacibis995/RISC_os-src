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
#ident	"$Header: man.c,v 1.10.1.4 90/05/07 18:52:07 wje Exp $"

#include <stdio.h>
#include <ctype.h>
#include <sgtty.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <signal.h>
#include <strings.h>

/*
 * man
 * link also to apropos and whatis
 * This version uses more for underlining and paging.
 */
#define	NROFFCAT "nroff -h -man"	/* for nroffing to cat file */
#define	NROFF	"nroff -man"		/* for nroffing to tty */
#define	MORE	"more -s"		/* paging filter */
#define	CAT_	"/bin/cat"		/* for when output is not a tty */
#define	CAT_S	"/bin/cat -s"		/* for '-' opt (no more) */

#define ALLMAN	"uapxl."	/* order to look through the manuals */
#define	ALLSECT	"1nl6823457pos"	/* order to look through sections */
#define	SECT1	"1nlos"		/* sections to look at if 1 is specified */
#define ALLMAN1	"uaxl."	/* order to look through section 1 manuals  */
#define	SUBSEC1	"cgmX"		/* subsections to try in section 1 */
#define SUBSEC2 "X"
#define	SUBSEC3	"sxmncfXry"
#define	SUBSEC4	"pfnX"
#define SUBSEC5 "X"
#define SUBSEC6 "X"
#define SUBSEC7 "pnXf"
#define	SUBSEC8	"cvX"

char	*bsd_prefixes[] = { "bsd_", "", "posix_", NULL };
char	*sysv_prefixes[] = { "", "bsd_", "posix_", NULL };
char	*posix_prefixes[] = { "posix_", "", "bsd_", NULL };

#ifdef MAN_SYSV_MODE
#define DEFAULT_VFLAG 1
#else MAN_SYSV_MODE
#define DEFAULT_VFLAG 0
#endif MAN_SYSV_MODE;

#define	WHATIS	"whatis"

int	nomore;
char	*CAT	= CAT_;
char	*manpath = "/usr/man";
char	*strcpy();
char	*strcat();
char	*getenv();
char	*calloc();
char	*trim();
int	remove();
int	apropos();
int	whatis();
int	eqname();
int	section;
int	subsec;
int	troffit;
int	mypid;

char	*Pager;
char	Pagebuf[1024];
char	*myname;
char	*termopt = NULL;
char	*ttermopt = NULL;
char	termname[1024];
char	*Ul;

int	Printname = 0;	/* -n option */
int	dflag = 0;
int	aflag = 0;
int	Vflag = DEFAULT_VFLAG;
char	**prefixes;
char	*systype = NULL;

#define	eq(a,b)	(strcmp(a,b) == 0)

main(argc, argv)
	int argc;
	char *argv[];
{
	char *mp;

	myname = argv[0];
	if ((mp = getenv("MANPATH")) != NULL)
		manpath = mp;
	getpager();
	umask(0);
	mypid = getpid();
	if (eqname(argv[0], "apropos")) {
		runpath(argc-1, argv+1, apropos);
		exit(0);
	}
	if (eqname(argv[0], "whatis")) {
		runpath(argc-1, argv+1, whatis);
		exit(0);
	}
	if (argc <= 1) {
		usage();
		exit(1);
	}
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch(argv[0][1]) {

		case 0:
			nomore++;
			CAT = CAT_S;
			break;

		case 't':
			troffit++;
			break;

		case 'k':
			apropos(argc-1, argv+1);
			exit(0);

		case 'f':
			whatis(argc-1, argv+1);
			exit(0);

		case 'S':
			if (argc < 2) {
				fprintf(stderr, "%s: missing systype arg\n",
					 myname);
				exit(1);
			}
			argc--, argv++;
			systype = *argv;
			break;

		case 'P':		/* backwards compatibility */
		case 'M':
			if (argc < 2) {
				fprintf(stderr, "%s: missing path\n", *argv);
				exit(1);
			}
			argc--, argv++;
			manpath = *argv;
			break;

		case 'w':		/* System V compatibility */
		case 'n':
			Printname = 1;
			break;
		
		case 'c':		/* System V compatibility */
			break;

		case 'd':		/* System V compatibility */
			manpath = ".";
			dflag++;
			break;

		case 'B':
			Vflag = 0;
			break;

		case 'V':
			Vflag = 1;
			break;
	
		case 'a':		/* 4.3 BSD-tahoe compatibility */
			aflag = 1;
			break;

		case 'T':		/* System V compatibility */
			termopt = argv[0]+2;
			break;
	
		case '1':		/* System V compatibility */
			if (argv[0][2] == '2' &&
			    argv[0][3] == 0) {
				ttermopt = argv[0];
				break;
			};
		default:
			usage();
			exit(1);
			break;
		}
		argc--, argv++;
	}
	if (argc < 1) {
		usage();
		exit(1);
	}

	if (termopt || ttermopt) {
		if (termopt == NULL) {
			termopt = getenv("TERM");
			if (termopt == NULL)
				termopt = "";
		}
		if (ttermopt == NULL)
			ttermopt = "";
		sprintf(termname,"%s%s",termopt,ttermopt);
		setenv("TERM",termname,1);
	};
	if (Vflag) {
		aflag++;
		prefixes = sysv_prefixes;
	} else {
		prefixes = bsd_prefixes;
	};
	if (systype) {
		if (eq(systype, "sysv")) 
			prefixes = sysv_prefixes;
		else if (eq(systype, "bsd")) 
			prefixes = bsd_prefixes;
		else if (eq(systype, "posix")) 
			prefixes = posix_prefixes;
		else {
			usage();
			exit(1);
		}
	}

	if (troffit == 0 && nomore == 0 && !isatty(1))
		nomore++;
	if (nomore)
		Pager = CAT;

	section = 0;
	do {
		if (eq(argv[0], "local")) {
			section = 'l';
			goto sectin;
		} else if (eq(argv[0], "new")) {
			section = 'n';
			goto sectin;
		} else if (eq(argv[0], "old")) {
			section = 'o';
			goto sectin;
		} else if (eq(argv[0], "public")) {
			section = 'p';
			goto sectin;
		} else if (isdigit(argv[0][0]) &&
		    (argv[0][1] == 0 || argv[0][2] == 0)) {
			section = argv[0][0];
			subsec = argv[0][1];
sectin:
			argc--, argv++;
			if (argc == 0) {
				fprintf(stderr,
				    "But what do you want from section %s?\n",
				    argv[-1]);
				exit(1);
			}
			continue;
		}
		if (argc < 1) {
			usage();
			exit(1);
		}
		manual(section, argv[0]);
		argc--, argv++;
	} while (argc > 0);
	exit(0);
}

runpath(ac, av, f)
	int ac;
	char *av[];
	int (*f)();
{

	if (ac > 0 && (strcmp(av[0], "-M") == 0 || strcmp(av[0], "-P") == 0)) {
		if (ac < 2) {
			fprintf(stderr, "%s: missing path\n", av[0]);
			exit(1);
		}
		manpath = av[1];
		ac -= 2, av += 2;
	}
	(*f)(ac, av);
	exit(0);
}

manual(sec, name)
	char sec, *name;
{
	char section = sec;
	char work[MAXPATHLEN+1];
	char path[MAXPATHLEN+1];
	struct stat stbuf;
	int last;
	char *mp= ALLMAN;
	char man_id = ' ';
	char *sp;
	char mbuf[MAXPATHLEN+1];
	int	found_one = 0;
	char **pp;

	if (dflag) {
		sprintf(work,"./%s",name);
		if (pathstat(work, path,&stbuf)) {
			do_manual_entry(sec,name,work,path,&stbuf);
			if (! aflag)
				return;
			found_one++;
		}
		fprintf(stderr,"%s: %s not found\n",myname,name);
		exit(1);
	};
		
	if (section == '1') {
		sp = SECT1;
		mp = ALLMAN1;
	}
	if (section == 0) {		/* no section given */
	    for (man_id = *mp++; man_id; man_id = *mp++) {
		if (man_id == '.') 
			mbuf[0] = 0;
		else
			sprintf(mbuf,"%c_man/",man_id);
		if (! pathstat(mbuf, path, &stbuf))
		  	continue;
		for (pp = prefixes; *pp != NULL; pp++) {
		    sp = ALLSECT;
		    for (section = *sp++; section; section = *sp++) {
			sprintf(work,"%s%sman%c",mbuf,*pp,
					section);
			if (! pathstat(work,path,&stbuf))
				continue;
			sprintf(work,"%s%sman%c/%s.%c%c",mbuf,*pp,
					section,name,section,0);
			last = strlen(work) - 1;
			if (pathstat(work, path, &stbuf)) {
				do_manual_entry(sec,name,work,path,&stbuf);
				if (! aflag)
					return;
				found_one++;
			};
			if (work[last] >= '1' && work[last] <= '8') {
				char *cp;

				switch (work[last]) {
				case '1': cp = SUBSEC1; break;
				case '2': cp = SUBSEC2; break;
				case '3': cp = SUBSEC3; break;
				case '4': cp = SUBSEC4; break;
				case '5': cp = SUBSEC5; break;
				case '6': cp = SUBSEC6; break;
				case '7': cp = SUBSEC7; break;
				case '8': cp = SUBSEC8; break;
				default:  cp = ""; break;
				}
				while (*cp) {
					work[last+1] = *cp++;
					if (pathstat(work, path, &stbuf)) {
						do_manual_entry(sec,name,work,path,&stbuf);
						if (! aflag)
							return;
						found_one++;
					}
				}
				work[last+1] = 0;
			}
		    };
		}
	    }
	    if (found_one)
		return;
	    if (section == 0) {
		if (sec == 0) {
			printf("No manual entry for %s.\n", name);
			return;
		}
		section = sec; /* for use by the printf at the end
					* of this procedure */
	    }
	} else {			/* section given */
	    for (man_id = *mp++; man_id; man_id = *mp++) {
		if (man_id == '.') 
			mbuf[0] = 0;
		else
			sprintf(mbuf,"%c_man/",man_id);
		for (pp = prefixes ; *pp != NULL ; pp++ ) {
		    sprintf(work,"%s%sman%c/%s.%c%c",mbuf,*pp,
				section,name,section,subsec);
		    last = strlen(work) - 
		      		(subsec == 0 ? 1 : 2);
		    if (pathstat(work, path, &stbuf)) {	 /* found a man entry */
			do_manual_entry(sec,name,work,path,&stbuf);
			if (! aflag)
				return;
			found_one++;
		    }
		    if ((section >= '1' && section <= '8') &&	/* any more? */
		        subsec == 0) {
			char *cp;

			switch (work[last]) {
			case '1': cp = SUBSEC1; break;
			case '2': cp = SUBSEC2; break;
			case '3': cp = SUBSEC3; break;
			case '4': cp = SUBSEC4; break;
			case '5': cp = SUBSEC5; break;
			case '6': cp = SUBSEC6; break;
			case '7': cp = SUBSEC7; break;
			case '8': cp = SUBSEC8; break;
			default:  cp = ""; break;
			}
			while (*cp) {
				work[last+1] = *cp++;
				if (pathstat(work, path, &stbuf)) {
					do_manual_entry(sec,name,work,path,&stbuf);
					if (! aflag)
						return;
					found_one++;
				}
			}
			work[last+1] = 0;
		    }
		    else if (section == 'o') {	/* XXX */
			char *cp;
			char sec;
			for (sec = '0'; sec <= '8'; sec++) {
				work[last] = sec;
				if (pathstat(work, path, &stbuf)) {
					do_manual_entry(sec,name,work,path,&stbuf);
					if (! aflag)
						return;
					found_one++;
				}
				switch (work[last]) {
				case '1': cp = SUBSEC1; break;
				case '2': cp = SUBSEC2; break;
				case '3': cp = SUBSEC3; break;
				case '4': cp = SUBSEC4; break;
				case '5': cp = SUBSEC5; break;
				case '6': cp = SUBSEC6; break;
				case '7': cp = SUBSEC7; break;
				case '8': cp = SUBSEC8; break;
				default:  cp = ""; break;
				}
				while (*cp) {
					work[last+1] = *cp++;
					if (pathstat(work, path, &stbuf)) {
						do_manual_entry(sec,name,work,path,&stbuf);
						if (! aflag)
							return;
						found_one++;
					}
				}
				work[last+1] = 0;
			 }
		    }
		}
	    }
	}
	if (found_one)
		return;
	printf("No entry for %s in section %c", name, section);
	if (subsec)
		putchar(subsec);
	printf(" of the manual.\n");
}
	

do_manual_entry(sec, name, actual_work, path, stbuf)
	char sec, *name;
	char *actual_work;
	char *path;
	struct stat *stbuf;
{
	char work[MAXPATHLEN+1];
	char work2[MAXPATHLEN+1];
	char realname[MAXPATHLEN+1];
	char cmdbuf[MAXPATHLEN+151];
	struct stat stbuf2;
	char *mp;
	FILE *it;
	char abuf[BUFSIZ];
	char mbuf[100];

	strcpy(work,actual_work);
	sprintf(realname, "%s/%s", path, work);
	if (Printname) {
		printf("%s\n", realname);
		return;
	}
	if (troffit) {
		troff(path, work);
		return;
	}
	if (!nomore) {
		if ((it = fopen(realname, "r")) == NULL) {
			goto catit;
		}
		if (fgets(abuf, BUFSIZ-1, it) &&
		   strncmp(abuf, ".so ", 4) == 0) {
			register char *cp = abuf+4;
			char *dp;

			while (*cp && *cp != '\n')
				cp++;
			*cp = 0;
			while (cp > abuf && *--cp != '/')
				;
			dp = ".so man";
			if (cp != abuf+strlen(dp)+1) {
tohard:
				fclose(it);
				nomore = 1;
				strcpy(work, abuf+4);
				goto hardway;
			}
			for (cp = abuf; *cp == *dp && *cp; cp++, dp++)
				;
			if (*dp)
				goto tohard;
			strcpy(work, cp-3);
		}
		fclose(it);
	}
catit:
	if (work[1] == '_') {
		sprintf(work2,"catman/%s",work);
		if ((mp = index(index(work2,'/') + 1,'/')) &&
		    (mp = index(mp + 1,'/')))
			*mp = 0;
	} else if (! strncmp(work,"bsd_man",7)) {
		sprintf(work2,"bsd_cat%c",work[7]);
	} else {
		sprintf(work2,"cat%c",work[3]);
	}
	sprintf(realname, "%s/%s", path, work2);
	if (stat(realname, &stbuf2) < 0)
		goto hardway;
	if (work[1] == '_') {
		sprintf(work2,"catman/%s",work);
	} else if (! strncmp(work,"bsd_man",7)) {
		sprintf(work2,"bsd_cat%s",work + 7);
	} else {
		sprintf(work2,"cat%s",work + 3);
	}
	sprintf(realname, "%s/%s", path, work2);
	if (stat(realname, &stbuf2) < 0
	    || stbuf2.st_mtime < stbuf->st_mtime
	    || stbuf2.st_size == 0) {
		if (nomore)
			goto hardway;
		printf("Reformatting page.  Wait...");
		fflush(stdout);
		unlink(work2);
		if (signal(SIGINT, SIG_IGN) == SIG_DFL) {
			(void) signal(SIGINT, remove);
			(void) signal(SIGQUIT, remove);
			(void) signal(SIGTERM, remove);
		}
		sprintf(cmdbuf, "%s %s/%s | col > /tmp/man%d; trap '' 1 15",
			NROFFCAT, path, work, mypid);
		if (system(cmdbuf)) {
			printf(" aborted (sorry)\n");
			remove();
			/*NOTREACHED*/
		}
		sprintf(cmdbuf, "/bin/mv -f /tmp/man%d %s/%s 2>/dev/null",
			mypid, path, work2);
		if (system(cmdbuf)) {
			sprintf(path,  "/");
			sprintf(work2, "tmp/man%d", mypid);
		}
		printf(" done\n");
	}
	strcpy(work, work2);
hardway:
	nroff(path, work);
	if (work2[0] == 't')
		remove();
}

/*
 * Use the manpath to look for
 * the file name.  The result of
 * stat is returned in stbuf, the
 * successful path in path.
 */
pathstat(name, path, stbuf)
	char *name, path[];
	struct stat *stbuf;
{
	char *cp, *tp, *ep;
	char **cpp;
	static char *manpaths[] = {"man", "cat", 0};
	static char *nopaths[]  = {"", 0};

	if (strncmp(name, "man", 3) == 0)
		cpp = manpaths;
	else
		cpp = nopaths;
	for ( ; *cpp ; cpp++) {
		for (cp = manpath; cp && *cp; cp = tp) {
			tp = index(cp, ':');
			if (tp) {
				if (tp == cp) {
					sprintf(path, "%s%s", *cpp,
						name+strlen(*cpp));
				}
				else {
					sprintf(path, "%.*s/%s%s", tp-cp, cp, 
						*cpp, name+strlen(*cpp));
				}
				ep = path + (tp-cp);
				tp++;
			} else {
				sprintf(path, "%s/%s%s", cp, *cpp,
					name+strlen(*cpp));
				ep = path + strlen(cp);
			}
			if (stat(path, stbuf) >= 0) {
				*ep = '\0';
				return (1);
			}
		}
	}
	return (0);
}

nroff(pp, wp)
	char *pp, *wp;
{
	char cmd[BUFSIZ];

	prompt_for_yes();
	chdir(pp);
	sprintf(cmd, "%s %s | col |%s",
		(wp[0] == 'c' || wp[0] == 't') ? "cat" : NROFF, 
		wp, Pager);
	(void) system(cmd);
}

/*
 * The subroutine troff() attempts to format the manual page using
 * some kind of troff command. First, it attempts to use "mantroff".
 * If it can't find that, it tries "vtroff".
 */

troff(pp, wp)
	char *pp, *wp;
{
	int pid;
	int wpid;

	chdir(pp);
	
	pid = fork();
	if (pid < 0) {
		perror("Can not format file");
		return;
	}
	if (pid == 0) {
		execlp("mantroff", "mantroff", "-man", wp, 0);
		execlp("vtroff", "vtroff", "-man", wp, 0);
		perror("Could not execute mantroff or vtroff");
		exit(1);
	}
	while ((wpid = wait(NULL)) != pid) {
		if (wpid == -1) {
			break;
		}
	}
}

any(c, sp)
	register int c;
	register char *sp;
{
	register int d;

	while (d = *sp++)
		if (c == d)
			return (1);
	return (0);
}

remove()
{
	char name[15];

	sprintf(name, "/tmp/man%d", mypid);
	unlink(name);
	exit(1);
}

unsigned int
blklen(ip)
	register char **ip;
{
	register unsigned int i = 0;

	while (*ip++)
		i++;
	return (i);
}

apropos(argc, argv)
	int argc;
	char **argv;
{
	char buf[BUFSIZ], file[MAXPATHLEN+1];
	char *gotit, *cp, *tp;
	register char **vp;

	if (argc == 0) {
		fprintf(stderr, "apropos what?\n");
		exit(1);
	}
	gotit = calloc(1, blklen(argv));
	for (cp = manpath; cp; cp = tp) {
		tp = index(cp, ':');
		if (tp) {
			if (tp == cp)
				strcpy(file, WHATIS);
			else
				sprintf(file, "%.*s/%s", tp-cp, cp, WHATIS);
			tp++;
		} else
			sprintf(file, "%s/%s", cp, WHATIS);
		if (freopen(file, "r", stdin) == NULL)
			continue;
		while (fgets(buf, sizeof buf, stdin) != NULL)
			for (vp = argv; *vp; vp++)
				if (match(buf, *vp)) {
					printf("%s", buf);
					gotit[vp - argv] = 1;
					for (vp++; *vp; vp++)
						if (match(buf, *vp))
							gotit[vp - argv] = 1;
					break;
				}
	}
	for (vp = argv; *vp; vp++)
		if (gotit[vp - argv] == 0)
			printf("%s: nothing appropriate\n", *vp);
}

match(bp, str)
	register char *bp;
	char *str;
{

	for (;;) {
		if (*bp == 0)
			return (0);
		if (amatch(bp, str))
			return (1);
		bp++;
	}
}

amatch(cp, dp)
	register char *cp, *dp;
{

	while (*cp && *dp && lmatch(*cp, *dp))
		cp++, dp++;
	if (*dp == 0)
		return (1);
	return (0);
}

lmatch(c, d)
	register int c, d;
{

	if (c == d)
		return (1);
	if (!isalpha(c) || !isalpha(d))
		return (0);
	if (islower(c))
		c = toupper(c);
	if (islower(d))
		d = toupper(d);
	return (c == d);
}

whatis(argc, argv)
	int argc;
	char **argv;
{
	register char *gotit, **vp;
	char buf[BUFSIZ], file[MAXPATHLEN+1], *cp, *tp;

	if (argc == 0) {
		fprintf(stderr, "whatis what?\n");
		exit(1);
	}
	for (vp = argv; *vp; vp++)
		*vp = trim(*vp);
	gotit = calloc(1, blklen(argv));
	for (cp = manpath; cp; cp = tp) {
		tp = index(cp, ':');
		if (tp) {
			if (tp == cp)
				strcpy(file, WHATIS);
			else
				sprintf(file, "%.*s/%s", tp-cp, cp, WHATIS);
			tp++;
		} else
			sprintf(file, "%s/%s", cp, WHATIS);
		if (freopen(file, "r", stdin) == NULL)
			continue;
		while (fgets(buf, sizeof buf, stdin) != NULL)
			for (vp = argv; *vp; vp++)
				if (wmatch(buf, *vp)) {
					printf("%s", buf);
					gotit[vp - argv] = 1;
					for (vp++; *vp; vp++)
						if (wmatch(buf, *vp))
							gotit[vp - argv] = 1;
					break;
				}
	}
	for (vp = argv; *vp; vp++)
		if (gotit[vp - argv] == 0)
			printf("%s: not found\n", *vp);
}

wmatch(buf, str)
	char *buf, *str;
{
	register char *bp, *cp;

	bp = buf;
again:
	cp = str;
	while (*bp && *cp && lmatch(*bp, *cp))
		bp++, cp++;
	if (*cp == 0 && (*bp == '(' || *bp == ',' || *bp == '\t' || *bp == ' '))
		return (1);
	while (isalpha(*bp) || isdigit(*bp))
		bp++;
	if (*bp != ',')
		return (0);
	bp++;
	while (isspace(*bp))
		bp++;
	goto again;
}

char *
trim(cp)
	register char *cp;
{
	register char *dp;

	for (dp = cp; *dp; dp++)
		if (*dp == '/')
			cp = dp + 1;
	if (cp[0] != '.') {
		if (cp + 3 <= dp && dp[-2] == '.' &&
		    any(dp[-1], "cosa12345678npP"))
			dp[-2] = 0;
		if (cp + 4 <= dp && dp[-3] == '.' &&
		    any(dp[-2], "13") && isalpha(dp[-1]))
			dp[-3] = 0;
	}
	return (cp);
}

/*
 * The subroutine getpager() sets the variable Pager to the string containing
 * the pager to use. If the variable MANPAGER is set, that is used without
 * modification. If not, PAGER is checked. If PAGER ends with 'more' or 'less',
 * the pager will be that name plus '-s'. If nothing is found, MORE (defined
 * above) is used.
 */

getpager()
{
	char *val;
	char *end;

	val = getenv("MANPAGER");
	if (val) {
		Pager = val;
		return;
	}

	val = getenv("PAGER");
	if (!val) {
		Pager = MORE;
		return;
	}

	for (end = val; *end && !isspace(*end); ++end);
	for (; end > val && *end != '/'; --end);
	if (end != val)
		++end;

	if (! strcmp(end, "more") || ! strcmp(end, "page")) {
		sprintf(Pagebuf, "ul | %s -s -f", val);
		Pager = Pagebuf;
		return;
	}
	if (! strcmp(end, "less")) {
		sprintf(Pagebuf, "%s -s",val);
		Pager = Pagebuf;
		return;
	}
 	Pager = val;
}

/*
 * The subroutine eqname() returns 1 if the basenames of the two strings
 * are equal.
 */

int
eqname(s1, s2)
	char *s1;
	char *s2;
{

	char *tmp;

	tmp = rindex(s1, '/');
	if (tmp) {
		s1 = tmp;
		s1++;
	}
	tmp = rindex(s2, '/');
	if (tmp) {
		s2 = tmp;
		s2++;
	}
	return (strcmp(s1, s2) == 0);
}

usage()
{
	fprintf(stderr, "Usage: man [-] [-[acdfkntwBV] [-Tterm] [-12] [-[PM] path] [section] [-S systype] name ...\n");
}


int	yes_count = 0;

prompt_for_yes()
{
	char	yes_buf[1024];

	yes_count++;
/*	if (! Vflag || */	/* man -a should ask this question too */
	if (yes_count == 1)
		return;

	printf("[Hit return for next manual page, q to quit]");
	fflush(stdout);
	if (fgets(yes_buf,sizeof(yes_buf) - 1,stdin) == NULL)
		return;
	if (yes_buf[0] == 'q' ||
	    yes_buf[1] == 'Q')
		exit(0);
}
