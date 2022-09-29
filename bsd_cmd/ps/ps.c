/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: ps.c,v 1.1.1.9.1.2 90/07/11 18:06:09 hawkes Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	ps - process status					*/
/*	examine and print certain things about processes	*/
/*								*/
/*	This is a bsd43 ps command - fixed by MIPS to display   */
/*	bsd type of format.					*/
/*								*/
/*	bugs:							*/
/*	-e option works for the effective uid only 		*/
/*	-k option is not supported by this version		*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include "sys/types.h"
#include <ctype.h>
#include "nlist.h"
#include "pwd.h"
#include "sys/param.h"
#include "sys/immu.h"	/* sys/immu.h must precede sys/region.h for 3b2 */
#include "sys/psw.h"	/* psw, program status word */
#include "sys/pcb.h"	/* pcb, program control blk */
#include "sys/sbd.h"	/* need for MAINSTORE	    */
#define MAINSTORE 0 /* Address of physical location 0 for /dev/mem driver. */

#include <filehdr.h>
#include <scnhdr.h>
#include <syms.h>
#include <ldfcn.h>

#include "sys/sysmacros.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/dir.h"
#include "sys/signal.h"
#include "sys/stat.h"
#include "sys/user.h"
#include "sys/var.h"
#include "sys/nami.h"

#include "../../lib/libmips/libutil.h"
#include "sys/inode.h"
#include "sys/vnode.h"
#include "sys/fixpoint.h"

#define pgtok(a)	((a)*(NBPP/1024))

static char psfile[] = "/etc/ps_data";

/*
 * ps output strings for 4.3 BSD style output
 */
char *elsestring=
    "  PID  TT STAT  TIME COMMAND\n";
char	*uhdr =
"%s   PID %%CPU %%MEM   SZ  RSS  TT STAT  TIME COMMAND\n";
char *vhdr =
"  PID  TT STAT  TIME SL RE PAGEIN  SIZE   RSS   LIM TSIZ TRS %%CPU %%MEM COMMAND\n";
char	*shdr =
"SSIZ   PID  TT STAT  TIME COMMAND\n";
char *bsdflstring =
"   F UID   PID  PPID CP PRI NI     ADDR SZ RSS    WCHAN STAT  TT  TIME COMMAND\n";
char *bsdelsestring="  PID  TT STAT  TIME COMMAND\n";

#define NTTYS	20 /* max num ttys that can be specified with the -t option  */
#define SIZ 	30 /* max num processes that can be specified with -p and -g */
#define ARGSIZ	30 /* size of buffer which holds args for -t, -p & -u options*/

#ifndef MAXLOGIN
#define MAXLOGIN 8 /* max number of char in userid that will be printed  */
#endif

/* Structure for storing user info */
struct udata {
	unsigned short uid;  /* numeric user id */
	char name[MAXLOGIN]; /* character user id may not be null terminated */
};

/* udata and devl granularity for structure allocation */
#define UDQ	50
int error_result;

/* Pointer to user data */
struct udata *ud;
int	nud = 0;	/* number of valid ud structures */
int	maxud = 0;	/* number of ud's allocated */

struct udata uid_tbl[SIZ];	/* table to store selected uid's */
int	nut = 0;		/* counter for uid_tbl */

/* see nlist(3c)			 */
/* swaplo(w): from /unix, 1st block # on device to be used for swap */

struct nlist nl[] = {
	"proc", (long)0, (short)0, (short)0,
	"swplo", (long)0, (short)0, (short)0,
	"v", (long)0, (short)0, (short)0,
	"id_string", (long) 0, (short) 0, (short) 0,
	"maxmem", (long) 0, (short) 0, (short) 0,
	0, (long)0, (short)0, (short)0,
};

struct proc prc;
#define	mproc	prc
#define	zproc	prc
struct	var  v;

#ifdef mips
#undef u
struct	user u;
#endif

int	bsdflg = 1;  /* Generate 4.3 BSD style ps output */
int	chkpid = -1;
int	retcode=1;
int	c, loopindex;
int	Uflg; 
int	aflg, cflg, eflg, gflg, kflg, lflg, nflg, sflg, tflg, uflg, vflg, xflg;
int	rawcpu, sumcpu;
int	errflg,	memfd, swmem;
FILE	*env_fd;
int	twidth;

int	isatty();
char	*gettty();
char	*ttyname();

daddr_t	swplo;   /* disk block number, physical, type long, see sys/types.h */
long	maxmem;

char	argbuf[ARGSIZ];
char	*parg;
char	*p1;		     /* points to successive option arguments */
char    *tptr, *ttyname(), mytty[MAXPATHLEN+1];
char	*coref,	*memf, *sysname;
long 	lseek();
static char stdbuf[BUFSIZ];

int	ndev;			/* number of devices */
int	maxdev;			/* number of devl structures allocated */

/* DIRSIZ: max char per directory, 14, see sys/dir.h  */
struct devl {			/* device list and sys/param.h*/
	char	dname[DIRSIZ];	/* device name	 */
	dev_t	dev;		/* device number */
} *devl;

char	*tty[NTTYS];	/* for t option */
int	ntty = 0;

int	pid[SIZ];	/* for p option */
int	npid = 0;

int	grpid[SIZ];	/* for g option */
int	ngrpid = 0;
int 	p_stat;
int	tsz, trs;

int     page_size;
#ifdef mips
int     pte_to_pfn_shift;
int     pfn_to_byte_shift;
#define pte_to_byte(x)  (((x) >> pte_to_pfn_shift) << pfn_to_byte_shift)
extern int      getpagesize();
#endif mips

extern int	chown();
extern unsigned short getegid();
extern int	errno;
extern char	*sys_errlist[];
extern void	exit();
extern char	*malloc();
extern char	*realloc();

char	*state();

main(argc, argv)
int argc;
char **argv;
{
	register char **ttyp = tty;
	register char *ap;
	char *name;
	char *p;
	int puid, ppid, ppgrp;	/* puid: process user id */
	/* ppid: parent process id, see sys/proc.h */
	/* ppgrp: process group id */
	int i, found, uid;
	extern char *optarg;	

	extern int optind;
	extern int num();	/* function to determine whether numeric    */
	int pgerrflg = 0;	/* err flg: non-numeric arg w/p & g options */
	void getdev();

	char *malloc();
	unsigned size;

#ifdef mips
        page_size = getpagesize();
        switch (page_size) {
        case 4096:      pte_to_pfn_shift = 12;
                        pfn_to_byte_shift = 12;
                        break;
        case 16384:     pte_to_pfn_shift = 10;
                        pfn_to_byte_shift = 14;
                        break;
        default:        fprintf(stderr,"ps: cannot handle %d byte page size\n",
                                page_size);
                        done(1);
        } /* switch */
#else
        page_size = NBPP;
#endif

	sysname = "/unix";

	coref = "/dev/kmem";
	memf = "/dev/mem";

	setbuf(stdout, stdbuf);
	argc--, argv++;
	if (argc > 0) {
	  ap = argv[0];
	  while (*ap) switch(*ap++) {
		case 'C':
			rawcpu++; /* ignore this for now */
			break;
		case 'S':
			sumcpu++; /* ignore this for now */
			break;
		case 'U':
			Uflg++;   /* ignore this for now */
			break;
		case 'a':
			aflg++;
			break;
		case 'c':
			cflg++;
			break;
		case 'e':
			eflg++;
			break;
		case 'g':
			gflg++;
			break;
#if !(u3b2 || u3b5 || mips)
                case 'k':               /* core file given */
                        kflg++;		/* c option unavailable on 3b2/5    */
                        break;		/* until virtual to physical memory */
                        	        /* address translation library      */
                                        /* routine is available             */
#endif
		case 'l':		/* long listing */
			lflg++;
			break;
		case 'n':		
			nflg++;
			break;
		case 's':
			sflg++;
			break;
		case 't':		 /* terminals */
			if (*ap)
				tptr = ap;
			else if ((tptr = ttyname(0)) != 0) {
				tptr = strcpy(mytty, tptr);
				if (strncmp(tptr, "/dev/", 5) == 0)
					tptr += 5;
			}
			if (strncmp(tptr, "tty",3) == 0)
				tptr += 3;
			aflg++;
			gflg++;
			if (tptr && *tptr == '?')
				xflg;
			while (*ap)
				ap++;
			break;

		case 'u':
			uflg++;
			break;
		case 'v':
			cflg = 1;
			vflg++;
			break;
		case 'w':
			if (twidth < 132)
				twidth = 132;
			else
				twidth = BUFSIZ;
			break;
		case 'x':
			xflg++;
			break;

		default:
			if (!isdigit(ap[-1]))
				break;
			chkpid = atoi(--ap);
			*ap = 0;
			aflg++;
			xflg++;
			break;
		}
	}
	if (lflg+sflg+uflg+vflg > 1) {
		fprintf(stderr, "ps: specify only one of s,l,v and u\n");
		exit(1);
	}
        if (tflg) *ttyp = 0;

        /* if specifying options not used current terminal is default */

        if ( !(aflg || eflg || uflg || tflg)) {
                name = 0;
                for (i = 2; i > -1; i--)
                        if (isatty(i)) {
                                name = ttyname(i);
                                break;
                        }
                if (name == 0) {
                        fprintf(stderr, "ps: can't find controlling terminal\n")
;
                        done(1);
                }
                *ttyp++ = name+5;
                *ttyp = 0;
                ntty++;
                tflg++;
        }

	if (!readata())
	  { /* get data from psfile */
	  getdev();
	  getpass();
	  getnl();
	  if (!Uflg) writeps_data();
	  }

	uconv();
	if(nl[0].n_value==0||nl[1].n_value==0||nl[2].n_value==0||nl[3].n_value==0||nl[4].n_value==0) {
		fprintf(stderr, "ps: no namelist\n");
		done(1);
	}
	if (kflg) {
                coref = argc == 2 ? argv[1] : "/core";
		memf = coref;
	}
	if ((memfd = open(coref, 0)) < 0) {
		fprintf(stderr, "ps: failed to open %s\n", coref);
		done(1);
	}
	if ((swmem = open(memf,0)) < 0) {
		fprintf(stderr, "ps: failed to open %s\n", memf);
		done(1);
	}

	/* Check to ensure that the kernel is strings match */
	error_result = check_kernel_id (memfd, (long)nl[3].n_value, sysname);
	if (error_result > 0)
	  { /* Print out a suitable error message */
	  switch (error_result)
	    {
	    case LIB_ERR_NOMATCH:
	     { /* wrong kernel id strings */
	     fprintf(stderr, "ps: Wrong kernel; kernel id strings mismatch\n");
	     break;
	     }

	    case LIB_ERR_KFILEOPEN:
	     { /* failed to open kernel file loaded */
	     fprintf (stderr, "ps: failed to open kernel file %s\n", sysname);
	     break;
	     }

	    default:
	     {
	    /*
	     * LIB_ERR_BADOFFSET 1   Bad offset value for symbol
	     * LIB_ERR_SHORTREAD 2   Short memory read error
	     * LIB_ERR_KLSEEK    4   Failed on kernel lseek operation
	     * LIB_ERR_KTEXTREAD 5   Failed reading kernel text segment
	     * LIB_ERR_KNODATA   6   Cannot locate data in kernel text file
	     */
	     fprintf (stderr, "ps: internal error= %d\n", error_result);
	     break;
	     }
	  }
         done (error_result);
	 }

	l_lseek(memfd, (long)nl[1].n_value, 0);	   /* Find base of swap table*/
	r_read(memfd, (char *)&swplo, (int)sizeof(swplo));

	l_lseek(memfd, (long)nl[2].n_value, 0);	   /* Find proc table size */
	r_read(memfd, (char *)&v, (int)sizeof(v));

	l_lseek(memfd, (long)nl[4].n_value, 0);	   /* Find maxmem size */
	r_read(memfd, &maxmem, sizeof(maxmem));

	l_lseek(memfd, (long)nl[0].n_value, 0);	   /* Locate process table */

	/* print header string */
	if (lflg)
	  printf (bsdflstring);
	else 
		if (uflg)
			printf (uhdr, nflg ? " UID" : "USER    ");
		else
			if (vflg)
				printf (vhdr);
			else
				if (sflg)
					printf (shdr);
				else
	  				printf (bsdelsestring);

	/* determine which processes to print info about */
	uid = getuid();
	for (i=0; i<v.v_proc; i++) {

		found = 1;
		r_read (memfd, (char *)&mproc, (int)sizeof(mproc));
		p_stat = mproc.p_stat;
		puid = mproc.p_uid;
		ppid = mproc.p_pid;
		ppgrp = mproc.p_pgrp;

		if (p_stat == 0 ||
		    ppgrp == 0 && xflg == 0) continue;

		if (tptr == 0 && gflg == 0 && xflg == 0 &&
		    ppid == 1) continue;
		if (uid != puid && aflg == 0)
			continue;
		if (chkpid != -1 && chkpid != ppid) continue;
		if (vflg && gflg == 0 && xflg == 0) {
			if (p_stat == SZOMB ||
			    mproc.p_flag&SWEXIT) continue;
			if (mproc.p_slptime > MAXSLP &&
			   (p_stat == SSLEEP ||
			   p_stat == SSTOP)) continue;
		}
		if (prprocess (uid,puid,found, nl[0].n_value + i*sizeof(mproc))) {
			printf ("\n");
			retcode = 0;
		}
	}
	done (retcode);	/*NOTREACHED*/
}

int done (exitno)
int exitno;
{
exit (exitno);
}

/*
* getdev() uses ftw() to pass pathnames under /dev to gdev() along with a
* status buffer.
*/
void
getdev()
{
	int gdev();
	int rcode;

	rcode = ftw ("/dev", gdev, 17);

	switch(rcode) {

	case 0:	 return;		/* successful return, devl populated */

	case 1:  fprintf(stderr, "ps: ftw() encountered problem\n");
		 done(1);

	case -1: fprintf(stderr, "ps: ftw() failed, %s\n", sys_errlist[errno]);
		 done(1);

	default:
	  fprintf(stderr, "ps: ftw() unexpected return, rcode=%d\n", rcode);
		 done(1);
	}
}

/*
* gdev() puts device names and ID into the devl structure for character
* special files in /dev.  The "/dev/" string is stripped from the name
* and if the resulting pathname exceeds DIRSIZ in length then the highest
* level directory names are stripped until the pathname is DIRSIZ or less.
*/
int
gdev (objptr, statp, numb)
char *objptr;
struct stat *statp;
int numb;
{
	register int i;
	int leng, start;
	static struct devl ldevl[2];
	static int lndev, consflg;

	switch (numb) {

	case FTW_F:	if ((statp->st_mode&S_IFMT) == S_IFCHR) {
				/* get more and be ready for syscon & systty */
				while ((ndev + lndev) >= maxdev) {
				 maxdev += UDQ;
				 devl = (struct devl *) ((devl == NULL) ?
				 malloc(sizeof(struct devl) * maxdev) :
				  realloc(devl, sizeof(struct devl) * maxdev));
				 if (devl == NULL) {
					fprintf(stderr,
			"ps: not enough memory for %d devices\n", maxdev);
					exit(1);
					}
				}

				/* save systty & syscon entries if */
				/* the console entry hasn't been seen*/

				if (!consflg &&
				   (strcmp("/dev/systty", objptr) == 0 ||
				    strcmp("/dev/syscon", objptr) == 0   )) {
					strncpy(ldevl[lndev].dname, &objptr[5], DIRSIZ);
					ldevl[lndev].dev = statp->st_rdev;
					lndev++;
					return(0);
				}

				leng = strlen(objptr);

				if (leng < (DIRSIZ + 4)) /* strip off /dev/ */
					strcpy(devl[ndev].dname, &objptr[5]);
				else {
					start = leng - DIRSIZ - 1;
			
					for (i = start; (i < leng) && (objptr[i] != '/'); i++) ;
					if (i == leng)
						strncpy(devl[ndev].dname, &objptr[start], DIRSIZ);
					else
						strncpy(devl[ndev].dname, &objptr[i+1], DIRSIZ);
				}
				devl[ndev].dev = statp->st_rdev;
				ndev++;
				/* put systty & syscon entries	 */
				/* in devl when console is found */

				if (strcmp("/dev/console", objptr) == 0) {
					consflg++;
					for (i=0; i < lndev; i++) {
						strncpy(devl[ndev].dname, ldevl[i].dname, DIRSIZ);
						devl[ndev].dev = ldevl[i].dev;
						ndev++;
					}
					lndev = 0;
				}
			}
			return(0);

	case FTW_D:
	case FTW_DNR:
	case FTW_NS:	return(0);

	default:	fprintf(stderr, "ps: gdev() error, %d, encountered\n", numb);
			return(1);
	}

	/* FTW_D:   Directory,	  FTW_F:  File	    */
	/* FTW_DNR: Directory-no read permission, FTW_NS: no status */
}

/* getarg finds next argument in list and copies arg into argbuf  */
/* p1 first pts to arg passed back from getopt routine.  p1 is then */
/* bumped to next character that is not a comma or blank - p1 null */
/* indicates end of list    */

int
getarg()
{
	char *parga;
	parga = argbuf;
	while(*p1 && *p1 != ',' && *p1 != ' ')
		*parga++ = *p1++;
	*parga = '\0';

	while( *p1 && ( *p1 == ',' || *p1 == ' ') )
		p1++;
}

/* Get name list data into nl structure */

int
getnl()
{
	nlist(sysname, nl);
}

/* gettty returns the user's tty device name string or ? if none  */
char *gettty ()
{
	register i;
	register char *p;
	register int mdev;
	struct vnode mvnode;
	int	old_offset;

	if (mproc.p_ttyvp == NULL) return ("?");

	old_offset = lseek(memfd,0,1);
	if (old_offset == -1) {
		l_lseek(memfd,((long) old_offset),0);
		return ("?");
	};

	l_lseek(memfd,((long) mproc.p_ttyvp), 0);
	r_read(memfd, (char *) &mvnode, sizeof(mvnode));
	mdev = mvnode.v_rdev;
	l_lseek(memfd,((long) old_offset),0);

	for (i=0; i<ndev; i++) {
		if (devl[i].dev == mdev) {
			p = devl[i].dname;
			if ((p[0]=='p'&&p[1]=='t'&&p[2]=='y') ||
			    (p[0]=='t'&&p[1]=='t'&&p[2]=='y'))
				p += 3;
			return(p);
		}
	}
	return ("?");
}

/* Get the passwd file data into the ud structure */
int
getpass()
{
	struct passwd *pw, *getpwent();

	ud = NULL;
	nud = 0;
	maxud = 0;

	while((pw=getpwent()) != NULL) {
		while(nud >= maxud) {
			maxud += UDQ;
			ud = (struct udata *) ((ud == NULL) ?
				malloc(sizeof(struct udata) * maxud) :
				realloc(ud, sizeof(struct udata) * maxud));
			if(ud == NULL) {
				fprintf(stderr,"ps: not enough memory for %d users\n", maxud);
				exit(1);
			}
		}
		/* copy fields from pw file structure to udata */
		ud[nud].uid = pw->pw_uid;
		strncpy(ud[nud].name, pw->pw_name, MAXLOGIN);
		nud++;
	}
	endpwent();
}

/*
* Get user name for full command (-f flag) instead of printing user id number
* To do this we search thru existing table of userid numbers and if puid is
* found return the corresponding name.  Otherwise search thru /etc/passwd.
*/
int getunam (puid)
int puid;
{
	int i;

	for(i=0; i<nud; i++)
		if(ud[i].uid == puid)
			return(i);
	return(-1);
}

/* print information about the process */

int prprocess (uid, puid, found, pp)
int uid, puid, found;
struct proc *pp;
{
	long addr;
	register char *cp1;
	register char *cp;
	register char *tp;
	char *ctime();
	time_t time();
	time_t *clock, *tloc;
	time_t tim;
	char timbuf[26];
	char *curtim = timbuf;
	char *sttim, *s1, *s2, *p2;
	long tm;
	int	match, i;
	register char **ttyp, *str;
	char buf[1024], *ptr;

	/* SIDL: intermediate state in process creation  */
	/* if SIDL then user block address may be bad or */
	/* contents of user block may be residue	 */
	if (mproc.p_stat==SIDL) {return(1);}

	/* If paging system, process is in memory, read in user block	 */
	/* If non-paged system, determine if process is in memory or swapped */
	/* then read in user block  */
	/* uptbl, user page table	  */
	/* NBPP, number of bytes per page, see sys/page.h */
	/* Replace NBPP by page_size, chages for M6000 */

	cp1 = (char *)&u + sizeof(u);
	i = ((char *)ubptbl(pp) - (char *)pp -
	    ((char *)mproc.p_ubptbl - (char *)&mproc))>>2;

	for (cp = (char *) &u; cp < cp1; i++, cp += page_size) {

		l_lseek(swmem,
			pte_to_byte(mproc.p_ubptbl[i].pgi.pg_pde) - MAINSTORE, 0);
		if (read(swmem, cp, cp1-cp > page_size ? page_size : cp1-cp) < 0)
			fprintf(stderr, "ps: cannot read %d\n", swmem);
		}

	/*
	* Get current terminal identifier string.
        * if (none (?) returned and aflg is set) then don't print info.
	* if (tflg set) then check if term is in list of desired terminals
        * and if so print it.
	*/
	tp = gettty();

	if (!aflg && (*tp == '?' )) return(0);

	if (tptr) { /* the t option */
		if(strcmp(tp,tptr) != 0)
			return(0);
	}

	if (lflg)
	  { /* front portion of the l option */
	  printf ("%4x", mproc.p_flag&0377);
	  /* UID */
	  printf ("%4u", puid);
	  }
	if (sflg)
		printf("%4d ", mproc.p_rssize);
	if (uflg)
		upr();

	if (lflg)
	  printf ("%6u", mproc.p_pid);	/* PID */
	else
		if (!uflg)
	  		printf ("%5u", mproc.p_pid);	/* PID */

	if (lflg) {
		printf ("%6u%3d",mproc.p_ppid,mproc.p_cpu&0377); /* PPID CPU*/
		printf ("%4d%3d",mproc.p_pri, mproc.p_nice); /* PRI NI */
		/* ADDR SZ RSS */
		printf ("%9x%3d %3d", ubptbl(pp), mproc.p_size, mproc.p_rssize);
		if (mproc.p_wchan)		      /* WCHAN */
		    printf ("%9x", mproc.p_wchan);
		else 
		    printf ("         ");/* Note extra width 10x in null case */
		printf (" %s", state()); /* STAT */
	 }

        /* Compute the time in the normal units */
	tm = (u.u_utime + u.u_stime + HZ/2)/HZ;

	/* Is this 'console'? */
	if ( tp[1] == 'o' && tp[2] == 'n' && tp[3] == 's')
	{
		tp[0]='c'; tp[1]='o'; tp[2]=NULL;
	}

	if (!lflg) {
	  printf ("%4s ", tp);	/* 4.3 BSD TT */
	  printf ("%s", state()); /* STAT */
	}
	else {
          printf ("%4s", tp);
	}
	printf (" %2ld:%.2ld", tm/60, tm%60);	/* TIME */

	/* if cflg set */
	s2 = u.u_comm;
	if (cflg) {
		for (i=0; u.u_comm[i] != NULL; i++)
			if (u.u_comm[i] == '/')
				s2 = &u.u_comm[i+1];
	}
	if (vflg)
		vpr();

	/* print "COMMAND" field */
        if (mproc.p_stat == SZOMB)
                  printf(" <defunct>");
        else if (mproc.p_flag & SWEXIT)
                  printf(" <exiting>");
	else if (mproc.p_flag & SSYS) {
		if (u.u_comm[0])
			printf (" %s", u.u_comm);
		else
			printf (" swapper");
		return(1);
	}
	/*
	* Set up address maps for user pcs.
	* PSARGSZ, length of cmd arg string today 40, see sys/user.h
	* tommorrow 80?, hope so for consistency
	*/
	if (!cflg) {
		for (cp=u.u_psargs; cp<&u.u_psargs[PSARGSZ]; cp++) {
			if (*cp == 0) break;
			if (*cp < ' ' || *cp > '~') {
				printf(" [ %s ]", u.u_comm);
				return(1);
			}
		}
		printf (" %.35s", u.u_psargs);
		if (eflg && (uid == puid)) {
			if ((env_fd = popen("env", "r")) != NULL) {
				putchar (' ');
               			while (fgets(buf, 1024, env_fd) != NULL) {
					for (i=0; buf[i] != '\n'; i++)
                        			(void) putchar(buf[i]);
					putchar(' ');
					}
				pclose(env_fd);
			}
			setreuid(uid);
		}
	}
	else
		printf (" %s", s2);
	return(1);

}

/* lseek with error checking */

l_lseek(fd, offset, whence)
int fd, whence;
long	offset;
{
	if (lseek(fd, offset, whence) == -1) {
		fprintf(stderr, "ps: l_lseek() error on lseek, %s\n", sys_errlist[errno]);
		done(1);
	}
}

/* read with error checking */
r_read (fd, buf, nbytes)
int	fd, nbytes;
char	*buf;
{
	int rbytes;
	rbytes = read(fd, buf, (unsigned)nbytes);
	if (rbytes != nbytes) {
		fprintf(stderr, "ps: r_read() error on read, rbytes=%d, nbytes=%d\n",
			rbytes, nbytes);
		done(1);
	}
}

/* special read unlinks psfile on read error */

int
psread (fd, bp, bs)
int fd;
char *bp;
unsigned bs;
{
	int rbs;

	rbs = read(fd, bp, bs);
	if(rbs != bs) {
		fprintf(stderr, "ps: psread() error on read, rbs=%d, bs=%d\n", rbs, bs);
		unlink(psfile);
	}
}

/* special write unlinks psfile on read error */
int pswrite (fd, bp, bs)
int fd;
char *bp;
unsigned bs;
{
	int wbs;

	wbs = write(fd, bp, bs);
	if(wbs != bs) {
		fprintf(stderr, "ps: pswrite() error on write, wbs=%d, bs=%d\n",
			wbs, bs);
		unlink(psfile);
	}
}

/* read in the open devices (terminals) and stores info in devl structure */

int readata()
{
struct stat sbuf1, sbuf2;
int fd;

if (stat(psfile, &sbuf1) < 0
    || (stat("/dev", &sbuf2) < 0
     || sbuf1.st_mtime <= sbuf2.st_mtime || sbuf1.st_mtime <= sbuf2.st_ctime)
    || (stat("/unix", &sbuf2) < 0
     || sbuf1.st_mtime <= sbuf2.st_mtime || sbuf1.st_mtime <= sbuf2.st_ctime)
    || (stat("/etc/passwd", &sbuf2) < 0
    || sbuf1.st_mtime <= sbuf2.st_mtime || sbuf1.st_mtime <= sbuf2.st_ctime)) {
		return(0);
	}

if (( fd = open(psfile, O_RDONLY)) < 0) return(0);

/* read /dev data from psfile */
psread(fd, &ndev, sizeof(ndev));
if((devl = (struct devl *)malloc(ndev * sizeof(*devl))) == NULL) {
	fprintf(stderr, "ps: malloc() for device table failed, %s\n",
		sys_errlist[errno]);
	exit(1);
}

/* read devl data from psfile */
psread (fd, devl, ndev * sizeof(*devl));

/* read /etc/passwd data from psfile */
psread (fd, &nud, sizeof(nud));

if((ud = (struct udata *)malloc(nud * sizeof(*ud))) == NULL) {
	fprintf(stderr, "ps: not enough memory for udata table\n");
	exit(1);
      }
psread(fd, ud, nud * sizeof(*ud));

/* read /unix data from psfile */
if (!Uflg) psread(fd, nl, sizeof(nl));

close(fd);
return(1);
}

/*
* search returns 1 if arg is found in array arr which has length num.
* It returns 0 if not found.
*/
int search (arr, number, arg)
int arr[];
int number, arg;
{
	int i;
	for (i = 0; i < number; i++)
		if (arg == arr[i])
			return(1);
	return(0);
}

/* after the u option */

int uconv ()
{
	int found;
	int pwuid;
	int i, j;

	/* search thru name array for oarg */
	for (i=0; i<nut; i++) {
		found = -1;
		for(j=0; j<nud; j++) {
			if (strncmp(uid_tbl[i].name, ud[j].name, MAXLOGIN)==0) {
				found = j;
				break;
			}
		}
		/* if not found and oarg is numeric */
		/* then search through number array */
		if (found < 0 && (uid_tbl[i].name[0] >= '0' && uid_tbl[i].name[0] <= '9')) {
			pwuid = atoi(uid_tbl[i].name);
			for (j=0; j<nud; j++) {
				if (pwuid == ud[j].uid) {
					found = j;
					break;
				}
			}
		}

		/* if found then enter found index into tbl array */
		if ( found != -1 ) {
			uid_tbl[i].uid = ud[found].uid;
			strncpy(uid_tbl[i].name,ud[found].name,MAXLOGIN);
		}else {
			fprintf(stderr,"ps: unknown user %s\n",uid_tbl[i].name);
			for(j=i+1; j<nut; j++) {
				strncpy(uid_tbl[j-1].name,uid_tbl[j].name,MAXLOGIN);
			}
			nut--;
			if (nut <= 0) exit(1);
			i--;
		}
	}
	return;
}

/* ufind will return 1 if puid is in table ; if not return 0 */
int ufind (puid)
int puid;
{
	int i;

	for(i=0; i<nut; i++)
		if(uid_tbl[i].uid == puid)
			return(1);
	return(0);
}

/* write data into /etc/ps_data file */

int writeps_data ()
{
	int fd;

	umask(02);
	unlink(psfile);
	if((fd = open(psfile, O_WRONLY | O_CREAT | O_EXCL, 0664)) == -1) {
		fprintf(stderr, "ps: open() for write failed\n");
		fprintf(stderr, "ps: /etc/ps_data, %s\n", sys_errlist[errno]);
		fprintf(stderr, "ps: Please notify your System Administrator\n");
	}
	else {
		/* make owner root, group sys */
		chown(psfile, (int)0, (int)getegid());

		/* write /dev data */
		pswrite(fd, &ndev, sizeof(ndev));
		pswrite(fd, devl, ndev * sizeof(*devl));

		/* write /etc/passwd data */
		pswrite(fd, &nud, sizeof(nud));
		pswrite(fd, ud, nud * sizeof(*ud));

		/* write /unix data */
		pswrite(fd, nl, sizeof(nl));

		close(fd);
	}
}

char *
state()
{
	static char res[5] = "    ";
	char *cp = res;

	switch (p_stat) {

	case SSTOP:
		*cp = 'T';
		break;

	case SSLEEP:
		if (mproc.p_pri >= PZERO)
			if (mproc.p_slptime >= MAXSLP)
				*cp = 'I';
			else
				*cp = 'S';
		/* else if (ap->p_flag & SPAGE) SPAGE nonexisting state */
		else if (mproc.p_flag)
			*cp = 'P';
		else
			*cp = 'D';
		break;

/*	case SWAIT:  nonexisting state */
	case SRUN:
	case SIDL:
		*cp = 'R';
		break;

	case SZOMB:
		*cp = 'Z';
		break;

	default:
		*cp = '?';
	}

	cp++;
	if (mproc.p_flag & SLOAD) {
/*		if (mproc.p_rssize > mproc.p_maxrss)   */
		if (mproc.p_rssize > maxmem)
			*cp++ = '>';
	} else
		*cp++ = 'W';
	if (mproc.p_nice < NZERO)
		*cp++ = '<';
	else if (mproc.p_nice > NZERO)
		*cp++ = 'N';
	if (mproc.p_flag & SUANOM)
		*cp++ = 'A';
	else if (mproc.p_flag & SSEQL)
		*cp++ = 'S';
	res[4] = '\0';
	return (res);
}

int
upr()
{
	int vmsize, rmsize;

	vmsize = mproc.p_size;
	rmsize = mproc.p_rssize;
	if (mproc.p_swrss)
		rmsize += mproc.p_swrss/mproc.p_maxrss;
	if (nflg)
		printf("%4d ", mproc.p_uid);
	else
		printf("%-8.8s ", ud[getunam(mproc.p_uid)].name);
	printf("%5d %4.1f %4.1f%5d%5d", mproc.p_pid,
		FIX_TO_DBL(mproc.p_pctcpu)*100.0, 
		(double)(mproc.p_rssize*100.0)/maxmem, vmsize, rmsize);
}

vpr()
{

	printf(" %2d %2d %6d %5d %5d",
	   mproc.p_slptime > 99 ? 99 : mproc.p_slptime,
	   mproc.p_time > 99 ? 99 : mproc.p_time, get_ru_majflt(),
	   mproc.p_size, mproc.p_rssize);
	if (maxmem == (BSD43_RLIM_INFINITY/NBPP))
		printf("    xx");
	else
		printf(" %5d", maxmem);
	tsiz();
	printf(" %4d %3d %4.1f %4.1f", tsz, trs, (double)mproc.p_pctcpu,
		(double)((mproc.p_rssize*100.0)/maxmem));
}

int
tsiz()
{
 	preg_t	p, *prp;
	reg_t	p2;
	long	v_offset;

	tsz = 0; trs = 0;
	/* Save current 'mproc' offset */
	v_offset = lseek(memfd, 0, 1);
	do {
		l_lseek(memfd, (long)(mproc.p_region), 0);
		r_read(memfd, (char *)&p, (int)sizeof(p));
		prp = &p;
		if (prp->p_type == PT_TEXT) {
			l_lseek(memfd, ((long)p.p_reg), 0);
        		r_read(memfd, (char *)&p2, (int)sizeof(p2));
	   		/* Locate process table */
			l_lseek(memfd, v_offset, 0);
			tsz = p2.r_pgsz;
			trs = p2.r_nvalid;
			return;
		}
	} while (p.p_reg);
	l_lseek(memfd, v_offset, 0);	   /* Locate process table */
        return;
}

int
get_ru_majflt()
{
long	uaddr_offset;

	uaddr_offset = (UADDR - ((long) &u));
#ifdef U_BSD43_EXTENSION
	if (u.u_bsd43_extension_p != NULL) {
	    if (fix_address((caddr_t) &(u.u_bsd43_extension_p),
                    (caddr_t) &u,
                    uaddr_offset) < 0)
        	return(0);
	};
#endif U_BSD43_EXTENSION
	return(u.u_ru.ru_majflt);
}

int
fix_address(loc,base,offset)
        caddr_t *loc;
        caddr_t base;
        long    offset;
{
        *loc -= offset;
        if (*loc < base ||
            *loc >= (base + (USIZE * NBPP)))
                return(-1);
        return(0);
}
