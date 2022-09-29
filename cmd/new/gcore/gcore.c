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
#ident	"$Header: gcore.c,v 1.1.1.6 90/06/05 16:49:48 wje Exp $"

/*
 * gcore - get core images of running processes
 *
 * Author: Girish Goyal
 *
 * This code uses /proc file system (or interface) to obtain information 
 * about running processes and generate a core dump.
 */

#include <stdio.h>
#include <nlist.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/sysmacros.h>
#include <sys/pcb.h>
#include <sys/sbd.h>
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/signal.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/fs/prfcntl.h>
#include <sys/message.h>

#undef	USIZE

/*
 * Create a core image on the file "core"
 *
 * It writes USIZE block of the
 * user.h area followed by the entire
 * data+stack segments.
 */

unsigned	page_size;
extern int	getpagesize();

caddr_t	userstack = (caddr_t)0x7ffff000;

union userdata {
	struct user user;
	char upages[8192];
} *userp;
#define u	userp->user
#define uarea	userp->upages

#define	NPSEGS	100

struct pfcseg psegs[NPSEGS];
int	npsegs;
char	errmsg[256];
char	fname[256];
char	*coref = NULL;

int	errflg;
int	debug;
char	*corepath = "core";

main(argc, argv)
	int argc;
	char **argv;
{
	int	fd;			/* input (/proc file) descriptor */
	int	ofd;			/* output (core file) descriptor */
	struct proc p;
	register int i, j;
	int 	pid;

	extern int	optind;
	extern int getopt();
	extern char *optarg;
	int	c;




	/* Process all options */
	while ((c = getopt(argc, argv, "#p:")) != EOF) {
	    switch(c) {
		case 'p':   corepath = optarg;
			    break;

		case '#':   debug++;
			    break;

		default:    errflg++;
	    }
	}
	if (errflg || optind >= argc) {
		printf("Usage: gcore [-p path] process-id ...\n", argv[0]);
		exit(1);
	}

	page_size = getpagesize();

	/* allocate memory to form core file names */
	if ((coref = (char *)malloc(strlen(corepath)+64)) == NULL) {
		fprintf(stderr, "Can't allocate memory for core file name.\n");
		exit(1);
	}

	for (i = optind; i < argc; i++) {
		printf("%s:\t", argv[i]);
		fflush(stdout);
		if ((pid = atoi(argv[i])) < 0) {
		    fprintf(stderr, "Invalid process-id.\n");
		    continue;
		}
		sprintf(fname, "/proc/%d", pid);
		if ((fd = open(fname,0)) < 0) {
			perror(fname);
			continue;
		}
		if (fcntl(fd, PFCGETPR, &p) == -1) {
		    perror("Can't read proc structure ");
		    close(fd);
		    continue;
		}
		if (p.p_flag & SSYS) {
		    fprintf(stderr, "System process.\n");
		    close(fd);
		    continue;
		}
		if (p.p_stat == SZOMB) {
		    fprintf(stderr, "Zombie process.\n");
		    close(fd);
		    continue;
		}
		if (p.p_flag & SWEXIT)
		    printf("Warning: process exiting.  ");

		sprintf(coref, "%s.%d", corepath, pid);
		if ((ofd = creat(coref, 0644)) < 0) {
		    sprintf(errmsg,"Can't create %s ",coref);
		    perror(errmsg);
		    close(fd);
		    continue;
		}
		if (debug)
			printf("Dumping to %s\n", coref);
		if (mkcore(pid, fd, ofd) >= 0) {
			printf("Dumped to %s\n", coref);
			fflush(stdout);
		}
		close(fd);
		close(ofd);
	}
	exit(0);
}


struct pfcseg *
findpfcseg(pregtype)
{
	register int	i;

	for (i = 0; i < npsegs; i++)
	    if (psegs[i].seg_pregtype == pregtype)
		return(&psegs[i]);
	return(NULL);
}

#undef	USIZE
#undef	ctob
#undef	btoc
#undef	poff

#define	btoc(x)		((x + page_size - 1)/page_size)
#define	ctob(x)		(x * page_size)
#define	poff(x)		((unsigned)x % page_size)

mkcore(pid, fd, ofd)
{
	struct vnode *vp;
	register preg_t	*prp;
	register proc_t *pp;
	register int	gap;
	caddr_t stack_base;		/* Low address of user's stack. */
	int stack_size;			/* Size of user's stack in bytes. */
	unsigned int	offset;		/* Stores position into file */
	struct pfcseg *psegp;
	struct pfcseg useg;

	/* Get uarea info */
	if (fcntl(fd, PFCGETUSEG, &useg) == -1) {
		perror("Can't get uarea info (PFCGETUSEG)");
		return (-1);
	}
	if ((userp = (union userdata *)malloc(useg.seg_size)) == NULL) { 
		fprintf(stderr,"Can't allocate #d bytes for uarea.\n",
			useg.seg_size);
		return (-1);
	}

	/* First of all, obtain user process virtual map */
	psegs[0].seg_vaddr = 0;
	for (npsegs = 0, psegp = &psegs[0], psegp->seg_vaddr = 0; 
			psegp < &psegs[NPSEGS]; psegp++, npsegs++) {
		if (fcntl(fd, PFCGETSEG, psegp) == -1)
		    break;
		(psegp+1)->seg_vaddr = psegp->seg_vaddr + psegp->seg_size;
		if (debug > 1)
		    printf("\tvaddr:%08x size:%08x regtype:%d pregtype:%d\n",
			psegp->seg_vaddr, psegp->seg_size, psegp->seg_regtype,
			psegp->seg_pregtype);
	}
	if (npsegs <= 0) {
		fprintf(stderr, "No virtual space to dump.\n");
		return (-1);
	}

	/* Read u-area first */
	if (lseek(fd, (long)useg.seg_vaddr, 0) == -1L 
	    || read(fd, uarea, useg.seg_size) != useg.seg_size) {
		sprintf(errmsg, "Can't lseek/read uarea ");
		perror(errmsg);
		return(-1);
	}


	/*	Put the region sizes into the u-block for the
	 *	dump.
	 */
	if (psegp = findpfcseg(PT_TEXT))
		u.u_tsize = btoc(psegp->seg_size);
	else
		u.u_tsize = 0;
		
	/*	In the following, we do not want to write
	**	out the gap but just the actual data.  The
	**	caluclation mirrors that in loadreg and
	**	mapreg which allocates the gap and the
	**	actual space separately.  We have to watch
	**	out for the case where the entire data region
	**	was given away by a brk(0).
	*/

	if (psegp = findpfcseg(PT_DATA)) {
		if (psegp->seg_size > 0)
		    u.u_dsize = btoc(psegp->seg_size);
	} else {
		u.u_dsize = 0;
	}

	if (psegp = findpfcseg(PT_STACK)) {
		stack_base = psegp->seg_vaddr;
		if ((stack_base + psegp->seg_size) > userstack)
		    stack_size = userstack - psegp->seg_vaddr;
		else
		    stack_size = psegp->seg_size;
	} else {
		stack_base = 0;
		stack_size = 0;
	}

#if later
#undef	dtop
#undef	NDPP
#define	NDPP	(page_size/512)
#define	dtop(x)	 ((x+NDPP-1)/NDPP)
	/*	Check the sizes against the current ulimit and
	**	don't write a file bigger than ulimit.  If we
	**	can't write everything, we would prefer to
	**	write the stack and not the data rather than
	**	the other way around.
	*/

#define	USIZE	btoc(useg.seg_size)
	if (USIZE + u.u_dsize + btoc(stack_size) > 
					(uint)dtop(u.u_limit)) {
		u.u_dsize = 0;
		if (USIZE + btoc(stack_size) > 
			   (uint)dtop(u.u_limit))
			stack_size = 0;
	}

	u.u_error = 0;
	if (USIZE + u.u_dsize + btoc(stack_size) >
			btoc(u.u_rlimit[BSD43_RLIMIT_CORE].rlim_cur)) {
		return(-1);
	};
#endif
	
	u.u_error = 0;
	u.u_syscall = DUCOREDUMP;

	/* Write uarea */
	if (debug)
	    printf("\twriting uarea (0x%x bytes) to offset: 0\n",useg.seg_size);
	if (write(ofd, uarea, useg.seg_size) != useg.seg_size) {
		sprintf(errmsg, "Can't write uarea ");
		perror(errmsg);
		return(-1);
	}

	/* Write data */
	offset = useg.seg_size;
#if later
	u.u_acflag |= ACORE;		/* SysV does this after first write */
#endif
	if (u.u_dsize) {
	    if (copydata(ofd, offset, fd, (caddr_t)u.u_exdata.ux_datorg, 
		(int)ctob(u.u_dsize) - poff(u.u_exdata.ux_datorg)) < 0) {
		perror("Can't write user data ");
		if (debug)
		    printf(errmsg);
		return(-1);
	    }
	    offset += ctob(u.u_dsize) - poff(u.u_exdata.ux_datorg);
	    offset = ctob(btoc(offset));
	}

	/* Write stack */
	if (stack_size)
	    if (copydata(ofd, offset, fd, stack_base, stack_size) < 0) {
		perror("Can't write user stack ");
		if (debug)
		    printf(errmsg);
		return(-1);
	    }
	return(0);
}

/* local buffer to copy data */
char	mybuf[0x80000];

copydata(ofd, offset, fd, addr, size)
caddr_t	addr;
long	offset;
int	ofd, fd, size;
{
	register int	cnt;

	if (debug)
	    printf("\tcopying 0x%x bytes from addr 0x%x to offset: 0x%x\n",
		size, addr, offset);
	while (size > 0) {
	    cnt = (size > sizeof(mybuf)) ? sizeof(mybuf) : size;
	    if (lseek(fd,addr,0) == -1L || (cnt=read(fd,mybuf,cnt)) < 0) {
		sprintf(errmsg,"Can't read 0x%x bytes from addr 0x%x\n", 
			cnt, addr);
		return(-1);
	    }
	    if (lseek(ofd,offset,0) == -1L || write(ofd,mybuf,cnt) != cnt) {
		sprintf(errmsg, "Can't write 0x%x bytes to offset 0x%x\n", 
					    cnt, offset);
		return(-1);
	    }
	    offset += cnt;
	    addr += cnt;
	    size -= cnt;
	}
	return(0);
}

