#ident "$Header: kernel.c,v 1.3.1.3.1.2 90/07/11 18:19:18 hawkes Exp $"

/*
 *  Top - a top users display for Berkeley Unix
 *  
 *  This file contains all the routines that retrieve values from
 *  kernel and user memory.
 */

#include <stdio.h>
#ifndef RISCOS
#if defined(FOUR_ONE) || defined(pyr)
#include <sys/pte.h>
#else
#include <machine/pte.h>
#endif
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#if defined(scs)
# define FLOAT		/* for pcrcpu in proc.h */
# include <sys/vm.h>	/* for struct spt */
#endif
#include <sys/proc.h>
#else RISCOS
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/signal.h>
#include <sys/sbd.h>
#include <sys/pcb.h>
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/termio.h>
#include <sys/fixpoint.h>
#include <sys/var.h>
#include <sys/sysinfo.h>
#endif RISCOS

#include "top.local.h"

/* useful externals */
extern int errno;
extern char *sys_errlist[];

#ifdef RISCOS
extern int Usize, pagesize;
extern int pte_to_pfn_shift;
extern int pfn_to_byte_shift;
#define pte_to_byte(x)	(((x) >> pte_to_pfn_shift) << pfn_to_byte_shift)
#endif RISCOS

static int kmem = -1;
static int mem = -1;

init_kernel()
{
    /* open kmem and mem */
    if ((kmem = open(KMEM, 0)) < 0)
    {
	perror(KMEM);
	exit(20);
    }
    if ((mem = open(MEM, 0)) < 0)
    {
	perror(MEM);
	exit(21);
    }
}

#if !defined(scs)

/*
 *  getu(p, u) - get the user structure for the process whose proc structure
 *	is pointed to by p.  The user structure is put in the buffer pointed
 *	to by u.  Return 0 if successful, -1 on failure (such as the process
 *	being swapped out).
 */

#ifdef RISCOS
fix_address(loc,base,offset)
	caddr_t	*loc;
	caddr_t	base;
	long	offset;
{
	*loc -= offset;
	if (*loc < base ||
	    *loc >= (base + (Usize * pagesize))) 
		return(-1);
	return(0);
}
#endif RISCOS


getu(p, u)

register struct proc *p;
struct user *u;

{
    register caddr_t upage;
#ifndef RISCOS
    struct pte uptes[UPAGES];
    register struct pte *pte;
#else RISCOS
    pde_t *pde;
    long  uaddr_offset;
#endif RISCOS
    register nbytes, n;

    /*
     *  Check if the process is currently loaded or swapped out.  The way we
     *  get the u area is totally different for the two cases.  For this
     *  application, we just don't bother if the process is swapped out.
     */
#ifndef RISCOS
    if (!(p->p_flag & SLOAD))
    {
	return(-1);
    }
#endif RISCOS

    /*
     *  Process is currently in memory, we hope!
     */
#ifndef RISCOS
    if (!getkval(p->p_addr, uptes, sizeof(uptes), "!p->p_addr"))
    {
	/* we can't seem to get to it, so pretend it's swapped out */
	return(-1);
    } 
#endif RISCOS
    upage = (caddr_t)u;
#ifndef RISCOS
    pte = uptes;
    for (nbytes = sizeof(struct user); nbytes > 0; nbytes -= NBPG)
    {
    	lseek(mem, pte++->pg_pfnum * NBPG, 0);
	n = MIN(nbytes, NBPG);
	if (read(mem, upage, n) != n)
	{
	    /* we can't seem to get to it, so pretend it's swapped out */
	    return(-1);
	}
	upage += n;
    }
#else RISCOS
    pde = ubptbl(p);
    for (nbytes = (Usize * pagesize); nbytes > 0; nbytes -= pagesize, pde++)
    {
    	lseek(mem, pte_to_byte(pde->pgi.pg_pde), 0);
	n = MIN(nbytes, pagesize);
	if (read(mem, upage, n) != n)
	{
	    /* we can't seem to get to it, so pretend it's swapped out */
	    return(-1);
	}
	upage += n;
    }
    uaddr_offset = (UADDR - ((long) u));
#ifdef U_BSD43_EXTENSION
    if (fix_address((caddr_t) &(u->u_bsd43_extension_p),
		    (caddr_t) u,
		    uaddr_offset) < 0)
	return(-1);
#endif U_BSD43_EXTENSION
    if (fix_address(&(u->u_pofile),
		    (caddr_t) u,
		    uaddr_offset) < 0)
	return(-1);
    if (fix_address((caddr_t) &(u->u_ar0),
		    (caddr_t) u,
		    uaddr_offset) < 0)
	return(-1);
#endif RISCOS
    return(0);
}
#endif !scs

/*
 *  getkval(offset, ptr, size, refstr) - get a value out of the kernel.
 *	"offset" is the byte offset into the kernel for the desired value,
 *  	"ptr" points to a buffer into which the value is retrieved,
 *  	"size" is the size of the buffer (and the object to retrieve),
 *  	"refstr" is a reference string used when printing error meessages,
 *	    if "refstr" starts with a '!', then a failure on read will not
 *  	    be fatal (this may seem like a silly way to do things, but I
 *  	    really didn't want the overhead of another argument).
 *  	
 */

getkval(offset, ptr, size, refstr)

long offset;
int *ptr;
int size;
char *refstr;

{
    if (lseek(kmem, offset, 0) == -1)
    {
	if (*refstr == '!')
	{
	    refstr++;
	}
	fprintf(stderr, "%s: lseek to %s: %s\n",
	    KMEM, refstr, sys_errlist[errno]);
	quit(22);
    }
    if (read(kmem, ptr, size) == -1)
    {
	if (*refstr == '!')
	{
	    /* we lost the race with the kernel, process isn't in memory */
	    return(0);
	} 
	else 
	{
	    fprintf(stderr, "%s: reading %s: %s\n",
		KMEM, refstr, sys_errlist[errno]);
	    quit(23);
	}
    }
    return(1);
}

#if defined(scs)
void
get_spt(spti, sptp)
	int		spti;
	struct spt	*sptp;
{
	extern struct spt	*spt;		/* defined in top.c */

	/* Let's get tricky, shall we?  *spt is a pointer in kernel space;
	 * we can use it in a getkval() call, but we can't dereference it
	 * directly.  However, C's pointer arithmetic will still work, even
	 * though we aren't going to use the pointer as a pointer...
	 */

	getkval(spt + spti, sptp, sizeof(struct spt), "spt");
}
#endif scs
