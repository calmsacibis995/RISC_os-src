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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: space.c,v 1.15.1.3.1.1.1.2 90/10/17 18:15:08 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/param.h"
#include "sys/immu.h"
#include "sys/sema.h"
#include "sys/acct.h"

struct	acct	acctbuf;
struct	vnode	*acctp;

#include "sys/fixpoint.h"

fix avenrun[3];		/* Load averages a la BSD. */
fix sq_avenrun[3];	/* Like avenrun, but with square decay, not 1/e. */
ufix loadavg[6];	/* Like avenrun, but more values. */

#include "sys/buf.h"

struct	buf	bfreelist[BQUEUES];	/* head of various buf free lists */
struct	pfree	pfreelist;		/* Head of physio buffer headers */

struct vnode	*rootdir;	/* Inode for root directory. */


int	nptalloced;	/* Total number of page tables allocated.	*/
int	nptfree;	/* Number of free page tables.			*/

#include "sys/region.h"

reg_t	ractive;
int	ractive_count;
reg_t	rfree;
reg_t	sysreg;

#include "sys/sbd.h"
#include "bsd/sys/time.h"
#include "sys/proc.h"

proc_t	*runq;		/* Head of linked list of running procs. */
proc_t	*curproc;	/* The currently running proc.		 */
int	curpri;		/* Priority of currently running proc.	 */

#include "sys/pfdat.h"

struct pfdat	*pclearp;	/* Points to last free page cleared. */
struct pclearstat pclearstat;	/* Statistics on pages cleared in idle() */
struct pfdat	phead;		/* Head of free page list. */
struct pfdat	*pfdat;		/* Page frame database. */
struct pfdat	**phash;	/* Page hash access to pfdat. */
struct pfdat	ptfree;		/* Head of page table free list. */
int		phashmask;	/* Page hash mask. */

#include "sys/sysinfo.h"

struct sysinfo sysinfo;
struct	syswait	syswait;
struct	syserr	syserr;
struct	dinfo	dinfo;		/* DU perf stat's */
struct  minfo minfo;

#include	"sys/swap.h"

int	remote_acct;		/* REMOTE flag for acctp */
int	bootstate;		/* DU boot flag */
short	dufstyp;		/* DU file system switch index */

swpt_t	swaptab[MSFILES];
int	nextswap;


/*	Each process has 3 pregions (text, data, and stack) plus
 *	enough for the maximum number of shared memory segments.
 *	We also need one extra null pregion to indicate the end
 *	of the list.  The maximum number of shared memory segments
 *	will be added in reginit.
 */

int	pregpp = 3 + 1;


/*	The following describe the physical memory configuration.
**
**		maxclick - The largest physical click number.
**			   ctob(maxclick) is the largest physical
**			   address configured plus 1.
**
**		physmem	 - The amount of physical memory configured
**			   in clicks.  ctob(maxclick) is the amount
**			   of physical memory in bytes.
**		kpbase	 - The physical address of the start of
**			   the allocatable memory area.  That is,
**			   after all kernel tables are allocated.
**			   Pfdat table entries are allocated for
**			   physical memory starting at this address.
**			   It is always on a page boundary.
*/

int	maxclick;
int	physmem;
int	kpbase;
int	pagesize = NBPP;


/*	The following are concerned with the kernel virtual
**	address space.
**
**		kptbl	- The address of the kernel page table.
**			  It is dynamically allocated in startup.c
*/

pde_t	*kptbl;

/*	The following space is concerned with the configuration of
**	NOFILES.  We have to do it this hard way because the code
**	which actually does this is in the assembly language file
**	misc.s and can't include header files or conveniently
**	print things.
**
**	The variable nofiles_cfg is assigned a value here to force
**	it to be in the data region rather than bss.  The value
**	will be overwritten by the code at vstart_s in misc.s.
**	However, this occurs before bss is cleared so that if
**	this variable were in bss, the value written by vstart_s
**	would be cleared in mlsetup when bss is cleared.
*/

int	nofiles_min = NOFILES_MIN;
int	nofiles_max = NOFILES_MAX;
int	nofiles_cfg = 1;		/* Originally configured value. */

/*	The following space is referenced by the R6000 routines, which are
**	generally lboot'ed, and by the sduart driver, which is common.  We
**	formally define the symbols here, and everyone else declares them as
**	'extern' of undefined length.
*/

#include "sys/bc.h"

int  ioa_ctlspace_vaddr   [SBC_SLOT_UPB];
int  memory_ctlspace_vaddr[SBC_SLOT_UPB];
uint memory_base_paddr	  [SBC_SLOT_UPB];	/* phys addr of each board */
uint memory_board_mbsize  [SBC_SLOT_UPB];	/* size in MBytes */
