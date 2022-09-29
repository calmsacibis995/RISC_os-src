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
#ident	"$Header: startup.c,v 1.84.1.15.1.5.1.4 91/01/09 19:25:24 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/param.h"
#include "sys/types.h"
#include "sys/psw.h"
#include "sys/conf.h"
#include "sys/boot.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/firmware.h"
#include "sys/sysmacros.h"
#include "sys/immu.h"
#include "sys/nvram.h"
#include "sys/pcb.h"
#include "sys/systm.h"
#include "sys/pfdat.h"
#include "sys/dir.h"
#include "sys/signal.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/reg.h"
#include "sys/limits.h"
#include "sys/utsname.h"
#include "sys/var.h"
#include "sys/tuneable.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/rambo.h"
#ifdef PROFILING
#include "sys/prof.h"
struct phdr phdr;
extern char eprol[], etext[];
#endif PROFILING
#include	"bsd43/sys/msgbuf.h"
#include "sys/kmem.h"
#include "sys/sysmips.h"
#include "sys/dkio.h"
#include "sys/ctype.h"
#include "sys/fs/fifo.h"

int 	pmsgbuf_initialized   = 0;
int	uarea_tlb_initialized = 0;	/* don't touch U-area yet! */

extern uint cp0_error_imask;

extern char *prom_version;	/* prom "version" value			    */
int prom_has_version; 		/* true if proms have "version" environment */
				/* variable defined (prom version >= 4.0)   */
int icache_size;
int dcache_size;
int scache_size;
char *mem_test_max=(char *)K0SIZE;	/* Max memory to look for
					 * during memory scan.
					 * This may be set by memory board
					 * config rtns.  This var is set
					 * to the default.
					 */
char *memory_limit=(char *)K0SIZE;	/* upper bound for mem_test_max
					 * (settable by command string)
					 */
unsigned int icachemask;
unsigned int dcachemask;
unsigned int scachemask;
int	syssegsz;		/* Size of kernel virtual map in clicks. */

extern (*level0_init)();	/* Lboot'able level 0 init routine */
extern int omachine_type;	/* Possibly wrong machine type */
extern int wrong_kernel;	/* Flag to tell us if wrong kernel being boot */
extern char *MACHINE_NAME[];	/* Machine names */
extern int cpu_config;

/*
 * Initialize exception handling vectors
 */
extern VEC_syscall(), VEC_cpfault(), VEC_trap(), VEC_int(), VEC_tlbmod();
extern VEC_tlbmiss(), VEC_breakpoint(), VEC_machine_check(), VEC_unexp();
extern VEC_addrerr(), VEC_ibe(), VEC_dbe(), VEC_dblword_noncached();
int  (*causevec[16])() = {
	/*  0: EXC_INT */		VEC_int,
	/*  1: EXC_MOD */		VEC_tlbmod,
	/*  2: EXC_RMISS */		VEC_tlbmiss,
	/*  3: EXC_WMISS */		VEC_tlbmiss,
	/*  4: EXC_RADE */		VEC_addrerr,
	/*  5: EXC_WADE */		VEC_addrerr,
	/*  6: EXC_IBE */		VEC_ibe,
	/*  7: EXC_DBE */		VEC_dbe,
	/*  8: EXC_SYSCALL */	 	VEC_syscall,
	/*  9: EXC_BREAK */		VEC_breakpoint,
	/* 10: EXC_II */		VEC_trap,
	/* 11: EXC_CPU */		VEC_cpfault,
	/* 12: EXC_OV */		VEC_trap,
	/* 13: EXC_TRAP */		VEC_trap,
	/* 14: EXC_DBL_NC */		VEC_dblword_noncached,
	/* 15: EXC_CHECK */		VEC_machine_check
};

extern baerror(), cerror(), adderr(), uerror(), uaerror(), softfp_adderr(),
	reviderror(), softfp_insterr(), fixade_error();
int  (*nofault_pc[NF_NENTRIES])() = {
	/* unused */		0,
	/* NF_BADADDR */	baerror,
	/* NF_COPYIO */		cerror,
	/* NF_ADDUPC */		adderr,
	/* NF_FSUMEM */		uerror,
	/* NF_USERACC */	0,/*uaerror*/
	/* NF_SOFTFP */		softfp_adderr,
	/* NF_REVID */		reviderror,
	/* NF_SOFTFPI */	softfp_insterr,
	/* NF_FIXADE */		fixade_error
};

int	utlbhndlr = UTLBHNDLR_3BUG;	/* utlbmiss handler type */

/* 
 * Routine called before main, to initialize various data structures.
 */

/*
 * Machine-dependent startup code.  The boot stack is still in use.
 * The kernel sp in proc 0 U area is setup on return.
 */
mlsetup(argc, argv, environ)
int argc;
char *argv[];
char *environ[];
{
	unsigned fpage;	/* first free physical page */
	extern char end[], edata[];
#if defined(SABLE_PC_SAMPLING) || defined(R6000_BUG_FLUSH)
	extern char etext[];	/* for 'prof' */
	extern char exception_hook[], eexception_hook[];
#endif SABLE_PC_SAMPLING
	extern char utlbmiss[], eutlbmiss[];
	extern char exception[], eexception[];
	extern char exception_r3030[], eexception_r3030[];
	extern char *id_string;

	splall();			/* Safety */

	/* fp unit regs may not be cleared and by doing this we stave off
	 *	any possibility of stray interrupts
	 */
	clear_fp_unit();

	/*
	 * copy down exception vector code
	 */
	bcopy(utlbmiss, UT_VEC, eutlbmiss - utlbmiss);
#if defined(SABLE_PC_SAMPLING) || defined(R6000_BUG_FLUSH)
	bcopy(exception_hook, E_VEC, eexception_hook - exception_hook);
	icachemask = etext-eutlbmiss;	/* force a reference to symbols */
#else
	/*
	 * initially machine_type will be correct for a Rx3030, so we
	 * can do the copy of the correct exception vector here. If
	 * by chance we are running on a different machine either
	 * the sanity_machine_type will pick it up, or we will crash,
	 * depending upon what is at the RAMBO_COUNTER address.
	 */
	if (IS_R3030)
		bcopy(exception_r3030, E_VEC, eexception_r3030-exception_r3030);
	else
		bcopy(exception, E_VEC, eexception - exception);
#endif SABLE_PC_SAMPLING

#ifndef SABLE
	bzero(edata, end - edata);	/* zero bss */
#endif SABLE
	config_cache();
	icachemask = (icache_size - 1) >> PNUMSHFT;
	dcachemask = (dcache_size - 1) >> PNUMSHFT;
	scachemask = (scache_size - 1) >> PNUMSHFT;
	flush_cache();

	/* for dbx */
	up = &u;

	sanity_machine_type();	/* Initial machine type */
#ifndef SABLE
	/* get console=? argument only, needed for console_init on R?2030 */
	/* needed special case because getargs may make cmn_err calls. */
	con_getargs(argc, argv, environ);
#endif

/*
 * Without this calls to printf won't work
 */
	prt_where = PRW_CONS|PRW_BUF;
	console_init();

	/*
	 * Enable CPU parity errors
	 */
	reset_cp0_error(cp0_error_imask);

	/*
	 * Quick check to see if user has booted correct kernel
	 */
	if (wrong_kernel) {
		printf("\n***\n");
		printf("*** Attempting to boot a %s kernel on a %s system!\n",
			MACHINE_NAME[omachine_type-1], MACHDEP(MACHINE_NAME));
		printf("*** Please boot the correct kernel for this system!\n");
		printf("***\n\n");
	}

#ifndef SABLE
	getargs(argc, argv, environ);
#endif
	video_console_init();
	fpage = btoc(K0_TO_PHYS((unsigned)end)); /* first page phys mem */
	fpage = xprinit(fpage);	/* Initialize xpr buffer */
#ifdef PROFILING
        /* get some memory for the profiling routines to use */
        s_textsize = 0;
	phdr.sample_hz = HZ;
        phdr.lowpc = (char *)
            ROUNDDOWN((unsigned)eprol, HISTFRACTION*sizeof(HISTCOUNTER));
        s_lowpc = (unsigned int)phdr.lowpc;
        phdr.highpc = (char *)
            ROUNDUP((unsigned)etext, HISTFRACTION*sizeof(HISTCOUNTER));
        s_textsize = (int)phdr.highpc - (int)phdr.lowpc;
        phdr.pc_bytes = (s_textsize / HISTFRACTION);
        phdr.bb_bytes = (s_textsize / HISTFRACTION);
        printf("Profiling kernel, textsize=%d(%d) [%x..%x]\n",
                phdr.pc_bytes, s_textsize, phdr.lowpc, phdr.highpc);
#define PC_SAMPLES
#ifdef PC_SAMPLES
	/* this buffer is for pc samples */
        phdr.pc_buf = (char *)PHYS_TO_K1(ctob(fpage));
	bzero( phdr.pc_buf, phdr.pc_bytes );
	fpage += btoc(phdr.pc_bytes);
        printf("got pc_buf at %x\n", phdr.pc_buf);
#endif PC_SAMPLES
#define BB_COUNTS
#ifdef BB_COUNTS
	/* this buffer is for bb counts */
        phdr.bb_buf = (char *)PHYS_TO_K1(ctob(fpage));
        _mcountoff = (int)phdr.bb_buf;
	bzero( phdr.bb_buf, phdr.bb_bytes );
	fpage += btoc(phdr.bb_bytes);
        printf("got bb_buf at %x\n", phdr.bb_buf);
#endif BB_COUNTS
#endif PROFILING

#ifdef MIPS_LOCAL
	/*
	 * Print id string.
	 */
	printf("\n%s\n\n", id_string);
#endif

	tlbinit(fpage);		/* initialize tlb, physmem, etc */
	coproc_find();
	if(showconfig && strcmp(utsname.m_type, MACHDEP(MACHINE_NAME))) {
		printf("System: %s configured as %s\n",
			MACHDEP(MACHINE_NAME), utsname.m_type);
	}
	if (IS_R2400) {
		if (*(ushort *)PHYS_TO_K1(SCR) & SCR_PAREN)
			printf("Parity ENABLED\n");
		else
			printf("Parity DISABLED\n");
	}
#ifdef R6000
	showconfig_r6000_boards();	/* Show SBC chip info */
#endif R6000
	/* can't do this until u area set up */
	(*level0_init)();	/* Call appropriate level 0 init routine */
				/* lboot'able in master.d/kernel.* */

	config_delay();		/* Configure delay macro factor */

	spl0();			/* lower IPL for autoconfig. */
}

/*
 * tlbinit:
 *	Initialize tlb, physmem, etc. The kernel now
 * has itself mapped virtual to physical from address 0x100000 to the end
 * end of bss.
 */
extern int malloc_limit;
extern int mbuf_limit;

tlbinit(fpage)
unsigned fpage;
{
	register i;
	extern int		syssegmisc;	/* miscellaneous sysseg */
	extern VEC_nofault(), VEC_dbe();
	extern  (*causevec[])();
	register unsigned	nextclick;
	extern int		firstfree;	/* First free click.	*/
	extern int		maxfree;	/* Last free click + 1.	*/
	extern int		limitmem;	/* limited on command line */
	char			*saved_mem_test_max;

	/*
	 * Invalidate entire tlb (including wired entries)
	 */
#ifdef R6000
	flush_tlb();
#else
	for (i = 0; i < NTLBENTRIES; i++)
		invaltlb(i);
#endif !R6000

	/* KLUDGE ALERT! */
	/* The 4.0 version of proms leaves a few words of memory
	 * uninitialized near the prom stack.  This causes a bus
	 * error and chaos during dumps.
	 * This "temporary" kludge writes into them and circumvents the problem.
	 */
	if (IS_R2300 ||
	    IS_R2400 ||
	    IS_R3030 ||
	    IS_R3200) {
		wbadaddr(PHYS_TO_K1(0xfff4),4);
		wbadaddr(PHYS_TO_K1(0xfff8),4);
		wbadaddr(PHYS_TO_K1(0xfffc),4);
	};

	/*
	 * Turn parity on for m/120 and Rx3030
	 */
	if (IS_R2400) {
		if (disable_parity) {
			*(ushort *)PHYS_TO_K1(SCR) &= ~SCR_PAREN;
			wbflush();
		} else {
			if (!(*(ushort *)PHYS_TO_K1(SCR) & SCR_PAREN)) {
				if (showconfig)
					printf("Parity DISABLED, enabling now!\n");
				*(ushort *)PHYS_TO_K1(SCR) |= SCR_PAREN;
				wbflush();
			}
		}
	} else if (IS_R3030) {
		register volatile struct rambo *rambo =
				((struct rambo *)PHYS_TO_K1(RAMBO_BASE));
		if (disable_parity) {
			rambo->rambo_creg &= ~ENABLEPARITY;
			wbflush();
			printf("Parity Disabled\n");
		} else {
			/*
			 * enable parity for the Rx3030 borads (By default the
			 * parity is diabled on power up)
			 */
			rambo->rambo_creg |= ENABLEPARITY;
			wbflush();
			printf("Parity Enabled\n");
		}
	}

	if (limitmem)
		mem_test_max = (char *) min((int)mem_test_max,limitmem);
	saved_mem_test_max = mem_test_max;
	lmem_setmax();	/* lbootable reset of maxmem */
	if (saved_mem_test_max < mem_test_max)
		mem_test_max = saved_mem_test_max;
	/*
	 * Size memory
	 */
	if (IS_R3030) {
		/*
		 * The Rx3030 does its memory differently to the other machines.
		 * we cannot detect that a memory read or write has failed,
		 * as we receive NO exceptions or interrupts.
		 * instead; we have to do a write to memory and see when
		 * the write wraps around.
		 *
		 * Since the Rx3030 can have either 1MBit or 4Mbit parts
		 * we have to determine which we have, then size.
		 */
		i = size_r3030_memory(min(memory_limit,mem_test_max));
	} else {
		causevec[EXC_DBE>>CAUSE_EXCSHIFT] = VEC_nofault;
	
		for (i = fpage; i < btoc(min(memory_limit,mem_test_max)); i++) {
		/*
		 * Unlike VAX version, use "wbadmemaddr" to avoid parity
		 * errors! This isn't possible on Jupiter because a parity
		 * error is the only means of detecting that memory isn't
		 * available.
		 */
			int	saved_word;
	
			/*
			 * save the old value of the word, if possible
			 */
			if (badaddr(PHYS_TO_K1((unsigned)ctob(i)),4))
				saved_word = 0;
			else
				saved_word =
				 *((int *) (PHYS_TO_K1((unsigned)ctob(i))));
			/*
			 * check that we can write the word
			 */
			if (IS_I2000) {
			    if (wbadmemaddr (PHYS_TO_K1((unsigned)ctob(i))))
			      break;
			}
			else {
			    if (wbadaddr(PHYS_TO_K1((unsigned)ctob(i)),4))
				    break;
			}		
			/*
			 * For partially-populated ECC memories,
			 * see that we can read it without error.
			 */
			if (!IS_I2000 && badaddr(PHYS_TO_K1((unsigned)ctob(i)),4))
				break;
			/*
			 * restore the original value of the word 
			 * (so that bsd43_pmsgbuf can be used if this is a 
			 * warm start)
			 */
			*((int *) (PHYS_TO_K1((unsigned)ctob(i)))) = saved_word;
		}
		causevec[EXC_DBE>>CAUSE_EXCSHIFT] = VEC_dbe;
	}
	maxmem = physmem = freemem = i;

	/*	No user page tables available yet.
	 */
	
	ptfree.pf_next = &ptfree;
	ptfree.pf_prev = &ptfree;

	/*	Set up memory parameters.
	*/
	i = physmem;
	if(v.v_maxpmem  &&  (v.v_maxpmem < i))
		i = v.v_maxpmem;
	physmem = i;
	maxclick = i;

	/*
	 * Initialize error message buffer (at end of core).
	 */
	maxclick -= btoc(sizeof(struct bsd43_msgbuf));
	maxmem = freemem = maxclick;
	bsd43_pmsgbuf = (struct bsd43_msgbuf *)PHYS_TO_K0((unsigned)ctob(maxclick));
	if (bsd43_pmsgbuf->msg_magic != BSD43_MSG_MAGIC) {
		bsd43_pmsgbuf->msg_magic = BSD43_MSG_MAGIC;
		bsd43_pmsgbuf->msg_bufx = 
			bsd43_pmsgbuf->msg_bufr = 0;
#ifndef SABLE
		bzero(bsd43_pmsgbuf->msg_bufc,BSD43_MSG_BSIZE);
#endif
	};
	pmsgbuf_initialized = 1;

	firstfree = fpage;
	maxfree = maxclick;

	ASSERT(firstfree < maxfree);

#ifndef SABLE
	/*
	 * Clear all unused physical memory
	 */
	if (IS_R3030 && is_mono()) {
		/*
		 * On the Rx3030 we will clear all of memory EXCEPT for the
		 * mono chrome screen (if initialised)
		 */
		bzero(PHYS_TO_K1(ctob(firstfree)), mono_base() - PHYS_TO_K1(ctob(firstfree)));
		bzero(mono_base() + 128 * 1024,
			PHYS_TO_K1(ctob(maxfree)) - (mono_base() + 128 * 1024));
	} else {
		bzero(PHYS_TO_K1(ctob(firstfree)), ctob(maxfree - firstfree));
	}
#endif

	/*
	 *	Figure out how much kernel virtual memory to allocate,
	 *	and map initialize the map for it.  
	 *
	 *	Currently I determine space required by duplicating code
	 *	from heavy K2 users.  Would be better to ask them how much
	 *	they use, or use common initialization code.  
	 *
	 *	Main users:
	 *		kern_malloc
	 *		buffer cache	Enough to cover max possible buffers
	 *				at max buffer size.
	 */
	malloc_limit += FIFO_MNB;			/* FIFO requirements */
	syssegsz = 0;
	syssegsz += btoc(malloc_limit);			/* kern_allo.c*/
	syssegsz += btoc(mbuf_limit);			/* kern_mbuf.c*/
	syssegsz += btoc( bio_nbuf_init() * bio_maxbsize ); /* buffer cache. */
	syssegsz += btoc(syssegmisc);			/* misc  */
	syssegsz = min(syssegsz, SYSSEGSZ_MAX);

	mapinit(sptmap, v.v_sptmap);
	mfree(sptmap, syssegsz, btoc(K2SEG));

	/*	Note that reginit must be called before
	**	mktables because reginit calculates the
	**	value of pregpp which is used by mktables.
	*/

	reginit();
	
	/*	Allocate some kernel tables.
	*/

	nextclick = firstfree;
	nextclick = mktables(nextclick);

	/*	Initialize the physical memory manager.
	*/

	meminit(nextclick, maxclick);

	/*	Initialize process 0.
	*/

	p0init();
}


/*
 * Create system space to hold page allocation and
 * buffer mapping structures and hash tables
 */

mktables(nextfree)
{
	register int	m;
	register int	i;
	register preg_t	*prp;
	extern int	pregpp;

	/*
	 *	MIPS -- system tables are allocated in k0seg
	 *	instead of "system virtual space". They don't
	 *	grow or move or get paged or anything, so they
	 *	don't need to be virtual on the MIPS machine.
	 *	TODO -- the PFDAT array does not need to map the 
	 *	other tables allocated here. It's size can be reduced
	 *	(slightly) by allocating it last.
	 *
	 *	The RISC/os VM system, however, requires that pfdat
	 *	include entries for the pages in kptbl, in order to
	 *	implement the pdetoregion() function.
	 */

	/* allocate space for tables associated with the new 
	 * memory allocator.
	 */
	i = btoc(btoc(malloc_limit) * sizeof(kmemusage_t));
	kmemusage = (kmemusage_t *) PHYS_TO_K0(ctob(nextfree));
	nextfree +=i;

	/*	Allocate space for the pfdat.
	*/

	i = btoc((maxclick - nextfree) * sizeof(struct pfdat));
	pfdat = (struct pfdat *) PHYS_TO_K0(ctob(nextfree));
	nextfree +=i;
	
	/*	Allocate space for the kernel pages tables.
	 */

#ifdef FRAG_PTS
	/*
	 * Calculate number of page frags needed, then add
	 * space needed for pf_regions arrays.
	 */
#define	REGSPACE	(PFRAG / ((NBPC/PFRAG) * sizeof(reg_t *)))
	i = roundup(syssegsz * sizeof(pde_t), PFRAG) / PFRAG;
	i += roundup(i, REGSPACE) / REGSPACE;
	i = btoc(pfragtob(i));
#else
	i = btoc(syssegsz * sizeof(pde_t));
#endif
	kpbase = ctob(nextfree);
	kptbl = (pde_t *) PHYS_TO_K0(ctob(nextfree));
	nextfree +=i;

	/*	Compute the smallest power of two larger than
	 *	the size of physical memory.
	 */

	m = physmem;
	while (m & (m - 1))
		 m = (m | (m - 1)) + 1;
	phashmask = (m>>3) - 1;

	/*	Allocate space for the page hash bucket
	 *	headers.
	 */

	i = btoc((m >> 3) * sizeof(*phash));
	phash = (struct pfdat **) PHYS_TO_K0(ctob(nextfree));
	nextfree +=i;

	/*	Allocate space for the pregion tables for each process
	 *	and link them to the process table entries.
	 *	The maximum number of regions allowed for is process is
	 *	3 for text, data, and stack plus the maximum number
	 *	of shared memory regions allowed.
	 */
	
	i = btoc(pregpp * sizeof(preg_t) * v.v_proc);
	prp = (preg_t *) PHYS_TO_K0(ctob(nextfree));
	nextfree +=i;
	for(i = 0  ;  i < v.v_proc  ;  i++, prp += pregpp)
		proc[i].p_region = prp;
	
	/*
	 * Allocate kernel virtual space for window on u-block
	 * ( for forking and tracing )
	 */

#ifdef PERFECT_COLORING	
	win_ublk = (int *)sptalloc_scc(USIZE, 0, -1, 0, btoct(&u));
#else	
	win_ublk = (int *)sptalloc(USIZE, 0, -1, 0);
#endif PERFECT_COLORING	

	/*	Return next available physical page.
	*/

	return(nextfree);
}

/*	Set up proc0
*/

extern int	nofiles_min;
extern int	nofiles_max;
extern int	nofiles_cfg;

p0init()
{
	register int	ii;
	register pde_t	*pdptr;
	extern int	userstack[];
	register caddr_t *vptr;
	register int 	i;
#define DOUBLE_ROUNDUP(x) ((((int) (x)) + (sizeof(double) - 1)) & ~(sizeof(double) - 1))


	/*	Initialize proc 0's ublock
	*/

	winublock();
	if(uballoc(&proc[0]) == 0)
		cmn_err(CE_PANIC,"p0init uballoc");
	/*	not using win_ublk here	*/
	winubunlock();

	/* 
	 * Map u block using wired tlb entries
	 */

	pdptr = proc[0].p_ubptbl;
	vptr = proc[0].p_tlbhi_tbl;
	for(ii = 0; ii < USIZE; ii++, pdptr++, vptr++) {
		tlbwired(TLBWIREDBASE + ii, 0, *vptr, pdptr->pgi.pg_pde);
	};
	uarea_tlb_initialized = 1;	/* ok now to access uarea */

	/*
	 * Initialized wired tlb entries for proc 0
	 */
	u.u_tsize   = 0;
	u.u_dsize   = 0;
	u.u_ssize   = 0;
	u.u_ar0     = 0;
	u.u_procp   = &proc[0];
	u.u_cmask   = CMASK;
	u.u_limit   = CDLIMIT;

	setup_wired_tlb(&proc[0]);

	/* 
	 * Set-up pofile and ofile.
	 * These structures are of configurable size at the 
	 * end of the U-area and can't be set-up until the uarea exists.
	 * This is done in vstart on the 3b2.
	 */
	nofiles_cfg = v.v_nofiles;
	if(v.v_nofiles < nofiles_min) v.v_nofiles = nofiles_min;
	if(v.v_nofiles > nofiles_max) v.v_nofiles = nofiles_max;
	u.u_pofile = (char *)(u.u_ofile + v.v_nofiles);

	u.u_sav_pix = (char *)(u.u_pofile + v.v_nofiles);
	u.u_jmp_pix = (char *)(u.u_sav_pix + PIXIE_SIZE);

	u.u_bsd43_start = time;

	/*	Initialize vhand parameters
	 */
	vminit();

	/*
	 * Set up the initial limits on process VM.
	 * Set the maximum resident set size to be all
	 * of (reasonably) available memory.  This causes
	 * any single, large process to start random page
	 * replacement once it fills memory.
	 */
	for (i = 0; i < BSD43_RLIM_NLIMITS; i++)
		u.u_rlimit[i].rlim_cur = u.u_rlimit[i].rlim_max = 
		    BSD43_RLIM_INFINITY;
	u.u_rlimit[BSD43_RLIMIT_STACK].rlim_cur = 
		(unsigned) ((ctob((unsigned) tune.t_maxumem)) - 1);
	u.u_rlimit[BSD43_RLIMIT_STACK].rlim_max =
		(unsigned) ((ctob((unsigned) tune.t_maxumem)) - 1);
	u.u_rlimit[BSD43_RLIMIT_DATA].rlim_cur =
		(unsigned) ((ctob((unsigned) tune.t_maxumem)) - 1);
	u.u_rlimit[BSD43_RLIMIT_DATA].rlim_max =
		(unsigned) ((ctob((unsigned) tune.t_maxumem)) - 1);
	u.u_rlimit[BSD43_RLIMIT_RSS].rlim_cur =
		u.u_rlimit[BSD43_RLIMIT_RSS].rlim_max = 
			(unsigned) ((ctob((unsigned) (LOOPPAGES - desfree))) - 1);
	proc[0].p_maxrss = (LOOPPAGES - desfree);

	/*
	 * BUG ??? XXX ~~~ cannot limit a bsd43_write to a value
	 * larger than (2GB-2), but *can* limit a sysv_write to such
	 * a value.
	 */
	u.u_rlimit[BSD43_RLIMIT_FSIZE].rlim_cur = 
		u.u_rlimit[BSD43_RLIMIT_FSIZE].rlim_max = 
			((uint)(u.u_limit << SCTRSHFT) > BSD43_RLIM_INFINITY)
			  ? BSD43_RLIM_INFINITY
			  : u.u_limit << SCTRSHFT;

	/*
	 * Setup credentials.  We can't call crget() as it calls kmem_alloc()
	 * and it is too early to do that.  Instead, we simulate crget()
	 * here and use a static credential structure.
	 */
	{   extern int cractive;
	    static struct ucred cred0;
	    u.u_cred = &cred0;		/* already initialized to 0's */
	    crhold(u.u_cred);
	    cractive++;
	}

	for (i = 1; i < NGROUPS; i++)
		u.u_groups[i] = NOGROUP;

	/* TODO could put kernel stack at end of pofiles 
	 * if we want to; requires locore to use u_stack instead
	 * of constant. 
	 */

	/*	initialize process data structures
	*/

	curproc = &proc[0];
	curpri = 0;

	proc[0].p_size = USIZE;
	proc[0].p_stat = SRUN;
	proc[0].p_flag = SLOAD | SSYS;
	proc[0].p_nice = NZERO;
	proc[0].p_tlbpid = 0;

#undef DOUBLE_ROUNDUP
}

/*	Machine-independent startup code
*/

startup()
{
	/*	Check to see if the configured value of NOFILES
	**	was illegal and changed by vstart_s.  If so,
	**	print a message.  We do this here because
	**	vstart_s is in assembly language and hard to
	**	print from.
	*/

	if(nofiles_cfg != v.v_nofiles){
		if(nofiles_cfg < nofiles_min)
			cmn_err(CE_NOTE,
				"Configured value of NOFILES (%d) is less than min (%d)",
				  nofiles_cfg, nofiles_min);
		else
			cmn_err(CE_NOTE,
				"Configured value of NOFILES (%d) is greater than max (%d)",
				  nofiles_cfg, nofiles_max);
		cmn_err(CE_CONT, "        NOFILES set to %d.", v.v_nofiles);

	}

	cmn_err(CE_CONT,
		"\nRISC/os Release %s %s Version %s\n",
		utsname.release, utsname.machine, utsname.version);
	cmn_err(CE_CONT,
		"Total real memory  = %d\n", ctob(physmem));
	cmn_err(CE_CONT,
		"Available memory   = %d\n", ctob(freemem));

#ifndef SABLE
	prom_has_version = ( !IS_I2000 && strlen(prom_version) != 0 ) ? 1 : 0;
#endif !SABLE
	bsd43_hwconf_init();
	/* Install optimized utlbmiss handler (if we can) */
	install_fast_utlbhndlr();
}

extern struct bootdevtbl devlist[];
extern struct devtable *Devboot;

/*
 * Search for character "c" in string "s".  Return the position in "s" that
 * c is found, or -1 if its not found.
 */
static int
match(c, s)
	register char c;
	register char *s;
{
	register int i;

	i = 0;
	while (*s) {
		if (*s == c)
			return (i);
		s++;
		i++;
	}
	return (-1);
}

/*
 * Parse the users disk name into a dev_t.  Names have the format of
 *	<device-name-prefix><controller-number>d<drive-number>s<fs-number>
 *	For example:	ips0d0s0 means the "ips" device, controller 0, drive 0,
 *			fs 0
 * Return 0 if the name is bad, otherwise return the appropriate dev.
 *
 * WARNING: this assumes some knowledge about the layout of the minor device
 *	    for disk drivers.
 */
static dev_t
devparse(cp)
	register char *cp;
{
	int len;
	register struct bootdevtbl *bp;
	char *oldcp;
	int ctlr, drive, fs;
	int maxctlr, maxdrive;
	int mjrnmbr,realctlr;
	int i;
	char tmp[10];

	bp = &devlist[0];
	oldcp = cp;
        if (match(':', cp) != -1) {
                /* dev is nfs, return special cookie */
                return(-1);
        }

	while (bp->name) {
		len = strlen(bp->name);
		if (strncmp(bp->name, cp, len)) {
			bp++;
			continue;
		}

		if( bp->has_func() == 0 ){
		    printf("\"%s\" is not a supported device on an %s\n", 
			bp->name, utsname.m_type);
			return(0);
		}

		/* device prefix matched.  now parse the rest of the name */
		cp += len;

		/* parse controller */
		for( i=0; i < sizeof(tmp) - 1; i++,cp++){
		    if( match( *cp, "0123456789") != -1)
			tmp[i] = *cp;
		    else
			break;
		}
		tmp[i] = '\0';
		if( i == 0 )
		    goto bad;
		ctlr = atoi(tmp);

		maxctlr = Devboot->ncpermjr * Devboot->nmajr;
		if( ctlr < 0 || ctlr >= maxctlr ){
		    printf("Controller #%d.  Should be less than %d\n",
					ctlr, maxctlr);
		    goto bad;
		}

		/* parse drive */
		if (*cp != 'd')
			goto bad;
		cp++;
		for( i=0; i < sizeof(tmp) - 1; i++,cp++){
		    if( match( *cp, "0123456789") != -1)
			tmp[i] = *cp;
		    else
			break;
		}
		tmp[i] = '\0';
		if( i == 0 )
		    goto bad;

		drive = atoi(tmp);

		if( drive < 0 || drive >= Devboot->ndrives){
		    printf("Drive #%d.  Should be less than %d\n",
				drive, Devboot->ndrives);
		    goto bad;
		}

		/* parse fs */
		if (*cp != 's')
			goto bad;
		cp++;
		for( i=0; i < sizeof(tmp) - 1; i++,cp++){
		    if( match( *cp, "0123456789") != -1)
			tmp[i] = *cp;
		    else
			break;
		}
		tmp[i] = '\0';
		if( i == 0 )
		    goto bad;

		fs = atoi(tmp);

		if ( fs < 0 || fs > 15 ){
		    printf("Partition #%d.  Should be less than 16\n", fs);
		    goto bad;
		}
		if (*cp) {
			/* last char must be a null */
			goto bad;
		}
		
		mjrnmbr = Devboot->mult[ctlr/Devboot->ncpermjr];
		realctlr = ctlr%Devboot->ncpermjr;
		
		return (makedev(mjrnmbr, (realctlr << 6) | (drive << 4) | fs));
	
	}
	printf("Unknown device name.  Known device names are:\n");
	bp = &devlist[0];
	while (bp->name) {
		printf("%s ", bp->name);
		bp++;
	}
	printf("\n");
	return (0);

bad:
	printf("Invalid device name \"%s\"\n", oldcp);
	return (0);
}

devinit()
{
	extern char *arg_rootname;
	extern char *arg_root;
	extern char *arg_swap;
	extern char *arg_dump;
	extern char *arg_initfile;
	extern int nodev();
	extern int icode_file();		/* hack */
	char buf[80];
	dev_t newdev = 0;

	if (IS_R3030) {
		/* check to see if we are a pizazz 33 */
		if ((cpu_config >> P_SPEED_SHIFT) >= 30) {
			if (strcmp(utsname.m_type, MT_R3030_S) == 0) {
				strcpy(utsname.m_type, MT_R3030_33_S);
			} else {
				strcpy(utsname.m_type, MT_R3030_33);
			}
		}
	}

	/* The following hack allows us to define a new arg_rootname
	 * variable in the nvram to specify the default root device.
	 * This hack was needed to specify a new name so that old kernels
	 * would still boot with new proms, since nvram vars are ALWAYS
	 * defined.  "arg_root", if present, will override "arg_rootname".
	 */
	if ((arg_rootname) && (arg_rootname[0] != '0') && !(arg_root))
	  arg_root = arg_rootname;

	if (arg_root) {
		/*
		 * Use user supplied name for root device, or prompt for it,
		 * according to value of "arg_root"
		 * arg_root may have an integer 1 or the string "1", in
		 * both of those case we want to prompt for the root to use.
		 */
		if ( ((arg_root != (char *)1) && 
			!((arg_root[0] == '1') && (arg_root[1] == '\0'))) ) {
			if (strcmp(arg_root, "swap") == 0) {
				/* put root on the swap device */
				newdev = swapdev;
			} else {
				/* use user supplied name */
				newdev = devparse(arg_root);
			}
		}
		while (!newdev) {
			printf("Root disk: ");
			gets(buf);
			newdev = devparse(buf);
		}
		rootdev = newdev;
		pipedev = rootdev;
		if ( ((newdev & 0xf) == 0) && !arg_swap )
			swapdev = rootdev + 1;
		else if (!arg_swap) {
			swapdev = rootdev;
		} else
			swapdev = devparse(arg_swap);
		if (rootdev != -1)
                        printf("Root on dev 0x%x,", rootdev);
                else
                        printf("Root on nfs file %s,", arg_root);
                if (swapdev != -1)
                        printf(" Swap on dev 0x%x\n", swapdev);
                else
                        printf(" Swap on nfs file %s", arg_swap);

                /* make dumpdev and vp please XXX */
		dumpdev = swapdev;
	}

        if (arg_swap && !arg_root) {
                swapdev = devparse(arg_swap);
                if (swapdev != -1)
                        printf("Swap on dev 0x%x\n", swapdev);
                else
                        printf("Swap on nfs file %s", arg_swap);
        }

	if (arg_initfile) {
		/*
		 * Use user supplied name for init program, or prompt for it,
		 * according to value of "initfile"
		 */
		if (arg_initfile != (char *)1) {
			if (strlen(arg_initfile) < 32) {
				/* name is okay - copy to icode */
				strcpy((char *) icode_file, arg_initfile);
			} else {
				printf("Filename for init is too long.\n");
				arg_initfile = 0;
			}
		} else
			arg_initfile = 0;

		while (arg_initfile == 0) {
			printf("Init file: ");
			gets(buf);
			if (strlen(buf) < 32) {
				strcpy((char *) icode_file, buf);
				break;
			} else
				printf("Filename for init is too long.\n");
		}
		printf("Using \"%s\" instead of /etc/init.\n",
			      (char *) icode_file);
	}

	/*
	 * If system doesn't know the size of the swap area, and the
	 * device has a size function, query the device for the size
	 * of the swap partition.
	 */
	if ((nswap == 0) && (swapdev != -1) &&
	    (bdevsw[bmajor(swapdev)].d_size != nodev)) {
		nswap = (*bdevsw[bmajor(swapdev)].d_size)(swapdev) -
				swplo;
	}
	if (showconfig)
		printf("Swapping on dev 0x%x, %dK bytes\n",
				 swapdev, (nswap * NBPSCTR) / 1024);
}

gets (p)
	register char *p;
{
	register int c;
	register char *base = p;

	while (1) {
		c = console_getc();
		switch (c) {
		      case '\r':
		      case '\n':
			console_putc('\r');
			console_putc('\n');
			*p = 0;
			goto done;

		      case '\b':
			p--;
			erase_one_char();
			break;

		      case '@':
			while (p > base) {
				erase_one_char();
				p--;
			}
			p = base;
			break;

		      default:
			*p++ = c;
			console_putc(c);
		}
	}
done:
	return;
}

				
static
erase_one_char()
{
	console_putc('\b');
	console_putc(' ');
	console_putc('\b');
}

#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/vnode.h"
#include "sys/pathname.h"
#include "sys/uio.h"
#include "sys/socket.h"
#include "sys/socketvar.h"
#include "bsd/netinet/in.h"

#include "../../../bsd_lib/libc/rpc/types.h"
#include "../../../bsd_lib/libc/rpc/xdr.h"
#include "../../../bsd_lib/libc/rpc/auth.h"
#include "../../../bsd_lib/libc/rpc/clnt.h"

#include "../../../bsd_lib/libc/rpc/pmap_rmt.h"
#include "../../../bsd_lib/libc/rpc/pmap_prot.h"
#include "../../../bsd_lib/librpcsvc/bootparam.h"

#include "sys/fs/nfs.h"
#include "sys/fs/nfs_clnt.h"
#include "sys/fs/nfs_rnode.h"

#ifndef NFSCLIENT
#define NFSCLIENT 1 /* To get nfs_args structure definition */
#endif NFSCLIENT
#include "sys/mount.h"

#include "bsd/net/if.h"
#include "bsd/net/route.h"

#include "../../../bsd_lib/librpcsvc/mount.h"
#include "sys/bootconf.h"
#include "sys/ctype.h"

#include "bsd/net/if.h"
#include "bsd/net/soioctl.h"           /* Original "ioctl.h" */


struct sockaddr_in root_sin;
struct sockaddr_in swap_sin;
extern struct sockaddr_in nfsdump_sin;
char *root_path;
char *swap_path;
char *dump_path;
char root_hostname[MAXHOSTNAMELEN+1];
char swap_hostname[MAXHOSTNAMELEN+1];
char dump_hostname[MAXHOSTNAMELEN+1];
struct pathname rpn;
struct pathname spn;
struct pathname dpn;
extern struct in_addr gblipaddr;

/*
 * Called by devinit to establish remote/local root and swap files.
 */
int
chk_bootparams()
{
	int rc;
	dev_t newdev;
	extern char *arg_root;
	extern char *arg_swap;
	extern char *arg_dump;
	char buf[80];

	/* do this BEFORE getfile which causes xid stamps to be initialized */
	inittodr(-1L);          /* hack for now - until we get time svc? */

	if (!arg_root) {
		/* get root file from bootparams */
		pn_alloc(&rpn);
		root_path = rpn.pn_path;
		do {
			rc = getfile("root", root_hostname,
			    (struct sockaddr *)&root_sin, root_path);
		} while (rc == ETIMEDOUT);
		if (rc) {
			pn_free(&rpn);
			return (rc);
		}
		rootdev = -1;
	} else if (rootdev == -1) {
		/* command line specified root */
		pn_alloc(&rpn);
		root_path = rpn.pn_path;
		parse_bootnames(arg_root,root_hostname,&root_sin,root_path);
	}
	/* check for local file specification by bootparams or command line. */
	if ( root_sin.sin_addr.s_addr == gblipaddr.s_addr ) {
		newdev = devparse(root_path);
		while (!newdev) {
			printf("Root disk: ");
			gets(buf);
			newdev = devparse(buf);
		}
		rootdev = newdev;
		pipedev = rootdev;
		if (((newdev & 0xf) == 0) && !arg_swap) {
			swapdev = rootdev + 1;
		}
	}

	if ( !arg_swap ) {
		pn_alloc(&spn);
		swap_path = spn.pn_path;
		do {
			rc = getfile("swap", swap_hostname,
			    (struct sockaddr *)&swap_sin, swap_path);
		} while (rc == ETIMEDOUT);
		if (rc) {
			pn_free(&spn);
			return (rc);
		}
		swapdev = -1;
	} else if ( swapdev == -1 ) {
		pn_alloc(&spn);
		swap_path = spn.pn_path;
		parse_bootnames(arg_swap,swap_hostname,&swap_sin,swap_path);
	}
	/* check for local file specification by bootparams or command line. */
	if ( swap_sin.sin_addr.s_addr == gblipaddr.s_addr ) {
		newdev = devparse(swap_path);
		while (!newdev) {
			printf("Swap disk: ");
			gets(buf);
			newdev = devparse(buf);
		}
		swapdev = newdev;
	}

	if ( !arg_dump ) {
		pn_alloc(&dpn);
		dump_path = dpn.pn_path;
		do {
			rc = getfile("dump", dump_hostname,
			    (struct sockaddr *)&nfsdump_sin, dump_path);
		} while (rc == ETIMEDOUT);
		if (rc) {
			pn_free(&dpn);
			return (rc);
		}
		dumpdev = -1;
	} else if ( dumpdev == -1 ) {
		pn_alloc(&dpn);
		dump_path = dpn.pn_path;
		parse_bootnames(arg_dump,dump_hostname,&nfsdump_sin,dump_path);
	}
	/* check for local file specification by bootparams or command line. */
	if ( nfsdump_sin.sin_addr.s_addr == gblipaddr.s_addr ) {
		newdev = devparse(dump_path);
		while (!newdev) {
			printf("Dump disk: ");
			gets(buf);
			newdev = devparse(buf);
		}
		dumpdev = newdev;
	}
	return(0);
}
parse_bootnames(bootarg,boothost,bootsin,bootpath)
char *bootarg;
char *boothost;
char *bootpath;
struct sockaddr_in *bootsin;
{
	register int bp;
	struct in_addr inet_addr();

	printf("parse_bootnames: %s\n",bootarg);
	if ( (bp = match(':',bootarg)) == -1 ) {
		cmn_err(CE_WARN,"parse_bootnames argument error %s\n",
							bootarg);
		cmn_err(CE_WARN,"should be internet_addr:file_name\n");
	}
	strncpy(boothost,bootarg,bp);
	boothost[bp] = '\0';
	strcpy(bootpath,&bootarg[bp+1]);
	bootsin->sin_addr = inet_addr(boothost);
	bootsin->sin_family = AF_INET;
	printf("parse_bootnames: host=%s,path=%s,sin=%x\n",
			boothost,bootpath,bootsin->sin_addr.s_addr);
}
