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
#ident "$Header: machdep.c,v 1.16 90/11/08 13:26:24 hawkes Exp $"

/*
 * machdep.c -- machine dependent prom routines
 */

#include "sys/types.h"
#include "sys/cmn_err.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/saio.h"
#include "machine/i8254clock.h"
#include "machine/hd146818.h"
#include "machine/param.h"
#include "machine/i8042.h"
#include "machine/rambo.h"
#include "saio/setjmp.h"
#include "prom/prom.h"
#include "prom/entrypt.h"
#include "saio/debug.h"

/*
 * nofault -- jmp_buf pointer for fault handling
 */
extern int *nofault;
jmp_buf fault_buf;
extern caddr_t frm_addr;

#ifdef PROM
extern int	machine_type;
extern unsigned total_memsize;

/*
 * mem_map is an array of structures that map a picture of what the
 * memory in this system looks like.  Each element of the array has
 * a start, which is the starting address of this chunk, and size
 * which is the size in mytes of this chunk.  The first element will
 * more always start at zero.  The size of the first chunk will be
 * return in memsize
 */
struct {
	u_int start;			/* Start address */
	u_int size;		      	/* Size in bytes */
} mem_map[MAP_SIZE];		    	/* No more than MAP_SIZE holes */

int mem_is_sized;			/* whether r3030 mem had been sized */
int memlimit;				/* memory size of r3030 */
int noclmem;

#ifdef RC6280

#include "machine/ioa.h"

#define MBYTES		(1024*1024)
#define PAGE_SIZE	16384

unsigned
clear_memory()
{
	int	region_index;
	int	ctlspace;
	u_int	brd_size;
	u_int	next_mem, phys_addr, chunk_size, todo_size, byte_offset;

	for (region_index = 0; region_index < MAP_SIZE; region_index++) {
	    mem_map[region_index].start = 0;	/* initialize */
	    mem_map[region_index].size  = 0;
	}

	/*
	 *  Find all the Memory boards, and determine the physical
	 *  address(es) and size(s) of the main memory region(s).
	 */
	region_index = 0;
	next_mem     = 0;
	while (1) {
	    if (next_mem == K1_TO_PHYS(IOA3)) {
		/*
		 *  Memory boards don't impinge upon IOA space.  If we're
		 *  up to the base of the lowest IOA, then jump beyond the
		 *  highest IOA.  Assume that IOA is 64MB.
		 */
		next_mem = K1_TO_PHYS(IOA1) + 64*MBYTES;
	    }
	    if (((ctlspace = find_ctlspace_r6000( next_mem/MBYTES )) == 0) || 
		((brd_size = decode_brd_size(ctlspace)) == 0)) {
		break;
	    }
	    if ( (mem_map[region_index].start + mem_map[region_index].size)
			== next_mem ) {
		/*
		 *  This Memory is contiguous to the previous range.  Append.
		 */
		mem_map[region_index].size += brd_size;
	    } else {
		/*
		 *  This Memory is discontiguous.  Start another region.
		 */
		region_index++;
		mem_map[region_index].start = next_mem;
		mem_map[region_index].size  = brd_size;
	    }
	    next_mem += brd_size;
	} /* while:  keep searching for more Memory boards */

	/*
	 *  Clear physical memory to zero
	 */
	flushall_tlb();
	total_memsize = 0;
	region_index = 0;
	while ( (region_index < MAP_SIZE) && (mem_map[region_index].size) ) {

	    phys_addr = mem_map[region_index].start;
	    todo_size = mem_map[region_index].size;
	    total_memsize += todo_size;

	    if (phys_addr < K0SIZE) {
		/*
		 *  This region can be directly addressed by K1 addresses.
		 *  Use the simple algorithm to clear memory.
		 */
		if (phys_addr < K1_TO_PHYS(PROM_STACK)) {
		    /*
		     *  This is the typical case:  the first Memory base
		     *  address is zero, but we don't want to clear low
		     *  memory -- it's being used!  Start higher.
		     */
		    phys_addr = K1_TO_PHYS(PROM_STACK);
		    todo_size -= phys_addr - mem_map[region_index].start;
		}
		clear_memory_r6000( PHYS_TO_K1(phys_addr)
#ifdef SABLE
				   ,8*16384 );	    /* for practice */
#else
				   ,todo_size );    /* num bytes to clear */
#endif !SABLE
	    } else {
		/*
		 *  This region can only be addressed by mapped KUseg or K2
		 *  addresses.  Set up contiguous KUseg tlb entries for 32MB
		 *  chunks, and clear 32MB at a time.  That's a handy size
		 *  because it's how much S-cache side of tlb entries can
		 *  address.
		 *  Assume addresses start on a page boundary and regions
		 *  are page-multiple in size.
		 */
		while (todo_size) {
		    chunk_size = (todo_size >= 32*MBYTES)
					     ? 32*MBYTES : todo_size;
		    /* set up the tlb entries */
		    for (byte_offset = 0; byte_offset < chunk_size;
			 byte_offset += PAGE_SIZE) {
			tlbwired( 0
				 ,KUBASE + byte_offset
				 ,PHYS_TO_TLB_R6000(phys_addr + byte_offset)
				   | TLB_V_R6000 | TLB_N_R6000 | TLB_D_R6000 );
		    }
		    /* clear the memory */
		    clear_memory_r6000( KUBASE
#ifdef SABLE
				       ,8*16384 );	/* for practice */
#else
				       ,chunk_size );	/* num bytes to clear */
#endif !SABLE
		    phys_addr += chunk_size;
		    todo_size -= chunk_size;
		} /* while still clearing chunks of the region */
		flushall_tlb();
	    }

	    region_index++;
	}

	return( mem_map[0].size );	/* memsize of first region */
}

#else !RC6280

#define MAX_3230_MEM	0x8000000	/* 128 MB max */

/*
 * clear_memory -- clear and size memory
 * returns size of memory
 */
volatile int *lastip;

unsigned
clear_memory()
{
	register volatile int *ip;
	int *first_ip;
	register int i;
	int pgsize, pgofset;
	int max_basic_memsize;		/* addressed by K0/K1 */
	extern char _fbss[];
	jmp_buf pop_buf;
	extern int *nofault;
	extern char *getenv();

	for (i = 0; i < MAP_SIZE; i++) {
	    mem_map[i].start = 0;	/* initialize */
	    mem_map[i].size  = 0;
	}

	/*
	 * establish the page size
	 */
	pgsize = 4096;
	pgofset = pgsize-1;
	max_basic_memsize = 256*1024*1024;		/* 256 MBytes */

#ifdef R3030
	if (IS_R3030) {		/* 3230 board */
	    if (mem_is_sized)
		memlimit = mem_is_sized;
	    else memlimit = size_r3030_memory(MAX_3230_MEM);
	}
	frm_addr = 0;
#endif

	/*
	 * clear the tlb
	 */
	flushall_tlb();

	ip = first_ip = (int *)PROM_STACK;
	lastip = 0;

	/*
	 * clear by pages.  Note: This code doesn't deal with holes, so
	 * only mem_map[0] is used.  But mem_map is now an array for
	 * possible further implementations
	 */

	for (ip = (int *)((unsigned)ip & ~pgofset); 
	     ip < (int *)(K1BASE + max_basic_memsize);
	     ip += pgsize/sizeof(*ip)) {
		if (wbadaddr(ip, sizeof(int *)) ) {
		    /*
		     * Found end of memory
		     */
		    break;

		} else {
		    /*
		     * Due to a bug in the Mitsubishi drams on the M2000,
		     * when the rams are written to the first time, it may
		     * not 'take'.  This is fine since the next time it will
		     * take.  However, the drams are arranged in such a way
		     * that a write to an odd address must also occur so
		     * that any subsequent writes 'take'.
		     */
		    if( IS_R3200 || IS_RB3125 )
			*(ip +1)= 0x0;

		    /*
		     * If on a 16Mb boundary, see if board is only half 
		     * populated.  'wbadaddr' doesn't return an error if
		     * writing to the unpopulated memory part of a board.
		     * So we must write to memory and then read to either
		     * generate an exception or make sure that what was
		     * read is the same as what was written.
		     */
		     if( (IS_R3200 || IS_RB3125) && (((int)ip % 0x1000000) == 0) ){
			if( setjmp(pop_buf)){
			    /* 
			     * Got an exception, so at end of memory
			     */
			    sa_spl();
			    nofault = 0;
			    break;
			}

			nofault = pop_buf;
			*ip = 0xdeadbeef;
			if( *ip != 0xdeadbeef ){
			    /*
			     * Wasn't the same as what was written, so
			     * at end of memory
			     */
			    nofault = 0;
			    break;
		 	}
		    }
		    /*
		     * If we finally made it here, then there's memory, so
		     * clear it out.
		     */
	    	    lastip = ip;
#ifndef SABLE
		    clearpage( ip,pgsize );
#endif !SABLE
		}
	}

	mem_map[0].size = K1_TO_PHYS((int)lastip) + pgsize;
	total_memsize   = mem_map[0].size;	/* only one region */
	return((unsigned)(mem_map[0].size));	/* memsize */
}
#endif !RC6280
#endif PROM

/*
 * get_memory -- read memory location as indicated by flags
 * ??? SHOULD THIS FLUSH CACHE BEFORE DOING READ ???
 */
unsigned
get_memory(caddr, width)
unsigned caddr;
int width;
{
	switch (width) {

	case SW_BYTE:
		return (*(unsigned char *)caddr);

	case SW_HALFWORD:
		return (*(unsigned short *)caddr);

	case SW_WORD:
		return(*(unsigned *)caddr);
	
	default:
		prom_error("get_memory: Illegal width");
	}
}

/*
 * set_memory -- set memory location
 */
set_memory(caddr, width, value)
unsigned caddr;
int width, value;
{
	switch (width) {

	case SW_BYTE:
		*(unsigned char *)caddr = value;
		break;

	case SW_HALFWORD:
		*(unsigned short *)caddr = value;
		break;

	case SW_WORD:
		*(unsigned *)caddr = value;
		break;
	
	default:
		prom_error("set_memory: Illegal switch");
	}
	clear_cache(caddr, width);
}

extern int RT_CLOCK_ADDR[];

is_nvvalid()
{
	register volatile struct rt_clock *rt = 
		(struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	extern char *get_nvram();

	if (IS_R2300 && ((rt->rt_regd & RTD_VRT) == 0)) {
		_errputs("nv ram lost battery backup\n");
		set_nvram(NVADDR_STATE, NVLEN_STATE, "\0");
	}

	return(*get_nvram(NVADDR_STATE, NVLEN_STATE) & NVSTATE_RAMVALID);
}

set_nvvalid()
{
	char newstate[2];
	extern char *get_nvram();

	newstate[0] = *get_nvram(NVADDR_STATE, NVLEN_STATE) | NVSTATE_RAMVALID;
	newstate[1] = 0;
	set_nvram(NVADDR_STATE, NVLEN_STATE, newstate);
}


#ifdef PROM
#ifdef R3030
/*
 * the memory system on the RX3030 is different to all other machines
 * currently in the MIPS lineup. We have to firstly determine whether
 * we have 4MB simms of 1MB simms, and then we can size memory to see
 * how much has been inserted. If we have as a baseline, 4MB simms we
 * will also check to see if a set of 1MB simms are inserted at the
 * beginning of the next memory slice. We use an Intel 8042 controller
 * to hold the size of the simms.
 */

/*
 * the basic strategy we follow is this:
 *
 * Assume we have 4MB simms. If we do then we have atleast 32MB of
 * memory. If we don't then we have atleast 8MB of memory. So, if
 * we have 1MB simms, but we assume we have 4MB simms then the 8MB
 * is replicated 4 times, once at 0MB, once at 8MB, once at 16, and
 * once at 24MB.
 *
 * So, set up for 4MB simms, and see if memory is replicated. If it
 * is then we have 1MB simms, if not we have 4MB simms.
 *
 * After setting up whether we have 1 or 4 MB simms we then step through
 * memory to see how many simms we have. If we are checking 4MB simms we
 * also check to see if we have one set of 1MB simms inserted after the
 * 4MB simms.
 * This allows The following valid memory configurations:
 *
 * Size of Memory	# 4MB simms	# 1 MB simms
 *	  8 MB			-		8
 *	 16 MB			-		16
 *	 24 MB			-		24
 *	 32 MB			-		32
 *	 32 MB			8		-
 *	 40 MB			8		8
 *	 64 MB			16		-
 *	 72 MB			16		8
 *	 96 MB			24		-
 *	104 MB			24		8
 *	128 MB			32		-
 *
 */

#define	pattern1	0x55aaff00	/* patterns for memory testing */
#define	pattern2	0xc33ca55a
#define	pattern3	0x11111111
#define	pattern4	0x44444444
#define	pattern5	0xff00cc33

#define P_4SIMMS	(1 << 6)	/* 4 Mb simms installed (0 = 1Mb) */

int		simms_type;
static	int	rambo_parity;

size_r3030_memory(max_bytes)
long	max_bytes;
{
	register int			mb;	/* size of memory currently */
	register unsigned long		t0, t1, t2, t3, t4;	/* temps */
	register int			step;		/* increment */
	register volatile unsigned long *p1, *p2;	/* pointers for steps */
	int				s;		/* spl holder */
	register volatile struct rambo *rambo = ((struct rambo *)PHYS_TO_K1(RAMBO_BASE));

	rambo_parity = rambo->rambo_creg;
	rambo->rambo_creg = rambo_parity & ~ENABLEPARITY;
	rambo->rambo_ch[1].dma_mode = FLUSH_RAMBO_FIFO | CLR_DMA_ERR;
	wbflush();
	if (pkbdset4() == 0) {		/* try to set up 4MB simms in I8042 */
		/*
		 * keyboard set up failed, assume 8mb anyway,
		 * that will work nomatter what.
		 */

		cmn_err(CE_WARN, "Keyboard Controller Failed Interaction, assuming 8Mb\n");
		return (0x800000);
	}
	p1 = (volatile unsigned long *)PHYS_TO_K1((unsigned)0);
	p2 = (volatile unsigned long *)PHYS_TO_K1((unsigned)0x800000);
	t1 = *p1;
	t2 = *p2;
	*p1 = pattern1;
	*(p1 + 1) = pattern3;
	*p2 = pattern2;
	*(p2 + 1) = pattern4;
	if ((pattern1 == *p1) && (pattern2 == *p2) &&
	    (pattern3 == *(p1 + 1)) && (pattern4 == *(p2 + 1))) {
		/*
		 * we have 4MB simms installed
		 */
		simms_type = P_4SIMMS;
		mb = step = 0x2000000;
	} else if ((pattern2 == *p1) && (pattern2 == *p2) &&
	    (pattern4 == *(p1 + 1)) && (pattern4 == *(p2 + 1))) {
		/*
		 * we have 1MB simms installed
		 */
		simms_type = 0;
		mb = step = 0x800000;
		if (pkbdset1() == 0) {		/* set up 1MB simms in I8042 */
			cmn_err(CE_WARN, "Keyboard Controller Failed Interaction, assuming 8Mb\n");
			return (0x800000);
	}
	} else {
		cmn_err(CE_PANIC,
			"Memory failure -\n0: (%x) != (%x)\n4: (%x) != (%x)\n",
			*p1, *p2, *(p1 + 1), *(p2 + 1));
	}
	*p2 = t2;	/* restore */
	*p1 = t1;

	/*
	 * we now step through memory looking at the start of blocks
	 * to see if they are there. If not, we assume that we have exceeded
	 * memory size. Step is the size of memory chunks.
	 */
	p1 = (volatile unsigned long *)PHYS_TO_K1(step);
	p2 = (volatile unsigned long *)PHYS_TO_K1(0);
	t0 = *p2;	/* save location 0 */
	for(; mb < max_bytes; mb += step) {
		t1 = *p1;
		t2 = *(p1 + 1);
		t3 = *(p1 + 2);
		t4 = *(p1 + 3);
		*p1       = pattern1;
		*(p1 + 1) = pattern2;
		*(p1 + 2) = pattern3;
		*(p1 + 3) = pattern4;
		/*
		 * we write to a well defined location to ensure that
		 * the data buss is not holding the last value by
		 * parasitic capacitance.
		 */
		*p2 = pattern5;
		wbflush();
		if ((*p1 != pattern1) ||
		    (*(p1 + 1) != pattern2) ||
		    (*(p1 + 2) != pattern3) ||
		    (*(p1 + 3) != pattern4) ||
		    (*p2 != pattern5))
			break;	/* at end of memory */
		*p1       = t1;
		*(p1 + 1) = t2;
		*(p1 + 2) = t3;
		*(p1 + 3) = t4;
		/*
		 * yes, there is a bank here, if we are checking 4Mb simms
		 * this might be the one allowable 1Mb bank, check if this is
		 * so, and stop if it is. (We only allow contiguous memory
		 * currently, this will change shortly)
		 */
		if (step == 0x2000000) {
			register volatile unsigned long *p3;

			p3 = p1 + (0x800000 / sizeof(unsigned long));
			t3 = *p3;
			t1 = *p1;
			if (t3 == t1) {
				/*
				 * if this fails then we know we have a 4Mb
				 * bank installed here. It this succeeds then
				 * we have to do more testing!
				 */
				t2 = *(p1 + 1);	/* save the others too */
				t4 = *(p3 + 1);
				*p1 = pattern1;
				*(p1 + 1) = pattern2;
				*p3 = pattern3;
				*(p3 + 1) = pattern4;
				*p2 = pattern5;
				wbflush();
				if ((*p1 != pattern1) ||
				    (*(p1 + 1) != pattern2) ||
				    (*p3 != pattern3) ||
				    (*(p3 + 1) != pattern4) ||
				    (*p2 != pattern5)) {
					/*
					 * a failure has occured, we assume
					 * that there was no memory here, or
					 * if there was it is faulty. we
					 * therefore assume that this bank was
					 * a 1Mb bank and be done with it!
					 */
					mb += 0x800000;
					*p1 = t1;
					*(p1 + 1) = t2;
					*p3 = t3;
					*(p3 + 1) = t4;
					break;
				}
				*p1 = t1;
				*(p1 + 1) = t2;
				*p3 = t3;
				*(p3 + 1) = t4;
			}
		}
		p1 += (step / sizeof(unsigned long));
	}
	*p2 = t2;
	rambo->rambo_ch[1].dma_mode = CHANNEL_EN | AUTO_RELOAD;
	rambo->rambo_creg = rambo_parity;
	return (mb);
}

#include "sys/kbd.h"

struct	kyboard {
	unsigned long	ky_data_w;
	unsigned long	ky_cmd_w;
};

#define	K_BASE 0x19000000
static volatile struct kyboard *ky_ptr = (struct kyboard *)PHYS_TO_K1(K_BASE);

#define KBD_DELAY	20

static
ky_cmd(ky, x)
volatile struct kyboard *ky;
int	x;
{
	DELAY(KBD_DELAY);
	while (ky->ky_cmd_w & KYS_IBF)
		DELAY(1);
	ky->ky_cmd_w = (x);
}

static
ky_out(ky, x)
volatile struct kyboard *ky;
int	x;
{
	DELAY(KBD_DELAY);
	while (ky->ky_cmd_w & KYS_IBF)
		DELAY(1);
	ky->ky_data_w = (x);
}

static
ky_in_ready(ky)
volatile struct kyboard *ky;
{
	DELAY(KBD_DELAY);
	return ((ky->ky_cmd_w & KYS_OBF) != 0);
}

static
ky_in(ky)
volatile struct kyboard *ky;
{
	DELAY(KBD_DELAY);
	while ( !(ky->ky_cmd_w & KYS_OBF) )
		DELAY(1);
	return (int)ky->ky_data_w;
}

pkbdset4()
{
	/*
	 * set up for 4MB simms in I8042
	 */

	unsigned long	t1;
	long	count = 0;

	while(ky_in_ready(ky_ptr))
		t1 += ky_in(ky_ptr);
	ky_cmd(ky_ptr, KYC_STEST);
	if ((ky_in(ky_ptr) & 0xff) != 0x55) {
		/*
		 * we failed self test, maybe there was data banked up
		 */
		for (count = 0; count < 1000; count++)
			if (ky_in_ready(ky_ptr))
				break;
		if (!ky_in_ready(ky_ptr) || ((ky_in(ky_ptr) & 0xff) != 0x55))
			return 0;
	}
	for(t1 = 0; ((t1 & 4) == 0) && (count < 10); count++) {
		ky_cmd(ky_ptr, 0xd0);
		t1 = ky_in(ky_ptr);
		ky_cmd(ky_ptr, 0xd1);
		ky_out(ky_ptr, t1 | 0x05);
		ky_cmd(ky_ptr, 0xd0);
		t1 = ky_in(ky_ptr);
		if ((t1 & 4) == 0) {
			cmn_err(CE_WARN, "Cannot set keyboard controller to 4Mb, retrying...\n");
		}
	}
	return ((t1 & 4) != 0);
}

pkbdset1()
{
	/*
	 * set up for 1MB simms in I8042
	 */

	unsigned long	t1;
	long	count = 0;

	while(ky_in_ready(ky_ptr))
		t1 += ky_in(ky_ptr);
	ky_cmd(ky_ptr, KYC_STEST);
	if ((ky_in(ky_ptr) & 0xff) != 0x55) {
		/*
		 * we failed self test, maybe there was data banked up
		 */
		for (count = 0; count < 1000; count++)
			if (ky_in_ready(ky_ptr))
				break;
		if (!ky_in_ready(ky_ptr) || ((ky_in(ky_ptr) & 0xff) != 0x55))
			return 0;
	}
	for(t1 = 4; ((t1 & 4) == 4) && (count < 10); count++) {
		ky_cmd(ky_ptr, 0xd0);
		t1 = ky_in(ky_ptr);
		ky_cmd(ky_ptr, 0xd1);
		ky_out(ky_ptr, (t1 & ~0x04) | 1);
		ky_cmd(ky_ptr, 0xd0);
		t1 = ky_in(ky_ptr);
		if ((t1 & 4) == 4) {
			cmn_err(CE_WARN, "Cannot set keyboard controller to 1Mb, retrying...\n");
		}
	}
	return ((t1 & 4) != 4);
}

#endif R3030
#endif PROM
