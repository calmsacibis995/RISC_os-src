#ident "$Header: pon_vm_3030.c,v 1.2.1.1 90/07/18 14:33:57 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#include "sys/param.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/setjmp.h"
#include "pon.h"

#define	PGSIZE			4096
#define TLBLO_DATA		(TLBLO_PFNMASK|TLBLO_N|TLBLO_D|TLBLO_V|TLBLO_G)
#define TLBHI_DATA		(TLBHI_VPNMASK|TLBHI_PIDMASK)
#define CODE_SPACE		0x80080000
#define RETURN_INSTRUCTION	0x3e00008

#define DISPATCH(x)		((int (*)())K1_TO_K0(x))

extern int clear_SR_PE();
extern int get_sr();
extern int matchtlb();
extern int pon_invaltlb();
extern int setjmp();
extern int read_indexed_hi();
extern int read_indexed_lo();
extern int set_tlbpid();
extern int ta_spl();
extern int tlbwired();
extern int write_indexed_hi();
extern int write_indexed_lo();
extern u_int (*move_code())();
extern int *nofault;
extern u_int _badvaddr_save;
extern char success[], failure[], skipped[], crlf[];

static jmp_buf fault_buf;
static u_int wpte[NTLBENTRIES];
static u_char *ka;
static u_char *k0a;
static u_char *k1a;
static u_char *pa;
static u_char *a = (u_char *)PHYS_TO_K1(PON_SCRATCHMEM + 0x80000);


static pte_setup()

{
	register u_char *p;
	register int i;

	ka = &a[PGSIZE];
	ka = (u_char *)(((u_int)ka) & TLBHI_VPNMASK);
	p = pa = (u_char *)K0_TO_PHYS(ka);
	k0a = (u_char *)PHYS_TO_K0(pa);
	k1a = (u_char *)PHYS_TO_K1(pa);

	for (i = 0; i < NTLBENTRIES; i++) {
		*(ka + i * PGSIZE) = i;
		wpte[i] = (u_int)(p + i * PGSIZE);
	}
}



/*
 * Test the TLB as a small memory array.  Just see if all the read/write bits
 * can be toggled and that all undefined bit read back zero.
 */
static tlb_ram()

{
	register u_int i;
	register u_int j;
	register u_int rpat;
	register u_int wpat;

#ifdef	DEBUG
	pon_puts("in tlb_ram\r\n");
#endif	DEBUG

	wpat = 0xa5a5a5a5;
	i = 0;
	do {
		j = 0;
		do {
			write_indexed_lo(i, wpat);
			rpat = read_indexed_lo(i);
			if (rpat != (wpat & TLBLO_DATA)) {
				return(0);
			}

			wpat = ~wpat;
			j++;
		} while (j != 2);

		i++;
	} while (i != NTLBENTRIES);

	i = 0;
	do {
		j = 0;
		do {
			write_indexed_hi(i, wpat);
			rpat = read_indexed_hi(i);
			if (rpat != (wpat & TLBHI_DATA)) {
				return(0);
			}

			wpat = ~wpat;
			j++;
		} while (j != 2);

		i++;
	} while (i != NTLBENTRIES);

	return(1);
}


/*
 * See if all the TLB slots respond to probes upon address match.
 */
static tlb_probe()

{
	register struct pte *pte;
	register int i;
	register int indx;

#ifdef	DEBUG
	pon_puts("in tlb_probe\r\n");
#endif	DEBUG

#ifdef	USED
	flushall_tlb();
	set_tlbpid(0);
#endif	USED
	for (i = 0; i < NTLBENTRIES; i++) {
		tlbwired(i, i * PGSIZE, wpte[i] | TLBLO_V);
		indx = matchtlb(0, i);
		if (indx != i << TLBINX_INXSHIFT) {
			return(0);
		}

		pon_invaltlb(i);
	}
	return(1);
}


static tlb_xlate()

{
	register u_char bucket;
	register u_int addr;
	register int i;

#ifdef	DEBUG
	pon_puts("in tlb_xlate\r\n");
#endif	DEBUG

#ifdef	USED
	flushall_tlb();
	set_tlbpid(0);
#endif	USED
	for (i = 0; i < NTLBENTRIES; i++) {
		addr = 0;
		do {
			tlbwired(i, addr, wpte[i] | TLBLO_V);
			if (setjmp(fault_buf)) {
				ta_spl();
				return(0);
			}
			else {
				nofault = &fault_buf[0];
				bucket = *(u_char *)addr;
				nofault = 0;
				if (bucket != *(ka + i * PGSIZE))  {
					return(0);
				}
			}

			pon_invaltlb(i);
			addr = (addr << 1) + PGSIZE;
		} while (addr < K0BASE);
	}
	return(1);
}


static tlb_valid()

{
	register volatile u_char *addr;
	register u_char bucket;
	register int i;
	register int ret = 1;

#ifdef	DEBUG
	pon_puts("in tlb_valid\r\n");
#endif	DEBUG

#ifdef	USED
	flushall_tlb();
	set_tlbpid(0);
#endif	USED
	for (i = 0; i < NTLBENTRIES; i++) {
		addr = (u_char *)(K2BASE + i * PGSIZE);
		tlbwired(i, addr, wpte[i]);
		if (setjmp(fault_buf)) {
			ta_spl();
			if (_badvaddr_save != (u_int)addr) {
				return(0);
			}
		}
		else {
			nofault = &fault_buf[0];
			bucket = *(u_char *)addr;
			nofault = 0;
			ret = 0;
			break;
		}

		pon_invaltlb(i);
	}

	return(ret);
}


static tlb_mod()

{
	register u_char bucket;
	register int i;
	register int ret = 1;

#ifdef	DEBUG
	pon_puts("in tlb_mod\r\n");
#endif	DEBUG

#ifdef	USED
	flushall_tlb();
	set_tlbpid(0);
#endif	USED
	for (i = 0; i < NTLBENTRIES; i++) {
		tlbwired(i, 0, wpte[i] | TLBLO_V);
		if (setjmp(fault_buf)) {
			ta_spl();
			if (_badvaddr_save != 1) {
				return(0);
			}
		}
		else {
			nofault = &fault_buf[0];
			*(u_char *)1 = i;
			nofault = 0;
			ret = 0;
			break;
		}

		pon_invaltlb(i);
	}

	for (i = 0; i < NTLBENTRIES; i++) {
		tlbwired(i, 0, wpte[i] | TLBLO_V | TLBLO_D);
		if (setjmp(fault_buf)) {
			ta_spl();
			return(0);
		}
		else {
			nofault = &fault_buf[0];
			*(u_char *)1 = i;
			nofault = 0;
			if (*(u_char *)1 != *(u_char *)0) {
				return(0);
			}
		}

		pon_invaltlb(i);
	}

	return(ret);
}


static tlb_pid()

{
	register u_char bucket;
	register int indx;
	register int validpid;
	register int testpid;
	register int lim;
	volatile int expect_fault;

#ifdef	DEBUG
	pon_puts("in tlb_pid\r\n");
#endif	DEBUG

#ifdef	USED
	flushall_tlb();
#endif	USED
	for (indx = 0; indx < NTLBENTRIES; indx++) {
		validpid = 0;
		for ( ; validpid < (TLBHI_NPID << TLBHI_PIDSHIFT); validpid += (0x10 << TLBHI_PIDSHIFT)) {
			set_tlbpid(validpid);
			testpid = 0;
			for ( ; testpid < (TLBHI_NPID << TLBHI_PIDSHIFT); testpid += (0x10 << TLBHI_PIDSHIFT)) {
				tlbwired(indx, 0 | testpid, wpte[indx] | TLBLO_V);
				expect_fault = (testpid != validpid);
				if (setjmp(fault_buf)) {
					ta_spl();
					if (!expect_fault) {
						return(0);
					}
					else if (_badvaddr_save != 0) {
						return(0);
					}
				}
				else {
					nofault = &fault_buf[0];
					bucket = *(u_char *)0;
					nofault = 0;
					if (expect_fault) {
						return(0);
					}
				}

				pon_invaltlb(indx);
			}
		}
	}

	return(1);
}


static tlb_g()

{
	register u_char bucket;
	register int indx;
	register int validpid;
	register int testpid;
	register int lim;
	volatile int expect_fault;

#ifdef	DEBUG
	pon_puts("in tlb_g\r\n");
#endif	DEBUG

#ifdef	USED
	flushall_tlb();
#endif	USED
	for (indx = 0; indx < NTLBENTRIES; indx++) {
		validpid = 0;
		for ( ; validpid < (TLBHI_NPID << TLBHI_PIDSHIFT); validpid += (0x10 << TLBHI_PIDSHIFT)) {
			set_tlbpid(validpid);
			testpid = 0;
			for ( ; testpid < (TLBHI_NPID << TLBHI_PIDSHIFT); testpid += (0x10 << TLBHI_PIDSHIFT)) {
				tlbwired(indx, 0 | testpid, wpte[indx] | TLBLO_V | TLBLO_G);
				if (setjmp(fault_buf)) {
					ta_spl();
					return(0);
				}
				else {
					nofault = &fault_buf[0];
					bucket = *(u_char *)0;
					nofault = 0;
				}

				pon_invaltlb(indx);
			}
		}
	}

	return(1);
}


static u_int n_addr[] = {
	KUBASE,
	KUBASE | 0x20000000,
	K2BASE,
	K2BASE | 0x20000000
};


static tlb_n()

{
	register int i;
	register u_int *cachep;
	register u_int *nocachep;
	register u_int mempat = 0x12345678;
	register u_int cachepat = 0x87654321;
	register u_int tlbpat;
	register u_int sr;
	register u_int addr;

#ifdef	DEBUG
	pon_puts("in tlb_n\r\n");
#endif	DEBUG

	cachep = (u_int *)(k0a + 4);
	nocachep = (u_int *)(k1a + 4);
	clear_SR_PE();
	*cachep = cachepat;
	*nocachep = mempat;
	if (*cachep == mempat) {
		if (get_sr() & SR_PE) {
			clear_SR_PE();
		}

		return(0);
	}

#ifdef	USED
	flushall_tlb();
	set_tlbpid(0);
#endif	USED
	for (i = 0; i < NTLBENTRIES; i++) {
		addr = n_addr[i & 3];
		tlbwired(i, addr, wpte[0] | TLBLO_V | TLBLO_D);
		if (setjmp(fault_buf)) {
			ta_spl();
			return(0);
		}
		else {
			nofault = &fault_buf[0];
			*cachep = cachepat;
			*nocachep = mempat;
			clear_SR_PE();
			tlbpat = *(u_int *)(addr+4);
			sr = get_sr();
			nofault = 0;
			if (tlbpat != cachepat) {
				return(0);
			}
		}

		pon_invaltlb(i);
	}

	for (i = 0; i < NTLBENTRIES; i++) {
		tlbwired(i, 0, wpte[0] | TLBLO_V | TLBLO_D | TLBLO_N);
		if (setjmp(fault_buf)) {
			ta_spl();
			return(0);
		}
		else {
			nofault = &fault_buf[0];
			*cachep = cachepat;
			*nocachep = mempat;
			tlbpat = *(u_int *)4;
			nofault = 0;
			if (tlbpat != mempat) {
				return(0);
			}
		}

		pon_invaltlb(i);
	}

	return(1);
}


u_int *end_of_text(text)

register u_int *text;

{
	register u_int *start;

	start = text;
	while(*text != RETURN_INSTRUCTION) text++;
	text++;	/* get the return instruction */
	text++; /* get the branch delay slot */
	return(text);
}


static int runlist[] = {
	(int)tlb_ram,
	(int)tlb_probe,
	(int)tlb_xlate,
	(int)tlb_valid,
	(int)tlb_mod,
	(int)tlb_pid,
	(int)tlb_g,
	(int)tlb_n,
};

#define	RUN_SIZE		(sizeof(runlist) / sizeof(int))


Pon_VM()

{
	register u_int *end_addr, (*func)();
	register int i;

	pon_puts("TLB Tests...");
	if (GetDepend() & (PON_FAULT_CACHE | PON_FAULT_MEM)) {
		pon_puts(skipped);
		goto norun;
	}

	ta_spl();
	get_cache_sp();
	pte_setup();

	for (i = 0; i < RUN_SIZE; i++) {
#ifdef	DEBUG
		pon_puthex(i);
		pon_puts(crlf);
#endif	DEBUG
		SetSR(GetSR() | SR_SWC);
		flushall_tlb();
		set_tlbpid(0);
		if(!(*DISPATCH(runlist[i]))()) {
			goto failed;
		}
	}

	SetSR(GetSR() & ~SR_SWC);
	pon_puts(success);
	flushall_tlb();
	return(PASS);

failed:
	SetSR(GetSR() & ~SR_SWC);
	flushall_tlb();
	pon_puts(failure);
	FastFlash(PON_TLB);

norun:
	SetDepend(PON_FAULT_TLB);
	return(FAIL);
}
