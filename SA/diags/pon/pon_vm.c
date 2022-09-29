#ident "$Header: pon_vm.c,v 1.12.1.1 90/07/18 14:33:54 huang Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
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
#define DISPATCH(x)		((int (*)())K1_TO_K0(x))
#define CODE_SPACE		0x80080000
#define RETURN_INSTRUCTION	0x3e00008

extern int *nofault;
extern int tlb_g();
extern int tlb_mod();
extern int tlb_n();
extern int tlb_pid();
extern int tlb_probe();
extern int tlb_ram();
extern int tlb_valid();
extern int tlb_xlate();
extern u_int (*move_code())();
extern u_int _badvaddr_save;
extern char success[], failure[], skipped[];

u_int *end_of_text();

jmp_buf fault_buf;


Pon_VM()

{
	register u_int *end_addr,(*func)();
	pon_set_leds(PON_TLB);

	pon_puts("TLB Test...");
	if (GetDepend() & (PON_FAULT_CACHE | PON_FAULT_MEM)) {
		pon_puts(skipped);
		goto norun;
	}

	ta_spl();
	get_cache_sp();
	pte_setup();

#ifdef	DEBUG
	pon_puts("Before tlb_ram\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_ram);
		func = move_code(CODE_SPACE,tlb_ram,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_ram))()) 
			goto failed;

#ifdef	DEBUG
	pon_puts("Before tlb_probe\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_probe);
		func = move_code(CODE_SPACE,tlb_probe,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_probe))())  {
		goto failed;
	}

#ifdef	DEBUG
	pon_puts("Before tlb_xlate\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_xlate);
		func = move_code(CODE_SPACE,tlb_xlate,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_xlate))()) {
		goto failed;
	}

#ifdef	DEBUG
	pon_puts("Before tlb_valid\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_valid);
		func = move_code(CODE_SPACE,tlb_valid,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_valid))()) {
		goto failed;
	}

#ifdef	DEBUG
	pon_puts("Before tlb_mod\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_mod);
		func = move_code(CODE_SPACE,tlb_mod,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_mod))()) {
		goto failed;
	}

#ifdef	DEBUG
	pon_puts("Before tlb_pid\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_pid);
		func = move_code(CODE_SPACE,tlb_pid,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_pid))()) {
		goto failed;
	}

#ifdef	DEBUG
	pon_puts("Before tlb_g\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_g);
		func = move_code(CODE_SPACE,tlb_g,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_g))()) {
		goto failed;
	}

#ifdef	DEBUG
	pon_puts("Before tlb_n\r\n");
#endif	DEBUG
	if(machine_type == BRDTYPE_M180) {
		end_addr = end_of_text(tlb_n);
		func = move_code(CODE_SPACE,tlb_n,end_addr);
		if (!(*func)())
			goto failed;
	}
	else if(!(*DISPATCH(tlb_n))()) {
		goto failed;
	}

	pon_puts(success);
	flushall_tlb();
	return(PASS);

failed:
	flushall_tlb();
	pon_puts(failure);
	FastFlash(PON_TLB);
	pon_set_leds(PON_TLB);

norun:
	SetDepend(PON_FAULT_TLB);
	return(FAIL);
}


u_int wpte[NTLBENTRIES];
u_char *ka;
u_char *k0a;
u_char *k1a;
u_char *pa;
u_char *a = (u_char *)PHYS_TO_K1(PON_SCRATCHMEM + 0x80000);


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
	u_int i;
	u_int j;
	u_int rpat;
	u_int wpat;
	int ret = 1;

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

	return(ret);
}


/*
 * See if all the TLB slots respond to probes upon address match.
 */
static tlb_probe()

{
	register struct pte *pte;
	int i;
	int indx;
	int ret = 1;

	flushall_tlb();
	set_tlbpid(0);
	for (i = 0; i < NTLBENTRIES; i++) {
		tlbwired(i, i * PGSIZE, wpte[i] | TLBLO_V);
		indx = matchtlb(0, i);
		if (indx != i << TLBINX_INXSHIFT) {
			return(0);
		}

		pon_invaltlb(i);
	}
	return(ret);
}


static tlb_xlate()

{
	register u_char bucket;
	register u_int addr;
	register int i;
	register int ret = 1;

	flushall_tlb();
	set_tlbpid(0);
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
	return(ret);
}


static tlb_valid()

{
	register volatile u_char *addr;
	register u_char bucket;
	register int i;
	register int ret = 1;

	flushall_tlb();
	set_tlbpid(0);
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

	flushall_tlb();
	set_tlbpid(0);
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
	int indx;
	int validpid;
	int testpid;
	int ret = 1;
	int lim;
	volatile int expect_fault;

	flushall_tlb();
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

	return(ret);
}


static tlb_g()

{
	register u_char bucket;
	int indx;
	int validpid;
	int testpid;
	int ret = 1;
	int lim;
	volatile int expect_fault;

	flushall_tlb();
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

	return(ret);
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
	register int ret = 1;
	register u_int *cachep;
	register u_int *nocachep;
	u_int mempat = 0x12345678;
	u_int cachepat = 0x87654321;
	u_int tlbpat;
	u_int sr;
	u_int addr;

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

	flushall_tlb();
	set_tlbpid(0);
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

	return(ret);
}

u_int *
end_of_text(text)
register u_int *text;
{
	register u_int *start;
	
	start = text;
	while(*text != RETURN_INSTRUCTION) text++;
	text++;	/* get the return instruction */
	text++; /* get the branch delay slot */
	return(text); 
}
	
