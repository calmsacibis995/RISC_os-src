#ident "$Header: pon_vm_6000.c,v 1.5.6.1 90/07/18 14:34:03 huang Exp $"
/* $Copyright$ */

#include "sys/param.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/setjmp.h"
#include "pon.h"

#define	PGSIZE			16384
#define NUM_PTES		  256
#define NUM_TLBS_PER_SIDE	 2048
#define NUM_TLBS_PER_LINE	   32

#define INDX_TO_PTE(indx)	*(wpte + (indx % NUM_PTES))

#define TLB_TO_K1(pte)		(PHYS_TO_K1( TLB_TO_PHYS_R6000(pte) ))

#define INVAL_CACHES(pte,size)	clear_cache_r6000( TLB_TO_K1(pte), size )

extern int *nofault;
extern u_int _badvaddr_save;
extern char success[],skipped[];

jmp_buf fault_buf;


Pon_VM()
{
	pon_set_leds(PON_TLB);

	pon_puts("TLB Test...");
	if (GetDepend() & (PON_FAULT_CACHE | PON_FAULT_MEM)) {
		pon_puts(skipped);
		goto norun;
	}

	ta_spl();
	pte_setup();

	if (!tlb_asid_wr()) {
		goto failed;
	}
	if (!tlb_xlate()) {
		goto failed;
	}
	if (!tlb_valid()) {
		goto failed;
	}
	if (!tlb_mod()) {
		goto failed;
	}
	if (!tlb_asid()) {
		goto failed;
	}
	if (!tlb_g()) {
		goto failed;
	}
	if (!tlb_n()) {
		goto failed;
	}

	pon_puts(success);
	flushall_tlb();
	return(PASS);

failed:
	flushall_tlb();
	FastFlash(PON_TLB);
	pon_set_leds(PON_TLB);

norun:
	SetDepend(PON_FAULT_TLB);
	return(FAIL);
}


u_int *wpte = (u_int *)PHYS_TO_K0(PON_SCRATCHMEM + 0x80000);
u_int ka;
u_int *k0a;
u_int *k1a;
u_int *pa;
u_int *a = (u_int *)PHYS_TO_K1(PON_SCRATCHMEM + 0x100000);


static pte_setup()
{
	register u_int *p;
	register int i;

	ka = (u_int)a & VPNMASK_R6000;
	p = pa = (u_int *)K0_TO_PHYS(ka);
	k0a = (u_int *)PHYS_TO_K0(pa);
	k1a = (u_int *)PHYS_TO_K1(pa);

	/*
	 *  Set up a list of prototype page table entries with the
	 *  pfn (properly colored!) shifted into position, but with
	 *  zeroes for the four "mode" bits.
	 */
	for (i = 0; i < NUM_PTES; i++) {
		*(u_int *)(ka + i * PGSIZE) = i;
		*(wpte+i) = (u_int)(PHYS_TO_TLB_R6000(
					 K0_TO_PHYS(ka + i * PGSIZE)));
	}
}


/*
 * Perform a simple write-read test of the Cp0 PID register.
 */
static tlb_asid_wr()
{
	int pid_value, actual_pid;

	for (pid_value = 0; pid_value < TLB_NPID_R6000; pid_value++) {
		set_tlbpid( pid_value );
		actual_pid = get_tlbpid();
		if (pid_value != actual_pid) {
			pon_puts("Cp0 ASID Write-Read Test FAILED \r\n");
			printf("  Expected %x  Actual %x \n"
			      ,pid_value, actual_pid);
			return(0);
		}
	}
	return(1);	/* passed */
}


static tlb_xlate()
{
	u_int bucket, expected;
	u_int addr;
	u_int pte;
	int indx, side;

	flushall_tlb();
	set_tlbpid(0);
	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE; indx++) {
#ifdef SABLE
		if (indx > 0  &&  indx < NUM_TLBS_PER_SIDE-200)
			indx += 10;
		if (indx > 50)
			indx += 100;
#endif SABLE
		addr = indx * PGSIZE;
		do {
			pte = INDX_TO_PTE(indx) | TLB_V_R6000;
			tlbwired(side, addr, pte);
			if (setjmp(fault_buf)) {
				ta_spl();
				pon_puts("Translate Test FAILED \n\r");
				printf("  Unexpected TLB miss  side %d  vAddr %x  pAddr %x  pte %x \n"
				      ,side, addr, TLB_TO_PHYS_R6000(pte), pte);
				return(0);
			}
			else {
				nofault = &fault_buf[0];
				bucket = *(u_int *)addr;
				nofault = 0;
				expected = *(u_int *)TLB_TO_K1(pte);
				if (bucket != expected)  {
					pon_puts("Translate Test FAILED \n\r");
					printf("  Expected %x  Actual %x  side %d  vAddr %x  pAddr %x  pte %x \n"
					      ,expected, bucket
					      ,side, addr
					      ,TLB_TO_PHYS_R6000(pte), pte);
					return(0);
				}
			}

			pon_invaltlb(side,addr);
			INVAL_CACHES(pte,4);

			/*
			 *  Work through lots of address bits
			 */
			addr += 16384*PGSIZE;

		} while (addr < K0BASE);

	    } /* for each tlb on a side */
	} /* for each side */
	return(1);
}


static tlb_valid()
{
	u_int addr;
	u_int pte;
	u_int bucket;
	int side, indx;
	int ret = 1;

	flushall_tlb();
	set_tlbpid(0);
	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE; indx++) {
#ifdef SABLE
		if (indx > 0  &&  indx < NUM_TLBS_PER_SIDE-200)
			indx += 10;
		if (indx > 50)
			indx += 100;
#endif SABLE
		addr = (u_int)(K2BASE + indx * PGSIZE);
		pte  = INDX_TO_PTE(indx);		/* NOT Valid */
		tlbwired(side, addr, pte);
		if (setjmp(fault_buf)) {
			ta_spl();
			if (_badvaddr_save != addr) {
				pon_puts("Valid Bit Test FAILED \r\n");
				printf("  Incorrect BadVaddr  Expected %x  Actual %x  side %d  pAddr %x  pte %x \n"
				      ,addr, _badvaddr_save, side
				      ,TLB_TO_PHYS_R6000(pte), pte);
				return(0);
			}
		}
		else {
			nofault = &fault_buf[0];
			bucket = *(u_int *)addr;
			nofault = 0;
			ret = 0;
			pon_puts("Valid Bit Test FAILED \r\n");
			printf("  Expected tlbmiss  side %d  vAddr %x  pAddr %x  pte %x \n"
				      ,side, addr, TLB_TO_PHYS_R6000(pte), pte);
			break;
		}

		pon_invaltlb(side,addr);
		INVAL_CACHES(pte,4);
	    } /* for each tlb on a side */
	} /* for each side */

	return(ret);
}


static tlb_mod()
{
	u_int bucket;
	u_int pte;
	u_int addr, test_addr;
	u_int old_value, actual, expected;
	int side, indx;
	int ret = 1;

	flushall_tlb();
	set_tlbpid(0);
	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE; indx++) {
#ifdef SABLE
		if (indx > 0  &&  indx < NUM_TLBS_PER_SIDE-200)
			indx += 10;
		if (indx > 50)
			indx += 100;
#endif SABLE
		addr = indx * PGSIZE;
		test_addr = addr + 1;
		pte = INDX_TO_PTE(indx) | TLB_V_R6000;    /* not Writeable */
		tlbwired(side, addr, pte);
		if (setjmp(fault_buf)) {
			ta_spl();
			if (_badvaddr_save != test_addr) {
				pon_puts("Mod Bit Test FAILED \r\n");
				printf("  Incorrect BadVaddr  Expected %x  Actual %x  side %d  pAddr %x  pte %x \n"
				      ,test_addr, _badvaddr_save, side
				      ,TLB_TO_PHYS_R6000(pte), pte);
				return(0);
			}
		}
		else {
			nofault = &fault_buf[0];
			*(u_char *)test_addr = indx;	/* cause TLBMOD exc */
			nofault = 0;
			ret = 0;
			pon_puts("Mod Bit Test FAILED \r\n");
			printf("  Expected tlbmiss  side %d  vAddr %x  pAddr %x  pte %x \n"
			      ,side, test_addr, TLB_TO_PHYS_R6000(pte), pte);
			break;
		}

		pon_invaltlb(side,addr);
		INVAL_CACHES(pte,4);

	    } /* for each tlb on a side */
	} /* for each side */

	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE; indx++) {
#ifdef SABLE
		if (indx > 0  &&  indx < NUM_TLBS_PER_SIDE-200)
			indx += 10;
		if (indx > 50)
			indx += 100;
#endif SABLE
		addr = indx * PGSIZE;
		pte = INDX_TO_PTE(indx)
			 | TLB_V_R6000 | TLB_D_R6000 | TLB_N_R6000;
		tlbwired(side, addr, pte);
		if (setjmp(fault_buf)) {
			ta_spl();
			pon_puts("Mod Bit Test FAILED \n\r");
			printf("  Unexpected TLB miss  side %d  vAddr %x  pAddr %x  pte %x \n"
			      ,side, addr, TLB_TO_PHYS_R6000(pte), pte);
			return(0);
		}
		else {
			nofault = &fault_buf[0];
			/*
			 *  After saving the original value (accessed through
			 *  K1), store a new value through the mapped address,
			 *  then read it back through K1 and compare.  (And
			 *  restore the original value!)
			 */
			old_value = *(volatile u_int *)TLB_TO_K1(pte);
			*(volatile u_int *)addr = expected = old_value*64 + 1;
			actual = *(volatile u_int *)TLB_TO_K1(pte);
			*(volatile u_int *)TLB_TO_K1(pte) = old_value;
			nofault = 0;
			if (actual != expected) {
				pon_puts("Mod Bit Test FAILED \r\n");
				printf("  Expected %x  Actual %x  side %d  vAddr %x  pAddr %x  pte %x \n"
				      ,expected, actual, side
				      ,addr, TLB_TO_PHYS_R6000(pte), pte);
				return(0);
			}
		}

		pon_invaltlb(side,addr);
		INVAL_CACHES(pte,4);

	    } /* for each tlb on a side */
	} /* for each side */

	return(ret);
}


static tlb_asid()
{
	u_int bucket;
	int indx, side;
	u_int pte;
	u_int addr;
	int validpid;
	int testpid;
	int ret = 1;
	volatile int expect_fault;

	flushall_tlb();
	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE;
#ifdef SABLE
		 indx += 63*NUM_TLBS_PER_LINE) {
#else
		 indx += NUM_TLBS_PER_LINE) {
#endif !SABLE
		addr = indx * PGSIZE;
		pte = INDX_TO_PTE(indx);
		/*
		 *  Walk a bit through the PID for each tlb line
		 */
		for (validpid = 1; validpid < TLB_NPID_R6000; validpid *= 2) {
		    for (testpid = 1; testpid < TLB_NPID_R6000; testpid *= 2) {
			set_tlbpid(testpid);
			tlbwired(side, addr, pte | TLB_V_R6000);
			expect_fault = (testpid != validpid);
			if (setjmp(fault_buf)) {
			    ta_spl();
			    if (!expect_fault) {
				pon_puts("ASID Test FAILED \r\n");
				printf("  Unexpected TLB miss  side %d  vAddr %x  ASID %x  pAddr %x  pte %x \n"
				      ,side, addr, validpid
				      ,TLB_TO_PHYS_R6000(pte), pte);
				return(0);
			    }
			    else if (_badvaddr_save != addr) {
				pon_puts("ASID Test FAILED \r\n");
				printf("  Incorrect BadVaddr  Expected %x  Actual %x  TLB ASID %x  access ASID %x  side %d  pAddr %x  pte %x \n"
				      ,addr, _badvaddr_save
				      ,testpid, validpid, side
				      ,TLB_TO_PHYS_R6000(pte), pte);
				return(0);
			    }
			}
			else {
			    nofault = &fault_buf[0];
			    set_tlbpid(validpid);
			    bucket = *(u_int *)addr;
			    nofault = 0;
			    if (expect_fault) {
				pon_puts("ASID Test FAILED \r\n");
				printf("  Expected TLB miss  side %d  vAddr %x  TLB ASID %x  access ASID %x  pAddr %x  pte %x \n"
				      ,side, addr, testpid, validpid
				      ,TLB_TO_PHYS_R6000(pte), pte);
				return(0);
			}
		    }

		    pon_invaltlb(side,addr);
		    INVAL_CACHES(pte,4);

		    } /* for each testpid value */
		} /* for each validpid value */
	    } /* for each tlb on a side */
	} /* for each side */

	return(ret);
}


static tlb_g()
{
	register u_int bucket;
	u_int pte;
	int indx, side;
	int addr;
	int validpid;
	int testpid;
	int ret = 1;
	int lim;
	int expect_fault;

	flushall_tlb();
	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE;
#ifdef SABLE
		 indx += 63*NUM_TLBS_PER_LINE) {
#else
		 indx += NUM_TLBS_PER_LINE) {
#endif !SABLE
		for (validpid = 1; validpid < TLB_NPID_R6000; validpid *= 2) {
		    for (testpid = 1; testpid < TLB_NPID_R6000; testpid *= 2) {
			addr = indx * PGSIZE;
			pte = INDX_TO_PTE(indx) | TLB_V_R6000 | TLB_G_R6000;
			set_tlbpid(validpid);
			tlbwired(side, addr, pte);
			if (setjmp(fault_buf)) {
			    ta_spl();
			    pon_puts("Global Bit Test FAILED \r\n");
			    printf("  Unexpected TLB miss  side %d  vAddr %x  TLB ASID %x  access ASID %x  pAddr %x  pte %x \n"
				   ,side, addr, validpid, testpid
				   ,TLB_TO_PHYS_R6000(pte), pte);
			    return(0);
			}
			else {
			    nofault = &fault_buf[0];
			    set_tlbpid(testpid);
			    bucket = *(u_int *)addr;
			    nofault = 0;
			}

			pon_invaltlb(side,addr);
			INVAL_CACHES(pte,4);

		    } /* for each testpid */
		} /* for each validpid */
	    } /* for each tlb on a side */
	} /* for each side */

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
	int indx, side;
	int ret = 1;
	u_int pte;
	u_int cachep;
	u_int nocachep;
	u_int mempat   = 0x12345678;
	u_int cachepat = 0x87654321;
	u_int tlbpat;
	u_int addr;

	flushall_tlb();
	set_tlbpid(0);
	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE; indx++) {
#ifdef SABLE
		if (indx > 0  &&  indx < NUM_TLBS_PER_SIDE-200)
			indx += 10;
		if (indx > 50)
			indx += 100;
#endif SABLE
		pte = INDX_TO_PTE(indx) | TLB_V_R6000 | TLB_D_R6000;
		nocachep = TLB_TO_K1(pte);
		cachep   = K1_TO_K0(nocachep);
		addr = n_addr[indx & 3] + indx * PGSIZE;
		tlbwired(side, addr, pte);
		if (setjmp(fault_buf)) {
		    ta_spl();
		    pon_puts("Noncached Bit Test FAILED \r\n");
		    printf("  Unexpected TLB miss  side %d  vAddr %x  pAddr %x  pte %x \n"
			   ,side, addr, TLB_TO_PHYS_R6000(pte), pte);
		    return(0);
		}
		else {
		    nofault = &fault_buf[0];
		    *(volatile u_int *)cachep   = cachepat;
		    *(volatile u_int *)nocachep = mempat;
		    tlbpat = *(volatile u_int *)addr;
		    nofault = 0;
		    if (tlbpat != cachepat) {
			pon_puts("Noncached Bit Test FAILED \r\n");
			printf("  Expected %x  Actual %x  side %d  vAddr %x  pAddr %x  pte %x (not-N) \n"
				,cachepat, tlbpat, side
				,addr, TLB_TO_PHYS_R6000(pte), pte);
			return(0);
		    }
		}

		pon_invaltlb(side,addr);
		INVAL_CACHES(pte,4);
	    } /* for each tlb on a side */
	} /* for each side */

	for (side = 0; side < 2; side++) {
	    for (indx = 0; indx < NUM_TLBS_PER_SIDE; indx++) {
#ifdef SABLE
		if (indx > 0  &&  indx < NUM_TLBS_PER_SIDE-200)
			indx += 10;
		if (indx > 50)
			indx += 100;
#endif SABLE
		pte = INDX_TO_PTE(indx)
			 | TLB_V_R6000 | TLB_D_R6000 | TLB_N_R6000;
		nocachep = TLB_TO_K1(pte);
		cachep   = K1_TO_K0(nocachep);
		addr = n_addr[indx & 3] + indx * PGSIZE;
		tlbwired(side, addr, pte);
		if (setjmp(fault_buf)) {
		    ta_spl();
		    pon_puts("Noncached Bit Test FAILED \r\n");
		    printf("  Unexpected TLB miss  side %d  vAddr %x  pAddr %x  pte %x \n"
			   ,side, addr, TLB_TO_PHYS_R6000(pte), pte);
		    return(0);
		}
		else {
		    nofault = &fault_buf[0];
		    *(volatile u_int *)cachep = cachepat;
		    *(volatile u_int *)nocachep = mempat;
		    tlbpat = *(volatile u_int *)addr;
		    nofault = 0;
		    if (tlbpat != mempat) {
			pon_puts("Noncached Bit Test FAILED \r\n");
			printf("  Expected %x  Actual %x  side %d  vAddr %x  pAddr %x  pte %x (N) \n"
				,cachepat, tlbpat, side
				,addr, TLB_TO_PHYS_R6000(pte), pte);
			return(0);
		    }
		}

		pon_invaltlb(side,addr);
		INVAL_CACHES(pte,4);
	    } /* for each tlb on a side */
	} /* for each side */

	return(ret);
}
