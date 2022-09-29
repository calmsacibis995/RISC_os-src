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
/* $Header: sbd.h,v 1.23 90/07/25 14:04:19 jj Exp $ */

#ifndef _SYS_SBD_
#define _SYS_SBD_ 1

/*
 *  MIPS System Board Data Definitions
 */

/*
 * Segment base addresses and sizes
 */
#define	K0BASE		0x80000000
#define	K0SIZE		0x20000000
#define	K1BASE		0xA0000000
#define	K1SIZE		0x20000000
#define	K2BASE		0xC0000000
#define	K2SIZE		0x20000000

/*
 * The MarkII machines maintain a segment table in the u-area page.
 * it is located just above the kernel stack.
 */
#define KSTEBASE	0xffffe000
#define KSTESIZE	0x1000
/* given a u, find the segment table above it */
#define utostbl(up)	((pde_t **)((caddr_t)up + (KSTEBASE - UADDR)))

/*
 * The MarkI machines maintain a pte window.
 * It is 4 megabytes long and is placed
 * 8 Meg from the top of kseg2, to leave space for upage. 
 * The KPTE window must be on a 4 Meg boundary.
 * (Note: this definition relies upon 2's complement arithmetic!)
 */
#define	KPTEBASE	(-0x2000000)
#define	KPTESIZE	0x1000000

#define	KUBASE		0
#define	KUSIZE		0x80000000

/*
 * Exception vectors
 */
#define	UT_VEC		K0BASE			/* utlbmiss vector */
#define	E_VEC		(K0BASE+0x80)		/* exception vector */
#define	R_VEC		(K1BASE+0x1fc00000)	/* reset vector */

/*
 * Address conversion macros
 */
#define	K0_TO_K1(x)	((unsigned)(x)|0xA0000000)	/* kseg0 to kseg1 */
#define	K1_TO_K0(x)	((unsigned)(x)&0x9FFFFFFF)	/* kseg1 to kseg0 */
#define	K0_TO_PHYS(x)	((unsigned)(x)&0x1FFFFFFF)	/* kseg0 to physical */
#define	K1_TO_PHYS(x)	((unsigned)(x)&0x1FFFFFFF)	/* kseg1 to physical */
#define	PHYS_TO_K0(x)	((unsigned)(x)|0x80000000)	/* physical to kseg0 */
#define	PHYS_TO_K1(x)	((unsigned)(x)|0xA0000000)	/* physical to kseg1 */

/*
 * Address predicates
 */
#define	IS_KSEG0(x)	((unsigned)(x) >= K0BASE && (unsigned)(x) < K1BASE)
#define	IS_KSEG1(x)	((unsigned)(x) >= K1BASE && (unsigned)(x) < K2BASE)
#ifdef R6000
#define	IS_KSEG2(x)	((unsigned)(x) >= K2BASE && (unsigned)(x) < UADDR)
#define	IS_KPTESEG(x)	0	/* doesn't exist */
#else
#define	IS_KSEG2(x)	((unsigned)(x) >= K2BASE && (unsigned)(x) < KPTEBASE)
#define	IS_KPTESEG(x)	((unsigned)(x) >= KPTEBASE)
#endif
#define	IS_KUSEG(x)	((unsigned)(x) < K0BASE)
#ifndef SYSTYPE_BSD43
#define IS_UAREA(x)	((caddr_t)(x) > (caddr_t)&u)
#endif SYSTYPE_BSD43

/*
 * Cache size constants
 */
#define	MINCACHE	+(4*1024)	/* leading plus for mas's benefit */
#define	MAXCACHE	+(64*1024)	/* leading plus for mas's benefit */

/*
 * TLB size constants
 */
#define	TLBWIREDBASE	0		/* WAG for now */
#define	NWIREDENTRIES	8		/* WAG for now */
#define	TLBRANDOMBASE	NWIREDENTRIES
#define	NRANDOMENTRIES	(NTLBENTRIES-NWIREDENTRIES)
#define	NTLBENTRIES	64		/* WAG for now */

/*
 * tlb entrylo format
 */
#if (! defined(SYSTYPE_BSD43)) || (! defined(LOCORE))
union tlb_lo {
	unsigned tl_word;		/* efficient access */
	struct {
#ifdef MIPSEB
		unsigned tls_pfn:20;	/* physical page frame number */
		unsigned tls_n:1;	/* non-cacheable */
		unsigned tls_d:1;	/* dirty (actually writeable) */
		unsigned tls_v:1;	/* valid */
		unsigned tls_g:1;	/* match any pid */
		unsigned :8;
#endif MIPSEB
#ifdef MIPSEL
		unsigned :8;
		unsigned tls_g:1;	/* match any pid */
		unsigned tls_v:1;	/* valid */
		unsigned tls_d:1;	/* dirty (actually writeable) */
		unsigned tls_n:1;	/* non-cacheable */
		unsigned tls_pfn:20;	/* physical page frame number */
#endif MIPSEL
	} tl_struct;
};

#define	tl_pfn		tl_struct.tls_pfn
#define	tl_n		tl_struct.tls_n
#define	tl_d		tl_struct.tls_d
#define	tl_v		tl_struct.tls_v
#define	tl_g		tl_struct.tls_g
#endif 

#define	TLBLO_PFNMASK	0xfffff000
#define	TLBLO_PFNSHIFT	12
#define	TLBLO_N		0x800		/* non-cacheable */
#define	TLBLO_D		0x400		/* writeable */
#define	TLBLO_V		0x200		/* valid bit */
#define	TLBLO_G		0x100		/* global access bit */

#define	TLBLO_FMT	"\20\14N\13D\12V\11G"

/*
 * TLB entryhi format
 */
#if (! defined(SYSTYPE_BSD43)) || (! defined(LOCORE))
union tlb_hi {
	unsigned th_word;		/* efficient access */
	struct {
#ifdef MIPSEB
		unsigned ths_vpn:20;	/* virtual page number */
		unsigned ths_pid:6;
		unsigned :6;
#endif MIPSEB
#ifdef MIPSEL
		unsigned :6;
		unsigned ths_pid:6;
		unsigned ths_vpn:20;	/* virtual page number */
#endif MIPSEL
	} th_struct;
};

#define	th_vpn		th_struct.ths_vpn
#define	th_pid		th_struct.ths_pid
#endif

#define	TLBHI_VPNMASK	0xfffff000
#define	TLBHI_VPNSHIFT	12
#define	TLBHI_PIDMASK	0xfc0
#define	TLBHI_PIDSHIFT	6
#ifdef R6000
#define TLBHI_NPID	256
#define SYS_TLBPID_UPB	7		/* max sys proc p_tlbpid */
#else 
#define	TLBHI_NPID	64
#define SYS_TLBPID_UPB	0		/* max sys proc p_tlbpid */
#endif R6000

/*
 * TLB index register
 */
#if (! defined(SYSTYPE_BSD43)) || (! defined(LOCORE))
union tlb_inx {
	unsigned ti_word;
	struct {
#ifdef MIPSEB
		unsigned tis_probe:1;	/* 1 => probe failure */
		unsigned :17;
		unsigned tis_inx:6;	/* tlb index for TLBWRITEI op */
		unsigned :8;
#endif MIPSEB
#ifdef MIPSEL
		unsigned :8;
		unsigned tis_inx:6;	/* tlb index for TLBWRITEI op */
		unsigned :17;
		unsigned tis_probe:1;	/* 1 => probe failure */
#endif MIPSEL
	} ti_struct;
};

#define	ti_probe	ti_struct.tis_probe
#define	ti_inx		ti_struct.tis_inx
#endif

#define	TLBINX_PROBE		0x80000000
#define	TLBINX_INXMASK		0x00003f00
#define	TLBINX_INXSHIFT		8

/*
 * TLB random register
 */
#if (! defined(SYSTYPE_BSD43)) || (! defined(LOCORE))
union tlb_rand {
	unsigned tr_word;
	struct {
#ifdef MIPSEB
		unsigned :18;
		unsigned trs_rand:6;	/* tlb index for TLBWRITER op */
		unsigned :8;
#endif MIPSEB
#ifdef MIPSEL
		unsigned :8;
		unsigned trs_rand:6;	/* tlb index for TLBWRITER op */
		unsigned :18;
#endif MIPSEL
	} tr_struct;
};

#define	tr_rand		ti_struct.tis_rand
#endif

#define	TLBRAND_RANDMASK	0x00003f00
#define	TLBRAND_RANDSHIFT	8

/*
 * TLB context register
 */
#if (! defined(SYSTYPE_BSD43)) || (! defined(LOCORE))
union tlb_ctxt {
	unsigned tc_word;		/* efficient access */
	struct {
#ifdef MIPSEB
		unsigned tcs_pteseg:11;	/* bits 21-31 of kernel pte window */
		unsigned tcs_vpn:19;	/* vpn of faulting ref (ro) */
		unsigned :2;
#endif MIPSEB
#ifdef MIPSEL
		unsigned :2;
		unsigned tcs_vpn:19;	/* vpn of faulting ref (ro) */
		unsigned tcs_pteseg:11;	/* bits 22-31 of kernel pte window */
#endif MIPSEL
	} tc_struct;
};

#define	tc_pteseg	tc_struct.tcs_pteseg
#define	tc_vpn		tc_struct.tcs_vpn
#endif

#define	TLBCTXT_BASEMASK	0xffe00000
#define	TLBCTXT_BASESHIFT	21

#define	TLBCTXT_VPNMASK		0x001ffffc
#define	TLBCTXT_VPNSHIFT	2

#if defined(SYSTYPE_BSD43) && defined(mips) && defined(LANGUAGE_C)
struct tlbinfo {
	union tlb_lo	lo;
	union tlb_hi	hi;
};
#endif 
#ifdef SYSTYPE_BSD43
/*
 * NPAGEMAP is the number of second level page-mappings that can be cached
 * in the tlb wired entries. The u-area and kernel stack take the other 2.
 * This should be defined as (NWIREDENTRIES-UPAGES) but that would cause
 * this file to depend on vmparam.h
 */
#define NPAGEMAP 6

#endif SYSTYPE_BSD43
/*
 * Status register
 */
#define	SR_CUMASK	0xf0000000	/* coproc usable bits */

#define	SR_CU3		0x80000000	/* Coprocessor 3 usable */
#define	SR_CU2		0x40000000	/* Coprocessor 2 usable */
#define	SR_CU1		0x20000000	/* Coprocessor 1 usable */
#define	SR_CU0		0x10000000	/* Coprocessor 0 usable */

/* Cache control bits */
#define	SR_TS		0x00200000	/* TLB shutdown */
#define	SR_PE		0x00100000	/* cache parity error */
#define	SR_CM		0x00080000	/* cache miss */
#define SR_CM1		0x00100000	/* cache miss, side 1 (R6000)*/
#define SR_CM0		0x00080000	/* cache miss, side 0 (R6000)*/
#define	SR_PZ		0x00040000	/* cache parity zero */
#define	SR_SWC		0x00020000	/* swap cache */
#define SR_ITP		0x00020000	/* invert tag parity (R6000)*/
#define	SR_ISC		0x00010000	/* Isolate data cache */

#define SR_RE		0x02000000	/* reverse endian */
#define	SR_BEV		0x00400000	/* use boot exception vectors */

#define SR_MM_MODE	0x00010000	/* lwl/swl/etc become scache/etc */

/*
 * Interrupt enable bits
 * (NOTE: bits set to 1 enable the corresponding level interrupt)
 */
#ifdef R6000
#define	SR_IMASK	0x00000f00	/* Interrupt mask */
#else
#define	SR_IMASK	0x0000ff00	/* Interrupt mask */
#endif !R6000
#define	SR_IMASK8	0x00000000	/* mask level 8 */
#define	SR_IMASK7	(0x00008000 & SR_IMASK)	/* mask level 7 */
#define	SR_IMASK6	(0x0000c000 & SR_IMASK)	/* mask level 6 */
#define	SR_IMASK5	(0x0000e000 & SR_IMASK)	/* mask level 5 */
#define	SR_IMASK4	(0x0000f000 & SR_IMASK)	/* mask level 4 */
#define	SR_IMASK3	(0x0000f800 & SR_IMASK)	/* mask level 3 */
#define	SR_IMASK2	(0x0000fc00 & SR_IMASK)	/* mask level 2 */
#define	SR_IMASK1	(0x0000fe00 & SR_IMASK)	/* mask level 1 */
#define	SR_IMASK0	(0x0000ff00 & SR_IMASK)	/* mask level 0 */

#define	SR_IBIT8	(0x00008000 & SR_IMASK)	/* bit level 8 */
#define	SR_IBIT7	(0x00004000 & SR_IMASK)	/* bit level 7 */
#define	SR_IBIT6	(0x00002000 & SR_IMASK)	/* bit level 6 */
#define	SR_IBIT5	(0x00001000 & SR_IMASK)	/* bit level 5 */
#define	SR_IBIT4	(0x00000800 & SR_IMASK)	/* bit level 4 */
#define	SR_IBIT3	(0x00000400 & SR_IMASK)	/* bit level 3 */
#define	SR_IBIT2	(0x00000200 & SR_IMASK)	/* bit level 2 */
#define	SR_IBIT1	(0x00000100 & SR_IMASK)	/* bit level 1 */

#ifdef R6000
#define SR_IBIT_FPINTR	SR_IBIT4
#else
#define SR_IBIT_FPINTR	SR_IBIT6
#endif

#define SR_2030MASK	(0xffff7fff)		/* irq5 bit off */

#define	SR_KUO		0x00000020	/* old kernel/user, 0 => k, 1 => u */
#define	SR_IEO		0x00000010	/* old interrupt enable, 1 => enable */
#define	SR_KUP		0x00000008	/* prev kernel/user, 0 => k, 1 => u */
#define	SR_IEP		0x00000004	/* prev interrupt enable, 1 => enable */
#define	SR_KUC		0x00000002	/* cur kernel/user, 0 => k, 1 => u */
#define	SR_IEC		0x00000001	/* cur interrupt enable, 1 => enable */

#define	SR_IMASKSHIFT	8

#define	SR_FMT		"\20\40BD\26TS\25PE\24CM\23PZ\22SwC\21IsC\20IM7\17IM6\16IM5\15IM4\14IM3\13IM2\12IM1\11IM0\6KUo\5IEo\4KUp\3IEp\2KUc\1IEc"

/*
 * Cause Register
 */
#define	CAUSE_BD	0x80000000	/* Branch delay slot */
#define	CAUSE_CEMASK	0x30000000	/* coprocessor error */
#define	CAUSE_CESHIFT	28

/* Interrupt pending bits */
#define	CAUSE_IP8	0x00008000	/* External level 8 pending */
#define	CAUSE_IP7	0x00004000	/* External level 7 pending */
#define	CAUSE_IP6	0x00002000	/* External level 6 pending */
#define	CAUSE_IP5	0x00001000	/* External level 5 pending */
#define	CAUSE_IP4	0x00000800	/* External level 4 pending */
#define	CAUSE_IP3	0x00000400	/* External level 3 pending */
#define	CAUSE_SW2	0x00000200	/* Software level 2 pending */
#define	CAUSE_SW1	0x00000100	/* Software level 1 pending */

#ifdef R6000
#define CAUSE_IP_FPINTR	CAUSE_IP4
#else
#define CAUSE_IP_FPINTR	CAUSE_IP6
#endif

#define	CAUSE_IPMASK	0x0000FF00	/* Pending interrupt mask */
#define	CAUSE_IPSHIFT	8

#define	CAUSE_EXCMASK	0x0000003C	/* Cause code bits */
#define	CAUSE_EXCSHIFT	2

#define	CAUSE_FMT	"\20\40BD\36CE1\35CE0\20IP8\17IP7\16IP6\15IP5\14IP4\13IP3\12SW2\11SW1\1INT"

/*
 *  Cp0 ERROR register
 */
#define C0_ERROR_IEXT	0x00080000	/* ignore external parity err */
#define C0_ERROR_IRF	0x00040000	/* ignore register parity err */
#define C0_ERROR_IDB	0x00020000	/* ignore data bus parity err */
#define C0_ERROR_IIB	0x00010000	/* ignore instr bus parity err */
#define C0_ERROR_IMASK	(C0_ERROR_IEXT|C0_ERROR_IRF|C0_ERROR_IDB|C0_ERROR_IIB)
#define C0_ERROR_EXT	0x00000008	/* external parity err */
#define C0_ERROR_RF	0x00000004	/* register parity err */
#define C0_ERROR_DB	0x00000002	/* data bus parity err */
#define C0_ERROR_IB	0x00000001	/* instr bus parity err */
#define C0_ERROR_MASK	(C0_ERROR_EXT|C0_ERROR_RF|C0_ERROR_DB|C0_ERROR_IB)

#ifndef R6000
#define	setsoftclock()	siron(CAUSE_SW1)
#define	setsoftnet()	siron(CAUSE_SW2)
#define	acksoftclock()	siroff(CAUSE_SW1)
#define	acksoftnet()	siroff(CAUSE_SW2)
#endif !R6000

/* Cause register exception codes */

#define	EXC_CODE(x)	((x)<<2)

/* Hardware exception codes */
#define	EXC_INT		EXC_CODE(0)	/* interrupt */
#define	EXC_MOD		EXC_CODE(1)	/* TLB mod */
#define	EXC_RMISS	EXC_CODE(2)	/* Read TLB Miss */
#define	EXC_WMISS	EXC_CODE(3)	/* Write TLB Miss */
#define	EXC_RADE	EXC_CODE(4)	/* Read Address Error */
#define	EXC_WADE	EXC_CODE(5)	/* Write Address Error */
#define	EXC_IBE		EXC_CODE(6)	/* Instruction Bus Error */
#define	EXC_DBE		EXC_CODE(7)	/* Data Bus Error */
#define	EXC_SYSCALL	EXC_CODE(8)	/* SYSCALL */
#define	EXC_BREAK	EXC_CODE(9)	/* BREAKpoint */
#define	EXC_II		EXC_CODE(10)	/* Illegal Instruction */
#define	EXC_CPU		EXC_CODE(11)	/* CoProcessor Unusable */
#define	EXC_OV		EXC_CODE(12)	/* OVerflow */
#define EXC_TRAP	EXC_CODE(13)	/* TRAP */
#define EXC_DBL_NC	EXC_CODE(14)	/* DouBLeword Non-Cached access */
#define EXC_CHECK	EXC_CODE(15)	/* Machine Check */

/* software exception codes */
#define	SEXC_SEGV	EXC_CODE(16)	/* Software detected seg viol */
#define	SEXC_RESCHED	EXC_CODE(17)	/* resched request */
#define	SEXC_PAGEIN	EXC_CODE(18)	/* page-in request */
#define	SEXC_CPU	EXC_CODE(19)	/* coprocessor unusable */
#define SEXC_BUS	EXC_CODE(20)	/* software detected bus error */
#define SEXC_KILL	EXC_CODE(21)	/* bad page in process (vfault) */


/*
 * Coprocessor 0 registers
 */
#define	C0_INX		$0		/* tlb index */
#define	C0_RAND		$1		/* tlb random */
#define	C0_TLBLO	$2		/* tlb entry low */

#define	C0_CTXT		$4		/* tlb context */

#define C0_PIDMASK	$6		/* Mips2 */
#define C0_ERROR	$7		/* Mips2 */
#define	C0_BADVADDR	$8		/* bad virtual address */

#define	C0_TLBHI	$10		/* tlb entry hi */
#define C0_PID		$10		/* Mips2 */

#define	C0_SR		$12		/* status register */
#define	C0_CAUSE	$13		/* exception cause */
#define	C0_EPC		$14		/* exception pc */
#define C0_PRID		$15		/* revision identifier */

/*
 * Coprocessor 0 operations
 */
#define	C0_READI  0x1		/* read ITLB entry addressed by C0_INDEX */
#define	C0_WRITEI 0x2		/* write ITLB entry addressed by C0_INDEX */
#define	C0_WRITER 0x6		/* write ITLB entry addressed by C0_RAND */
#define	C0_PROBE  0x8		/* probe for ITLB entry addressed by TLBHI */
#define	C0_RFE	  0x10		/* restore for exception */

/*
 * Flags for the nofault handler. 0 means no fault is expected.
 */
#define	NF_BADADDR	1	/* badaddr, wbadaddr */
#define	NF_COPYIO	2	/* copyin, copyout */
#define	NF_ADDUPC	3	/* addupc */
#define	NF_FSUMEM	4	/* fubyte, subyte, fuword, suword */
#define	NF_USERACC	5	/* useracc */
#define	NF_SOFTFP	6	/* softfp */
#define	NF_REVID	7	/* revision ids */
#define	NF_SOFTFPI	8	/* fp instr fetch */
#define	NF_FIXADE	9	/* fix address errors */
#define	NF_NENTRIES	10

/*
 * Chip interrupt vector
 */
#define	NC0VECS		8
#ifndef SYSTYPE_BSD43
#ifdef INKERNEL
extern int (*c0vec_tbl[])();
extern char * code_names[];	/* names for fault codes */
#endif INKERNEL
#else SYSTYPE_BSD43
#ifndef LOCORE
#ifdef KERNEL
extern int (*c0vec_tbl[])();
#endif KERNEL
#endif LOCORE
#endif SYSTYPE_BSD43

#endif	_SYS_SBD_
