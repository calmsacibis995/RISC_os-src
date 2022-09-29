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
#ident	"$Header: genassym.c,v 1.33.1.9.1.4 90/08/20 17:58:23 hawkes Exp $"

#undef KERNEL
#define KERNEL			/* Gotta get uid_t and gid_t */
#include	"sys/types.h"
#undef KERNEL
#include	"sys/cmn_err.h"
#include	"sys/sbd.h"
#include	"sys/cpu_board.h"
#include	"sys/immu.h"
#include	"sys/pcb.h"
#include	"sys/reg.h"
#include	"sys/dir.h"
#include	"sys/signal.h"
#include	"sys/region.h"
#define KERNEL			/* Gotta get the magic u #define */
#include	"sys/user.h"
#undef KERNEL
#include	"sys/proc.h"
#include	"sys/boot.h"
#include	"sys/firmware.h"
#include	"sys/errno.h"
#include	"sys/fpu.h"
#include 	"sys/softfp.h"
#include	"sys/bc.h"
#include	"sys/ctlspace.h"
#include	"sys/ioa.h"
#include	"sys.s"

/*
** this program generates the needed defines for the assembly language
** files.  This is done so that the nasty '#ifdef LOCORE' that
** is rampant through many include files does not have to be done for
** this port.
*/
main()
{
	register struct user	*up = (struct user *)0;
	register struct proc	*pp = (struct proc *)0;
	register struct pcb	*pcbp = (struct pcb *)0;
	register struct region	*rp = (struct region *)0;
	register struct pregion *prp = (struct pregion *)0;

	/*
	** the various include files that locore must include
	*/
	printf( "#include	\"sys/asm.h\"\n" );
	printf( "#include	\"sys/regdef.h\"\n" );
	printf( "#include	\"sys/fpu.h\"\n" );
	printf( "#include	\"sys/softfp.h\"\n" );
	printf( "#include	\"sys/signal.h\"\n" );

	/*
	** exception vector address
	*/
	printf( "#define	E_VEC	0x%0x\n", E_VEC );

	/*
	** various offsets and defines for the ublock
	*/
	printf( "ABS(u,0x%x)\n", UADDR );
	printf( "#define	U_PROCP	0x%x\n", &up->u_procp );

	/*
	** the nofault index defines
	*/
	printf( "#define	NF_BADADDR	%d\n", NF_BADADDR );
	printf( "#define	NF_COPYIO	%d\n", NF_COPYIO );
	printf( "#define	NF_ADDUPC	%d\n", NF_ADDUPC );
	printf( "#define	NF_FSUMEM	%d\n", NF_FSUMEM );
	printf( "#define	NF_USERACC	%d\n", NF_USERACC );
	printf( "#define	NF_SOFTFP	%d\n", NF_SOFTFP );
	printf( "#define	NF_REVID	%d\n", NF_REVID );
	printf( "#define	NF_SOFTFPI	%d\n", NF_SOFTFPI );
	printf( "#define	NF_FIXADE	%d\n", NF_FIXADE );

	/*
	** various offsets and defines for the proc structure
	*/
	printf( "#define	P_FLAG	0x%x\n", &pp->p_flag );
	printf( "#define	P_SYSID	0x%x\n", &pp->p_sysid );
	printf( "#define	P_TLBPID	0x%x\n", &pp->p_tlbpid );
	printf( "#define	P_UBPTBL	0x%x\n", pp->p_ubptbl );
	printf( "#define	P_TLBHI_TBL	0x%x\n", pp->p_tlbhi_tbl );
	printf( "#define	P_FP	0x%x\n", &pp->p_fp);
	printf( "#define	P_FP_SIGINTR1	%d\n", P_FP_SIGINTR1);
	printf( "#define	P_FP_SIGINTR2	%d\n", P_FP_SIGINTR2);

	/*
	** defines for Co-processor 0
	*/
	printf( "#define	C0_BADVADDR	$8\n" );
	printf( "#define	C0_CAUSE	$13\n" );
	printf( "#define	C0_CTXT		$4\n" );
	printf( "#define	C0_EPC		$14\n" );
	printf( "#define	C0_ERROR	$7\n" );
	printf( "#define	C0_INX		$0\n" );
	printf( "#define	C0_SR		$12\n" );
	printf( "#define	C0_PID		$10\n" );
	printf( "#define	C0_PIDMASK	$6\n" );
	printf( "#define	C0_PRID		$15\n" );
	printf( "#define	C0_TLBHI	$10\n" );
	printf( "#define	C0_TLBLO	$2\n" );

	printf( "#define	C0_PROBE	%d\n", C0_PROBE );
	printf( "#define	C0_READI	%d\n", C0_READI );
	printf( "#define	C0_RFE		%d\n", C0_RFE );
	printf( "#define	C0_WRITEI	%d\n", C0_WRITEI );
	printf( "#define	C0_WRITER	%d\n", C0_WRITER );

	printf( "#define	SR_CU1	0x%x\n", SR_CU1 );
	printf( "#define	SR_IEC	0x%x\n", SR_IEC );
	printf( "#define	SR_IEP	0x%x\n", SR_IEP );
	printf( "#define	SR_ISC	0x%x\n", SR_ISC );
	printf( "#define	SR_SWC	0x%x\n", SR_SWC );
	printf( "#define	SR_PZ	0x%x\n", SR_PZ );
	printf( "#define	SR_PE	0x%x\n", SR_PE );
	printf( "#define	SR_BEV	0x%x\n", SR_BEV );
	printf( "#define	SR_CM0	0x%x\n", SR_CM0 );
	printf( "#define	SR_CM1	0x%x\n", SR_CM1 );
	printf( "#define	SR_MM_MODE	0x%x\n", SR_MM_MODE );
	printf( "#define	SR_IMASK	0x%x\n", SR_IMASK );
	printf( "#define	SR_IMASK0	0x%x\n", SR_IMASK0 );
	printf( "#define	SR_IMASK1	0x%x\n", SR_IMASK1 );
	printf( "#define	SR_IMASK2	0x%x\n", SR_IMASK2 );
	printf( "#define	SR_IMASK3	0x%x\n", SR_IMASK3 );
	printf( "#define	SR_IMASK4	0x%x\n", SR_IMASK4 );
	printf( "#define	SR_IMASK5	0x%x\n", SR_IMASK5 );
	printf( "#define	SR_IMASK6	0x%x\n", SR_IMASK6 );
	printf( "#define	SR_IMASK7	0x%x\n", SR_IMASK7 );
	printf( "#define	SR_IMASK8	0x%x\n", SR_IMASK8 );
	printf( "#define	SR_KUP	0x%x\n", SR_KUP );

	printf( "#define	CAUSE_EXCMASK	0x%x\n", CAUSE_EXCMASK );
	printf( "#define	CAUSE_BD	0x%x\n", CAUSE_BD );
	printf( "#define	CAUSE_CEMASK	0x%x\n", CAUSE_CEMASK );
	printf( "#define	CAUSE_CESHIFT	0x%x\n", CAUSE_CESHIFT );
	printf( "#define	CAUSE_IP4	0x%x\n", CAUSE_IP4 );
	printf( "#define	CAUSE_IP5	0x%x\n", CAUSE_IP5 );
	printf( "#define	CAUSE_IP6	0x%x\n", CAUSE_IP6 );
	printf( "#define	CAUSE_IP8	0x%x\n", CAUSE_IP8 );
	printf( "#define	CAUSE_IPMASK	0x%x\n", CAUSE_IPMASK );
	printf( "#define	CAUSE_IPSHIFT	%d\n", CAUSE_IPSHIFT );
	printf( "#define	CAUSE_IP_FPINTR	0x%x\n", CAUSE_IP_FPINTR );

	printf( "#define	C0_ERROR_IMASK	0x%x\n", C0_ERROR_IMASK );
	printf( "#define	C0_ERROR_MASK	0x%x\n", C0_ERROR_MASK );
	printf( "#define	C0_ERROR_EXT	0x%x\n", C0_ERROR_EXT );

	/*
	** offsets for various fields in the PCB
	*/
	printf( "#define	PCB_FP	%d\n", PCB_FP );
	printf( "#define	PCB_PC	%d\n", PCB_PC );
	printf( "#define	PCB_S0	%d\n", PCB_S0 );
	printf( "#define	PCB_S1	%d\n", PCB_S1 );
	printf( "#define	PCB_S2	%d\n", PCB_S2 );
	printf( "#define	PCB_S3	%d\n", PCB_S3 );
	printf( "#define	PCB_S4	%d\n", PCB_S4 );
	printf( "#define	PCB_S5	%d\n", PCB_S5 );
	printf( "#define	PCB_S6	%d\n", PCB_S6 );
	printf( "#define	PCB_S7	%d\n", PCB_S7 );
	printf( "#define	PCB_SP	%d\n", PCB_SP );
	printf( "#define	PCB_SR	%d\n", PCB_SR );

	/*
	** offsets to the various registers in a jump buffer
	*/
	printf( "#define	JB_FP	%d\n", JB_FP );
	printf( "#define	JB_PC	%d\n", JB_PC );
	printf( "#define	JB_S0	%d\n", JB_S0 );
	printf( "#define	JB_S1	%d\n", JB_S1 );
	printf( "#define	JB_S2	%d\n", JB_S2 );
	printf( "#define	JB_S3	%d\n", JB_S3 );
	printf( "#define	JB_S4	%d\n", JB_S4 );
	printf( "#define	JB_S5	%d\n", JB_S5 );
	printf( "#define	JB_S6	%d\n", JB_S6 );
	printf( "#define	JB_S7	%d\n", JB_S7 );
	printf( "#define	JB_SP	%d\n", JB_SP );
	printf( "#define	JB_SR	%d\n", JB_SR );

	/*
	** various offsets for profiling info in the user structure
	*/
	printf( "#define	PR_BASE	0x%x\n", &up->u_prof.pr_base );
	printf( "#define	PR_OFF	0x%x\n", &up->u_prof.pr_off );
	printf( "#define	PR_SCALE	0x%x\n", &up->u_prof.pr_scale );
	printf( "#define	PR_SIZE	0x%x\n", &up->u_prof.pr_size );
#ifdef PIXIE
	printf( "#define	U_SAV_PIX 0x%x\n", &up->u_sav_pix );
	printf( "#define	U_JMP_PIX 0x%x\n", &up->u_jmp_pix );
#endif PIXIE

	/*
	** offsets into the u area for time keeping
	*/
	printf( "#define	U_UTIME	0x%x\n", &up->u_utime);
	printf( "#define	U_STIME	0x%x\n", &up->u_stime);
	printf( "#define	B_UTIME	0x%x\n", &up->u_ru.ru_utime);
	printf( "#define	B_STIME	0x%x\n", &up->u_ru.ru_stime);
	printf( "#define	R_UTIME	0x%x\n", &up->u_rambo_ticks[0]);
	printf( "#define	R_STIME	0x%x\n", &up->u_rambo_ticks[2]);
	printf( "#define	R_LEVEL 0x%x\n", &up->u_rambo_ticks[4]);

#ifdef notdef
	/*
	** defines for the exception frame offsets for registers
	*/
	printf( "#define	EF_A0	%d\n", EF_A0 );
	printf( "#define	EF_A1	%d\n", EF_A1 );
	printf( "#define	EF_A2	%d\n", EF_A2 );
	printf( "#define	EF_A3	%d\n", EF_A3 );
	printf( "#define	EF_AT	%d\n", EF_AT );
	printf( "#define	EF_BADVADDR	%d\n", EF_BADVADDR );
	printf( "#define	EF_CAUSE	%d\n", EF_CAUSE );
	printf( "#define	EF_EPC	%d\n", EF_EPC );
	printf( "#define	EF_FP	%d\n", EF_FP );
	printf( "#define	EF_GP	%d\n", EF_GP );
	printf( "#define	EF_K1	%d\n", EF_K1 );
	printf( "#define	EF_MDHI	%d\n", EF_MDHI );
	printf( "#define	EF_MDLO	%d\n", EF_MDLO );
	printf( "#define	EF_RA	%d\n", EF_RA );
	printf( "#define	EF_S0	%d\n", EF_S0 );
	printf( "#define	EF_S1	%d\n", EF_S1 );
	printf( "#define	EF_S2	%d\n", EF_S2 );
	printf( "#define	EF_S3	%d\n", EF_S3 );
	printf( "#define	EF_S4	%d\n", EF_S4 );
	printf( "#define	EF_S5	%d\n", EF_S5 );
	printf( "#define	EF_S6	%d\n", EF_S6 );
	printf( "#define	EF_S7	%d\n", EF_S7 );
	printf( "#define	EF_SAVEDPC	%d\n", EF_SAVEDPC );
	printf( "#define	EF_SIZE	%d\n", EF_SIZE );
	printf( "#define	EF_SP	%d\n", EF_SP );
	printf( "#define	EF_SR	%d\n", EF_SR );
	printf( "#define	EF_T0	%d\n", EF_T0 );
	printf( "#define	EF_T1	%d\n", EF_T1 );
	printf( "#define	EF_T2	%d\n", EF_T2 );
	printf( "#define	EF_T3	%d\n", EF_T3 );
	printf( "#define	EF_T4	%d\n", EF_T4 );
	printf( "#define	EF_T5	%d\n", EF_T5 );
	printf( "#define	EF_T6	%d\n", EF_T6 );
	printf( "#define	EF_T7	%d\n", EF_T7 );
	printf( "#define	EF_T8	%d\n", EF_T8 );
	printf( "#define	EF_T9	%d\n", EF_T9 );
	printf( "#define	EF_V0	%d\n", EF_V0 );
	printf( "#define	EF_V1	%d\n", EF_V1 );
#else
	printf( "#include \"sys/reg.h\"\n");
#endif notdef

	/*
	** tlb defines
	*/
	printf( "#define	TLBINX_INXSHIFT	%d\n", TLBINX_INXSHIFT );
	printf( "#define	TLBWIREDBASE	%d\n", TLBWIREDBASE );
	printf( "#define	TLBRANDOMBASE	%d\n", TLBRANDOMBASE );
	printf( "#define	NRANDOMENTRIES	%d\n", NRANDOMENTRIES );
	printf( "#define	NWIREDENTRIES	%d\n", NWIREDENTRIES );
	printf( "#define	TLBHI_VPNMASK	%d\n", TLBHI_VPNMASK );
	printf( "#define	TLBHI_VPNSHIFT	%d\n", TLBHI_VPNSHIFT );
	printf( "#define	TLBHI_PIDSHIFT	%d\n", TLBHI_PIDSHIFT );
	printf( "#define	TLBHI_PIDMASK	%d\n", TLBHI_PIDMASK );
	printf( "#define	TLBLO_D	%d\n", TLBLO_D );

	/*
	** misc defines
	*/
	printf( "#define	NBPW	%d\n", sizeof(int) );
	printf( "#define	REGBSIZE	%d\n", sizeof(int) );

	printf( "#define	USRDATA	0x%x\n", USRDATA );
	printf( "#define	USERSTACK	0x%x\n", USERSTACK );
	printf( "#define	EMULATE_AREA 0x%x\n", EMULATE_AREA );
	printf( "#define	REDZONEPAGES %d\n", REDZONEPAGES );
	printf( "#define	NPGPT %d\n", NPGPT );
	printf( "#define	NBPDE %d\n", sizeof (pde_t));
	printf( "#define	PG_PFNUM %d\n", PG_PFNUM );
	printf( "#define	PG_G %d\n", PG_G );
	printf( "#define	PG_N %d\n", PG_N );
	printf( "#define	PG_VR %d\n", PG_VR );
	printf( "#define	EA_SIZE %d\n", EA_SIZE );
	printf( "#define	PT_STACK %d\n", PT_STACK );
	printf( "#define	PGOFSET %d\n", NBPP - 1 );

	printf( "#define	K0BASE	0x%x\n", K0BASE );
	printf( "#define	K0SIZE	0x%x\n", K0SIZE );
	printf( "#define	K1BASE	0x%x\n", K1BASE );
	printf( "#define	K2BASE	0x%x\n", K2BASE );
	printf( "#define	KERNELSTACK	0x%x\n", KERNELSTACK );
	printf( "#define	KPTEBASE	0x%x\n", KPTEBASE );
	printf( "#define	KSTEBASE	0x%x\n", KSTEBASE );
	printf( "#define	UADDR		0x%x\n", UADDR );
	printf( "#define	SEXC_CPU	0x%x\n", SEXC_CPU );
	printf( "#define	SEXC_RESCHED	0x%x\n", SEXC_RESCHED );
#ifdef notdef
	printf( "#define	SBE_ADDR	0x%x\n", SBE_ADDR );
#endif
	printf( "#define GBA_RMW_UNLOCK		0x%x\n", GBA_RMW_UNLOCK );
	printf( "#define GBA0_MISC_BASE		0x%x\n", GBA0_MISC_BASE );
	printf( "#define GBA1_MISC_BASE		0x%x\n", GBA1_MISC_BASE );
	printf( "#define GBA0_MAP		0x%x\n", GBA0_MAP );
	printf( "#define GBA1_MAP		0x%x\n", GBA1_MAP );
	printf( "#define GBA_SPAN		0x%x\n", GBA_SPAN );
	printf( "#define GMAP_SHIFT		0x%x\n", GMAP_SHIFT );
	printf( "#define GBA_OPCODE_MASK	0x%x\n", GBA_OPCODE_MASK );
	printf( "#define GBA_OPCODE_RMW		0x%x\n", GBA_OPCODE_RMW );
	printf( "#define LOCK_IOA		0x%x\n", LOCK_IOA );
	printf( "#define	CSR_IVECTMASK	0x%x\n", CSR_IVECTMASK );
	printf( "#define	CSR_IVECTSET	0x%x\n", CSR_IVECTSET );
	printf( "#define	CSR_IVECTCLR	0x%x\n", CSR_IVECTCLR );
	printf( "#define	CSR_COMPARE	0x%x\n", CSR_COMPARE );
	printf( "#define	CSR_COUNT	0x%x\n", CSR_COUNT );
	printf( "#define	CSR_ERRREG	0x%x\n", CSR_ERRREG );
	printf( "#define	SBCerrBadType	0x%x\n", SBCerrBadType );
	printf( "#define	SBCerrBadType5	0x%x\n", SBCerrBadType5 );
	printf( "#define	CSR_IVECTSET_TIMER	0x%x\n",
		CSR_IVECTSET_TIMER);
	printf( "#define	CSR_IVECTSET_SW1	0x%x\n",
		CSR_IVECTSET_SW1);
	printf( "#define	CSR_IVECTSET_SW2	0x%x\n",
		CSR_IVECTSET_SW2);
	printf( "#define	CSR_IVECTMASK_NONE	0x%x\n",
		CSR_IVECTMASK_NONE);
	printf( "#define	CSR_IVECTMASK_ALL	0x%x\n",
		CSR_IVECTMASK_ALL);
	printf( "#define	CSR_IVECTMASK_SW1	0x%x\n",
		CSR_IVECTMASK_SW1);
	printf( "#define	CSR_IVECTMASK_SW2	0x%x\n",
		CSR_IVECTMASK_SW2);
	printf( "#define	CSR_IVECTMASK_GBA	0x%x\n",
		CSR_IVECTMASK_GBA);
	printf( "#define	CSR_IVECTMASK_IO	0x%x\n",
		CSR_IVECTMASK_IO);
	printf( "#define	CSR_IVECTMASK_TIMER	0x%x\n",
		CSR_IVECTMASK_TIMER);
	printf( "#define IOA3	0x%x\n", IOA3 );
	printf( "#define IOA2	0x%x\n", IOA2 );
	printf( "#define IOA1	0x%x\n", IOA1 );
	printf( "#define IOA_ERRORINFO_REG	0x%x\n", IOA_ERRORINFO_REG );
	printf( "#define IOC_BUSY_RESET		0x%x\n", IOC_BUSY_RESET );
	/*
	 *  Parallel instructions, when SR_MM_MODE is set
	 */
	printf( "#define	lcache	lwl \n" );
	printf( "#define	scache	swl \n" );
	printf( "#define	flush	lwr $0, \n" );
	printf( "#define	inval	swr $0, \n" );
	/*
	 *  R6000 cache line byte sizes, and alignment shifts
	 */
	printf( "#define R6000_ILINE_BSIZE	32 \n");
	printf( "#define R6000_ILINE_SHIFT	5 \n");
	printf( "#define R6000_DLINE_BSIZE	8 \n");
	printf( "#define R6000_SLINE_BSIZE	128 \n");
	printf( "#define R6000_SLINE_SHIFT	7 \n");

	printf( "#define	SYS_exece	%d\n", SYS_execve );
	printf( "#define	SYS_exit	%d\n", SYS_exit );

	printf( "#define	PCB_FPREGS	0x%x\n", pcbp->pcb_fpregs );
	printf( "#define	CE_PANIC	%d\n", CE_PANIC );
	printf( "#define	PG_ADDR		0x%x\n", PG_ADDR );
	printf( "#define	PCB_OWNEDFP	%d\n", &pcbp->pcb_ownedfp);
	printf( "#define	PCB_FPC_CSR	%d\n", &pcbp->pcb_fpc_csr);
	printf( "#define	PCB_FPC_EIR	%d\n", &pcbp->pcb_fpc_eir);
	printf( "#define	PCB_BD_EPC	%d\n", &pcbp->pcb_bd_epc);
	printf( "#define	PCB_BD_CAUSE	%d\n", &pcbp->pcb_bd_cause);
	printf( "#define	PCB_BD_RA	%d\n", &pcbp->pcb_bd_ra);
	printf( "#define	PCB_BD_INSTR	%d\n", &pcbp->pcb_bd_instr);
	printf( "#define	PCB_SOFTFP_PC	%d\n", &pcbp->pcb_softfp_pc);
	printf( "#define	PCB_RESCHED	%d\n", &pcbp->pcb_resched);

	printf( "#define	SR_IBIT_FPINTR	0x%x\n", SR_IBIT_FPINTR);
	printf( "#define	SR_IBIT4	0x%x\n", SR_IBIT4);
	printf( "#define	SR_IBIT6	0x%x\n", SR_IBIT6);
	printf( "#define	SR_IBIT8	0x%x\n", SR_IBIT8);
	printf( "#define	SR_2030MASK	0x%x\n", SR_2030MASK );

	printf( "#define	EXC_RMISS	%d\n", EXC_RMISS);
	printf( "#define	EXC_WMISS	%d\n", EXC_WMISS);
	printf( "#define	EXC_DBE		%d\n", EXC_DBE);
	printf( "#define	EXC_IBE		%d\n", EXC_IBE);
	printf( "#define	EXC_RADE	%d\n", EXC_RADE);
	printf( "#define	EXC_WADE	%d\n", EXC_WADE);
	printf( "#define	EXC_CHECK	%d\n", EXC_CHECK);

	printf( "#define	EFAULT	%d\n", EFAULT );

	/*
	 * offset into region
	 */
	printf( "#define	R_LIST %d\n", &rp->r_list );
	printf( "#define	R_LISTSZ %d\n", &rp->r_listsz );
	printf( "#define	P_REG %d\n", &prp->p_reg );
	printf( "#define	P_REGVA %d\n", &prp->p_regva );


	printf( "#define	BPTSHFT		%d\n", BPTSHFT );
	printf( "#define	PNUMSHFT	%d\n", PNUMSHFT );
	printf( "#define	PTE_PNUMSHFT	%d\n", PTE_PNUMSHFT );
	printf( "#define	POFFMASK	%d\n", POFFMASK );
	printf( "#define	NBPP		%d\n", NBPP );
	printf( "#define	SNUMSHFT	%d\n", SNUMSHFT );
	printf( "#define	SOFFMASK	%d\n", SOFFMASK );
	printf( "#define	MINCACHE	+(4*1024)\n" );
	printf( "#define	MAXCACHE	+(64*1024)\n" );


	/*
	** oddles of prom stuff
	*/
	printf( "#define	PROM_RESET	0x%x\n", PROM_RESET );
	printf( "#define	PROM_EXEC	0x%x\n", PROM_EXEC );
	printf( "#define	PROM_RESTART	0x%x\n", PROM_RESTART );
	printf( "#define	PROM_REINIT	0x%x\n", PROM_REINIT );
	printf( "#define	PROM_REBOOT	0x%x\n", PROM_REBOOT );
	printf( "#define	PROM_OPEN	0x%x\n", PROM_OPEN );
	printf( "#define	PROM_CLOSE	0x%x\n", PROM_CLOSE );
	printf( "#define	PROM_READ	0x%x\n", PROM_READ );
	printf( "#define	PROM_IOCTL	0x%x\n", PROM_IOCTL );
	printf( "#define	PROM_DISABLECMD	0x%x\n", PROM_DISABLECMD );
	printf( "#define	PROM_AUTOBOOT	0x%x\n", PROM_AUTOBOOT );
	printf( "#define	PROM_GETS	0x%x\n", PROM_GETS );
	printf( "#define	PROM_GETCHAR	0x%x\n", PROM_GETCHAR );
	printf( "#define	PROM_PUTCHAR	0x%x\n", PROM_PUTCHAR );
	printf( "#define	PROM_PRINTF	0x%x\n", PROM_PRINTF );
	printf( "#define	PROM_ORW_RMW	0x%x\n", PROM_ORW_RMW );
	printf( "#define	PROM_ORH_RMW	0x%x\n", PROM_ORH_RMW );
	printf( "#define	PROM_ORB_RMW	0x%x\n", PROM_ORB_RMW );
	printf( "#define	PROM_ANDW_RMW	0x%x\n", PROM_ANDW_RMW );
	printf( "#define	PROM_ANDH_RMW	0x%x\n", PROM_ANDH_RMW );
	printf( "#define	PROM_ANDB_RMW	0x%x\n", PROM_ANDB_RMW );
	printf(	"#define	PROM_GETENV	0x%x\n", PROM_GETENV );
	printf(	"#define	PROM_SETENV	0x%x\n", PROM_SETENV );
	printf(	"#define	PROM_NV_GET	0x%x\n", PROM_NV_GET );
	printf(	"#define	PROM_NV_SET	0x%x\n", PROM_NV_SET );
#ifdef PROBE_BUG
	printf( "#define	PROBE_BUG	1\n"); 
#endif PROBE_BUG
	printf( "#define	TLBLO_G		0x%x\n", TLBLO_G );
	printf( "#define	TLBINX_INXMASK	0x%x\n", TLBINX_INXMASK );
	printf( "#define	UTLBSAVE	0x%x\n", &u.u_pcb.pcb_c2regs[0] );
	printf( "#define	UP_U_KSEGFLG	0x%x\n", &u.u_ksegflg );
}
