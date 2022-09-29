#ident "$Header: regdesc.c,v 1.6 90/01/11 14:48:13 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * regdesc.c -- %r descriptions for MIPS cpu registers
 */

#include "machine/cpu.h"
#include "saio/saioctl.h"

#define	NULL	0

/*
 * CP0 status register description
 */
struct reg_values imask_values[] = {
	{ SR_IMASK8,	"8" },
	{ SR_IMASK7,	"7" },
	{ SR_IMASK6,	"6" },
	{ SR_IMASK5,	"5" },
	{ SR_IMASK4,	"4" },
	{ SR_IMASK3,	"3" },
	{ SR_IMASK2,	"2" },
	{ SR_IMASK1,	"1" },
	{ SR_IMASK0,	"0" },
	{ 0,		NULL },
};

struct reg_desc sr_desc[] = {
	/* mask	     shift      name   format  values */
	{ SR_CU3,	0,	"CU3",	NULL,	NULL },
	{ SR_CU2,	0,	"CU2",	NULL,	NULL },
	{ SR_CU1,	0,	"CU1",	NULL,	NULL },
	{ SR_CU0,	0,	"CU0",	NULL,	NULL },
	{ SR_BEV,	0,	"BEV",	NULL,	NULL },
	{ SR_TS,	0,	"TS",	NULL,	NULL },
	{ SR_PE,	0,	"PE",	NULL,	NULL },
	{ SR_CM,	0,	"CM",	NULL,	NULL },
	{ SR_PZ,	0,	"PZ",	NULL,	NULL },
	{ SR_SWC,	0,	"SwC",	NULL,	NULL },
	{ SR_ISC,	0,	"IsC",	NULL,	NULL },
	{ SR_IBIT8,	0,	"IM8",	NULL,	NULL },
	{ SR_IBIT7,	0,	"IM7",	NULL,	NULL },
	{ SR_IBIT6,	0,	"IM6",	NULL,	NULL },
	{ SR_IBIT5,	0,	"IM5",	NULL,	NULL },
	{ SR_IBIT4,	0,	"IM4",	NULL,	NULL },
	{ SR_IBIT3,	0,	"IM3",	NULL,	NULL },
	{ SR_IBIT2,	0,	"IM2",	NULL,	NULL },
	{ SR_IBIT1,	0,	"IM1",	NULL,	NULL },
	{ SR_IMASK,	0,	"IPL",	NULL,	imask_values },
	{ SR_KUO,	0,	"KUo",	NULL,	NULL },
	{ SR_IEO,	0,	"IEo",	NULL,	NULL },
	{ SR_KUP,	0,	"KUp",	NULL,	NULL },
	{ SR_IEP,	0,	"IEp",	NULL,	NULL },
	{ SR_KUC,	0,	"KUc",	NULL,	NULL },
	{ SR_IEC,	0,	"IEc",	NULL,	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

struct reg_desc sr_desc_r6000[] = {
	/* mask	     shift      name   format  values */
	{ SR_CU3,	0,	"CU3",	NULL,	NULL },
	{ SR_CU2,	0,	"CU2",	NULL,	NULL },
	{ SR_CU1,	0,	"CU1",	NULL,	NULL },
	{ SR_CU0,	0,	"CU0",	NULL,	NULL },
	{ SR_RE,	0,	"RE",	NULL,	NULL },
	{ SR_BEV,	0,	"BEV",	NULL,	NULL },
	{ SR_CM1,	0,	"CM1",	NULL,	NULL },
	{ SR_CM0,	0,	"CM0",	NULL,	NULL },
	{ SR_PZ,	0,	"PZ",	NULL,	NULL },
	{ SR_ITP,	0,	"ITP",	NULL,	NULL },
	{ SR_MM_MODE,	0,	"MM_MODE",NULL,	NULL },
	{ SR_IBIT8,	0,	"IM8",	NULL,	NULL },
	{ SR_IBIT7,	0,	"IM7",	NULL,	NULL },
	{ SR_IBIT6,	0,	"IM6",	NULL,	NULL },
	{ SR_IBIT5,	0,	"IM5",	NULL,	NULL },
	{ SR_IBIT4,	0,	"IM4",	NULL,	NULL },
	{ SR_IBIT3,	0,	"IM3",	NULL,	NULL },
	{ SR_IBIT2,	0,	"IM2",	NULL,	NULL },
	{ SR_IBIT1,	0,	"IM1",	NULL,	NULL },
	{ SR_IMASK,	0,	"IPL",	NULL,	imask_values },
	{ SR_KUO,	0,	"KUo",	NULL,	NULL },
	{ SR_IEO,	0,	"IEo",	NULL,	NULL },
	{ SR_KUP,	0,	"KUp",	NULL,	NULL },
	{ SR_IEP,	0,	"IEp",	NULL,	NULL },
	{ SR_KUC,	0,	"KUc",	NULL,	NULL },
	{ SR_IEC,	0,	"IEc",	NULL,	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

/*
 * CP0 cause register description
 */
struct reg_values exc_values[] = {
	{ EXC_INT,	"INT" },
	{ EXC_MOD,	"MOD" },
	{ EXC_RMISS,	"RMISS" },
	{ EXC_WMISS,	"WMISS" },
	{ EXC_RADE,	"RADE" },
	{ EXC_WADE,	"WADE" },
	{ EXC_IBE,	"IBE" },
	{ EXC_DBE,	"DBE" },
	{ EXC_SYSCALL,	"SYSCALL" },
	{ EXC_BREAK,	"BREAK" },
	{ EXC_II,	"II" },
	{ EXC_CPU,	"CPU" },
	{ EXC_OV,	"OV" },
	{ EXC_TRAP,	"TRAP" },
	{ EXC_DBL_NC,	"DBL_NC" },
	{ EXC_CHECK,	"CHECK" },
	{ 0,		NULL },
};

struct reg_desc cause_desc[] = {
	/* mask	     shift      name   format  values */
	{ CAUSE_BD,	0,	"BD",	NULL,	NULL },
	{ CAUSE_CEMASK,	-CAUSE_CESHIFT,	"CE",	"%d",	NULL },
	{ CAUSE_IP8,	0,	"IP8",	NULL,	NULL },
	{ CAUSE_IP7,	0,	"IP7",	NULL,	NULL },
	{ CAUSE_IP6,	0,	"IP6",	NULL,	NULL },
	{ CAUSE_IP5,	0,	"IP5",	NULL,	NULL },
	{ CAUSE_IP4,	0,	"IP4",	NULL,	NULL },
	{ CAUSE_IP3,	0,	"IP3",	NULL,	NULL },
	{ CAUSE_SW2,	0,	"SW2",	NULL,	NULL },
	{ CAUSE_SW1,	0,	"SW1",	NULL,	NULL },
	{ CAUSE_EXCMASK,0,	"EXC",	NULL,	exc_values },
	{ 0,		0,	NULL,	NULL,	NULL },
};

/* 
 * Physical Tag  desc for 6000 
 */
struct reg_desc ptag_desc_6000[] = {
	/* mask	     		shift   name   format  values */
	{ TLB_PFNMASK_R6000,	  0,	"PA",	"0x%x",	NULL },
	{ 0,			  0,	NULL,	NULL,	NULL },
};

/* 
 * tlb entry desc for 6000 tlbs
 */
struct reg_desc tlb_desc_6000[] = {
	/* mask	     		shift   name   format  values */
	{ TLB_PFNMASK_R6000,	  0,	"PA",	"0x%x",	NULL },
	{ TLB_N_R6000,	  	  0,	"N",	NULL,	NULL },
	{ TLB_D_R6000,	  	  0,	"D",	NULL,	NULL },
	{ TLB_V_R6000,	  	  0,	"V",	NULL,	NULL },
	{ TLB_G_R6000,	  	  0,	"G",	NULL,	NULL },
	{ 0,			  0,	NULL,	NULL,	NULL },
};

/*
 * CP0 tlb entry high description
 */
struct reg_desc tlbhi_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBHI_VPNMASK,0,	"VA",	"0x%x",	NULL },
	{ TLBHI_PIDMASK,-TLBHI_PIDSHIFT,"PID",	"%d",	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

/*
 * CP0 tlb entry low description
 */
struct reg_desc tlblo_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBLO_PFNMASK,0,	"PA",	"0x%x",	NULL },
	{ TLBLO_N,	0,	"N",	NULL,	NULL },
	{ TLBLO_D,	0,	"D",	NULL,	NULL },
	{ TLBLO_V,	0,	"V",	NULL,	NULL },
	{ TLBLO_G,	0,	"G",	NULL,	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

/*
 * CP0 tlb index description
 */
struct reg_desc tlbinx_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBINX_PROBE,	0,	"PFAIL",NULL,	NULL },
	{ TLBINX_INXMASK, -TLBINX_INXSHIFT, "INDEX", "%d", NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

/*
 * CP0 tlb random description
 */
struct reg_desc tlbrand_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBRAND_RANDMASK, -TLBRAND_RANDSHIFT, "RANDOM", "%d", NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};

/*
 * CP0 context description
 */
struct reg_desc tlbctxt_desc[] = {
	/* mask	     shift      name   format  values */
	{ TLBCTXT_BASEMASK, 0,	"PTEBASE", "0x%x", NULL },
	{ TLBCTXT_VPNMASK, 11,	"BADVAP", "0x%x", NULL},
	{ 0,		0,	NULL,	NULL,	NULL },
};

/*
 * CP0 Error Register description
 */
struct reg_desc error_desc_r6000[] = {
	/* mask	     shift      name   format  values */
	{ C0_ERROR_IEXT,0,	"IEXT",	NULL,	NULL },
	{ C0_ERROR_IRF,	0,	"IRF",	NULL,	NULL },
	{ C0_ERROR_IDB,	0,	"IDB",	NULL,	NULL },
	{ C0_ERROR_IIB,	0,	"IIB",	NULL,	NULL },
	{ C0_ERROR_EXT,0,	"EXT",	NULL,	NULL },
	{ C0_ERROR_RF,	0,	"RF",	NULL,	NULL },
	{ C0_ERROR_DB,	0,	"DB",	NULL,	NULL },
	{ C0_ERROR_IB,	0,	"IB",	NULL,	NULL },
	{ 0,		0,	NULL,	NULL,	NULL },
};
