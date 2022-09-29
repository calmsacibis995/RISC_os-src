/*
 *	$Header: pon_ecc.h,v 1.4.1.1 90/07/18 14:30:50 huang Exp $
 */
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
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

/********************************************************
 *							*
 *			ECCdef.h			*
 *							*
 ********************************************************/
/*
 *	Cache definitions.
 */
#define	LOG_IBLOCK	6			/* log2 bytes in I block */
#define	LOG_DBLOCK	6			/* log2 bytes in D block */
#define	IBLOCKSIZE	16			/* words in an I block */
#define	DBLOCKSIZE	16			/* words in a D block */
/*
 *	Slot/bank/row definitions.
 */
#define	M2K_BankSize	(8 << 20)		/* bank size (Mb) */
#define	MEM_ROW		(1 << 2)		/* row selection bit */
/*
 *	Memory register addresses.
 */
#define	M2K_MemSpacePitch	256		/* pitch between boards */
#define	M2K_MemAddrBase		0xbd100000
/*
 *	Memory address register.
 */
#define	MAR_PRIV(a)	(((a) >> 16) & 0xfe00)
#define	MAR_VME(a)	(((a) >> 24) & 0x00fe)
/*
 *	Memory control register bits.
 */
#define	M2K_McrVmeEnb		(1 << 1)	/* VME port enable */
#define	M2K_McrSynErrr		(1 << 3)	/* enable syndrome latches */
#define	M2K_McrSglErr		(1 << 5)	/* int on single error */
#define	M2K_McrDblErr		(1 << 6)	/* int on double error */
#define	M2K_McrEcc		(1 << 7)	/* inhibit correction */
#define	M2K_McrWrtData		(1 << 8)	/* inhibit data write */
#define	M2K_McrChkData		(1 << 9)	/* inhibit check write */
#define	M2K_McrLevel		(3 << 13)	/* int level mask */
#define	M2K_McrLevlNo		(0 << 13)	/* no int level */
#define	M2K_McrLevel3		(1 << 13)	/* level 3 */
#define	M2K_McrLevel1		(2 << 13)	/* level 1 */
#define	M2K_McrLevel7		(3 << 13)	/* level 7 */
/*
 *	Memory status register bits.
 */
#define	M2K_MsrPrvErr		(1 << 0)	/* VME = 1, Private = 0 */
#define	M2K_MsrSngErr		(1 << 1)	/* single bit error */
#define	M2K_MsrDblErr		(1 << 2)	/* double bit error */
#define	M2K_MsrOddAdr		(1 << 3)	/* error in odd row */
#define	M2K_MsrBank(s)		(((s)>>4)&3)	/* bank number of error */
#define	M2K_MsrSynd(s)		(((s)>>8)&0x7f)	/* syndrome of error */
/*
 *	Joe random useful addresses.
 */
#define	DiagBase		0x9e010003	/* diag state base address */
#define	M2K_ErrAddrReg		0xbe000800	/* system bus error address */
#define	M2K_VmeImr		0xbe020003	/* VME interrupt mask reg */
#define	M2K_VmeIsr		0xbe020007	/* VME interrupt status reg */
#define	M2K_ErrStatusReg	0xbe100002	/* System bus error reg */
/*
 *	CPU error status register definitions.
 */
#define	M2K_IsrAcFail		(1 << 5)	/* power fail */
#define	M2K_IsrRdProEr		(1 << 4)	/* read protocol error (L) */
#define	M2K_IsrRdTimOt		(1 << 3)	/* read time out (L) */
#define	M2K_IsrRdBusEr		(1 << 2)	/* read bus error (L) */
#define	M2K_IsrWrTimOt		(1 << 1)	/* write time out (L) */
#define	M2K_IsrWrBusEr		(1 << 0)	/* write bus error (L) */
/*
 *	VME address macros.
 */
#define	PHYS_TO_VME(x)	((unsigned) (x) | 0x10000000)
#define	VME_TO_PHYS(x)	((unsigned) (x) & 0xefffffff)
/*
 *	VME interrupt masks and IACK addresses.
 */
#define	M2K_VmeIrq7		(1 << 7)
#define	M2K_VmeIrq6		(1 << 6)
#define	M2K_VmeIrq5		(1 << 5)
#define	M2K_VmeIrq4		(1 << 4)
#define	M2K_VmeIrq3		(1 << 3)
#define	M2K_VmeIrq2		(1 << 2)
#define	M2K_VmeIrq1		(1 << 1)
#define	M2K_VmeIAck7		0xbdf0000e
#define	M2K_VmeIAck6		0xbdf0000c
#define	M2K_VmeIAck5		0xbdf0000a
#define	M2K_VmeIAck4		0xbdf00008
#define	M2K_VmeIAck3		0xbdf00006
#define	M2K_VmeIAck2		0xbdf00004
#define	M2K_VmeIAck1		0xbdf00002
/*
 *	Exception frame offsets.
 */
#define	E_AT		3
#define	E_RA		4
#define	E_V0		5
#define	E_V1		6
#define	E_A0		7
#define	E_A1		8
#define	E_A2		9
#define	E_A3		10
#define	E_T0		11
#define	E_T1		12
#define	E_T2		13
#define	E_T3		14
#define	E_T4		15
#define	E_T5		16
#define	E_T6		17
#define	E_T7		18
#define	E_T8		19
#define	E_T9		20
#define	E_EPC		21
#define	E_FRAME_SIZE	22
/*
 *	M2000 write buffer flush macro.
 */
#define	FLUSHWB		.set	noat;\
			lui	$at,0xbfc0;\
			lw	$0,0x0010($at);\
			.set	at;
/*
 *	Test variable structure:
 *
 *	  This is used since data available through the "gp" is in
 *	PROM and can't be reliably modified.
 */
#define	WorkSpace	0xa0022000	/* offset to avoid cache conflicts */
#define	TestVarBase	0x80021000	/* offset to avoid cache conflicts */
#ifndef	LOCORE
typedef struct {
  unsigned BankNumber : 3;		/* memory bank number */
  unsigned long Cause;			/* exception cause */
  unsigned long CpuIsr;			/* CPU interrupt status register */
  unsigned long VmeIsr;			/* VME interrupt status register */
  unsigned long ErrAdr;			/* error address register */
  unsigned short IAck[8];		/* interrupt acknowledge values */
  long BitArray[128];			/* data->check bit translation table */
} TestVar;
#endif	LOCORE
/*
 *	Return frame offsets.
 */
#define	SaveK0		0
#define	SaveK1		1
#define	SaveRa		2
#define	SaveFp		3
#define	SaveV0		4
#define	SaveSize	5
/*
 *	Local board type.
 */
#define	NoBoard		0
#define	M2k32Meg	1		/* 32 Mb */
#define	M2k16Meg	2		/* 16 Mb */
/*
 *	Macros to allow C code to access memory registers cleanly.
 */
#define	M2kBase(s)	((volatile short *) (M2K_MemAddrBase + (s) *\
			 M2K_MemSpacePitch))
#define	M2kIV(s)	M2kBase(s)[MEM_IV / sizeof(short)]
#define	M2kCNTRL(s)	M2kBase(s)[MEM_CNTRL / sizeof(short)]
#define	M2kADDR(s)	M2kBase(s)[MEM_ADDR / sizeof(short)]
#define	M2kSTAT(s)	M2kBase(s)[MEM_STAT / sizeof(short)]
