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
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: ctlspace.h,v 1.6.1.2 90/05/10 06:09:00 wje Exp $ */

#ifndef	_SYS_CTLSPACE_
#define	_SYS_CTLSPACE_	1

/* Copyright(C) 1988, MIPS Computer Systems */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *  defined bit positions within Interrupt Vector 
 */
#define	CSR_IVECTSET_TIMER		0x00000001
#define	CSR_IVECTSET_MEM_ERR		0x00000002
#define	CSR_IVECTSET_MEM_ERR_BIT		 1
#define	CSR_IVECTSET_IOA_ERR		0x00000004
#define	CSR_IVECTSET_IOA_ERR_BIT		 2
#define CSR_IVECTSET_DUART		0x00000008
#define CSR_IVECTSET_IOA1_GBA1_OP	0x00000010
#define CSR_IVECTSET_IOA1_GBA0_OP	0x00000020
#define CSR_IVECTSET_IOA1_GBA1_LOP	0x00000040
#define CSR_IVECTSET_IOA1_GBA0_LOP	0x00000080
#define CSR_IVECTSET_IOA2_GBA1_OP	0x00000100
#define CSR_IVECTSET_IOA2_GBA0_OP	0x00000200
#define CSR_IVECTSET_IOA2_GBA1_LOP	0x00000400
#define CSR_IVECTSET_IOA2_GBA0_LOP	0x00000800
#define CSR_IVECTSET_IOA3_GBA1_OP	0x00001000
#define CSR_IVECTSET_IOA3_GBA0_OP	0x00002000
#define CSR_IVECTSET_IOA3_GBA1_LOP	0x00004000
#define CSR_IVECTSET_IOA3_GBA0_LOP	0x00008000
#define CSR_IVECTSET_SW1		0x40000000
#define CSR_IVECTSET_SW2		0x80000000

#define CSR_IVECTSET_GBA		( CSR_IVECTSET_IOA1_GBA0_OP	\
					| CSR_IVECTSET_IOA1_GBA1_OP	\
					| CSR_IVECTSET_IOA2_GBA0_OP	\
					| CSR_IVECTSET_IOA2_GBA1_OP	\
					| CSR_IVECTSET_IOA3_GBA0_OP	\
					| CSR_IVECTSET_IOA3_GBA1_OP	\
					)

/*
 *  The InterruptVector gives us the ability to very selectively enable and
 *  disable individual interrupts and classes of interrupts, but the classic
 *  Unix model of interrupts is a prioritized hierarchy of interrupts.
 *  For now, we'll implement the InterruptVectorMask values to follow this
 *  classic model as previously implemented for the R2000/R3000 in the 
 *  Status Register and Cause Register interrupt bits.
 */
#define CSR_IVECTMASK_NONE	( CSR_IVECTSET_TIMER	\
				| CSR_IVECTSET_MEM_ERR	\
				| CSR_IVECTSET_IOA_ERR	\
				| CSR_IVECTSET_DUART	\
				| CSR_IVECTSET_GBA	\
				| CSR_IVECTSET_SW1	\
				| CSR_IVECTSET_SW2	\
				)
#define CSR_IVECTMASK_ALL	0
#define CSR_IVECTMASK_SW1	(CSR_IVECTMASK_NONE & ~CSR_IVECTSET_SW1)
#define CSR_IVECTMASK_SW2	(CSR_IVECTMASK_SW1  & ~CSR_IVECTSET_SW2)
#define CSR_IVECTMASK_GBA	(CSR_IVECTMASK_SW2  & ~CSR_IVECTSET_GBA)
#define CSR_IVECTMASK_IO	(CSR_IVECTMASK_GBA  & ~CSR_IVECTSET_DUART)
#define CSR_IVECTMASK_TIMER	(CSR_IVECTMASK_IO   & ~CSR_IVECTSET_TIMER)

#endif	_SYS_CTLSPACE_
