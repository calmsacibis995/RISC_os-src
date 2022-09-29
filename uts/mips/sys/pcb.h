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
/* $Header: pcb.h,v 1.12.4.2 90/05/10 06:31:25 wje Exp $ */

#ifndef	_SYS_PCB_
#define	_SYS_PCB_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * MIPS process control block
 * MUST be first element of upage
 */

/*
 * pcb_regs indices
 */
#define	PCB_S0		0	/* callee saved regs.... */
#define	PCB_S1		1
#define	PCB_S2		2
#define	PCB_S3		3
#define	PCB_S4		4
#define	PCB_S5		5
#define	PCB_S6		6
#define	PCB_S7		7
#define	PCB_SP		8	/* stack pointer */
#define	PCB_FP		9	/* frame pointer */
#define	PCB_PC		10	/* program counter */
#define	PCB_SR		11	/* C0 status register */
#define	NPCBREGS	12	/* number of regs saved at ctxt switch */

/*
 * jmp_buf offsets
 */
#define	JB_S0		0	/* callee saved regs.... */
#define	JB_S1		1
#define	JB_S2		2
#define	JB_S3		3
#define	JB_S4		4
#define	JB_S5		5
#define	JB_S6		6
#define	JB_S7		7
#define	JB_SP		8	/* stack pointer */
#define	JB_FP		9	/* frame pointer */
#define	JB_PC		10	/* program counter */
#define	JB_SR		11	/* C0 status register */
#define	NJBREGS		12

/*
 * single step information
 * used to hold instructions that have been replaced by break's when
 * single stepping
 */
struct ssi {
	int ssi_cnt;			/* number of bp's installed */
	struct ssi_bp {
		unsigned *bp_addr;	/* address of replaced instruction */
		unsigned bp_inst;	/* replaced instruction */
	} ssi_bp[2];
};

typedef struct pcb
{
	/*
	 * General purpose registers saved at context switch time.
	 *
	 * NOTE: current switch assembler code assumes that pcb is first
	 * entry in u area, and that pcb_regs is the first thing in the
	 * pcb.
	 */
	int	pcb_regs[NPCBREGS];

	int	pcb_resched;	/* non-zero if time to resched */
	/* These are use in branch delay instruction emulation */
	int	pcb_bd_epc;	/* epc register */
	int	pcb_bd_cause;	/* cause register */
	int	pcb_bd_ra;	/* address to return to if doing bd emulation */
	int	pcb_bd_instr;	/* the branch instr for the bd emulation */
	/* This is use in fp instruction emulation */
	int	pcb_softfp_pc;	/* resulting pc after fp emulation */
	/*
	 * Space for the state of all the potential coprocessors. WASTEFUL!
	 */
	int	pcb_fpregs[32];	/* floating point */
	int	pcb_fpc_csr;	/* floating point control and status reg */
	int	pcb_fpc_eir;	/* floating point exception instruction reg */
	int	pcb_ownedfp;	/* has owned fp at one time */
	int	pcb_c2regs[32];	/* TBD */
	int	pcb_c3regs[32];	/* TBD */
	int	pcb_sstep;	/* non-zero if single stepping */
	struct	ssi pcb_ssi;	/* single step state info */
} pcb_t;

#endif	_SYS_PCB_
