#ident "$Header: pon_subr_3030.s,v 1.2.1.1 90/07/18 14:33:18 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright
# |-----------------------------------------------------------|
# | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restrictive Rights Legend                        |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 252.227-7013.  |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#  $ */

#include "machine/asm.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/regdef.h"
#include "machine/i8042.h"
#include "prom/prom.h"
#include "pon.h"

#define	KBD_DELAY		20000

#define	NOP_DELAY \
				nop; \
				nop; \
				nop; \
				nop; \
				nop; \
				nop; \
				nop; \
				nop; \
				nop; \
				nop

#ifdef	SABLE
#define DELAY(x)
#else	SABLE
#define	DELAY(x) \
				li	t5,+(x); \
9:				subu	t5,1; \
				bne	zero,t5,9b
#endif	SABLE

/*
 * GetIntR: Get the contents of the M20 system interrupt register.
 */
LEAF(GetIntR)

		lw	v0,K1BASE|(INT_REG&0x1ffffffc)
		j	ra

END(GetIntR)


/*
 * GetKIntR: Get the M20 interrupt and FPU status information from the
 *	     keyboard port 1 register.
 */
LEAF(GetKIntR)

		li	t0,K1BASE|KBD_BASE	# keyboard address

	/*
	 * Wait for input register to be ready.
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for command

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		li	t1,I8042RIP
		sw	t1,4(t0)		# send read input port command
		NOP_DELAY

	/*
	 * Wait for input register to be ready
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042OBF		# mask output buffer full bit
		bne	t2,zero,2f		# ready for data

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		lw	v0,0(t0)		# get input port data
		j	ra
3:
		li	v0,-1
		j	ra

END(GetKIntR)


/*
 * GetSErrR: Get the contents of the M20 system error register.
 */
LEAF(GetSErrR)

		lw	v0,K1BASE|SYS_EREG
		j	ra

END(GetSErrR)


/*
 * GetSCtlR: Get the contents of the M20 system control register.
 */
LEAF(GetSCtlR)

		lw	v0,K1BASE|SYS_CREG
		j	ra

END(GetSCtlR)


/*
 * SetSCtlR: Set the contents of the M20 system control register.
 */
LEAF(SetSCtlR)

		sw	a0,K1BASE|SYS_CREG
		j	ra

END(SetSCtlR)


/*
 * GetKCnfR: Get the contents of the M20 system configuration register
 *	     from keyboard port 2.
 */
LEAF(GetKCnfR)

		li	t0,K1BASE|KBD_BASE	# keyboard address

	/*
	 * Wait for input register to be ready.
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for command

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		li	t1,I8042ROP
		sw	t1,4(t0)		# send read output port command
		NOP_DELAY

	/*
	 * Wait for input register to be ready
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042OBF		# mask output buffer full bit
		bne	t2,zero,2f		# ready for data

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		lw	v0,0(t0)		# get output port data
		j	ra
3:
		li	v0,-1
		j	ra

END(GetKCnfR)


/*
 * SetKCnfR: Set the contents of the system configuration register
 *	     from keyboard port 2.
 */
LEAF(SetKCnfR)

		li	t0,K1BASE|KBD_BASE	# keyboard address

	/*
	 * Wait for input register to be ready.
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for command

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		li	t1,I8042WOP
		sw	t1,4(t0)		# send write output port command
		NOP_DELAY

	/*
	 * Wait for output register to be ready
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for data

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		sw	a0,0(t0)		# put output port data
		j	ra
3:
		li	v0,-1
		j	ra

END(SetKCnfR)


/*
 * SetBadParity: Set force bad parity in the M20 system.
 */
LEAF(SetBadParity)

		li	t0,K1BASE|KBD_BASE	# keyboard address

	/*
	 * Wait for input register to be ready.
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for command

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		li	t1,I8042WOP
		sw	t1,4(t0)		# send write output port command
		NOP_DELAY

	/*
	 * Wait for input register to be ready
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for command

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
 #		and	t3,~P2_FORCE_BADP_B
		li	t3,0xed
		sw	t3,0(t0)		# set force bad parity
		DELAY(200)

		li	v0,OK
		j	ra
3:
		li	v0,BAD
		j	ra

END(SetBadParity)


/*
 * UnsetBadParity: Unset force bad parity in the M20 system.
 */
LEAF(UnsetBadParity)

		li	t0,K1BASE|KBD_BASE	# keyboard address

	/*
	 * Wait for input register to be ready.
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for command

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
		li	t1,I8042WOP
		sw	t1,4(t0)		# send write output port command
		NOP_DELAY

	/*
	 * Wait for input register to be ready
	 */
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,4(t0)		# get status
		and	t2,I8042IBF		# mask input buffer full bit
		beq	t2,zero,2f		# ready for command

		NOP_DELAY
		subu	t1,1
		bne	t1,zero,1b

		b	3f			# timed out
2:
 #		or	t3,P2_FORCE_BADP_B
		li	t3,0xef
		sw	t3,0(t0)		# unset force bad parity
		DELAY(200)

		li	v0,OK
		j	ra
3:
		li	v0,BAD
		j	ra

END(UnsetBadParity)


LEAF(SetPonEnviron)

		li	v1,K1BASE | (TODC_CLOCK_ADDR_R3030 + (4 * NVADDR_FAILCODE) + 3)
		lbu	v0,(v1)
		or	v0,a0
		sb	v0,(v1)
		j	ra

END(SetPonEnviron)


LEAF(GetPonEnviron)

		li	v1,K1BASE | (TODC_CLOCK_ADDR_R3030 + (4 * NVADDR_FAILCODE) + 3)
		lbu	v0,(v1)
		j	ra

END(GetPonEnviron)


LEAF(ResetPonEnviron)

		li	v1,K1BASE | (TODC_CLOCK_ADDR_R3030 + (4 * NVADDR_FAILCODE) + 3)
		sb	zero,(v1)
		j	ra

END(ResetPonEnviron)


LEAF(WrtRamdacCntrl13)

		.set	noreorder
		and	v0,a0,0xff
		srl	v1,a0,8
		and	v1,0xff
		li	t0,RAMDAC_ADRLO|K1BASE
		li	t1,RAMDAC_ADRHI|K1BASE
		li	t2,RAMDAC_CNTRL|K1BASE
		sw	v1,(t1)
		sw	v0,(t0)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		j	ra
		nop
		.set	reorder

END(WrtRamdacCntrl13)


LEAF(WrtRamdacCntrl14)

		.set	noreorder
		and	v0,a0,0xff
		srl	v1,a0,8
		and	v1,0xff
		li	t0,RAMDAC_ADRLO|K1BASE
		li	t1,RAMDAC_ADRHI|K1BASE
		li	t2,RAMDAC_CNTRL|K1BASE
		sw	v1,(t1)
		sw	v0,(t0)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		j	ra
		nop
		.set	reorder

END(WrtRamdacCntrl14)


LEAF(WrtRamdacCntrl256)

		.set	noreorder
		and	v0,a0,0xff
		srl	v1,a0,8
		and	v1,0xff
		li	t0,K1BASE|RAMDAC_ADRLO
		li	t1,K1BASE|RAMDAC_ADRHI
		li	t2,K1BASE|RAMDAC_CNTRL
		sw	v1,(t1)
		sw	v0,(t0)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		sw	a2,(t2)
		sw	a3,(t2)
		sw	a1,(t2)
		j	ra
		nop
		.set	reorder

END(WrtRamdacCntrl256)


#define	V_BLANK_BIT		0x20
#define	H_BLANK_BIT		0x10
#define	COLOR_RSV		0xCE

#define	CLOCK_DUR		1562500
#define	CLOCK_SLACK		1000

#define	COLOR_VBLNK_MIN		15
#define	COLOR_VBLNK_MAX		50

#define KREG			0x14000000+0x02080004
#define RAMBO_COUNT		0x1c000000+0xc00
#define	WRITEMASK_REG		0x16100000	/* R/W word register */

LEAF(CfbPresent)

		.set	noreorder
/*
 *  This routine is derived from a disassembly of the saio C version routine
 *  to detect the color frame buffer.  The C code has been left in as comments.
 *
 *  The algorithm is as follows:
 *	Read the kreg
 *	loop for a certain time of rambo info
 *		if the reserved bit change assume that we have been reading
 *			random information and not a color board
 *		count number of transision of the h_blank signal
 *	if the number of transitions of the hblank is reasonable then
 *		we have a color board
 */
 # CfbPresent() {
 #	register volatile unsigned long *time;
 #	register volatile unsigned long *kreg;
 #	register unsigned long stime, etime, ntime;
 #	register unsigned long skreg, s1kreg;
 #	register unsigned long v_count, color_ret;
 #	register unsigned long wrap_iminent;

 #	time = (volatile unsigned long *)PHYS_TO_K1(RAMBO_COUNT);
		li	a0,K1BASE|RAMBO_COUNT

 #	kreg = (volatile unsigned long *)PHYS_TO_K1(KREG);
		li	a1,K1BASE|KREG

 #	wrap_iminent = 0;
		move	t5,zero

 #	stime = *time;
		lw	a2,(a0)

 #	if ((stime + CLOCK_DUR + CLOCK_SLACK) < stime) {
		addu	t6,a2,CLOCK_DUR + CLOCK_SLACK
		.set	noat
		sltu	AT,t6,a2
		beq	AT,zero,1f
		nop
		.set	at

 #		wrap_iminent++;
		addu	t5,1

 #		etime = stime + CLOCK_DUR + CLOCK_SLACK;
		addu	a3,a2,CLOCK_DUR + CLOCK_SLACK

 #	} else {
		b	2f
		nop

1:
 #		etime = stime + CLOCK_DUR;
		addu	a3,a2,CLOCK_DUR

 #	}
2:
 #	skreg = *kreg;
		lw	t1,(a1)

 #	v_count = 0;
		move	t3,zero

 #	color_ret = 1;
		li	t4,1

 #	while (color_ret) {
		beq	t4,zero,3f
		nop
10:
 #		ntime = *time;
		lw	t0,(a0)

 #		if ( ((wrap_iminent && (ntime < stime)) | !wrap_iminent) &&
 #			(ntime > etime) ) {
		sltu	t7,zero,t5
		move	v1,t7
		move	t8,v1
		beq	t8,zero,4f
		nop
		sltu	t9,t0,a2
		move	v1,t9
4:
		move	t6,v1
		sltiu	t7,t5,1
		or	t8,t6,t7
		beq	t8,zero,5f
		nop
		.set	noat
		sltu	AT,a3,t0
		beq	AT,zero,5f
		nop
		.set	at

 #			break;
		b	3f
		nop

 #		}
5:
 #		if ((skreg ^ (s1kreg = *kreg)) & COLOR_RSV) {
		lw	t2,(a1)
		nop
		xor	t9,t1,t2
		and	t6,t9,COLOR_RSV
		beq	t6,zero,6f
		nop

 #			color_ret = 0;
		move	t4,zero

 #			break;
		b	3f
		nop

 #		}
6:
 #		if ((skreg ^ s1kreg) & V_BLANK_BIT) {
		xor	t7,t1,t2
		and	t8,t7,0x20
		beq	t8,zero,7f
		nop

 #			v_count++;
		addu	t3,1

 #			skreg = s1kreg;
		move	t1,t2

 #		}
 #	}
7:
		bne	t4,zero,10b
		nop
3:

 #	if (color_ret) {
		beq	t4,zero,8f
		nop

 #		if ((v_count > COLOR_VBLNK_MIN) && (v_count < COLOR_VBLNK_MAX)){
		sltiu	AT,t3,16
		bne	AT,zero,8f
		nop
		sltiu	AT,t3,50
		beq	AT,zero,8f
		nop

 #			*write_mask_reg = 0xffffffff;
		li	t9,-1
		li	t6,K1BASE|WRITEMASK_REG
		sw	t9,(t6)

 #			return(1);
		li	v0,1
		b	9f
		nop
 #		}
 #	}
8:

 #	return(0);
		move	v0,zero

 # }
9:
		j	ra
		nop
		.set	reorder

END(CfbPresent)
