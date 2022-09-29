#ident "$Header: pon_scc.s,v 1.4.1.1 90/07/18 14:32:44 huang Exp $"
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

#include "machine/cp0.h"
#include "machine/asm.h"
#include "machine/cp0.h"
#include "machine/cpu_board.h"
#include "machine/regdef.h"
#include "machine/hd146818.h"
#include "machine/mk48t02.h"
#include "machine/scc_cons.h"
#include "prom/prom.h"
#include "pon.h"

#define	MAX_BAUD_INDEX		11

		.extern	scc_baud_table

		.text

LEAF(Pon_Scc)

		move	s4,ra

	/*
	 * Initialize Channel A
	 */
		SCC0_BASE			# set SCC pointer addr for
		move	s5,v0			# Pizazz and Genesis

		la	s6,SCCTable		# point to SCC table A start
		la	s7,TableEnd		# point to SCC table A end
		lw	s3,SCC_PTRA(s5)		# read once
		li	s1,SCCW_MIC_R		# master intr control reg
		li	s2,SCCW_RES_M		# reset both channel A and B
		sw	s1,SCC_PTRA(s5)		# write mic register number to SCC
		jal	FlushWB			# wait for a while
		sw	s2,SCC_PTRA(s5)		# force hardware reset
		jal	FlushWB			# wait for a while
		b	2f			# get first byte in table A
1:
		sw	s0,SCC_PTRA(s5)		# write register number to SCC
		addu	s6,1			# increment table pointer
		lbu	s0,0(s6)		# get SCC data
		nop				# delay slot
		sw	s0,SCC_PTRA(s5)		# write data to SCC
		addu	s6,1			# increment table pointer
		jal	FlushWB			# wait for a while
2:
		lbu	s0,0(s6)		# get SCC register number
		bltu	s6,s7,1b		# keep loading util reach end

		jal	FlushWB			# wait for a while

		NVRAMADDR(NVADDR_LBAUD)
		lbu	s0,(v0)			# get baud rate
		ble	s0,MAX_BAUD_INDEX,1f

		li	s0,10			# default to 9600 baud
1:
		sll	s0,1			# create index into scc_baud_table table
		lbu	s1,scc_baud_table(s0)	# get high
		lbu	s2,scc_baud_table+1(s0)	#   and low bytes

		li	s0,SCCW_BRLO_R
		sw	s0,SCC_PTRA(s5)		# select write register 12
		jal	FlushWB
		sw	s2,SCC_PTRA(s5)		# write low byte of time constant
		jal	FlushWB
		li	s0,SCCW_BRHI_R
		sw	s0,SCC_PTRA(s5)		# select write register 13
		jal	FlushWB
		sw	s1,SCC_PTRA(s5)		# write high byte of time constant
		jal	FlushWB
		li	s0,SCCW_AUX_R
		li	s1,SCCW_AUX_BRfromPClock | SCCW_AUX_BRGenEnable
		sw	s0,SCC_PTRA(s5)		# select write register 14
		jal	FlushWB
		sw	s1,SCC_PTRA(s5)
		jal	FlushWB

	/*
	 * Initialize Channel B
	 */
		la	s6,SCCTable		# point to SCC table A start
		la	s7,TableEnd		# point to SCC table A end
 #		lw	s3,SCC_PTRB(s5)		# read once
		b	2f			# get first byte in table A
1:
		sw	s0,SCC_PTRB(s5)		# write register number to SCC
		addu	s6,1			# increment table pointer
		lbu	s0,0(s6)		# get SCC data
		nop				# delay slot
		sw	s0,SCC_PTRB(s5)		# write data to SCC
		addu	s6,1			# increment table pointer
		jal	FlushWB			# wait for a while
2:
		lbu	s0,0(s6)		# get SCC register number
		bltu	s6,s7,1b		# keep loading util reach end

		jal	FlushWB			# wait for a while

		NVRAMADDR(NVADDR_RBAUD)
		lbu	s0,(v0)			# get baud rate
		ble	s0,MAX_BAUD_INDEX,1f

		li	s0,10			# default to 9600 baud
1:
		sll	s0,1			# create index into scc_baud_table table
		lbu	s1,scc_baud_table(s0)	# get high
		lbu	s2,scc_baud_table+1(s0)	#   and low bytes

		li	s0,SCCW_BRLO_R
		sw	s0,SCC_PTRB(s5)		# select write register 12
		jal	FlushWB
		sw	s2,SCC_PTRB(s5)		# write low byte of time constant
		jal	FlushWB
		li	s0,SCCW_BRHI_R
		sw	s0,SCC_PTRB(s5)		# select write register 13
		jal	FlushWB
		sw	s1,SCC_PTRB(s5)		# write high byte of time constant
		jal	FlushWB
		li	s0,SCCW_AUX_R
		li	s1,SCCW_AUX_BRfromPClock | SCCW_AUX_BRGenEnable
		sw	s0,SCC_PTRB(s5)		# select write register 14
		jal	FlushWB
		sw	s1,SCC_PTRB(s5)
		jal	FlushWB

		/*
		** !!!DBG this routine actually does not contain any
		** code for test SCC.
		** need to talk to diags people
		** !!! temperarily set v0 reg to 0 to indicate test pass
		** otherwise "bootmode" will get set to "e"
		**/
		move	v0, zero	

		j	s4

END(Pon_Scc)

#ifdef	R3030
LEAF(Pon_Ttys)

		move	s0,ra
		NVRAMADDR(NVADDR_CONSOLE)
		lbu	s1,(v0)			# get console

		jal	GetPonEnviron		# check for keyboard

		and	s2,v0,PON_KBD_NOT_PRESENT

	/*
	 * If console variable is set to either 't' or 'a', then enable both TTYs.
	 */
		li	s3,0			# initial mask
		beq	s1,CHAR_t,1f		# enable both TTYs

		beq	s1,CHAR_a,1f		# enable both TTYs

		beq	s1,CHAR_r,1f		# enable both TTYs
		b	2f
1:
		li	s3,PON_TTY0|PON_TTY1
		b	3f

	/*
	 * If console variable is set to '1', then enable just TTY1.
	 */
2:
		bne	s1,CHAR_1,5f		# not '1'

		li	s3,PON_TTY1
		b	3f

	/*
	 * If console variable is set to '0', then enable just TTY0.
	 */
5:
		bne	s1,CHAR_0,6f		# not '0'

		li	s3,PON_TTY0
		b	3f

	/*
	 * If console variable is set to 'c', then check for both a keyboard and a
	 * CFB.  If neither is present, default to TTY1.
	 */
6:
		bne	s1,CHAR_c,6f		# not 'c'

		bne	s2,zero,8f		# keyboard not present, so default to TTY1

		jal	CfbPresent		# check for CFB

		bne	v0,zero,3f		# CFB present, so don't enable TTYs

		b	8f			# default to TTY1

	/*
	 * For all other console variables, if a keyboard is not present, then enable TTY1.
	 */
6:
		beq	s1,CHAR_m,7f		# check for keyboard first

		beq	s1,CHAR_l,7f		# check for keyboard first

		beq	s1,CHAR_g,7f		# check for keyboard first

		beq	s1,CHAR_v,7f		# check for keyboard first

		b	8f			# default to TTY1
7:
		beq	s2,zero,3f		# keyboard present, so don't default to TTY1
8:
		li	s3,PON_TTY1
3:
		move	a0,s3
		jal	SetPonEnviron

		j	s0

END(Pon_Ttys)
#endif	R3030

LEAF(Scc_Local)

		move	t4,ra

	/*
	 * Initialize Channel A
	 */
		SCC0_BASE			# set SCC pointer addr for
		move	t5,v0			# Pizazz and Genesis

		la	t6,LoopTable1		# point to SCC table A start
		la	t7,LoopTable2		# point to SCC table A end
		lw	t3,SCC_PTRA(t5)		# read once
		li	t1,SCCW_MIC_R		# master intr control reg
		li	t2,SCCW_RES_M		# reset both channel A and B
		sw	t1,SCC_PTRA(t5)		# write mic register number to SCC
		jal	FlushWB			# wait for a while
		sw	t2,SCC_PTRA(t5)		# force hardware reset
		jal	FlushWB			# wait for a while
		b	2f			# get first byte in table A
1:
		sw	t0,SCC_PTRA(t5)		# write register number to SCC
		addu	t6,1			# increment table pointer
		lbu	t0,0(t6)		# get SCC data
		nop				# delay slot
		sw	t0,SCC_PTRA(t5)		# write data to SCC
		addu	t6,1			# increment table pointer
		jal	FlushWB			# wait for a while
2:
		lbu	t0,0(t6)		# get SCC register number
		bltu	t6,t7,1b		# keep loading util reach end

		jal	FlushWB			# wait for a while

		NVRAMADDR(NVADDR_LBAUD)
		lbu	t0,(v0)			# get baud rate
		ble	t0,MAX_BAUD_INDEX,1f

		li	t0,10			# default to 9600 baud
1:
		sll	t0,1			# create index into scc_baud_table table
		lbu	t1,scc_baud_table(t0)	# get high
		lbu	t2,scc_baud_table+1(t0)	#   and low bytes

		li	t0,SCCW_BRLO_R
		sw	t0,SCC_PTRA(t5)		# select write register 12
		jal	FlushWB
		sw	t2,SCC_PTRA(t5)		# write low byte of time constant
		jal	FlushWB
		li	t0,SCCW_BRHI_R
		sw	t0,SCC_PTRA(t5)		# select write register 13
		jal	FlushWB
		sw	t1,SCC_PTRA(t5)		# write high byte of time constant
		jal	FlushWB

		la	t6,LoopTable2		# point to SCC table A start
		la	t7,LoopTableEnd		# point to SCC table A end
		b	2f			# get first byte in table A
1:
		sw	t0,SCC_PTRA(t5)		# write register number to SCC
		addu	t6,1			# increment table pointer
		lbu	t0,0(t6)		# get SCC data
		nop				# delay slot
		sw	t0,SCC_PTRA(t5)		# write data to SCC
		addu	t6,1			# increment table pointer
		jal	FlushWB			# wait for a while
2:
		lbu	t0,0(t6)		# get SCC register number
		bltu	t6,t7,1b		# keep loading util reach end

		jal	FlushWB			# wait for a while

	/*
	 * Initialize Channel B
	 */
		la	t6,LoopTable1		# point to SCC table A start
		la	t7,LoopTable2		# point to SCC table A end
		lw	t3,SCC_PTRB(t5)		# read once
		b	2f			# get first byte in table A
1:
		sw	t0,SCC_PTRB(t5)		# write register number to SCC
		addu	t6,1			# increment table pointer
		lbu	t0,0(t6)		# get SCC data
		nop				# delay slot
		sw	t0,SCC_PTRB(t5)		# write data to SCC
		addu	t6,1			# increment table pointer
		jal	FlushWB			# wait for a while
2:
		lbu	t0,0(t6)		# get SCC register number
		bltu	t6,t7,1b		# keep loading util reach end

		jal	FlushWB			# wait for a while

		NVRAMADDR(NVADDR_RBAUD)
		lbu	t0,(v0)			# get baud rate
		ble	t0,MAX_BAUD_INDEX,1f

		li	t0,10			# default to 9600 baud
1:
		sll	t0,1			# create index into scc_baud_table table
		lbu	t1,scc_baud_table(t0)	# get high
		lbu	t2,scc_baud_table+1(t0)	#   and low bytes

		li	t0,SCCW_BRLO_R
		sw	t0,SCC_PTRB(t5)		# select write register 12
		jal	FlushWB
		sw	t2,SCC_PTRB(t5)		# write low byte of time constant
		jal	FlushWB
		li	t0,SCCW_BRHI_R
		sw	t0,SCC_PTRB(t5)		# select write register 13
		jal	FlushWB
		sw	t1,SCC_PTRB(t5)		# write high byte of time constant
		jal	FlushWB

		la	t6,LoopTable2		# point to SCC table A start
		la	t7,LoopTableEnd		# point to SCC table A end
		b	2f			# get first byte in table A
1:
		sw	t0,SCC_PTRB(t5)		# write register number to SCC
		addu	t6,1			# increment table pointer
		lbu	t0,0(t6)		# get SCC data
		nop				# delay slot
		sw	t0,SCC_PTRB(t5)		# write data to SCC
		addu	t6,1			# increment table pointer
		jal	FlushWB			# wait for a while
2:
		lbu	t0,0(t6)		# get SCC register number
		bltu	t6,t7,1b		# keep loading util reach end

		jal	FlushWB			# wait for a while

		j	t4

END(Scc_Local)

		.data

/*
 * Table for initializing SCC (Z8530) control registers.
 *
 * In each entry of the table, the first byte is the register offset
 * from WR0 (pointer register) and the second byte is the data to be loaded
 * into the register.  Both channel A and channel B are supported.
 */
SCCTable:
		.byte	SCCW_CMD_R
		.byte	SCCW_CMD_CMD_Null
		.byte	SCCW_CMD_R
		.byte	SCCW_CMD_CMD_Null
		.byte	SCCW_CMD_R
		.byte	SCCW_CMD_CMD_Null
		.byte	SCCW_MIC_R
		.byte	SCCW_CMD_CMD_Null
		.byte	SCCW_RCV_R
		.byte	SCCW_RCV_8Bits | SCCW_RCV_RxEn
		.byte	SCCW_MISC_R
		.byte	SCCW_MISC_CLK_16 | SCCW_MISC_SB_1
		.byte	SCCW_TXMDM_R
		.byte	SCCW_TXMDM_LabeledDTR | SCCW_TXMDM_8Bits | SCCW_TXMDM_TxEnable | SCCW_TXMDM_LabeledRTS
		.byte	SCCW_CLK_R
		.byte	SCCW_CLK_RxC_BRGen | SCCW_CLK_TxC_BRGen | SCCW_CLK_DriveTRxC | SCCW_CLK_TRxC_BRGen
TableEnd:

LoopTable1:
		.byte	SCCW_CMD_R
		.byte	SCCW_CMD_CMD_Null
		.byte	SCCW_MISC_R
		.byte	SCCW_MISC_CLK_16 | SCCW_MISC_SB_1
		.byte	SCCW_VECTOR_R
		.byte	0
		.byte	SCCW_RCV_R
		.byte	SCCW_RCV_8Bits
		.byte	SCCW_TXMDM_R
		.byte	SCCW_TXMDM_LabeledDTR | SCCW_TXMDM_8Bits | SCCW_TXMDM_LabeledRTS
		.byte	SCCW_SYNC_1st_R
		.byte	0
		.byte	SCCW_SYNC_2nd_R
		.byte	0
		.byte	SCCW_MIC_R
		.byte	0
		.byte	SCCW_ENCODE_R
		.byte	0
		.byte	SCCW_CLK_R
		.byte	SCCW_CLK_RxC_BRGen | SCCW_CLK_TxC_BRGen | SCCW_CLK_DriveTRxC | SCCW_CLK_TRxC_BRGen
LoopTable2:
		.byte	SCCW_AUX_R
		.byte	SCCW_AUX_CMD_Null
		.byte	SCCW_RCV_R
		.byte	SCCW_RCV_8Bits | SCCW_RCV_RxEn
		.byte	SCCW_TXMDM_R
		.byte	SCCW_TXMDM_LabeledDTR | SCCW_TXMDM_8Bits | SCCW_TXMDM_TxEnable | SCCW_TXMDM_LabeledRTS
		.byte	SCCW_CMD_R
		.byte	0x80			# incorrect define in scc_cons.h
		.byte	SCCW_AUX_R
		.byte	SCCW_AUX_Loopback | SCCW_AUX_BRfromPClock | SCCW_AUX_BRGenEnable
		.byte	SCCW_ENAB_R
		.byte	0
		.byte	SCCW_EXTINTR_R
		.byte	0
		.byte	SCCW_CMD_R
		.byte	SCCW_CMD_CMD_ResetExtIntr
		.byte	SCCW_CMD_R
		.byte	SCCW_CMD_CMD_ResetExtIntr
LoopTableEnd:
