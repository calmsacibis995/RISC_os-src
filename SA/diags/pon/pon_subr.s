#ident "$Header: pon_subr.s,v 1.18.1.1 90/07/18 14:33:11 huang Exp $"
/* $Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
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
#include "machine/bc.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "machine/duart.h"
#include "machine/scc_cons.h"
#include "machine/cp0.h"
#include "machine/cpu_board.h"
#include "machine/mem_board.h"
#include "machine/vmereg.h"
#include "machine/mk48t02.h"
#include "machine/hd146818.h"
#include "pon.h"
#include "prom.h"
#include "machine/i8042.h"

#ifdef	SABLE
#define SHORTWAIT	1
#else
#define SHORTWAIT	100000
#endif	SABLE
#define ALLLEDSLIT	0x3f
#define ALLLEDSOUT	0x00

#define	Tmp0		t5	/* short term temp reg */
#define	Tmp1		t6	/* short term temp reg */
#define	Ioadr		t0	/* current io address */
#define	Membd		t2	/* current board index */
#define	Retval		v0	/* proc call return value */
#define	DLED		(LED_REG_R3200 | 0xa0000000)
#define	SA16(x)		+((x)+VME_A16SBASE+K1BASE)
#define VMEBASE		VME_A16SBASE + K1BASE
#define	REFIL_MSK	0xfffffff0
#define JUMP_A2		0x00c00008
#define	OP_MSK		0xfc000000
#define JAL		0x0c000000
#define RETURN_INST	0x3e00008
#define PROM_SPACE	0xb0000000
#define	TARGET_MSK	0x03ffffff
#define CHA_RB3125_PTR	(SCC_RB3125_CHA_PTR | 0xa0000000)
#define CHA_RB3125_DATA	(SCC_RB3125_CHA_DATA | 0xa0000000)

#ifdef	SABLE
#define DELAY(x)
#else	SABLE
#define	DELAY(x)				\
	li	Tmp0,+(x);			\
9:	subu	Tmp0,1;				\
	bne	zero,Tmp0,9b
#endif	SABLE

#define INTDIS	0xfffffffe
#define INTEN	0x1
#define KBD_DELAY               20000

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

#ifndef	R3030
	.globl f_slot[8]
	.globl table

LEAF (dmemconfig)

	.set noreorder
	mfc0	Retval,C0_SR		#read the status register
	nop
	.set reorder
	move	t4,Retval
	and	Retval,INTDIS		#disable interrupts
	.set noreorder
	mtc0	Retval,C0_SR		#write value into status register
	mtc0	zero,C0_CAUSE		#clear any software interrupt
	.set reorder
	li	t9,8			#mem boards could be in slot0-7
	li	Ioadr,0			#start from slot 0
	li	t7,8			#bits to shift for mem address
	li	Retval,0		#mem boards
	li	a0,2
probe_loop1:
	beq	Ioadr,t9,finish
	sll	t5,Ioadr,8
	sh	Ioadr,SA16(MEM_IV)(t5)	# write to generate possible bus error
					# if board isn't there

	DELAY(800000)
	.set noreorder
	mfc0	Tmp0,C0_CAUSE
	nop
	.set reorder
	and	Tmp0,CAUSE_IP8		#look for bus error
	bne	Tmp0,zero,probe_again	#try next slot
config1:
	sll	Tmp1,Retval,t7		#mem address
	addu	Tmp1,Retval
	sll	Tmp1,1
	la	a1,f_slot
	addu	a1,Retval
	sb	Ioadr,(a1)	#slot no of existed mem board

	addiu	Ioadr,1
	addiu	Retval,1
	j	probe_loop1
finish:
	j	ra
forever:
	li	a0,DLED
	li	v1,0xff
	sb	v1,(a0)
	b	forever

probe_again:
	lw	zero,SBE_ADDR+K1BASE	#clear pending write bus error
	sw	zero,SBE_ADDR+K1BASE
	DELAY(10)
	.set noreorder
	mfc0	Tmp0,C0_CAUSE
	nop
	.set reorder
	and	Tmp0,CAUSE_IP8
	bne	Tmp0,zero,probe_again
	addiu	Ioadr,1
	j	probe_loop1

END(dmemconfig)
#endif	!R3030


LEAF(pon_puthex)

		move	t2,ra
		move	t3,a0
		li	t8,8			# Number of digits to display
2:
		srl	a0,t3,28		# Isolate digit
		lb	a0,hexdigit(a0)		# Convert it to hexidecimal
		jal	pon_putc		# Print it

		sll	t3,4			# Set up next nibble
		sub	t8,1			# Entire number printed?
		bne	t8,zero,2b
		j	t2			# Yes - done

END(pon_puthex)


LEAF(pon_puts)

		move	t8,ra
		move	t2,a0
		beq	zero,zero,3f		# jump forward
1:
		addu	t2,1			# Bump to next character
		jal	pon_putc		# No - Display character
3:
		lb	a0,0(t2)		# Get character to display
		bne	a0,zero,1b		# End of string?
		j	t8

END(pon_puts)


/*
 * putc: v1 contains byte to be printed.
 */
#ifdef	SABLE
#define	DELAYCNT	1
#else	SABLE
#define	DELAYCNT	0x8000
#endif	SABLE

 /* This wrapper determines which machine the code is being executed on. */

LEAF(pon_putc)
		move	t9,ra
		move	t0,a0	/* save the character to print */
		jal	get_machine_type /* find out which machine */
		move	a0,t0	/* restore character to print */
		beq	v0,BRDTYPE_RB3125,1f /* if Genesis use scc putc */
		beq	v0,BRDTYPE_R3030,1f /* if Pizazz use scc putc */
		jal	pon_2681_putc /* if not Genesis/Pizazz use 2681 putc */
		b	2f
1:		
		jal	pon_scc_putc
2:
		move	ra,t9
		j	ra
	
END(pon_putc)

LEAF(pon_scc_putc)
/*
 * SCC channel A low level put char.  Poll Tx Buffer Empty until it's set,
 * then put char in a0 to SCC.
 */

		move	t1,ra
		move	t4,a0			# save character

		SCC0_BASE			# get SCC base
		move	t6,v0

#ifdef	R3030
		jal	GetPonEnviron
		and	v1,v0,PON_TTY0		# check if tty0 should be used
		beq	v1,zero,2f
#endif	R3030
		li	t5,DELAYCNT
1:
		lw	t7,SCC_PTRA(t6)		# get channel A status
		and	t7,SCCR_STATUS_TxReady	# check if TBE is set
		bne	t7,zero,4f		# transmitter ready

		sub	t5,1			# decrement count
		beq	t5,zero,4f		# spin until 0,
						#   then just write the character
		b	1b
4:
		sw	t4,SCC_DATAA(t6)	# put char to SCC
#ifndef	R3030
		jal	FlushWB
		NOP_DELAY
#else
		NOP_DELAY

2:
		and	v1,v0,PON_TTY1		# check if tty1 should be used
		beq	v1,zero,3f

		li	t5,DELAYCNT
5:
		lw	t7,SCC_PTRB(t6)		# get channel B status
		and	t7,SCCR_STATUS_TxReady	# check if TBE is set
		bne	t7,zero,6f		# transmitter ready

		sub	t5,1			# decrement count
		beq	t5,zero,6f		# spin until 0,
						#   then just write the character

		b	5b
6:
		sw	t4,SCC_DATAB(t6)	# put char to SCC
		NOP_DELAY
#endif	R3030
3:
		j	t1			# return to the calling addr

END(pon_scc_putc)

LEAF(pon_2681_putc)

		move	t1,ra
		move	t4,a0
		NVRAM(NVADDR_CONSOLE)
		move	t5,v0			# move the the console byte into t5
		li	t5,0x72			# the letter 'r'
		beq	t5,t7,3f		# read an 'r'? remote?

		DUART0_BASE
		move	t5,v0			# move the the DUART base into t5
		li	t7,DELAYCNT
1:
		lbu	t6,DOFF_SRA(t5)		# the transmitter empty?
		and	t6,SREMT
		bne	t6,zero,7f

		addiu	t7,t7,-1		# decrement the count
		beq	t7,zero,2f		# spin till it be 0
		b	1b			# just spin till ready
2:
		j	t1			# and just return
7:
		li	t7,DELAYCNT
1:
		lbu	t6,DOFF_SRA(t5)		# the transmitter ready?
		and	t6,SRTXRDY
		bne	t6,zero,9f

		addiu	t7,t7,-1		# decrement the count
		beq	t7,zero,2f		# spin till it be 0

		b	1b			# just spin till ready
2:
		j	t1			# and just return
9:
		sb	t4,DOFF_THRA(t5)	 # Yes - send the character
		jal	FlushWB

		j	t1			# and return

        # 'r' or remote console case; write char to both ports
3:
		DUART0_BASE
		move	t5,v0			# move the the DUART base into t5
		li	t7,DELAYCNT
1:
		lbu	t6,DOFF_SRA(t5)		# the transmitter empty?
		and	t6,SREMT
		bne	t6,zero,7f

		addiu	t7,t7,-1		# decrement the count
		beq	t7,zero,2f		# spin till it be 0

		b	1b			# just spin till ready
2:
		j	t1			# and just return
7:
		li	t7,DELAYCNT
1:
		lbu	t6,DOFF_SRA(t5)		# the transmitter ready?
		and	t6,SRTXRDY
		bne	t6,zero,9f

		addiu	t7,t7,-1		# decrement the count
		beq	t7,zero,2f		# spin till it be 0

		b	1b			# just spin till ready
2:
		j	t1			# and just return
9:
		sb	t4,DOFF_THRA(t5)	# Yes - send the character
		jal	FlushWB

		li	t7,0x1000
1:
		addiu	t7,t7,-1		# decrement the count
		beq	t7,zero,2f		# spin till it be 0

		b	1b			# just spin till ready
2:

		li	t7,DELAYCNT
1:
		lbu	t6,DOFF_SRB(t5)		# the transmitter empty?
		and	t6,SREMT
		bne	t6,zero,7f

		addiu	t7,t7,-1		# decrement the count
		beq	t7,zero,2f		# spin till it be 0

		b	1b			# just spin till ready
2:
		j	t1			# and just return
7:
		li	t7,DELAYCNT
1:
		lbu	t6,DOFF_SRB(t5)		# the transmitter ready?
		and	t6,SRTXRDY
		bne	t6,zero,9f

		addiu	t7,t7,-1		# decrement the count
		beq	t7,zero,2f		# spin till it be 0

		b	1b			# just spin till ready
2:
		j	t1			# and just return
9:
		sb	t4,DOFF_THRB(t5)	 # Yes - send the character
		jal	FlushWB

		j	t1			# and return

END(pon_2681_putc)


 #  FastFlash blinks all the LEDs on and off rapid rate (about 1/4 second);
 #  ShortWait provides a delay-loop of approximately 1/4 second when running
 #  from prom.

LEAF(FastFlash)

		move	t0,ra			# save our return address
#ifndef	R3030
		li	t9,15			# flash 'em for about 5 secs
		move	t8,a0			# save LED error value to write
2:
		li	a0,ALLLEDSOUT		# so turn the LEDS off
		jal	pon_set_leds		# write to the CPU LEDS

		li	v0,SHORTWAIT		# get the delay count
1:
		addiu	v0,v0,-1		# decrement the count
		bgtz	v0,1b			# spin till it be 0

		move	a0,t8			# now show LED error value
		jal	pon_set_leds		# write to the CPU LEDS

		li	v0,SHORTWAIT		# get the delay count
1:
		addiu	v0,v0,-1		# decrement the count
		bgtz	v0,1b			# spin till it be 0

		addiu	t9,t9,-1		# decrement the loop-counter
		bgtz	t9,2b			# keep on flashin'
#else	!R3030
		li	t9,15			# loop count
		li	t8,K1BASE|SYS_CREG
		li	v1,CR_BUZZ_L
3:
		sw	v1,(t8)			# turn on buzzer

		li	v0,20000
2:
		subu	v0,1			# wait a while
		bne	v0,zero,2b

		li	v0,CR_ENA_BUZZER_B	# turn off buzzer
		sw	v0,(t8)

		li	v0,10000
4:
		subu	v0,1			# wait a while
		bne	v0,zero,4b

		subu	t9,1			# next frequency
		bne	t9,zero,3b

		li	v0,CR_ENA_BUZZER_B	# turn off buzzer
		sw	v0,(t8)
#endif	!R3030
		j	t0			# and back to the caller

END(FastFlash)


LEAF(GetCause)

		.set noreorder
		mfc0	v0,C0_CAUSE
		.set reorder
		j	ra

END(GetCause)


LEAF(PonHandler)

		.set	noreorder
		mtc0	zero,C0_PID		# XXX
		.set	reorder

		la	a0,exception_msg	# Indicate vector entered on
		jal	pon_puts		# Display vector designator

		.set noreorder
		mfc0	a0,C0_EPC		# Display the exception PC
		.set reorder
#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex

		la	a0,cause_msg		# Display cause register heading
		jal	pon_puts

		.set noreorder
		mfc0	a0,C0_CAUSE		# Get cause register
		.set reorder

#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE+4
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex		# Display cause register

		la	a0,badvaddr_msg		# Display BadVAddr heading
		jal	pon_puts

		.set noreorder
		mfc0	a0,C0_BADVADDR		# Get the BadVAddr register
		.set reorder
#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE+8
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex		# Display it

		la	a0,status_msg		# Display status heading
		jal	pon_puts

		.set noreorder
		mfc0	a0,C0_SR		# Get the status register
		.set reorder
#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE+12
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex		# Display it

#ifndef	R3030
		jal	get_machine_type

		bne	v0,BRDTYPE_R2400,1f	# if not Intrepid

		la	a0,far_msg		# Display fault address heading
		jal	pon_puts

		lw	a0,FAR|K1BASE		# Get the fault address register
#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE+16
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex		# Display it

		la	a0,fid_msg		# Display fault ID heading
		jal	pon_puts

		lhu	a0,FID|K1BASE		# Get the fault ID register
#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE+20
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex		# Display it
1:
		bne	v0,BRDTYPE_R6300,1f	# if not 6000

		la	a0,error_reg_msg
		jal	pon_puts

		.set	noreorder
		mfc0	a0,C0_ERROR		# display the Error Register
		li	a1,C0_ERROR_IMASK	# and ignore further errs
		mtc0	a1,C0_ERROR
		.set	reorder
		jal	pon_puthex
1:
		la	a0,crlf
		jal	pon_puts
#else	!R3030
		la	a0,scntrl_msg		# Display system control register heading
		jal	pon_puts

		lw	a0,SYS_CREG|K1BASE	# Get the system control register
#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE+16
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex		# Display it

		la	a0,serror_msg		# Display system error register heading
		jal	pon_puts

		lhu	a0,SYS_EREG|K1BASE	# Get the system error register
#ifdef	DEBUG
		li	a1,PON_SCRATCHMEM|K1BASE+20
		sw	a0,0(a1)
#endif	DEBUG
		jal	pon_puthex		# Display it

		la	a0,crlf
		jal	pon_puts
#endif	!R3030

 # Don't clobber the current LEDs code!

#ifdef	USED
		li	a0,PON_FAULT_PATTERN
		jal	pon_set_leds		# write pattern to LEDs
1:
		b	1b			# loop forever
#endif	USED
		SETNVRAM(CHAR_e, NVADDR_BOOTMODE)
		j	PonError

END(PonHandler)


#ifndef	R3030
LEAF(get_nvram_baud)

		move	t0,ra			# save return address
		beq	a0,zero,1f		# if a0 is positive, then looking for lbaud

		NVRAMADDR(NVADDR_LBAUD)		# address of first lbaud ascii char
		b	2f
1:						# else looking for rbaud
		NVRAMADDR(NVADDR_RBAUD)		# address of first rbaud ascii char
2:
		lbu	a1,0(v0)		# read in the first ascii character
		slti	$at,a1,0xb		# if > 0xb
		bne	zero,$at,3f
		li	a1,0xa			# you get 9600
3:
		jal	get_machine_type
		beq	v0,BRDTYPE_RB3125,4f	# for Genesis
		la	a2,baud_table		# Point to baud table
		b	5f
4:
		la	a2,scc_baud_table		# Point to baud table
5:
		sll	a1,1
		add	a2,a2,a1
		lhu	v0,0(a2)
		j	t0			# return

END(get_nvram_baud)

LEAF(InitDuart1B)

		move	t9,ra			# save our return address

 # initialize the DUART channel to allow debug prints

		DUART0_BASE
		move	v1,v0
		la	a0,table		# Point to DUART inialization table
		beq	zero,zero,2f		# go forward and pickup 1st offset byte
1:
		or	a2,v1,v0		# Create address of DUART port to initialize
		addu	a0,1			# bump to initial value of current register
		lbu	a1,0(a0)		# Pickup initialization dat
		addu	a0,1
		sb	a1,0(a2)		# store the data
		jal	FlushWB
2:
		lbu	v0,0(a0)		# Pick up register
		bne	v0,zero,1b		# Done?
 # need to reset the baud rate based on the nvram 'rbaud' field

		move	a0,zero			# argument to indicate rbaud rate
		jal	get_nvram_baud

		DUART0_BASE
		move	a0,v0
		ori	a0,a0,0x20
		sb	v0,DOFF_CSR(a0)		# (channel A St/Clk Sel Reg)
		jal	FlushWB

		li	t6,SEL_MR1		# point to mr1
		sb	t6,DOFF_CR(a0)		# (channel A CMD register)
		jal	FlushWB

		li	t7,0x13			# (no parity, 8 bits/char)
		sb	t7,DOFF_MR(a0)		# (MR1A)
		jal	FlushWB

		li	t8,b110
		beq	v0,t8,1f		# see if nvram was set to 110 baud

		li	t8,stop1		# (1 stop bit)
		b	2f
1:
		li	t8,stop2		# (2 stop bits if 110 baud)
2:
		sb	t8,DOFF_MR(a0)		# (MR2A)
		jal	FlushWB
		j	t9

END(InitDuart1B)
#endif	!R3030


LEAF(GetSR)

		.set noreorder
		mfc0	v0,C0_SR
		nop
		j	ra
		nop
		.set reorder

END(GetSR)


LEAF(SetSR)

		.set noreorder
		mtc0	a0,C0_SR
		nop
		j	ra
		nop
		.set reorder

END(SetSR)


LEAF(GetISR)

                lw      v0,machine_type
                beq     v0,BRDTYPE_R2400,1f     # if M120
                beq     v0,BRDTYPE_M180,1f      # if M180
                beq     v0,BRDTYPE_RB3125,3f

                lbu     v0,(K1BASE | VME_ISR)
                b       2f
1:
                lhu     v0,(K1BASE | ISR)
2:
                j       ra
3:
                lhu     v0,(K1BASE | VME_ISR_RB3125)
                and     v0,VME_ISR_MSK_RB3125
                b       2b
END(GetISR)


LEAF(SetISR)
#ifndef	R3030
		sh	a0,ISR|K1BASE
#endif
		j	ra
END(SetISR)


LEAF(GetIMR)

            lw      v0,machine_type
            beq     v0,BRDTYPE_R2400,1f     # if M120
            beq     v0,BRDTYPE_M180,1f      # if M180
            beq     v0,BRDTYPE_RB3125,3f

            lbu     v0,(K1BASE | VME_IMR)
            b       2f
1:
            lhu     v0,(K1BASE | IMR)
2:
            j       ra
3:
            lhu     v0,(K1BASE | VME_IMR_RB3125)
            and     v0,VME_ISR_MSK_RB3125
            b       2b

END(GetIMR)


LEAF(SetIMR)
#ifndef	R3030
           	lw      v0,machine_type
                beq     v0,BRDTYPE_R2400,1f     # if M120
                beq     v0,BRDTYPE_M180,1f      # if M180
                beq     v0,BRDTYPE_RB3125,3f

                sb      a0,(K1BASE | VME_IMR)
                b       2f
1:
                sh      a0,(K1BASE | IMR)
3:
                sh      a0,(K1BASE | VME_IMR_RB3125)
                b       2f
#endif
2:
                j       ra

END(SetIMR)


LEAF(GetSCR)

		lhu	v0,SCR|K1BASE
		j	ra

END(GetSCR)


LEAF(SetSCR)
#ifndef	R3030
		sh	a0,SCR|K1BASE
#endif
		j	ra

END(SetSCR)


LEAF(nvram_addr)

		sll	a0,a0,2
		sub	a1,a1,1
		beq	a1,BRDTYPE_R2400,1f

		sll	a1,a1,2
		lw	v1,RT_CLOCK_ADDR(a1)
		nop
		addi	v1,a0,RT_MEM_OFFSET
		j	2f
1:
		sll	a1,a1,2
		lw	v1,TODC_CLOCK_ADDR(a1)
		nop
		addi	v1,a0,TODC_MEM_OFFSET
2:
		move	v0,v1
		j	ra

END(nvram_addr)


 # Write pattern to cpu board leds

LEAF(pon_set_leds)

#ifndef	R3030
		move	t1,ra
		move	a1,a0			# machine_type uses a0
		jal	get_machine_type

		beq	v0,BRDTYPE_R2400,1f
		beq	v0,BRDTYPE_R6300,2f	# 1=on 0=off */

		/* not R2400 and not R6300 */
		or	a1,LED_LMEM_RUN|LED_FPBD_RUN

1:		not	a1			# for 0=on 1=off */
2:		sub	v0,1
		sll	v1,v0,2			# machine_type index
		lw	v1,LED_REG(v1)		# get LED_REG addr
		sb	a1,(v1)
		j	t1
#else	!R3030
		j	ra
#endif	!R3030

END(pon_set_leds)


LEAF(GetDepend)

		move	a1,ra
		NVRAMADDR(NVADDR_PONMASK)
		move	v1,v0
		lbu	v0,0(v1)
		lbu	a0,4(v1)
		sll	a0,8
		or	v0,a0
		lbu	a0,8(v1)
		sll	a0,16
		or	v0,a0
		lbu	a0,12(v1)
		sll	a0,24
		or	v0,a0
		j	a1

END(GetDepend)


/*
 * Sets dependency bits in the NVRAM so that tests which depend on some other
 * previous tests can refer to this bits (4 bytes) and need or need not run
 * depending on test's result in the PON environment.
 */
LEAF(SetDepend)

		move	a1,ra
		move	a2,a0				# save error code
		NVRAMADDR(NVADDR_PONMASK)
		move	v1,zero
		lbu	v1,0(v0)			# read the previous contents at this address
		or	v1,a2				# or it with error code supplied
		and	v1,0xff
		sb	v1,0(v0)			# store the result back

		srl	a2,8				# shift error code to "or" with next byte
		lbu	v1,4(v0)
		or	v1,a2
		and	v1,0xff
		sb	v1,4(v0)

		srl	a2,8
		lbu	v1,8(v0)
		or	v1,a2
		and	v1,0xff
		sb	v1,8(v0)

		srl	a2,8
		lbu	v1,12(v0)
		or	v1,a2
		and	v1,0xff
		sb	v1,12(v0)
#ifndef	R3030
		jal	FlushWB
#endif	!R3030
		j	a1

END(SetDepend)


LEAF(ResetDepend)

		move	a1,ra
		NVRAMADDR(NVADDR_PONMASK)
		sb	zero,0(v0)			# store the result back
		sb	zero,4(v0)
		sb	zero,8(v0)
		sb	zero,12(v0)
#ifndef	R3030
		jal	FlushWB
#endif	!R3030
		j	a1

END(ResetDepend)


LEAF(get_nvramaddr)

	move	a1,ra
	move	a2,a0				# save offset
	NVRAMADDR(0)				# get base address
	sll	a2,2
	addu	v0,a2
	j	a1

END(get_nvramaddr)


#ifndef	R3030
LEAF(GetMemIoReg)

	li	a3,VMEBASE
	sll	a1,8
	add	v0,a1,a0			# save offset
	add	v1,v0,a3
	lh	v0,(v1)
	j	ra

END(GetMemIoReg)


LEAF(PutMemIoReg)

	li	a3,VMEBASE
	sll	a1,8
	add	v0,a1,a0			# save offset
	add	v1,v0,a3
	sh	a2,(v1)
	j	ra

END(PutMemIoReg)
#endif	!R3030


/*
 * This routine is used to move code from PROM space into memory so that the moved
 * code can be executed through cache space. The routine will block align the
 * destination address and source ending address.
 * a0 destination pointer (cached)
 * a1 source start address (uncached)
 * a2 source end address (uncached)
 */
LEAF(move_code)

	move	t5,zero		# initialize subroutine counter
	move	v0,a0
	and	a0,a0,REFIL_MSK # If destination address is not blk aligned. align it
	move	v1,a0
	move	t1,a0
	or	a1,0xa0000000	# make sure the addresses are uncached & unmapped
	or	a2,0xa0000000	# make sure the addresses are uncached & unmapped
2:
	lw	v0,0(a1)	# read source data
	and	t2,v0,OP_MSK
	beq	t2,JAL,mod_jal
	sw	v0,0(a0)	# write source data into destination
1:
	add	a1,4		# increment source pointer
	add	a0,4		# increment desatination pointer
	beq	a1,a2,done	# check to see if done
	b	2b		# not done so do it again
done:
	li	v0,JUMP_A2	# Add a j a2 instruction to the memory resident code
	sw	v0,0(a0)	# write out the j a2 instruction to return to the PROM
	sw	zero,4(a0)	# write out nop in branch delay slot
	add	a0,8
	move	v1,a0
5:
	and	v0,v1,~REFIL_MSK # determine if copy did not end on a block boundary
	beq	v0,zero,3f	# go to fill in block
	add	v1,4
	b	5b
3:
	beq	v1,a0,4f
	sw	zero,0(a0)	# write good parity into the aligned block
	add	a0,4
	b	3b
mod_jal:
	and	t6,a0,REFIL_MSK # If destination address is not blk aligned. align it
	add	t5,1		# subroutine counter
	li	t4,0x5000	# 5000 hex lines of code allowed per subroutine
	mult	t4,t5		# create a new target address for each subroutine
	mflo	t4		# fetch only the low 32 bits (should be enough)
	and	t6,0x1fffffff	# mask off address space bits
	add	t4,t6		# Create new target address
	srl	t4,2		# shift target address right
	and	t4,t4,TARGET_MSK
	and	t3,v0,OP_MSK	# Mask off jal inst. target address
	or	t3,t4		# or in jal opcode
	sw	t3,0(a0)	# store modified jal instruction
	sll	t4,2		# shift target address left
	or	t4,0xa0000000	# point destination address to unmapped, uncached space
move_sub:			# move subroutine
	and	t2,v0,~OP_MSK	# get target address
	sll	t2,2		# shift left 2
	or	t2,PROM_SPACE	# create prom address
5:
	lw	v0,0(t2)	# read instructions
	sw	v0,0(t4)	# store instruction
	add	t2,4		# increment instruction read address
	add	t4,4		# increment destination address
	beq	v0,RETURN_INST,6f # if return inst. prepare to return to main code
	b	5b
6:
	lw	v0,0(t2)	# read branch delay slot
	sw	v0,0(t4)	# store branch delay
	b	1b
4:
	move	a0,t1
	move	v0,t1
	j	ra

END(move_code)

LEAF(clr_lance_buffer)
	li	a0,0xbf000000
	li	a1,0xbf100000
1:
	sw	zero,0(a0)
	add	a0,4
	bne	a0,a1,1b
	j	ra
END(clr_lance_buffer)


/*
 * Table for loading DUART control registers.
 *
 * In each entry of the table, the first byte is the offset of the register
 * from DUART_BASE and the second byte (low order byte) is the data to be
 * loaded into the register.
 */

		.data
		.align 1
table:
		#
		#  Channel B Setup
		#
		.byte	DOFF_CRB, RES_RX	# crb - reset receiver
		.byte	DOFF_CRB, RES_TX	# crb - reset transmitter
		.byte	DOFF_CRB, RES_ERR	# crb - reset error status
		.byte	DOFF_CRB, RES_BRK	# crb - reset break interrupt
		.byte	DOFF_CRB, SEL_MR1	# crb - point to mr1
		.byte	DOFF_MRB, 0x13		# mr1b - no parity, 8 bits per char
		.byte	DOFF_MRB, 0x07		# mr2b - 1 stop bit
		.byte	DOFF_CRB, SEL_MR1	# crb - point to mode register 1
		.byte	DOFF_CSRB, b9600	# csrb - RxC = 9600, TxC = 9600
		#
		#  Output Port - Turn all bits on
		#
		.byte	DOFF_SOPBC, 0x03	# sopbc - Set output port bits command
						# set DTR and RTS
		#
		# Turn on the channel
		#
		.byte	DOFF_OPCR, 0x00		# opcr - Complement of opr register
		.byte	DOFF_CRB, EN_TX | EN_RX	# crb - Enable transmitter and receiver
		.half	0

EXPORT(scc_table)
		.byte	SCCW_MIC_R, 0
		.byte	SCCW_AUX_R, SCCW_AUX_BRfromPClock
		.byte	SCCW_MISC_R, (SCCW_MISC_PAR_ParityOff|SCCW_MISC_CLK_16|SCCW_MISC_SB_1)
		.byte	SCCW_BRLO_R, (B_CONST(9600) & 0xff)
		.byte	SCCW_BRHI_R, ((B_CONST(9600)>>8) & 0xff)
		.byte	SCCW_CLK_R, (SCCW_CLK_TRxC_BRGen |SCCW_CLK_TxC_BRGen | SCCW_CLK_RxC_BRGen | SCCW_CLK_DriveTRxC)
		.byte	SCCW_AUX_R, (SCCW_AUX_BRfromPClock | SCCW_AUX_BRGenEnable)
		.byte	SCCW_RCV_R, (SCCW_RCV_8Bits | SCCW_RCV_RxEn)
		.byte	SCCW_TXMDM_R, (SCCW_TXMDM_8Bits | SCCW_TXMDM_TxEnable | SCCW_TXMDM_LabeledDTR | SCCW_TXMDM_LabeledRTS)
		.byte	SCCW_TXMDM_R, (SCCW_TXMDM_8Bits|SCCW_TXMDM_TxEnable | SCCW_CLK_DriveTRxC)
		.half	0xcdcd			/* just in case */

		.align 2
baud_table:
		.half	b75
		.half	b110
		.half	b134
		.half	b150
		.half	b300
		.half	b600
		.half	b1200
		.half	b1800
		.half	b2400
		.half	b4800
		.half	b9600
		.half	b19200

EXPORT(scc_baud_table)
		.half	B_CONST(75)
		.half	B_CONST(110)
		.half	B_CONST(134)
		.half	B_CONST(150)
		.half	B_CONST(300)
		.half	B_CONST(600)
		.half	B_CONST(1200)
		.half	B_CONST(1800)
		.half	B_CONST(2400)
		.half	B_CONST(4800)
		.half	B_CONST(9600)
		.half	B_CONST(19200)

hexdigit:
		.ascii	"0123456789abcdef"
EXPORT(crlf)
		.asciiz	"\r\n"
EXPORT(success)
		.asciiz	" PASSED\r\n"
EXPORT(failure)
		.asciiz	" *** FAILED ***\r\n"
EXPORT(skipped)
		.asciiz	" *** SKIPPED ***\r\n"
exception_msg:
		.asciiz "\r\nException at: "
cause_msg:
		.asciiz ", Cause:     "
badvaddr_msg:
		.asciiz "\r\nBad VAddress: "
status_msg:
		.asciiz ", Status:    "
#ifndef	R3030
error_reg_msg:
		.asciiz	", Error: "
far_msg:
		.asciiz "\r\nFault Addr:   "
fid_msg:
		.asciiz	", Fault ID:  "
#else	!R3030
scntrl_msg:
		.asciiz "\r\nControl Reg:  "
serror_msg:
		.asciiz ", Error Reg: "
#endif	!R3030

		BSS(pon_tmp, 4)
