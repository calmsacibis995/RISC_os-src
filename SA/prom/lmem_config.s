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
#ident "$Header: lmem_config.s,v 1.33 90/10/26 15:37:30 hawkes Exp $"

/*
 * lmem_config.s -- local memory configuration
 *
 * TODO: check about local memory reset requirements
 */

#include "prom/prom.h"
#include "machine/asm.h"
#include "machine/bc.h"
#include "machine/cpu.h"
#include "machine/ioa.h"
#include "machine/regdef.h"
#include "machine/cpu_board.h"
#include "machine/mem_board.h"
#include "mipsvme/vmereg.h"
#include "machine/i8254clock.h"
	.extern	mbyte_size_mask
	.extern	mbyte_size_mask_end

#define INTERLEAVE
/*
 * Current definition of local memory control register bits
 * (Do NOT put error enables in CONTROL_BITS!)
 */
#define	CONTROL_BITS	+(PRIV_ENAB|SYS_ENAB|INT_LEVELA|INT_LEVELB)
#define	ERRENAB_BITS	+(EN_SYNERR|EN_PRIVERR)
#define	BASEADDR	0x0
#define	MAX_A24_ADDR	0xC00000	/* must be < 16MB for A24 enable */
#define DCOUNT		1000000

/*
 * char constants
 */
#define	CHAR_M		0x4d
#define	CHAR_m		0x6d

/*
 * LED pattern interpretation
 *
 * BIT 7 indicates that board is part of interleave pair
 * BIT 6 indicates that board is odd board of interleave pair
 * BITS 5-0 indicate board base address in megabytes
 */
#define	LED_INTERLEAVE	0x80
#define	LED_INTLVODD	0x40
#define	LED_BASEMASK	0x3f
#define MEM_LED		0xbd000012
#define	MEM_ID_B1	0xbd000082
#define	CPU_ID_B1	0xbff00007    

/*
 * Configuration/Powerup Diagnostic Register Layout
 * NOTE:  All register pseudonyms start with an upper case letter
 *
 * Register allocation scheme:
 *	Any routine starting with "__" will only use and reference
 *		Global registers
 *	Any cpp macro will only use and reference
 *		Global registers
 *	Any other routine may destroy Local register and must be
 *		called with caution
 *
 * Global register allocation
 */
#define	Ra_save0	s0	/* level 0 ra save registers */
#define	Ra_save1	s1	/* level 1 ra save registers */
#define	Tmp0		s2	/* short term temp reg */
#define	Tmp1		s3	/* short term temp reg */
#define	Mask4meg	s4	/* position mask for 4 MB mem cards */
#define	Mask8meg	s5	/* position mask for 8 MB mem cards */
#define	Mask16meg	s6	/* position mask for 16 MB mem cards */
#define	Mask32meg	s7	/* position mask for 32 MB mem cards */
#define	Arg0		a0	/* proc call argument */
#define	Arg1		a1	/* proc call argument */
#define	Arg2		a2	/* proc call argument */
#define	Ioadr		a3	/* current io address */
#define	Retval		v0	/* proc call return value */
#define	Bevhndlr	fp	/* address of fault handler */

/*
 * lmem_config local variables
 */
#define	Vec		t0	/* current interrupt vector */
#define	Membd		t2	/* current board index */
#define	Mask		t3	/* current position mask */
#define Cpuid		t4	/* cpu id number */

/*
 * Handy macros
 */
#define	SA16(x)		+((x)+VME_A16SBASE+K1BASE)

#ifdef SABLE
#define DELAY(x)
#else
#define	DELAY(x)				\
	li	Tmp0,+(x);			\
9:	subu	Tmp0,1;				\
	bne	zero,Tmp0,9b
#endif !SABLE

	SHARED(memsize)

/*
 * lmem_config  -- level 0 routine to configure local memory
 */
LEAF(lmem_config)
	.set	noreorder
	li	Tmp0,SR_BEV		# exception via BEV
	mtc0	Tmp0,C0_SR
	.set	reorder
	move	Ra_save0,ra		# stash return address

	MACHINE_SPECIFIC
L_M6000:
L_M20:
#ifdef SABLE
	li	Retval,8*1024*1024
#else
	/* XXX add stuff!! */
	li	Retval,32*1024*1024
#endif !SABLE
	j	Ra_save0
L_M2000: ; L_RB3125:
	.set	noreorder
	lw	zero,SBE_ADDR+K1BASE	# clear any pending Write bus errs
	sw	zero,SBE_ADDR+K1BASE	# clear any pending Write bus errs
	.set	reorder
	b	1f
L_M500: ; L_M800: ; L_M1000: ; L_M120: ; L_M180:
	lw	zero,SBE_ADDR+K1BASE	# clear any pending Write bus errs
	b	1f
1:
	LED_MACHDEP(LMEM_PATTERN)	# show that lmem config underway
        DELAY(10000)
	/* only cpu zero configs memory, the rest wait */
	L_CPUID(Cpuid)		# get cpuid
	beq	Cpuid,zero,2f	# we only do vme config for cpu 0
	/*
	 * loop waiting for cpu 0 to finish configuring memory
	 */
	sb	zero,K1BASE+VME_IMR	# mask all vme interrupts for non-cpu0
	/*
	 * Spin a while to meet dram power-up requirements and to
	 * insure all vme cards are well out of reset initialization
	 */
	DELAY(50000)

wait_for_cpu0:
	/* wait for first 4mg to be addressable */
	la	Bevhndlr,mem_not_ready	# current fault handler
	DELAY(10000)
	lw	Tmp0,K1BASE|(0x400000-4)	# try to read address 4mg-1

wait_for_mem_config:
	DELAY(10000)
	lw	Retval,memsize		# read memory size from pre-agreed addr
	beq	zero,Retval,wait_for_mem_config	# if zero, not done or no mem
	li	Bevhndlr,0		# clear fault handler
	j	Ra_save0		# returns last memaddr + 1


2:
	/*
	 * Spin a while to meet dram power-up requirements and to
	 * insure all vme cards are well out of reset initialization
	 */
	DELAY(50000)
	/*
	 * setup initial values for allocating vectors and io addresses
	 */
	li	Vec,LMEM_VEC_BASE
	li	Ioadr,LMEM_ADR_BASE
	li	Membd,0
	la	Bevhndlr,lmem_hndlr	# current fault handler
	li	Mask4meg,0		# initialize masks for found boards
	li	Mask8meg,0
	li	Mask16meg,0
	li	Mask32meg,0
	li	Mask,1			# initialize current pos mask

	/*
	 * probe for a local memory card by writing to VME Supervisor
	 * A16 address 0, if memory cards are out there, all unconfigured
	 * cards will respond and generate an interrupt to the vector
	 * written.  Normal VME interrupt arbitration will condition
	 * the closest card for configuration, we reset its io address
	 * and initialize the card via its new io address.  Repeat
	 * this process until there are no unconfigured cards which
	 * respond to SA16 0, at which time a bus error will be generated.
	 */
	MACHINE_SPECIFIC
L_M6000:
	/* XXX won't get here because we've already exited ... ?? */
L_M2000: ; L_RB3125:
	j	m2000mem		# Use jump do to nested macros
L_M20:
L_M500: ; L_M800: ; L_M1000: ; L_M120: ; L_M180:
probe_loop:
	.set	noreorder
	mtc0	zero,C0_CAUSE		# clear any software interrupts
	.set	reorder
	LED_MACHDEP(LMEM_CLRVME_PATTERN) # show waiting for interrupt
	sh	zero,SA16(MEM_CNTRL)	# make sure all enables off
	sh	Vec,SA16(MEM_IV)	# probe for memory board
	LED_MACHDEP(LMEM_WAIT_PATTERN)	# show waiting for interrupt
	li	Tmp0,1<<3
	sb	Tmp0,K1BASE+VME_IMR	# unmask level 3 vme interrupts
retry:
	DELAY(1000)			# allow some time for things to happen
	.set	noreorder
	mfc0	Tmp0,C0_CAUSE
	nop
	and	Tmp0,CAUSE_IP8|CAUSE_IP3 # look for bus error or vme interrupt
	.set	reorder
	beq	zero,Tmp0,retry		# nothing yet
	and	Tmp1,Tmp0,CAUSE_IP8
	bne	Tmp1,zero,do_config	# write bus error, no more boards
	LED_MACHDEP(LMEM_ACK_PATTERN)	# show that we're ack'ing
	move	Arg0,Vec		# vector we expect
	jal	__ack_vme		# acknowledge vme interrupt
	srl	Tmp0,Ioadr,IOADDR_SHIFT
	sh	Tmp0,SA16(MEM_IOADDR)	# set new io address
	/*
	 * The various masks here correlate io addresses with board types
	 */
	addu	Tmp1,Ioadr,VME_A16SBASE+K1BASE # VME Supr A16 cpu address
	lhu	Tmp0,+(MBOXMEM_PROMID+(ID_DRAMTYPE*2))(Tmp1) # get dram type
	and	Tmp0,0xff		# only low 8 bits significant
	bne	Tmp0,CHAR_M,1f
	or	Mask16meg,Mask		# fully populated 1Mb drams (16MB)
	b	8f

1:	bne	Tmp0,CHAR_m,1f
	or	Mask8meg,Mask		# half populated 1Mb drams (8MB)
	b	8f

1:	or	Mask4meg,Mask		# fully populated 256K drams (4MB)
	/*
	 * bump everything and look for another board
	 */
8:	addu	Vec,1
	addu	Membd,1
	addu	Ioadr,LMEM_IO_SIZE
	sll	Mask,1
	b	probe_loop

do_config:
	/*
	 * at this point we have found all boards, now go through masks
	 * that indicate quantity of each board type and config them
	 */
	lw	zero,SBE_ADDR+K1BASE	# clear any pending Write bus errs
	DELAY(10)
	.set	noreorder
	mfc0	Tmp0,C0_CAUSE
	nop
	and	Tmp0,CAUSE_IP8
	.set	reorder
	bne	Tmp0,zero,do_config	# more wbe's were in write buffer
	bne	Membd,zero,1f		# at least one board present
	li	Bevhndlr,0		# clear fault handler
	li	Retval,0		# show no local memory present
	j	Ra_save0		# return

1:	li	Arg0,BASEADDR		# initial base address for memory
	move	Arg1,Mask16meg
	li	Arg2,0x1000000		# 16MB
	jal	config			# config 16 Meg boards
	move	Arg0,Retval		# put 8 Meg boards after 16 Meg bds
	move	Arg1,Mask8meg
	li	Arg2,0x800000		# 8MB
	jal	config			# config 8 Meg boards
	move	Arg0,Retval		# put 4 Meg boards after 8 Meg bds
	move	Arg1,Mask4meg
	li	Arg2,0x400000		# 4MB
	jal	config			# config 4 Meg boards
	li	Bevhndlr,0		# clear fault handler
	j	Ra_save0		# returns last memaddr + 1

	/*
	 * M2000 & Genesis memory board configuration
	 * returns number of boards in v0 and memory size in v1.
	 */
m2000mem:
	.set	noreorder
	mtc0	zero,C0_CAUSE		#clear any software interrupt
	.set	reorder
	LED_MACHDEP(LMEM_CLRVME_PATTERN) # show waiting for interrupt
	DELAY(100000)
	li	s5,0x82			# value for mem cntrl (InhibECC,EnVME)
	li	s6,0x800e		# mem cntl reg (EnA24,EnSynErr,EnVME)
	/*
	 * RefreshRate configuration:
	 * R325x boards - Set RefreshRate for 25Mhz.
	 * R326x boards - Set RefreshRate for 33Mhz. 20Mhz is no longer
	 *		supported. RB3133 only use the R326x boards.
	 */
	lbu	Tmp0,K1BASE+IDPROM_REV	# get cpu brd rev from cpu id prom
	and	Tmp0,0xF0		# upper nibble is the board speed
	bne	Tmp0,REV_RB3133,1f	# branch if not 33Mhz Genesis
	or	s6,0x400 		# Set refresh rate for 33Mhz
1:
	/*
	 * Configure memory boards starting from the largest size possible
	 * (128MB). To probe on a slot, do not write zero to the control 
	 * register since it might have been configured. Instead write to 
	 * the interrupt vector register. If the cpu board is M/2000-25 or 
	 * Genesis-25, set the RefreshRate on R325x boards, but not R326x 
	 * boards.
	 * NOTE: Only the first board configured should have A24 enabled.	
	 */
	li	Retval,0		# mem boards
	li	v1,0			# mem size
	li	Tmp1,BRDSIZE_128MEG	# starting board size is 128MB
	/*
	 * loop on board size
	 */
m2000probe_loop1:
	/*
	 * loop on slot
	 */
	move	Ioadr,zero		# start from slot 0
1:
	sll	Arg1,Ioadr,8		# shift to slot byte offset
	sh	Ioadr,SA16(MEM_IV)(Arg1) # write to generate possible bus error
					 # if the board isn't there
	DELAY(8000)
	.set	noreorder
	mfc0	Tmp0,C0_CAUSE
	nop
	and 	Tmp0,CAUSE_IP8		# look for bus error
	.set	reorder
	bne	Tmp0,zero,99f		# branch if no mem board found
	lh	Arg0,SA16(MEM_BRDSIZE)(Arg1)	# check mem size
	and	Arg0,0xF0		# upper nibble is the size
	bne	Arg0,Tmp1,99f		# is it the right size?

	srl 	Tmp0,v1,24		# MEM_ADDR uses address<31..24>
	sll	Arg0,Tmp0,8
	addu	Arg0,Tmp0
	sh	Arg0,SA16(MEM_ADDR)(Arg1)
	sh	s5,SA16(MEM_CNTRL)(Arg1) # mem cntrl: disable ecc	
	sh	s6,SA16(MEM_CNTRL)(Arg1) # mem cntrl: enable ecc

	lh	Tmp0,SA16(MEM_BRDTYPE)(Arg1) # check mem board type
	beq	Tmp0,BRDTYPE_R3260,3f	# branch if R326x boards

	lbu	Tmp0,K1BASE+IDPROM_BRDTYPE # get cpu brd type from cpu id prom
	beq	Tmp0,BRDTYPE_RB3125,2f	# branch to set Refresh if Genesis cpu

	lbu	Tmp0,K1BASE+IDPROM_REV	# get cpu brd rev from cpu id prom
	and	Tmp0,0xF0		# upper nibble is the board speed
	bne	Tmp0,REV_R3200_25,3f	# branch if not 25Mhz
2:
	or	Tmp0,s6,0x400 		# set R325x refresh rate for 25Mhz
	sh	Tmp0,SA16(MEM_CNTRL)(Arg1) # mem cntrl
3:
	and	s6,0x7fff		# disable A24 after first board
	not	Arg0,Retval
	sh	Arg0,SA16(MEM_LEDREG)(Arg1) # display mem leds
	srl	Tmp0,Tmp1,2		# shift brd size to word index
	lw	Arg0,m2000_memsize_map(Tmp0)
	addu	v1,Arg0			# new memsize
	addiu	Retval,1		# increment no. of boards config'ed
99:
	.set	noreorder
	lw	zero,SBE_ADDR+K1BASE	# clear pending write bus error
	sw	zero,SBE_ADDR+K1BASE	# clear pending write bus error
	.set	reorder
	DELAY(10)
	.set 	noreorder
	mfc0	Tmp0,C0_CAUSE
	nop
	and	Tmp0,CAUSE_IP8
	.set	reorder
	bne	Tmp0,zero,99b		# wait until bus error cleared
	add	Ioadr,1			# next slot to probe
	blt	Ioadr,8,1b		# only 8 system bus slots

	subu	Tmp1,0x10		# next size of boards
	bne	Tmp1,zero,m2000probe_loop1

m2000finish:
	j	Ra_save0

	END(lmem_config)

/*
 * config local register allocation
 */
#define	Baseaddr	t0		/* current memory base address */
#define	Curmask		t1		/* current position mask */
#define	Memsize		t2		/* current memory card size */
#define	Bd1		t3		/* io address slot index */
#define	Bd2		t4		/* io address slot index */
#define	Ioadr1		t5		/* io address */
#define	Ioadr2		t6		/* io address */
#define	Basebits	t7		/* base address bits */
#define	Addrbits	t8		/* address register bits */
#define	Controlbits	t9		/* control register bits */

/*
 * config -- level 1 routine to configure a group of equal sized
 * memory cards
 */
config:
	move	Baseaddr,Arg0		# save args
	move	Curmask,Arg1
	move	Memsize,Arg2
	move	Ra_save1,ra		# stash return address

next_bit:
	/*
	 * loop through mask of boards present assigning base addresses
	 * and completing configuration.  If two board are available,
	 * interleave them.
	 */
	bgeu	Memsize,0x800000,1f	# must be using 1MB drams
	li	Addrbits,0		# 256K drams
	b	2f

1:	li	Addrbits,MEGDRAM	# 1MB drams
2:	move	Arg0,Curmask		# find next board
	jal	__ffs
	move	Bd1,Retval
	beq	Bd1,zero,no_board	# no more boards
	subu	Bd1,1			# call first bit BIT 0, not BIT 1
	/*
	 * clear bit we just found
	 */
	li	Tmp0,1
	sll	Tmp0,Tmp0,Bd1
	not	Tmp0
	and	Curmask,Tmp0

	mul	Ioadr1,Bd1,LMEM_IO_SIZE	# calculate io address from bit pos
	addu	Ioadr1,LMEM_ADR_BASE+VME_A16SBASE+K1BASE

	srl	Tmp0,Baseaddr,22	# position base address bits
	or	Addrbits,Tmp0		# or into address register setup

#ifdef INTERLEAVE
	/*
	 * Look for a board that can be paired with one just found
	 * for interleaving
	 */
	move	Arg0,Curmask
	jal	__ffs
	move	Bd2,Retval
	beq	Bd2,zero,one_board	# can't interleave
	subu	Bd2,1			# call first bit BIT 0, not BIT 1
	/*
	 * clear this bit from mask
	 */
	li	Tmp0,1
	sll	Tmp0,Tmp0,Bd2
	not	Tmp0
	and	Curmask,Tmp0

	mul	Ioadr2,Bd2,LMEM_IO_SIZE # calculate io address from bit pos
	addu	Ioadr2,LMEM_ADR_BASE+VME_A16SBASE+K1BASE

	/*
	 * Now enable appropriate address comparators depending on
	 * board size and interleave position
	 */
	bne	Memsize,0x400000,1f
	or	Addrbits,ENAB_24|ENAB_23
	b	2f

1:	bne	Memsize,0x800000,1f
	or	Addrbits,ENAB_24
	b	2f

1:	bne	Memsize,0x1000000,config_error
	/*
	 * build value for memory address register and drop it in
	 */
2:	or	Tmp0,Addrbits,INTLV|INTLV_BRD0
	sh	Tmp0,MEM_ADDR(Ioadr1)
	/*
	 * set up control register
	 */
	li	Controlbits,CONTROL_BITS
	addu	Tmp1,Baseaddr,Memsize

	bne	Memsize,0x1000000,1f	/* 16Meg board fix */
	bgtu	Tmp1,Memsize,3f
	b	2f

1:	addu	Tmp1,Memsize
	bgtu	Tmp1,MAX_A24_ADDR,3f
2:	or	Controlbits,AM39_ENABLE
3:	lbu	Tmp0,refresh_map(Bd1)
	sll	Tmp0,REFRESH_SHIFT
	and	Tmp0,REFRESH_MASK
	or	Tmp0,Controlbits
	/*
	 * Write control bits once without error enables to flush any
	 * pending errors and allow dummy read cycles without errors
	 */
	sh	Tmp0,MEM_CNTRL(Ioadr1)
	/*
	 * High bit of led register indicates board is part of interleave pair
	 * next bit indicates board zero or board 1 of interleave pair
	 * low 6 bits of led register will show base address in MB
	 */
	srl	Tmp0,Baseaddr,20
	and	Tmp0,LED_BASEMASK
	or	Tmp0,LED_INTERLEAVE
	not	Tmp0
	sh	Tmp0,MEM_LEDREG(Ioadr1)

	/*
	 * Now do it all over again for other board of interleave pair
	 */
	or	Tmp0,Addrbits,INTLV|INTLV_BRD1
	sh	Tmp0,MEM_ADDR(Ioadr2)
	lbu	Tmp0,refresh_map(Bd2)
	sll	Tmp0,REFRESH_SHIFT
	and	Tmp0,REFRESH_MASK
	or	Tmp0,Controlbits
	sh	Tmp0,MEM_CNTRL(Ioadr2)
	srl	Tmp0,Baseaddr,20
	and	Tmp0,LED_BASEMASK
	or	Tmp0,LED_INTERLEAVE|LED_INTLVODD
	not	Tmp0
	sh	Tmp0,MEM_LEDREG(Ioadr2)
	/*
	 * Perform initial dummy cycles on low bank to condition drams
	 */
	addu	Tmp0,Baseaddr,K1BASE	# cpu k1 address
	addu	Tmp1,Tmp0,0x200		# end point
1:	lw	zero,0(Tmp0)
	addu	Tmp0,4
	bltu	Tmp0,Tmp1,1b
	/*
	 * Bump base address by 2 * memsize, since interleaved
	 */
	addu	Baseaddr,Memsize
	addu	Baseaddr,Memsize
	/*
	 * Perform initial dummy cycles on high bank to condition drams
	 */
	addu	Tmp0,Baseaddr,K1BASE-0x200
	addu	Tmp1,Tmp0,0x200
1:	lw	zero,0(Tmp0)
	addu	Tmp0,4
	bltu	Tmp0,Tmp1,1b
	/*
	 * Now enable errors on both boards
	 */
	lhu	Tmp0,MEM_CNTRL(Ioadr1)
	or	Tmp0,ERRENAB_BITS
	sh	Tmp0,MEM_CNTRL(Ioadr1)
	lhu	Tmp0,MEM_CNTRL(Ioadr2)
	or	Tmp0,ERRENAB_BITS
	sh	Tmp0,MEM_CNTRL(Ioadr2)
	b	next_bit
#endif INTERLEAVE

one_board:
	/*
	 * configure non-interleaved board
	 *
	 * First calculate address comparator enable bits
	 */
1:	bne	Memsize,0x400000,1f
	or	Addrbits,ENAB_24|ENAB_23|ENAB_22
	b	2f

1:	bne	Memsize,0x800000,1f
	or	Addrbits,ENAB_24|ENAB_23
	b	2f

1:	bne	Memsize,0x1000000,config_error
	or	Addrbits,ENAB_24
2:	sh	Addrbits,MEM_ADDR(Ioadr1)
	li	Controlbits,CONTROL_BITS
	addu	Tmp1,Baseaddr,Memsize

	bne	Memsize,0x1000000,1f	/* 16Meg board fix */
	bgtu	Tmp1,Memsize,3f
	b	2f

1:	bgtu	Tmp1,MAX_A24_ADDR,3f
2:	or	Controlbits,AM39_ENABLE
3:	lbu	Tmp0,refresh_map(Bd1)
	sll	Tmp0,REFRESH_SHIFT
	and	Tmp0,REFRESH_MASK
	or	Tmp0,Controlbits
	/*
	 * Write control bits without error enables to flush any
	 * pending errors and allow us to run dummy read cycles
	 * without errors being detected
	 */
	sh	Tmp0,MEM_CNTRL(Ioadr1)
	srl	Tmp0,Baseaddr,20
	and	Tmp0,LED_BASEMASK
	not	Tmp0
	sh	Tmp0,MEM_LEDREG(Ioadr1)
	/*
	 * Perform initial dummy cycles on low bank to condition drams
	 */
	addu	Tmp0,Baseaddr,K1BASE
	addu	Tmp1,Tmp0,0x100
1:	lw	zero,0(Tmp0)
	addu	Tmp0,4
	bltu	Tmp0,Tmp1,1b
	addu	Baseaddr,Memsize
	/*
	 * Perform initial dummy cycles on high bank to condition drams
	 */
	addu	Tmp0,Baseaddr,K1BASE-0x100
	addu	Tmp1,Tmp0,0x100
1:	lw	zero,0(Tmp0)
	addu	Tmp0,4
	bltu	Tmp0,Tmp1,1b
	/*
	 * Now enable errors
	 */
	lhu	Tmp0,MEM_CNTRL(Ioadr1)
	or	Tmp0,ERRENAB_BITS
	sh	Tmp0,MEM_CNTRL(Ioadr1)
	b	next_bit

no_board:
	/*
	 * configured all boards of this size, return next available base
	 */
	move	Retval,Baseaddr
	j	Ra_save1

config_error:
	/*
	 * something that should never happen, happened!
	 */
	LED_MACHDEP(LMEM_ERROR_PATTERN)
1:	b	1b

/*
 * __ack_vme(expected_vector) -- acknowledge level 3 vme interrupts
 * until we see the desired vector
 */
__ack_vme:
	li	Tmp0,1<<3
	sb	Tmp0,K1BASE+VME_IMR	# unmask level 3 interrupts
1:
	DELAY(100)
	lbu	Tmp0,K1BASE+VME_ISR
	beq	Tmp0,zero,1b		# no level 3 pending
	lhu	Tmp0,K1BASE+VME_IACK+(2*2)	# do iack to get vector
	and	Tmp0,0xff		# local mem only drives low 8 bits
	bne	Tmp0,Arg0,1b		# not the one we were expecting
	j	ra

/*
 * __ffs(mask) -- find first bit set in mask
 * lsb returns 1, msb returns 32, no bits found returns 0
 * searches lsb to msb
 */
__ffs:
	li	Retval,0		# initial bit pos counter
	beq	Arg0,zero,2f		# no bits set
1:	addu	Retval,1		# bump bit pos
	and	Tmp0,Arg0,1		# check lsb
	srl	Arg0,1			# new bit to lsb in prep for next loop
	beq	Tmp0,zero,1b		# not set, try again
2:	j	ra

/*
 * __set_leds -- write pattern to cpu board leds
 */
LEAF(__set_leds)
	move	Arg1,Arg0		# machine_type uses Arg0
	move	Arg2,ra
	jal	get_machine_type
	beq	v0,BRDTYPE_M180,1f
	beq	v0,BRDTYPE_R2400,1f
	beq	v0,BRDTYPE_R6300,2f
	beq	v0,BRDTYPE_R3030,3f

	/* not R2400 and not R6300 */
	or	Arg0,LED_LMEM_RUN|LED_FPBD_RUN

1:	not	Arg0			# 0=on 1=off
2:	sub	v1,v0,1			# zero base it
	sll	v1,2			# machine_type index
	lw	v1,LED_REG(v1)		# get LED_REG addr
	sb	Arg1,(v1)
3:
	j	Arg2
	END(__set_leds)

/*
 * __lmem_reset -- reset local memory refresh counters
 *		   
 */
LEAF(__lmem_reset)
	li	Tmp0,+((~LMEM_RESET_PATTERN)|LED_FPBD_RUN)&~LED_LMEM_RUN
	sb	Tmp0,LED_REG_R2300+K1BASE
	DELAY(100)
	li	Tmp0,(~LMEM_RSTDONE_PATTERN)|LED_LMEM_RUN|LED_FPBD_RUN
	sb	Tmp0,LED_REG_R2300+K1BASE
	j	ra
	END(__lmem_reset)
/*
 * __lmem_reset_2000 -- reset local memory refresh counters.
 *	machine type was figured before calling, so just have 2 separate 
 *	routines, since they are only a few instrubtions
 *		   
 */
LEAF(__lmem_reset_m2000)
	li	Tmp0,2
	sb	Tmp0,CPU_CON_M2000|K1BASE
	DELAY(100)
	li	Tmp0,0
	sb	Tmp0,CPU_CON_M2000|K1BASE
	j	ra
	END(__lmem_reset_m2000)


#ifndef	R3030

#define	BrdType		Mask4meg
#define	CtlSpaceAddr	Mask8meg
#define CtlSpaceAddrUpb	Mask16meg
#define MemoryBaseAddr	Mask32meg
#define BUSERR_PATTERN	0xdeadbeef

/*
 * __lmem_reset_r6000 -- reset local memory refresh counters.
 *
 *  Initiate Memory board refresh on each Memory in turn, beginning its refresh
 *  cycle at least 32 cycles later than the previous Memory.  Ideally, the
 *  refresh cycles should start every 1024/(#boards) cycles.
 *
 *  In order to get this so carefully timed, we must execute cached i-fetches.
 *  Thus, we must follow this sequence:  (1) initialize the caches, (2) start
 *  the refresh going on the first Memory, and initialize the board for use,
 *  (3) copy the critical routine to physical memory, and jump to its K0
 *  entrypoint.
 */
LEAF(__lmem_reset_r6000)
	move	Ra_save0,ra		# save return address
	/*
	 *  Until we're told otherwise by the hardware folks, we don't
	 *  need to worry about initializing the refresh counters.
	 */
	j	Ra_save0		# XXX

	li	sp,BRDTYPE_R6300	# for init_cache()
	jal	init_cache

	move	Arg0,zero		# find CtlSpace for first Memory
	jal	find_ctlspace_r6000
	
	/* XXXXXX add stuff */
	j	Ra_save0		# exit
	END(__lmem_reset_r6000)

/*
 *  find_ctlspace_r6000 (Mbyte_base_addr)
 *
 *  Find the CtlSpace for the specified physical base address (in Mbytes) and
 *  return the CtlSpace address for that board (in v0) and the Mbyte size (v1).
 *
 *  Uses registers t4..t8, a0..a2.
 */
LEAF(find_ctlspace_r6000)
	move	t8,ra			# save return address
	.set	noreorder
	mfc0	t7,C0_SR		# save SR
	move	a2,Bevhndlr		# (BD) save current exc handler addr
	or	t4,t7,SR_BEV		# enable BEV
	mtc0	t4,C0_SR
	.set	reorder

	li	t4,CTL_SPACE+CTL_SPACE_UNIT_BSIZE	 # slot #1
	add	t5,t4,SBC_SLOT_UPB*CTL_SPACE_UNIT_BSIZE	 # last slot, + 1 slot
	move	t6,Arg0			# remember base address

	/*
	 *  Step through each slot's BrdAddress, and look for a match
	 */
1:	add	Arg1,t4,SBC_BRDADDR
	.set	noreorder
	la	Bevhndlr,__r6000_slot_unoccupied
	lw	Arg0,(Arg1)		# BrdAddress for this slot
	move	Bevhndlr,a2		# (BDSLOT) BusErr returns here
					#  and we restore original exc hndlr
	.set	reorder
	beq	Arg0,BUSERR_PATTERN,3f	# if Bus Error, then nothing here
	jal	decode_brd_address	# decode it
	beq	v1,zero,2f		# skip it if Mbyte size is zero
	beq	v0,t6,4f		# found a match?
2:	add	t4,CTL_SPACE_UNIT_BSIZE	#  no, try next slot
	bne	t4,t5,1b

	/* if fall through to here, then didn't find a match */
3:	move	Retval,zero		# return zero
	b	9f			# go exit

	/* found a match:  return K1 address of this CtlSpace */
4:	move	Retval,t4
	.set	noreorder
9:	j	t8			# exit
	mtc0	t7,C0_SR		# (BDSLOT) restore SR
	.set	reorder
	END(find_ctlspace_r6000)

/*
 *  make_brd_address (base_mbyte_address, mbyte_size)
 *
 *  Takes a base address (in Mbytes) and a size (in Mbytes),
 *  and returns a Bus Chip BoardAddress value.
 *
 *  base_mbyte_address must be a physical Mbyte address (not K0 or K1).
 *
 *  base_mbyte_size must be a power of two, in the range of 0..512 (MBytes).
 *
 *  must NOT use a2 or a3.
 */
LEAF(make_brd_address)
	.set	reorder
	move	t0,zero			# init the index
	move	t1,zero

1:	bne	a1,zero,2f		# if zero size, then know index now
	li	t7,1
	li	t8,1
	b	3f

2:	addi	t0,4			# increment index
	andi	t1,a1,1			# least-significant bit set?
	srl	a1,1			#   shift next bit down
	beq	t1,zero,1b		# no, keep shifting/incrementing

3:	la	t1,mbyte_size_mask
	add	t0,t1			# index into table
	lw	t0,(t0)			# and retrieve the size_mask

	move	v0,zero			# init the return value
	nor	t8,zero,a0		# form ~base_mbyte_address
	and	t8,t0			# mbyte_size_mask & ~base_mbyte_address
	and	t7,t0,a0		# mbyte_size_mask &  base_mbyte_address

					# t7 has wanna-be-ones
					# t8 has wanna-be-zeroes

	move	t0,zero			# init starting bit shift
	li	t1,32			#  and ending bit shift

4:	andi	t2,t7,1			# isolate least-significant bit
	sll	t2,t2,t0		#  shift for even-numbered bit
	or	v0,t2			#   and merge into result
	srl	t7,1			# prepare for next bit
	addi	t0,1

	andi	t2,t8,1			# isolate least-significant bit
	sll	t2,t2,t0		#  shift for odd-numbered bit
	or	v0,t2			#   and merge into result
	srl	t8,1			# prepare for next bit
	addi	t0,1
	bne	t0,t1,4b		# all done?

	j	ra			# exit
	END(make_brd_address)


/*
 * __ctlspace_init_r6000  --  initialize R6000 CtlSpace
 */

LEAF(__ctlspace_init_r6000)
	.set	noreorder
	li	Tmp0,SR_BEV		# exception via BEV
	mtc0	Tmp0,C0_SR
	.set	reorder
	move	Ra_save0,ra		# stash return address

	/*
	 *  Pass One:  Configure I/O Controllers (IOC's)
	 *	       Reset BrdAddr of Memory boards to be unresponsive
	 *
	 *	The Primary IOC is the first one with BootProm.  At Reset it	
	 *	responds to all of physical memory.  We now reset it to respond
	 *	to its correct range (0x1c000000 for 64 MB).
	 *      The non-Primary IOC's are set up below the Primary, 32 MB each.
	 *	NOTE:  The first IOC *must* have BootProm.
	 *
	 *      We reset the BrdAddress of the Memory boards to be unresponsive
	 *	because we may have restarted at the BootProm entrypoint by
	 *	way of a   "go 0xbfc00000"   and not a hardware Reset.
	 *
	 *	NOTE:  Byte zero of the IdProm contains the special SBC
	 *		self-configuration information.  The "normal" IdProm
	 *		information starts in the next word.
	 */
	li	CtlSpaceAddr,CTL_SPACE+CTL_SPACE_UNIT_BSIZE	# slot #1
	add	CtlSpaceAddrUpb,CtlSpaceAddr,SBC_SLOT_UPB*CTL_SPACE_UNIT_BSIZE
	move	Ioadr,zero		# Primary IOC addr:  not yet seen
1:
	/*
	 *  beginning of loop
	 */
#ifdef R6000_BUG_IDPROM
	add	Arg1,CtlSpaceAddr,SBC_CTLMISC
#else
	add	Arg1,CtlSpaceAddr,SBC_IDPROM+4
#endif R6000_BUG_IDPROM
	.set	noreorder
	la	Bevhndlr,__r6000_slot_unoccupied
	lw	Arg0,(Arg1)		# may cause Bus Error
	nop				# (LDSLOT)
	.set	reorder
	beq	Arg0,BUSERR_PATTERN,10f	# if BusError, then quit loop
#ifdef R6000_BUG_IDPROM
	and	Arg2,Arg0,BrdHasBootProm
	li	BrdType,BRDTYPE_R6360
	bne	Arg2,zero,99f
	and	Arg2,Arg0,BrdIsMem
	li	BrdType,BRDTYPE_R6350
	bne	Arg2,zero,99f
	li	BrdType,BRDTYPE_R6300
99:
#else
	andi	BrdType,Arg0,0xFF
#endif R6000_BUG_IDPROM
	
	bne	BrdType,BRDTYPE_R6360,8f  # not an IOC
	/*
	 *  IOC == BRDTYPE_R6360
	 */
	add	Tmp0,CtlSpaceAddr,SBC_BRDADDR	# addr of BrdAddress
	bne	Ioadr,zero,3f		# already configured Primary IOA?
	/*   Primary   */
	li	Arg0,IOA1_BRD		# firm, fixed BrdAddress for Pri IOA
	sw	Arg0,0(Tmp0)		# reset BrdAddress for Primary
	li	Ioadr,(IOA2 >>20)&0x1ff	# form MB base address of next IOC
	b	9f
3:	/*   non-Primary   */
	move	Arg0,Ioadr		# Mbyte base address
	li	Arg1,32			# Mbyte size
	jal	make_brd_address
	sw	Retval,0(Tmp0)		# set BrdAddress for non-Primary
	sub	Ioadr,32	 	# Mbyte base addr of next IOC
	b	9f

	/*
	 *  everything else (including MEMORY and CPU)
	 */
8:	li	Retval,0xffffffff
	add	Tmp0,CtlSpaceAddr,SBC_BRDADDR
	sw	Retval,0(Tmp0)

9:	add	CtlSpaceAddr,CTL_SPACE_UNIT_BSIZE  # next CtlSpace address
	bne	CtlSpaceAddr,CtlSpaceAddrUpb,1b
10:

	/*
	 *  Pass Two:  Determine Memory board sizes
	 */
	li	CtlSpaceAddr,CTL_SPACE+CTL_SPACE_UNIT_BSIZE	# slot #1
	add	CtlSpaceAddrUpb,CtlSpaceAddr,SBC_SLOT_UPB*CTL_SPACE_UNIT_BSIZE
	move	Tmp1,zero		# largest encountered MEMORY
1:
	/*
	 *  beginning of loop
	 */
#ifdef R6000_BUG_IDPROM
	add	Arg1,CtlSpaceAddr,SBC_CTLMISC
#else
	add	Arg1,CtlSpaceAddr,SBC_IDPROM+4
#endif R6000_BUG_IDPROM
	.set	noreorder
	la	Bevhndlr,__r6000_slot_unoccupied
	lw	Arg0,(Arg1)		# may cause Bus Error
	nop				# (LDSLOT)
	.set	reorder
	beq	Arg0,BUSERR_PATTERN,10f	# if BusError, then quit loop
#ifdef R6000_BUG_IDPROM
	and	Arg2,Arg0,BrdHasBootProm
	li	BrdType,BRDTYPE_R6360
	bne	Arg2,zero,99f
	and	Arg2,Arg0,BrdIsMem
	li	BrdType,BRDTYPE_R6350
	bne	Arg2,zero,99f
	li	BrdType,BRDTYPE_R6300
99:
#else
	andi	BrdType,Arg0,0xFF
#endif R6000_BUG_IDPROM
	bne	BrdType,BRDTYPE_R6350,9f # not a MEMORY
	/*
	 *  MEMORY == BRDTYPE_R6350
	 */
	add	Tmp0,CtlSpaceAddr,SBC_MEMCTL	
	li	Arg0,EnaSErrCorrect	# enable ECC correction
	sw	Arg0,0(Tmp0)
#ifdef R6000_BUG_IDPROM
	/*
	 *  Some pre-FCS Memory boards have defective IdProm circuitry which,
	 *  if anything other than byte zero is accessed, will make the Memory
	 *  behave weirdly.  This means we can't look at the Size field to
	 *  distinguish a 32MB from a 128MB board.
	 *  What we can do, however, is temporarily configure the Memory as if
	 *  it were 128MB, then see if the board responds to addresses modulo
	 *  32MB; that is, store nonzero to offset zero, zero to offset 32MB,
	 *  then read offset zero:  if we read a zero, then it's really a 32MB
	 *  board.
	 *  Remember the Mbyte size in the Memory's SBC COMPARE register.
	 */
	li	Arg0,0			# Mbyte base address is zero
	li	Arg1,128		# Mbyte size is 128MB
	jal	make_brd_address
	add	Arg0,CtlSpaceAddr,SBC_BRDADDR
	sw	Retval,0(Arg0)		# set BrdAddress for test
	li	Arg2,K1BASE
	sw	Arg2,(Arg2)		# store nonzero at offset zero
#ifdef SABLE
	move	Arg1,Arg2		# set up to store zero at offset zero
#else
	add	Arg1,Arg2,32*1024*1024
#endif !SABLE
	sw	zero,(Arg1)		# store zero at offset 32MB
	lw	Arg1,(Arg2)		# load value at offset zero
	li	Arg2,0xffffffff
	sw	Arg2,(Arg0)		# clobber test BrdAddr
#ifdef SABLE
	add	Arg0,CtlSpaceAddr,SBC_IDPROM+4	# Sable: assume IdProm good
	lb	Arg2,ID_MEMSIZE_OFF(Arg0)	# N, for 2^^N Mbytes
	li	Arg0,1
	sll	Retval,Arg0,Arg2	# size in Mbytes
#else
	li	Retval,32		# tentatively 32MB
#endif SABLE
	beq	Arg1,zero,2f		# if read zero, then 32MB
	li	Retval,128		#  otherwise, 128MB
2:
#else
	add	Arg0,CtlSpaceAddr,SBC_IDPROM+4
	lb	Arg1,ID_MEMSIZE_OFF(Arg0)	# N, for 2^^N Mbytes
	li	Arg0,1
	sll	Retval,Arg0,Arg1	# size in Mbytes
#endif R6000_BUG_IDPROM
	add	Arg0,CtlSpaceAddr,SBC_COMPARE
	sw	Retval,(Arg0)		# remember in SBC_COMPARE
	slt	Arg2,Tmp1,Retval	# new candidate for largest?
	beq	Arg2,zero,9f		# no, this one is <= known largest
	move	Tmp1,Retval		# remember new largest MEMORY

9:	add	CtlSpaceAddr,CTL_SPACE_UNIT_BSIZE  # next CtlSpace address
	bne	CtlSpaceAddr,CtlSpaceAddrUpb,1b
10:

	/*
	 *  Pass Three:  Configure largest Memory boards
	 *	Locate the largest MEMORY boards, and reset their BrdAddress'es
	 *	appropriately.
	 *	Determine the size of the next largest MEMORY boards.  We need
	 *	to configure the MEMORY with the largest boards first, since
	 *	a board of size N must have a base address of a multiple of N.
	 */
	li	CtlSpaceAddr,CTL_SPACE+CTL_SPACE_UNIT_BSIZE	# slot #1
	add	CtlSpaceAddrUpb,CtlSpaceAddr,SBC_SLOT_UPB*CTL_SPACE_UNIT_BSIZE
	move	MemoryBaseAddr,zero	# first MEMORY
	move	Ioadr,zero		# largest MEMORY of next smaller size
1:
	/*
	 *  beginning of loop
	 */
#ifdef R6000_BUG_IDPROM
	add	Arg1,CtlSpaceAddr,SBC_CTLMISC
#else
	add	Arg1,CtlSpaceAddr,SBC_IDPROM+4
#endif R6000_BUG_IDPROM
	.set	noreorder
	la	Bevhndlr,__r6000_slot_unoccupied
	lw	Arg0,(Arg1)		# may cause Bus Error
	nop				# (LDSLOT)
	.set	reorder
	beq	Arg0,BUSERR_PATTERN,10f	# if BusError, then quit loop
#ifdef R6000_BUG_IDPROM
	and	Arg2,Arg0,BrdHasBootProm
	li	BrdType,BRDTYPE_R6360
	bne	Arg2,zero,99f
	and	Arg2,Arg0,BrdIsMem
	li	BrdType,BRDTYPE_R6350
	bne	Arg2,zero,99f
	li	BrdType,BRDTYPE_R6300
99:
#else
	andi	BrdType,Arg0,0xFF
#endif R6000_BUG_IDPROM
	bne	BrdType,BRDTYPE_R6350,9f # not a MEMORY
	/*
	 *  MEMORY == BRDTYPE_R6350
	 */
	add	Arg0,CtlSpaceAddr,SBC_COMPARE
	lw	Retval,(Arg0)		# size in MBytes
	bne	Retval,Tmp1,2f		# not one of the large sizes

	sltiu	Tmp0,MemoryBaseAddr,384	# if this Memory would intrude
	bne	Tmp0,zero,3f		#  into IOA space,
	li	MemoryBaseAddr,512	#   then bump it up to beyond IOAs
3:
	move	Arg0,MemoryBaseAddr	# base Mbyte addr of board
	move	Arg1,Retval		# size in Mbytes
	add	MemoryBaseAddr,Retval	# set up for next MEMORY
	jal	make_brd_address
	add	Tmp0,CtlSpaceAddr,SBC_BRDADDR
	sw	Retval,0(Tmp0)		# reset BrdAddress
	b	9f

2:	/* MEMORY board size is not the size we're configuring now */
	slt	Arg0,Tmp1,Retval	# is this board larger?
	bne	Arg0,zero,9f		#  yes, so we've already configured it
	slt	Arg0,Ioadr,Retval	# is board larger than next smallest?
	beq	Arg0,zero,9f
	move	Ioadr,Retval		#  yes, remember next smallest size

9:	add	CtlSpaceAddr,CTL_SPACE_UNIT_BSIZE  # next CtlSpace address
	bne	CtlSpaceAddr,CtlSpaceAddrUpb,1b
10:

	/*
	 *  Pass Four:  Configure next largest Memory boards
	 *	Locate the next largest MEMORY boards, and reset their
	 *	BrdAddress'es appropriately.
	 *	NOTE:  only expect two sizes of memory boards.  We need no more
	 *	       passes to configure MEMORY.
	 */
	li	CtlSpaceAddr,CTL_SPACE+CTL_SPACE_UNIT_BSIZE	# slot #1
	add	CtlSpaceAddrUpb,CtlSpaceAddr,SBC_SLOT_UPB*CTL_SPACE_UNIT_BSIZE
	beq	Ioadr,zero,10f		# are there any smaller MEMORY boards?

	move	Tmp1,Ioadr		# largest MEM of next smaller size
1:
	/*
	 *  start of loop
	 */
#ifdef R6000_BUG_IDPROM
	add	Arg1,CtlSpaceAddr,SBC_CTLMISC
#else
	add	Arg1,CtlSpaceAddr,SBC_IDPROM+4
#endif R6000_BUG_IDPROM
	.set	noreorder
	la	Bevhndlr,__r6000_slot_unoccupied
	lw	Arg0,(Arg1)		# may cause Bus Error
	nop				# (LDSLOT)
	.set	reorder
	beq	Arg0,BUSERR_PATTERN,10f	# if BusError, then quit loop
#ifdef R6000_BUG_IDPROM
	and	Arg2,Arg0,BrdHasBootProm
	li	BrdType,BRDTYPE_R6360
	bne	Arg2,zero,99f
	and	Arg2,Arg0,BrdIsMem
	li	BrdType,BRDTYPE_R6350
	bne	Arg2,zero,99f
	li	BrdType,BRDTYPE_R6300
99:
#else
	andi	BrdType,Arg0,0xFF
#endif R6000_BUG_IDPROM
	bne	BrdType,BRDTYPE_R6350,9f # not a MEMORY
	/*
	 *  MEMORY == BRDTYPE_R6350
	 */
	add	Arg0,CtlSpaceAddr,SBC_COMPARE
	lw	Retval,(Arg0)		# size in MBytes
	bne	Retval,Tmp1,9f		# not one of the small sizes

	sltiu	Tmp0,MemoryBaseAddr,384	# if this Memory would intrude
	bne	Tmp0,zero,3f		#  into IOA space,
	li	MemoryBaseAddr,512	#   then bump it up to beyond IOAs
3:
	move	Arg0,MemoryBaseAddr	#  base Mbyte addr of board
	move	Arg1,Retval		#  size in Mbytes
	add	MemoryBaseAddr,Retval	# set up for next MEMORY
	jal	make_brd_address
	add	Tmp0,CtlSpaceAddr,SBC_BRDADDR
	sw	Retval,0(Tmp0)		# reset BrdAddress

9:	add	CtlSpaceAddr,CTL_SPACE_UNIT_BSIZE  # next CtlSpace address
	bne	CtlSpaceAddr,CtlSpaceAddrUpb,1b
10:
11:	move	Bevhndlr,zero		# clear fault handler
	j	Ra_save0		# exit to caller
	END(__ctlspace_init_r6000)

/*
 *  __r6000_slot_unoccupied  --  exception handler for __ctlspace_init_r6000()
 *
 *  Returns Arg0 of ZERO, to signify nothing in this slot.  Uses Tmp0.
 * Does NOT reset Bevhndlr.
 */
LEAF(__r6000_slot_unoccupied)
	.set	noreorder
	mfc0	Tmp0,C0_EPC		# pc causing exception
	li	Arg0,BUSERR_PATTERN
	addi	Tmp0,4			# return past the failing Load
	j	Tmp0
	c0	C0_RFE			# (BDSLOT)
	.set	reorder
	END(__r6000_slot_unoccupied)

#else
/*
 * stub routines for 3030 to satisfy references
 */

LEAF(__lmem_reset_r6000)
	j	ra
	END(__lmem_reset_r6000)

LEAF(__ctlspace_init_r6000)
	j	ra
	END(__ctlspace_init_r6000)

#endif	R3030


mem_not_ready:
	.set	noreorder
	mfc0	Tmp0,C0_CAUSE		# get cause
	nop
	andi	Tmp0,0x3f		# isolate exception
	.set	reorder
	li	Tmp1,EXC_DBE		# read exception constant
	bne	Tmp0,Tmp1,1f		# see if we still have read error
	j	wait_for_cpu0		# cpu0 not done
1:					# if not read error, print pattern
/*
 * lmem_hndlr -- handler for faults while in local memory configuration
 * code.
 */
lmem_hndlr:
	.set	noreorder
	mfc0	Tmp1,C0_CAUSE		# get cause
	nop
	andi	Tmp1,0x3f		# isolate exception
	.set	reorder
1:
#ifndef R3030
	LED_MACHDEP(LMEM_FAULT_PATTERN)	# don't expect faults, so just
	DELAY(500000)
	move	Arg0,Tmp1
	jal	__set_leds		# put up pattern and loop
	DELAY(500000)
#endif !R3030
	b	1b

/*
 * boot exception vector handlers
 *
 * These routines check Bevhndlr for a current exception handler
 * if non-zero, jump to the appropriate handler
 * else set the leds and spin
 */
LEAF(bev_utlbmiss)
	beq	Bevhndlr,zero,1f
	.set	noreorder
	j	Bevhndlr
	move	Bevhndlr,zero		# (BDSLOT)
	.set	reorder
1:
	li	Tmp1,BEV_UTLBMISS_PATTERN
	j	loopit
	END(bev_utlbmiss)

LEAF(bev_general)
	beq	Bevhndlr,zero,1f
	.set	noreorder
	j	Bevhndlr
	move	Bevhndlr,zero		# (BDSLOT)
	.set	reorder
1:
	li	Tmp1,BEV_GENERAL_PATTERN
	j	loopit
	END(bev_general)

loopit:
#define	Tmp2		s4	/* short term temp reg only on bev_handlers */
#define	Tmp3		s5	/* short term temp reg only on bev_handlers */
	# loop putting out message,cause and epc

	.set	noreorder
	mfc0	Tmp3,C0_CAUSE		# get cause
	nop
	andi	Tmp3,0x3f		# isolate exception
	mfc0	Tmp2,C0_EPC		# get epc
1:
	li	Arg0,0x3f		# signal start of loop
	.set	reorder
	jal	__set_leds		# put up pattern and loop
#ifndef R3030
	DELAY(DCOUNT)
	move	Arg0,Tmp1
	jal	__set_leds		# put up pattern and loop
	DELAY(DCOUNT)
	move	Arg0,Tmp3		# cause
	jal	__set_leds
	DELAY(DCOUNT)
	move	Arg0,Tmp2		# low 4 bits
	andi	Arg0,0xf
	jal	__set_leds
	DELAY(DCOUNT)
	move	Arg0,Tmp2		# 4-7
	srl	Arg0,4
	andi	Arg0,0xf
	ori	Arg0,0x10		# next nibble indicator
	jal	__set_leds
	DELAY(DCOUNT)
	move	Arg0,Tmp2		# 8-11
	srl	Arg0,8
	andi	Arg0,0xf
	jal	__set_leds
	DELAY(DCOUNT)
	move	Arg0,Tmp2		# 12-15
	srl	Arg0,12
	andi	Arg0,0xf
	ori	Arg0,0x10		# next nibble indicator
	jal	__set_leds
	DELAY(DCOUNT)
	move	Arg0,Tmp2		# 16-19
	srl	Arg0,16
	andi	Arg0,0xf
	jal	__set_leds
	DELAY(DCOUNT)
#endif !R3030
	b	1b

	.data
refresh_map:
	.byte	0,4,2,6,1,5,3,7
	.byte	0,4,2,6,1,5,3,7

	.align	2
m2000_memsize_map:
	.word	0x00000000
	.word	0x01000000	/* BRDSIZE_16MEG */
	.word	0x02000000	/* BRDSIZE_32MEG */
	.word	0x04000000	/* BRDSIZE_64MEG */
	.word	0x08000000	/* BRDSIZE_128MEG */
	.word	0x10000000	/* BRDSIZE_256MEG */
	.word	0x00000000
