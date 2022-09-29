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
/* $Header: csu.mips.s,v 1.58.1.3 90/12/14 12:35:43 chungc Exp $*/

/*
 * csu.mips.s -- prom startup code
 */

#include "prom/prom.h"
#include "saio/saio.h"
#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "machine/mem_board.h"
#include "machine/timer.h"
#include "machine/ioa.h"
#include "machine/hd146818.h"
#include "machine/mk48t02.h"
#include "machine/bc.h"

/*
 * ASCII codes for boot mode options
 * m => monitor mode on reset: enter command parser
 * d => diagnostic mode on reset: monitor mode w/o clearing memory
 *	when this option is set, pon isn't run.
 * e => error found during power-on testing. used to force the user
 *      to see errors incurred, ie don't autoboot.
 * c => cold start on reset: perform autoboot
 * w => warm start on reset: look for "restart block" in memory,
 *	if found
 *		transfer to warm start code pointed at by restart block;
 *	if not found
 *		cold start
 * default => monitor mode
 */
#define CHAR_1		0x31
#define CHAR_c		0x63		/* cold start on reset */
#define CHAR_d		0x64		/* diagnostic mode on reset */
#define CHAR_e		0x65		/* error incurred during power-on testing
*/
#define CHAR_w		0x77		/* warm start on reset */

#define	SCC_CHAN_A	8	/* used for offset to scc channel a */
#define	SCC_CMD		3	/* offset for command reg */

#undef LEDDBG

#define WBFLUSH	.set noat;	\
		lui	$at,0xbfc0;\
		lw	$0,($at);  \
		.set at; \
9:		bc0f	9b

#ifdef SABLE
#define DELAY(x)
#else
#define	DELAY(x)				\
	li	t1,+(x);			\
9:	subu	t1,1;				\
	bne	zero,t1,9b
#endif !SABLE

#ifdef R3030
	LBSS(gs_rows, 4)		/* no. of rows for ansi screen */
	LBSS(gs_cols, 4)		/* no. of columns for ansi screen */
	LBSS(gs_curx, 4)		/* cursor position for ansi screen */
	LBSS(gs_cury, 4)		/* cursor position for ansi screen */
	LBSS(gs_bufaddr, 4)		/* buffer address of ansi screen */
#endif
	SHAREDBSS(cpu_present,MAX_CPUS*4)
	SHAREDBSS(memsize,4)		/* addressible by K0/K1 */
	SHAREDBSS(total_memsize,4)	/* total in system */
	SHAREDBSS(scsi_first,4)
	BSS(_fault_sp,4)
	BSS(environ, 4)
	BSS(maxcpu, 4)
	BSS(memtop, 4)
	BSS(malloc_start, 4)
	BSS(delay_mult,4)
	LBSS(movecode_tmp, 512)
	LBSS(rmw_tmp, 4)
	LBSS(exec_sp, 4)

	.text

/*
 * prom entry point table
 */
LEAF(start)
	j	realstart	/* prom entry point */
	j	_promexec	/* exec new program */
	j	_exit		/* reenter command loop without initializing */
	j	_init		/* reenter monitor and reinitialize */
	j	reboot		/* perform power-up boot (if set) */
	j	autoboot1	/* perform autoboot always */

	j	open		/* saio open() */
	j	read		/* saio read() */
	j	write		/* saio write() */
	j	ioctl		/* saio ioctl() */
	j	close		/* saio close() */
	j	getchar		/* get char from "console" */
	j	putchar		/* put char to "console" */
	j	showchar	/* visibly display any char */
	j	gets		/* get string from "console" */
	j	puts		/* put string to "console" */
	j	printf		/* print formatted string to "console" */

	j	init_proto	/* initialize protocol routines */
	j	proto_enable	/* set-up tty for protocol */
	j	proto_disable	/* put tty back to normal mode */
	j	getpkt		/* get protocol packet */
	j	putpkt		/* put protocol packet */

	j	orw_rmw		/* or word, read-modify-write cycle */
	j	orh_rmw		/* or half, read-modify-write cycle */
	j	orb_rmw		/* or half, read-modify-write cycle */
	j	andw_rmw	/* and word, read-modify-write cycle */
	j	andh_rmw	/* and half, read-modify-write cycle */
	j	andb_rmw	/* and half, read-modify-write cycle */

	j	flush_cache	/* flush entire cache */
	j	clear_cache	/* clear range in cache */

	j	setjmp		/* save stack state */
	j	longjmp		/* restore stack state */
	j	bev_utlbmiss	/* utlbmiss boot exception vector */
	j	getenv		/* get environment variable */
	j	_setenv		/* set environment variable */
	j	atob		/* ascii to binary conversion */
	j	strcmp		/* string compare */
	j	strlen		/* string length */
	j	strcpy		/* string copy */
	j	strcat		/* string concat */

	j	command_parser	/* command parser */
	j	parse_range	/* range parser */
	j	_argvize	/* tokenizer */
	j	help		/* prints help from command tables */

	j	dump		/* dump command */
	j	setenv		/* setenv command */
	j	unsetenv	/* unsetenv command */
	j	printenv	/* printenv command */
	j	bev_general	/* general boot exception vector */
	j	enable		/* enable console command */
	j	disable		/* disable console command */

	j	clear_nofault	/* clear any existing fault handlers */

	j	notimplemented	/* SPECIAL NOT IMPLEMENTED entry pt */

	j	nv_get		/* get nvram info by index  */
	j	nv_set		/* set nvram info by index  */
	j	warmreset	/* reset the machine thru warmreset */
 
 	j	diag_monitor	/* invoke diagnostics monitor */

	j	notimplemented	/* space for future entries */
	j	notimplemented	/* space for future entries */
	j	notimplemented	/* space for future entries */
	j	notimplemented	/* space for future entries */
	j	notimplemented	/* space for future entries */
	j	notimplemented	/* space for future entries */
	j	notimplemented	/* space for future entries */
	j	notimplemented	/* space for future entries */

realstart:
	#
	# save these important registers in less important ones
	# we'll save these to mem when it's available
	#
	move	t8,v0
	move	t9,v1
	move	s5,a0
	move	s6,a1
	move	s7,a2
	.set	noreorder
	mfc0	k0,C0_EPC		# save for nvram
	move	k1,ra			# save for nvram
	mfc0	gp,C0_SR
	mfc0	t7,C0_CAUSE
	li	v0,SR_BEV
	mtc0	v0,C0_SR		# state unknown on reset
	mtc0	zero,C0_CAUSE		# clear software interrupts
	nop				# paranoia
	.set	reorder

#if (RC6280 || RB3125)
	/*
 	 * If last three hardware resets did not get to the prom monitor,
	 * jump to the diags monitor if it exists. This is to invoke the
 	 * diags monitor when the machine has severe hardware faults.
	 *
	 * NVADDR_FLAG<7..4> counts the number of incomplete hardware resets.
	 * This counter gets reset to zero just before main() is called.
 	 */
 	NVRAM(NVADDR_FLAG)
 	and	a0, v0, NVFLAG_RSTMASK	# check for number of failed resets
	srl	a0, NVFLAG_RSTSHIFT
 	beq	a0, 3, 1f		# branch if last 3 attempts failed 
 	addu	v0, (1<<NVFLAG_RSTSHIFT) # count up reset attempts
 	sb	v0, (v1)
 	WBFLUSH				# (this is not needed for R6000)
 	j	2f
 1:
 	and	v0, ~NVFLAG_RSTMASK	# zero the counter
 	sb	v0, (v1)
 	WBFLUSH				# (this is not needed for R6000)
 	jal	diag_monitor		# call diags monitor if it exists
 2:
#endif (RC6280 || RB3125)
 
	.globl	_ndevices[]		# only used by prom recognition
	.globl	sccs_version[]
	.globl	scc_table
	.globl	mem_is_sized		# r3030 mem sized indicator
	.globl	noclmem			# do not clear memory
#if	!R3030
	.globl	duart_table
#endif	!R3030

	jal	prom_recognition	# call prom recognition rouitne

	MACHINE_SPECIFIC
L_M120: ; L_M180: ; L_M20:
	#
	# store regs at a meg
	#
#define STOREIT(n,m) sw	n,(m*4)+(0xa0100000)
#define FEEDFACE(m) sw	v0,(m*4)+(0xa0100000)
	#
	# any FEEDFACES are regs we've decided are not that important and
	#	have used to store other reg values in.
	#
	STOREIT($30,30)		# save fp ASAP so we can zero it, because
	move	fp,zero		#  bev_general and bev_utlbmiss believe fp
				#  contains an override exception vector
	STOREIT(t8,2)
	li	v0,0xfeedface	# for all those feedface folks (say that fast)
	STOREIT($0,0)
	FEEDFACE(1)
	STOREIT(t9,3)
	STOREIT(s5,4)
	STOREIT(s6,5)
	STOREIT(s7,6)
	STOREIT($7,7)
	STOREIT($8,8)
	STOREIT($9,9)
	STOREIT($10,10)
	STOREIT($11,11)
	STOREIT($12,12)
	STOREIT($13,13)
	STOREIT($14,14)
	FEEDFACE(15)
	STOREIT($16,16)
	STOREIT($17,17)
	STOREIT($18,18)
	STOREIT($19,19)
	STOREIT($20,20)
	FEEDFACE(21)
	FEEDFACE(22)
	FEEDFACE(23)
	FEEDFACE(24)
	FEEDFACE(25)
	FEEDFACE(26)
	FEEDFACE(27)
	FEEDFACE(28)
	STOREIT($29,29)
	STOREIT(k1,31)		# ra
	STOREIT(gp,32)		# Sr
	STOREIT(t7,33)		# Cause
	b	2f
L_RB3125:
	move	fp,zero		# zero fp ASAP, because bev_general and
				#  bev_utlbmiss believe fp contains an
				#  exception vector
	jal	clr_lance_buffer
	b	4f

L_M2000: 
	move	fp,zero		# zero fp ASAP, because bev_general and
				#  bev_utlbmiss believe fp contains an
				#  exception vector
4:	li	a0,0
	jal	__set_leds
	DELAY(300000)

	la	a0,K1BASE | CPU_CR_M2000
1:
	lw	a1,(a0)
	and	a1,(1<<13)
	beq	a1,zero,1b	# wait for VME RESETB to go high
	j	3f

L_M6000:
	move	fp,zero		# zero fp ASAP, because bev_general and
				#  bev_utlbmiss believe fp contains an
				#  exception vector
	li	a0,0xffffffff	# clear external interrupt bits
	sw	a0,CSR_IVECTCLR(zero)
	.set	noreorder
	li	a0,0xFF
/*	
 * "mtc0 a0,C0_PIDMASK"  must be executed in branch delay slot to avoid
 * bug with this instruction when executed non-cacheably in 3.1 CPU chip.
 */
	j	1f			
	mtc0	a0,C0_PIDMASK		# init PID_MASK
	nop
1:
	li	a0,C0_ERROR_IMASK|C0_ERROR_MASK
	mtc0	a0,C0_ERROR		# init ERROR reg to ignore parity errors
/*	
 * "mtc0 a0,C0_PID"  must be executed in branch delay slot to avoid
 * bug with this instruction when executed non-cacheably in 3.1 CPU chip.
 */
 	j	1f
	mtc0	zero,C0_PID		# and init PID (after PID_MASK settles)
	nop
1:	
	.set	reorder
	move	s0,zero			# initialize the "callee saved"
	move	s1,zero			#  registers to init parity, because
	move	s2,zero			#   these registers are likely to be
	move	s3,zero			#    first used in a way that will
	move	s4,zero			#     cause a parity error
	move	s5,zero
	move	s6,zero
	move	s7,zero
	li	a0,0x3F			# light all the LEDs to show we're alive
	jal	__set_leds
	jal	init_cache		# must clear parity errors prior
					#  to initmem() and flush_cache()
	/* and fall into the general Mbox case ... */

L_M500: ; L_M800: ; L_M1000:
	#
	# unfortunately the above machines cannot write the regs to mem
	#	because mem can only be written after mungo amounts of
	#	code have been executed thereby trashing the regs we want to 
	#	save
	#
	move	fp,zero		# zero fp ASAP, because bev_general and
				#  bev_utlbmiss believe fp contains an
				#  exception vector
2:
	li	a0,0
	jal	__set_leds
	DELAY(300000)
	/*
	 * At power up, the timers come up in an unknown state and therefore
	 * timer interrupts may be pending.  The following stops the timers and
	 * clear any timer interrupt.
	 */
3:
	MACHINE_SPECIFIC
L_M6000: ; L_M20:
	b	2f			# nothing needed here
L_M120: ; L_M500: ; L_M800: ; L_M1000: ; L_M2000: ; L_M180: ; L_RB3125:
#ifndef SABLE
	sll	a1,v0,2			# machine type index
	lw	a0,PT_CLOCK_ADDR(a1)	# get addr of timer base addr
	or	a0,K1BASE+0xc		# offset to K1 and control register
	li	v0,CTCW_SC(2)|CTCW_16B|CTCW_MODE(MODE_STS)
	sb	v0,(a0)			# stop timer 2
	WBFLUSH
	li	v0,CTCW_SC(1)|CTCW_16B|CTCW_MODE(MODE_STS)
	sb	v0,(a0)			# stop timer 1
	WBFLUSH
	li	v0,CTCW_SC(0)|CTCW_16B|CTCW_MODE(MODE_STS)
	sb	v0,(a0)			# stop timer 0
	WBFLUSH
#endif !SABLE
	.set	noreorder
	mfc0	v0,C0_CAUSE
	nop
	and	v1,v0,CAUSE_IP7		# timer 1 interrupt pending?
	.set	reorder
	beq	v1,zero,1f		# no
	lw	a0,TIM1_ACK_ADDR(a1)	# machine specific timer 1 ack addr
	or	a0,K1BASE		# offset to K1
	lb	zero,(a0)		# acknowledge timer 1
1:	and	v1,v0,CAUSE_IP5		# timer 0 interrupt pending?
	beq	v1,zero,1f		# no
	lw	a0,TIM0_ACK_ADDR(a1)	# machine specific timer 1 ack addr
	or	a0,K1BASE		# offset to K1
	lb	zero,(a0)		# acknowledge timer 0
2:
1:
	jal	check_cpuid		# check to see cpuid in correct range
	jal	set_gp			# set the gp registers

 #
 #	stuff EPC and ra into nvram
 #
	NVRAM(NVADDR_RESETEPC)
	sb	k0,(v1)
	WBFLUSH
	srl	k0,8
	sb	k0,4(v1)
	WBFLUSH
	srl	k0,8
	sb	k0,8(v1)
	WBFLUSH
	srl	k0,8
	sb	k0,12(v1)
	WBFLUSH

	/* NVRAM(NVADDR_RESETRA) */
	/* !!! assume that resetra follows resetepc !!! */
	sb	k1,16(v1)
	WBFLUSH
	srl	k1,8
	sb	k1,20(v1)
	WBFLUSH
	srl	k1,8
	sb	k1,24(v1)
	WBFLUSH
	srl	k1,8
	sb	k1,28(v1)
	WBFLUSH

	MACHINE_SPECIFIC
L_M6000:
	jal	__ctlspace_init_r6000 	# init CtlSpace BrdAddress'es, etc.
	jal	__lmem_reset_r6000	# reset memory refresh cntrs
	jal	lmem_config		# configure memory
	li	a0,MEMCFG_PATTERN	# mem config done
	jal	__set_leds
	b	7f
L_M120: ; L_M180: ; L_M20:
	li	v1,1			# Non mem config at this point
	sw	zero,scsi_first		# allow spc, udc initialization
					# even if bootmode 'd'
	b	6f
L_M500: ; L_M800: ; L_M1000:		# all mboxes
	jal	__lmem_reset		# reset local memory refresh cntrs
	jal	lmem_config		# configure local memory
	lbu	v1,CPU_CONFIG+K1BASE
	and	v1,CONFIG_VMEMEM
	b	6f
L_M2000: ; L_RB3125:
	jal	__lmem_reset_m2000	# reset local memory refresh cntrs
	jal	lmem_config		# configure local memory
	srl	v1,6
	.set	noreorder
	mtc0	zero,C0_INX
	mtc0	v1,C0_TLBHI
	nop
	lw	v1,CPU_CR_M2000+K1BASE
	nop
	and	v1,CR_LMEMSELB
	.set	reorder
6:
	bne	v1,zero,2f		# not running via local memory
	bne	v0,zero,2f		# some local memory is there
	LED_MACHDEP(LMEM_NOMEM_PATTERN)	# No Memory!
1:	b	1b
2:	LED_MACHDEP(MEMCFG_PATTERN)	# mem config done

	#
	# save caches to memory (debugging maneuver) at 3mg (dcache first)
	#
	jal	swapisolate
7:
	#
	# Perform any initialization PON may require.
	#
	jal     Pon_Init

	MACHINE_SPECIFIC
L_M500: ; L_M800: ; L_M1000: 		# all mboxes
	lbu	v0,CPU_CONFIG+K1BASE
	and	v0,CONFIG_POWERUP
	bne	v0,zero,init_sys	# power-up or on-board reset
	b	6f
L_M120: ; L_M180:
	lh	v0,SCR+K1BASE
	and	v0,SCR_COLDSTART
	bne	v0,zero,init_sys	# power-up or on-board reset
	b	6f
L_M2000: ; L_RB3125:
	lw	v0,CPU_CR_M2000+K1BASE
	and	v0,CR_COLDSTART
	bne	v0,zero,init_sys	# power-up or on-board reset
	b	6f
L_M20:
L_M6000:
	j	init_sys		# XXX assume CONFIG_POWERUP coldstart
6:
	CALCULATE_SP(sp,(4*4))		# reset stack
	SW_GLOBAL(sp,_fault_sp,v1)	# store sp away
	subu	sp,EXSTKSZ		# top piece of stack for fault handling
	NVRAM(NVADDR_BOOTMODE)
	bne	v0,CHAR_w,reboot1	# warm start not requested
	/*
	 * warm start code
	 */
	LED_SA(WARMST_PATTERN)
	jal	warm_start		# returns if bad restart block

reboot1:
	bne     v0,CHAR_d,init_sys      # if NOT bootmode 'd', run pon diags
	jal	init_env		# initialize environment
reboot:
	NVRAM(NVADDR_BOOTMODE)
	bne     v0,CHAR_d,no_pon        # if NOT bootmode 'd', clear memory
	jal	config_cache		# determine cache sizes
	jal	flush_cache		# flush cache
	LED_SA(CACHE_PATTERN)
	jal	_hook_exceptions
	.set	noreorder
	mtc0	zero,C0_SR		# knock down the BEV bit
	.set	reorder			# so badaddr() will work
	jal	early_init
	jal	clear_restart		# disable warm starts on image
	jal	_config_delay
	jal	_init_saio		# must have machine_type and memory
	LED_SA(SAIO_PATTERN)

	/*
	 * Now let bus error interrupts come in
	 * (first clear any old ones that may be pending)
	 */
	MACHINE_SPECIFIC
L_M500: ; L_M800: ; L_M1000: 
	lw	zero,SBE_ADDR+K1BASE
	b	1f
L_M2000: ; L_RB3125:
	.set noreorder
	lw	zero,SBE_ADDR+K1BASE
	sw	zero,SBE_ADDR+K1BASE
	.set reorder
	b	1f
L_M120: ; L_M180:
	lw	zero,FAR+K1BASE
1:
	li	v0,PROM_SR		# allow bus error interrupts only
	
L_M20:
	b	2f
L_M6000:
	move	v0,zero			# PROM_SR for 6000
2:
	.set	noreorder
	mtc0	v0,C0_SR
	.set	reorder

	NVRAM(NVADDR_FLAG)
 	and	v0, ~NVFLAG_RSTMASK	# zero the reset counter
	sb	v0, (v1)
	WBFLUSH

	jal	main			# enter monitor
	li	a0,0
	jal	exit

init_sys:
	NVRAM(NVADDR_BOOTMODE)
	move    k0,v0                   # save bootmode character

	MACHINE_SPECIFIC
L_M120: ; L_M180:
	beq     k0,CHAR_d,no_pon        # if NOT bootmode 'd', clear memory

	lh	v0,SCR+K1BASE
	and	v0,SCR_COLDSTART
	beq	v0,zero,no_pon		# if we're doing a WARM start...
	b 	pon_start	
L_M2000: ; L_RB3125:			# NVRAM_BOOTMODE which is another
	beq     k0,CHAR_d,no_pon        # if NOT bootmode 'd', clear memory

	lw	v0,CPU_CR_M2000+K1BASE
	and	v0,CR_COLDSTART

	beq	v0,zero,no_pon		# if we're doing a WARM start...
	b	pon_start		#  otherwise, run the Pons
L_M6000:
	.set	noreorder
	li	a0,C0_ERROR_IMASK|C0_ERROR_MASK
	mtc0	a0,C0_ERROR		# clear any pending parity errors
	.set	reorder

	bne	k0,CHAR_d,pon_start	# if NOT bootmode 'd', run pons

	li	sp,BRDTYPE_R6300	# save machine type in SP for PON
#ifndef	R3030
	jal	Pon_Duart		# initialize duart
#endif
	b	no_pon
L_M20:
	bne	k0,CHAR_d,pon_start	# if NOT bootmode 'd', run pons
	b	no_pon
L_M500: ; L_M800: ; L_M1000: 		# all mboxes
	jal	__lmem_reset		# reset local memory refresh cntrs
	beq     k0,CHAR_d,no_pon        # if NOT bootmode 'd', clear memory

	lbu	v0,CPU_CONFIG+K1BASE
	and	v0,CONFIG_POWERUP
	beq	v0,zero,no_pon		# if we're doing a WARM start...
	
	/*
	 * Jump to Pon_Start to execute PON diagnostics.  PON will jump back
	 * to no_pon when completed.
	 */
pon_start:
	beq     k0,CHAR_e,no_pon        # if bootmode 'e', skip PON

	j	Pon_Diags

XLEAF(no_pon)
	jal	initmem			# initializes sp
					# and de-cache the 'sp' if VM was run
	NVRAM(NVADDR_BOOTMODE)	# check bootmode to determine if clear memory 
	bne	v0,CHAR_d,3f
	sw	v0,noclmem
3:
	jal	initialize
	jal	setup_fp	# initialize the fp chip 

	/*
	 * KLUDGE ALERT: There is a problem between the CPU and FPU.  On
	 * a cold reset (or power up), they may not be in sync.  To insure
	 * that they are, a warm reset needs to be done.  However, since it
	 * will reset the CPU (and therefore reboot the proms), we must do it 
	 * now rather than before to insure that pons are executed and memory
	 * is cleared.
	 */
	MACHINE_SPECIFIC
L_M6000:
	li	a0,C0_ERROR_MASK	# clear any pending parity errors
	or	a0,C0_ERROR_IEXT	# XXX ignore External
	.set	noreorder
	mtc0	a0,C0_ERROR		#  and enable parity error detection
	.set	reorder
L_M120: ; L_M500: ; L_M800: ; L_M1000: ; L_M180: ; L_M20:
	b	checkauto
L_M2000: ; L_RB3125:			# NVRAM_BOOTMODE which is another
	lw	v0,CPU_CR_M2000+K1BASE
	and	v0,CR_COLDSTART
	beq	v0,zero,checkauto	# 1 indicates coldstart
	li	v0,CON_SWWARMRESET
	sb	v0,CPU_CON_M2000|K1BASE	# set reset bit in config register
	/*
	 * Shouldn't get here
	 */
3:
	LED_MACHDEP(0x3f)
	DELAY(1000)
	LED_MACHDEP(0)
	DELAY(1000)
	b	3b

checkauto:

	NVRAM(NVADDR_FLAG)
 	and	v0, ~NVFLAG_RSTMASK	# zero the reset counter
	sb	v0, (v1)
	WBFLUSH

	/*
	 * check for autoboot
	 */
#ifdef R3030
	jal	PonStatus
#endif
	NVRAM(NVADDR_BOOTMODE)
	beq	v0,CHAR_c,_autoboot		# autoboot system
	beq	v0,CHAR_w,_autoboot		# if warm start failed, autoboot
	jal	main
	li	a0,0
	jal	exit
	END(start)

/*
 * _init -- reinitialize monitor command entry pt
 */
LEAF(_init)
	.set	noreorder
	mtc0	zero,C0_SR
	.set	reorder
	lw	s0,memsize		# save memsize in mem_is_sized
	jal	initmem			# resets SP, too
	sw	s0,mem_is_sized
	jal	initialize
	jal	main
	li	a0,0
	jal	exit			# shouldn't get here

autoboot1:
	.set	noreorder
	mtc0	zero,C0_SR
	.set	reorder
	jal	initmem			# initmem initializes sp
	jal	initialize
_autoboot:
	j	autoboot
	END(_init)


/*
 * _exit()
 *
 * Re-enter prom command loop without clearing memory.
 * Prom stack will be reset and bss area cleared.    
 * In the case of Rx3230, screen parameters will be preserved.
 *
 */
LEAF(_exit)
	.set	noreorder
	mtc0	zero,C0_SR		# back to a known sr
	mtc0	zero,C0_CAUSE		# clear software interrupts
	.set	reorder
        lw      s0,memsize              # save memsize in mem_is_sized
	lw	s6,total_memsize
	lw	s7,memtop		# save memtop
#ifdef R3030
	lw	s1,gs_rows		# save screen parameters 
	lw 	s2,gs_cols
	lw	s3,gs_curx
	lw	s4,gs_cury
	lw	s5,gs_bufaddr
#endif
	jal	initmem			# init prom stack & zero out bss area
        sw      s0,memsize		# restore memsize
        sw      s0,mem_is_sized
	sw	s6,total_memsize
	sw	s7,memtop		# restore memtop
#ifdef R3030
	sw	s1,gs_rows		# restore screen parameters 
	sw 	s2,gs_cols
	sw	s3,gs_curx
	sw	s4,gs_cury
	sw	s5,gs_bufaddr
#endif

	jal	_hook_exceptions
	jal	clear_restart		# disable warm starts on image
	jal	_init_malloc_prom	
        jal     init_env        	# initialize environment
	jal	_init_saio
	MACHINE_SPECIFIC
L_M500: ; L_M800: ; L_M1000:
	/*
	 * Now let bus error interrupts come in
	 * (first clear any old ones that may be pending)
	 */
	lw	zero,SBE_ADDR+K1BASE
	b	1f
L_M2000: ; L_RB3125:
	.set	noreorder
	lw	zero,SBE_ADDR+K1BASE
	sw	zero,SBE_ADDR+K1BASE
	.set	reorder
L_M120: ; L_M180: ; L_M20:
1:
	li	v0,PROM_SR		# allow bus error interrupts only
	b	2f
L_M6000:
	sw	zero,CSR_IVECTMASK(zero)  # disable external interrupts
	.set	noreorder
	mfc0	a0,C0_ERROR		# retrieve current ERROR reg value
	nop				# (LDSLOT)
	mtc0	a0,C0_ERROR		# rewrite to clear pending errs
	.set	reorder
	move	v0,zero			# PROM_SR for the 6000
2:
	.set	noreorder
	mtc0	v0,C0_SR
	.set	reorder
	jal	main
	li	a0,0
	jal	exit			# shouldn't get here
	END(_exit)

EXECFRM=(4*4)+4
LEAF(_promexec)
	SW_GLOBAL(sp,exec_sp,v1)
	CALCULATE_SP(sp, EXECFRM)
	sw	ra,EXECFRM-4(sp)	# save users ra
	jal	promexec
	lw	ra,EXECFRM-4(sp)	# restore users ra
	LW_GLOBAL(sp,exec_sp,v1)	# restore users sp
	j	ra
	END(_promexec)

/*
 * initmem -- config and init cache, clear prom bss
 * clears memory between 0 and PROM_STACK-4 inclusive
 */
STK_RESERVED=(4*4)			/* reserve top four words */
INITMEMFRM=STK_RESERVED+4
NESTED(initmem, INITMEMFRM, zero)
	CALCULATE_SP(v1,STK_RESERVED)
	and	v0,v1,~(PROM_STACK_SIZE-1)	# first word to init
	add	t2,v1,STK_RESERVED-4		# last word to init
 	.set	noreorder
1:	sw	zero,0(v0)		# zero phys mem
 	bne	v0,t2,1b
 	add	v0,4			# BDSLOT
 	.set	reorder

	/*
	 * Initialize stack
	 */
	move	sp,v1
	SW_GLOBAL(sp,_fault_sp,v0)
	subu	sp,EXSTKSZ		# top piece of stack for fault handling
	subu	sp,INITMEMFRM
	sw	ra,INITMEMFRM-4(sp)
	jal	get_machine_type
	sw	v0,machine_type		# everyone loves "machine_type"
	jal	_config_delay		# C routines use "delay_mult"
	LED_SA(ZEROBSS_PATTERN)
	jal	config_cache		# determine cache sizes
	jal	flush_cache		# flush cache
	LED_SA(CACHE2_PATTERN)
	lw	ra,INITMEMFRM-4(sp)
	addu	sp,INITMEMFRM
	j	ra
	END(initmem)

/*
 * initialize -- initialize prom state
 */
INITFRM=(4*4)+(2*4)
NESTED(initialize, INITFRM, zero)
	subu	sp,INITFRM
	sw	ra,INITFRM-4(sp)
	jal	_hook_exceptions
	jal	early_init	# initialize enough for error msgs
	lw	v0,machine_type
	beq	v0,BRDTYPE_M180,1f
	bne	v0,BRDTYPE_R2400,2f
1:	NVRAM(NVADDR_MEMPARITY)	# remember if we want parity enabled for M/120
	sw	v0,INITFRM-8(sp)
2:

	/*
	 * Moved clear_memory from below _init_saio to here since memory
	 * size is needed by _init_malloc_prom BEFORE we call _init_saio
	 */
	MACHINE_SPECIFIC
L_M6000:
	sw	zero,CSR_IVECTMASK(zero)	# disable extern ints
	move	v0,zero		# PROM_SR for the 6000
	b	2f
L_M120: ; L_M180:
	lw	zero,FAR+K1BASE
	lw	v0,INITFRM-8(sp) # Nvram MEMPARITY value
	beq	v0,CHAR_0,1f
	lhu	v0,SCR+K1BASE
	or	v0,SCR_PAREN	# turn on parity if memparity env var not '0'
	sh	v0,SCR+K1BASE
L_M20:
L_M500: ; L_M800: ; L_M1000: ; L_M2000: ; L_RB3125: # all mboxes
1:
	li	v0,PROM_SR	# allow bus error interrupts only
2:
	.set	noreorder
	mtc0	v0,C0_SR
	.set	reorder
	jal	clear_memory	# clear memory and tlb

	sw	v0,memsize	# size of first no-holey chunk of memory
#ifndef MULTI
	sw	v0,memtop	# for promexec
#endif
	LED_SA(ZEROMEM_PATTERN)
	jal	_init_malloc_prom	# set up prom malloc region
	/*
	 * Must initialize environment before devices, because some devices
	 * may be parameterized by environment
	 */
	jal	init_env	# initialize environment
	LED_SA(ENV_PATTERN)
#ifdef R3030
	jal	_videoreset
#endif
	jal	_init_saio	# initialize saio library
	LED_SA(SAIO2_PATTERN)

	MACHINE_SPECIFIC
L_M500: ; L_M800: ; L_M1000:
	/*
	 * Now let bus error interrupts come in
	 * (first clear any old ones that may be pending)
	 */
	lw	zero,SBE_ADDR+K1BASE
	b	1f
L_M2000: ; L_RB3125:
	.set noreorder
	lw	zero,SBE_ADDR+K1BASE
	sw	zero,SBE_ADDR+K1BASE
	.set reorder
	b	1f
L_M6000:
L_M20:
	/* XXX do anything special here? */
	b	1f
L_M120: ; L_M180:
	la	v0,(0xa0000000 | FAR)
	lw	zero,0(v0)
1:
#ifdef MULTI
	L_CPUID(v1)		# load cpuid
	bne	v1,zero,1f	# only cpu_zero inits memory
	.set	noreorder
	li	v0,PROM_SR	# allow bus error interrupts only
	mtc0	v0,C0_SR
	.set	reorder
	jal	clear_memory	# clear memory and tlb
	# we only want one memsize even on MULTI
	sw	v0,memsize
	DELAY(100000)		# wait for other cpus to read memsize
	b	2f


1:
	L_CPUID(t0)		# load cpuid
	sll	v1,t0,0x2	# 4 words, one for each cpu
	sw	t0,cpu_present(v1)
	DELAY(20000)		# wait for other cpus
2:
	li	v1,0		# now calculate who's there, cpu0 is always
	la	a1,cpu_present	# base address
	#
	# load the cpu_present entry for every non-cpu0 cpu and make it's
	#	entry(it's cpuid), the maxcpu. This way we don't worry
	#	about holes, overlaps are tougher and we don't worry
	#	about them.
	#
	lw	a0,4(a1)	# cpu1
	beq	a0,zero,1f
	move	v1,a0
1:
	lw	a0,8(a1)	# cpu2
	beq	a0,zero,1f
	move	v1,a0
1:
	lw	a0,12(a1)	# cpu3
	beq	a0,zero,1f
	move	v1,a0
1:
	addi	v1,1
	sw	v1,maxcpu	# store how max cpu no. there is

	# calculate:  
	#	promspace = maxcpu*PROM_STACK_SIZE
	#	memtop[cpuid] = (((memsize-promspace)/maxcpu)*cpuid)+promspace
	#
	sll	a0,v1,PROM_STACK_SIZE_SHIFT # promspace=maxcpu*PROMSTACKSIZE
	lw	v0,memsize
	subu	v0,a0			# memsize-promspace
	div	v0,v0,v1		#	/maxcpu
	L_CPUID(t0)			# load cpuid
	mul	v0,v0,t0		#	*cpuid
	addu	v0,a0			#	+promspace
	sw	a0,memtop		# store unshared
#endif
	li	a0,ZEROMEM_PATTERN
	jal	__set_leds
	lw	ra,INITFRM-4(sp)
	addu	sp,INITFRM
	j	ra

	END(initialize)

/*
 * notimplemented -- deal with calls to unimplemented prom services
 */
NOTIMPFRM=(4*4)+4
NESTED(notimplemented, NOTIMPFRM, zero)
	subu	sp,NOTIMPFRM
	la	a0,notimp_msg
	jal	printf
	li	a0,1
	jal	exit
	END(notimplemented)

notimp_msg:
	.asciiz	"ERROR: call to unimplemented prom routine\n"

	.align	4

/*
 * invaltlb(i): Invalidate the ith ITLB entry.
 * called whenever a specific TLB entry needs to be invalidated.
 */
LEAF(invaltlb)
	.set	noreorder
	li	t2,K0BASE&TLBHI_VPNMASK
	mtc0	t2,C0_TLBHI		# invalidate entry
	mtc0	zero,C0_TLBLO
	sll	a0,TLBINX_INXSHIFT
	mtc0	a0,C0_INX
	nop
	c0	C0_WRITEI
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(invaltlb)

/*
 * Read-modify-write cycle routines for Mbox'es
 * RMW_TOGGLE when read causes read-modify-write cycle with immediately
 * following read and write cycles
 *
 * a0	target address
 * a1	mask to AND or OR
 * a2	0 == AND   1 == OR
 * a3	1 == byte  2 == halfword  4 == word
 */
LEAF(generic_rmw_std)
	.set	noreorder
	la	t2,rmw_tmp              # address for first pass RMW_TOGGLE
        la      t3,rmw_tmp              # address for first pass RMW location
        mfc0    v1,C0_SR                # save sr
        nop                             # (probably don't need this, but...)
        mtc0    zero,C0_SR              # interrupts off
        la      v0,1f
        and     v0,0x9fffffff           # convert to k0seg address
        j       v0                      # run cached
        nop                             # BDSLOT
1:
	WBFLUSH
	nop				# BD slot
	lb	v0,0(t2)		# hit RMW toggle
	/*
	 *  Read the target address using the appropriate width
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	lb	v0,0(t3)		# (BDSLOT) Read 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	lh	v0,0(t3)		# (BDSLOT) Read 16-bit location
4:	lw	v0,0(t3)		# Read 32-bit location
9:
	/*
	 *  Perform the appropriate action:  AND or OR
	 */
	bne	a2,zero,5f		# AND or OR?
	or	t0,a1,v0		# (BDSLOT)   Modify with OR
	and	t0,a1,v0		# no Branch: Modify with AND
5:
	/*
	 *  Write the target address using the appropriate width
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	sb	t0,0(t3)		# (BDSLOT) Write 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	sh	t0,0(t3)		# (BDSLOT) Write 16-bit location
4:	sw	t0,0(t3)		# Write 32-bit location
9:
	WBFLUSH
	nop				# BD slot
	beq	t3,a0,3f		# RMW done to target
	li	t2,RMW_TOGGLE		# BD slot: Real RMW_TOGGLE
	b	1b			# do it for real from cache
	move	t3,a0			# BD slot: Real RMW target

3:	mtc0	v1,C0_SR		# restore sr
	j	ra			# return and go uncached again
	nop
	.set	reorder
	END(generic_rmw_std)

#ifndef R3030
/*
 *  build_gbamap_rmw (rmw_addr, gbamap_base)
 *
 *  For the specified VME address space location (a0), resets its GbaMap entry
 *  to include the Lock bit and to change the OpCode to RMW.
 *  Returns the address of the GbaMap entry in v0, and the original contents
 *  in v1 (so the caller can restore the entry).
 *  The caller is expected to disable interrupts during the time the GbaMap
 *  entry is thus altered.
 *
 *  Uses v0, v1, t0
 */
LEAF(build_gbamap_rmw)
	and	v0,a0,GBA_SPAN-1	# form the addr of GbaMap entry
	srl	v0,VPNSHIFT_R6000	#  using the addr of the Map base
	sll	v0,2			#   and adding
	add	v0,a1			#    the page offset*4
	lw	v1,(v0)			# return original contents
	and	t0,v1,~GBA_OPCODE_MASK
	or	t0,(GBA_OPCODE_RMW | LOCK_IOA)
	sw	t0,(v0)			# add Lock and OpCode-RMW
	j	ra
	END(build_gbamap_rmw)

/*
 * Read-modify-write cycle routines for R6000
 * RMW_TOGGLE when read causes read-modify-write cycle with immediately
 * following read and write cycles
 *
 * a0	target address
 * a1	mask to AND or OR
 * a2	0 == AND   1 == OR
 * a3	1 == byte  2 == halfword  4 == word
 */
LEAF(generic_rmw_r6000)
	move	t9,ra			# save ra
	jal	get_ioa_ctlspace_addr	# what is CtlSpace for this IOA?
	beq	v0,zero,99f		# if not an IOA, then do in-line
	move	t8,v0			# save CtlSpace base address
	move	t7,a1			# save mask value
	li	t6,GBA0_MISC_BASE
	add	a1,v0,GBA0_MAP		# and pass GbaMap base as arg2
	beq	v1,zero,1f
	li	t6,GBA1_MISC_BASE
	add	a1,v0,GBA1_MAP
	.set	noreorder
1:	mfc0	t5,C0_SR		# save current SR
	jal	build_gbamap_rmw	# build an appropriate gbamap entry
	mtc0	zero,C0_SR		# (BDSLOT) disable interrupts
	/*
	 *  Read the target address using the appropriate width,
	 *  which also Locks the VMEbus.
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	lb	t0,0(a0)		# (BDSLOT) Read 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	lh	t0,0(a0)		# (BDSLOT) Read 16-bit location
4:	lw	t0,0(a0)		# Read 32-bit location
9:
	/*
	 *  Perform the appropriate action:  AND or OR
	 */
	bne	a2,zero,1f		# AND or OR?
	or	t2,t0,t7		# (BDSLOT)   Modify with OR
	and	t2,t0,t7		# no Branch: Modify with AND
1:
	/*
	 *  Write the target address using the appropriate width
	 */
	add	t6,t8			# form GBA_MISC address
	and	t4,a0,3			#  and merge the original low-order
	or	t6,t4			#   byte-offset-in-word bits
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	sb	t2,GBA_RMW_UNLOCK(t6)	# (BDSLOT) Write 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	sh	t2,GBA_RMW_UNLOCK(t6)	# (BDSLOT) Write 16-bit location
4:	sw	t2,GBA_RMW_UNLOCK(t6)	# Write 32-bit location
9:
	/*
	 *  Restore original GbaMap entry, and clear IOC BusyReset
	 */
	sw	v1,(v0)			# restore original GbaMap entry
	li	t1,IOC_BUSY_RESET
	sw	t1,IOA_ERRORINFO_REG(t8)

	j	t9			# return
	mtc0	t5,C0_SR		# (BDSLOT) restore sr

99:
	/*
	 *  Not a GBA address, so perform the simple load/modify/store.
	 *
	 *  Read the target address using the appropriate width,
	 *  which also Locks the VMEbus.
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	lb	t0,0(a0)		# (BDSLOT) Read 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	lh	t0,0(a0)		# (BDSLOT) Read 16-bit location
4:	lw	t0,0(a0)		# Read 32-bit location
9:
	/*
	 *  Perform the appropriate action:  AND or OR
	 */
	bne	a2,zero,1f		# AND or OR?
	or	t2,t0,a1		# (BDSLOT)   Modify with OR
	and	t2,t0,a1		# no Branch: Modify with AND
1:
	/*
	 *  Write the target address using the appropriate width
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	sb	t2,(a0)			# (BDSLOT) Write 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	sh	t2,(a0)			# (BDSLOT) Write 16-bit location
4:	sw	t2,(a0)			# Write 32-bit location
9:
	j	t9			# exit
	nop				# (BDSLOT)
	.set	reorder
	END(generic_rmw_r6000)
#endif	!R3030

LEAF(orw_rmw)
	li	a2,1			# OR
	li	a3,4			# 4-byte location
#ifndef	R3030
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,generic_rmw_r6000
#endif
	j	generic_rmw_std		# do it
	END(orw_rmw)

LEAF(orh_rmw)
	li	a2,1			# OR
	li	a3,2			# 2-byte location
#ifndef	R3030
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,generic_rmw_r6000
#endif
	j	generic_rmw_std		# do it
	END(orh_rmw)

LEAF(orb_rmw)
	li	a2,1			# OR
	li	a3,1			# 1-byte location
#ifndef	R3030
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,generic_rmw_r6000
#endif
	j	generic_rmw_std		# do it
	END(orb_rmw)

LEAF(andw_rmw)
	li	a2,0			# AND
	li	a3,4			# 4-byte location
#ifndef R3030
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,generic_rmw_r6000
#endif
	j	generic_rmw_std		# do it
	END(andw_rmw)

LEAF(andh_rmw)
	li	a2,0			# AND
	li	a3,2			# 2-byte location
#ifndef R3030
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,generic_rmw_r6000
#endif
	j	generic_rmw_std		# do it
	END(andh_rmw)

LEAF(andb_rmw)
	li	a2,0			# AND
	li	a3,1			# 1-byte location
#ifndef	R3030
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,generic_rmw_r6000
#endif
	j	generic_rmw_std		# do it
	END(andb_rmw)

LEAF(check_cpuid)
	L_CPUID(v0)			# load cpuid
	blt	v0,zero,1f		# if cpuid < '0' reset it
	li	v1,3
	bgt	v0,v1,1f		# if cpuid > '3' reset it
	j	ra
1:
	NVRAM(NVADDR_CPUID)
	li	v0,CHAR_0
	sb	v0,(v1)			# reset to '0'
	li	a0,CPUID_RESET_PATTERN	# loop so user knows it was reset
	jal	__set_leds
2:	b	2b
	END(check_cpuid)

LEAF(set_gp)
	L_CPUID(v0)			# load cpuid
	sll	v0,PROM_STACK_SIZE_SHIFT
	la	v1,_gp			# what gp is for cpu0
	addu	gp,v0,v1		# gp = &_gp * (cpuid * prom_stack_size)
	j	ra
	END(set_gp)


/*
 * This code sets up the FP Chip, so that the interrupt vector is
 * initialized.  Note, since we are not reading from the chip, this
 * should still work even if a chip isn't installed.
 */
LEAF(setup_fp)
	.set 	noreorder
	mfc0	v0,C0_SR	# get current SR (and save it in v0)
	li	v1,SR_CU1	# (LDSLOT)
	mtc0	v1,C0_SR	# make FPU ops street-legal
	nop			# wait 2 cycles before Cp1 can be accessed
	nop

	ctc1	zero,$31	# move 0 into the fp status register
	nop

	j	ra		# exit
	mtc0	v0,C0_SR	# (BDSLOT) restore SR, disallow Cp1 accesses
	.set 	reorder
	END(setup_fp)

/*
 *  copy_r6000_to_cacheable( start_address, end_address )
 *
 *  The R6000 cannot make cached i-fetches to BootProm, so to make this
 *  possible we copy the desired section of BootProm text to the appropriate
 *  section of the s-cache.  'start_address' is the first instruction to
 *  copy, and 'end_address' is the instruction *after* the last instruction.
 *  We copy entire s-cache lines.
 */
LEAF(copy_r6000_to_cacheable)
	.set	noreorder
	mfc0	t9,C0_SR		# save SR, and
	li	t0,SR_MM_MODE		#  enable SCACHE instructions
	or	t0,t9
	mtc0	t0,C0_SR
	.set	reorder
	/*
	 *  Form the SCACHE target address to get s-cache shared hits using K0
	 *  i-fetches.
	 *  Bits <31:25> stay unchanged, and bits <13:0> are adjusted to back
	 *  up to an s-cache line boundary.
	 */
	srl	a0,7			# reset start_address to
	sll	a0,7			#  an s-cache line boundary
1:	and	t0,a0,0xFE003FFF	# isolate <31:25>, <13:0> for data write
	srl	t1,a0,14		# r-justify pfn, and isolate the least
	li	t2,0xF			#  signif 4 bits for the index into the
	and	t1,t2			#   correct 1/16'th of the s-cache
	bne	t1,t2,2f		# are these bits 0xF?
	li	t1,7			#  yes, so change 0xF to 0x7
2:	sll	t2,t1,9			# shift those pfn lsbits into position
	srl	t3,a0,5			#  for forming the ptag write address,
	and	t3,0x1FC		#   merge in 7 msbits of byte-in-page,
	or	t3,t2			#    to form index from start of ptags
	add	t3,0x3E000		#    and index from start of s-cache
	sll	t1,14			# shift pfn lsbits into position
	or	t0,t1			#  these become <17:14> for data write
	li	t1,32			# words to move in this line
	and	t2,a0,0x1FFFC000	# isolate <28:14> for ptag contents
	srl	t2,4			#  and shift into position
	.set	noreorder
	scache	t2,(t3)			#  and write ptag to get s-cache hit
3:	inval	(a0)			# invalidate i-cache line
	lw	t2,0(a0)		# grab a source word
	lw	t3,4(a0)		#  or two
	lw	t4,8(a0)		#  well, unroll a bit more
	lw	t5,12(a0)
	scache	t2,0(t0)		# stuff into target locations
	scache	t3,4(t0)
	scache	t4,8(t0)
	scache	t5,12(t0)
	.set	reorder
	sub	t1,4			# subtract word count
	addi	a0,16			# increment source address
	addi	t0,16			#  and      target address
	bne	t1,zero,3b		# and loop back to finish s-cache line

	sltu	t0,a0,a1		# more to copy?
	bne	t0,zero,1b		# yes, keep copy another line

	.set	noreorder
	j	ra			# exit
	mtc0	t9,C0_SR		# (BDSLOT) and restore SR
	.set	reorder
	END(copy_r6000_to_cacheable)

#ifdef RC6280
/*
 *  clear_memory_r6000 (start_phys_address, byte_count)
 *
 *  Zero memory from the start_address for the byte_count size.
 *  We presume that the addresses are mapped for the range, either
 *  because the address is a K1SEG address or it's a KUseg/K2SEG range
 *  which has existing tlb entries.
 *
 *  For now, do the simple thing and just write singleword zeroes.
 *  A more complex scheme involves using the s-cache and encouraging the
 *  hardware to do blockmode writebacks to physical memory.
 */
CLEAR_MEMORY_FRM=(4*4)+(1*4)		# 4 argsaves, ra
NESTED(clear_memory_r6000, CLEAR_MEMORY_FRM, zero)
	subu	sp,CLEAR_MEMORY_FRM
	sw	ra,CLEAR_MEMORY_FRM-4*1(sp)
	sw	a0,CLEAR_MEMORY_FRM+4*0(sp)
	sw	a1,CLEAR_MEMORY_FRM+4*1(sp)

	/* allow the heart of this algorithm to execute cached */
	la	a0,1f			# first instruction
	la	a1,2f			# past last instruction
	jal	copy_r6000_to_cacheable

	/* restore a0 and a1 */
	lw	a0,CLEAR_MEMORY_FRM+4*0(sp)
	lw	a1,CLEAR_MEMORY_FRM+4*1(sp)
	add	t0,a1,a0		# ending address, plus one byte
	sub	t0,32			# loop-ending address

	la	v0,1f-K0SIZE		# flip into cached i-fetch mode
	j	v0
	.set	noreorder
	nop
1:	sw	zero,0(a0)
	sw	zero,4(a0)
	sw	zero,8(a0)
	sw	zero,12(a0)
	sw	zero,16(a0)
	sw	zero,20(a0)
	sw	zero,24(a0)
	sw	zero,28(a0)
	bne	a0,t0,1b		# done?
	addu	a0,32			# (BDSLOT) increment target
	.set	reorder

	lw	ra,CLEAR_MEMORY_FRM-4*1(sp)
	addu	sp,CLEAR_MEMORY_FRM
	j	ra
2:
	END(clear_memory_r6000)
#endif RC6280
/* 
** prom recognition message
 */
wrong_prom_msg:
	.asciiz	"\r\n>>> Fatal Error: Wrong PROM Installed <<<\r\n"
Mbox_expect_msg:
	.asciiz	"\r\n*** Expected PROM -- M500/M800/M1000\r\n"
M120_expect_msg:
	.asciiz	"\r\n*** Expected PROM -- M120/RC3240\r\n"
M2000_expect_msg:
	.asciiz	"\r\n*** Expected PROM -- M2000\r\n"
EXCALIBUR_expect_msg:
	.asciiz	"\r\n*** Expected PROM -- RC6280\r\n"
M20_expect_msg:
	.asciiz	"\r\n*** Expected PROM -- Rx3230\r\n"
GENESIS_expect_msg:
	.asciiz	"\r\n*** Expected PROM -- m2000-25\r\n"
Mbox_detect_msg:
	.asciiz	"*** Detected PROM -- M500/M800/M1000\r\n"
M120_detect_msg:
	.asciiz	"*** Detected PROM -- M120/RC3240\r\n"
M2000_detect_msg:
	.asciiz	"*** Detected PROM -- M2000\r\n"
EXCALIBUR_detect_msg:
	.asciiz	"*** Detected PROM -- RC6280\r\n"
M20_detect_msg:
	.asciiz	"*** Detected PROM -- Rx32300\r\n"
GENESIS_detect_msg:
	.asciiz	"*** Detected PROM -- m2000-25\r\n    "
UNKNOWN_detect_msg:
	.asciiz	"*** Detected PROM -- ??? Unknown\r\n"

	.align	4

/*
** This routine is used to detect the wrong prom which has been installed on
** a right machine. If a wrong prom has been dtetcted then this routine never
** get returned and the error message will showed on the console and LEDS
** infinitely, otherwise this routine will return to the caller and proceed
** the normal prom start up procedures.
*/
LEAF(prom_recognition)
	.set	reorder
	move	t9,ra			# save ra
	la	a1,_ndevices		# get start address of _ndevices[]
	jal	get_machine_type
	subu	v0,1			# get index to _ndevices
	sll	a0,v0,2
	addu	a1,a0	
	lw	v1,(a1)			# get number of devices from _ndevices
	bne	v1,zero,11f		# if v1 != 0 then goto 11f -- success

/*
** PROM RECOGNITION fail
** pre-initialize the DUART channel to allow PROM recognition msg prints
*/

	MACHINE_SPECIFIC
L_M500: ; L_M800: ; L_M1000: 		# all mboxes
L_M120: ; L_M180:
L_M6000: ; L_M2000:
	DUART0_BASE			# Setup DUART pointer
#if	!R3030
	la	a0,duart_table		# Point to DUART inialization table
#endif	!R3030
	b	2f			# go forward and pickup 1st offset byte
1:
	addu	a2,s1,v0
	addu	a0,1			# bump to initial value of current reg.
	lbu	a1,0(a0)		# Pickup initialization dat
	addu	a0,1
	sb	a1,0(a2)		# store the data
	jal	FlushWB			# Guarantee write occurs
2:
	lbu	s1,0(a0)		# Pick up register
	bne	s1,0xcd,1b		# Done?
	b	3f

L_M20: ; L_RB3125:
	SCC0_BASE			# setup SCC pointer
	addiu	v0,v0,SCC_CHAN_A
	lbu	a0,SCC_CMD(v0)
	la	a0,scc_table		# Point to SCC initialization table
	b	2f			# go forward and pickup 1st reg byte
1:
	sb	s1,SCC_CMD(v0)
	addu	a0,1			# bump to initial value of current reg.
	lbu	a1,0(a0)		# Pickup initialization dat
	addu	a0,1
	DELAY(1)			# relaxation time
	sb	a1,SCC_CMD(v0)		# store the data
	jal	FlushWB			# Guarantee write occurs
2:
	lbu	s1,0(a0)		# Pick up register
	bne	s1,0xcd,1b		# Done?

3:
	LED_MACHDEP(0xff)
	DELAY(200000)
	LED_MACHDEP(0)
	DELAY(200000)
	LED_MACHDEP(0xff)
	DELAY(200000)
	LED_MACHDEP(0)
	DELAY(200000)

	la	a0,wrong_prom_msg
	jal	pon_puts

	/* detect the wrong prom ID and show it on the consle/LEDS */
	move	v0,zero			# set index 0 to v0
4:	la	a1,_ndevices		# get start address of _ndevices[]
	sll	a0,v0,2
	addu	a1,a0	
	lw	v1,(a1)			# get number of devices from _ndevices
	bne	v1,zero,6f		# if v1 != 0 then goto 6f
	addu	v0,1
	ble	v0,BRDTYPE_RB3125-1,4b	# current available last board index
					# in device table, !!! should be updated
					# with device table last new entry.
6:	beq	v0,BRDTYPE_R2300-1,63f	# M500
	beq	v0,BRDTYPE_R2600-1,63f	# M800
	beq	v0,BRDTYPE_R2800-1,63f	# M1000
	beq	v0,BRDTYPE_R2400-1,64f	# M120
	beq	v0,BRDTYPE_R3200-1,65f	# M2000
	beq	v0,BRDTYPE_R6300-1,66f	# M6000
	beq	v0,BRDTYPE_M180-1,64f	# M180
	beq	v0,BRDTYPE_R3030-1,67f	# M20
	beq	v0,BRDTYPE_RB3125-1,68f	# GENESIS
	beq	v0,BRDTYPE_RB3125,69f	# the rest undefined boards

63:	LED_MACHDEP(0xfc)		# Mbox board index = 1, 2, 3
	la	a0,Mbox_detect_msg
	b	7f
64:	LED_MACHDEP(0xfb)		# M120 board index = 4
	la	a0,M120_detect_msg
	b	7f
65:	LED_MACHDEP(0xfa)		# M2000 board index = 5
	la	a0,M2000_detect_msg
	b	7f
66:	LED_MACHDEP(0xf9)		# M6000 board index = 6
	la	a0,EXCALIBUR_detect_msg
	b	7f
67:	LED_MACHDEP(0xf9)		# M20 board index = 7
	la	a0,M20_detect_msg
	b	7f
68:	LED_MACHDEP(0xf4)		# GENESIS board index = 11
	la	a0,GENESIS_detect_msg
	b	7f
69:	LED_MACHDEP(0xff)		# unknown machine
	la	a0,UNKNOWN_detect_msg

7:	jal	pon_puts
	la	a0,sccs_version
	jal	pon_puts
	DELAY(400000)

	/* detect the expected right prom ID and show this message */
	MACHINE_SPECIFIC
L_M500: ; L_M800: ; L_M1000: 		# all mboxes
	la	a0,Mbox_expect_msg
	j	8f
L_M120: ; L_M180:
	la	a0,M120_expect_msg
	j	8f
L_M2000:
	la	a0,M2000_expect_msg
	j	8f
L_RB3125:
	la	a0,GENESIS_expect_msg
	j	8f
L_M6000:
	la	a0,EXCALIBUR_expect_msg
	j	8f
L_M20:
	la	a0,M20_expect_msg

8:	jal	pon_puts
	j	3b

11:	j	t9			# PROM RECOGNITION success - exit

	END(prom_recognition)


#ifndef DIAGMON_START
#define DIAGMON_START		R_VEC	/* default to reset vector */
#endif !DIAGMON_START

#define DIAGMON_ID		"MIPS"	/* id word to compare */
#define DIAGMON_ID_OFFSET	0x200	/* id address offset from start */

#define Bevhndlr		fp	/* address of fault handler */

/*
 * diag_monitor() -- Diagnostics Monitor Support
 *
 * A diags monitor can be programmed into the unused space of bootprom as
 * an executable image independent of bootprom image. This routine will
 * first check for the existence of the diags monitor then a match of the
 * machine type before jumping to it. 
 *
 * The start address (DIAGMON_START) of the diags monitor must be defined 
 * either on the assembler command line in makefile or in a header file. 
 * It defaults to bootprom reset address if undefined.
 *
 * The id words in the diag monitor are located 0x200 from its start point
 * and structured as:
 *
 *		.word	"id-word"
 *		.word	machine_type		# ex. BRDTYPE_R6300 for RC6280
 *
 * Returns a 0 if diags monitor not found, a 1 if machine type mismatch.
 *
 * Uses a1,a2,a3,v0,v1 & t0.
 */
LEAF(diag_monitor)
#if (RC6280 || RB3125)
	lw	v0, _diagmon_id			# expected id word
	move	v1, zero			# default of actual id word
	la	a1, DIAGMON_START
	move	a2, Bevhndlr			# save current Bev exc handler
	.set	noreorder
	la	Bevhndlr, _diagmon_notfound	# Bev handler for reading id
	lw	v1, DIAGMON_ID_OFFSET(a1)	# read actaul id word 
						#   (may cause exception)
	move	Bevhndlr, a2			# (LDSLOT) BurError returns 
						#   here and restore original 
						#   Bev exc handler
	.set	reorder
	beq	v0, v1, 1f			# branch if id match
	move	v0, zero
	j	ra				# return 0 if no diags monitor
1:
	/*
	 * match machine type
	 */
	move	a3, ra				# save ra
	jal	get_machine_type		# get expected mach type in v0
	move	ra, a3				# restore ra

	move	v1, zero			# default of actual machine type
	la	a1, DIAGMON_START
	move	a2, Bevhndlr			# save current Bev exc handler
	.set	noreorder
	la	Bevhndlr, _diagmon_notfound	# Bev handler for reading type
	lw	v1, DIAGMON_ID_OFFSET+4(a1)	# read actaul type 
						#   (may cause exception)
	move	Bevhndlr, a2			# (LDSLOT) BurError returns 
						#   here and restore original 
						#   Bev exc handler
	.set	reorder
	beq	v0, v1, 1f			# branch if type also match
	li	v0, 1
	j	ra				# return 1 if mach type mismatch
1:
	la	v0, DIAGMON_START
	jal	v0				# execute diags monitor

	j	realstart			# not expected to get here

_diagmon_id:
	.ascii	DIAGMON_ID
	.word	0
#else
	move	v0, zero			# no diags monitor
	j	ra
#endif (RC6280 || RB3125)
	END(diag_monitor)

/*
 * _diagmon_notfound() -- exception handler for diag_monitor()
 *
 * Just skip the offending instruction. 
 * Uses t0.
 */
LEAF(_diagmon_notfound)	
	.set	noreorder
	mfc0	t0, C0_EPC
	nop
	addi	t0, 4		# return past the failing load
	j	t0
	c0	C0_RFE		# (BDSLOT)
	.set	reorder
	END(_diagmon_notfound)
