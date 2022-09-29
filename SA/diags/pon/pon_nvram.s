#ident "$Header: pon_nvram.s,v 1.2.7.1 90/07/18 14:32:28 huang Exp $"
/* $Copyright$ */
 ##############################################################################
 #									      
 #									      
 #	%Q%	%I%	%M%						      
 #									      
 #	pon_nvram.s 
 #	power-on test of HD146818 real
 #	time clock battery backup ram (50 bytes).
 #
 #      P/N:  000000-000						      
 #      Date: 11/4/86							      
 #									      
 #      Irresponsible Engineer: Pat Harris				      
 #      Backup Engineer:	God
 #									      
 #-----------------------------------------------------------------------
 #									      
 #	Functional Description: 					      
 #
 # 	The last 50 bytes of the 64 total bytes are tested by first
 #	saving the resident value then testing the location with
 #	5's then a's. The initial resident value is then restored.
 # 									      
 #---------------------------------------------------------------------
 #									      
 #	Revision History:						      
 #									      
 #	Rev. 000 - Initial release
 #									      
 ##############################################################################

#include "machine/regdef.h"
#include "machine/asm.h"
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "pon.h"

#undef	PHYS_TO_K1(x)
#define	PHYS_TO_K1(x)	((x)|0xA0000000)	/* physical to kseg1 */
#define	RT_CLOCK_BASE	PHYS_TO_K1(0x1e010003)
#define	RT_RAM_LAST	PHYS_TO_K1(0x1e0100ff)
#define	RT_MEM_OFFSET	56	# byte offset to available ram
#define FIVES		0x55
#define ACES		0xaa

#define PRINT(x,y,z)	\
	la	a0,address; \
	jal	pon_puts;	\
	nop;		\
	move	a0,x;   \
	jal	pon_puthex; \
	nop;	\
	la	a0,expect; \
	jal	pon_puts;	\
	nop;		\
	move	a0,y;   \
	jal	pon_puthex; \
	nop;	\
	la	a0,actual; \
	jal	pon_puts;	\
	nop;		\
	move	a0,z;	\
	jal	pon_puthex;	\
	nop;	\
	la	a0,xormsg; \
	jal	pon_puts;	\
	nop;		\
	xor	a0,y,z; \
	jal	pon_puthex;	\
	nop;	\
	la	a0,CRLF; \
	jal	pon_puts;	\
	nop

#define	DELAY(x)				\
	li	t3,+(x);			\
9:	subu	t3,1;				\
	bne	zero,t3,9b


LEAF(Pon_Nvram)
	move	k0,ra
	la	Bevhndlr,PonHandler	# PON EXCEPTION handler
	li	a0,PON_NVRAM_START	# show that "we've only just begun..."
	jal	pon_set_leds		# write to the CPU LEDS

	li	v0,RT_CLOCK_BASE	# base address of 64 bytes of ram
	addu	v0,v0,RT_MEM_OFFSET
	li	v1,RT_RAM_LAST		# last byte address of 64 byte array
$32:
	li	t1, FIVES	# set expected data
	DELAY(1000)
	lbu	t0, 0(v0)	# save the data first!!
	DELAY(1000)
	sb	t1, 0(v0)	# write the test data (FIVES)
	DELAY(1000)
	lbu	t2,0(v0)	# now read it back
	bne	t1,t2,16f	# error?
	li	t1, ACES	# set expected data
	DELAY(1000)
	sb	t1, 0(v0)	# write the test data (ACES)
	DELAY(1000)
	lbu	t2,0(v0)	# now read it back
	bne	t1,t2,16f	# error?
	DELAY(1000)
	sb	t0, 0(v0)	# restore the original data!!

	addu	v0, v0, 4	# bump address to our next byte
	bleu	v0, v1, $32	# are we done yet?

 #  Now see if the "Console" character is what it should be: 'l' or 'r'
 #  ie see if the battery has failed!!
	li	v0,0xbe0100f7	# 'console' byte location
	DELAY(1000)
	lbu	t0,0(v0)	# what is the byte
	li	t1,0x6c		# the letter 'l'
	beq	t0,t1,2f	# did we read an 'l'?
	li	t1,0x72		# no, then try the letter 'r'	
	bne	t0,t1,4f	# did we read an 'r'?
2:
 # now for the second "battery good" test
	li	v0,NVRAM_DIAG_LOC	# address of DIAG byte
	DELAY(1000)
	lbu	t0,0(v0)	# what is the byte
	li	t1,FP_FAIL	# largest error code possible
	subu	t0,t1,t0	# should be positive or zero
	bgez	t0,3f		# negative implies an error
	
 # No, then the power's out!!	
4:
   	la	a0,power_fail
   	jal	pon_puts
   	la	a0,CRLF
   	jal	pon_puts
	li	a0,TOD_POWER_FAIL	# show "we're deadbeef..."
	jal	FastFlash
	li	a0,TOD_POWER_FAIL	# show "we're deadbeef..."
	jal	pon_set_leds		# write to the CPU LEDS
	li	v0,NVRAM_DIAG_LOC	# address of DIAG byte
	li	a0,TOD_POWER_FAIL		# setup our error code
	sb	a0, 0(v0)		# and write the diag location
	li	v0,NVRAM_BOOTMODE_LOC	# 'bootmode' nvram location
	lb	a0, 0(v0)		# and read it in
	li	a1,CHAR_d		# let's see if "diag" mode set
	beq	a0,a1,1f		# don't write 'e' if "diag" mode
	li	a0,CHAR_e		# setup our error char 'e'
	sb	a0, 0(v0)		# and write it out
1:
	li	v0,1			# set error return value
 	j	k0			# then just return


 #  The test has successfully completed!
3:
 #	li	a0,0			# zero out the LEDS
 #	jal	pon_set_leds		# write to the CPU LEDS
 #  	la	a0,success
 #  	jal	pon_puts
 #  	la	a0,CRLF
 #  	jal	pon_puts
	move	v0,zero			# indicate we were sucessful
 	j	k0			# and return

 # We've incurred an ERROR!!
16:
	PRINT(v0,t1,t2)
	sb	t0, 0(v0)		# restore the original data!!
   	la	a0,failure
   	jal	pon_puts
   	la	a0,CRLF
   	jal	pon_puts
	li	a0,NVRAM_FAIL		# show "we're deadbeef..."
	jal	FastFlash
	li	a0,NVRAM_FAIL		# show "we're deadbeef..."
	jal	pon_set_leds		# write to the CPU LEDS
	li	v0,NVRAM_DIAG_LOC	# address of DIAG byte
	li	a0,NVRAM_FAIL		# setup our error code
	sb	a0, 0(v0)		# and write the diag location
	li	a0,CHAR_e		# setup our error char 'e'
	li	v0,NVRAM_BOOTMODE_LOC	# 'bootmode' mvram location
	sb	a0, 0(v0)		# and write it out
1:
	li	v0,1			# set error return value
 	j	k0			# then just return

	END(Pon_Nvram)

address:
        .asciiz "Nvram Address: "
expect:
        .asciiz "  Expected: "
actual:
        .asciiz "  Actual: " 
xormsg:
        .asciiz "  Xor: " 
 #success:
 #       .asciiz "Nvram test was SUCCESSFUL..." 
power_fail:
        .asciiz "Nvram data retention test FAILED..." 
failure:
        .asciiz "Nvram test FAILED..." 
CRLF:
	.asciiz "\r\n\r"
