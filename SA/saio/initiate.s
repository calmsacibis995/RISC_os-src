#ident "$Header: initiate.s,v 1.2 90/01/16 17:10:15 huang Exp $"
/*	%Q%	%I%	%M% */
/* $Copyright$ */

/*
 * initiate.s -- assembler routines for transfering to and from client code
 */

#include "machine/regdef.h"
#include "machine/asm.h"
#include "saio/setjmp.h"

/*
 * initiate(argc, argv, environ, client_pc, retflag);
 */
INITIATEFRM=(4*4)+4
NESTED(initiate, INITIATEFRM, zero)
	subu	sp,INITIATEFRM
	sw	ra,INITIATEFRM-4(sp)
	sw	a0,INITIATEFRM(sp)	# save args in argsave locs
	sw	a1,INITIATEFRM+4(sp)
	sw	a2,INITIATEFRM+8(sp)
	sw	a3,INITIATEFRM+12(sp)
	la	t0,jb			# save callee saves (execee might not)
	sw	fp,JB_FP*4(t0)
	sw	s0,JB_S0*4(t0)
	sw	s1,JB_S1*4(t0)
	sw	s2,JB_S2*4(t0)
	sw	s3,JB_S3*4(t0)
	sw	s4,JB_S4*4(t0)
	sw	s5,JB_S5*4(t0)
	sw	s6,JB_S6*4(t0)
	sw	s7,JB_S7*4(t0)
	jal	flush_cache		# make sure i cache is consistent
	lw	a0,INITIATEFRM(sp)	# reload arg0 (argc)
	lw	a1,INITIATEFRM+4(sp)	# reload arg1 (argv)
	lw	a2,INITIATEFRM+8(sp)	# reload arg2 (envp)
	lw	v0,INITIATEFRM+12(sp)	# reload arg3 (client_pc)
	lw	a3,INITIATEFRM+16(sp)	# get retflag
	jal	v0			# call client
	la	gp,_gp
	la	t0,jb			# reload callee saved
	lw	fp,JB_FP*4(t0)
	lw	s0,JB_S0*4(t0)
	lw	s1,JB_S1*4(t0)
	lw	s2,JB_S2*4(t0)
	lw	s3,JB_S3*4(t0)
	lw	s4,JB_S4*4(t0)
	lw	s5,JB_S5*4(t0)
	lw	s6,JB_S6*4(t0)
	lw	s7,JB_S7*4(t0)
	lw	ra,INITIATEFRM-4(sp)
	addu	sp,INITIATEFRM
	j	ra
	END(initiate)

	LBSS(jb, JB_SIZE*4)
