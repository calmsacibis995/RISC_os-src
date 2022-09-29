#ident "$Header: csu.mips.s,v 1.10 90/01/16 15:49:00 huang Exp $"
/*	%Q%	%I%	%M% */
/* $Copyright$ */

/*
 * csu.s -- standalone io library startup code
 */

#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/asm.h"
#include "prom/entrypt.h"
#include "saio/setjmp.h"
#include "saio/saio.h"

	.text

STARTFRM=	EXSTKSZ			# leave room for fault stack
NESTED(start, STARTFRM, zero)
	la	gp,_gp
	subu	v0,sp,4*4		# leave room for argsaves
	sw	v0,_fault_sp		# small stack for fault handling
	bne	a3,zero,1f		# no return
	la	t0,jb
	sw	ra,JB_PC*4(t0)		# ra and sp to get back to exec'er
	sw	sp,JB_SP*4(t0)
1:
	subu	sp,STARTFRM		# fault stack can grow to here + 16
	sw	zero,STARTFRM-4(sp)	# keep debuggers happy
	sw	a0,STARTFRM(sp)		# home args
	sw	a1,STARTFRM+4(sp)
	sw	a2,environ
	sw	a3,retflag		# return or exit flag
	sw	zero,mbufs		# init pointer to mbufs to nil
	jal	get_machine_type	# what machine are we running on?
	sw	v0,machine_type
	jal	config_cache		# sizes cache
	jal	flush_cache		# just to be sure
	jal	_hook_exceptions
	jal	_init_malloc_saio	# set up malloc region for saio
	jal	_config_delay		# set delay multiplier
	jal	_init_saio		# calls all initialization routines
	lw	a0,STARTFRM+4(sp)	# copy strings out of prom area
	la	a1,_argv_strings
	jal	_copystrings
	sw	v0,STARTFRM+4(sp)
	lw	a0,environ
	la	a1,_environ_strings
	jal	_copystrings
	sw	v0,environ
	lw	a0,STARTFRM(sp)		# reload argc, argv, environ
	lw	a1,STARTFRM+4(sp)
	lw	a2,environ
	la	a3,_dbgstart		# where to debugger starts client
	jal	_check_dbg		# check for debug request
	b	_nodbgmon

_dbgstart:				# stack has moved so resave these
	sw	a0,STARTFRM(sp)
	sw	a1,STARTFRM+4(sp)
	sw	a2,environ
_nodbgmon:
	jal	_hook_exceptions	# dbgmon has taken vectors
	lw	a0,STARTFRM(sp)		# reload argc, argv, environ
	lw	a1,STARTFRM+4(sp)
	lw	a2,environ
	jal	main
	lw	v1,retflag
	beq	v1,zero,_exit
	move	v0,a0
	jal	exit
	END(start)

LEAF(_exit)
	li	ra,+PROM_RESTART
	j	ra
	END(_exit)

	BSS(environ,4)			# environment pointer
	BSS(mbufs,4)			# base of mbufs struct
	LBSS(retflag,4)			# return or exit flag
	LBSS(jb,JB_SIZE*4)		# return jump_buf
