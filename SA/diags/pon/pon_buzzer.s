#ident "$Header: pon_buzzer.s,v 1.2.5.1 90/07/18 14:29:36 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
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

#include "mips/asm.h"
#include "mips/regdef.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "pon.h"

#define	BUZZ_DELAY		300000

		.text

/*
 * Checks the checksum in the ID PROM.
 */
LEAF(Pon_Buzzer)

		move	v0,zero			# initialize return value
#ifndef	R3030
		li	s0,BRDTYPE_R3030
		bne	s0,sp,pass		# if M20
#endif	!R3030

		li	s0,K1BASE|SYS_CREG
1:
		li	s1,CR_BUZZ_H|CR_BUZZ_L
		li	s3,4			# number of tones
3:
		sw	s1,(s0)			# turn on buzzer

		li	s2,BUZZ_DELAY
2:
		subu	s2,1			# wait a while
		bne	s2,zero,2b

		li	s2,CR_ENA_BUZZER_B	# turn off buzzer
		sw	s2,(s0)

		j	ra

END(Pon_Buzzer)
