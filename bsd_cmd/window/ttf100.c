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
#ident	"$Header: ttf100.c,v 1.1.2.2 90/05/07 19:55:54 wje Exp $"

#include "ww.h"
#include "tt.h"

/*
 * Freedom 100
 */

#define G (WWM_GRP << WWC_MSHIFT)
short f100_frame[16] = {
	' ',	'J'|G,	'K'|G,	'A'|G,
	'J'|G,	'J'|G,	'B'|G,	'M'|G,
	'K'|G,	'D'|G,	'K'|G,	'O'|G,
	'C'|G,	'L'|G,	'N'|G,	'I'|G
};
extern struct tt_str *gen_AE, *gen_AS;

tt_f100()
{
	static struct tt_str ae = { "\033%", 2 };
	static struct tt_str as = { "\033$", 2 };

	if (tt_generic() < 0)
		return -1;
	tt.tt_frame = f100_frame;
	tt.tt_availmodes |= WWM_GRP;
	gen_AS = &as;
	gen_AE = &ae;
	return 0;
}
