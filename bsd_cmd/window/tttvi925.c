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
#ident	"$Header: tttvi925.c,v 1.1.2.2 90/05/07 19:56:58 wje Exp $"

#include "ww.h"
#include "tt.h"

/*
 * Televideo 925 as emulated by Microterm.
 *
 * From David Barto <sdcsvax!celerity!barto>.
 */

#define G (WWM_GRP << WWC_MSHIFT)
short tvi925_frame[16] = {
	' ',	'~'|G,	'|'|G,	'c'|G,
	'~'|G,	'~'|G,	'`'|G,	'e'|G,
	'|'|G,	'a'|G,	'|'|G,	'g'|G,
	'b'|G,	'f'|G,	'h'|G,	'd'|G
};

tt_tvi925()
{

	if (tt_generic() < 0)
		return -1;
	tt.tt_availmodes |= WWM_GRP;
	tt.tt_frame = tvi925_frame;
	return 0;
}
