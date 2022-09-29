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
#ident	"$Header: ttwyse75.c,v 1.1.2.2 90/05/07 19:57:09 wje Exp $"

#include "ww.h"
#include "tt.h"

/*
 * Wyse-75
 */

#define G (WWM_GRP << WWC_MSHIFT)
short wyse75_frame[16] = {
	' ',	'x'|G,	'q'|G,	'm'|G,
	'x'|G,	'x'|G,	'l'|G,	't'|G,
	'q'|G,	'j'|G,	'q'|G,	'v'|G,
	'k'|G,	'u'|G,	'w'|G,	'v'|G
};

extern struct tt_str *gen_AS;
extern struct tt_str *gen_AE;

tt_wyse75()
{

	if (tt_generic() < 0)
		return -1;
	tt.tt_availmodes |= WWM_GRP;
	tt.tt_frame = wyse75_frame;
	if (gen_AS == 0) {
		gen_AS = (struct tt_str *) malloc(sizeof(struct tt_str));
		if (gen_AS != 0) {
			gen_AS->ts_str = "\033(0";
			gen_AS->ts_n = 3;
		}
	}
	if (gen_AE == 0) {
		gen_AE = (struct tt_str *) malloc(sizeof(struct tt_str));
		if (gen_AE != 0) {
			gen_AE->ts_str = "\033(B";
			gen_AE->ts_n = 3;
		}
	}
	return 0;
}
