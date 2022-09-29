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
#ident	"$Header: ttwyse60.c,v 1.1.2.2 90/05/07 19:57:03 wje Exp $"

#include "ww.h"
#include "tt.h"

/*
 * Wyse-60
 */

#define G (WWM_GRP << WWC_MSHIFT)
short wyse60_frame[16] = {
	' ',	'6'|G,	':'|G,	'1'|G,
	'6'|G,	'6'|G,	'2'|G,	'4'|G,
	':'|G,	'5'|G,	':'|G,	'='|G,
	'3'|G,	'9'|G,	'0'|G,	'0'|G
};

extern struct tt_str *gen_AS;
extern struct tt_str *gen_AE;

tt_wyse60()
{

	if (tt_generic() < 0)
		return -1;
	tt.tt_availmodes |= WWM_GRP;
	tt.tt_frame = wyse60_frame;
	if (gen_AS == 0) {
		gen_AS = (struct tt_str *) malloc(sizeof(struct tt_str));
		if (gen_AS != 0) {
			gen_AS->ts_str = "\033H\002";
			gen_AS->ts_n = 3;
		}
	}
	if (gen_AE == 0) {
		gen_AE = (struct tt_str *) malloc(sizeof(struct tt_str));
		if (gen_AE != 0) {
			gen_AE->ts_str = "\033H\003";
			gen_AE->ts_n = 3;
		}
	}
	return 0;
}
