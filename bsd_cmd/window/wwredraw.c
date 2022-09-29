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
#ident	"$Header: wwredraw.c,v 1.1.2.2 90/05/07 20:02:57 wje Exp $"

#include "ww.h"
#include "tt.h"

wwredraw()
{
	register i, j;
	register union ww_char *os;

	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		wwtouched[i] = WWU_TOUCHED;
		os = wwos[i];
		for (j = wwncol; --j >= 0;)
			(os++)->c_w = ' ';
	}
}
