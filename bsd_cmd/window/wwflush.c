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
#ident	"$Header: wwflush.c,v 1.1.2.2 90/05/07 20:00:47 wje Exp $"

#include "ww.h"
#include "tt.h"

wwflush()
{
	if (wwcursorrow < 0 || wwcursorrow >= wwnrow
	    || wwcursorcol < 0 || wwcursorcol >= wwncol)
		(*tt.tt_move)(0, 0);
	else
		(*tt.tt_move)(wwcursorrow, wwcursorcol);
	ttflush();
}
