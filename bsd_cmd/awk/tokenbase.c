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
#ident	"$Header: tokenbase.c,v 1.1.2.3 90/05/07 18:06:06 wje Exp $"

#include "awk.h"
struct tok
{	char *tnm;
	int yval;
} tok[]	= {
/*
 * "ed - < tokenscript" will replace this comment with useable data
 */
};

char *tokname(n)
{
	if (n<=256 || n >= LASTTOKEN)
		n = 257;
	return(tok[n-257].tnm);
}
