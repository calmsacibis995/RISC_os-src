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
#ident	"$Header: vec.c,v 1.1.1.2 90/05/09 17:34:19 wje Exp $"

/*
 *	$Header: vec.c,v 1.1.1.2 90/05/09 17:34:19 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>


#include	<ctypes.h>
#include	<debug.h>
#include	<vec.h>

#define			vecBlank(c)	\
				(((c) == (CCharType) ' ') ||	\
				((c) == (CCharType) '\t') ||	\
				((c) == (CCharType) '\n') ||	\
				((c) == (CCharType) '\r'))

CUnsfType		vecParse (vec, vlen, text)

CCharPtrType		*vec;
CUnsfType		vlen;
CCharPtrType		text;

{
	CUnsfType		k;
	CCharType		c;
	CBoolType		intext;

	k = (CUnsfType) 0;
	intext = FALSE;

	while (((c = *text) != (CCharType) 0) && (k < vlen)) {
		if (vecBlank (c)) {
			if (intext) {
				*text = (CCharType) 0;
				intext = FALSE;
			}
		}
		else if (! intext) {
			*vec++ = text;
			intext = TRUE;
			k++;
		}
		text++;
	}

	return ((c != (CUnsfType) 0) ? (CUnsfType) 0 : k);
}

