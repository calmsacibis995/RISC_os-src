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
/* $Header: oid.h,v 1.2.1.2 90/05/09 17:31:31 wje Exp $ */
#ifndef		_OID_H_
#define		_OID_H_

/*
 *	$Header: oid.h,v 1.2.1.2 90/05/09 17:31:31 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>

typedef struct asn_enum {
    char *enum_text;
    int enum_number;
} asn_enum;

typedef struct tree {
    char *text_name;		/* Text name of this tree */
    int numeric_name;		/* Numeric name of this tree */
    struct tree *subtree[64];	/* Subtrees */
    asn_enum *enums;		/* Enum values (NULL if not an enum) */
} tree;

CIntfType		oidDecode ();
CIntfType		oidEncode ();

#endif		/*	_OID_H_		*/
