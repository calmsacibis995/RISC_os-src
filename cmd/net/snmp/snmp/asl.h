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
/* $Header: asl.h,v 1.1.1.2 90/05/09 17:25:21 wje Exp $ */
#ifndef		_ASL_H_
#define		_ASL_H_

/*
 *	$Header: asl.h,v 1.1.1.2 90/05/09 17:25:21 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include        <asn.h>

typedef		CUnswType		AslIdType;

AslIdType	aslLanguage ();
AslIdType	aslChoice ();
AslIdType	aslAny ();
CVoidType	aslInit ();

#ifdef		INLINE

#include	<asldefs.h>

#define		aslSon(n)		aslSonDef(n)
#define		aslKind(n)		aslKindDef(n)
#define		aslMinLen(n)		aslMinLenDef(n)
#define		aslNext(n)		aslNextDef(n)

#else		/*	INLINE		*/

AsnTypeType	aslKind ();
AslIdType	aslSon ();
AslIdType	aslNext ();
AsnLengthType	aslMinLen ();

#endif		/*	INLINE		*/

#endif		/*	_ASL_H_		*/
