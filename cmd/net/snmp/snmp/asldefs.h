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
/* $Header: asldefs.h,v 1.1.1.2 90/05/09 17:25:27 wje Exp $ */
#ifndef		_ASLDEFS_H_
#define		_ASLDEFS_H_

/*
 *	$Header: asldefs.h,v 1.1.1.2 90/05/09 17:25:27 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<asl.h>
#include	<asn.h>

typedef         struct                  AslNodeTag {

                AsnTypeType          	aslNodeKind;
                CUnswType               aslNodeStuff;
                AsnLengthType		aslNodeMinLen;
		AslIdType		aslNodeNext;

                }                       AslNodeType;

typedef         AslNodeType             *AslNodePtrType;

/*
extern		AslNodeType	aslNodeTable [];

#define		aslIdToPtr(n)	\
			(& (aslNodeTbl [ ((AslIdType)(n)) ]))
*/

#define		aslIdToPtr(n)	\
			((AslNodePtrType) ((AslIdType)(n)))

#define		aslPtrToId(n)	\
			((AslIdType) ((AslNodePtrType) (n)))

#define		aslKindDef(n)	((aslIdToPtr (n))->aslNodeKind)
#define		aslMinLenDef(n)	\
		((AsnLengthType) (aslIdToPtr (n))->aslNodeMinLen)
#define		aslNextDef(n)	\
		((AslIdType) ((aslIdToPtr (n))->aslNodeNext))
#define		aslSonDef(n)	\
		((AslIdType) ((aslIdToPtr (n))->aslNodeStuff))

#endif		/*	_ASLDEFS_H_	*/
