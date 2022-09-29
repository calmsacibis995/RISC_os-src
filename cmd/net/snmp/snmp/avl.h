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
/* $Header: avl.h,v 1.1.1.2 90/05/09 17:26:47 wje Exp $ */
#ifndef		_AVL_H_
#define		_AVL_H_

/*
 *	$Header: avl.h,v 1.1.1.2 90/05/09 17:26:47 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<error.h>

typedef		ErrStatusType		AvlStatusType;

typedef		CUnswType		AvlIdType;

typedef		enum			AvlBalanceTag {

		avlDirBalanced,
		avlDirLeft,
		avlDirRight

		}			AvlBalanceType;

typedef		AvlBalanceType		(*AvlCmpFnType) ();

typedef		AvlStatusType		(*AvlPrintFnType) ();

typedef		CUnswType		AvlInfoType;

typedef		CByteType		AvlNameType;

typedef		AvlNameType		*AvlNamePtrType;

typedef		CUnsfType		AvlLengthType;

AvlIdType	avlNew ();
AvlIdType	avlFree ();
AvlStatusType	avlInsert ();
AvlStatusType	avlRemove ();
AvlInfoType	avlFind ();
AvlInfoType	avlCessor ();
CVoidType	avlInit ();

#endif		/*	_AVL_H_	*/
