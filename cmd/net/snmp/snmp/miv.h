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
/* $Header: miv.h,v 1.1.1.2 90/05/09 17:29:44 wje Exp $ */
#ifndef		_MIV_H_
#define		_MIV_H_

/*
 *	$Header: miv.h,v 1.1.1.2 90/05/09 17:29:44 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<mix.h>
#include	<mis.h>

typedef		struct			MivStrTag {

		CUnsfType		mivStrMaxLen;
		CUnsfType		mivStrLen;
		CBytePtrType		mivStrData;

		}			MivStrType;

typedef		MivStrType		*MivStrPtrType;

MisStatusType	mivIntlRW ();
MisStatusType	mivIntlRO ();

MisStatusType	mivUnslRW ();
MisStatusType	mivUnslRO ();

MisStatusType	mivCounterRW ();
MisStatusType	mivCounterRO ();

MisStatusType	mivGuageRW ();
MisStatusType	mivGuageRO ();

MisStatusType	mivTicksRW ();
MisStatusType	mivTicksRO ();

MisStatusType	mivStringRW ();
MisStatusType	mivStringRO ();

MisStatusType	mivIPAddrRW ();
MisStatusType	mivIPAddrRO ();

MisStatusType	mivObjectIdRW ();
MisStatusType	mivObjectIdRO ();

#endif		/*	_MIV_H_	*/
