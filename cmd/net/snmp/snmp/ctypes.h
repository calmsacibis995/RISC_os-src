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
/* $Header: ctypes.h,v 1.1.1.2 90/05/09 17:26:53 wje Exp $ */
#ifndef		_CTYPES_H_
#define		_CTYPES_H_

/*
 *	$Header: ctypes.h,v 1.1.1.2 90/05/09 17:26:53 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

typedef		long			CIntwType;

typedef		CIntwType		*CIntwPtrType;

typedef		unsigned long		CUnswType;

typedef		CUnswType      		*CUnswPtrType;

typedef		char			CCharType;

typedef		CCharType		*CCharPtrType;

typedef		unsigned char		CByteType;

typedef		CByteType		*CBytePtrType;

typedef		char			CIntbType;

typedef		CIntbType		*CIntbPtrType;

typedef		unsigned char		CUnsbType;

typedef		CUnsbType		*CUnsbPtrType;

typedef		long			CIntlType;

typedef		CIntlType		*CIntlPtrType;

typedef		unsigned long		CUnslType;

typedef		CUnslType		*CUnslPtrType;

typedef		short int		CIntsType;

typedef		CIntsType		*CIntsPtrType;

typedef		unsigned short int	CUnssType;

typedef		CUnssType		*CUnssPtrType;

typedef		int			CBoolType;

#define		TRUE			(1)
#define		FALSE			(0)

typedef		CBoolType		*CBoolPtrType;

typedef		int			CIntfType;

typedef		CIntfType		*CIntfPtrType;

typedef		unsigned int		CUnsfType;

typedef		CUnsfType		*CUnsfPtrType;

#define		CVoidType		void

#endif		/*	_CTYPES_H_	*/
