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
/* $Header: smp.h,v 1.1.1.2 90/05/09 17:32:23 wje Exp $ */
#ifndef		_SMP_H_
#define		_SMP_H_

/*
 *	$Header: smp.h,v 1.1.1.2 90/05/09 17:32:23 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<error.h>
#include	<aps.h>

typedef		CUnswType		SmpIdType;

typedef		CUnswType		SmpSocketType;

typedef		ErrStatusType		SmpStatusType;

typedef		SmpStatusType		(*SmpHandlerType) ();

typedef		SmpStatusType		(*SmpSendFnType) ();

typedef		enum			SmpErrorTag {

		smpErrorNone,
		smpErrorTooBig,
		smpErrorNoSuch,
		smpErrorBadValue,
		smpErrorReadOnly,
		smpErrorGeneric

		}			SmpErrorType;

typedef		enum			SmpCommandTag {

		smpCommandGet,
		smpCommandNext,
		smpCommandRsp,
		smpCommandSet,
		smpCommandTrap

		}			SmpCommandType;

typedef		enum			SmpKindTag {

		smpKindNone,
		smpKindInteger,
		smpKindOctetString,
		smpKindIPAddr,
		smpKindOpaque,
		smpKindCounter,
		smpKindGuage,
		smpKindTimeTicks,
		smpKindObjectId,
		smpKindNull

		}			SmpKindType;


typedef		enum			SmpTrapTag {

		smpTrapColdStart,
		smpTrapWarmStart,
		smpTrapLinkDown,
		smpTrapLinkUp,
		smpTrapAuthenticationFailure,
		smpTrapEgpNeighborLoss,
		smpTrapEnterpriseSpecific

		}			SmpTrapType;

typedef		CIntlType		SmpSequenceType;

typedef		CUnssType		SmpIndexType;

typedef		CUnsfType		SmpLengthType;

typedef		CBytePtrType		SmpNameType;

typedef		CBytePtrType		SmpValueType;

typedef		CUnslType		SmpNumberType;

typedef		struct			SmpBindTag {

		SmpLengthType		smpBindNameLen;
		SmpNameType		smpBindName;
		SmpKindType		smpBindKind;
		SmpLengthType		smpBindValueLen;
		SmpValueType		smpBindValue;
		SmpNumberType		smpBindNumber;

		}			SmpBindType;

typedef		SmpBindType		*SmpBindPtrType;

typedef		struct			SmpRequestTag {

		SmpCommandType		smpRequestCmd;
		ApsIdType		smpRequestCommunity;
		SmpSequenceType		smpRequestId;
		SmpErrorType		smpRequestError;
		SmpIndexType		smpRequestIndex;
		SmpLengthType		smpRequestEnterpriseLen;
		SmpNameType		smpRequestEnterprise;
		SmpLengthType		smpRequestAgentLen;
		SmpValueType		smpRequestAgent;
		SmpTrapType		smpRequestGenericTrap;
		SmpNumberType		smpRequestSpecificTrap;
		SmpNumberType		smpRequestTimeStamp;
		SmpIndexType		smpRequestCount;
		SmpBindPtrType		smpRequestBinds;

		}			SmpRequestType;

typedef		SmpRequestType		*SmpRequestPtrType;

SmpIdType		smpNew ();
SmpIdType		smpFree ();

CVoidType		smpInit ();
SmpStatusType		smpInput ();
SmpStatusType		smpRequest ();

#endif		/*	_SMP_H_	*/
