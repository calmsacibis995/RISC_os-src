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
#ident	"$Header: snmptrap.c,v 1.3.1.2 90/05/09 17:36:07 wje Exp $"

/*
 *	$Header: snmptrap.c,v 1.3.1.2 90/05/09 17:36:07 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include        <sys/types.h>
#include        <sys/socket.h>
#include        <netinet/in.h>
#include        <stdio.h>
#include        <netdb.h>
#include        <signal.h>

#include	<host.h>

#include	<ctypes.h>
#include	<rdx.h>
#include	<debug.h>
#include	<smp.h>
#include	<aps.h>
#include	<ap0.h>
#include	<asn.h>
#include	<asl.h>
#include	<smx.h>
#include	<udp_request.h>

#define		cmdBufferSize		(2048)
#define		cmdBindListSize		(20)
#define		cmdTextSize		(64)

static	void	cmdInit ()

{
	aslInit ();
	asnInit ();
	apsInit ();
	ap0Init ();
	smpInit ();
}

static  SmpStatusType   myUpCall (smp, req)

SmpIdType               smp;
SmpRequestPtrType       req;

{
	smp = smp;
	req = req;
        return (errOk);
}

static	CIntfType	usage (s)

CCharPtrType		s;

{
	fprintf (stderr, "Usage: %s", s);
	fprintf (stderr, " host");
	fprintf (stderr, " [-p port]");
	fprintf (stderr, " [-c community]");
	fprintf (stderr, " [enterprise] [agent addr] [generic trap]");
	fprintf (stderr, " [specific trap] [timestamp]");
	fprintf (stderr, " [name] [type] [value] ...\n");
	return (1);
}

int		trapCommand (argc, argv)

int		argc;
char		**argv;

{
	int			s;
	int			result;
	struct	sockaddr	salocal;
	struct	sockaddr_in	*sin;
        struct  servent         *svp;

	u_long			fhost;
	u_short			fport;
	CUnslType		number;
	CIntlType		value;
	CUnslType		ticks;

	CByteType		buf [ cmdBufferSize ];
	CBytePtrType		bp;
	SmpBindType		bindList [ cmdBindListSize ];
	SmpIndexType		bindCount;
	SmpIdType		smp;
	ApsIdType		communityId;
	SmpSocketType		udp;
	SmpRequestType		req;
	SmpBindPtrType		bindp;
	SmpKindType		kind;

	CCharPtrType		*ap;
	CCharPtrType		cp;
	CBoolType		noerror;
	CIntfType		len;
	CIntfType		space;

	CCharPtrType		communityString;
	CCharPtrType		fhostString;
	CCharPtrType		fportString;

	communityString = (CCharPtrType) 0;
	fhostString = (CCharPtrType) 0;
	fportString = (CCharPtrType) 0;

	ap = (CCharPtrType *) argv + 1;
	argc--;
	if (argc) {
		argc--;
		fhostString = *ap++;
	}
	noerror = TRUE;
	while ((argc != 0) && (**ap == (CCharType) '-') && (noerror)) {
		cp = *ap;
		cp++;
		ap++;
		argc--;
		while ((*cp != (CCharType) 0) && (noerror)) {
			switch (*cp) {

			case 'c':
				argc--;
				communityString = *ap++;
				break;

			case 'p':
				argc--;
				fportString = *ap++;
				break;

			default:
				noerror = FALSE;
				break;
			}
			cp++;
		}
	}

	if ((! noerror) || (fhostString == (CCharPtrType) 0)) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}

	fhost = (u_long) hostAddress (fhostString);
	if (fhost == (u_long) -1) {
		fprintf (stderr, "%s: Bad foreign host: %s\n",
			argv [ 0 ], fhostString);
		return (2);
	}

	if (fportString != (CCharPtrType) 0) {
                if (rdxDecodeAny (& number, fportString) < (CIntfType) 0) {
                        fprintf (stderr, "%s: Bad local port: %s\n",
                                argv [ 0 ], fportString);
                        return (2);
                }
                else {
                        fport = htons ((u_short) number);
                }
        }
        else {
                svp = getservbyname ("snmp-trap", "udp");
                if (svp == (struct servent *) 0) {
                        fprintf (stderr, "%s: No such service: %s/%s\n",
                                "snmp", "udp");
                        return (2);
                }
                fport = (u_short) svp->s_port;
        }

	if (communityString == (CCharPtrType) 0) {
		communityString = (CCharPtrType) "public";
	}

	cmdInit ();

	bp = buf;
	space = cmdBufferSize;

	if (argc <= 0) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}
	else if ((len = smxTextToObjectId (bp, space, *ap)) < (CIntfType) 0) {
		fprintf (stderr, "%s: Bad enterprise %s\n", argv [ 0 ], *ap);
		return (2);
	}
	else {
		req.smpRequestEnterprise = (SmpNameType) bp;
		req.smpRequestEnterpriseLen = (SmpLengthType) len;
		bp += len;
		space -= len;
		argc--;
		ap++;
	}

	if (argc <= 0) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}
	else if ((len = smxTextToIPAddr (bp, space, *ap)) < (CIntfType) 0) {
		fprintf (stderr, "%s: Bad agent address %s\n", argv [ 0 ], *ap);
		return (2);
	}
	else {
		req.smpRequestAgent = (SmpValueType) bp;
		req.smpRequestAgentLen = (SmpLengthType) len;
		bp += len;
		space -= len;
		argc--;
		ap++;
	}

	if (argc <= 0) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}
	else if ((smxTextToInteger (& value, *ap) < (CIntfType) 0) ||
		(value < (CIntlType) 0) ||
		(value > (CIntlType) smpTrapEnterpriseSpecific)) {
		fprintf (stderr, "%s: Bad generic trap %s\n", argv [ 0 ], *ap);
		return (2);
	}
	else {
		req.smpRequestGenericTrap = (SmpTrapType) value;
		argc--;
		ap++;
	}

	if (argc <= 0) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}
	else if (smxTextToInteger (& value, *ap) < (CIntfType) 0) {
		fprintf (stderr, "%s: Bad specific trap %s\n", argv [ 0 ], *ap);
		return (2);
	}
	else {
		req.smpRequestSpecificTrap = (SmpNumberType) value;
		argc--;
		ap++;
	}

	if (argc <= 0) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}
	else if (smxTextToCounter (& ticks, *ap) < (CIntfType) 0) {
		fprintf (stderr, "%s: Bad time stamp %s\n", argv [ 0 ], *ap);
		return (2);
	}
	else {
		req.smpRequestTimeStamp = (SmpNumberType) ticks;
		argc--;
		ap++;
	}

	if ((argc / 3 > cmdBindListSize) || ((argc % 3) != 0)) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}

	bindp = bindList;
	bindCount = (SmpIndexType) 0;

	for (noerror = TRUE; (noerror) && (argc > 0); argc -= 3) {
		if ((len = smxTextToObjectId (bp, space, *ap)) <
			(CIntfType) 0) {
			noerror = FALSE;
		}
		else if ((kind = smxTextToKind (*(++ap))) == smpKindNone) {
			noerror = FALSE;
		}
		else {
			ap++;
			bindp->smpBindName = (SmpNameType) bp;
			bindp->smpBindNameLen = (SmpLengthType) len;
			bindp->smpBindKind = kind;
			bp += len;
			space -= len;
			bindp->smpBindValue = (SmpValueType) bp;
			bindp->smpBindValueLen = (SmpLengthType) space;
			if ((len = smxTextToValue (bindp, *ap)) <
				(CIntfType) 0) {
				noerror = FALSE;
			}
			else {
				ap++;
				space -= len;
				bp += len;
				bindp++;
				bindCount++;
			}
		}
	}

	if (! noerror) {
		fprintf (stderr, "%s: Error parsing argument %s\n",
			argv [ 0 ], *ap);
		return (2);
	}

	s = socket (AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		(void) perror ("socket");
		return (1);
	}

	sin = (struct sockaddr_in *) & salocal;
        bzero ((char *) sin, sizeof (*sin));
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = (u_long) 0;
	sin->sin_port = (u_short) 0;

	result = bind (s, & salocal, sizeof (*sin));
	if (result < 0) {
		(void) perror ("bind");
		return (1);
	}

        udp = udpNew (s, fhost, fport);
        smp = smpNew (udp, udpSend, myUpCall);
	if (smp == (SmpIdType) 0) {
		fprintf (stderr,
			"%s: Error creating protocol object\n", argv [ 0 ]);
		udp = udpFree (udp);
		return (2);
	}

	communityId = apsNew ((ApsNameType) communityString,
		(ApsNameType) "trivial", (ApsGoodiesType) 0);

	req.smpRequestCmd = smpCommandTrap;
	req.smpRequestCommunity = communityId;
	req.smpRequestId = (SmpSequenceType) 0;
	req.smpRequestError = smpErrorNone;
	req.smpRequestIndex = (SmpIndexType) 0;
	req.smpRequestCount = bindCount;
	req.smpRequestBinds = bindList;

	if (smpRequest (smp, & req) != errOk) {
		fprintf (stderr, "%s: Error sending protocol request\n",
			argv [ 0 ]);
		smp = smpFree (smp);
		udp = udpFree (udp);
		communityId = apsFree (communityId);
		return (2);
	}

	smp = smpFree (smp);
	udp = udpFree (udp);
	communityId = apsFree (communityId);
	return (close (s));
}

int	main (argc, argv)

int     argc;
char    *argv [];

{
        exit (trapCommand (argc, argv));
}

