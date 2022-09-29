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
#ident	"$Header: snmptrapd.c,v 1.7.1.2 90/05/09 17:36:52 wje Exp $"

/*
 *	$Header: snmptrapd.c,v 1.7.1.2 90/05/09 17:36:52 wje Exp $
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
#include	<syslog.h>

#include	<host.h>

#include	<ctypes.h>
#include	<rdx.h>
#include	<debug.h>
#include	<smp.h>
#include	<aps.h>
#include	<ap0.h>
#include	<asn.h>
#include	<smx.h>
#include	<oid.h>
#include	<udp_request.h>

#define		cmdTextSize		(64)
#define		cmdBufferSize		(2048)

asn_enum GenericTraps_enum[] = {
    "coldStart", 0, "warmStart", 1, "linkDown", 2, "linkUp", 3,
    "authenticationFailure", 4, "egpNeighborLoss", 5,
    "enterpriseSpecific", 6, NULL };

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
	SmpIndexType		i;
	SmpBindPtrType		bind;
	CCharType		text [ cmdTextSize ];

	asn_enum		enums;
	asn_enum *		enumsp = &enums;

        if (req->smpRequestCmd != smpCommandTrap) {
                return (errOk);
        }

        smp = smp;
	syslog (LOG_DAEMON, "\nCommand: %d\n", req->smpRequestCmd);
	if (smxObjectIdToText (text, (CIntfType) cmdTextSize,
		(CBytePtrType) req->smpRequestEnterprise,
		(CIntfType) req->smpRequestEnterpriseLen) < (CIntfType) 0) {
		syslog (LOG_DAEMON, "Enterprise: GARBLED\n");
	}
	else {
		syslog (LOG_DAEMON, "Enterprise: %s\n", text);
	}
	if (smxIPAddrToText (text, (CIntfType) cmdTextSize,
		(CBytePtrType) req->smpRequestAgent,
		(CIntfType) req->smpRequestAgentLen) < (CIntfType) 0) {
		syslog (LOG_DAEMON, "Agent: GARBLED\n");
	}
	else {
		syslog (LOG_DAEMON, "Agent: %s\n", text);
	}
	for (enumsp = GenericTraps_enum; enumsp->enum_text; enumsp++) {
		if (enumsp->enum_number == req->smpRequestGenericTrap)
			break;
	}

	if (enumsp->enum_text) {
		syslog (LOG_DAEMON, "Generic Trap: %s\n", enumsp->enum_text);
	} else {
		syslog (LOG_DAEMON, "Generic Trap: %d\n",
			req->smpRequestGenericTrap);
	}
	syslog (LOG_DAEMON, "Specific Trap: %d\n", req->smpRequestSpecificTrap);
	syslog (LOG_DAEMON, "Time Stamp: %d\n", req->smpRequestTimeStamp);
	syslog (LOG_DAEMON, "Count: %d\n", req->smpRequestCount);

	bind = req->smpRequestBinds;
	for (i = req->smpRequestCount; i != 0; i--) {
		if (smxNameToText (text, (CIntfType) cmdTextSize,
			(CBytePtrType) bind->smpBindName,
			(CIntfType) bind->smpBindNameLen,
			&enumsp) >=
			(CIntfType) 0) {
			syslog (LOG_DAEMON, "Name: %s\n", text);
		}
		syslog (LOG_DAEMON, "Kind: %s\n", smxKindToText (bind->smpBindKind));
		if (smxValueToText (text, (CIntfType) cmdTextSize,
			bind, enumsp) >= (CIntfType) 0) {
			syslog (LOG_DAEMON, "Value: %s\n", text);
		}
		else {
			syslog (LOG_DAEMON, "Value: GARBLED\n");
		}
		bind++;
	}
        return (errOk);
}

static	CIntfType	usage (s)

CCharPtrType		s;

{
	fprintf (stderr, "Usage: %s", s);
	fprintf (stderr, " [-p lport]");
	fprintf (stderr, " [-c community]");
	fprintf (stderr, "\n");
	return (1);
}

int		trapdCommand (argc, argv)

int		argc;
char		**argv;

{
	int			s;
	int			salen;
	int			result;
	struct	sockaddr	salocal;
	struct	sockaddr	saremote;
	struct	sockaddr_in	*sin;
        struct  servent         *svp;

	u_short			lport;
	CUnslType		number;

	CByteType		buf [ cmdBufferSize ];
	CBytePtrType		bp;
	SmpIdType		smp;
	ApsIdType		communityId;
	SmpSocketType		udp;
	CCharPtrType		*ap;
	CCharPtrType		cp;
	CBoolType		noerror;

	CCharPtrType		communityString;
	CCharPtrType		lportString;

	communityString = (CCharPtrType) 0;
	lportString = (CCharPtrType) 0;

	ap = (CCharPtrType *) argv + 1;
	argc--;
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
				lportString = *ap++;
				break;

			default:
				noerror = FALSE;
				break;
			}
			cp++;
		}
	}

	if ((argc != 0) || (! noerror)) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}

	if (lportString != (CCharPtrType) 0) {
                if (rdxDecodeAny (& number, lportString) < (CIntfType) 0) {
                        fprintf (stderr, "%s: Bad local port: %s\n",
                                argv [ 0 ], lportString);
                        return (2);
                }
                else {
                        lport = htons ((u_short) number);
                }
        }
        else {
                svp = getservbyname ("snmptrap", "udp");
                if (svp == (struct servent *) 0) {
                        fprintf (stderr, "%s: No such service: %s/%s\n",
                                "snmptrap", "udp");
                        return (2);
                }
                lport = (u_short) svp->s_port;
        }

	if (communityString == (CCharPtrType) 0) {
		communityString = (CCharPtrType) "public";
	}

	cmdInit ();

	s = socket (AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		(void) perror ("socket");
		return (1);
	}

	sin = (struct sockaddr_in *) & salocal;
        bzero ((char *) sin, sizeof (*sin));
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = (u_long) 0;
	sin->sin_port = lport;

	result = bind (s, & salocal, sizeof (*sin));
	if (result < 0) {
		(void) perror ("bind");
		return (1);
	}

        udp = udpNew (s, (u_long) 0, (u_short) 0);
	sin = (struct sockaddr_in *) & saremote;

	communityId = apsNew ((ApsNameType) communityString,
		(ApsNameType) "trivial", (ApsGoodiesType) 0);

	do {
        	smp = smpNew (udp, udpSend, myUpCall);
		if (smp == (SmpIdType) 0) {
			fprintf (stderr,
				"%s: Error creating protocol object\n",
				argv [ 0 ]);
			udp = udpFree (udp);
			communityId = apsFree (communityId);
			return (2);
		}

		salen = sizeof (saremote);
		result = recvfrom (s, (char *) buf, (int) cmdBufferSize,
			(int) 0, & saremote, & salen);
		DEBUGBYTES (buf, result);
		DEBUG0 ("\n");

		for (bp = buf; ((result > 0) &&
			(smpInput (smp, *bp++) == errOk));
			result--);

		DEBUG1 ("result: %d\n", result);
		smp = smpFree (smp);

	} while (result >= 0);

	udp = udpFree (udp);
	communityId = apsFree (communityId);
	return (close (s));
}

int	main (argc, argv)

int     argc;
char    *argv [];

{
	if (fork())
		exit (0);

        exit (trapdCommand (argc, argv));
}

