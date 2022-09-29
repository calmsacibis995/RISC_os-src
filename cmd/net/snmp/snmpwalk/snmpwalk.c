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
#ident	"$Header: snmpwalk.c,v 1.3.1.3 90/05/09 17:37:04 wje Exp $"

/*
 *	$Header: snmpwalk.c,v 1.3.1.3 90/05/09 17:37:04 wje Exp $
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
#include	<sys/time.h>

#include	<host.h>

#include	<ctypes.h>
#include	<local.h>
#include	<rdx.h>
#include	<debug.h>
#include	<smp.h>
#include	<aps.h>
#include	<ap0.h>
#include	<asn.h>
#include	<smx.h>
#include	<oid.h>
#include	<udp_request.h>

#define		cmdBufferSize		(2048)
#define		cmdBindListSize		(1)
#define		cmdTextSize		(512)
#define		MAX_NAME_LEN		(32)

static	void	cmdInit ()

{
	aslInit ();
	asnInit ();
	apsInit ();
	ap0Init ();
	smpInit ();
}

static		CBoolType		cmdDone;
static		CByteType		buf [ cmdBufferSize ];
static		SmpBindType		bindList [ cmdBindListSize ];
static		SmpRequestType		req;
static		SmpSequenceType		requestId;
static		CByteType		root[MAX_NAME_LEN];
static		CUnslType		rootlen;

extern		CBoolType		DisplayInternetNumbers;


static  SmpStatusType   myUpCall (smp, req)

SmpIdType               smp;
SmpRequestPtrType       req;

{
	int			cmp;
	SmpIndexType		i;
	SmpBindPtrType		bind;
	SmpBindPtrType		orig;
	CCharType		text [ cmdTextSize ];

	asn_enum		enums;
	asn_enum *		enumsp = &enums;

	if ((req->smpRequestCmd != smpCommandRsp) ||
		(req->smpRequestId != requestId)) {
		return (errOk);
	}

	if (req->smpRequestError == smpErrorNone) {
		bind = req->smpRequestBinds;
		if ((bind->smpBindNameLen < rootlen) ||
			(bcmp (root, bind->smpBindName, rootlen))) {
			cmdDone = TRUE;
		} else {
			bind = req->smpRequestBinds;
			for (i = req->smpRequestCount; i != 0; i--) {
				if (smxNameToText (text,
					(CIntfType) cmdTextSize,
					(CBytePtrType) bind->smpBindName,
					(CIntfType) bind->smpBindNameLen,
					&enumsp) >=
					(CIntfType) 0) {
					printf ("Name: %s\n", text);
				}
				printf ("%s: ",
					smxKindToText (bind->smpBindKind));
				if (smxValueToText (text,
					(CIntfType) cmdTextSize,
					bind, enumsp) >= (CIntfType) 0) {
					printf ("%s\n", text);
				}
				else {
					printf ("GARBLED\n");
				}
				printf ("\n");
				bind++;
			}

			req->smpRequestCmd = smpCommandNext;
			req->smpRequestId = ++requestId;
			(void) smpRequest (smp, req);
		}
	}
	else {
		cmdDone = TRUE;
	}
        return (errOk);
}

static	CIntfType	usage (s)

CCharPtrType		s;

{
	fprintf (stderr, "Usage: %s", s);
	fprintf (stderr, " host");
	fprintf (stderr, " [-n]");
	fprintf (stderr, " [-p port]");
	fprintf (stderr, " [-r retrycnt]");
	fprintf (stderr, " [-t timeout]");
	fprintf (stderr, " [-i requestId]");
	fprintf (stderr, " [-c community]");
	fprintf (stderr, " [name]\n");
	return (1);
}

int		walkCommand (argc, argv)

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
	unsigned		timeout, retry;
	struct	sigvec		newSignalVector;

	u_long			fhost;
	u_short			fport;
	CUnslType		number;

	CByteType		pkt [ cmdBufferSize ];
	CBytePtrType		bp;
	SmpIndexType		bindCount;
	SmpIdType		smp;
	ApsIdType		communityId;
	SmpSocketType		udp;
	SmpBindPtrType		bindp;
	CCharPtrType		*ap;
	CCharPtrType		cp;
	CBoolType		noerror;
	CIntfType		len;
	CIntfType		space;

	CCharPtrType		communityString;
	CCharPtrType		fhostString;
	CCharPtrType		fportString;
	CCharPtrType		retryString;
	CCharPtrType		requestIdString;
	CCharPtrType		timeoutString;

	CCharPtrType		default_mib = ".1.3.6.1.2.1";
	CCharPtrType		*dm = &default_mib;

	struct timeval		tv;
	fd_set			readfds;
	int			cnt;

	communityString = (CCharPtrType) 0;
	fhostString = (CCharPtrType) 0;
	fportString = (CCharPtrType) 0;
	retryString = (CCharPtrType) 0;
	requestIdString = (CCharPtrType) 0;
	timeoutString = (CCharPtrType) 0;

	ap = (CCharPtrType *) argv + 1;
	argc--;
	/* The foreign host is the first argument. */
	if (argc) {
		fhostString = *ap++;
		argc--;
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

			case 'i':
				argc--;
				requestIdString = *ap++;
				break;

			case 'n':
				DisplayInternetNumbers = 1;
				break;

			case 'p':
				argc--;
				fportString = *ap++;
				break;

			case 'r':
				argc--;
				retryString = *ap++;
				break;

			case 't':
				argc--;
				timeoutString = *ap++;
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

	if (argc > 1) {
		return ((int) usage ((CCharPtrType) argv [ 0 ]));
	}

	fhost = (u_long) hostAddress (fhostString);
	if (fhost == (u_long) -1) {
		fprintf (stderr, "%s: %s: unknown host\n",
			argv [ 0 ], fhostString);
		return (2);
	}

	if (fportString != (CCharPtrType) 0) {
                if (rdxDecodeAny (& number, fportString) < (CIntfType) 0) {
                        fprintf (stderr, "%s: Bad  port number: %s\n",
                                argv [ 0 ], fportString);
                        return (2);
                }
                else {
                        fport = htons ((u_short) number);
                }
        }
        else {
                svp = getservbyname ("snmp", "udp");
                if (svp == (struct servent *) 0) {
                        fprintf (stderr, "%s: No such service: %s/%s\n",
                                "snmp", "udp");
                        return (2);
                }
                fport = (u_short) svp->s_port;
        }

	if (retryString != (CCharPtrType) 0) {
                if (rdxDecodeAny (& number, retryString) < (CIntfType) 0) {
                        fprintf (stderr, "%s: Bad retry count: %s\n",
                                argv [ 0 ], retryString);
                        return (2);
                }
                else {
                        retry = number;
                }
        } else {
		retry = (unsigned) 3;
	}

	if (timeoutString != (CCharPtrType) 0) {
                if (rdxDecodeAny (& number, timeoutString) < (CIntfType) 0) {
                        fprintf (stderr, "%s: Bad timeout value: %s\n",
                                argv [ 0 ], timeoutString);
                        return (2);
                }
		else {
			timeout = (unsigned) number;
		}
        } else {
		timeout = (unsigned) 10;
	}

	if (requestIdString != (CCharPtrType) 0) {
                if (rdxDecodeAny (& number, requestIdString) < (CIntfType) 0) {
                        fprintf (stderr, "%s: Bad requestId value: %s\n",
                                argv [ 0 ], requestIdString);
                        return (2);
                }
		else {
			requestId = (SmpSequenceType) number;
		}
	}
	else {
			requestId = (SmpSequenceType) 0;
	}

	if (communityString == (CCharPtrType) 0) {
		communityString = (CCharPtrType) "public";
	}

	cmdInit ();

	bp = buf;
	space = cmdBufferSize;
	bindp = bindList;
	bindCount = (SmpIndexType) 0;

	if (argc == 0) {
		ap = dm;
	}

	if ((len = smxTextToObjectId (bp, space, *ap)) < (CIntfType) 0) {
		fprintf (stderr, "%s: Error parsing argument %s\n",
			argv [ 0 ], *ap);
		return (2);
	} else {
		rootlen = len;
		bcopy (bp, root, rootlen);
		ap++;
		bindp->smpBindName = (SmpNameType) bp;
		bindp->smpBindNameLen = (SmpLengthType) len;
		bp += len;
		space -= len;
		bindp->smpBindKind = smpKindOctetString;
		bindp->smpBindValue = (SmpValueType) 0;
		bindp->smpBindValueLen = (SmpLengthType) 0;
		bindCount++;
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

	req.smpRequestCmd = smpCommandNext;
	req.smpRequestCommunity = communityId;
	req.smpRequestId = requestId;
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

	sin = (struct sockaddr_in *) & saremote;

	cmdDone = FALSE;

	do {
		cnt = 0;
		while ((retry) && (cnt != 1)) {
	        	smp = smpNew (udp, udpSend, myUpCall);
			if (smp == (SmpIdType) 0) {
				fprintf (stderr,
					"%s: Error creating protocol object\n",
					argv [ 0 ]);
				udp = udpFree (udp);
				communityId = apsFree (communityId);
				return (2);
			}

			FD_ZERO (&readfds);
			FD_SET (s, &readfds);
			if (timeout) {
				tv.tv_sec = timeout;
				tv.tv_usec = 0;
				cnt = select (s + 1, &readfds, 0, 0, &tv);
			} else {
				cnt = select (s + 1, &readfds, 0, 0, 0);
			}
			switch (cnt) {
			case 1:
				if (!FD_ISSET(s, &readfds)) {
					printf ("%s: select: Invalid file descriptor.", argv[0]);
					if (retry)
						printf (" Retrying...");
					printf ("\n");
					cnt = 0;
				}
				break;
			case 0:
				printf ("%s: Request timeout.", argv[0]);
				if (retry)
					printf (" Retrying...");
				printf ("\n");
				break;
			case -1:
				perror("select");
				break;
			default:
				printf("%s: select returned %d\n", argv[0], cnt);
				break;
			}
			if (cnt == 0) {
				retry--;
				if (smpRequest (smp, & req) != errOk) {
					fprintf (stderr, "%s: Error sending protocol request\n",
						argv [ 0 ]);
					smp = smpFree (smp);
					udp = udpFree (udp);
					communityId = apsFree (communityId);
					return (2);
				}
				smp = smpFree (smp);
			}
		}

		if (cnt) {
			salen = sizeof (saremote);
			result = recvfrom (s, (char *) pkt,
				(int) cmdBufferSize, (int) 0,
				&saremote, &salen);
			DEBUGBYTES (pkt, result);
			DEBUG0 ("\n");

			for (bp = pkt; ((result > 0) &&
				(smpInput (smp, *bp++) == errOk));
				result--);

			smp = smpFree (smp);

			DEBUG1 ("result: %d\n", result);
		} else
			exit (4);

	} while ((result >= 0) && (! cmdDone));

	udp = udpFree (udp);
	communityId = apsFree (communityId);
	return (close (s));
}

int	main (argc, argv)

int     argc;
char    *argv [];

{
        exit (walkCommand (argc, argv));
}

