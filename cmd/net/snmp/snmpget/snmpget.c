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
#ident	"$Header: snmpget.c,v 1.5.1.3 90/05/09 17:35:26 wje Exp $"

/*
 *	$Header: snmpget.c,v 1.5.1.3 90/05/09 17:35:26 wje Exp $
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
#define		cmdBindListSize		(64)
#define		cmdTextSize		(512)

static		SmpSequenceType		requestId;
static		CBoolType		cmdDone;

extern		CBoolType		DisplayInternetNumbers;

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

        if ((req->smpRequestCmd != smpCommandRsp) ||
                (req->smpRequestId != requestId)) {
                return (errOk);
        }

	cmdDone = TRUE;
        smp = smp;

	if (req->smpRequestError != smpErrorNone) {
		printf ("Error: %s.  Index: %d.\n", smxErrorToText (req->smpRequestError), req->smpRequestIndex);
	}

	bind = req->smpRequestBinds;
	for (i = req->smpRequestCount; i != 0; i--) {
		if (smxNameToText (text, (CIntfType) cmdTextSize,
			(CBytePtrType) bind->smpBindName,
			(CIntfType) bind->smpBindNameLen,
			&enumsp) >=
			(CIntfType) 0) {
			printf ("Name: %s\n", text);
		}

		if (req->smpRequestError == smpErrorNone) {
			printf ("%s: ", smxKindToText (bind->smpBindKind));
			if (smxValueToText (text, (CIntfType) cmdTextSize,
				bind, enumsp) >= (CIntfType) 0) {
				printf ("%s\n", text);
			}
			else {
				printf ("GARBLED\n");
			}
		}
		printf ("\n");
		bind++;
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
	fprintf (stderr, " [name] ...\n");
	return (1);
}

int		getCommand (argc, argv)

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

	CByteType		buf [ cmdBufferSize ];
	CBytePtrType		bp;
	SmpBindType		bindList [ cmdBindListSize ];
	SmpIndexType		bindCount;
	SmpIdType		smp;
	ApsIdType		communityId;
	SmpSocketType		udp;
	SmpRequestType		req;
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

	if (argc > cmdBindListSize) {
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
                        fprintf (stderr, "%s: Bad port number: %s\n",
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

	for (noerror = TRUE; (noerror) && (argc > 0); argc--) {
		if ((len = smxTextToObjectId (bp, space, *ap)) <
			(CIntfType) 0) {
			noerror = FALSE;
		}
		else {
			ap++;
			bindp->smpBindName = (SmpNameType) bp;
			bindp->smpBindNameLen = (SmpLengthType) len;
			bp += len;
			space -= len;
			bindp->smpBindKind = smpKindOctetString;
			bindp->smpBindValue = (SmpValueType) 0;
			bindp->smpBindValueLen = (SmpLengthType) 0;
			bindp++;
			bindCount++;
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
		fprintf (stderr, "%s: Error creating protocol object\n",
			argv [ 0 ]);
		udp = udpFree (udp);
		return (2);
	}

	communityId = apsNew ((ApsNameType) communityString,
		(ApsNameType) "trivial", (ApsGoodiesType) 0);

	req.smpRequestCmd = smpCommandGet;
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
			if (timeout != (unsigned) 0) {
				tv.tv_sec = timeout;
				tv.tv_usec = 0;
				cnt = select (s + 1, &readfds, 0, 0, &tv);
			} else {
				cnt = select (s + 1, &readfds, 0, 0, 0);
			}
			switch (cnt) {
			case 1:
				if (!FD_ISSET(s, &readfds))
					cnt = 0;
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
			result = recvfrom (s, (char *) buf,
				(int) cmdBufferSize, (int) 0,
				&saremote, &salen);
			DEBUGBYTES (buf, result);
			DEBUG0 ("\n");

			for (bp = buf; ((result > 0) &&
				(smpInput (smp, *bp++) == errOk));
				result--);

			DEBUG1 ("result: %d\n", result);
			smp = smpFree (smp);
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
        exit (getCommand (argc, argv));
}

