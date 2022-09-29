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
#ident	"$Header: host.c,v 1.2.1.3 90/05/09 17:28:03 wje Exp $"

/*
 *	$Header: host.c,v 1.2.1.3 90/05/09 17:28:03 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>
#include	<netdb.h>
#include	<stdio.h>

#include	<local.h>
#include	<host.h>
#include	<ctypes.h>

CBoolType	DisplayInternetNumbers = FALSE;

long		hostAddress (string)

char		*string;

{
	struct		hostent		*hp;
	long		host;
	struct		netent		*np;

	host = (long) inet_addr (string);
	if (host == -1L) {
		hp = gethostbyname (string);
		if (hp == NULL) {
			np = getnetbyname (string);
			if (np == NULL) {
				return (-1);
			} else if (np->n_addrtype != AF_INET) {
				return (-1);
			} else {
				host = 0L;
				bcopy (np->n_net, (char *) & host,
					sizeof (host));
			}
		}
		else if (hp->h_addrtype != AF_INET) {
			return (-1);
		}
		else {
			host = 0L;
			bcopy (hp->h_addr, (char *) & host,
				sizeof (host));
		}
	}
	return (host);
}

int		hostString (result, n, host)

char		*result;
int		n;
long		host;

{
	struct		hostent		*hp;
	struct		netent		*np;
	struct		in_addr		in;
	int				k;
	char				*cp;
	u_long				mask, subnetshift, net;

	if (!DisplayInternetNumbers) {
		hp = gethostbyaddr ((char *) & host, (int) sizeof (host),
			(int) AF_INET);
		if ((hp != NULL) && ((k = strlen (hp->h_name)) > 0)) {
			if (k > n) {
				return (0);
			}
			else {
				(void) strcpy (result, hp->h_name);
				return (k);
			}
		}

		if (IN_CLASSA(net)) {
			mask = IN_CLASSA_NET;
			subnetshift = 8;
		} else if (IN_CLASSB(net)) {
			mask = IN_CLASSB_NET;
			subnetshift = 8;
		} else {
			mask = IN_CLASSC_NET;
			subnetshift = 4;
		}
		/*
		 * If there are more bits than the standard mask
		 * would suggest, subnets must be in use.
		 * Guess at the subnet mask, assuming reasonable
		 * width subnet fields.
		 */
		while (net &~ mask)
			mask = (long)mask >> subnetshift;

		net = net & mask;
		while ((mask & 1) == 0)
			mask >>= 1, net >>= 1;
		np = getnetbyaddr (net, (int) AF_INET);
		if ((np != NULL) && ((k = strlen (np->n_name)) > 0)) {
			if (k > n) {
				return (0);
			}
			else {
				(void) strcpy (result, np->n_name);
				return (k);
			}
		}
	}

	(void) bzero ((char *) & in, (int) sizeof (in));
	in.s_addr = (u_long) host;
	cp = inet_ntoa (in);
	if (cp == (char *) NULL) {
		return (0);
	}
	else if ((k = strlen (cp)) > n) {
		return (0);
	}
	else {
		(void) strcpy (result, cp);
		return (k);
	}
}

