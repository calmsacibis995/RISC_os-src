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
#ident	"$Header: keylogin.c,v 1.1.1.2 90/05/09 19:12:14 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)keylogin.c	1.1 88/07/14 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/*
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.1
 */

/*
 * Set secret key on local machine
 */
#include <stdio.h>
#include <rpc/rpc.h>
#include <rpc/key_prot.h>

main(argc,argv)
	int argc;
	char *argv[];
{
	char fullname[MAXNETNAMELEN + 1];
	char secret[HEXKEYBYTES + 1];
	char *getpass();

	getnetname(fullname);
	if (! getsecretkey(fullname, secret, getpass("Password:"))) {
		fprintf(stderr, "Can't find %s's secret key\n", fullname);
		exit(1);
	}
	if (secret[0] == 0) {
		fprintf(stderr, "Password incorrect for %s\n", fullname);
		exit(1);
	}
	if (key_setsecret(secret) < 0) {
		fprintf(stderr, "Could not set %s's secret key\n", fullname);
		fprintf(stderr, "Maybe the keyserver is down?\n");
		exit(1);
	}
	exit(0);
}
