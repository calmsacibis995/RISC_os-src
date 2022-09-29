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
#ident	"$Header: publickey.c,v 1.2.1.2 90/05/09 14:44:54 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)publickey.c	1.2 88/05/08 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.3
 */

/*
 * Public key lookup routines
 */
#include <stdio.h>
#include <pwd.h>
#include <rpc/rpc.h>
#include <rpc/key_prot.h>


extern char *strchr();
extern char *strcpy();

static char PKMAP[] = "publickey.byname";

/*
 * Get somebody's encrypted secret key from the database, using
 * the given passwd to decrypt it.
 */
getsecretkey(netname, secretkey, passwd)
	char *netname;
	char *secretkey;
	char *passwd;
{
	char *domain;
	int len;
	char *lookup;
	int err;
	char *p;


	err = yp_get_default_domain(&domain);
	if (err) {
		return(0);
	}
	err = yp_match(domain, PKMAP, netname, strlen(netname), &lookup, &len);
	if (err) {
		return(0);
	}
	lookup[len] = 0;
	p = strchr(lookup,':');
	if (p == NULL) {
		free(lookup);
		return(0);
	}
	p++;
	if (!xdecrypt(p, passwd)) {
		free(lookup);
		return(0);
	}
	if (bcmp(p, p + HEXKEYBYTES, KEYCHECKSUMSIZE) != 0) {
		secretkey[0] = 0;
		free(lookup);
		return(1);
	}
	p[HEXKEYBYTES] = 0;
	(void) strcpy(secretkey, p);
	free(lookup);
	return(1);
}



/*
 * Get somebody's public key
 */
getpublickey(netname, publickey)
	char *netname;
	char *publickey;
{
	char *domain;
	int len;
	char *lookup;
	int err;
	char *p;

	err = yp_get_default_domain(&domain);	
	if (err) {
		return(0);
	}
	err = yp_match(domain, PKMAP, netname, strlen(netname), &lookup, &len);
	if (err) {
		return(0);
	}
	p = strchr(lookup, ':');
	if (p == NULL) {
		free(lookup);
		return(0);
	}
	*p = 0;	
	(void) strcpy(publickey, lookup);
	return(1);
}
