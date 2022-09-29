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
#ident	"$Header: xcrypt.c,v 1.2.1.2 90/05/09 14:49:56 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)xcrypt.c	1.4 88/07/26 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.3
 */

/*
 * Hex encryption/decryption and utility routines
 */

#include <stdio.h>
#include <des_crypt.h>

extern char *malloc();

extern char hex[];	/* forward */
static char hexval();

/*
 * Encrypt a secret key given passwd
 * The secret key is passed and returned in hex notation.
 * Its length must be a multiple of 16 hex digits (64 bits).
 */
xencrypt(secret, passwd)
	char *secret;
	char *passwd;
{
	char key[8];
	char ivec[8];
	char *buf;
	int err;
	int len;

	len = strlen(secret) / 2;
	buf = malloc((unsigned)len);

	hex2bin(len, secret, buf);
	passwd2des(passwd, key);
	bzero(ivec, 8);

	err = cbc_crypt(key, buf, len, DES_ENCRYPT | DES_HW, ivec);
	if (DES_FAILED(err)) {	
		free(buf);
		return (0);
	}
	bin2hex(len, (unsigned char *) buf, secret);
	free(buf);
	return (1);
}

/*
 * Decrypt secret key using passwd
 * The secret key is passed and returned in hex notation.
 * Once again, the length is a multiple of 16 hex digits
 */
xdecrypt(secret, passwd)
	char *secret;
	char *passwd;
{
	char key[8];
	char ivec[8];
	char *buf;
	int err;
	int len;

	len = strlen(secret) / 2;
	buf = malloc((unsigned)len);

	hex2bin(len, secret, buf);
	passwd2des(passwd, key);	
	bzero(ivec, 8);

	err = cbc_crypt(key, buf, len, DES_DECRYPT | DES_HW, ivec);
	if (DES_FAILED(err)) {
		free(buf);
		return (0);
	}
	bin2hex(len, (unsigned char *) buf, secret);
	free(buf);
	return (1);
}


/*
 * Turn password into DES key
 */
passwd2des(pw, key)
	char *pw;
	char *key;
{
	int i;

	bzero(key, 8);
	for (i = 0; *pw; i = (i+1)%8) {
		key[i] ^= *pw++ << 1;
	}
	des_setparity(key);
}



/*
 * Hex to binary conversion
 */
static
hex2bin(len, hexnum, binnum)
	int len;
	char *hexnum;
	char *binnum;
{
	int i;

	for (i = 0; i < len; i++) {
		*binnum++ = 16 * hexval(hexnum[2*i]) + hexval(hexnum[2*i+1]);	
	}
}

/*
 * Binary to hex conversion
 */
static
bin2hex(len, binnum, hexnum)
	int len;
	unsigned char *binnum;
	char *hexnum;
{
	int i;
	unsigned val;

	for (i = 0; i < len; i++) {
		val = binnum[i];
		hexnum[i*2] = hex[val >> 4];
		hexnum[i*2+1] = hex[val & 0xf];
	}
	hexnum[len*2] = 0;
}

static char hex[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
};

static char
hexval(c)
	char c;
{
	if (c >= '0' && c <= '9') {
		return (c - '0');
	} else if (c >= 'a' && c <= 'z') {
		return (c - 'a' + 10);
	} else if (c >= 'A' && c <= 'Z') {
		return (c - 'A' + 10);
	} else {
		return (-1);
	}
}
